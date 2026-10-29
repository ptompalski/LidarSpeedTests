"""Resource sampler process (``python -m lidarbench.monitoring.sampler``).

Started by :class:`~lidarbench.monitoring.psutil_backend.PsutilBackend` before
the timed interval. Protocol (one JSON object per line):

- stdout ``{"event": "ready", "perf_ns", "wall_ns", "pid", "capabilities"}`` once
  counters are primed;
- stdin ``{"cmd": "attach", "pid", "parent_pid", "launch_perf_ns", "launch_wall_ns"}``
  starts process-tree sampling; the first tree interval starts at the launch
  instant. ``parent_pid`` (the orchestrator) guards against pid reuse;
- stdin ``{"cmd": "stop"}`` (or end of input, if the orchestrator dies) takes a
  final sample, prints ``{"event": "stopped", ...}`` and exits 0.

Rows are flushed as they are written, so samples survive a crash.
"""

from __future__ import annotations

import argparse
import csv
import json
import os
import platform
import queue
import sys
import threading
import time
from collections.abc import Sequence
from dataclasses import dataclass
from pathlib import Path
from typing import IO, Any

import psutil

from lidarbench.monitoring.base import ERRORS_COLUMN, MACHINE_COLUMNS, TREE_COLUMNS

_IO_SUPPORTED = hasattr(psutil.Process, "io_counters")


def _session_of(pid: int) -> int | None:
    """Session id of ``pid`` on POSIX (``None`` on Windows or if it cannot be read).

    Tools run as session leaders (``start_new_session``). Their helpers may be
    reparented away from the tree (for example R ``parallelly`` workers started
    through a shell) but stay in the session, so they are still sampled.
    """
    if sys.platform == "win32":
        return None
    try:
        sid = os.getsid(pid)
    except OSError:
        return None
    return sid


def _now() -> tuple[int, int]:
    perf = time.perf_counter_ns()
    return perf, time.time_ns()


def _emit(event: dict[str, Any]) -> None:
    sys.stdout.write(json.dumps(event, sort_keys=True) + "\n")
    sys.stdout.flush()


@dataclass
class _Tracked:
    proc: psutil.Process
    cpu: float = 0.0
    io_read: int = 0
    io_write: int = 0
    emitted_cpu: float = 0.0
    emitted_read: int = 0
    emitted_write: int = 0
    rss: int = 0
    alive: bool = True
    counted_new: bool = False


class _CsvSink:
    def __init__(self, path: Path, columns: Sequence[str]) -> None:
        self._handle: IO[str] = open(path, "x", encoding="utf-8", newline="")
        self._writer = csv.DictWriter(self._handle, fieldnames=list(columns))
        self._writer.writeheader()
        self._handle.flush()
        self.rows = 0

    def write(self, row: dict[str, Any]) -> None:
        self._writer.writerow({k: "" if v is None else v for k, v in row.items()})
        self._handle.flush()
        self.rows += 1

    def close(self) -> None:
        self._handle.close()


class Sampler:
    """Collects whole-machine and process-tree samples."""

    def __init__(self, machine_csv: Path, tree_csv: Path) -> None:
        self.machine = _CsvSink(machine_csv, MACHINE_COLUMNS)
        self.tree = _CsvSink(tree_csv, TREE_COLUMNS)
        self.capabilities: dict[str, Any] = {
            "platform": platform.system(),
            "psutil_version": psutil.__version__,
            "logical_cpus": psutil.cpu_count(logical=True),
            "process_io_counters": _IO_SUPPORTED,
        }
        psutil.cpu_percent(interval=None)
        self._disk = self._disk_counters()
        self.capabilities["disk_io_counters"] = self._disk is not None
        self._machine_start = _now()
        self._tree_start: tuple[int, int] | None = None
        self._root: psutil.Process | None = None
        self._root_error: str | None = None
        self._session: int | None = None
        self._launched: float | None = None
        self._procs: dict[tuple[int, float], _Tracked] = {}
        self._denied: set[str] = set()

    @staticmethod
    def _disk_counters() -> Any:
        try:
            return psutil.disk_io_counters(perdisk=False)
        except Exception:
            return None

    def attach(self, pid: int, parent_pid: int, launch_perf_ns: int, launch_wall_ns: int) -> None:
        """Start observing the tree rooted at ``pid``, launched at the given instant."""
        self._tree_start = (launch_perf_ns, launch_wall_ns)
        try:
            root = psutil.Process(pid)
            if root.ppid() != parent_pid:
                raise psutil.NoSuchProcess(pid, msg="pid belongs to another process")
            self._root = root
            sid = _session_of(pid)
            self._session = sid if sid == pid else None
            self._launched = root.create_time()
        except psutil.Error as exc:
            self._root_error = f"root process {pid} not observable: {type(exc).__name__}"
        self.capabilities["membership"] = (
            "descendants of the root plus every process of its session"
            if self._session is not None
            else "descendants of the root"
        )
        self._poll_tree()

    def _session_members(self) -> list[psutil.Process]:
        """Processes of the root's session (POSIX), including ones reparented away from it."""
        if self._session is None:
            return []
        members = []
        for proc in psutil.process_iter():
            try:
                if _session_of(proc.pid) == self._session and (
                    self._launched is None or proc.create_time() >= self._launched - 1.0
                ):
                    members.append(proc)
            except (OSError, psutil.Error):
                continue
        return members

    def _discover(self) -> list[psutil.Process]:
        found: dict[int, psutil.Process] = {}
        if self._root is not None:
            try:
                if self._root.is_running():
                    found[self._root.pid] = self._root
                    for child in self._root.children(recursive=True):
                        found[child.pid] = child
            except psutil.Error:
                pass
        for proc in self._session_members():
            found.setdefault(proc.pid, proc)
        for tracked in self._procs.values():
            if tracked.alive:
                found.setdefault(tracked.proc.pid, tracked.proc)
        return list(found.values())

    def _poll_tree(self) -> None:
        seen: set[tuple[int, float]] = set()
        for proc in self._discover():
            try:
                with proc.oneshot():
                    key = (proc.pid, proc.create_time())
                    times = proc.cpu_times()
                    rss = proc.memory_info().rss
                    io = getattr(proc, "io_counters")() if _IO_SUPPORTED else None  # noqa: B009
            except (psutil.NoSuchProcess, psutil.ZombieProcess):
                continue
            except psutil.AccessDenied as exc:
                self._denied.add(f"access denied to pid {exc.pid}")
                continue
            tracked = self._procs.get(key)
            if tracked is None:
                tracked = self._procs[key] = _Tracked(proc)
            tracked.cpu = times.user + times.system
            tracked.rss = rss
            if io is not None:
                tracked.io_read, tracked.io_write = io.read_bytes, io.write_bytes
            seen.add(key)
        for key, tracked in self._procs.items():
            if tracked.alive and key not in seen:
                tracked.alive = False
                tracked.rss = 0

    def _machine_row(self, end: tuple[int, int], index: int) -> dict[str, Any]:
        errors: list[str] = []
        row: dict[str, Any] = {"cpu_percent": psutil.cpu_percent(interval=None)}
        try:
            vm = psutil.virtual_memory()
            row.update(
                memory_used_bytes=vm.total - vm.available,
                memory_available_bytes=vm.available,
                memory_total_bytes=vm.total,
            )
        except Exception as exc:
            errors.append(f"virtual_memory: {type(exc).__name__}")
        disk = self._disk_counters()
        if disk is not None and self._disk is not None:
            read = disk.read_bytes - self._disk.read_bytes
            write = disk.write_bytes - self._disk.write_bytes
            if read >= 0 and write >= 0:
                row.update(disk_read_bytes=read, disk_write_bytes=write)
            else:
                errors.append("disk counters wrapped or reset")
        elif self._disk is None:
            errors.append("disk_io_counters unavailable")
        self._disk = disk
        start = self._machine_start
        self._machine_start = end
        return {**self._interval(index, start, end), **row, ERRORS_COLUMN: ";".join(errors)}

    @staticmethod
    def _interval(index: int, start: tuple[int, int], end: tuple[int, int]) -> dict[str, Any]:
        return {
            "sample_index": index,
            "interval_start_perf_ns": start[0],
            "interval_end_perf_ns": end[0],
            "interval_start_wall_ns": start[1],
            "interval_end_wall_ns": end[1],
        }

    def _tree_row(self, end: tuple[int, int], index: int) -> dict[str, Any]:
        assert self._tree_start is not None
        start = self._tree_start
        self._tree_start = end
        base = self._interval(index, start, end)
        if self._root is None:
            return {**base, ERRORS_COLUMN: self._root_error or "process tree not observed"}
        self._poll_tree()
        errors = sorted(self._denied)
        self._denied.clear()
        cpu = 0.0
        read = write = 0
        new = exited = 0
        alive = rss = 0
        gone = []
        for key, t in self._procs.items():
            cpu += t.cpu - t.emitted_cpu
            read += t.io_read - t.emitted_read
            write += t.io_write - t.emitted_write
            t.emitted_cpu, t.emitted_read, t.emitted_write = t.cpu, t.io_read, t.io_write
            if not t.counted_new:
                new += 1
                t.counted_new = True
            if t.alive:
                alive += 1
                rss += t.rss
            else:
                exited += 1
                gone.append(key)
        for key in gone:
            del self._procs[key]
        seconds = (end[0] - start[0]) / 1e9
        incomplete = bool(errors)
        row: dict[str, Any] = {
            "process_count": alive,
            "new_processes": new,
            "exited_processes": exited,
            "cpu_seconds": None if incomplete else round(cpu, 6),
            "cpu_percent": (
                None if incomplete or seconds <= 0 else round(100.0 * cpu / seconds, 4)
            ),
            "rss_bytes": None if incomplete else rss,
            "io_read_bytes": read if _IO_SUPPORTED and not incomplete else None,
            "io_write_bytes": write if _IO_SUPPORTED and not incomplete else None,
        }
        return {**base, **row, ERRORS_COLUMN: ";".join(errors)}

    def sample(self) -> None:
        """Close the current interval(s) and write one row per active scope."""
        end = _now()
        if end[0] > self._machine_start[0]:
            self.machine.write(self._machine_row(end, self.machine.rows))
        if self._tree_start is not None and end[0] > self._tree_start[0]:
            self.tree.write(self._tree_row(end, self.tree.rows))

    def close(self) -> None:
        """Close the sample files."""
        self.machine.close()
        self.tree.close()


def _read_commands(commands: queue.Queue[dict[str, Any]], wake: threading.Event) -> None:
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            commands.put(json.loads(line))
        except json.JSONDecodeError:
            commands.put({"cmd": "invalid", "line": line[:200]})
        wake.set()
    commands.put({"cmd": "stop", "reason": "stdin closed"})
    wake.set()


def run(machine_csv: Path, tree_csv: Path, interval_s: float) -> int:
    """Sample until stopped; returns the process exit code."""
    sampler = Sampler(machine_csv, tree_csv)
    commands: queue.Queue[dict[str, Any]] = queue.Queue()
    wake = threading.Event()
    threading.Thread(target=_read_commands, args=(commands, wake), daemon=True).start()
    perf, wall = _now()
    _emit(
        {
            "event": "ready",
            "pid": os.getpid(),
            "perf_ns": perf,
            "wall_ns": wall,
            "capabilities": sampler.capabilities,
        }
    )
    interval_ns = int(interval_s * 1e9)
    next_tick = perf + interval_ns
    warnings: list[str] = []
    stop_reason = None
    try:
        while stop_reason is None:
            wake.wait(max(0.0, (next_tick - time.perf_counter_ns()) / 1e9))
            wake.clear()
            while not commands.empty() and stop_reason is None:
                cmd = commands.get()
                if cmd.get("cmd") == "attach":
                    sampler.attach(
                        int(cmd["pid"]),
                        int(cmd.get("parent_pid", os.getppid())),
                        int(cmd["launch_perf_ns"]),
                        int(cmd["launch_wall_ns"]),
                    )
                elif cmd.get("cmd") == "stop":
                    stop_reason = str(cmd.get("reason", "requested"))
                else:
                    warnings.append(f"ignored command: {cmd}")
            current = time.perf_counter_ns()
            if stop_reason is not None or current >= next_tick:
                sampler.sample()
                next_tick = max(next_tick + interval_ns, current + interval_ns // 2)
    finally:
        sampler.close()
    usage = psutil.Process().cpu_times()
    _emit(
        {
            "event": "stopped",
            "reason": stop_reason,
            "machine_rows": sampler.machine.rows,
            "tree_rows": sampler.tree.rows,
            "sampler_cpu_seconds": usage.user + usage.system,
            "warnings": warnings,
        }
    )
    return 0


def main(argv: Sequence[str] | None = None) -> int:
    """Command-line entry point."""
    parser = argparse.ArgumentParser(prog="lidarbench.monitoring.sampler")
    parser.add_argument("--machine-csv", type=Path, required=True)
    parser.add_argument("--tree-csv", type=Path, required=True)
    parser.add_argument("--interval", type=float, required=True)
    args = parser.parse_args(argv)
    try:
        return run(args.machine_csv, args.tree_csv, args.interval)
    except Exception as exc:
        _emit({"event": "error", "error": f"{type(exc).__name__}: {exc}"})
        print(f"sampler failed: {type(exc).__name__}: {exc}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
