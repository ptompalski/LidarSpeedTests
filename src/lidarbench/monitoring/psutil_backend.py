"""Default cross-platform monitor backend: a psutil sampler in a separate process.

The sampler (:mod:`lidarbench.monitoring.sampler`) is started and primed
**before** the timed interval. The only monitoring work inside the interval is
one ``attach`` line written to the sampler's stdin right after launch. The
sampler runs in its own session/process group, so it is never part of the
benchmarked tree and is not hit by terminal Ctrl+C; it exits on its own if the
orchestrator dies (end of stdin).

See ``design/EXECUTION.md`` for what is and is not measured.
"""

from __future__ import annotations

import json
import os
import platform
import queue
import subprocess
import sys
import threading
from collections.abc import Sequence
from pathlib import Path
from typing import IO, Any

from lidarbench.core.clock import Instant, now
from lidarbench.monitoring.base import (
    IO_SEMANTICS,
    MACHINE_SAMPLES_FILE,
    TREE_SAMPLES_FILE,
    MonitorReport,
    MonitorStartError,
    Scope,
)

SAMPLER_MODULE = "lidarbench.monitoring.sampler"
MIN_INTERVAL_S = 0.05
MAX_INTERVAL_S = 60.0
CLOCK_TOLERANCE_NS = 50_000_000


def _detached_kwargs() -> dict[str, Any]:
    if sys.platform == "win32":
        return {"creationflags": subprocess.CREATE_NEW_PROCESS_GROUP}
    return {"start_new_session": True}


def _pump(stream: IO[bytes], events: queue.Queue[dict[str, Any] | None]) -> None:
    for raw in stream:
        try:
            events.put(json.loads(raw.decode("utf-8", errors="replace")))
        except json.JSONDecodeError:
            events.put({"event": "garbage", "line": raw[:200].decode("utf-8", "replace")})
    events.put(None)


class PsutilMonitor:
    """Handle on one running sampler process."""

    def __init__(
        self,
        proc: subprocess.Popen[bytes],
        events: queue.Queue[dict[str, Any] | None],
        report: MonitorReport,
        stop_timeout_s: float,
        stderr: IO[bytes],
    ) -> None:
        self._proc = proc
        self._events = events
        self._report = report
        self._stop_timeout_s = stop_timeout_s
        self._stderr = stderr
        self._stopped = False

    @property
    def pid(self) -> int:
        """PID of the sampler process."""
        return self._proc.pid

    @property
    def report(self) -> MonitorReport:
        """The (possibly incomplete) report."""
        return self._report

    def _send(self, command: dict[str, Any]) -> bool:
        assert self._proc.stdin is not None
        try:
            self._proc.stdin.write((json.dumps(command) + "\n").encode("utf-8"))
            self._proc.stdin.flush()
        except (OSError, ValueError) as exc:
            self._report.errors.append(f"sampler unreachable ({command['cmd']}): {exc}")
            return False
        return True

    def attach(self, pid: int, launch: Instant) -> None:
        """Start process-tree sampling; never raises."""
        if self._send(
            {
                "cmd": "attach",
                "pid": pid,
                "parent_pid": os.getpid(),
                "launch_perf_ns": launch.perf_ns,
                "launch_wall_ns": launch.wall_ns,
            }
        ):
            self._report.attached = True

    def stop(self) -> MonitorReport:
        """Stop the sampler and return the report; never raises."""
        if self._stopped:
            return self._report
        self._stopped = True
        report = self._report
        if self._proc.poll() is not None:
            report.errors.append(f"sampler exited early with code {self._proc.returncode}")
        else:
            self._send({"cmd": "stop"})
        try:
            if self._proc.stdin is not None:
                self._proc.stdin.close()
        except OSError:
            pass
        stopped = None
        try:
            while True:
                event = self._events.get(timeout=self._stop_timeout_s)
                if event is None:
                    break
                if event.get("event") == "stopped":
                    stopped = event
                elif event.get("event") == "error":
                    report.errors.append(f"sampler error: {event.get('error')}")
        except queue.Empty:
            report.errors.append(f"sampler did not stop within {self._stop_timeout_s} s")
        try:
            code = self._proc.wait(timeout=self._stop_timeout_s)
        except subprocess.TimeoutExpired:
            self._proc.kill()
            code = self._proc.wait()
            report.errors.append("sampler killed after stop timeout")
        self._stderr.close()
        if stopped is not None:
            report.sampler.update({k: v for k, v in stopped.items() if k != "event"})
        report.sampler["exit_code"] = code
        report.stopped_cleanly = stopped is not None and code == 0
        if not report.stopped_cleanly and code != 0:
            report.errors.append(f"sampler exit code {code}")
        return report


class PsutilBackend:
    """psutil-based monitor backend (whole-machine and process-tree scopes)."""

    name = "psutil"
    scopes = frozenset({Scope.WHOLE_MACHINE, Scope.PROCESS_TREE})

    def __init__(
        self,
        interval_s: float = 0.5,
        *,
        python: str | os.PathLike[str] | None = None,
        startup_timeout_s: float = 30.0,
        stop_timeout_s: float = 10.0,
        sampler_argv: Sequence[str] | None = None,
    ) -> None:
        """Configure the backend.

        Args:
            interval_s: Sampling interval (0.05-60 s).
            python: Interpreter for the sampler; defaults to the orchestrator's.
            startup_timeout_s: How long to wait for the sampler's ready message.
            stop_timeout_s: How long to wait for the sampler to stop.
            sampler_argv: Override the sampler command prefix (testing only).
        """
        if not MIN_INTERVAL_S <= interval_s <= MAX_INTERVAL_S:
            raise ValueError(f"interval_s must be within {MIN_INTERVAL_S}-{MAX_INTERVAL_S} s")
        self.interval_s = float(interval_s)
        self._python = os.fspath(python) if python is not None else sys.executable
        self._startup_timeout_s = startup_timeout_s
        self._stop_timeout_s = stop_timeout_s
        self._sampler_argv = tuple(sampler_argv) if sampler_argv else None

    def probe(self) -> tuple[bool, str]:
        """Whether psutil and the sampler interpreter are usable."""
        try:
            import psutil

            psutil.cpu_percent(interval=None)
        except Exception as exc:
            return False, f"psutil unusable: {type(exc).__name__}: {exc}"
        if not Path(self._python).is_file():
            return False, f"sampler interpreter not found: {self._python}"
        return True, ""

    def describe(self) -> dict[str, Any]:
        """Static facts about what this backend measures on this platform."""
        system = platform.system()
        return {
            "backend": self.name,
            "interval_s": self.interval_s,
            "scopes": sorted(str(s) for s in self.scopes),
            "platform": system,
            "process_io_semantics": IO_SEMANTICS.get(system, "unknown platform"),
        }

    def start(self, monitor_dir: Path, attempt_id: str) -> PsutilMonitor:
        """Start and prime the sampler; returns once it is ready.

        Raises:
            MonitorStartError: The sampler could not start or did not become ready.
        """
        monitor_dir = Path(monitor_dir)
        monitor_dir.mkdir(parents=True, exist_ok=True)
        if any(monitor_dir.iterdir()):
            raise MonitorStartError(f"refusing to reuse non-empty monitor directory {monitor_dir}")
        machine_csv = monitor_dir / MACHINE_SAMPLES_FILE
        tree_csv = monitor_dir / TREE_SAMPLES_FILE
        prefix = self._sampler_argv or (self._python, "-m", SAMPLER_MODULE)
        argv = [
            *prefix,
            "--machine-csv",
            os.fspath(machine_csv),
            "--tree-csv",
            os.fspath(tree_csv),
            "--interval",
            repr(self.interval_s),
        ]
        report = MonitorReport(
            self.name, attempt_id, self.interval_s, monitor_dir, machine_csv, tree_csv
        )
        stderr = open(monitor_dir / "sampler-stderr.log", "xb")
        try:
            proc = subprocess.Popen(
                argv,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=stderr,
                cwd=monitor_dir,
                **_detached_kwargs(),
            )
        except OSError as exc:
            stderr.close()
            raise MonitorStartError(f"cannot start sampler: {exc}") from exc
        assert proc.stdout is not None
        events: queue.Queue[dict[str, Any] | None] = queue.Queue()
        threading.Thread(target=_pump, args=(proc.stdout, events), daemon=True).start()

        def fail(reason: str) -> MonitorStartError:
            if proc.poll() is None:
                proc.kill()
            proc.wait()
            stderr.close()
            return MonitorStartError(reason)

        try:
            event = events.get(timeout=self._startup_timeout_s)
        except queue.Empty:
            raise fail(f"sampler not ready within {self._startup_timeout_s} s") from None
        if event is None or event.get("event") != "ready":
            detail = "exited" if event is None else event.get("error", event)
            raise fail(f"sampler failed during startup ({detail}); code {proc.poll()}")
        local = now()
        report.ready = Instant(int(event["perf_ns"]), int(event["wall_ns"]))
        report.capabilities = dict(event.get("capabilities", {}))
        report.sampler["pid"] = event.get("pid")
        offset_delta = (local.wall_ns - local.perf_ns) - (
            report.ready.wall_ns - report.ready.perf_ns
        )
        if abs(offset_delta) > CLOCK_TOLERANCE_NS:
            report.clock_alignment = "wall"
            report.errors.append(
                f"monotonic clocks differ between processes by {offset_delta} ns; "
                "aligning samples on wall time"
            )
        return PsutilMonitor(proc, events, report, self._stop_timeout_s, stderr)
