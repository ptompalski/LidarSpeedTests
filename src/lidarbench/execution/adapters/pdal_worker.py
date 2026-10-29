"""Launcher process for the PDAL adapter (stdlib only).

Run as the benchmarked subprocess by
:class:`~lidarbench.execution.adapters.pdal.PdalAdapter`::

    python -m lidarbench.execution.adapters.pdal_worker --job <work_dir>/pdal-job.json

PDAL has no batch mode for per-tile rasters with neighbour buffers, and one
``pdal pipeline`` process handles one pipeline. The worker therefore launches
the native ``pdal pipeline <pipeline.json> --metadata <file>`` command once per
input tile (argv, no shell), at most ``concurrency`` at a time. It adds no
processing of its own.

Each child's combined stdout/stderr goes to a per-tile log in the work
directory and is echoed to the worker's stdout afterwards (so the attempt logs
contain it). After the first failure no further pipeline is started and the
running ones are terminated. The worker writes ``lidarbench.pdal-run/1`` (exit
codes and wall-clock launch/exit instants of every child) atomically and exits
0 only if every pipeline exited 0.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import subprocess
import sys
import threading
import time
from collections.abc import Sequence
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from typing import Any

JOB_SCHEMA = "lidarbench.pdal-job/1"
RUN_SCHEMA = "lidarbench.pdal-run/1"
CLOCK = "time.time_ns() in lidarbench.execution.adapters.pdal_worker"
MODE_FLAGS = {"stream": ("--stream",), "standard": ("--nostream",), "prefer": ()}
"""``pdal pipeline`` flags per execution mode (``prefer`` is PDAL's own default)."""


class JobError(ValueError):
    """The job file is unusable."""


def pipeline_argv(pdal: str, pipeline: Path, metadata: Path, mode: str) -> list[str]:
    """``pdal pipeline <pipeline> --metadata <metadata> [mode flag]``."""
    return [
        pdal,
        "pipeline",
        os.fspath(pipeline),
        "--metadata",
        os.fspath(metadata),
        *MODE_FLAGS[mode],
    ]


def load_job(path: Path) -> dict[str, Any]:
    """Read and check a ``lidarbench.pdal-job/1`` file."""
    try:
        job = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError) as exc:
        raise JobError(f"cannot read job {path}: {exc}") from exc
    if not isinstance(job, dict) or job.get("schema") != JOB_SCHEMA:
        raise JobError(f"job schema must be {JOB_SCHEMA}")
    if not isinstance(job.get("pdal"), str) or not Path(job["pdal"]).is_absolute():
        raise JobError("job.pdal must be an absolute executable path")
    if job.get("execution_mode") not in MODE_FLAGS:
        raise JobError(f"job.execution_mode must be one of {sorted(MODE_FLAGS)}")
    concurrency = job.get("concurrency")
    if isinstance(concurrency, bool) or not isinstance(concurrency, int) or concurrency < 1:
        raise JobError("job.concurrency must be a positive integer")
    tiles = job.get("tiles")
    if not isinstance(tiles, list) or not tiles:
        raise JobError("job.tiles must be a non-empty list")
    for i, tile in enumerate(tiles):
        if not isinstance(tile, dict) or not all(
            isinstance(tile.get(k), str) and tile[k]
            for k in ("stem", "pipeline", "metadata", "log")
        ):
            raise JobError(f"job.tiles[{i}] needs stem, pipeline, metadata and log names")
    if not isinstance(job.get("result"), str) or not job["result"]:
        raise JobError("job.result must name the run record")
    return job


def _sha256(path: Path) -> str | None:
    try:
        return hashlib.sha256(path.read_bytes()).hexdigest()
    except OSError:
        return None


class _Runner:
    def __init__(self, job: dict[str, Any], base: Path) -> None:
        self.job = job
        self.base = base
        self.stop = threading.Event()
        self.lock = threading.Lock()
        self.running: dict[int, subprocess.Popen[bytes]] = {}

    def run_tile(self, index: int) -> dict[str, Any]:
        tile = self.job["tiles"][index]
        log = self.base / tile["log"]
        argv = pipeline_argv(
            self.job["pdal"],
            self.base / tile["pipeline"],
            self.base / tile["metadata"],
            self.job["execution_mode"],
        )
        record: dict[str, Any] = {
            "stem": tile["stem"],
            "pipeline": tile["pipeline"],
            "metadata": tile["metadata"],
            "log": tile["log"],
            "argv": argv,
            "started": False,
            "exit_code": None,
            "start_wall_ns": None,
            "end_wall_ns": None,
            "terminated": False,
            "error": None,
        }
        if self.stop.is_set():
            return record
        with open(log, "wb") as handle:
            with self.lock:
                if self.stop.is_set():
                    return record
                record["start_wall_ns"] = time.time_ns()
                try:
                    process = subprocess.Popen(
                        argv,
                        stdin=subprocess.DEVNULL,
                        stdout=handle,
                        stderr=subprocess.STDOUT,
                        cwd=self.base,
                        shell=False,
                    )
                except OSError as exc:
                    record["error"] = f"cannot start pdal: {exc.strerror or exc}"
                    self.stop.set()
                    return record
                record["started"] = True
                self.running[index] = process
            code = process.wait()
            record["end_wall_ns"] = time.time_ns()
            with self.lock:
                self.running.pop(index, None)
                record["exit_code"] = code
                if code != 0:
                    record["terminated"] = self.stop.is_set()
                    if not self.stop.is_set():
                        self.stop.set()
                        for other in self.running.values():
                            other.terminate()
        return record


def run(job_path: Path) -> int:
    """Execute a job; returns the worker exit status."""
    base = job_path.parent
    job = load_job(job_path)
    runner = _Runner(job, base)
    started = time.time_ns()
    count = len(job["tiles"])
    with ThreadPoolExecutor(max_workers=min(job["concurrency"], count)) as pool:
        records = list(pool.map(runner.run_tile, range(count)))
    finished = time.time_ns()

    failed = [r for r in records if r["exit_code"] != 0]
    for r in records:
        log = base / r["log"]
        r["log_sha256"] = _sha256(log) if r["started"] else None
        if not r["started"]:
            continue
        print(
            f"== pdal pipeline {r['stem']} ({r['pipeline']}): exit {r['exit_code']} ==", flush=True
        )
        try:
            text = log.read_text(encoding="utf-8", errors="replace")
        except OSError:
            text = ""
        if text:
            print(text, end="" if text.endswith("\n") else "\n", flush=True)
    record = {
        "schema": RUN_SCHEMA,
        "clock": CLOCK,
        "worker_start_wall_ns": started,
        "worker_end_wall_ns": finished,
        "concurrency": job["concurrency"],
        "execution_mode": job["execution_mode"],
        "tiles": records,
        "succeeded": not failed,
    }
    target = base / job["result"]
    tmp = target.with_name(target.name + ".tmp")
    tmp.write_text(json.dumps(record, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    os.replace(tmp, target)
    if failed:
        names = ", ".join(r["stem"] for r in failed)
        print(
            f"ERROR: {len(failed)} PDAL pipeline(s) failed or did not run: {names}", file=sys.stderr
        )
        return 1
    return 0


def main(argv: Sequence[str] | None = None) -> int:
    """Command-line entry point."""
    parser = argparse.ArgumentParser(prog="pdal_worker")
    parser.add_argument("--job", required=True, type=Path)
    args = parser.parse_args(argv)
    try:
        return run(args.job.absolute())
    except JobError as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
