"""Execute one planned attempt with untimed preparation and checks around the timed run.

Phase order (each phase records its own monotonic/wall instants)::

    input_verification  untimed  inputs exist and are regular files; sizes recorded
    config_files        untimed  adapter config files written into work_dir
    monitor_start       untimed  sampler process started and primed
    process_supervision contains the timed interval (``process.start``..``process.end``:
                        fresh process startup, input reading, compute, output writing)
                        plus untimed tree cleanup and monitor stop
    output_check        untimed  expected outputs present (presence only; not validation)

The result is written to ``<attempt_dir>/execution.json`` with exclusive create.
Nothing is retried. Structural and numerical validation are separate verdicts.
"""

from __future__ import annotations

import dataclasses
import json
import os
from collections.abc import Callable, Iterator
from contextlib import contextmanager
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from lidarbench.core.clock import Instant, now
from lidarbench.execution.adapters.base import Invocation
from lidarbench.execution.plan import InvocationPlan, write_config_files
from lidarbench.execution.process import (
    CancellationToken,
    ExecutionInterrupted,
    ExecutionStatus,
    ProcessResult,
    ThreadBudget,
    run_process,
)
from lidarbench.monitoring.base import MonitorBackend, MonitorHandle, MonitorStartError
from lidarbench.monitoring.summary import unavailable_summary

ATTEMPT_EXECUTION_SCHEMA = "lidarbench.attempt-execution/1"
EXECUTION_RECORD = "execution.json"
STDOUT_LOG = "stdout.log"
STDERR_LOG = "stderr.log"
MONITOR_DIR = "monitor"


@dataclass(frozen=True)
class Phase:
    """Timing of one attempt phase."""

    name: str
    contains_timed_interval: bool
    start: Instant
    end: Instant

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "name": self.name,
            "contains_timed_interval": self.contains_timed_interval,
            "start": self.start.to_record(),
            "end": self.end.to_record(),
            "elapsed_ns": self.end.perf_ns - self.start.perf_ns,
        }


@dataclass(frozen=True)
class AttemptExecution:
    """Outcome of :func:`execute_plan`."""

    adapter_id: str
    workload_id: str
    attempt_dir: Path
    process: ProcessResult | None
    phases: tuple[Phase, ...]
    inputs: tuple[dict[str, Any], ...] = ()
    outputs: tuple[dict[str, Any], ...] = ()
    failure_phase: str | None = None
    failure_reason: str | None = None
    warnings: tuple[str, ...] = ()
    metadata: dict[str, Any] = field(default_factory=dict)

    @property
    def execution_status(self) -> ExecutionStatus:
        """``succeeded`` only if the process itself succeeded and no phase failed."""
        if self.process is None:
            return ExecutionStatus.FAILED
        if self.failure_phase is not None and self.process.succeeded:
            return ExecutionStatus.FAILED
        return self.process.execution_status

    @property
    def missing_required_outputs(self) -> tuple[str, ...]:
        """Required expected outputs absent after the run."""
        return tuple(o["path"] for o in self.outputs if o["required"] and not o["exists"])

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "schema": ATTEMPT_EXECUTION_SCHEMA,
            "adapter_id": self.adapter_id,
            "workload_id": self.workload_id,
            "execution_status": str(self.execution_status),
            "failure_phase": self.failure_phase,
            "failure_reason": self.failure_reason,
            "phases": [p.to_record() for p in self.phases],
            "inputs": list(self.inputs),
            "outputs": list(self.outputs),
            "missing_required_outputs": list(self.missing_required_outputs),
            "output_check_semantics": "presence only; structural/numerical validation are "
            "separate verdicts",
            "warnings": list(self.warnings),
            "metadata": dict(self.metadata),
            "process": self.process.to_record() if self.process else None,
        }


class _Phases:
    def __init__(self) -> None:
        self.done: list[Phase] = []

    @contextmanager
    def run(self, name: str, contains_timed_interval: bool = False) -> Iterator[None]:
        start = now()
        try:
            yield
        finally:
            self.done.append(Phase(name, contains_timed_interval, start, now()))


def _verify_inputs(invocation: Invocation) -> tuple[list[dict[str, Any]], list[str]]:
    records, problems = [], []
    for path in invocation.inputs:
        try:
            st = os.stat(path)
        except OSError as exc:
            problems.append(f"{path}: {exc.strerror or exc}")
            records.append({"path": os.fspath(path), "exists": False})
            continue
        if not Path(path).is_file():
            problems.append(f"{path}: not a regular file")
        records.append(
            {
                "path": os.fspath(path),
                "exists": True,
                "bytes": st.st_size,
                "mtime_ns": st.st_mtime_ns,
            }
        )
    return records, problems


def _check_outputs(plan: InvocationPlan) -> list[dict[str, Any]]:
    records = []
    for artifact in plan.expected_outputs:
        path = Path(artifact.path)
        exists = path.exists()
        records.append(
            {
                "path": os.fspath(path),
                "kind": str(artifact.kind),
                "required": artifact.required,
                "exists": exists,
                "bytes": path.stat().st_size if exists and path.is_file() else None,
            }
        )
    return records


def execute_plan(
    plan: InvocationPlan,
    invocation: Invocation,
    attempt_dir: Path,
    *,
    timeout_s: float | None = None,
    cancel: CancellationToken | None = None,
    monitor_backend: MonitorBackend | None = None,
    monitor_required: bool = True,
    thread_budget: ThreadBudget | None = None,
    terminate_grace_s: float = 5.0,
    run: Callable[..., ProcessResult] = run_process,
) -> AttemptExecution:
    """Run a ready plan in ``attempt_dir`` and write ``execution.json``.

    Args:
        plan: A ready plan from :func:`~lidarbench.execution.plan.plan_invocation`.
        invocation: The invocation the plan was built from.
        attempt_dir: Existing, empty attempt directory (logs, monitor, record).
        timeout_s: Timeout of the timed process interval.
        cancel: Cooperative cancellation token.
        monitor_backend: Monitor backend, or ``None`` to run without monitoring
            (recorded as ``disabled``).
        monitor_required: If the backend cannot start, do not launch (``True``) or
            run with monitoring recorded as ``unavailable`` (``False``).
        thread_budget: Configured budget; defaults to ``invocation.threads``.
        terminate_grace_s: POSIX grace between ``SIGTERM`` and ``SIGKILL``.

    Raises:
        ValueError: The plan is not ready or does not match the invocation.
        FileExistsError: ``attempt_dir`` already holds an execution record or logs.
    """
    if not plan.ready or plan.command is None:
        raise ValueError(f"plan is not ready ({plan.status}): {plan.reason}")
    if plan.workload_id != invocation.workload_id:
        raise ValueError("plan and invocation describe different workloads")
    attempt_dir = Path(attempt_dir)
    record_path = attempt_dir / EXECUTION_RECORD
    for name in (EXECUTION_RECORD, STDOUT_LOG, STDERR_LOG, MONITOR_DIR):
        if (attempt_dir / name).exists():
            raise FileExistsError(f"attempt directory already contains {name}")
    budget = thread_budget or ThreadBudget(invocation.threads)

    phases = _Phases()
    warnings: list[str] = []
    base: dict[str, Any] = {
        "adapter_id": plan.adapter_id,
        "workload_id": plan.workload_id,
        "attempt_dir": attempt_dir,
        "metadata": {
            **dict(plan.metadata),
            "implementation_status": (
                str(plan.implementation_status) if plan.implementation_status else None
            ),
        },
    }

    def finish(**kwargs: Any) -> AttemptExecution:
        result = AttemptExecution(
            **base, phases=tuple(phases.done), warnings=tuple(warnings), **kwargs
        )
        with open(record_path, "x", encoding="utf-8", newline="\n") as handle:
            json.dump(result.to_record(), handle, indent=2, sort_keys=True)
            handle.write("\n")
        return result

    with phases.run("input_verification"):
        inputs, problems = _verify_inputs(invocation)
    if problems:
        return finish(
            process=None,
            inputs=tuple(inputs),
            failure_phase="input_verification",
            failure_reason="; ".join(problems),
        )

    with phases.run("config_files"):
        try:
            write_config_files(plan.command, invocation.work_dir)
            config_error = None
        except OSError as exc:
            config_error = f"{type(exc).__name__}: {exc}"
    if config_error:
        return finish(
            process=None,
            inputs=tuple(inputs),
            failure_phase="config_files",
            failure_reason=config_error,
        )

    monitor: MonitorHandle | None = None
    monitor_error: str | None = None
    if monitor_backend is not None:
        with phases.run("monitor_start"):
            try:
                monitor = monitor_backend.start(attempt_dir / MONITOR_DIR, attempt_dir.name)
            except MonitorStartError as exc:
                monitor_error = str(exc)
        if monitor_error is not None:
            if monitor_required:
                return finish(
                    process=None,
                    inputs=tuple(inputs),
                    failure_phase="monitor_start",
                    failure_reason=f"monitoring required but unavailable: {monitor_error}",
                )
            warnings.append(f"running without monitoring: {monitor_error}")

    interrupted: ExecutionInterrupted | None = None
    with phases.run("process_supervision", contains_timed_interval=True):
        try:
            process = run(
                plan.command,
                stdout_path=attempt_dir / STDOUT_LOG,
                stderr_path=attempt_dir / STDERR_LOG,
                timeout_s=timeout_s,
                cancel=cancel,
                monitor=monitor,
                thread_budget=budget,
                terminate_grace_s=terminate_grace_s,
            )
        except ExecutionInterrupted as exc:
            interrupted = exc
            process = exc.result
    if interrupted is not None:
        finish(process=process, inputs=tuple(inputs))
        raise interrupted
    if monitor_error is not None:
        process = dataclasses.replace(
            process, monitoring=unavailable_summary(f"monitor start failed: {monitor_error}")
        )

    with phases.run("output_check"):
        outputs = _check_outputs(plan)
    return finish(process=process, inputs=tuple(inputs), outputs=tuple(outputs))
