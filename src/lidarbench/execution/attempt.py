"""Execute one planned attempt with untimed preparation and checks around the timed run.

Phase order (each phase records its own monotonic/wall instants)::

    input_verification  untimed  inputs exist and are regular files; sizes recorded
    config_files        untimed  adapter config files written into work_dir
    monitor_start       untimed  sampler process started and primed
    process_supervision contains the timed interval (``process.start``..``process.end``:
                        fresh process startup, input reading, compute, output writing)
                        plus untimed tree cleanup and monitor stop
    interpretation      untimed  adapter reads tool-specific results (only with an adapter)
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
from lidarbench.execution.adapters.base import (
    AcceptedException,
    CommandSpec,
    Interpretation,
    Invocation,
    ToolAdapter,
    ToolTiming,
)
from lidarbench.execution.plan import InvocationPlan, write_config_files
from lidarbench.execution.process import (
    CancellationToken,
    ExecutionInterrupted,
    ExecutionStatus,
    ProcessOutcome,
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
    interpretation: dict[str, Any] | None = None
    """Adapter interpretation record (``None`` when no adapter interpreted the run)."""
    accepted_exception: dict[str, Any] | None = None
    """Declared exception applied to this run (raw exit status kept in ``process``)."""

    @property
    def tool_timing(self) -> dict[str, Any] | None:
        """Tool-reported inner interval with startup/shutdown overhead, if any."""
        return (self.interpretation or {}).get("tool_timing")

    @property
    def execution_status(self) -> ExecutionStatus:
        """``succeeded`` only if the process itself succeeded and no phase failed.

        The one exception is a declared :class:`AcceptedException` whose exit
        status equals the raw exit status of a process that exited by itself.
        """
        if self.process is None:
            return ExecutionStatus.FAILED
        if self.failure_phase is not None and self.process.succeeded:
            return ExecutionStatus.FAILED
        if (
            self.failure_phase is None
            and self.accepted_exception is not None
            and _exit_matches(self.process, self.accepted_exception.get("exit_code"))
        ):
            return ExecutionStatus.SUCCEEDED
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
            "interpretation": self.interpretation,
            "accepted_exception": self.accepted_exception,
            "process": self.process.to_record() if self.process else None,
        }


def _exit_matches(process: ProcessResult, exit_code: Any) -> bool:
    """Whether a process exited by itself, cleanly supervised, with ``exit_code``."""
    return (
        process.outcome is ProcessOutcome.EXITED
        and isinstance(exit_code, int)
        and not isinstance(exit_code, bool)
        and exit_code >= 0
        and process.exit_code == exit_code
        and not process.exceptions
        and process.end is not None
    )


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


def _timing_record(timing: ToolTiming, process: ProcessResult) -> dict[str, Any]:
    record: dict[str, Any] = {
        "clock": timing.clock,
        "boundary": timing.boundary,
        "start_wall_ns": timing.start_wall_ns,
        "end_wall_ns": timing.end_wall_ns,
        "workload_elapsed_ns": timing.end_wall_ns - timing.start_wall_ns,
        "marks": dict(timing.marks),
        "outer_timing_boundary": process.timing_boundary,
        "outer_elapsed_ns": process.elapsed_ns,
        "startup_overhead_ns": None,
        "shutdown_overhead_ns": None,
        "within_outer_interval": None,
        "note": (
            "Inner interval reported by the tool's own wall clock. The benchmark's timed "
            "interval is the outer process interval (fresh-process mode); startup overhead "
            "is the wall time from process launch to the tool's workload start (process "
            "creation, runtime and library start-up, input setup), shutdown overhead from "
            "workload end to process exit."
        ),
    }
    if process.start is not None and process.end is not None:
        startup = timing.start_wall_ns - process.start.wall_ns
        shutdown = process.end.wall_ns - timing.end_wall_ns
        record.update(
            startup_overhead_ns=startup,
            shutdown_overhead_ns=shutdown,
            within_outer_interval=startup >= 0 and shutdown >= 0,
        )
    return record


def _interpret(
    adapter: ToolAdapter, invocation: Invocation, process: ProcessResult
) -> tuple[dict[str, Any], str | None, list[str], AcceptedException | None]:
    """Run the adapter's interpretation.

    Returns (record, failure_reason, warnings, accepted exception). Any defect
    (exception, wrong type, non-JSON details) is a failure: an unreadable
    interpretation never lets an attempt count as succeeded.
    """
    try:
        result = adapter.interpret(invocation, process)
        if result is None:
            return {"adapter_id": adapter.adapter_id, "provided": False}, None, [], None
        if not isinstance(result, Interpretation):
            raise TypeError(f"interpret returned {type(result).__name__}, not Interpretation")
        accepted = result.accepted_exception
        if accepted is not None and not isinstance(accepted, AcceptedException):
            raise TypeError(f"accepted_exception is {type(accepted).__name__}")
        details = json.loads(json.dumps(dict(result.details), allow_nan=False))
        warnings = [str(w) for w in result.warnings]
        timing = _timing_record(result.tool_timing, process) if result.tool_timing else None
    except Exception as exc:
        reason = f"adapter interpretation failed: {type(exc).__name__}: {exc}"
        return (
            {"adapter_id": adapter.adapter_id, "provided": False, "error": reason},
            reason,
            [],
            None,
        )
    if timing is not None and timing["within_outer_interval"] is False:
        warnings.append(
            "tool-reported workload interval lies partly outside the outer process interval "
            "(clock adjustment or misreported times); startup/shutdown overhead is unreliable"
        )
    record = {
        "adapter_id": adapter.adapter_id,
        "provided": True,
        "failure_reason": result.failure_reason,
        "warnings": warnings,
        "tool_timing": timing,
        "details": details,
        "accepted_exception": accepted.to_record() if accepted else None,
    }
    return record, result.failure_reason, warnings, accepted


def _accept(
    accepted: AcceptedException, command: CommandSpec, process: ProcessResult
) -> tuple[dict[str, Any] | None, str | None]:
    """Check an accepted exception against the declaration and the raw process."""
    if accepted.name not in command.accepted_exceptions:
        return None, (
            f"adapter reported exception {accepted.name!r}, which the command did not declare "
            f"(declared: {list(command.accepted_exceptions)}); not accepted"
        )
    if not _exit_matches(process, accepted.exit_code):
        return None, (
            f"exception {accepted.name!r} covers exit status {accepted.exit_code}, but the "
            f"process {process.outcome} with status {process.exit_code}; not accepted"
        )
    return {**accepted.to_record(), "raw_exit_code": process.exit_code}, None


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
    adapter: ToolAdapter | None = None,
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
        adapter: The adapter that planned the attempt. When given, its
            :meth:`~ToolAdapter.interpret` runs (untimed) after a process that
            exited by itself; an objection fails the attempt (phase
            ``interpretation``), it can never turn a failure into a success.

    Raises:
        ValueError: The plan is not ready or does not match the invocation or adapter.
        FileExistsError: ``attempt_dir`` already holds an execution record or logs.
    """
    if not plan.ready or plan.command is None:
        raise ValueError(f"plan is not ready ({plan.status}): {plan.reason}")
    if plan.workload_id != invocation.workload_id:
        raise ValueError("plan and invocation describe different workloads")
    if adapter is not None and adapter.adapter_id != plan.adapter_id:
        raise ValueError("plan was made by a different adapter")
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

    interpretation: dict[str, Any] | None = None
    accepted_record: dict[str, Any] | None = None
    failure_phase: str | None = None
    failure_reason: str | None = None
    if adapter is not None and process.outcome is ProcessOutcome.EXITED:
        with phases.run("interpretation"):
            interpretation, objection, extra, accepted = _interpret(adapter, invocation, process)
            if objection is None and accepted is not None:
                accepted_record, objection = _accept(accepted, plan.command, process)
        warnings.extend(extra)
        if objection is not None:
            # A failed process keeps its own status; the reason explains it.
            failure_phase = "interpretation" if process.succeeded else "process"
            failure_reason = objection
        elif accepted_record is not None:
            warnings.append(
                f"accepted exception {accepted_record['name']} under "
                f"{accepted_record['policy']} (raw exit status {process.exit_code})"
            )

    with phases.run("output_check"):
        outputs = _check_outputs(plan)
    return finish(
        process=process,
        inputs=tuple(inputs),
        outputs=tuple(outputs),
        failure_phase=failure_phase,
        failure_reason=failure_reason,
        interpretation=interpretation,
        accepted_exception=accepted_record,
    )
