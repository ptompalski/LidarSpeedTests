"""Cross-platform process execution engine.

:func:`run_process` launches one adapter-provided :class:`CommandSpec` as a
fresh process (argv list, ``shell=False``, explicit ``cwd``), streams stdout and
stderr to files as raw bytes, enforces a timeout, honours cooperative
cancellation, and terminates the whole process tree when needed.

Timed interval (``execution_mode="fresh-process"``, ``timing_boundary="process"``)
    Starts immediately before :class:`subprocess.Popen` and ends when the OS
    reports the root process's exit. It therefore includes process startup
    (interpreter or library loading), input reading, computation and output
    writing done by the process. Writes still buffered in the OS page cache
    when the process exits are not included (the tool's own ``fsync`` calls are).
    Monitor startup, input verification and output validation happen before or
    after it (see :mod:`lidarbench.execution.attempt`).

Status safety
    :attr:`ProcessResult.execution_status` is derived, never stored: it is
    ``succeeded`` only when the root exited by itself with code 0, before any
    timeout or cancellation, and the engine raised no error. Nothing is retried.
"""

from __future__ import annotations

import contextlib
import math
import os
import signal
import subprocess
import sys
import threading
import time
import traceback
from collections.abc import Callable, Iterator, Mapping
from dataclasses import dataclass, field
from enum import StrEnum
from pathlib import Path
from typing import Any, TypeVar

import psutil

from lidarbench.core.clock import Instant, now
from lidarbench.execution.adapters.base import THREAD_CAP_VARIABLES, CommandSpec
from lidarbench.execution.guardian import Guardian, shared_guardian
from lidarbench.execution.plan import validate_command_spec
from lidarbench.execution.process_tree import (
    LaunchSetupError,
    ReapResult,
    TerminationRecord,
    TreeController,
    descendants_snapshot,
    make_tree_controller,
)
from lidarbench.monitoring.base import MonitorHandle, MonitorReport
from lidarbench.monitoring.summary import (
    disabled_summary,
    summarize_monitoring,
    unavailable_summary,
)

_T = TypeVar("_T")

PROCESS_RESULT_SCHEMA = "lidarbench.process-execution/1"
EXECUTION_MODE_FRESH_PROCESS = "fresh-process"
TIMING_BOUNDARY_PROCESS = "process"

RECORDED_ENV_VARIABLES = (
    *THREAD_CAP_VARIABLES,
    "OMP_THREAD_LIMIT",
    "OMP_DYNAMIC",
    "OMP_PROC_BIND",
    "OMP_PLACES",
    "OMP_MAX_ACTIVE_LEVELS",
    "BLIS_NUM_THREADS",
    "GOTO_NUM_THREADS",
    "RCPP_PARALLEL_NUM_THREADS",
    "R_DATATABLE_NUM_THREADS",
    "NUMBA_NUM_THREADS",
    "GDAL_NUM_THREADS",
    "PYTHONHASHSEED",
    "PYTHONUTF8",
    "LANG",
    "LC_ALL",
)
"""Variables whose effective child value is recorded. The full environment is
never recorded (it may hold credentials); every adapter delta key is."""


class ProcessOutcome(StrEnum):
    """What happened to the process, as observed by the engine."""

    EXITED = "exited"
    """The root process exited by itself (any exit code)."""
    TIMED_OUT = "timed_out"
    CANCELLED = "cancelled"
    LAUNCH_FAILED = "launch_failed"
    """The OS refused to start the process (missing executable, bad cwd, ...)."""
    ENGINE_ERROR = "engine_error"
    """The engine failed while supervising; the tree was terminated."""


class ExecutionStatus(StrEnum):
    """Execution verdict (ARCHITECTURE.md §8.1, launched attempts only)."""

    SUCCEEDED = "succeeded"
    FAILED = "failed"
    TIMED_OUT = "timed_out"
    INTERRUPTED = "interrupted"


class CancellationToken:
    """Thread-safe cooperative cancellation for one or more executions."""

    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._event = threading.Event()
        self._reason: str | None = None
        self._at: Instant | None = None
        self._listeners: list[threading.Event] = []

    @property
    def cancelled(self) -> bool:
        """Whether cancellation was requested."""
        return self._event.is_set()

    @property
    def reason(self) -> str | None:
        """Reason given to :meth:`cancel`."""
        return self._reason

    @property
    def requested_at(self) -> Instant | None:
        """When cancellation was first requested."""
        return self._at

    def cancel(self, reason: str = "cancelled") -> None:
        """Request cancellation (idempotent; the first reason wins).

        Safe to call from signal handlers and other threads.
        """
        with self._lock:
            if not self._event.is_set():
                self._at = now()
                self._reason = reason
                self._event.set()
            listeners = list(self._listeners)
        for listener in listeners:
            listener.set()

    def wait(self, timeout_s: float | None = None) -> bool:
        """Block until cancelled or ``timeout_s`` elapses; return :attr:`cancelled`."""
        return self._event.wait(timeout_s)

    def _subscribe(self, event: threading.Event) -> None:
        with self._lock:
            self._listeners.append(event)
        if self.cancelled:
            event.set()

    def _unsubscribe(self, event: threading.Event) -> None:
        with self._lock:
            with contextlib.suppress(ValueError):
                self._listeners.remove(event)


@contextlib.contextmanager
def cancel_on_signals(token: CancellationToken) -> Iterator[CancellationToken]:
    """Turn Ctrl+C / termination signals into ``token.cancel()`` (main thread only).

    POSIX: ``SIGINT``, ``SIGTERM``, ``SIGHUP``. Windows: ``SIGINT``, ``SIGBREAK``.
    The previous handlers are restored on exit.
    """
    names = ("SIGINT", "SIGBREAK") if sys.platform == "win32" else ("SIGINT", "SIGTERM", "SIGHUP")
    previous: dict[signal.Signals, Any] = {}

    def handler(signum: int, _frame: Any) -> None:
        token.cancel(f"received {signal.Signals(signum).name}")

    try:
        for name in names:
            sig = getattr(signal, name, None)
            if sig is not None:
                previous[sig] = signal.signal(sig, handler)
        yield token
    finally:
        for sig, old in previous.items():
            signal.signal(sig, old)


@dataclass(frozen=True)
class ThreadBudget:
    """The configured thread budget of an attempt and, optionally, its enforcement.

    ``cpu_set`` lists the logical CPUs the whole process tree is pinned to (the
    budget is then enforced by the OS); ``None`` means the budget is only
    requested through the tool's options and thread environment variables.
    """

    requested_threads: int
    threading_model: str | None = None
    cpu_set: tuple[int, ...] | None = None
    cpu_set_detail: str = ""

    def __post_init__(self) -> None:
        if self.requested_threads < 1:
            raise ValueError("requested_threads must be >= 1")
        if self.cpu_set is not None and (
            not self.cpu_set or len(set(self.cpu_set)) != len(self.cpu_set)
        ):
            raise ValueError("cpu_set must list distinct CPUs")


@dataclass(frozen=True)
class ExceptionRecord:
    """An exception raised while launching or supervising a process."""

    phase: str
    type: str
    message: str
    traceback: str = ""

    @classmethod
    def from_exception(cls, phase: str, exc: BaseException) -> ExceptionRecord:
        """Capture an exception with its traceback."""
        text = "".join(traceback.format_exception(type(exc), exc, exc.__traceback__))
        return cls(phase, type(exc).__name__, str(exc), text)

    def to_record(self) -> dict[str, str]:
        """JSON-ready representation."""
        return {
            "phase": self.phase,
            "type": self.type,
            "message": self.message,
            "traceback": self.traceback,
        }


def _affinity_count() -> int | None:
    try:
        return len(getattr(psutil.Process(), "cpu_affinity")())  # noqa: B009 - not on macOS
    except (AttributeError, psutil.Error, OSError):
        return None


def environment_record(
    delta: Mapping[str, str], child_env: Mapping[str, str], budget: ThreadBudget | None
) -> dict[str, Any]:
    """Record the thread budget and relevant child environment variables."""
    variables = {}
    for name in sorted({*RECORDED_ENV_VARIABLES, *delta}):
        value = child_env.get(name)
        source = "delta" if name in delta else ("inherited" if value is not None else "unset")
        variables[name] = {"value": value, "source": source}
    return {
        "thread_budget": {
            "requested_threads": budget.requested_threads if budget else None,
            "threading_model": budget.threading_model if budget else None,
            "cpu_set": list(budget.cpu_set) if budget and budget.cpu_set else None,
            "cpu_set_detail": budget.cpu_set_detail if budget else "",
            "semantics": "configured request; enforced by CPU affinity when cpu_set is given "
            "(see cpu_affinity); measured CPU use is in monitoring and accounting",
        },
        "host": {
            "logical_cpus": psutil.cpu_count(logical=True),
            "physical_cpus": psutil.cpu_count(logical=False),
            "orchestrator_affinity_cpus": _affinity_count(),
        },
        "env_delta": dict(sorted(delta.items())),
        "variables": variables,
    }


@dataclass(frozen=True)
class ProcessResult:
    """Immutable record of one process execution."""

    outcome: ProcessOutcome
    argv: tuple[str, ...]
    cwd: Path
    stdout_path: Path
    stderr_path: Path
    environment: Mapping[str, Any]
    timeout_s: float | None = None
    pid: int | None = None
    exit_code: int | None = None
    start: Instant | None = None
    end: Instant | None = None
    cancel_reason: str | None = None
    stdout_bytes: int | None = None
    stderr_bytes: int | None = None
    exceptions: tuple[ExceptionRecord, ...] = ()
    warnings: tuple[str, ...] = ()
    termination: TerminationRecord | None = None
    leftover_cleanup: TerminationRecord | None = None
    process_group: Mapping[str, Any] = field(default_factory=dict)
    accounting: Mapping[str, Any] | None = None
    monitoring: Mapping[str, Any] = field(default_factory=dict)
    cpu_affinity: Mapping[str, Any] = field(default_factory=dict)
    execution_mode: str = EXECUTION_MODE_FRESH_PROCESS
    timing_boundary: str = TIMING_BOUNDARY_PROCESS

    @property
    def launched(self) -> bool:
        """Whether an OS process was created."""
        return self.pid is not None

    @property
    def elapsed_ns(self) -> int | None:
        """Monotonic wall-clock duration of the timed interval."""
        if self.start is None or self.end is None:
            return None
        return self.end.perf_ns - self.start.perf_ns

    @property
    def elapsed_s(self) -> float | None:
        """:attr:`elapsed_ns` in seconds."""
        ns = self.elapsed_ns
        return None if ns is None else ns / 1e9

    @property
    def termination_signal(self) -> str | None:
        """POSIX signal name that killed the root process, if any."""
        if self.exit_code is None or self.exit_code >= 0 or sys.platform == "win32":
            return None
        try:
            return signal.Signals(-self.exit_code).name
        except ValueError:
            return f"signal {-self.exit_code}"

    @property
    def execution_status(self) -> ExecutionStatus:
        """Verdict derived from the outcome; ``succeeded`` requires a clean exit 0."""
        if self.outcome is ProcessOutcome.TIMED_OUT:
            return ExecutionStatus.TIMED_OUT
        if self.outcome is ProcessOutcome.CANCELLED:
            return ExecutionStatus.INTERRUPTED
        if (
            self.outcome is ProcessOutcome.EXITED
            and self.exit_code == 0
            and not self.exceptions
            and self.end is not None
        ):
            return ExecutionStatus.SUCCEEDED
        return ExecutionStatus.FAILED

    @property
    def succeeded(self) -> bool:
        """Shorthand for ``execution_status is SUCCEEDED``."""
        return self.execution_status is ExecutionStatus.SUCCEEDED

    @property
    def cleanup_complete(self) -> bool:
        """Whether no process of the tree is known to have survived cleanup."""
        return all(r is None or r.complete for r in (self.termination, self.leftover_cleanup))

    def stdout_text(self) -> str:
        """Captured stdout decoded as UTF-8 (invalid bytes replaced)."""
        return self.stdout_path.read_bytes().decode("utf-8", errors="replace")

    def stderr_text(self) -> str:
        """Captured stderr decoded as UTF-8 (invalid bytes replaced)."""
        return self.stderr_path.read_bytes().decode("utf-8", errors="replace")

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation (paths as strings, instants with both clocks)."""
        return {
            "schema": PROCESS_RESULT_SCHEMA,
            "execution_status": str(self.execution_status),
            "outcome": str(self.outcome),
            "execution_mode": self.execution_mode,
            "timing_boundary": self.timing_boundary,
            "argv": list(self.argv),
            "cwd": os.fspath(self.cwd),
            "launched": self.launched,
            "pid": self.pid,
            "exit_code": self.exit_code,
            "exit_code_hex": (
                f"{self.exit_code & 0xFFFFFFFF:#010x}"
                if self.exit_code is not None and sys.platform == "win32"
                else None
            ),
            "termination_signal": self.termination_signal,
            "timeout_s": self.timeout_s,
            "cancel_reason": self.cancel_reason,
            "start": self.start.to_record() if self.start else None,
            "end": self.end.to_record() if self.end else None,
            "elapsed_ns": self.elapsed_ns,
            "elapsed_s": self.elapsed_s,
            "stdout": {"path": os.fspath(self.stdout_path), "bytes": self.stdout_bytes},
            "stderr": {"path": os.fspath(self.stderr_path), "bytes": self.stderr_bytes},
            "exceptions": [e.to_record() for e in self.exceptions],
            "warnings": list(self.warnings),
            "termination": self.termination.to_record() if self.termination else None,
            "leftover_cleanup": (
                self.leftover_cleanup.to_record() if self.leftover_cleanup else None
            ),
            "cleanup_complete": self.cleanup_complete,
            "process_group": dict(self.process_group),
            "process_accounting": dict(self.accounting) if self.accounting else None,
            "cpu_affinity": dict(self.cpu_affinity),
            "environment": dict(self.environment),
            "monitoring": dict(self.monitoring),
        }


class ExecutionInterrupted(KeyboardInterrupt):
    """``KeyboardInterrupt`` raised after the tree was cleaned up; carries the result."""

    def __init__(self, result: ProcessResult) -> None:
        super().__init__("execution interrupted")
        self.result = result


@dataclass
class _Run:
    """Mutable state of one execution, frozen into a :class:`ProcessResult`."""

    outcome: ProcessOutcome | None = None
    pid: int | None = None
    start: Instant | None = None
    reap: ReapResult | None = None
    stop_reason: str | None = None
    stop_at: Instant | None = None
    cancel_reason: str | None = None
    exceptions: list[ExceptionRecord] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    termination: TerminationRecord | None = None
    leftover_cleanup: TerminationRecord | None = None
    interrupted: BaseException | None = None


def _size(path: Path) -> int | None:
    try:
        return path.stat().st_size
    except OSError:
        return None


def run_process(
    command: CommandSpec,
    *,
    stdout_path: Path,
    stderr_path: Path,
    timeout_s: float | None = None,
    cancel: CancellationToken | None = None,
    monitor: MonitorHandle | None = None,
    thread_budget: ThreadBudget | None = None,
    terminate_grace_s: float = 5.0,
    descendant_poll_s: float | None = 0.5,
    controller_factory: Callable[[], TreeController] = make_tree_controller,
) -> ProcessResult:
    """Run one command as a fresh process and return an immutable result.

    Args:
        command: Adapter-provided process description (validated here again).
        stdout_path: New file for raw stdout bytes (exclusive create).
        stderr_path: New file for raw stderr bytes (exclusive create).
        timeout_s: Wall-clock limit for the timed interval, or ``None``.
        cancel: Cooperative cancellation token.
        monitor: A monitor already started (outside the interval), or ``None``
            if monitoring is disabled. It is attached after launch and stopped
            after the process tree is gone.
        thread_budget: The configured thread budget, recorded verbatim.
        terminate_grace_s: POSIX grace between ``SIGTERM`` and ``SIGKILL``.
        descendant_poll_s: How often the engine snapshots descendants so that
            escaped descendants can still be cleaned up; ``None`` disables it.
        controller_factory: Tree-control strategy (tests only).

    Returns:
        The result. Launch failures, timeouts and engine errors are recorded in
        it, not raised.

    Raises:
        ValueError: Invalid arguments.
        FileExistsError: A log file already exists (records are write-once).
        AdapterContractError: The command violates the adapter contract.
        ExecutionInterrupted: ``KeyboardInterrupt`` arrived; the tree was
            terminated and the result is attached to the exception.
    """
    try:
        validate_command_spec(command)
        if timeout_s is not None and not (math.isfinite(timeout_s) and timeout_s > 0):
            raise ValueError("timeout_s must be a positive finite number or None")
        if terminate_grace_s < 0:
            raise ValueError("terminate_grace_s must be >= 0")
        if descendant_poll_s is not None and descendant_poll_s <= 0:
            raise ValueError("descendant_poll_s must be positive or None")
        for path in (stdout_path, stderr_path):
            if Path(path).exists():
                raise FileExistsError(f"refusing to overwrite log file {path}")
    except BaseException:
        if monitor is not None:
            monitor.stop()
        raise

    child_env = {**os.environ, **command.env}
    environment = environment_record(command.env, child_env, thread_budget)
    state = _Run()
    controller = controller_factory()
    if thread_budget is not None and thread_budget.cpu_set:
        controller.request_affinity(thread_budget.cpu_set)
    state.warnings.extend(controller.warnings)
    accounting: Mapping[str, Any] | None = None
    try:
        with open(stdout_path, "xb") as out, open(stderr_path, "xb") as err:
            _supervise(
                command,
                child_env,
                out,
                err,
                state,
                controller,
                timeout_s,
                cancel,
                monitor,
                terminate_grace_s,
                descendant_poll_s,
            )
        accounting = _shielded(
            state,
            lambda: (state.reap.accounting if state.reap else None) or controller.accounting(),
        )
        state.warnings.extend(w for w in controller.warnings if w not in state.warnings)
    finally:
        controller.close()

    monitoring = _shielded(state, lambda: _finish_monitoring(monitor, state))
    end = state.reap.end if state.reap else None
    assert state.outcome is not None
    result = ProcessResult(
        outcome=state.outcome,
        argv=tuple(command.argv),
        cwd=Path(command.cwd),
        stdout_path=Path(stdout_path),
        stderr_path=Path(stderr_path),
        environment=environment,
        timeout_s=timeout_s,
        pid=state.pid,
        exit_code=state.reap.returncode if state.reap else None,
        start=state.start,
        end=end,
        cancel_reason=state.cancel_reason,
        stdout_bytes=_size(Path(stdout_path)),
        stderr_bytes=_size(Path(stderr_path)),
        exceptions=tuple(state.exceptions),
        warnings=tuple(state.warnings),
        termination=state.termination,
        leftover_cleanup=state.leftover_cleanup,
        process_group=controller.details(),
        accounting=accounting,
        monitoring=monitoring,
        cpu_affinity=dict(controller.affinity),
    )
    if state.interrupted is not None:
        raise ExecutionInterrupted(result) from state.interrupted
    return result


def _shielded(state: _Run, action: Callable[[], _T]) -> _T:
    """Run a cleanup step to completion, deferring ``KeyboardInterrupt``.

    Repeated Ctrl+C must not abandon a process tree, a sampler or the record;
    the first interrupt is re-raised as :class:`ExecutionInterrupted` later.
    """
    while True:
        try:
            return action()
        except KeyboardInterrupt as exc:
            if state.interrupted is None:
                state.interrupted = exc


def _finish_monitoring(monitor: MonitorHandle | None, state: _Run) -> dict[str, Any]:
    if monitor is None:
        return disabled_summary()
    try:
        report: MonitorReport = monitor.stop()
    except Exception as exc:
        return unavailable_summary(f"monitor stop failed: {type(exc).__name__}: {exc}")
    if state.start is None or state.reap is None:
        summary = unavailable_summary("no timed interval (process not launched or not reaped)")
        summary["backend"] = report.to_record()
        return summary
    try:
        return summarize_monitoring(report, state.start, state.reap.end)
    except Exception as exc:
        summary = unavailable_summary(f"summary failed: {type(exc).__name__}: {exc}")
        summary["backend"] = report.to_record()
        return summary


def _supervise(
    command: CommandSpec,
    child_env: dict[str, str],
    out: Any,
    err: Any,
    state: _Run,
    controller: TreeController,
    timeout_s: float | None,
    cancel: CancellationToken | None,
    monitor: MonitorHandle | None,
    grace_s: float,
    poll_s: float | None,
) -> None:
    if cancel is not None and cancel.cancelled:
        state.outcome = ProcessOutcome.CANCELLED
        state.cancel_reason = cancel.reason
        state.warnings.append("cancelled before launch; no process was started")
        return

    guardian = _guardian_for(controller, state)
    state.start = now()
    try:
        proc = subprocess.Popen(
            list(command.argv),
            cwd=os.fspath(command.cwd),
            env=child_env,
            stdin=subprocess.DEVNULL,
            stdout=out,
            stderr=err,
            shell=False,
            close_fds=True,
            **controller.popen_kwargs(),
        )
    except (OSError, ValueError) as exc:
        state.outcome = ProcessOutcome.LAUNCH_FAILED
        state.exceptions.append(ExceptionRecord.from_exception("launch", exc))
        return

    state.pid = proc.pid
    wake = threading.Event()
    reaped = threading.Event()

    def reaper() -> None:
        try:
            state.reap = controller.reap(proc)
        except BaseException as exc:
            state.exceptions.append(ExceptionRecord.from_exception("reap", exc))
        finally:
            reaped.set()
            wake.set()

    if cancel is not None:
        cancel._subscribe(wake)
    reaper_thread: threading.Thread | None = None
    try:
        try:
            controller.after_launch(proc)
        except LaunchSetupError as exc:
            state.exceptions.append(ExceptionRecord.from_exception("tree-setup", exc))
            state.stop_reason = "tree control setup failed"
        if guardian is not None:
            _guard(state, lambda: guardian.watch(proc.pid))
        reaper_thread = threading.Thread(target=reaper, name="lidarbench-reaper", daemon=True)
        reaper_thread.start()
        controller.verify_affinity(proc.pid)
        if monitor is not None and state.stop_reason is None:
            monitor.attach(proc.pid, state.start)
        if state.stop_reason is None:
            _wait(state, controller, proc.pid, wake, reaped, timeout_s, cancel, poll_s)
    except BaseException as exc:
        if isinstance(exc, KeyboardInterrupt):
            state.interrupted = exc
            state.stop_reason = "KeyboardInterrupt"
            state.cancel_reason = "KeyboardInterrupt"
            state.stop_at = now()
        else:
            state.exceptions.append(ExceptionRecord.from_exception("supervise", exc))
            state.stop_reason = f"engine error: {type(exc).__name__}"
    finally:
        if cancel is not None:
            cancel._unsubscribe(wake)
        if reaper_thread is None:
            reaper_thread = threading.Thread(target=reaper, name="lidarbench-reaper", daemon=True)
            reaper_thread.start()
        _stop_tree(state, controller, proc, grace_s, reaped, reaper_thread)
    state.outcome = _classify(state, timeout_s)
    if state.outcome is ProcessOutcome.EXITED or state.termination is None:
        _shielded(state, lambda: _clean_leftovers(state, controller, reaped))
    if guardian is not None:
        _guard(state, lambda: guardian.release(proc.pid))


def _guardian_for(controller: TreeController, state: _Run) -> Guardian | None:
    """The supervisor-death guardian for POSIX session trees (``None`` elsewhere)."""
    if controller.strategy != "posix-session":
        return None
    try:
        return shared_guardian()
    except OSError as exc:
        state.warnings.append(f"supervisor-death guardian unavailable: {exc}")
        return None


def _guard(state: _Run, action: Callable[[], None]) -> None:
    try:
        action()
    except OSError as exc:
        state.warnings.append(f"supervisor-death guardian failed: {exc}")


def _stop_tree(
    state: _Run,
    controller: TreeController,
    proc: subprocess.Popen[bytes],
    grace_s: float,
    reaped: threading.Event,
    reaper_thread: threading.Thread,
) -> None:
    """Terminate (if requested) and reap; a repeated Ctrl+C escalates to an immediate kill."""
    while True:
        try:
            if state.stop_reason is not None and not reaped.is_set() and not state.termination:
                state.termination = _terminate(controller, proc, state.stop_reason, grace_s, reaped)
            reaper_thread.join(timeout=30.0)
            break
        except KeyboardInterrupt as exc:
            if state.interrupted is None:
                state.interrupted = exc
            if not reaped.is_set():
                if state.stop_reason is None:
                    state.stop_reason = "KeyboardInterrupt"
                    state.cancel_reason = "KeyboardInterrupt"
                    state.stop_at = now()
                state.termination = None
                grace_s = 0.0
    if not reaped.is_set():
        state.exceptions.append(
            ExceptionRecord("reap", "TimeoutError", "root process was not reaped")
        )


def _clean_leftovers(state: _Run, controller: TreeController, reaped: threading.Event) -> None:
    leftovers = controller.leftovers() if reaped.is_set() else []
    if leftovers:
        names = ", ".join(f"{p.pid}" for p in leftovers)
        state.warnings.append(
            f"{len(leftovers)} descendant process(es) outlived the root and were killed: {names}"
        )
        state.leftover_cleanup = controller.kill_leftovers(leftovers)


def _terminate(
    controller: TreeController,
    proc: subprocess.Popen[bytes],
    reason: str,
    grace_s: float,
    reaped: threading.Event,
) -> TerminationRecord:
    try:
        return controller.terminate(proc, reason, grace_s, reaped)
    except Exception as exc:
        record = TerminationRecord(reason, controller.strategy)
        record.errors.append(f"{type(exc).__name__}: {exc}")
        with contextlib.suppress(OSError):
            proc.kill()
        return record


def _wait(
    state: _Run,
    controller: TreeController,
    pid: int,
    wake: threading.Event,
    reaped: threading.Event,
    timeout_s: float | None,
    cancel: CancellationToken | None,
    poll_s: float | None,
) -> None:
    assert state.start is not None
    deadline = state.start.perf_ns + int(timeout_s * 1e9) if timeout_s is not None else None
    while not reaped.is_set():
        if cancel is not None and cancel.cancelled:
            state.stop_reason = "cancelled"
            state.cancel_reason = cancel.reason
            state.stop_at = cancel.requested_at
            return
        current = time.perf_counter_ns()
        if deadline is not None and current >= deadline:
            state.stop_reason = "timeout"
            state.stop_at = now()
            return
        waits = [poll_s] if poll_s is not None else []
        if deadline is not None:
            waits.append((deadline - current) / 1e9)
        wake.wait(min(waits) if waits else None)
        wake.clear()
        if poll_s is not None and not reaped.is_set():
            controller.track(descendants_snapshot(pid))


def _classify(state: _Run, timeout_s: float | None) -> ProcessOutcome:
    if any(e.phase in ("reap", "supervise", "tree-setup") for e in state.exceptions):
        return ProcessOutcome.ENGINE_ERROR
    if state.stop_reason is None:
        return ProcessOutcome.EXITED
    assert state.reap is not None or state.termination is not None
    end = state.reap.end if state.reap else None
    # The process may have exited by itself just before the stop decision.
    if end is not None and state.stop_at is not None and end.perf_ns <= state.stop_at.perf_ns:
        if state.stop_reason == "timeout":
            assert state.start is not None and timeout_s is not None
            if end.perf_ns - state.start.perf_ns <= int(timeout_s * 1e9):
                return ProcessOutcome.EXITED
        elif state.interrupted is None:
            return ProcessOutcome.EXITED
    if state.stop_reason == "timeout":
        return ProcessOutcome.TIMED_OUT
    return ProcessOutcome.CANCELLED
