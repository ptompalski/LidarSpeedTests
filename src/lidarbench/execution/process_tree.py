"""Portable process-tree control: launch isolation, reaping, termination, accounting.

One :class:`TreeController` instance manages one benchmarked process and every
descendant it starts. The same interface is used on every operating system;
only the strategy differs:

``posix-session`` (Linux, macOS)
    The child starts in a new session (``start_new_session=True``), so it leads a
    new process group whose id equals its pid. Termination sends ``SIGTERM`` to
    the whole group, waits a grace period, then sends ``SIGKILL``. Descendants
    that left the group (``setsid``/``setpgid``, typical for daemons) are found
    with a psutil snapshot taken while the root is still alive and are signalled
    individually. The root is reaped with ``os.wait4`` so the kernel's resource
    usage (CPU time and peak RSS of the root plus every descendant that was
    waited for) is recorded.

``windows-job`` (Windows)
    The child is created suspended with ``CREATE_NEW_PROCESS_GROUP``, assigned
    to a Job Object and only then resumed, so no grandchild can start outside
    the job. Termination calls ``TerminateJobObject`` (Windows has no reliable
    graceful signal for non-console processes, so there is no grace phase).
    Job accounting covers every process that ever ran in the job, including
    short-lived ones.

``psutil-tree`` (fallback)
    Used on Windows when a Job Object cannot be created or assigned. Only
    descendants visible in a psutil snapshot can be terminated; children of
    already-exited intermediates are invisible (documented limitation).
"""

from __future__ import annotations

import os
import signal
import subprocess
import sys
import threading
import time
from abc import ABC, abstractmethod
from collections.abc import Iterable
from dataclasses import dataclass, field
from typing import Any

import psutil

from lidarbench.core.clock import Instant, now

_POLL_S = 0.02


@dataclass(frozen=True)
class ProcessInfo:
    """Identity of one OS process, for cleanup records."""

    pid: int
    name: str | None = None
    create_time: float | None = None

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {"pid": self.pid, "name": self.name, "create_time": self.create_time}


@dataclass
class TerminationRecord:
    """What the engine did to stop a process tree, and whether it worked."""

    reason: str
    strategy: str
    actions: list[str] = field(default_factory=list)
    survivors: list[ProcessInfo] = field(default_factory=list)
    errors: list[str] = field(default_factory=list)

    @property
    def complete(self) -> bool:
        """Whether no process of the tree is known to be alive afterwards."""
        return not self.survivors

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "reason": self.reason,
            "strategy": self.strategy,
            "actions": list(self.actions),
            "complete": self.complete,
            "survivors": [p.to_record() for p in self.survivors],
            "errors": list(self.errors),
        }


@dataclass(frozen=True)
class ReapResult:
    """Exit status of the root process plus optional kernel accounting."""

    returncode: int
    end: Instant
    """Captured immediately after the OS reported the exit."""
    accounting: dict[str, Any] | None = None


class LaunchSetupError(RuntimeError):
    """The process started but could not be placed under tree control."""


def _info(proc: psutil.Process) -> ProcessInfo:
    try:
        return ProcessInfo(proc.pid, proc.name(), proc.create_time())
    except psutil.Error:
        return ProcessInfo(proc.pid)


def _alive(proc: psutil.Process) -> bool:
    try:
        return proc.is_running() and proc.status() != psutil.STATUS_ZOMBIE
    except psutil.Error:
        return False


def descendants_snapshot(pid: int) -> list[psutil.Process]:
    """Live descendants of ``pid`` (empty if it has exited)."""
    try:
        return psutil.Process(pid).children(recursive=True)
    except psutil.Error:
        return []


def _kill_all(procs: Iterable[psutil.Process], record: TerminationRecord) -> None:
    for proc in procs:
        try:
            proc.kill()
            record.actions.append(f"kill pid {proc.pid}")
        except psutil.NoSuchProcess:
            pass
        except psutil.Error as exc:
            record.errors.append(f"kill pid {proc.pid}: {type(exc).__name__}: {exc}")


class TreeController(ABC):
    """Platform strategy for launching, reaping and terminating one process tree."""

    strategy: str

    def __init__(self) -> None:
        self.warnings: list[str] = []
        self.root_pid: int | None = None
        self._tracked: dict[tuple[int, float], psutil.Process] = {}

    def popen_kwargs(self) -> dict[str, Any]:
        """Extra keyword arguments for :class:`subprocess.Popen`."""
        return {}

    def after_launch(self, proc: subprocess.Popen[bytes]) -> None:
        """Place a freshly started process under control.

        Raises:
            LaunchSetupError: The process could not be controlled and must be killed.
        """
        self.root_pid = proc.pid

    @abstractmethod
    def reap(self, proc: subprocess.Popen[bytes]) -> ReapResult:
        """Block until the root exits; return its exit code and accounting."""

    def track(self, procs: Iterable[psutil.Process]) -> None:
        """Remember descendants so they can be terminated even if they escape."""
        for proc in procs:
            try:
                self._tracked.setdefault((proc.pid, proc.create_time()), proc)
            except psutil.Error:
                continue

    def tracked_alive(self) -> list[psutil.Process]:
        """Remembered descendants that are still running (pid reuse is checked)."""
        return [p for p in self._tracked.values() if _alive(p)]

    @abstractmethod
    def terminate(
        self, proc: subprocess.Popen[bytes], reason: str, grace_s: float, reaped: threading.Event
    ) -> TerminationRecord:
        """Stop the root and every descendant; ``reaped`` is set once the root is reaped."""

    @abstractmethod
    def leftovers(self) -> list[psutil.Process]:
        """Processes of the tree still alive after the root was reaped."""

    def kill_leftovers(self, procs: list[psutil.Process]) -> TerminationRecord:
        """Kill processes that outlived the root."""
        record = TerminationRecord("descendants outlived the root process", self.strategy)
        _kill_all(procs, record)
        psutil.wait_procs(procs, timeout=5)
        record.survivors = [_info(p) for p in procs if _alive(p)]
        return record

    def accounting(self) -> dict[str, Any] | None:
        """Whole-tree accounting gathered by the OS, if the strategy provides it."""
        return None

    def details(self) -> dict[str, Any]:
        """Strategy description for the execution record."""
        return {"strategy": self.strategy, "warnings": list(self.warnings)}

    def close(self) -> None:  # noqa: B027 - optional hook
        """Release OS resources."""


if sys.platform != "win32":

    class PosixSessionController(TreeController):
        """``start_new_session`` + ``killpg`` + ``wait4`` (Linux and macOS)."""

        strategy = "posix-session"

        def popen_kwargs(self) -> dict[str, Any]:
            return {"start_new_session": True}

        def reap(self, proc: subprocess.Popen[bytes]) -> ReapResult:
            _, status, usage = os.wait4(proc.pid, 0)
            end = now()
            returncode = os.waitstatus_to_exitcode(status)
            proc.returncode = returncode
            rss_scale = 1 if sys.platform == "darwin" else 1024
            return ReapResult(
                returncode,
                end,
                {
                    "source": "wait4",
                    "scope": "root process plus descendants it (recursively) waited for",
                    "cpu_user_seconds": usage.ru_utime,
                    "cpu_system_seconds": usage.ru_stime,
                    "max_rss_bytes": usage.ru_maxrss * rss_scale,
                    "max_rss_semantics": "largest resident set of any single included process",
                    "block_input_operations": usage.ru_inblock,
                    "block_output_operations": usage.ru_oublock,
                    "voluntary_context_switches": usage.ru_nvcsw,
                    "involuntary_context_switches": usage.ru_nivcsw,
                },
            )

        def _group_members(self) -> list[psutil.Process]:
            if self.root_pid is None:
                return []
            members = []
            for proc in psutil.process_iter():
                try:
                    if os.getpgid(proc.pid) == self.root_pid and _alive(proc):
                        members.append(proc)
                except (OSError, psutil.Error):
                    continue
            return members

        def _signal_group(self, sig: signal.Signals, record: TerminationRecord) -> None:
            assert self.root_pid is not None
            try:
                os.killpg(self.root_pid, sig)
                record.actions.append(f"killpg {self.root_pid} {sig.name}")
            except ProcessLookupError:
                pass
            except PermissionError:
                # macOS reports EPERM when the group holds only zombies.
                record.actions.append(f"killpg {self.root_pid} {sig.name}: EPERM (ignored)")

        def _wait_gone(self, reaped: threading.Event, timeout_s: float) -> list[psutil.Process]:
            deadline = time.monotonic() + timeout_s
            while True:
                remaining = self.leftovers()
                if reaped.is_set() and not remaining:
                    return []
                if time.monotonic() >= deadline:
                    return remaining
                time.sleep(_POLL_S)

        def terminate(
            self,
            proc: subprocess.Popen[bytes],
            reason: str,
            grace_s: float,
            reaped: threading.Event,
        ) -> TerminationRecord:
            record = TerminationRecord(reason, self.strategy)
            self.track(descendants_snapshot(proc.pid))
            if grace_s > 0:
                self._signal_group(signal.SIGTERM, record)
                for p in self.tracked_alive():
                    try:
                        p.terminate()
                    except psutil.Error:
                        pass
                if not self._wait_gone(reaped, grace_s):
                    return record
            self._signal_group(signal.SIGKILL, record)
            _kill_all(self.tracked_alive(), record)
            remaining = self._wait_gone(reaped, 10.0)
            record.survivors = [_info(p) for p in remaining]
            if not reaped.is_set():
                record.survivors.append(ProcessInfo(proc.pid, None, None))
                record.errors.append("root process was not reaped after SIGKILL")
            return record

        def leftovers(self) -> list[psutil.Process]:
            remaining = {p.pid: p for p in self._group_members()}
            for proc in self.tracked_alive():
                remaining.setdefault(proc.pid, proc)
            return list(remaining.values())

        def kill_leftovers(self, procs: list[psutil.Process]) -> TerminationRecord:
            record = TerminationRecord("descendants outlived the root process", self.strategy)
            self._signal_group(signal.SIGKILL, record)
            _kill_all(procs, record)
            psutil.wait_procs(procs, timeout=5)
            record.survivors = [_info(p) for p in self.leftovers()]
            return record


class PsutilTreeController(TreeController):
    """Fallback: terminate whatever a psutil snapshot can see."""

    strategy = "psutil-tree"

    def reap(self, proc: subprocess.Popen[bytes]) -> ReapResult:
        returncode = proc.wait()
        return ReapResult(returncode, now())

    def terminate(
        self, proc: subprocess.Popen[bytes], reason: str, grace_s: float, reaped: threading.Event
    ) -> TerminationRecord:
        record = TerminationRecord(reason, self.strategy)
        self.track(descendants_snapshot(proc.pid))
        try:
            proc.kill()
            record.actions.append(f"kill root pid {proc.pid}")
        except OSError as exc:
            record.errors.append(f"kill root: {exc}")
        _kill_all(self.tracked_alive(), record)
        reaped.wait(10.0)
        psutil.wait_procs(self.tracked_alive(), timeout=5)
        record.survivors = [_info(p) for p in self.tracked_alive()]
        if not reaped.is_set():
            record.survivors.append(ProcessInfo(proc.pid))
            record.errors.append("root process was not reaped after kill")
        return record

    def leftovers(self) -> list[psutil.Process]:
        return self.tracked_alive()


if sys.platform == "win32":
    import ctypes
    from ctypes import wintypes

    _kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)
    _ntdll = ctypes.WinDLL("ntdll")

    _CREATE_SUSPENDED = 0x00000004
    _JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE = 0x00002000
    _JOB_BASIC_ACCOUNTING = 1
    _JOB_BASIC_PROCESS_ID_LIST = 3
    _JOB_BASIC_AND_IO_ACCOUNTING = 8
    _JOB_EXTENDED_LIMIT = 9
    _PROCESS_RIGHTS = 0x0001 | 0x0100 | 0x0800 | 0x1000  # terminate, set quota, resume, query
    _ENGINE_EXIT_CODE = 1
    _MAX_JOB_PIDS = 4096

    class _IoCounters(ctypes.Structure):
        _fields_ = [
            ("ReadOperationCount", ctypes.c_ulonglong),
            ("WriteOperationCount", ctypes.c_ulonglong),
            ("OtherOperationCount", ctypes.c_ulonglong),
            ("ReadTransferCount", ctypes.c_ulonglong),
            ("WriteTransferCount", ctypes.c_ulonglong),
            ("OtherTransferCount", ctypes.c_ulonglong),
        ]

    class _BasicLimit(ctypes.Structure):
        _fields_ = [
            ("PerProcessUserTimeLimit", ctypes.c_longlong),
            ("PerJobUserTimeLimit", ctypes.c_longlong),
            ("LimitFlags", wintypes.DWORD),
            ("MinimumWorkingSetSize", ctypes.c_size_t),
            ("MaximumWorkingSetSize", ctypes.c_size_t),
            ("ActiveProcessLimit", wintypes.DWORD),
            ("Affinity", ctypes.c_size_t),
            ("PriorityClass", wintypes.DWORD),
            ("SchedulingClass", wintypes.DWORD),
        ]

    class _ExtendedLimit(ctypes.Structure):
        _fields_ = [
            ("BasicLimitInformation", _BasicLimit),
            ("IoInfo", _IoCounters),
            ("ProcessMemoryLimit", ctypes.c_size_t),
            ("JobMemoryLimit", ctypes.c_size_t),
            ("PeakProcessMemoryUsed", ctypes.c_size_t),
            ("PeakJobMemoryUsed", ctypes.c_size_t),
        ]

    class _BasicAccounting(ctypes.Structure):
        _fields_ = [
            ("TotalUserTime", ctypes.c_longlong),
            ("TotalKernelTime", ctypes.c_longlong),
            ("ThisPeriodTotalUserTime", ctypes.c_longlong),
            ("ThisPeriodTotalKernelTime", ctypes.c_longlong),
            ("TotalPageFaultCount", wintypes.DWORD),
            ("TotalProcesses", wintypes.DWORD),
            ("ActiveProcesses", wintypes.DWORD),
            ("TotalTerminatedProcesses", wintypes.DWORD),
        ]

    class _BasicAndIoAccounting(ctypes.Structure):
        _fields_ = [("BasicInfo", _BasicAccounting), ("IoInfo", _IoCounters)]

    class _ProcessIdList(ctypes.Structure):
        _fields_ = [
            ("NumberOfAssignedProcesses", wintypes.DWORD),
            ("NumberOfProcessIdsInList", wintypes.DWORD),
            ("ProcessIdList", ctypes.c_size_t * _MAX_JOB_PIDS),
        ]

    _kernel32.CreateJobObjectW.argtypes = [wintypes.LPVOID, wintypes.LPCWSTR]
    _kernel32.CreateJobObjectW.restype = wintypes.HANDLE
    _kernel32.SetInformationJobObject.argtypes = [
        wintypes.HANDLE,
        ctypes.c_int,
        wintypes.LPVOID,
        wintypes.DWORD,
    ]
    _kernel32.SetInformationJobObject.restype = wintypes.BOOL
    _kernel32.QueryInformationJobObject.argtypes = [
        wintypes.HANDLE,
        ctypes.c_int,
        wintypes.LPVOID,
        wintypes.DWORD,
        wintypes.LPDWORD,
    ]
    _kernel32.QueryInformationJobObject.restype = wintypes.BOOL
    _kernel32.AssignProcessToJobObject.argtypes = [wintypes.HANDLE, wintypes.HANDLE]
    _kernel32.AssignProcessToJobObject.restype = wintypes.BOOL
    _kernel32.TerminateJobObject.argtypes = [wintypes.HANDLE, wintypes.UINT]
    _kernel32.TerminateJobObject.restype = wintypes.BOOL
    _kernel32.OpenProcess.argtypes = [wintypes.DWORD, wintypes.BOOL, wintypes.DWORD]
    _kernel32.OpenProcess.restype = wintypes.HANDLE
    _kernel32.CloseHandle.argtypes = [wintypes.HANDLE]
    _kernel32.CloseHandle.restype = wintypes.BOOL
    _ntdll.NtResumeProcess.argtypes = [wintypes.HANDLE]
    _ntdll.NtResumeProcess.restype = ctypes.c_long

    def _win_error(what: str) -> OSError:
        code = ctypes.get_last_error()
        return OSError(code, f"{what} failed: {ctypes.FormatError(code)}")

    class WindowsJobController(TreeController):
        """Suspended launch + Job Object (``KILL_ON_JOB_CLOSE``) + resume."""

        strategy = "windows-job"

        def __init__(self) -> None:
            super().__init__()
            self._job = _kernel32.CreateJobObjectW(None, None)
            if not self._job:
                raise _win_error("CreateJobObjectW")
            limits = _ExtendedLimit()
            limits.BasicLimitInformation.LimitFlags = _JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE
            if not _kernel32.SetInformationJobObject(
                self._job, _JOB_EXTENDED_LIMIT, ctypes.byref(limits), ctypes.sizeof(limits)
            ):
                error = _win_error("SetInformationJobObject")
                _kernel32.CloseHandle(self._job)
                raise error
            self._assigned = False

        def popen_kwargs(self) -> dict[str, Any]:
            return {"creationflags": subprocess.CREATE_NEW_PROCESS_GROUP | _CREATE_SUSPENDED}

        def after_launch(self, proc: subprocess.Popen[bytes]) -> None:
            super().after_launch(proc)
            handle = _kernel32.OpenProcess(_PROCESS_RIGHTS, False, proc.pid)
            if not handle:
                raise LaunchSetupError(str(_win_error("OpenProcess")))
            try:
                if _kernel32.AssignProcessToJobObject(self._job, handle):
                    self._assigned = True
                else:
                    self.warnings.append(
                        f"{_win_error('AssignProcessToJobObject')}; "
                        "falling back to psutil tree termination"
                    )
                status = _ntdll.NtResumeProcess(handle)
                if status != 0:
                    raise LaunchSetupError(f"NtResumeProcess failed: NTSTATUS {status:#x}")
            finally:
                _kernel32.CloseHandle(handle)

        def reap(self, proc: subprocess.Popen[bytes]) -> ReapResult:
            returncode = proc.wait()
            return ReapResult(returncode, now())

        def _query(self, info_class: int, struct: ctypes.Structure) -> bool:
            return bool(
                _kernel32.QueryInformationJobObject(
                    self._job, info_class, ctypes.byref(struct), ctypes.sizeof(struct), None
                )
            )

        def _job_members(self) -> list[psutil.Process]:
            if not self._assigned:
                return []
            ids = _ProcessIdList()
            if not self._query(_JOB_BASIC_PROCESS_ID_LIST, ids):
                return []
            members = []
            for pid in ids.ProcessIdList[: ids.NumberOfProcessIdsInList]:
                try:
                    members.append(psutil.Process(int(pid)))
                except psutil.Error:
                    continue
            return members

        def _active(self) -> int:
            info = _BasicAccounting()
            return int(info.ActiveProcesses) if self._query(_JOB_BASIC_ACCOUNTING, info) else 0

        def terminate(
            self,
            proc: subprocess.Popen[bytes],
            reason: str,
            grace_s: float,
            reaped: threading.Event,
        ) -> TerminationRecord:
            record = TerminationRecord(reason, self.strategy)
            self.track(descendants_snapshot(proc.pid))
            if self._assigned:
                if _kernel32.TerminateJobObject(self._job, _ENGINE_EXIT_CODE):
                    record.actions.append("TerminateJobObject")
                else:
                    record.errors.append(str(_win_error("TerminateJobObject")))
            try:
                proc.kill()
            except OSError:
                pass
            _kill_all(self.tracked_alive(), record)
            reaped.wait(10.0)
            deadline = time.monotonic() + 10.0
            while self._active() and time.monotonic() < deadline:
                time.sleep(_POLL_S)
            remaining = {p.pid: p for p in self._job_members() if _alive(p)}
            for p in self.tracked_alive():
                remaining.setdefault(p.pid, p)
            record.survivors = [_info(p) for p in remaining.values()]
            if not reaped.is_set():
                record.survivors.append(ProcessInfo(proc.pid))
                record.errors.append("root process was not reaped after termination")
            return record

        def leftovers(self) -> list[psutil.Process]:
            remaining = {p.pid: p for p in self._job_members() if _alive(p)}
            for p in self.tracked_alive():
                remaining.setdefault(p.pid, p)
            return list(remaining.values())

        def kill_leftovers(self, procs: list[psutil.Process]) -> TerminationRecord:
            record = TerminationRecord("descendants outlived the root process", self.strategy)
            if self._assigned and _kernel32.TerminateJobObject(self._job, _ENGINE_EXIT_CODE):
                record.actions.append("TerminateJobObject")
            _kill_all(procs, record)
            psutil.wait_procs(procs, timeout=5)
            record.survivors = [_info(p) for p in self.leftovers()]
            return record

        def accounting(self) -> dict[str, Any] | None:
            if not self._assigned:
                return None
            info = _BasicAndIoAccounting()
            limits = _ExtendedLimit()
            if not (
                self._query(_JOB_BASIC_AND_IO_ACCOUNTING, info)
                and self._query(_JOB_EXTENDED_LIMIT, limits)
            ):
                return None
            basic = info.BasicInfo
            return {
                "source": "windows-job-object",
                "scope": "every process that ran in the job, including short-lived ones",
                "cpu_user_seconds": basic.TotalUserTime / 1e7,
                "cpu_system_seconds": basic.TotalKernelTime / 1e7,
                "total_processes": int(basic.TotalProcesses),
                "peak_job_memory_bytes": int(limits.PeakJobMemoryUsed),
                "peak_job_memory_semantics": "peak committed (private) memory of the whole job",
                "peak_process_memory_bytes": int(limits.PeakProcessMemoryUsed),
                "io_read_bytes": int(info.IoInfo.ReadTransferCount),
                "io_write_bytes": int(info.IoInfo.WriteTransferCount),
                "io_other_bytes": int(info.IoInfo.OtherTransferCount),
                "io_semantics": "all I/O operations (files, pipes, devices, network)",
            }

        def details(self) -> dict[str, Any]:
            return {
                "strategy": self.strategy if self._assigned else PsutilTreeController.strategy,
                "job_assigned": self._assigned,
                "warnings": list(self.warnings),
            }

        def close(self) -> None:
            if self._job:
                _kernel32.CloseHandle(self._job)
                self._job = None


def make_tree_controller() -> TreeController:
    """Return the best tree controller for this platform."""
    if sys.platform == "win32":
        try:
            return WindowsJobController()
        except OSError as exc:
            controller = PsutilTreeController()
            controller.warnings.append(f"Job Object unavailable ({exc}); using psutil-tree")
            return controller
    else:
        return PosixSessionController()
