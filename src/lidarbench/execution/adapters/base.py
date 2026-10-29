"""Language-independent tool adapter interface (``lidarbench.adapter/1``).

A tool adapter translates a tool-neutral workload invocation into a native
process description for one piece of processing software. The orchestrator
owns everything else. See ``design/ADAPTERS.md`` for the full contract.

Adapters answer seven questions:

1. Which tool and version is this? (:meth:`ToolAdapter.identify`)
2. Is it installed and usable on this platform? (:meth:`ToolAdapter.probe`)
3. Which workloads does it implement, and how? (:meth:`ToolAdapter.workload_support`)
4. Which executable and argument vector run a workload? (:meth:`ToolAdapter.build_command`)
5. Which environment variables and threading model apply? (:class:`CommandSpec`,
   :class:`ThreadingSupport`)
6. Which outputs are expected, plus tool-specific metadata?
   (:meth:`ToolAdapter.expected_outputs`, :meth:`ToolAdapter.invocation_metadata`)
7. Why is a tool or combination unavailable or unsupported? (:class:`Availability`,
   :class:`WorkloadSupport`, :class:`UnsupportedCombinationError`)

Adapters must not time processes, monitor resources, manage campaigns, launch
the benchmarked process, or compare results between tools. They only describe.
"""

from __future__ import annotations

import os
import re
from abc import ABC, abstractmethod
from collections.abc import Mapping
from dataclasses import dataclass, field
from enum import StrEnum
from pathlib import Path
from typing import TYPE_CHECKING, Any, ClassVar

from lidarbench.core.platform import PlatformInfo

if TYPE_CHECKING:
    from lidarbench.execution.process import ProcessResult

ADAPTER_SCHEMA = "lidarbench.adapter/1"

ADAPTER_ID_PATTERN = re.compile(r"^[a-z][a-z0-9_-]{0,63}$")
WORKLOAD_ID_PATTERN = re.compile(r"^[A-Za-z0-9][A-Za-z0-9_.-]{0,127}$")
EXCEPTION_NAME_PATTERN = re.compile(r"^[a-z][a-z0-9_]{0,127}$")

ParameterValue = str | int | float | bool
MetadataValue = str | int | float | bool | None


class UnavailableReason(StrEnum):
    """Machine-readable reason why a tool cannot run on this machine."""

    NOT_INSTALLED = "not_installed"
    PLATFORM_UNSUPPORTED = "platform_unsupported"
    VERSION_UNSUPPORTED = "version_unsupported"
    MISCONFIGURED = "misconfigured"
    PROBE_FAILED = "probe_failed"


class ImplementationStatus(StrEnum):
    """Status of one (workload, tool) implementation (ARCHITECTURE.md §8.2)."""

    RUNNABLE = "runnable"
    DIAGNOSTIC_ONLY = "diagnostic_only"
    UNSUPPORTED = "unsupported"
    BLOCKED = "blocked"

    @property
    def dispatchable(self) -> bool:
        """Whether an attempt with this status may launch a process."""
        return self in (ImplementationStatus.RUNNABLE, ImplementationStatus.DIAGNOSTIC_ONLY)


class ThreadingModel(StrEnum):
    """How a tool uses the requested thread count."""

    SINGLE_THREADED = "single_threaded"
    """The tool always uses one thread; only ``threads == 1`` is meaningful."""

    NATIVE_THREADS = "native_threads"
    """The tool's own flag or setting (for example ``-cores N``)."""

    WORKER_PROCESSES = "worker_processes"
    """The tool starts worker processes (for example R ``future`` multisession)."""

    CONCURRENT_FILES = "concurrent_files"
    """Files are processed concurrently, one thread each (for example lasR)."""


class ArtifactKind(StrEnum):
    """Broad kind of an expected output artifact."""

    RASTER = "raster"
    POINT_CLOUD = "point_cloud"
    VECTOR = "vector"
    TABLE = "table"
    LOG = "log"
    OTHER = "other"


class UnsupportedCombinationError(Exception):
    """Raised by :meth:`ToolAdapter.build_command` for an unsupported combination.

    The orchestrator records the attempt as ``skipped_unsupported`` with
    :attr:`reason`; it is never a benchmark failure.
    """

    def __init__(self, reason: str) -> None:
        super().__init__(reason)
        self.reason = reason


@dataclass(frozen=True)
class Availability:
    """Whether a tool is installed and usable on this machine."""

    available: bool
    reason: UnavailableReason | None = None
    detail: str = ""
    executables: Mapping[str, Path] = field(default_factory=dict)
    """Resolved absolute executables, keyed by role (for example ``"main"``)."""

    def __post_init__(self) -> None:
        if self.available and self.reason is not None:
            raise ValueError("an available tool cannot carry an unavailability reason")
        if not self.available and (self.reason is None or not self.detail):
            raise ValueError("an unavailable tool needs a reason and a detail message")

    @classmethod
    def ok(cls, executables: Mapping[str, Path] | None = None) -> Availability:
        """Build an ``available`` result."""
        return cls(True, executables=dict(executables or {}))

    @classmethod
    def unavailable(cls, reason: UnavailableReason, detail: str) -> Availability:
        """Build an ``unavailable`` result with an explicit reason."""
        return cls(False, reason=reason, detail=detail)


@dataclass(frozen=True)
class ToolIdentity:
    """Identity of an installed tool, recorded in campaign identity."""

    tool: str
    version: str | None
    executables: Mapping[str, Path] = field(default_factory=dict)
    details: Mapping[str, MetadataValue] = field(default_factory=dict)
    """Tool-specific identity facts (runtime version, packages, build flags)."""


@dataclass(frozen=True)
class ThreadingSupport:
    """Threading model and accepted thread counts for one workload."""

    model: ThreadingModel = ThreadingModel.SINGLE_THREADED
    min_threads: int = 1
    max_threads: int | None = 1
    """Upper bound, or ``None`` for no adapter-imposed limit."""

    def __post_init__(self) -> None:
        if self.min_threads < 1:
            raise ValueError("min_threads must be >= 1")
        if self.max_threads is not None and self.max_threads < self.min_threads:
            raise ValueError("max_threads must be >= min_threads")
        if self.model is ThreadingModel.SINGLE_THREADED and self.max_threads != 1:
            raise ValueError("single-threaded workloads must have max_threads == 1")

    def accepts(self, threads: int) -> bool:
        """Return whether ``threads`` is within the supported range."""
        if threads < self.min_threads:
            return False
        return self.max_threads is None or threads <= self.max_threads

    def describe_range(self) -> str:
        """Human-readable thread range, for example ``1-8`` or ``1+``."""
        upper = "+" if self.max_threads is None else f"-{self.max_threads}"
        if self.max_threads == self.min_threads:
            return str(self.min_threads)
        return f"{self.min_threads}{upper}"


@dataclass(frozen=True)
class WorkloadSupport:
    """Declared capability of an adapter for one workload ID."""

    workload_id: str
    status: ImplementationStatus
    reason: str = ""
    """Required for every status except ``runnable``."""
    threading: ThreadingSupport = field(default_factory=ThreadingSupport)

    def __post_init__(self) -> None:
        if not WORKLOAD_ID_PATTERN.match(self.workload_id):
            raise ValueError(f"invalid workload id: {self.workload_id!r}")
        if self.status is not ImplementationStatus.RUNNABLE and not self.reason:
            raise ValueError(f"{self.workload_id}: status {self.status} requires a reason")


@dataclass(frozen=True)
class Invocation:
    """Tool-neutral request built by the orchestrator for one attempt.

    All paths are absolute. ``output_dir`` and ``work_dir`` exist and are owned by
    the attempt; the adapter must not create or delete anything itself.
    """

    workload_id: str
    inputs: tuple[Path, ...]
    output_dir: Path
    work_dir: Path
    threads: int = 1
    parameters: Mapping[str, ParameterValue] = field(default_factory=dict)

    def __post_init__(self) -> None:
        if not WORKLOAD_ID_PATTERN.match(self.workload_id):
            raise ValueError(f"invalid workload id: {self.workload_id!r}")
        if not self.inputs:
            raise ValueError("an invocation needs at least one input")
        for path in (*self.inputs, self.output_dir, self.work_dir):
            if not Path(path).is_absolute():
                raise ValueError(f"invocation paths must be absolute: {path}")
        if self.threads < 1:
            raise ValueError("threads must be >= 1")

    def config_path(self, name: str) -> Path:
        """Absolute path where the orchestrator writes config file ``name``."""
        return self.work_dir / name


@dataclass(frozen=True)
class ConfigFile:
    """A text file the orchestrator writes into ``Invocation.work_dir`` before launch.

    Used by tools that read a configuration file (PDAL pipelines, R bridge
    ``invocation.json``). Reference it in ``argv`` via :meth:`Invocation.config_path`.
    """

    name: str
    content: str


@dataclass(frozen=True)
class CommandSpec:
    """Native process description returned by :meth:`ToolAdapter.build_command`.

    ``argv`` is passed to the operating system as a list with no shell, so paths
    containing spaces or shell metacharacters need no quoting. ``argv[0]`` must be
    an absolute executable path.
    """

    argv: tuple[str, ...]
    cwd: Path
    env: Mapping[str, str] = field(default_factory=dict)
    """Environment *delta* applied on top of the orchestrator environment."""
    config_files: tuple[ConfigFile, ...] = ()
    accepted_exceptions: tuple[str, ...] = ()
    """Names of :class:`AcceptedException` s that :meth:`ToolAdapter.interpret` may
    report for this command. Declared before launch (and recorded with the plan),
    normally only when an explicit, frozen opt-in enables them. Empty by default:
    no nonzero exit status can then be accepted."""


@dataclass(frozen=True)
class ArtifactSpec:
    """One expected output of an invocation."""

    path: Path
    kind: ArtifactKind
    required: bool = True
    description: str = ""


@dataclass(frozen=True)
class ToolTiming:
    """Workload interval reported by the tool itself (an *inner* boundary).

    Wall-clock nanoseconds since the Unix epoch, as read by the tool's own clock
    (for example R ``Sys.time()``). The benchmark's timed interval stays the
    orchestrator's outer process interval; this only describes how much of it
    the tool attributes to the workload.
    """

    start_wall_ns: int
    end_wall_ns: int
    clock: str
    boundary: str
    """What the interval contains, for example ``"evaluation of the task call"``."""
    marks: Mapping[str, int] = field(default_factory=dict)
    """Optional named wall-clock instants (for example ``packages_loaded``)."""

    def __post_init__(self) -> None:
        for name, value in (
            ("start_wall_ns", self.start_wall_ns),
            ("end_wall_ns", self.end_wall_ns),
        ):
            if isinstance(value, bool) or not isinstance(value, int):
                raise ValueError(f"{name} must be an integer")
        if self.end_wall_ns < self.start_wall_ns:
            raise ValueError("end_wall_ns must be >= start_wall_ns")
        for name, value in self.marks.items():
            if isinstance(value, bool) or not isinstance(value, int):
                raise ValueError(f"mark {name!r} must be an integer")


@dataclass(frozen=True)
class AcceptedException:
    """A named, documented tool behaviour accepted under an explicit opt-in policy.

    The only way an interpretation can accept a nonzero exit status. The
    orchestrator honours it only when the name was declared beforehand in
    :attr:`CommandSpec.accepted_exceptions`, the interpretation has no failure,
    and the process exited by itself with exactly :attr:`exit_code`. The raw exit
    status and the evidence are always recorded.
    """

    name: str
    """Stable identifier, for example ``lascanopy_all_values_above_height_cutoff``."""
    exit_code: int
    """The raw exit status this exception covers (``0`` records an exception
    without changing the status)."""
    policy: str
    """The opt-in that authorizes it, for example ``lastools.accept_known_cutoff_warning``."""
    evidence: tuple[str, ...]
    """The tool output lines that were recognized (non-empty)."""

    def __post_init__(self) -> None:
        if not EXCEPTION_NAME_PATTERN.match(self.name):
            raise ValueError(f"invalid exception name: {self.name!r}")
        if isinstance(self.exit_code, bool) or not isinstance(self.exit_code, int):
            raise ValueError("exit_code must be an integer")
        if self.exit_code < 0:
            raise ValueError("a signal termination can never be an accepted exception")
        if not self.policy:
            raise ValueError("an accepted exception needs the authorizing policy")
        if not self.evidence or not all(isinstance(e, str) for e in self.evidence):
            raise ValueError("an accepted exception needs its evidence lines")

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "name": self.name,
            "exit_code": self.exit_code,
            "policy": self.policy,
            "evidence": list(self.evidence),
        }


@dataclass(frozen=True)
class Interpretation:
    """Tool-specific reading of a finished process (see :meth:`ToolAdapter.interpret`).

    Interpretation can make an attempt *fail*. The single, narrow exception is an
    :class:`AcceptedException` that the command declared before launch; nothing
    else turns a process that did not exit with status 0 into a success.
    """

    failure_reason: str | None = None
    """Why the run must not count as succeeded (``None``: no objection)."""
    warnings: tuple[str, ...] = ()
    tool_timing: ToolTiming | None = None
    details: Mapping[str, Any] = field(default_factory=dict)
    """JSON-ready tool facts recorded with the attempt (versions, applied settings)."""
    accepted_exception: AcceptedException | None = None
    """A declared, documented exception (see :class:`AcceptedException`)."""


def argv_value(value: ParameterValue | os.PathLike[str]) -> str:
    """Convert a value to a single argv element without shell quoting.

    Floats use the shortest round-trip representation. Booleans are rejected
    because their spelling is tool-specific.
    """
    if isinstance(value, bool):
        raise TypeError("booleans have tool-specific spellings; convert explicitly")
    if isinstance(value, os.PathLike):
        return os.fspath(value)
    if isinstance(value, float):
        return repr(value)
    if isinstance(value, int | str):
        return str(value)
    raise TypeError(f"unsupported argv value type: {type(value).__name__}")


THREAD_CAP_VARIABLES = (
    "OMP_NUM_THREADS",
    "OPENBLAS_NUM_THREADS",
    "MKL_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS",
    "NUMEXPR_NUM_THREADS",
)


def thread_cap_env(threads: int) -> dict[str, str]:
    """Environment delta capping common math/BLAS thread pools at ``threads``."""
    if threads < 1:
        raise ValueError("threads must be >= 1")
    return dict.fromkeys(THREAD_CAP_VARIABLES, str(threads))


class ToolAdapter(ABC):
    """Abstract base class for tool adapters.

    Subclasses set :attr:`adapter_id` and :attr:`display_name`, and implement
    :meth:`probe`, :meth:`identify`, :meth:`workload_support`,
    :meth:`build_command` and :meth:`expected_outputs`. Adapters must be
    constructible without arguments so the registry can instantiate them.
    """

    adapter_id: ClassVar[str]
    """Stable identifier, matching the entry-point name (``^[a-z][a-z0-9_-]*$``)."""

    display_name: ClassVar[str]
    """Human-readable name."""

    @abstractmethod
    def probe(self, platform_info: PlatformInfo) -> Availability:
        """Check installation and platform support.

        Must not raise for missing software; return
        :meth:`Availability.unavailable` with a reason instead.
        """

    @abstractmethod
    def identify(self) -> ToolIdentity:
        """Detect the tool version and identity. Called only when available."""

    @abstractmethod
    def workload_support(self) -> list[WorkloadSupport]:
        """Declare every workload this adapter knows about, including
        unsupported and blocked ones, each with a reason."""

    @abstractmethod
    def build_command(self, invocation: Invocation) -> CommandSpec:
        """Translate an invocation into a native process description.

        Called only for dispatchable workloads with an accepted thread count.

        Raises:
            UnsupportedCombinationError: The parameters or inputs form a
                combination the tool cannot run.
        """

    @abstractmethod
    def expected_outputs(self, invocation: Invocation) -> list[ArtifactSpec]:
        """List expected artifacts; every path must be inside ``invocation.output_dir``."""

    def invocation_metadata(self, invocation: Invocation) -> Mapping[str, MetadataValue]:
        """Tool-specific facts to record with the attempt (default: none)."""
        return {}

    def interpret(self, invocation: Invocation, process: ProcessResult) -> Interpretation | None:
        """Read tool-specific results after the process exited (untimed).

        Called by the orchestrator only for processes that exited by themselves
        (any exit code), before ``work_dir`` is cleaned up. Use it to read a
        tool's own result record or logs, report warnings and an inner workload
        interval, and reject runs whose exit code hides a failure. A nonzero exit
        can only be accepted through an :class:`AcceptedException` declared in
        :attr:`CommandSpec.accepted_exceptions`. The default returns ``None``
        (exit code alone decides).
        """
        return None

    def support_for(self, workload_id: str, threads: int = 1) -> WorkloadSupport:
        """Resolve support for one workload and thread count.

        Unknown workloads and out-of-range thread counts become explicit
        ``unsupported`` results rather than errors.
        """
        for support in self.workload_support():
            if support.workload_id != workload_id:
                continue
            if support.status.dispatchable and not support.threading.accepts(threads):
                return WorkloadSupport(
                    workload_id,
                    ImplementationStatus.UNSUPPORTED,
                    reason=(
                        f"{threads} thread(s) requested; {self.adapter_id} supports "
                        f"{support.threading.describe_range()} for {workload_id} "
                        f"({support.threading.model})"
                    ),
                    threading=support.threading,
                )
            return support
        return WorkloadSupport(
            workload_id,
            ImplementationStatus.UNSUPPORTED,
            reason=f"adapter {self.adapter_id!r} declares no implementation of {workload_id!r}",
        )
