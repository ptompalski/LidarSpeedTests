"""Monitor backend protocol and the shared sample schema.

Two scopes are always kept apart, in separate files and separate summary
sections, so a whole-machine figure can never be mistaken for a process figure:

``whole-machine``
    Everything running on the host, including unrelated activity and the
    orchestrator itself. File ``machine-samples.csv``.
``process-tree``
    The benchmarked process and the descendants the sampler could observe.
    File ``process-tree-samples.csv``.

Every sample row describes one interval ``[start, end]`` on both the monotonic
(``perf_counter_ns``) and the wall clock. A metric that could not be measured is
an empty cell (``None``), never ``0``.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import StrEnum
from pathlib import Path
from typing import Any, Protocol

from lidarbench.core.clock import Instant

SAMPLES_SCHEMA = "lidarbench.monitor-samples/1"
SUMMARY_SCHEMA = "lidarbench.monitor-summary/1"

MACHINE_SAMPLES_FILE = "machine-samples.csv"
TREE_SAMPLES_FILE = "process-tree-samples.csv"


class Scope(StrEnum):
    """Measurement scope."""

    WHOLE_MACHINE = "whole-machine"
    PROCESS_TREE = "process-tree"


class MetricKind(StrEnum):
    """How a sampled value relates to its interval."""

    INTERVAL_MEAN = "interval_mean"
    """Average over the interval (for example CPU percent)."""

    INSTANT = "instant"
    """Instantaneous value read at the interval end (for example RSS)."""

    INTERVAL_DELTA = "interval_delta"
    """Amount accumulated during the interval (for example CPU seconds, bytes read)."""

    COUNT = "count"
    """Number of processes at the interval end."""


class MonitoringStatus(StrEnum):
    """Coverage verdict for monitoring of one timed interval."""

    AVAILABLE = "available"
    PARTIAL = "partial"
    UNAVAILABLE = "unavailable"
    DISABLED = "disabled"


@dataclass(frozen=True)
class Metric:
    """One sampled metric column."""

    name: str
    unit: str
    kind: MetricKind
    description: str
    lost_on_exit: bool = False
    """Per-process counter: a process that exits between two samples takes its
    last, unobserved contribution with it, so affected values are lower bounds."""

    def to_record(self) -> dict[str, str | bool]:
        """JSON-ready representation."""
        return {
            "unit": self.unit,
            "kind": str(self.kind),
            "description": self.description,
            "lost_on_exit": self.lost_on_exit,
        }


INTERVAL_COLUMNS = (
    "sample_index",
    "interval_start_perf_ns",
    "interval_end_perf_ns",
    "interval_start_wall_ns",
    "interval_end_wall_ns",
)

MACHINE_METRICS = (
    Metric(
        "cpu_percent",
        "percent_of_all_logical_cpus",
        MetricKind.INTERVAL_MEAN,
        "Whole-machine CPU utilisation (psutil.cpu_percent); includes unrelated activity.",
    ),
    Metric(
        "memory_used_bytes",
        "bytes",
        MetricKind.INSTANT,
        "Whole-machine memory in use, defined as total minus available.",
    ),
    Metric(
        "memory_available_bytes",
        "bytes",
        MetricKind.INSTANT,
        "Whole-machine memory available without swapping (psutil.virtual_memory).",
    ),
    Metric("memory_total_bytes", "bytes", MetricKind.INSTANT, "Installed physical memory."),
    Metric(
        "disk_read_bytes",
        "bytes",
        MetricKind.INTERVAL_DELTA,
        "Bytes read from all physical disks (psutil.disk_io_counters); all processes.",
    ),
    Metric(
        "disk_write_bytes",
        "bytes",
        MetricKind.INTERVAL_DELTA,
        "Bytes written to all physical disks (psutil.disk_io_counters); all processes.",
    ),
)

TREE_METRICS = (
    Metric(
        "process_count",
        "processes",
        MetricKind.COUNT,
        "Processes of the observed tree alive at the interval end.",
        lost_on_exit=True,
    ),
    Metric(
        "new_processes",
        "processes",
        MetricKind.INTERVAL_DELTA,
        "Processes first observed during the interval.",
    ),
    Metric(
        "exited_processes",
        "processes",
        MetricKind.INTERVAL_DELTA,
        "Observed processes that exited during the interval; CPU and I/O they "
        "consumed after their last observation are missing.",
    ),
    Metric(
        "cpu_seconds",
        "cpu_seconds",
        MetricKind.INTERVAL_DELTA,
        "User plus system CPU time of observed processes (all threads).",
        lost_on_exit=True,
    ),
    Metric(
        "cpu_percent",
        "percent_of_one_logical_cpu",
        MetricKind.INTERVAL_MEAN,
        "cpu_seconds / interval length * 100; exceeds 100 when several cores are busy.",
        lost_on_exit=True,
    ),
    Metric(
        "rss_bytes",
        "bytes",
        MetricKind.INSTANT,
        "Sum of resident set sizes of observed processes; shared pages are counted "
        "once per process.",
        lost_on_exit=True,
    ),
    Metric(
        "io_read_bytes",
        "bytes",
        MetricKind.INTERVAL_DELTA,
        "Per-process read counters summed over observed processes (semantics are "
        "platform-specific; see metadata).",
        lost_on_exit=True,
    ),
    Metric(
        "io_write_bytes",
        "bytes",
        MetricKind.INTERVAL_DELTA,
        "Per-process write counters summed over observed processes (semantics are "
        "platform-specific; see metadata).",
        lost_on_exit=True,
    ),
)

ERRORS_COLUMN = "errors"
MACHINE_COLUMNS = (*INTERVAL_COLUMNS, *(m.name for m in MACHINE_METRICS), ERRORS_COLUMN)
TREE_COLUMNS = (*INTERVAL_COLUMNS, *(m.name for m in TREE_METRICS), ERRORS_COLUMN)

IO_SEMANTICS = {
    "Linux": "read_bytes/write_bytes from /proc/<pid>/io: bytes fetched from or sent to "
    "the storage layer (page-cache hits are not counted; writes are counted when dirtied).",
    "Windows": "ReadTransferCount/WriteTransferCount: all I/O including files, pipes, "
    "devices and network.",
    "Darwin": "unavailable: psutil exposes no per-process I/O counters on macOS.",
}


class MonitorStartError(RuntimeError):
    """The monitor could not be started; nothing was launched."""


@dataclass
class MonitorReport:
    """What a monitor recorded, returned by :meth:`MonitorHandle.stop`."""

    backend: str
    attempt_id: str
    interval_s: float
    monitor_dir: Path
    machine_samples: Path
    tree_samples: Path
    ready: Instant | None = None
    attached: bool = False
    stopped_cleanly: bool = False
    clock_alignment: str = "monotonic"
    capabilities: dict[str, Any] = field(default_factory=dict)
    sampler: dict[str, Any] = field(default_factory=dict)
    errors: list[str] = field(default_factory=list)

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "backend": self.backend,
            "attempt_id": self.attempt_id,
            "interval_s": self.interval_s,
            "machine_samples": self.machine_samples.name,
            "tree_samples": self.tree_samples.name,
            "ready": self.ready.to_record() if self.ready else None,
            "attached": self.attached,
            "stopped_cleanly": self.stopped_cleanly,
            "clock_alignment": self.clock_alignment,
            "capabilities": dict(self.capabilities),
            "sampler": dict(self.sampler),
            "errors": list(self.errors),
        }


class MonitorHandle(Protocol):
    """A running monitor for one attempt."""

    def attach(self, pid: int, launch: Instant) -> None:
        """Start process-tree sampling of ``pid`` (launched at ``launch``). Never raises."""

    def stop(self) -> MonitorReport:
        """Stop sampling and return the report. Never raises."""


class MonitorBackend(Protocol):
    """A monitor backend plugin (entry-point group ``lidarbench.monitors``)."""

    name: str
    scopes: frozenset[Scope]

    def probe(self) -> tuple[bool, str]:
        """Whether the backend can run here, with a reason when it cannot."""

    def start(self, monitor_dir: Path, attempt_id: str) -> MonitorHandle:
        """Start sampling (outside any timed interval).

        Raises:
            MonitorStartError: The monitor did not become ready.
        """
