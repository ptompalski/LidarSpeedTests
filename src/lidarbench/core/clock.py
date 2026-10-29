"""Monotonic and wall-clock timestamps.

Durations always come from :func:`time.perf_counter_ns` (monotonic, highest
available resolution). Wall timestamps from :func:`time.time_ns` are recorded
beside them for humans and cross-host correlation, never for durations.
"""

from __future__ import annotations

import time
from dataclasses import dataclass
from datetime import UTC, datetime
from typing import Any


@dataclass(frozen=True, order=True)
class Instant:
    """A moment captured on both the monotonic and the wall clock."""

    perf_ns: int
    """``time.perf_counter_ns()``; only differences are meaningful."""

    wall_ns: int
    """``time.time_ns()``; nanoseconds since the Unix epoch (UTC)."""

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {"perf_ns": self.perf_ns, "wall_ns": self.wall_ns, "wall_utc": iso_utc(self.wall_ns)}


def now() -> Instant:
    """Capture the current instant (monotonic clock first, it times the work)."""
    perf = time.perf_counter_ns()
    return Instant(perf, time.time_ns())


def iso_utc(wall_ns: int) -> str:
    """ISO 8601 UTC string with microsecond precision for a ``time_ns()`` value."""
    seconds, remainder = divmod(wall_ns, 1_000_000_000)
    stamp = datetime.fromtimestamp(seconds, tz=UTC).replace(microsecond=remainder // 1000)
    return stamp.isoformat().replace("+00:00", "Z")
