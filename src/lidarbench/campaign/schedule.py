"""Deterministic campaign schedules (``schedule-v2``).

Structure (same as the legacy R ``benchmark_schedule``):

- all warm-up rows first, then all measurement rows;
- a *unit* is one (workload, dataset, cpu count); every unit runs every tool
  once per repetition;
- the order of units is reshuffled for every repetition;
- ``tool_order = "balanced"``: each unit draws one random base permutation of
  the tools per phase, rotated by one position per repetition (a Latin-square
  design), so each tool occupies each position equally often (within one when
  the repetitions are not a multiple of the number of tools);
- ``tool_order = "randomized"``: a fresh random permutation for every unit and
  repetition (unbiased in expectation, not balanced in a single campaign).

Randomness comes from an explicitly specified generator, SplitMix64 with
rejection-sampled bounded integers and Fisher-Yates shuffles, so schedules are
identical on every OS and Python version. The seed is recorded. R schedules
cannot be reproduced bit-for-bit (different generator); legacy schedules stay
in their own manifests.
"""

from __future__ import annotations

import csv
import io
from collections.abc import Iterable, Sequence
from dataclasses import dataclass
from typing import Any, TypeVar

from lidarbench.config.campaign import CampaignConfig, SchedulingConfig

SCHEDULE_SCHEMA = "lidarbench.schedule/2"
RNG_DESCRIPTION = "splitmix64; bounded integers by rejection sampling; Fisher-Yates shuffle"
PHASES = ("warmup", "measurement")
SCHEDULE_COLUMNS = (
    "schedule_index",
    "entry_key",
    "phase",
    "repetition",
    "workload",
    "dataset",
    "cpus",
    "tool",
    "tool_position",
)

_MASK = (1 << 64) - 1
_T = TypeVar("_T")


class SplitMix64:
    """SplitMix64 pseudo-random generator (Steele, Lea and Flood, 2014)."""

    def __init__(self, seed: int) -> None:
        self._state = seed & _MASK

    def next_u64(self) -> int:
        """Next 64-bit output."""
        self._state = (self._state + 0x9E3779B97F4A7C15) & _MASK
        z = self._state
        z = ((z ^ (z >> 30)) * 0xBF58476D1CE4E5B9) & _MASK
        z = ((z ^ (z >> 27)) * 0x94D049BB133111EB) & _MASK
        return z ^ (z >> 31)

    def below(self, n: int) -> int:
        """Uniform integer in ``[0, n)`` without modulo bias."""
        if n < 1:
            raise ValueError("n must be >= 1")
        limit = (1 << 64) - ((1 << 64) % n)
        while True:
            value = self.next_u64()
            if value < limit:
                return value % n

    def permutation(self, items: Sequence[_T]) -> list[_T]:
        """Fisher-Yates shuffle of a copy of ``items``."""
        out = list(items)
        for i in range(len(out) - 1, 0, -1):
            j = self.below(i + 1)
            out[i], out[j] = out[j], out[i]
        return out


@dataclass(frozen=True, order=True)
class Unit:
    """One (workload, dataset, cpu count) combination."""

    workload: str
    dataset: str
    cpus: int


@dataclass(frozen=True)
class ScheduleEntry:
    """One row of a frozen schedule."""

    schedule_index: int
    phase: str
    repetition: int
    workload: str
    dataset: str
    cpus: int
    tool: str
    tool_position: int

    @property
    def unit_key(self) -> str:
        """Workload/dataset/cpus/tool, shared by warm-up and measurement rows."""
        return f"{self.workload}/{self.dataset}/c{self.cpus}/{self.tool}"

    @property
    def entry_key(self) -> str:
        """Unique, stable key of this schedule row."""
        return f"{self.phase}/{self.unit_key}/r{self.repetition}"

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "schedule_index": self.schedule_index,
            "entry_key": self.entry_key,
            "phase": self.phase,
            "repetition": self.repetition,
            "workload": self.workload,
            "dataset": self.dataset,
            "cpus": self.cpus,
            "tool": self.tool,
            "tool_position": self.tool_position,
        }

    @classmethod
    def from_record(cls, record: dict[str, Any]) -> ScheduleEntry:
        """Inverse of :meth:`to_record` (the derived ``entry_key`` is checked)."""
        entry = cls(
            schedule_index=int(record["schedule_index"]),
            phase=str(record["phase"]),
            repetition=int(record["repetition"]),
            workload=str(record["workload"]),
            dataset=str(record["dataset"]),
            cpus=int(record["cpus"]),
            tool=str(record["tool"]),
            tool_position=int(record["tool_position"]),
        )
        if record.get("entry_key") != entry.entry_key:
            raise ValueError(f"schedule row {entry.schedule_index}: entry_key mismatch")
        return entry


def campaign_units(config: CampaignConfig) -> list[Unit]:
    """All selected units of a campaign, sorted."""
    return sorted(
        Unit(workload.name, dataset, cpus)
        for workload in config.workloads
        for dataset in config.workload_datasets(workload)
        for cpus in config.cpu_counts
    )


def build_schedule(
    units: Iterable[Unit], tools: Iterable[str], policy: SchedulingConfig
) -> tuple[ScheduleEntry, ...]:
    """Build the schedule; the result depends only on the sets and the policy.

    Raises:
        ValueError: No units or tools, or duplicates.
    """
    unit_list, tool_list = list(units), list(tools)
    if not unit_list or not tool_list:
        raise ValueError("a schedule needs at least one unit and one tool")
    if len(set(unit_list)) != len(unit_list) or len(set(tool_list)) != len(tool_list):
        raise ValueError("schedule units and tools must be unique")
    unit_list.sort()
    tool_list.sort()
    rng = SplitMix64(policy.seed)
    n_tools = len(tool_list)
    rows: list[ScheduleEntry] = []
    for phase in PHASES:
        count = policy.warmups if phase == "warmup" else policy.repetitions
        bases = (
            [rng.permutation(tool_list) for _ in unit_list]
            if policy.tool_order == "balanced"
            else []
        )
        for repetition in range(1, count + 1):
            for i in rng.permutation(range(len(unit_list))):
                unit = unit_list[i]
                if policy.tool_order == "balanced":
                    shift = (repetition - 1) % n_tools
                    order = bases[i][shift:] + bases[i][:shift]
                else:
                    order = rng.permutation(tool_list)
                for position, tool in enumerate(order, start=1):
                    rows.append(
                        ScheduleEntry(
                            schedule_index=len(rows) + 1,
                            phase=phase,
                            repetition=repetition,
                            workload=unit.workload,
                            dataset=unit.dataset,
                            cpus=unit.cpus,
                            tool=tool,
                            tool_position=position,
                        )
                    )
    return tuple(rows)


def campaign_schedule(config: CampaignConfig) -> tuple[ScheduleEntry, ...]:
    """The schedule of a campaign configuration."""
    return build_schedule(campaign_units(config), [t.id for t in config.tools], config.scheduling)


def schedule_record(entries: Sequence[ScheduleEntry], policy: SchedulingConfig) -> dict[str, Any]:
    """Frozen JSON form of a schedule (identity component ``schedule``)."""
    return {
        "schema": SCHEDULE_SCHEMA,
        "rng": RNG_DESCRIPTION,
        "seed": policy.seed,
        "tool_order": policy.tool_order,
        "warmups": policy.warmups,
        "repetitions": policy.repetitions,
        "entries": [e.to_record() for e in entries],
    }


def schedule_csv(entries: Sequence[ScheduleEntry]) -> bytes:
    """CSV rendering of a schedule (UTF-8, ``\\n`` line endings)."""
    buffer = io.StringIO(newline="")
    writer = csv.DictWriter(buffer, fieldnames=SCHEDULE_COLUMNS, lineterminator="\n")
    writer.writeheader()
    for entry in entries:
        writer.writerow(entry.to_record())
    return buffer.getvalue().encode("utf-8")
