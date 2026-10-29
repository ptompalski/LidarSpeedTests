"""Deterministic schedule-v2: generator, structure, balance and golden values."""

from __future__ import annotations

import itertools
from collections import Counter

import pytest

from lidarbench.campaign.schedule import (
    ScheduleEntry,
    SplitMix64,
    Unit,
    build_schedule,
    schedule_csv,
    schedule_record,
)
from lidarbench.config.campaign import SchedulingConfig
from lidarbench.core.canonical import digest

UNITS = [Unit("w1", "d", 1), Unit("w1", "d", 4), Unit("w2", "d", 1)]
TOOLS = ["a", "b", "c"]


def policy(**kw) -> SchedulingConfig:
    kw.setdefault("seed", 42)
    kw.setdefault("repetitions", 3)
    kw.setdefault("warmups", 1)
    return SchedulingConfig(**kw)


def test_splitmix64_reference_vector():
    rng = SplitMix64(1234567)
    assert [rng.next_u64() for _ in range(5)] == [
        6457827717110365317,
        3203168211198807973,
        9817491932198370423,
        4593380528125082431,
        16408922859458223821,
    ]


def test_bounded_and_permutation_are_complete():
    rng = SplitMix64(7)
    assert {rng.below(3) for _ in range(300)} == {0, 1, 2}
    assert sorted(rng.permutation(range(10))) == list(range(10))
    with pytest.raises(ValueError):
        rng.below(0)


def test_golden_schedules_are_stable_across_platforms_and_versions():
    balanced = build_schedule(UNITS, TOOLS, policy())
    assert [(e.phase[0], e.repetition, e.workload, e.cpus, e.tool) for e in balanced[:6]] == [
        ("w", 1, "w2", 1, "c"),
        ("w", 1, "w2", 1, "a"),
        ("w", 1, "w2", 1, "b"),
        ("w", 1, "w1", 1, "a"),
        ("w", 1, "w1", 1, "c"),
        ("w", 1, "w1", 1, "b"),
    ]
    assert digest([e.to_record() for e in balanced]) == (
        "a5d77dee3721ceee5fe8068630e4415bb1578956c898b89d9197f6a6a7206b09"
    )
    randomized = build_schedule(UNITS, TOOLS, policy(tool_order="randomized"))
    assert digest([e.to_record() for e in randomized]) == (
        "d617033ff17e9f8dabc2de9e230c9d28199abb60dbd8949f2cae1d90e7354317"
    )


def test_independent_of_input_order_and_seed_sensitive():
    one = build_schedule(UNITS, TOOLS, policy())
    assert one == build_schedule(list(reversed(UNITS)), list(reversed(TOOLS)), policy())
    assert one != build_schedule(UNITS, TOOLS, policy(seed=43))


@pytest.mark.parametrize("order", ["balanced", "randomized"])
@pytest.mark.parametrize("n_tools", [1, 2, 3, 4])
@pytest.mark.parametrize("repetitions", [1, 2, 3, 5, 8])
def test_structure_counts_and_balance(order, n_tools, repetitions):
    tools = ["a", "b", "c", "d"][:n_tools]
    entries = build_schedule(
        UNITS, tools, policy(repetitions=repetitions, warmups=2, tool_order=order)
    )
    assert [e.schedule_index for e in entries] == list(range(1, len(entries) + 1))
    assert len(entries) == len(UNITS) * n_tools * (repetitions + 2)
    phases = [e.phase for e in entries]
    assert phases == sorted(phases, key=lambda p: p != "warmup")
    assert len({e.entry_key for e in entries}) == len(entries)
    for phase, count in (("warmup", 2), ("measurement", repetitions)):
        rows = [e for e in entries if e.phase == phase]
        per_unit_tool = Counter((e.workload, e.cpus, e.tool) for e in rows)
        assert set(per_unit_tool.values()) == {count}
        for _, group in itertools.groupby(rows, key=lambda e: (e.workload, e.cpus, e.repetition)):
            group = list(group)
            assert sorted(e.tool for e in group) == sorted(tools)
            assert [e.tool_position for e in group] == list(range(1, n_tools + 1))
        if order == "balanced":
            for unit in UNITS:
                positions = Counter(
                    (e.tool, e.tool_position)
                    for e in rows
                    if (e.workload, e.cpus) == (unit.workload, unit.cpus)
                )
                values = [positions.get((t, p), 0) for t in tools for p in range(1, n_tools + 1)]
                assert max(values) - min(values) <= 1


def test_zero_warmups_and_validation():
    entries = build_schedule(UNITS, TOOLS, policy(warmups=0))
    assert all(e.phase == "measurement" for e in entries)
    with pytest.raises(ValueError):
        build_schedule([], TOOLS, policy())
    with pytest.raises(ValueError):
        build_schedule(UNITS, ["a", "a"], policy())


def test_records_round_trip_and_csv():
    entries = build_schedule(UNITS, TOOLS, policy())
    record = schedule_record(entries, policy())
    assert record["seed"] == 42 and record["schema"] == "lidarbench.schedule/2"
    assert tuple(ScheduleEntry.from_record(r) for r in record["entries"]) == entries
    bad = dict(record["entries"][0], entry_key="tampered")
    with pytest.raises(ValueError):
        ScheduleEntry.from_record(bad)
    lines = schedule_csv(entries).decode("utf-8").splitlines()
    assert lines[0].startswith("schedule_index,entry_key,phase")
    assert len(lines) == len(entries) + 1


def _attempt(entry, n, status, *, launched=True, state="completed", missing=()):
    from lidarbench.campaign.store import AttemptInfo

    result = None
    if state == "completed":
        result = {
            "execution_status": status,
            "launched": launched,
            "missing_required_outputs": list(missing),
        }
    return AttemptInfo(
        f"A{n:03d}", None, state, {"entry": entry.to_record(), "sequence": n}, result  # type: ignore[arg-type]
    )


@pytest.mark.parametrize(
    ("history", "retry_failed", "expected"),
    [
        ([], False, "run"),
        ([("succeeded", {})], False, "skip-complete"),
        ([("skipped_blocked", {"launched": False})], False, "skip-complete"),
        ([("succeeded", {"missing": ["x"]})], False, "skip-failed"),
        ([("failed", {})], False, "skip-failed"),
        ([("timed_out", {})], True, "retry-failed"),
        ([("failed", {"launched": False})], False, "retry-interrupted"),
        ([("interrupted", {})], False, "retry-interrupted"),
        ([(None, {"state": "abandoned"})], False, "retry-interrupted"),
        ([("failed", {}), ("succeeded", {})], False, "skip-complete"),
        ([("failed", {}), ("interrupted", {})], False, "skip-failed"),
    ],
)
def test_resume_decisions(history, retry_failed, expected):
    from lidarbench.campaign.runner import Selection, decide

    entries = build_schedule([Unit("w", "d", 1)], ["a"], policy(repetitions=1, warmups=1))
    measurement = entries[1]
    attempts = [
        _attempt(measurement, i, status, **kw) for i, (status, kw) in enumerate(history, start=1)
    ]
    decisions = decide(entries, attempts, Selection(), retry_failed)
    assert str(decisions[1].action) == expected
    warmup_dispatched = decisions[0].action.dispatches
    assert warmup_dispatched == decisions[1].action.dispatches


def test_limit_counts_measurements_and_keeps_their_warmups():
    from lidarbench.campaign.runner import Selection, decide

    entries = build_schedule([Unit("w", "d", 1)], ["a", "b"], policy(warmups=1, repetitions=3))
    decisions = decide(entries, [], Selection(limit=2), False)
    runs = [d for d in decisions if str(d.action) == "run"]
    assert len(runs) == 2
    assert [str(d.action) for d in decisions if d.entry.phase == "warmup"] == ["warmup"] * 2
