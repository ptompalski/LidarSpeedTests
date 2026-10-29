"""The generic adapter interpretation phase of ``execute_plan`` (tool-neutral)."""

from __future__ import annotations

import json
from pathlib import Path

import pytest
from campaign_helpers import ScriptedAdapter

from lidarbench.core.platform import get_platform_info
from lidarbench.execution.adapters import Interpretation, Invocation, ToolTiming
from lidarbench.execution.attempt import execute_plan
from lidarbench.execution.plan import plan_invocation


class _Interpreting(ScriptedAdapter):
    def __init__(self, result=None, error: Exception | None = None) -> None:
        self.result = result
        self.error = error
        self.calls = 0

    def interpret(self, invocation, process):
        self.calls += 1
        if self.error is not None:
            raise self.error
        if callable(self.result):
            return self.result(process)
        return self.result


def _run(tmp_path: Path, adapter: ScriptedAdapter, workload: str = "scripted.ok"):
    dirs = [tmp_path / name for name in ("in", "out", "work", "attempt")]
    for path in dirs:
        path.mkdir()
    (dirs[0] / "tile.laz").write_bytes(b"x")
    inv = Invocation(workload, (dirs[0] / "tile.laz",), dirs[1], dirs[2])
    plan = plan_invocation(adapter, inv, platform_info=get_platform_info())
    execution = execute_plan(plan, inv, dirs[3], timeout_s=60, adapter=adapter)
    record = json.loads((dirs[3] / "execution.json").read_text(encoding="utf-8"))
    return execution, record


def test_without_adapter_there_is_no_interpretation(tmp_path):
    adapter = ScriptedAdapter()
    dirs = [tmp_path / n for n in ("in", "out", "work", "attempt")]
    for path in dirs:
        path.mkdir()
    (dirs[0] / "t.laz").write_bytes(b"x")
    inv = Invocation("scripted.ok", (dirs[0] / "t.laz",), dirs[1], dirs[2])
    plan = plan_invocation(adapter, inv, platform_info=get_platform_info())
    execution = execute_plan(plan, inv, dirs[3], timeout_s=60)
    assert execution.interpretation is None and execution.tool_timing is None
    assert "interpretation" not in [p.name for p in execution.phases]


def test_default_interpret_is_recorded_as_not_provided(tmp_path):
    execution, record = _run(tmp_path, ScriptedAdapter())
    assert str(execution.execution_status) == "succeeded"
    assert record["interpretation"] == {"adapter_id": "scripted", "provided": False}


def test_objection_fails_a_clean_exit(tmp_path):
    adapter = _Interpreting(Interpretation(failure_reason="tool log says ERROR", warnings=("w",)))
    execution, record = _run(tmp_path, adapter)
    assert str(execution.execution_status) == "failed"
    assert record["failure_phase"] == "interpretation"
    assert record["failure_reason"] == "tool log says ERROR"
    assert record["warnings"] == ["w"]


def test_objection_explains_a_failed_process_without_changing_status(tmp_path):
    adapter = _Interpreting(Interpretation(failure_reason="exit 3 means bad input"))
    _, record = _run(tmp_path, adapter, "scripted.fail")
    assert record["execution_status"] == "failed" and record["process"]["exit_code"] == 3
    assert record["failure_phase"] == "process"
    assert record["failure_reason"] == "exit 3 means bad input"


@pytest.mark.parametrize(
    "adapter",
    [
        _Interpreting(error=RuntimeError("parser crashed")),
        _Interpreting("not an interpretation"),
        _Interpreting(Interpretation(details={"bad": float("nan")})),
        _Interpreting(Interpretation(details={"bad": object()})),
    ],
)
def test_defective_interpretation_fails_closed(tmp_path, adapter):
    execution, record = _run(tmp_path, adapter)
    assert str(execution.execution_status) == "failed"
    assert record["failure_phase"] == "interpretation"
    assert "adapter interpretation failed" in record["failure_reason"]


def test_tool_timing_overheads_are_derived_from_the_outer_interval(tmp_path):
    def inside(process):
        start = process.start.wall_ns + 1_000
        return Interpretation(
            tool_timing=ToolTiming(start, start + 10, "test clock", "inner", {"m": start}),
            details={"k": [1, "two"]},
        )

    execution, record = _run(tmp_path, _Interpreting(inside))
    assert str(execution.execution_status) == "succeeded"
    timing = record["interpretation"]["tool_timing"]
    assert timing["startup_overhead_ns"] == 1_000 and timing["workload_elapsed_ns"] == 10
    assert timing["within_outer_interval"] is True
    assert timing["outer_timing_boundary"] == "process" and timing["marks"] == {
        "m": timing["start_wall_ns"]
    }
    assert execution.tool_timing == timing
    assert record["interpretation"]["details"] == {"k": [1, "two"]}


def test_inner_interval_outside_the_process_is_flagged(tmp_path):
    def outside(process):
        start = process.start.wall_ns - 5_000_000_000
        return Interpretation(tool_timing=ToolTiming(start, start + 1, "skewed", "inner"))

    execution, record = _run(tmp_path, _Interpreting(outside))
    assert str(execution.execution_status) == "succeeded"
    assert record["interpretation"]["tool_timing"]["within_outer_interval"] is False
    assert any("outside the outer process interval" in w for w in record["warnings"])


def test_interpret_is_not_called_for_timeouts(tmp_path):
    adapter = _Interpreting(Interpretation(failure_reason="never"))
    dirs = [tmp_path / n for n in ("in", "out", "work", "attempt")]
    for path in dirs:
        path.mkdir()
    (dirs[0] / "t.laz").write_bytes(b"x")
    inv = Invocation("scripted.sleep", (dirs[0] / "t.laz",), dirs[1], dirs[2])
    plan = plan_invocation(adapter, inv, platform_info=get_platform_info())
    execution = execute_plan(plan, inv, dirs[3], timeout_s=0.5, adapter=adapter)
    assert str(execution.execution_status) == "timed_out"
    assert adapter.calls == 0 and execution.interpretation is None


@pytest.mark.parametrize(
    "kwargs",
    [
        {"start_wall_ns": 2, "end_wall_ns": 1},
        {"start_wall_ns": 1.0, "end_wall_ns": 2},
        {"start_wall_ns": 1, "end_wall_ns": 2, "marks": {"m": True}},
    ],
)
def test_tool_timing_validation(kwargs):
    with pytest.raises(ValueError):
        ToolTiming(clock="c", boundary="b", **kwargs)
