"""The generic adapter interpretation phase of ``execute_plan`` (tool-neutral)."""

from __future__ import annotations

import dataclasses
import json
import sys
from pathlib import Path

import pytest
from campaign_helpers import ScriptedAdapter

from lidarbench.core.platform import get_platform_info
from lidarbench.execution.adapters import (
    AcceptedException,
    CommandSpec,
    Interpretation,
    Invocation,
    ToolTiming,
)
from lidarbench.execution.attempt import execute_plan
from lidarbench.execution.plan import AdapterContractError, plan_invocation, validate_command_spec


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


# -- declared accepted exceptions (the only way a nonzero exit can succeed) ------------


def _exception(code: int, name: str = "known_tool_warning") -> AcceptedException:
    return AcceptedException(name, code, "test.opt_in", ("WARNING: known",))


class _Declaring(_Interpreting):
    """Declares ``known_tool_warning`` on its commands (as an opt-in would)."""

    declared: tuple[str, ...] = ("known_tool_warning",)

    def build_command(self, invocation):
        return dataclasses.replace(
            super().build_command(invocation), accepted_exceptions=self.declared
        )


def test_declared_exception_accepts_its_exact_exit_status(tmp_path):
    adapter = _Declaring(Interpretation(accepted_exception=_exception(3)))
    execution, record = _run(tmp_path, adapter, "scripted.fail")
    assert str(execution.execution_status) == "succeeded"
    assert record["execution_status"] == "succeeded" and record["failure_phase"] is None
    assert record["process"]["exit_code"] == 3
    assert record["process"]["execution_status"] == "failed"  # raw process verdict kept
    assert record["accepted_exception"] == {
        "name": "known_tool_warning",
        "exit_code": 3,
        "policy": "test.opt_in",
        "evidence": ["WARNING: known"],
        "raw_exit_code": 3,
    }
    assert record["interpretation"]["accepted_exception"]["name"] == "known_tool_warning"
    assert any("accepted exception known_tool_warning" in w for w in record["warnings"])


def test_declared_exception_with_exit_zero_is_recorded(tmp_path):
    adapter = _Declaring(Interpretation(accepted_exception=_exception(0)))
    execution, record = _run(tmp_path, adapter)
    assert str(execution.execution_status) == "succeeded"
    assert record["accepted_exception"]["exit_code"] == 0


@pytest.mark.parametrize(
    ("adapter", "workload", "text"),
    [
        # Not declared by the command: never accepted.
        (_Interpreting(Interpretation(accepted_exception=_exception(3))), "scripted.fail",
         "did not declare"),
        # Declared, but the process exited with another status.
        (_Declaring(Interpretation(accepted_exception=_exception(1))), "scripted.fail",
         "covers exit status 1"),
        # A different (undeclared) name.
        (_Declaring(Interpretation(accepted_exception=_exception(3, "other_warning"))),
         "scripted.fail", "did not declare"),
        # An objection always wins over an exception.
        (_Declaring(Interpretation(failure_reason="ERROR line", accepted_exception=_exception(3))),
         "scripted.fail", "ERROR line"),
        # Wrong type.
        (_Declaring(Interpretation(accepted_exception="known_tool_warning")),  # type: ignore[arg-type]
         "scripted.fail", "adapter interpretation failed"),
    ],
)  # fmt: skip
def test_undeclared_or_mismatched_exceptions_never_succeed(tmp_path, adapter, workload, text):
    execution, record = _run(tmp_path, adapter, workload)
    assert str(execution.execution_status) == "failed"
    assert record["failure_phase"] == "process"
    assert text in record["failure_reason"]
    assert record["accepted_exception"] is None


def test_declared_exception_cannot_rescue_a_timeout(tmp_path):
    adapter = _Declaring(Interpretation(accepted_exception=_exception(0)))
    dirs = [tmp_path / n for n in ("in", "out", "work", "attempt")]
    for path in dirs:
        path.mkdir()
    (dirs[0] / "t.laz").write_bytes(b"x")
    inv = Invocation("scripted.sleep", (dirs[0] / "t.laz",), dirs[1], dirs[2])
    plan = plan_invocation(adapter, inv, platform_info=get_platform_info())
    execution = execute_plan(plan, inv, dirs[3], timeout_s=0.5, adapter=adapter)
    assert str(execution.execution_status) == "timed_out"
    assert execution.accepted_exception is None


@pytest.mark.parametrize(
    "kwargs",
    [
        {"name": "Bad Name", "exit_code": 1, "policy": "p", "evidence": ("x",)},
        {"name": "ok", "exit_code": -9, "policy": "p", "evidence": ("x",)},
        {"name": "ok", "exit_code": True, "policy": "p", "evidence": ("x",)},
        {"name": "ok", "exit_code": 1, "policy": "", "evidence": ("x",)},
        {"name": "ok", "exit_code": 1, "policy": "p", "evidence": ()},
    ],
)
def test_accepted_exception_validation(kwargs):
    with pytest.raises(ValueError):
        AcceptedException(**kwargs)


@pytest.mark.parametrize("names", [("Bad Name",), ["known"], (3,)])
def test_command_spec_rejects_invalid_exception_declarations(tmp_path, names):
    spec = CommandSpec(argv=(sys.executable,), cwd=tmp_path, accepted_exceptions=names)
    with pytest.raises(AdapterContractError, match="exception"):
        validate_command_spec(spec)
