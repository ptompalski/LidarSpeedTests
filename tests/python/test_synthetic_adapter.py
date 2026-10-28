"""Tests for the synthetic adapter: argv construction and known, deterministic outputs."""

from __future__ import annotations

import json
import os
import subprocess
import sys
from dataclasses import replace
from pathlib import Path

import pytest

from lidarbench.core.platform import get_platform_info
from lidarbench.execution.adapters import ImplementationStatus, Invocation
from lidarbench.execution.adapters.synthetic import (
    CHECKSUM,
    HASHCHAIN,
    JOB_FILE_NAME,
    WORKER_MODULE,
    SyntheticAdapter,
)
from lidarbench.execution.adapters.synthetic_worker import hashchain_digest
from lidarbench.execution.plan import (
    InvocationPlan,
    PlanStatus,
    plan_invocation,
    write_config_files,
)

ABC_SHA256 = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
ABC_HASHCHAIN_3 = "8a8313399839809b2e75ce8d5400b02f9087d8751d6dd46874dd79cb720559e5"


@pytest.fixture
def layout(tmp_path: Path) -> dict[str, Path]:
    root = tmp_path / "campaign root with spaces"
    inputs = root / "input files"
    out = root / "attempt 01" / "out put"
    work = root / "attempt 01" / "work dir"
    for d in (inputs, out, work):
        d.mkdir(parents=True)
    a = inputs / "tile 1 (a).laz"
    b = inputs / "tile;2 $HOME.laz"
    a.write_bytes(b"abc")
    b.write_bytes(b"")
    return {"a": a, "b": b, "out": out, "work": work}


def invocation(layout: dict[str, Path], workload: str, *inputs: str, **kw) -> Invocation:
    paths = tuple(layout[k] for k in (inputs or ("a", "b")))
    return Invocation(workload, paths, layout["out"], layout["work"], **kw)


def execute(plan: InvocationPlan, work_dir: Path) -> subprocess.CompletedProcess[str]:
    """Minimal stand-in for the future runner: structured argv, env delta, no shell."""
    assert plan.ready and plan.command is not None
    write_config_files(plan.command, work_dir)
    return subprocess.run(
        list(plan.command.argv),
        cwd=plan.command.cwd,
        env={**os.environ, **plan.command.env},
        capture_output=True,
        text=True,
        shell=False,
        timeout=120,
        check=False,
    )


class TestCapabilities:
    def test_always_available_with_current_interpreter(self):
        adapter = SyntheticAdapter()
        availability = adapter.probe(get_platform_info())
        assert availability.available
        assert availability.executables["python"].is_absolute()
        identity = adapter.identify()
        assert identity.tool == "synthetic"
        assert identity.details["python_version"]

    def test_declares_explicit_statuses(self):
        statuses = {w.workload_id: w.status for w in SyntheticAdapter().workload_support()}
        assert statuses == {
            "synthetic.checksum": ImplementationStatus.RUNNABLE,
            "synthetic.hashchain": ImplementationStatus.RUNNABLE,
            "synthetic.blocked": ImplementationStatus.BLOCKED,
            "synthetic.unsupported": ImplementationStatus.UNSUPPORTED,
        }

    def test_never_claims_real_lidar_workloads(self):
        support = SyntheticAdapter().support_for("pixel_metrics_1a")
        assert support.status is ImplementationStatus.UNSUPPORTED

    def test_missing_interpreter_is_unavailable(self, layout):
        adapter = SyntheticAdapter(python=layout["out"] / "no such python")
        plan = plan_invocation(adapter, invocation(layout, CHECKSUM))
        assert plan.status is PlanStatus.SKIPPED_UNAVAILABLE
        assert "not_installed" in plan.reason


class TestArgumentConstruction:
    def test_checksum_argv_is_structured(self, layout):
        plan = plan_invocation(SyntheticAdapter(), invocation(layout, CHECKSUM))
        assert plan.ready and plan.command is not None
        argv = plan.command.argv
        assert Path(argv[0]).is_absolute()
        assert argv[1:] == (
            "-m",
            WORKER_MODULE,
            "checksum",
            "--output-dir",
            str(layout["out"]),
            str(layout["a"]),
            str(layout["b"]),
        )
        assert plan.command.cwd == layout["work"]
        assert plan.command.env["OMP_NUM_THREADS"] == "1"
        assert plan.command.config_files == ()

    def test_hashchain_uses_config_file(self, layout):
        inv = invocation(layout, HASHCHAIN, parameters={"iterations": 3})
        plan = plan_invocation(SyntheticAdapter(), inv)
        assert plan.command is not None
        assert plan.command.argv[-2:] == ("--job", str(layout["work"] / JOB_FILE_NAME))
        job = json.loads(plan.command.config_files[0].content)
        assert job["inputs"] == [str(layout["a"]), str(layout["b"])]
        assert job["iterations"] == 3
        assert plan.metadata == {"iterations": 3}

    @pytest.mark.parametrize(
        ("change", "reason"),
        [
            ({"threads": 2}, "2 thread(s)"),
            ({"parameters": {"iterations": 0}}, "1.."),
            ({"parameters": {"iterations": True}}, "integer"),
            ({"parameters": {"colour": "red"}}, "unknown parameters"),
        ],
    )
    def test_unsupported_combinations_are_explicit(self, layout, change, reason):
        inv = replace(invocation(layout, HASHCHAIN), **change)
        plan = plan_invocation(SyntheticAdapter(), inv)
        assert plan.status is PlanStatus.SKIPPED_UNSUPPORTED
        assert reason in plan.reason

    def test_duplicate_input_names_unsupported(self, layout, tmp_path):
        other = tmp_path / "elsewhere"
        other.mkdir()
        dup = other / layout["a"].name.upper()
        dup.write_bytes(b"x")
        inv = Invocation(CHECKSUM, (layout["a"], dup), layout["out"], layout["work"])
        plan = plan_invocation(SyntheticAdapter(), inv)
        assert plan.status is PlanStatus.SKIPPED_UNSUPPORTED
        assert "unique ignoring case" in plan.reason

    @pytest.mark.parametrize("workload", ["synthetic.blocked", "synthetic.unsupported"])
    def test_non_dispatchable(self, layout, workload):
        plan = plan_invocation(SyntheticAdapter(), invocation(layout, workload))
        assert not plan.ready and plan.reason


class TestKnownOutputs:
    def test_checksum_outputs_with_spaces_in_paths(self, layout):
        plan = plan_invocation(SyntheticAdapter(), invocation(layout, CHECKSUM))
        result = execute(plan, layout["work"])
        assert result.returncode == 0, result.stderr
        assert [a.path.exists() for a in plan.expected_outputs] == [True, True]
        first = plan.expected_outputs[0].path.read_text(encoding="utf-8")
        assert first == (
            '{"bytes":3,"input_name":"tile 1 (a).laz","schema":"lidarbench.synthetic-output/1",'
            f'"sha256":"{ABC_SHA256}","workload_id":"synthetic.checksum"}}\n'
        )
        second = json.loads(plan.expected_outputs[1].path.read_text(encoding="utf-8"))
        assert second["bytes"] == 0
        assert sorted(p.name for p in layout["out"].iterdir()) == sorted(
            a.path.name for a in plan.expected_outputs
        )

    def test_hashchain_is_deterministic_and_matches_golden(self, layout, tmp_path):
        assert hashchain_digest([ABC_SHA256], 3) == ABC_HASHCHAIN_3
        inv = invocation(layout, HASHCHAIN, "a", parameters={"iterations": 3})
        plan = plan_invocation(SyntheticAdapter(), inv)
        result = execute(plan, layout["work"])
        assert result.returncode == 0, result.stderr
        record = json.loads(plan.expected_outputs[0].path.read_text(encoding="utf-8"))
        assert record["digest"] == ABC_HASHCHAIN_3
        assert record["inputs"] == [{"name": "tile 1 (a).laz", "sha256": ABC_SHA256}]
        first_bytes = plan.expected_outputs[0].path.read_bytes()

        # A second attempt in fresh directories yields byte-identical output.
        out2, work2 = tmp_path / "second out", tmp_path / "second work"
        out2.mkdir()
        work2.mkdir()
        plan2 = plan_invocation(SyntheticAdapter(), replace(inv, output_dir=out2, work_dir=work2))
        assert execute(plan2, work2).returncode == 0
        assert plan2.expected_outputs[0].path.read_bytes() == first_bytes

    def test_worker_failure_is_reported_by_exit_code(self, layout):
        inv = invocation(layout, CHECKSUM)
        plan = plan_invocation(SyntheticAdapter(), inv)
        layout["a"].unlink()
        result = execute(plan, layout["work"])
        assert result.returncode == 1
        assert "ERROR" in result.stderr

    def test_worker_runs_in_interpreter_without_shell(self, layout):
        plan = plan_invocation(SyntheticAdapter(), invocation(layout, CHECKSUM))
        assert plan.command is not None
        assert Path(plan.command.argv[0]) == Path(sys.executable).absolute()
