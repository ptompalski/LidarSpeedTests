"""Tests for planning: adapter output -> ready command or explicit skip."""

from __future__ import annotations

import json
from dataclasses import replace
from pathlib import Path

import pytest
from adapter_fakes import FakePdalAdapter, FakeRscriptAdapter, FakeWindowsOnlyAdapter

from lidarbench.core.platform import PlatformInfo
from lidarbench.execution.adapters import (
    Availability,
    CommandSpec,
    ConfigFile,
    ImplementationStatus,
    Invocation,
    UnavailableReason,
    UnsupportedCombinationError,
)
from lidarbench.execution.plan import (
    AdapterContractError,
    PlanStatus,
    plan_invocation,
    validate_command_spec,
    write_config_files,
)

LINUX = PlatformInfo("Linux", "6.0", "x86_64", "3.11.0", 4, 8 * 2**30)


@pytest.fixture
def spaced(tmp_path: Path) -> Invocation:
    root = tmp_path / "dir with spaces"
    (root / "out put").mkdir(parents=True)
    (root / "work dir").mkdir()
    return Invocation(
        "demo.metrics",
        (root / "tile one.laz", root / "tile;two$(x).laz"),
        root / "out put",
        root / "work dir",
    )


class TestSkips:
    def test_missing_software_is_skipped_unavailable(self, spaced):
        plan = plan_invocation(FakeRscriptAdapter(), spaced, platform_info=LINUX)
        assert plan.status is PlanStatus.SKIPPED_UNAVAILABLE
        assert not plan.ready and plan.command is None
        assert plan.reason.startswith("not_installed:")

    def test_platform_unsupported(self, spaced):
        plan = plan_invocation(FakeWindowsOnlyAdapter(), spaced, platform_info=LINUX)
        assert plan.status is PlanStatus.SKIPPED_UNAVAILABLE
        assert "platform_unsupported" in plan.reason

    def test_probe_exception_is_unavailable(self, spaced):
        class Exploding(FakePdalAdapter):
            def probe(self, platform_info):
                raise RuntimeError("boom")

        plan = plan_invocation(Exploding(), spaced, platform_info=LINUX)
        assert plan.status is PlanStatus.SKIPPED_UNAVAILABLE
        assert "probe_failed" in plan.reason and "boom" in plan.reason

    def test_probe_wrong_return_type_is_unavailable(self, spaced):
        class ReturnsBool(FakePdalAdapter):
            def probe(self, platform_info):
                return True

        plan = plan_invocation(ReturnsBool(), spaced, platform_info=LINUX)
        assert plan.status is PlanStatus.SKIPPED_UNAVAILABLE
        assert "returned bool" in plan.reason

    def test_unsupported_and_blocked(self, spaced):
        adapter = FakePdalAdapter()
        avail = Availability.ok()
        blocked = plan_invocation(adapter, replace(spaced, workload_id="demo.tin"), avail)
        assert blocked.status is PlanStatus.SKIPPED_BLOCKED
        assert blocked.implementation_status is ImplementationStatus.BLOCKED
        assert blocked.reason

        unknown = plan_invocation(adapter, replace(spaced, workload_id="generate_DSM2"), avail)
        assert unknown.status is PlanStatus.SKIPPED_UNSUPPORTED
        assert "declares no implementation" in unknown.reason

    def test_unsupported_thread_count(self, spaced):
        plan = plan_invocation(FakePdalAdapter(), replace(spaced, threads=4), Availability.ok())
        assert plan.status is PlanStatus.SKIPPED_UNSUPPORTED
        assert "4 thread(s)" in plan.reason

    def test_build_time_unsupported_combination(self, spaced):
        class Picky(FakePdalAdapter):
            def build_command(self, invocation):
                raise UnsupportedCombinationError("LAS 1.0 input not supported")

        plan = plan_invocation(Picky(), spaced, Availability.ok())
        assert plan.status is PlanStatus.SKIPPED_UNSUPPORTED
        assert plan.reason == "LAS 1.0 input not supported"


class TestReady:
    def test_config_file_tool(self, spaced):
        plan = plan_invocation(FakePdalAdapter(), spaced, Availability.ok())
        assert plan.ready
        assert plan.implementation_status is ImplementationStatus.RUNNABLE
        cmd = plan.command
        assert cmd is not None
        assert cmd.argv[1:] == ("pipeline", str(spaced.work_dir / "pipeline.json"))
        assert plan.expected_outputs[0].path == spaced.output_dir / "metrics.tif"
        assert plan.metadata == {"pipeline_file": "pipeline.json"}

        [written] = write_config_files(cmd, spaced.work_dir)
        pipeline = json.loads(written.read_text(encoding="utf-8"))["pipeline"]
        assert [s.get("filename") for s in pipeline[:2]] == [str(p) for p in spaced.inputs]
        with pytest.raises(FileExistsError):
            write_config_files(cmd, spaced.work_dir)

    def test_rscript_style_when_installed(self, spaced, tmp_path):
        import sys

        # Any existing executable stands in for Rscript; the command is not run.
        adapter = FakeRscriptAdapter(rscript=sys.executable, script=str(tmp_path / "my bridge.R"))
        plan = plan_invocation(adapter, replace(spaced, threads=6), platform_info=LINUX)
        assert plan.ready and plan.command is not None
        argv = plan.command.argv
        assert argv[1] == "--vanilla"
        assert argv[2] == str(tmp_path / "my bridge.R")
        assert argv[3] == str(spaced.work_dir / "invocation.json")
        payload = json.loads(plan.command.config_files[0].content)
        assert payload["workers"] == 6
        assert plan.command.env["OMP_NUM_THREADS"] == "1"


class TestContractValidation:
    def _cmd(self, tmp_path: Path, **kw) -> CommandSpec:
        base: dict = {"argv": (str(tmp_path / "tool"),), "cwd": tmp_path}
        base.update(kw)
        return CommandSpec(**base)

    def test_valid(self, tmp_path):
        validate_command_spec(self._cmd(tmp_path, env={"OMP_NUM_THREADS": "1"}))

    @pytest.mark.parametrize(
        ("kwargs", "message"),
        [
            ({"argv": ()}, "non-empty"),
            ({"argv": ["tool"]}, "tuple"),
            ({"argv": ("relative-tool", "x")}, "absolute executable"),
            ({"argv": ("/bin/tool", 3)}, "not str"),
            ({"argv": ("/bin/tool", "a\x00b")}, "NUL"),
            ({"cwd": Path("relative")}, "cwd"),
            ({"env": {"BAD NAME": "1"}}, "environment variable name"),
            ({"env": {"OK": 1}}, "NUL-free string"),
            ({"config_files": (ConfigFile("../escape.json", "{}"),)}, "portable"),
            ({"config_files": (ConfigFile("NUL.json", "{}"),)}, "portable"),
            ({"config_files": (ConfigFile("a.json", ""), ConfigFile("A.JSON", ""))}, "duplicate"),
        ],
    )
    def test_violations(self, tmp_path, kwargs, message):
        cmd = self._cmd(tmp_path, **kwargs)
        with pytest.raises(AdapterContractError, match=message):
            validate_command_spec(cmd)

    def test_output_outside_output_dir_rejected(self, spaced):
        from lidarbench.execution.adapters import ArtifactKind, ArtifactSpec

        class Escaping(FakePdalAdapter):
            def expected_outputs(self, invocation):
                return [ArtifactSpec(invocation.output_dir / ".." / "x.tif", ArtifactKind.RASTER)]

        with pytest.raises(AdapterContractError, match="outside"):
            plan_invocation(Escaping(), spaced, Availability.ok())

    def test_unexpected_adapter_exception_is_contract_error(self, spaced):
        class Buggy(FakePdalAdapter):
            def build_command(self, invocation):
                raise KeyError("oops")

        with pytest.raises(AdapterContractError, match="KeyError"):
            plan_invocation(Buggy(), spaced, Availability.ok())

    def test_unavailable_reason_enum_round_trip(self):
        a = Availability.unavailable(UnavailableReason.MISCONFIGURED, "bad config")
        assert f"{a.reason}" == "misconfigured"
