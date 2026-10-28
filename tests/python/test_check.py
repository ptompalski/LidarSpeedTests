"""Tests for capability reporting and the ``lidarbench check`` command."""

from __future__ import annotations

import io
import json

from adapter_fakes import FakePdalAdapter, FakeRscriptAdapter, FakeWindowsOnlyAdapter

from lidarbench import cli
from lidarbench.check import run_check
from lidarbench.core.platform import PlatformInfo
from lidarbench.execution.adapters import AdapterRegistry, ImplementationStatus
from lidarbench.execution.adapters.capabilities import inspect_adapter, inspect_registry
from lidarbench.execution.adapters.synthetic import SyntheticAdapter

LINUX = PlatformInfo("Linux", "6.0", "x86_64", "3.11.0", 4, 8 * 2**30)


def registry_with(*classes) -> AdapterRegistry:
    registry = AdapterRegistry()
    for cls in classes:
        registry.register(cls)
    return registry


class TestInspect:
    def test_available_adapter(self):
        report = inspect_adapter(FakePdalAdapter(), LINUX)
        assert report.availability.available
        assert report.identity is not None and report.identity.version == "9.9.9"
        assert [w.workload_id for w in report.workloads] == ["demo.metrics", "demo.tin"]
        assert report.errors == ()
        data = report.to_dict()
        assert data["schema"] == "lidarbench.adapter/1"
        assert data["workloads"][1]["status"] == "blocked"
        json.dumps(data)

    def test_missing_tool_reported_unavailable_without_identity(self):
        report = inspect_adapter(FakeRscriptAdapter(), LINUX)
        assert not report.availability.available
        assert report.availability.reason == "not_installed"
        assert report.identity is None
        assert report.workloads  # capabilities are still declared

    def test_adapter_defects_are_contained(self):
        class BadIdentify(FakePdalAdapter):
            def identify(self):
                raise OSError("version command crashed")

        class BadWorkloads(FakePdalAdapter):
            def workload_support(self):
                return [*super().workload_support(), *super().workload_support()]

        report = inspect_adapter(BadIdentify(), LINUX)
        assert report.availability.reason == "probe_failed"
        assert "version command crashed" in report.errors[0]
        assert "duplicate" in inspect_adapter(BadWorkloads(), LINUX).errors[0]

    def test_invalid_identity_is_contained(self):
        from pathlib import Path

        from lidarbench.execution.adapters import ToolIdentity

        class PathDetail(FakePdalAdapter):
            def identify(self):
                return ToolIdentity("x", "1", details={"home": Path("/x")})

        class WrongType(FakePdalAdapter):
            def identify(self):
                return "1.0"

        for cls, message in ((PathDetail, "JSON scalar"), (WrongType, "returned str")):
            report = inspect_adapter(cls(), LINUX)
            assert report.identity is None
            assert report.availability.reason == "probe_failed"
            assert message in report.errors[0]
            json.dumps(report.to_dict())

    def test_constructor_failure(self):
        class BadInit(FakePdalAdapter):
            adapter_id = "bad-init"

            def __init__(self):
                raise RuntimeError("cannot construct")

        [report] = inspect_registry(registry_with(BadInit), LINUX)
        assert report.availability.reason == "probe_failed"
        assert "cannot construct" in report.errors[0]


class TestCheckCommand:
    def test_text_output(self):
        out = io.StringIO()
        registry = registry_with(SyntheticAdapter, FakeRscriptAdapter, FakeWindowsOnlyAdapter)
        code = run_check(registry=registry, platform_info=LINUX, out=out)
        text = out.getvalue()
        assert code == 0  # unavailable tools are not a failure
        assert "Operating system : Linux 6.0 (x86_64)" in text
        assert "synthetic: Synthetic test adapter" in text
        assert "status     : available" in text
        assert "UNAVAILABLE (not_installed)" in text
        assert "UNAVAILABLE (platform_unsupported)" in text
        assert "tool has no TIN rasterizer" in text
        assert "native_threads, threads 1-8" in text

    def test_json_output_and_filter(self):
        out = io.StringIO()
        registry = registry_with(SyntheticAdapter, FakePdalAdapter)
        code = run_check(
            as_json=True,
            adapter_ids=["fake-pdal"],
            registry=registry,
            platform_info=LINUX,
            out=out,
        )
        data = json.loads(out.getvalue())
        assert code == 0
        assert data["schema"] == "lidarbench.check/1"
        assert data["platform"]["system"] == "Linux"
        assert [a["adapter_id"] for a in data["adapters"]] == ["fake-pdal"]
        statuses = {w["workload_id"]: w["status"] for w in data["adapters"][0]["workloads"]}
        assert statuses == {
            "demo.metrics": ImplementationStatus.RUNNABLE.value,
            "demo.tin": ImplementationStatus.BLOCKED.value,
        }

    def test_unknown_adapter_filter(self):
        err = io.StringIO()
        code = run_check(adapter_ids=["nope"], registry=AdapterRegistry(), err=err)
        assert code == 2
        assert "unknown adapter" in err.getvalue()

    def test_load_failures_are_listed(self):
        from importlib.metadata import EntryPoint

        registry = AdapterRegistry.discover(
            [EntryPoint("broken", "lidarbench_no_such_module:X", "lidarbench.adapters")]
        )
        out = io.StringIO()
        assert run_check(registry=registry, platform_info=LINUX, out=out) == 0
        assert "failed to load" in out.getvalue()
        assert "none registered" in out.getvalue()

    def test_cli_check_uses_installed_adapters(self, capsys):
        assert cli.main(["check", "--json"]) == 0
        data = json.loads(capsys.readouterr().out)
        synthetic = next(a for a in data["adapters"] if a["adapter_id"] == "synthetic")
        assert synthetic["available"] is True
        assert cli.main(["check"]) == 0
        assert "synthetic" in capsys.readouterr().out
