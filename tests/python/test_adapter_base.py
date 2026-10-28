"""Tests for the adapter interface types and default behaviour."""

from __future__ import annotations

from pathlib import Path

import pytest
from adapter_fakes import FakePdalAdapter, FakeWindowsOnlyAdapter

from lidarbench.execution.adapters import (
    Availability,
    ImplementationStatus,
    Invocation,
    ThreadingModel,
    ThreadingSupport,
    UnavailableReason,
    WorkloadSupport,
    argv_value,
    thread_cap_env,
)


def make_invocation(tmp_path: Path, workload: str = "demo.metrics", **kw) -> Invocation:
    inp = tmp_path / "in.laz"
    return Invocation(workload, (inp,), tmp_path / "out", tmp_path / "work", **kw)


class TestAvailability:
    def test_unavailable_requires_reason_and_detail(self):
        with pytest.raises(ValueError):
            Availability(False)
        with pytest.raises(ValueError):
            Availability(False, UnavailableReason.NOT_INSTALLED, "")

    def test_available_cannot_have_reason(self):
        with pytest.raises(ValueError):
            Availability(True, UnavailableReason.NOT_INSTALLED, "x")

    def test_constructors(self):
        assert Availability.ok().available
        missing = Availability.unavailable(UnavailableReason.NOT_INSTALLED, "no tool")
        assert not missing.available
        assert missing.reason == "not_installed"


class TestWorkloadSupport:
    def test_non_runnable_status_requires_reason(self):
        for status in ("diagnostic_only", "unsupported", "blocked"):
            with pytest.raises(ValueError, match="requires a reason"):
                WorkloadSupport("w", ImplementationStatus(status))

    def test_invalid_workload_id(self):
        with pytest.raises(ValueError):
            WorkloadSupport("bad id", ImplementationStatus.RUNNABLE)

    def test_dispatchable(self):
        assert ImplementationStatus.RUNNABLE.dispatchable
        assert ImplementationStatus.DIAGNOSTIC_ONLY.dispatchable
        assert not ImplementationStatus.UNSUPPORTED.dispatchable
        assert not ImplementationStatus.BLOCKED.dispatchable


class TestThreadingSupport:
    def test_single_threaded_must_cap_at_one(self):
        with pytest.raises(ValueError):
            ThreadingSupport(ThreadingModel.SINGLE_THREADED, 1, 4)

    def test_ranges(self):
        bounded = ThreadingSupport(ThreadingModel.NATIVE_THREADS, 2, 8)
        assert not bounded.accepts(1) and bounded.accepts(2) and bounded.accepts(8)
        assert not bounded.accepts(9)
        assert bounded.describe_range() == "2-8"
        unbounded = ThreadingSupport(ThreadingModel.WORKER_PROCESSES, 1, None)
        assert unbounded.accepts(512)
        assert unbounded.describe_range() == "1+"


class TestInvocation:
    def test_requires_absolute_paths(self, tmp_path):
        with pytest.raises(ValueError, match="absolute"):
            Invocation("w", (Path("rel.laz"),), tmp_path, tmp_path)

    def test_requires_inputs_and_threads(self, tmp_path):
        with pytest.raises(ValueError):
            Invocation("w", (), tmp_path, tmp_path)
        with pytest.raises(ValueError):
            Invocation("w", (tmp_path / "a",), tmp_path, tmp_path, threads=0)

    def test_config_path_is_in_work_dir(self, tmp_path):
        inv = make_invocation(tmp_path)
        assert inv.config_path("job.json") == tmp_path / "work" / "job.json"


class TestSupportFor:
    def test_declared(self):
        support = FakePdalAdapter().support_for("demo.metrics")
        assert support.status is ImplementationStatus.RUNNABLE

    def test_undeclared_workload_is_explicitly_unsupported(self):
        support = FakePdalAdapter().support_for("pixel_metrics_1a")
        assert support.status is ImplementationStatus.UNSUPPORTED
        assert "declares no implementation" in support.reason

    def test_thread_count_outside_range_is_unsupported(self):
        adapter = FakeWindowsOnlyAdapter()
        assert adapter.support_for("demo.metrics", 8).status is ImplementationStatus.DIAGNOSTIC_ONLY
        support = adapter.support_for("demo.metrics", 16)
        assert support.status is ImplementationStatus.UNSUPPORTED
        assert "16 thread(s)" in support.reason and "1-8" in support.reason

    def test_blocked_keeps_its_reason(self):
        support = FakePdalAdapter().support_for("demo.tin", threads=4)
        assert support.status is ImplementationStatus.BLOCKED
        assert "triangulation" in support.reason


class TestArgvHelpers:
    def test_argv_value(self, tmp_path):
        assert argv_value(3) == "3"
        assert argv_value(0.1) == "0.1"
        assert argv_value(-1234.5678901234567) == "-1234.5678901234567"
        assert argv_value(tmp_path / "a b") == str(tmp_path / "a b")
        assert argv_value("x y") == "x y"
        with pytest.raises(TypeError):
            argv_value(True)
        with pytest.raises(TypeError):
            argv_value(None)  # type: ignore[arg-type]

    def test_thread_cap_env(self):
        env = thread_cap_env(2)
        assert env["OMP_NUM_THREADS"] == "2"
        assert env["OPENBLAS_NUM_THREADS"] == "2"
        with pytest.raises(ValueError):
            thread_cap_env(0)
