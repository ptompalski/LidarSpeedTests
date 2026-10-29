"""End-to-end: synthetic adapter -> plan -> execute_plan with the psutil monitor."""

from __future__ import annotations

import json
import os
import sys
import textwrap
import threading
import time
from pathlib import Path

import psutil
import pytest

from lidarbench.execution.adapters.base import CommandSpec, Invocation
from lidarbench.execution.adapters.synthetic import CHECKSUM, HASHCHAIN, SyntheticAdapter
from lidarbench.execution.adapters.synthetic_worker import hashchain_digest
from lidarbench.execution.attempt import EXECUTION_RECORD, execute_plan
from lidarbench.execution.plan import plan_invocation
from lidarbench.execution.process import CancellationToken, ExecutionStatus, run_process
from lidarbench.monitoring.base import MonitorStartError
from lidarbench.monitoring.psutil_backend import PsutilBackend

pytestmark = [pytest.mark.integration, pytest.mark.timeout(180)]

ABC_SHA256 = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"


@pytest.fixture
def layout(tmp_path: Path) -> dict[str, Path]:
    root = tmp_path / "campaign root"
    paths = {
        "inputs": root / "input files",
        "out": root / "attempt 01" / "out put",
        "work": root / "attempt 01" / "work dir",
        "attempt": root / "attempt 01" / "record dir",
    }
    for d in paths.values():
        d.mkdir(parents=True)
    paths["a"] = paths["inputs"] / "tile 1 (a).laz"
    paths["a"].write_bytes(b"abc")
    return paths


def invocation(layout, workload, **kw) -> Invocation:
    return Invocation(workload, (layout["a"],), layout["out"], layout["work"], **kw)


def backend(**kw) -> PsutilBackend:
    kw.setdefault("interval_s", 0.1)
    return PsutilBackend(**kw)


def load_record(attempt: Path) -> dict:
    return json.loads((attempt / EXECUTION_RECORD).read_text(encoding="utf-8"))


class TestSyntheticAttempt:
    def test_hashchain_success_with_monitoring(self, layout):
        inv = invocation(layout, HASHCHAIN, parameters={"iterations": 300_000})
        plan = plan_invocation(SyntheticAdapter(), inv)
        result = execute_plan(plan, inv, layout["attempt"], monitor_backend=backend())
        assert result.execution_status is ExecutionStatus.SUCCEEDED
        output = json.loads((layout["out"] / "hashchain.json").read_text(encoding="utf-8"))
        assert output["digest"] == hashchain_digest([ABC_SHA256], 300_000)
        assert result.missing_required_outputs == ()

        record = load_record(layout["attempt"])
        assert record["execution_status"] == "succeeded"
        process = record["process"]
        mon = process["monitoring"]
        assert mon["monitoring_status"] == "available", mon["reasons"]
        tree = mon["process_tree"]
        assert tree["scope"] == "process-tree" and tree["coverage_fraction"] == 1.0
        assert tree["metrics"]["cpu_seconds"]["total"] > 0
        assert tree["metrics"]["rss_bytes"]["peak"] > 1_000_000
        assert mon["whole_machine"]["metrics"]["memory_total_bytes"]["peak"] > 0
        assert process["environment"]["thread_budget"]["requested_threads"] == 1
        assert process["environment"]["variables"]["OMP_NUM_THREADS"]["source"] == "delta"

        monitor_dir = layout["attempt"] / "monitor"
        assert (monitor_dir / "machine-samples.csv").stat().st_size > 0
        assert (monitor_dir / "process-tree-samples.csv").stat().st_size > 0

    def test_untimed_phases_are_outside_the_timed_interval(self, layout):
        inv = invocation(layout, CHECKSUM)
        plan = plan_invocation(SyntheticAdapter(), inv)
        result = execute_plan(plan, inv, layout["attempt"], monitor_backend=backend())
        phases = {p.name: p for p in result.phases}
        assert list(phases) == [
            "input_verification",
            "config_files",
            "monitor_start",
            "process_supervision",
            "output_check",
        ]
        timed_start, timed_end = result.process.start.perf_ns, result.process.end.perf_ns
        for name in ("input_verification", "config_files", "monitor_start"):
            assert phases[name].end.perf_ns <= timed_start
        assert phases["output_check"].start.perf_ns >= timed_end
        sup = phases["process_supervision"]
        assert sup.start.perf_ns <= timed_start and timed_end <= sup.end.perf_ns
        assert result.inputs[0]["bytes"] == 3

    def test_worker_failure_is_failed(self, layout):
        inv = invocation(layout, CHECKSUM)
        plan = plan_invocation(SyntheticAdapter(), inv)
        (layout["out"] / "tile 1 (a).laz.checksum.json").write_text("pre-existing")
        result = execute_plan(plan, inv, layout["attempt"], monitor_backend=None)
        assert result.process.exit_code == 1
        assert result.execution_status is ExecutionStatus.FAILED
        assert "FileExistsError" in result.process.stderr_text()
        assert load_record(layout["attempt"])["process"]["monitoring"]["monitoring_status"] == (
            "disabled"
        )

    def test_missing_input_is_not_launched(self, layout):
        inv = Invocation(
            CHECKSUM, (layout["inputs"] / "missing.laz",), layout["out"], layout["work"]
        )
        plan = plan_invocation(SyntheticAdapter(), inv)
        result = execute_plan(plan, inv, layout["attempt"], monitor_backend=backend())
        assert result.process is None
        assert result.failure_phase == "input_verification"
        assert result.execution_status is ExecutionStatus.FAILED
        assert not (layout["attempt"] / "monitor").exists()
        assert load_record(layout["attempt"])["execution_status"] == "failed"

    def test_timeout_through_execute_plan(self, layout):
        inv = invocation(layout, HASHCHAIN, parameters={"iterations": 100_000_000})
        plan = plan_invocation(SyntheticAdapter(), inv)
        result = execute_plan(
            plan, inv, layout["attempt"], timeout_s=1.0, monitor_backend=backend()
        )
        assert result.execution_status is ExecutionStatus.TIMED_OUT
        assert result.process.termination.complete
        assert load_record(layout["attempt"])["execution_status"] == "timed_out"
        mon = result.process.monitoring
        assert mon["process_tree"]["metrics"]["cpu_seconds"]["total"] > 0.3

    def test_cancellation_through_execute_plan(self, layout):
        inv = invocation(layout, HASHCHAIN, parameters={"iterations": 100_000_000})
        plan = plan_invocation(SyntheticAdapter(), inv)
        token = CancellationToken()
        threading.Timer(1.0, token.cancel, args=("operator",)).start()
        result = execute_plan(plan, inv, layout["attempt"], cancel=token)
        assert result.execution_status is ExecutionStatus.INTERRUPTED
        assert load_record(layout["attempt"])["process"]["cancel_reason"] == "operator"

    def test_record_is_write_once(self, layout):
        inv = invocation(layout, CHECKSUM)
        plan = plan_invocation(SyntheticAdapter(), inv)
        execute_plan(plan, inv, layout["attempt"])
        with pytest.raises(FileExistsError):
            execute_plan(plan, inv, layout["attempt"])

    def test_skip_plans_are_rejected(self, layout):
        inv = invocation(layout, "synthetic.blocked")
        plan = plan_invocation(SyntheticAdapter(), inv)
        with pytest.raises(ValueError, match="not ready"):
            execute_plan(plan, inv, layout["attempt"])


class TestMonitoringFailures:
    def test_sampler_never_ready(self, tmp_path):
        slow = backend(
            sampler_argv=(sys.executable, "-c", "import time; time.sleep(60)"),
            startup_timeout_s=1.0,
        )
        with pytest.raises(MonitorStartError, match="not ready"):
            slow.start(tmp_path / "mon", "a")

    def test_sampler_crashes_at_startup(self, tmp_path):
        broken = backend(sampler_argv=(sys.executable, "-c", "raise SystemExit(3)"))
        with pytest.raises(MonitorStartError, match="startup"):
            broken.start(tmp_path / "mon", "a")

    def test_required_monitoring_failure_prevents_launch(self, layout):
        inv = invocation(layout, CHECKSUM)
        plan = plan_invocation(SyntheticAdapter(), inv)
        broken = backend(sampler_argv=(sys.executable, "-c", "raise SystemExit(3)"))
        result = execute_plan(plan, inv, layout["attempt"], monitor_backend=broken)
        assert result.process is None
        assert result.failure_phase == "monitor_start"
        assert result.execution_status is ExecutionStatus.FAILED
        assert not list(layout["out"].iterdir())

    def test_optional_monitoring_failure_runs_with_unavailable_monitoring(self, layout):
        inv = invocation(layout, CHECKSUM)
        plan = plan_invocation(SyntheticAdapter(), inv)
        broken = backend(sampler_argv=(sys.executable, "-c", "raise SystemExit(3)"))
        result = execute_plan(
            plan, inv, layout["attempt"], monitor_backend=broken, monitor_required=False
        )
        assert result.execution_status is ExecutionStatus.SUCCEEDED
        mon = result.process.monitoring
        assert mon["monitoring_status"] == "unavailable"
        assert mon["process_tree"] is None and mon["whole_machine"] is None
        assert any("without monitoring" in w for w in result.warnings)

    def test_sampler_dying_mid_run_does_not_change_execution_status(self, tmp_path):
        monitor = backend().start(tmp_path / "mon", "a")
        threading.Timer(0.5, lambda: psutil.Process(monitor.pid).kill()).start()
        code = "import time; time.sleep(2)"
        result = run_process(
            CommandSpec((sys.executable, "-c", code), tmp_path),
            stdout_path=tmp_path / "o",
            stderr_path=tmp_path / "e",
            monitor=monitor,
        )
        assert result.succeeded
        mon = result.monitoring
        assert mon["monitoring_status"] in {"partial", "unavailable"}
        assert mon["reasons"]
        if mon["process_tree"] is not None:
            assert mon["process_tree"]["coverage_fraction"] < 1.0
            cpu = mon["process_tree"]["metrics"]["cpu_seconds"]
            assert cpu["status"] != "available"

    def test_monitor_is_stopped_when_run_rejects_arguments(self, tmp_path):
        monitor = backend().start(tmp_path / "mon", "a")
        with pytest.raises(ValueError):
            run_process(
                CommandSpec((sys.executable, "-c", "pass"), tmp_path),
                stdout_path=tmp_path / "o",
                stderr_path=tmp_path / "e",
                monitor=monitor,
                timeout_s=-1,
            )
        assert not psutil.pid_exists(monitor.pid) or (
            psutil.Process(monitor.pid).status() == psutil.STATUS_ZOMBIE
        )

    def test_monitor_dir_is_never_reused(self, tmp_path):
        (tmp_path / "mon").mkdir()
        (tmp_path / "mon" / "old.csv").write_text("x")
        with pytest.raises(MonitorStartError, match="non-empty"):
            backend().start(tmp_path / "mon", "a")

    def test_invalid_interval(self):
        with pytest.raises(ValueError):
            PsutilBackend(interval_s=0.001)


class TestProcessTreeMonitoring:
    def test_children_are_observed(self, tmp_path):
        body = textwrap.dedent("""
            import subprocess, sys
            busy = "import time\\nt=time.time()\\nwhile time.time()-t<1.5: pass"
            kids = [subprocess.Popen([sys.executable, "-c", busy]) for _ in range(2)]
            sys.exit(max(k.wait() for k in kids))
            """)
        path = tmp_path / "parent.py"
        path.write_text(body, encoding="utf-8")
        monitor = backend().start(tmp_path / "mon", "a")
        result = run_process(
            CommandSpec((sys.executable, os.fspath(path)), tmp_path),
            stdout_path=tmp_path / "o",
            stderr_path=tmp_path / "e",
            monitor=monitor,
        )
        assert result.succeeded
        tree = result.monitoring["process_tree"]
        assert tree["metrics"]["process_count"]["peak"] >= 3
        assert tree["observed_processes"] >= 3
        # Two busy children: well above one core on multi-core hosts, never 0.
        assert tree["metrics"]["cpu_percent"]["peak"] > 50
        assert tree["metrics"]["cpu_seconds"]["total"] > 1.0

    def test_root_exit_tail_is_flagged_not_reported_as_complete(self, tmp_path):
        busy = "import time\nt=time.process_time()\nwhile time.process_time()-t<0.4: pass"
        monitor = backend(interval_s=0.5).start(tmp_path / "mon", "a")
        result = run_process(
            CommandSpec((sys.executable, "-c", busy), tmp_path),
            stdout_path=tmp_path / "o",
            stderr_path=tmp_path / "e",
            monitor=monitor,
        )
        tree = result.monitoring["process_tree"]
        if tree is None or tree["status"] == "unavailable":
            pytest.skip("root exited before the sampler attached")
        cpu = tree["metrics"]["cpu_seconds"]
        assert cpu["status"] != "available"
        assert cpu["values_are_lower_bounds"] and cpu["total_is_lower_bound"]
        assert tree["metrics"]["rss_bytes"]["values_are_lower_bounds"]
        if result.accounting is not None:
            kernel = result.accounting["cpu_user_seconds"] + result.accounting["cpu_system_seconds"]
            assert kernel >= 0.35 and cpu["total"] <= kernel + 0.05

    @pytest.mark.skipif(sys.platform != "darwin", reason="macOS I/O counter gap")
    def test_macos_io_is_null_not_zero(self, tmp_path):
        monitor = backend().start(tmp_path / "mon", "a")
        result = run_process(
            CommandSpec((sys.executable, "-c", "import time; time.sleep(0.5)"), tmp_path),
            stdout_path=tmp_path / "o",
            stderr_path=tmp_path / "e",
            monitor=monitor,
        )
        io = result.monitoring["process_tree"]["metrics"]["io_read_bytes"]
        assert io["total"] is None and io["status"] == "unavailable"

    def test_short_process_gets_samples(self, tmp_path):
        monitor = backend().start(tmp_path / "mon", "a")
        start = time.monotonic()
        result = run_process(
            CommandSpec((sys.executable, "-c", "pass"), tmp_path),
            stdout_path=tmp_path / "o",
            stderr_path=tmp_path / "e",
            monitor=monitor,
        )
        assert time.monotonic() - start < 30
        assert result.monitoring["whole_machine"]["coverage_fraction"] == 1.0
        tree = result.monitoring["process_tree"]
        if tree["status"] == "unavailable":
            # Exited before the sampler attached: values must be missing, not zero.
            assert tree["metrics"]["cpu_seconds"]["total"] is None
        else:
            assert tree["metrics"]["cpu_seconds"]["total"] is not None
