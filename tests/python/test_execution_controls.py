"""CPU pinning, the supervisor-death guardian and session-wide process sampling."""

from __future__ import annotations

import os
import subprocess
import sys
import time

import psutil
import pytest

from lidarbench.execution import affinity
from lidarbench.execution.adapters.base import CommandSpec
from lidarbench.execution.guardian import Guardian
from lidarbench.execution.process import ThreadBudget, run_process

linux_only = pytest.mark.skipif(sys.platform != "linux", reason="sched_setaffinity is Linux-only")
posix_only = pytest.mark.skipif(sys.platform == "win32", reason="POSIX sessions")


def test_order_prefers_one_cpu_per_core_and_reserves_the_first_core():
    cores = [[0, 32], [1, 33], [2, 34], [3, 35]]
    assert affinity._order(cores, 1) == [1]
    assert affinity._order(cores, 3) == [1, 2, 3]
    assert affinity._order(cores, 4) == [0, 1, 2, 3]  # all cores needed: none reserved
    assert affinity._order(cores, 6) == [0, 1, 2, 3, 32, 33]  # SMT siblings last


def test_select_cpus_reports_unsupported_and_oversized_budgets(monkeypatch):
    with pytest.raises(ValueError):
        affinity.select_cpus(0)
    monkeypatch.setattr(affinity, "_allowed_cores", lambda: "CPU pinning is not supported on x")
    assert affinity.select_cpus(1) == (None, "CPU pinning is not supported on x")
    monkeypatch.setattr(affinity, "_allowed_cores", lambda: ([0, 1], [[0], [1]]))
    cpus, reason = affinity.select_cpus(3)
    assert cpus is None and "exceeds" in reason
    assert affinity.select_cpus(1) == (
        (1,),
        "one logical CPU per physical core; first core " "left to the orchestrator and monitor",
    )


def test_thread_budget_rejects_bad_cpu_sets():
    with pytest.raises(ValueError):
        ThreadBudget(2, cpu_set=(1, 1))
    with pytest.raises(ValueError):
        ThreadBudget(1, cpu_set=())
    assert ThreadBudget(2, cpu_set=(3, 4)).cpu_set == (3, 4)


@linux_only
def test_run_process_pins_the_whole_tree(tmp_path):
    allowed = sorted(os.sched_getaffinity(0))
    target = (allowed[-1],)
    script = (
        "import os, subprocess, sys;"
        "print(sorted(os.sched_getaffinity(0)));"
        "out = subprocess.run([sys.executable, '-c', "
        "'import os; print(sorted(os.sched_getaffinity(0)))'], capture_output=True, text=True);"
        "print(out.stdout.strip())"
    )
    result = run_process(
        CommandSpec(argv=(sys.executable, "-c", script), cwd=tmp_path),
        stdout_path=tmp_path / "out.log",
        stderr_path=tmp_path / "err.log",
        thread_budget=ThreadBudget(1, cpu_set=target, cpu_set_detail="test"),
    )
    assert result.succeeded, result.stderr_text()
    lines = result.stdout_text().split()
    assert lines == [str(list(target)), str(list(target))]  # root and its child
    record = result.to_record()
    assert record["cpu_affinity"]["status"] == "applied"
    assert record["cpu_affinity"]["cpus"] == list(target)
    assert record["environment"]["thread_budget"]["cpu_set"] == list(target)


def test_run_process_without_cpu_set_records_not_requested(tmp_path):
    result = run_process(
        CommandSpec(argv=(sys.executable, "-c", "pass"), cwd=tmp_path),
        stdout_path=tmp_path / "out.log",
        stderr_path=tmp_path / "err.log",
        thread_budget=ThreadBudget(1),
    )
    assert result.to_record()["cpu_affinity"]["status"] == "not_requested"


@posix_only
def test_guardian_kills_watched_groups_when_the_supervisor_disappears():
    victim = subprocess.Popen(
        [sys.executable, "-c", "import time; time.sleep(60)"], start_new_session=True
    )
    released = subprocess.Popen(
        [sys.executable, "-c", "import time; time.sleep(60)"], start_new_session=True
    )
    try:
        guardian = Guardian()
        guardian.watch(victim.pid)
        guardian.watch(released.pid)
        guardian.release(released.pid)
        guardian._pipe.close()  # what the OS does when the orchestrator dies
        assert victim.wait(timeout=10) == -9
        time.sleep(0.2)
        assert released.poll() is None
    finally:
        for proc in (victim, released):
            if proc.poll() is None:
                proc.kill()
                proc.wait()


@posix_only
def test_sampler_follows_session_members_reparented_away_from_the_tree(tmp_path):
    from lidarbench.monitoring.sampler import Sampler

    marker = tmp_path / "orphan.pid"
    # The shell starts a background sleeper and exits: the sleeper is reparented to
    # init (no longer a descendant) but stays in the session, like R parallelly workers.
    script = f"sleep 30 & echo $! > '{marker}'; sleep 3"
    root = subprocess.Popen(["sh", "-c", script], start_new_session=True)
    try:
        for _ in range(100):
            if marker.exists() and marker.read_text().strip():
                break
            time.sleep(0.05)
        orphan = int(marker.read_text())
        sampler = Sampler(tmp_path / "machine.csv", tmp_path / "tree.csv")
        sampler.attach(root.pid, os.getpid(), time.perf_counter_ns(), time.time_ns())
        found = {p.pid for p in sampler._discover()}
        assert {root.pid, orphan} <= found
        assert "session" in sampler.capabilities["membership"]
        sampler.close()
    finally:
        try:
            os.killpg(root.pid, 9)
        except ProcessLookupError:
            pass
        root.wait()
        if "orphan" in locals():
            try:
                psutil.Process(orphan).kill()
            except psutil.Error:
                pass
