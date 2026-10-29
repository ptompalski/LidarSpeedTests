"""The sampler, exercised in-process (it normally runs as a separate process)."""

from __future__ import annotations

import io
import itertools
import json
import os
import subprocess
import sys
import time
from pathlib import Path

import pytest

from lidarbench.core.clock import now
from lidarbench.execution.adapters.base import CommandSpec
from lidarbench.execution.process import ExecutionStatus, ProcessOutcome, run_process
from lidarbench.execution.process_tree import PsutilTreeController
from lidarbench.monitoring import sampler as sampler_mod
from lidarbench.monitoring.base import MACHINE_METRICS, TREE_METRICS
from lidarbench.monitoring.summary import read_samples

pytestmark = pytest.mark.timeout(120)

BUSY = "import time\nt=time.time()\nwhile time.time()-t<0.6: pass"


def child(code: str) -> subprocess.Popen[bytes]:
    return subprocess.Popen([sys.executable, "-c", code])


def test_sampler_records_both_scopes_and_exited_processes(tmp_path):
    proc = child(BUSY)
    launch = now()
    s = sampler_mod.Sampler(tmp_path / "m.csv", tmp_path / "t.csv")
    s.attach(proc.pid, os.getpid(), launch.perf_ns, launch.wall_ns)
    for _ in range(3):
        time.sleep(0.15)
        s.sample()
    proc.wait()
    time.sleep(0.05)
    s.sample()
    s.close()
    machine = read_samples(tmp_path / "m.csv", MACHINE_METRICS)
    tree = read_samples(tmp_path / "t.csv", TREE_METRICS)
    assert len(machine) == len(tree) == 4
    assert tree[0]["interval_start_perf_ns"] == launch.perf_ns
    assert all(r["memory_total_bytes"] > 0 for r in machine)
    assert tree[0]["new_processes"] == 1
    assert sum(r["exited_processes"] for r in tree) == 1
    assert tree[-1]["process_count"] == 0
    assert sum(r["cpu_seconds"] for r in tree) > 0.2
    for prev, cur in itertools.pairwise(tree):
        assert cur["interval_start_perf_ns"] == prev["interval_end_perf_ns"]


def test_unobservable_root_yields_null_metrics_not_zero(tmp_path):
    proc = child("pass")
    proc.wait()
    launch = now()
    s = sampler_mod.Sampler(tmp_path / "m.csv", tmp_path / "t.csv")
    s.attach(proc.pid, os.getpid(), launch.perf_ns, launch.wall_ns)
    time.sleep(0.02)
    s.sample()
    s.close()
    (row,) = read_samples(tmp_path / "t.csv", TREE_METRICS)
    assert row["cpu_seconds"] is None and row["rss_bytes"] is None
    assert row["process_count"] is None
    assert "not observable" in row["errors"]


def test_foreign_parent_is_rejected(tmp_path):
    proc = child("import time; time.sleep(5)")
    try:
        launch = now()
        s = sampler_mod.Sampler(tmp_path / "m.csv", tmp_path / "t.csv")
        s.attach(proc.pid, proc.pid + 999_999, launch.perf_ns, launch.wall_ns)
        time.sleep(0.02)
        s.sample()
        s.close()
        (row,) = read_samples(tmp_path / "t.csv", TREE_METRICS)
        assert row["cpu_seconds"] is None
    finally:
        proc.kill()
        proc.wait()


def test_protocol_loop(tmp_path, monkeypatch, capsys):
    proc = child("import time; time.sleep(0.3)")
    launch = now()
    commands = [
        {
            "cmd": "attach",
            "pid": proc.pid,
            "parent_pid": os.getpid(),
            "launch_perf_ns": launch.perf_ns,
            "launch_wall_ns": launch.wall_ns,
        },
        {"cmd": "bogus"},
    ]
    text = "".join(json.dumps(c) + "\n" for c in commands) + "not json\n"
    monkeypatch.setattr(sys, "stdin", io.StringIO(text))
    code = sampler_mod.main(
        [
            "--machine-csv",
            os.fspath(tmp_path / "m.csv"),
            "--tree-csv",
            os.fspath(tmp_path / "t.csv"),
            "--interval",
            "0.05",
        ]
    )
    proc.wait()
    assert code == 0
    events = [json.loads(line) for line in capsys.readouterr().out.splitlines()]
    assert events[0]["event"] == "ready"
    assert events[-1]["event"] == "stopped" and events[-1]["reason"] == "stdin closed"
    assert len(events[-1]["warnings"]) == 2
    assert events[-1]["tree_rows"] >= 1


def test_main_reports_errors(tmp_path, capsys):
    (tmp_path / "m.csv").write_text("exists")
    code = sampler_mod.main(
        [
            "--machine-csv",
            os.fspath(tmp_path / "m.csv"),
            "--tree-csv",
            os.fspath(tmp_path / "t.csv"),
            "--interval",
            "0.1",
        ]
    )
    assert code == 2
    assert json.loads(capsys.readouterr().out)["event"] == "error"


class TestPsutilFallbackController:
    """The Windows fallback strategy, exercised on any OS."""

    def run(self, tmp_path: Path, code: str, **kw):
        return run_process(
            CommandSpec((sys.executable, "-c", code), tmp_path),
            stdout_path=tmp_path / "o",
            stderr_path=tmp_path / "e",
            controller_factory=PsutilTreeController,
            descendant_poll_s=0.1,
            **kw,
        )

    def test_success(self, tmp_path):
        result = self.run(tmp_path, "pass")
        assert result.succeeded
        assert result.process_group["strategy"] == "psutil-tree"

    def test_timeout_kills_tracked_children(self, tmp_path):
        code = (
            "import subprocess, sys, time\n"
            "k = subprocess.Popen([sys.executable, '-c', 'import time; time.sleep(60)'])\n"
            f"open({os.fspath(tmp_path / 'pid')!r}, 'w').write(str(k.pid))\n"
            "time.sleep(60)\n"
        )
        result = self.run(tmp_path, code, timeout_s=2.0)
        assert result.outcome is ProcessOutcome.TIMED_OUT
        assert result.execution_status is ExecutionStatus.TIMED_OUT
        assert result.termination.complete
        import psutil

        kid = int((tmp_path / "pid").read_text())
        assert not psutil.pid_exists(kid) or psutil.Process(kid).status() == "zombie"
