"""Process execution engine: completion, failures, timeouts, cancellation and trees."""

from __future__ import annotations

import json
import os
import signal
import subprocess
import sys
import textwrap
import threading
import time
from pathlib import Path

import psutil
import pytest

from lidarbench.core.clock import now
from lidarbench.execution.adapters.base import CommandSpec, thread_cap_env
from lidarbench.execution.plan import AdapterContractError
from lidarbench.execution.process import (
    CancellationToken,
    ExceptionRecord,
    ExecutionInterrupted,
    ExecutionStatus,
    ProcessOutcome,
    ProcessResult,
    ThreadBudget,
    cancel_on_signals,
    run_process,
)
from lidarbench.execution.process_tree import LaunchSetupError, make_tree_controller
from lidarbench.monitoring.base import MonitorReport

pytestmark = pytest.mark.timeout(120)

POSIX = sys.platform != "win32"
posix_only = pytest.mark.skipif(not POSIX, reason="POSIX signals")


def script(tmp_path: Path, name: str, body: str) -> Path:
    path = tmp_path / name
    path.write_text(textwrap.dedent(body), encoding="utf-8")
    return path


def command(cwd: Path, *args: str | os.PathLike[str], **env: str) -> CommandSpec:
    return CommandSpec((sys.executable, *map(os.fspath, args)), cwd, env=env)


def run(cmd: CommandSpec, tmp_path: Path, **kw) -> ProcessResult:
    logs = tmp_path / "logs"
    logs.mkdir(exist_ok=True)
    n = len(list(logs.iterdir()))
    kw.setdefault("descendant_poll_s", 0.1)
    kw.setdefault("terminate_grace_s", 1.0)
    return run_process(cmd, stdout_path=logs / f"{n}.out", stderr_path=logs / f"{n}.err", **kw)


def gone(pid: int) -> bool:
    try:
        return psutil.Process(pid).status() == psutil.STATUS_ZOMBIE
    except psutil.NoSuchProcess:
        return True


def wait_for_file(path: Path, timeout: float = 20.0) -> str:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if path.exists() and (text := path.read_text(encoding="utf-8")).endswith("\n"):
            return text
        time.sleep(0.02)
    raise AssertionError(f"{path} not written")


SPAWN_TREE = """
    import subprocess, sys, time
    from pathlib import Path
    out = Path(sys.argv[1])
    kids = [subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])
            for _ in range(2)]
    out.write_text(" ".join(str(k.pid) for k in kids) + "\\n", encoding="utf-8")
    time.sleep(60)
"""


class TestCompletion:
    def test_success_captures_streams_timing_and_environment(self, tmp_path):
        body = """
            import os, sys
            print("out é")
            print("err", file=sys.stderr)
            print(os.getcwd())
        """
        cmd = command(tmp_path, script(tmp_path, "ok.py", body), **thread_cap_env(3))
        before = now()
        result = run(cmd, tmp_path, thread_budget=ThreadBudget(3, "native_threads"))
        after = now()
        assert result.execution_status is ExecutionStatus.SUCCEEDED and result.succeeded
        assert result.outcome is ProcessOutcome.EXITED
        assert result.exit_code == 0 and result.termination_signal is None
        lines = result.stdout_text().splitlines()
        assert lines[0] == "out é"
        assert Path(lines[1]).resolve() == tmp_path.resolve()
        assert result.stderr_text().strip() == "err"
        assert result.stdout_bytes == result.stdout_path.stat().st_size
        assert before.perf_ns <= result.start.perf_ns < result.end.perf_ns <= after.perf_ns
        assert result.elapsed_ns == result.end.perf_ns - result.start.perf_ns > 0
        record = result.to_record()
        assert record["execution_mode"] == "fresh-process"
        assert record["timing_boundary"] == "process"
        assert record["monitoring"]["monitoring_status"] == "disabled"
        env = record["environment"]
        assert env["thread_budget"]["requested_threads"] == 3
        assert "measured" in env["thread_budget"]["semantics"]
        assert env["variables"]["OMP_NUM_THREADS"] == {"value": "3", "source": "delta"}
        assert env["env_delta"]["OMP_NUM_THREADS"] == "3"
        assert record["cleanup_complete"] is True
        json.dumps(record)

    def test_env_delta_reaches_child_and_orchestrator_env_is_untouched(self, tmp_path):
        cmd = command(
            tmp_path, "-c", "import os; print(os.environ['LB_TEST_VAR'])", LB_TEST_VAR="x y"
        )
        result = run(cmd, tmp_path)
        assert result.stdout_text().strip() == "x y"
        assert "LB_TEST_VAR" not in os.environ

    @pytest.mark.skipif(not POSIX, reason="wait4 accounting is POSIX")
    def test_posix_kernel_accounting_is_recorded(self, tmp_path):
        busy = "import time\nt=time.process_time()\nwhile time.process_time()-t<0.3: pass"
        result = run(command(tmp_path, "-c", busy), tmp_path)
        acc = result.to_record()["process_accounting"]
        assert acc["source"] == "wait4"
        assert acc["cpu_user_seconds"] + acc["cpu_system_seconds"] >= 0.25
        assert acc["max_rss_bytes"] > 1_000_000


class TestFailures:
    def test_nonzero_exit_is_failed(self, tmp_path):
        result = run(command(tmp_path, "-c", "import sys; sys.exit(3)"), tmp_path)
        assert result.outcome is ProcessOutcome.EXITED
        assert result.exit_code == 3
        assert result.execution_status is ExecutionStatus.FAILED
        assert not result.succeeded

    def test_uncaught_exception_is_failed_with_stderr(self, tmp_path):
        result = run(command(tmp_path, "-c", "raise RuntimeError('boom')"), tmp_path)
        assert result.execution_status is ExecutionStatus.FAILED
        assert "RuntimeError: boom" in result.stderr_text()

    @posix_only
    def test_killed_by_signal_is_failed(self, tmp_path):
        code = "import os, signal; os.kill(os.getpid(), signal.SIGKILL)"
        result = run(command(tmp_path, "-c", code), tmp_path)
        assert result.exit_code == -signal.SIGKILL
        assert result.termination_signal == "SIGKILL"
        assert result.execution_status is ExecutionStatus.FAILED

    def test_missing_executable_is_launch_failure(self, tmp_path):
        cmd = CommandSpec((os.fspath(tmp_path / "no such tool"),), tmp_path)
        result = run(cmd, tmp_path)
        assert result.outcome is ProcessOutcome.LAUNCH_FAILED
        assert result.execution_status is ExecutionStatus.FAILED
        assert not result.launched and result.end is None and result.elapsed_ns is None
        assert result.exceptions[0].phase == "launch"
        assert result.exceptions[0].type in {"FileNotFoundError", "PermissionError", "OSError"}

    def test_missing_cwd_is_launch_failure(self, tmp_path):
        result = run(command(tmp_path / "missing dir", "-c", "pass"), tmp_path)
        assert result.outcome is ProcessOutcome.LAUNCH_FAILED
        assert not result.succeeded

    def test_contract_violations_are_rejected_before_launch(self, tmp_path):
        with pytest.raises(AdapterContractError):
            run(CommandSpec(("python",), tmp_path), tmp_path)

    def test_logs_are_write_once(self, tmp_path):
        (tmp_path / "out").write_text("x")
        with pytest.raises(FileExistsError):
            run_process(
                command(tmp_path, "-c", "pass"),
                stdout_path=tmp_path / "out",
                stderr_path=tmp_path / "err",
            )
        assert (tmp_path / "out").read_text() == "x"

    @pytest.mark.parametrize("timeout", [0, -1, float("nan"), float("inf")])
    def test_invalid_timeout(self, tmp_path, timeout):
        with pytest.raises(ValueError):
            run(command(tmp_path, "-c", "pass"), tmp_path, timeout_s=timeout)

    def test_engine_error_kills_process_and_fails(self, tmp_path):
        class BrokenController(type(make_tree_controller())):  # type: ignore[misc]
            def after_launch(self, proc):
                super().after_launch(proc)
                raise LaunchSetupError("cannot control tree")

        result = run(
            command(tmp_path, "-c", "import time; time.sleep(60)"),
            tmp_path,
            controller_factory=BrokenController,
        )
        assert result.outcome is ProcessOutcome.ENGINE_ERROR
        assert result.execution_status is ExecutionStatus.FAILED
        assert result.termination is not None and result.termination.complete
        assert gone(result.pid)


class TestStatusInvariant:
    @pytest.mark.parametrize("outcome", list(ProcessOutcome))
    @pytest.mark.parametrize("exit_code", [None, 0, 1, -9])
    @pytest.mark.parametrize("with_exception", [False, True])
    def test_only_clean_exit_zero_succeeds(self, tmp_path, outcome, exit_code, with_exception):
        result = ProcessResult(
            outcome=outcome,
            argv=("x",),
            cwd=tmp_path,
            stdout_path=tmp_path / "o",
            stderr_path=tmp_path / "e",
            environment={},
            pid=1,
            exit_code=exit_code,
            start=now(),
            end=now(),
            exceptions=(ExceptionRecord("reap", "E", "m"),) if with_exception else (),
        )
        expected = outcome is ProcessOutcome.EXITED and exit_code == 0 and not with_exception
        assert result.succeeded is expected
        assert (result.to_record()["execution_status"] == "succeeded") is expected


class TestTimeouts:
    def test_timeout_kills_and_records(self, tmp_path):
        start = time.monotonic()
        result = run(
            command(tmp_path, "-c", "import time; time.sleep(60)"), tmp_path, timeout_s=0.5
        )
        assert time.monotonic() - start < 15
        assert result.outcome is ProcessOutcome.TIMED_OUT
        assert result.execution_status is ExecutionStatus.TIMED_OUT
        assert result.termination is not None and result.termination.complete
        assert result.termination.reason == "timeout"
        assert result.elapsed_s is not None and result.elapsed_s >= 0.5
        assert gone(result.pid)

    def test_timeout_kills_children(self, tmp_path):
        pids_file = tmp_path / "pids.txt"
        cmd = command(tmp_path, script(tmp_path, "tree.py", SPAWN_TREE), pids_file)
        result = run(cmd, tmp_path, timeout_s=3)
        assert result.outcome is ProcessOutcome.TIMED_OUT
        kids = [int(p) for p in pids_file.read_text().split()]
        assert len(kids) == 2
        assert all(gone(p) for p in kids)
        assert result.cleanup_complete

    @posix_only
    def test_sigterm_ignored_escalates_to_sigkill(self, tmp_path):
        body = """
            import signal, time
            signal.signal(signal.SIGTERM, signal.SIG_IGN)
            print("ready", flush=True)
            time.sleep(60)
        """
        result = run(
            command(tmp_path, script(tmp_path, "stubborn.py", body)),
            tmp_path,
            timeout_s=1.0,
            terminate_grace_s=0.5,
        )
        assert result.outcome is ProcessOutcome.TIMED_OUT
        assert result.termination_signal == "SIGKILL"
        actions = " ".join(result.termination.actions)
        assert "SIGTERM" in actions and "SIGKILL" in actions

    def test_fast_process_is_not_reported_as_timeout(self, tmp_path):
        result = run(command(tmp_path, "-c", "pass"), tmp_path, timeout_s=30)
        assert result.outcome is ProcessOutcome.EXITED and result.succeeded


class TestCancellation:
    def test_cancel_during_run(self, tmp_path):
        token = CancellationToken()
        threading.Timer(0.5, token.cancel, args=("user request",)).start()
        result = run(command(tmp_path, "-c", "import time; time.sleep(60)"), tmp_path, cancel=token)
        assert result.outcome is ProcessOutcome.CANCELLED
        assert result.execution_status is ExecutionStatus.INTERRUPTED
        assert result.cancel_reason == "user request"
        assert result.termination is not None and result.termination.complete
        assert gone(result.pid)

    def test_cancel_kills_children(self, tmp_path):
        token = CancellationToken()
        pids_file = tmp_path / "pids.txt"
        cmd = command(tmp_path, script(tmp_path, "tree.py", SPAWN_TREE), pids_file)
        watcher = threading.Thread(
            target=lambda: (wait_for_file(pids_file), token.cancel("stop")), daemon=True
        )
        watcher.start()
        result = run(cmd, tmp_path, cancel=token)
        assert result.execution_status is ExecutionStatus.INTERRUPTED
        assert all(gone(int(p)) for p in pids_file.read_text().split())

    def test_cancelled_before_launch_starts_nothing(self, tmp_path):
        token = CancellationToken()
        token.cancel("early")
        marker = tmp_path / "ran"
        code = f"open({os.fspath(marker)!r}, 'w').close()"
        result = run(command(tmp_path, "-c", code), tmp_path, cancel=token)
        assert not result.launched and not marker.exists()
        assert result.execution_status is ExecutionStatus.INTERRUPTED

    def test_keyboard_interrupt_cleans_up_and_carries_result(self, tmp_path):
        class InterruptingMonitor:
            def attach(self, pid, launch):
                raise KeyboardInterrupt

            def stop(self):
                return MonitorReport("fake", "a", 1.0, tmp_path, tmp_path / "m", tmp_path / "t")

        with pytest.raises(ExecutionInterrupted) as info:
            run(
                command(tmp_path, "-c", "import time; time.sleep(60)"),
                tmp_path,
                monitor=InterruptingMonitor(),
            )
        result = info.value.result
        assert isinstance(info.value, KeyboardInterrupt)
        assert result.execution_status is ExecutionStatus.INTERRUPTED
        assert gone(result.pid)

    @posix_only
    def test_sigint_becomes_cancellation(self, tmp_path):
        token = CancellationToken()
        threading.Timer(0.5, os.kill, args=(os.getpid(), signal.SIGINT)).start()
        with cancel_on_signals(token):
            result = run(
                command(tmp_path, "-c", "import time; time.sleep(60)"), tmp_path, cancel=token
            )
        assert result.execution_status is ExecutionStatus.INTERRUPTED
        assert result.cancel_reason == "received SIGINT"
        assert signal.getsignal(signal.SIGINT) is signal.default_int_handler

    @posix_only
    def test_repeated_ctrl_c_during_grace_escalates_and_keeps_result(self, tmp_path):
        body = """
            import signal, time
            signal.signal(signal.SIGTERM, signal.SIG_IGN)
            time.sleep(60)
        """
        timers = [
            threading.Timer(delay, os.kill, args=(os.getpid(), signal.SIGINT))
            for delay in (0.5, 1.0)
        ]
        started = time.monotonic()
        for t in timers:
            t.start()
        with pytest.raises(ExecutionInterrupted) as info:
            run(
                command(tmp_path, script(tmp_path, "stubborn.py", body)),
                tmp_path,
                terminate_grace_s=20.0,
            )
        for t in timers:
            t.join()
        result = info.value.result
        assert time.monotonic() - started < 15
        assert result.execution_status is ExecutionStatus.INTERRUPTED
        assert result.termination_signal == "SIGKILL"
        assert result.termination is not None and result.termination.complete
        assert gone(result.pid)


class TestChildProcesses:
    def test_orphaned_background_child_is_killed_after_normal_exit(self, tmp_path):
        body = """
            import subprocess, sys
            from pathlib import Path
            kid = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])
            Path(sys.argv[1]).write_text(f"{kid.pid}\\n", encoding="utf-8")
        """
        pid_file = tmp_path / "kid.txt"
        result = run(command(tmp_path, script(tmp_path, "orphan.py", body), pid_file), tmp_path)
        assert result.succeeded
        kid = int(pid_file.read_text())
        assert gone(kid)
        assert result.leftover_cleanup is not None and result.leftover_cleanup.complete
        assert any("outlived" in w for w in result.warnings)

    @posix_only
    def test_escaped_session_descendant_is_killed_on_timeout(self, tmp_path):
        body = """
            import subprocess, sys, time
            from pathlib import Path
            kid = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"],
                                   start_new_session=True)
            Path(sys.argv[1]).write_text(f"{kid.pid}\\n", encoding="utf-8")
            time.sleep(60)
        """
        pid_file = tmp_path / "kid.txt"
        result = run(
            command(tmp_path, script(tmp_path, "escape.py", body), pid_file),
            tmp_path,
            timeout_s=2.0,
        )
        assert result.outcome is ProcessOutcome.TIMED_OUT
        assert gone(int(pid_file.read_text()))

    def test_child_failure_propagated_by_parent_is_failed(self, tmp_path):
        body = """
            import subprocess, sys
            sys.exit(subprocess.call([sys.executable, "-c", "import sys; sys.exit(7)"]))
        """
        result = run(command(tmp_path, script(tmp_path, "wrap.py", body)), tmp_path)
        assert result.exit_code == 7 and not result.succeeded


class TestPathsWithSpaces:
    def test_cwd_and_arguments_with_spaces_and_metacharacters(self, tmp_path):
        work = tmp_path / "work dir; $HOME & (x)"
        work.mkdir()
        arg = "a b;c $HOME 'q' \"d\" & | > <"
        code = "import json, os, sys; print(json.dumps([os.getcwd(), sys.argv[1]]))"
        result = run(command(work, "-c", code, arg), tmp_path)
        cwd, received = json.loads(result.stdout_text())
        assert Path(cwd).resolve() == work.resolve()
        assert received == arg

    @posix_only
    def test_executable_path_with_spaces(self, tmp_path):
        exe_dir = tmp_path / "tool dir"
        exe_dir.mkdir()
        exe = exe_dir / "my tool"
        exe.write_text('#!/bin/sh\necho "$1"\nexit 4\n', encoding="utf-8")
        exe.chmod(0o755)
        result = run(CommandSpec((os.fspath(exe), "x y"), tmp_path), tmp_path)
        assert result.stdout_text() == "x y\n"
        assert result.exit_code == 4 and not result.succeeded

    def test_log_paths_with_spaces(self, tmp_path):
        logs = tmp_path / "log dir"
        logs.mkdir()
        result = run_process(
            command(tmp_path, "-c", "print('hi')"),
            stdout_path=logs / "std out.log",
            stderr_path=logs / "std err.log",
        )
        assert result.succeeded and result.stdout_text().strip() == "hi"


@pytest.mark.platform
@pytest.mark.skipif(sys.platform != "win32", reason="Windows Job Object")
def test_windows_job_object_strategy(tmp_path):
    result = run(command(tmp_path, "-c", "pass"), tmp_path)
    assert result.process_group["strategy"] == "windows-job"
    assert result.accounting is not None and result.accounting["source"] == "windows-job-object"


@pytest.mark.platform
@pytest.mark.skipif(not POSIX, reason="POSIX sessions")
def test_posix_session_strategy(tmp_path):
    code = "import os; print(os.getpid() == os.getpgid(0) == os.getsid(0))"
    result = run(command(tmp_path, "-c", code), tmp_path)
    assert result.process_group["strategy"] == "posix-session"
    assert result.stdout_text().strip() == "True"


def test_stdin_is_not_inherited(tmp_path):
    code = "import sys; print(repr(sys.stdin.read()))"
    result = run(command(tmp_path, "-c", code), tmp_path, timeout_s=20)
    assert result.succeeded and result.stdout_text().strip() == "''"


def test_subprocess_module_is_used_without_shell(tmp_path, monkeypatch):
    seen = {}
    real = subprocess.Popen

    def spy(*args, **kwargs):
        seen.update(kwargs, argv=args[0])
        return real(*args, **kwargs)

    monkeypatch.setattr(subprocess, "Popen", spy)
    run(command(tmp_path, "-c", "pass"), tmp_path)
    assert seen["shell"] is False and isinstance(seen["argv"], list)
    assert seen["cwd"] == os.fspath(tmp_path)
