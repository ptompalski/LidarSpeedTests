"""Exclusive lock files with stale-owner detection and explicit release."""

from __future__ import annotations

import json
import os
import socket
import subprocess
import sys

import pytest

from lidarbench.core.locks import (
    ExclusiveLock,
    LockError,
    LockHeldError,
    default_state_dir,
    inspect_lock,
    lock_state,
    release_lock,
)


def _dead_owner(path, host=None):
    proc = subprocess.Popen([sys.executable, "-c", "pass"])
    proc.wait()
    path.write_text(
        json.dumps(
            {
                "host": host or socket.gethostname(),
                "pid": proc.pid,
                "process_create_time": 1.0,
                "token": "x",
            }
        ),
        encoding="utf-8",
    )


def test_acquire_release_and_contention(tmp_path):
    path = tmp_path / "ws" / ".lidarbench.lock"
    lock = ExclusiveLock(path, "workspace")
    owner = lock.acquire()
    assert owner["pid"] == os.getpid() and path.exists()
    with pytest.raises(LockHeldError) as info:
        ExclusiveLock(path, "workspace").acquire()
    assert info.value.state == "active"
    lock.release()
    assert not path.exists()
    with ExclusiveLock(path, "workspace") as again:
        assert again.held
    assert not path.exists()


def test_release_refuses_foreign_lock(tmp_path):
    path = tmp_path / "lock"
    lock = ExclusiveLock(path, "x")
    lock.acquire()
    data = json.loads(path.read_text(encoding="utf-8"))
    data["token"] = "someone-else"
    os.chmod(path, 0o644)
    path.write_text(json.dumps(data), encoding="utf-8")
    with pytest.raises(LockError, match="no longer owned"):
        lock.release()
    assert path.exists()


def test_stale_lock_is_reported_not_removed(tmp_path):
    path = tmp_path / "lock"
    _dead_owner(path)
    with pytest.raises(LockHeldError) as info:
        ExclusiveLock(path, "x").acquire()
    assert info.value.state == "stale"
    assert "lock release --confirm" in str(info.value)
    assert path.exists()
    report = release_lock(path)
    assert report.released and not path.exists()


def test_active_lock_cannot_be_released(tmp_path):
    path = tmp_path / "lock"
    lock = ExclusiveLock(path, "x")
    lock.acquire()
    with pytest.raises(LockHeldError):
        release_lock(path, force=True)
    assert inspect_lock(path).state == "active"
    lock.release()


def test_other_host_needs_force(tmp_path):
    path = tmp_path / "lock"
    _dead_owner(path, host="another-host.invalid")
    assert lock_state(json.loads(path.read_text(encoding="utf-8"))) == "unverifiable"
    with pytest.raises(LockHeldError):
        release_lock(path)
    assert release_lock(path, force=True).released


def test_unreadable_lock_is_unverifiable(tmp_path):
    path = tmp_path / "lock"
    path.write_text("{garbage", encoding="utf-8")
    assert inspect_lock(path).state == "unverifiable"


def test_state_dir_override(monkeypatch, tmp_path):
    monkeypatch.setenv("LIDARBENCH_STATE_DIR", str(tmp_path))
    assert default_state_dir() == tmp_path
    monkeypatch.delenv("LIDARBENCH_STATE_DIR")
    first = default_state_dir()
    monkeypatch.setenv("XDG_RUNTIME_DIR", str(tmp_path))
    assert default_state_dir() == first and first.name == "lidarbench"
