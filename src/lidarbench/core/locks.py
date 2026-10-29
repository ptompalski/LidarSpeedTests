"""Exclusive lock files for campaign workspaces and benchmark execution.

A lock is a file created with exclusive create that records who holds it
(host, PID, process creation time, command, random token). Locks are released
only by their owner (token match). A lock whose owner process no longer exists
on this host is reported as ``stale``; it is never removed automatically.
:func:`release_lock` removes it on explicit request.

Two scopes are used by campaign runs:

- **workspace lock** ``<workspace>/.lidarbench.lock``: one run per workspace;
- **machine lock** in the per-user state directory: one benchmark run per user
  on this machine, whatever the workspace, so two campaigns cannot disturb each
  other's measurements. It is per user; other users' runs are not visible.
"""

from __future__ import annotations

import contextlib
import json
import os
import secrets
import socket
import sys
from dataclasses import dataclass
from pathlib import Path
from types import TracebackType
from typing import Any, Literal

import psutil

from lidarbench.core.clock import iso_utc, now

LOCK_SCHEMA = "lidarbench.lock/1"
WORKSPACE_LOCK_NAME = ".lidarbench.lock"
MACHINE_LOCK_NAME = "benchmark-execution.lock"
STATE_DIR_ENV = "LIDARBENCH_STATE_DIR"
CREATE_TIME_TOLERANCE_S = 0.05

LockState = Literal["active", "stale", "unverifiable"]


class LockError(RuntimeError):
    """A lock could not be acquired or released."""


class LockHeldError(LockError):
    """The lock is held by another run."""

    def __init__(self, path: Path, owner: dict[str, Any] | None, state: LockState) -> None:
        self.path = path
        self.owner = owner
        self.state = state
        who = (
            f"pid {owner.get('pid')} on {owner.get('host')} since {owner.get('acquired_at')}"
            if owner
            else "an unreadable owner record"
        )
        advice = {
            "active": "Wait for that run to finish; do not run competing benchmarks.",
            "stale": "Its process no longer exists. Inspect the workspace, then release it "
            f"explicitly: lidarbench lock release --confirm {path}",
            "unverifiable": "It cannot be verified from this host. Release it explicitly only "
            f"if you are sure: lidarbench lock release --confirm --force {path}",
        }[state]
        super().__init__(f"lock {path} is held ({state}) by {who}. {advice}")


def default_state_dir() -> Path:
    """Per-user directory for machine-scope locks.

    ``$LIDARBENCH_STATE_DIR`` overrides it. Otherwise: Windows
    ``%LOCALAPPDATA%\\lidarbench``; macOS ``~/Library/Caches/lidarbench``; other
    systems ``~/.local/state/lidarbench``. Session-scoped variables such as
    ``$XDG_RUNTIME_DIR`` are deliberately not used: cron, ``su`` and interactive
    sessions of one user must all resolve to the same lock.
    """
    override = os.environ.get(STATE_DIR_ENV)
    if override:
        return Path(override)
    if sys.platform == "win32":
        base = os.environ.get("LOCALAPPDATA")
        return Path(base) / "lidarbench" if base else Path.home() / "AppData/Local/lidarbench"
    if sys.platform == "darwin":
        return Path.home() / "Library" / "Caches" / "lidarbench"
    return Path.home() / ".local" / "state" / "lidarbench"


def _self_owner(purpose: str, token: str) -> dict[str, Any]:
    proc = psutil.Process()
    try:
        command = proc.cmdline()
    except (psutil.Error, OSError):
        command = [sys.executable]
    return {
        "schema": LOCK_SCHEMA,
        "purpose": purpose,
        "host": socket.gethostname(),
        "pid": os.getpid(),
        "process_create_time": proc.create_time(),
        "command": command,
        "acquired_at": iso_utc(now().wall_ns),
        "token": token,
    }


def read_lock(path: Path) -> dict[str, Any] | None:
    """Return the owner record of a lock file, or ``None`` if unreadable."""
    try:
        with open(path, encoding="utf-8") as handle:
            owner = json.load(handle)
    except (OSError, ValueError):
        return None
    return owner if isinstance(owner, dict) else None


def lock_state(owner: dict[str, Any] | None) -> LockState:
    """Whether a lock owner is still running (``active``), gone (``stale``) or unknown."""
    if owner is None or owner.get("host") != socket.gethostname():
        return "unverifiable"
    pid, created = owner.get("pid"), owner.get("process_create_time")
    if not isinstance(pid, int) or not isinstance(created, int | float):
        return "unverifiable"
    try:
        actual = psutil.Process(pid).create_time()
    except psutil.NoSuchProcess:
        return "stale"
    except psutil.Error:
        return "unverifiable"
    return "active" if abs(actual - created) <= CREATE_TIME_TOLERANCE_S else "stale"


class ExclusiveLock:
    """Exclusive lock file; use as a context manager."""

    def __init__(self, path: Path, purpose: str) -> None:
        self.path = Path(path)
        self.purpose = purpose
        self._token: str | None = None

    @property
    def held(self) -> bool:
        """Whether this object currently holds the lock."""
        return self._token is not None

    def acquire(self) -> dict[str, Any]:
        """Create the lock file.

        Returns:
            The owner record written to the lock file.

        Raises:
            LockHeldError: The lock exists.
        """
        if self._token is not None:
            raise LockError(f"lock already held by this object: {self.path}")
        self.path.parent.mkdir(parents=True, exist_ok=True)
        token = secrets.token_hex(16)
        owner = _self_owner(self.purpose, token)
        try:
            fd = os.open(self.path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o644)
        except FileExistsError:
            existing = read_lock(self.path)
            raise LockHeldError(self.path, existing, lock_state(existing)) from None
        with os.fdopen(fd, "w", encoding="utf-8") as handle:
            json.dump(owner, handle, indent=2, sort_keys=True)
            handle.write("\n")
            handle.flush()
            os.fsync(handle.fileno())
        self._token = token
        return owner

    def release(self) -> None:
        """Remove the lock file if (and only if) this object owns it.

        Raises:
            LockError: The file is missing or now belongs to someone else.
        """
        if self._token is None:
            return
        owner = read_lock(self.path)
        token, self._token = self._token, None
        if owner is None or owner.get("token") != token:
            raise LockError(f"lock {self.path} is no longer owned by this run; left in place")
        os.unlink(self.path)

    def __enter__(self) -> ExclusiveLock:
        self.acquire()
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc: BaseException | None,
        tb: TracebackType | None,
    ) -> None:
        self.release()


@dataclass(frozen=True)
class LockReport:
    """Result of :func:`inspect_lock` or :func:`release_lock`."""

    path: Path
    exists: bool
    owner: dict[str, Any] | None
    state: LockState | None
    released: bool = False

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "path": os.fspath(self.path),
            "exists": self.exists,
            "state": self.state,
            "owner": self.owner,
            "released": self.released,
        }


def inspect_lock(path: Path) -> LockReport:
    """Describe a lock file without modifying it."""
    if not path.exists():
        return LockReport(path, False, None, None)
    owner = read_lock(path)
    return LockReport(path, True, owner, lock_state(owner))


def release_lock(path: Path, *, force: bool = False) -> LockReport:
    """Explicitly remove a lock left by a run that no longer exists.

    Args:
        path: Lock file.
        force: Also remove an ``unverifiable`` lock (another host or unreadable).

    Raises:
        LockHeldError: The owner is still running, or the lock is unverifiable
            and ``force`` is false.
    """
    report = inspect_lock(path)
    if not report.exists:
        return report
    assert report.state is not None
    if report.state == "active" or (report.state == "unverifiable" and not force):
        raise LockHeldError(path, report.owner, report.state)
    with contextlib.suppress(FileNotFoundError):
        os.unlink(path)
    return LockReport(path, True, report.owner, report.state, released=True)
