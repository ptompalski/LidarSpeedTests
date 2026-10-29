"""Supervisor-death guard for POSIX process sessions (``python -m lidarbench.execution.guardian``).

A benchmarked tool runs in its own session, so if the orchestrator is killed
(``SIGKILL``, OOM killer, power-off of the terminal) nothing would stop the
tool tree, which would keep running unsupervised and could overlap a later run.
Windows needs no guardian: the Job Object is created with
``KILL_ON_JOB_CLOSE``.

The guardian is a tiny helper started once per orchestrator process, in its own
session (terminal signals do not reach it), with a pipe on stdin. The
orchestrator writes ``watch <pgid>`` after launching a tool and
``release <pgid>`` after the tool's tree is gone. When stdin reaches EOF (the
orchestrator exited, however it died) every process group still watched is
killed with ``SIGKILL``. A guardian failure is never fatal for a run; it is
recorded as a warning.
"""

from __future__ import annotations

import os
import signal
import subprocess
import sys
import threading
from typing import IO

_lock = threading.Lock()
_shared: Guardian | None = None


class Guardian:
    """Client side: the pipe to one guardian process."""

    def __init__(self) -> None:
        self._proc = subprocess.Popen(
            [sys.executable, "-m", "lidarbench.execution.guardian"],
            stdin=subprocess.PIPE,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            start_new_session=True,
            close_fds=True,
        )
        assert self._proc.stdin is not None
        self._pipe: IO[bytes] = self._proc.stdin

    @property
    def alive(self) -> bool:
        """Whether the guardian process is still running."""
        return self._proc.poll() is None

    def _send(self, line: str) -> None:
        with _lock:
            self._pipe.write(line.encode("ascii") + b"\n")
            self._pipe.flush()

    def watch(self, pgid: int) -> None:
        """Kill process group ``pgid`` if this process dies before :meth:`release`."""
        self._send(f"watch {pgid}")

    def release(self, pgid: int) -> None:
        """Stop watching ``pgid`` (its tree is gone)."""
        self._send(f"release {pgid}")


def shared_guardian() -> Guardian | None:
    """The process-wide guardian (started on first use); ``None`` on Windows."""
    global _shared
    if sys.platform == "win32":
        return None
    with _lock:
        if _shared is None or not _shared.alive:
            _shared = Guardian()
        return _shared


def main() -> int:
    """Guardian loop: track watched groups; kill the remaining ones on EOF."""
    if sys.platform == "win32":
        return 2
    for name in ("SIGINT", "SIGTERM", "SIGHUP"):
        signal.signal(getattr(signal, name), signal.SIG_IGN)
    watched: set[int] = set()
    for raw in sys.stdin.buffer:
        parts = raw.decode("ascii", errors="replace").split()
        if len(parts) != 2 or not parts[1].isdigit():
            continue
        action, pgid = parts[0], int(parts[1])
        if action == "watch" and pgid > 1:
            watched.add(pgid)
        elif action == "release":
            watched.discard(pgid)
    for pgid in watched:
        try:
            os.killpg(pgid, signal.SIGKILL)
        except (ProcessLookupError, PermissionError):
            pass
    return 0


if __name__ == "__main__":
    sys.exit(main())
