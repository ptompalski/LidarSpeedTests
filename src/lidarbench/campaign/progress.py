"""Live terminal progress for ``lidarbench run`` (interactive terminals only).

While an attempt runs, a background thread redraws one status line about once
per second: the campaign position, the tool, the number of expected output
files that already exist (one raster per tile for every tile workload), the
elapsed time, an ETA and the timeout. It only checks ``os.stat`` on paths the
adapter declared, reads no file contents and never touches records, so it has
no effect on what is measured or stored. Some tools create a raster when they
start a tile, so the count may run slightly ahead of finished tiles.
"""

from __future__ import annotations

import os
import shutil
import threading
import time
from collections.abc import Callable, Iterator, Sequence
from contextlib import AbstractContextManager, contextmanager, nullcontext
from pathlib import Path
from typing import TextIO

ProgressFactory = Callable[[str, Sequence[Path], float | None], AbstractContextManager[None]]


def no_progress(
    label: str, outputs: Sequence[Path], timeout_s: float | None
) -> AbstractContextManager[None]:
    """The default: no live display."""
    return nullcontext()


def format_duration(seconds: float) -> str:
    """``42s``, ``3m05s`` or ``1h02m``."""
    s = max(0, int(seconds))
    if s < 60:
        return f"{s}s"
    if s < 3600:
        return f"{s // 60}m{s % 60:02d}s"
    return f"{s // 3600}h{(s % 3600) // 60:02d}m"


def render_line(
    label: str,
    done: int,
    total: int,
    elapsed_s: float,
    timeout_s: float | None,
    width: int,
    *,
    unicode: bool = True,
) -> str:
    """One status line that fits in ``width`` columns."""
    fill, empty = ("█", "░") if unicode else ("#", "-")
    parts = [format_duration(elapsed_s)]
    if total and 0 < done < total:
        parts.append(f"ETA {format_duration(elapsed_s / done * (total - done))}")
    if timeout_s:
        parts.append(f"timeout {format_duration(timeout_s)}")
    tail = " · ".join(parts) if unicode else " | ".join(parts)
    if total:
        count = f"{done}/{total} files"
        bar_width = max(0, min(30, width - len(label) - len(count) - len(tail) - 6))
        if bar_width >= 5:
            filled = round(bar_width * min(done, total) / total)
            bar = fill * filled + empty * (bar_width - filled)
            line = f"{label}  {bar}  {count}  {tail}"
        else:
            line = f"{label}  {count}  {tail}"
    else:
        line = f"{label}  {tail}"
    return line[: max(0, width - 1)]


class TerminalProgress:
    """A :data:`ProgressFactory` that draws on an interactive terminal stream."""

    def __init__(
        self,
        stream: TextIO,
        *,
        interval_s: float = 1.0,
        clock: Callable[[], float] = time.monotonic,
    ) -> None:
        self.stream = stream
        self.interval_s = interval_s
        self.clock = clock
        encoding = (getattr(stream, "encoding", None) or "").lower().replace("-", "")
        self.unicode = encoding in ("utf8", "utf8sig")

    def __call__(
        self, label: str, outputs: Sequence[Path], timeout_s: float | None
    ) -> AbstractContextManager[None]:
        return self._attempt(label, tuple(outputs), timeout_s)

    @staticmethod
    def count(outputs: Sequence[Path]) -> int:
        """How many of ``outputs`` exist."""
        done = 0
        for path in outputs:
            try:
                os.stat(path)
            except OSError:
                continue
            done += 1
        return done

    def _draw(
        self, label: str, outputs: Sequence[Path], start: float, timeout_s: float | None
    ) -> None:
        width = shutil.get_terminal_size((100, 24)).columns
        line = render_line(
            label,
            self.count(outputs),
            len(outputs),
            self.clock() - start,
            timeout_s,
            width,
            unicode=self.unicode,
        )
        try:
            self.stream.write("\r\x1b[2K" + line)
            self.stream.flush()
        except (OSError, ValueError):
            pass

    @contextmanager
    def _attempt(
        self, label: str, outputs: Sequence[Path], timeout_s: float | None
    ) -> Iterator[None]:
        stop = threading.Event()
        start = self.clock()

        def loop() -> None:
            while True:
                self._draw(label, outputs, start, timeout_s)
                if stop.wait(self.interval_s):
                    return

        thread = threading.Thread(target=loop, name="lidarbench-progress", daemon=True)
        thread.start()
        try:
            yield
        finally:
            stop.set()
            thread.join(timeout=5)
            try:
                self.stream.write("\r\x1b[2K")
                self.stream.flush()
            except (OSError, ValueError):
                pass


def terminal_progress_for(stream: TextIO) -> ProgressFactory:
    """:class:`TerminalProgress` on an interactive terminal, else :func:`no_progress`."""
    try:
        interactive = stream.isatty()
    except (AttributeError, ValueError):
        interactive = False
    if interactive and os.environ.get("TERM") != "dumb":
        return TerminalProgress(stream)
    return no_progress
