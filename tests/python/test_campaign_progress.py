"""Live terminal progress of ``lidarbench run`` (campaign/progress.py)."""

from __future__ import annotations

import io
from collections.abc import Iterator, Sequence
from contextlib import contextmanager
from pathlib import Path

from campaign_helpers import make_runner, write_config, write_layout

from lidarbench.campaign.progress import (
    TerminalProgress,
    format_duration,
    no_progress,
    render_line,
    terminal_progress_for,
)


def test_format_duration() -> None:
    assert format_duration(0) == "0s"
    assert format_duration(59.9) == "59s"
    assert format_duration(185) == "3m05s"
    assert format_duration(3720) == "1h02m"
    assert format_duration(-3) == "0s"


def test_render_line_bar_count_eta_and_timeout() -> None:
    line = render_line("[2/4] lidr pixel_metrics_1a 8 cpu", 25, 100, 60, 7200, 120)
    assert line.startswith("[2/4] lidr pixel_metrics_1a 8 cpu  █")
    assert "░" in line
    assert "25/100 files" in line
    assert "1m00s · ETA 3m00s · timeout 2h00m" in line
    assert len(line) < 120


def test_render_line_ascii_narrow_and_no_outputs() -> None:
    ascii_line = render_line("x", 1, 2, 5, None, 80, unicode=False)
    assert "#" in ascii_line and "-" in ascii_line and "█" not in ascii_line
    assert "5s | ETA 5s" in ascii_line
    narrow = render_line("a long label for a narrow terminal", 1, 4, 5, 60, 40)
    assert len(narrow) <= 39
    assert render_line("x", 0, 0, 3, None, 80) == "x  3s"
    assert "ETA" not in render_line("x", 4, 4, 3, None, 80)


def test_count_and_drawing(tmp_path: Path) -> None:
    outputs = [tmp_path / "a.tif", tmp_path / "b.tif"]
    stream = io.StringIO()
    progress = TerminalProgress(stream, interval_s=0.01)
    assert progress.count(outputs) == 0
    with progress("label", outputs, None):
        outputs[0].write_bytes(b"x")
    assert progress.count(outputs) == 1
    text = stream.getvalue()
    assert "label" in text and "/2 files" in text
    assert text.endswith("\r\x1b[2K")


class _Tty(io.StringIO):
    def isatty(self) -> bool:
        return True


def test_terminal_progress_only_on_a_tty(monkeypatch) -> None:
    monkeypatch.setenv("TERM", "xterm")
    assert terminal_progress_for(io.StringIO()) is no_progress
    assert isinstance(terminal_progress_for(_Tty()), TerminalProgress)
    monkeypatch.setenv("TERM", "dumb")
    assert terminal_progress_for(_Tty()) is no_progress


def test_runner_wraps_each_launched_attempt(tmp_path: Path) -> None:
    base = write_layout(tmp_path)
    config = write_config(
        base,
        workloads='[[workloads]]\nid = "scripted.ok"\n',
        tools='[[tools]]\nid = "scripted"\n',
        scheduling="repetitions = 2\nwarmups = 0\ncooldown_s = 0",
    )
    calls: list[tuple[str, list[Path], float | None, list[bool]]] = []

    @contextmanager
    def record(label: str, outputs: Sequence[Path], timeout_s: float | None) -> Iterator[None]:
        before = [p.exists() for p in outputs]
        yield
        calls.append((label, list(outputs), timeout_s, before + [p.exists() for p in outputs]))

    summary = make_runner(config, base, progress=record).run()
    assert summary.exit_code == 0
    assert [c[0] for c in calls] == [
        "[1/2] scripted scripted.ok 1 cpu",
        "[2/2] scripted scripted.ok 1 cpu",
    ]
    for _, outputs, _, seen in calls:
        assert [p.name for p in outputs] == ["ok.txt"]
        assert seen == [False, True]
