"""Tests for the CLI module."""

import pytest

from lidarbench import cli


def test_main_help():
    """Test that --help works."""
    with pytest.raises(SystemExit) as exc_info:
        cli.main(["--help"])
    assert exc_info.value.code == 0


def test_main_version():
    """Test that --version works."""
    with pytest.raises(SystemExit) as exc_info:
        cli.main(["--version"])
    assert exc_info.value.code == 0


def test_version_command(capsys):
    """Test the version command."""
    exit_code = cli.main(["version"])
    assert exit_code == 0
    captured = capsys.readouterr()
    assert "lidarbench" in captured.out
    assert "2.0.0a1" in captured.out


def test_no_command(capsys):
    """Test that no command shows help."""
    exit_code = cli.main([])
    assert exit_code == 0
    captured = capsys.readouterr()
    assert "usage:" in captured.out.lower() or "lidarbench" in captured.out


def test_placeholder_command_fails():
    """Test that unimplemented commands fail with proper message."""
    exit_code = cli.main(["init"])
    assert exit_code == 1


def test_unknown_command_fails():
    """Test that unknown commands fail."""
    exit_code = cli.main(["nonexistent"])
    assert exit_code == 1
