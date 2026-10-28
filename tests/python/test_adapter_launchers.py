"""Tests for argv launch helpers."""

from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path

import pytest

from lidarbench.execution.adapters.launchers import (
    native_argv,
    probe_version_output,
    python_module_argv,
    resolve_executable,
    rscript_argv,
)


def test_native_argv_keeps_each_value_as_one_element(tmp_path):
    exe = tmp_path / "bin dir" / "tool"
    argv = native_argv(exe, "-i", tmp_path / "a b.laz", "-step", 20, "-cutoff", -12.5)
    assert argv == (str(exe), "-i", str(tmp_path / "a b.laz"), "-step", "20", "-cutoff", "-12.5")


def test_rscript_argv(tmp_path):
    r = tmp_path / "R" / "Rscript"
    script = tmp_path / "my scripts" / "runner.R"
    assert rscript_argv(r, script, "x") == (str(r), "--vanilla", str(script), "x")
    assert rscript_argv(r, script, vanilla=False) == (str(r), str(script))


def test_python_module_argv(tmp_path):
    py = tmp_path / "venv" / "python"
    assert python_module_argv(py, "pkg.mod", "--n", 3) == (str(py), "-m", "pkg.mod", "--n", "3")
    assert python_module_argv(py, "m", isolated=True)[1] == "-I"
    with pytest.raises(ValueError):
        python_module_argv(py, "bad module; rm")


def test_resolve_executable():
    found = resolve_executable(sys.executable)
    assert found is not None and found.is_absolute()
    assert resolve_executable("lidarbench-definitely-not-installed-tool") is None
    assert resolve_executable("") is None


def test_probe_version_output():
    text = probe_version_output([sys.executable, "--version"])
    assert text is not None and "Python" in text
    assert probe_version_output(["lidarbench-definitely-not-installed-tool"]) is None


def test_argv_with_spaces_and_metacharacters_reaches_process_verbatim(tmp_path):
    weird = [str(tmp_path / "a b"), "x;y", "$(echo no)", "'q'", '"dq"', "*", "%PATH%", "a\\ b"]
    script = "import sys, json; print(json.dumps(sys.argv[1:]))"
    argv = native_argv(Path(sys.executable), "-c", script, *weird)
    out = subprocess.run(argv, capture_output=True, text=True, check=True, shell=False).stdout
    assert json.loads(out) == weird
