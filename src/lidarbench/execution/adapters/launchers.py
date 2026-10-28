"""Helpers for building argv for common launch styles.

These cover the four launch styles the adapter interface is designed for:

- native executables (``lascanopy64``, ``pdal``): :func:`native_argv`;
- R scripts: :func:`rscript_argv`;
- Python modules in a configured interpreter: :func:`python_module_argv`;
- tools that read a configuration file: write it through
  :class:`~lidarbench.execution.adapters.base.ConfigFile` and pass
  :meth:`~lidarbench.execution.adapters.base.Invocation.config_path` as an argument.

Every helper returns a tuple of strings for ``subprocess`` with ``shell=False``.
"""

from __future__ import annotations

import os
import re
import shutil
import subprocess
from collections.abc import Iterable
from pathlib import Path

from lidarbench.execution.adapters.base import ParameterValue, argv_value

ArgValue = ParameterValue | os.PathLike[str]


def resolve_executable(name_or_path: str | os.PathLike[str]) -> Path | None:
    """Resolve an executable to an absolute path, or ``None`` if not found.

    A value containing a directory separator is treated as a path; otherwise the
    ``PATH`` is searched (honouring ``PATHEXT`` on Windows).
    """
    raw = os.fspath(name_or_path)
    if not raw:
        return None
    found = shutil.which(raw)
    # absolute(), not resolve(): a venv interpreter symlink must keep its venv path.
    return Path(found).absolute() if found else None


def _args(args: Iterable[ArgValue]) -> tuple[str, ...]:
    return tuple(argv_value(a) for a in args)


def native_argv(executable: Path, *args: ArgValue) -> tuple[str, ...]:
    """argv for a native executable."""
    return (os.fspath(executable), *_args(args))


def rscript_argv(
    rscript: Path,
    script: Path,
    *args: ArgValue,
    vanilla: bool = True,
) -> tuple[str, ...]:
    """argv for ``Rscript [--vanilla] <script> args...``."""
    options = ("--vanilla",) if vanilla else ()
    return (os.fspath(rscript), *options, os.fspath(script), *_args(args))


def python_module_argv(
    interpreter: Path,
    module: str,
    *args: ArgValue,
    isolated: bool = False,
) -> tuple[str, ...]:
    """argv for ``python [-I] -m <module> args...`` in a given interpreter.

    ``isolated`` adds ``-I`` (ignore ``PYTHON*`` variables and user site).
    """
    if not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*(\.[A-Za-z_][A-Za-z0-9_]*)*", module):
        raise ValueError(f"invalid Python module name: {module!r}")
    options = ("-I",) if isolated else ()
    return (os.fspath(interpreter), *options, "-m", module, *_args(args))


def probe_version_output(argv: Iterable[str], timeout_s: float = 30.0) -> str | None:
    """Run a short version command and return combined stdout/stderr text.

    Intended for :meth:`ToolAdapter.identify`. Returns ``None`` if the command
    cannot be started or times out; never raises for a missing tool.
    """
    try:
        completed = subprocess.run(
            list(argv),
            capture_output=True,
            timeout=timeout_s,
            check=False,
            shell=False,
            stdin=subprocess.DEVNULL,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    out = completed.stdout + completed.stderr
    return out.decode("utf-8", errors="replace")
