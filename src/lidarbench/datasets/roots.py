"""Portable dataset locations and machine-local data roots.

A dataset manifest never stores an absolute path. It names a *root label*
(for example ``petawawa-fresh``) and a POSIX relative path below that root.
Each machine maps labels to local directories in a small TOML file::

    schema = "lidarbench.data-roots/1"

    [roots]
    petawawa-fresh = "/data/nvme2/LidarSpeedTestWorkingDir/input_data_no_synthetic"
    fixtures = "D:/lidar/fixtures"

Lookup order (later entries override earlier ones): the default file in the
per-user configuration directory, the file named by ``$LIDARBENCH_DATA_ROOTS``
(which replaces the default file), then explicit ``LABEL=PATH`` overrides from
the command line or a campaign configuration.

Relative paths are checked so that the same manifest is usable on Linux,
Windows and macOS: no absolute or drive paths, no ``..``, no backslashes,
no Windows-reserved names or characters, no trailing dots or spaces.
"""

from __future__ import annotations

import os
import re
import sys
import tomllib
from collections.abc import Iterable, Mapping
from dataclasses import dataclass, field
from pathlib import Path

ROOTS_SCHEMA = "lidarbench.data-roots/1"
ROOTS_ENV = "LIDARBENCH_DATA_ROOTS"
ROOTS_FILE_NAME = "data-roots.toml"

LABEL_PATTERN = re.compile(r"^[a-z0-9][a-z0-9_.-]{0,63}$")
_RESERVED = re.compile(r"^(CON|PRN|AUX|NUL|COM[0-9]|LPT[0-9])(\..*)?$", re.IGNORECASE)
_FORBIDDEN_CHARS = set('<>:"\\|?*')


class DataRootError(ValueError):
    """A root label is unknown or a portable path is invalid."""


def check_component(name: str, where: str) -> str:
    """Validate one path component for portability across Linux, Windows and macOS."""
    if not name or name in (".", ".."):
        raise DataRootError(f"{where}: empty, '.' or '..' path component")
    bad = sorted({c for c in name if c in _FORBIDDEN_CHARS or ord(c) < 32})
    if bad:
        raise DataRootError(f"{where}: component {name!r} contains non-portable {bad!r}")
    if _RESERVED.match(name):
        raise DataRootError(f"{where}: {name!r} is a reserved name on Windows")
    if name[-1] in ". ":
        raise DataRootError(f"{where}: {name!r} ends with a dot or space (not portable)")
    return name


def check_relative_path(value: object, where: str, *, allow_empty: bool = False) -> str:
    """Validate a POSIX relative path (``a/b/c``) and return it unchanged.

    Raises:
        DataRootError: The path is absolute, uses ``\\`` or ``..``, or is not portable.
    """
    if not isinstance(value, str):
        raise DataRootError(f"{where} must be a string, got {value!r}")
    if value == "" and allow_empty:
        return value
    if not value or value.startswith("/") or re.match(r"^[A-Za-z]:", value):
        raise DataRootError(f"{where} must be a non-empty relative POSIX path, got {value!r}")
    for part in value.split("/"):
        check_component(part, f"{where} ({value!r})")
    return value


def check_label(value: object, where: str) -> str:
    """Validate a root label."""
    if not isinstance(value, str) or not LABEL_PATTERN.match(value):
        raise DataRootError(f"{where} must match {LABEL_PATTERN.pattern}, got {value!r}")
    return value


def default_roots_file() -> Path:
    """Per-user data-roots file.

    Windows ``%APPDATA%\\lidarbench``; macOS ``~/Library/Application Support/lidarbench``;
    otherwise ``$XDG_CONFIG_HOME/lidarbench`` or ``~/.config/lidarbench``.
    """
    if sys.platform == "win32":
        base = os.environ.get("APPDATA")
        folder = Path(base) if base else Path.home() / "AppData" / "Roaming"
    elif sys.platform == "darwin":
        folder = Path.home() / "Library" / "Application Support"
    else:
        xdg = os.environ.get("XDG_CONFIG_HOME")
        folder = Path(xdg) if xdg else Path.home() / ".config"
    return folder / "lidarbench" / ROOTS_FILE_NAME


@dataclass(frozen=True)
class DataRoots:
    """Mapping of root labels to local directories, with where each came from."""

    roots: Mapping[str, Path] = field(default_factory=dict)
    sources: Mapping[str, str] = field(default_factory=dict)

    def path(self, label: str) -> Path:
        """Local directory of ``label``.

        Raises:
            DataRootError: The label is not configured on this machine.
        """
        try:
            return self.roots[label]
        except KeyError:
            raise DataRootError(
                f"data root {label!r} is not configured on this machine; add it to "
                f"{default_roots_file()} (or ${ROOTS_ENV}) or pass --root {label}=PATH"
            ) from None

    def resolve(self, label: str, relative: str) -> Path:
        """Absolute directory for ``relative`` below root ``label``.

        Links inside the root are allowed only if the final directory stays within
        the (resolved) root.

        Raises:
            DataRootError: Unknown label, invalid path or an escape from the root.
        """
        check_relative_path(relative, "location.path", allow_empty=True)
        root = self.path(label)
        target = root.joinpath(*relative.split("/")) if relative else root
        real_root = Path(os.path.realpath(root))
        real_target = Path(os.path.realpath(target))
        if real_target != real_root and real_root not in real_target.parents:
            raise DataRootError(f"{target} resolves outside data root {label!r} ({real_root})")
        return target

    def with_overrides(self, overrides: Mapping[str, Path], source: str) -> DataRoots:
        """Copy with ``overrides`` taking precedence."""
        roots = dict(self.roots)
        sources = dict(self.sources)
        for label, path in overrides.items():
            roots[check_label(label, f"{source} root label")] = Path(os.path.abspath(path))
            sources[label] = source
        return DataRoots(roots, sources)

    def to_record(self) -> dict[str, dict[str, str]]:
        """JSON-ready view (machine-specific; never part of a manifest)."""
        return {
            label: {"path": os.fspath(path), "source": self.sources.get(label, "")}
            for label, path in sorted(self.roots.items())
        }


def parse_roots_file(path: Path) -> dict[str, Path]:
    """Read a ``lidarbench.data-roots/1`` file. Relative paths resolve against its folder.

    Raises:
        DataRootError: Unreadable file, wrong schema, unknown keys or bad labels.
    """
    try:
        data = tomllib.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, tomllib.TOMLDecodeError) as exc:
        raise DataRootError(f"cannot read data roots file {path}: {exc}") from exc
    unknown = sorted(set(data) - {"schema", "roots"})
    if unknown:
        raise DataRootError(f"{path}: unknown key(s) {unknown}; allowed: ['roots', 'schema']")
    if data.get("schema") != ROOTS_SCHEMA:
        raise DataRootError(f"{path}: schema must be {ROOTS_SCHEMA!r}, got {data.get('schema')!r}")
    table = data.get("roots", {})
    if not isinstance(table, dict):
        raise DataRootError(f"{path}: roots must be a table")
    out: dict[str, Path] = {}
    for label, value in table.items():
        check_label(label, f"{path}: roots label")
        if not isinstance(value, str) or not value.strip():
            raise DataRootError(f"{path}: roots.{label} must be a non-empty path string")
        local = Path(os.path.expanduser(value))
        out[label] = Path(os.path.abspath(local if local.is_absolute() else path.parent / local))
    return out


def parse_root_overrides(items: Iterable[str] | None) -> dict[str, Path]:
    """Parse ``LABEL=PATH`` command-line overrides."""
    out: dict[str, Path] = {}
    for item in items or ():
        label, sep, value = item.partition("=")
        if not sep or not value:
            raise DataRootError(f"--root expects LABEL=PATH, got {item!r}")
        check_label(label, "--root label")
        out[label] = Path(os.path.abspath(os.path.expanduser(value)))
    return out


def load_data_roots(
    roots_file: Path | None = None,
    overrides: Mapping[str, Path] | None = None,
    *,
    environ: Mapping[str, str] | None = None,
) -> DataRoots:
    """Combine the roots file (explicit, ``$LIDARBENCH_DATA_ROOTS`` or default) and overrides.

    A missing default file is not an error; an explicitly named file must exist.
    """
    env = os.environ if environ is None else environ
    roots = DataRoots()
    if roots_file is not None:
        file, required = roots_file, True
    elif env.get(ROOTS_ENV):
        file, required = Path(env[ROOTS_ENV]), True
    else:
        file, required = default_roots_file(), False
    if file.is_file() or required:
        roots = roots.with_overrides(parse_roots_file(file), os.fspath(file))
    if overrides:
        roots = roots.with_overrides(overrides, "override")
    return roots
