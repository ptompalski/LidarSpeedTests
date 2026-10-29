"""Scan a dataset directory: classify files, hash them and read LAS/LAZ headers.

The scan refuses anything that would make a dataset non-portable or ambiguous:
links, special files, unclassified files, names that cannot exist on Windows or
macOS (case-only collisions, reserved names) and traces of an active or
interrupted preparation (``*.lidarbench-partial-*``, ``*.pending-*``, the
legacy R ``.preparation-lock``).
"""

from __future__ import annotations

import fnmatch
import hashlib
import os
import stat
from collections.abc import Callable
from dataclasses import dataclass
from pathlib import Path

from lidarbench.core.canonical import HASH_CHUNK_BYTES
from lidarbench.datasets.lasheader import LasHeaderError, read_las_header
from lidarbench.datasets.manifest import FileEntry, LasFacts, Layout
from lidarbench.datasets.roots import DataRootError, check_component

PARTIAL_MARKER = ".lidarbench-partial-"
PENDING_MARKER = ".pending-"
LEGACY_LOCK = ".preparation-lock"

Progress = Callable[[str, int], None]
"""Called with (relative path, bytes hashed so far in total)."""


class InventoryError(RuntimeError):
    """The directory cannot be inventoried as a dataset."""


class InventoryCancelledError(InventoryError):
    """Hashing stopped because cancellation was requested."""


def interrupted_marker(name: str) -> bool:
    """Whether a file or directory name indicates an active or interrupted preparation."""
    return PARTIAL_MARKER in name or PENDING_MARKER in name or name == LEGACY_LOCK


@dataclass(frozen=True)
class ScannedFile:
    """One scanned file; digests are ``None`` in a quick (size-only) scan."""

    path: str
    role: str
    bytes: int
    sha256: str | None
    md5: str | None
    las: LasFacts | None

    def to_entry(self) -> FileEntry:
        """Manifest entry (requires a hashing scan)."""
        if self.sha256 is None:
            raise InventoryError(f"{self.path}: not hashed")
        return FileEntry(self.path, self.role, self.bytes, self.sha256, self.md5, self.las)


@dataclass(frozen=True)
class ScanResult:
    """Inventory of a dataset directory."""

    root: Path
    files: tuple[ScannedFile, ...]
    excluded: tuple[str, ...]

    def entries(self) -> tuple[FileEntry, ...]:
        """Manifest entries (requires a hashing scan)."""
        return tuple(f.to_entry() for f in self.files)


def classify(name: str, layout: Layout) -> str | None:
    """Role of a file name under ``layout``; ``None`` if excluded.

    Raises:
        InventoryError: The name matches no role or more than one.
    """
    if any(fnmatch.fnmatchcase(name, p) for p in layout.exclude):
        return None
    roles = [
        role
        for role, patterns in layout.role_patterns().items()
        if any(fnmatch.fnmatchcase(name, p) for p in patterns)
    ]
    if len(roles) != 1:
        problem = "matches no declared role" if not roles else f"matches several roles {roles}"
        raise InventoryError(f"{name}: {problem}")
    return roles[0]


def hash_file(
    path: Path, *, md5: bool = False, progress: Callable[[int], None] | None = None
) -> tuple[str, str | None, int]:
    """SHA-256 (and optionally MD5) of a file in one pass. Returns (sha256, md5, bytes)."""
    sha = hashlib.sha256()
    legacy = hashlib.md5(usedforsecurity=False) if md5 else None
    total = 0
    with open(path, "rb") as handle:
        while chunk := handle.read(HASH_CHUNK_BYTES):
            sha.update(chunk)
            if legacy is not None:
                legacy.update(chunk)
            total += len(chunk)
            if progress is not None:
                progress(len(chunk))
    return sha.hexdigest(), legacy.hexdigest() if legacy is not None else None, total


def _walk(root: Path) -> list[tuple[str, Path]]:
    if not root.is_dir() or root.is_symlink():
        raise InventoryError(f"dataset directory not found (or is a link): {root}")
    found: list[tuple[str, Path]] = []
    for dirpath, dirnames, filenames in os.walk(root, followlinks=False):
        here = Path(dirpath)
        for name in sorted(dirnames + filenames):
            full = here / name
            rel = full.relative_to(root).as_posix()
            if interrupted_marker(name):
                raise InventoryError(
                    f"{full}: an active or interrupted preparation left this entry; "
                    "finish or discard that preparation first"
                )
            try:
                check_component(name, rel)
            except DataRootError as exc:
                raise InventoryError(str(exc)) from None
            mode = os.lstat(full).st_mode
            if stat.S_ISLNK(mode) or _reparse(full):
                raise InventoryError(f"{full}: links are not allowed inside a dataset")
            if name in filenames:
                if not stat.S_ISREG(mode):
                    raise InventoryError(f"{full}: not a regular file")
                found.append((rel, full))
        dirnames.sort()
    found.sort(key=lambda item: item[0])
    folded: dict[str, str] = {}
    for rel, _ in found:
        key = rel.casefold()
        if key in folded:
            raise InventoryError(
                f"{rel!r} and {folded[key]!r} differ only in case (not portable to Windows/macOS)"
            )
        folded[key] = rel
    return found


def _reparse(path: Path) -> bool:
    """Windows junctions and other reparse points (always False elsewhere)."""
    tag = getattr(os.lstat(path), "st_reparse_tag", 0)
    return bool(tag)


def _las(path: Path, role: str, layout: Layout) -> LasFacts | None:
    if role != "input" or layout.las_headers == "none":
        return None
    try:
        return LasFacts.from_header(read_las_header(path))
    except LasHeaderError as exc:
        if layout.las_headers == "required":
            raise InventoryError(f"input is not a readable LAS/LAZ file: {exc}") from None
        return None


def scan_directory(
    root: Path,
    layout: Layout,
    *,
    hash_files: bool = True,
    md5: bool = False,
    progress: Progress | None = None,
    cancelled: Callable[[], bool] | None = None,
) -> ScanResult:
    """Inventory ``root`` under ``layout``.

    Each file is fingerprinted (size, mtime) before and after hashing; a file
    that changes while being read is an error.

    Raises:
        InventoryError: See the module docstring.
    """
    files: list[ScannedFile] = []
    excluded: list[str] = []
    done = 0
    for rel, full in _walk(root):
        role = classify(full.name, layout)
        if role is None:
            excluded.append(rel)
            continue
        if cancelled is not None and cancelled():
            raise InventoryCancelledError(f"inventory of {root} cancelled")
        before = os.stat(full)
        sha = legacy = None
        if hash_files:

            def tick(n: int, rel: str = rel) -> None:
                nonlocal done
                done += n
                if progress is not None:
                    progress(rel, done)

            sha, legacy, _ = hash_file(full, md5=md5, progress=tick)
        las = _las(full, role, layout)
        after = os.stat(full)
        if (before.st_size, before.st_mtime_ns) != (after.st_size, after.st_mtime_ns):
            raise InventoryError(f"{full}: changed while being read")
        files.append(ScannedFile(rel, role, before.st_size, sha, legacy, las))
    if not any(f.role == "input" for f in files):
        raise InventoryError(f"{root}: no input files match {list(layout.inputs)}")
    return ScanResult(root, tuple(files), tuple(excluded))
