"""Artifact inventory: completeness, uniqueness and format signatures (stdlib only).

Expectations are the product of the unique input-tile stems and the
implementation's declared primary artifact names, never "whatever a peer tool
produced". Every expected file must exist, be a regular non-empty file and carry
the format signature implied by its extension. Any other file fails, except
declared optional sidecars (for example GDAL ``*.aux.xml``), which never satisfy
a requirement. Readability (decoding) is checked by the format backend later.
"""

from __future__ import annotations

import os
from collections.abc import Sequence
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from lidarbench.workloads.contract import Implementation, WorkloadContract

SIGNATURES: dict[str, tuple[bytes, ...]] = {
    ".tif": (b"II*\x00", b"MM\x00*", b"II+\x00", b"MM\x00+"),
    ".tiff": (b"II*\x00", b"MM\x00*", b"II+\x00", b"MM\x00+"),
    ".las": (b"LASF",),
    ".laz": (b"LASF",),
    ".gpkg": (b"SQLite format 3\x00",),
}


def tile_stem(path: Path) -> str:
    """Input file name without its final extension (R ``file_path_sans_ext``)."""
    return path.stem


@dataclass(frozen=True)
class ArtifactFile:
    """One expected primary artifact."""

    tile: str
    name: str
    path: Path
    size: int | None
    problems: tuple[str, ...] = ()

    @property
    def present(self) -> bool:
        """Whether a regular file exists."""
        return self.size is not None

    def to_record(self) -> dict[str, Any]:
        """JSON-serializable form."""
        return {
            "tile": self.tile,
            "name": self.name,
            "bytes": self.size,
            "problems": list(self.problems),
        }


@dataclass(frozen=True)
class Inventory:
    """Result of comparing an output directory with the expected artifacts."""

    expected: tuple[ArtifactFile, ...]
    unexpected: tuple[str, ...] = ()
    ignored: tuple[str, ...] = ()
    errors: tuple[str, ...] = field(default=())

    @property
    def problems(self) -> list[str]:
        """Every reason the inventory is not complete and clean."""
        out = list(self.errors)
        for artifact in self.expected:
            out.extend(f"{artifact.name}: {p}" for p in artifact.problems)
        out.extend(f"unexpected file: {name}" for name in self.unexpected)
        return out

    @property
    def complete(self) -> bool:
        """Every expected artifact is present and valid and nothing is unexpected."""
        return not self.problems

    def to_record(self) -> dict[str, Any]:
        """JSON-serializable form."""
        return {
            "expected": [a.to_record() for a in self.expected],
            "unexpected": list(self.unexpected),
            "ignored_sidecars": list(self.ignored),
            "errors": list(self.errors),
        }


def _signature_problem(path: Path) -> str | None:
    signatures = SIGNATURES.get(path.suffix.lower())
    if signatures is None:
        return None
    width = max(len(s) for s in signatures)
    try:
        with open(path, "rb") as handle:
            head = handle.read(width)
    except OSError as exc:
        return f"cannot read: {exc}"
    if not any(head.startswith(s) for s in signatures):
        return f"content is not a {path.suffix.lower()} file (signature {head[:4]!r})"
    return None


def _listing(output_dir: Path) -> tuple[list[str], list[str]]:
    """Relative POSIX names of all entries, plus names that are not regular files."""
    names: list[str] = []
    irregular: list[str] = []
    for root, dirs, files in os.walk(output_dir, followlinks=False):
        base = Path(root)
        for d in list(dirs):
            if (base / d).is_symlink():
                irregular.append((base / d).relative_to(output_dir).as_posix())
                dirs.remove(d)
        for f in files:
            path = base / f
            rel = path.relative_to(output_dir).as_posix()
            names.append(rel)
            if path.is_symlink() or not path.is_file():
                irregular.append(rel)
    return sorted(names), sorted(irregular)


def take_inventory(
    contract: WorkloadContract,
    implementation: Implementation,
    inputs: Sequence[Path],
    output_dir: Path,
) -> Inventory:
    """Compare ``output_dir`` with the artifacts expected for ``inputs``."""
    errors: list[str] = []
    stems = [tile_stem(Path(p)) for p in inputs]
    if not stems:
        errors.append("no input tiles: nothing defines the expected artifacts")
    seen: dict[str, int] = {}
    for stem in stems:
        seen[stem.lower()] = seen.get(stem.lower(), 0) + 1
    duplicates = sorted(s for s, n in seen.items() if n > 1)
    if duplicates:
        errors.append(f"duplicate input tile stems (case-insensitive): {duplicates}")
    if not output_dir.is_dir():
        errors.append(f"output directory does not exist: {output_dir}")
        names: list[str] = []
        irregular: list[str] = []
    else:
        names, irregular = _listing(output_dir)

    expected_names: dict[str, str] = {}
    for stem in stems:
        for name in implementation.expected_names(stem):
            expected_names[name] = stem
    lower_names: dict[str, list[str]] = {}
    for name in names:
        lower_names.setdefault(name.lower(), []).append(name)

    artifacts: list[ArtifactFile] = []
    for name, stem in expected_names.items():
        path = output_dir / name
        problems: list[str] = []
        size: int | None = None
        collisions = [n for n in lower_names.get(name.lower(), []) if n != name]
        if collisions:
            problems.append(f"duplicate product differing only by case: {collisions}")
        if name not in names:
            problems.append("missing")
        elif name in irregular:
            problems.append("not a regular file (symlink or special file)")
        else:
            size = path.stat().st_size
            if size == 0:
                problems.append("zero bytes")
            else:
                signature = _signature_problem(path)
                if signature:
                    problems.append(signature)
        artifacts.append(ArtifactFile(stem, name, path, size, tuple(problems)))

    unexpected: list[str] = []
    ignored: list[str] = []
    for name in names:
        if name in expected_names:
            continue
        if "/" not in name and contract.is_ignored_sidecar(name) and name not in irregular:
            ignored.append(name)
        else:
            unexpected.append(name)
    unexpected.extend(n for n in irregular if n not in names)
    return Inventory(tuple(artifacts), tuple(sorted(unexpected)), tuple(ignored), tuple(errors))
