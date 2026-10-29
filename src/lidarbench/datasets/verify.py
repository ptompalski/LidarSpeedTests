"""Verify local files against a registered dataset manifest.

Modes:

``full``   hash every file (SHA-256, plus MD5 where the manifest records one)
           and compare sizes, roles and LAS/LAZ header facts: status ``verified``.
``quick``  compare the inventory, sizes and header facts without hashing:
           status ``consistent`` at best -- never ``verified``.

``--provenance`` additionally checks the external provenance receipts
(readiness, preparation and download receipts) byte for byte.

A dataset whose data root is not configured on this machine is
``unavailable``, not failed. Verification only reads files.
"""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from lidarbench.datasets.inventory import (
    InventoryError,
    Progress,
    ScannedFile,
    hash_file,
    scan_directory,
)
from lidarbench.datasets.manifest import DatasetManifest, manifest_sha256
from lidarbench.datasets.roots import DataRootError, DataRoots

MODES = ("full", "quick")


@dataclass
class VerificationReport:
    """Outcome of verifying one dataset."""

    dataset_id: str
    mode: str
    status: str = "failed"
    """``verified`` | ``consistent`` | ``failed`` | ``unavailable``."""
    directory: str | None = None
    problems: list[str] = field(default_factory=list)
    files_checked: int = 0
    bytes_hashed: int = 0
    excluded_present: list[str] = field(default_factory=list)
    provenance_checked: int = 0
    content_sha256: str = ""
    manifest_sha256: str = ""

    @property
    def ok(self) -> bool:
        """Whether nothing failed (``verified`` or ``consistent``)."""
        return self.status in ("verified", "consistent")

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "dataset_id": self.dataset_id,
            "mode": self.mode,
            "status": self.status,
            "directory": self.directory,
            "content_sha256": self.content_sha256,
            "manifest_sha256": self.manifest_sha256,
            "files_checked": self.files_checked,
            "bytes_hashed": self.bytes_hashed,
            "provenance_records_checked": self.provenance_checked,
            "excluded_present": self.excluded_present,
            "problems": self.problems,
        }


def _compare(manifest: DatasetManifest, scanned: tuple[ScannedFile, ...], full: bool) -> list[str]:
    problems = []
    expected = {f.path: f for f in manifest.files}
    actual = {f.path: f for f in scanned}
    for path in sorted(set(expected) - set(actual)):
        problems.append(f"{path}: missing")
    for path in sorted(set(actual) - set(expected)):
        problems.append(f"{path}: not in the manifest")
    for path in sorted(set(expected) & set(actual)):
        want, have = expected[path], actual[path]
        if want.role != have.role:
            problems.append(f"{path}: role {have.role!r} != manifest {want.role!r}")
        if want.bytes != have.bytes:
            problems.append(f"{path}: size {have.bytes} != manifest {want.bytes}")
            continue
        if full and want.sha256 != have.sha256:
            problems.append(f"{path}: SHA-256 differs from the manifest")
        if full and want.md5 is not None and want.md5 != have.md5:
            problems.append(f"{path}: MD5 differs from the manifest (legacy receipt)")
        if want.las is not None and want.las != have.las:
            problems.append(f"{path}: LAS/LAZ header facts differ from the manifest")
    return problems


def verify_dataset(
    manifest: DatasetManifest,
    roots: DataRoots,
    *,
    mode: str = "full",
    provenance: bool = False,
    progress: Progress | None = None,
    cancelled: Callable[[], bool] | None = None,
    directory: Path | None = None,
) -> VerificationReport:
    """Verify the local copy of ``manifest``.

    Args:
        manifest: Registered manifest.
        roots: Data roots of this machine.
        mode: ``full`` or ``quick``.
        provenance: Also verify the external provenance receipts.
        progress: Hashing progress callback.
        cancelled: Polled between files.
        directory: Explicit dataset directory (otherwise resolved from ``roots``).
    """
    if mode not in MODES:
        raise ValueError(f"mode must be one of {MODES}")
    report = VerificationReport(
        manifest.dataset_id,
        mode,
        content_sha256=manifest.content_sha256,
        manifest_sha256=manifest_sha256(manifest),
    )
    try:
        folder = directory or roots.resolve(manifest.location.root, manifest.location.path)
    except DataRootError as exc:
        report.status = "unavailable"
        report.problems.append(str(exc))
        return report
    report.directory = str(folder)
    full = mode == "full"
    need_md5 = full and any(f.md5 is not None for f in manifest.files)
    try:
        scan = scan_directory(
            folder,
            manifest.layout,
            hash_files=full,
            md5=need_md5,
            progress=progress,
            cancelled=cancelled,
        )
    except InventoryError as exc:
        report.problems.append(str(exc))
        return report
    report.files_checked = len(scan.files)
    report.bytes_hashed = sum(f.bytes for f in scan.files) if full else 0
    report.excluded_present = list(scan.excluded)
    report.problems.extend(_compare(manifest, scan.files, full))
    if provenance:
        report.problems.extend(_check_provenance(manifest, roots, report))
    if not report.problems:
        report.status = "verified" if full else "consistent"
    return report


def _check_provenance(
    manifest: DatasetManifest, roots: DataRoots, report: VerificationReport
) -> list[str]:
    problems = []
    for record in manifest.provenance.records:
        where = f"provenance {record.location.root}:{record.location.path}"
        try:
            path = roots.resolve(record.location.root, record.location.path)
        except DataRootError as exc:
            problems.append(f"{where}: {exc}")
            continue
        if not path.is_file() or path.is_symlink():
            problems.append(f"{where}: missing (or not a regular file)")
            continue
        sha, md5, size = hash_file(path, md5=record.md5 is not None)
        report.provenance_checked += 1
        if size != record.bytes or sha != record.sha256:
            problems.append(f"{where}: receipt bytes changed")
        elif record.md5 is not None and md5 != record.md5:
            problems.append(f"{where}: receipt MD5 changed")
    return problems
