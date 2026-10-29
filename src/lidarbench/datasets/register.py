"""Register existing data as a dataset manifest (no R, no reprocessing).

Registration reads a directory below a data root, inventories and hashes it,
captures the recipe's external provenance receipts, optionally cross-checks
legacy R receipts, and publishes a write-once manifest. It never modifies the
data. A directory with traces of an active or interrupted preparation is
refused.
"""

from __future__ import annotations

import datetime as _dt
import platform
from collections.abc import Callable
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import lidarbench
from lidarbench.datasets import legacy_petawawa
from lidarbench.datasets.inventory import (
    LEGACY_LOCK,
    InventoryError,
    Progress,
    ScanResult,
    hash_file,
    scan_directory,
)
from lidarbench.datasets.manifest import (
    DatasetManifest,
    DerivedFrom,
    Location,
    ManifestError,
    Provenance,
    ProvenanceRecord,
)
from lidarbench.datasets.recipes import DatasetSpec, Recipe
from lidarbench.datasets.registry import DatasetRegistry, RegistryError
from lidarbench.datasets.roots import DataRoots

LEGACY_IMPORTERS = (legacy_petawawa.IMPORTER,)


class RegistrationError(RuntimeError):
    """The data cannot be registered as declared."""


@dataclass(frozen=True)
class RegistrationResult:
    """A registered (or already registered) manifest."""

    manifest: DatasetManifest
    path: Path
    created: bool


def _records(recipe: Recipe, roots: DataRoots) -> dict[str, ProvenanceRecord]:
    out: dict[str, ProvenanceRecord] = {}
    for spec in recipe.records:
        path = roots.resolve(recipe.root, spec.path)
        if not path.is_file() or path.is_symlink():
            raise RegistrationError(
                f"provenance record {spec.path} not found below root {recipe.root!r} ({path})"
            )
        sha, md5, size = hash_file(path, md5=spec.md5)
        out[spec.path] = ProvenanceRecord(
            Location(recipe.root, spec.path), size, sha, md5, spec.description
        )
    return out


def _derived_from(recipe: Recipe, registry: DatasetRegistry) -> tuple[DerivedFrom, ...]:
    out = []
    for item in recipe.derived_from:
        content = item.content_sha256
        if (
            item.dataset_id is not None
            and content is None
            and registry.has_manifest(item.dataset_id)
        ):
            content = registry.manifest(item.dataset_id).content_sha256
        out.append(DerivedFrom(item.description, item.dataset_id, content))
    return tuple(out)


def _utc_now() -> str:
    return _dt.datetime.now(_dt.UTC).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def build_manifest(
    recipe: Recipe,
    spec: DatasetSpec,
    scan: ScanResult,
    records: dict[str, ProvenanceRecord],
    registry: DatasetRegistry,
    *,
    details: dict[str, Any] | None = None,
    notes: tuple[str, ...] = (),
    registered_at: str | None = None,
) -> DatasetManifest:
    """Assemble a manifest from a hashed scan."""
    preparation = recipe.preparation.to_record()
    preparation["dataset_parameters"] = dict(spec.parameters)
    registration: dict[str, Any] = {
        "lidarbench_version": lidarbench.__version__,
        "registered_at": registered_at or _utc_now(),
        "registered_on": platform.system(),
    }
    if scan.excluded:
        registration["excluded_files_present"] = list(scan.excluded)
    return DatasetManifest(
        dataset_id=spec.id,
        version=recipe.version,
        title=spec.title,
        kind=recipe.kind,
        recipe=recipe.id,
        location=Location(recipe.root, spec.path),
        layout=recipe.layout,
        files=scan.entries(),
        license=recipe.license,
        declared_metadata=recipe.declared_metadata,
        provenance=Provenance(
            source_citation=recipe.source_citation,
            source_url=recipe.source_url,
            derived_from=_derived_from(recipe, registry),
            preparation=preparation,
            records=tuple(records[k] for k in sorted(records)),
            details=details or {},
            notes=recipe.notes + notes,
        ),
        registration=registration,
    )


def register_dataset(
    recipe: Recipe,
    spec: DatasetSpec,
    registry: DatasetRegistry,
    roots: DataRoots,
    *,
    legacy_export: Path | None = None,
    skip_legacy_crosscheck: bool = False,
    progress: Progress | None = None,
    cancelled: Callable[[], bool] | None = None,
    registered_at: str | None = None,
) -> RegistrationResult:
    """Register one dataset of ``recipe``.

    Raises:
        RegistrationError: Missing data or receipts, inconsistent legacy receipts,
            interrupted preparation, or a conflicting existing manifest.
    """
    root_dir = roots.path(recipe.root)
    if (root_dir / LEGACY_LOCK).exists():
        raise RegistrationError(
            f"{root_dir / LEGACY_LOCK} exists: an R preparation is active or was interrupted"
        )
    folder = roots.resolve(recipe.root, spec.path)
    legacy = recipe.legacy_receipts
    if legacy is not None and legacy not in LEGACY_IMPORTERS:
        raise RegistrationError(f"unknown legacy receipt importer {legacy!r}")
    if legacy is not None and legacy_export is None and not skip_legacy_crosscheck:
        raise RegistrationError(
            f"recipe {recipe.id} has legacy R receipts: pass --legacy-export (written by "
            "bridges/r/export_petawawa_receipts.R) or explicitly --skip-legacy-crosscheck"
        )
    try:
        scan = scan_directory(
            folder,
            recipe.layout,
            md5=legacy is not None or any(r.md5 for r in recipe.records),
            progress=progress,
            cancelled=cancelled,
        )
    except InventoryError as exc:
        raise RegistrationError(f"{spec.id}: {exc}") from None
    records = _records(recipe, roots)
    details: dict[str, Any] = {}
    notes: tuple[str, ...] = ()
    if legacy is not None and legacy_export is not None:
        try:
            export, export_sha = legacy_petawawa.load_export(legacy_export)
            problems = legacy_petawawa.crosscheck(
                export, spec.parameters, recipe.preparation.method, scan, records
            )
        except (legacy_petawawa.LegacyReceiptError, ManifestError) as exc:
            raise RegistrationError(str(exc)) from None
        if problems:
            shown = "\n  ".join(problems[:20])
            raise RegistrationError(f"{spec.id}: legacy receipts disagree:\n  {shown}")
        details["legacy_receipts"] = legacy_petawawa.details(export, export_sha, spec.parameters)
        notes = ("Every file MD5 was cross-checked against the R preparation receipts.",)
    elif legacy is not None:
        notes = ("Registered WITHOUT the legacy R receipt cross-check (explicitly skipped).",)
    manifest = build_manifest(
        recipe,
        spec,
        scan,
        records,
        registry,
        details=details,
        notes=notes,
        registered_at=registered_at,
    )
    try:
        path, created = registry.publish(manifest)
    except RegistryError as exc:
        raise RegistrationError(str(exc)) from None
    return RegistrationResult(manifest if created else registry.manifest(spec.id), path, created)
