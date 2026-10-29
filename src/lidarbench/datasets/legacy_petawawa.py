"""Cross-check a historical Petawawa preparation against its R receipts.

The R preparation (``R/prepare_fresh_data.R``) recorded MD5 receipts in RDS
files. ``bridges/r/export_petawawa_receipts.R`` exports their plain fields
once as JSON (``lidarbench.petawawa-receipts/1``); this module compares that
export with the bytes lidarbench hashes during registration. Nothing is
recomputed or repaired: the receipts are evidence, and any disagreement
refuses registration.

Recipe dataset ``parameters`` select what a directory is:

* ``{part = "prepared", target_density = N}``: ``1_org/tile_1000_density_N``;
* ``{part = "repaired"}``: the repaired source tiles;
* ``{part = "archive"}``: the downloaded original archive.
"""

from __future__ import annotations

from collections.abc import Mapping
from pathlib import Path
from typing import Any

from lidarbench.core.canonical import sha256_bytes
from lidarbench.core.store import read_json
from lidarbench.datasets.inventory import ScanResult
from lidarbench.datasets.manifest import ManifestError, ProvenanceRecord

EXPORT_SCHEMA = "lidarbench.petawawa-receipts/1"
IMPORTER = "petawawa-fresh"
_SECTIONS = (
    "schema",
    "stage",
    "root",
    "download",
    "stage_identity",
    "preparation",
    "ready",
    "repairs",
    "tiles",
)


class LegacyReceiptError(ValueError):
    """The receipts are missing, malformed or disagree with the data."""


def load_export(path: Path) -> tuple[dict[str, Any], str]:
    """Read an export; returns (record, SHA-256 of the file bytes)."""
    try:
        raw = path.read_bytes()
        data = read_json(path)
    except (OSError, ValueError) as exc:
        raise LegacyReceiptError(f"cannot read receipt export {path}: {exc}") from exc
    if not isinstance(data, dict) or data.get("schema") != EXPORT_SCHEMA:
        raise LegacyReceiptError(f"{path}: schema must be {EXPORT_SCHEMA!r}")
    missing = [k for k in _SECTIONS if k not in data]
    if missing:
        raise LegacyReceiptError(f"{path}: missing section(s) {missing}")
    return data, sha256_bytes(raw)


def _record_md5(records: Mapping[str, ProvenanceRecord], suffix: str) -> str | None:
    for path, record in records.items():
        if path == suffix or path.endswith("/" + suffix):
            return record.md5
    return None


def check_chain(
    export: Mapping[str, Any], method: str, records: Mapping[str, ProvenanceRecord]
) -> list[str]:
    """Checks common to every dataset of the stage (readiness, archive, policy)."""
    problems = []
    stage = export["stage"]
    ready, prep, download = export["ready"], export["preparation"], export["download"]
    if ready.get("status") != "data-qc-passed":
        problems.append(f"{stage}/ready.rds status is {ready.get('status')!r}, not data-qc-passed")
    if ready.get("policy_version") != method or prep.get("method") != method:
        problems.append(
            f"policy {ready.get('policy_version')!r}/{prep.get('method')!r} != recipe method "
            f"{method!r}"
        )
    archives = {
        download.get("md5"),
        ready.get("archive_md5"),
        prep.get("archive_md5"),
        export["stage_identity"].get("archive_md5"),
    }
    if len(archives) != 1:
        problems.append(f"archive MD5 differs between receipts: {sorted(map(str, archives))}")
    transition = export.get("negative_z_transition")
    if transition is not None and transition.get("current_policy") != method:
        problems.append("negative-Z transition does not end at the recipe method")
    checks = (
        (f"{stage}/prepared/preparation.rds", ready.get("preparation_md5")),
        ("download/download.rds", None),
    )
    for suffix, expected in checks:
        actual = _record_md5(records, suffix)
        if actual is None:
            problems.append(f"provenance record {suffix} was not captured")
        elif expected is not None and actual != expected:
            problems.append(f"{suffix} MD5 {actual} != readiness receipt {expected}")
    return problems


def _by_path(scan: ScanResult) -> dict[str, Any]:
    return {f.path: f for f in scan.files}


def check_prepared(export: Mapping[str, Any], density: Any, scan: ScanResult) -> list[str]:
    """Compare one density directory with its per-tile receipts."""
    problems = []
    files = _by_path(scan)
    tiles = [t for t in export["tiles"] if t.get("density") == density]
    if not tiles:
        return [f"no tile receipts for target density {density!r}"]
    repaired = {r["tile"]: r.get("md5") for r in export["repairs"]}
    expected: set[str] = set()
    for t in tiles:
        name = f"{t['tile']}.laz"
        lax, receipt = f"{t['tile']}.lax", f"{t['tile']}.laz.rds"
        expected |= {name, lax, receipt}
        laz_file, lax_file = files.get(name), files.get(lax)
        if laz_file is None or lax_file is None or receipt not in files:
            problems.append(f"{t['tile']}: prepared tile, index or receipt missing")
            continue
        if laz_file.md5 != t.get("laz_md5"):
            problems.append(f"{name}: MD5 {laz_file.md5} != receipt {t.get('laz_md5')}")
        if lax_file.md5 != t.get("lax_md5"):
            problems.append(f"{lax}: MD5 {lax_file.md5} != receipt {t.get('lax_md5')}")
        if laz_file.las is None or laz_file.las.points != t.get("points"):
            problems.append(f"{name}: header point count differs from the receipt")
        if t.get("index_validation") != "passed":
            problems.append(f"{name}: index-query validation not recorded as passed")
        if t.get("source_md5") != repaired.get(t["tile"]):
            problems.append(f"{name}: source MD5 differs from the repair receipt")
    extra = sorted(set(files) - expected)
    if extra:
        problems.append(f"files without tile receipts: {extra[:5]}")
    return problems


def check_repaired(export: Mapping[str, Any], scan: ScanResult) -> list[str]:
    """Compare the repaired source tiles with the repair receipts."""
    problems = []
    files = _by_path(scan)
    prep = export["preparation"]
    sources = dict(zip(prep.get("tiles", []), prep.get("source_md5", []), strict=False))
    receipts_md5 = dict(zip(prep.get("tiles", []), prep.get("repairs_md5", []), strict=False))
    method = prep.get("method")
    expected: set[str] = set()
    for r in export["repairs"]:
        tile = r["tile"]
        name, receipt = f"{tile}.laz", f"{tile}.laz.rds"
        expected |= {name, receipt}
        if name not in files or receipt not in files:
            problems.append(f"{tile}: repaired tile or repair receipt missing")
            continue
        if files[name].md5 != r.get("md5") or r.get("md5") != sources.get(tile):
            problems.append(f"{name}: MD5 differs from the repair receipt or preparation sources")
        if files[receipt].md5 != receipts_md5.get(tile):
            problems.append(f"{receipt}: MD5 differs from preparation.rds repairs_md5")
        if r.get("policy_version") is None:
            # fresh_check_repair_policy(): receipts without a policy are accepted
            # under the negative-Z policy only if no audited source Z is negative.
            if r.get("negative_source_z") != 0:
                problems.append(f"{receipt}: pre-policy repair audit has negative source Z")
        elif r.get("policy_version") != method:
            problems.append(f"{receipt}: repair policy {r.get('policy_version')!r} != {method!r}")
    extra = sorted(set(files) - expected)
    if extra:
        problems.append(f"files without repair receipts: {extra[:5]}")
    return problems


def check_archive(export: Mapping[str, Any], scan: ScanResult) -> list[str]:
    """Compare the downloaded archive with the download receipt."""
    download = export["download"]
    inputs = [f for f in scan.files if f.role == "input"]
    if len(inputs) != 1:
        return ["the archive dataset must contain exactly one input file"]
    archive = inputs[0]
    problems = []
    if archive.md5 != download.get("md5"):
        problems.append(
            f"{archive.path}: MD5 {archive.md5} != download receipt {download.get('md5')}"
        )
    if archive.bytes != download.get("bytes"):
        problems.append(f"{archive.path}: size differs from the download receipt")
    return problems


def crosscheck(
    export: Mapping[str, Any],
    parameters: Mapping[str, Any],
    method: str,
    scan: ScanResult,
    records: Mapping[str, ProvenanceRecord],
) -> list[str]:
    """All checks for one dataset directory. Returns problems (empty if consistent)."""
    part = parameters.get("part")
    if part == "archive":
        return check_archive(export, scan)
    problems = check_chain(export, method, records)
    if part == "prepared":
        problems += check_prepared(export, parameters.get("target_density"), scan)
    elif part == "repaired":
        problems += check_repaired(export, scan)
    else:
        raise ManifestError(
            f"dataset parameters.part must be archive/prepared/repaired, got {part!r}"
        )
    return problems


def details(
    export: Mapping[str, Any], export_sha256: str, parameters: Mapping[str, Any]
) -> dict[str, Any]:
    """Provenance details copied into the manifest (receipt facts, not recomputed)."""
    prep = export["preparation"]
    out: dict[str, Any] = {
        "schema": EXPORT_SCHEMA,
        "export_sha256": export_sha256,
        "stage": export["stage"],
        "checksum_note": "MD5 values are content identifiers from the R receipts, "
        "cross-checked against the registered bytes; they do not establish authenticity.",
        "download": export["download"],
        "root": export["root"],
        "negative_z_transition": export.get("negative_z_transition"),
        "parallel_transition": export.get("parallel_transition"),
        "ready": export["ready"],
        "stage_identity": {k: v for k, v in export["stage_identity"].items() if k != "members"},
        "preparation": {k: v for k, v in prep.items() if k not in ("source_md5", "repairs_md5")},
    }
    part = parameters.get("part")
    if part == "prepared":
        out["tiles"] = [
            {k: t[k] for k in ("tile", "seed", "points", "source_md5", "index_validation")}
            for t in export["tiles"]
            if t.get("density") == parameters.get("target_density")
        ]
    elif part == "repaired":
        out["repairs"] = list(export["repairs"])
        out["archive_members"] = export["stage_identity"].get("members")
    return out
