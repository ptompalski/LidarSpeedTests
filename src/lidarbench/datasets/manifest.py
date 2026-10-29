"""Dataset manifests (``lidarbench.dataset/1``): schema, strict parsing and identity.

A manifest describes one directory of files exactly: every file's POSIX
relative path, role, size, SHA-256 (and optionally MD5, kept only to
cross-check legacy R receipts), plus LAS/LAZ public-header facts for point
clouds. It also declares the dataset kind (original ``source`` data,
``derived`` benchmark data or a synthetic ``fixture``), its version, license,
declared (never inferred) spatial metadata and provenance.

Identities:

* ``content_sha256`` -- digest of the sorted ``(path, role, bytes, sha256)``
  inventory. Identical bytes give an identical content identity on every OS.
* :func:`manifest_sha256` -- digest of the canonical manifest record. A
  campaign can pin it.

Parsing follows the configuration rules of the project: unknown keys are
errors, values are never coerced and errors name the offending key. See
``design/DATASETS.md``.
"""

from __future__ import annotations

import re
from collections.abc import Mapping, Sequence
from dataclasses import dataclass, field
from typing import Any

from lidarbench.core.canonical import CanonicalError, canonical_bytes, digest
from lidarbench.datasets.lasheader import LasHeader
from lidarbench.datasets.roots import DataRootError, check_label, check_relative_path

MANIFEST_SCHEMA = "lidarbench.dataset/1"
CONTENT_SCHEMA = "lidarbench.dataset-content/1"

DATASET_ID_PATTERN = re.compile(r"^[a-z0-9][a-z0-9_.-]{0,63}(/[a-z0-9][a-z0-9_.-]{0,63}){0,3}$")
VERSION_PATTERN = re.compile(r"^[A-Za-z0-9][A-Za-z0-9_.+-]{0,63}$")
SHA256_PATTERN = re.compile(r"^[0-9a-f]{64}$")
MD5_PATTERN = re.compile(r"^[0-9a-f]{32}$")

KINDS = ("source", "derived", "fixture")
"""``source``: original data as obtained from its custodian (never modified).
``derived``: produced from source data by a documented recipe.
``fixture``: small synthetic data for automated tests (not a benchmark result)."""

ROLES = ("input", "index", "receipt", "auxiliary")
"""``input``: files handed to tools; ``index``: spatial indexes (e.g. ``.lax``);
``receipt``: preparation receipts; ``auxiliary``: anything else declared."""

LAS_HEADER_POLICIES = ("required", "optional", "none")
REDISTRIBUTION = ("permitted", "permitted-with-attribution", "not-permitted", "unknown")


class ManifestError(ValueError):
    """A manifest or recipe is invalid."""


# --------------------------------------------------------------------------- helpers


def _obj(
    value: Any, where: str, required: Sequence[str], optional: Sequence[str] = ()
) -> dict[str, Any]:
    if not isinstance(value, Mapping):
        raise ManifestError(f"{where} must be an object")
    allowed = set(required) | set(optional)
    unknown = sorted(set(value) - allowed)
    if unknown:
        raise ManifestError(f"{where}: unknown key(s) {unknown}; allowed: {sorted(allowed)}")
    missing = sorted(set(required) - set(value))
    if missing:
        raise ManifestError(f"{where}: missing key(s) {missing}")
    return dict(value)


def _str(value: Any, where: str, *, empty: bool = False) -> str:
    if not isinstance(value, str) or (not empty and not value.strip()):
        raise ManifestError(f"{where} must be a {'' if empty else 'non-empty '}string")
    return value


def _opt_str(value: Any, where: str) -> str | None:
    return None if value is None else _str(value, where)


def _int(value: Any, where: str, minimum: int = 0) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value < minimum:
        raise ManifestError(f"{where} must be an integer >= {minimum}, got {value!r}")
    return value


def _opt_int(value: Any, where: str) -> int | None:
    return None if value is None else _int(value, where, 1)


def _float3(value: Any, where: str) -> tuple[float, float, float]:
    if (
        not isinstance(value, list | tuple)
        or len(value) != 3
        or not all(isinstance(v, int | float) and not isinstance(v, bool) for v in value)
    ):
        raise ManifestError(f"{where} must be an array of three numbers")
    return (float(value[0]), float(value[1]), float(value[2]))


def _choice(value: Any, where: str, choices: Sequence[str]) -> str:
    if value not in choices:
        raise ManifestError(f"{where} must be one of {list(choices)}, got {value!r}")
    return str(value)


def _hex(value: Any, where: str, pattern: re.Pattern[str]) -> str:
    if not isinstance(value, str) or not pattern.match(value):
        raise ManifestError(f"{where} must be a lowercase hex digest, got {value!r}")
    return value


def _patterns(value: Any, where: str) -> tuple[str, ...]:
    if not isinstance(value, list | tuple) or not all(
        isinstance(p, str) and p and "/" not in p and "\\" not in p for p in value
    ):
        raise ManifestError(f"{where} must be an array of file-name patterns")
    return tuple(value)


def _json_object(value: Any, where: str) -> dict[str, Any]:
    if not isinstance(value, Mapping):
        raise ManifestError(f"{where} must be an object")
    try:
        canonical_bytes(value)
    except CanonicalError as exc:
        raise ManifestError(f"{where}: {exc}") from None
    return dict(value)


def _rel(value: Any, where: str, *, allow_empty: bool = False) -> str:
    try:
        return check_relative_path(value, where, allow_empty=allow_empty)
    except DataRootError as exc:
        raise ManifestError(str(exc)) from None


def check_dataset_id(value: Any, where: str = "dataset_id") -> str:
    """Validate a dataset ID such as ``petawawa-fresh-v4/tile_1000_density_1``."""
    if not isinstance(value, str) or not DATASET_ID_PATTERN.match(value):
        raise ManifestError(f"{where} must match {DATASET_ID_PATTERN.pattern}, got {value!r}")
    return value


# --------------------------------------------------------------------------- parts


@dataclass(frozen=True)
class LasFacts:
    """Public-header facts of a LAS/LAZ file (read without any LiDAR library)."""

    version: str
    point_format: int
    compressed: bool
    points: int
    scale: tuple[float, float, float]
    offset: tuple[float, float, float]
    min: tuple[float, float, float]
    max: tuple[float, float, float]
    crs: str | None
    """``"wkt"``, ``"geotiff"`` or ``None``: which CRS record the header carries."""

    @classmethod
    def from_header(cls, header: LasHeader) -> LasFacts:
        """Facts of a parsed header."""
        return cls(
            version=f"{header.version[0]}.{header.version[1]}",
            point_format=header.point_format,
            compressed=header.compressed,
            points=header.point_count,
            scale=header.scale,
            offset=header.offset,
            min=header.min_xyz,
            max=header.max_xyz,
            crs="wkt" if header.crs_wkt else ("geotiff" if header.has_geotiff_keys else None),
        )

    @classmethod
    def parse(cls, value: Any, where: str) -> LasFacts:
        """Strictly parse a JSON object."""
        d = _obj(
            value,
            where,
            (
                "version",
                "point_format",
                "compressed",
                "points",
                "scale",
                "offset",
                "min",
                "max",
                "crs",
            ),
        )
        if not isinstance(d["compressed"], bool):
            raise ManifestError(f"{where}.compressed must be true or false")
        return cls(
            version=_str(d["version"], f"{where}.version"),
            point_format=_int(d["point_format"], f"{where}.point_format"),
            compressed=d["compressed"],
            points=_int(d["points"], f"{where}.points"),
            scale=_float3(d["scale"], f"{where}.scale"),
            offset=_float3(d["offset"], f"{where}.offset"),
            min=_float3(d["min"], f"{where}.min"),
            max=_float3(d["max"], f"{where}.max"),
            crs=None if d["crs"] is None else _choice(d["crs"], f"{where}.crs", ("wkt", "geotiff")),
        )

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "version": self.version,
            "point_format": self.point_format,
            "compressed": self.compressed,
            "points": self.points,
            "scale": list(self.scale),
            "offset": list(self.offset),
            "min": list(self.min),
            "max": list(self.max),
            "crs": self.crs,
        }


@dataclass(frozen=True)
class FileEntry:
    """One file of a dataset."""

    path: str
    role: str
    bytes: int
    sha256: str
    md5: str | None = None
    las: LasFacts | None = None

    @classmethod
    def parse(cls, value: Any, where: str) -> FileEntry:
        """Strictly parse a JSON object."""
        d = _obj(value, where, ("path", "role", "bytes", "sha256"), ("md5", "las"))
        return cls(
            path=_rel(d["path"], f"{where}.path"),
            role=_choice(d["role"], f"{where}.role", ROLES),
            bytes=_int(d["bytes"], f"{where}.bytes"),
            sha256=_hex(d["sha256"], f"{where}.sha256", SHA256_PATTERN),
            md5=None if d.get("md5") is None else _hex(d["md5"], f"{where}.md5", MD5_PATTERN),
            las=None if d.get("las") is None else LasFacts.parse(d["las"], f"{where}.las"),
        )

    def identity_record(self) -> dict[str, Any]:
        """The part of the entry that defines dataset content identity."""
        return {"path": self.path, "role": self.role, "bytes": self.bytes, "sha256": self.sha256}

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        record: dict[str, Any] = self.identity_record()
        if self.md5 is not None:
            record["md5"] = self.md5
        if self.las is not None:
            record["las"] = self.las.to_record()
        return record


@dataclass(frozen=True)
class Location:
    """Portable location: a root label plus a POSIX path relative to that root."""

    root: str
    path: str

    @classmethod
    def parse(cls, value: Any, where: str) -> Location:
        """Strictly parse a JSON object."""
        d = _obj(value, where, ("root", "path"))
        try:
            root = check_label(d["root"], f"{where}.root")
        except DataRootError as exc:
            raise ManifestError(str(exc)) from None
        return cls(root, _rel(d["path"], f"{where}.path", allow_empty=True))

    def to_record(self) -> dict[str, str]:
        """JSON-ready representation."""
        return {"root": self.root, "path": self.path}


@dataclass(frozen=True)
class Layout:
    """How files are classified into roles and which files are deliberately excluded."""

    inputs: tuple[str, ...] = ("*.las", "*.laz")
    indexes: tuple[str, ...] = ()
    receipts: tuple[str, ...] = ()
    auxiliary: tuple[str, ...] = ()
    exclude: tuple[str, ...] = ()
    las_headers: str = "required"

    @classmethod
    def parse(cls, value: Any, where: str) -> Layout:
        """Strictly parse a JSON object or TOML table (defaults resolved)."""
        d = _obj(
            value,
            where,
            (),
            ("inputs", "indexes", "receipts", "auxiliary", "exclude", "las_headers"),
        )
        defaults = cls()
        layout = cls(
            inputs=_patterns(d.get("inputs", list(defaults.inputs)), f"{where}.inputs"),
            indexes=_patterns(d.get("indexes", []), f"{where}.indexes"),
            receipts=_patterns(d.get("receipts", []), f"{where}.receipts"),
            auxiliary=_patterns(d.get("auxiliary", []), f"{where}.auxiliary"),
            exclude=_patterns(d.get("exclude", []), f"{where}.exclude"),
            las_headers=_choice(
                d.get("las_headers", defaults.las_headers),
                f"{where}.las_headers",
                LAS_HEADER_POLICIES,
            ),
        )
        if not layout.inputs:
            raise ManifestError(f"{where}.inputs must name at least one pattern")
        return layout

    def role_patterns(self) -> dict[str, tuple[str, ...]]:
        """Role -> file-name patterns."""
        return {
            "input": self.inputs,
            "index": self.indexes,
            "receipt": self.receipts,
            "auxiliary": self.auxiliary,
        }

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "inputs": list(self.inputs),
            "indexes": list(self.indexes),
            "receipts": list(self.receipts),
            "auxiliary": list(self.auxiliary),
            "exclude": list(self.exclude),
            "las_headers": self.las_headers,
        }


@dataclass(frozen=True)
class License:
    """License and redistribution terms, as established by the data custodian."""

    name: str
    url: str | None = None
    attribution: str | None = None
    redistribution: str = "unknown"
    notes: str | None = None

    @classmethod
    def parse(cls, value: Any, where: str) -> License:
        """Strictly parse a JSON object or TOML table."""
        d = _obj(value, where, ("name",), ("url", "attribution", "redistribution", "notes"))
        return cls(
            name=_str(d["name"], f"{where}.name"),
            url=_opt_str(d.get("url"), f"{where}.url"),
            attribution=_opt_str(d.get("attribution"), f"{where}.attribution"),
            redistribution=_choice(
                d.get("redistribution", "unknown"), f"{where}.redistribution", REDISTRIBUTION
            ),
            notes=_opt_str(d.get("notes"), f"{where}.notes"),
        )

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "name": self.name,
            "url": self.url,
            "attribution": self.attribution,
            "redistribution": self.redistribution,
            "notes": self.notes,
        }


@dataclass(frozen=True)
class DeclaredMetadata:
    """Spatial metadata *declared* by the custodian or preparer; never inferred."""

    horizontal_epsg: int | None = None
    vertical_epsg: int | None = None
    xyz_units: str | None = None
    confirmation: str | None = None

    @classmethod
    def parse(cls, value: Any, where: str) -> DeclaredMetadata:
        """Strictly parse a JSON object or TOML table."""
        d = _obj(
            value, where, (), ("horizontal_epsg", "vertical_epsg", "xyz_units", "confirmation")
        )
        return cls(
            horizontal_epsg=_opt_int(d.get("horizontal_epsg"), f"{where}.horizontal_epsg"),
            vertical_epsg=_opt_int(d.get("vertical_epsg"), f"{where}.vertical_epsg"),
            xyz_units=_opt_str(d.get("xyz_units"), f"{where}.xyz_units"),
            confirmation=_opt_str(d.get("confirmation"), f"{where}.confirmation"),
        )

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "horizontal_epsg": self.horizontal_epsg,
            "vertical_epsg": self.vertical_epsg,
            "xyz_units": self.xyz_units,
            "confirmation": self.confirmation,
        }


@dataclass(frozen=True)
class ProvenanceRecord:
    """An immutable receipt outside the dataset directory (e.g. a readiness receipt)."""

    location: Location
    bytes: int
    sha256: str
    md5: str | None = None
    description: str = ""

    @classmethod
    def parse(cls, value: Any, where: str) -> ProvenanceRecord:
        """Strictly parse a JSON object."""
        d = _obj(value, where, ("location", "bytes", "sha256"), ("md5", "description"))
        return cls(
            location=Location.parse(d["location"], f"{where}.location"),
            bytes=_int(d["bytes"], f"{where}.bytes"),
            sha256=_hex(d["sha256"], f"{where}.sha256", SHA256_PATTERN),
            md5=None if d.get("md5") is None else _hex(d["md5"], f"{where}.md5", MD5_PATTERN),
            description=_str(d.get("description", ""), f"{where}.description", empty=True),
        )

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        record: dict[str, Any] = {
            "location": self.location.to_record(),
            "bytes": self.bytes,
            "sha256": self.sha256,
            "description": self.description,
        }
        if self.md5 is not None:
            record["md5"] = self.md5
        return record


@dataclass(frozen=True)
class DerivedFrom:
    """A dataset (or external original) this dataset was derived from."""

    description: str
    dataset_id: str | None = None
    content_sha256: str | None = None

    @classmethod
    def parse(cls, value: Any, where: str) -> DerivedFrom:
        """Strictly parse a JSON object or TOML table."""
        d = _obj(value, where, ("description",), ("dataset_id", "content_sha256"))
        return cls(
            description=_str(d["description"], f"{where}.description"),
            dataset_id=(
                None
                if d.get("dataset_id") is None
                else check_dataset_id(d["dataset_id"], f"{where}.dataset_id")
            ),
            content_sha256=(
                None
                if d.get("content_sha256") is None
                else _hex(d["content_sha256"], f"{where}.content_sha256", SHA256_PATTERN)
            ),
        )

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "description": self.description,
            "dataset_id": self.dataset_id,
            "content_sha256": self.content_sha256,
        }


@dataclass(frozen=True)
class Provenance:
    """Where the data came from and how it was produced."""

    source_citation: str
    source_url: str | None = None
    derived_from: tuple[DerivedFrom, ...] = ()
    preparation: Mapping[str, Any] = field(default_factory=dict)
    records: tuple[ProvenanceRecord, ...] = ()
    details: Mapping[str, Any] = field(default_factory=dict)
    notes: tuple[str, ...] = ()

    @classmethod
    def parse(cls, value: Any, where: str) -> Provenance:
        """Strictly parse a JSON object."""
        d = _obj(
            value,
            where,
            ("source_citation",),
            ("source_url", "derived_from", "preparation", "records", "details", "notes"),
        )
        notes = d.get("notes", [])
        if not isinstance(notes, list) or not all(isinstance(n, str) for n in notes):
            raise ManifestError(f"{where}.notes must be an array of strings")
        return cls(
            source_citation=_str(d["source_citation"], f"{where}.source_citation"),
            source_url=_opt_str(d.get("source_url"), f"{where}.source_url"),
            derived_from=tuple(
                DerivedFrom.parse(v, f"{where}.derived_from[{i}]")
                for i, v in enumerate(_list(d.get("derived_from", []), f"{where}.derived_from"))
            ),
            preparation=_json_object(d.get("preparation", {}), f"{where}.preparation"),
            records=tuple(
                ProvenanceRecord.parse(v, f"{where}.records[{i}]")
                for i, v in enumerate(_list(d.get("records", []), f"{where}.records"))
            ),
            details=_json_object(d.get("details", {}), f"{where}.details"),
            notes=tuple(notes),
        )

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation."""
        return {
            "source_citation": self.source_citation,
            "source_url": self.source_url,
            "derived_from": [d.to_record() for d in self.derived_from],
            "preparation": dict(self.preparation),
            "records": [r.to_record() for r in self.records],
            "details": dict(self.details),
            "notes": list(self.notes),
        }


def _list(value: Any, where: str) -> list[Any]:
    if not isinstance(value, list | tuple):
        raise ManifestError(f"{where} must be an array")
    return list(value)


# --------------------------------------------------------------------------- manifest


def content_digest(files: Sequence[FileEntry]) -> str:
    """Content identity of an inventory (order-independent)."""
    ordered = sorted(files, key=lambda f: f.path)
    return digest({"schema": CONTENT_SCHEMA, "files": [f.identity_record() for f in ordered]})


@dataclass(frozen=True)
class DatasetManifest:
    """A complete, validated ``lidarbench.dataset/1`` manifest."""

    dataset_id: str
    version: str
    title: str
    kind: str
    location: Location
    layout: Layout
    files: tuple[FileEntry, ...]
    license: License
    provenance: Provenance
    declared_metadata: DeclaredMetadata = field(default_factory=DeclaredMetadata)
    recipe: str | None = None
    registration: Mapping[str, Any] = field(default_factory=dict)

    def __post_init__(self) -> None:
        paths = [f.path for f in self.files]
        if paths != sorted(paths):
            raise ManifestError("files must be sorted by path")
        folded = [p.casefold() for p in paths]
        if len(set(folded)) != len(folded):
            raise ManifestError(
                "files contain duplicate paths or paths differing only in case "
                "(not representable on Windows/macOS)"
            )
        if not self.inputs:
            raise ManifestError("a dataset needs at least one file with role 'input'")

    @property
    def content_sha256(self) -> str:
        """Content identity (see module docstring)."""
        return content_digest(self.files)

    @property
    def inputs(self) -> tuple[FileEntry, ...]:
        """Files given to tools, in path order."""
        return tuple(f for f in self.files if f.role == "input")

    @property
    def total_bytes(self) -> int:
        """Sum of all file sizes."""
        return sum(f.bytes for f in self.files)

    def file(self, path: str) -> FileEntry:
        """Entry by relative path."""
        for entry in self.files:
            if entry.path == path:
                return entry
        raise KeyError(path)

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation (includes ``content_sha256``)."""
        return {
            "schema": MANIFEST_SCHEMA,
            "dataset_id": self.dataset_id,
            "version": self.version,
            "title": self.title,
            "kind": self.kind,
            "recipe": self.recipe,
            "location": self.location.to_record(),
            "layout": self.layout.to_record(),
            "content_sha256": self.content_sha256,
            "file_count": len(self.files),
            "total_bytes": self.total_bytes,
            "files": [f.to_record() for f in self.files],
            "declared_metadata": self.declared_metadata.to_record(),
            "license": self.license.to_record(),
            "provenance": self.provenance.to_record(),
            "registration": dict(self.registration),
        }

    def summary(self) -> dict[str, Any]:
        """Short JSON-ready description (for listings and campaign identity)."""
        return {
            "dataset_id": self.dataset_id,
            "version": self.version,
            "kind": self.kind,
            "title": self.title,
            "recipe": self.recipe,
            "location": self.location.to_record(),
            "content_sha256": self.content_sha256,
            "manifest_sha256": manifest_sha256(self),
            "file_count": len(self.files),
            "input_count": len(self.inputs),
            "total_bytes": self.total_bytes,
            "license": self.license.name,
            "redistribution": self.license.redistribution,
        }


def manifest_sha256(manifest: DatasetManifest) -> str:
    """Digest of the canonical manifest record."""
    return digest(manifest.to_record())


def parse_manifest(value: Any) -> DatasetManifest:
    """Strictly parse a manifest record.

    ``content_sha256``, ``file_count`` and ``total_bytes`` are recomputed and
    must equal the stored values, so a hand-edited inventory is detected.

    Raises:
        ManifestError: With a message naming the offending key.
    """
    d = _obj(
        value,
        "manifest",
        (
            "schema",
            "dataset_id",
            "version",
            "title",
            "kind",
            "location",
            "layout",
            "content_sha256",
            "file_count",
            "total_bytes",
            "files",
            "license",
            "provenance",
        ),
        ("recipe", "declared_metadata", "registration"),
    )
    if d["schema"] != MANIFEST_SCHEMA:
        raise ManifestError(f"schema must be {MANIFEST_SCHEMA!r}, got {d['schema']!r}")
    version = d["version"]
    if not isinstance(version, str) or not VERSION_PATTERN.match(version):
        raise ManifestError(f"version must match {VERSION_PATTERN.pattern}, got {version!r}")
    files = tuple(
        FileEntry.parse(v, f"files[{i}]") for i, v in enumerate(_list(d["files"], "files"))
    )
    recipe = d.get("recipe")
    manifest = DatasetManifest(
        dataset_id=check_dataset_id(d["dataset_id"]),
        version=version,
        title=_str(d["title"], "title"),
        kind=_choice(d["kind"], "kind", KINDS),
        recipe=None if recipe is None else _str(recipe, "recipe"),
        location=Location.parse(d["location"], "location"),
        layout=Layout.parse(d["layout"], "layout"),
        files=files,
        declared_metadata=DeclaredMetadata.parse(
            d.get("declared_metadata", {}), "declared_metadata"
        ),
        license=License.parse(d["license"], "license"),
        provenance=Provenance.parse(d["provenance"], "provenance"),
        registration=_json_object(d.get("registration", {}), "registration"),
    )
    for key, actual in (
        ("content_sha256", manifest.content_sha256),
        ("file_count", len(files)),
        ("total_bytes", manifest.total_bytes),
    ):
        if d[key] != actual:
            raise ManifestError(f"{key} {d[key]!r} does not match the file inventory ({actual!r})")
    return manifest
