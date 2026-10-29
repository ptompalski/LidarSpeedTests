"""Dataset recipes (``lidarbench.dataset-recipe/1``, TOML).

A recipe is the human-written, version-controlled description of a dataset
family: its explicit version, kind (source/derived/fixture), license, declared
metadata, file layout, provenance receipts, how the data are prepared (and
whether lidarbench itself can prepare them), optional download terms, and the
concrete datasets (directories below a data root) it yields.

Preparation engines:

``python``  lidarbench generates the data itself (synthetic fixtures only).
``r-cli``   the historical R preparation CLI produces the data; lidarbench only
            documents and, on explicit request, invokes it unchanged.
``none``    original source data; nothing is prepared.
"""

from __future__ import annotations

import tomllib
from collections.abc import Mapping
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from lidarbench.datasets.manifest import (
    KINDS,
    MD5_PATTERN,
    SHA256_PATTERN,
    VERSION_PATTERN,
    DeclaredMetadata,
    DerivedFrom,
    Layout,
    License,
    ManifestError,
    _choice,
    _hex,
    _int,
    _json_object,
    _list,
    _obj,
    _opt_str,
    _rel,
    _str,
    check_dataset_id,
)
from lidarbench.datasets.roots import DataRootError, check_label

RECIPE_SCHEMA = "lidarbench.dataset-recipe/1"
ENGINES = ("python", "r-cli", "none")
REDISTRIBUTABLE = ("permitted", "permitted-with-attribution")


def _strings(value: Any, where: str) -> tuple[str, ...]:
    items = _list(value, where)
    return tuple(_str(v, f"{where}[{i}]") for i, v in enumerate(items))


@dataclass(frozen=True)
class DatasetSpec:
    """One dataset (directory) a recipe yields."""

    id: str
    path: str
    title: str
    parameters: Mapping[str, Any] = field(default_factory=dict)

    @classmethod
    def parse(cls, value: Any, where: str) -> DatasetSpec:
        """Strictly parse a TOML table."""
        d = _obj(value, where, ("id", "path", "title"), ("parameters",))
        return cls(
            id=check_dataset_id(d["id"], f"{where}.id"),
            path=_rel(d["path"], f"{where}.path", allow_empty=True),
            title=_str(d["title"], f"{where}.title"),
            parameters=_json_object(d.get("parameters", {}), f"{where}.parameters"),
        )


@dataclass(frozen=True)
class RecordSpec:
    """A provenance receipt below the recipe's root (outside the dataset directories)."""

    path: str
    description: str
    md5: bool = True

    @classmethod
    def parse(cls, value: Any, where: str) -> RecordSpec:
        """Strictly parse a TOML table."""
        d = _obj(value, where, ("path", "description"), ("md5",))
        md5 = d.get("md5", True)
        if not isinstance(md5, bool):
            raise ManifestError(f"{where}.md5 must be true or false")
        return cls(
            _rel(d["path"], f"{where}.path"), _str(d["description"], f"{where}.description"), md5
        )


@dataclass(frozen=True)
class Preparation:
    """How the datasets are produced."""

    engine: str
    method: str
    generator: str | None = None
    entry_point: str | None = None
    config_example: str | None = None
    stages: tuple[str, ...] = ()
    requires: tuple[str, ...] = ()
    documentation: str | None = None
    parameters: Mapping[str, Any] = field(default_factory=dict)

    @classmethod
    def parse(cls, value: Any, where: str) -> Preparation:
        """Strictly parse a TOML table."""
        d = _obj(
            value,
            where,
            ("engine", "method"),
            (
                "generator",
                "entry_point",
                "config_example",
                "stages",
                "requires",
                "documentation",
                "parameters",
            ),
        )
        prep = cls(
            engine=_choice(d["engine"], f"{where}.engine", ENGINES),
            method=_str(d["method"], f"{where}.method"),
            generator=_opt_str(d.get("generator"), f"{where}.generator"),
            entry_point=_opt_str(d.get("entry_point"), f"{where}.entry_point"),
            config_example=_opt_str(d.get("config_example"), f"{where}.config_example"),
            stages=_strings(d.get("stages", []), f"{where}.stages"),
            requires=_strings(d.get("requires", []), f"{where}.requires"),
            documentation=_opt_str(d.get("documentation"), f"{where}.documentation"),
            parameters=_json_object(d.get("parameters", {}), f"{where}.parameters"),
        )
        if prep.engine == "python" and not prep.generator:
            raise ManifestError(f"{where}.generator is required for engine 'python'")
        if prep.engine == "r-cli" and (not prep.entry_point or not prep.stages):
            raise ManifestError(f"{where}: engine 'r-cli' needs entry_point and stages")
        return prep

    def to_record(self) -> dict[str, Any]:
        """JSON-ready representation (copied into manifests)."""
        return {
            "engine": self.engine,
            "method": self.method,
            "generator": self.generator,
            "entry_point": self.entry_point,
            "config_example": self.config_example,
            "stages": list(self.stages),
            "requires": list(self.requires),
            "documentation": self.documentation,
            "parameters": dict(self.parameters),
        }


@dataclass(frozen=True)
class Download:
    """Public download of an original file (allowed only with established terms)."""

    url: str
    file: str
    bytes: int
    md5: str | None
    sha256: str | None
    checksum_origin: str
    terms_url: str

    @classmethod
    def parse(cls, value: Any, where: str) -> Download:
        """Strictly parse a TOML table."""
        d = _obj(
            value,
            where,
            ("url", "file", "bytes", "checksum_origin", "terms_url"),
            ("md5", "sha256"),
        )
        url = _str(d["url"], f"{where}.url")
        if not url.startswith("https://"):
            raise ManifestError(f"{where}.url must use https")
        md5 = None if d.get("md5") is None else _hex(d["md5"], f"{where}.md5", MD5_PATTERN)
        sha = (
            None
            if d.get("sha256") is None
            else _hex(d["sha256"], f"{where}.sha256", SHA256_PATTERN)
        )
        if md5 is None and sha is None:
            raise ManifestError(f"{where}: an expected md5 or sha256 is required")
        name = _rel(d["file"], f"{where}.file")
        if "/" in name:
            raise ManifestError(f"{where}.file must be a file name")
        return cls(
            url=url,
            file=name,
            bytes=_int(d["bytes"], f"{where}.bytes", 1),
            md5=md5,
            sha256=sha,
            checksum_origin=_str(d["checksum_origin"], f"{where}.checksum_origin"),
            terms_url=_str(d["terms_url"], f"{where}.terms_url"),
        )


@dataclass(frozen=True)
class Recipe:
    """A validated dataset recipe."""

    id: str
    version: str
    title: str
    kind: str
    root: str
    description: str
    layout: Layout
    license: License
    source_citation: str
    source_url: str | None
    declared_metadata: DeclaredMetadata
    preparation: Preparation
    datasets: tuple[DatasetSpec, ...]
    derived_from: tuple[DerivedFrom, ...] = ()
    records: tuple[RecordSpec, ...] = ()
    legacy_receipts: str | None = None
    notes: tuple[str, ...] = ()
    download: Download | None = None
    path: Path | None = None

    def dataset(self, dataset_id: str) -> DatasetSpec:
        """Dataset spec by ID."""
        for spec in self.datasets:
            if spec.id == dataset_id:
                return spec
        raise KeyError(dataset_id)

    def summary(self) -> dict[str, Any]:
        """JSON-ready overview."""
        return {
            "id": self.id,
            "version": self.version,
            "title": self.title,
            "kind": self.kind,
            "root": self.root,
            "engine": self.preparation.engine,
            "method": self.preparation.method,
            "datasets": [d.id for d in self.datasets],
            "license": self.license.name,
            "redistribution": self.license.redistribution,
            "download": None if self.download is None else self.download.url,
        }


def parse_recipe(data: Mapping[str, Any], path: Path | None = None) -> Recipe:
    """Validate a parsed recipe document.

    Raises:
        ManifestError: With a message naming the offending key.
    """
    d = _obj(
        data,
        "recipe",
        (
            "schema",
            "id",
            "version",
            "title",
            "kind",
            "root",
            "license",
            "source",
            "preparation",
            "datasets",
        ),
        (
            "description",
            "layout",
            "declared_metadata",
            "derived_from",
            "provenance",
            "download",
        ),
    )
    if d["schema"] != RECIPE_SCHEMA:
        raise ManifestError(f"schema must be {RECIPE_SCHEMA!r}, got {d['schema']!r}")
    recipe_id = d["id"]
    if not isinstance(recipe_id, str) or "/" in recipe_id:
        raise ManifestError("id must be a dataset-id component without '/'")
    check_dataset_id(recipe_id, "id")
    version = d["version"]
    if not isinstance(version, str) or not VERSION_PATTERN.match(version):
        raise ManifestError(f"version must match {VERSION_PATTERN.pattern}, got {version!r}")
    try:
        root = check_label(d["root"], "root")
    except DataRootError as exc:
        raise ManifestError(str(exc)) from None
    source = _obj(d["source"], "source", ("citation",), ("url",))
    prov = _obj(d.get("provenance", {}), "provenance", (), ("records", "legacy_receipts", "notes"))
    kind = _choice(d["kind"], "kind", KINDS)
    license_ = License.parse(d["license"], "license")
    datasets = tuple(
        DatasetSpec.parse(v, f"datasets[{i}]")
        for i, v in enumerate(_list(d["datasets"], "datasets"))
    )
    if not datasets:
        raise ManifestError("datasets must list at least one dataset")
    ids = [s.id for s in datasets]
    if len(set(ids)) != len(ids):
        raise ManifestError("datasets: duplicate id")
    for spec in datasets:
        if not spec.id.startswith(recipe_id + "/"):
            raise ManifestError(f"datasets id {spec.id!r} must start with {recipe_id + '/'!r}")
    preparation = Preparation.parse(d["preparation"], "preparation")
    if (preparation.engine == "none") != (kind == "source"):
        raise ManifestError("preparation.engine 'none' is used exactly for kind 'source'")
    download = None if d.get("download") is None else Download.parse(d["download"], "download")
    if download is not None and (
        kind != "source" or license_.redistribution not in REDISTRIBUTABLE
    ):
        raise ManifestError(
            "download is allowed only for original source data whose license permits "
            f"redistribution ({list(REDISTRIBUTABLE)})"
        )
    legacy = prov.get("legacy_receipts")
    return Recipe(
        id=recipe_id,
        version=version,
        title=_str(d["title"], "title"),
        kind=kind,
        root=root,
        description=_str(d.get("description", ""), "description", empty=True),
        layout=Layout.parse(d.get("layout", {}), "layout"),
        license=license_,
        source_citation=_str(source["citation"], "source.citation"),
        source_url=_opt_str(source.get("url"), "source.url"),
        declared_metadata=DeclaredMetadata.parse(
            d.get("declared_metadata", {}), "declared_metadata"
        ),
        preparation=preparation,
        datasets=datasets,
        derived_from=tuple(
            DerivedFrom.parse(v, f"derived_from[{i}]")
            for i, v in enumerate(_list(d.get("derived_from", []), "derived_from"))
        ),
        records=tuple(
            RecordSpec.parse(v, f"provenance.records[{i}]")
            for i, v in enumerate(_list(prov.get("records", []), "provenance.records"))
        ),
        legacy_receipts=None if legacy is None else _str(legacy, "provenance.legacy_receipts"),
        notes=_strings(prov.get("notes", []), "provenance.notes"),
        download=download,
        path=path,
    )


def load_recipe(path: Path) -> Recipe:
    """Read and validate a recipe file.

    Raises:
        ManifestError: The file cannot be read or is invalid (message names the file).
    """
    try:
        data = tomllib.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, tomllib.TOMLDecodeError) as exc:
        raise ManifestError(f"{path}: cannot read recipe: {exc}") from exc
    try:
        recipe = parse_recipe(data, path)
    except ManifestError as exc:
        raise ManifestError(f"{path}: {exc}") from None
    if path.stem != recipe.id:
        raise ManifestError(f"{path}: file name must be {recipe.id}.toml")
    return recipe
