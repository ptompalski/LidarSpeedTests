"""Workload contracts (``lidarbench.workload/1``, TOML).

A contract is the tool-neutral definition of *what* a workload computes, the
declared per-tool implementations (status, equivalence, artifact naming, cell
ownership convention) and the numerical tolerances used to validate outputs.
Contracts are data, never executed. Loading follows the campaign-configuration
rules: unknown keys are errors, values are never coerced, every error names the
offending key, and the parsed record has a stable SHA-256 digest.

Equivalence is *declared* here, never inferred from results
(``matched_definition`` or ``non_comparable``). A declared match only says the
definitions are meant to agree; per-attempt validation decides whether a given
output does.
"""

from __future__ import annotations

import fnmatch
import math
import os
import re
import tomllib
from collections.abc import Iterable, Mapping
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from lidarbench.core.canonical import digest
from lidarbench.execution.adapters.base import WORKLOAD_ID_PATTERN, ImplementationStatus

CONTRACT_SCHEMA = "lidarbench.workload/1"
DEFAULT_SUITE = "workloads-v7"

SUITE_PATTERN = re.compile(r"^[A-Za-z0-9][A-Za-z0-9_.-]{0,63}$")
TOOL_PATTERN = re.compile(r"^[A-Za-z][A-Za-z0-9_.-]{0,63}$")

CELL_OWNERSHIP = {
    "right_down": (
        "Global partition of the zero-origin grid: column i owns i*r <= X < (i+1)*r and "
        "row j owns j*r < Y <= (j+1)*r (rightward X, downward Y on every edge, including "
        "the tile's outer edges). Each point belongs to exactly one cell."
    ),
    "lidr_buffered_clamp": (
        "right_down on interior edges; a point exactly on the core raster's maximum X "
        "(minimum Y) edge is clamped into the last column (bottom row) only when the "
        "buffered layout does not extend past that edge (pinned lidR 4.3.3)."
    ),
    "pdal_binmode_floor": (
        "PDAL writers.gdal binmode (>= 2.5.0) on a grid whose lower-left corner is the "
        "tile's snapped (xmin, ymin): column floor((X - xmin) / r) and row "
        "floor((Y - ymin) / r) counted upwards, evaluated in IEEE-754 binary64 exactly as "
        "PDAL does. On exact edges: rightward X and upward Y, so a point on a horizontal "
        "edge belongs to the cell north of it and a point on the grid's north edge is "
        "outside the tile's raster."
    ),
    "cell_centre_sample": (
        "No point binning: each cell's value is a surface (for example a linear TIN) "
        "sampled at the cell centre, so point ownership does not apply; only the grid "
        "geometry is shared."
    ),
    "undeclared": (
        "The tool's cell ownership has not been established; numerical agreement can be "
        "measured diagnostically but never certified."
    ),
}
"""Named cell-ownership conventions a contract or implementation may declare."""

PRODUCTS = ("raster", "point_cloud")
"""Output product kinds; a contract declares exactly one table of these names."""
POINT_FORMATS = ("laz",)
POINT_Z = {
    "height_above_ground": (
        "Output Z = input Z minus the ground surface (point_cloud.surface built from the "
        "point_cloud.ground_classes points of the tile and its buffer) at the point's XY."
    ),
}
SURFACES = ("linear_tin",)
POINT_OWNERSHIP = {
    "closed_header_bounds": (
        "An artifact holds unchanged input points lying inside its tile's closed header "
        "bounds: the tile's own points, plus points of neighbouring tiles lying exactly on "
        "a shared edge (tools differ on which tile owns those). No buffer point is written."
    ),
}
POINT_ATTRIBUTES = (
    "x",
    "y",
    "gps_time",
    "intensity",
    "return_number",
    "number_of_returns",
    "classification",
)
"""Point attributes a point-cloud contract may require to be unchanged (``x``/``y`` required)."""
OUTSIDE_SURFACE = {
    "discard": "Points the ground surface does not cover are not written.",
    "extrapolate": "Points the ground surface does not cover get an extrapolated ground.",
    "undeclared": "The tool's handling of points outside the ground surface is not established.",
}
"""Declared per-implementation handling of points outside the ground surface."""

POPULATION_INPUTS = ("original",)
POPULATION_POINTS = ("all", "classes")
"""``all``: no filter; ``classes``: only points whose ASPRS class is in ``population.classes``."""
POPULATION_Z = ("original",)
UNITS = ("metre",)
TILINGS = ("per_input_file",)
EXTENT_RULES = ("header_bounds_snapped_outward",)
IMPLEMENTATION_EXTENT_RULES = ("header_bounds_snapped_nearest",)
"""Declared tool extent rules that differ from the contract's (validated only under the
``accept_known_extent_discrepancies`` policy; design/VALIDATION.md)."""
EMPTY_CELLS = ("nodata",)
NODATA_SOURCES = ("file_metadata",)
REFERENCE_METHODS = ("cell_statistic", "none")
"""``none``: the contract defines no independent numerical reference; validation stops at
the structural level and the numerical level is ``unsupported`` (descriptive timings)."""
STATISTICS = ("mean", "max")
EQUIVALENCE = ("matched_definition", "non_comparable")
BAND_NAMES = ("required", "not_required")
COMPARABILITY = (
    "strictly_comparable",
    "functionally_comparable",
    "non_comparable",
    "unsupported",
)
"""Declared classifications (:meth:`WorkloadContract.declared_comparability`)."""
ARTIFACT_PLACEHOLDER = "{stem}"


class ContractError(ValueError):
    """A workload contract is invalid or cannot be found."""


def _table(value: Any, where: str, allowed: set[str], required: Iterable[str] = ()) -> dict:
    if not isinstance(value, dict):
        raise ContractError(f"{where} must be a table")
    unknown = sorted(set(value) - allowed)
    if unknown:
        raise ContractError(f"{where}: unknown key(s) {unknown}; allowed: {sorted(allowed)}")
    missing = sorted(set(required) - set(value))
    if missing:
        raise ContractError(f"{where}: missing required key(s) {missing}")
    return value


def _str(value: Any, where: str, pattern: re.Pattern[str] | None = None) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ContractError(f"{where} must be a non-empty string, got {value!r}")
    if pattern is not None and not pattern.match(value):
        raise ContractError(f"{where} must match {pattern.pattern}, got {value!r}")
    return value


def _choice(value: Any, where: str, choices: Iterable[str]) -> str:
    options = tuple(choices)
    if not isinstance(value, str) or value not in options:
        raise ContractError(f"{where} must be one of {list(options)}, got {value!r}")
    return value


def _float(value: Any, where: str, *, minimum: float | None = None, positive: bool = False):
    # TOML distinguishes 20 from 20.0; contracts require explicit floats (no coercion).
    if not isinstance(value, float) or not math.isfinite(value):
        raise ContractError(f"{where} must be a finite float (write 20.0, not 20), got {value!r}")
    if positive and value <= 0:
        raise ContractError(f"{where} must be > 0, got {value}")
    if minimum is not None and value < minimum:
        raise ContractError(f"{where} must be >= {minimum}, got {value}")
    return value


def _bool(value: Any, where: str) -> bool:
    if not isinstance(value, bool):
        raise ContractError(f"{where} must be true or false, got {value!r}")
    return value


def _list(value: Any, where: str) -> list[Any]:
    if not isinstance(value, list):
        raise ContractError(f"{where} must be an array")
    return value


@dataclass(frozen=True)
class Population:
    """Which points contribute and with which Z."""

    input: str
    points: str
    z: str
    require_finite_xyz: bool
    horizontal_units: str
    classes: tuple[int, ...] = ()
    """ASPRS classes kept when ``points == "classes"`` (sorted, distinct); else empty."""


@dataclass(frozen=True)
class RasterProduct:
    """Required raster geometry and encoding semantics."""

    resolution: float
    origin: tuple[float, float]
    extent: str
    cell_ownership: str
    empty_cells: str
    bands: tuple[str, ...]
    nodata_source: str
    forbidden_values: tuple[float, ...]

    @property
    def band_count(self) -> int:
        """Required number of bands (an empty name list means one unnamed band)."""
        return max(1, len(self.bands))


@dataclass(frozen=True)
class PointCloudProduct:
    """Required point-cloud product semantics (one artifact per input tile)."""

    format: str
    z: str
    ground_classes: tuple[int, ...]
    surface: str
    ownership: str
    preserved_attributes: tuple[str, ...]
    max_missing_fraction: float
    """Largest fraction of a tile's own points that may be absent from its artifact
    (points outside the ground surface, or edge points written to a neighbour)."""


@dataclass(frozen=True)
class Tolerance:
    """Per-value numerical acceptance: ``|a - r| <= absolute + relative * |r|``."""

    absolute: float
    relative: float

    def limit(self, reference: float) -> float:
        """Allowed absolute error for one reference value."""
        return self.absolute + self.relative * abs(reference)


@dataclass(frozen=True)
class Implementation:
    """One tool's declared implementation of a contract."""

    tool: str
    status: ImplementationStatus
    equivalence: str
    artifacts: tuple[str, ...]
    cell_ownership: str
    band_names: str
    note: str = ""
    reason: str = ""
    value_precision: float | None = None
    """Declared output quantum of the tool's values (for example 0.01 m when the tool
    rounds to centimetres); the numerical tolerance is widened by half of it."""
    extent: str | None = None
    """Declared extent rule of the tool's rasters when it differs from the contract's
    (``header_bounds_snapped_nearest``: each header bound rounded to the nearest grid
    line, so up to one edge cell holding points can be cut off); ``None`` = contract."""
    outside_surface: str | None = None
    """Point-cloud products only: declared handling of points outside the ground
    surface (:data:`OUTSIDE_SURFACE`); ``None`` for rasters."""

    def expected_names(self, stem: str) -> tuple[str, ...]:
        """Primary artifact file names expected for one input tile stem."""
        return tuple(p.replace(ARTIFACT_PLACEHOLDER, stem) for p in self.artifacts)

    @property
    def ownership_declared(self) -> bool:
        """Whether the tool's cell ownership convention is established."""
        return self.cell_ownership != "undeclared"


@dataclass(frozen=True)
class WorkloadContract:
    """A parsed, validated workload contract."""

    suite: str
    id: str
    title: str
    description: str
    population: Population
    tiling: str
    buffer: float
    raster: RasterProduct | None
    """The raster product, or ``None`` for a point-cloud contract."""
    reference_method: str
    statistic: str
    tolerance: Tolerance
    ignored_sidecars: tuple[str, ...]
    equivalence_group: str
    implementations: Mapping[str, Implementation]
    record: Mapping[str, Any] = field(repr=False, compare=False, default_factory=dict)
    source: Path | None = field(default=None, compare=False)
    point_cloud: PointCloudProduct | None = None
    """The point-cloud product, or ``None`` for a raster contract."""

    @property
    def product(self) -> str:
        """``raster`` or ``point_cloud`` (one of :data:`PRODUCTS`)."""
        return "point_cloud" if self.point_cloud is not None else "raster"

    def require_raster(self) -> RasterProduct:
        """The raster product; raises :class:`ContractError` for a point-cloud contract."""
        if self.raster is None:
            raise ContractError(f"contract {self.key} declares no raster product")
        return self.raster

    @property
    def key(self) -> str:
        """``suite/id``."""
        return f"{self.suite}/{self.id}"

    @property
    def digest(self) -> str:
        """SHA-256 of the canonical parsed contract (independent of TOML layout)."""
        return digest(dict(self.record))

    @property
    def has_reference(self) -> bool:
        """Whether the contract defines an independent numerical reference."""
        return self.reference_method != "none"

    def implementation(self, tool: str) -> Implementation | None:
        """Declared implementation for ``tool`` (exact name), or ``None``."""
        return self.implementations.get(tool)

    def declared_comparability(self, tool: str) -> str:
        """Declared classification of ``tool``'s implementation (one of :data:`COMPARABILITY`).

        Derived from the declaration only, never from results:

        * ``strictly_comparable``: runnable, ``matched_definition`` and the
          contract's own cell partition;
        * ``functionally_comparable``: ``matched_definition`` with a different,
          declared cell convention (same statistic and grid; edge points may be
          owned differently, so per-attempt ``boundary_difference_points`` decides);
        * ``non_comparable``: declared ``non_comparable`` (including
          ``diagnostic_only``);
        * ``unsupported``: no implementation, or status ``unsupported``/``blocked``.
        """
        impl = self.implementations.get(tool)
        if impl is None or not impl.status.dispatchable:
            return "unsupported"
        if (
            impl.equivalence != "matched_definition"
            or not impl.ownership_declared
            or self.raster is None
        ):
            return "non_comparable"
        if impl.cell_ownership == self.raster.cell_ownership:
            return "strictly_comparable"
        return "functionally_comparable"

    def is_ignored_sidecar(self, name: str) -> bool:
        """Whether ``name`` is an optional sidecar that is never a primary artifact."""
        return any(fnmatch.fnmatchcase(name, pattern) for pattern in self.ignored_sidecars)


def _implementation(tool: str, value: Any, where: str, product: str = "raster") -> Implementation:
    raster_keys = {"cell_ownership", "band_names", "value_precision", "extent"}
    point_keys = {"outside_surface"}
    table = _table(
        value,
        where,
        {
            "status",
            "equivalence",
            "artifacts",
            "note",
            "reason",
            *(raster_keys if product == "raster" else point_keys),
        },
        ("status", "equivalence"),
    )
    _str(tool, where, TOOL_PATTERN)
    status = ImplementationStatus(
        _choice(table["status"], f"{where}.status", [s.value for s in ImplementationStatus])
    )
    equivalence = _choice(table["equivalence"], f"{where}.equivalence", EQUIVALENCE)
    reason = _str(table["reason"], f"{where}.reason") if "reason" in table else ""
    if status is not ImplementationStatus.RUNNABLE and not reason:
        raise ContractError(f"{where}.reason is required for status {status.value!r}")
    if status is ImplementationStatus.DIAGNOSTIC_ONLY and equivalence != "non_comparable":
        raise ContractError(f"{where}: diagnostic_only implementations must be non_comparable")
    artifacts: list[str] = []
    if status.dispatchable:
        required = ("cell_ownership", "band_names") if product == "raster" else ("outside_surface",)
        for key in ("artifacts", *required):
            if key not in table:
                raise ContractError(f"{where}.{key} is required for a dispatchable status")
        for i, pattern in enumerate(_list(table["artifacts"], f"{where}.artifacts")):
            text = _str(pattern, f"{where}.artifacts[{i}]")
            if ARTIFACT_PLACEHOLDER not in text or "/" in text or "\\" in text:
                raise ContractError(
                    f"{where}.artifacts[{i}] must be a file name containing {{stem}}, got {text!r}"
                )
            if any(ch in text.replace(ARTIFACT_PLACEHOLDER, "") for ch in "*?[]{}"):
                raise ContractError(f"{where}.artifacts[{i}] must be an exact name pattern")
            artifacts.append(text)
        if not artifacts or len(set(artifacts)) != len(artifacts):
            raise ContractError(f"{where}.artifacts must list distinct names")
    ownership = _choice(
        table.get("cell_ownership", "undeclared"), f"{where}.cell_ownership", CELL_OWNERSHIP
    )
    band_names = _choice(table.get("band_names", "not_required"), f"{where}.band_names", BAND_NAMES)
    note = _str(table["note"], f"{where}.note") if "note" in table else ""
    precision = (
        _float(table["value_precision"], f"{where}.value_precision", positive=True)
        if "value_precision" in table
        else None
    )
    extent = (
        _choice(table["extent"], f"{where}.extent", IMPLEMENTATION_EXTENT_RULES)
        if "extent" in table
        else None
    )
    if extent is not None and ownership == "undeclared":
        raise ContractError(f"{where}.extent requires a declared cell_ownership")
    outside = (
        _choice(table["outside_surface"], f"{where}.outside_surface", OUTSIDE_SURFACE)
        if "outside_surface" in table
        else None
    )
    return Implementation(
        tool=tool,
        status=status,
        equivalence=equivalence,
        artifacts=tuple(artifacts),
        cell_ownership=ownership,
        band_names=band_names,
        note=note,
        reason=reason,
        value_precision=precision,
        extent=extent,
        outside_surface=outside,
    )


def _classes(value: Any, where: str) -> tuple[int, ...]:
    raw = _list(value, where)
    for i, item in enumerate(raw):
        if isinstance(item, bool) or not isinstance(item, int) or not 0 <= item <= 255:
            raise ContractError(f"{where}[{i}] must be an ASPRS class 0-255, got {item!r}")
    if not raw or len(set(raw)) != len(raw) or list(raw) != sorted(raw):
        raise ContractError(f"{where} must be a non-empty, sorted, distinct list")
    return tuple(raw)


def _raster(value: Any, buffer: float) -> RasterProduct:
    rt = _table(
        value,
        "raster",
        {
            "resolution",
            "origin",
            "extent",
            "cell_ownership",
            "empty_cells",
            "bands",
            "nodata_source",
            "forbidden_values",
        },
        (
            "resolution",
            "origin",
            "extent",
            "cell_ownership",
            "empty_cells",
            "bands",
            "nodata_source",
        ),
    )
    resolution = _float(rt["resolution"], "raster.resolution", positive=True)
    origin_list = _list(rt["origin"], "raster.origin")
    if len(origin_list) != 2:
        raise ContractError("raster.origin must have two values [x, y]")
    origin = (
        _float(origin_list[0], "raster.origin[0]"),
        _float(origin_list[1], "raster.origin[1]"),
    )
    if origin != (0.0, 0.0):
        raise ContractError("raster.origin must be [0.0, 0.0] in this schema version")
    ownership = _choice(rt["cell_ownership"], "raster.cell_ownership", CELL_OWNERSHIP)
    if ownership == "undeclared":
        raise ContractError("raster.cell_ownership must be a declared convention")
    bands = tuple(
        _str(b, f"raster.bands[{i}]") for i, b in enumerate(_list(rt["bands"], "raster.bands"))
    )
    if len(set(bands)) != len(bands):
        raise ContractError("raster.bands must be distinct")
    if buffer < resolution:
        raise ContractError(
            "spatial.buffer must be >= raster.resolution so snapped cells are filled"
        )
    return RasterProduct(
        resolution=resolution,
        origin=origin,
        extent=_choice(rt["extent"], "raster.extent", EXTENT_RULES),
        cell_ownership=ownership,
        empty_cells=_choice(rt["empty_cells"], "raster.empty_cells", EMPTY_CELLS),
        bands=bands,
        nodata_source=_choice(rt["nodata_source"], "raster.nodata_source", NODATA_SOURCES),
        forbidden_values=tuple(
            _float(v, f"raster.forbidden_values[{i}]")
            for i, v in enumerate(_list(rt.get("forbidden_values", []), "raster.forbidden_values"))
        ),
    )


def _point_cloud(value: Any, buffer: float, population: Population) -> PointCloudProduct:
    keys = {
        "format",
        "z",
        "ground_classes",
        "surface",
        "ownership",
        "preserved_attributes",
        "max_missing_fraction",
    }
    pc = _table(value, "point_cloud", keys, keys)
    if population.points != "all":
        raise ContractError('point_cloud products require population.points = "all"')
    if buffer <= 0:
        raise ContractError("spatial.buffer must be > 0 for a point_cloud ground surface")
    attributes = tuple(
        _choice(a, f"point_cloud.preserved_attributes[{i}]", POINT_ATTRIBUTES)
        for i, a in enumerate(_list(pc["preserved_attributes"], "point_cloud.preserved_attributes"))
    )
    if len(set(attributes)) != len(attributes) or not {"x", "y"} <= set(attributes):
        raise ContractError("point_cloud.preserved_attributes must be distinct and include x and y")
    missing = _float(pc["max_missing_fraction"], "point_cloud.max_missing_fraction", minimum=0.0)
    if missing >= 1.0:
        raise ContractError("point_cloud.max_missing_fraction must be < 1.0")
    return PointCloudProduct(
        format=_choice(pc["format"], "point_cloud.format", POINT_FORMATS),
        z=_choice(pc["z"], "point_cloud.z", POINT_Z),
        ground_classes=_classes(pc["ground_classes"], "point_cloud.ground_classes"),
        surface=_choice(pc["surface"], "point_cloud.surface", SURFACES),
        ownership=_choice(pc["ownership"], "point_cloud.ownership", POINT_OWNERSHIP),
        preserved_attributes=attributes,
        max_missing_fraction=missing,
    )


def parse_contract(data: Mapping[str, Any], source: Path | None = None) -> WorkloadContract:
    """Validate a decoded TOML document and build a :class:`WorkloadContract`."""
    top = _table(
        dict(data),
        "contract",
        {
            "schema",
            "suite",
            "id",
            "title",
            "description",
            "population",
            "spatial",
            "raster",
            "point_cloud",
            "reference",
            "tolerance",
            "artifacts",
            "equivalence",
            "implementations",
        },
        (
            "schema",
            "suite",
            "id",
            "title",
            "population",
            "spatial",
            "reference",
            "equivalence",
            "implementations",
        ),
    )
    if top["schema"] != CONTRACT_SCHEMA:
        raise ContractError(f"schema must be {CONTRACT_SCHEMA!r}, got {top['schema']!r}")
    suite = _str(top["suite"], "suite", SUITE_PATTERN)
    workload_id = _str(top["id"], "id", WORKLOAD_ID_PATTERN)
    title = _str(top["title"], "title")
    description = _str(top["description"], "description") if "description" in top else ""

    pop = _table(
        top["population"],
        "population",
        {"input", "points", "z", "require_finite_xyz", "horizontal_units", "classes"},
        ("input", "points", "z", "require_finite_xyz", "horizontal_units"),
    )
    points = _choice(pop["points"], "population.points", POPULATION_POINTS)
    classes: tuple[int, ...] = ()
    if points == "classes":
        if "classes" not in pop:
            raise ContractError('population.classes is required when points = "classes"')
        raw = _list(pop["classes"], "population.classes")
        for i, value in enumerate(raw):
            if isinstance(value, bool) or not isinstance(value, int) or not 0 <= value <= 255:
                raise ContractError(
                    f"population.classes[{i}] must be an ASPRS class 0-255, got {value!r}"
                )
        if not raw or len(set(raw)) != len(raw) or list(raw) != sorted(raw):
            raise ContractError("population.classes must be a non-empty, sorted, distinct list")
        classes = tuple(raw)
    elif "classes" in pop:
        raise ContractError('population.classes is only allowed when points = "classes"')
    population = Population(
        input=_choice(pop["input"], "population.input", POPULATION_INPUTS),
        points=points,
        z=_choice(pop["z"], "population.z", POPULATION_Z),
        require_finite_xyz=_bool(pop["require_finite_xyz"], "population.require_finite_xyz"),
        horizontal_units=_choice(pop["horizontal_units"], "population.horizontal_units", UNITS),
        classes=classes,
    )
    if not population.require_finite_xyz:
        raise ContractError("population.require_finite_xyz must be true in this schema version")

    spatial = _table(top["spatial"], "spatial", {"tiling", "buffer"}, ("tiling", "buffer"))
    tiling = _choice(spatial["tiling"], "spatial.tiling", TILINGS)
    buffer = _float(spatial["buffer"], "spatial.buffer", minimum=0.0)

    has_raster, has_points = "raster" in top, "point_cloud" in top
    if has_raster == has_points:
        raise ContractError(
            "contract must declare exactly one product table: [raster] or [point_cloud]"
        )
    raster = _raster(top["raster"], buffer) if has_raster else None
    point_cloud = _point_cloud(top["point_cloud"], buffer, population) if has_points else None
    product = "raster" if raster is not None else "point_cloud"

    ref = _table(top["reference"], "reference", {"method", "statistic"}, ("method",))
    method = _choice(ref["method"], "reference.method", REFERENCE_METHODS)
    if method == "cell_statistic":
        if "statistic" not in ref:
            raise ContractError("reference: missing required key(s) ['statistic']")
        statistic = _choice(ref["statistic"], "reference.statistic", STATISTICS)
        if raster is None:
            raise ContractError('point_cloud contracts require reference.method = "none"')
        if raster.band_count != 1:
            raise ContractError("cell_statistic references produce exactly one band")
        if population.points != "all":
            raise ContractError('cell_statistic references require population.points = "all"')
    else:
        if "statistic" in ref:
            raise ContractError('reference.statistic is not allowed when method = "none"')
        statistic = ""

    if method == "none":
        if "tolerance" in top:
            raise ContractError('tolerance is not allowed when reference.method = "none"')
        tolerance = Tolerance(absolute=0.0, relative=0.0)
    else:
        if "tolerance" not in top:
            raise ContractError("contract: missing required key(s) ['tolerance']")
        tol = _table(
            top["tolerance"], "tolerance", {"absolute", "relative"}, ("absolute", "relative")
        )
        tolerance = Tolerance(
            absolute=_float(tol["absolute"], "tolerance.absolute", minimum=0.0),
            relative=_float(tol["relative"], "tolerance.relative", minimum=0.0),
        )

    art = _table(top.get("artifacts", {}), "artifacts", {"ignored_sidecars"})
    sidecars = tuple(
        _str(p, f"artifacts.ignored_sidecars[{i}]")
        for i, p in enumerate(_list(art.get("ignored_sidecars", []), "artifacts.ignored_sidecars"))
    )

    eq = _table(top["equivalence"], "equivalence", {"group"}, ("group",))
    group = _str(eq["group"], "equivalence.group")

    impl_table = top["implementations"]
    if not isinstance(impl_table, dict) or not impl_table:
        raise ContractError("implementations must be a non-empty table of tools")
    implementations = {
        tool: _implementation(tool, value, f"implementations.{tool}", product)
        for tool, value in sorted(impl_table.items())
    }
    if point_cloud is not None:
        for tool, impl in implementations.items():
            if impl.equivalence != "non_comparable":
                raise ContractError(
                    f"implementations.{tool}.equivalence must be non_comparable: point_cloud "
                    "contracts define no numerical reference in this schema version"
                )
    lowered = [t.lower() for t in implementations]
    if len(set(lowered)) != len(lowered):
        raise ContractError("implementations: tool names must differ by more than case")

    record = {
        "schema": CONTRACT_SCHEMA,
        "suite": suite,
        "id": workload_id,
        "title": title,
        **({"description": description} if description else {}),
        "population": dict(pop),
        "spatial": {"tiling": tiling, "buffer": buffer},
        **(
            {
                "raster": {
                    "resolution": raster.resolution,
                    "origin": list(raster.origin),
                    "extent": raster.extent,
                    "cell_ownership": raster.cell_ownership,
                    "empty_cells": raster.empty_cells,
                    "bands": list(raster.bands),
                    "nodata_source": raster.nodata_source,
                    "forbidden_values": list(raster.forbidden_values),
                }
            }
            if raster is not None
            else {}
        ),
        **(
            {
                "point_cloud": {
                    "format": point_cloud.format,
                    "z": point_cloud.z,
                    "ground_classes": list(point_cloud.ground_classes),
                    "surface": point_cloud.surface,
                    "ownership": point_cloud.ownership,
                    "preserved_attributes": list(point_cloud.preserved_attributes),
                    "max_missing_fraction": point_cloud.max_missing_fraction,
                }
            }
            if point_cloud is not None
            else {}
        ),
        "reference": {"method": method, **({"statistic": statistic} if statistic else {})},
        **(
            {"tolerance": {"absolute": tolerance.absolute, "relative": tolerance.relative}}
            if method != "none"
            else {}
        ),
        "artifacts": {"ignored_sidecars": list(sidecars)},
        "equivalence": {"group": group},
        "implementations": {
            tool: {
                "status": impl.status.value,
                "equivalence": impl.equivalence,
                "artifacts": list(impl.artifacts),
                **(
                    {"cell_ownership": impl.cell_ownership, "band_names": impl.band_names}
                    if product == "raster"
                    else {}
                ),
                **(
                    {"outside_surface": impl.outside_surface}
                    if impl.outside_surface is not None
                    else {}
                ),
                **({"note": impl.note} if impl.note else {}),
                **({"reason": impl.reason} if impl.reason else {}),
                **(
                    {"value_precision": impl.value_precision}
                    if impl.value_precision is not None
                    else {}
                ),
                **({"extent": impl.extent} if impl.extent is not None else {}),
            }
            for tool, impl in implementations.items()
        },
    }
    return WorkloadContract(
        suite=suite,
        id=workload_id,
        title=title,
        description=description.strip(),
        population=population,
        tiling=tiling,
        buffer=buffer,
        raster=raster,
        reference_method=method,
        statistic=statistic,
        tolerance=tolerance,
        ignored_sidecars=sidecars,
        equivalence_group=group,
        implementations=implementations,
        record=record,
        source=source,
        point_cloud=point_cloud,
    )


def load_contract(path: str | os.PathLike[str]) -> WorkloadContract:
    """Load and validate one contract file."""
    file = Path(path)
    try:
        with open(file, "rb") as handle:
            data = tomllib.load(handle)
    except OSError as exc:
        raise ContractError(f"cannot read contract {file}: {exc}") from exc
    except tomllib.TOMLDecodeError as exc:
        raise ContractError(f"{file}: invalid TOML: {exc}") from exc
    try:
        contract = parse_contract(data, source=file.resolve())
    except ContractError as exc:
        raise ContractError(f"{file}: {exc}") from exc
    if contract.id != file.stem:
        raise ContractError(f"{file}: file name must equal the contract id {contract.id!r}")
    return contract


@dataclass(frozen=True)
class WorkloadSuite:
    """All contracts of one suite directory, keyed by workload id."""

    name: str
    path: Path
    contracts: Mapping[str, WorkloadContract]

    def get(self, workload_id: str) -> WorkloadContract:
        """Contract for ``workload_id``; raises :class:`ContractError` if undefined."""
        try:
            return self.contracts[workload_id]
        except KeyError:
            raise ContractError(
                f"suite {self.name!r} defines no contract for {workload_id!r}; "
                f"defined: {sorted(self.contracts)}"
            ) from None


def load_suite(path: str | os.PathLike[str]) -> WorkloadSuite:
    """Load every ``*.toml`` contract in a suite directory."""
    root = Path(path)
    if not root.is_dir():
        raise ContractError(f"workload suite directory not found: {root}")
    contracts: dict[str, WorkloadContract] = {}
    for file in sorted(root.glob("*.toml")):
        contract = load_contract(file)
        if contract.suite != root.name:
            raise ContractError(f"{file}: suite {contract.suite!r} != directory {root.name!r}")
        contracts[contract.id] = contract
    if not contracts:
        raise ContractError(f"workload suite {root} contains no contracts")
    return WorkloadSuite(root.name, root.resolve(), contracts)


def default_suite_dir(suite: str = DEFAULT_SUITE) -> Path:
    """``workloads/<suite>`` of the source checkout containing this package.

    Contracts live beside the package in the repository (``workloads/``), not in
    the installed wheel; pass an explicit directory when running elsewhere.
    """
    package = Path(__file__).resolve().parents[1]
    return package.parents[1] / "workloads" / suite
