"""Structural (geospatial) checks of raster products (requires numpy).

Checks, per artifact: GeoTIFF driver, band count and (when the implementation
declares them) band names/order, numeric data type, no mask other than the
NoData value, axis-aligned north-up geometry, exact resolution, zero-origin
alignment, exact expected extent, CRS identity and metre units, and no
infinite values. Nothing is cropped, resampled, repaired or filled.

Only the frozen opt-in ``accept_known_extent_discrepancies`` relaxes the extent
rule, in two ways:

* outward extension by at most one cell per edge, with the full expected core
  present. The product is compared through an in-memory index window onto the
  expected core.
* an implementation that declares ``extent = "header_bounds_snapped_nearest"``
  (pinned lidR crops to the header bounds with ``terra::crop(snap = "near")``):
  an edge may be one cell inward where the header bound lies at least half a
  cell inside the outward grid line. lidR lays the raster out on the bounds of
  the points it rasterizes (the contract population of the tile and its
  buffer) snapped outward before that crop, so an edge may also be exactly
  the inward-most of that layout edge and the header-snapped edge; this only
  differs from the header rule where the population stops short of the tile
  edge (for example sparse ground points at the outer edge of the dataset).
  Only the covered part of the reference is compared; the cut cells and their
  points are counted by the numerical level.

The extension or cut is recorded and the best possible numerical outcome
becomes ``accepted_exception``.
"""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass, field
from typing import Any

import numpy as np

from lidarbench.validation.grid import (
    GridSpec,
    _ceil_div,
    _floor_div,
    expected_grid,
    grid_from_transform,
)
from lidarbench.validation.io import RasterData
from lidarbench.validation.policies import Policies
from lidarbench.workloads.contract import Implementation, WorkloadContract

GEOMETRY_TOLERANCE = 1e-8
"""Absolute tolerance (metres) for cell size and edge positions, as in validation-v1."""

ALLOWED_MASK_FLAGS = ({"all_valid"}, {"nodata"})
METRE_UNITS = {"metre", "meter", "metres", "meters", "m"}


@dataclass
class RasterStructure:
    """Structural verdict for one raster plus what numerical validation needs."""

    expected_grid: GridSpec
    actual_grid: GridSpec | None = None
    errors: list[str] = field(default_factory=list)
    numerical_caps: list[str] = field(default_factory=list)
    """Reasons the numerical claim cannot be certified even if values agree."""
    accepted_exceptions: list[str] = field(default_factory=list)
    window: tuple[int, int] | None = None
    """``(row_offset, col_offset)`` of the expected core inside the product."""
    inset: tuple[int, int] | None = None
    """``(row_offset, col_offset)`` of the product inside the expected grid, when a
    declared inward extent rule was accepted (the product is the smaller one)."""
    trimmed: tuple[int, int, int, int] | None = None
    """Edge cells W/E/S/N of the expected grid that the product does not cover."""
    extent_delta: tuple[float, float, float, float] | None = None
    finite_cells: int = 0

    @property
    def passed(self) -> bool:
        """No structural error."""
        return not self.errors

    def core(self, band: np.ndarray) -> np.ndarray:
        """The expected-core view of one decoded band (no copy, no resampling)."""
        if self.window is None:
            raise ValueError("no aligned core window")
        r0, c0 = self.window
        rows, cols = self.expected_grid.shape
        return band[r0 : r0 + rows, c0 : c0 + cols]

    def covered(self, reference: np.ndarray) -> np.ndarray:
        """The view of an expected-grid array that the (smaller) product covers."""
        if self.inset is None or self.actual_grid is None:
            raise ValueError("no inset window")
        r0, c0 = self.inset
        rows, cols = self.actual_grid.shape
        return reference[r0 : r0 + rows, c0 : c0 + cols]

    def to_record(self) -> dict[str, Any]:
        """JSON-serializable form."""
        return {
            "passed": self.passed,
            "errors": list(self.errors),
            "expected_grid": self.expected_grid.to_record(),
            "actual_grid": self.actual_grid.to_record() if self.actual_grid else None,
            "extent_delta": list(self.extent_delta) if self.extent_delta else None,
            "accepted_exceptions": list(self.accepted_exceptions),
            "trimmed_cells_wesn": list(self.trimmed) if self.trimmed else None,
            "numerical_caps": list(self.numerical_caps),
            "finite_cells": self.finite_cells,
        }


def _check_crs(raster: RasterData, expected_epsg: int | None, out: RasterStructure) -> None:
    crs = raster.crs
    if expected_epsg is not None:
        if crs.wkt is None:
            out.errors.append(f"CRS missing; expected EPSG:{expected_epsg}")
        elif crs.epsg != expected_epsg:
            out.errors.append(f"CRS EPSG:{crs.epsg} != expected EPSG:{expected_epsg}")
    elif crs.wkt is None:
        out.numerical_caps.append(
            "horizontal CRS is unknown in both the declaration and the product; metre "
            "units of the grid are not established"
        )
    else:
        out.numerical_caps.append(
            f"product CRS (EPSG:{crs.epsg}) could not be checked: no CRS was declared "
            "for the inputs"
        )
    if crs.wkt is not None:
        if crs.is_projected is False:
            out.errors.append("CRS is geographic; the contract grid requires metre units")
        elif (crs.linear_units or "").lower() not in METRE_UNITS:
            out.errors.append(f"CRS linear units are {crs.linear_units!r}, not metres")


def check_raster(
    raster: RasterData,
    contract: WorkloadContract,
    implementation: Implementation,
    tile_bounds: tuple[float, float, float, float],
    expected_epsg: int | None,
    policies: Policies,
    bands: tuple[str, ...] | None = None,
    population_bounds: Callable[[], tuple[float, float, float, float] | None] | None = None,
) -> RasterStructure:
    """Run every structural check on one decoded raster.

    ``bands`` are the contract bands this artifact holds (default: all of them); a
    per-band artifact (``Implementation.artifact_bands``) holds exactly one.
    ``population_bounds`` returns the XY bounds of the tile's buffered contract
    population; it is called only when a declared ``header_bounds_snapped_nearest``
    raster is cut further inward than the header rule allows.
    """
    product = contract.require_raster()
    r = product.resolution
    out = RasterStructure(expected_grid=expected_grid(tile_bounds, r))
    expected_bands = product.bands if bands is None else bands
    band_count = max(1, len(expected_bands))
    if raster.driver != "GTiff":
        out.errors.append(f"driver is {raster.driver}, not GeoTIFF")
    if raster.count != band_count:
        held = "" if bands is None else f" for its band(s) {list(expected_bands)}"
        out.errors.append(f"{raster.count} band(s); the contract requires {band_count}{held}")
    elif expected_bands and implementation.band_names == "required":
        names = tuple(raster.descriptions)
        if names != expected_bands:
            out.errors.append(f"band names {list(names)} != required {list(expected_bands)}")
    for i, dtype in enumerate(raster.dtypes):
        if np.dtype(dtype).kind not in "fiu":
            out.errors.append(f"band {i + 1} has non-numeric type {dtype}")
    for i, flags in enumerate(raster.mask_flags):
        if set(flags) not in ALLOWED_MASK_FLAGS:
            out.errors.append(
                f"band {i + 1} mask flags {sorted(flags)}: only a NoData value (or no mask) "
                "is accepted"
            )
    infinite = int(np.isinf(raster.values).sum())
    if infinite:
        out.errors.append(f"{infinite} infinite value(s)")
    out.finite_cells = int(np.isfinite(raster.values).sum())
    _check_crs(raster, expected_epsg, out)

    actual = grid_from_transform(
        raster.transform, raster.width, raster.height, r, GEOMETRY_TOLERANCE
    )
    if actual is None:
        out.errors.append(
            f"geometry is rotated, not north-up, not {r} m or not aligned to the zero "
            f"origin (transform {list(raster.transform)})"
        )
        return out
    out.actual_grid = actual
    exp = out.expected_grid
    out.extent_delta = (
        actual.xmin - exp.xmin,
        actual.xmax - exp.xmax,
        actual.ymin - exp.ymin,
        actual.ymax - exp.ymax,
    )
    if actual == exp:
        out.window = (0, 0)
        return out
    grow = (
        exp.col0 - actual.col0,
        (actual.col0 + actual.ncols) - (exp.col0 + exp.ncols),
        exp.row0 - actual.row0,
        (actual.row0 + actual.nrows) - (exp.row0 + exp.nrows),
    )
    description = (
        f"extent {list(actual.extent())} != expected {list(exp.extent())} "
        f"(outward cells W/E/S/N = {list(grow)})"
    )
    outward_within_one = all(0 <= g <= 1 for g in grow)
    if policies.accept_known_extent_discrepancies and outward_within_one:
        out.window = (grow[3], grow[0])
        out.accepted_exceptions.append(
            "accepted outward raster extension of at most one cell per edge with the full "
            f"core present (accept_known_extent_discrepancies): {description}"
        )
        return out
    nearest = (
        policies.accept_known_extent_discrepancies
        and implementation.extent == "header_bounds_snapped_nearest"
    )
    if (
        nearest
        and all(g in (0, -1) for g in grow)
        and all(-g <= ok for g, ok in zip(grow, _nearest_inward(tile_bounds, exp), strict=True))
    ):
        _accept_inset(out, grow)
        out.accepted_exceptions.append(
            "accepted declared extent rule header_bounds_snapped_nearest: edges whose header "
            "bound lies at least half a cell inside the outward grid line are cut by one cell "
            f"(accept_known_extent_discrepancies): {description}"
        )
        return out
    if nearest and population_bounds is not None and all(g <= 0 for g in grow):
        bounds = population_bounds()
        if bounds is not None and _layout_edges_match(actual, exp, tile_bounds, bounds):
            _accept_inset(out, grow)
            out.accepted_exceptions.append(
                "accepted declared extent rule header_bounds_snapped_nearest with the raster "
                "laid out on the buffered population bounds "
                f"{[round(v, 3) for v in bounds]} snapped outward: edges are cut where the "
                "population stops inside the header-snapped edge "
                f"(accept_known_extent_discrepancies): {description}"
            )
            return out
    out.errors.append(description)
    return out


def _accept_inset(out: RasterStructure, grow: tuple[int, int, int, int]) -> None:
    out.inset = (-grow[3], -grow[0])
    out.trimmed = (-grow[0], -grow[1], -grow[2], -grow[3])


def _layout_edges_match(
    actual: GridSpec,
    exp: GridSpec,
    tile_bounds: tuple[float, float, float, float],
    population: tuple[float, float, float, float],
) -> bool:
    """Whether every edge is the expected edge, a header-nearest edge, or the inward-most
    of a header-nearest edge and the population bound snapped outward (grid-line indices)."""
    r = exp.resolution
    inward = _nearest_inward(tile_bounds, exp)
    p_min_x, p_min_y, p_max_x, p_max_y = population
    edges = (
        (actual.col0, exp.col0, 1, _floor_div(p_min_x, r), max),
        (actual.col0 + actual.ncols, exp.col0 + exp.ncols, -1, _ceil_div(p_max_x, r), min),
        (actual.row0, exp.row0, 1, _floor_div(p_min_y, r), max),
        (actual.row0 + actual.nrows, exp.row0 + exp.nrows, -1, _ceil_div(p_max_y, r), min),
    )
    for (edge, expected, step, layout, inward_most), cut in zip(edges, inward, strict=True):
        header = {expected, expected + step * cut}
        if edge not in header | {inward_most(h, layout) for h in header}:
            return False
    return True


def _nearest_inward(
    tile_bounds: tuple[float, float, float, float], exp: GridSpec
) -> tuple[int, int, int, int]:
    """Edges W/E/S/N where rounding the header bound to the nearest grid line moves inward.

    A bound exactly half a cell from both lines may round either way (both are allowed).
    """
    min_x, min_y, max_x, max_y = tile_bounds
    half = exp.resolution / 2
    return (
        int(min_x - exp.xmin >= half),
        int(exp.xmax - max_x >= half),
        int(min_y - exp.ymin >= half),
        int(exp.ymax - max_y >= half),
    )
