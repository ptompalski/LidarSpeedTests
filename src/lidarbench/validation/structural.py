"""Structural (geospatial) checks of raster products (requires numpy).

Checks, per artifact: GeoTIFF driver, band count and (when the implementation
declares them) band names/order, numeric data type, no mask other than the
NoData value, axis-aligned north-up geometry, exact resolution, zero-origin
alignment, exact expected extent, CRS identity and metre units, and no
infinite values. Nothing is cropped, resampled, repaired or filled.

Only the frozen opt-in ``accept_known_extent_discrepancies`` relaxes one rule:
outward extension by at most one cell per edge, with the full expected core
present. The product is then compared through an in-memory index window onto
the expected core, the extension is recorded and the best possible numerical
outcome becomes ``accepted_exception``.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any

import numpy as np

from lidarbench.validation.grid import GridSpec, expected_grid, grid_from_transform
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

    def to_record(self) -> dict[str, Any]:
        """JSON-serializable form."""
        return {
            "passed": self.passed,
            "errors": list(self.errors),
            "expected_grid": self.expected_grid.to_record(),
            "actual_grid": self.actual_grid.to_record() if self.actual_grid else None,
            "extent_delta": list(self.extent_delta) if self.extent_delta else None,
            "accepted_exceptions": list(self.accepted_exceptions),
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
) -> RasterStructure:
    """Run every structural check on one decoded raster."""
    product = contract.raster
    r = product.resolution
    out = RasterStructure(expected_grid=expected_grid(tile_bounds, r))
    if raster.driver != "GTiff":
        out.errors.append(f"driver is {raster.driver}, not GeoTIFF")
    if raster.count != product.band_count:
        out.errors.append(f"{raster.count} band(s); the contract requires {product.band_count}")
    elif product.bands and implementation.band_names == "required":
        names = tuple(raster.descriptions)
        if names != product.bands:
            out.errors.append(f"band names {list(names)} != required {list(product.bands)}")
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
    out.errors.append(description)
    return out
