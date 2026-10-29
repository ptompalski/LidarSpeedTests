"""Zero-origin raster grids and cell-ownership conventions (requires numpy).

Cell indices are *global* integers on the contract's zero-origin grid: column
``i`` spans ``[i*r, (i+1)*r)`` in X and row ``j`` (counted upwards from Y = 0)
spans ``(j*r, (j+1)*r]`` in Y under the ``right_down`` convention. Edges are
the float64 values ``i*r``; indices are computed with ``floor``/``ceil`` and then
corrected with exact comparisons against those edges, so a point lying exactly
on an edge is assigned by the convention, never by division rounding.

A tile's expected grid is its unbuffered header XY bounds snapped outward to
multiples of ``r``. Arrays are north-up: array row 0 is the top (largest Y).
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from fractions import Fraction

import numpy as np
from numpy.typing import NDArray

FloatArray = NDArray[np.float64]
IntArray = NDArray[np.int64]


def _floor_div(value: float, r: float) -> int:
    return math.floor(Fraction(value) / Fraction(r))


def _ceil_div(value: float, r: float) -> int:
    return math.ceil(Fraction(value) / Fraction(r))


@dataclass(frozen=True)
class GridSpec:
    """An axis-aligned, north-up grid of ``ncols`` x ``nrows`` cells of size ``r``.

    ``col0``/``row0`` are the global indices of the west column and the south row.
    """

    resolution: float
    col0: int
    row0: int
    ncols: int
    nrows: int

    def __post_init__(self) -> None:
        if not (self.resolution > 0 and math.isfinite(self.resolution)):
            raise ValueError("resolution must be positive and finite")
        if self.ncols < 1 or self.nrows < 1:
            raise ValueError("a grid needs at least one column and one row")

    @property
    def xmin(self) -> float:
        """West edge."""
        return self.col0 * self.resolution

    @property
    def xmax(self) -> float:
        """East edge."""
        return (self.col0 + self.ncols) * self.resolution

    @property
    def ymin(self) -> float:
        """South edge."""
        return self.row0 * self.resolution

    @property
    def ymax(self) -> float:
        """North edge."""
        return (self.row0 + self.nrows) * self.resolution

    @property
    def shape(self) -> tuple[int, int]:
        """``(rows, columns)``."""
        return (self.nrows, self.ncols)

    @property
    def size(self) -> int:
        """Number of cells."""
        return self.nrows * self.ncols

    def extent(self) -> tuple[float, float, float, float]:
        """``(xmin, xmax, ymin, ymax)``."""
        return (self.xmin, self.xmax, self.ymin, self.ymax)

    def cell_center(self, row: int, col: int) -> tuple[float, float]:
        """Centre coordinates of array cell ``(row, col)``."""
        r = self.resolution
        return (self.xmin + (col + 0.5) * r, self.ymax - (row + 0.5) * r)

    def to_record(self) -> dict[str, object]:
        """JSON-serializable form."""
        return {
            "resolution": self.resolution,
            "xmin": self.xmin,
            "xmax": self.xmax,
            "ymin": self.ymin,
            "ymax": self.ymax,
            "ncols": self.ncols,
            "nrows": self.nrows,
        }


def expected_grid(bounds: tuple[float, float, float, float], resolution: float) -> GridSpec:
    """Grid of a tile: header bounds ``(min_x, min_y, max_x, max_y)`` snapped outward.

    Raises:
        ValueError: The bounds are non-finite or have zero area.
    """
    min_x, min_y, max_x, max_y = bounds
    if not all(math.isfinite(v) for v in bounds):
        raise ValueError(f"non-finite tile bounds {bounds}")
    if not (min_x < max_x and min_y < max_y):
        raise ValueError(f"zero-area tile bounds {bounds}")
    col0, col1 = _floor_div(min_x, resolution), _ceil_div(max_x, resolution)
    row0, row1 = _floor_div(min_y, resolution), _ceil_div(max_y, resolution)
    return GridSpec(resolution, col0, row0, col1 - col0, row1 - row0)


def column_index(x: FloatArray, r: float) -> IntArray:
    """Global column owning each X: ``i*r <= x < (i+1)*r``."""
    i = np.floor(x / r)
    i -= i * r > x
    i += (i + 1) * r <= x
    return i.astype(np.int64)


def row_index(y: FloatArray, r: float) -> IntArray:
    """Global row owning each Y under right/down lookup: ``j*r < y <= (j+1)*r``."""
    j = np.ceil(y / r) - 1
    j += (j + 1) * r < y
    j -= j * r >= y
    return j.astype(np.int64)


def _round_half_away(value: float) -> float:
    return math.copysign(math.floor(abs(value) + 0.5), value)


def assign_cells(x: FloatArray, y: FloatArray, grid: GridSpec, convention: str) -> IntArray:
    """Flat (row-major, north-up) index of the owned cell of each point, or -1.

    ``x``/``y`` must be the complete population the tool sees for this tile
    (core plus buffer), because ``lidr_buffered_clamp`` depends on its extremes.

    Raises:
        ValueError: Unknown or undeclared convention.
    """
    r = grid.resolution
    if convention == "pdal_binmode_floor":
        # Mirrors PDAL Raster::xCell/yCell: floor((v - origin) / r) in binary64 with the
        # origin at the grid's lower-left corner; rows are counted upwards.
        with np.errstate(invalid="ignore", over="ignore"):
            col = np.floor((x - grid.xmin) / r)
            up = np.floor((y - grid.ymin) / r)
        row = (grid.nrows - 1) - up
        inside = (col >= 0) & (col < grid.ncols) & (row >= 0) & (row < grid.nrows)
        flat = np.where(inside, row * grid.ncols + col, -1)
        return flat.astype(np.int64)
    i = column_index(x, r)
    j = row_index(y, r)
    if convention == "lidr_buffered_clamp":
        if x.size:
            # Pinned lidR builds its raster layout from the buffered cloud; terra then
            # includes points on the layout's outer max-X/min-Y edges. Mirrors
            # benchmark_reference_cells() in R/1_functions.R.
            edge_x = _round_half_away((float(np.max(x)) - r / 2) / r) * r
            edge_y = _round_half_away((float(np.min(y)) - r / 2) / r) * r
            if not edge_x + r > grid.xmax:
                on_edge = x == grid.xmax
                i = np.where(on_edge, grid.col0 + grid.ncols - 1, i)
            if not edge_y < grid.ymin:
                on_edge = y == grid.ymin
                j = np.where(on_edge, grid.row0, j)
    elif convention != "right_down":
        raise ValueError(f"no cell assignment for convention {convention!r}")
    col = i - grid.col0
    row = (grid.row0 + grid.nrows - 1) - j
    inside = (col >= 0) & (col < grid.ncols) & (row >= 0) & (row < grid.nrows)
    return np.where(inside, row * grid.ncols + col, -1).astype(np.int64)


def ownership_differences(a: IntArray, b: IntArray) -> int:
    """Number of points owned by a different cell (or owned vs not) under two assignments."""
    return int(np.count_nonzero(a != b))


def grid_from_transform(
    transform: tuple[float, float, float, float, float, float],
    width: int,
    height: int,
    resolution: float,
    tolerance: float,
) -> GridSpec | None:
    """Global grid of a north-up raster whose edges sit on multiples of ``resolution``.

    ``transform`` is GDAL-ordered ``(x0, dx, rx, y0, ry, dy)``. Returns ``None``
    when the raster is rotated, has a different cell size or is misaligned.
    """
    x0, dx, rx, y0, ry, dy = transform
    if rx != 0 or ry != 0:
        return None
    if abs(dx - resolution) > tolerance or abs(dy + resolution) > tolerance:
        return None
    col0 = round(x0 / resolution)
    top = round(y0 / resolution)
    if abs(x0 - col0 * resolution) > tolerance or abs(y0 - top * resolution) > tolerance:
        return None
    return GridSpec(resolution, col0, top - height, width, height)
