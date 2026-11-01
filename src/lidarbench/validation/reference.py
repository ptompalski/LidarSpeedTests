"""Independent cell-statistic references and value/NoData comparison (requires numpy).

The reference is computed directly from decoded input points with float64
accumulation; it does not call any processing package. Empty cells are NaN
(NoData), never zero.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Any

import numpy as np

from lidarbench.validation.grid import FloatArray, GridSpec, IntArray
from lidarbench.workloads.contract import Tolerance

MAX_EXAMPLES = 10


def cell_statistic(z: FloatArray, cells: IntArray, grid: GridSpec, statistic: str) -> FloatArray:
    """Per-cell ``mean`` or ``max`` of ``z`` over owned points; NaN where empty.

    Returns a north-up ``(rows, cols)`` float64 array.
    """
    owned = cells >= 0
    index = cells[owned]
    values = z[owned]
    counts = np.bincount(index, minlength=grid.size)
    out = np.full(grid.size, np.nan, dtype=np.float64)
    occupied = counts > 0
    if statistic == "mean":
        sums = np.bincount(index, weights=values, minlength=grid.size)
        out[occupied] = sums[occupied] / counts[occupied]
    elif statistic == "max":
        peak = np.full(grid.size, -np.inf, dtype=np.float64)
        np.maximum.at(peak, index, values)
        out[occupied] = peak[occupied]
    else:
        raise ValueError(f"unsupported statistic {statistic!r}")
    return out.reshape(grid.shape)


PERCENTILES = {"z_p5": 0.05, "z_p25": 0.25, "z_p50": 0.50, "z_p75": 0.75, "z_p95": 0.95}
ABOVE_THRESHOLD = {"z_above2": 2.0}


def _float32_rank_weights(n: Any, p: float) -> tuple[Any, Any, Any]:
    """lasR's type-7 rank in binary32: ``(p / 100.0f) * (float)(n - 1) + 1``.

    Returns 0-based lower and upper indices and the binary32 fraction (Metrics.cpp).
    """
    f32 = np.float32
    with np.errstate(over="ignore"):
        rank = (f32(round(p * 100)) / f32(100.0)) * (n - 1).astype(f32) + f32(1.0)
    rank = rank.astype(f32)
    floor = np.floor(rank)
    lo = floor.astype(np.int64) - 1
    hi = np.minimum(np.ceil(rank).astype(np.int64) - 1, n - 1)
    return lo, hi, (rank - floor).astype(np.float64)


def cell_metrics(
    z: FloatArray,
    cells: IntArray,
    grid: GridSpec,
    bands: tuple[str, ...],
    *,
    single_point_sd: str | None = None,
    percentile_rank: str | None = None,
) -> dict[str, FloatArray]:
    """Per-cell metrics (``workloads.contract.METRICS``) of ``z`` over owned points.

    Returns one north-up ``(rows, cols)`` float64 array per band; empty cells are
    NaN in every band and ``z_sd`` is NaN in cells with fewer than two points.
    Percentiles are R type 7 on the sorted values of each cell.

    Declared implementation conventions (``workloads.contract``):
    ``single_point_sd = "zero"`` makes ``z_sd`` 0 in single-point cells, and
    ``percentile_rank = "float32"`` computes the type-7 rank in binary32 as lasR does.
    """
    owned = cells >= 0
    index = cells[owned]
    values = z[owned]
    counts = np.bincount(index, minlength=grid.size)
    occupied = counts > 0
    safe = np.maximum(counts, 1)
    out: dict[str, FloatArray] = {}

    def finish(name: str, data: FloatArray, valid: Any = occupied) -> None:
        result = np.full(grid.size, np.nan, dtype=np.float64)
        result[valid] = data[valid]
        out[name] = result.reshape(grid.shape)

    mean = np.bincount(index, weights=values, minlength=grid.size) / safe
    for band in bands:
        if band == "z_max":
            peak = np.full(grid.size, -np.inf, dtype=np.float64)
            np.maximum.at(peak, index, values)
            finish(band, peak)
        elif band == "z_mean":
            finish(band, mean)
        elif band == "z_sd":
            # Two-pass (centred) sum of squares: no cancellation for large elevations.
            squares = np.bincount(index, weights=(values - mean[index]) ** 2, minlength=grid.size)
            sd = np.sqrt(squares / np.maximum(counts - 1, 1))
            if single_point_sd == "zero":
                finish(band, np.where(counts == 1, 0.0, sd))
            elif single_point_sd is None:
                finish(band, sd, counts > 1)
            else:
                raise ValueError(f"unsupported single_point_sd {single_point_sd!r}")
        elif band in ABOVE_THRESHOLD:
            above = (values > ABOVE_THRESHOLD[band]).astype(np.float64)
            finish(band, np.bincount(index, weights=above, minlength=grid.size) / safe)
        elif band not in PERCENTILES:
            raise ValueError(f"unsupported metric {band!r}")
    wanted = [b for b in bands if b in PERCENTILES]
    if wanted:
        order = np.lexsort((values, index))
        ordered = values[order]
        starts = np.concatenate(([0], np.cumsum(counts)[:-1]))
        n = counts[occupied]
        first = starts[occupied]
        for band in wanted:
            if percentile_rank == "float32":
                lo, hi, weight = _float32_rank_weights(n, PERCENTILES[band])
            elif percentile_rank is None:
                h = (n - 1) * PERCENTILES[band]
                lo = np.floor(h).astype(np.int64)
                hi = np.minimum(lo + 1, n - 1)
                weight = h - lo
            else:
                raise ValueError(f"unsupported percentile_rank {percentile_rank!r}")
            low = ordered[first + lo]
            data = np.zeros(grid.size, dtype=np.float64)
            data[occupied] = low + (ordered[first + hi] - low) * weight
            finish(band, data)
    return {band: out[band] for band in bands}


@dataclass
class Comparison:
    """Cell-by-cell comparison of an output band with its reference."""

    compared_cells: int = 0
    reference_cells: int = 0
    actual_cells: int = 0
    nodata_where_reference_has_value: int = 0
    value_where_reference_is_empty: int = 0
    values_exceeding_tolerance: int = 0
    max_absolute_error: float | None = None
    max_error_to_limit_ratio: float | None = None
    sentinel_collisions: dict[str, int] = field(default_factory=dict)
    examples: list[dict[str, Any]] = field(default_factory=list)

    @property
    def mask_differences(self) -> int:
        """Cells whose NoData status differs."""
        return self.nodata_where_reference_has_value + self.value_where_reference_is_empty

    @property
    def passed(self) -> bool:
        """Exact mask agreement, every value within tolerance, no sentinel collision."""
        return (
            self.mask_differences == 0
            and self.values_exceeding_tolerance == 0
            and not self.sentinel_collisions
        )

    def problems(self) -> list[str]:
        """Human-readable reasons for failure."""
        out = []
        if self.nodata_where_reference_has_value:
            out.append(
                f"{self.nodata_where_reference_has_value} cell(s) are NoData in the output "
                "but occupied in the reference"
            )
        if self.value_where_reference_is_empty:
            out.append(
                f"{self.value_where_reference_is_empty} cell(s) have output values but are "
                "empty in the reference"
            )
        if self.values_exceeding_tolerance:
            out.append(
                f"{self.values_exceeding_tolerance} value(s) exceed the tolerance; maximum "
                f"absolute error {self.max_absolute_error!r}"
            )
        for sentinel, count in sorted(self.sentinel_collisions.items()):
            out.append(
                f"{count} reference value(s) equal the NoData sentinel {sentinel}; "
                "NoData and data cannot be distinguished"
            )
        return out

    def to_record(self) -> dict[str, Any]:
        """JSON-serializable form."""
        return {
            "passed": self.passed,
            "compared_cells": self.compared_cells,
            "reference_cells": self.reference_cells,
            "actual_cells": self.actual_cells,
            "nodata_where_reference_has_value": self.nodata_where_reference_has_value,
            "value_where_reference_is_empty": self.value_where_reference_is_empty,
            "values_exceeding_tolerance": self.values_exceeding_tolerance,
            "max_absolute_error": self.max_absolute_error,
            "max_error_to_limit_ratio": self.max_error_to_limit_ratio,
            "sentinel_collisions": dict(self.sentinel_collisions),
            "examples": list(self.examples),
        }


def _finite_or_none(value: float) -> float | None:
    return value if math.isfinite(value) else None


def compare_band(
    actual: FloatArray,
    reference: FloatArray,
    tolerance: Tolerance,
    grid: GridSpec,
    sentinels: tuple[float, ...] = (),
    encode: Any = None,
) -> Comparison:
    """Compare a decoded band (NaN = NoData) with a reference of the same shape.

    ``sentinels`` are NoData codes that must never equal a legitimate reference
    value; ``encode`` (for example ``numpy.float32``) maps reference values to the
    output's storage type before that check, since a value that rounds onto a
    sentinel in storage is indistinguishable from NoData.
    """
    if actual.shape != reference.shape:
        raise ValueError(f"shape mismatch {actual.shape} != {reference.shape}")
    actual_valid = ~np.isnan(actual)
    reference_valid = ~np.isnan(reference)
    result = Comparison(
        reference_cells=int(reference_valid.sum()),
        actual_cells=int(actual_valid.sum()),
        nodata_where_reference_has_value=int((reference_valid & ~actual_valid).sum()),
        value_where_reference_is_empty=int((actual_valid & ~reference_valid).sum()),
    )
    both = actual_valid & reference_valid
    result.compared_cells = int(both.sum())
    error = np.abs(actual - reference)
    limit = tolerance.absolute + tolerance.relative * np.abs(reference)
    exceeded = both & ~(error <= limit)
    result.values_exceeding_tolerance = int(exceeded.sum())
    if result.compared_cells:
        result.max_absolute_error = _finite_or_none(float(np.max(error[both])))
        with np.errstate(divide="ignore", invalid="ignore"):
            ratio = error[both] / limit[both]
        result.max_error_to_limit_ratio = _finite_or_none(float(np.max(ratio)))
    ref_values = reference[reference_valid]
    stored = ref_values if encode is None else np.asarray(ref_values, dtype=encode)
    for sentinel in sentinels:
        hits = int(np.count_nonzero(stored == sentinel))
        if hits:
            result.sentinel_collisions[repr(float(sentinel))] = hits

    problem = exceeded | (actual_valid != reference_valid)
    rows, cols = np.nonzero(problem)
    for row, col in zip(rows[:MAX_EXAMPLES], cols[:MAX_EXAMPLES], strict=True):
        x, y = grid.cell_center(int(row), int(col))
        a, ref = float(actual[row, col]), float(reference[row, col])
        result.examples.append(
            {
                "row": int(row),
                "col": int(col),
                "x_center": x,
                "y_center": y,
                "actual": _finite_or_none(a),
                "reference": _finite_or_none(ref),
                "absolute_error": _finite_or_none(abs(a - ref)),
                "limit": _finite_or_none(float(limit[row, col])),
            }
        )
    return result
