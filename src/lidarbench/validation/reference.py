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
