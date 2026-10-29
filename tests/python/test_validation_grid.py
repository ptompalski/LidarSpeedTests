"""Tests for grids, cell-ownership conventions, references and comparisons."""

from __future__ import annotations

import math
import random
from fractions import Fraction

import pytest

np = pytest.importorskip("numpy")

from lidarbench.validation.grid import (  # noqa: E402
    GridSpec,
    assign_cells,
    column_index,
    expected_grid,
    grid_from_transform,
    ownership_differences,
    row_index,
)
from lidarbench.validation.reference import cell_statistic, compare_band  # noqa: E402
from lidarbench.workloads.contract import Tolerance  # noqa: E402

TOL = Tolerance(2e-6, 2e-7)


def _edge_cases(r: float, base: float, n: int = 40) -> list[float]:
    values = []
    for k in range(-3, n):
        edge = (math.floor(base / r) + k) * r
        values += [edge, math.nextafter(edge, -math.inf), math.nextafter(edge, math.inf)]
    rng = random.Random(7)
    values += [base + rng.uniform(-50, 50) for _ in range(500)]
    return values


@pytest.mark.parametrize("r", [20.0, 1.0, 0.5, 0.25])
@pytest.mark.parametrize("base", [5000000.0, 500000.0, 0.0, -1234.0])
def test_indices_match_exact_rational_oracle(r, base):
    values = _edge_cases(r, base)
    arr = np.array(values)
    cols = column_index(arr, r)
    rows = row_index(arr, r)
    for v, i, j in zip(values, cols.tolist(), rows.tolist(), strict=True):
        q = Fraction(v) / Fraction(r)
        assert i == math.floor(q), (v, r)
        assert j == math.ceil(q) - 1, (v, r)


def test_expected_grid_snaps_outward():
    g = expected_grid((500000.0, 5000000.0, 500100.0, 5000100.0), 20.0)
    assert (g.xmin, g.xmax, g.ymin, g.ymax, g.ncols, g.nrows) == (
        500000.0,
        500100.0,
        5000000.0,
        5000100.0,
        5,
        5,
    )
    g = expected_grid((500100.37, 4999990.5, 500199.99, 5000081.0), 20.0)
    assert (g.xmin, g.xmax, g.ymin, g.ymax) == (500100.0, 500200.0, 4999980.0, 5000100.0)
    g = expected_grid((-10.5, -0.25, 3.0, 2.0), 1.0)
    assert (g.col0, g.ncols, g.row0, g.nrows) == (-11, 14, -1, 3)
    for bad in [(0.0, 0.0, 0.0, 5.0), (0.0, 0.0, math.inf, 5.0), (5.0, 0.0, 1.0, 5.0)]:
        with pytest.raises(ValueError):
            expected_grid(bad, 1.0)
    with pytest.raises(ValueError):
        GridSpec(1.0, 0, 0, 0, 1)


def test_right_down_partition_on_edges():
    grid = GridSpec(20.0, 0, 0, 2, 2)  # x, y in [0, 40]
    x = np.array([0.0, 20.0, 39.999, 40.0, 10.0, 10.0, 10.0, 10.0])
    y = np.array([10.0, 10.0, 10.0, 10.0, 0.0, 20.0, 40.0, 40.001])
    cells = assign_cells(x, y, grid, "right_down")
    # north-up flat index: row 0 is y in (20, 40], row 1 is y in (0, 20]
    assert cells.tolist() == [2, 3, 3, -1, -1, 2, 0, -1]


def test_lidr_clamp_depends_on_buffered_layout():
    """Mirrors benchmark_reference_cells(): layout edges round half away from zero."""
    grid = GridSpec(20.0, 10, 10, 2, 2)  # x, y in [200, 240]
    # Nothing south of the core: the layout's bottom edge is the core's min Y, so
    # terra's outer-edge inclusion keeps min-Y points in the bottom row. For positive
    # X the layout always reaches past a point on max X, so such points stay excluded.
    x = np.array([240.0, 210.0, 205.0])
    y = np.array([210.0, 200.0, 230.0])
    isolated = assign_cells(x, y, grid, "lidr_buffered_clamp")
    assert isolated.tolist() == [-1, 2, 0]
    assert assign_cells(x, y, grid, "right_down").tolist() == [-1, -1, 0]
    assert ownership_differences(isolated, assign_cells(x, y, grid, "right_down")) == 1
    # Buffered points below min Y extend the layout: no clamping.
    buffered = assign_cells(np.append(x, 230.0), np.append(y, 185.0), grid, "lidr_buffered_clamp")
    assert buffered.tolist() == [-1, -1, 0, -1]
    # With negative coordinates the max-X rounding goes the other way and clamps.
    west = GridSpec(20.0, -2, 0, 2, 2)
    clamped = assign_cells(np.array([0.0]), np.array([10.0]), west, "lidr_buffered_clamp")
    assert clamped.tolist() == [3]
    with pytest.raises(ValueError, match="undeclared"):
        assign_cells(x, y, grid, "undeclared")


def test_cell_statistics_with_empty_and_unusual_values():
    grid = GridSpec(1.0, 0, 0, 2, 2)
    x = np.array([0.5, 0.5, 0.5, 1.5, 1.5, 5.0])
    y = np.array([1.5, 1.5, 1.5, 0.5, 0.5, 0.5])
    z = np.array([-12.5, 0.0, 2500.123, 1e-3, -1e-3, 999.0])
    cells = assign_cells(x, y, grid, "right_down")
    mean = cell_statistic(z, cells, grid, "mean")
    peak = cell_statistic(z, cells, grid, "max")
    assert mean[0, 0] == pytest.approx((-12.5 + 0.0 + 2500.123) / 3)
    assert peak[0, 0] == 2500.123
    assert mean[1, 1] == 0.0 and peak[1, 1] == 1e-3
    assert np.isnan(mean[0, 1]) and np.isnan(peak[1, 0])  # empty cells are NoData, not 0
    with pytest.raises(ValueError):
        cell_statistic(z, cells, grid, "median")


def test_compare_tolerance_boundary_and_masks():
    grid = GridSpec(1.0, 0, 0, 2, 2)
    ref = np.array([[1000.0, np.nan], [-5.0, 0.0]])
    limit = TOL.absolute + TOL.relative * 1000.0
    ok = ref.copy()
    ok[0, 0] += limit * 0.999
    assert compare_band(ok, ref, TOL, grid).passed
    bad = ref.copy()
    bad[0, 0] += limit * 1.01
    result = compare_band(bad, ref, TOL, grid)
    assert result.values_exceeding_tolerance == 1 and not result.passed
    assert result.examples[0]["x_center"] == 0.5 and result.examples[0]["y_center"] == 1.5
    filled = ref.copy()
    filled[0, 1] = 0.0  # empty cell filled with zero
    assert compare_band(filled, ref, TOL, grid).value_where_reference_is_empty == 1
    holes = ref.copy()
    holes[1, 1] = np.nan
    r = compare_band(holes, ref, TOL, grid)
    assert r.nodata_where_reference_has_value == 1 and r.mask_differences == 1
    assert any("NoData in the output" in p for p in r.problems())
    with pytest.raises(ValueError):
        compare_band(ref[:1], ref, TOL, grid)


def test_float32_sentinel_collision():
    grid = GridSpec(1.0, 0, 0, 1, 2)
    ref = np.array([[-9999.0001, 5.0]])
    assert compare_band(ref, ref, TOL, grid, (-9999.0,)).passed  # distinct in float64
    collided = compare_band(ref, ref, TOL, grid, (-9999.0,), encode=np.float32)
    assert collided.sentinel_collisions == {"-9999.0": 1} and not collided.passed


def test_float32_storage_is_within_tolerance():
    rng = np.random.default_rng(1)
    ref = rng.uniform(-50, 3000, size=(30, 30))
    stored = ref.astype(np.float32).astype(np.float64)
    grid = GridSpec(1.0, 0, 0, 30, 30)
    result = compare_band(stored, ref, TOL, grid)
    assert result.passed and result.max_error_to_limit_ratio < 1


def test_grid_from_transform():
    assert grid_from_transform((100.0, 20.0, 0.0, 200.0, 0.0, -20.0), 3, 2, 20.0, 1e-8) == GridSpec(
        20.0, 5, 8, 3, 2
    )
    assert grid_from_transform((100.0, 20.0, 0.1, 200.0, 0.0, -20.0), 3, 2, 20.0, 1e-8) is None
    assert grid_from_transform((100.0, 10.0, 0.0, 200.0, 0.0, -10.0), 3, 2, 20.0, 1e-8) is None
    assert grid_from_transform((100.0, 20.0, 0.0, 200.0, 0.0, 20.0), 3, 2, 20.0, 1e-8) is None
    assert grid_from_transform((105.0, 20.0, 0.0, 200.0, 0.0, -20.0), 3, 2, 20.0, 1e-8) is None
