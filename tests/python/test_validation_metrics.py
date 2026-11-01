"""Tests for the ``cell_metrics`` reference (pixel_metrics_2, workloads-v8).

The reference is checked against an independent pure-Python oracle (R ``sd`` with
denominator n - 1, ``mean(z > 2)`` and ``quantile(type = 7)``), then exercised
through the validation pipeline with a nine-band product (lidR/lasR layout) and
nine single-band products (LAStools ``artifact_bands`` layout).
"""

from __future__ import annotations

import copy
import math
import random
import statistics
import struct
import tomllib
from fractions import Fraction
from pathlib import Path

import pytest
from validation_fixtures import EPSG, X0, Y0, Cloud, neighbour_scene, write_las, write_raster

from lidarbench.validation import Outcome, Status, ValidationRequest, validate_outputs
from lidarbench.validation.validators import (
    CellMetricsValidator,
    discover_validators,
    select_validator,
)
from lidarbench.workloads import METRICS, ContractError, default_suite_dir, parse_contract

np = pytest.importorskip("numpy")
pytest.importorskip("rasterio")
pytest.importorskip("laspy")

from lidarbench.validation.grid import GridSpec  # noqa: E402
from lidarbench.validation.reference import cell_metrics  # noqa: E402

SUITE_DIR = default_suite_dir()
BANDS = ("z_max", "z_mean", "z_sd", "z_above2", "z_p5", "z_p25", "z_p50", "z_p75", "z_p95")
LASTOOLS_SUFFIXES = ("max", "avg", "std", "dns", "p05", "p25", "p50", "p75", "p95")
BUFFER = 20.0


def _data() -> dict:
    with open(SUITE_DIR / "pixel_metrics_2.toml", "rb") as handle:
        return tomllib.load(handle)


CONTRACT = parse_contract(_data())


def _test_contract() -> object:
    """The v8 contract with every tool on the contract partition (convention-free tests)."""
    data = copy.deepcopy(_data())
    for impl in data["implementations"].values():
        if "cell_ownership" in impl:
            impl["cell_ownership"] = "right_down"
            impl.pop("extent", None)
    return parse_contract(data)


# -- independent oracle ---------------------------------------------------------------


def _f32(value: float) -> float:
    """Round to IEEE-754 binary32 (independent of numpy)."""
    return struct.unpack("<f", struct.pack("<f", value))[0]


def oracle_metrics(
    zs: list[float], single_point_sd: str | None = None, percentile_rank: str | None = None
) -> dict[str, float]:
    """R ``metrics_multiple()`` (R/1_functions.R) for one cell, in plain Python.

    ``single_point_sd = "zero"``: LAStools' 0 for one point; ``percentile_rank =
    "float32"``: lasR's binary32 rank ``(p / 100.0f) * (float)(n - 1) + 1``.
    """
    n = len(zs)
    ordered = sorted(zs)

    def type7(p: float) -> float:
        if percentile_rank == "float32":
            rank = _f32(_f32(_f32(_f32(round(p * 100)) / _f32(100.0)) * _f32(n - 1)) + 1.0)
            lo, hi = math.floor(rank) - 1, min(math.ceil(rank) - 1, n - 1)
            return ordered[lo] + (ordered[hi] - ordered[lo]) * (rank - math.floor(rank))
        h = (n - 1) * p
        lo = math.floor(h)
        hi = min(lo + 1, n - 1)
        return ordered[lo] + (h - lo) * (ordered[hi] - ordered[lo])

    single = 0.0 if single_point_sd == "zero" else math.nan
    return {
        "z_max": max(zs),
        "z_mean": math.fsum(zs) / n,
        "z_sd": statistics.stdev(zs) if n > 1 else single,
        "z_above2": sum(z > 2 for z in zs) / n,
        "z_p5": type7(0.05),
        "z_p25": type7(0.25),
        "z_p50": type7(0.50),
        "z_p75": type7(0.75),
        "z_p95": type7(0.95),
    }


def oracle_tile(
    clouds: dict[str, Cloud], stem: str, resolution: float, **declared: str
) -> tuple[dict[str, list[list[float]]], tuple[float, float]]:
    """Per-band north-up references (contract partition) and (xmin, ymax) of the grid."""
    r = Fraction(resolution)
    tile = clouds[stem]
    minx, maxx, miny, maxy = min(tile.x), max(tile.x), min(tile.y), max(tile.y)
    c0, c1 = math.floor(Fraction(minx) / r), math.ceil(Fraction(maxx) / r)
    r0, r1 = math.floor(Fraction(miny) / r), math.ceil(Fraction(maxy) / r)
    cells: dict[tuple[int, int], list[float]] = {}
    for cloud in clouds.values():
        for x, y, z in zip(cloud.x, cloud.y, cloud.z, strict=True):
            if not (minx - BUFFER <= x <= maxx + BUFFER and miny - BUFFER <= y <= maxy + BUFFER):
                continue
            i = math.floor(Fraction(x) / r)
            j = math.ceil(Fraction(y) / r) - 1
            if c0 <= i < c1 and r0 <= j < r1:
                cells.setdefault((r1 - 1 - j, i - c0), []).append(z)
    out = {b: [[math.nan] * (c1 - c0) for _ in range(r1 - r0)] for b in BANDS}
    for (row, col), zs in cells.items():
        for band, value in oracle_metrics(zs, **declared).items():
            out[band][row][col] = value
    return out, (float(c0 * r), float(r1 * r))


# -- reference --------------------------------------------------------------------------


def test_metric_names_are_the_contract_bands():
    assert tuple(METRICS) == BANDS == CONTRACT.raster.bands
    assert CONTRACT.reference_method == "cell_metrics" and CONTRACT.statistic == ""


def test_cell_metrics_match_r_semantics():
    rng = random.Random(7)
    grid = GridSpec(resolution=1.0, col0=0, row0=0, ncols=4, nrows=3)
    sizes = {0: 1, 1: 2, 2: 0, 3: 7, 5: 50, 6: 3, 11: 1000}  # empty, single and tied cells
    cells, zs = [], []
    for cell, n in sizes.items():
        for _ in range(n):
            cells.append(cell)
            zs.append(round(rng.choice([rng.uniform(-3, 40), 1.5, 2.0, 2.0]), 2))
    order = list(range(len(zs)))
    rng.shuffle(order)
    z = np.array([zs[i] for i in order])
    c = np.array([cells[i] for i in order])
    c = np.concatenate([c, [-1, -1]])  # unowned points never contribute
    z = np.concatenate([z, [999.0, -999.0]])
    got = cell_metrics(z, c, grid, BANDS)
    assert list(got) == list(BANDS)
    for cell in range(grid.size):
        row, col = divmod(cell, grid.ncols)
        members = [zs[i] for i in range(len(zs)) if cells[i] == cell]
        if not members:
            assert all(math.isnan(got[b][row, col]) for b in BANDS)
            continue
        for band, expected in oracle_metrics(members).items():
            actual = float(got[band][row, col])
            if math.isnan(expected):
                assert band == "z_sd" and len(members) == 1 and math.isnan(actual)
            else:
                assert actual == pytest.approx(expected, rel=1e-12, abs=1e-12), (band, cell)
    assert got["z_above2"][0, 1] in (0.0, 0.5, 1.0)  # Z == 2 is not above 2


def test_cell_metrics_keep_the_requested_band_order():
    grid = GridSpec(resolution=1.0, col0=0, row0=0, ncols=1, nrows=1)
    got = cell_metrics(np.array([1.0, 3.0]), np.array([0, 0]), grid, ("z_p50", "z_max"))
    assert list(got) == ["z_p50", "z_max"] and got["z_p50"][0, 0] == 2.0
    with pytest.raises(ValueError, match="unsupported metric"):
        cell_metrics(np.array([1.0]), np.array([0]), grid, ("z_median",))


def test_metrics_validator_is_discovered_and_selected():
    validators, errors = discover_validators()
    assert errors == []
    chosen = select_validator(CONTRACT, validators)
    assert isinstance(chosen, CellMetricsValidator) and chosen.name == "cell-metrics"
    assert not CellMetricsValidator().applicable(parse_contract(_mean_data()))


def _mean_data() -> dict:
    with open(SUITE_DIR / "pixel_metrics_1a.toml", "rb") as handle:
        return tomllib.load(handle)


# -- pipeline ---------------------------------------------------------------------------


@pytest.fixture(scope="module")
def scene(tmp_path_factory) -> tuple[Path, dict[str, Cloud]]:
    root = tmp_path_factory.mktemp("metrics scene")
    return root, neighbour_scene(root)


def _write(
    clouds: dict[str, Cloud],
    out: Path,
    *,
    per_band: bool,
    edit=None,
    descriptions: tuple[str, ...] | None = BANDS,
    **declared: str,
) -> Path:
    out.mkdir(parents=True, exist_ok=True)
    for stem in clouds:
        refs, (xmin, ymax) = oracle_tile(clouds, stem, 20.0, **declared)
        arrays = {b: np.array(refs[b]) for b in BANDS}
        if edit is not None:
            edit(stem, arrays)
        if per_band:
            for band, suffix in zip(BANDS, LASTOOLS_SUFFIXES, strict=True):
                write_raster(
                    out / f"{stem}_{suffix}.tif",
                    [arrays[band]],
                    xmin=xmin,
                    ymax=ymax,
                    resolution=20.0,
                )
        else:
            write_raster(
                out / f"{stem}.tif",
                [arrays[b] for b in BANDS],
                xmin=xmin,
                ymax=ymax,
                resolution=20.0,
                descriptions=descriptions,
            )
    return out


def _run(scene_dir: Path, out: Path, tool: str, contract=None):
    request = ValidationRequest(
        contract=contract or _test_contract(),
        tool=tool,
        inputs=tuple(sorted(scene_dir.glob("*.las"))),
        output_dir=out,
        expected_epsg=EPSG,
    )
    return validate_outputs(request)


def test_nine_band_product_is_validated(scene, tmp_path):
    scene_dir, clouds = scene
    out = _write(clouds, tmp_path / "out", per_band=False)
    report = _run(scene_dir, out, "lasR")
    v = report.verdicts
    assert v.outcome is Outcome.VALIDATED, v.to_record()
    record = report.to_record()
    east = next(a for a in record["artifacts"] if a["tile"] == "east")
    numerical = east["numerical"]
    assert numerical["reference"]["method"] == "cell-metrics/1"
    assert numerical["reference"]["bands"] == list(BANDS)
    assert set(numerical["comparison"]["bands"]) == set(BANDS)
    assert any("per-cell metrics" in c for c in record["claims"]["established"])


def test_per_band_products_are_validated(scene, tmp_path):
    scene_dir, clouds = scene
    out = _write(clouds, tmp_path / "out", per_band=True)
    report = _run(scene_dir, out, "lastools")
    assert report.verdicts.outcome is Outcome.VALIDATED, report.verdicts.to_record()
    names = sorted(a["name"] for a in report.to_record()["artifacts"])
    assert len(names) == 18 and "east_p05.tif" in names
    std = next(a for a in report.to_record()["artifacts"] if a["name"] == "west_std.tif")
    assert std["numerical"]["comparison"]["passed"] is True  # single band: flat record


def test_a_wrong_band_fails_and_names_it(scene, tmp_path):
    scene_dir, clouds = scene

    def shift_p95(stem, arrays):
        if stem == "west":
            arrays["z_p95"] = arrays["z_p95"] + 0.01

    out = _write(clouds, tmp_path / "nine", per_band=False, edit=shift_p95)
    v = _run(scene_dir, out, "lasR").verdicts
    assert v.numerical.status is Status.FAILED
    assert any(r.startswith("west.tif: z_p95: ") for r in v.numerical.reasons), v.numerical.reasons
    assert not any("z_mean" in r for r in v.numerical.reasons)
    out = _write(clouds, tmp_path / "single", per_band=True, edit=shift_p95)
    v = _run(scene_dir, out, "lastools").verdicts
    assert v.numerical.status is Status.FAILED
    assert any(r.startswith("west_p95.tif: ") for r in v.numerical.reasons), v.numerical.reasons


def test_sd_values_in_empty_cells_fail(scene, tmp_path):
    """single_point_sd covers one-point cells only; empty cells stay NoData in every band."""
    scene_dir, clouds = scene

    def zero_where_empty(stem, arrays):
        arrays["z_sd"] = np.nan_to_num(arrays["z_sd"], nan=0.0)

    out = _write(clouds, tmp_path / "out", per_band=True, edit=zero_where_empty)
    v = _run(scene_dir, out, "lastools").verdicts
    assert v.numerical.status is Status.FAILED
    assert any("_std.tif" in r and "empty in the reference" in r for r in v.numerical.reasons)


# -- declared implementation conventions -----------------------------------------------


@pytest.fixture(scope="module")
def sparse_scene(tmp_path_factory) -> tuple[Path, dict[str, Cloud]]:
    """One 100 m tile: single-point cells, and a 554-point cell whose z_p95 depends on
    the rank arithmetic (rank 526.35 vs binary32 526.3499756 across a 4.31 m gap)."""
    root = tmp_path_factory.mktemp("sparse scene")
    rng = random.Random(3)
    pts = [(X0 + 5.0, Y0 + 95.0, 150.0), (X0 + 45.0, Y0 + 55.0, 151.25)]  # one point each
    pts += [(X0 + 85.0 + i, Y0 + 15.0, 140.0 + i) for i in range(3)]
    zs = [*sorted(round(rng.uniform(150.0, 209.0), 2) for _ in range(525)), 209.72, 214.03]
    zs += sorted(round(rng.uniform(214.04, 230.0), 2) for _ in range(27))
    pts += [(X0 + 20.5 + (i % 19), Y0 + 20.5 + (i // 19) * 0.6, z) for i, z in enumerate(zs)]
    pts += [(X0, Y0, 150.0), (X0 + 100.0, Y0 + 100.0, 150.0)]  # header corners
    xs, ys, zs_all = zip(*pts, strict=True)
    return root, {"solo": write_las(root / "solo.las", xs, ys, zs_all)}


def _declared_contract(tool: str, **declared: str):
    """The v8 contract with ``tool`` on the contract partition and only ``declared``."""
    data = copy.deepcopy(_data())
    impl = data["implementations"][tool]
    for key in ("single_point_sd", "percentile_rank"):
        impl.pop(key, None)
    impl.update(cell_ownership="right_down", **declared)
    return parse_contract(data)


def test_float32_rank_moves_the_pilot_percentile():
    zs = [0.3 * i for i in range(525)] + [209.72, 214.03] + list(range(300, 327))
    exact = oracle_metrics(zs)["z_p95"]
    single = oracle_metrics(zs, percentile_rank="float32")["z_p95"]
    assert 0.9e-4 < exact - single < 1.2e-4  # beyond the 2e-6 + 2e-7 * |z| tolerance
    grid = GridSpec(resolution=1.0, col0=0, row0=0, ncols=1, nrows=1)
    got = cell_metrics(
        np.array(zs, dtype=float), np.zeros(len(zs), dtype=np.int64), grid, ("z_p95",),
        percentile_rank="float32",
    )  # fmt: skip
    assert got["z_p95"][0, 0] == single
    sd = cell_metrics(np.array([5.0]), np.array([0]), grid, ("z_sd",), single_point_sd="zero")
    assert sd["z_sd"][0, 0] == 0.0


@pytest.mark.parametrize(
    ("declared", "written", "outcome"),
    [
        ({"single_point_sd": "zero"}, {"single_point_sd": "zero"}, Outcome.VALIDATED),
        ({}, {"single_point_sd": "zero"}, Outcome.VALIDATION_FAILED),
        ({"single_point_sd": "zero"}, {}, Outcome.VALIDATION_FAILED),
        ({"percentile_rank": "float32"}, {"percentile_rank": "float32"}, Outcome.VALIDATED),
        ({}, {"percentile_rank": "float32"}, Outcome.VALIDATION_FAILED),
        ({"percentile_rank": "float32"}, {}, Outcome.VALIDATION_FAILED),
    ],
)
def test_declared_conventions_are_reproduced_exactly(
    sparse_scene, tmp_path, declared, written, outcome
):
    scene_dir, clouds = sparse_scene
    # LAStools (per-band files) for z_sd; lasR (nine bands, no declared 0.01 m output
    # precision that would hide a 1e-4 m percentile difference) for the rank.
    tool = "lastools" if "single_point_sd" in {**declared, **written} else "lasR"
    out = _write(clouds, tmp_path / "out", per_band=tool == "lastools", **written)
    report = _run(scene_dir, out, tool, _declared_contract(tool, **declared))
    v = report.verdicts
    assert v.outcome is outcome, v.to_record()
    if outcome is Outcome.VALIDATED:
        notes = " ".join(v.numerical.reasons)
        if "single_point_sd" in declared:
            assert "2 single-point cell(s) compared with z_sd = 0" in notes
        else:
            assert "percentile_rank = float32" in notes
        artifacts = report.to_record()["artifacts"]
        if tool == "lastools":
            std = next(a for a in artifacts if a["name"].endswith("_std.tif"))
            declared_sd = std["numerical"]["reference"]["declared"]["single_point_sd"]
            assert declared_sd == {"value": "zero", "single_point_cells": 2}
            p95 = next(a for a in artifacts if a["name"].endswith("_p95.tif"))
            assert "declared" not in p95["numerical"]["reference"]
        else:
            reference = artifacts[0]["numerical"]["reference"]
            assert reference["declared"] == {"percentile_rank": "float32"}
    else:
        failed = [r for r in v.numerical.reasons if "exceed" in r or "empty" in r or "NoData" in r]
        prefix = "solo_std.tif" if tool == "lastools" else "solo.tif: z_p95: "
        assert failed and all(r.startswith(prefix) for r in failed), failed


def test_declared_variants_share_the_reference_cache(sparse_scene, tmp_path):
    from lidarbench.validation.validators import ReferenceCache, declared_conventions

    contract = parse_contract(_data())
    assert declared_conventions(contract) == {
        "right_down",
        "right_down|single_point_sd=zero",
        "pdal_binmode_floor|single_point_sd=zero",
        "right_down|percentile_rank=float32",
        "lidr_buffered_clamp",
    }
    scene_dir, clouds = sparse_scene
    cache = ReferenceCache()
    for attempt in ("first", "second"):
        out = _write(clouds, tmp_path / attempt, per_band=False, percentile_rank="float32")
        request = ValidationRequest(
            contract=contract,
            tool="lasR",
            inputs=tuple(sorted(scene_dir.glob("*.las"))),
            output_dir=out,
            expected_epsg=EPSG,
        )
        v = validate_outputs(request, reference_cache=cache).verdicts
        assert v.numerical.status in (Status.PASSED, Status.ACCEPTED_EXCEPTION), v.to_record()
    assert cache.hits == 1 and cache.misses == 1


def test_band_layout_is_structural(scene, tmp_path):
    scene_dir, clouds = scene
    out = _write(clouds, tmp_path / "names", per_band=False, descriptions=BANDS[::-1])
    v = _run(scene_dir, out, "lasR").verdicts
    assert v.structural.status is Status.FAILED and "band names" in v.structural.reasons[0]
    out = _write(clouds, tmp_path / "count", per_band=True)
    refs, (xmin, ymax) = oracle_tile(clouds, "west", 20.0)
    write_raster(
        out / "west_avg.tif",
        [np.array(refs["z_mean"])] * 2,
        xmin=xmin,
        ymax=ymax,
        resolution=20.0,
    )
    v = _run(scene_dir, out, "lastools").verdicts
    assert v.structural.status is Status.FAILED
    assert "west_avg.tif: 2 band(s); the contract requires 1 for its band(s) ['z_mean']" in (
        v.structural.reasons
    )


def test_declared_conventions_of_the_v8_contract_are_used(scene, tmp_path):
    """The real contract: lasR is on the contract partition, so its reference is shared."""
    scene_dir, clouds = scene
    out = _write(clouds, tmp_path / "out", per_band=False)
    assert _run(scene_dir, out, "lasR", CONTRACT).verdicts.outcome is Outcome.VALIDATED


# -- contract rules -----------------------------------------------------------------------


@pytest.mark.parametrize(
    ("mutate", "message"),
    [
        (
            lambda d: d["implementations"]["lastools"].update(single_point_sd="nodata"),
            "single_point_sd",
        ),
        (
            lambda d: d["implementations"]["lasR"].update(percentile_rank="float16"),
            "percentile_rank",
        ),
        (
            lambda d: (
                d["raster"].update(bands=["z_max", "z_mean"])
                or d["implementations"]["lasR"].pop("percentile_rank")
                or d["implementations"]["lastools"].update(
                    artifacts=["{stem}_max.tif", "{stem}_avg.tif"],
                    artifact_bands=["z_max", "z_mean"],
                )
            ),
            "needs a cell_metrics reference with a z_sd band",
        ),
        (lambda d: d["raster"].update(bands=["z_max", "z_median"]), r"raster.bands\[1\]"),
        (lambda d: d["raster"].update(bands=[]), "need named raster.bands"),
        (lambda d: d["reference"].update(statistic="mean"), "not allowed when method"),
        (lambda d: d["population"].update(points="first_returns"), 'points = "all"'),
        (
            lambda d: d["implementations"]["lastools"].update(
                artifact_bands=d["implementations"]["lastools"]["artifact_bands"][:-1]
            ),
            "one band per artifact",
        ),
        (
            lambda d: d["implementations"]["lastools"].update(artifact_bands=["z_max"] * 9),
            "exactly once",
        ),
        (
            lambda d: d["implementations"]["lastools"].update(artifact_bands=[1] * 9),
            r"artifact_bands\[0\]",
        ),
    ],
)
def test_invalid_metric_contracts_name_the_key(mutate, message):
    data = copy.deepcopy(_data())
    mutate(data)
    with pytest.raises(ContractError, match=message):
        parse_contract(data)


def test_artifact_bands_need_a_multi_band_raster():
    data = copy.deepcopy(_mean_data())
    data["implementations"]["lastools"]["artifact_bands"] = ["z_mean"]
    with pytest.raises(ContractError, match="two or more"):
        parse_contract(data)


@pytest.mark.parametrize("key", ["single_point_sd", "percentile_rank"])
def test_declared_metric_conventions_need_a_metrics_reference(key):
    data = copy.deepcopy(_mean_data())
    data["implementations"]["lasR"][key] = {"single_point_sd": "zero"}.get(key, "float32")
    with pytest.raises(ContractError, match=f"{key} needs a cell_metrics reference"):
        parse_contract(data)


def test_v8_declarations_are_recorded():
    record = CONTRACT.record["implementations"]
    assert record["lastools"]["single_point_sd"] == "zero"
    assert record["lasR"]["percentile_rank"] == "float32"
    assert "single_point_sd" not in record["lidR"] and "percentile_rank" not in record["lidR"]


def test_bands_of_maps_each_artifact_to_its_band():
    impl = CONTRACT.implementation("lastools")
    assert impl is not None and impl.artifact_bands == BANDS
    assert impl.bands_of("t 1", "t 1_dns.tif", BANDS) == ("z_above2",)
    assert CONTRACT.implementation("lasR").bands_of("t", "t.tif", BANDS) == BANDS
    assert CONTRACT.record["implementations"]["lastools"]["artifact_bands"] == list(BANDS)
    assert "artifact_bands" not in CONTRACT.record["implementations"]["lasR"]
