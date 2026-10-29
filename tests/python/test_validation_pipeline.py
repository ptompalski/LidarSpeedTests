"""End-to-end tests of the validation pipeline on synthetic fixtures.

Tool outputs are simulated by an oracle written here in pure Python with exact
``Fraction`` arithmetic (it shares no code with the module under test), saved
as real GeoTIFFs and then perturbed to exercise each failure mode.
"""

from __future__ import annotations

import json
import math
import shutil
from collections.abc import Callable
from fractions import Fraction
from pathlib import Path

import pytest
from validation_fixtures import (
    EPSG,
    X0,
    Y0,
    Cloud,
    neighbour_scene,
    write_las,
    write_raster,
)

from lidarbench.cli import main as cli_main
from lidarbench.core.canonical import pretty_bytes
from lidarbench.validation import (
    Comparability,
    Outcome,
    Policies,
    Status,
    ValidationRequest,
    validate_outputs,
)
from lidarbench.validation import pipeline as pipeline_module
from lidarbench.validation import population as population_module
from lidarbench.validation.io import BackendStatus
from lidarbench.validation.validators import (
    CellStatisticValidator,
    discover_validators,
)
from lidarbench.workloads import default_suite_dir, load_suite, parse_contract

np = pytest.importorskip("numpy")
pytest.importorskip("rasterio")
pytest.importorskip("laspy")

SUITE = load_suite(default_suite_dir())
MEAN = SUITE.get("pixel_metrics_1a")
DSM = SUITE.get("generate_DSM1")
BUFFER = 20.0


# -- independent oracle ---------------------------------------------------------------


def _round_half_away(v: Fraction) -> int:
    return int(math.copysign(math.floor(abs(v) + Fraction(1, 2)), v))


def oracle(
    clouds: dict[str, Cloud],
    stem: str,
    resolution: float,
    statistic: str,
    convention: str,
    neighbours: bool = True,
) -> tuple[list[list[float]], tuple[float, float]]:
    """North-up reference array and (xmin, ymax) of the expected grid."""
    r = Fraction(resolution)
    tile = clouds[stem]
    minx, maxx, miny, maxy = min(tile.x), max(tile.x), min(tile.y), max(tile.y)
    c0, c1 = math.floor(Fraction(minx) / r), math.ceil(Fraction(maxx) / r)
    r0, r1 = math.floor(Fraction(miny) / r), math.ceil(Fraction(maxy) / r)
    pts = []
    for name, cloud in clouds.items():
        if name != stem and not neighbours:
            continue
        for x, y, z in zip(cloud.x, cloud.y, cloud.z, strict=True):
            if minx - BUFFER <= x <= maxx + BUFFER and miny - BUFFER <= y <= maxy + BUFFER:
                pts.append((x, y, z))
    clamp_x = clamp_y = False
    if convention == "lidr_buffered_clamp":
        half = r / 2
        edge_x = _round_half_away((Fraction(max(p[0] for p in pts)) - half) / r) * r
        edge_y = _round_half_away((Fraction(min(p[1] for p in pts)) - half) / r) * r
        clamp_x = not edge_x + r > c1 * r
        clamp_y = not edge_y < r0 * r
    cells: dict[tuple[int, int], list[float]] = {}
    for x, y, z in pts:
        i = math.floor(Fraction(x) / r)
        j = math.ceil(Fraction(y) / r) - 1
        if clamp_x and Fraction(x) == c1 * r:
            i = c1 - 1
        if clamp_y and Fraction(y) == r0 * r:
            j = r0
        if c0 <= i < c1 and r0 <= j < r1:
            cells.setdefault((r1 - 1 - j, i - c0), []).append(z)
    out = [[math.nan] * (c1 - c0) for _ in range(r1 - r0)]
    for (row, col), zs in cells.items():
        out[row][col] = math.fsum(zs) / len(zs) if statistic == "mean" else max(zs)
    return out, (float(c0 * r), float(r1 * r))


@pytest.fixture(scope="module")
def scene(tmp_path_factory) -> tuple[Path, dict[str, Cloud]]:
    root = tmp_path_factory.mktemp("scene")
    return root, neighbour_scene(root)


def simulate(
    scene_dir: Path,
    clouds: dict[str, Cloud],
    out: Path,
    contract=MEAN,
    convention: str = "right_down",
    *,
    name: str = "{stem}.tif",
    descriptions: list[str] | None = None,
    edit: Callable[[str, np.ndarray], np.ndarray] | None = None,
    neighbours: bool = True,
    grow: tuple[int, int, int, int] = (0, 0, 0, 0),
    **raster_kwargs,
) -> Path:
    """Write one simulated product per tile; ``grow`` adds W/E/S/N cells (negative crops)."""
    out.mkdir(parents=True, exist_ok=True)
    r = contract.raster.resolution
    if descriptions is None and contract.raster.bands:
        descriptions = list(contract.raster.bands)
    for stem in clouds:
        ref, (xmin, ymax) = oracle(
            clouds, stem, r, contract.statistic, convention, neighbours=neighbours
        )
        arr = np.array(ref)
        w, e, s, n = grow
        arr = np.pad(arr, ((max(n, 0), max(s, 0)), (max(w, 0), max(e, 0))), constant_values=np.nan)
        arr = arr[
            -n if n < 0 else 0 : arr.shape[0] + s if s < 0 else arr.shape[0],
            -w if w < 0 else 0 : arr.shape[1] + e if e < 0 else arr.shape[1],
        ]
        if edit is not None:
            arr = edit(stem, arr)
        write_raster(
            out / name.format(stem=stem),
            [arr],
            xmin=xmin - w * r,
            ymax=ymax + n * r,
            resolution=r,
            descriptions=descriptions,
            **raster_kwargs,
        )
    return out


def run(scene_dir: Path, out: Path, contract=MEAN, tool="lasR", **kwargs):
    inputs = tuple(sorted(scene_dir.glob("*.las")))
    request = ValidationRequest(
        contract=contract,
        tool=tool,
        inputs=inputs,
        output_dir=out,
        **{"expected_epsg": EPSG, **kwargs},
    )
    return validate_outputs(request)


# -- correct products ---------------------------------------------------------------


@pytest.mark.parametrize("contract", [MEAN, DSM], ids=["mean20", "max1"])
def test_contract_convention_output_is_validated_and_comparable(scene, tmp_path, contract):
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out", contract)
    report = run(scene_dir, out, contract, "lasR")
    v = report.verdicts
    assert v.outcome is Outcome.VALIDATED, v.to_record()
    assert v.boundary_difference_points == 0
    assert v.comparability() == (Comparability.COMPARABLE, ())
    record = report.to_record()
    pretty_bytes(record)  # canonical JSON: no NaN, no numpy scalars
    assert record["schema"] == "lidarbench.validation-report/1"
    assert set(record["input_sha256"]) == {"west.las", "east.las"}
    assert any("cell ownership equals" in c for c in record["claims"]["established"])
    east = next(a for a in record["artifacts"] if a["tile"] == "east")
    assert east["numerical"]["reference"]["population_sources"] == ["east", "west"]


def test_lidr_convention_passes_but_is_descriptive_only(scene, tmp_path):
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out", MEAN, "lidr_buffered_clamp")
    v = run(scene_dir, out, MEAN, "lidR").verdicts
    assert v.outcome is Outcome.VALIDATED
    assert v.boundary_difference_points and v.boundary_difference_points > 0
    cls, reasons = v.comparability()
    assert cls is Comparability.DESCRIPTIVE_ONLY and "different cell" in reasons[0]
    # the same products do not satisfy the contract convention: never hidden
    v = run(scene_dir, out, MEAN, "lasR").verdicts
    assert v.numerical.status is Status.FAILED


@pytest.mark.parametrize(
    ("contract", "symptom"),
    [(MEAN, "exceed the tolerance"), (DSM, "NoData in the output")],
    ids=["mean20", "max1"],
)
def test_neighbouring_tile_points_are_required(scene, tmp_path, contract, symptom):
    """The east tile's first column (X0+100..) gets points stored only in the west file."""
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out", contract, neighbours=False)
    v = run(scene_dir, out, contract, "lasR").verdicts
    assert v.numerical.status is Status.FAILED
    assert any(r.startswith("east.tif") and symptom in r for r in v.numerical.reasons)
    assert not any(r.startswith("west.tif") for r in v.numerical.reasons)


# -- numerical failures -----------------------------------------------------------------


def _bump(amount: float, stem_name: str = "west"):
    def edit(stem, arr):
        if stem == stem_name:
            row, col = np.argwhere(~np.isnan(arr))[0]
            arr[row, col] += amount
        return arr

    return edit


def test_tolerance_is_enforced(scene, tmp_path):
    scene_dir, clouds = scene
    tiny = simulate(scene_dir, clouds, tmp_path / "tiny", MEAN, edit=_bump(1e-7), dtype="float64")
    assert run(scene_dir, tiny).verdicts.outcome is Outcome.VALIDATED
    big = simulate(scene_dir, clouds, tmp_path / "big", MEAN, edit=_bump(0.01), dtype="float64")
    report = run(scene_dir, big)
    assert report.verdicts.numerical.status is Status.FAILED
    detail = next(a for a in report.to_record()["artifacts"] if a["tile"] == "west")
    example = detail["numerical"]["comparison"]["examples"][0]
    assert example["absolute_error"] == pytest.approx(0.01, rel=1e-3)


def test_empty_cells_written_as_zero_fail(scene, tmp_path):
    scene_dir, clouds = scene

    def zero_fill(stem, arr):
        arr[np.isnan(arr)] = 0.0
        return arr

    out = simulate(scene_dir, clouds, tmp_path / "out", MEAN, edit=zero_fill)
    v = run(scene_dir, out).verdicts
    assert v.numerical.status is Status.FAILED
    assert any("empty in the reference" in r for r in v.numerical.reasons)


def test_missing_nodata_value_cells_fail(scene, tmp_path):
    scene_dir, clouds = scene

    def drop(stem, arr):
        arr[np.argwhere(~np.isnan(arr))[0][0], np.argwhere(~np.isnan(arr))[0][1]] = np.nan
        return arr

    out = simulate(scene_dir, clouds, tmp_path / "out", DSM, edit=drop)
    v = run(scene_dir, out, DSM).verdicts
    assert v.numerical.status is Status.FAILED


def test_sentinel_collision_fails(tmp_path):
    inputs = tmp_path / "in"
    inputs.mkdir()
    clouds = {
        "t": write_las(
            inputs / "t.las",
            [X0 + 0.5, X0 + 10.5, X0 + 19.5],
            [Y0 + 0.5, Y0 + 10.5, Y0 + 19.5],
            [-9999.0, 5.0, 6.0],
        )
    }
    out = simulate(inputs, clouds, tmp_path / "out", DSM)
    v = run(inputs, out, DSM).verdicts
    assert v.numerical.status is Status.FAILED
    assert any("NoData sentinel" in r for r in v.numerical.reasons)


# -- artifact and structural failures ------------------------------------------------------


def test_missing_artifact_stops_before_structure(scene, tmp_path):
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out")
    (out / "east.tif").unlink()
    v = run(scene_dir, out).verdicts
    assert v.artifacts.status is Status.FAILED and "east.tif: missing" in v.artifacts.reasons
    assert v.structural.status is Status.NOT_RUN and v.numerical.status is Status.NOT_RUN
    assert v.comparability()[0] is Comparability.EXCLUDED
    assert v.report_inclusion("performance-only") == (False, "validation_failed")


def test_truncated_raster_is_unreadable(scene, tmp_path):
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out")
    data = (out / "west.tif").read_bytes()
    (out / "west.tif").write_bytes(data[: len(data) // 3])
    v = run(scene_dir, out).verdicts
    assert v.artifacts.status is Status.FAILED
    assert any("cannot decode raster" in r for r in v.artifacts.reasons)


@pytest.mark.parametrize(
    ("kwargs", "fragment"),
    [
        ({"epsg": 32618}, "EPSG:32618 != expected EPSG:2959"),
        ({"epsg": None}, "CRS missing"),
        ({"epsg": 4326}, "geographic"),
        ({"descriptions": ["mean"]}, "band names"),
        ({"transform": (X0 + 0.5, 20.0, 0.0, Y0 + 100.0, 0.0, -20.0)}, "aligned"),
        ({"transform": (X0, 10.0, 0.0, Y0 + 100.0, 0.0, -10.0)}, "aligned"),
        ({"grow": (0, 1, 0, 0)}, "outward cells W/E/S/N = [0, 1, 0, 0]"),
        ({"grow": (0, -1, 0, 0)}, "outward cells W/E/S/N = [0, -1, 0, 0]"),
        ({"dtype": "int16", "nodata": -9999}, None),
    ],
)
def test_structural_failures(scene, tmp_path, kwargs, fragment):
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out", MEAN, **kwargs)
    v = run(scene_dir, out).verdicts
    if fragment is None:  # integer encoding is structurally valid but loses precision
        assert v.structural.status is Status.PASSED
        assert v.numerical.status is Status.FAILED
        return
    assert v.structural.status is Status.FAILED, v.to_record()
    assert any(fragment in r for r in v.structural.reasons), v.structural.reasons
    assert v.numerical.status is Status.NOT_RUN


def test_infinite_values_fail(scene, tmp_path):
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out", DSM, edit=_bump(math.inf))
    v = run(scene_dir, out, DSM).verdicts
    assert v.structural.status is Status.FAILED
    assert any("infinite" in r for r in v.structural.reasons)


def test_extent_policy_is_explicit_and_limited(scene, tmp_path):
    scene_dir, clouds = scene
    policy = Policies(accept_known_extent_discrepancies=True)
    outward = simulate(scene_dir, clouds, tmp_path / "outward", MEAN, grow=(0, 1, 0, 1))
    assert run(scene_dir, outward).verdicts.structural.status is Status.FAILED
    v = run(scene_dir, outward, policies=policy).verdicts
    assert v.numerical.status is Status.ACCEPTED_EXCEPTION
    assert not v.validated_performance
    assert v.report_inclusion("validated") == (True, "accepted_exception")
    assert v.comparability()[0] is Comparability.DESCRIPTIVE_ONLY
    for grow in ((0, 2, 0, 0), (-1, 0, 0, 0), (0, 0, 0, -1)):
        bad = simulate(scene_dir, clouds, tmp_path / f"bad{grow}", MEAN, grow=grow)
        assert run(scene_dir, bad, policies=policy).verdicts.structural.status is Status.FAILED


# -- unverified, unsupported, unavailable ------------------------------------------------------


def test_undeclared_tool_convention_is_unverified(scene, tmp_path):
    scene_dir, clouds = scene
    out = simulate(
        scene_dir, clouds, tmp_path / "out", MEAN, name="{stem}_avg.tif", descriptions=[]
    )
    v = run(scene_dir, out, MEAN, "lastools").verdicts
    assert v.numerical.status is Status.UNVERIFIED
    assert "agrees" in v.numerical.reasons[0]
    assert v.boundary_difference_points is None
    assert v.report_inclusion("validated") == (False, "unverified")
    assert v.report_inclusion("performance-only") == (True, "unverified (unverified)")


def test_unknown_crs_caps_numerical(scene, tmp_path):
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out")
    v = run(scene_dir, out, expected_epsg=None).verdicts
    assert v.numerical.status is Status.UNVERIFIED
    assert any("no CRS was declared" in r for r in v.numerical.reasons)


def test_performance_only_mode(scene, tmp_path):
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out")
    v = run(scene_dir, out, validation_mode="performance-only").verdicts
    assert (v.structural.status, v.numerical.status) == (Status.PASSED, Status.UNVERIFIED)
    assert v.report_inclusion("performance-only")[0] and not v.report_inclusion("validated")[0]

    empty = simulate(scene_dir, clouds, tmp_path / "empty", edit=lambda s, a: a * np.nan)
    v = run(scene_dir, empty, validation_mode="performance-only").verdicts
    assert v.structural.status is Status.FAILED


def test_execution_failure_is_never_inspected(scene, tmp_path):
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out")
    v = run(scene_dir, out, execution_status="timed_out").verdicts
    assert v.outcome is Outcome.EXECUTION_FAILED
    assert v.artifacts.status is Status.NOT_RUN


def test_unsupported_and_blocked_implementations(scene, tmp_path):
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out")
    v = run(scene_dir, out, MEAN, "PDAL").verdicts
    assert (v.structural.status, v.numerical.status) == (Status.UNSUPPORTED, Status.UNSUPPORTED)
    data = json.loads(json.dumps(MEAN.record))
    data["implementations"]["lasR"] = {
        "status": "blocked",
        "equivalence": "non_comparable",
        "reason": "known defect",
    }
    blocked = parse_contract(data)
    v = run(scene_dir, out, blocked, "lasR").verdicts
    assert v.artifacts.status is Status.FAILED and "contradicts" in v.artifacts.reasons[0]


def test_missing_raster_backend_is_unavailable(scene, tmp_path, monkeypatch):
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out")
    monkeypatch.setattr(
        pipeline_module,
        "probe_raster_backend",
        lambda: BackendStatus("rasterio", False, "rasterio missing", {}),
    )
    v = run(scene_dir, out).verdicts
    assert v.artifacts.status is Status.UNAVAILABLE
    assert v.outcome is Outcome.VALIDATION_INCOMPLETE
    assert not v.report_inclusion("performance-only")[0]


def test_missing_point_backend_is_unavailable(scene, tmp_path, monkeypatch):
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out")
    monkeypatch.setattr(
        CellStatisticValidator,
        "probe",
        lambda self: BackendStatus("laspy", False, "laspy missing", {}),
    )
    v = run(scene_dir, out).verdicts
    assert v.numerical.status is Status.UNAVAILABLE
    assert v.report_inclusion("performance-only") == (True, "unverified (unavailable)")


def test_no_applicable_validator_is_unsupported(scene, tmp_path):
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out")
    request = ValidationRequest(
        MEAN, "lasR", tuple(sorted(scene_dir.glob("*.las"))), out, expected_epsg=EPSG
    )
    v = validate_outputs(request, validators=[]).verdicts
    assert v.numerical.status is Status.UNSUPPORTED
    validators, errors = discover_validators()
    assert [x.name for x in validators] == ["cell-statistic"] and errors == []


# -- input problems -----------------------------------------------------------------------


def _copy_scene(scene_dir: Path, dest: Path) -> Path:
    dest.mkdir()
    for f in scene_dir.glob("*.las"):
        shutil.copy2(f, dest / f.name)
    return dest


def test_header_payload_mismatch_fails(scene, tmp_path):
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out")
    bad = _copy_scene(scene_dir, tmp_path / "in")
    west = clouds["west"]
    write_las(bad / "west.las", west.x, west.y, west.z, header_count=len(west.x) - 1)
    v = run(bad, out).verdicts
    assert v.numerical.status is Status.FAILED
    assert any("input cannot support a reference" in r for r in v.numerical.reasons)


def test_header_bounds_that_exclude_points_fail(scene, tmp_path):
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out")
    bad = _copy_scene(scene_dir, tmp_path / "in")
    w = clouds["west"]
    bounds = (min(w.x), min(w.y), min(w.z), max(w.x) - 5.0, max(w.y), max(w.z))
    write_las(bad / "west.las", w.x, w.y, w.z, header_bounds=bounds)
    v = run(bad, out).verdicts
    assert v.structural.status is Status.FAILED or v.numerical.status is Status.FAILED


def test_degenerate_input_fails(scene, tmp_path):
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out")
    bad = _copy_scene(scene_dir, tmp_path / "in")
    w = clouds["west"]
    write_las(bad / "west.las", w.x, w.y, w.z, header_bounds=(X0, Y0, 0.0, X0, Y0 + 100, 1.0))
    v = run(bad, out).verdicts
    assert v.structural.status is Status.FAILED
    assert any("zero-area" in r for r in v.structural.reasons)


def test_input_changed_during_validation(scene, tmp_path, monkeypatch):
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out")
    calls: dict[str, int] = {}

    def fake_sha(path):
        calls[str(path)] = calls.get(str(path), 0) + 1
        return f"sha-{calls[str(path)]}"

    monkeypatch.setattr(population_module, "sha256_file", fake_sha)
    v = run(scene_dir, out).verdicts
    assert v.numerical.status is Status.FAILED
    assert any("inputs changed during validation" in r for r in v.numerical.reasons)


# -- CLI ------------------------------------------------------------------------------------


def test_cli_validate(scene, tmp_path, capsys):
    scene_dir, clouds = scene
    out = simulate(scene_dir, clouds, tmp_path / "out")
    report = tmp_path / "report.json"
    args = [
        "validate",
        "--workload",
        "pixel_metrics_1a",
        "--tool",
        "lasR",
        "--outputs",
        str(out),
        "--input-dir",
        str(scene_dir),
        "--epsg",
        str(EPSG),
    ]
    assert cli_main([*args, "-o", str(report)]) == 0
    assert json.loads(report.read_text())["verdicts"]["outcome"] == "validated"
    assert cli_main([*args, "-o", str(report)]) == 2  # reports are write-once
    assert cli_main([*args, "--mode", "performance-only"]) == 3
    assert cli_main([*args[:-2], "--tool", "lidR"]) in (1, 3)
    assert (
        cli_main(
            [
                "validate",
                "--workload",
                "nope",
                "--tool",
                "x",
                "--outputs",
                str(out),
                "--input-dir",
                str(scene_dir),
            ]
        )
        == 2
    )
    assert (
        cli_main(
            ["validate", "--workload", "pixel_metrics_1a", "--tool", "x", "--outputs", str(out)]
        )
        == 2
    )
    (out / "west.tif").unlink()
    assert cli_main(args) == 1
    assert "artifacts   failed" in capsys.readouterr().out
