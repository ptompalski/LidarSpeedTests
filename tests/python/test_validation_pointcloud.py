"""Structural validation of point-cloud (height-normalization) products."""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest

from lidarbench.validation.pipeline import ValidationRequest, validate_outputs
from lidarbench.validation.status import Status
from lidarbench.workloads import default_suite_dir, load_suite

laspy = pytest.importorskip("laspy")

X0, Y0 = 500000.0, 5000000.0
CONTRACT = load_suite(default_suite_dir("workloads-v7")).get("normalization")


def ground(x):
    return 100.0 + 0.1 * (x - X0)


def _write(path: Path, pts: dict, point_format: int = 1) -> None:
    header = laspy.LasHeader(point_format=point_format, version="1.2")
    header.scales = np.array([0.01, 0.01, 0.001])
    header.offsets = np.array([X0, Y0, 0.0])
    las = laspy.LasData(header)
    las.x, las.y, las.z = pts["x"], pts["y"], pts["z"]
    las.classification = pts["classification"]
    las.intensity = pts["intensity"]
    las.return_number = pts["return_number"]
    las.number_of_returns = pts["number_of_returns"]
    if point_format == 1:
        las.gps_time = pts["gps_time"]
    las.write(path)


def _tile(xmin: float, rng: np.random.Generator, gps0: float) -> dict:
    gx, gy = np.meshgrid(np.arange(xmin, xmin + 101, 5.0), np.arange(Y0, Y0 + 101, 5.0))
    gx, gy = gx.ravel(), gy.ravel()
    vx = rng.uniform(xmin + 0.5, xmin + 99.5, 400).round(2)
    vy = rng.uniform(Y0 + 0.5, Y0 + 99.5, 400).round(2)
    x = np.concatenate([gx, vx])
    y = np.concatenate([gy, vy])
    height = np.concatenate([np.zeros(gx.size), rng.uniform(1, 30, 400).round(3)])
    n = x.size
    return {
        "x": x,
        "y": y,
        "z": (ground(x) + height).round(3),
        "height": height,
        "classification": np.concatenate([np.full(gx.size, 2), np.full(400, 5)]).astype(np.uint8),
        "intensity": rng.integers(0, 4000, n).astype(np.uint16),
        "return_number": np.ones(n, np.uint8),
        "number_of_returns": np.ones(n, np.uint8),
        "gps_time": gps0 + np.arange(n) * 1e-4,
    }


def _subset(pts: dict, keep) -> dict:
    return {k: v[keep] for k, v in pts.items()}


def _concat(a: dict, b: dict) -> dict:
    return {k: np.concatenate([a[k], b[k]]) for k in a}


def _normalized(pts: dict) -> dict:
    return {**pts, "z": pts["height"].copy()}


@pytest.fixture
def scene(tmp_path: Path):
    """Both tiles hold points exactly on their shared edge X0+100."""
    rng = np.random.default_rng(20261030)
    west = _tile(X0, rng, 1000.0)
    east = _tile(X0 + 100, rng, 2000.0)
    inputs = tmp_path / "in"
    inputs.mkdir()
    _write(inputs / "w.las", west)
    _write(inputs / "e.las", east)
    out = tmp_path / "out"
    out.mkdir()
    return {"west": west, "east": east, "inputs": inputs, "out": out}


def _validate(scene, tool: str = "lidR", mode: str = "numerical"):
    request = ValidationRequest(
        contract=CONTRACT,
        tool=tool,
        inputs=(scene["inputs"] / "w.las", scene["inputs"] / "e.las"),
        output_dir=scene["out"],
        validation_mode=mode,
    )
    return validate_outputs(request, validators=[])


def _write_outputs(scene, west: dict | None = None, east: dict | None = None, **kw) -> None:
    _write(scene["out"] / "w.laz", west or _normalized(scene["west"]), **kw)
    _write(scene["out"] / "e.laz", east or _normalized(scene["east"]), **kw)


def _structural(report, name: str) -> dict:
    return next(a for a in report.artifacts if a["name"] == name)["structural"]


def test_plausible_normalization_passes_structurally_without_a_reference(scene):
    west_edge = scene["west"]["x"] == X0 + 100
    east = _normalized(_concat(scene["east"], _subset(scene["west"], west_edge)))
    _write_outputs(scene, east=east)
    report = _validate(scene)
    v = report.verdicts
    assert v.artifacts.status is Status.PASSED
    assert v.structural.status is Status.PASSED, v.structural.reasons
    assert v.numerical.status is Status.UNSUPPORTED
    east_record = _structural(report, "e.laz")
    assert east_record["neighbour_edge_points_written"] == int(west_edge.sum()) > 0
    assert east_record["unmatched_output_points"] == 0
    assert east_record["compared_attributes"] == [
        "x",
        "y",
        "gps_time",
        "intensity",
        "return_number",
        "number_of_returns",
        "classification",
    ]
    west_record = _structural(report, "w.laz")
    assert west_record["missing_tile_points"] == 0
    assert west_record["ground_z_range"][0] <= 100.0
    record = report.to_record()
    assert record["contract"]["product"] == "point_cloud"
    assert record["contract"]["cell_ownership"] is None
    assert any("point cloud holds only unchanged" in c for c in record["claims"]["established"])


def test_corrupt_heights_fail_the_structural_level(scene):
    west = _normalized(scene["west"])
    west["z"][:3] = -2147483.0
    _write_outputs(scene, west=west)
    v = _validate(scene).verdicts
    assert v.structural.status is Status.FAILED
    assert any("3 output height(s) imply a ground elevation" in r for r in v.structural.reasons)
    assert v.numerical.status is Status.NOT_RUN


def test_buffer_points_and_changed_attributes_are_foreign(scene):
    near = (scene["east"]["x"] < X0 + 110) & (scene["east"]["x"] > X0 + 100)
    west = _normalized(_concat(scene["west"], _subset(scene["east"], near)))
    _write_outputs(scene, west=west)
    reasons = _validate(scene).verdicts.structural.reasons
    assert any(f"{int(near.sum())} output point(s) are not unchanged" in r for r in reasons)

    east = _normalized(scene["east"])
    east["classification"][0] = 6
    _write_outputs(scene, east=east)
    reasons = _validate(scene).verdicts.structural.reasons
    assert any("e.laz: 1 output point(s) are not unchanged" in r for r in reasons)


def test_missing_points_are_bounded_by_the_contract(scene):
    west = _normalized(scene["west"])
    _write_outputs(scene, west=_subset(west, np.arange(west["x"].size) >= 5))
    v = _validate(scene).verdicts
    assert v.structural.status is Status.FAILED
    assert any("5 of the tile's" in r and "missing" in r for r in v.structural.reasons)


def test_lost_attributes_fail(scene):
    _write_outputs(scene, point_format=0)
    reasons = _validate(scene).verdicts.structural.reasons
    assert any("lost the gps_time attribute" in r for r in reasons)


def test_performance_only_and_diagnostic_lasr(scene):
    _write_outputs(scene)
    v = _validate(scene, mode="performance-only").verdicts
    assert v.structural.status is Status.PASSED
    assert v.numerical.status is Status.UNVERIFIED
    v = _validate(scene, tool="lasR").verdicts
    assert v.structural.status is Status.PASSED
    assert v.implementation_status == "diagnostic_only"
    assert v.comparability()[0].value == "diagnostic_only"


def test_unreadable_output_fails_the_artifact_level(scene):
    _write_outputs(scene)
    path = scene["out"] / "w.laz"
    path.write_bytes(path.read_bytes()[:400])
    v = _validate(scene).verdicts
    assert v.artifacts.status is Status.FAILED
    assert v.structural.status is Status.NOT_RUN


def test_edge_points_outside_the_closed_bounds_are_foreign(scene):
    east = _subset(scene["east"], scene["east"]["x"] > X0 + 100)
    _write(scene["inputs"] / "e.las", east)
    west_edge = scene["west"]["x"] == X0 + 100
    _write_outputs(scene, east=_normalized(_concat(east, _subset(scene["west"], west_edge))))
    reasons = _validate(scene).verdicts.structural.reasons
    assert any(f"e.laz: {int(west_edge.sum())} output point(s)" in r for r in reasons)
