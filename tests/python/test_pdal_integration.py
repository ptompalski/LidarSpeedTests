"""Optional real-PDAL integration tests (skipped unless PDAL >= 2.5.0 is installed).

Runs the generated pipelines with a real ``pdal`` through the generic execution
engine on the synthetic two-tile fixture (points exactly on interior, shared,
south and north edges; an empty cell; unusual Z values), in directories whose
names contain spaces and non-ASCII characters, then validates the persisted
GeoTIFFs with the independent validator under the declared
``pdal_binmode_floor`` convention. Nothing is installed::

    LIDARBENCH_PDAL=/path/to/pdal uv run pytest tests/python/test_pdal_integration.py
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest
from validation_fixtures import EPSG, X0, Y0, neighbour_scene, write_las

from lidarbench.core.platform import get_platform_info
from lidarbench.execution.adapters import Invocation
from lidarbench.execution.adapters.pdal import PdalAdapter
from lidarbench.execution.attempt import execute_plan
from lidarbench.execution.plan import plan_invocation
from lidarbench.workloads import default_suite_dir, load_suite

pytest.importorskip("numpy")
rasterio = pytest.importorskip("rasterio")
pytest.importorskip("laspy")

pytestmark = pytest.mark.integration

PLATFORM = get_platform_info()
_ADAPTER = PdalAdapter()
_AVAILABILITY = _ADAPTER.probe(PLATFORM)
if not _AVAILABILITY.available:
    pytest.skip(f"PDAL unavailable: {_AVAILABILITY.detail}", allow_module_level=True)

SUITE = load_suite(default_suite_dir())


def _plane(x: float, y: float) -> float:
    return 150.0 + 0.1 * (x - X0) + 0.05 * (y - Y0)


def ground_scene(directory: Path) -> None:
    """Two 100 m tiles: planar ground (2) and water (9) points plus vegetation (1) far above."""
    for name, x0 in (("west", X0), ("east", X0 + 100.0)):
        xs, ys, zs, cs = [], [], [], []
        for i in range(21):
            for j in range(21):
                x, y = x0 + 5.0 * i, Y0 + 5.0 * j
                xs.append(x)
                ys.append(y)
                zs.append(_plane(x, y))
                cs.append(9 if i == 7 else 2)
                if i < 20 and j < 20:
                    xs.append(x + 2.5)
                    ys.append(y + 2.5)
                    zs.append(_plane(x + 2.5, y + 2.5) + 25.0)
                    cs.append(1)
        write_las(directory / f"{name}.las", xs, ys, zs, classification=cs)


def _run(
    tmp_path: Path,
    workload: str,
    mode: str = "stream",
    threads: int = 2,
    scene=neighbour_scene,
    validate: bool = True,
):
    from lidarbench.validation.pipeline import ValidationRequest, validate_outputs

    base = tmp_path / "PDAL run ü"
    paths = {k: base / v for k, v in {"in": "in tiles", "out": "out dir", "work": "work dir",
                                         "attempt": "attempt"}.items()}  # fmt: skip
    for path in paths.values():
        path.mkdir(parents=True)
    scene(paths["in"])
    adapter = PdalAdapter(execution_mode=mode)
    assert adapter.probe(PLATFORM).available
    inv = Invocation(
        workload,
        tuple(sorted(p for p in paths["in"].iterdir() if p.suffix in (".las", ".laz"))),
        paths["out"],
        paths["work"],
        threads=threads,
        parameters={"epsg": EPSG},
    )
    plan = plan_invocation(adapter, inv, availability=adapter.probe(PLATFORM))
    assert plan.ready, plan.reason
    execution = execute_plan(plan, inv, paths["attempt"], timeout_s=600, adapter=adapter)
    record = json.loads((paths["attempt"] / "execution.json").read_text(encoding="utf-8"))
    assert str(execution.execution_status) == "succeeded", record["failure_reason"]
    if not validate:
        return inv, record, None
    report = validate_outputs(
        ValidationRequest(
            contract=SUITE.get(workload),
            tool="pdal",
            inputs=inv.inputs,
            output_dir=inv.output_dir,
            expected_epsg=EPSG,
        )
    )
    return inv, record, report


@pytest.mark.parametrize("workload", ["pixel_metrics_1a", "generate_DSM1"])
def test_real_pdal_outputs_validate_under_the_declared_convention(tmp_path, workload):
    from lidarbench.validation.status import Comparability, Status

    inv, record, report = _run(tmp_path, workload)
    details = record["interpretation"]["details"]
    assert all(t["metadata"]["present"] for t in details["tiles"])
    assert record["interpretation"]["tool_timing"] is not None
    output_type = "mean" if workload == "pixel_metrics_1a" else "max"
    for artifact in sorted(inv.output_dir.glob("*.tif")):
        with rasterio.open(artifact) as src:
            assert src.count == 1 and src.nodata == -9999.0
            assert src.descriptions[0] == output_type and src.dtypes[0] == "float64"
    v = report.verdicts
    assert v.artifacts.status is Status.PASSED, v.artifacts.reasons
    assert v.structural.status is Status.PASSED, v.structural.reasons
    assert v.numerical.status is Status.PASSED, v.numerical.reasons
    assert v.boundary_difference_points and v.boundary_difference_points > 0
    assert v.comparability()[0] is Comparability.DESCRIPTIVE_ONLY


def test_real_pdal_standard_mode_matches_stream_mode(tmp_path):
    from lidarbench.validation.status import Status

    _, _, stream = _run(tmp_path / "stream", "generate_DSM1", "stream")
    _, _, standard = _run(tmp_path / "standard", "generate_DSM1", "standard", threads=1)
    assert stream.verdicts.numerical.status is Status.PASSED
    assert standard.verdicts.numerical.status is Status.PASSED


def test_real_pdal_dem_is_a_class_filtered_tin_validated_structurally(tmp_path):
    from lidarbench.validation.status import Outcome, Status

    inv, record, report = _run(tmp_path, "generate_DEM", scene=ground_scene)
    details = record["interpretation"]["details"]
    assert details["execution_mode"] == "standard"  # filters.delaunay cannot stream
    v = report.verdicts
    assert v.artifacts.status is Status.PASSED, v.artifacts.reasons
    assert v.structural.status is Status.PASSED, v.structural.reasons
    assert v.numerical.status is Status.UNSUPPORTED and v.outcome is Outcome.UNVERIFIED
    # Not part of validation: a linear TIN reproduces a planar ground surface exactly
    # at every cell centre, and the vegetation points 25 m above it are excluded.
    import numpy as np

    for artifact in sorted(inv.output_dir.glob("*.tif")):
        with rasterio.open(artifact) as src:
            assert src.count == 1 and src.nodata == -9999.0 and src.dtypes[0] == "float64"
            band = src.read(1)
            rows, cols = np.indices(band.shape)
            x = src.bounds.left + (cols + 0.5) * src.res[0]  # cell centres
            y = src.bounds.top - (rows + 0.5) * src.res[1]
            expected = _plane(x, y)
            valid = band != src.nodata
            assert valid.mean() > 0.99
            assert np.abs(band[valid] - expected[valid]).max() < 1e-6


def copc_scene(directory: Path, base=neighbour_scene) -> None:
    """A LAS fixture converted to real COPC files (writers.copc, scale kept)."""
    import subprocess

    staging = directory / "las"
    staging.mkdir()
    base(staging)
    pdal = _ADAPTER._pdal
    assert pdal is not None
    for las in sorted(staging.glob("*.las")):
        target = directory / f"{las.stem}.copc.laz"
        stages = [
            {"type": "readers.las", "filename": str(las)},
            {"type": "writers.copc", "filename": str(target), "forward": "all",
             "scale_x": 0.01, "scale_y": 0.01, "scale_z": 0.001},
        ]  # fmt: skip
        subprocess.run(
            [str(pdal), "pipeline", "--stdin"],
            input=json.dumps({"pipeline": stages}),
            text=True,
            check=True,
            capture_output=True,
        )
        las.unlink()
    staging.rmdir()


@pytest.mark.parametrize("workload", ["pixel_metrics_1a", "generate_DSM1", "generate_DEM"])
def test_real_pdal_on_copc_queries_neighbour_buffers(tmp_path, workload):
    from lidarbench.datasets.lasheader import read_las_header
    from lidarbench.validation.status import Status

    base = ground_scene if workload == "generate_DEM" else neighbour_scene
    inv, record, report = _run(tmp_path, workload, scene=lambda d: copc_scene(d, base))
    assert all(read_las_header(p).copc for p in inv.inputs)
    for doc in record["interpretation"]["details"]["pipelines"].values():
        readers = [s for s in doc["pipeline"] if s["type"].startswith("readers.")]
        assert {r["type"] for r in readers} == {"readers.copc"}
        assert all("bounds" in r for r in readers[1:])
    v = report.verdicts
    assert v.structural.status is Status.PASSED, v.structural.reasons
    expected = Status.UNSUPPORTED if workload == "generate_DEM" else Status.PASSED
    assert v.numerical.status is expected, v.numerical.reasons


def pond_scene(directory: Path) -> None:
    """Two 100 m tiles: planar ground (2), a water column (9) 2 m below the plane at
    i == 7, vegetation (1) 25 m above the plane at square centres and, in the west
    tile, one vegetation point 3 m west of all ground points (outside the hull)."""
    for name, x0 in (("west", X0), ("east", X0 + 100.0)):
        xs, ys, zs, cs = [], [], [], []
        for i in range(21):
            for j in range(21):
                x, y = x0 + 5.0 * i, Y0 + 5.0 * j
                xs.append(x)
                ys.append(y)
                zs.append(_plane(x, y) - (2.0 if i == 7 else 0.0))
                cs.append(9 if i == 7 else 2)
                if i < 20 and j < 20:
                    xs.append(x + 2.5)
                    ys.append(y + 2.5)
                    zs.append(_plane(x + 2.5, y + 2.5) + 25.0)
                    cs.append(1)
        if name == "west":
            xs.append(X0 - 3.0)
            ys.append(Y0 + 50.0)
            zs.append(_plane(X0 - 3.0, Y0 + 50.0) + 25.0)
            cs.append(1)
        write_las(directory / f"{name}.las", xs, ys, zs, classification=cs)


def test_real_pdal_normalization_uses_ground_and_water_and_keeps_attributes(tmp_path):
    from collections import Counter

    import laspy
    import numpy as np

    pytest.importorskip("lazrs")
    inv, record, _ = _run(tmp_path, "normalization", scene=pond_scene, validate=False)
    details = record["interpretation"]["details"]
    assert details["execution_mode"] == "standard"  # filters.hag_delaunay cannot stream
    assert details["output_problems"] == [] and details["warning_lines"] == 0
    assert all(t["metadata"]["writers.las"]["filename"] for t in details["tiles"])
    assert all(t["metadata"]["points_read"] > 0 for t in details["tiles"])
    bounds = PdalAdapter().header_bounds(inv)
    for source in inv.inputs:
        src, out = laspy.read(source), laspy.read(inv.output_dir / f"{source.stem}.laz")
        assert out.header.are_points_compressed
        assert out.header.point_format.id == src.header.point_format.id
        assert list(out.header.point_format.extra_dimension_names) == []
        assert list(out.header.scales) == list(src.header.scales)
        assert list(out.header.offsets) == list(src.header.offsets)
        assert np.isfinite(out.z).all()

        def key(las, i):
            return (int(las.X[i]), int(las.Y[i]), int(las.classification[i]))

        source_index = {key(src, i): i for i in range(len(src.points))}
        assert len(source_index) == len(src.points)
        # Every tile point is written once with X, Y and classification unchanged; the
        # only extra points are neighbour ground points exactly on the shared edge.
        written = Counter(key(out, i) for i in range(len(out.points)))
        assert all(written[k] >= 1 for k in source_index)
        min_x, min_y, max_x, max_y = bounds[source]
        for i in range(len(out.points)):
            k = key(out, i)
            if written[k] == 1 and k in source_index:
                s = source_index[k]
                assert out.intensity[i] == src.intensity[s]
                assert out.return_number[i] == src.return_number[s]
                cls = int(src.classification[s])
                x, y = float(src.x[s]), float(src.y[s])
                if cls in (2, 9):
                    expected = 0.0  # ground and water are the surface
                elif x < X0:
                    # Outside the ground hull: height above the nearest ground point.
                    expected = float(src.z[s]) - _plane(X0, y)
                else:
                    # Square centres next to the water column lie 1 m above the TIN's
                    # water-lowered surface: class 9 is ground.
                    column = int((x - (X0 if x < X0 + 100.0 else X0 + 100.0)) // 5.0)
                    expected = 26.0 if column in (6, 7) else 25.0
                assert abs(float(out.z[i]) - expected) < 0.0015, (x, y, cls, float(out.z[i]))
            else:
                assert int(out.classification[i]) in (2, 9)
                assert float(out.x[i]) in (min_x, max_x) or float(out.y[i]) in (min_y, max_y)
    if "normalization" in SUITE.contracts:  # the workloads-v7 contract
        from lidarbench.validation.pipeline import ValidationRequest, validate_outputs
        from lidarbench.validation.status import Status

        report = validate_outputs(
            ValidationRequest(
                contract=SUITE.get("normalization"),
                tool="pdal",
                inputs=inv.inputs,
                output_dir=inv.output_dir,
                expected_epsg=EPSG,
            )
        )
        assert report.verdicts.artifacts.status is Status.PASSED
        assert (
            report.verdicts.structural.status is Status.PASSED
        ), report.verdicts.structural.reasons
