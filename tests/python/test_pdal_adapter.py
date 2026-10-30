"""PDAL adapter with a fake ``pdal`` (no PDAL needed).

Covers discovery (option, environment, PATH), version and capability checks
(PDAL >= 2.5.0, required stages and writers.gdal options), platform checks,
pipeline generation (snapped grid, buffered neighbours, CRS, paths with spaces,
non-ASCII and Windows separators), the launcher, conservative interpretation,
captured pipelines/metadata/timing, a campaign run, and independent validation
of the produced rasters under the declared ``pdal_binmode_floor`` convention.
"""

from __future__ import annotations

import json
import math
import os
import subprocess
import sys
from dataclasses import replace
from pathlib import Path, PureWindowsPath

import pytest
from campaign_helpers import write_config
from pdal_fakes import fake_env, make_fake_pdal, posix_only
from validation_fixtures import EPSG, mark_copc, neighbour_scene, write_las

from lidarbench.campaign.results import collect_rows
from lidarbench.campaign.runner import CampaignRunner, RunOptions
from lidarbench.config.campaign import load_config
from lidarbench.core.platform import PlatformInfo, get_platform_info
from lidarbench.execution.adapters import (
    ImplementationStatus,
    Invocation,
    ThreadingModel,
    UnavailableReason,
)
from lidarbench.execution.adapters import pdal_worker as worker
from lidarbench.execution.adapters.pdal import (
    EXECUTABLE_ENV,
    JOB_FILE,
    NODATA,
    OTHER_WORKLOADS,
    RUN_FILE,
    WORKER_MODULE,
    WORKLOADS,
    PdalAdapter,
    ProbeError,
    TileGrid,
    buffered_neighbours,
    build_pipeline,
    locate_executable,
    parse_drivers,
    parse_stage_options,
    parse_version,
    render_json,
    snapped_grid,
)
from lidarbench.execution.adapters.registry import AdapterRegistry
from lidarbench.execution.attempt import execute_plan
from lidarbench.execution.plan import AdapterContractError, PlanStatus, plan_invocation
from lidarbench.workloads import default_suite_dir, load_suite

PLATFORM = get_platform_info()
SUITE = load_suite(default_suite_dir())
needs_posix = pytest.mark.skipif(posix_only, reason="the fake pdal is a POSIX shebang script")


@pytest.fixture(autouse=True)
def _isolated_environment(monkeypatch, tmp_path):
    """No real PDAL from the developer's environment leaks into unit tests."""
    monkeypatch.delenv(EXECUTABLE_ENV, raising=False)
    monkeypatch.setenv("PATH", os.fspath(tmp_path / "empty PATH"))
    fake_env(monkeypatch)


@pytest.fixture
def pdal(tmp_path: Path) -> Path:
    return make_fake_pdal(tmp_path / "Program Files" / "PDAL ü" / "bin")


@pytest.fixture
def layout(tmp_path: Path) -> dict[str, Path]:
    base = tmp_path / "attempt root"
    paths = {
        "inputs": base / "input tiles; $x",
        "out": base / "out dir",
        "work": base / "work dir",
        "attempt": base / "attempt",
    }
    for path in paths.values():
        path.mkdir(parents=True)
    return paths


def _scene(layout) -> None:
    neighbour_scene(layout["inputs"])
    for name in ("west", "east"):
        (layout["inputs"] / f"{name}.las").rename(layout["inputs"] / f"{name} tile.las")


def _adapter(pdal: Path, **options) -> PdalAdapter:
    adapter = PdalAdapter(executable=os.fspath(pdal), **options)
    availability = adapter.probe(PLATFORM)
    assert availability.available, availability.detail
    return adapter


def _invocation(layout, workload="pixel_metrics_1a", threads=2, **parameters) -> Invocation:
    return Invocation(
        workload,
        tuple(sorted(layout["inputs"].glob("*.las"))),
        layout["out"],
        layout["work"],
        threads=threads,
        parameters=parameters,
    )


# -- registration, declarations and contract parity -----------------------------------


def test_adapter_is_a_registered_entry_point():
    registry = AdapterRegistry.discover()
    assert registry.adapter_class("pdal") is PdalAdapter


def test_workload_declarations():
    support = {s.workload_id: s for s in PdalAdapter().workload_support()}
    for workload_id in WORKLOADS:
        s = support[workload_id]
        assert s.status is ImplementationStatus.RUNNABLE
        assert s.threading.model is ThreadingModel.CONCURRENT_FILES
        assert (s.threading.min_threads, s.threading.max_threads) == (1, None)
    for s in OTHER_WORKLOADS:
        assert support[s.workload_id].status is ImplementationStatus.UNSUPPORTED
        assert support[s.workload_id].reason
    assert PdalAdapter().support_for("pixel_metrics_1a", threads=64).status.dispatchable


BINMODE = sorted(w for w, spec in WORKLOADS.items() if spec.method == "binmode")


@pytest.mark.parametrize("workload_id", BINMODE)
def test_workloads_match_the_v2_contracts(workload_id):
    contract = SUITE.get(workload_id)
    impl = contract.implementation(PdalAdapter.contract_tool)
    workload = WORKLOADS[workload_id]
    assert impl is not None and impl.status is ImplementationStatus.RUNNABLE
    assert impl.artifacts == (workload.artifact_pattern,)
    assert impl.cell_ownership == "pdal_binmode_floor"
    assert impl.equivalence == "matched_definition"
    assert {"mean": "mean", "max": "max"}[contract.statistic] == workload.output_type
    assert contract.raster.resolution == workload.resolution
    assert contract.buffer == workload.buffer
    assert NODATA in contract.raster.forbidden_values  # data colliding with NoData fails
    assert contract.declared_comparability("pdal") == "functionally_comparable"


def test_v2_suite_keeps_the_v1_definitions():
    v1 = load_suite(default_suite_dir("workloads-v1"))
    v2 = load_suite(default_suite_dir("workloads-v2"))
    for workload_id, contract in v2.contracts.items():
        old, new = dict(v1.get(workload_id).record), dict(contract.record)
        for record in (old, new):
            record.pop("suite")
            record["equivalence"] = None
        new["implementations"] = {k: v for k, v in new["implementations"].items() if k != "pdal"}
        assert old == new


def test_declared_classification_of_every_tool():
    for contract in (c for c in SUITE.contracts.values() if c.has_reference):
        assert contract.declared_comparability("lasR") == "strictly_comparable"
        assert contract.declared_comparability("lidR") == "functionally_comparable"
        assert contract.declared_comparability("lastools") == "functionally_comparable"
        assert contract.declared_comparability("pdal") == "functionally_comparable"
        assert contract.declared_comparability("unknown") == "unsupported"


def test_adapter_module_is_stdlib_only():
    code = (
        "import sys, lidarbench.execution.adapters.pdal\n"
        "bad = {'numpy', 'rasterio', 'laspy'} & set(sys.modules)\n"
        "assert not bad, bad\n"
    )
    subprocess.run([sys.executable, "-c", code], check=True)


@pytest.mark.parametrize(
    "options",
    [
        {"executable": 3},
        {"execution_mode": "fast"},
        {"expected_version": "2.6"},
        {"version_timeout_s": 0},
        {"version_timeout_s": True},
        {"python": None},
    ],
)
def test_invalid_options_raise_type_error(options):
    with pytest.raises(TypeError):
        PdalAdapter(**options)


# -- parsing --------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("text", "numbers", "full"),
    [
        (
            "----\npdal 2.6.3 (git-version: Release)\n----\n",
            (2, 6, 3),
            "2.6.3 (git-version: Release)",
        ),
        ("pdal 2.10.2\n", (2, 10, 2), "2.10.2"),
        ("pdal 2.8.0rc1 (git-version: 1a2b3c)", (2, 8, 0), "2.8.0rc1 (git-version: 1a2b3c)"),
    ],
)
def test_parse_version(text, numbers, full):
    assert parse_version(text) == (numbers, full)


@pytest.mark.parametrize("text", ["", "PDAL version unknown", "pdal 2.6.3\npdal 2.6.3\n"])
def test_parse_version_rejects_unidentifiable_output(text):
    with pytest.raises(ProbeError):
        parse_version(text)


def test_parse_drivers_and_options():
    assert parse_drivers('[{"name": "readers.las"}, {"x": 1}, 3]') == {"readers.las"}
    listed = '["writers.gdal", [{"name": "binmode"}, {"name": "nodata", "default": "nan"}]]'
    assert parse_stage_options(listed, "writers.gdal") == {"binmode", "nodata"}
    assert parse_stage_options('{"writers.gdal": [{"name": "width"}]}', "writers.gdal") == {"width"}
    for bad in ("nope", '{"a": 1}', '["readers.las", []]'):
        with pytest.raises(ProbeError):
            parse_stage_options(bad, "writers.gdal")
    for bad in ("nope", "{}"):
        with pytest.raises(ProbeError):
            parse_drivers(bad)


# -- grid and pipeline generation -----------------------------------------------------


@pytest.mark.parametrize(
    ("bounds", "r", "expected"),
    [
        ((500000.0, 5000000.0, 500100.0, 5000100.0), 20.0, (25000, 250000, 5, 5)),
        ((500100.37, 5000000.0, 500200.0, 5000100.0), 20.0, (25005, 250000, 5, 5)),
        ((500100.37, 5000000.5, 500199.5, 5000099.5), 1.0, (500100, 5000000, 100, 100)),
        ((-10.5, -0.25, 9.5, 0.25), 20.0, (-1, -1, 2, 2)),
    ],
)
def test_snapped_grid(bounds, r, expected):
    grid = snapped_grid(bounds, r)
    assert (grid.col0, grid.row0, grid.width, grid.height) == expected
    assert grid.origin_x == grid.col0 * r and grid.origin_y == grid.row0 * r


def test_snapped_grid_matches_the_validator():
    grid_module = pytest.importorskip("lidarbench.validation.grid")
    for bounds in [
        (500000.0, 5000000.0, 500100.0, 5000100.0),
        (500100.37, 5000000.01, 500199.99, 5000099.5),
        (0.1, 0.1, 0.3, 0.30000000000000004),
    ]:
        for r in (1.0, 20.0, 0.1):
            ours = snapped_grid(bounds, r)
            theirs = grid_module.expected_grid(bounds, r)
            assert (ours.col0, ours.row0, ours.width, ours.height) == (
                theirs.col0,
                theirs.row0,
                theirs.ncols,
                theirs.nrows,
            )
            assert (ours.origin_x, ours.origin_y) == (theirs.xmin, theirs.ymin)


def test_snapped_grid_rejects_degenerate_bounds():
    for bounds in [(0.0, 0.0, 0.0, 1.0), (0.0, 0.0, math.inf, 1.0)]:
        with pytest.raises(ValueError):
            snapped_grid(bounds, 1.0)


def test_buffered_neighbours_use_the_closed_buffer_rectangle():
    a, b, c, d = (Path(f"/t/{n}.las") for n in "abcd")
    bounds = {
        a: (0.0, 0.0, 100.0, 100.0),
        b: (120.0, 0.0, 220.0, 100.0),  # exactly 20 m away: inside the closed rectangle
        c: (120.01, 0.0, 220.0, 100.0),  # beyond the buffer
        d: (-50.0, 110.0, 10.0, 150.0),
    }
    assert buffered_neighbours(bounds, a, 20.0) == [b, d]
    assert buffered_neighbours(bounds, c, 20.0) == [b]


def test_pipeline_document():
    grid = TileGrid(20.0, 25000, 250000, 5, 5)
    tile, other = Path("/data/my tile.las"), Path("/data/ńeighbour.laz")
    alone = build_pipeline(tile, [], Path("/out/my tile.tif"), grid, "mean")["pipeline"]
    assert [s["type"] for s in alone] == ["readers.las", "writers.gdal"]
    assert alone[-1]["inputs"] == ["tile"] and "override_srs" not in alone[-1]
    stages = build_pipeline(tile, [other], Path("/out/my tile.tif"), grid, "max", 2959)["pipeline"]
    assert [s["type"] for s in stages] == [
        "readers.las",
        "readers.las",
        "filters.merge",
        "writers.gdal",
    ]
    assert stages[2]["inputs"] == ["tile", "neighbour_1"]
    writer = stages[-1]
    assert writer == {
        "type": "writers.gdal",
        "filename": os.fspath(Path("/out/my tile.tif")),
        "inputs": ["merged"],
        "binmode": True,
        "output_type": "max",
        "resolution": 20.0,
        "origin_x": 500000.0,
        "origin_y": 5000000.0,
        "width": 5,
        "height": 5,
        "nodata": -9999.0,
        "data_type": "double",
        "gdaldriver": "GTiff",
        "override_srs": "EPSG:2959",
    }
    # Each readers.las gets one decompression thread (PDAL's default pool is 7), so
    # concurrency comes only from concurrent pipelines (the thread budget).
    assert all(s["threads"] == 1 for s in stages if s["type"] == "readers.las")
    legacy = build_pipeline(tile, [other], Path("/o.tif"), grid, "max", reader_threads=None)
    assert all("threads" not in s for s in legacy["pipeline"])


@needs_posix
def test_reader_threads_follow_the_probed_options(pdal, layout, monkeypatch):
    _scene(layout)
    adapter = _adapter(pdal)
    assert adapter.probe(PLATFORM).available
    assert adapter.identify().details["reader_threads"] == 1
    plan = plan_invocation(adapter, _invocation(layout))
    assert plan.metadata["reader_threads"] == 1
    monkeypatch.setenv("FAKE_PDAL_READER_THREADS", "absent")
    old = _adapter(pdal)
    assert old.probe(PLATFORM).available
    assert "not set" in str(old.identify().details["reader_threads"])
    for _, _, doc in old.pipelines(_invocation(layout)):
        assert all("threads" not in s for s in doc["pipeline"])
    grid = TileGrid(1.0, 0, 0, 1, 1)
    tile = PureWindowsPath(r"C:\Data sets\tiles ü\a b.laz")
    out = PureWindowsPath(r"D:\out dir\a b.tif")
    text = render_json(build_pipeline(tile, [], out, grid, "max"))  # type: ignore[arg-type]
    stages = json.loads(text)["pipeline"]
    assert stages[0]["filename"] == r"C:\Data sets\tiles ü\a b.laz"
    assert stages[-1]["filename"] == r"D:\out dir\a b.tif"
    assert "ü" in text  # UTF-8, not escaped


# -- discovery and probing ------------------------------------------------------------


def test_locate_precedence(tmp_path, pdal):
    env = {EXECUTABLE_ENV: "/from/env/pdal", "PATH": os.fspath(pdal.parent)}
    assert locate_executable("/opt/pdal", env) == (Path("/opt/pdal").absolute(), "option")
    assert locate_executable("", env) == (Path("/from/env/pdal").absolute(), "environment")


@needs_posix
def test_probe_and_identify(pdal):
    adapter = _adapter(pdal)
    identity = adapter.identify()
    assert identity.tool == "PDAL" and identity.version == "2.6.3"
    assert identity.details["full_version"] == "2.6.3 (git-version: abc123)"
    assert identity.details["discovery"] == "option"
    assert identity.details["execution_mode"] == "stream"
    assert identity.executables["pdal"] == pdal
    assert Path(identity.executables["python"]).is_absolute()


@needs_posix
def test_probe_from_environment_and_path(pdal, monkeypatch):
    monkeypatch.setenv(EXECUTABLE_ENV, os.fspath(pdal))
    adapter = PdalAdapter()
    assert adapter.probe(PLATFORM).available
    assert adapter.identify().details["discovery"] == "environment"
    monkeypatch.delenv(EXECUTABLE_ENV)
    monkeypatch.setenv("PATH", os.fspath(pdal.parent))
    adapter = PdalAdapter()
    assert adapter.probe(PLATFORM).available
    assert adapter.identify().details["discovery"] == "PATH"


def test_missing_installation_is_unavailable_and_a_skip_plan(layout, tmp_path):
    availability = PdalAdapter().probe(PLATFORM)
    assert availability.reason is UnavailableReason.NOT_INSTALLED
    assert "not installed automatically" in availability.detail
    write_las(layout["inputs"] / "a.las", [0.5], [0.5], [1.0])
    plan = plan_invocation(PdalAdapter(), _invocation(layout))
    assert plan.status is PlanStatus.SKIPPED_UNAVAILABLE
    missing = PdalAdapter(executable=os.fspath(tmp_path / "nowhere" / "pdal"))
    assert missing.probe(PLATFORM).reason is UnavailableReason.MISCONFIGURED


def test_unsupported_platform(pdal):
    other = PlatformInfo("FreeBSD", "14", "amd64", "3.11", 8, 2**34)  # type: ignore[arg-type]
    availability = PdalAdapter(executable=os.fspath(pdal)).probe(other)
    assert availability.reason is UnavailableReason.PLATFORM_UNSUPPORTED


@needs_posix
@pytest.mark.parametrize(
    ("env", "options", "reason", "text"),
    [
        ({"version": "2.4.1"}, {}, UnavailableReason.VERSION_UNSUPPORTED, "older than 2.5.0"),
        ({}, {"expected_version": "2.7.0"}, UnavailableReason.VERSION_UNSUPPORTED, "expected"),
        ({"options": "nobinmode"}, {}, UnavailableReason.VERSION_UNSUPPORTED, "binmode"),
        ({"drivers": "nomerge"}, {}, UnavailableReason.MISCONFIGURED, "filters.merge"),
        ({"options": "broken"}, {}, UnavailableReason.PROBE_FAILED, "not JSON"),
    ],
)
def test_unusable_installations(pdal, monkeypatch, env, options, reason, text):
    fake_env(monkeypatch, **env)
    availability = PdalAdapter(executable=os.fspath(pdal), **options).probe(PLATFORM)
    assert availability.reason is reason and text in availability.detail


@needs_posix
def test_expected_version_accepts_the_match(pdal):
    assert _adapter(pdal, expected_version="2.6.3").identify().version == "2.6.3"


# -- command construction -------------------------------------------------------------


@needs_posix
def test_plan_with_spaces_and_metacharacters(pdal, layout):
    _scene(layout)
    adapter = _adapter(pdal)
    inv = _invocation(layout, "generate_DSM1", threads=3, epsg=EPSG)
    plan = plan_invocation(adapter, inv, availability=adapter.probe(PLATFORM))
    assert plan.ready, plan.reason
    command = plan.command
    assert command.argv[1:] == ("-m", WORKER_MODULE, "--job", os.fspath(layout["work"] / JOB_FILE))
    assert command.env["GDAL_NUM_THREADS"] == "1" and command.env["OMP_NUM_THREADS"] == "1"
    files = {c.name: c.content for c in command.config_files}
    assert sorted(files) == [JOB_FILE, "pipeline-0001.json", "pipeline-0002.json"]
    job = json.loads(files[JOB_FILE])
    assert job["schema"] == worker.JOB_SCHEMA and job["concurrency"] == 3
    assert job["pdal"] == os.fspath(pdal) and job["execution_mode"] == "stream"
    assert [t["stem"] for t in job["tiles"]] == ["east tile", "west tile"]
    east = json.loads(files["pipeline-0001.json"])["pipeline"]
    assert east[0]["filename"] == os.fspath(layout["inputs"] / "east tile.las")
    assert east[1]["filename"] == os.fspath(layout["inputs"] / "west tile.las")
    writer = east[-1]
    assert writer["filename"] == os.fspath(layout["out"] / "east tile.tif")
    assert (writer["origin_x"], writer["width"]) == (500100.0, 100)  # snapped from 500100.37
    assert writer["override_srs"] == f"EPSG:{EPSG}"
    assert [a.path.name for a in plan.expected_outputs] == ["east tile.tif", "west tile.tif"]
    meta = plan.metadata
    assert meta["binmode"] is True and meta["output_type"] == "max"
    assert meta["neighbour_reads"] == 2 and meta["pipelines"] == 2


@needs_posix
@pytest.mark.parametrize(
    ("change", "text"),
    [
        ({"parameters": {"epsg": "2959"}}, "positive integer"),
        ({"parameters": {"epsg": True}}, "positive integer"),
        ({"parameters": {"res": 2.0}}, "does not accept"),
        ({"workload_id": "generate_DSM2"}, "no workloads-v2 contract"),
    ],
)
def test_unsupported_combinations(pdal, layout, change, text):
    write_las(layout["inputs"] / "a.las", [0.5], [0.5], [1.0])
    adapter = _adapter(pdal)
    plan = plan_invocation(adapter, replace(_invocation(layout), **change), adapter.probe(PLATFORM))
    assert plan.status is PlanStatus.SKIPPED_UNSUPPORTED and text in plan.reason


@needs_posix
def test_paths_pdal_would_misread_are_unsupported(pdal, layout, tmp_path):
    write_las(layout["inputs"] / "a.las", [0.5], [0.5], [1.0])
    adapter = _adapter(pdal)
    hashed = tmp_path / "out #1"
    hashed.mkdir()
    plan = plan_invocation(adapter, replace(_invocation(layout), output_dir=hashed))
    assert plan.status is PlanStatus.SKIPPED_UNSUPPORTED and "'#'" in plan.reason
    (layout["inputs"] / "b.txt").write_text("x")
    inv = replace(_invocation(layout), inputs=(layout["inputs"] / "b.txt",))
    assert "LAS/LAZ" in plan_invocation(adapter, inv).reason
    write_las(layout["inputs"] / "sub.las", [0.5], [0.5], [1.0])
    twin = layout["inputs"] / "twin"
    twin.mkdir()
    write_las(twin / "SUB.las", [0.5], [0.5], [1.0])
    inv = replace(_invocation(layout), inputs=(layout["inputs"] / "sub.las", twin / "SUB.las"))
    assert "same output" in plan_invocation(adapter, inv).reason


@needs_posix
def test_unreadable_header_fails_planning_instead_of_skipping(pdal, layout):
    (layout["inputs"] / "broken.las").write_bytes(b"not a las file")
    adapter = _adapter(pdal)
    with pytest.raises(AdapterContractError):
        plan_invocation(adapter, _invocation(layout), adapter.probe(PLATFORM))


# -- execution through the generic engine (fake PDAL) ---------------------------------


def _execute(pdal, layout, monkeypatch, mode, workload="pixel_metrics_1a", threads=2, **opts):
    pytest.importorskip("rasterio")
    pytest.importorskip("laspy")
    fake_env(monkeypatch, mode, log=os.fspath(layout["attempt"].parent / "pdal-calls.jsonl"))
    _scene(layout)
    adapter = _adapter(pdal, **opts)
    inv = _invocation(layout, workload, threads=threads, epsg=EPSG)
    plan = plan_invocation(adapter, inv, availability=adapter.probe(PLATFORM))
    assert plan.ready, plan.reason
    execution = execute_plan(plan, inv, layout["attempt"], timeout_s=120, adapter=adapter)
    record = json.loads((layout["attempt"] / "execution.json").read_text(encoding="utf-8"))
    return execution, record, inv


def _calls(layout) -> list[list[str]]:
    path = layout["attempt"].parent / "pdal-calls.jsonl"
    return [json.loads(line)["args"] for line in path.read_text(encoding="utf-8").splitlines()]


@pytest.mark.platform
@needs_posix
@pytest.mark.parametrize("workload", BINMODE)
def test_execute_plan_and_validate_independently(pdal, layout, monkeypatch, workload):
    execution, record, inv = _execute(pdal, layout, monkeypatch, "ok", workload)
    assert str(execution.execution_status) == "succeeded", record["failure_reason"]
    interpretation = record["interpretation"]
    details = interpretation["details"]
    assert details["output_problems"] == [] and details["warning_lines"] == 0
    assert sorted(details["pipelines"]) == ["pipeline-0001.json", "pipeline-0002.json"]
    assert details["pipelines"]["pipeline-0001.json"]["pipeline"][-1]["binmode"] is True
    for tile in details["tiles"]:
        assert tile["exit_code"] == 0 and tile["metadata"]["present"]
        assert tile["metadata"]["points_read"] > 0 and len(tile["metadata"]["sha256"]) == 64
        assert tile["metadata"]["writers.gdal"]["filename"]
    timing = interpretation["tool_timing"]
    assert timing["boundary"].startswith("first pdal pipeline launch")
    assert timing["startup_overhead_ns"] >= 0
    stdout = (layout["attempt"] / "stdout.log").read_text(encoding="utf-8")
    assert "== pdal pipeline east tile (pipeline-0001.json): exit 0 ==" in stdout
    pipelines = [c for c in _calls(layout) if c[0] == "pipeline"]
    assert len(pipelines) == 2 and all(c[-1] == "--stream" for c in pipelines)

    from lidarbench.validation.pipeline import ValidationRequest, validate_outputs
    from lidarbench.validation.status import Comparability, Status

    report = validate_outputs(
        ValidationRequest(
            contract=SUITE.get(workload),
            tool="pdal",
            inputs=inv.inputs,
            output_dir=inv.output_dir,
            expected_epsg=EPSG,
        )
    )
    v = report.verdicts
    assert v.artifacts.status is Status.PASSED, v.artifacts.reasons
    assert v.structural.status is Status.PASSED, v.structural.reasons
    assert v.numerical.status is Status.PASSED, v.numerical.reasons
    # The fixture has points on horizontal cell edges: PDAL's upward-Y rule owns them
    # differently from the contract partition, so the timing is never "comparable".
    assert v.boundary_difference_points and v.boundary_difference_points > 0
    assert v.comparability()[0] is Comparability.DESCRIPTIVE_ONLY
    impl = report.to_record()["implementation"]
    assert impl["declared_comparability"] == "functionally_comparable"


def _ground_scene(layout) -> None:
    """Two 100 m tiles of ground (2) and water (9) points plus vegetation (1) to drop."""
    from validation_fixtures import X0, Y0

    for name, x0 in (("west tile", X0), ("east tile", X0 + 100.0)):
        xs, ys, zs, cs = [], [], [], []
        for i in range(21):
            for j in range(21):
                xs.append(x0 + 5.0 * i)
                ys.append(Y0 + 5.0 * j)
                zs.append(150.0 + 0.1 * i + 0.05 * j)
                cs.append(9 if i == 3 else 2)
                if i < 20 and j < 20:
                    xs.append(x0 + 5.0 * i + 1.0)
                    ys.append(Y0 + 5.0 * j + 1.0)
                    zs.append(170.0)
                    cs.append(1)
        write_las(layout["inputs"] / f"{name}.las", xs, ys, zs, classification=cs)


def test_dem_pipeline_is_a_cropped_class_filtered_tin(pdal, layout):
    _ground_scene(layout)
    adapter = _adapter(pdal)
    inv = _invocation(layout, "generate_DEM", epsg=EPSG)
    (_, _, east), (_, _, west) = adapter.pipelines(inv)
    stages = [s["type"] for s in west["pipeline"]]
    assert stages == [
        "readers.las",
        "readers.las",
        "filters.merge",
        "filters.crop",
        "filters.range",
        "filters.delaunay",
        "filters.faceraster",
        "writers.raster",
    ]
    assert all(s["override_srs"] == f"EPSG:{EPSG}" for s in west["pipeline"][:2])
    by_type = {s["type"]: s for s in west["pipeline"]}
    assert by_type["filters.range"]["limits"] == "Classification[2:2],Classification[9:9]"
    from validation_fixtures import X0, Y0

    assert by_type["filters.crop"]["bounds"] == (
        f"([{X0 - 20.0!r}, {X0 + 120.0!r}], [{Y0 - 20.0!r}, {Y0 + 120.0!r}])"
    )
    face = by_type["filters.faceraster"]
    assert (face["resolution"], face["origin_x"], face["width"], face["height"]) == (
        1.0,
        X0,
        100,
        100,
    )
    assert face["max_triangle_edge_length"] == 100.0 and face["nodata"] == NODATA
    assert by_type["writers.raster"]["data_type"] == "double"
    assert "binmode" not in json.dumps(east)
    meta = plan_invocation(adapter, inv, adapter.probe(PLATFORM)).metadata
    assert meta["writer"] == "writers.raster" and meta["binmode"] is None
    assert meta["execution_mode"] == "standard" and meta["configured_execution_mode"] == "stream"
    assert meta["classes"] == "2,9" and meta["max_triangle_edge_length"] == 100.0


@needs_posix
def test_dem_needs_the_tin_stages(pdal, layout, monkeypatch):
    fake_env(monkeypatch, drivers="notin")
    _ground_scene(layout)
    adapter = _adapter(pdal)
    plan = plan_invocation(adapter, _invocation(layout, "generate_DEM"), adapter.probe(PLATFORM))
    assert plan.status is PlanStatus.SKIPPED_UNSUPPORTED and "filters.delaunay" in plan.reason
    plan = plan_invocation(adapter, _invocation(layout, "generate_DSM1"), adapter.probe(PLATFORM))
    assert plan.ready, plan.reason


def test_dem_contract_is_structural_only():
    contract = SUITE.get("generate_DEM")
    impl = contract.implementation(PdalAdapter.contract_tool)
    assert impl is not None and impl.status is ImplementationStatus.RUNNABLE
    assert impl.artifacts == (WORKLOADS["generate_DEM"].artifact_pattern,)
    assert not contract.has_reference and contract.population.classes == (2, 9)
    assert contract.population.classes == WORKLOADS["generate_DEM"].classes
    assert contract.raster.resolution == WORKLOADS["generate_DEM"].resolution
    assert contract.buffer == WORKLOADS["generate_DEM"].buffer
    assert contract.declared_comparability("pdal") == "non_comparable"


@pytest.mark.platform
@needs_posix
@pytest.mark.parametrize("configured", ["stream", "prefer"])
def test_dem_runs_standard_and_validates_structurally(pdal, layout, monkeypatch, configured):
    pytest.importorskip("rasterio")
    pytest.importorskip("laspy")
    fake_env(monkeypatch, "ok", log=os.fspath(layout["attempt"].parent / "pdal-calls.jsonl"))
    _ground_scene(layout)
    adapter = _adapter(pdal, execution_mode=configured)
    inv = _invocation(layout, "generate_DEM", epsg=EPSG)
    plan = plan_invocation(adapter, inv, availability=adapter.probe(PLATFORM))
    assert plan.ready, plan.reason
    execution = execute_plan(plan, inv, layout["attempt"], timeout_s=120, adapter=adapter)
    record = json.loads((layout["attempt"] / "execution.json").read_text(encoding="utf-8"))
    assert str(execution.execution_status) == "succeeded", record["failure_reason"]
    details = record["interpretation"]["details"]
    assert details["execution_mode"] == "standard"
    for tile in details["tiles"]:
        assert tile["metadata"]["writers.raster"]["filename"]
    pipelines = [c for c in _calls(layout) if c[0] == "pipeline"]
    assert len(pipelines) == 2 and all(c[-1] == "--nostream" for c in pipelines)

    from lidarbench.validation.pipeline import ValidationRequest, validate_outputs
    from lidarbench.validation.status import Comparability, Outcome, Status

    report = validate_outputs(
        ValidationRequest(
            contract=SUITE.get("generate_DEM"),
            tool="pdal",
            inputs=inv.inputs,
            output_dir=inv.output_dir,
            expected_epsg=EPSG,
        )
    )
    v = report.verdicts
    assert v.artifacts.status is Status.PASSED, v.artifacts.reasons
    assert v.structural.status is Status.PASSED, v.structural.reasons
    assert v.numerical.status is Status.UNSUPPORTED
    assert "structural validation only" in v.numerical.reasons[0]
    assert v.outcome is Outcome.UNVERIFIED
    assert v.comparability()[0] is Comparability.DESCRIPTIVE_ONLY
    assert v.report_inclusion("performance-only") == (True, "unverified (unsupported)")
    assert v.report_inclusion("validated")[0] is False


@pytest.mark.platform
@needs_posix
@pytest.mark.parametrize(("mode", "flag"), [("standard", ["--nostream"]), ("prefer", [])])
def test_execution_mode_flags(pdal, layout, monkeypatch, mode, flag):
    execution, record, _ = _execute(
        pdal, layout, monkeypatch, "ok", "generate_DSM1", execution_mode=mode
    )
    assert str(execution.execution_status) == "succeeded", record["failure_reason"]
    for call in (c for c in _calls(layout) if c[0] == "pipeline"):
        assert call[2] == "--metadata" and call[4:] == flag


@pytest.mark.platform
@needs_posix
@pytest.mark.parametrize(
    ("mode", "threads", "phase", "text"),
    [
        ("fail", 2, "process", "Couldn't read LAS header"),
        ("failsecond", 1, "process", "west tile: pdal pipeline exited with status 1"),
        ("warning", 2, "interpretation", "warnings (not accepted)"),
        ("error0", 2, "interpretation", "errors despite exit status 0"),
        ("nooutput", 2, "interpretation", "missing east tile.tif"),
        ("nometadata", 2, "interpretation", "--metadata"),
    ],
)
def test_execute_plan_never_hides_pdal_problems(
    pdal, layout, monkeypatch, mode, threads, phase, text
):
    execution, record, _ = _execute(pdal, layout, monkeypatch, mode, threads=threads)
    assert str(execution.execution_status) == "failed"
    assert record["failure_phase"] == phase
    assert text in record["failure_reason"]


@pytest.mark.platform
@needs_posix
def test_first_failure_stops_further_pipelines(pdal, layout, monkeypatch):
    _scene(layout)
    write_las(
        layout["inputs"] / "zz north.las", [500010.5, 500020.5], [5000300.5, 5000310.5], [1.0, 2.0]
    )
    fake_env(monkeypatch, "failsecond")
    adapter = _adapter(pdal)
    inv = _invocation(layout, threads=1)
    plan = plan_invocation(adapter, inv, availability=adapter.probe(PLATFORM))
    execution = execute_plan(plan, inv, layout["attempt"], timeout_s=120, adapter=adapter)
    assert str(execution.execution_status) == "failed"
    record = json.loads((layout["attempt"] / "execution.json").read_text(encoding="utf-8"))
    assert "zz north: not run (stopped after an earlier failure)" in record["failure_reason"]
    assert record["interpretation"]["tool_timing"] is None


# -- launcher -------------------------------------------------------------------------


def test_worker_rejects_bad_jobs(tmp_path, capsys):
    job = tmp_path / "job.json"
    for content in ("nope", json.dumps({"schema": "x"})):
        job.write_text(content, encoding="utf-8")
        assert worker.main(["--job", os.fspath(job)]) == 2
    good = {
        "schema": worker.JOB_SCHEMA,
        "pdal": os.fspath(tmp_path.absolute() / "pdal"),
        "execution_mode": "stream",
        "concurrency": 1,
        "tiles": [{"stem": "a", "pipeline": "p.json", "metadata": "m.json", "log": "a.log"}],
        "result": RUN_FILE,
    }
    for key, value in [
        ("concurrency", 0),
        ("tiles", []),
        ("pdal", "pdal"),
        ("execution_mode", "x"),
    ]:
        job.write_text(json.dumps({**good, key: value}), encoding="utf-8")
        assert worker.main(["--job", os.fspath(job)]) == 2
    assert "ERROR:" in capsys.readouterr().err


def test_worker_records_a_missing_pdal(tmp_path):
    job = {
        "schema": worker.JOB_SCHEMA,
        "pdal": os.fspath(tmp_path.absolute() / "missing pdal"),
        "execution_mode": "standard",
        "concurrency": 2,
        "tiles": [
            {"stem": s, "pipeline": f"{s}.json", "metadata": f"{s}.m.json", "log": f"{s}.log"}
            for s in ("a", "b")
        ],
        "result": RUN_FILE,
    }
    path = tmp_path / JOB_FILE
    path.write_text(json.dumps(job), encoding="utf-8")
    assert worker.main(["--job", os.fspath(path)]) == 1
    run = json.loads((tmp_path / RUN_FILE).read_text(encoding="utf-8"))
    assert run["schema"] == worker.RUN_SCHEMA and run["succeeded"] is False
    assert any("cannot start pdal" in (t["error"] or "") for t in run["tiles"])
    assert all(t["argv"][-1] == "--nostream" for t in run["tiles"])


# -- campaign -------------------------------------------------------------------------


@pytest.mark.platform
@needs_posix
def test_campaign_runs_pdal_through_the_common_schema(tmp_path, pdal, monkeypatch):
    pytest.importorskip("rasterio")
    pytest.importorskip("laspy")
    fake_env(monkeypatch)
    base = tmp_path / "bench root"
    for name in ("scratch", "state"):
        (base / name).mkdir(parents=True)
    neighbour_scene(base / "data")
    config_path = write_config(
        base,
        workloads=(
            f'[[workloads]]\nid = "pixel_metrics_1a"\nparameters = {{ epsg = {EPSG} }}\n'
            '[[workloads]]\nid = "generate_DSM1"\n'
        ),
        tools=f'[[tools]]\nid = "pdal"\noptions = {{ executable = "{pdal}" }}\n',
        cpu_counts="[1, 2]",
        scheduling="repetitions = 1\nwarmups = 0\ncooldown_s = 0",
    )
    config_path.write_text(
        config_path.read_text(encoding="utf-8").replace('"*.laz"', '"*.las"'), encoding="utf-8"
    )
    registry = AdapterRegistry()
    registry.register(PdalAdapter)
    summary = CampaignRunner(
        load_config(config_path),
        options=RunOptions(),
        registry=registry,
        state_dir=base / "state",
        git_info=lambda: {"available": False, "reason": "test"},
        report=lambda line: None,
        handle_signals=False,
    ).run()
    assert summary.statuses == {"succeeded": 4}, summary.to_record()
    for row in collect_rows(summary.campaign_path):
        assert row["tool_adapter"] == "pdal" and row["tool_version"] == "2.6.3"
        assert row["timing_boundary"] == "process" and row["elapsed_ns"] > 0
        assert row["threading_model"] == "concurrent_files"
        assert row["missing_required_outputs"] == 0
    invocations = [
        json.loads(p.read_text(encoding="utf-8"))
        for p in (summary.campaign_path / "attempts").glob("*/invocation.json")
    ]
    for inv in invocations:
        names = [c["name"] for c in inv["command"]["config_files"]]
        assert names == ["pipeline-0001.json", "pipeline-0002.json", JOB_FILE]
        assert inv["plan"]["metadata"]["binmode"] is True


# -- COPC inputs ------------------------------------------------------------------------


def _copc_scene(layout) -> None:
    _scene(layout)
    for tile in layout["inputs"].glob("*.las"):
        mark_copc(tile)


@pytest.mark.parametrize("workload", sorted(WORKLOADS))
def test_copc_inputs_query_only_the_buffer_of_neighbours(pdal, layout, workload):
    _copc_scene(layout)
    adapter = _adapter(pdal)
    inv = _invocation(layout, workload, epsg=EPSG)
    bounds = adapter.header_bounds(inv)
    assert adapter.copc_inputs(inv) == frozenset(inv.inputs)
    for tile, _, doc in adapter.pipelines(inv):
        readers = [s for s in doc["pipeline"] if s["type"].startswith("readers.")]
        assert {r["type"] for r in readers} == {"readers.copc"}
        assert all(r["requests"] == 1 and "threads" not in r for r in readers)
        assert "bounds" not in readers[0]  # the tile itself is read whole
        min_x, min_y, max_x, max_y = bounds[tile]
        box = f"([{min_x - 20.0!r}, {max_x + 20.0!r}], [{min_y - 20.0!r}, {max_y + 20.0!r}])"
        assert len(readers) == 2 and readers[1]["bounds"] == box
    meta = plan_invocation(adapter, inv, adapter.probe(PLATFORM)).metadata
    assert meta["reader"].startswith("readers.copc") and meta["neighbour_reads"] == 2


@needs_posix
def test_copc_inputs_need_readers_copc(pdal, layout, monkeypatch):
    fake_env(monkeypatch, drivers="nocopc")
    _copc_scene(layout)
    adapter = _adapter(pdal)
    plan = plan_invocation(adapter, _invocation(layout, "generate_DSM1"), adapter.probe(PLATFORM))
    assert plan.status is PlanStatus.SKIPPED_UNSUPPORTED and "readers.copc" in plan.reason


@pytest.mark.platform
@needs_posix
@pytest.mark.parametrize("workload", BINMODE)
def test_copc_buffer_queries_keep_the_validated_results(pdal, layout, monkeypatch, workload):
    pytest.importorskip("rasterio")
    pytest.importorskip("laspy")
    fake_env(monkeypatch, "ok")
    _copc_scene(layout)
    adapter = _adapter(pdal)
    inv = _invocation(layout, workload, epsg=EPSG)
    plan = plan_invocation(adapter, inv, availability=adapter.probe(PLATFORM))
    assert plan.ready, plan.reason
    execution = execute_plan(plan, inv, layout["attempt"], timeout_s=120, adapter=adapter)
    assert str(execution.execution_status) == "succeeded"

    from lidarbench.validation.pipeline import ValidationRequest, validate_outputs
    from lidarbench.validation.status import Status

    report = validate_outputs(
        ValidationRequest(
            contract=SUITE.get(workload),
            tool="pdal",
            inputs=inv.inputs,
            output_dir=inv.output_dir,
            expected_epsg=EPSG,
        )
    )
    assert report.verdicts.numerical.status is Status.PASSED, report.verdicts.numerical.reasons
