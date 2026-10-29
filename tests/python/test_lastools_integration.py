"""Integration: real LAStools through the generic execution engine and a campaign.

Skipped unless a LAStools installation is found (``LIDARBENCH_LASTOOLS_BIN`` or
``PATH``) with usable ``lascanopy64`` and ``lasgrid64`` binaries. Nothing is
downloaded or installed, and licensing is never touched: an unlicensed run fails
(``-fail``), it is not worked around.

Inputs are the synthetic two-tile scene of ``validation_fixtures`` in a
directory with spaces. The documented native behaviour (WORKLOADS.md) is
checked: single-core ``lascanopy`` exits 1 on the deliberate all-point cutoff,
multi-core parents may exit 0 while printing the same warning, and both are
accepted only under the explicit opt-in, as a named exception. Successful
execution is not validation: native cell ownership is undeclared, so outputs
are at best ``unverified`` and never comparable with other tools.
"""

from __future__ import annotations

import json
import shutil
from pathlib import Path

import pytest
from validation_fixtures import EPSG, neighbour_scene

from lidarbench.campaign.results import collect_rows
from lidarbench.campaign.runner import CampaignRunner, RunOptions
from lidarbench.config.campaign import load_config
from lidarbench.core.platform import get_platform_info
from lidarbench.execution.adapters import Invocation
from lidarbench.execution.adapters.lastools import CUTOFF_EXCEPTION, LastoolsAdapter
from lidarbench.execution.adapters.registry import AdapterRegistry
from lidarbench.execution.attempt import execute_plan
from lidarbench.execution.plan import plan_invocation

pytestmark = [pytest.mark.integration, pytest.mark.slow, pytest.mark.timeout(1800)]

PLATFORM = get_platform_info()
WORKLOADS = ("pixel_metrics_1a", "generate_DSM1")


@pytest.fixture(scope="module")
def installed():
    adapter = LastoolsAdapter()
    availability = adapter.probe(PLATFORM)
    if not availability.available:
        pytest.skip(f"LAStools unavailable: {availability.detail}")
    missing = [w for w in WORKLOADS if adapter.support_for(w, 2).status.value != "runnable"]
    if missing:
        pytest.skip(f"LAStools installation lacks commands for {missing}")
    return adapter, availability


@pytest.fixture(scope="module")
def scene(tmp_path_factory):
    root = tmp_path_factory.mktemp("lastools integration")
    inputs = root / "lidar inputs"
    neighbour_scene(inputs)
    return root, inputs


def _campaign(installed, scene, campaign_id: str, opt_in: bool):
    adapter, _ = installed
    root, inputs = scene
    base = root / campaign_id
    (base / "scratch").mkdir(parents=True)
    (base / "state").mkdir()
    bin_dir = adapter.identify().details["bin_dir"]
    workloads = "".join(
        f'[[workloads]]\nid = "{w}"\nparameters = {{ epsg = {EPSG} }}\n' for w in WORKLOADS
    )
    options = (
        f'options = {{ bin_dir = "{Path(str(bin_dir)).as_posix()}", '
        f"accept_known_cutoff_warning = {str(opt_in).lower()} }}"
    )
    config = base / "campaign.toml"
    config.write_text(
        "\n".join(
            [
                'schema = "lidarbench.campaign-config/1"',
                f'campaign_id = "{campaign_id}"',
                'workspace = "workspace"',
                "cpu_counts = [1, 2]",
                'validation_mode = "performance-only"',
                "timeout_s = 900.0",
                "[scheduling]",
                "repetitions = 1",
                "warmups = 0",
                "cooldown_s = 0",
                "[monitoring]",
                "enabled = true",
                "required = false",
                "[scratch]",
                'roots = { local = "scratch" }',
                'retain = "always"',
                "[[datasets]]",
                'id = "scene"',
                f'path = "{inputs.as_posix()}"',
                'include = ["*.las"]',
                workloads,
                f'[[tools]]\nid = "lastools"\n{options}\n',
            ]
        ),
        encoding="utf-8",
    )
    registry = AdapterRegistry()
    registry.register(LastoolsAdapter)
    before = sorted(p.name for p in inputs.iterdir())
    summary = CampaignRunner(
        load_config(config),
        options=RunOptions(),
        registry=registry,
        state_dir=base / "state",
        git_info=lambda: {"available": False, "reason": "integration test"},
        report=lambda line: None,
        handle_signals=False,
    ).run()
    assert sorted(p.name for p in inputs.iterdir()) == before  # inputs untouched
    return summary


def _attempts(summary):
    out = []
    for path in sorted((summary.campaign_path / "attempts").iterdir()):
        result = json.loads((path / "result.json").read_text(encoding="utf-8"))
        invocation = json.loads((path / "invocation.json").read_text(encoding="utf-8"))
        out.append((result, invocation))
    return out


@pytest.fixture(scope="module")
def opted_in(installed, scene):
    return _campaign(installed, scene, "lastools-opt-in", opt_in=True)


def test_campaign_measures_lastools_through_the_generic_engine(opted_in, installed):
    adapter, _ = installed
    assert opted_in.statuses == {"succeeded": 4}, opted_in.to_record()
    rows = collect_rows(opted_in.campaign_path)
    assert {(r["workload_id"], r["cpus"]) for r in rows} == {
        (w, c) for w in WORKLOADS for c in (1, 2)
    }
    for row in rows:
        assert row["tool_version"] == adapter.identify().version
        assert row["timing_boundary"] == "process" and row["elapsed_ns"] > 0
        assert row["threading_model"] == "native_threads"
        assert row["missing_required_outputs"] == 0 and row["numerical_status"] == "unverified"
        if row["workload_id"] == "pixel_metrics_1a":
            assert row["accepted_exception"] == CUTOFF_EXCEPTION
            assert row["exit_code"] in (0, 1)
            if row["cpus"] == 1:
                assert row["exit_code"] == 1  # documented single-core cutoff status
        else:
            assert row["exit_code"] == 0 and row["accepted_exception"] is None


def test_argv_and_identity_records(opted_in):
    for result, invocation in _attempts(opted_in):
        argv = invocation["command"]["argv"]
        assert Path(argv[0]).stem in ("lascanopy64", "lasgrid64")
        assert argv[argv.index("-cores") + 1] == str(result["entry"]["cpus"])
        assert argv[-3:] == ["-epsg", str(EPSG), "-fail"]
        assert all(p in argv for p in invocation["inputs"])
    tools = (opted_in.campaign_path / "identity" / "tools.json").read_text(encoding="utf-8")
    assert "sha256" in tools
    for text in ("licensed to", "non-profit", "unlicensed", "(academic)", "(commercial)"):
        assert text not in tools


def test_cutoff_warning_fails_without_the_opt_in(installed, scene):
    summary = _campaign(installed, scene, "lastools-default", opt_in=False)
    by_workload: dict[str, set[str]] = {}
    for row in collect_rows(summary.campaign_path):
        by_workload.setdefault(row["workload_id"], set()).add(row["execution_status"])
        assert row["accepted_exception"] is None
    assert by_workload == {"pixel_metrics_1a": {"failed"}, "generate_DSM1": {"succeeded"}}


def test_tool_errors_fail_the_attempt(installed, tmp_path):
    adapter, availability = installed
    dirs = [tmp_path / n for n in ("in puts", "out", "work", "attempt")]
    for path in dirs:
        path.mkdir()
    garbage = dirs[0] / "not a point cloud.las"
    garbage.write_bytes(b"this is not LAS data at all" * 10)
    inv = Invocation("generate_DSM1", (garbage,), dirs[1], dirs[2], threads=1)
    plan = plan_invocation(adapter, inv, availability=availability)
    assert plan.ready, plan.reason
    execution = execute_plan(plan, inv, dirs[3], timeout_s=300, adapter=adapter)
    assert str(execution.execution_status) == "failed"
    assert execution.failure_reason and execution.accepted_exception is None


def test_execution_is_not_validation_and_never_comparable(opted_in, tmp_path):
    """Native outputs are never validated or comparable; see WORKLOADS.md."""
    pytest.importorskip("numpy")
    pytest.importorskip("rasterio")
    pytest.importorskip("laspy")
    from lidarbench.validation import Comparability, Outcome, ValidationRequest, validate_outputs
    from lidarbench.workloads import default_suite_dir, load_suite

    suite = load_suite(default_suite_dir())
    for result, invocation in _attempts(opted_in):
        contract = suite.get(result["workload_id"])
        output_dir = Path(invocation["output_dir"])
        request = {
            "contract": contract,
            "tool": "lastools",
            "inputs": tuple(Path(p) for p in invocation["inputs"]),
            "execution_status": result["execution_status"],
            "expected_epsg": EPSG,
        }
        verdicts = validate_outputs(ValidationRequest(output_dir=output_dir, **request)).verdicts
        assert verdicts.outcome is not Outcome.VALIDATED
        assert verdicts.comparability()[0] is not Comparability.COMPARABLE
        # LAStools writes .tfw world files, which workloads-v1 does not list as sidecars.
        primaries = tmp_path / result["attempt_id"]
        primaries.mkdir()
        for tif in output_dir.glob("*.tif"):
            shutil.copy2(tif, primaries / tif.name)
        primary = validate_outputs(ValidationRequest(output_dir=primaries, **request)).verdicts
        assert primary.artifacts.status.value == "passed", primary.to_record()
        assert primary.outcome is not Outcome.VALIDATED
        assert primary.comparability()[0] is not Comparability.COMPARABLE
