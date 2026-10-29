"""Integration: real lidR/lasR through the R bridge, the generic execution API and a campaign.

Skipped unless Rscript, the renv-pinned packages and the optional validation
dependencies are installed. Inputs are the synthetic two-tile scene of
``validation_fixtures`` (neighbouring-tile buffer, points on cell edges, empty
cells). Outputs are validated with the independent Python references; the
expected verdicts are the documented ones (WORKLOADS.md, design/VALIDATION.md):

* lidR matches its own buffered-clamp convention, which differs from the
  contract partition on boundary points: validated, but descriptive only;
* lasR follows the contract partition but writes one extra cell on the east and
  north edges: strict structural validation fails, the explicit extent policy
  gives ``accepted_exception``.

Both tools executing successfully is therefore *not* evidence that they are
equivalent; neither result is comparable.
"""

from __future__ import annotations

import json
import os
import shutil
import subprocess
from pathlib import Path
from types import MappingProxyType

import pytest

pytest.importorskip("numpy")
pytest.importorskip("rasterio")
pytest.importorskip("laspy")

from validation_fixtures import EPSG, neighbour_scene

from lidarbench.campaign.results import collect_rows
from lidarbench.campaign.runner import CampaignRunner, RunOptions
from lidarbench.config.campaign import load_config
from lidarbench.core.platform import get_platform_info
from lidarbench.execution.adapters import Invocation
from lidarbench.execution.adapters.lasr import LasRAdapter
from lidarbench.execution.adapters.lidr import LidRAdapter
from lidarbench.execution.adapters.registry import AdapterRegistry
from lidarbench.execution.adapters.rscript import RWorkload
from lidarbench.execution.attempt import execute_plan
from lidarbench.execution.plan import plan_invocation
from lidarbench.validation import (
    Comparability,
    Outcome,
    Policies,
    Status,
    ValidationRequest,
    validate_outputs,
)
from lidarbench.workloads import default_suite_dir, load_suite

pytestmark = [pytest.mark.integration, pytest.mark.slow, pytest.mark.timeout(1800)]

REPO = Path(__file__).resolve().parents[2]
PLATFORM = get_platform_info()
SUITE = load_suite(default_suite_dir())
WORKLOADS = ("pixel_metrics_1a", "generate_DSM1")


@pytest.fixture(scope="module")
def adapters():
    probed = {}
    for cls in (LidRAdapter, LasRAdapter):
        adapter = cls()
        availability = adapter.probe(PLATFORM)
        if not availability.available:
            pytest.skip(f"{cls.adapter_id} unavailable: {availability.detail}")
        probed[cls.adapter_id] = (adapter, availability)
    return probed


def _write_lax(rscript: Path, files: list[Path]) -> None:
    """Index the fixture tiles once, before they become a protected dataset."""
    code = "source('renv/activate.R'); lasR::exec(lasR::write_lax(), on = commandArgs(TRUE))"
    done = subprocess.run(
        [os.fspath(rscript), "--vanilla", "-e", code, *map(os.fspath, files)],
        cwd=REPO,
        capture_output=True,
        text=True,
        timeout=600,
        check=False,
    )
    assert done.returncode == 0, done.stderr[-2000:]


@pytest.fixture(scope="module")
def indexed_scene(adapters, tmp_path_factory):
    root = tmp_path_factory.mktemp("r adapters")
    inputs = root / "lidar inputs"
    neighbour_scene(inputs)
    tiles = sorted(inputs.glob("*.las"))
    _write_lax(adapters["lasr"][1].executables["rscript"], tiles)
    assert all(t.with_suffix(".lax").stat().st_size > 0 for t in tiles)
    return root, inputs


@pytest.fixture(scope="module")
def campaign(adapters, indexed_scene):
    root, inputs = indexed_scene
    (root / "scratch").mkdir()
    (root / "state").mkdir()
    workloads = "".join(
        f'[[workloads]]\nid = "{w}"\nparameters = {{ epsg = {EPSG} }}\n' for w in WORKLOADS
    )
    config = root / "campaign.toml"
    config.write_text(
        "\n".join(
            [
                'schema = "lidarbench.campaign-config/1"',
                'campaign_id = "r-adapters-integration"',
                'workspace = "workspace"',
                "cpu_counts = [2]",
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
                '[[tools]]\nid = "lidr"\n[[tools]]\nid = "lasr"\n',
            ]
        ),
        encoding="utf-8",
    )
    registry = AdapterRegistry()
    registry.register(LidRAdapter)
    registry.register(LasRAdapter)
    before = sorted(p.name for p in inputs.iterdir())
    summary = CampaignRunner(
        load_config(config),
        options=RunOptions(),
        registry=registry,
        state_dir=root / "state",
        git_info=lambda: {"available": False, "reason": "integration test"},
        report=lambda line: None,
        handle_signals=False,
    ).run()
    assert sorted(p.name for p in inputs.iterdir()) == before  # inputs untouched
    return summary, inputs


def _attempts(summary):
    out = []
    for path in sorted((summary.campaign_path / "attempts").iterdir()):
        result = json.loads((path / "result.json").read_text(encoding="utf-8"))
        execution = json.loads((path / "execution.json").read_text(encoding="utf-8"))
        invocation = json.loads((path / "invocation.json").read_text(encoding="utf-8"))
        out.append((result, execution, invocation))
    return out


def test_campaign_runs_both_tools_with_common_schema(campaign, adapters):
    summary, _ = campaign
    assert summary.statuses == {"succeeded": 4}, summary.to_record()
    rows = collect_rows(summary.campaign_path)
    assert {(r["tool"], r["workload_id"]) for r in rows} == {
        (t, w) for t in ("lidr", "lasr") for w in WORKLOADS
    }
    for row in rows:
        assert row["execution_status"] == "succeeded" and row["exit_code"] == 0
        assert row["timing_boundary"] == "process"
        # Validation runs in the runner: lidR passes structure (performance-only mode, so
        # numerical stays unverified); lasR's known one-cell outward extent fails the strict
        # structural level because this campaign does not enable the extent policy.
        if row["tool"] == "lidr":
            assert (row["structural_status"], row["numerical_status"]) == ("passed", "unverified")
        else:
            assert (row["structural_status"], row["numerical_status"]) == ("failed", "not_run")
        assert row["output_files"] >= 2 and row["missing_required_outputs"] == 0
        assert 0 < row["tool_workload_elapsed_ns"] < row["elapsed_ns"]
        assert 0 < row["startup_overhead_ns"] < row["elapsed_ns"]
        identity = adapters[row["tool_adapter"]][0].identify()
        assert row["tool_version"] == identity.version


def test_bridge_records_versions_threads_and_inner_timing(campaign, adapters):
    summary, _ = campaign
    for result, execution, _ in _attempts(summary):
        adapter = adapters[result["tool_adapter"]][0]
        interpretation = execution["interpretation"]
        assert interpretation["failure_reason"] is None
        record = interpretation["details"]["bridge_record"]
        assert record["status"] == "succeeded" and record["warnings"] == []
        assert record["legacy"]["call"] == adapter.workloads[result["workload_id"]].call
        locked = adapter.identify().details
        for name in adapter.required_packages:
            assert record["packages"][name] == locked[f"locked.{name}"]
        assert record["loaded_namespaces"][adapter.r_package] == adapter.identify().version
        assert record["catalog"]["buffer"] == 20 and record["catalog"]["crs"] == f"EPSG:{EPSG}"
        threads = record["threads"]
        if adapter.r_package == "lidR":
            assert threads["future_plan"] == "multisession" and threads["future_workers"] == 2
            assert threads["lidr_threads"] == 1
            assert threads["environment"]["OMP_NUM_THREADS"] == "1"
        else:
            assert threads["lasr_strategy"] == "concurrent-files"
            assert threads["lasr_ncores"] == [2]
            assert threads["environment"]["OMP_NUM_THREADS"] == "2"
        timing = result["tool_timing"]
        assert timing["within_outer_interval"] is True
        assert timing["marks"]["bridge_start"] <= timing["start_wall_ns"]
        assert timing["startup_overhead_ns"] > timing["marks"]["packages_loaded"] - (
            timing["marks"]["bridge_start"]
        )
        # Lazily loaded namespaces (sf/terra: GDAL, PROJ, GEOS) stay outside the
        # inner interval.
        marks = timing["marks"]
        assert marks["packages_loaded"] <= marks["namespaces_preloaded"]
        assert marks["namespaces_preloaded"] <= timing["start_wall_ns"]
        preloaded = record["preloaded_namespaces"]
        for name in ("sf", "terra"):
            assert preloaded[name] == record["loaded_namespaces"][name]
        assert not {"sf", "terra"} & set(record["workload_loaded_namespaces"])
        assert "sf, terra" in timing["boundary"]


def _validate(result, invocation, policies=None):
    contract = SUITE.get(result["workload_id"])
    tool = {"lidr": "lidR", "lasr": "lasR"}[result["tool_adapter"]]
    return validate_outputs(
        ValidationRequest(
            contract=contract,
            tool=tool,
            inputs=tuple(Path(p) for p in invocation["inputs"]),
            output_dir=Path(invocation["output_dir"]),
            execution_status=result["execution_status"],
            expected_epsg=EPSG,
            policies=policies or Policies(),
        )
    )


def test_outputs_against_independent_validator(campaign):
    """Real outputs reproduce the documented per-tool verdicts; none is comparable."""
    summary, _ = campaign
    comparabilities = []
    for result, _, invocation in _attempts(summary):
        if result["tool_adapter"] == "lidr":
            verdicts = _validate(result, invocation).verdicts
            assert verdicts.outcome is Outcome.VALIDATED, verdicts.to_record()
            assert verdicts.boundary_difference_points and verdicts.boundary_difference_points > 0
        else:
            strict = _validate(result, invocation).verdicts
            assert strict.structural.status is Status.FAILED
            assert all(
                "outward cells W/E/S/N = [0, 1, 0, 1]" in r for r in strict.structural.reasons
            )
            verdicts = _validate(
                result, invocation, Policies(accept_known_extent_discrepancies=True)
            ).verdicts
            assert verdicts.numerical.status is Status.ACCEPTED_EXCEPTION, verdicts.to_record()
            assert verdicts.boundary_difference_points == 0
            assert not verdicts.validated_performance
        comparabilities.append(verdicts.comparability()[0])
    assert comparabilities and all(c is Comparability.DESCRIPTIVE_ONLY for c in comparabilities)


def _run(adapter, inputs: list[Path], root: Path, workload="pixel_metrics_1a"):
    root.mkdir()
    out, work, attempt = root / "out", root / "work", root / "attempt"
    for path in (out, work, attempt):
        path.mkdir()
    inv = Invocation(workload, tuple(inputs), out, work, threads=1)
    plan = plan_invocation(adapter, inv)
    assert plan.ready, plan.reason
    execute_plan(plan, inv, attempt, timeout_s=900, adapter=adapter)
    return json.loads((attempt / "execution.json").read_text(encoding="utf-8"))


def test_lasr_refuses_unindexed_inputs_without_touching_them(adapters, tmp_path):
    inputs = tmp_path / "unindexed tiles"
    neighbour_scene(inputs)
    before = sorted(p.name for p in inputs.iterdir())
    record = _run(adapters["lasr"][0], sorted(inputs.glob("*.las")), tmp_path / "run")
    assert record["execution_status"] == "failed" and record["failure_phase"] == "process"
    assert "LAX" in record["failure_reason"]
    assert record["process"]["exit_code"] == 1
    assert sorted(p.name for p in inputs.iterdir()) == before


class _DriftedLidR(LidRAdapter):
    """Pins a call that R/0_setup.R does not contain."""

    workloads = MappingProxyType(
        {
            "pixel_metrics_1a": RWorkload(
                "pixel_metrics_1a",
                call=LidRAdapter.workloads["pixel_metrics_1a"].call.replace("res = 20", "res = 10"),
                buffer=20.0,
            )
        }
    )


def test_definition_mismatch_is_refused(indexed_scene, adapters, tmp_path):
    _, inputs = indexed_scene
    adapter = _DriftedLidR()
    assert adapter.probe(PLATFORM).available
    record = _run(adapter, sorted(inputs.glob("*.las")), tmp_path / "run")
    assert record["execution_status"] == "failed"
    assert "differs from the definition pinned by the adapter" in record["failure_reason"]
    assert record["process"]["exit_code"] == 2
    assert not list((tmp_path / "run" / "out").iterdir())


@pytest.mark.parametrize(
    ("prefix", "exit_code", "text"),
    [
        ("warning('synthetic workload warning'); ", 3, "R warning during the workload"),
        ("stop('synthetic workload error'); ", 1, "synthetic workload error"),
    ],
)
def test_real_r_warnings_and_errors_fail(
    adapters, indexed_scene, tmp_path, monkeypatch, prefix, exit_code, text
):
    """Exercise the R-side condition handling with a patched copy of the legacy project.

    The copy runs without renv (its path would select another renv library), so
    the probed renv library paths are passed through ``R_LIBS``.
    """
    _, inputs = indexed_scene
    lidr = adapters["lidr"][0]
    project = tmp_path / "patched project"
    shutil.copytree(REPO / "bridges", project / "bridges")
    (project / "R").mkdir()
    shutil.copy2(REPO / "R" / "1_functions.R", project / "R" / "1_functions.R")
    original = LidRAdapter.workloads["pixel_metrics_1a"].call
    patched = "{ " + prefix + original + " }"
    setup = (REPO / "R" / "0_setup.R").read_text(encoding="utf-8")
    assert setup.count(f'"{original}"') == 1
    (project / "R" / "0_setup.R").write_text(
        setup.replace(f'"{original}"', f'"{patched}"'), encoding="utf-8"
    )
    monkeypatch.setenv("R_LIBS", os.pathsep.join(lidr._probe_record["library_paths"]))

    class Patched(LidRAdapter):
        workloads = MappingProxyType(
            {"pixel_metrics_1a": RWorkload("pixel_metrics_1a", patched, 20.0)}
        )

    adapter = Patched(project=str(project), renv=False)
    assert adapter.probe(PLATFORM).available
    record = _run(adapter, sorted(inputs.glob("*.las")), tmp_path / "run")
    assert record["execution_status"] == "failed"
    assert record["process"]["exit_code"] == exit_code
    assert text in record["failure_reason"]
    bridge = record["interpretation"]["details"]["bridge_record"]
    assert bridge["phase"] == "workload"
    if exit_code == 3:
        assert bridge["warnings"] == [
            {"phase": "workload", "message": "synthetic workload warning"}
        ]
        assert "R warning (workload): synthetic workload warning" in record["warnings"]
