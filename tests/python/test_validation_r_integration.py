"""Integration: validate real pinned lidR/lasR outputs on the synthetic fixtures.

Runs ``tests/python/fixtures/run_r_workloads.R`` (the exact calls of
``R/0_setup.R`` with the harness's ``prepare_input``) outside any timing, then
validates the persisted GeoTIFFs with the independent Python references. Skipped
when Rscript or the pinned R packages are unavailable.

Expected outcomes are the ones WORKLOADS.md documents for these tools:

* lidR agrees exactly with its own (outer-edge clamping) cell convention, which
  differs from the contract partition on boundary points: validated, but only
  descriptive (``boundary_difference_points > 0``);
* lasR follows the contract partition but writes rasters one cell larger on the
  east/north edges: rejected by strict validation, ``accepted_exception`` only
  under the explicit ``accept_known_extent_discrepancies`` policy.
"""

from __future__ import annotations

import shutil
import subprocess
from pathlib import Path

import pytest

pytest.importorskip("numpy")
pytest.importorskip("rasterio")
pytest.importorskip("laspy")

from validation_fixtures import EPSG, neighbour_scene

from lidarbench.validation import (
    Comparability,
    Outcome,
    Policies,
    Status,
    ValidationRequest,
    validate_outputs,
)
from lidarbench.workloads import default_suite_dir, load_suite

pytestmark = [pytest.mark.integration, pytest.mark.slow, pytest.mark.timeout(900)]

REPO = Path(__file__).resolve().parents[2]
DRIVER = Path(__file__).resolve().parent / "fixtures" / "run_r_workloads.R"
SUITE = load_suite(default_suite_dir())


def _r_available() -> str | None:
    rscript = shutil.which("Rscript")
    if rscript is None:
        return "Rscript not found"
    probe = subprocess.run(
        [rscript, "-e", 'source("renv/activate.R"); library(lidR); library(lasR)'],
        cwd=REPO,
        capture_output=True,
        text=True,
        timeout=300,
        check=False,
    )
    if probe.returncode != 0:
        return f"pinned lidR/lasR unavailable: {probe.stderr[-300:]}"
    return None


@pytest.fixture(scope="module")
def r_outputs(tmp_path_factory):
    reason = _r_available()
    if reason:
        pytest.skip(reason)
    root = tmp_path_factory.mktemp("r integration")
    inputs, outputs = root / "inputs", root / "outputs"
    neighbour_scene(inputs)
    outputs.mkdir()
    done = subprocess.run(
        ["Rscript", str(DRIVER), str(REPO), str(inputs), str(outputs), str(EPSG)],
        capture_output=True,
        text=True,
        timeout=900,
        check=False,
    )
    assert done.returncode == 0, done.stderr[-2000:]
    status = dict(
        line.split("\t", 1)
        for line in (outputs / "status.tsv").read_text(encoding="utf-8").splitlines()
    )
    return inputs, outputs, status


def _validate(r_outputs, workload: str, tool: str, policies: Policies | None = None):
    inputs, outputs, status = r_outputs
    key = f"{workload}__{tool}"
    execution = "succeeded" if status[key] == "succeeded" else "failed"
    request = ValidationRequest(
        contract=SUITE.get(workload),
        tool=tool,
        inputs=tuple(sorted(inputs.glob("*.las"))),
        output_dir=outputs / key,
        execution_status=execution,
        expected_epsg=EPSG,
        policies=policies or Policies(),
    )
    return validate_outputs(request)


@pytest.mark.parametrize("workload", ["pixel_metrics_1a", "generate_DSM1"])
def test_lidr_matches_its_convention_but_is_descriptive(r_outputs, workload):
    report = _validate(r_outputs, workload, "lidR")
    v = report.verdicts
    assert v.execution_status == "succeeded"
    assert v.outcome is Outcome.VALIDATED, v.to_record()
    assert report.to_record()["inventory"]["ignored_sidecars"]  # lidR's VRT mosaic
    assert v.boundary_difference_points and v.boundary_difference_points > 0
    assert v.comparability()[0] is Comparability.DESCRIPTIVE_ONLY


@pytest.mark.parametrize("workload", ["pixel_metrics_1a", "generate_DSM1"])
def test_lasr_extent_needs_explicit_policy(r_outputs, workload):
    strict = _validate(r_outputs, workload, "lasR").verdicts
    assert strict.structural.status is Status.FAILED
    assert all("outward cells W/E/S/N = [0, 1, 0, 1]" in r for r in strict.structural.reasons)
    accepted = _validate(
        r_outputs, workload, "lasR", Policies(accept_known_extent_discrepancies=True)
    ).verdicts
    assert accepted.numerical.status is Status.ACCEPTED_EXCEPTION, accepted.to_record()
    assert accepted.boundary_difference_points == 0
    assert not accepted.validated_performance
    assert accepted.comparability()[0] is Comparability.DESCRIPTIVE_ONLY


def test_lidr_output_is_not_the_contract_partition(r_outputs):
    """The convention discrepancy is measured and recorded, not hidden."""
    report = _validate(r_outputs, "pixel_metrics_1a", "lidR").to_record()
    diagnostics = [a["numerical"]["contract_convention_comparison"] for a in report["artifacts"]]
    assert not all(d["passed"] for d in diagnostics)
