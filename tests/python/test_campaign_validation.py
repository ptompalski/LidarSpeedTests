"""Validation inside the campaign runner (design/VALIDATION.md §8, CAMPAIGNS.md §5)."""

from __future__ import annotations

import json
import os
from pathlib import Path

import pytest
from campaign_helpers import write_config
from pdal_fakes import fake_env, make_fake_pdal, posix_only
from validation_fixtures import EPSG, neighbour_scene

from lidarbench.campaign.results import collect_rows, cpu_usage
from lidarbench.campaign.runner import CampaignRunner, RunOptions
from lidarbench.campaign.store import CampaignStore
from lidarbench.campaign.validation import REPORT_REL
from lidarbench.config.campaign import load_config
from lidarbench.execution.adapters.pdal import EXECUTABLE_ENV, PdalAdapter
from lidarbench.execution.adapters.registry import AdapterRegistry
from lidarbench.reporting.dataset import ReportPolicy, build_campaign_dataset

needs_posix = pytest.mark.skipif(posix_only, reason="the fake pdal is a POSIX shebang script")


@pytest.fixture(autouse=True)
def _isolated_environment(monkeypatch, tmp_path):
    monkeypatch.delenv(EXECUTABLE_ENV, raising=False)
    monkeypatch.setenv("PATH", os.fspath(tmp_path / "empty PATH"))
    fake_env(monkeypatch)


def _campaign(tmp_path: Path, *, mode: str = "numerical", extra: str = "", run: bool = True):
    pytest.importorskip("rasterio")
    pytest.importorskip("laspy")
    pdal = make_fake_pdal(tmp_path / "pdal bin")
    base = tmp_path / "bench root"
    for name in ("scratch", "state"):
        (base / name).mkdir(parents=True)
    neighbour_scene(base / "data")
    config_path = write_config(
        base,
        workloads=(
            f'[[workloads]]\nid = "pixel_metrics_1a"\nparameters = {{ epsg = {EPSG} }}\n'
            f'[[workloads]]\nid = "generate_DSM1"\nparameters = {{ epsg = {EPSG} }}\n'
        ),
        tools=f'[[tools]]\nid = "pdal"\noptions = {{ executable = "{pdal}" }}\n',
        cpu_counts="[1, 2]",
        scheduling='repetitions = 2\nwarmups = 1\ncooldown_s = 0\ncpu_affinity = "none"',
        extra=extra,
    )
    text = config_path.read_text(encoding="utf-8").replace('"*.laz"', '"*.las"')
    text = text.replace('validation_mode = "performance-only"', f'validation_mode = "{mode}"')
    config_path.write_text(text, encoding="utf-8")
    registry = AdapterRegistry()
    registry.register(PdalAdapter)
    runner = CampaignRunner(
        load_config(config_path),
        options=RunOptions(),
        registry=registry,
        state_dir=base / "state",
        git_info=lambda: {"available": False, "reason": "test"},
        report=lambda line: None,
        handle_signals=False,
    )
    return (runner.run() if run else None), runner, config_path, base, registry


def _results(campaign_path: Path) -> list[dict]:
    return [
        json.loads(p.read_text(encoding="utf-8"))
        for p in sorted((campaign_path / "attempts").glob("*/result.json"))
    ]


@needs_posix
def test_runner_validates_and_reuses_identical_outputs(tmp_path):
    summary, runner, *_ = _campaign(tmp_path)
    assert summary.statuses == {"succeeded": 12}, summary.to_record()
    results = _results(summary.campaign_path)
    fresh = {}
    for result in results:
        attempt = summary.campaign_path / "attempts" / result["attempt_id"]
        assert REPORT_REL in result["files"], "the report is sealed by the digest chain"
        report = json.loads((attempt / REPORT_REL).read_text(encoding="utf-8"))
        assert report["schema"] == "lidarbench.validation-report/1"
        assert report["contract"]["key"].startswith("workloads-v5/")
        assert report["tool"] == "pdal"
        validation = result["validation"]
        assert validation["report"] == REPORT_REL
        assert validation["outcome"] == report["verdicts"]["outcome"] == "validated"
        assert result["artifacts_status"] == result["structural_status"] == "passed"
        assert result["numerical_status"] == "passed"
        if validation["reused_from"] is None:
            fresh.setdefault(result["workload_id"], []).append(result["attempt_id"])
            assert report["reuse"] is None
            assert report["backends"]["decoding"]["input_digests"].startswith("trusted")
        else:
            assert report["reuse"]["source_attempt_id"] == validation["reused_from"]
            assert "byte-identical" in report["reuse"]["statement"]
    # Deterministic outputs: one full validation per workload, every other attempt reuses it.
    assert {w: len(ids) for w, ids in fresh.items()} == {"pixel_metrics_1a": 1, "generate_DSM1": 1}
    assert runner._validator is not None
    assert (runner._validator.validated, runner._validator.reused) == (2, 10)
    rows = collect_rows(summary.campaign_path)
    assert {r["validation_outcome"] for r in rows} == {"validated"}
    assert sum(1 for r in rows if r["validation_reused_from"]) == 10


@needs_posix
def test_validated_reports_are_eligible_for_validated_reporting(tmp_path):
    summary, *_ = _campaign(tmp_path)
    dataset = build_campaign_dataset(summary.campaign_path, ReportPolicy("validated", 0.0))
    attempts = dataset.table("attempts").rows
    measured = [r for r in attempts if r["phase"] == "measurement"]
    assert {r["evidence_tier"] for r in measured} == {"validated"}
    assert all(r["report_included"] for r in measured)
    assert {r["section"] for r in dataset.table("summary").rows} == {"validated"}


@needs_posix
def test_resume_reuses_published_reports(tmp_path):
    _, runner, *_ = _campaign(tmp_path)
    store = CampaignStore(runner.store.path)
    store.load()
    # A new run (here: a retry of everything via a fresh validator) indexes published reports.
    from lidarbench.campaign.validation import CampaignValidator

    identity, _ = runner._identity()
    validator = CampaignValidator(runner.config, identity, store.attempts())
    assert len(validator._index) == 2


@needs_posix
def test_performance_only_mode_checks_structure_without_reference(tmp_path):
    summary, *_ = _campaign(tmp_path, mode="performance-only")
    for result in _results(summary.campaign_path):
        assert result["structural_status"] == "passed"
        assert result["numerical_status"] == "unverified"
        assert result["validation"]["outcome"] == "unverified"


@needs_posix
def test_reuse_can_be_disabled(tmp_path):
    summary, runner, *_ = _campaign(tmp_path, extra="[validation]\nreuse_identical_outputs = false")
    assert all(r["validation"]["reused_from"] is None for r in _results(summary.campaign_path))
    assert runner._validator is not None and runner._validator.reused == 0
    # Without reuse the shared reference cache still avoids recomputing references.
    assert runner._validator.cache.hits > 0


def test_cpu_usage_flags_budget_overruns():
    process = {
        "elapsed_s": 2.0,
        "process_accounting": {"cpu_user_seconds": 5.0, "cpu_system_seconds": 0.4},
        "cpu_affinity": {"status": "applied", "cpus": [3, 4]},
    }
    usage = cpu_usage(process, 2.6, 1)
    assert usage["process_cpu_seconds"] == pytest.approx(5.4)
    assert usage["measured_parallelism"] == pytest.approx(2.7)
    assert usage["cpu_budget_exceeded"] is True and usage["cpu_affinity"] == "3 4"
    assert cpu_usage(process, 2.6, 4)["cpu_budget_exceeded"] is False
    # The sampled total wins when it saw more (reparented session members).
    assert cpu_usage({**process, "process_accounting": None}, 2.6, 1)["process_cpu_seconds"] == 2.6
    assert cpu_usage({}, None, 1) == {
        "process_cpu_seconds": None,
        "measured_parallelism": None,
        "cpu_budget_exceeded": None,
        "cpu_affinity": None,
    }
