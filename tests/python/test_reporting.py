"""Reporting dataset tests on synthetic campaign records (no benchmarks are run)."""

from __future__ import annotations

import math
from collections import Counter
from pathlib import Path

import pytest
from report_fixtures import (
    make_campaign,
    validation_report,
    verdict_record,
    with_validation_reports,
)

from lidarbench.campaign.results import collect_rows
from lidarbench.reporting.commands import snapshot
from lidarbench.reporting.dataset import (
    ATTEMPT_COLUMNS,
    TABLE_SPECS,
    ReportPolicy,
    build_campaign_dataset,
)
from lidarbench.reporting.stats import describe
from lidarbench.workloads.contract import default_suite_dir, load_suite


@pytest.fixture(scope="module")
def campaign(tmp_path_factory: pytest.TempPathFactory) -> Path:
    return make_campaign(tmp_path_factory.mktemp("report"))


def _unit(dataset, workload, cpus, tool):
    rows = [
        r
        for r in dataset.table("summary").rows
        if (r["workload"], r["cpus"], r["tool"]) == (workload, cpus, tool)
    ]
    assert len(rows) == 1
    return rows[0]


# -- statistics ---------------------------------------------------------------------


def test_describe_reports_variability_and_never_zero_for_single_samples():
    assert describe([])["n"] == 0 and describe([])["mean"] is None
    one = describe([2.0])
    assert one["mean"] == 2.0 and one["median"] == 2.0
    assert one["sd"] is None and one["cv"] is None and one["q1"] is None
    four = describe([1, 2, 3, 4, None, float("nan"), True])
    assert four["n"] == 4 and four["mean"] == 2.5
    assert four["q1"] == pytest.approx(1.75) and four["q3"] == pytest.approx(3.25)  # R type 7
    assert four["sd"] == pytest.approx(math.sqrt(5 / 3))
    assert four["cv"] == pytest.approx(four["sd"] / 2.5)


def test_policy_rejects_unknown_modes_and_bad_thresholds():
    with pytest.raises(ValueError):
        ReportPolicy("fast")
    with pytest.raises(ValueError):
        ReportPolicy("validated", -1)
    assert ReportPolicy("validated").to_record()["included_tiers"] == [
        "validated",
        "accepted_exception",
    ]


# -- performance-only report of execution-only records --------------------------------


def test_dataset_is_read_only_and_keeps_every_attempt(campaign: Path):
    before = snapshot(campaign)
    dataset = build_campaign_dataset(campaign, ReportPolicy("performance-only", 0.0))
    assert snapshot(campaign) == before
    attempts = dataset.table("attempts").rows
    assert len(attempts) == len(collect_rows(campaign))
    assert set(attempts[0]) == set(ATTEMPT_COLUMNS)
    assert list(dataset.tables) == list(TABLE_SPECS)
    exclusions = Counter(r["exclusion"] for r in attempts)
    assert exclusions["warm-up"] == sum(1 for r in attempts if r["phase"] == "warmup")
    assert exclusions["execution: failed"] == 4  # scripted.fail, 2 cpus x 2 reps
    assert all(r["evidence_tier"] == "execution_only" for r in attempts if r["report_included"])


def test_summary_counts_tiers_skips_failures_and_variability(campaign: Path):
    dataset = build_campaign_dataset(campaign, ReportPolicy("performance-only", 0.0))
    ok = _unit(dataset, "synthetic.checksum", 1, "synthetic")
    assert ok["section"] == "descriptive" and ok["evidence"] == "execution_only"
    assert (ok["n_planned"], ok["n_included"], ok["n_execution_only"], ok["n_missing"]) == (
        2,
        2,
        2,
        0,
    )
    assert ok["sd_seconds"] is not None and ok["min_seconds"] <= ok["median_seconds"]
    assert ok["n_warmups"] == 1 and ok["availability"] == "measured (descriptive only)"
    skipped = _unit(dataset, "synthetic.checksum", 2, "synthetic")
    assert skipped["section"] == "no_measurements" and skipped["n_skipped"] == 2
    assert skipped["availability"].startswith("skipped: ")
    unsupported = _unit(dataset, "scripted.ok", 1, "synthetic")
    assert "declares no implementation" in unsupported["availability"]
    failed = _unit(dataset, "scripted.fail", 1, "scripted")
    assert failed["n_failed"] == 2 and failed["n_included"] == 0
    assert failed["availability"] == "no eligible measurements: execution: failed"
    reps = dataset.table("repetitions").rows
    assert len(reps) == sum(r["n_included"] for r in dataset.table("summary").rows)


def test_issues_cover_failures_skips_and_unvalidated_outputs(campaign: Path):
    dataset = build_campaign_dataset(campaign, ReportPolicy("performance-only", 0.0))
    kinds = Counter(r["kind"] for r in dataset.table("issues").rows)
    assert kinds["execution_failed"] == 6  # measurements and warm-ups
    assert kinds["skipped_unsupported"] > 0
    assert kinds["outputs_not_validated"] == sum(
        1 for r in dataset.table("attempts").rows if r["evidence_tier"] == "execution_only"
    )
    assert kinds["monitoring_disabled"] == 1
    assert "monitoring_missing" in kinds  # included, monitoring disabled
    failed = next(r for r in dataset.table("issues").rows if r["kind"] == "execution_failed")
    assert "exit 3" in failed["detail"]


def test_validated_report_excludes_unvalidated_timings(campaign: Path):
    dataset = build_campaign_dataset(campaign, ReportPolicy("validated", 0.0))
    attempts = dataset.table("attempts").rows
    assert not any(r["report_included"] for r in attempts)
    succeeded = [r for r in attempts if r["exclusion"].startswith("validation: ")]
    assert succeeded and all("validation_incomplete" in r["exclusion"] for r in succeeded)
    assert "No measurement is eligible under this report's policy." in dataset.notes
    assert not dataset.table("comparisons").rows and not dataset.table("scaling").rows


def test_short_timings_are_excluded_and_flagged(campaign: Path):
    dataset = build_campaign_dataset(campaign, ReportPolicy("performance-only", 1000.0))
    attempts = dataset.table("attempts").rows
    short = [r for r in attempts if r["exclusion"] == "timing below 1000.0 s"]
    assert short and all(r["short_duration"] for r in short)
    assert any(r["kind"] == "short_timing" for r in dataset.table("issues").rows)


def test_descriptive_pairs_get_no_ratio(campaign: Path):
    dataset = build_campaign_dataset(campaign, ReportPolicy("performance-only", 0.0))
    pairs = dataset.table("comparisons").rows
    assert len(pairs) == 1
    pair = pairs[0]
    assert (pair["tool_a"], pair["tool_b"]) == ("synthetic", "synth-b")
    assert pair["comparable"] is False and pair["speedup_a_vs_b_median"] is None
    assert "require a validated report" in pair["reasons"]
    assert "no validation report identifies the workload contract" in pair["reasons"]


def test_scaling_is_within_one_tool(campaign: Path):
    dataset = build_campaign_dataset(campaign, ReportPolicy("performance-only", 0.0))
    rows = dataset.table("scaling").rows
    assert {(r["tool"], r["cpus"]) for r in rows} == {("scripted", 1), ("scripted", 2)}
    two = next(r for r in rows if r["cpus"] == 2)
    assert two["baseline_cpus"] == 1 and two["computed"] is True
    assert two["scaling_ratio"] == pytest.approx(
        two["baseline_median_seconds"] / two["median_seconds"]
    )
    assert two["parallel_efficiency"] == pytest.approx(two["scaling_ratio"] / 2)
    assert two["evidence"] == "execution_only"


def test_environment_tools_datasets_and_declared_labels(campaign: Path):
    suite = load_suite(default_suite_dir())
    dataset = build_campaign_dataset(campaign, ReportPolicy("performance-only", 0.0), suite)
    env = {(r["section"], r["key"]): r["value"] for r in dataset.table("environment").rows}
    for key in ("system", "architecture", "cpu_model", "logical_cpus", "total_memory_bytes"):
        assert ("environment", key) in env
    assert env[("campaign", "campaign_id")] == "test-campaign"
    tools = {r["tool"]: r for r in dataset.table("tools").rows}
    assert set(tools) == {"synthetic", "synth-b", "scripted"}
    assert tools["synth-b"]["adapter"] == "synthetic" and tools["scripted"]["version"] == "1.0"
    assert dataset.table("datasets").rows[0]["input_count"] == 2
    assert len(dataset.table("implementations").rows) == 9
    assert dataset.campaign and dataset.campaign["workload_suite"]["name"] == "workloads-v5"
    # Synthetic workloads have no contract in the suite: no declared label is invented.
    assert all(r["declared_comparability"] is None for r in dataset.table("summary").rows)


# -- validated records ---------------------------------------------------------------


def _validated(campaign: Path, target: Path, per_tool: dict[str, dict] | None = None) -> Path:
    per_tool = per_tool or {}

    def report_for(result):
        if result["workload_id"] != "synthetic.checksum" or result["execution_status"] != (
            "succeeded"
        ):
            return None
        tool = result["entry"]["tool"]
        options = dict(per_tool.get(tool, {}))
        digest = options.pop("digest", "d" * 64)
        return validation_report(verdict_record(**options), digest)

    return with_validation_reports(campaign, target, report_for)


def test_validated_comparison_passes_the_gate(campaign: Path, tmp_path: Path):
    copy = _validated(campaign, tmp_path / "test-campaign")
    dataset = build_campaign_dataset(copy, ReportPolicy("validated", 0.0))
    unit = _unit(dataset, "synthetic.checksum", 1, "synthetic")
    assert unit["section"] == "validated" and unit["n_validated"] == 2
    pair = dataset.table("comparisons").rows[0]
    assert pair["comparable"] is True and pair["reasons"] is None
    assert pair["speedup_a_vs_b_median"] == pytest.approx(
        pair["median_b_seconds"] / pair["median_a_seconds"]
    )
    assert pair["speedup_a_vs_b_mean"] == pytest.approx(
        pair["mean_b_seconds"] / pair["mean_a_seconds"]
    )
    attempts = [r for r in dataset.table("attempts").rows if r["report_included"]]
    assert attempts and all(r["verdict_source"] == "validation_report" for r in attempts)
    assert all(r["comparability"] == "comparable" for r in attempts)
    # The same validated records in a performance-only report never yield ratios.
    descriptive = build_campaign_dataset(copy, ReportPolicy("performance-only", 0.0))
    assert descriptive.table("comparisons").rows[0]["comparable"] is False


@pytest.mark.parametrize(
    ("options", "reason"),
    [
        ({"boundary": 1}, "owned by a different cell"),
        ({"equivalence": "non_comparable"}, "declared equivalence is non_comparable"),
        ({"digest": "e" * 64}, "different contract digest"),
    ],
)
def test_gate_refuses_mismatched_conditions(campaign, tmp_path, options, reason):
    copy = _validated(campaign, tmp_path / "test-campaign", {"synth-b": dict(options)})
    dataset = build_campaign_dataset(copy, ReportPolicy("validated", 0.0))
    pair = dataset.table("comparisons").rows[0]
    assert pair["comparable"] is False and pair["speedup_a_vs_b_median"] is None
    assert reason in pair["reasons"]


def test_validation_failures_are_excluded_and_reported(campaign: Path, tmp_path: Path):
    copy = _validated(campaign, tmp_path / "test-campaign", {"synth-b": {"numerical": "failed"}})
    dataset = build_campaign_dataset(copy, ReportPolicy("performance-only", 0.0))
    unit = _unit(dataset, "synthetic.checksum", 1, "synth-b")
    assert unit["n_included"] == 0 and unit["n_validation_failed"] == 2
    failures = [r for r in dataset.table("issues").rows if r["kind"] == "validation_failed"]
    assert Counter(r["phase"] for r in failures) == {"measurement": 2, "warmup": 1}
    assert not dataset.table("comparisons").rows


def test_unverified_outputs_are_descriptive(campaign: Path, tmp_path: Path):
    copy = _validated(
        campaign,
        tmp_path / "test-campaign",
        {"synth-b": {"numerical": "unverified", "validation_mode": "performance-only"}},
    )
    validated = build_campaign_dataset(copy, ReportPolicy("validated", 0.0))
    assert _unit(validated, "synthetic.checksum", 1, "synth-b")["n_included"] == 0
    perf = build_campaign_dataset(copy, ReportPolicy("performance-only", 0.0))
    unit = _unit(perf, "synthetic.checksum", 1, "synth-b")
    assert unit["evidence"] == "unverified" and unit["section"] == "descriptive"
    assert any(r["kind"] == "unverified_output" for r in perf.table("issues").rows)


def test_inconsistent_validation_report_is_never_included(campaign: Path, tmp_path: Path):
    copy = _validated(
        campaign, tmp_path / "test-campaign", {"synth-b": {"execution_status": "failed"}}
    )
    dataset = build_campaign_dataset(copy, ReportPolicy("performance-only", 0.0))
    assert _unit(dataset, "synthetic.checksum", 1, "synth-b")["n_included"] == 0
    problems = [r for r in dataset.table("issues").rows if r["kind"] == "invalid_validation_record"]
    assert len(problems) == 3 and "differs" in problems[0]["detail"]  # incl. the warm-up


EXTENT_REASON = (
    "west.tif: accepted outward raster extension of at most one cell per edge with the full "
    "core present (accept_known_extent_discrepancies): extent [...]"
)


@pytest.fixture(scope="module")
def tolerant_campaign(tmp_path_factory: pytest.TempPathFactory) -> Path:
    return make_campaign(
        tmp_path_factory.mktemp("tolerant"), extra='[validation]\ncomparability = "edge-tolerant"'
    )


@pytest.mark.parametrize(
    ("options", "note"),
    [
        ({"boundary": 6}, "6 point(s) are owned by a different cell"),
        (
            {"numerical": "accepted_exception", "numerical_reasons": [EXTENT_REASON]},
            "accepted extent exception",
        ),
    ],
)
def test_edge_tolerant_policy_compares_declared_edge_differences(
    campaign, tolerant_campaign, tmp_path, options, note
):
    tolerant = _validated(
        tolerant_campaign, tmp_path / "t" / "test-campaign", {"synth-b": dict(options)}
    )
    dataset = build_campaign_dataset(tolerant, ReportPolicy("validated", 0.0))
    pair = dataset.table("comparisons").rows[0]
    assert pair["comparable"] is True and pair["speedup_a_vs_b_median"] is not None
    assert pair["comparability_policy"] == "edge-tolerant"
    assert note in pair["accepted_differences"] and "edge-tolerant" in pair["accepted_differences"]
    assert any("edge-tolerant (frozen in the campaign)" in n for n in dataset.notes)
    # The same verdicts in a strict campaign are refused.
    strict = _validated(campaign, tmp_path / "s" / "test-campaign", {"synth-b": dict(options)})
    refused = build_campaign_dataset(strict, ReportPolicy("validated", 0.0))
    row = refused.table("comparisons").rows[0]
    assert row["comparable"] is False and row["comparability_policy"] == "strict"
    assert row["speedup_a_vs_b_median"] is None


@pytest.mark.parametrize(
    ("options", "reason"),
    [
        ({"numerical": "accepted_exception"}, "numerical status is accepted_exception"),
        ({"boundary": None}, "cell ownership agreement with the contract was not established"),
        ({"equivalence": "non_comparable"}, "declared equivalence is non_comparable"),
    ],
)
def test_edge_tolerant_policy_still_refuses_other_differences(
    tolerant_campaign, tmp_path, options, reason
):
    copy = _validated(
        tolerant_campaign, tmp_path / "t" / "test-campaign", {"synth-b": dict(options)}
    )
    pair = build_campaign_dataset(copy, ReportPolicy("validated", 0.0)).table("comparisons").rows[0]
    assert pair["comparable"] is False and pair["speedup_a_vs_b_median"] is None
    assert reason in pair["reasons"]
