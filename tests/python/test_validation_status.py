"""Tests for the validation status model and the comparability gate."""

from __future__ import annotations

import itertools

import pytest

from lidarbench.validation import (
    AttemptVerdicts,
    Comparability,
    ComparisonSubject,
    LevelVerdict,
    NotComparableError,
    Outcome,
    ReportMode,
    Status,
    comparison_gate,
    require_comparable,
    speedup,
)
from lidarbench.validation.status import LEVEL_STATUSES, Level, combine

EXECUTION = (
    "succeeded",
    "failed",
    "timed_out",
    "interrupted",
    "skipped_unsupported",
    "skipped_blocked",
    "skipped_unavailable",
)
P = LevelVerdict(Status.PASSED)


def _all_combinations():
    for execution, a, s, n, impl, mode, boundary in itertools.product(
        EXECUTION,
        LEVEL_STATUSES[Level.ARTIFACTS],
        LEVEL_STATUSES[Level.STRUCTURAL],
        LEVEL_STATUSES[Level.NUMERICAL],
        ("runnable", "diagnostic_only"),
        ("numerical", "performance-only"),
        (None, 0, 3),
    ):
        try:
            yield AttemptVerdicts(
                execution,
                LevelVerdict(a),
                LevelVerdict(s),
                LevelVerdict(n),
                implementation_status=impl,
                equivalence="matched_definition",
                boundary_difference_points=boundary,
                validation_mode=mode,
            )
        except ValueError:
            continue


def test_failed_or_incomplete_validation_is_never_validated():
    seen = 0
    for v in _all_combinations():
        seen += 1
        levels = (v.artifacts.status, v.structural.status, v.numerical.status)
        if v.validated_performance:
            assert v.execution_status == "succeeded"
            assert levels == (Status.PASSED,) * 3
            assert v.validation_mode == "numerical"
            assert v.implementation_status == "runnable"
        if Status.FAILED in levels or Status.NOT_RUN in levels or v.execution_status != "succeeded":
            assert not v.validated_performance
            for mode in ReportMode:
                assert v.report_inclusion(mode)[0] is False
            assert v.comparability()[0] is Comparability.EXCLUDED
        if v.comparability()[0] is Comparability.COMPARABLE:
            assert v.validated_performance and v.boundary_difference_points == 0
        included, _ = v.report_inclusion(ReportMode.VALIDATED)
        if included:
            assert v.numerical.status in (Status.PASSED, Status.ACCEPTED_EXCEPTION)
        included, label = v.report_inclusion(ReportMode.PERFORMANCE_ONLY)
        if included:
            assert v.structurally_successful
            assert label in ("validated", "accepted_exception") or label.startswith("unverified")
    assert seen > 1000


@pytest.mark.parametrize(
    ("kwargs", "message"),
    [
        ({"execution_status": "failed", "artifacts": P}, "earlier level"),
        ({"artifacts": LevelVerdict(Status.FAILED), "structural": P}, "earlier level"),
        (
            {"artifacts": LevelVerdict(Status.UNAVAILABLE), "structural": P},
            "earlier level",
        ),
        (
            {"artifacts": P, "structural": LevelVerdict(Status.NOT_RUN), "numerical": P},
            "earlier level",
        ),
        (
            {
                "artifacts": P,
                "structural": P,
                "numerical": LevelVerdict(Status.ACCEPTED_EXCEPTION),
                "validation_mode": "performance-only",
            },
            "performance-only",
        ),
        (
            {
                "artifacts": P,
                "structural": P,
                "numerical": P,
                "implementation_status": "diagnostic_only",
            },
            "diagnostic_only",
        ),
        ({"artifacts": LevelVerdict(Status.UNVERIFIED)}, "not allowed"),
        ({"structural": LevelVerdict(Status.ACCEPTED_EXCEPTION)}, "not allowed"),
        ({"boundary_difference_points": -1}, ">= 0"),
    ],
)
def test_invariants(kwargs, message):
    base = {"execution_status": "succeeded"}
    base.update(kwargs)
    with pytest.raises(ValueError, match=message):
        AttemptVerdicts(**base)


def _ok(numerical=Status.PASSED, **kwargs):
    defaults = {
        "artifacts": P,
        "structural": P,
        "numerical": LevelVerdict(numerical),
        "equivalence": "matched_definition",
        "boundary_difference_points": 0,
    }
    defaults.update(kwargs)
    return AttemptVerdicts("succeeded", **defaults)


def test_outcomes_and_labels():
    assert _ok().outcome is Outcome.VALIDATED
    assert _ok().comparability() == (Comparability.COMPARABLE, ())
    assert _ok(Status.ACCEPTED_EXCEPTION).outcome is Outcome.ACCEPTED_EXCEPTION
    assert _ok(Status.ACCEPTED_EXCEPTION).report_inclusion("validated") == (
        True,
        "accepted_exception",
    )
    unverified = _ok(Status.UNVERIFIED)
    assert unverified.outcome is Outcome.UNVERIFIED
    assert unverified.report_inclusion("validated") == (False, "unverified")
    assert unverified.report_inclusion("performance-only") == (True, "unverified (unverified)")
    unavailable = _ok(Status.UNAVAILABLE)
    assert unavailable.report_inclusion("performance-only") == (True, "unverified (unavailable)")
    assert _ok(Status.UNSUPPORTED).comparability()[0] is Comparability.DESCRIPTIVE_ONLY
    assert _ok(Status.FAILED).outcome is Outcome.VALIDATION_FAILED
    assert _ok(Status.NOT_RUN).outcome is Outcome.VALIDATION_INCOMPLETE
    incomplete = AttemptVerdicts("succeeded", LevelVerdict(Status.UNAVAILABLE))
    assert incomplete.outcome is Outcome.VALIDATION_INCOMPLETE
    assert AttemptVerdicts("timed_out").outcome is Outcome.EXECUTION_FAILED
    assert AttemptVerdicts("skipped_blocked").outcome is Outcome.BLOCKED
    assert AttemptVerdicts("skipped_unsupported").outcome is Outcome.UNSUPPORTED
    assert AttemptVerdicts("skipped_unavailable").outcome is Outcome.UNAVAILABLE


def test_comparability_reasons():
    cls, reasons = _ok(boundary_difference_points=7).comparability()
    assert cls is Comparability.DESCRIPTIVE_ONLY and "7 point(s)" in reasons[0]
    cls, reasons = _ok(boundary_difference_points=None).comparability()
    assert cls is Comparability.DESCRIPTIVE_ONLY and "not established" in reasons[0]
    cls, reasons = _ok(equivalence="non_comparable").comparability()
    assert cls is Comparability.DESCRIPTIVE_ONLY and "non_comparable" in reasons[0]
    diag = _ok(Status.UNVERIFIED, implementation_status="diagnostic_only")
    assert diag.comparability()[0] is Comparability.DIAGNOSTIC_ONLY
    record = _ok().to_record()
    assert record["validated_performance"] is True and record["comparability"] == "comparable"


def test_combine_precedence():
    assert combine([]) is Status.NOT_RUN
    assert combine([Status.PASSED, Status.ACCEPTED_EXCEPTION]) is Status.ACCEPTED_EXCEPTION
    assert (
        combine([Status.PASSED, Status.UNVERIFIED, Status.ACCEPTED_EXCEPTION]) is Status.UNVERIFIED
    )
    assert combine([Status.UNVERIFIED, Status.FAILED]) is Status.FAILED
    assert combine([Status.PASSED, Status.UNAVAILABLE]) is Status.UNAVAILABLE


def _subject(tool="lidR", verdicts=None, **overrides):
    values = {
        "tool": tool,
        "verdicts": verdicts or _ok(),
        "contract_key": "workloads-v1/pixel_metrics_1a",
        "contract_digest": "abc",
        "equivalence_group": "workloads-v1/pixel_metrics_1a",
        "dataset_digest": "d1",
        "campaign_identity": "c1",
        "timing_boundary": "process",
        "run_dimensions": {"cpus": 1},
    }
    values.update(overrides)
    return ComparisonSubject(**values)


def test_gate_passes_only_when_everything_matches():
    a, b = _subject("lidR"), _subject("lasR")
    assert comparison_gate(a, b, "validated").comparable
    assert speedup(a, 2.0, b, 4.0, "validated") == 2.0
    with pytest.raises(ValueError, match="positive"):
        speedup(a, 0.0, b, 4.0, "validated")


@pytest.mark.parametrize(
    ("b", "mode", "fragment"),
    [
        (_subject("lasR"), "performance-only", "validated report"),
        (_subject("lidR"), "validated", "same tool"),
        (_subject("lasR", _ok(Status.ACCEPTED_EXCEPTION)), "validated", "descriptive_only"),
        (_subject("lasR", _ok(boundary_difference_points=5)), "validated", "5 point(s)"),
        (_subject("lasR", AttemptVerdicts("failed")), "validated", "excluded"),
        (_subject("lasR", contract_digest="zzz"), "validated", "contract digest"),
        (_subject("lasR", dataset_digest="d2"), "validated", "dataset"),
        (_subject("lasR", timing_boundary="workload"), "validated", "timing boundary"),
        (_subject("lasR", run_dimensions={"cpus": 2}), "validated", "run dimensions"),
        (_subject("lasR", campaign_identity="c2"), "validated", "campaign identity"),
    ],
)
def test_gate_refuses_ratios(b, mode, fragment):
    a = _subject("lidR")
    result = comparison_gate(a, b, mode)
    assert not result.comparable
    assert any(fragment in reason for reason in result.reasons)
    with pytest.raises(NotComparableError):
        require_comparable(a, b, mode)
    with pytest.raises(NotComparableError):
        speedup(a, 1.0, b, 2.0, mode)
