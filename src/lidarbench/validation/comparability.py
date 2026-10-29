"""Cross-tool comparability gate (ARCHITECTURE.md §8.3).

Two timings may enter a direct cross-tool comparison (ratio, speedup, ranking)
only if both attempts are individually ``comparable`` *and* they measure the
same thing under the same conditions. There is deliberately no way to compute a
ratio across a failed gate: :func:`speedup` raises :class:`NotComparableError`.
"""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass, field
from typing import Any

from lidarbench.validation.status import (
    AttemptVerdicts,
    Comparability,
    ComparabilityPolicy,
    ReportMode,
)


class NotComparableError(ValueError):
    """Two attempts failed the comparability gate."""

    def __init__(self, reasons: tuple[str, ...]) -> None:
        super().__init__("not comparable: " + "; ".join(reasons))
        self.reasons = reasons


@dataclass(frozen=True)
class ComparisonSubject:
    """Everything the gate needs to know about one attempt."""

    tool: str
    verdicts: AttemptVerdicts
    contract_key: str
    contract_digest: str
    equivalence_group: str
    dataset_digest: str
    campaign_identity: str
    timing_boundary: str
    run_dimensions: Mapping[str, Any] = field(default_factory=dict)
    """For example cores and input/scratch storage labels."""


@dataclass(frozen=True)
class GateResult:
    """Outcome of :func:`comparison_gate`."""

    comparable: bool
    reasons: tuple[str, ...]
    notes: tuple[str, ...] = ()
    """Differences accepted under the comparability policy (labels for the comparison)."""
    policy: str = ComparabilityPolicy.STRICT.value

    def to_record(self) -> dict[str, Any]:
        """JSON-serializable form."""
        return {
            "comparable": self.comparable,
            "reasons": list(self.reasons),
            "notes": list(self.notes),
            "policy": self.policy,
        }


def comparison_gate(
    a: ComparisonSubject,
    b: ComparisonSubject,
    report_mode: ReportMode | str,
    policy: ComparabilityPolicy | str = ComparabilityPolicy.STRICT,
) -> GateResult:
    """Check every condition for comparing ``a`` with ``b`` directly.

    ``policy`` is the campaign's frozen comparability policy; under
    ``edge-tolerant`` the accepted edge differences are returned as ``notes``.
    """
    policy = ComparabilityPolicy(policy)
    reasons: list[str] = []
    notes: list[str] = []
    if ReportMode(report_mode) is not ReportMode.VALIDATED:
        reasons.append("cross-tool comparisons require a validated report")
    for subject in (a, b):
        cls, why = subject.verdicts.comparability(policy)
        if cls is not Comparability.COMPARABLE:
            reasons.append(f"{subject.tool}: {cls.value} ({'; '.join(why)})")
        else:
            notes.extend(f"{subject.tool}: {w}" for w in why)
    if a.tool == b.tool:
        reasons.append("both attempts use the same tool")
    pairs = (
        ("contract", a.contract_key, b.contract_key),
        ("contract digest", a.contract_digest, b.contract_digest),
        ("equivalence group", a.equivalence_group, b.equivalence_group),
        ("dataset", a.dataset_digest, b.dataset_digest),
        ("campaign identity", a.campaign_identity, b.campaign_identity),
        ("timing boundary", a.timing_boundary, b.timing_boundary),
        ("run dimensions", dict(a.run_dimensions), dict(b.run_dimensions)),
    )
    for label, left, right in pairs:
        if left != right:
            reasons.append(f"different {label}: {left!r} != {right!r}")
    return GateResult(not reasons, tuple(reasons), tuple(notes), policy.value)


def require_comparable(
    a: ComparisonSubject,
    b: ComparisonSubject,
    report_mode: ReportMode | str,
    policy: ComparabilityPolicy | str = ComparabilityPolicy.STRICT,
) -> None:
    """Raise :class:`NotComparableError` unless the gate passes."""
    result = comparison_gate(a, b, report_mode, policy)
    if not result.comparable:
        raise NotComparableError(result.reasons)


def speedup(
    a: ComparisonSubject,
    a_seconds: float,
    b: ComparisonSubject,
    b_seconds: float,
    report_mode: ReportMode | str,
    policy: ComparabilityPolicy | str = ComparabilityPolicy.STRICT,
) -> float:
    """``b_seconds / a_seconds`` (how many times faster ``a`` is), gate permitting."""
    require_comparable(a, b, report_mode, policy)
    if not (a_seconds > 0 and b_seconds > 0):
        raise ValueError("durations must be positive")
    return b_seconds / a_seconds
