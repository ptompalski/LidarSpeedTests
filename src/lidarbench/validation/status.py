"""Validation status model: separate verdicts per level and derived outcomes.

Each attempt carries independent verdicts, produced in order:

==============  ==========================================================  ==========
level           question                                                    producer
==============  ==========================================================  ==========
execution       did the process run and exit successfully?                  runner
artifacts       is every required file present, non-empty, of the right     validator
                format and readable, with nothing unexpected?
structural      do the products have the required CRS, grid, resolution,    validator
                alignment, extent, bands and encoding?
numerical       do the values and NoData masks match an independent         validator
                reference within the contract tolerance?
comparability   may this timing enter a cross-tool comparison?              derived
==============  ==========================================================  ==========

Statuses (not every status is valid at every level, see :data:`LEVEL_STATUSES`):

* ``passed`` / ``failed``: the check ran and decided.
* ``accepted_exception``: passed only under an explicit, frozen opt-in policy.
* ``unverified``: everything that could be checked passed, but the numerical
  claim cannot be established (performance-only mode, undeclared tool
  conventions, unknown CRS units, diagnostic-only implementations).
* ``unavailable``: the validator or an optional dependency is not usable here.
* ``unsupported``: no reference or structural check exists for this product.
* ``not_applicable``: the level does not apply to the workload.
* ``not_run``: a prerequisite did not pass, so the level was not attempted.

Fail-closed invariant (enforced by :class:`AttemptVerdicts`): a level can only
be ``passed``/``accepted_exception`` if every earlier level passed. Therefore
failed or incomplete validation can never yield :attr:`validated_performance`.
"""

from __future__ import annotations

from collections.abc import Iterable, Mapping
from dataclasses import dataclass
from enum import StrEnum
from typing import Any


class Status(StrEnum):
    """Verdict of one validation level."""

    PASSED = "passed"
    ACCEPTED_EXCEPTION = "accepted_exception"
    FAILED = "failed"
    UNVERIFIED = "unverified"
    UNAVAILABLE = "unavailable"
    UNSUPPORTED = "unsupported"
    NOT_APPLICABLE = "not_applicable"
    NOT_RUN = "not_run"


class Level(StrEnum):
    """Validation levels, in dependency order."""

    ARTIFACTS = "artifacts"
    STRUCTURAL = "structural"
    NUMERICAL = "numerical"


LEVEL_STATUSES: Mapping[Level, frozenset[Status]] = {
    Level.ARTIFACTS: frozenset({Status.PASSED, Status.FAILED, Status.UNAVAILABLE, Status.NOT_RUN}),
    Level.STRUCTURAL: frozenset(
        {Status.PASSED, Status.FAILED, Status.UNAVAILABLE, Status.UNSUPPORTED, Status.NOT_RUN}
    ),
    Level.NUMERICAL: frozenset(Status),
}

EXECUTION_SUCCEEDED = "succeeded"
EXECUTION_SKIPS = {
    "skipped_unsupported": "unsupported",
    "skipped_blocked": "blocked",
    "skipped_unavailable": "unavailable",
}

_ACCEPTING = frozenset({Status.PASSED, Status.ACCEPTED_EXCEPTION})
_DESCRIPTIVE_NUMERICAL = frozenset(
    {Status.UNVERIFIED, Status.UNAVAILABLE, Status.UNSUPPORTED, Status.NOT_APPLICABLE}
)


class Outcome(StrEnum):
    """Single derived label summarizing an attempt for reports."""

    VALIDATED = "validated"
    ACCEPTED_EXCEPTION = "accepted_exception"
    UNVERIFIED = "unverified"
    VALIDATION_FAILED = "validation_failed"
    VALIDATION_INCOMPLETE = "validation_incomplete"
    EXECUTION_FAILED = "execution_failed"
    UNSUPPORTED = "unsupported"
    BLOCKED = "blocked"
    UNAVAILABLE = "unavailable"


class Comparability(StrEnum):
    """Attempt-level comparability class (ARCHITECTURE.md §8.1)."""

    COMPARABLE = "comparable"
    DESCRIPTIVE_ONLY = "descriptive_only"
    DIAGNOSTIC_ONLY = "diagnostic_only"
    EXCLUDED = "excluded"


class ReportMode(StrEnum):
    """Which attempts a report may contain."""

    VALIDATED = "validated"
    PERFORMANCE_ONLY = "performance-only"


@dataclass(frozen=True)
class LevelVerdict:
    """Status of one level plus human-readable reasons."""

    status: Status
    reasons: tuple[str, ...] = ()

    @classmethod
    def of(cls, status: Status | str, *reasons: str) -> LevelVerdict:
        """Build a verdict, dropping empty reasons."""
        return cls(Status(status), tuple(r for r in reasons if r))

    def to_record(self) -> dict[str, Any]:
        """JSON-serializable form."""
        return {"status": self.status.value, "reasons": list(self.reasons)}


NOT_RUN = LevelVerdict(Status.NOT_RUN)


@dataclass(frozen=True)
class AttemptVerdicts:
    """All verdicts of one attempt.

    Raises:
        ValueError: A status is not valid for its level, or a level accepts
            although an earlier level did not pass (fail-closed invariant).
    """

    execution_status: str
    artifacts: LevelVerdict = NOT_RUN
    structural: LevelVerdict = NOT_RUN
    numerical: LevelVerdict = NOT_RUN
    implementation_status: str = "runnable"
    equivalence: str = "non_comparable"
    boundary_difference_points: int | None = None
    validation_mode: str = "numerical"

    def __post_init__(self) -> None:
        for level, verdict in self._levels():
            if verdict.status not in LEVEL_STATUSES[level]:
                raise ValueError(f"{level.value}: status {verdict.status.value!r} is not allowed")
        prerequisite_ok = self.execution_status == EXECUTION_SUCCEEDED
        for level, verdict in self._levels():
            if verdict.status in _ACCEPTING and not prerequisite_ok:
                raise ValueError(
                    f"{level.value} cannot be {verdict.status.value} when an earlier level "
                    "did not pass"
                )
            prerequisite_ok = prerequisite_ok and verdict.status is Status.PASSED
        if self.validation_mode == "performance-only" and self.numerical.status in _ACCEPTING:
            raise ValueError("performance-only validation cannot accept numerical results")
        if self.implementation_status == "diagnostic_only" and self.numerical.status in _ACCEPTING:
            raise ValueError("diagnostic_only implementations cannot pass numerical validation")
        if self.boundary_difference_points is not None and self.boundary_difference_points < 0:
            raise ValueError("boundary_difference_points must be >= 0")

    def _levels(self) -> tuple[tuple[Level, LevelVerdict], ...]:
        return (
            (Level.ARTIFACTS, self.artifacts),
            (Level.STRUCTURAL, self.structural),
            (Level.NUMERICAL, self.numerical),
        )

    @property
    def execution_succeeded(self) -> bool:
        """Whether the process ran and its exit was interpreted as success."""
        return self.execution_status == EXECUTION_SUCCEEDED

    @property
    def structurally_successful(self) -> bool:
        """Execution succeeded and artifacts and structure passed."""
        return (
            self.execution_succeeded
            and self.artifacts.status is Status.PASSED
            and self.structural.status is Status.PASSED
        )

    @property
    def validated_performance(self) -> bool:
        """True only when every level passed (no exception, nothing unverified)."""
        return self.structurally_successful and self.numerical.status is Status.PASSED

    @property
    def outcome(self) -> Outcome:
        """Single summary label (see :class:`Outcome`)."""
        if self.execution_status in EXECUTION_SKIPS:
            return Outcome(EXECUTION_SKIPS[self.execution_status])
        if not self.execution_succeeded:
            return Outcome.EXECUTION_FAILED
        if any(v.status is Status.FAILED for _, v in self._levels()):
            return Outcome.VALIDATION_FAILED
        if not self.structurally_successful or self.numerical.status is Status.NOT_RUN:
            return Outcome.VALIDATION_INCOMPLETE
        if self.numerical.status is Status.PASSED:
            return Outcome.VALIDATED
        if self.numerical.status is Status.ACCEPTED_EXCEPTION:
            return Outcome.ACCEPTED_EXCEPTION
        return Outcome.UNVERIFIED

    def comparability(self) -> tuple[Comparability, tuple[str, ...]]:
        """Attempt-level comparability class and the reasons for it."""
        outcome = self.outcome
        if outcome not in (Outcome.VALIDATED, Outcome.ACCEPTED_EXCEPTION, Outcome.UNVERIFIED):
            return Comparability.EXCLUDED, (f"outcome is {outcome.value}",)
        if self.implementation_status == "diagnostic_only":
            return Comparability.DIAGNOSTIC_ONLY, ("implementation is diagnostic_only",)
        reasons: list[str] = []
        if self.implementation_status != "runnable":
            reasons.append(f"implementation status is {self.implementation_status}")
        if self.equivalence != "matched_definition":
            reasons.append(f"declared equivalence is {self.equivalence}")
        if self.numerical.status is not Status.PASSED:
            reasons.append(f"numerical status is {self.numerical.status.value}")
        if self.boundary_difference_points is None:
            reasons.append("cell ownership agreement with the contract was not established")
        elif self.boundary_difference_points > 0:
            reasons.append(
                f"{self.boundary_difference_points} point(s) are owned by a different cell "
                "than under the contract convention"
            )
        if self.validation_mode != "numerical":
            reasons.append(f"validation mode is {self.validation_mode}")
        if reasons:
            return Comparability.DESCRIPTIVE_ONLY, tuple(reasons)
        return Comparability.COMPARABLE, ()

    def report_inclusion(self, mode: ReportMode | str) -> tuple[bool, str]:
        """Whether a report in ``mode`` may include this timing, and its label.

        ``validated`` reports include validated attempts and explicitly labelled
        accepted exceptions. ``performance-only`` reports also include
        structurally successful attempts whose numerical status is unverified,
        unavailable, unsupported or not applicable, labelled as such. Failed or
        incomplete validation is never included.
        """
        report_mode = ReportMode(mode)
        outcome = self.outcome
        if outcome in (Outcome.VALIDATED, Outcome.ACCEPTED_EXCEPTION):
            return True, outcome.value
        if (
            report_mode is ReportMode.PERFORMANCE_ONLY
            and outcome is Outcome.UNVERIFIED
            and self.numerical.status in _DESCRIPTIVE_NUMERICAL
        ):
            return True, f"unverified ({self.numerical.status.value})"
        return False, outcome.value

    def to_record(self) -> dict[str, Any]:
        """JSON-serializable form including every derived field."""
        comparability, reasons = self.comparability()
        return {
            "execution_status": self.execution_status,
            "artifacts": self.artifacts.to_record(),
            "structural": self.structural.to_record(),
            "numerical": self.numerical.to_record(),
            "implementation_status": self.implementation_status,
            "equivalence": self.equivalence,
            "boundary_difference_points": self.boundary_difference_points,
            "validation_mode": self.validation_mode,
            "outcome": self.outcome.value,
            "structurally_successful": self.structurally_successful,
            "validated_performance": self.validated_performance,
            "comparability": comparability.value,
            "comparability_reasons": list(reasons),
        }


_PRECEDENCE = (
    Status.FAILED,
    Status.NOT_RUN,
    Status.UNAVAILABLE,
    Status.UNSUPPORTED,
    Status.UNVERIFIED,
    Status.ACCEPTED_EXCEPTION,
    Status.PASSED,
    Status.NOT_APPLICABLE,
)


def combine(statuses: Iterable[Status]) -> Status:
    """Combine per-artifact statuses of one level into the weakest one.

    Precedence: failed > not_run > unavailable > unsupported > unverified >
    accepted_exception > passed > not_applicable. Empty input is ``not_run``.
    """
    present = set(statuses)
    for status in _PRECEDENCE:
        if status in present:
            return status
    return Status.NOT_RUN
