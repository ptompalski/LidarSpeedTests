"""Stable reporting dataset derived from immutable campaign records.

``lidarbench.report-dataset/1`` is a set of flat tables with fixed columns,
built only from a verified campaign directory (``CampaignStore`` digest chain).
Nothing in the campaign is modified; every derived value can be traced back to
the per-attempt ``result.json`` (``attempt_id``) and the frozen identity.

Evidence tiers of an included measurement (weakest first):

``execution_only``
    The process succeeded with every required output, but no validation level
    was run (performance-only reports only). Outputs are not validated.
``unverified``
    Artifacts and structure passed; the numerical claim is unverified
    (performance-only reports only).
``accepted_exception``
    Passed only under an explicit, frozen opt-in policy (also an execution-level
    accepted exception such as the LAStools cutoff warning).
``validated``
    Every validation level passed.

Only ``validated`` attempts may enter cross-tool comparisons, and only through
:func:`lidarbench.validation.comparability.speedup`, which enforces the
comparability gate (ARCHITECTURE.md §8.3). Every other included timing is
descriptive. Exclusions follow the legacy R report and ARCHITECTURE.md §8.4:
warm-ups, superseded retries, failures, skips, missing outputs, validation
ineligibility under the report mode and sub-``min_seconds`` timings (flagged).
"""

from __future__ import annotations

import json
import math
from collections import Counter
from collections.abc import Iterable, Mapping, Sequence
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import lidarbench
from lidarbench.campaign.results import RESULT_COLUMNS, rows_for_attempts
from lidarbench.campaign.store import AttemptInfo, CampaignStore
from lidarbench.core.canonical import digest
from lidarbench.core.clock import iso_utc, now
from lidarbench.core.store import read_json
from lidarbench.reporting.stats import describe, max_or_none, mean_or_none
from lidarbench.validation.comparability import (
    ComparisonSubject,
    comparison_gate,
    speedup,
)
from lidarbench.validation.pipeline import REPORT_SCHEMA as VALIDATION_REPORT_SCHEMA
from lidarbench.validation.status import (
    AttemptVerdicts,
    Comparability,
    LevelVerdict,
    Outcome,
    ReportMode,
    Status,
)
from lidarbench.workloads.contract import WorkloadSuite

REPORT_DATASET_SCHEMA = "lidarbench.report-dataset/1"
REPORT_MODES = tuple(m.value for m in ReportMode)
TIERS = ("execution_only", "unverified", "accepted_exception", "validated")
"""Evidence tiers, weakest first."""

TIER_DESCRIPTIONS = {
    "validated": "every validation level passed",
    "accepted_exception": "passed only under an explicit, frozen opt-in policy",
    "unverified": "artifacts and structure passed; numerical agreement not established",
    "execution_only": "process succeeded with all required outputs; outputs were not validated",
}

DEFAULT_MIN_SECONDS = 0.1


class ReportError(RuntimeError):
    """The report cannot be prepared from the given records."""


@dataclass(frozen=True)
class ReportPolicy:
    """Frozen choices of one report; recorded in the dataset."""

    mode: str
    min_seconds: float = DEFAULT_MIN_SECONDS

    def __post_init__(self) -> None:
        if self.mode not in REPORT_MODES:
            raise ValueError(f"report mode must be one of {REPORT_MODES}")
        if not (math.isfinite(self.min_seconds) and self.min_seconds >= 0):
            raise ValueError("min_seconds must be a finite number >= 0")

    def to_record(self) -> dict[str, Any]:
        """JSON-ready description, including the rules applied."""
        tiers = (
            ["validated", "accepted_exception"]
            if self.mode == "validated"
            else ["validated", "accepted_exception", "unverified", "execution_only"]
        )
        return {
            "mode": self.mode,
            "min_seconds": self.min_seconds,
            "included_tiers": tiers,
            "selection": "latest attempt (start sequence) per schedule entry; a failed final "
            "retry never falls back to an earlier success",
            "exclusions": [
                "warm-up",
                "superseded retry",
                "abandoned or incomplete record",
                "execution not succeeded (failed, timed out, interrupted, skipped)",
                "missing required outputs",
                "validation not eligible under the report mode",
                "missing timing",
                f"timing below {self.min_seconds} s (flagged)",
            ],
            "timing": "outer fresh-process interval (timing_boundary of each record); "
            "tool-reported inner intervals are shown separately and never mixed",
            "comparisons": "cross-tool ratios only through the comparability gate "
            "(validated report, comparable attempts, same contract, dataset, campaign, "
            "timing boundary and run dimensions)",
            "scaling": "within one tool, workload, dataset and campaign; ratio of medians "
            "relative to the smallest CPU count with included measurements",
            "variability": "sample SD (n-1), CV and type-7 quartiles; n < 2 gives empty "
            "variability, never zero",
        }


@dataclass
class Table:
    """One table of the reporting dataset (fixed columns)."""

    name: str
    columns: tuple[str, ...]
    description: str
    rows: list[dict[str, Any]] = field(default_factory=list)

    def add(self, **values: Any) -> None:
        """Append a row; unknown columns are a programming error."""
        unknown = set(values) - set(self.columns)
        if unknown:
            raise KeyError(f"{self.name}: unknown columns {sorted(unknown)}")
        self.rows.append({c: values.get(c) for c in self.columns})


@dataclass
class ReportDataset:
    """All tables plus metadata of one report."""

    policy: ReportPolicy
    campaign: dict[str, Any] | None
    tables: dict[str, Table]
    notes: list[str] = field(default_factory=list)
    sources: dict[str, Any] = field(default_factory=dict)
    generated_at: str = field(default_factory=lambda: iso_utc(now().wall_ns))

    def table(self, name: str) -> Table:
        """A table by name."""
        return self.tables[name]

    def metadata(self) -> dict[str, Any]:
        """JSON-ready description of the dataset (without row data)."""
        return {
            "schema": REPORT_DATASET_SCHEMA,
            "generated_at": self.generated_at,
            "generated_by": {"lidarbench_version": lidarbench.__version__},
            "policy": self.policy.to_record(),
            "campaign": self.campaign,
            "notes": list(self.notes),
            "tiers": TIER_DESCRIPTIONS,
            "sources": self.sources,
            "tables": {
                name: {
                    "description": t.description,
                    "columns": list(t.columns),
                    "rows": len(t.rows),
                }
                for name, t in self.tables.items()
            },
        }


# -- table definitions -----------------------------------------------------------

ATTEMPT_REPORT_COLUMNS = (
    "execution_mode",
    "contract_tool",
    "declared_comparability",
    "verdict_source",
    "validation_report",
    "contract_key",
    "contract_digest",
    "verdict_artifacts",
    "verdict_structural",
    "verdict_numerical",
    "boundary_difference_points",
    "outcome",
    "comparability",
    "comparability_reasons",
    "evidence_tier",
    "short_duration",
    "report_included",
    "exclusion",
)
ATTEMPT_COLUMNS = (*RESULT_COLUMNS, *ATTEMPT_REPORT_COLUMNS)

UNIT_COLUMNS = ("workload", "workload_id", "dataset", "cpus", "tool")

SUMMARY_COLUMNS = (
    *UNIT_COLUMNS,
    "tool_adapter",
    "tool_version",
    "contract_tool",
    "declared_comparability",
    "threading_model",
    "timing_boundary",
    "section",
    "evidence",
    "availability",
    "n_planned",
    "n_attempts",
    "n_selected",
    "n_not_attempted",
    "n_included",
    "n_excluded",
    "n_missing",
    "n_failed",
    "n_timed_out",
    "n_interrupted",
    "n_skipped",
    "n_missing_outputs",
    "n_validation_failed",
    "n_validation_incomplete",
    "n_validated",
    "n_accepted_exception",
    "n_unverified",
    "n_execution_only",
    "n_short",
    "n_warmups",
    "mean_seconds",
    "sd_seconds",
    "cv",
    "median_seconds",
    "q1_seconds",
    "q3_seconds",
    "min_seconds",
    "max_seconds",
    "tool_workload_mean_seconds",
    "startup_overhead_mean_seconds",
    "n_monitored",
    "n_partial_monitoring",
    "n_monitoring_missing",
    "tree_cpu_seconds_mean",
    "tree_cpu_seconds_sd",
    "tree_cpu_seconds_is_lower_bound",
    "process_cpu_seconds_mean",
    "process_cpu_seconds_sd",
    "measured_parallelism_mean",
    "n_cpu_budget_exceeded",
    "tree_peak_rss_bytes_mean",
    "tree_peak_rss_bytes_max",
    "machine_cpu_percent_mean",
    "machine_cpu_percent_sd",
    "machine_memory_used_peak_bytes_max",
    "skip_reasons",
    "exclusions",
)

REPETITION_COLUMNS = (
    *UNIT_COLUMNS,
    "repetition",
    "attempt_id",
    "tool_position",
    "evidence_tier",
    "timing_boundary",
    "elapsed_s",
    "tool_workload_s",
    "startup_overhead_s",
    "monitoring_status",
    "tree_cpu_seconds",
    "process_cpu_seconds",
    "measured_parallelism",
    "cpu_budget_exceeded",
    "tree_peak_rss_bytes",
    "machine_cpu_percent_mean",
    "machine_memory_used_peak_bytes",
    "start_wall_utc",
)

COMPARISON_COLUMNS = (
    "workload",
    "workload_id",
    "dataset",
    "cpus",
    "tool_a",
    "tool_b",
    "n_a",
    "n_b",
    "evidence_a",
    "evidence_b",
    "median_a_seconds",
    "median_b_seconds",
    "mean_a_seconds",
    "mean_b_seconds",
    "comparable",
    "comparability_policy",
    "speedup_a_vs_b_median",
    "speedup_a_vs_b_mean",
    "accepted_differences",
    "reasons",
)

SCALING_COLUMNS = (
    "workload",
    "workload_id",
    "dataset",
    "tool",
    "tool_version",
    "cpus",
    "baseline_cpus",
    "n",
    "baseline_n",
    "evidence",
    "median_seconds",
    "baseline_median_seconds",
    "sd_seconds",
    "scaling_ratio",
    "parallel_efficiency",
    "computed",
    "reasons",
)

ISSUE_COLUMNS = (
    "kind",
    "scope",
    "phase",
    "workload",
    "dataset",
    "cpus",
    "tool",
    "repetition",
    "attempt_id",
    "superseded",
    "detail",
)

ENVIRONMENT_COLUMNS = ("section", "key", "value")
TOOL_COLUMNS = (
    "tool",
    "adapter",
    "adapter_class",
    "available",
    "unavailable_reason",
    "unavailable_detail",
    "version",
    "contract_tool",
)
TOOL_DETAIL_COLUMNS = ("tool", "kind", "key", "value")
DATASET_COLUMNS = (
    "dataset",
    "input_count",
    "file_count",
    "total_bytes",
    "dataset_sha256",
    "manifest_id",
    "manifest_version",
    "manifest_kind",
    "manifest_sha256",
    "content_sha256",
)
IMPLEMENTATION_COLUMNS = (
    "workload",
    "workload_id",
    "parameters",
    "datasets",
    "timeout_s",
    "tool",
    "implementation_status",
    "reason",
    "threading_model",
    "min_threads",
    "max_threads",
    "contract_tool",
    "declared_comparability",
)

TABLE_SPECS: dict[str, tuple[tuple[str, ...], str]] = {
    "attempts": (
        ATTEMPT_COLUMNS,
        "Every attempt (warm-ups, retries, failures, skips) with its results-table/1 "
        "fields and the report's derived verdicts, evidence tier and exclusion reason.",
    ),
    "summary": (
        SUMMARY_COLUMNS,
        "One row per planned measurement unit (workload, dataset, CPUs, tool): counts of "
        "every outcome, timing variability over included repetitions and resources.",
    ),
    "repetitions": (
        REPETITION_COLUMNS,
        "Every included measurement repetition (the samples behind the summary).",
    ),
    "comparisons": (
        COMPARISON_COLUMNS,
        "Every cross-tool pair with included measurements in the same unit, the "
        "comparability gate result and, only if it passed, the ratios.",
    ),
    "scaling": (
        SCALING_COLUMNS,
        "Within-tool CPU scaling relative to the smallest CPU count with included measurements.",
    ),
    "issues": (
        ISSUE_COLUMNS,
        "Execution failures, skips, validation failures, unverified outputs, missing "
        "measurements and other problems, one row per occurrence.",
    ),
    "environment": (
        ENVIRONMENT_COLUMNS,
        "Operating system, hardware, campaign settings and provenance.",
    ),
    "tools": (TOOL_COLUMNS, "Tool availability and versions frozen in campaign identity."),
    "tool_details": (
        TOOL_DETAIL_COLUMNS,
        "Tool identity details (package versions, runtimes) and executable digests.",
    ),
    "datasets": (DATASET_COLUMNS, "Dataset digests and manifest references."),
    "implementations": (
        IMPLEMENTATION_COLUMNS,
        "Declared implementation status of each workload per tool.",
    ),
}

ISSUE_KINDS = {
    "tool_unavailable": "tool unavailable when the campaign was created",
    "monitoring_disabled": "resource monitoring disabled for the campaign",
    "not_attempted": "planned measurement without any attempt",
    "incomplete_record": "attempt without receipt or tombstone",
    "interrupted": "attempt interrupted or abandoned",
    "execution_failed": "process failed",
    "timed_out": "process timed out",
    "skipped_unavailable": "tool unavailable",
    "skipped_unsupported": "workload unsupported by the tool",
    "skipped_blocked": "implementation blocked",
    "missing_outputs": "required outputs missing",
    "validation_failed": "a validation level failed",
    "validation_incomplete": "validation did not complete",
    "invalid_validation_record": "validation record could not be interpreted",
    "unverified_output": "outputs structurally checked but numerically unverified",
    "outputs_not_validated": "outputs were not validated (execution-only evidence)",
    "accepted_exception": "accepted under an explicit, frozen policy",
    "short_timing": "timing below the minimum duration",
    "missing_timing": "succeeded but no timing recorded",
    "monitoring_partial": "monitoring covered only part of the timed interval",
    "monitoring_missing": "no resource measurements for an included attempt",
    "cpu_budget_exceeded": "measured CPU time exceeds the configured CPU budget",
}


def new_tables() -> dict[str, Table]:
    """Empty campaign tables in their fixed order."""
    return {name: Table(name, cols, desc) for name, (cols, desc) in TABLE_SPECS.items()}


# -- per-attempt derivation ---------------------------------------------------------


@dataclass
class _Derived:
    verdicts: AttemptVerdicts | None
    verdict_source: str
    validation_report: str | None = None
    contract_key: str | None = None
    contract_digest: str | None = None
    equivalence_group: str | None = None
    problem: str | None = None
    tier: str | None = None
    exclusion: str | None = None


def _level(record: Any) -> LevelVerdict:
    if not isinstance(record, Mapping):
        raise ValueError("verdict level is not an object")
    return LevelVerdict.of(str(record.get("status")), *map(str, record.get("reasons") or []))


def _validation_reports(info: AttemptInfo) -> list[str]:
    files = (info.result or {}).get("files") or {}
    return sorted(
        rel for rel in files if rel.startswith("validation/") and rel.lower().endswith(".json")
    )


def derive_verdicts(info: AttemptInfo) -> _Derived:
    """Attempt verdicts from its validation report, or from ``result.json`` alone."""
    result = info.result
    if result is None:
        return _Derived(None, "none")
    execution = str(result.get("execution_status"))
    reports = _validation_reports(info)
    if len(reports) > 1:
        return _Derived(
            AttemptVerdicts(execution),
            "invalid",
            ";".join(reports),
            problem=f"{len(reports)} validation reports; none is used",
        )
    if reports:
        rel = reports[0]
        try:
            report = read_json(info.path / rel)
            if not isinstance(report, dict) or report.get("schema") != VALIDATION_REPORT_SCHEMA:
                raise ValueError(f"not a {VALIDATION_REPORT_SCHEMA} record")
            v = report["verdicts"]
            if v.get("execution_status") != execution:
                raise ValueError(
                    f"execution status {v.get('execution_status')!r} differs from the "
                    f"attempt's {execution!r}"
                )
            contract = report.get("contract") or {}
            verdicts = AttemptVerdicts(
                execution_status=execution,
                artifacts=_level(v["artifacts"]),
                structural=_level(v["structural"]),
                numerical=_level(v["numerical"]),
                implementation_status=str(v.get("implementation_status")),
                equivalence=str(v.get("equivalence")),
                boundary_difference_points=v.get("boundary_difference_points"),
                validation_mode=str(v.get("validation_mode")),
            )
            return _Derived(
                verdicts,
                "validation_report",
                rel,
                contract.get("key"),
                contract.get("digest"),
                contract.get("equivalence_group"),
            )
        except (KeyError, TypeError, ValueError, OSError) as exc:
            return _Derived(AttemptVerdicts(execution), "invalid", rel, problem=str(exc))
    try:
        verdicts = AttemptVerdicts(
            execution_status=execution,
            structural=LevelVerdict.of(str(result.get("structural_status") or "not_run")),
            numerical=LevelVerdict.of(str(result.get("numerical_status") or "not_run")),
            implementation_status=str(result.get("implementation_status") or "unknown"),
            validation_mode=str(result.get("validation_mode")),
        )
    except ValueError as exc:
        return _Derived(AttemptVerdicts(execution), "invalid", problem=str(exc))
    return _Derived(verdicts, "result")


_NOT_VALIDATED = frozenset({Status.NOT_RUN})
_NOT_VALIDATED_NUMERICAL = frozenset({Status.NOT_RUN, Status.UNVERIFIED})


def evidence_tier(derived: _Derived, accepted_execution: bool, mode: str) -> str | None:
    """Evidence tier of an attempt under ``mode`` (``None``: not includable)."""
    v = derived.verdicts
    if v is None or derived.verdict_source == "invalid":
        return None
    included, _ = v.report_inclusion(mode)
    tier: str | None = None
    if included:
        tier = {
            Outcome.VALIDATED: "validated",
            Outcome.ACCEPTED_EXCEPTION: "accepted_exception",
        }.get(v.outcome, "unverified")
    elif (
        mode == ReportMode.PERFORMANCE_ONLY.value
        and derived.verdict_source == "result"
        and v.execution_succeeded
        and v.artifacts.status in _NOT_VALIDATED
        and v.structural.status in _NOT_VALIDATED
        and v.numerical.status in _NOT_VALIDATED_NUMERICAL
    ):
        tier = "execution_only"
    if tier == "validated" and accepted_execution:
        tier = "accepted_exception"
    return tier


def _exclusion(row: Mapping[str, Any], derived: _Derived, policy: ReportPolicy) -> str:
    if row["phase"] != "measurement":
        return "warm-up"
    if row["superseded"]:
        return "superseded retry"
    if row["record_state"] != "completed":
        return f"record {row['record_state']}"
    if row["execution_status"] != "succeeded":
        return f"execution: {row['execution_status']}"
    if row["missing_required_outputs"]:
        return "missing required outputs"
    if derived.tier is None:
        outcome = derived.verdicts.outcome.value if derived.verdicts else "unknown"
        return f"validation: {outcome} (not eligible in a {policy.mode} report)"
    elapsed = row["elapsed_s"]
    if elapsed is None or not math.isfinite(float(elapsed)):
        return "missing timing"
    if float(elapsed) < policy.min_seconds:
        return f"timing below {policy.min_seconds} s"
    return "included"


# -- builder ---------------------------------------------------------------------


def _ns_to_s(value: Any) -> float | None:
    return None if value is None else int(value) / 1e9


def _json(value: Any) -> str | None:
    if value is None:
        return None
    return json.dumps(value, sort_keys=True, ensure_ascii=False)


def _weakest(tiers: Iterable[str]) -> str | None:
    present = set(tiers)
    for tier in TIERS:
        if tier in present:
            return tier
    return None


class _Builder:
    def __init__(
        self, store: CampaignStore, policy: ReportPolicy, suite: WorkloadSuite | None
    ) -> None:
        self.store = store
        self.policy = policy
        self.suite = suite
        self.tables = new_tables()
        self.notes: list[str] = []
        c = store.components
        self.config: dict[str, Any] = c["config"]
        # Frozen campaign policy; campaigns created before the policy existed are strict.
        self.comparability_policy = str(
            (self.config.get("validation") or {}).get("comparability") or "strict"
        )
        self.tool_records: dict[str, dict[str, Any]] = {t["id"]: t for t in c["tools"]["tools"]}
        self.dataset_records: dict[str, dict[str, Any]] = {
            d["id"]: d for d in c["datasets"]["datasets"]
        }
        self.workload_records: dict[str, dict[str, Any]] = {
            w["name"]: w for w in c["workloads"]["workloads"]
        }
        self.tool_order = [t["id"] for t in self.config["tools"]]
        self.workload_order = [w["name"] for w in self.config["workloads"]]
        self.dataset_order = [d["id"] for d in self.config["datasets"]]

    # helpers ------------------------------------------------------------------

    def contract_tool(self, tool: str) -> str | None:
        record = self.tool_records.get(tool)
        if record is None:
            return None
        return str((record.get("details") or {}).get("contract_tool") or record["adapter"])

    def declared(self, workload_name: str, tool: str) -> str | None:
        if self.suite is None:
            return None
        workload = self.workload_records.get(workload_name) or {}
        contract = self.suite.contracts.get(str(workload.get("id")))
        contract_tool = self.contract_tool(tool)
        if contract is None or contract_tool is None:
            return None
        if contract.implementation(contract_tool) is None:
            return "unsupported"
        return contract.declared_comparability(contract_tool)

    def dataset_digest(self, dataset: str) -> str | None:
        record = self.dataset_records.get(dataset)
        return digest(record) if record is not None else None

    def unit_sort_key(self, unit: tuple[str, str, int, str]) -> tuple[Any, ...]:
        workload, dataset, cpus, tool = unit

        def pos(seq: list[str], value: str) -> int:
            return seq.index(value) if value in seq else len(seq)

        return (
            pos(self.workload_order, workload),
            pos(self.dataset_order, dataset),
            cpus,
            pos(self.tool_order, tool),
        )

    # build --------------------------------------------------------------------

    def build(self) -> ReportDataset:
        infos = self.store.attempts()
        rows = rows_for_attempts(self.store, infos)
        derived: dict[str, _Derived] = {}
        attempts = self.tables["attempts"]
        for info, row in zip(infos, rows, strict=True):
            d = derive_verdicts(info)
            result = info.result or {}
            d.tier = evidence_tier(d, bool(row["accepted_exception"]), self.policy.mode)
            d.exclusion = _exclusion(row, d, self.policy)
            derived[info.attempt_id] = d
            comparability: tuple[str | None, tuple[str, ...]] = (None, ())
            if d.verdicts is not None:
                cls, reasons = d.verdicts.comparability(self.comparability_policy)
                comparability = (cls.value, reasons)
            elapsed = row["elapsed_s"]
            attempts.add(
                **row,
                execution_mode=(result.get("timing") or {}).get("execution_mode"),
                contract_tool=self.contract_tool(row["tool"]) if row["tool"] else None,
                declared_comparability=(
                    self.declared(row["workload"], row["tool"]) if row["workload"] else None
                ),
                verdict_source=d.verdict_source,
                validation_report=d.validation_report,
                contract_key=d.contract_key,
                contract_digest=d.contract_digest,
                verdict_artifacts=d.verdicts.artifacts.status.value if d.verdicts else None,
                verdict_structural=d.verdicts.structural.status.value if d.verdicts else None,
                verdict_numerical=d.verdicts.numerical.status.value if d.verdicts else None,
                boundary_difference_points=(
                    d.verdicts.boundary_difference_points if d.verdicts else None
                ),
                outcome=d.verdicts.outcome.value if d.verdicts else None,
                comparability=comparability[0],
                comparability_reasons="; ".join(comparability[1]) or None,
                evidence_tier=d.tier,
                short_duration=(elapsed is not None and float(elapsed) < self.policy.min_seconds),
                report_included=d.exclusion == "included",
                exclusion=d.exclusion,
            )
        self.derived = derived
        self._units()
        self._comparisons()
        self._scaling()
        self._issues()
        self._environment()
        self._tools()
        self._datasets()
        self._implementations()
        self._notes()
        manifest = self.store.manifest or {}
        campaign = {
            "campaign_id": self.store.campaign_id,
            "identity_sha256": self.store.identity_sha256,
            "created_at": manifest.get("created_at"),
            "created_by": manifest.get("created_by"),
            "description": self.config.get("description"),
            "validation_mode": self.config.get("validation_mode"),
            "schedule_entries": manifest.get("schedule_entries"),
            "component_sha256": self.store.component_digests(),
            "workload_suite": (
                {
                    "name": self.suite.name,
                    "contracts": {k: c.digest for k, c in sorted(self.suite.contracts.items())},
                }
                if self.suite
                else None
            ),
        }
        return ReportDataset(self.policy, campaign, self.tables, self.notes)

    # summary --------------------------------------------------------------------

    def _units(self) -> None:
        planned: dict[tuple[str, str, int, str], set[int]] = {}
        for entry in self.store.schedule:
            if entry.phase == "measurement":
                key = (entry.workload, entry.dataset, entry.cpus, entry.tool)
                planned.setdefault(key, set()).add(entry.repetition)
        self.planned = planned
        rows = self.tables["attempts"].rows
        by_unit: dict[tuple[str, str, int, str], list[dict[str, Any]]] = {}
        for row in rows:
            key = (row["workload"], row["dataset"], row["cpus"], row["tool"])
            by_unit.setdefault(key, []).append(row)
        self.included: dict[tuple[str, str, int, str], list[dict[str, Any]]] = {}
        summary = self.tables["summary"]
        repetitions = self.tables["repetitions"]
        for unit in sorted(planned, key=self.unit_sort_key):
            workload, dataset, cpus, tool = unit
            unit_rows = by_unit.get(unit, [])
            measured = [r for r in unit_rows if r["phase"] == "measurement"]
            selected = [r for r in measured if not r["superseded"]]
            included = [r for r in selected if r["report_included"]]
            self.included[unit] = included
            for r in sorted(included, key=lambda r: (r["repetition"], r["sequence"] or 0)):
                repetitions.add(
                    workload=workload,
                    workload_id=r["workload_id"],
                    dataset=dataset,
                    cpus=cpus,
                    tool=tool,
                    repetition=r["repetition"],
                    attempt_id=r["attempt_id"],
                    tool_position=r["tool_position"],
                    evidence_tier=r["evidence_tier"],
                    timing_boundary=r["timing_boundary"],
                    elapsed_s=r["elapsed_s"],
                    tool_workload_s=_ns_to_s(r["tool_workload_elapsed_ns"]),
                    startup_overhead_s=_ns_to_s(r["startup_overhead_ns"]),
                    monitoring_status=r["monitoring_status"],
                    tree_cpu_seconds=r["tree_cpu_seconds"],
                    process_cpu_seconds=r["process_cpu_seconds"],
                    measured_parallelism=r["measured_parallelism"],
                    cpu_budget_exceeded=r["cpu_budget_exceeded"],
                    tree_peak_rss_bytes=r["tree_peak_rss_bytes"],
                    machine_cpu_percent_mean=r["machine_cpu_percent_mean"],
                    machine_memory_used_peak_bytes=r["machine_memory_used_peak_bytes"],
                    start_wall_utc=r["start_wall_utc"],
                )
            attempted_reps = {r["repetition"] for r in measured}
            tiers = Counter(r["evidence_tier"] for r in included)
            statuses = Counter(r["execution_status"] for r in selected)
            outcomes = Counter(
                r["outcome"] for r in selected if r["execution_status"] == "succeeded"
            )
            timing = describe(r["elapsed_s"] for r in included)
            monitored = [r for r in included if r["monitoring_status"] == "available"]
            cpu = describe(r["tree_cpu_seconds"] for r in monitored)
            exact_cpu = describe(r["process_cpu_seconds"] for r in included)
            machine_cpu = describe(r["machine_cpu_percent_mean"] for r in monitored)
            rss = [r["tree_peak_rss_bytes"] for r in monitored]
            evidence = _weakest(tiers)
            section = (
                "no_measurements"
                if not included
                else "validated" if evidence == "validated" else "descriptive"
            )
            skip_reasons = sorted({str(r["skip_reason"]) for r in selected if r["skip_reason"]})
            exclusions = Counter(r["exclusion"] for r in measured if r["exclusion"] != "included")
            tool_record = self.tool_records.get(tool) or {}
            first = (included or selected or unit_rows or [{}])[0]
            summary.add(
                workload=workload,
                workload_id=(self.workload_records.get(workload) or {}).get("id"),
                dataset=dataset,
                cpus=cpus,
                tool=tool,
                tool_adapter=tool_record.get("adapter"),
                tool_version=tool_record.get("version"),
                contract_tool=self.contract_tool(tool),
                declared_comparability=self.declared(workload, tool),
                threading_model=first.get("threading_model"),
                timing_boundary=";".join(sorted({str(r["timing_boundary"]) for r in included}))
                or None,
                section=section,
                evidence=evidence,
                availability=self._availability(
                    included,
                    selected,
                    planned[unit],
                    attempted_reps,
                    section,
                    skip_reasons,
                    exclusions,
                ),
                n_planned=len(planned[unit]),
                n_attempts=len(measured),
                n_selected=len(selected),
                n_not_attempted=len(planned[unit] - attempted_reps),
                n_included=len(included),
                n_excluded=len(selected) - len(included),
                n_missing=len(planned[unit]) - len(included),
                n_failed=statuses["failed"],
                n_timed_out=statuses["timed_out"],
                n_interrupted=statuses["interrupted"]
                + sum(1 for r in selected if r["record_state"] == "incomplete"),
                n_skipped=sum(n for s, n in statuses.items() if s and s.startswith("skipped_")),
                n_missing_outputs=sum(1 for r in selected if r["missing_required_outputs"]),
                n_validation_failed=outcomes[Outcome.VALIDATION_FAILED.value],
                n_validation_incomplete=sum(
                    1
                    for r in selected
                    if r["outcome"] == Outcome.VALIDATION_INCOMPLETE.value
                    and r["evidence_tier"] is None
                ),
                n_validated=tiers["validated"],
                n_accepted_exception=tiers["accepted_exception"],
                n_unverified=tiers["unverified"],
                n_execution_only=tiers["execution_only"],
                n_short=sum(1 for r in selected if r["short_duration"]),
                n_warmups=sum(1 for r in unit_rows if r["phase"] != "measurement"),
                mean_seconds=timing["mean"],
                sd_seconds=timing["sd"],
                cv=timing["cv"],
                median_seconds=timing["median"],
                q1_seconds=timing["q1"],
                q3_seconds=timing["q3"],
                min_seconds=timing["min"],
                max_seconds=timing["max"],
                tool_workload_mean_seconds=mean_or_none(
                    _ns_to_s(r["tool_workload_elapsed_ns"]) for r in included
                ),
                startup_overhead_mean_seconds=mean_or_none(
                    _ns_to_s(r["startup_overhead_ns"]) for r in included
                ),
                n_monitored=len(monitored),
                n_partial_monitoring=sum(
                    1 for r in included if r["monitoring_status"] == "partial"
                ),
                n_monitoring_missing=len(included) - len(monitored),
                tree_cpu_seconds_mean=cpu["mean"],
                tree_cpu_seconds_sd=cpu["sd"],
                tree_cpu_seconds_is_lower_bound=(
                    any(bool(r["tree_cpu_seconds_is_lower_bound"]) for r in monitored)
                    if monitored
                    else None
                ),
                process_cpu_seconds_mean=exact_cpu["mean"],
                process_cpu_seconds_sd=exact_cpu["sd"],
                measured_parallelism_mean=mean_or_none(r["measured_parallelism"] for r in included),
                n_cpu_budget_exceeded=sum(1 for r in included if r["cpu_budget_exceeded"]),
                tree_peak_rss_bytes_mean=mean_or_none(rss),
                tree_peak_rss_bytes_max=max_or_none(rss),
                machine_cpu_percent_mean=machine_cpu["mean"],
                machine_cpu_percent_sd=machine_cpu["sd"],
                machine_memory_used_peak_bytes_max=max_or_none(
                    r["machine_memory_used_peak_bytes"] for r in monitored
                ),
                skip_reasons="; ".join(skip_reasons) or None,
                exclusions="; ".join(f"{k} ({n})" for k, n in sorted(exclusions.items())) or None,
            )

    @staticmethod
    def _availability(
        included: Sequence[Mapping[str, Any]],
        selected: Sequence[Mapping[str, Any]],
        planned: set[int],
        attempted: set[int],
        section: str,
        skip_reasons: Sequence[str],
        exclusions: Counter[str],
    ) -> str:
        if included:
            text = "measured" if section == "validated" else "measured (descriptive only)"
            if len(included) < len(planned):
                text += f"; {len(planned) - len(included)} of {len(planned)} repetitions missing"
            return text
        if selected and all(str(r["execution_status"]).startswith("skipped_") for r in selected):
            return "skipped: " + ("; ".join(skip_reasons) or "no reason recorded")
        if not attempted:
            return "not attempted"
        common = exclusions.most_common(1)
        return "no eligible measurements" + (f": {common[0][0]}" if common else "")

    # comparisons ---------------------------------------------------------------

    def _subject(
        self, unit: tuple[str, str, int, str], rows: Sequence[Mapping[str, Any]]
    ) -> tuple[ComparisonSubject, list[str]]:
        extra: list[str] = []
        tool = unit[3]
        ds = [self.derived[r["attempt_id"]] for r in rows]
        chosen = next(
            (
                d
                for d in ds
                if d.verdicts is None
                or d.verdicts.comparability(self.comparability_policy)[0]
                is not Comparability.COMPARABLE
            ),
            ds[0],
        )
        contracts = {(d.contract_key, d.contract_digest, d.equivalence_group) for d in ds}
        if len(contracts) > 1:
            extra.append(f"{tool}: attempts reference different contracts")
        if chosen.contract_key is None:
            extra.append(f"{tool}: no validation report identifies the workload contract")
        boundaries = {r["timing_boundary"] for r in rows}
        modes = {r["execution_mode"] for r in rows}
        if len(boundaries) > 1:
            extra.append(f"{tool}: mixed timing boundaries {sorted(map(str, boundaries))}")
        if len(modes) > 1:
            extra.append(f"{tool}: mixed execution modes {sorted(map(str, modes))}")
        assert chosen.verdicts is not None
        subject = ComparisonSubject(
            tool=tool,
            verdicts=chosen.verdicts,
            contract_key=str(chosen.contract_key),
            contract_digest=str(chosen.contract_digest),
            equivalence_group=str(chosen.equivalence_group),
            dataset_digest=str(self.dataset_digest(unit[1])),
            campaign_identity=self.store.identity_sha256,
            timing_boundary=str(sorted(map(str, boundaries))[0]),
            run_dimensions={"cpus": unit[2], "execution_mode": sorted(map(str, modes))[0]},
        )
        return subject, extra

    def _comparisons(self) -> None:
        table = self.tables["comparisons"]
        groups: dict[tuple[str, str, int], list[tuple[str, str, int, str]]] = {}
        for unit in sorted(self.included, key=self.unit_sort_key):
            if self.included[unit]:
                groups.setdefault(unit[:3], []).append(unit)
        for (workload, dataset, cpus), units in groups.items():
            for i, ua in enumerate(units):
                for ub in units[i + 1 :]:
                    ra, rb = self.included[ua], self.included[ub]
                    sa, extra_a = self._subject(ua, ra)
                    sb, extra_b = self._subject(ub, rb)
                    policy = self.comparability_policy
                    gate = comparison_gate(sa, sb, self.policy.mode, policy)
                    reasons = list(gate.reasons) + extra_a + extra_b
                    ta, tb = describe(r["elapsed_s"] for r in ra), describe(
                        r["elapsed_s"] for r in rb
                    )
                    ratio_median = ratio_mean = None
                    if not reasons:
                        ratio_median = speedup(
                            sa, ta["median"], sb, tb["median"], self.policy.mode, policy
                        )
                        ratio_mean = speedup(
                            sa, ta["mean"], sb, tb["mean"], self.policy.mode, policy
                        )
                    table.add(
                        workload=workload,
                        workload_id=ra[0]["workload_id"],
                        dataset=dataset,
                        cpus=cpus,
                        tool_a=ua[3],
                        tool_b=ub[3],
                        n_a=ta["n"],
                        n_b=tb["n"],
                        evidence_a=_weakest(r["evidence_tier"] for r in ra),
                        evidence_b=_weakest(r["evidence_tier"] for r in rb),
                        median_a_seconds=ta["median"],
                        median_b_seconds=tb["median"],
                        mean_a_seconds=ta["mean"],
                        mean_b_seconds=tb["mean"],
                        comparable=not reasons,
                        comparability_policy=policy,
                        speedup_a_vs_b_median=ratio_median,
                        speedup_a_vs_b_mean=ratio_mean,
                        accepted_differences="; ".join(gate.notes) or None,
                        reasons="; ".join(reasons) or None,
                    )

    # scaling ---------------------------------------------------------------------

    def _scaling(self) -> None:
        table = self.tables["scaling"]
        series: dict[tuple[str, str, str], list[tuple[str, str, int, str]]] = {}
        for unit in sorted(self.included, key=self.unit_sort_key):
            if self.included[unit]:
                series.setdefault((unit[0], unit[1], unit[3]), []).append(unit)
        for (workload, dataset, tool), units in series.items():
            if len(units) < 2:
                continue
            units.sort(key=lambda u: u[2])
            base = units[0]
            base_rows = self.included[base]
            base_t = describe(r["elapsed_s"] for r in base_rows)
            base_bound = {r["timing_boundary"] for r in base_rows}
            base_modes = {r["execution_mode"] for r in base_rows}
            for unit in units:
                rows = self.included[unit]
                t = describe(r["elapsed_s"] for r in rows)
                reasons = []
                bound = {r["timing_boundary"] for r in rows}
                modes = {r["execution_mode"] for r in rows}
                if len(bound | base_bound) > 1:
                    reasons.append(
                        f"mixed timing boundaries {sorted(map(str, bound | base_bound))}"
                    )
                if len(modes | base_modes) > 1:
                    reasons.append(f"mixed execution modes {sorted(map(str, modes | base_modes))}")
                ratio = efficiency = None
                if not reasons and t["median"] and base_t["median"]:
                    ratio = base_t["median"] / t["median"]
                    efficiency = ratio * base[2] / unit[2]
                table.add(
                    workload=workload,
                    workload_id=rows[0]["workload_id"],
                    dataset=dataset,
                    tool=tool,
                    tool_version=rows[0]["tool_version"],
                    cpus=unit[2],
                    baseline_cpus=base[2],
                    n=t["n"],
                    baseline_n=base_t["n"],
                    evidence=_weakest(
                        [r["evidence_tier"] for r in rows] + [r["evidence_tier"] for r in base_rows]
                    ),
                    median_seconds=t["median"],
                    baseline_median_seconds=base_t["median"],
                    sd_seconds=t["sd"],
                    scaling_ratio=ratio,
                    parallel_efficiency=efficiency,
                    computed=ratio is not None,
                    reasons="; ".join(reasons) or None,
                )

    # issues ------------------------------------------------------------------------

    def _issues(self) -> None:
        issues = self.tables["issues"]
        for tool_id in self.tool_order:
            record = self.tool_records.get(tool_id) or {}
            if not record.get("available", True):
                issues.add(
                    kind="tool_unavailable",
                    scope="campaign",
                    tool=tool_id,
                    detail=f"{record.get('unavailable_reason')}: {record.get('unavailable_detail')}",
                )
        monitoring = self.config.get("monitoring") or {}
        if not monitoring.get("enabled", True):
            issues.add(
                kind="monitoring_disabled",
                scope="campaign",
                detail="no CPU or memory measurements were recorded",
            )
        for unit in sorted(self.planned, key=self.unit_sort_key):
            attempted = {
                r["repetition"]
                for r in self.tables["attempts"].rows
                if r["phase"] == "measurement"
                and (r["workload"], r["dataset"], r["cpus"], r["tool"]) == unit
            }
            for rep in sorted(self.planned[unit] - attempted):
                issues.add(
                    kind="not_attempted",
                    scope="measurement",
                    phase="measurement",
                    workload=unit[0],
                    dataset=unit[1],
                    cpus=unit[2],
                    tool=unit[3],
                    repetition=rep,
                    detail="planned measurement has no attempt (campaign not finished?)",
                )
        for row in self.tables["attempts"].rows:
            d = self.derived[row["attempt_id"]]
            for kind, detail in self._attempt_issues(row, d):
                issues.add(
                    kind=kind,
                    scope="attempt",
                    phase=row["phase"],
                    workload=row["workload"],
                    dataset=row["dataset"],
                    cpus=row["cpus"],
                    tool=row["tool"],
                    repetition=row["repetition"],
                    attempt_id=row["attempt_id"],
                    superseded=row["superseded"],
                    detail=detail,
                )

    def _attempt_issues(self, row: Mapping[str, Any], d: _Derived) -> list[tuple[str, str | None]]:
        out: list[tuple[str, str | None]] = []
        status = row["execution_status"]
        if row["record_state"] == "incomplete":
            return [("incomplete_record", "no completed.json or abandoned.json")]
        if status == "interrupted":
            return [("interrupted", row["failure_reason"])]
        if status == "failed":
            detail = f"{row['failure_phase']}: {row['failure_reason']}"
            if row["exit_code"] is not None:
                detail += f" (exit {row['exit_code']})"
            return [("execution_failed", detail)]
        if status == "timed_out":
            return [("timed_out", row["failure_reason"])]
        if status and str(status).startswith("skipped_"):
            return [(str(status), row["skip_reason"])]
        if row["missing_required_outputs"]:
            out.append(("missing_outputs", f"{row['missing_required_outputs']} file(s)"))
        if d.problem:
            out.append(("invalid_validation_record", d.problem))
        v = d.verdicts
        if v is not None and d.verdict_source != "invalid":
            if v.outcome is Outcome.VALIDATION_FAILED:
                out.append(("validation_failed", _level_reasons(v)))
            elif d.tier is None and v.outcome is Outcome.VALIDATION_INCOMPLETE:
                out.append(("validation_incomplete", _level_reasons(v)))
        if d.tier == "unverified":
            out.append(("unverified_output", _level_reasons(v) if v else None))
        elif d.tier == "execution_only":
            out.append(
                ("outputs_not_validated", "no validation report; structural/numerical not run")
            )
        if row["accepted_exception"] or (v is not None and v.outcome is Outcome.ACCEPTED_EXCEPTION):
            out.append(("accepted_exception", row["accepted_exception"] or _level_reasons(v)))
        if row["elapsed_s"] is None and status == "succeeded":
            out.append(("missing_timing", None))
        elif row["short_duration"]:
            out.append(("short_timing", f"{row['elapsed_s']:.6f} s < {self.policy.min_seconds} s"))
        if row["report_included"] and row["monitoring_status"] != "available":
            kind = (
                "monitoring_partial"
                if row["monitoring_status"] == "partial"
                else "monitoring_missing"
            )
            out.append((kind, f"monitoring_status={row['monitoring_status']}"))
        if row["report_included"] and row["cpu_budget_exceeded"]:
            out.append(
                (
                    "cpu_budget_exceeded",
                    f"{row['process_cpu_seconds']:.3f} CPU-s in {row['elapsed_s']:.3f} s "
                    f"(parallelism {row['measured_parallelism']:.2f} > {row['cpus']} CPUs)",
                )
            )
        return out

    # descriptive tables -----------------------------------------------------------

    def _environment(self) -> None:
        table = self.tables["environment"]
        env = self.store.components["environment"]
        for key, value in env.items():
            table.add(section="environment", key=key, value=value)
        manifest = self.store.manifest or {}
        campaign = {
            "campaign_id": self.store.campaign_id,
            "identity_sha256": self.store.identity_sha256,
            "created_at": manifest.get("created_at"),
            "lidarbench_version": (manifest.get("created_by") or {}).get("lidarbench_version"),
            "description": self.config.get("description"),
            "validation_mode": self.config.get("validation_mode"),
            "cpu_counts": _json(self.config.get("cpu_counts")),
            "timeout_s": self.config.get("timeout_s"),
        }
        for key, value in campaign.items():
            table.add(section="campaign", key=key, value=value)
        for section in ("scheduling", "monitoring", "integrity"):
            for key, value in (self.config.get(section) or {}).items():
                table.add(
                    section=section,
                    key=key,
                    value=_json(value) if isinstance(value, (list, dict)) else value,
                )
        schedule = self.store.components["schedule"]
        for key in ("rng", "seed", "tool_order", "warmups", "repetitions"):
            if key in schedule:
                value = schedule[key]
                table.add(
                    section="schedule",
                    key=key,
                    value=_json(value) if isinstance(value, (list, dict)) else value,
                )
        provenance_path = self.store.path / "provenance.json"
        if provenance_path.is_file():
            provenance = read_json(provenance_path)
            for key in ("host", "platform", "boot_time_epoch_s", "available_memory_bytes"):
                table.add(section="provenance", key=key, value=provenance.get(key))
            git = provenance.get("git") or {}
            for key in ("available", "revision", "branch", "dirty", "diff_sha256"):
                if key in git:
                    table.add(section="provenance.git", key=key, value=git.get(key))

    def _tools(self) -> None:
        tools, details = self.tables["tools"], self.tables["tool_details"]
        for tool_id in self.tool_order:
            record = self.tool_records.get(tool_id) or {}
            tools.add(
                tool=tool_id,
                adapter=record.get("adapter"),
                adapter_class=record.get("adapter_class"),
                available=record.get("available"),
                unavailable_reason=record.get("unavailable_reason"),
                unavailable_detail=record.get("unavailable_detail"),
                version=record.get("version"),
                contract_tool=self.contract_tool(tool_id),
            )
            for key, value in sorted((record.get("details") or {}).items()):
                details.add(tool=tool_id, kind="detail", key=key, value=value)
            for role, exe in sorted((record.get("executables") or {}).items()):
                details.add(tool=tool_id, kind="executable", key=role, value=_json(exe))

    def _datasets(self) -> None:
        table = self.tables["datasets"]
        for dataset_id in self.dataset_order:
            record = self.dataset_records.get(dataset_id) or {}
            manifest = record.get("manifest") or {}
            table.add(
                dataset=dataset_id,
                input_count=len(record.get("inputs") or []),
                file_count=record.get("file_count"),
                total_bytes=record.get("total_bytes"),
                dataset_sha256=self.dataset_digest(dataset_id),
                manifest_id=manifest.get("dataset_id") or manifest.get("id"),
                manifest_version=manifest.get("version"),
                manifest_kind=manifest.get("kind"),
                manifest_sha256=manifest.get("manifest_sha256"),
                content_sha256=manifest.get("content_sha256"),
            )

    def _implementations(self) -> None:
        table = self.tables["implementations"]
        for name in self.workload_order:
            w = self.workload_records.get(name) or {}
            for tool_id in self.tool_order:
                impl = (w.get("implementations") or {}).get(tool_id) or {}
                table.add(
                    workload=name,
                    workload_id=w.get("id"),
                    parameters=_json(w.get("parameters")),
                    datasets=";".join(w.get("datasets") or []),
                    timeout_s=w.get("effective_timeout_s"),
                    tool=tool_id,
                    implementation_status=impl.get("status") or impl.get("error"),
                    reason=impl.get("reason"),
                    threading_model=impl.get("threading_model"),
                    min_threads=impl.get("min_threads"),
                    max_threads=impl.get("max_threads"),
                    contract_tool=self.contract_tool(tool_id),
                    declared_comparability=self.declared(name, tool_id),
                )

    def _notes(self) -> None:
        notes = self.notes
        attempts = self.tables["attempts"].rows
        if self.policy.mode == "performance-only":
            notes.append(
                "Performance-only report: outputs may differ numerically or algorithmically; "
                "no timing in it establishes equivalent results or a cross-tool speedup."
            )
        sources = Counter(r["verdict_source"] for r in attempts if r["record_state"] == "completed")
        if attempts and not sources.get("validation_report"):
            notes.append(
                "No attempt has a validation report: outputs were not validated, so no "
                "cross-tool comparison can pass the comparability gate."
            )
        if self.suite is not None:
            notes.append(
                f"Declared comparability labels come from workload suite {self.suite.name!r} "
                "at report time; validated comparisons use only the contract recorded in each "
                "validation report (and frozen in the campaign identity)."
            )
        else:
            notes.append("No workload suite was loaded: declared comparability labels are empty.")
        if not any(r["report_included"] for r in attempts):
            notes.append("No measurement is eligible under this report's policy.")
        unavailable = [t for t in self.tool_order if not self.tool_records[t].get("available")]
        if unavailable:
            notes.append("Unavailable tools (recorded as skips): " + ", ".join(unavailable) + ".")
        if self.comparability_policy == "edge-tolerant":
            notes.append(
                "Comparability policy edge-tolerant (frozen in the campaign): tools whose "
                "validated outputs differ from the contract only at the edges (a declared cell "
                "convention that owns some boundary points differently, or an outward extent "
                "accepted under accept_known_extent_discrepancies) are compared; every owned "
                "cell value still matched the independent reference. Accepted differences are "
                "listed per comparison (accepted_differences)."
            )
        else:
            notes.append(
                "Comparability policy strict: only tools that own every point exactly like the "
                "contract partition and pass numerically are compared."
            )
        notes.append(
            "Speedups are computed only within this campaign (one machine, one frozen "
            "identity); results of different campaigns are never combined into ratios."
        )


def build_campaign_dataset(
    campaign_path: Path, policy: ReportPolicy, suite: WorkloadSuite | None = None
) -> ReportDataset:
    """Verify a campaign and derive its reporting dataset (read-only).

    Raises:
        CampaignExistsError: Not a complete campaign directory.
        CampaignIntegrityError: A record fails verification.
    """
    store = CampaignStore(Path(campaign_path))
    store.load()
    return _Builder(store, policy, suite).build()


def empty_dataset(policy: ReportPolicy) -> ReportDataset:
    """Dataset without a campaign (historical-only reports)."""
    return ReportDataset(
        policy,
        None,
        new_tables(),
        ["No lidarbench campaign was selected; only historical results are reported."],
    )


def _level_reasons(v: AttemptVerdicts | None) -> str | None:
    if v is None:
        return None
    parts = []
    for name, level in (
        ("artifacts", v.artifacts),
        ("structural", v.structural),
        ("numerical", v.numerical),
    ):
        text = f"{name}={level.status.value}"
        if level.reasons:
            text += " (" + "; ".join(level.reasons) + ")"
        parts.append(text)
    return ", ".join(parts)


__all__ = [
    "REPORT_DATASET_SCHEMA",
    "REPORT_MODES",
    "TABLE_SPECS",
    "TIERS",
    "ReportDataset",
    "ReportError",
    "ReportPolicy",
    "Table",
    "build_campaign_dataset",
    "derive_verdicts",
    "empty_dataset",
    "evidence_tier",
]
