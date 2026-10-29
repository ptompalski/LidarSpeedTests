"""Summaries of monitoring samples over one timed interval.

Ports the semantics of the legacy ``benchmark_monitor_summary()`` (R): only the
part of each sample interval that overlaps the timed interval counts, means are
time-weighted by that overlap, and coverage below the full interval makes the
result ``partial``. Extensions: two scopes summarised separately, per-metric
availability, and missing values stay ``None`` (they are never treated as 0).
"""

from __future__ import annotations

import csv
from collections.abc import Iterable, Mapping, Sequence
from pathlib import Path
from typing import Any

from lidarbench.core.clock import Instant
from lidarbench.monitoring.base import (
    ERRORS_COLUMN,
    INTERVAL_COLUMNS,
    MACHINE_METRICS,
    SUMMARY_SCHEMA,
    TREE_METRICS,
    Metric,
    MetricKind,
    MonitoringStatus,
    MonitorReport,
    Scope,
)

COVERAGE_TOLERANCE_NS = 1_000_000

Row = Mapping[str, Any]


class MalformedSamplesError(ValueError):
    """Samples are inconsistent (non-increasing or overlapping intervals, bad columns)."""


def _parse(value: str, integer: bool) -> int | float | None:
    if value == "":
        return None
    return int(value) if integer else float(value)


def read_samples(path: Path, metrics: Sequence[Metric]) -> list[dict[str, Any]]:
    """Read a sample CSV; empty cells become ``None``.

    Raises:
        MalformedSamplesError: Missing columns or unparsable values.
    """
    with open(path, encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        expected = {*INTERVAL_COLUMNS, *(m.name for m in metrics), ERRORS_COLUMN}
        missing = expected - set(reader.fieldnames or ())
        if missing:
            raise MalformedSamplesError(f"{path.name}: missing columns {sorted(missing)}")
        rows = []
        for raw in reader:
            try:
                row: dict[str, Any] = {c: _parse(raw[c], True) for c in INTERVAL_COLUMNS}
                for metric in metrics:
                    row[metric.name] = _parse(raw[metric.name], False)
            except (TypeError, ValueError) as exc:
                raise MalformedSamplesError(f"{path.name}: {exc}") from exc
            row[ERRORS_COLUMN] = raw[ERRORS_COLUMN] or ""
            rows.append(row)
    return rows


def _bounds(row: Row, clock: str) -> tuple[int, int]:
    return row[f"interval_start_{clock}_ns"], row[f"interval_end_{clock}_ns"]


def _check(rows: Sequence[Row], clock: str) -> None:
    previous_end = None
    for row in rows:
        start, end = _bounds(row, clock)
        if start is None or end is None or end <= start:
            raise MalformedSamplesError(f"sample {row['sample_index']}: invalid interval")
        if previous_end is not None and start < previous_end:
            raise MalformedSamplesError(f"sample {row['sample_index']}: overlapping intervals")
        previous_end = end


def _status(covered_ns: int, duration_ns: int) -> MonitoringStatus:
    if covered_ns <= 0:
        return MonitoringStatus.UNAVAILABLE
    if duration_ns - covered_ns <= COVERAGE_TOLERANCE_NS:
        return MonitoringStatus.AVAILABLE
    return MonitoringStatus.PARTIAL


def _has_exits(row: Row) -> bool:
    return bool(row.get("exited_processes"))


def summarize_metric(
    metric: Metric, weighted: Sequence[tuple[Row, int, int]], duration_ns: int
) -> dict[str, Any]:
    """Summarise one metric from ``(row, overlap_ns, row_length_ns)`` triples."""
    valid = [(r[metric.name], w, n) for r, w, n in weighted if r[metric.name] is not None]
    covered = sum(w for _, w, _ in valid)
    status = _status(covered, duration_ns)
    lossy = (
        sum(1 for r, _, _ in weighted if r[metric.name] is not None and _has_exits(r))
        if metric.lost_on_exit
        else 0
    )
    if lossy and status is MonitoringStatus.AVAILABLE:
        status = MonitoringStatus.PARTIAL
    record: dict[str, Any] = {
        **metric.to_record(),
        "status": str(status),
        "coverage_fraction": min(1.0, covered / duration_ns) if duration_ns > 0 else None,
        "sample_count": len(valid),
        "samples_with_exited_processes": lossy,
        "values_are_lower_bounds": bool(lossy),
    }
    if lossy:
        record["reason"] = (
            f"{lossy} sample(s) include processes that exited between samples; their "
            "unobserved final CPU, I/O and memory are missing (see process_accounting)"
        )
    if not valid:
        record["reason"] = "no valid samples overlap the timed interval"
        if metric.kind is MetricKind.INTERVAL_DELTA:
            record["total"] = None
        else:
            record.update(mean=None, peak=None)
        return record
    if metric.kind is MetricKind.INTERVAL_DELTA:
        record["total"] = sum(v * w / n for v, w, n in valid)
        record["total_is_lower_bound"] = status is not MonitoringStatus.AVAILABLE
    else:
        record["mean"] = sum(v * w for v, w, _ in valid) / covered
        record["peak"] = max(v for v, _, _ in valid)
    return record


def summarize_scope(
    rows: Sequence[Row],
    metrics: Iterable[Metric],
    start: Instant,
    end: Instant,
    clock: str = "perf",
) -> dict[str, Any]:
    """Summarise one scope's samples over ``[start, end]``.

    Raises:
        MalformedSamplesError: Inconsistent sample intervals.
        ValueError: ``end`` is before ``start``.
    """
    lo, hi = (start.perf_ns, end.perf_ns) if clock == "perf" else (start.wall_ns, end.wall_ns)
    if hi < lo:
        raise ValueError("timed interval ends before it starts")
    _check(rows, clock)
    duration = hi - lo
    weighted = []
    for row in rows:
        r_start, r_end = _bounds(row, clock)
        overlap = min(r_end, hi) - max(r_start, lo)
        if overlap > 0 or (duration == 0 and r_start <= lo <= r_end):
            weighted.append((row, max(overlap, 1), r_end - r_start))
    covered = sum(w for _, w, _ in weighted)
    effective = max(duration, 1)
    status = _status(covered, effective)
    errors = sorted({e for r, _, _ in weighted for e in r[ERRORS_COLUMN].split(";") if e})
    return {
        "status": str(status),
        "coverage_fraction": min(1.0, covered / effective),
        "covered_seconds": covered / 1e9,
        "sample_count": len(weighted),
        "sample_errors": errors,
        "metrics": {m.name: summarize_metric(m, weighted, effective) for m in metrics},
    }


def _combine(statuses: Sequence[MonitoringStatus]) -> MonitoringStatus:
    if all(s is MonitoringStatus.AVAILABLE for s in statuses):
        return MonitoringStatus.AVAILABLE
    if all(s is MonitoringStatus.UNAVAILABLE for s in statuses):
        return MonitoringStatus.UNAVAILABLE
    return MonitoringStatus.PARTIAL


def unavailable_summary(reason: str) -> dict[str, Any]:
    """Summary for an attempt whose monitoring failed entirely."""
    return {
        "schema": SUMMARY_SCHEMA,
        "monitoring_status": str(MonitoringStatus.UNAVAILABLE),
        "reason": reason,
        "whole_machine": None,
        "process_tree": None,
    }


def disabled_summary() -> dict[str, Any]:
    """Summary for an attempt run explicitly without monitoring."""
    return {
        "schema": SUMMARY_SCHEMA,
        "monitoring_status": str(MonitoringStatus.DISABLED),
        "reason": "monitoring disabled by configuration",
        "whole_machine": None,
        "process_tree": None,
    }


def summarize_monitoring(report: MonitorReport, start: Instant, end: Instant) -> dict[str, Any]:
    """Summarise both scopes of a monitor report over the timed interval ``[start, end]``."""
    clock = "perf" if report.clock_alignment == "monotonic" else "wall"
    scopes: dict[str, dict[str, Any] | None] = {}
    statuses = []
    reasons = list(report.errors)
    for key, scope, path, metrics in (
        ("whole_machine", Scope.WHOLE_MACHINE, report.machine_samples, MACHINE_METRICS),
        ("process_tree", Scope.PROCESS_TREE, report.tree_samples, TREE_METRICS),
    ):
        try:
            rows = read_samples(path, metrics)
            summary = summarize_scope(rows, metrics, start, end, clock)
        except (OSError, ValueError) as exc:
            reasons.append(f"{scope}: {type(exc).__name__}: {exc}")
            scopes[key] = None
            statuses.append(MonitoringStatus.UNAVAILABLE)
            continue
        summary["scope"] = str(scope)
        if scope is Scope.WHOLE_MACHINE:
            summary["includes_unrelated_activity"] = True
        else:
            summary["includes_unrelated_activity"] = False
            summary["observed_processes"] = sum(r["new_processes"] or 0 for r in rows)
            summary["limitations"] = [
                "processes that start and exit between samples are not observed",
                "CPU and I/O used after a process's last observation are missing",
                "descendants of exited intermediates are only found if seen earlier",
            ]
        scopes[key] = summary
        statuses.append(MonitoringStatus(summary["status"]))
    status = _combine(statuses)
    if not report.attached and status is not MonitoringStatus.UNAVAILABLE:
        reasons.append("process tree was never attached")
    return {
        "schema": SUMMARY_SCHEMA,
        "monitoring_status": str(status),
        "reasons": reasons,
        "clock": clock,
        "timed_interval": {"start": start.to_record(), "end": end.to_record()},
        "backend": report.to_record(),
        **scopes,
    }
