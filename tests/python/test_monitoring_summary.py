"""Monitoring summary: overlap weighting, coverage, and missing values staying missing."""

from __future__ import annotations

import csv
from pathlib import Path

import pytest

from lidarbench.core.clock import Instant, iso_utc, now
from lidarbench.monitoring.base import (
    MACHINE_COLUMNS,
    MACHINE_METRICS,
    TREE_COLUMNS,
    TREE_METRICS,
    MonitorReport,
)
from lidarbench.monitoring.summary import (
    MalformedSamplesError,
    read_samples,
    summarize_monitoring,
    summarize_scope,
)

S = 1_000_000_000


def row(i: int, start_s: float, end_s: float, **values: object) -> dict[str, object]:
    return {
        "sample_index": i,
        "interval_start_perf_ns": int(start_s * S),
        "interval_end_perf_ns": int(end_s * S),
        "interval_start_wall_ns": int(start_s * S) + 10**18,
        "interval_end_wall_ns": int(end_s * S) + 10**18,
        "errors": "",
        **values,
    }


def at(seconds: float) -> Instant:
    return Instant(int(seconds * S), int(seconds * S) + 10**18)


def tree_values(cpu_s: float | None, rss: int | None, count: int = 1) -> dict[str, object]:
    return {
        "process_count": count,
        "new_processes": 0,
        "exited_processes": 0,
        "cpu_seconds": cpu_s,
        "cpu_percent": None if cpu_s is None else cpu_s * 100,
        "rss_bytes": rss,
        "io_read_bytes": None,
        "io_write_bytes": None,
    }


def write_csv(path: Path, columns: tuple[str, ...], rows: list[dict[str, object]]) -> Path:
    with open(path, "w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(columns), restval="")
        writer.writeheader()
        for r in rows:
            writer.writerow({k: "" if v is None else v for k, v in r.items()})
    return path


class TestClock:
    def test_now_is_monotonic_and_formats_utc(self):
        a, b = now(), now()
        assert b.perf_ns >= a.perf_ns
        assert iso_utc(1_500_000_000_123_456_789) == "2017-07-14T02:40:00.123456Z"
        assert a.to_record()["wall_utc"].endswith("Z")


class TestScopeSummary:
    def test_time_weighted_mean_uses_overlap_only(self):
        rows = [
            row(0, 0, 1, **tree_values(1.0, 100)),
            row(1, 1, 2, **tree_values(0.5, 300)),
        ]
        summary = summarize_scope(rows, TREE_METRICS, at(0.5), at(2.0))
        assert summary["status"] == "available"
        assert summary["coverage_fraction"] == pytest.approx(1.0)
        cpu = summary["metrics"]["cpu_seconds"]
        # 0.5 s of the first interval (1.0 cpu-s prorated) + full second interval.
        assert cpu["total"] == pytest.approx(0.5 + 0.5)
        assert cpu["total_is_lower_bound"] is False
        rss = summary["metrics"]["rss_bytes"]
        assert rss["mean"] == pytest.approx((100 * 0.5 + 300 * 1.0) / 1.5)
        assert rss["peak"] == 300

    def test_partial_coverage_is_reported_not_extrapolated(self):
        rows = [row(0, 0, 1, **tree_values(1.0, 100))]
        summary = summarize_scope(rows, TREE_METRICS, at(0), at(4))
        assert summary["status"] == "partial"
        assert summary["coverage_fraction"] == pytest.approx(0.25)
        assert summary["metrics"]["cpu_seconds"]["total"] == pytest.approx(1.0)
        assert summary["metrics"]["cpu_seconds"]["total_is_lower_bound"] is True

    def test_missing_values_are_never_zero(self):
        rows = [
            row(0, 0, 1, **tree_values(None, None)),
            row(1, 1, 2, **tree_values(None, None)),
        ]
        summary = summarize_scope(rows, TREE_METRICS, at(0), at(2))
        cpu = summary["metrics"]["cpu_seconds"]
        assert cpu["status"] == "unavailable"
        assert cpu["total"] is None
        assert summary["metrics"]["rss_bytes"]["peak"] is None
        assert summary["metrics"]["io_read_bytes"]["total"] is None
        # The process count was measured, so it is available.
        assert summary["metrics"]["process_count"]["status"] == "available"

    def test_partially_missing_metric_is_partial(self):
        rows = [
            row(0, 0, 1, **tree_values(0.2, 100)),
            row(1, 1, 2, **tree_values(None, None)),
        ]
        cpu = summarize_scope(rows, TREE_METRICS, at(0), at(2))["metrics"]["cpu_percent"]
        assert cpu["status"] == "partial"
        assert cpu["mean"] == pytest.approx(20.0)
        assert cpu["coverage_fraction"] == pytest.approx(0.5)

    def test_exited_processes_make_counters_lower_bounds(self):
        exited = {**tree_values(0.01, 0, count=0), "exited_processes": 1, "new_processes": 1}
        rows = [row(0, 0, 1, **tree_values(0.5, 100)), row(1, 1, 2, **exited)]
        metrics = summarize_scope(rows, TREE_METRICS, at(0), at(2))["metrics"]
        for name in ("cpu_seconds", "cpu_percent", "rss_bytes", "process_count"):
            assert metrics[name]["status"] == "partial", name
            assert metrics[name]["values_are_lower_bounds"] is True
            assert metrics[name]["samples_with_exited_processes"] == 1
        assert metrics["cpu_seconds"]["total_is_lower_bound"] is True
        assert metrics["new_processes"]["status"] == "available"
        assert metrics["new_processes"]["values_are_lower_bounds"] is False

    def test_no_overlap_is_unavailable(self):
        rows = [row(0, 0, 1, **tree_values(1.0, 1))]
        summary = summarize_scope(rows, TREE_METRICS, at(5), at(6))
        assert summary["status"] == "unavailable"
        assert summary["sample_count"] == 0

    @pytest.mark.parametrize(
        "rows",
        [
            [row(0, 1, 1, **tree_values(1, 1))],
            [row(0, 0, 2, **tree_values(1, 1)), row(1, 1, 3, **tree_values(1, 1))],
        ],
        ids=["empty-interval", "overlapping"],
    )
    def test_malformed_intervals_are_rejected(self, rows):
        with pytest.raises(MalformedSamplesError):
            summarize_scope(rows, TREE_METRICS, at(0), at(3))

    def test_wall_clock_alignment(self):
        rows = [row(0, 0, 2, **tree_values(2.0, 1))]
        summary = summarize_scope(rows, TREE_METRICS, at(0), at(1), clock="wall")
        assert summary["metrics"]["cpu_seconds"]["total"] == pytest.approx(1.0)


class TestReportSummary:
    def make_report(self, tmp_path: Path, machine: list, tree: list) -> MonitorReport:
        return MonitorReport(
            "psutil",
            "a",
            0.5,
            tmp_path,
            write_csv(tmp_path / "m.csv", MACHINE_COLUMNS, machine),
            write_csv(tmp_path / "t.csv", TREE_COLUMNS, tree),
            attached=True,
        )

    def machine(self, i, a, b, cpu=50.0):
        return row(
            i,
            a,
            b,
            cpu_percent=cpu,
            memory_used_bytes=10,
            memory_available_bytes=90,
            memory_total_bytes=100,
            disk_read_bytes=None,
            disk_write_bytes=None,
        )

    def test_scopes_are_kept_separate_and_labelled(self, tmp_path):
        report = self.make_report(
            tmp_path,
            [self.machine(0, -1, 1), self.machine(1, 1, 3, cpu=10.0)],
            [row(0, 0, 3, **tree_values(3.0, 5))],
        )
        summary = summarize_monitoring(report, at(0), at(2))
        assert summary["monitoring_status"] == "available"
        assert summary["whole_machine"]["scope"] == "whole-machine"
        assert summary["whole_machine"]["includes_unrelated_activity"] is True
        assert summary["process_tree"]["includes_unrelated_activity"] is False
        assert summary["whole_machine"]["metrics"]["cpu_percent"]["mean"] == pytest.approx(30.0)
        assert (
            summary["whole_machine"]["metrics"]["cpu_percent"]["unit"]
            == "percent_of_all_logical_cpus"
        )
        assert (
            summary["process_tree"]["metrics"]["cpu_percent"]["unit"]
            == "percent_of_one_logical_cpu"
        )
        assert summary["whole_machine"]["metrics"]["disk_read_bytes"]["total"] is None

    def test_empty_tree_samples_make_monitoring_partial(self, tmp_path):
        report = self.make_report(tmp_path, [self.machine(0, 0, 2)], [])
        summary = summarize_monitoring(report, at(0), at(2))
        assert summary["monitoring_status"] == "partial"
        assert summary["process_tree"]["status"] == "unavailable"

    def test_unreadable_samples_are_unavailable(self, tmp_path):
        report = self.make_report(tmp_path, [], [])
        report.machine_samples.unlink()
        report.tree_samples.write_text("garbage\n1\n", encoding="utf-8")
        summary = summarize_monitoring(report, at(0), at(1))
        assert summary["monitoring_status"] == "unavailable"
        assert summary["whole_machine"] is None and summary["process_tree"] is None
        assert len(summary["reasons"]) == 2

    def test_read_samples_keeps_empty_cells_as_none(self, tmp_path):
        path = write_csv(tmp_path / "t.csv", TREE_COLUMNS, [row(0, 0, 1, **tree_values(None, 7))])
        (parsed,) = read_samples(path, TREE_METRICS)
        assert parsed["cpu_seconds"] is None
        assert parsed["rss_bytes"] == 7.0

    def test_machine_metrics_declared(self):
        assert {m.name for m in MACHINE_METRICS} >= {"cpu_percent", "memory_used_bytes"}
