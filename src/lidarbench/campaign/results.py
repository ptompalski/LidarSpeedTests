"""Flat, stable results tables for analysis (``lidarbench.results-table/1``).

The authoritative records are the per-attempt ``result.json`` files; this
module reads and verifies them (digest chain, campaign identity) and flattens
each attempt into one row with a fixed column order. Abandoned attempts appear
as rows with ``record_state = "abandoned"`` and ``execution_status =
"interrupted"`` so interruptions are counted, not hidden. Nothing is
aggregated or filtered: warm-ups, failures, retries and skips are all rows,
and the ``superseded`` column marks rows of an entry that has a later attempt.

Formats: JSON Lines (default), CSV (UTF-8, empty cell for null) and Parquet
(requires the optional ``pyarrow`` package).
"""

from __future__ import annotations

import csv
import io
import json
from collections.abc import Sequence
from pathlib import Path
from typing import Any

from lidarbench.campaign.store import AttemptInfo, CampaignStore

RESULTS_TABLE_SCHEMA = "lidarbench.results-table/1"
FORMATS = ("jsonl", "csv", "parquet")

RESULT_COLUMNS: tuple[str, ...] = (
    "campaign_id",
    "identity_sha256",
    "attempt_id",
    "run_id",
    "sequence",
    "record_state",
    "superseded",
    "schedule_index",
    "entry_key",
    "phase",
    "repetition",
    "workload",
    "workload_id",
    "dataset",
    "cpus",
    "tool",
    "tool_adapter",
    "tool_version",
    "tool_position",
    "attempt_reason",
    "execution_status",
    "failure_phase",
    "failure_reason",
    "skip_reason",
    "exit_code",
    "accepted_exception",
    "timing_boundary",
    "elapsed_ns",
    "elapsed_s",
    "start_wall_utc",
    "end_wall_utc",
    "tool_workload_elapsed_ns",
    "startup_overhead_ns",
    "monitoring_status",
    "tree_cpu_seconds",
    "tree_cpu_seconds_is_lower_bound",
    "tree_peak_rss_bytes",
    "process_cpu_seconds",
    "measured_parallelism",
    "cpu_budget_exceeded",
    "cpu_affinity",
    "machine_cpu_percent_mean",
    "machine_memory_used_peak_bytes",
    "artifacts_status",
    "structural_status",
    "numerical_status",
    "validation_outcome",
    "validation_reused_from",
    "validation_duration_ns",
    "validation_mode",
    "missing_required_outputs",
    "output_files",
    "output_bytes",
    "cache_policy",
    "cooldown_s",
    "threading_model",
    "scratch_cleanup_complete",
)


def _metric(monitoring: dict[str, Any], scope: str, name: str, key: str) -> Any:
    section = monitoring.get(scope) or {}
    metric = (section.get("metrics") or {}).get(name) or {}
    return metric.get(key)


def _execution(info: AttemptInfo) -> dict[str, Any]:
    path = info.path / "execution.json"
    if not path.exists():
        return {}
    with open(path, encoding="utf-8") as handle:
        data = json.load(handle)
    return data if isinstance(data, dict) else {}


def attempt_row(info: AttemptInfo, store: CampaignStore) -> dict[str, Any]:
    """Flatten one verified attempt into a results row."""
    started = info.started or {}
    entry = started.get("entry", {})
    row: dict[str, Any] = dict.fromkeys(RESULT_COLUMNS)
    row.update(
        campaign_id=store.campaign_id,
        identity_sha256=store.identity_sha256,
        attempt_id=info.attempt_id,
        run_id=started.get("run_id"),
        sequence=started.get("sequence"),
        record_state=info.state,
        schedule_index=entry.get("schedule_index"),
        entry_key=entry.get("entry_key"),
        phase=entry.get("phase"),
        repetition=entry.get("repetition"),
        workload=entry.get("workload"),
        dataset=entry.get("dataset"),
        cpus=entry.get("cpus"),
        tool=entry.get("tool"),
        tool_position=entry.get("tool_position"),
        attempt_reason=started.get("reason"),
        execution_status=info.execution_status,
    )
    result = info.result
    if result is None:
        if info.abandoned:
            row["failure_reason"] = info.abandoned.get("reason")
        return row
    timing = result.get("timing") or {}
    tool_timing = result.get("tool_timing") or {}
    process = _execution(info).get("process") or {}
    monitoring = process.get("monitoring") or {}
    scratch = result.get("scratch") or {}
    cleanup = scratch.get("cleanup") or {}
    outputs = result.get("outputs") or []
    row.update(
        workload_id=result.get("workload_id"),
        tool_adapter=result.get("tool_adapter"),
        tool_version=result.get("tool_version"),
        failure_phase=result.get("failure_phase"),
        failure_reason=result.get("failure_reason"),
        skip_reason=result.get("skip_reason"),
        exit_code=result.get("exit_code"),
        accepted_exception=(result.get("accepted_exception") or {}).get("name"),
        timing_boundary=timing.get("timing_boundary"),
        elapsed_ns=timing.get("elapsed_ns"),
        elapsed_s=timing.get("elapsed_s"),
        start_wall_utc=(timing.get("start") or {}).get("wall_utc"),
        end_wall_utc=(timing.get("end") or {}).get("wall_utc"),
        tool_workload_elapsed_ns=tool_timing.get("workload_elapsed_ns"),
        startup_overhead_ns=tool_timing.get("startup_overhead_ns"),
        monitoring_status=result.get("monitoring_status"),
        tree_cpu_seconds=_metric(monitoring, "process_tree", "cpu_seconds", "total"),
        tree_cpu_seconds_is_lower_bound=_metric(
            monitoring, "process_tree", "cpu_seconds", "total_is_lower_bound"
        ),
        tree_peak_rss_bytes=_metric(monitoring, "process_tree", "rss_bytes", "peak"),
        machine_cpu_percent_mean=_metric(monitoring, "whole_machine", "cpu_percent", "mean"),
        machine_memory_used_peak_bytes=_metric(
            monitoring, "whole_machine", "memory_used_bytes", "peak"
        ),
        structural_status=result.get("structural_status"),
        numerical_status=result.get("numerical_status"),
        artifacts_status=result.get("artifacts_status"),
        validation_outcome=(result.get("validation") or {}).get("outcome"),
        validation_reused_from=(result.get("validation") or {}).get("reused_from"),
        validation_duration_ns=(result.get("validation") or {}).get("duration_ns"),
        validation_mode=result.get("validation_mode"),
        missing_required_outputs=len(result.get("missing_required_outputs") or []),
        output_files=len(outputs),
        output_bytes=sum(int(o.get("bytes") or 0) for o in outputs),
        cache_policy=result.get("cache_policy"),
        cooldown_s=result.get("cooldown_s"),
        threading_model=(result.get("threads") or {}).get("threading_model"),
        scratch_cleanup_complete=cleanup.get("complete") if cleanup else None,
    )
    row.update(cpu_usage(process, row["tree_cpu_seconds"], entry.get("cpus")))
    return row


BUDGET_TOLERANCE = 1.10
BUDGET_SLACK_S = 0.1


def cpu_usage(process: dict[str, Any], sampled_cpu: Any, cpus: Any) -> dict[str, Any]:
    """Exact CPU time, measured parallelism and the budget-overrun flag of one attempt.

    ``process_cpu_seconds`` is the larger of the OS accounting of waited-for
    descendants (``rusage``, exact for them) and the sampled process-tree total
    (which also sees session members that were reparented away from the tree).
    An attempt exceeds its budget when that CPU time is above
    ``cpus * elapsed * 1.10 + 0.1 s``.
    """
    accounting = process.get("process_accounting") or {}
    user, system = accounting.get("cpu_user_seconds"), accounting.get("cpu_system_seconds")
    exact = (
        float(user) + float(system)
        if isinstance(user, int | float) and isinstance(system, int | float)
        else None
    )
    candidates = [v for v in (exact, sampled_cpu) if isinstance(v, int | float)]
    cpu = max(candidates) if candidates else None
    elapsed = process.get("elapsed_s")
    parallelism = (
        cpu / float(elapsed)
        if cpu is not None and isinstance(elapsed, int | float) and elapsed > 0
        else None
    )
    exceeded = None
    if cpu is not None and isinstance(elapsed, int | float) and isinstance(cpus, int):
        exceeded = cpu > cpus * float(elapsed) * BUDGET_TOLERANCE + BUDGET_SLACK_S
    affinity = process.get("cpu_affinity") or {}
    return {
        "process_cpu_seconds": cpu,
        "measured_parallelism": parallelism,
        "cpu_budget_exceeded": exceeded,
        "cpu_affinity": (
            " ".join(str(c) for c in affinity.get("cpus") or []) or affinity.get("status")
        ),
    }


def collect_rows(campaign_path: Path) -> list[dict[str, Any]]:
    """Verify a campaign and return one row per attempt, ordered by start sequence.

    Incomplete attempts (no receipt and no tombstone) are included with
    ``record_state = "incomplete"``; they are never silently dropped.

    Raises:
        CampaignIntegrityError: A frozen record or attempt fails verification.
    """
    store = CampaignStore(campaign_path)
    store.load()
    return rows_for_attempts(store, store.attempts())


def rows_for_attempts(store: CampaignStore, infos: Sequence[AttemptInfo]) -> list[dict[str, Any]]:
    """Results rows of already verified attempts, with the ``superseded`` flag set."""
    rows = [attempt_row(info, store) for info in infos]
    latest: dict[str, int] = {}
    for row in rows:
        if row["entry_key"] is not None:
            latest[row["entry_key"]] = max(latest.get(row["entry_key"], 0), row["sequence"] or 0)
    for row in rows:
        key = row["entry_key"]
        row["superseded"] = key is not None and (row["sequence"] or 0) < latest[key]
    return rows


def _csv_value(value: Any) -> Any:
    if value is None:
        return ""
    if isinstance(value, bool):
        return "true" if value else "false"
    return value


def render_rows(rows: Sequence[dict[str, Any]], fmt: str) -> bytes:
    """Serialize rows as ``jsonl`` or ``csv`` (fixed column order)."""
    if fmt == "jsonl":
        lines = [
            json.dumps({c: row.get(c) for c in RESULT_COLUMNS}, ensure_ascii=False) for row in rows
        ]
        return ("\n".join(lines) + ("\n" if lines else "")).encode("utf-8")
    if fmt == "csv":
        buffer = io.StringIO(newline="")
        writer = csv.DictWriter(buffer, fieldnames=RESULT_COLUMNS, lineterminator="\n")
        writer.writeheader()
        for row in rows:
            writer.writerow({c: _csv_value(row.get(c)) for c in RESULT_COLUMNS})
        return buffer.getvalue().encode("utf-8")
    raise ValueError(f"unsupported text format {fmt!r}")


def write_results(rows: Sequence[dict[str, Any]], path: Path, fmt: str) -> None:
    """Write rows to a new file (refuses to overwrite).

    Raises:
        FileExistsError: ``path`` exists.
        RuntimeError: Parquet requested but ``pyarrow`` is not installed.
    """
    if fmt not in FORMATS:
        raise ValueError(f"format must be one of {FORMATS}")
    if path.exists():
        raise FileExistsError(f"refusing to overwrite {path}")
    if fmt == "parquet":
        try:
            import pyarrow as pa  # type: ignore[import-not-found]
            import pyarrow.parquet as pq  # type: ignore[import-not-found]
        except ImportError as exc:
            raise RuntimeError(
                "Parquet export needs the optional 'pyarrow' package; use jsonl or csv"
            ) from exc
        table = pa.Table.from_pylist(
            [{c: row.get(c) for c in RESULT_COLUMNS} for row in rows],
            metadata={b"schema": RESULTS_TABLE_SCHEMA.encode()},
        )
        pq.write_table(table, path)
        return
    with open(path, "xb") as handle:
        handle.write(render_rows(rows, fmt))
