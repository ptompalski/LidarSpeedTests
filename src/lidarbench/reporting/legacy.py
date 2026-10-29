"""Read-only import of historical R benchmark results.

Two sources are supported, both read without modification:

``legacy-csv``
    A directory of the original per-run result CSVs that fed the published
    site (``<host>_<task>_<run>_<tool>.csv``, one row per run; no campaign
    identity, no validation). Read with the stdlib only, as in the legacy
    ``R/4_compileResults.R`` "legacy" mode.
``r-campaign``
    A JSON export (``lidarbench.legacy-r-campaign/1``) of a campaign of the
    legacy R harness, written by ``bridges/r/export_legacy_campaign.R``, which
    verifies every receipt with the harness's own ``benchmark_read_campaign()``
    before exporting. R campaigns store RDS receipts, which Python cannot
    verify, so this route needs R once; reporting the export does not.

Historical workloads are **never** mapped onto lidarbench workload contracts:
the legacy ``task_id`` is kept verbatim as ``legacy_task_id`` with an explicit
``workload_definition`` label, nothing is validated, and historical rows never
enter comparisons, scaling ratios or the campaign tables. Rows are summarized
per host, task, run, tool and tool version (versions are never pooled).
"""

from __future__ import annotations

import csv
import json
import math
from collections.abc import Iterable, Mapping, Sequence
from datetime import datetime
from pathlib import Path
from typing import Any

from lidarbench.core.canonical import digest, sha256_file
from lidarbench.reporting.dataset import ReportError, ReportPolicy, Table
from lidarbench.reporting.stats import describe

LEGACY_EXPORT_SCHEMA = "lidarbench.legacy-r-campaign/1"
CSV_DEFINITION = (
    "legacy task_id from a historical per-run CSV; definition as in the R harness at the "
    "time, not mapped to a lidarbench workload contract"
)
CAMPAIGN_DEFINITION = (
    "legacy R harness task_id (workloads-v1 era); not mapped to a lidarbench workload contract"
)
LEGACY_CSV_REQUIRED = ("tool", "success", "start_time", "finish_time", "task_id", "run_id")
CAMPAIGN_FIELDS = ("campaign_id", "attempt_id", "identity_id")

HISTORICAL_COLUMNS = (
    "source_kind",
    "source",
    "source_file",
    "source_sha256",
    "source_row",
    "legacy_campaign_id",
    "legacy_identity_id",
    "attempt_id",
    "legacy_task_id",
    "workload_definition",
    "run_id",
    "tool",
    "tool_version",
    "cores",
    "phase",
    "repetition",
    "host",
    "sysname",
    "release",
    "cpu",
    "cpu_threads",
    "ram_bytes",
    "r_version",
    "success",
    "execution_status",
    "start_time",
    "finish_time",
    "elapsed_s",
    "timing_precision",
    "validation_mode",
    "validation_status",
    "structural_validation_passed",
    "comparison_status",
    "output_file_count",
    "superseded",
    "report_included",
    "exclusion",
)

HISTORICAL_SUMMARY_COLUMNS = (
    "source_kind",
    "source",
    "legacy_campaign_id",
    "host",
    "legacy_task_id",
    "run_id",
    "tool",
    "tool_version",
    "cores",
    "n_records",
    "n_measurements",
    "n_included",
    "n_excluded",
    "n_failed",
    "mean_seconds",
    "sd_seconds",
    "cv",
    "median_seconds",
    "q1_seconds",
    "q3_seconds",
    "min_seconds",
    "max_seconds",
    "validation_statuses",
    "timing_precision",
    "exclusions",
)

HISTORICAL_SOURCE_COLUMNS = ("source_kind", "source", "files", "rows", "sha256", "detail")

HISTORICAL_TABLE_SPECS = {
    "historical_attempts": (
        HISTORICAL_COLUMNS,
        "Historical R results, one row per record, read-only; legacy task IDs are kept verbatim.",
    ),
    "historical_summary": (
        HISTORICAL_SUMMARY_COLUMNS,
        "Historical timings per source, host, task, run, tool and tool version (descriptive).",
    ),
    "historical_sources": (
        HISTORICAL_SOURCE_COLUMNS,
        "Imported historical sources and their SHA-256 digests.",
    ),
}


def new_historical_tables() -> dict[str, Table]:
    """Empty historical tables."""
    return {n: Table(n, cols, desc) for n, (cols, desc) in HISTORICAL_TABLE_SPECS.items()}


def _text(value: Any) -> str | None:
    if value is None:
        return None
    text = str(value).strip()
    return None if text in ("", "NA", "NaN") else text


def _number(value: Any) -> float | int | None:
    text = _text(value)
    if text is None:
        return None
    try:
        number = float(text)
    except ValueError:
        return None
    if not math.isfinite(number):
        return None
    return int(number) if number.is_integer() and "." not in text and "e" not in text else number


def _bool(value: Any) -> bool | None:
    text = _text(value)
    if text is None:
        return None
    return {"TRUE": True, "true": True, "1": True, "FALSE": False, "false": False, "0": False}.get(
        text
    )


def _time(value: Any) -> datetime | None:
    text = _text(value)
    if text is None:
        return None
    try:
        return datetime.fromisoformat(text)
    except ValueError:
        return None


def _elapsed(start: Any, finish: Any) -> tuple[float | None, str | None]:
    a, b = _time(start), _time(finish)
    if a is None or b is None or (a.tzinfo is None) != (b.tzinfo is None):
        return None, None
    fractional = "." in str(start) or "." in str(finish)
    precision = "sub-second CSV timestamps" if fractional else "second-resolution CSV timestamps"
    return (b - a).total_seconds(), precision


# -- legacy per-run CSVs ------------------------------------------------------------------


def read_legacy_csv_dir(path: Path, policy: ReportPolicy, tables: dict[str, Table]) -> None:
    """Import every ``*.csv`` in ``path`` (non-recursive) into ``tables``.

    Raises:
        ReportError: No CSVs, campaign records presented as legacy CSVs, or a
            required column is missing.
    """
    files = sorted(p for p in path.iterdir() if p.is_file() and p.suffix.lower() == ".csv")
    if not files:
        raise ReportError(f"no legacy result CSVs found in {path}")
    source = path.resolve().name
    digests: dict[str, str] = {}
    rows = tables["historical_attempts"]
    count = 0
    for file in files:
        sha = sha256_file(file)
        digests[file.name] = sha
        with open(file, encoding="utf-8", newline="") as handle:
            reader = csv.DictReader(handle)
            header = reader.fieldnames or []
            if any(f in header for f in CAMPAIGN_FIELDS):
                raise ReportError(
                    f"{file}: campaign records cannot be imported as legacy results; "
                    "export the R campaign with bridges/r/export_legacy_campaign.R"
                )
            missing = [c for c in LEGACY_CSV_REQUIRED if c not in header]
            if missing:
                raise ReportError(f"{file}: not a legacy result CSV (missing {missing})")
            for index, record in enumerate(reader, start=1):
                count += 1
                tool = _text(record.get("tool"))
                success = _number(record.get("success"))
                status = (
                    {1: "succeeded", 0: "failed"}.get(int(success)) if success in (0, 1) else None
                )
                elapsed, precision = _elapsed(record.get("start_time"), record.get("finish_time"))
                exclusion = _legacy_exclusion(status, elapsed, policy)
                rows.add(
                    source_kind="legacy-csv",
                    source=source,
                    source_file=file.name,
                    source_sha256=sha,
                    source_row=index,
                    legacy_task_id=_text(record.get("task_id")),
                    workload_definition=CSV_DEFINITION,
                    run_id=_text(record.get("run_id")),
                    tool=tool,
                    tool_version=_text(record.get(f"{tool}_version")) if tool else None,
                    cores=_number(record.get("cores")),
                    phase="measurement",
                    host=_text(record.get("nodename")),
                    sysname=_text(record.get("sysname")),
                    release=_text(record.get("release")),
                    cpu=_text(record.get("cpu")),
                    cpu_threads=_number(record.get("cpu_threads")),
                    ram_bytes=_number(record.get("ram")),
                    r_version=_text(record.get("Rversion")),
                    success=success,
                    execution_status=status,
                    start_time=_text(record.get("start_time")),
                    finish_time=_text(record.get("finish_time")),
                    elapsed_s=elapsed,
                    timing_precision=precision,
                    validation_status="unverified",
                    comparison_status="legacy-unidentified",
                    output_file_count=_number(record.get("output_file_count")),
                    superseded=False,
                    report_included=exclusion == "included",
                    exclusion=exclusion,
                )
    tables["historical_sources"].add(
        source_kind="legacy-csv",
        source=source,
        files=len(files),
        rows=count,
        sha256=digest(digests),
        detail="sha256 of canonical JSON {file name: sha256}; no campaign identity, "
        "no validation; success=1 is reported as succeeded",
    )


def _legacy_exclusion(status: str | None, elapsed: float | None, policy: ReportPolicy) -> str:
    if status != "succeeded":
        return f"execution: {status or 'unknown'}"
    if elapsed is None or not math.isfinite(elapsed):
        return "missing or unparseable timing"
    if elapsed < policy.min_seconds:
        return f"timing below {policy.min_seconds} s"
    return "included"


# -- exported R-harness campaigns ---------------------------------------------------------


def read_r_campaign_export(path: Path, policy: ReportPolicy, tables: dict[str, Table]) -> None:
    """Import a ``lidarbench.legacy-r-campaign/1`` export.

    Raises:
        ReportError: Not an export, or a record lacks identifying fields.
    """
    try:
        with open(path, encoding="utf-8") as handle:
            export = json.load(handle)
    except (OSError, ValueError) as exc:
        raise ReportError(f"{path}: cannot read R campaign export: {exc}") from None
    if not isinstance(export, dict) or export.get("schema") != LEGACY_EXPORT_SCHEMA:
        raise ReportError(f"{path}: not a {LEGACY_EXPORT_SCHEMA} export")
    records = export.get("records")
    if not isinstance(records, list):
        raise ReportError(f"{path}: export has no records list")
    sha = sha256_file(path)
    campaign_id = export.get("campaign_id")
    identity_id = export.get("identity_id")
    validation_mode = export.get("validation_mode")
    keyed: list[tuple[tuple[Any, ...], float, str, Mapping[str, Any]]] = []
    for record in records:
        if not isinstance(record, dict) or not _text(record.get("attempt_id")):
            raise ReportError(f"{path}: record without attempt_id")
        key = tuple(
            _text(record.get(f)) for f in ("task_id", "run_id", "tool", "phase", "repetition_id")
        )
        order = _number(record.get("started_recorded_at"))
        keyed.append(
            (
                key,
                float(order) if order is not None else -math.inf,
                str(record["attempt_id"]),
                record,
            )
        )
    latest: dict[tuple[Any, ...], tuple[float, str]] = {}
    for key, order, attempt, _ in keyed:
        latest[key] = max(latest.get(key, (-math.inf, "")), (order, attempt))
    rows = tables["historical_attempts"]
    for index, (key, order, attempt, record) in enumerate(keyed, start=1):
        tool = _text(record.get("tool"))
        phase = _text(record.get("phase")) or "measurement"
        superseded = latest[key] != (order, attempt)
        start_epoch = _number(record.get("monitoring_timed_start_epoch"))
        finish_epoch = _number(record.get("monitoring_timed_finish_epoch"))
        if start_epoch is not None and finish_epoch is not None:
            elapsed: float | None = float(finish_epoch) - float(start_epoch)
            precision: str | None = "monitor-bound sub-second epochs"
        else:
            elapsed, _ = _elapsed(record.get("start_time"), record.get("finish_time"))
            precision = "CSV timestamps (second resolution)" if elapsed is not None else None
        status = _text(record.get("execution_status"))
        validation = _text(record.get("validation_status"))
        structural = _bool(record.get("structural_validation_passed"))
        exclusion = _campaign_exclusion(
            phase,
            superseded,
            status,
            _number(record.get("success")),
            validation,
            structural,
            _text(record.get("validation_report_md5")),
            elapsed,
            policy,
        )
        rows.add(
            source_kind="r-campaign",
            source=path.name,
            source_file=path.name,
            source_sha256=sha,
            source_row=index,
            legacy_campaign_id=campaign_id,
            legacy_identity_id=identity_id,
            attempt_id=attempt,
            legacy_task_id=_text(record.get("task_id")),
            workload_definition=CAMPAIGN_DEFINITION,
            run_id=_text(record.get("run_id")),
            tool=tool,
            tool_version=_text(record.get(f"{tool}_version")) if tool else None,
            cores=_number(record.get("cores")),
            phase=phase,
            repetition=_number(record.get("repetition_id")),
            host=_text(record.get("nodename")),
            sysname=_text(record.get("sysname")),
            release=_text(record.get("release")),
            cpu=_text(record.get("cpu")),
            cpu_threads=_number(record.get("cpu_threads")),
            ram_bytes=_number(record.get("ram")),
            r_version=_text(record.get("Rversion")),
            success=_number(record.get("success")),
            execution_status=status,
            start_time=_text(record.get("start_time")),
            finish_time=_text(record.get("finish_time")),
            elapsed_s=elapsed,
            timing_precision=precision,
            validation_mode=_text(record.get("validation_mode")) or validation_mode,
            validation_status=validation,
            structural_validation_passed=structural,
            comparison_status=_text(record.get("comparison_status")),
            output_file_count=_number(record.get("output_file_count")),
            superseded=superseded,
            report_included=exclusion == "included",
            exclusion=exclusion,
        )
    tables["historical_sources"].add(
        source_kind="r-campaign",
        source=path.name,
        files=1,
        rows=len(records),
        sha256=sha,
        detail=f"campaign {campaign_id} (identity {identity_id}), exported "
        f"{export.get('exported_at')} by {export.get('exporter', {}).get('r_version')}; "
        "receipts verified by benchmark_read_campaign() at export",
    )


def _campaign_exclusion(
    phase: str,
    superseded: bool,
    status: str | None,
    success: float | int | None,
    validation: str | None,
    structural: bool | None,
    validation_md5: str | None,
    elapsed: float | None,
    policy: ReportPolicy,
) -> str:
    """Exclusion rule of the legacy ``benchmark_report_prepare()`` (same order)."""
    if phase != "measurement":
        return "warm-up"
    if superseded:
        return "superseded retry"
    if status != "succeeded" or success != 1:
        return f"execution: {status or 'unknown'}"
    eligible = validation in ("passed", "accepted_exception") or (
        policy.mode == "performance-only"
        and validation == "unverified"
        and structural is True
        and bool(validation_md5)
    )
    if not eligible:
        return f"validation: {validation or 'not recorded'}"
    if elapsed is None or not math.isfinite(elapsed):
        return "missing or unparseable timing"
    if elapsed < policy.min_seconds:
        return f"timing below {policy.min_seconds} s"
    return "included"


# -- summary --------------------------------------------------------------------------


def summarize_historical(tables: dict[str, Table]) -> None:
    """Fill ``historical_summary`` from ``historical_attempts``."""
    groups: dict[tuple[Any, ...], list[dict[str, Any]]] = {}
    fields = (
        "source_kind",
        "source",
        "legacy_campaign_id",
        "host",
        "legacy_task_id",
        "run_id",
        "tool",
        "tool_version",
        "cores",
    )
    for row in tables["historical_attempts"].rows:
        groups.setdefault(tuple(row[f] for f in fields), []).append(row)
    summary = tables["historical_summary"]
    for key in sorted(groups, key=lambda k: tuple("" if v is None else str(v) for v in k)):
        rows = groups[key]
        measured = [r for r in rows if r["phase"] == "measurement" and not r["superseded"]]
        included = [r for r in measured if r["report_included"]]
        t = describe(r["elapsed_s"] for r in included)
        exclusions: dict[str, int] = {}
        for r in measured:
            if r["exclusion"] != "included":
                exclusions[r["exclusion"]] = exclusions.get(r["exclusion"], 0) + 1
        summary.add(
            **dict(zip(fields, key, strict=True)),
            n_records=len(rows),
            n_measurements=len(measured),
            n_included=len(included),
            n_excluded=len(measured) - len(included),
            n_failed=sum(1 for r in measured if r["execution_status"] != "succeeded"),
            mean_seconds=t["mean"],
            sd_seconds=t["sd"],
            cv=t["cv"],
            median_seconds=t["median"],
            q1_seconds=t["q1"],
            q3_seconds=t["q3"],
            min_seconds=t["min"],
            max_seconds=t["max"],
            validation_statuses=_join(r["validation_status"] for r in measured),
            timing_precision=_join(r["timing_precision"] for r in included),
            exclusions="; ".join(f"{k} ({n})" for k, n in sorted(exclusions.items())) or None,
        )


def _join(values: Iterable[Any]) -> str | None:
    return "; ".join(sorted({str(v) for v in values if v is not None})) or None


def load_historical(sources: Sequence[Path], policy: ReportPolicy) -> dict[str, Table]:
    """Import historical sources (directories of CSVs or R campaign exports).

    Raises:
        ReportError: A source is missing or not recognized.
    """
    tables = new_historical_tables()
    for source in sources:
        path = Path(source)
        if path.is_dir():
            read_legacy_csv_dir(path, policy, tables)
        elif path.is_file() and path.suffix.lower() == ".json":
            read_r_campaign_export(path, policy, tables)
        else:
            raise ReportError(
                f"historical source {path} is neither a directory of legacy CSVs nor a "
                f"{LEGACY_EXPORT_SCHEMA} JSON export"
            )
    summarize_historical(tables)
    return tables
