"""Read-only import of historical R results (legacy CSVs and R campaign exports)."""

from __future__ import annotations

import json
import os
import shutil
import subprocess
from pathlib import Path

import pytest

from lidarbench.core.canonical import sha256_file
from lidarbench.reporting.dataset import ReportError, ReportPolicy
from lidarbench.reporting.legacy import (
    CAMPAIGN_DEFINITION,
    CSV_DEFINITION,
    LEGACY_EXPORT_SCHEMA,
    load_historical,
)

REPO = Path(__file__).resolve().parents[2]
HEADER = (
    '"","tool","success","start_time","finish_time","output_file_count","output_total_size",'
    '"task_id","run_id","cores","nodename","sysname","release","cpu","cpu_threads","ram",'
    '"Rversion","lidR_version","lasR_version","lastools_version"\n'
)


def _legacy_row(tool: str, success: int, start: str, finish: str, version: str) -> str:
    versions = {"lidR": "", "lasR": "", "lastools": ""}
    versions[tool] = version
    return (
        f'"1","{tool}",{success},{start},{finish},100,187M,"generate_DEM",'
        f'"ALS_1000_10_16_HDD_HDD",16,"HOST-A","Windows","10 x64","Xeon",40,68639309824,'
        f'"R version 4.3.3",{versions["lidR"]},{versions["lasR"]},{versions["lastools"]}\n'
    )


@pytest.fixture
def legacy_dir(tmp_path: Path) -> Path:
    directory = tmp_path / "results"
    directory.mkdir()
    rows = {
        "a_lidR.csv": _legacy_row(
            "lidR", 1, "2024-05-10 06:53:42.5", "2024-05-10 06:57:10.5", "4.1.1"
        ),
        "b_lidR.csv": _legacy_row("lidR", 1, "2024-05-11 06:00:00", "2024-05-11 06:03:00", "4.1.2"),
        "c_lasR.csv": _legacy_row("lasR", 0, "2024-05-10 07:00:00", "2024-05-10 07:00:05", "0.5.3"),
        "d_lasR.csv": _legacy_row(
            "lasR", 1, "2024-05-10 07:00:00", "2024-05-10 07:00:00.05", "0.5.3"
        ),
    }
    for name, row in rows.items():
        (directory / name).write_text(HEADER + row, encoding="utf-8")
    (directory / "notes.txt").write_text("ignored", encoding="utf-8")
    return directory


def test_legacy_csvs_are_imported_verbatim_and_never_mapped(legacy_dir: Path):
    before = {p.name: sha256_file(p) for p in legacy_dir.iterdir()}
    tables = load_historical([legacy_dir], ReportPolicy("performance-only"))
    assert {p.name: sha256_file(p) for p in legacy_dir.iterdir()} == before
    rows = tables["historical_attempts"].rows
    assert len(rows) == 4
    assert {r["legacy_task_id"] for r in rows} == {"generate_DEM"}
    assert {r["workload_definition"] for r in rows} == {CSV_DEFINITION}
    assert {r["validation_status"] for r in rows} == {"unverified"}
    first = next(r for r in rows if r["source_file"] == "a_lidR.csv")
    assert first["elapsed_s"] == pytest.approx(208.0)
    assert first["timing_precision"] == "sub-second CSV timestamps"
    assert first["tool_version"] == "4.1.1" and first["ram_bytes"] == 68639309824
    assert first["source_sha256"] == before["a_lidR.csv"]
    exclusions = {r["source_file"]: r["exclusion"] for r in rows}
    assert exclusions["c_lasR.csv"] == "execution: failed"
    assert exclusions["d_lasR.csv"] == "timing below 0.1 s"
    source = tables["historical_sources"].rows[0]
    assert (source["source_kind"], source["files"], source["rows"]) == ("legacy-csv", 4, 4)


def test_legacy_summary_never_pools_tool_versions(legacy_dir: Path):
    tables = load_historical([legacy_dir], ReportPolicy("performance-only"))
    lidr = [r for r in tables["historical_summary"].rows if r["tool"] == "lidR"]
    assert sorted(r["tool_version"] for r in lidr) == ["4.1.1", "4.1.2"]
    assert all(r["n_included"] == 1 and r["sd_seconds"] is None for r in lidr)


def test_campaign_records_are_refused_as_legacy_csvs(tmp_path: Path):
    directory = tmp_path / "results"
    directory.mkdir()
    (directory / "x.csv").write_text("campaign_id,tool\nc,lidR\n", encoding="utf-8")
    with pytest.raises(ReportError, match="campaign records"):
        load_historical([directory], ReportPolicy("validated"))
    (directory / "x.csv").unlink()
    with pytest.raises(ReportError, match="no legacy result CSVs"):
        load_historical([directory], ReportPolicy("validated"))
    (directory / "y.csv").write_text("tool,success\nlidR,1\n", encoding="utf-8")
    with pytest.raises(ReportError, match="missing"):
        load_historical([directory], ReportPolicy("validated"))
    with pytest.raises(ReportError, match="neither"):
        load_historical([tmp_path / "missing"], ReportPolicy("validated"))


def _export(tmp_path: Path, records: list[dict], **extra) -> Path:
    path = tmp_path / "export.json"
    record = {
        "schema": LEGACY_EXPORT_SCHEMA,
        "campaign_id": "legacy-fixture",
        "identity_id": "abc",
        "validation_mode": "numerical",
        "exported_at": "2026-10-22T00:00:00Z",
        "exporter": {"r_version": "R version 4.6.1"},
        "records": records,
        **extra,
    }
    path.write_text(json.dumps(record), encoding="utf-8")
    return path


def _record(attempt: str, order: float, repetition: int = 1, **fields) -> dict:
    base = {
        "attempt_id": attempt,
        "task_id": "pixel_metrics_1a",
        "run_id": "ALS_1000_1_1_SSD_SSD",
        "tool": "lidR",
        "phase": "measurement",
        "repetition_id": repetition,
        "execution_status": "succeeded",
        "success": 1,
        "start_time": "2026-10-22T12:00:00.000000Z",
        "finish_time": "2026-10-22T12:00:02.000000Z",
        "validation_status": "passed",
        "lidR_version": "4.3.3",
        "started_recorded_at": order,
    }
    return {**base, **fields}


def test_r_campaign_export_reuses_the_legacy_selection_and_exclusions(tmp_path: Path):
    path = _export(
        tmp_path,
        [
            _record("a1", 1.0, execution_status="failed", success=0),
            _record("a2", 2.0),
            _record("a3", 3.0, repetition=2, validation_status="failed"),
            _record(
                "a4",
                4.0,
                repetition=3,
                validation_status="unverified",
                structural_validation_passed="TRUE",
                validation_report_md5="m",
            ),
            _record("a5", 5.0, phase="warmup"),
            _record(
                "a6",
                6.0,
                repetition=4,
                monitoring_timed_start_epoch=10.0,
                monitoring_timed_finish_epoch=13.5,
            ),
        ],
    )
    validated = load_historical([path], ReportPolicy("validated"))
    rows = {r["attempt_id"]: r for r in validated["historical_attempts"].rows}
    assert rows["a1"]["exclusion"] == "superseded retry" and rows["a1"]["superseded"] is True
    assert rows["a2"]["exclusion"] == "included" and rows["a2"]["elapsed_s"] == 2.0
    assert rows["a2"]["timing_precision"] == "CSV timestamps (second resolution)"
    assert rows["a3"]["exclusion"] == "validation: failed"
    assert rows["a4"]["exclusion"] == "validation: unverified"
    assert rows["a5"]["exclusion"] == "warm-up"
    assert rows["a6"]["elapsed_s"] == 3.5
    assert rows["a6"]["timing_precision"] == "monitor-bound sub-second epochs"
    assert {r["workload_definition"] for r in rows.values()} == {CAMPAIGN_DEFINITION}
    performance = load_historical([path], ReportPolicy("performance-only"))
    rows = {r["attempt_id"]: r for r in performance["historical_attempts"].rows}
    assert rows["a4"]["exclusion"] == "included"
    summary = performance["historical_summary"].rows
    assert len(summary) == 1 and summary[0]["n_included"] == 3


def test_r_campaign_export_must_have_the_schema(tmp_path: Path):
    bad = tmp_path / "bad.json"
    bad.write_text(json.dumps({"schema": "other"}), encoding="utf-8")
    with pytest.raises(ReportError, match=LEGACY_EXPORT_SCHEMA):
        load_historical([bad], ReportPolicy("validated"))
    with pytest.raises(ReportError, match="attempt_id"):
        load_historical([_export(tmp_path, [{"tool": "lidR"}])], ReportPolicy("validated"))


def _r_ready() -> bool:
    rscript = shutil.which("Rscript")
    if rscript is None or not (REPO / "renv" / "activate.R").is_file():
        return False
    code = (
        'source("renv/activate.R"); q(status = as.integer(!all(vapply(c("readr", "dplyr", '
        '"testthat", "withr", "tidyr", "purrr", "forcats", "stringr", "glue", "magrittr"), '
        "requireNamespace, logical(1), quietly = TRUE))))"
    )
    try:
        out = subprocess.run(
            [rscript, "--vanilla", "-e", code],
            cwd=REPO,
            capture_output=True,
            timeout=120,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return False
    return out.returncode == 0


@pytest.mark.integration
@pytest.mark.skipif(os.environ.get("LIDARBENCH_SKIP_R") == "1", reason="R tests disabled")
def test_r_exporter_round_trip(tmp_path: Path):
    if not _r_ready():
        pytest.skip("Rscript with the renv library (readr, dplyr, testthat) is not available")
    reports = tmp_path / "reports"
    fixture = subprocess.run(
        [
            "Rscript",
            "--vanilla",
            str(REPO / "tests/python/r_legacy_campaign_fixture.R"),
            str(REPO),
            str(reports),
        ],
        capture_output=True,
        text=True,
        timeout=600,
        check=False,
    )
    assert fixture.returncode == 0, fixture.stderr
    before = {p: sha256_file(p) for p in reports.rglob("*") if p.is_file()}
    output = tmp_path / "export.json"
    command = [
        "Rscript",
        "--vanilla",
        str(REPO / "bridges/r/export_legacy_campaign.R"),
        "--project",
        str(REPO),
        "--renv",
        "on",
        "--reports",
        str(reports),
        "--campaign",
        "legacy-fixture",
        "--output",
        str(output),
    ]
    run = subprocess.run(command, capture_output=True, text=True, timeout=600, check=False)
    assert run.returncode == 0, run.stderr
    assert {p: sha256_file(p) for p in reports.rglob("*") if p.is_file()} == before
    again = subprocess.run(command, capture_output=True, text=True, timeout=600, check=False)
    assert again.returncode == 2 and "refusing to overwrite" in again.stderr
    tables = load_historical([output], ReportPolicy("validated"))
    rows = tables["historical_attempts"].rows
    assert len(rows) == 5
    exclusions = sorted(r["exclusion"] for r in rows)
    assert exclusions == [
        "included",
        "included",
        "superseded retry",
        "validation: failed",
        "warm-up",
    ]
    assert {r["legacy_campaign_id"] for r in rows} == {"legacy-fixture"}
    summary = tables["historical_summary"].rows[0]
    assert summary["n_included"] == 2 and summary["median_seconds"] == pytest.approx(3.0)
    # Tampering is detected by the harness's own reader; nothing is exported.
    attempt = next((reports / "campaigns" / "legacy-fixture" / "attempts").iterdir())
    result = attempt / "result.csv"
    result.chmod(0o644)
    result.write_text(result.read_text() + "\n", encoding="utf-8")
    bad = subprocess.run(
        [*command[:-1], str(tmp_path / "bad.json")],
        capture_output=True,
        text=True,
        timeout=600,
        check=False,
    )
    assert bad.returncode == 1 and not (tmp_path / "bad.json").exists()
