"""``lidarbench report``: outputs, safety checks and optional Quarto/R rendering."""

from __future__ import annotations

import io
import json
import shutil
import sys
from pathlib import Path

import pytest
from report_fixtures import make_campaign, writable_copy

from lidarbench import cli
from lidarbench.core.canonical import sha256_file
from lidarbench.core.locks import WORKSPACE_LOCK_NAME, ExclusiveLock
from lidarbench.reporting.commands import report_command, snapshot
from lidarbench.reporting.site import TEMPLATE_FILES, probe_render

posix_only = pytest.mark.skipif(sys.platform == "win32", reason="fake executables are scripts")

FAKE_QUARTO = """#!{python}
import os, pathlib, sys
if sys.argv[1:] == ["--version"]:
    print("9.9.9")
    sys.exit(0)
if os.environ.get("FAKE_QUARTO_FAIL"):
    print("render exploded", file=sys.stderr)
    sys.exit(1)
assert os.environ["QUARTO_R"] and os.environ.get("R_LIBS")
site = pathlib.Path("_site")
site.mkdir()
(site / "index.html").write_text("<html></html>")
print("Output created: _site/index.html")
"""

FAKE_RSCRIPT = """#!{python}
import os
missing = set(filter(None, os.environ.get("FAKE_R_MISSING", "").split(",")))
print("R_VERSION\\tR version 9.9.9 (fake)")
print("LIB\\t/fake/library")
for name in os.environ["LIDARBENCH_PACKAGES"].split(","):
    ok = name not in missing
    print(f"PKG\\t{{name}}\\t{{'TRUE' if ok else 'FALSE'}}\\t{{'1.0' if ok else ''}}")
"""


@pytest.fixture(scope="module")
def campaign(tmp_path_factory: pytest.TempPathFactory) -> Path:
    return make_campaign(tmp_path_factory.mktemp("cmd"))


@pytest.fixture
def fakes(tmp_path: Path) -> tuple[str, str]:
    paths = []
    for name, text in (("quarto", FAKE_QUARTO), ("Rscript", FAKE_RSCRIPT)):
        path = tmp_path / "bin" / name
        path.parent.mkdir(exist_ok=True)
        path.write_text(text.format(python=sys.executable), encoding="utf-8")
        path.chmod(0o755)
        paths.append(str(path))
    return paths[0], paths[1]


def _run(campaign: Path | None, **kwargs) -> tuple[int, str, str]:
    out, err = io.StringIO(), io.StringIO()
    code = report_command(str(campaign) if campaign else None, out=out, err=err, **kwargs)
    return code, out.getvalue(), err.getvalue()


def test_report_writes_tables_site_sources_and_receipt(campaign: Path, tmp_path: Path):
    before = snapshot(campaign)
    output = tmp_path / "report"
    code, out, err = _run(campaign, output=str(output), render="never", min_seconds=0.0)
    assert code == 0, err
    assert snapshot(campaign) == before
    for name in (*TEMPLATE_FILES, "summary.md", "report-dataset.json", "R/reporting.R"):
        assert (output / name).is_file(), name
    for table in (
        "attempts",
        "summary",
        "repetitions",
        "comparisons",
        "scaling",
        "issues",
        "environment",
        "tools",
        "tool_details",
        "datasets",
        "implementations",
        "report_meta",
        "table_index",
    ):
        assert (output / "tables" / f"{table}.csv").is_file(), table
    jsonl = (output / "tables" / "results-table.jsonl").read_text(encoding="utf-8")
    assert len(jsonl.splitlines()) == len(list((campaign / "attempts").iterdir()))
    receipt = json.loads((output / "report.json").read_text(encoding="utf-8"))
    assert receipt["schema"] == "lidarbench.report/1"
    assert receipt["campaign_records"]["file_sha256"] == before
    assert receipt["render"]["status"] == "not_requested"
    assert receipt["policy"]["mode"] == "performance-only"  # from the campaign's validation_mode
    for entry in receipt["files"].values():
        assert sha256_file(output / entry["file"]) == entry["sha256"]
    metadata = json.loads((output / "report-dataset.json").read_text(encoding="utf-8"))
    assert metadata["schema"] == "lidarbench.report-dataset/1"
    assert metadata["tables"]["summary"]["columns"][0] == "workload"
    assert "Measurement units" in (output / "summary.md").read_text(encoding="utf-8")
    assert "included measurements" in out and "not rendered" in out


def test_default_output_is_a_new_directory_in_the_workspace(campaign: Path, tmp_path: Path):
    copy = writable_copy(campaign, tmp_path / "ws" / "campaigns" / "test-campaign")
    code, _, _ = _run(copy, render="never", mode="validated")
    assert code == 0
    reports = list((tmp_path / "ws" / "reports").iterdir())
    assert len(reports) == 1 and reports[0].name.startswith("test-campaign-validated-")
    assert snapshot(copy) == snapshot(campaign)


def test_report_refuses_unsafe_or_existing_outputs(campaign: Path, tmp_path: Path):
    existing = tmp_path / "exists"
    existing.mkdir()
    code, _, err = _run(campaign, output=str(existing), render="never")
    assert code == 1 and "refusing to overwrite" in err
    code, _, err = _run(campaign, output=str(campaign / "report"), render="never")
    assert code == 1 and "inside the campaign" in err
    repo = Path(__file__).resolve().parents[2]
    code, _, err = _run(campaign, output=str(repo / "docs" / "new-report"), render="never")
    assert code == 1 and "published historical site" in err
    assert not (repo / "docs" / "new-report").exists()


def test_report_usage_errors(tmp_path: Path, campaign: Path):
    assert _run(None)[0] == 2
    assert _run(None, historical=[str(tmp_path)])[0] == 2  # needs --output
    assert _run(campaign, output=str(tmp_path / "r"), render="sometimes")[0] == 2
    assert _run(campaign, output=str(tmp_path / "r"), min_seconds=-1)[0] == 2
    code, _, err = _run(campaign, output=str(tmp_path / "r"), suite=str(tmp_path / "none"))
    assert code == 2 and "suite" in err
    code, _, err = _run(tmp_path / "missing", output=str(tmp_path / "r"))
    assert code == 1


def test_report_refuses_while_a_run_is_active(campaign: Path, tmp_path: Path):
    copy = writable_copy(campaign, tmp_path / "ws" / "campaigns" / "test-campaign")
    with ExclusiveLock(tmp_path / "ws" / WORKSPACE_LOCK_NAME, "test"):
        code, _, err = _run(copy, output=str(tmp_path / "r"), render="never")
    assert code == 3 and "holds" in err
    assert not (tmp_path / "r").exists()


def test_tampered_records_stop_the_report(campaign: Path, tmp_path: Path):
    copy = writable_copy(campaign, tmp_path / "ws" / "campaigns" / "test-campaign")
    result = next(p for p in (copy / "attempts").glob("*/result.json"))
    result.write_text(result.read_text(encoding="utf-8") + " ", encoding="utf-8")
    code, _, err = _run(copy, output=str(tmp_path / "r"), render="never")
    assert code == 1 and "does not match its receipt" in err


def test_include_raw_copies_the_records(campaign: Path, tmp_path: Path):
    code, _, _ = _run(campaign, output=str(tmp_path / "r"), render="never", include_raw=True)
    assert code == 0
    assert snapshot(tmp_path / "r" / "raw" / "test-campaign") == snapshot(campaign)


def test_historical_only_report(tmp_path: Path):
    legacy = tmp_path / "results"
    legacy.mkdir()
    (legacy / "x.csv").write_text(
        "tool,success,start_time,finish_time,task_id,run_id,lidR_version\n"
        "lidR,1,2024-05-10 06:00:00,2024-05-10 06:00:10,generate_DEM,ALS_1,4.1.1\n",
        encoding="utf-8",
    )
    code, out, err = _run(
        None, historical=[str(legacy)], output=str(tmp_path / "r"), render="never"
    )
    assert code == 0, err
    assert "historical records: 1" in out
    assert (tmp_path / "r" / "tables" / "historical_summary.csv").is_file()
    summary = (tmp_path / "r" / "summary.md").read_text(encoding="utf-8")
    assert "Historical results" in summary and "Measurement units" not in summary
    code, _, err = _run(None, historical=[str(tmp_path / "nothing")], output=str(tmp_path / "s"))
    assert code == 1 and "neither" in err


def test_missing_quarto_is_diagnosed(campaign: Path, tmp_path: Path):
    missing = str(tmp_path / "no-quarto")
    code, _, err = _run(
        campaign,
        output=str(tmp_path / "a"),
        quarto=missing,
        rscript=str(tmp_path / "no-r"),
        min_seconds=0.0,
    )
    assert code == 0 and "Rendering skipped" in err and "Quarto was not found" in err
    assert "Rscript was not found" in err
    assert (tmp_path / "a" / "tables" / "summary.csv").is_file()
    receipt = json.loads((tmp_path / "a" / "report.json").read_text(encoding="utf-8"))
    assert receipt["render"]["status"] == "skipped"
    code, _, _ = _run(campaign, output=str(tmp_path / "b"), quarto=missing, render="always")
    assert code == 5 and (tmp_path / "b" / "tables" / "summary.csv").is_file()


@posix_only
def test_render_with_fake_quarto_and_r(campaign, tmp_path, fakes, monkeypatch):
    quarto, rscript = fakes
    code, _, err = _run(
        campaign,
        output=str(tmp_path / "r"),
        quarto=quarto,
        rscript=rscript,
        use_renv=False,
        render="always",
    )
    assert code == 0, err
    assert (tmp_path / "r" / "_site" / "index.html").is_file()
    assert (tmp_path / "r" / "render.log").is_file()
    receipt = json.loads((tmp_path / "r" / "report.json").read_text(encoding="utf-8"))
    assert receipt["render"]["status"] == "rendered"
    assert receipt["render"]["probe"]["quarto_version"] == "9.9.9"
    assert receipt["render"]["probe"]["packages"]["knitr"] == "1.0"
    monkeypatch.setenv("FAKE_QUARTO_FAIL", "1")
    code, _, err = _run(
        campaign, output=str(tmp_path / "f"), quarto=quarto, rscript=rscript, use_renv=False
    )
    assert code == 5 and "render exploded" in err


@posix_only
def test_missing_r_packages_are_diagnosed(tmp_path, fakes, monkeypatch):
    quarto, rscript = fakes
    monkeypatch.setenv("FAKE_R_MISSING", "rmarkdown,ggplot2")
    probe = probe_render(quarto, rscript, None)
    assert not probe.ready
    assert any("'rmarkdown'" in p for p in probe.problems)
    assert any("ggplot2" in w for w in probe.warnings)
    assert probe.r_version == "R version 9.9.9 (fake)" and probe.lib_paths == ["/fake/library"]


def test_cli_dispatches_report(campaign: Path, tmp_path: Path, capsys):
    code = cli.main(
        [
            "report",
            str(campaign),
            "-o",
            str(tmp_path / "r"),
            "--render",
            "never",
            "--mode",
            "validated",
            "--min-seconds",
            "0",
        ]
    )
    assert code == 0
    assert "test-campaign (validated)" in capsys.readouterr().out


@pytest.mark.integration
def test_real_quarto_render(campaign: Path, tmp_path: Path):
    probe = probe_render(checkout=Path(__file__).resolve().parents[2])
    if not probe.ready or probe.warnings:
        pytest.skip(
            "Quarto, Rscript, knitr, rmarkdown and ggplot2 are needed: "
            + "; ".join(probe.problems + probe.warnings)
        )
    code, _, err = _run(campaign, output=str(tmp_path / "r"), render="always", min_seconds=0.0)
    assert code == 0, err
    site = tmp_path / "r" / "_site"
    for page in (
        "index",
        "performance",
        "comparisons",
        "scaling",
        "resources",
        "issues",
        "environment",
        "historical",
        "downloads",
    ):
        assert (site / f"{page}.html").is_file(), page
    assert (site / "tables" / "summary.csv").is_file()
    performance = (site / "performance.html").read_text(encoding="utf-8")
    assert "Descriptive results" in performance and "execution_only" in performance
    shutil.rmtree(tmp_path / "r")
