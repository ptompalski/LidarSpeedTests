"""``lidarbench report``: prepare a report from an existing campaign (read-only).

Steps: refuse while a run holds the workspace lock; snapshot the SHA-256 of
every campaign file; verify the campaign and derive the reporting dataset;
import historical sources; write tables, a Markdown summary and the Quarto
project into a **new** directory; render if possible; check that the campaign
records are unchanged; write ``report.json`` last.

Exit codes: ``0`` report prepared (rendering may have been skipped with
``--render auto``); ``1`` integrity, data or output error; ``2`` usage;
``3`` a run is active in the workspace; ``5`` rendering was required or
attempted and failed.
"""

from __future__ import annotations

import argparse
import json
import shutil
import sys
from collections import Counter
from pathlib import Path
from typing import Any, TextIO

from lidarbench.campaign.results import render_rows, rows_for_attempts
from lidarbench.campaign.store import CampaignExistsError, CampaignIntegrityError, CampaignStore
from lidarbench.core.canonical import digest, sha256_file
from lidarbench.core.clock import iso_utc, now
from lidarbench.core.locks import WORKSPACE_LOCK_NAME, inspect_lock
from lidarbench.core.store import iter_files
from lidarbench.reporting.dataset import (
    DEFAULT_MIN_SECONDS,
    REPORT_MODES,
    ReportDataset,
    ReportError,
    ReportPolicy,
    build_campaign_dataset,
    empty_dataset,
)
from lidarbench.reporting.legacy import load_historical
from lidarbench.reporting.output import summary_markdown, write_dataset, write_new
from lidarbench.reporting.site import (
    RenderProbe,
    RenderResult,
    probe_render,
    render_site,
    repo_root,
    stage_site,
)
from lidarbench.workloads.contract import (
    ContractError,
    WorkloadSuite,
    default_suite_dir,
    load_suite,
)

REPORT_SCHEMA = "lidarbench.report/1"
REPORT_RECORD = "report.json"
RAW_DIR = "raw"
REPORTS_DIR = "reports"
RENDER_MODES = ("auto", "always", "never")

EXIT_OK = 0
EXIT_ERROR = 1
EXIT_USAGE = 2
EXIT_LOCKED = 3
EXIT_RENDER = 5


def add_report_parser(subparsers: Any) -> None:
    """Register ``lidarbench report``."""
    report = subparsers.add_parser(
        "report",
        help="prepare a report (tables + Quarto site) from an existing campaign",
        description=(
            "Verify a campaign and write a new report directory: machine-readable tables, "
            "a Markdown summary and a Quarto website (rendered when Quarto and R are "
            "available). Campaign records are never modified and nothing is benchmarked."
        ),
    )
    report.add_argument(
        "campaign", nargs="?", help="campaign directory (<workspace>/campaigns/<id>)"
    )
    report.add_argument(
        "-o",
        "--output",
        help="new report directory (default: <workspace>/reports/<campaign>-<mode>-<UTC time>)",
    )
    report.add_argument(
        "--mode",
        choices=REPORT_MODES,
        help="validated or performance-only (default: from the campaign's validation_mode)",
    )
    report.add_argument(
        "--min-seconds",
        type=float,
        default=DEFAULT_MIN_SECONDS,
        help="timings below this are excluded and flagged (default: %(default)s)",
    )
    report.add_argument(
        "--historical",
        action="append",
        metavar="SOURCE",
        default=[],
        help="read-only historical R results: a directory of legacy per-run CSVs or a "
        "lidarbench.legacy-r-campaign/1 JSON export (repeatable)",
    )
    report.add_argument("--suite", help="workload suite directory for declared comparability")
    report.add_argument(
        "--render",
        choices=RENDER_MODES,
        default="auto",
        help="auto: render if Quarto and R are available; always: fail otherwise; never",
    )
    report.add_argument("--quarto", help="Quarto executable (default: $QUARTO or PATH)")
    report.add_argument("--rscript", help="Rscript (default: $LIDARBENCH_RSCRIPT or PATH)")
    report.add_argument(
        "--no-renv", action="store_true", help="do not use the checkout's renv library"
    )
    report.add_argument(
        "--include-raw",
        action="store_true",
        help="also copy the verified campaign records into <report>/raw/",
    )


def report_command_from_args(parsed: argparse.Namespace) -> int:
    """Dispatch parsed CLI arguments."""
    return report_command(
        parsed.campaign,
        output=parsed.output,
        mode=parsed.mode,
        min_seconds=parsed.min_seconds,
        historical=parsed.historical,
        suite=parsed.suite,
        render=parsed.render,
        quarto=parsed.quarto,
        rscript=parsed.rscript,
        use_renv=not parsed.no_renv,
        include_raw=parsed.include_raw,
    )


def snapshot(path: Path) -> dict[str, str]:
    """SHA-256 of every file below a campaign directory (relative POSIX paths)."""
    return {p.relative_to(path).as_posix(): sha256_file(p) for p in iter_files(path)}


def _default_mode(store: CampaignStore) -> str:
    validation_mode = store.components["config"].get("validation_mode")
    return "performance-only" if validation_mode == "performance-only" else "validated"


def _load_suite(suite: str | None) -> tuple[WorkloadSuite | None, str | None]:
    path = Path(suite) if suite else default_suite_dir()
    if not suite and not path.is_dir():
        return None, None
    try:
        return load_suite(path), None
    except ContractError as exc:
        return None, str(exc)


def _inside(path: Path, parent: Path) -> bool:
    try:
        path.resolve().relative_to(parent.resolve())
    except ValueError:
        return False
    return True


def _output_problem(output: Path, campaign: Path | None, checkout: Path | None) -> str | None:
    if output.exists():
        return f"refusing to overwrite existing {output}; choose a new report directory"
    if campaign is not None and _inside(output, campaign):
        return "the report directory must not be inside the campaign directory"
    if checkout is not None:
        if output.resolve() == checkout.resolve():
            return "the report directory must not be the repository root"
        if _inside(output, checkout / "docs"):
            return "refusing to write into docs/ (the published historical site)"
    return None


def report_command(
    campaign: str | None,
    *,
    output: str | None = None,
    mode: str | None = None,
    min_seconds: float = DEFAULT_MIN_SECONDS,
    historical: list[str] | None = None,
    suite: str | None = None,
    render: str = "auto",
    quarto: str | None = None,
    rscript: str | None = None,
    use_renv: bool = True,
    include_raw: bool = False,
    out: TextIO | None = None,
    err: TextIO | None = None,
) -> int:
    """Prepare a report; see the module docstring for exit codes."""
    out = out or sys.stdout
    err = err or sys.stderr
    historical = list(historical or [])
    if campaign is None and not historical:
        err.write("Error: give a campaign directory and/or --historical sources\n")
        return EXIT_USAGE
    if render not in RENDER_MODES:
        err.write(f"Error: --render must be one of {RENDER_MODES}\n")
        return EXIT_USAGE
    checkout = repo_root()
    store: CampaignStore | None = None
    campaign_path = Path(campaign) if campaign else None
    warnings: list[str] = []
    try:
        if campaign_path is not None:
            store = CampaignStore(campaign_path)
            store.load()
            lock = inspect_lock(campaign_path.parent.parent / WORKSPACE_LOCK_NAME)
            if lock.exists and lock.state == "active":
                err.write(
                    f"Error: a run holds {lock.path}; report after it finishes so the "
                    "records are complete and stable\n"
                )
                return EXIT_LOCKED
            if lock.exists:
                warnings.append(
                    f"workspace lock {lock.path} exists ({lock.state}); no active "
                    "run was detected, records are checked for changes"
                )
        mode = mode or (_default_mode(store) if store else "performance-only")
        policy = ReportPolicy(mode, min_seconds)
    except (CampaignExistsError, CampaignIntegrityError) as exc:
        err.write(f"Error: {exc}\n")
        return EXIT_ERROR
    except FileNotFoundError as exc:
        err.write(f"Error: not a campaign directory: {exc}\n")
        return EXIT_ERROR
    except ValueError as exc:
        err.write(f"Error: {exc}\n")
        return EXIT_USAGE

    stamp = iso_utc(now().wall_ns).replace("-", "").replace(":", "")[:15] + "Z"
    if output:
        out_dir = Path(output)
    elif store is not None and campaign_path is not None:
        out_dir = campaign_path.parent.parent / REPORTS_DIR / f"{store.campaign_id}-{mode}-{stamp}"
    else:
        err.write("Error: --output is required for a report without a campaign\n")
        return EXIT_USAGE
    problem = _output_problem(out_dir, campaign_path, checkout)
    if problem:
        err.write(f"Error: {problem}\n")
        return EXIT_ERROR

    loaded_suite, suite_problem = _load_suite(suite)
    if suite_problem:
        if suite:
            err.write(f"Error: {suite_problem}\n")
            return EXIT_USAGE
        warnings.append(f"workload suite not loaded: {suite_problem}")

    try:
        before = snapshot(campaign_path) if campaign_path else {}
        if campaign_path is not None:
            dataset = build_campaign_dataset(campaign_path, policy, loaded_suite)
        else:
            dataset = empty_dataset(policy)
        hist = load_historical([Path(h) for h in historical], policy) if historical else {}
        results_jsonl = None
        if store is not None:
            results_jsonl = render_rows(rows_for_attempts(store, store.attempts()), "jsonl")
    except (CampaignExistsError, CampaignIntegrityError, ReportError, OSError) as exc:
        err.write(f"Error: {exc}\n")
        return EXIT_ERROR
    dataset.notes.extend(warnings)
    if hist:
        dataset.notes.append(
            "Historical R results are read-only and descriptive: legacy task IDs are not "
            "mapped to lidarbench workload contracts and never enter comparisons."
        )

    out_dir.mkdir(parents=True)
    files = write_dataset(dataset, hist, out_dir, results_jsonl)
    files["summary.md"] = {
        "file": "summary.md",
        "sha256": write_new(out_dir / "summary.md", summary_markdown(dataset, hist).encode()),
    }
    staged = stage_site(out_dir, checkout)
    if include_raw and campaign_path is not None:
        shutil.copytree(campaign_path, out_dir / RAW_DIR / campaign_path.name)

    probe: RenderProbe | None = None
    if render == "never":
        result = RenderResult("not_requested")
    else:
        probe = probe_render(quarto, rscript, checkout, use_renv)
        if probe.ready:
            result = render_site(out_dir, probe)
        else:
            result = RenderResult("skipped", "; ".join(probe.problems))

    if campaign_path is not None:
        after = snapshot(campaign_path)
        if after != before:
            changed = sorted(set(before) ^ set(after)) + sorted(
                k for k in set(before) & set(after) if before[k] != after[k]
            )
            err.write(
                "Error: campaign records changed while the report was prepared "
                f"({', '.join(changed[:5])}); discard {out_dir} and report again after "
                "execution has stopped\n"
            )
            return EXIT_ERROR

    receipt = {
        "schema": REPORT_SCHEMA,
        "generated_at": dataset.generated_at,
        "policy": policy.to_record(),
        "campaign": dataset.campaign,
        "campaign_records": (
            {
                "path": str(campaign_path.resolve()),
                "files": len(before),
                "sha256": digest(before),
                "file_sha256": before,
            }
            if campaign_path is not None
            else None
        ),
        "historical_sources": hist["historical_sources"].rows if hist else [],
        "files": files,
        "staged_sources": staged,
        "raw_copy": f"{RAW_DIR}/{campaign_path.name}" if include_raw and campaign_path else None,
        "render": {**result.to_record(), "probe": probe.to_record() if probe else None},
        "warnings": warnings,
    }
    write_new(
        out_dir / REPORT_RECORD, (json.dumps(receipt, indent=2, sort_keys=True) + "\n").encode()
    )
    _print_summary(out, err, out_dir, dataset, hist, result, probe)
    if result.status == "failed" or (render == "always" and result.status != "rendered"):
        return EXIT_RENDER
    return EXIT_OK


def _print_summary(
    out: TextIO,
    err: TextIO,
    out_dir: Path,
    dataset: ReportDataset,
    hist: dict[str, Any],
    result: RenderResult,
    probe: RenderProbe | None,
) -> None:
    t = dataset.tables
    out.write(f"Report: {out_dir}\n")
    if dataset.campaign:
        attempts = t["attempts"].rows
        out.write(
            f"  campaign {dataset.campaign['campaign_id']} ({dataset.policy.mode}): "
            f"{len(attempts)} attempts, {sum(1 for r in attempts if r['report_included'])} "
            f"included measurements, {len(t['summary'].rows)} units, "
            f"{sum(1 for r in t['comparisons'].rows if r['comparable'])} validated comparisons\n"
        )
        kinds = Counter(r["kind"] for r in t["issues"].rows)
        if kinds:
            out.write("  issues: " + ", ".join(f"{k} {n}" for k, n in sorted(kinds.items())) + "\n")
    if hist:
        out.write(f"  historical records: {len(hist['historical_attempts'].rows)}\n")
    out.write(f"  tables: {out_dir / 'tables'}\n  summary: {out_dir / 'summary.md'}\n")
    if result.status == "rendered":
        out.write(f"  site: {out_dir / str(result.site)}\n")
    elif result.status == "not_requested":
        out.write(
            "  site: not rendered (--render never); run 'quarto render' in the report "
            "directory\n"
        )
    else:
        err.write(f"Rendering {result.status}: {result.detail}\n")
        if result.status == "skipped":
            err.write(
                "  The tables and summary.md are complete. To render later, install "
                f"the missing pieces and run 'quarto render' in {out_dir}\n"
            )
    if probe is not None:
        for warning in probe.warnings:
            err.write(f"Warning: {warning}\n")
