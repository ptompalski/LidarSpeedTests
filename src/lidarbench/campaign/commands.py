"""Command implementations for ``lidarbench run``, ``resume``, ``preflight``, ``export``,
``lock``, ``scratch``."""

from __future__ import annotations

import json
import sys
from pathlib import Path
from typing import Any, TextIO

from lidarbench.campaign.identity import IdentityError
from lidarbench.campaign.progress import terminal_progress_for
from lidarbench.campaign.results import FORMATS, collect_rows, render_rows, write_results
from lidarbench.campaign.runner import (
    CampaignRunner,
    RunError,
    RunOptions,
    RunSummary,
    Selection,
)
from lidarbench.campaign.store import (
    CampaignExistsError,
    CampaignIntegrityError,
    CampaignStore,
    campaign_dir,
)
from lidarbench.config.campaign import ConfigError, load_config
from lidarbench.core.locks import (
    MACHINE_LOCK_NAME,
    WORKSPACE_LOCK_NAME,
    ExclusiveLock,
    LockError,
    LockHeldError,
    default_state_dir,
    inspect_lock,
    release_lock,
)
from lidarbench.core.scratch import cleanup_orphan, find_owned_scratch

EXIT_USAGE = 2
EXIT_LOCKED = 3


def _render_summary(summary: RunSummary, out: TextIO) -> None:
    head = "Dry run (nothing written)" if summary.dry_run else f"Run {summary.run_id}"
    lines = [
        f"{head}: campaign {summary.campaign_id} [{summary.campaign_state}]",
        f"  path     : {summary.campaign_path}",
        f"  identity : {summary.identity_sha256}",
    ]
    if summary.differences:
        lines.append("  identity differs from the frozen campaign; a new campaign_id is required:")
        for name, diffs in summary.differences.items():
            lines += [f"    {name}: {d}" for d in diffs[:10]]
    if summary.decisions:
        counts = ", ".join(f"{k}={v}" for k, v in sorted(summary.action_counts().items()))
        lines.append(f"  schedule : {len(summary.decisions)} entries ({counts})")
    if summary.dry_run and summary.decisions:
        lines.append("")
        lines.append(
            f"  {'#':>4}  {'phase':<11} {'rep':>3}  {'workload':<22} {'dataset':<12} "
            f"{'cpus':>4}  {'tool':<12} {'pos':>3}  {'action':<17} plan"
        )
        for d in summary.decisions:
            e = d.entry
            preview = summary.previews.get(e.schedule_index)
            plan = ""
            if preview:
                plan = preview["status"] + (f": {preview['reason']}" if preview["reason"] else "")
            lines.append(
                f"  {e.schedule_index:>4}  {e.phase:<11} {e.repetition:>3}  {e.workload:<22} "
                f"{e.dataset:<12} {e.cpus:>4}  {e.tool:<12} {e.tool_position:>3}  "
                f"{d.action!s:<17} {plan or d.note}"
            )
    if summary.statuses:
        statuses = ", ".join(f"{k}={v}" for k, v in sorted(summary.statuses.items()))
        lines.append(f"  attempts : {statuses}")
        lines.append(
            "  note     : execution status is not numerical validity; structural and "
            "numerical verdicts are recorded separately"
        )
    if summary.stop_reason:
        lines.append(f"  stopped  : {summary.stop_reason}")
    for warning in summary.warnings:
        lines.append(f"  warning  : {warning}")
    out.write("\n".join(lines) + "\n")


def run_command(
    config_path: str,
    *,
    dry_run: bool = False,
    as_json: bool = False,
    workloads: list[str] | None = None,
    datasets: list[str] | None = None,
    tools: list[str] | None = None,
    cpus: list[int] | None = None,
    phases: list[str] | None = None,
    limit: int | None = None,
    retry_failed: bool = False,
    allow_dirty: bool = False,
    require_existing: bool = False,
    out: TextIO | None = None,
    err: TextIO | None = None,
    **runner_kwargs: Any,
) -> int:
    """Run, resume or dry-run a campaign; returns the process exit code."""
    out = out or sys.stdout
    err = err or sys.stderr
    try:
        config = load_config(config_path)
        selection = Selection(
            workloads=frozenset(workloads or ()),
            datasets=frozenset(datasets or ()),
            tools=frozenset(tools or ()),
            cpus=frozenset(cpus or ()),
            phases=frozenset(phases or ()),
            limit=limit,
        )
        options = RunOptions(selection, retry_failed, allow_dirty, require_existing)
        runner_kwargs.setdefault("report", lambda line: print(line, file=err, flush=True))
        if not dry_run and not as_json:
            runner_kwargs.setdefault("progress", terminal_progress_for(err))
        runner = CampaignRunner(config, options=options, **runner_kwargs)
        summary = runner.dry_run() if dry_run else runner.run()
    except LockHeldError as exc:
        err.write(f"Error: {exc}\n")
        return EXIT_LOCKED
    except (ConfigError, CampaignExistsError, CampaignIntegrityError, IdentityError) as exc:
        err.write(f"Error: {exc}\n")
        return 1
    except RunError as exc:
        err.write(f"Error: {exc}\n")
        return exc.exit_code
    except KeyboardInterrupt:
        err.write("Interrupted before execution started; no attempt was left running.\n")
        return 130
    if as_json:
        out.write(json.dumps(summary.to_record(), indent=2, sort_keys=True) + "\n")
    else:
        _render_summary(summary, out)
    return summary.exit_code


def preflight_command(
    config_path: str,
    *,
    full_hash: bool = False,
    as_json: bool = False,
    out: TextIO | None = None,
    err: TextIO | None = None,
    **preflight_kwargs: Any,
) -> int:
    """Read-only readiness check of a campaign configuration.

    Returns:
        ``0`` when no check failed (warnings allowed), ``1`` otherwise.
    """
    from lidarbench.campaign.preflight import render_report, run_preflight

    out = out or sys.stdout
    err = err or sys.stderr
    report = run_preflight(config_path, full_hash=full_hash, **preflight_kwargs)
    if as_json:
        out.write(json.dumps(report.to_record(), indent=2, sort_keys=True) + "\n")
    else:
        out.write(render_report(report))
    if report.campaign_id is None:
        err.write(f"Error: {report.items[0].message}\n")
    return report.exit_code


def export_command(
    campaign_path: str,
    *,
    fmt: str = "jsonl",
    output: str | None = None,
    out: TextIO | None = None,
    err: TextIO | None = None,
) -> int:
    """Export a verified results table."""
    out = out or sys.stdout
    err = err or sys.stderr
    if fmt not in FORMATS:
        err.write(f"Error: format must be one of {FORMATS}\n")
        return EXIT_USAGE
    try:
        rows = collect_rows(Path(campaign_path))
        if output is None:
            if fmt == "parquet":
                err.write("Error: parquet output needs --output\n")
                return EXIT_USAGE
            out.write(render_rows(rows, fmt).decode("utf-8"))
        else:
            write_results(rows, Path(output), fmt)
    except (CampaignExistsError, CampaignIntegrityError, FileExistsError, RuntimeError) as exc:
        err.write(f"Error: {exc}\n")
        return 1
    except FileNotFoundError as exc:
        err.write(f"Error: not a campaign directory: {exc}\n")
        return 1
    return 0


def _lock_path(target: str) -> Path:
    path = Path(target)
    return path / WORKSPACE_LOCK_NAME if path.is_dir() else path


def lock_command(
    action: str,
    target: str | None,
    *,
    confirm: bool = False,
    force: bool = False,
    out: TextIO | None = None,
    err: TextIO | None = None,
) -> int:
    """Show or explicitly release a workspace or machine lock.

    ``target`` is a workspace directory, a lock file, or ``None`` for the
    per-user machine lock.
    """
    out = out or sys.stdout
    err = err or sys.stderr
    path = _lock_path(target) if target else default_state_dir() / MACHINE_LOCK_NAME
    if action == "status":
        out.write(json.dumps(inspect_lock(path).to_record(), indent=2, sort_keys=True) + "\n")
        return 0
    if not confirm:
        err.write("Error: releasing a lock needs --confirm\n")
        return EXIT_USAGE
    try:
        report = release_lock(path, force=force)
    except LockError as exc:
        err.write(f"Error: {exc}\n")
        return EXIT_LOCKED
    out.write(json.dumps(report.to_record(), indent=2, sort_keys=True) + "\n")
    return 0


def scratch_command(
    config_path: str,
    *,
    clean: bool = False,
    confirm: bool = False,
    out: TextIO | None = None,
    err: TextIO | None = None,
) -> int:
    """List (or explicitly delete) orphaned run-owned scratch of a campaign.

    Deletion requires ``--clean --confirm``, holds the workspace lock, and only
    touches directories whose marker proves ownership and whose attempt already
    has a receipt or a tombstone (so it cannot belong to a running attempt).
    """
    out = out or sys.stdout
    err = err or sys.stderr
    if clean and not confirm:
        err.write("Error: deleting scratch needs --clean --confirm\n")
        return EXIT_USAGE
    try:
        config = load_config(config_path)
    except ConfigError as exc:
        err.write(f"Error: {exc}\n")
        return 1
    store = CampaignStore(campaign_dir(config.workspace, config.campaign_id))
    lock = ExclusiveLock(config.workspace / WORKSPACE_LOCK_NAME, "scratch-cleanup")
    try:
        if clean:
            lock.acquire()
        finished: set[str] = set()
        if store.exists:
            store.load()
            finished = {a.attempt_id for a in store.attempts() if a.state != "incomplete"}
        report = []
        protected = [d.path for d in config.datasets] + [config.workspace]
        for label, root in sorted(config.scratch.roots.items()):
            for orphan in find_owned_scratch(root, config.campaign_id):
                item = {
                    "root": label,
                    **orphan,
                    "attempt_finished": orphan["attempt_id"] in finished,
                }
                if clean:
                    if item["attempt_finished"]:
                        item["cleanup"] = cleanup_orphan(
                            orphan["path"], root, config.campaign_id, protected
                        ).to_record()
                    else:
                        item["cleanup"] = {
                            "complete": False,
                            "reason": "attempt has no receipt or tombstone yet; run "
                            "'lidarbench resume' first",
                        }
                report.append(item)
    except LockHeldError as exc:
        err.write(f"Error: {exc}\n")
        return EXIT_LOCKED
    except (CampaignExistsError, CampaignIntegrityError) as exc:
        err.write(f"Error: {exc}\n")
        return 1
    finally:
        if lock.held:
            lock.release()
    out.write(json.dumps({"campaign_id": config.campaign_id, "orphans": report}, indent=2) + "\n")
    return 0
