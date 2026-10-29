"""Synthetic campaign records for reporting tests (no LiDAR software is run).

``make_campaign`` runs a small real campaign with the synthetic and scripted
test adapters (two synthetic tool instances, a multi-threaded scripted tool,
a failing workload and unsupported combinations that become recorded skips).

``with_validation_reports`` copies a campaign and re-seals selected attempts of
the *copy* with a synthetic ``validation/report.json`` so that validated
comparisons can be tested before the runner writes validation reports. The
original campaign is never modified.
"""

from __future__ import annotations

import json
import os
import shutil
import stat
from collections.abc import Callable
from pathlib import Path
from typing import Any

from campaign_helpers import make_runner, write_config, write_layout

from lidarbench.campaign.store import COMPLETED, RESULT, CampaignStore
from lidarbench.validation.pipeline import REPORT_SCHEMA

WORKLOADS = (
    '[[workloads]]\nid = "synthetic.checksum"\n'
    '[[workloads]]\nid = "scripted.ok"\n'
    '[[workloads]]\nid = "scripted.fail"\n'
)
TOOLS = (
    '[[tools]]\nid = "synthetic"\n'
    '[[tools]]\nid = "synth-b"\nadapter = "synthetic"\n'
    '[[tools]]\nid = "scripted"\n'
)


def make_campaign(tmp_path: Path, monitoring: str = "enabled = false") -> Path:
    """Run the synthetic campaign and return its directory."""
    base = write_layout(tmp_path)
    config = write_config(
        base,
        workloads=WORKLOADS,
        tools=TOOLS,
        cpu_counts="[1, 2]",
        scheduling="repetitions = 2\nwarmups = 1\ncooldown_s = 0",
        monitoring=monitoring,
    )
    summary = make_runner(config, base).run()
    assert summary.exit_code == 0
    return base / "workspace" / "campaigns" / "test-campaign"


def writable_copy(campaign: Path, target: Path) -> Path:
    """Copy a campaign directory and make the copy writable."""
    shutil.copytree(campaign, target)
    for root, dirs, files in os.walk(target):
        for name in dirs + files:
            path = Path(root, name)
            path.chmod(path.stat().st_mode | stat.S_IWUSR)
    return target


def verdict_record(
    execution_status: str = "succeeded",
    numerical: str = "passed",
    boundary: int | None = 0,
    equivalence: str = "matched_definition",
    validation_mode: str = "numerical",
) -> dict[str, Any]:
    """A synthetic ``verdicts`` record (levels before numerical pass)."""
    return {
        "execution_status": execution_status,
        "artifacts": {"status": "passed", "reasons": []},
        "structural": {"status": "passed", "reasons": []},
        "numerical": {"status": numerical, "reasons": [] if numerical == "passed" else ["x"]},
        "implementation_status": "runnable",
        "equivalence": equivalence,
        "boundary_difference_points": boundary,
        "validation_mode": validation_mode,
    }


def validation_report(verdicts: dict[str, Any], digest: str = "d" * 64) -> dict[str, Any]:
    """A synthetic ``lidarbench.validation-report/1`` record."""
    return {
        "schema": REPORT_SCHEMA,
        "contract": {
            "key": "workloads-v2/synthetic.checksum",
            "digest": digest,
            "equivalence_group": "workloads-v2/synthetic.checksum",
        },
        "verdicts": verdicts,
        "synthetic_fixture": True,
    }


def with_validation_reports(
    campaign: Path,
    target: Path,
    report_for: Callable[[dict[str, Any]], dict[str, Any] | None],
) -> Path:
    """Copy ``campaign`` and add ``validation/report.json`` where ``report_for`` says.

    ``report_for`` receives each completed ``result.json`` and returns the
    report to add, or ``None`` to leave the attempt unchanged.
    """
    copy = writable_copy(campaign, target)
    store = CampaignStore(copy)
    store.load()
    for attempt in sorted((copy / "attempts").iterdir()):
        result_path = attempt / RESULT
        if not result_path.is_file():
            continue
        result = json.loads(result_path.read_text(encoding="utf-8"))
        report = report_for(result)
        if report is None:
            continue
        (attempt / "validation").mkdir()
        (attempt / "validation" / "report.json").write_text(json.dumps(report), "utf-8")
        result_path.unlink()
        (attempt / COMPLETED).unlink()
        result["files"] = store.attempt_files(attempt)
        store.seal_attempt(attempt, result)
    return copy
