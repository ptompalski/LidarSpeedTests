"""Write a reporting dataset: CSV tables, JSON metadata and a Markdown summary.

Every file is created exclusively (an existing file is never overwritten).
CSV: UTF-8, ``\\n`` line endings, fixed column order, empty cell for null,
``true``/``false`` for booleans, full ``repr`` precision for floats.
"""

from __future__ import annotations

import csv
import io
import json
from collections import Counter
from collections.abc import Mapping, Sequence
from pathlib import Path
from typing import Any

from lidarbench.core.canonical import sha256_bytes
from lidarbench.reporting.dataset import TIER_DESCRIPTIONS, ReportDataset, Table

TABLES_DIR = "tables"
DATASET_RECORD = "report-dataset.json"
META_TABLE = "report_meta"
INDEX_TABLE = "table_index"


def _cell(value: Any) -> Any:
    if value is None:
        return ""
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, float):
        return repr(value)
    if isinstance(value, (list, dict)):
        return json.dumps(value, sort_keys=True, ensure_ascii=False)
    return value


def table_csv(columns: Sequence[str], rows: Sequence[Mapping[str, Any]]) -> bytes:
    """Render rows as CSV bytes with a fixed header."""
    buffer = io.StringIO(newline="")
    writer = csv.writer(buffer, lineterminator="\n")
    writer.writerow(columns)
    for row in rows:
        writer.writerow([_cell(row.get(c)) for c in columns])
    return buffer.getvalue().encode("utf-8")


def write_new(path: Path, data: bytes) -> str:
    """Create ``path`` exclusively and return the SHA-256 of ``data``."""
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "xb") as handle:
        handle.write(data)
    return sha256_bytes(data)


def meta_rows(dataset: ReportDataset) -> list[dict[str, Any]]:
    """Key/value rows for the Quarto pages (base R reads CSV only)."""
    campaign = dataset.campaign or {}
    rows: list[dict[str, Any]] = [
        {"key": "schema", "value": "lidarbench.report-dataset/1"},
        {"key": "generated_at", "value": dataset.generated_at},
        {"key": "mode", "value": dataset.policy.mode},
        {"key": "min_seconds", "value": dataset.policy.min_seconds},
        {"key": "campaign_id", "value": campaign.get("campaign_id")},
        {"key": "identity_sha256", "value": campaign.get("identity_sha256")},
        {"key": "campaign_created_at", "value": campaign.get("created_at")},
        {"key": "campaign_description", "value": campaign.get("description")},
        {"key": "campaign_validation_mode", "value": campaign.get("validation_mode")},
    ]
    rows += [{"key": "note", "value": note} for note in dataset.notes]
    rows += [{"key": f"tier:{name}", "value": text} for name, text in TIER_DESCRIPTIONS.items()]
    return rows


def write_dataset(
    dataset: ReportDataset,
    historical: Mapping[str, Table],
    out_dir: Path,
    results_jsonl: bytes | None = None,
) -> dict[str, dict[str, Any]]:
    """Write every table and ``report-dataset.json``; return the file index."""
    index: dict[str, dict[str, Any]] = {}
    tables: dict[str, Table] = {**dataset.tables, **historical}
    for name, table in tables.items():
        rel = f"{TABLES_DIR}/{name}.csv"
        sha = write_new(out_dir / rel, table_csv(table.columns, table.rows))
        index[name] = {"file": rel, "sha256": sha, "rows": len(table.rows)}
    rel = f"{TABLES_DIR}/{META_TABLE}.csv"
    meta = meta_rows(dataset)
    index[META_TABLE] = {
        "file": rel,
        "sha256": write_new(out_dir / rel, table_csv(("key", "value"), meta)),
        "rows": len(meta),
    }
    if results_jsonl is not None:
        rel = f"{TABLES_DIR}/results-table.jsonl"
        index["results-table"] = {
            "file": rel,
            "sha256": write_new(out_dir / rel, results_jsonl),
            "rows": results_jsonl.count(b"\n"),
        }
    descriptions = {name: t.description for name, t in tables.items()}
    descriptions[META_TABLE] = "Report metadata, notes and evidence-tier definitions."
    descriptions["results-table"] = (
        "Unfiltered lidarbench.results-table/1 export of every attempt (JSON Lines)."
    )
    listing = [
        {
            "name": n,
            "file": e["file"],
            "rows": e["rows"],
            "sha256": e["sha256"],
            "description": descriptions.get(n),
        }
        for n, e in index.items()
    ]
    rel = f"{TABLES_DIR}/{INDEX_TABLE}.csv"
    index[INDEX_TABLE] = {
        "file": rel,
        "sha256": write_new(
            out_dir / rel, table_csv(("name", "file", "rows", "sha256", "description"), listing)
        ),
        "rows": len(listing),
    }
    metadata = dataset.metadata()
    for name, table in historical.items():
        metadata["tables"][name] = {
            "description": table.description,
            "columns": list(table.columns),
            "rows": len(table.rows),
        }
    for name, entry in index.items():
        metadata["tables"].setdefault(name, {}).update(file=entry["file"], sha256=entry["sha256"])
    data = (json.dumps(metadata, indent=2, sort_keys=True, ensure_ascii=False) + "\n").encode()
    index[DATASET_RECORD] = {
        "file": DATASET_RECORD,
        "sha256": write_new(out_dir / DATASET_RECORD, data),
    }
    return index


# -- Markdown summary (no Quarto or R needed) ------------------------------------------


def _fmt(value: Any, digits: int = 3) -> str:
    if value is None:
        return ""
    if isinstance(value, bool):
        return "yes" if value else "no"
    if isinstance(value, float):
        return f"{value:.{digits}f}"
    return str(value).replace("|", "\\|").replace("\n", " ")


def markdown_table(rows: Sequence[Mapping[str, Any]], columns: Sequence[str]) -> str:
    """A pipe table (empty string when there are no rows)."""
    if not rows:
        return "_none_\n"
    lines = ["| " + " | ".join(columns) + " |", "|" + "---|" * len(columns)]
    lines += ["| " + " | ".join(_fmt(r.get(c)) for c in columns) + " |" for r in rows]
    return "\n".join(lines) + "\n"


def summary_markdown(dataset: ReportDataset, historical: Mapping[str, Table]) -> str:
    """Plain Markdown overview readable without Quarto."""
    campaign = dataset.campaign or {}
    out = ["# LidarSpeedTests report\n"]
    if campaign:
        out.append(f"- Campaign: `{campaign.get('campaign_id')}`")
        out.append(f"- Identity: `{campaign.get('identity_sha256')}`")
        out.append(f"- Created: {campaign.get('created_at')}")
    out.append(
        f"- Report mode: **{dataset.policy.mode}** (minimum timing "
        f"{dataset.policy.min_seconds} s)"
    )
    out.append(f"- Generated: {dataset.generated_at}\n")
    for note in dataset.notes:
        out.append(f"> {note}\n")
    t = dataset.tables
    if campaign:
        out.append("## Measurement units\n")
        out.append(
            markdown_table(
                t["summary"].rows,
                (
                    "workload",
                    "dataset",
                    "cpus",
                    "tool",
                    "tool_version",
                    "section",
                    "evidence",
                    "n_planned",
                    "n_included",
                    "median_seconds",
                    "sd_seconds",
                    "min_seconds",
                    "max_seconds",
                    "availability",
                ),
            )
        )
        validated = [r for r in t["comparisons"].rows if r["comparable"]]
        out.append("## Validated cross-tool comparisons\n")
        out.append(
            markdown_table(
                validated,
                (
                    "workload",
                    "dataset",
                    "cpus",
                    "tool_a",
                    "tool_b",
                    "median_a_seconds",
                    "median_b_seconds",
                    "speedup_a_vs_b_median",
                ),
            )
        )
        refused = len(t["comparisons"].rows) - len(validated)
        if refused:
            out.append(
                f"{refused} tool pair(s) did not pass the comparability gate; see "
                "`tables/comparisons.csv` for the reasons.\n"
            )
        out.append("## Issues\n")
        counts = Counter(r["kind"] for r in t["issues"].rows)
        out.append(
            markdown_table(
                [{"kind": k, "count": n} for k, n in sorted(counts.items())], ("kind", "count")
            )
        )
    hist = historical.get("historical_sources")
    if hist is not None and hist.rows:
        out.append("## Historical results (read-only, descriptive)\n")
        out.append(markdown_table(hist.rows, ("source_kind", "source", "files", "rows", "sha256")))
    out.append("## Tables\n")
    out.append(
        "Machine-readable tables are in `tables/`; `report-dataset.json` describes "
        "every column set and digest.\n"
    )
    return "\n".join(out)
