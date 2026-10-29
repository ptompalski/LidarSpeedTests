"""Transparent benchmark reports from immutable campaign records.

* :mod:`~lidarbench.reporting.dataset`: the ``lidarbench.report-dataset/1``
  tables (attempts, summary, repetitions, comparisons, scaling, issues,
  environment, tools, datasets, implementations).
* :mod:`~lidarbench.reporting.legacy`: read-only historical R results.
* :mod:`~lidarbench.reporting.output`: CSV/JSON/Markdown writers.
* :mod:`~lidarbench.reporting.site`: optional Quarto staging and rendering.
* :mod:`~lidarbench.reporting.commands`: ``lidarbench report``.

Everything here is stdlib-only; Quarto and R are probed at run time.
See ``design/REPORTING.md``.
"""

from lidarbench.reporting.dataset import (
    REPORT_DATASET_SCHEMA,
    REPORT_MODES,
    TIERS,
    ReportDataset,
    ReportError,
    ReportPolicy,
    Table,
    build_campaign_dataset,
)
from lidarbench.reporting.legacy import LEGACY_EXPORT_SCHEMA, load_historical

__all__ = [
    "LEGACY_EXPORT_SCHEMA",
    "REPORT_DATASET_SCHEMA",
    "REPORT_MODES",
    "TIERS",
    "ReportDataset",
    "ReportError",
    "ReportPolicy",
    "Table",
    "build_campaign_dataset",
    "load_historical",
]
