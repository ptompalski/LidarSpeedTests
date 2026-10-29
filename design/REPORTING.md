# Reporting (`lidarbench report`, `lidarbench.report-dataset/1`)

Status: implemented (Phase 6, Task 11). Code:
[`reporting/`](../src/lidarbench/reporting/) (stdlib only),
Quarto templates in [`reporting/templates/`](../src/lidarbench/reporting/templates/),
R exporter [`bridges/r/export_legacy_campaign.R`](../bridges/r/export_legacy_campaign.R).

A report is derived from the immutable records of **one** campaign (plus,
optionally, read-only historical R results). Nothing is benchmarked, re-run,
validated, repaired or rewritten. The published historical site (`docs/`) and
the legacy R reporting (`R/reporting.R`, `R/5_report.R`, root `*.qmd`) are
unchanged.

## 1. Command

```sh
lidarbench report WORKSPACE/campaigns/ID                       # default mode from the campaign
lidarbench report WORKSPACE/campaigns/ID --mode validated -o REPORT_DIR
lidarbench report WORKSPACE/campaigns/ID --historical _OLD/results --historical r-export.json
lidarbench report --historical _OLD/results -o REPORT_DIR      # historical-only report
lidarbench report ... --render auto|always|never --quarto PATH --rscript PATH --no-renv
lidarbench report ... --min-seconds 0.1 --suite workloads/workloads-v4 --include-raw
```

| Option | Meaning |
|---|---|
| `--mode` | `validated` or `performance-only`. Default: `performance-only` for a `performance-only` campaign, otherwise `validated`. |
| `-o` | New report directory. Default `<workspace>/reports/<campaign>-<mode>-<UTC>`. An existing directory, a path inside the campaign, the repository root or `docs/` is refused. |
| `--min-seconds` | Shorter timings are excluded and flagged (default 0.1 s, as in R). |
| `--historical` | Directory of legacy per-run CSVs, or a `lidarbench.legacy-r-campaign/1` export (repeatable). |
| `--suite` | Workload suite for the *declared* comparability labels (default `workloads/workloads-v4` of the checkout). |
| `--render` | `auto` (default): render if Quarto, Rscript, `knitr` and `rmarkdown` are found, otherwise explain what is missing; `always`: exit 5 unless rendered; `never`. |
| `--include-raw` | Also copy the verified campaign directory into `<report>/raw/`. |

Exit codes: `0` prepared (rendering may have been skipped under `auto`);
`1` integrity, data or output error; `2` usage; `3` a run holds the workspace
lock; `5` rendering required or attempted and failed.

Safety: the campaign is verified through its digest chain (`CampaignStore`);
the SHA-256 of every file in the campaign directory is taken before and after
the report, and any change stops the report. An active workspace lock refuses
the report; a stale one is recorded as a warning. Every report file is created
exclusively; `report.json` (`lidarbench.report/1`) is written last and lists
the policy, the SHA-256 of every report file and of every campaign record,
the staged sources and the render outcome with the Quarto/R probe.

## 2. Report directory

```text
<report>/
  report.json                 lidarbench.report/1 receipt (written last)
  report-dataset.json         lidarbench.report-dataset/1: policy, notes, tables, columns, digests
  summary.md                  dependency-free overview (no Quarto or R needed)
  tables/*.csv                the reporting dataset (§3)
  tables/results-table.jsonl  unfiltered lidarbench.results-table/1 export of every attempt
  _quarto.yml, *.qmd, lidarbench_report.R, styles.css   Quarto project (knitr engine)
  R/reporting.R               legacy R reporting, staged from the checkout when present
  render.log, _site/          after rendering
  raw/<campaign>/             with --include-raw
```

CSV: UTF-8, `\n`, fixed column order, empty cell = missing (never 0),
`true`/`false`, full float precision.

## 3. Reporting dataset (`lidarbench.report-dataset/1`)

| Table | One row per | Content |
|---|---|---|
| `attempts` | attempt (warm-ups, retries, failures, skips, abandoned) | all `lidarbench.results-table/1` columns + `execution_mode`, `contract_tool`, `declared_comparability`, `verdict_source`, `validation_report`, `contract_key/digest`, `verdict_artifacts/structural/numerical`, `boundary_difference_points`, `outcome`, `comparability(_reasons)`, `evidence_tier`, `short_duration`, `report_included`, `exclusion` |
| `summary` | planned measurement unit (workload × dataset × CPUs × tool, from the frozen schedule) | tool version, `section` (`validated`/`descriptive`/`no_measurements`), weakest `evidence`, `availability`; counts: planned, attempts, selected, not attempted, included, missing, failed, timed out, interrupted, skipped, missing outputs, validation failed/incomplete, per tier, short, warm-ups; timing mean, SD, CV, median, quartiles, min, max; tool-reported inner time and start-up overhead; monitored counts and process-tree CPU s, exact CPU s (`process_cpu_seconds`: max of `rusage` and the sampled tree), mean measured parallelism, `n_cpu_budget_exceeded`, peak RSS, whole-machine CPU % and memory peak |
| `repetitions` | included measurement | the samples behind `summary` (timing, inner timing, resources, tool position, evidence tier) |
| `comparisons` | pair of tools with included measurements in the same workload/dataset/CPUs | gate result, `comparability_policy`, `accepted_differences` (edge differences accepted under `edge-tolerant`), reasons; ratios **only** if the gate passed |
| `scaling` | tool × CPU count, for series with ≥ 2 CPU counts | median vs the smallest CPU count, scaling ratio, parallel efficiency, weakest evidence |
| `issues` | problem occurrence | `tool_unavailable`, `monitoring_disabled`, `not_attempted`, `incomplete_record`, `interrupted`, `execution_failed`, `timed_out`, `skipped_*`, `missing_outputs`, `validation_failed`, `validation_incomplete`, `invalid_validation_record`, `unverified_output`, `outputs_not_validated`, `accepted_exception`, `short_timing`, `missing_timing`, `monitoring_partial`, `monitoring_missing`, `cpu_budget_exceeded` (included attempt whose CPU time exceeds `cpus × elapsed × 1.10 + 0.1 s`) |
| `environment` | key | OS, release, architecture, CPU model, logical/physical CPUs, RAM, Python; campaign settings; schedule RNG/seed; provenance (host, platform, git) |
| `tools`, `tool_details` | tool / detail | availability, version, adapter; identity details (R and package versions, LAStools build, PDAL version …) and executable SHA-256 |
| `datasets` | dataset | input/file counts, bytes, dataset digest, manifest ID/version/SHA-256 |
| `implementations` | workload × tool | declared implementation status, threading range, declared comparability |
| `historical_*` | historical record / group / source | §6 |

### 3.1 Selection, verdicts and exclusions

Per schedule entry the latest attempt (start sequence) is *selected*; older
attempts are `superseded` and a failed final retry never falls back to an
earlier success (as in R). Verdicts come from the attempt's
`validation/*.json` (`lidarbench.validation-report/1`, verified through the
attempt digest chain) when present, otherwise from `result.json`
(`structural_status`, `numerical_status`) through `AttemptVerdicts`, so the
fail-closed invariant applies. A report whose execution status disagrees with
the attempt, or more than one report, is `invalid` and never included.

Exclusions, in order (first match): warm-up; superseded retry; abandoned or
incomplete record; execution not `succeeded`; missing required outputs;
validation not eligible under the report mode; missing timing; timing below
`--min-seconds` (flagged `short_duration`).

### 3.2 Evidence tiers

| Tier | Meaning | `validated` report | `performance-only` report |
|---|---|---|---|
| `validated` | every level passed | included | included |
| `accepted_exception` | passed under an explicit frozen policy (also an execution-level accepted exception) | included, labelled | included, labelled |
| `unverified` | artifacts and structure passed; numerical not established | excluded | included, descriptive |
| `execution_only` | succeeded with all required outputs; **no validation level was run** (no validation report) | excluded | included, descriptive |

`execution_only` covers attempts without a validation report: campaigns created
before the runner validated outputs (every attempt has `structural_status =
not_run`, which the status model calls `validation_incomplete`), workloads
without a contract, and validation errors. The runner now writes
`validation/report.json` for every succeeded attempt with a contract
(VALIDATION.md §8); a reused verdict (`reuse.source_attempt_id`) is read like
any other report. Those timings are shown only in performance-only
reports, in their own tier, and never enter comparisons. A summary unit whose
included measurements are all `validated` is in the *validated* section; any
weaker tier puts it in the *descriptive* section.

### 3.3 Comparisons, scaling and variability

Cross-tool ratios use only `validation.comparability.speedup()`, which raises
unless `comparison_gate()` passes (ARCHITECTURE.md §8.3): validated report
mode; every included attempt `comparable` under the campaign's frozen
comparability policy (`strict`, or `edge-tolerant`, which accepts declared
boundary-point differences and the explicit outward-extent exception and lists
them; campaigns without the setting are `strict`); the same contract key, digest and
equivalence group (from the validation reports; no report ⇒ refused); the
same dataset digest, campaign identity (one machine), timing boundary and run
dimensions (CPUs, execution mode). Mixed boundaries or modes within one tool's
attempts are refused as well. Pairs that fail keep a row with the reasons and
no ratio. `speedup_a_vs_b_median` = median(b) / median(a); the mean ratio is
also given. Reports never combine campaigns, so different hardware or
datasets are never compared.

CPU scaling is within one tool, version, workload, dataset and campaign:
`scaling_ratio` = median at the smallest CPU count with included measurements
/ median at this CPU count; `parallel_efficiency` = ratio × baseline CPUs /
CPUs; not computed when timing boundaries or execution modes differ. These
are labelled with the weakest evidence tier and are not cross-tool claims.

Variability: every repetition is in `repetitions`; summaries use sample SD
(n − 1), CV and type-7 quartiles (R's default). Fewer than two samples give an
empty SD, never zero. Timing is the outer fresh-process interval
(`timing_boundary`), never mixed with tool-reported inner intervals, which are
summarized separately. Resource aggregates use only included repetitions with
`monitoring_status = available`; partial and missing monitoring is counted.

### 3.4 Declared comparability

`declared_comparability` comes from `WorkloadContract.declared_comparability()`
of the suite loaded at report time (contracts are not yet part of campaign
identity; the suite name and contract digests are recorded in the dataset).
It is a label only: the gate uses the contract recorded in each attempt's
validation report.

## 4. Quarto site

The report directory is a Quarto website (knitr engine) with pages Overview,
Performance (validated and descriptive sections, units without measurements),
Validated comparisons, CPU scaling, CPU and memory, Issues, Environment,
Historical and Downloads. Pages read only `tables/*.csv` through
`lidarbench_report.R` (base R + `knitr`; `ggplot2` optional) and never
recompute inclusion or ratios. Figures: every repetition as a point with the
median, within-tool scaling lines and per-repetition CPU time / peak RSS. The
mean ± SD figure is drawn by the legacy `benchmark_report_plot()` from the
staged `R/reporting.R`, as on the legacy campaign site; only its axis label is
replaced at render time ("Process wall time mean (s; outer fresh-process
interval)" instead of the legacy "Workload time mean", which described the
in-session R interval), and the caption names the timing boundary. CPU figures
use `process_cpu_seconds`.

Rendering: Quarto from `--quarto`, `$QUARTO` or `PATH`; Rscript from
`--rscript`, `$LIDARBENCH_RSCRIPT` or `PATH`. The probe activates the
checkout's renv (unless `--no-renv`) and passes its library paths to Quarto
(`R_LIBS`, `QUARTO_R`). Missing pieces are listed by name with how to provide
them; the tables and `summary.md` are complete without them, and
`quarto render` can be run later in the report directory.

## 5. Differences from the legacy R report

| Behaviour | R (`benchmark_build_report`) | Python |
|---|---|---|
| Input | RDS manifest + `result.csv` | verified JSON records |
| Build location | temp dir inside the campaign | new directory outside the campaign |
| Unit | task × run × tool (run ID encodes CPUs/storage) | workload × dataset × CPUs × tool from the frozen schedule |
| Tiers | passed / accepted / unverified (structural) | + `execution_only` (no validation ran), performance-only only |
| Speedups | none | gated cross-tool ratios; within-tool CPU scaling |
| Variability | mean, SD, median, range | + CV, quartiles, every repetition |
| Missing Quarto | error | tables + diagnostics; `--render always` to require it |
| Storage/tile-size views | parsed from run IDs | not inferred; datasets are explicit |

## 6. Historical R results (read-only)

* **Legacy per-run CSVs** (`_OLD/results/*.csv`, the data behind the published
  site): read with the stdlib. Required columns `tool, success, start_time,
  finish_time, task_id, run_id`; files with campaign columns are refused.
  `success = 1` is reported as `succeeded`; elapsed = finish − start, with the
  timestamp precision stated; numerical status `unverified`.
* **R-harness campaigns** (RDS receipts): exported once with
  `Rscript --vanilla bridges/r/export_legacy_campaign.R --project . --renv on
  --reports REPORTS --campaign ID --output NEW.json`. It reads the campaign with
  the harness's own `benchmark_read_campaign()` (verifying every receipt),
  checks the raw files are unchanged, and writes every column verbatim
  (`lidarbench.legacy-r-campaign/1`). The import applies the legacy
  `benchmark_report_prepare()` selection (latest `started.rds` time per task,
  run, tool, phase, repetition) and exclusions, including monitor-bound timing.

Historical task IDs are kept verbatim (`legacy_task_id`) with an explicit
`workload_definition`; they are never mapped onto lidarbench workload
contracts, never validated and never enter comparisons, scaling or the
campaign tables. `historical_summary` groups by source, campaign, host, task,
run, tool and tool version (never pooled). Sources are recorded with SHA-256.

## 7. Limitations

* Campaigns created before runner validation (for example the Task 13
  performance-only pilot) have no validation reports; validated reports show
  availability and issues only for them, and comparisons are refused with
  reasons.
* One campaign per report; cross-campaign or cross-machine comparisons are
  deliberately not offered.
* Storage and tile-size views of the legacy site are not reproduced; the
  published historical site stays as it is.
* Verified on Linux (real Quarto 1.6 render and R 4.6); Windows and macOS are
  type-checked only.
