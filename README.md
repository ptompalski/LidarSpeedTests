Work in progress: this repository is for testing the performance of LiDAR processing software packages, designed for—but not limited to—typical forestry-related workflows involving airborne laser scanning data. Currently tested packages are lidR, lasR, and LAStools.

Fresh-data preparation uses `cli` for status messages and parent-process progress
bars for the repair/check, preparation and verification phases. Progress counts completed tile
jobs (all density variants per tile during preparation), including successfully
rechecked resumed jobs. Workers keep separate logs rather than drawing competing
bars. Progress visibility follows `cli`'s terminal defaults; benchmark timing and
report output are unchanged.

Preparation and verification default to **16 tile workers**, with one native
thread per worker. Override the budget with `--cores N`; large clouds can require
several GiB per worker, so reduce it on memory-constrained machines.

To finish an existing preparation whose output files and completion receipts
are present, but whose final verification was interrupted, run:

```sh
Rscript --vanilla R/prepare_fresh_data.R --verify-full /path/to/config.R
```

Use `--verify-pilot` for the two-tile preparation. These commands acquire the
preparation lock and refuse to run alongside an active preparation. Interrupt
the old run normally (Ctrl+C) and wait for it to exit and release its lock first.
Do not remove an active lock or start another preparation.

Verification restarts its checks, not data generation: it reuses existing clouds,
indexes and receipts, checks the frozen configuration and data identities, and
performs the full per-tile QC in parallel. It permits verifier source-code changes
without rewriting historical implementation hashes or policy-transition records;
generation/resume commands retain their stricter implementation checks.
Successful verification creates a missing `ready.rds`; an existing readiness
receipt and all prepared data remain unchanged. Each `verification-*` directory
records the verifier implementation, worker logs, per-tile successes and final
status. A failed verification never creates a readiness receipt, and a hard-killed
verifier may leave no final status record.

## LidarSpeedTests 2.0 orchestrator (in development)

The Python orchestrator `lidarbench` is being built beside the R harness (see
[design/ARCHITECTURE.md](design/ARCHITECTURE.md) and
[MIGRATION_PLAN.md](MIGRATION_PLAN.md)). It runs campaigns with the `lidr` and
`lasr` adapters (lidR and lasR through `Rscript`) for the mean-elevation and
maximum-DSM workloads, and with a deterministic `synthetic` test adapter.

```sh
uv sync                            # create .venv/ from uv.lock (install uv: https://docs.astral.sh/uv/)
uv run lidarbench check            # OS facts and detected tool adapters
uv run lidarbench check --json     # the same as a lidarbench.check/1 JSON document
uv run lidarbench run CONFIG.toml --dry-run   # schedule, resume decisions, planned commands
uv run lidarbench run CONFIG.toml             # create or resume a campaign and execute it
uv run lidarbench export WORKSPACE/campaigns/ID --format csv -o results.csv
uv run lidarbench validate --workload pixel_metrics_1a --tool lasR --outputs OUT --input-dir TILES --epsg 2959
uv run pytest tests/python         # Python test suite
```

`lidarbench run` freezes a campaign (configuration, workloads, tool binaries,
input hashes, hardware/OS, code digests and a seeded `schedule-v2`) in
`campaign.json` and refuses to resume when any of it changes. It then runs the
schedule one attempt at a time and records every attempt write-once. Resume
skips completed entries, retries interrupted ones, and retries failures only
with `--retry-failed`. Each attempt records execution status separately from
structural and numerical validation, so execution success alone never means
the results are numerically valid. Inputs are hashed once per run, never
inside a timed attempt. See [design/CAMPAIGNS.md](design/CAMPAIGNS.md) and
[configs/examples/synthetic-campaign.toml](configs/examples/synthetic-campaign.toml).

[uv](https://docs.astral.sh/uv/) manages the Python environment: `uv.lock` pins
every Python dependency, the way `renv.lock` pins the R packages. Plain
`pip install -e .` also works. The R environment is unchanged and still managed
by renv.

`check` lists each adapter's availability (with the reason when software is
missing or the platform is unsupported), version and declared workloads, including
why a workload is unsupported or blocked. New tools are added as plugins; see
[design/ADAPTERS.md](design/ADAPTERS.md).

The `lidr` and `lasr` adapters run each attempt as a fresh
`Rscript --vanilla bridges/r/runner.R` process. They use the project's renv
library and the unchanged legacy R code (`R/1_functions.R`, the calls of
`R/0_setup.R`). R is optional for the Python package. Without R, or without the
locked packages (`renv::restore()`), these tools are reported as unavailable,
and their attempts are recorded as `skipped_unavailable`. R errors and R
warnings during a workload fail the attempt. lasR inputs need `.lax` indexes
(create them once before the campaign). The timed interval includes R start-up;
the R workload time and the start-up overhead are reported separately. See
[design/R_BRIDGE.md](design/R_BRIDGE.md) and
[configs/examples/r-campaign.toml](configs/examples/r-campaign.toml).

The process execution engine (`lidarbench.execution.attempt.execute_plan`) runs
a planned command as a fresh process with timeouts, cancellation, whole-tree
cleanup and `psutil` monitoring of the process tree and the whole machine. See
[design/EXECUTION.md](design/EXECUTION.md) for what is timed, measured, and the
known monitoring limitations.

`lidarbench validate` checks tool outputs against a workload contract
(`workloads/workloads-v1/*.toml`) outside any timing, with separate verdicts for
artifacts, raster structure (CRS, grid, alignment, extent, bands) and values,
compared with independent Python references. Only the mean-elevation (20 m) and
maximum-elevation (1 m) workloads have references so far. Failed or incomplete
validation is never reported as validated; unverified results can only appear in
labelled descriptive reports. It needs the optional `validation` extra
(`pip install "lidarbench[validation]"`; included by `uv sync`). See
[design/VALIDATION.md](design/VALIDATION.md) for which claims it establishes and
which remain unverified.

## Benchmark pilot on prepared data

The benchmark pilot is separate from preparation's `--pilot` stage. It stages
unchanged copies of the two approved neighboring density-1 tiles and their
indexes from a verified preparation. It checks readiness, manifest identity and
completion-receipt checksums, and freezes that provenance with the campaign.
It never processes all 100 tiles or repeats data preparation.

From the repository root, initialize a **new** pilot workspace, check it, then
explicitly start the benchmark:

```sh
Rscript --vanilla R/6_pilot.R --init /path/to/fresh-data/full /path/to/new-pilot-workspace [/path/to/LAStools/bin]
Rscript --vanilla R/6_pilot.R --preflight /path/to/new-pilot-workspace
# Run only after the preflight succeeds:
Rscript --vanilla R/6_pilot.R --run /path/to/new-pilot-workspace fresh-pilot-001
```

Initialization creates `benchmark.R`, `scratch/` and `reports/` in the workspace.
It inherits confirmed CRS/datum metadata and the LAStools directory from the
preparation (or uses the optional LAStools `bin` argument, e.g. a newer
versioned install), uses the repository's benchmark defaults with the 1-/2-core matrix,
and refuses to overwrite an existing workspace. It does not stage inputs or run
workloads. Review/edit `benchmark.R` before preflight if needed.

Preflight only stages/checks inputs and checks configuration and executable
availability; it does not run workloads or establish numerical acceptance.
The pilot tests all defined tasks with lidR, lasR and LAStools at 1 and 2 cores,
with one warm-up and three measured repetitions per task/tool/core combination.
Tools run sequentially, not as 16 competing benchmark workers. The 16-worker
default applies to data preparation/verification, not this small pilot matrix.
Strict validation remains enabled: unsupported/blocked combinations are skipped,
and known cross-tool differences can fail validation; timings alone are not
acceptance. LAStools requires the appropriate installed tools and entitlement.
Results are recorded under `<workspace>/reports/campaigns/<campaign-id>`.
Use the same configuration and ID to resume an unchanged campaign; use a new
ID after changing the code, configuration or inputs.

To accept documented raster edge conventions, set
`accept_known_extent_discrepancies = TRUE` in the workspace configuration and
use a new campaign ID. This allows at most one aligned extra cell per edge,
never missing core cells or changed core values/NoData masks. Accepted R outputs
are labeled `accepted_exception`; native numerical checks remain `unverified`.
This does not enable the broader surface-support exception policy.

Native software identity records executable checksums and LAStools software
builds, not the licensing banner, which can vary between identical `-version`
queries. Licensing notices are still shown; this does not establish entitlement.
By default, nonzero processing statuses fail. In particular, pinned `lascanopy`
can exit with status 1 when an all-point elevation metric deliberately sets its
cutoff below every input point. The explicit, frozen opt-in
`accept_known_lastools_cutoff_warning = TRUE` accepts only its complete
all-values-above-cutoff warning block for basic lascanopy metrics, with raw status
0 or 1. The warning must name a supplied input and the actual below-range cutoff;
other warnings, errors and nonzero statuses remain rejected. Raw status/logs and
the named execution exception are retained. Complete valid outputs are still
required, and native numerical validation remains `unverified`, excluded from
timing/comparison summaries. The current pilot configuration enables this opt-in;
the example/default configuration does not. Do not normalize elevations or
raise the cutoff to hide the warning.

Historical pilots remain supported by selecting `historical_input_root` instead
of `prepared_stage_root`; do not configure both sources. The original
`--preflight`/`--run` forms without workspace arguments still use
`LIDAR_BENCHMARK_CONFIG` and `LIDAR_CAMPAIGN_ID`.

Metrics and maximum DSM are checked against each tool's grid-edge ownership.
`boundary_difference_points` records differing point assignments; affected
attempts are descriptive only, excluded from cross-tool comparison counts.
lasR DEM uses an independent unextrapolated ground TIN and an explicit
Float32 precision budget; DEM remains non-comparable. Non-repeatable lasR
first-return TIN DSM is always skipped, including diagnostic mode. Native
warnings outside the explicit cutoff exception, errors and missing tiles remain
rejected. Use a new campaign ID
after these validator changes; historical results are not rewritten.

Tests are named by responsibility (for example, workloads, validation, monitoring
and reporting). See [WORKLOADS.md](WORKLOADS.md) for current saved-data identifiers
and compatibility requirements. The optional completed-pilot review test uses
`LIDAR_PILOT_REVIEW_ROOT` to select its campaign directory.

## Performance-only reports

To summarize recorded timings without requiring numerical equivalence:

```sh
LIDAR_REPORTS_PATH=/data/nvme2/LidarSpeedTestWorkingDir/fresh-benchmark-pilot/reports \
LIDAR_CAMPAIGN_ID=fresh-pilot-006 \
LIDAR_REPORT_MODE=performance-only \
Rscript --vanilla R/5_report.R
```

This creates a new report directory within the selected campaign; no benchmark
rerun or changes to its execution records are needed. The default report mode
remains `validated`.

Performance-only mode includes successful attempts whose immutable validation
receipt records structural success, even when numerical validation is
`unverified`. It retains existing validated/accepted-exception timings too.
Validation labels are unchanged; `n_unverified` counts included unverified
measurements, and `n_comparable` is always zero. Times describe each tool's own
output, not equivalent-result speedups. Failed/skipped execution, failed/missing
validation, warmups, superseded retries and invalid/sub-0.1-second timings remain
excluded. Existing error/warning and artifact checks are not relaxed.

### Lightweight execution checks

Reporting mode and execution validation are separate settings. To avoid
expensive input-cloud decoding and reference calculations during a new
performance-focused campaign, set this in the workspace's `benchmark.R`:

```r
validation_mode = "performance-only"
```

The default is `"numerical"`, retaining the existing exhaustive checks.
Performance-only execution still requires successful processing with acceptable
logs, exactly the expected nonempty/readable primary files, CRS/grid/band
metadata, finite raster coverage and valid output coordinates. It reads output
payloads but never builds buffered input clouds or numerical references.
Point-cloud output checks still decode the output once to check its payload;
they do not reread the input or neighboring clouds. Input checksums, prepared
readiness checks, campaign identity and blocked-workload policies are unchanged.
These safeguards can still take time, but no per-output input-cloud
decompression/reference loop runs.

Successful checks are recorded as `unverified`, with explicit structural-success
and validation-mode fields in the attempt and immutable validation receipt.
They are resumable under the same frozen campaign and appear only in
performance-only timing reports, not numerically validated comparisons.
Changing `validation_mode` requires a new campaign ID; stop any active run first.

Warmups are optional. Set `scheduling$warmups` to `0L` to omit them; retain three
measured repetitions. Without warmups, first-use overhead can affect the first
measurement. Neither setting flushes caches or guarantees a particular cache
state. Warmup settings are frozen too, so changing them requires a new campaign.

## Full benchmark on prepared data

The full runner uses all nine task definitions, recording explicit skips for
unsupported/blocked combinations. It no longer selects only `pixel_metrics_2`.
Initialize a separate workspace; inputs are read directly, never copied or
regenerated:

```sh
Rscript --vanilla R/3_runBenchmarks.R --init /path/to/fresh-data/full /path/to/new-full-workspace /path/to/LAStools/bin
```

Review `<workspace>/benchmark.R` before preflight. It selects the 100 prepared
tiles at densities 1, 2, 5, 10, 20 and 50; cores 1, 2, 4, 8, 16 and 32; all three
tools; one warmup and three measured repetitions. Explicit `datasets_org` paths
replace the historical dataset layout. The optional native-bin argument permits
a newer LAStools install than the preparation's pinned build. As with pilot
initialization, exception policies default to `FALSE`; enable the known extent
and cutoff policies explicitly if required. CPU/RAM monitoring defaults to
`FALSE`, as in the pilot; enable `resource_monitoring` before starting if needed.

Preflight checks readiness, all 600 tile/index completion-receipt checksums,
dataset inventories, configuration and executable availability. It does not
process workloads, create a campaign or establish numerical acceptance:

```sh
Rscript --vanilla R/3_runBenchmarks.R --preflight /path/to/full-workspace
# Only after preflight succeeds:
Rscript --vanilla R/3_runBenchmarks.R --run /path/to/full-workspace fresh-full-001
```

By default all task definitions are selected. Add `--task <task-id>` to preflight
or run only one task, without editing the runner:

```sh
Rscript --vanilla R/3_runBenchmarks.R --preflight /path/to/full-workspace --task simple_metrics_1a
Rscript --vanilla R/3_runBenchmarks.R --run /path/to/full-workspace simple-metrics-001 --task simple_metrics_1a
```

`simple_metrics_1a` is an alias for the saved task ID `pixel_metrics_1a`;
other tasks use their existing IDs (e.g. `pixel_metrics_2`, `generate_DEM`).
Unknown IDs are rejected with the available task list. The filter retains all
configured datasets, cores, tools and repetitions: simple metrics alone has
432 scheduled entries with the default matrix. Use a separate campaign ID from
the all-task run and repeat the same `--task` when resuming. `--task` also works
with the environment-variable invocation; it does not apply to `--init`.

The current configured workspace is
`/data/nvme2/LidarSpeedTestWorkingDir/fresh-benchmark-full`, with LAStools 261023
and the same enabled extent/cutoff exception policies as the successful pilot.
It now uses `validation_mode = "performance-only"` and zero warmups:
**324 measured entries** for simple metrics alone, or **2916** for all tasks.
The interrupted `simple-metrics-001` campaign is retained unchanged; use a new
ID such as `simple-metrics-002` with the updated settings.
No benchmark is launched by initialization or preflight.

`cli` reports overall `Task X of N` progress (3888 with the example defaults),
with the workload, tool,
dataset/core/storage run, phase and repetition. Each entry is one scheduled
tool call, not one tile; warmups, skips and already-complete entries count toward
schedule traversal. The progress bar follows `cli`'s terminal defaults, while
the numbered messages also appear in redirected logs. Completion of the
schedule does not imply every workload succeeded; inspect the recorded outcomes.
Tools remain sequential and workload timing excludes progress/status messages.
Campaign identity/input hashing before dispatch can take several minutes.

Report the completed campaign without requiring equivalent numerical outputs:

```sh
LIDAR_REPORTS_PATH=/data/nvme2/LidarSpeedTestWorkingDir/fresh-benchmark-full/reports \
LIDAR_CAMPAIGN_ID=fresh-full-001 \
LIDAR_REPORT_MODE=performance-only \
Rscript --vanilla R/5_report.R
```

Performance-only reporting alone does not change execution checks; use
`validation_mode` for lightweight execution. Neither mode unblocks known-broken
workloads. Use a new campaign ID after changing source code,
configuration or inputs. Invoking the runner without arguments still supports
`LIDAR_BENCHMARK_CONFIG`/`LIDAR_CAMPAIGN_ID`, now selecting all task/run combinations.
