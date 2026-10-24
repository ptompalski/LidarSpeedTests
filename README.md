All content moved to:
https://ptompalski.github.io/LidarSpeedTests/

## Reproducible environment

The development target, confirmed for Chat 3, is **Linux and R 4.6.1**.
This checkout uses Ubuntu 26.04.1 LTS (x86-64); it is not a reproduction of the
historical Windows machines. Changing OS, native libraries, hardware, or tools
defines a different environment, not a package-version-only comparison.
`.R-version`, `DESCRIPTION`, and `renv.lock` declare the runtime and R dependencies.
This remains a script repository, not an installable R package.

Install R 4.6.1 separately (renv does not install R). On Ubuntu, source builds
need a C/C++20/Fortran toolchain and native headers. Install prerequisites with
your administrator, for example:

```sh
sudo apt-get install build-essential gfortran cmake pkg-config \
  libgdal-dev libgeos-dev libproj-dev libudunits2-dev \
  libcurl4-openssl-dev libssl-dev libxml2-dev \
  libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
  libpng-dev libtiff-dev libjpeg-dev libwebp-dev libgl1-mesa-dev libglu1-mesa-dev
```

Names vary by distribution. For source-build errors, install the named missing
library/header rather than bypassing configure checks. The checkout's baseline
native tools are GCC/G++/GFortran 15.2.0, CMake 4.2.3, GDAL 3.12.2,
GEOS 3.14.1, PROJ 9.7.1, HarfBuzz 12.3.2, and FriBidi 1.0.16.
R reports the system reference BLAS (`libblas.so.3.12.1`) and LAPACK 3.12.0.
renv does not freeze these libraries, BLAS, compiler flags, or the OS. Capture
their versions again on the eventual campaign machine.

Install **Quarto 1.10.18** separately from its official
[release distribution](https://github.com/quarto-dev/quarto-cli/releases/tag/v1.10.18)
and place `quarto` on PATH. The checker also accepts `QUARTO=/path/to/quarto`.
Quarto supplies its own Pandoc; the R rendering stack (`knitr`, `rmarkdown`,
`gt`, and other report dependencies) is in the lockfile.

From the repository root with the matching R:

```sh
# R automatically sources .Rprofile and bootstraps the pinned renv.
Rscript -e 'renv::restore(strict = TRUE, retry = FALSE, prompt = FALSE)'
# --vanilla skips .Rprofile, so activate explicitly in such commands.
Rscript --vanilla -e 'source("renv/activate.R"); renv::status()'
Rscript --vanilla R/check_environment.R
Rscript --vanilla tests/testthat.R
```

Track `.Rprofile`, `.R-version`, `DESCRIPTION`, `renv.lock`, `renv/activate.R`,
and `renv/settings.json`; do not track libraries or caches.
`.Rprofile` selects renv's supported classic lockfile format for compact,
hash-bearing package records.
The explicit snapshot uses `DESCRIPTION` so dependencies embedded in workload
strings, commented preparation code, tests, and Quarto reports are not lost.
`R/` is no longer excluded by `.renvignore`. lasR is installed from
`https://r-lidar.r-universe.dev`, not an unrecorded GitHub HEAD or a substitute
CRAN package; its lock record retains repository/source provenance.
To intentionally update dependencies outside a frozen campaign:

```r
renv::install("lasR", repos = c(
  r_lidar = "https://r-lidar.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
))
# Edit DESCRIPTION when adding/removing direct dependencies, then:
renv::install()
renv::snapshot(prompt = FALSE)
```

**LAStools is external and not supplied by this repository or renv.**
Before enabling it, obtain the appropriate Linux distribution from rapidlasso,
verify the exact native executable names in `benchmark.example.R` and
`R/0_setup.R`, and record distribution/version, source URL, binary SHA-256
checksums, and license/entitlement. For example, use `sha256sum` on
`lascanopy64`, `lasheight64`, `lasgrid64`, and `blast2dem64` in the configured
installation directory. The version-query helper alone does not establish binary
identity or licensing. LAStools includes differently licensed components; do not
assume that availability, a successful exit, or a demo executable permits an
unrestricted benchmark. No LAStools distribution or license has been verified in
this checkout, so an all-tool campaign remains blocked.

The environment checker loads the locked R packages and reports linked
geospatial versions, but does not launch benchmarks, download point clouds, or
render/overwrite `docs/`. Setup, plotting helpers, real catalog metadata, and
lasR pipeline construction are covered by tests. Campaign reporting is described
below; real-input numerical acceptance and native Windows behavior remain
external limitations. Chat 5's workload definitions, supported DSM API migration
and remaining equivalence limitations are documented in [WORKLOADS.md](WORKLOADS.md).

Catalog metadata now uses supported `lidR::npoints()`, `sf::st_area()`, and
catalog header accessors: counts are exact and densities are unrounded, using
summed tile areas and first-return counts as in lidR's catalog display.
Printed “thousand/million” values are no longer mistaken for raw counts.
RAM is reported in bytes through `ps::ps_system_memory()`; the strict `Z > 2`
metric population is unchanged. Chat 5 changes its units to a user-confirmed
fraction and standardizes band names for new attempts only; historical records
are not rewritten.
The historical plotting helper retains Segoe UI. New campaign figures use the
platform's generic `sans` font and a new descriptive layout; historical visual
parity is not claimed or required.

## Running the benchmark harness

Workload definitions and relative dataset paths are in `R/0_setup.R`. Copy
`benchmark.example.R` to the ignored `benchmark.local.R` and edit its machine-local
paths, core counts, and explicit tool selection. Alternatively, set
`LIDAR_BENCHMARK_CONFIG` to a trusted R file containing the same `list(...)`.
Run scripts from the repository root; relative configuration paths resolve there.
No machine configuration is required for compilation/reporting. Both use
`LIDAR_REPORTS_PATH` (default `results`) independently of `benchmark.local.R`;
set it explicitly when raw records live elsewhere. Compilation requires explicit
campaign selection or explicit legacy mode below.

Create report and scratch directories yourself before running. Input roots must
contain the datasets declared in setup. Input, report, and scratch trees must not
overlap; scratch roots cannot be a filesystem root, home, or the working directory.
The runner validates every selected input and required executable before starting
processing. Missing tools fail explicitly, without substituting binaries or
silently dropping enabled tools. On Windows use native paths such as
`C:/LAStools/bin`; on Linux use paths to the installed native tools. The exact
configured LAStools names (including `lasheight64` and `lasgrid64`) must exist;
`.exe` is appended only on Windows.

External commands use `processx` executable/argument vectors, not shell command
strings. Each input filename is a separate argument, including filenames with
spaces or shell metacharacters. LAStools task definitions use
`list(executable = "tool", args = c(...))`; the whole-argument placeholders
`{inputs}`, `{output}`, and `{cores}` expand without parsing or shell evaluation.
Metric calls also use `{height_cutoff}`, derived below the catalog's minimum Z
before timing so lascanopy does not silently exclude points below 1.37.
Result CSVs retain exit status, stdout, stderr,
and error/warning diagnostics. R dependencies, including `processx` and `future`,
are restored from the project lockfile.

Each non-skipped tool call creates a unique run-owned directory inside its
configured scratch root. Outputs are counted before that directory is removed
on success or failure; failed outputs are not retained. Cleanup refuses unowned
directories, changed ownership handles, and linked/redirected scratch paths.
It never resets a configured root or input/report directory. Worker strategies
and monitoring processes have error-path cleanup. If cleanup cannot be completed,
the failure is reported rather than treated as success; unsafe scratch paths are
left untouched, and scratch is retained if resource cleanup fails. Diagnostics
identify retained scratch directories for manual inspection.

### Campaign resource monitoring (Chat 9)

The chosen campaign target is **Linux whole-machine monitoring**. Enable it with
`resource_monitoring = TRUE` and configure
`monitoring = list(scope = "whole-machine", interval_seconds = 1)` (the defaults
for older configurations). Intervals must be between 0.05 and 60 seconds.
The scope is explicit so a future process-tree backend can be added without
silently changing measurement meaning; `process-tree` is currently rejected.
Enabled monitoring requires readable Linux `/proc/stat` and `/proc/meminfo`.
Other platforms fail explicitly before execution; opt out with
`resource_monitoring = FALSE`, which records `disabled`, not zero utilization.
The old `cpu_ram_usage.ps1` is retained only as a historical Windows logger:
the campaign runner no longer launches it or uses `powershell`, `monitor_script`
or a shared `system_monitoring_path`.

A supervised R subprocess starts after worker/thread setup and before timing.
Readiness requires its initial counter snapshot, within ten seconds. Each
attempt directory holds `monitor-samples.csv`, stdout/stderr logs, readiness/stop
receipts, and `monitor-report.rds`. Samples contain the attempt ID (inside its
campaign directory), scope, platform, units, requested sampling interval, UTC
Unix-epoch interval endpoints, observed monotonic interval duration, CPU percent,
and used/available/total RAM in bytes. Actual intervals can exceed the requested
cadence under load. CPU is the aggregate busy fraction across all logical CPUs,
on a **0–100% machine scale**, excluding idle and iowait; guest ticks are not
double-counted. RAM used is `MemTotal - MemAvailable`, not process RSS or simply
total minus free RAM. These definitions are not interchangeable with historical
Windows counters.

On workload success or failure, shutdown requests a final counter snapshot,
waits for a clean exit and reaps the process before validation/cooldown.
Unexpected exit, invalid counters, missing samples and shutdown timeout are
explicit failures, never successful/zero-valued measurements. Timeout triggers
process-tree termination; existing resource-cleanup failure/scratch-retention
semantics still apply. Setup/shutdown, validation, checksums, cleanup and cooldown
remain outside workload timing. Startup overhead can perturb the machine even
though it is untimed. Supervising the child protects normal parent exit; an
OS/machine crash can still leave an incomplete attempt, which cannot be compiled.

Summaries use only sample intervals overlapping the exact timed workload:
`cpu_mean_percent` and `ram_mean_used_bytes` are overlap-duration-weighted;
`cpu_peak_percent` and `ram_peak_used_bytes` are maxima of overlapping samples.
CPU is an interval average, and RAM is an endpoint snapshot held over that
interval for weighting; subinterval clipping is an estimate, not instantaneous
attribution or a guarantee to capture brief peaks. The report and result's
`monitoring_timed_*_epoch` fields preserve subsecond timed boundaries (unlike
historical CSV datetime precision). Coverage seconds/fraction and overlapping
sample count are explicit; incomplete coverage is `partial`. No overlap is an
error. A stable system clock is required. These are **whole-machine**, not
workload-only measurements: unrelated jobs, OS services, file cache and the
monitor itself contribute. Keep the machine otherwise idle and record unrelated
activity; tool serialization alone cannot establish isolation.

The report stores the raw-sample checksum and deterministic summary. Its checksum
is in the immutable result; campaign ingestion verifies both files and recomputes
the summary, rejecting changed/missing samples or reports. Summaries retain
failed attempts and warm-ups; they do not make them measurement-eligible.
Skipped/pre-monitor-preparation failures record `not_run` when enabled, monitor
failures record `unavailable` with execution/log diagnostics, and pre-Chat-9
records are labeled `unavailable-historical` during outcome compilation.
No historical samples are invented or migrated. The monitoring subsystem does
not generate publication intermediates; the Chat 10 report builder below does.

Review the selected combinations in `R/3_runBenchmarks.R` before sourcing it.
Do not run a full campaign until inputs, dependencies, and workload contracts
have been approved.

### Campaign identity and immutable attempts (Chat 6)

For an authorized run, set `LIDAR_CAMPAIGN_ID` to a new filename-safe identifier
before sourcing the runner. The selection `B`, selected task definitions/run
matrix, validated machine configuration, code revision and execution-source dirty
state/checksums, `renv.lock` checksum, actual R/package versions and installed
R-code/native-library checksums, selected native executable checksums and raw
`-version` output, OS/CPU/RAM and relevant storage mounts/devices/filesystems,
geospatial libraries, and parallelism policy/core
counts/thread environment are frozen in
`<reports_path>/campaigns/<campaign_id>/manifest.rds`. Every file in each selected
input directory (including LAX indexes) is checksummed. MD5 is a byte-identity
check, not a security signature or proof of dataset/tool provenance or licensing.
Reusing the ID requires an exact match; changes require a new ID, not an edited
manifest. Paths and hardware are deliberately part of identity.

`benchmark_campaign(id, B, benchmark_tasks, benchmark_runs, benchmark_config)`
creates or resumes that manifest. Pass its returned handle as `campaign` to
`benchmark_run()`. Identity is recomputed before dispatch, even when existing work
could otherwise be skipped. This involves reading all selected input bytes and
installed package code, outside timing, and can be expensive; it does not trust
file size/mtime as a checksum cache. Do not modify inputs/environment during a
running call. The scheduling and parallelism policy below is also frozen.

Each invocation has an explicit positive integer `repetition_id` (default `1`
for low-level calls). Each tool attempt reserves a
unique directory under `attempts/` containing `started.rds`, an atomic
`result.csv`, and a checksum receipt `completed.rds`. Campaign, identity,
repetition and attempt IDs accompany the outcomes, timing, diagnostics and
metadata. Validated successful and explicitly skipped work resumes without
re-execution; execution/validation failures and unverified work retry automatically.
Existing `force_rerun_*` flags append another
attempt, never overwrite one. Metadata/preparation failures also get attempt
records. Interrupted/uncompleted attempts remain on disk and never count as
completed work. A retry gets its own ID; it does not erase the interrupted
attempt. Compilation explicitly blocks on incomplete attempts rather than
silently omitting them; preserve that campaign for diagnosis and use a new
campaign for reporting if interruption recovery is needed. Completion is
single-writer per attempt, and reading detects changed result bytes/identities
and changed/missing validation reports.

### Repetitions, ordering and timing policy (Chat 8)

`benchmark.example.R` declares `scheduling`: **3 measured repetitions, 1 warm-up
round, seed 20261020, 5 seconds cooldown, `uncontrolled-warm` cache policy and
1 native thread per R file worker**. Older configuration files receive these
same documented defaults during validation. Counts, seed and cooldown can be
changed before campaign creation; nested native threading and other cache
policies are rejected. A policy change requires a new campaign ID.

The manifest freezes the entire schedule, not just the seed. For each selected
task/run and phase, a seeded random tool permutation is rotated across
repetitions: every tool appears once per repetition, and each tool's counts
across order positions differ by at most one (exact balance at multiples of
the tool count). Task/run block order is shuffled each round. Input selection
row order and configured tool order do not change the generated schedule, and
scheduling uses Mersenne-Twister/Inversion/Rejection independently of the
caller's RNG kind and preserves the caller's RNG state. Unsupported/blocked tools keep
their scheduled slots and explicit skipped records; they do not execute work.
Balance describes the planned slots, not a guarantee of equal executed samples
when tools skip/fail or completed measurements are resumed.

The runner uses `benchmark_execute_schedule()` and dispatches **one tool at a
time**, never futures across benchmark tools. An exclusive directory lock under
the reports directory prevents overlapping harness calls sharing that directory,
including cooldown. An interrupted process can leave this lock behind: verify
that no runner is alive before manually removing that specific empty lock.
Separate report roots and unrelated programs are not covered; keep the machine
otherwise idle. This is not process-tree monitoring (Chat 9).

Warm-ups run through the same preparation, workload, validation and cleanup path,
with immutable `phase = "warmup"` records, never as measured repetitions.
`phase`, `repetition_id`, `schedule_index` and `tool_position` link scheduled
attempts to the manifest. Warm-ups are repeated on every scheduler invocation,
including resume, because a previous process's cache state is not reusable.
Completed measured work resumes independently of warm-ups; failed measurements
retain their attempts and retry. Low-level `benchmark_run()` calls remain
available for explicit retries, default to measurement, and have missing schedule
indices unless a matching frozen schedule row is supplied.

**No cold-cache claim is supported.** Input checksums, metadata, warm-ups and
untimed numerical references all touch inputs; the OS/tool caches are neither
evicted nor verified. Warm-ups do not guarantee uniform cache residency,
especially when they fail, or when the working set exceeds RAM. A warm-up count
of zero is allowed but still means uncontrolled cache, not cold cache.

The recorded interval starts immediately before the workload call and stops
when it returns/errors. It includes R expression evaluation or native process
launch, input I/O, computation and artifact persistence. Campaign checksums,
metadata/catalog preparation, scratch allocation, worker/thread setup, monitor
startup/cleanup, artifact validation and scratch cleanup are **outside timing**.
After each attempted workload with a start timestamp (including failures), its
result is persisted and the configured cooldown sleeps **outside timing** and
after cleanup; skipped, pre-execution failures and resumed work do not sleep.
Cooldown is an explicit minimum delay, not a verified thermal/power equilibrium.

Run `cores` is the total processing budget: lidR uses that many multisession
file workers with `set_lidr_threads(1)` in the parent and worker startup; lasR
uses `concurrent_files(cores)` with no nested point parallelism, verified by
strategy readback; LAStools receives `-cores cores`. Data-table threads are
limited to one. OpenMP nesting is disabled, native outer thread limits match
the budget, and child-process BLAS/Rcpp thread environment limits are one.
Thread environment, lidR/data-table counts and future plan are restored even
on failure; lasR's temporary strategy is unset during cleanup.

Already loaded threaded BLAS cannot reliably be capped with environment
variables. Execution therefore requires a recognized **reference single-threaded
BLAS** and fails explicitly for known threaded or unrecognized BLAS paths.
Other BLAS implementations need verified runtime control before support can be
claimed; do not bypass this guard or relabel requested limits as observed CPU
usage. Attempt records contain configured `workers`, `native_threads`,
`blas_threads`, cache policy, cooldown and timing boundary. These are execution
settings, not utilization measurements or proof of proprietary-tool internals.
Warm-ups remain in compiled raw records, but `measurement_eligible` requires
measurement phase plus execution/validation success. Reporting below additionally
selects one attempt per repetition and flags invalid/sub-0.1-second timings.
Pre-Chat-8 campaign records remain readable as measurements with missing
schedule indices; they are not retroactively assigned balanced slots. Their
identity cannot resume under the changed execution policy.

Compile only one explicitly selected campaign:

```sh
LIDAR_REPORTS_PATH=/path/to/reports LIDAR_CAMPAIGN_ID=approved-pilot Rscript --vanilla -e \
  'source("renv/activate.R"); source("R/4_compileResults.R")'
```

This writes `compiled.rds` inside that campaign, retaining **all attempts and
repetitions**, including failed retries. It never chooses package versions per
task or deduplicates retries. Raw manifests/attempts are not overwritten.
For historical top-level CSVs, explicitly unset `LIDAR_CAMPAIGN_ID` and set
`LIDAR_RESULTS_MODE=legacy`; that separate path writes the historical `r.rds`,
labels records `legacy-unidentified`, and retains every recorded version rather
than guessing the newest. Legacy mode refuses campaign-schema CSVs. Neither
mode silently falls back to the other or pools campaigns. Campaign report pages
use the separate reproducible build below, never legacy RDS/PNG files.
The maximum-output-count heuristic has been removed: expectations
now come from each attempt's input/workload manifest, never from peer results.

### Campaign tables, figures and Quarto site (Chat 10)

From the repository root in the restored environment:

```sh
LIDAR_REPORTS_PATH=/path/to/reports LIDAR_CAMPAIGN_ID=approved-pilot \
  Rscript --vanilla R/5_report.R
```

This single route verifies the selected campaign's immutable manifest, result
receipts, validation reports and monitoring samples, selects attempts, generates
intermediates/CSV tables/PNG figures, then renders every Quarto page. It never
sources `R/0_setup.R`, reads machine configuration, starts workloads/monitors, or
requires access to the original point clouds/executables. Missing, corrupt,
incomplete or empty campaign records block the build rather than silently
falling back to historical results. Legacy mode is explicitly rejected here.
No input files or raw attempts are rewritten. Build only after execution has
stopped: active execution locks and changed raw-record snapshots fail explicitly.

The printed path is a fresh directory
`<reports_path>/campaigns/<campaign_id>/report-<unique-id>/`. Each build contains:

| Output | Meaning |
|---|---|
| `report.rds` | Selected campaign manifest, all attempts, summaries and availability |
| `compiled.rds` | All attempts with selection/exclusion/timing/comparability fields |
| `CPU-RAM.rds` | Replacement whole-machine utilization summary generator |
| `complex1.rds` | Replacement summary of actual complex workload calls, both task IDs |
| `tables/*.csv` | Basic, complex, storage, tile-size, utilization, attempt ledger and availability |
| `figures/*.png` | Available timing/CPU/RAM means with sample-SD bars |
| `build.rds` | Successful-build receipt: raw/report-source checksums, policies, R session and Quarto version |
| `render.log`, `_site/` | Render diagnostics and complete local HTML site with downloadable tables/figures |

These are **new replacements**, not recovered historical generator semantics or
schema-compatible reproductions of old RDS files. No copying from `_OLD/`,
`graphics/`, root-level RDS files or `docs/` is required. Published historical
`docs/` and existing graphics remain unchanged. `_quarto.yml` defaults to
ignored `_site/`, explicitly lists the eight report pages, and disables cache/
freeze reuse. Do not change the output directory to `docs/` for a test render.
Root rendering without a generated `report.rds` fails with the build command;
use the staged site, not manual copying of intermediates. The new templates
replace the historical source narrative; previously published HTML is retained.

Selection is deterministic and conservative: for each tool/task/run/phase/
repetition, use the latest precise `started.rds` timestamp; lexical attempt ID
breaks an exact tie. A failed last retry does **not** fall back to an earlier
success. All retries and warm-ups remain in the ledger with explicit exclusions.
Timing aggregates require measured phase, recorded execution success, passed
artifact validation or a user-accepted surface exception (see Chat 11 below),
and finite elapsed seconds >= 0.1. Short durations remain
flagged without changing execution/validation status. The summary includes
planned, selected, missing, included, excluded, failed, skipped, validation-failed
and short counts plus mean, sample SD, median and range. One sample has NA SD.
Manual unscheduled repetitions remain attributable to their campaign and can
increase selected counts beyond the frozen planned count; no schedule is invented
for old/manual records. Old schemas without a schedule have unknown planned/
missing counts.

Timing uses immutable monitor-bound subsecond epochs where present; otherwise
the legacy result CSV datetime serialization has **second resolution**. The
ledger states which precision was available. Unmonitored subsecond measurements
cannot be retrospectively recovered or certified. Complex summaries use actual
end-to-end workload calls only; independent basic timings are never summed into
a claimed sequential workflow/CHM. Complex calls remain blocked by default; explicit Chat 11 diagnostic opt-in
enables execution without making their failed/unverified outputs eligible.

Non-comparable/unknown-contract timings are clearly descriptive, not equivalent
cross-tool comparisons. `n_comparable` additionally requires a recorded matched
definition/group and validated output; this is not proof of general real-input
equivalence. Storage/tile-size outputs retain individual run strata and standard
run-ID dimensions, without inventing device types or causal speedups. Unknown
run-ID dimensions are labeled unavailable; fewer than two **measured**
configurations cannot establish a comparison. Consult the campaign's input
checksums/mounts before comparing strata.

Utilization requires eligible full-coverage whole-machine monitoring. Missing,
disabled and partial monitoring is not zero; raw diagnostics, coverage and sample
counts remain in the ledger. Repetition means/SDs weight eligible attempts equally
after the per-attempt duration-weighted calculation; peaks are maxima across
eligible samples. CPU uses percent and RAM uses bytes (GiB in figures), never
historical Windows percentages or process RSS. Every section labels unavailable
measurements explicitly; no missing figure is linked as if it existed.

For intermediates without rendering, use the same generator from the root:

```r
source("renv/activate.R")
source("R/1_functions.R")
source("R/reporting.R")
benchmark_build_report("/path/to/reports", "approved-pilot", render = FALSE)
```

That produces a fresh staged build with `rendered = FALSE` in its receipt; no
HTML is claimed. For a successful rendered build, open `_site/index.html` or
serve that `_site/` directory locally. Builds are kept separately for provenance;
incomplete builds have no `build.rds` success receipt. Real campaign publication
and the authorized pilot remain separate tasks (Chats 11-12).

### Artifact correctness (Chat 7)

`prepare_input()` now stops on lidR chunk errors. The runner builds a validation
manifest from the input catalog headers/file checksums before timing, then checks
every required per-tile artifact after execution/resource monitoring and before
owned-scratch cleanup. Validation/reference computation is **outside timing**.
It can reread neighboring point payloads repeatedly and run untimed reference
algorithms; this is deliberately correctness-first, not an authorized full-data
campaign or a guarantee of low validation cost.

Each inspected attempt retains `validation.rds` beside its immutable result CSV.
The report includes input identity/bounds, required filenames, actual output
checksums, per-artifact failures or numerical error/support summaries, tolerances
and validation timestamps. Its checksum is bound into the result CSV. Primary
artifacts themselves still live only until scratch cleanup. Execution `success`,
`execution_status`, diagnostics and timing remain separate from
`validation_status` (`passed`, `failed`, `unverified`, `not_run`) and
`validation_diagnostic`; compiler `validated_success` requires execution success
**and** passed validation. Legacy records without validation are unverified, not
retroactively certified from counts or compared to the largest peer output.

See [WORKLOADS.md](WORKLOADS.md#artifact-validation-chat7-v1) for exact ownership,
support and tolerance rules. Synthetic neighboring metric fixtures pass, but
surface extent/support discrepancies are explicitly rejected. LAStools numerical
conventions, lasR tree equivalence and blocked normalization/complex workflows
are **not certified**; successful structural inspection alone is unverified.
Passing a tool-specific reference is not proof of cross-tool equivalence or
of real-input CRS, vertical units, licensing or provenance.

## Workload contracts (Chat 5)

[WORKLOADS.md](WORKLOADS.md) defines populations, thresholds, units, SD/quantiles,
buffers/grids, bands, NoData, artifacts and the complete supported tool/task
matrix. Inspect `benchmark_contracts` after sourcing setup. New CSVs carry
contract/comparability labels; these are not correctness certification.
LAStools metrics now use an all-Z cutoff and all-return density, but native
equivalence remains unverified. lasR uses supported `dsm()` APIs.

**lasR normalization and both complex tasks are disabled** after synthetic
fixtures found corrupt elevations from pinned TIN boundary normalization.
They record skipped attempts with the blocker diagnostic; candidate pipelines
and artifact requirements are retained, not claimed working. No complex
end-to-end workload is currently runnable. Basic DSMs use original elevations,
not canopy heights; summed independent timings are not a sequential pipeline.

## Fresh Petawawa download, repair and preparation

`R/prepare_fresh_data.R` is a **new preparation method**, not a reconstruction
of which historical repair was actually used. The current method uses a new,
empty destination, for example
`/data/nvme2/LidarSpeedTestWorkingDir/input_data_no_synthetic`; the previous
`input_data` run and recovered inputs remain untouched.
No real download or preparation was run while implementing it.
Sourcing the script only defines functions.

**Current policy (`fresh-petawawa-v4`): discard source synthetic points
before decoding, using the native `-drop_synthetic` filter.** Do not resume the
old v2 full run with this policy: use a separate local configuration and a new,
empty root, then prepare and verify a new pilot before full preparation.
Old repaired/prepared products cannot be reused because their point population
and density reference may differ. Frozen-policy guards deliberately reject
in-place upgrades, failed-preparation retries and parallel transitions from v2.
Existing outputs, receipts and the downloaded archive are not deleted.
V4 additionally discards source points with **Z < 0**, with individual removal
audits, before any other repair. Z = 0 is retained. This is explicit
user-authorized curation, not a claim that negative elevation is universally
invalid. An interrupted synthetic-filtered v3 full run can use the narrowly
guarded recovery below; another data root or download is not required.

Independent inspection of tiny uncompressed LAS 1.2/format-1 records
confirms the writer preserves the complete flag vectors. The first divergence
is inside the installed **rlas 1.9.5 native reader**, before lidR construction:
its `C_reader` can return one flagged point from 138 correctly flagged records.
In `SMART_POPULATOR_BOOL`, `auto fill_val = name[0]` retains a `vector<bool>`
proxy across `reserve()`, which invalidates it. An isolated C++ AddressSanitizer
reproducer confirms the use-after-free; taking a Boolean value before reserve
fixes that reproducer, **not the installed dependency**. The lost 137-point
prefix also explains the 376-versus-239 densified result. See the
[rlas 1.9.5 source](https://github.com/cran/rlas/blob/c3f5ad7b7d078decfb401eaa0a2c1987c443cb76/src/rlasstreamer.cpp)
and the latest development-plan handoff for evidence and bounded checks.

The filter examines native point flags before the affected R Boolean-vector
population, rather than subsetting an already decoded `Synthetic_flag` column.
Prepared reads do not filter: any remaining synthetic flag or synthetic read
diagnostic is fatal, so validation cannot silently hide flagged outputs.
An entirely synthetic source fails as empty. Tiny mixed-flag worker and
independent disk-bit checks cover this workaround; the installed dependency
has **not** been repaired or changed. The same upstream macro handles other
Boolean flags, so this is not a general dependency fix or proof of real-data
readiness. Preserve the old run for later recovery after an explicit dependency
fix/build-identity transition; do not edit frozen receipts.

Copy `fresh-data.example.R` to a trusted file outside git, for example
`/data/nvme2/LidarSpeedTestWorkingDir/fresh-data-no-synthetic.local.R`. Before the first
command, fill in `metadata_confirmation` and the non-secret `tool_license`
confirmation, and review the native tool paths. Never put licence keys in
configuration or provenance. Confirm the original delivery's **EPSG:2959,
EPSG:5713 and metre XYZ**; a conflicting embedded horizontal CRS is an error,
not permission to reproject. The vertical declaration is retained in provenance;
it is not a vertical transformation or proof supplied by the GeoPackage.
The output parent and protected historical root must exist. The new root must
be absent or empty; existing unrelated files and linked/overlapping roots are
refused. Configuration is frozen on initialization, including download settings.

**Run these commands yourself; they can take substantial time, RAM and disk.**
Use the restored Linux/R 4.6.1 project environment. Download requires the `curl`
executable and uses the original approximately 18 GB NFIS ZIP URL recovered
from the historical script, with HTTPS redirects, resumable `.part` transfer,
failure logs, ZIP inventory checks and a checksum receipt. Its live availability
and contents were not reverified here. Optional `archive_md5` is only meaningful
when obtained independently; a locally calculated MD5 is a content identifier,
not proof of authenticity.

```sh
CONFIG=/data/nvme2/LidarSpeedTestWorkingDir/fresh-data-no-synthetic.local.R
Rscript --vanilla R/prepare_fresh_data.R --download "$CONFIG"
Rscript --vanilla R/prepare_fresh_data.R --pilot "$CONFIG"
Rscript --vanilla R/prepare_fresh_data.R --verify-pilot "$CONFIG"

# After reviewing the pilot data-QC receipts, explicitly request all 100 tiles:
Rscript --vanilla R/prepare_fresh_data.R --full "$CONFIG"
Rscript --vanilla R/prepare_fresh_data.R --verify-full "$CONFIG"
```

The pilot extracts only `298000_5093000` and `298000_5094000`. The full stage
requires the successful pilot receipt, rechecks its files and refuses changed
implementation/tools/runtime. Both prepare **1, 2, 5, 10, 20 and 50 points/m2**.
Only selected members are extracted, one tile per worker; the original ZIP stays
immutable. Extra/missing/ambiguous tile matches or unsafe ZIP paths are errors.
Repair and preparation each use **eight tile workers by default**, capped to the
number of selected tiles (two for the pilot). Each preparation worker processes
its tile's six densities sequentially. OpenMP, data.table and lidR threads are
capped at one per worker; native/index-query subprocesses are supervised, so
eight workers is not a limit of eight operating-system processes.
Override the operational budget without changing the frozen configuration:

```sh
Rscript --vanilla R/prepare_fresh_data.R --full "$CONFIG" --cores 8
# Use --cores 1 for serial operation, or a smaller budget if RAM/storage is tight.
```

Download, checksum/preflight work, pilot re-verification and final verification
remain serial. Eight dense tiles can require substantial aggregate memory and
scratch space; this is not an adaptive RAM-based scheduler or a guaranteed
eightfold speedup. The benchmark runner's own core settings are unchanged.
The preparation lock prevents another preparation invocation, not unrelated
machine activity. After a crash, inspect partial outputs and verify no
preparation workers or native children remain before removing that specific
empty stale lock. Partial/unreceipted tiles are refused during ordinary resume;
the explicit interrupted-full recovery below archives them instead.

The user-approved repair (`fresh-petawawa-v4`) first discards source points
whose LAS synthetic flag is set, using native filtering before R decoding.
It then removes negative-Z source points before invalid-return, duplicate,
ground-XY or pulse cleanup, recording `drop_negative_Z`, filtered-input row
IDs and original Z in the action audit. This prevents a negative ground point
from displacing a valid ground return at the same XY. An entirely negative
retained source fails rather than yielding empty products.
It preserves the original coordinate
scales, offsets and
retained XYZ values: **no 0.01 m rounding**. It drops zero/out-of-range legacy
return fields, drops repeated XYZ locations retaining the first source row,
then retains the **lowest-Z class-2 ground point at each repeated XY** (first
source row on ties), followed by the highest-Z point for repeated pulse/return
labels (first row on ties). Ground cleanup does not change elevations or remove
non-ground points just because they share XY. Its audit records each removed
row's original Z, the chosen ground source-row ID and its Z.
Pulses use `PointSourceID + gpstime`, plus scanner channel when present.
Return counts are raised to the maximum reported count/return label in that
group; valid return labels are not renumbered, and partial tiled/thinned pulses
are not mistaken for complete pulses. These are explicit curation assumptions,
not recovery of unknown physical returns. Remaining QC defects still cause a
failure rather than an undocumented fix.
Each repaired tile has a sidecar with source-member/checksum/original header,
read warnings, native filter, original header point count, retained count and
discarded synthetic count. Repair actions and retained-row mappings use
**row IDs in the filtered input**, not original archive row numbers; native
filtering does not produce individual discarded-row IDs. Later repair removals
are audited separately. Density counts and area use the repaired, filtered source.

Each density gets `round(target * repaired source area)` points. Thinning samples
points without replacement. Densification retains the original points and adds
seeded copies with XY jitter up to 0.25 m, rejecting collisions and coordinates
outside the repaired source bounds, and Z noise drawn from
`+/-seq(0.10, 0.50, 0.05)` m on the original coordinate grid. Copied GPS-time
ranges are disjoint. Classification and other attributes/flags are preserved;
**synthetic derivation is labeled in the preparation manifest**, not by changing
the LAS synthetic flag. Distribute those provenance files with the data; these
copies are not additional observed laser returns. Z perturbation also applies
to copied ground points and is not a model of new ground-truth measurements.
This differs deliberately from historical Z-only densification and its rounded
source-density multiplier.

Outputs are under `pilot/` or `full/`: repaired sources and action sidecars in
`repaired/`, then benchmark LAZ/LAX files in
`prepared/1_org/tile_1000_density_<density>/`, preparation identity in
`prepared/preparation.rds`, and `ready.rds` only after all checks pass.
Checks cover finite/valid return attributes, warning/error-free `las_check()`,
non-collinear terrain support, named-tile bounds, confirmed CRS, exact target
counts, persisted full point records, original precision/format, sorting,
LAX signature plus an isolated lidR/rlas native indexed query against decoded
reference records,
and final inventory/checksums. Verification recomputes checksums and rereads
every prepared cloud; it is not a cheap existence check. The seed schedule is
stable across pilot/full: base seed + 100 * density position (zero-based) +
tile position in the sorted 100-tile inventory (one-based).

The LAS synthetic flag marks points according to their producer, not necessarily
this workflow. V3 deliberately excludes those source points without clearing
their flags or changing the archive. Retained records must have complete,
nonmissing logical synthetic flags, all FALSE. Source preflight, round-trip
validation and isolated index queries use the same strict checked reader;
prepared reads reject every warning. Raw reads retain non-synthetic diagnostics
for the approved repair audit, but reject every synthetic diagnostic.
All `las_check()` warnings/errors still fail.

Keep the archive, source/action receipts, manifests, process logs and final
checksums. Resume accepts only the same configuration, implementation, archive,
tools/runtime and intact completed outputs. Failed transfers retain `.part`;
failed repair diagnostics stay in the stage directory. Extraction scratch is
cleaned safely. Budget for the ZIP, repaired copies, all six density variants,
native sort scratch and several in-memory copies of a dense tile **per active
worker**. Parallel executions retain worker stdout/stderr under
`execution-<unique-id>/repair/` and `prepare/` in the stage directory, plus
start/finish receipts recording the requested/effective worker budget and
implementation. New artifact sidecars identify their execution; existing
completed sidecars remain unchanged. A worker failure stops active siblings
and their children rather than leaving the rest of the pool running. At a full
1 km2 per tile the six targets total approximately **176 million points for
the pair, or 8.8 billion for 100 tiles**, before scratch/repaired-source overhead.

### Resuming the v3 full repair after the negative-elevation stop

The reported failure for `309000_5091000` (`1 points below 0`) occurred during
repair QC, before the repaired tile was published. The user authorized dropping
negative-Z source points. Use the **same** synthetic-filtered configuration,
root and downloaded archive; do not edit existing identities or receipts:

```sh
CONFIG=/data/nvme2/LidarSpeedTestWorkingDir/fresh-data-no-synthetic.local.R
Rscript --vanilla R/prepare_fresh_data.R --resume-negative-z-full "$CONFIG" && \
Rscript --vanilla R/prepare_fresh_data.R --verify-full "$CONFIG"
```

Defaults remain eight tile workers with one native thread each; append
`--cores N` to the preparation command to change the budget.
Before resuming, confirm no prior workers/native children remain. A stale lock
must be inspected explicitly, never removed automatically.

This command supports only the exact prior v3 implementation pair
`3d79583687c92ed0cd74afab1defbbcb` / `b0a94be880119b1df463a011fc1b76a2`,
with unchanged other implementation files, configuration, archive, tools and
runtime. It requires the verified pilot and an interrupted **full repair**
stage without full prepared products/readiness. It records the explicit
v3-to-v4 transition in `negative-z-transition.rds` and the execution transition
in `full/parallel-transition.rds`, preserving the old root/stage identities,
pilot manifest/readiness and completed repair bytes/sidecars. Transition
receipts bind the old frozen-file hashes and the new implementation.

The pilot is reverified, and completed repairs are checksum/QC-checked before
reuse. Old repair action audits must show **no negative source Z**: negative
points removed by an earlier rule could have affected ground/pulse selection,
even if the final old repair has no negatives. Missing/ambiguous audits or such
negative removals therefore stop reuse rather than claiming equivalent output.
New repair receipts identify v4 explicitly and retain their negative-Z audit.
Unreceipted partial products and interrupted owned scratch are archived, not
deleted. The failed/missing repairs are generated under v4; existing completed
repairs and the pilot keep their original provenance. Density area/counts
derive from each repaired source as before. Ordinary `--full` works after a
successful explicit transition; repeat the explicit command if another
interruption leaves partial products.

### Restarting the first failed v1 pilot without another download

**Historical v1-to-v2 instructions only.** V3's source filtering is a different
policy and requires a new root; the current script refuses this upgrade.

If v1 stopped on repeated ground XY/different Z **before writing any repaired
or prepared files**, keep the existing configuration and download. After updating
the script, explicitly run:

```sh
CONFIG=/data/nvme2/LidarSpeedTestWorkingDir/fresh-data.local.R
Rscript --vanilla R/prepare_fresh_data.R --upgrade-failed-pilot "$CONFIG"
Rscript --vanilla R/prepare_fresh_data.R --pilot "$CONFIG"
Rscript --vanilla R/prepare_fresh_data.R --verify-pilot "$CONFIG"
```

The upgrade archives the old pilot directory and its root identity under
`pilot-failed-v1-<unique-id>/`, retaining the failure/read diagnostics, then
records the approved v2 policy. It neither downloads nor alters the ZIP.
It refuses changed configuration, active/stale execution locks, retained scratch,
any repaired/prepared files, a successful pilot or a full stage. Those cases
require inspection/a new root, not deletion of manifests or bypassing identity
guards. Only the failed v1-to-v2 ground-repair transition is supported.

### Retrying the failed v2 preparation after the index-query fix

The installed `lassort64` can reject a filtered spatial query because it expects
the unfiltered header count. Its documented `-extra_pass` did not resolve this
in the tested build. This was an error in our validation route, not evidence
that a spatial subset should contain every input point.
The query now runs through `lidR::readLAS(filter = "-inside ...")` in a supervised
R subprocess using the native rlas reader. All R warnings and captured native
warning/error diagnostics fail the check, including index failures followed by
scan fallback. Query counts, attributes and coordinates must still match the
full decoded reference. No native failure is reclassified as success.

For a failed v2 pilot with completed repaired tiles and no readiness receipt,
the following is **historical v2-only recovery documentation**, not a way to
switch to v3. The current v3 policy refuses that root; use a new root instead.
Under the matching v2 policy, the explicit preparation retry was:

```sh
CONFIG=/data/nvme2/LidarSpeedTestWorkingDir/fresh-data.local.R
Rscript --vanilla R/prepare_fresh_data.R --retry-failed-preparation "$CONFIG"
Rscript --vanilla R/prepare_fresh_data.R --verify-pilot "$CONFIG"
```

This is potentially long-running: it verifies the archive and repaired
bytes/receipts/QC, archives the prior `prepared/` tree and stage identity under
`pilot-preparation-failed-<unique-id>/`, then regenerates **all** density variants.
The repaired source files and ZIP stay in place and unchanged. Provenance links
the reused repairs to the archived preparation and its original implementation.
Changed configuration, repair policy, inputs, tools/runtime, retained scratch,
a successful pilot or a full stage are refused. Do not delete or edit frozen
manifests to make an ordinary `--pilot` call bypass identity checks.

### Resuming the stopped full run with parallel workers

For the full run stopped under the known preceding serial implementation, or
the initial parallel implementation that rejected valid source synthetic flags,
keep the same configuration, archive, verified pilot and completed outputs.
**Historical v2 recovery, not a v2-to-v3 migration.** V3 requires a new root and
refuses reuse of these products. For the old v2 run, do not execute this
recovery until the dependency and all mixed-flag checks have been fixed and the
frozen runtime/build transition explicitly authorized.
After confirming no preparation process remains and resolving any stale empty
lock as described above, run manually:

```sh
CONFIG=/data/nvme2/LidarSpeedTestWorkingDir/fresh-data.local.R
# Defaults to eight workers; optionally append --cores N.
Rscript --vanilla R/prepare_fresh_data.R --resume-parallel-full "$CONFIG" && \
Rscript --vanilla R/prepare_fresh_data.R --verify-full "$CONFIG"
```

This explicit transition accepts only these known preceding code identities
(or the current code), with unchanged policy, configuration, archive/members,
tools and runtime. Unknown implementation changes are refused. It preserves
the original stage identity and any frozen preparation manifest, recording
old/new identities separately in `parallel-transition.rds`. Completed files and
their receipts are retained and checked; unreceipted LAZ/LAX files and interrupted
`lidar-run-*` scratch move to the new execution's `interrupted/` archive, not
deletion. Missing products are generated with the unchanged per-tile/density
seed schedule and full QC. No redownload or unnecessary re-repair is required.
After this transition, ordinary `--full "$CONFIG"` resumes with the same code
and may use a different `--cores` budget. A completed full stage is not a valid
target for the explicit interrupted-run command. Do not edit frozen manifests
or delete partial products to force reuse.

`data-qc-passed` does **not** mean cross-tool numerical acceptance or resolution
of lasR/native workload defects. A new benchmark pilot and campaign ID are
still required. Do not use `R/6_pilot.R` unchanged: it is checksum-bound to the
old recovered files. The default runner's `benchmark_data/1_org` paths also do
not match these new `prepared/1_org` paths; review dataset mapping before any
separately authorized benchmark run. Retain the source attribution and
Open Government Licence - Canada terms described below.

Short checks use only temporary synthetic ZIP/LAS fixtures, with real
sampling/repair/LAS IO and LAX construction but stubbed LAStools sort/query
processes. They require `zip` for the ZIP fixture (explicit skip if absent);
production download/extraction does not require that executable. No native
LAStools correctness, original-archive acceptance or full-scale memory claim is
established by these tests:

```sh
timeout 90s Rscript --vanilla -e \
  'source("renv/activate.R"); testthat::test_dir("tests/testthat", filter = "^fresh-data$|^chat4-data$", stop_on_failure = TRUE)'
```

An additional opt-in `fresh-native` case uses just 225 synthetic points with
the installed `lassort64`/`lasindex64`, reproduces the failing filtered count,
and checks the supervised-reader replacement and corrupt-index rejection:

```sh
LIDAR_FRESH_LASTOOLS_PATH=/home/ptompals/opt/lastools/bin \
  Rscript --vanilla -e \
  'source("renv/activate.R"); testthat::test_dir("tests/testthat", filter = "^fresh-native$", stop_on_failure = TRUE)'
```

Without that environment variable this native case skips explicitly. It does
not establish acceptance of real inputs or all native benchmark workloads.

Bounded parallel tests check real process overlap, single-thread limits,
seed/RNG preservation, thread restoration, failure cleanup, partial-file
archiving and the guarded full-stage transition. A separate opt-in native case
repairs two 225-point synthetic tiles in isolated workers and prepares one
density with actual sort/index/query tools, comparing complete point records
against the deterministic reference. Those native fixtures deliberately carry
138 source synthetic flags; native filtering discards exactly those records
before repair. One additional negative-Z source point is removed and audited;
the filtered index query must match the flag-free, nonnegative reference:

```sh
timeout 60s Rscript --vanilla -e \
  'source("renv/activate.R"); testthat::test_dir("tests/testthat", filter = "^fresh-parallel$", stop_on_failure = TRUE)'
timeout 60s Rscript --vanilla -e \
  'source("renv/activate.R"); testthat::test_dir("tests/testthat", filter = "^fresh-negative-z$", stop_on_failure = TRUE)'
timeout 60s env LIDAR_FRESH_LASTOOLS_PATH=/home/ptompals/opt/lastools/bin \
  Rscript --vanilla -e \
  'source("renv/activate.R"); testthat::test_dir("tests/testthat", filter = "^fresh-parallel-native$", stop_on_failure = TRUE)'
```

The tiny `^fresh-flags$` cases exercise zero/one/138/all flagged source points,
exact native-filtered record retention, repeated reads, empty-source rejection,
flag-free repair/thinning/densification and fatal synthetic diagnostics/flags.
Independent disk-bit cases retain source/variant vector and mutation checks,
with GPS-time record matching. The fixture decoder checks
the signature, version, header size, data offset, point format/record length
and count/file size before inspecting the legacy classification flag bit.
These mixed-flag cases verify the workaround, not a fix to the dependency:

```sh
timeout 45s Rscript --vanilla -e \
  'source("renv/activate.R"); testthat::test_dir("tests/testthat", filter = "^fresh-flags$", stop_on_failure = TRUE)'
```

## Petawawa input preparation (Chat 4, historical-method route)

**Historical preparation was recovered**, not invented, from the user-supplied
`_OLD/R/2_data.R`, `99_IncreaseDensity.R`, and `99_repairPetawawaALS2012.R`.
Their MD5 identities at recovery were respectively
`b110ca6a754816d286027585b52dd09b`,
`9bddef7946565cfa4b5ac9c269287ec8`, and
`5fdfb399625d1ff18db9f16e7c15efe3`.
The archive is user-owned, untracked reference material; the new implementation
does not source or modify it. The Petawawa script records this actual source:

```text
https://opendata.nfis.org/mapserver/PRF.html
https://opendata.nfis.org/downloads/petawawa/Raster/LiDAR%202012/PRF_LiDAR2012_LAS.zip
```

The latter is a **historically recorded URL**, not a verified current download
or a license grant. The old script estimates the archive at 18 GB. Download and
extraction are manual, user-authorized operations; no preparation function
downloads anything. Confirm the dataset's license/attribution and projected CRS
from its documentation before processing. The public landing page was readable
on 2026-10-21 but did not establish those facts. The repository's MIT license does
not license the point clouds.

`ALS_tiles.gpkg` is retained unchanged as the approved **100 tile IDs**, not as
a download manifest or trustworthy CRS/footprint reference: it contains processed
LAS headers and old Windows paths, lacks `Download_LAZ`, and has an undefined
engineering CRS. IDs are recovered from those filenames with either path
separator. The approved small neighboring subset is `298000_5093000` and
`298000_5094000`; no point clouds are committed. Supply exactly one repaired
`<tile_id>.las` or `<tile_id>.laz` per selected tile, directly in `source_root`.
The historical selection already uses 1 km tiles; there is no new retile, buffer,
clipping or XY perturbation. Edge tiles can have partial coverage. Coordinates
must lie within the named tile's closed 1 km bounds; source-edge ownership is
preserved rather than inventing a different half-open clipping rule.

The user approved a **seeded adaptation**, not bit-for-bit reproduction of the
historical random draws (no seeds were recorded):

- Targets 1, 2, 5 and 10 points/m² use `lidR::random()` without pulse grouping.
  Each retains `round(target * lidR::area(source))` points, sampled without
  replacement. A source below the target fails instead of silently returning an
  under-density tile. This is point thinning, not complete-pulse thinning.
- Targets 20 and 50 preserve the recovered algorithm: round the source density,
  calculate `multiplier = target / rounded_density - 1`, append
  `floor(multiplier)` full copies and a random partial copy of
  `round(fraction(multiplier) * source_count)` points. Each copied point gets
  independently sampled **Z-only** noise from
  `c(-seq(0.10, 0.50, 0.05), seq(0.10, 0.50, 0.05))` metres. All copies receive
  the same GPS-time increment `0.013 / multiplier`, as the recovered executable
  code actually does; an unused iteration-dependent variable in the old script
  does not change that rule. XY, classification, intensity, return attributes and
  other point fields are inherited. Invalid/sufficiently dense sources fail.
- Base seed is `20261021`. Each tile/target uses base plus 100 times the
  zero-based density position in `c(1,2,5,10,20,50)` plus its one-based position
  in the sorted full 100-tile selection. Results do not depend on subset order or
  resume order. RNG kinds are Mersenne-Twister/Inversion/Rejection; the caller's
  RNG state is restored. Preparation is serial, one tile/target at a time.
- Require **already repaired** inputs with documented repair provenance; do not
  silently run the recovered repair script. That script filters zero returns,
  rounds XYZ to 0.01 m, deduplicates by XYZ/GPS/intensity/return fields, excludes
  pulses over eight points, chooses maximum Z for repeated return numbers, and
  repairs return counts/order. Its historical use on the supplied inputs is not
  yet confirmed. The new preflight requires valid legacy return numbers 1–7.
- Preserve the supplied LAS 1.1/1.2 point format 1 or 3, CRS, coordinate scales,
  offsets and point attributes; quantize modified Z on the existing scale.
  Unknown CRS may be assigned **only from explicitly confirmed configuration**;
  conflicting CRS fails. There is no COPC/LAS 1.4-to-1.2 conversion and no blanket
  VLR removal: those belonged to the unrelated SPL example, not recovered
  Petawawa preparation. Other formats need a separately approved conversion.
  `lassort64 -point_source -gps_time -olaz` and a LASindex executable finish each
  tile. The example explicitly selects `lasindex64`, present in this Linux
  installation; the old Windows script used `lasindex`. Configure exact names
  with `executables = c(sort = "lassort64", index = "lasindex64")`;
  there is no automatic binary-name substitution.
  Confirm those executable identities and license/entitlement separately from
  the benchmark tools; no restricted/demo processing is authorized by this code.

Density uses `lidR::area()` on the source footprint, not an assumed 1,000,000 m²
for partly covered tiles. Reported output-footprint density can change after
thinning. Densification uses *rounded* source density, so its achieved density
is approximate, not necessarily exactly 20/50. Metadata records source area,
source density, actual count, output bounds/area/density, and count per source
area. Tests check the exact count formulas, not a misleading exact-density claim.
Full-record comparison after sorting checks every point attribute and coordinate,
count, CRS, LAS format/version, scales and offsets against the seeded variant.
An external `.lax` must exist and be nonempty; this does **not** prove native index
query correctness. Real source coverage and index behavior still need validation.
Whole-tile transforms and canonical comparisons can require substantial RAM,
especially at 50 points/m²; this is not a streaming implementation.

**Commands for the user**, once source metadata and repaired data are available:

```sh
# Copy/edit outside the checkout, supplying the path, CRS, data/tool license,
# and repair provenance. Create a NEW empty output directory yourself.
cp data-preparation.example.R /your/path/petawawa.local.R
# Read/hash sources and query native tool versions; no preprocessing:
Rscript --vanilla -e 'source("renv/activate.R"); source("R/2_data.R"); cfg <- dget("/your/path/petawawa.local.R"); str(petawawa_preflight(cfg))'
# USER-RUN preprocessing of ONLY the approved neighboring pair:
Rscript --vanilla -e 'source("renv/activate.R"); source("R/2_data.R"); prepare_petawawa(dget("/your/path/petawawa.local.R"))'
```

Outputs are `<output_root>/1_org/tile_1000_density_<target>/<tile_id>.laz`,
with `.lax` and `.laz.rds` validation/checksum records. Root `preparation.rds`
records source/output identity, method parameters, source and native executable
MD5 checksums, version stdout/stderr, configuration/provenance, code/index/lockfile
checksums, R/package/native-library versions, and seed policy.
MD5 detects accidental changes; it does not authenticate downloaded sources.
Keep metadata with the data and retain original archive checksums in
`repair_provenance` or a referenced source manifest.

Re-running the identical command resumes only intact, checksummed tile/target
pairs. A changed input, seed, environment, configuration, tool or implementation
requires a **new empty output root**, not reuse of old results. Missing/corrupt
outputs, orphan outputs without metadata, and partial publication fail explicitly;
inspect/quarantine the named files yourself before retrying. Processing happens
in private temporary children; cleanup never resets an input/output root.
No normalization is performed: benchmark normalization is a workload, not input
preparation. This manifest is not the Chat 6 benchmark campaign/attempt schema.

**Full-data preparation is separately authorized**, not the default: only after
explicit approval, set `cfg$tiles <- petawawa_tiles()` and call
`prepare_petawawa(cfg, full = TRUE)` with a new output root. No real preparation
or download has been run during Chat 4. Regeneration still needs the original
repaired sources, verified CRS/license/repair history and native-tool validation.

### Recovered historical inputs (2026-10-21)

The user recovered the previously benchmarked, already thinned/densified clouds
from backup and approved **reuse unchanged as historical inputs**. The local
root is `/data/nvme1/LidarSpeedTestData`, containing all six
`tile_1000_density_<target>` directories directly (no `benchmark_data/1_org`
prefix). Do not move/rewrite the backup to satisfy the runner's current relative
paths; that layout needs explicit configuration/integration before a campaign.
No download or new thinning/densification is needed to reuse these files.
Original unmodified sources have not been recovered, so regeneration remains a
different, blocked operation. Do not pass these density variants to
`prepare_petawawa()` as though they were the original repaired source.

Read-only inspection found 100 LAZ files and 100 LAX indexes per density:
600 clouds and 600 indexes, about 43.9 GiB total. Every set has exactly the
approved tile IDs; all header XY bounds fit the named 1 km tiles. Density-1
point counts and XY bounds match the tracked GeoPackage for all 100 tiles.
This corroborates the user's historical identification but is not proof that
point payloads match old runs: old file checksums are unavailable. All files use
point format 1; densities 1/2/5/10 use LAS 1.1 and densities 20/50 use LAS 1.2.
**All 600 headers lack a CRS. The user confirmed the CRS is unknown.** Do not
infer an EPSG code from coordinates or the undefined GeoPackage CRS.

`input-manifests/petawawa-recovered-pilot.csv` freezes the approved neighboring
pair at all six densities: relative paths, point-cloud and index MD5 checksums,
file bytes, header counts/formats and explicitly unknown CRS. Its 12 clouds plus
12 indexes total about 0.95 GiB and remain external to git. Only these 24 files
were checksummed; the rest of the backup was inventoried by filename/header,
not fully checksummed or decoded. Checksums identify the recovered bytes now,
not their original seeds, repair history, or equivalence to the new seeded
workflow. No preparation metadata is retroactively fabricated.

The user reports these inputs are based on the open Petawawa data online.
Exact source/license/attribution terms, CRS and repair provenance still need
verification for the recovered files; the source-catalogue findings below
resolve the published source's horizontal CRS and licence, not the backup's
missing metadata or chain of custody. Header/checksum inspection is not full point-attribute,
decompression, numerical correctness or native index-query validation; this
does not authorize or establish readiness for a benchmark campaign.

To verify the frozen pilot read-only, without preprocessing:

```sh
LIDAR_RECOVERED_INPUT_ROOT=/data/nvme1/LidarSpeedTestData \
  Rscript --vanilla -e 'source("renv/activate.R"); testthat::test_dir("tests/testthat", filter = "chat4-recovered-data", stop_on_failure = TRUE)'
```

### Chat 11 pre-pilot metadata investigation (2026-10-22)

**Historical pause, superseded by the authorized execution below.** At this
inspection the pilot was paused at the user's request. Inputs remained unchanged at
`/data/nvme1/LidarSpeedTestData`. The approved output parent is
`/data/nvme2/LidarSpeedTestWorkingDir`; read-only inspection/check logs and public
metadata snapshots are under its `chat11-diagnostics/` child. This is not a
campaign or a pilot report.

The authoritative NFIS catalogue record
[Petawawa Research Forest - LiDAR 2012, LAS Files](https://saforah2.nfis.org/geonetwork-main/srv/eng/csw?request=GetRecordById&service=CSW&version=2.0.2&elementSetName=full&outputSchema=http%3A%2F%2Fwww.isotc211.org%2F2005%2Fgmd&id=f8e16c36-f267-42af-9d20-8a90677fa0a1)
links the **same original LAS archive** recorded in `_OLD/R/2_data.R`.
Its `referenceSystemInfo` explicitly identifies **EPSG:2959,
NAD83(CSRS) / UTM zone 18N**, whose horizontal units are metres, and its
`MD_LegalConstraints` specifies the
[Open Government Licence - Canada](https://open.canada.ca/en/open-government-licence-canada).
Attribute the source to Natural Resources Canada / Canadian Forest Service,
Canadian Wood Fibre Centre (distributed by Pacific Forestry Centre), identify
the dataset, and retain the licence link. In the absence of a provider-specific
attribution statement, the licence requires:
"Contains information licensed under the Open Government Licence – Canada."
Do not imply government endorsement or apply the repository's MIT licence to
the source data.

**Vertical datum is a supported lead, not confirmed backup metadata.**
The recovered repair script names a source folder
`2012als_2959_5713_unixvz_prf`: EPSG:5713 is **CGVD28 height**, in metres.
A separate NFIS record,
[Harmonized 2012 airborne laser scanning](https://saforah2.nfis.org/geonetwork-main/srv/eng/csw?request=GetRecordById&service=CSW&version=2.0.2&elementSetName=full&outputSchema=http%3A%2F%2Fwww.isotc211.org%2F2005%2Fgmd&id=8db8d84d-1f91-491b-8c8a-78b3364c2f8d),
explicitly states NAD83(CSRS)/UTM 18N and CGVD28. That is a **different,
harmonized product**, linked to normalized clouds; its vertical statement is
corroboration, not proof about the unnormalized historical backup. Confirm the
backup's source/repair lineage, XYZ units and vertical datum with its custodian
or original delivery metadata before recording them as established.

The two density-1 pilot headers still have **zero VLRs and no CRS**.
Their XYZ scale factors are 0.0001 coordinate units, which describe quantization,
not metres or a datum. They report `lassort64 (240402) non-profit`; this does
not identify all historical preparation steps. No CRS was assigned to a cloud,
catalogue or GeoPackage, and the frozen recovered-input manifest remains
`unknown`. Source metadata cannot alone restore missing file metadata or prove
which repair/thinning/densification was actually applied.

Other pilot blockers at that point were the rejected surface extents/support masks,
user-disabled lasR normalization/complex calls, LAStools entitlement/distribution
provenance and numerical conventions, and explicit integration of the
two-file subset with the runner's different directory layout. A new pilot must
not accidentally select all 100 tiles or use assigned CRS to waive validation.
See the Chat 11 handoff in the development plan for evidence and readiness.

### Chat 11 approved policies (2026-10-22 follow-up)

The custodian has now **confirmed** the backup's lineage, EPSG:2959 horizontal
CRS, EPSG:5713 vertical datum and metre XYZ units inherited unchanged from the
original Petawawa source. This supersedes the investigation's pending-confirmation
status, not its observation that the physical headers have no CRS records.

Three explicit machine-configuration fields implement the approved decisions:

```r
input_metadata = list(
  horizontal_epsg = 2959L, vertical_epsg = 5713L, xyz_units = "metre",
  confirmation = "Custodian confirmed source lineage and unchanged XYZ units/datum on 2026-10-22."
),
accept_known_surface_discrepancies = TRUE,
diagnostic_lasR = TRUE
```

Add these fields inside the configuration's `list(...)`, as shown in
`benchmark.example.R`. Without opt-in, metadata stays header-derived,
surface validation stays strict and affected lasR calls stay disabled.
All declarations/policies are frozen in campaign identity; changing them
requires a new campaign rather than resuming incompatible work.

Metadata is applied to in-memory lidR catalogs and through supported
`lasR::set_crs()` stages before processing, without rewriting/reprojecting
the backups. The horizontal declaration reaches persisted artifacts; the source
vertical datum and XYZ units are retained in configuration/validation provenance.
Conflicting file CRS is an error. For declared historical inputs, lasR requires
existing nonempty LAX indexes: otherwise it would automatically create indexes
in the protected input directory. The recovered backup already has indexes;
no new indexes are generated there by this path.

The surface policy accepts the documented R terrain/DSM support differences
and specified extent differences up to one cell per edge, retaining exception
reasons and measurements. Wrong CRS/alignment/resolution/bands, larger extent
differences, corrupt/missing artifacts, absent finite overlap, and numerical
errors on shared occupied cells still fail. No shared occupied cells means
numerical comparison was impossible, explicitly recorded, not a numerical pass.
See [the exact exception policy](WORKLOADS.md#user-accepted-surface-discrepancies-chat-11).
`accepted_exception` attempts enter descriptive summaries and resume, but remain
separate from `passed`; `n_accepted_exception` counts them, `validated_success`
is false and `n_comparable` excludes them.

`diagnostic_lasR` enables standalone normalization and **both complex pipelines**
without fixing/filtering their NA behavior. Execution errors/warnings and
validation failures remain explicit; an otherwise clean diagnostic inspection
is unverified, not passed. Those attempts do not enter eligible timing summaries.
The R tools run serially under the existing scheduler, not simultaneously.

The enabled local configuration is saved outside git at
`/data/nvme2/LidarSpeedTestWorkingDir/benchmark-pilot.R`, with report/scratch
children under that output parent, Linux monitoring and all three tools:

```sh
export LIDAR_BENCHMARK_CONFIG=/data/nvme2/LidarSpeedTestWorkingDir/benchmark-pilot.R
```

This selects configuration, **not permission to run the default matrix**.
Use the dedicated two-file pilot entry point below, not the default selection in
`R/3_runBenchmarks.R`; do not select all 100 tiles or bypass preflight.
Policy approval alone did not launch the pilot. Subsequent explicit execution
authorization and LAStools licence confirmation enabled the run below; native
numerical acceptance remains separate from entitlement.

### Resuming the authorized two-tile pilot

The user authorized execution and confirmed an installed LAStools licence.
The saved configuration now enables all three tools. Licence-query diagnostics
are private local files; licence keys/holder details must not enter git or the
report. Version/checksum identity and the user's entitlement confirmation are
retained separately from numerical acceptance.

Use the dedicated pilot entry point, **not the default matrix in
`R/3_runBenchmarks.R`**:

```sh
export LIDAR_BENCHMARK_CONFIG=/data/nvme2/LidarSpeedTestWorkingDir/benchmark-pilot.R
Rscript --vanilla R/6_pilot.R --preflight
LIDAR_CAMPAIGN_ID=petawawa-pilot-20261022-v2 Rscript --vanilla R/6_pilot.R --run
LIDAR_REPORTS_PATH=/data/nvme2/LidarSpeedTestWorkingDir/reports \
  LIDAR_CAMPAIGN_ID=petawawa-pilot-20261022-v2 Rscript --vanilla R/5_report.R
```

Preflight copies only the two frozen density-1 clouds and their existing indexes
unchanged into `pilot-inputs/tile_1000_density_1` under the approved output parent.
Source/destination checksums and provenance are retained; existing changed or
extra files are refused, never overwritten. No metadata rewrite, regeneration,
download or selection of the remaining 98 tiles occurs. Original files stay
under `historical_input_root`; `input_roots` selects the staged subset only.
The pilot selects all nine task IDs at 1/2 cores, retaining unsupported slots
and diagnostic complex calls, with configured repetitions, warm-ups and monitoring.
The manifest binds original/staged byte identities and execution policies.

For declared inputs, native calls explicitly assign horizontal `-epsg 2959`
and use `-fail` (no coordinate reprojection, demo fallback or force flag).
Native structural success still remains unverified until numerical conventions
are established. Matching-ID reruns use existing immutable resume rules; failures
retry and warm-ups rerun. A changed execution identity requires a new campaign.

### Real pilot outcome (2026-10-22)

Campaign `petawawa-pilot-20261022-v2` completed all **216 frozen slots**:
nine tasks, two core counts, three tools, one warm-up and three measured
repetitions. One explicit failed-measurement retry brings the immutable history
to **217 attempts**; resuming an unsupported slot added no attempt. Execution
outcomes are 161 succeeded, eight failed native canopy calls and 48 unsupported
skips. All 169 executed attempts have full whole-machine monitoring coverage.

**No real attempt passed validation or qualified as an accepted exception.**
Source return-field compliance warnings, numerical/geometry differences and
deferred lasR normalization/complex defects remain explicit. One-core native
canopy calls exit nonzero after height-cutoff warnings; two-core native calls
can return zero despite warnings. None of this is waived by the installed
licence or by the bounded surface-exception policy. All timing/comparison
sections therefore correctly report **no eligible measurements**.

The eight-page report is outside the published site:
`/data/nvme2/LidarSpeedTestWorkingDir/reports/campaigns/petawawa-pilot-20261022-v2/report-9e45241319e19/_site/index.html`.
The campaign also retains `pilot-review.rds`, `pilot-attempt-review.csv` and
`pilot-raw-diagnostic-timings.csv`. The last file describes unaccepted raw
executions only, not benchmark comparisons. Original and staged byte identities
are in the frozen manifest and `pilot-inputs/provenance.rds`.

Two exploratory campaigns were interrupted to fix quadratic reference-raster
lookups and accidental 0.001-m requantization of buffered validation points.
Their incomplete attempts and scratch outputs remain separately archived, not
pooled into this report. The final campaign also stopped twice on a transient
native version-output mismatch; current identities matched before continuation
of unattempted slots. The guard was not bypassed and frozen code was not changed.
See the plan's handoff for exact history, checks and outstanding limitations.
**Execution/review is complete; acceptance is blocked and a full campaign is
not ready.** Historical inputs and published `docs/` were not modified.

For opt-in read-only review of the saved real campaign alongside the full suite:

```sh
LIDAR_RECOVERED_INPUT_ROOT=/data/nvme1/LidarSpeedTestData \
LIDAR_CHAT11_PILOT_ROOT=/data/nvme2/LidarSpeedTestWorkingDir/reports/campaigns/petawawa-pilot-20261022-v2 \
TMPDIR=/data/nvme2/LidarSpeedTestWorkingDir/chat11-diagnostics \
Rscript --vanilla tests/testthat.R
```

## Full campaign (Chat 12): blocked, not executed

The Chat 11 accepted-output prerequisite is **not met**. The saved pilot has
169 failed validations and 48 unsupported/not-run validations, with zero eligible
or comparable measurements. Its report is diagnostic, not publishable benchmark
evidence. Requesting Chat 12 preparation does not waive those failures.

Before freezing or executing a full campaign:

- Resolve the input return-field compliance, shared-cell numerical/geometry,
  native warning/exit and deferred lasR normalization/complex blockers; rerun and
  review an explicitly authorized small pilot with accepted outputs.
- Obtain explicit approval of the input inventory/densities, task/tool/core
  matrix, repetitions/warm-ups, compute duration, scratch/report/archive storage
  budget and output-retention policy. The two-tile pilot approval is not approval
  for the remaining tiles or a full campaign.
- Review `benchmark.local.R` (or `LIDAR_BENCHMARK_CONFIG`), dataset paths in
  `R/0_setup.R` and selection `B` in `R/3_runBenchmarks.R` against that approval.
  **The current runner selects only `pixel_metrics_2`, not a full campaign.**
  Its default dataset paths also differ from the staged pilot layout. Do not
  run it as-is or assume uncommenting the entire matrix is authorized.
- Freeze the final execution sources, lockfile/runtime/native tools, approved
  inputs, hardware/storage and measurement policy before the first attempt.
  For uncommitted work, retain an exact source snapshot and base revision/patch
  privately alongside the manifest; a revision alone cannot restore dirty code.
  Keep dependencies, sources and inputs unchanged throughout execution.

The following are **manual, potentially long-running commands**, not commands
executed during this Chat 12 preparation. Use them only after the gates above
and configuration/selection review, from the repository root. Replace the
example configuration path and campaign ID with the approved values:

```sh
# Full regression suite (the previous run took about 25 minutes).
Rscript --vanilla tests/testthat.R
Rscript --vanilla R/check_environment.R

# Execution fingerprints inputs/environment and then runs the reviewed selection.
LIDAR_BENCHMARK_CONFIG=/path/to/approved-full-config.R \
LIDAR_CAMPAIGN_ID=approved-full-campaign \
  Rscript --vanilla -e 'source("renv/activate.R"); source("R/3_runBenchmarks.R")'

# Only after execution has stopped; use reports_path from the approved config.
LIDAR_REPORTS_PATH=/path/to/reports \
LIDAR_CAMPAIGN_ID=approved-full-campaign \
  Rscript --vanilla R/5_report.R
```

Reinvoking execution is not a no-op: warm-ups rerun and failed measurements
retry, retaining immutable attempts. Identity changes require a new ID.
Do not bypass identity guards or delete interrupted attempts to compile.

After review, archive the entire campaign directory (manifest, all attempts,
logs, validation/monitoring files, report builds and their receipts), the exact
source snapshot and restoration instructions. Retain checksums of the archive
and verify a separate restored copy before publication. This is a private
archive until checked for machine-local information and licence diagnostics;
never include licence secrets. Current cleanup removes primary products even
after failed validation, so this is **not** a complete output-product archive.
Any requirement to preserve those products needs an explicit retention decision
before execution, not a claim that checksums reconstruct deleted outputs.

Review coverage of the approved frozen schedule, all failures/retries/exclusions,
accepted exceptions versus comparable results, sample counts and uncertainty.
Publish only with separate approval; keep historical `docs/` unchanged until
then. No new accepted results or completed full-campaign archive exist yet.

## Tests

Chat 11 (`filter = "chat11"`) uses tiny indexed synthetic clouds to check declared
CRS propagation, unchanged input bytes/inventory, strict defaults and bounded
surface acceptance, diagnostic normalization/complex dispatch, and frozen policy
identity. Chat 10 also checks that accepted exceptions resume and contribute only
descriptive measurements, never validated comparisons.

Chat 10 (`filter = "chat10"`) uses temporary **synthetic reporting** campaign
receipts, not numerical acceptance or real campaign evidence. It exercises
latest-retry selection, failures/skips/short/unvalidated exclusions, warm-ups,
sample counts/SD/range, unavailable/partial monitoring, missing dimensions,
complex-call versus independent timings, immutable snapshot guards, compiler
decoupling, replacement intermediates, PNG/CSV generation and an actual **eight-
page Quarto render**. It asserts that raw attempt bytes and all published `docs/`
bytes are unchanged. It requires the pinned plotting/rendering stack and Quarto
1.10.18 on PATH (or `QUARTO`); missing Quarto is a failure, not an implicit skip.
No machine configuration, point clouds, LAStools or benchmark execution is used.
Fixture directories/sites are temporary and cleaned automatically. Linux report
generation/rendering is exercised; Windows fonts/rendering are not certified.

Chat 9 tests use the locked `callr`, `processx`, and `ps` dependencies plus Linux
`/proc`; no PowerShell, native LAStools, point clouds, new dependency versions or
real campaign inputs are needed. Live-monitor cases explicitly skip on non-Linux
platforms. Controlled Rscript workloads exercise native-process success/failure,
immutable sample/report ingestion, interval clipping, coverage, monitor death,
startup failure and forced shutdown cleanup. Sampling does not assert a fixed
CPU load or RAM quantity on a shared machine.

All automated tests use `testthat`, without making this repository an R package.
Restore the lockfile first as above; `testthat`, `withr`, and subprocess/fixture
dependencies are declared in `DESCRIPTION` and pinned with the runtime packages.
The standalone test runner activates renv even when invoked with `--vanilla`.

Run the suite from the repository root; failures produce a nonzero exit status:

```sh
Rscript --vanilla tests/testthat.R
```

Tests are in `tests/testthat/test-*.R`; shared temporary fixtures are in
`tests/testthat/helper-fixtures.R`. To run just one chat's tests from the root:

```sh
Rscript --vanilla -e 'source("renv/activate.R"); testthat::test_dir("tests/testthat", filter = "chat1", stop_on_failure = TRUE)'
```

Chat 1/2 tests require no machine-local configuration, point clouds, or LAStools
installation. They use Rscript subprocesses and small catalog/monitor stand-ins;
since Chat 6, dispatch fixtures also fingerprint the full pinned package stack
and geospatial library versions for campaign identity. Chat 3 checks
Quarto, constructs real lasR pipelines without executing them, and writes tiny
synthetic LAS fixtures for the public lidR metadata interfaces. It restores into
a fresh temporary library using the lockfile and the normal renv cache; this is
library isolation, not a cache-free source rebuild or a native-tool/data test.
The isolated process also uses the standard library bundled with the pinned R,
including locked recommended packages such as `KernSmooth`.
The restoration check may require repository access if packages are not cached.
It may rebuild packages from source, so the native build prerequisites are also
required for that check; a source restore can take tens of minutes. Use the
`chat3-environment` filter for the shorter API/runtime checks without restoration.
Strict restoration disallows a substitute repository
for URL-pinned records, and `retry = FALSE` disallows retrying with newer versions.
Unix-specific shell, permission, and symlink checks are explicitly skipped on Windows; permission
checks also skip when the current user bypasses the tested restrictions.

Chat 4 checks (`filter = "chat4"`) use tiny, temporary synthetic LAS/LAZ tiles:
real lidR thinning/densification/IO with stubbed native sorting/index creation,
not the user's source data. They cover metadata/scope guards, exact target count
formulas, neighboring tile bounds/CRS, inherited attributes, Z/GPS modifications,
seed reproducibility/RNG restoration, persisted provenance, resume checksums and
failure/partial-output cleanup. They never download or prepare real benchmark
data. Native sort/index correctness, source-data quality and Windows behavior
are not established by these fixtures.

`chat4-recovered-data` always checks the tracked historical manifest schema.
Its real-file checksum/header case is opt-in via `LIDAR_RECOVERED_INPUT_ROOT`;
otherwise it skips explicitly. It reads the approved 24 external files without
changing them, downloading data, decoding full point payloads or preprocessing.

Chat 5 (`filter = "chat5"`) exercises known-value metrics, persisted R-tool
GeoTIFF bands/geometry, first-return versus all-return DSMs, GeoPackage
persistence, native-command cutoff contracts, supported/blocked dispatch and
the pinned-lasR normalization defect. It requires the pinned geospatial stack,
uses temporary synthetic data, and does not run LAStools or a campaign. Native
processing, Windows and neighboring real-tile numerical equivalence are not
established by these checks.

Chat 6 (`filter = "chat6"`) uses temporary input/index bytes and native-executable
copies, fixture-only native version responses, and the installed pinned package
stack. It checks manifest identity, actual source/lockfile/native-byte changes,
thread environment changes, resume/repetition IDs, append-only retries and
diagnostics, completion integrity, campaign-only compilation and explicit legacy
separation. Git revision queries are read-only; source-change tests hash a
temporary checkout stand-in, never edit the real installed packages or inputs.
The updated Chat 1/2/5 fixtures also cover campaign-aware dispatch. These tests
do not establish real LAStools version/provenance/license validity, Windows
behavior, numerical correctness or readiness to run a benchmark campaign.

Chat 7 (`filter = "chat7"`) uses tiny neighboring synthetic LAS tiles and actual
pinned R-tool GeoTIFF/LAZ/GPKG outputs. It covers missing/corrupt/partial products,
geometry/bands/NoData, numerical/attribute errors, sparse cells, boundary mismatch
rejection, chunk diagnostics, disabled complex requirements, report integrity,
validated resume and failed-validation retries. Native filenames/structural
inspection use renamed R fixtures, **not LAStools processing**. No real benchmark
data, external-file opt-in, runtime upgrade or full campaign is needed. The full
runner still includes the isolated-restore test and opt-in historical-data skip;
Windows and licensed native correctness remain external limitations.

Chat 8 (`filter = "chat8"`) checks repetition counts, per-position balance for
one to three tools and partial/full cycles, seeded reproducibility and RNG
preservation, serial frozen dispatch, warm-up/measurement separation, immutable
resume records, policy incompatibility, execution locking and untimed cooldown.
It uses actual temporary multisession workers, supported lidR/lasR thread APIs
and an Rscript child to check inherited limits and cleanup on failure. These
tests require the pinned stack and reference BLAS, use no real inputs or
licensed LAStools processing, and do not certify Windows, cold-cache behavior,
thermal equilibrium or observed native CPU utilization.
