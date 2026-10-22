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
lasR pipeline construction are covered by tests. Data preparation, complete
report generation, numerical workload equivalence, and native Windows behavior
remain separate tasks. Chat 5's workload definitions, supported DSM API migration
and remaining equivalence limitations are documented in [WORKLOADS.md](WORKLOADS.md).

Catalog metadata now uses supported `lidR::npoints()`, `sf::st_area()`, and
catalog header accessors: counts are exact and densities are unrounded, using
summed tile areas and first-return counts as in lidR's catalog display.
Printed “thousand/million” values are no longer mistaken for raw counts.
RAM is reported in bytes through `ps::ps_system_memory()`; the strict `Z > 2`
metric population is unchanged. Chat 5 changes its units to a user-confirmed
fraction and standardizes band names for new attempts only; historical records
are not rewritten.
The plotting helper retains its historical Segoe UI font; font availability and
visual parity on Linux must be resolved when rebuilding reports in Chat 10.

## Running the benchmark harness

Workload definitions and relative dataset paths are in `R/0_setup.R`. Copy
`benchmark.example.R` to the ignored `benchmark.local.R` and edit its machine-local
paths, core counts, and explicit tool selection. Alternatively, set
`LIDAR_BENCHMARK_CONFIG` to a trusted R file containing the same `list(...)`.
Run scripts from the repository root; relative configuration paths resolve there.
No machine configuration is required to read the default `results` directory.
Compilation requires explicit campaign selection or explicit legacy mode below.

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

Monitoring remains the existing Windows whole-machine PowerShell script. Keep
`resource_monitoring = FALSE` on Linux: requesting it fails explicitly. On Windows,
configure an existing monitoring output directory, script, and PowerShell
executable; monitor stdout/stderr logs remain there. Campaign attribution and
cross-platform monitoring are not implemented by this configuration change.
Startup must produce stdout within ten seconds without startup stderr errors;
that health check does not validate utilization samples or counter accuracy.

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
measurement phase plus execution/validation success. Reporting/aggregation is
still Chat 10.
Pre-Chat-8 campaign records remain readable as measurements with missing
schedule indices; they are not retroactively assigned balanced slots. Their
identity cannot resume under the changed execution policy.

Compile only one explicitly selected campaign:

```sh
LIDAR_CAMPAIGN_ID=approved-pilot Rscript --vanilla -e \
  'source("renv/activate.R"); source("R/4_compileResults.R")'
```

This writes `compiled.rds` inside that campaign, retaining **all attempts and
repetitions**, including failed retries. It never chooses package versions per
task or deduplicates retries. Raw manifests/attempts are not overwritten.
For historical top-level CSVs, explicitly unset `LIDAR_CAMPAIGN_ID` and set
`LIDAR_RESULTS_MODE=legacy`; that separate path writes the historical `r.rds`,
labels records `legacy-unidentified`, and retains every recorded version rather
than guessing the newest. Legacy mode refuses campaign-schema CSVs. Neither
mode silently falls back to the other or pools campaigns. Report rebuilding/
attempt selection for publication remains Chat 10; old report pages are not yet
campaign-aware. The maximum-output-count heuristic has been removed: expectations
now come from each attempt's input/workload manifest, never from peer results.

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

## Petawawa input preparation (Chat 4)

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
verification. Header/checksum inspection is not full point-attribute,
decompression, numerical correctness or native index-query validation; this
does not authorize or establish readiness for a benchmark campaign.

To verify the frozen pilot read-only, without preprocessing:

```sh
LIDAR_RECOVERED_INPUT_ROOT=/data/nvme1/LidarSpeedTestData \
  Rscript --vanilla -e 'source("renv/activate.R"); testthat::test_dir("tests/testthat", filter = "chat4-recovered-data", stop_on_failure = TRUE)'
```

## Tests

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
