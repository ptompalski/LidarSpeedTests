# Code audit and environment recommendations

Date: 2026-10-20

## Summary

Use **renv**, an explicitly pinned R version, and a separate record of native
dependencies and external tools for the next benchmark campaign. Fix the
benchmark harness before a full rerun: current failure handling can record
unsuccessful processing as successful, some workloads are not equivalent across
tools, and the tracked sources do not reproduce the complete published site.

## Scope and limitations

Reviewed the six R scripts, Quarto sources and configuration, PowerShell resource
monitoring script, ignore files, and `ALS_tiles.gpkg`.

All six R scripts parsed successfully. Small, dependency-free checks reproduced
the failure-status relabeling and incorrect version ordering described below.
The GeoPackage was inspected read-only.

No end-to-end benchmark or site render was performed. The checkout lacks the
benchmark point clouds and required intermediate reporting files. The inspected
environment has R 4.6.1, but lacks the required project R packages and Quarto.
Current package documentation was consulted, but runtime compatibility with the
newest package versions remains unverified.

## Findingswewadwadhh


### High priority

1. **Compilation can turn failures into successes.**
   `R/4_compileResults.R:86` uses
   `if_else(success == 1 & as.numeric(processing_time) < 0.1, 0, 1)`.
   Every existing `success = 0` becomes `1`. Preserve recorded failures;
   implausibly short elapsed times should be an additional diagnostic, not a
   replacement for execution and output validation.

2. **LAStools exit status is not checked.**
   `R/1_functions.R:383-404` calls `system()` but only recognizes the string
   `"ERROR"` as failure. A numeric nonzero command status can therefore be marked
   successful. Capture exit status, stdout, and stderr, and record failures
   explicitly.

3. **Metric workloads are not equivalent across tools.**
   `R/0_setup.R:89-105` and `R/1_functions.R:1-24` compare percentage of all
   points above 2 in lidR/lasR with LAStools `-cov`, which measures first returns
   above a cover cutoff. LAStools also applies a default 1.37 m height cutoff
   to several statistics, unlike the R calls. Define matching point populations,
   thresholds, units, percentile conventions, and standard-deviation definitions
   before comparing performance. All these basic metric tasks currently use
   original elevations, not normalized heights.

4. **Dataset preparation is not reproducible from the repository.**
   `R/2_data.R` is entirely commented out and describes a different source dataset
   from the Petawawa data described in `index.qmd`. It expects a `Download_LAZ`
   column, but the tracked GeoPackage has no such column. The GeoPackage contains
   100 tile records. The published thinning and artificial densification procedure
   is missing. Recover the actual preparation code, fix random seeds, and retain
   checksums for the resulting benchmark inputs.

5. **The publication pipeline is incomplete.**
   `R/4_compileResults.R` writes `r.rds`, while `results_table.qmd:19` requires
   `CPU-RAM.rds` and `results_table_complex.qmd:19` requires `complex1.rds`.
   Neither intermediate file nor its generation pipeline is tracked. Most figure
   pages reference existing PNGs without tracked figure-generation code. A site
   render alone would not regenerate those figures from new benchmark results.

### Medium priority

1. **Run identity and version selection do not reliably identify a campaign.**
   `R/1_functions.R:440-485` skips runs when result filenames already exist.
   Those filenames do not include the dataset checksum, code revision, or full
   dependency environment. `R/4_compileResults.R:108-160` selects versions using
   character sorting or dot removal rather than semantic version comparison.
   For example, dot removal ranks `0.9.10` above `0.10.0`. Introduce explicit
   campaign and repetition IDs; select a complete campaign instead of choosing
   the newest version independently for each task.

2. **Output checks do not establish correctness.**
   `R/1_functions.R:80-113` counts files and bytes, while
   `R/4_compileResults.R:163-169` infers expected file counts from the largest
   observed result. Catalog processing is configured to continue after chunk
   errors with `opt_stop_early(ctg) <- FALSE`. Define expected artifacts per
   workload and validate raster geometry, bands, valid values, point counts,
   spatial coverage, and numerical tolerances.

3. **Timing methodology permits cache and ordering bias.**
   Tools always execute in the order lidR, lasR, LAStools. There is no explicit
   repetition dimension, and `Sys.sleep(5)` statements occur after `return()`,
   so they never execute. Balance or randomize tool order, repeat measurements,
   document warm-cache versus cold-cache policy, and report variability.
   Make both worker counts and native thread counts explicit.

4. **Paths, process invocation, and scratch cleanup need hardening.**
   `R/0_setup.R` assumes Windows drive paths and a local LAStools installation.
   Command paths and input filenames are assembled without quoting.
   `R/1_functions.R:27-29` recursively deletes the configured output directory,
   and all runs use a shared scratch location. Use configurable paths, structured
   process arguments, and unique, validated scratch directories per run.

## Upgrade and maintenance concerns

- `.renvignore` excludes `R/*`, hiding the main benchmark scripts from default
  renv dependency discovery. Remove this exclusion or declare dependencies
  explicitly.
- Current lasR documentation deprecates `chm()` in favor of `dsm()`. On
  unnormalized data, this operation generates a DSM rather than a CHM.
- Basic surface tasks operate on original elevations, whereas the complex
  pipeline normalizes before creating its canopy raster. Summing independent
  basic-task timings is not a fully equivalent sequential end-to-end workflow.
- Private functions such as `lidR:::fast_countover()` and
  `benchmarkme:::` helpers are fragile upgrade dependencies.
- `benchmark_ALS_info()` parses fixed lines of printed catalog summaries.
  Prefer supported structured metadata interfaces.
- CPU and RAM monitoring is Windows-specific and measures whole-machine usage,
  so unrelated activity can influence the recorded utilization.

## Environment recommendation

### Prefer renv for this repository

renv provides established R project isolation, package provenance recording,
lockfiles, and restoration. Use it with a pinned R installation, managed
separately through a tool such as rig or a versioned machine/container setup.

uvr is a viable CLI-first alternative. Its current documentation describes a
manifest, lockfile, isolated project library, custom repositories, frozen
synchronization, and R installation/version management. The recommendation for
renv is about minimizing tooling change during benchmark repair, not a claim
that uvr lacks these features. Use one project environment manager rather than
maintaining competing lockfiles.

Neither tool alone freezes LAStools, Quarto, operating-system libraries,
compiler options, hardware, or input data.

### Platform and external dependencies

Native Windows requires the fewest changes to the existing harness and
PowerShell monitoring. Linux is also suitable after adapting paths, executable
detection, and monitoring. Do not interpret a different OS or machine as a
package-version-only comparison with historical results.

Record the exact versions or identities of:

- R, Quarto, and the environment manager.
- LAStools distribution, executable checksums, and license status; avoid
  restricted or demo processing.
- GDAL, GEOS, PROJ, BLAS, compiler, and relevant build flags.
- Hardware, storage paths, power configuration, and parallelism settings.

Linux source installations need a suitable compiler toolchain and native
geospatial development libraries, plus the system dependencies required by
packages such as sf, terra, and the plotting stack.

### Suggested initial package setup

Run in R from the repository root:

```r
install.packages("renv", repos = "https://cloud.r-project.org")
renv::init(bare = TRUE)
```

After any prompted restart:

```r
repos <- c(
  CRAN = "https://cloud.r-project.org",
  r_lidar = "https://r-lidar.r-universe.dev"
)
options(repos = repos)

# Avoid relying on the current dependency-discovery exclusions.
renv::settings$snapshot.type("all")

install.packages(
  c(
    "lidR", "fs", "sf", "tidyverse", "future", "callr",
    "glue", "benchmarkme", "magrittr", "cli",
    "gt", "hms", "lubridate", "htmltools", "RColorBrewer",
    "knitr", "rmarkdown", "curl"
  ),
  repos = repos["CRAN"]
)

install.packages(
  "lasR",
  repos = repos[c("r_lidar", "CRAN")]
)

renv::snapshot()
```

This is a proposed setup, not an installation performed during the audit.
Commit `renv.lock`, `.Rprofile`, `renv/activate.R`, and `renv/settings.json`.
Restore on another machine using the matching R version and `renv::restore()`.

At audit time, CRAN reported lidR **4.3.3** and r-universe reported lasR
**0.21.2**. These are observations, not permanent version requirements.
The lasR documentation directs installation through r-universe rather than
CRAN.

## Recommended rerun sequence

1. Repair failure handling, unsupported task/tool handling, run identity, and
   output validation.
2. Recover the exact input-preparation workflow and missing reporting scripts.
3. Define equivalent workloads, including normalization, point filtering,
   buffer sizes, raster alignment, output formats, and parallelism.
4. Create a fresh environment using the newest chosen package versions, then
   freeze it for the entire campaign.
5. Run correctness checks on a small set of neighboring tiles to exercise
   buffering before scaling up.
6. Run repeated, balanced measurements and preserve every attempt, including
   failures, diagnostics, and output-validation results.
7. Regenerate tables and figures from that campaign and render the site.

Each campaign should retain its repository commit, lockfile, input checksums,
external-tool identities, configuration, repetition IDs, raw timings, and logs.
Do not update dependencies midway through a campaign.

## Sources

- [renv introduction and limitations](https://rstudio.github.io/renv/articles/renv.html)
- [renv initialization](https://rstudio.github.io/renv/reference/init.html)
- [renv snapshot modes](https://rstudio.github.io/renv/reference/snapshot.html)
- [uvr documentation](https://github.com/nbafrank/uvr)
- [lidR CRAN metadata](https://cran.r-project.org/web/packages/lidR/DESCRIPTION)
- [lasR installation documentation](https://r-lidar.github.io/lasR/)
- [lasR r-universe metadata](https://r-lidar.r-universe.dev/api/packages/lasR)
- [lasR DSM and deprecated CHM alias](https://r-lidar.github.io/lasR/reference/dsm.html)
- [lasR metric definitions](https://r-lidar.github.io/lasR/reference/metric_engine.html)
- [LAStools lascanopy metric definitions and cutoffs](https://downloads.rapidlasso.de/html/lascanopy_README.html)
