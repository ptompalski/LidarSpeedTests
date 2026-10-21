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
`lascanopy`, `lasheight64`, `lasgrid64`, and `blast2dem` in the configured
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
remain separate tasks. In particular, lasR's deprecated `chm()` alias is retained
for Chat 5's supported DSM/workload-contract decision; this environment work does
not silently change workload semantics.

Catalog metadata now uses supported `lidR::npoints()`, `sf::st_area()`, and
catalog header accessors: counts are exact and densities are unrounded, using
summed tile areas and first-return counts as in lidR's catalog display.
Printed “thousand/million” values are no longer mistaken for raw counts.
RAM is reported in bytes through `ps::ps_system_memory()`; the strict `Z > 2`
metric population is unchanged. Historical records are not rewritten.
The plotting helper retains its historical Segoe UI font; font availability and
visual parity on Linux must be resolved when rebuilding reports in Chat 10.

## Running the benchmark harness

Workload definitions and relative dataset paths are in `R/0_setup.R`. Copy
`benchmark.example.R` to the ignored `benchmark.local.R` and edit its machine-local
paths, core counts, and explicit tool selection. Alternatively, set
`LIDAR_BENCHMARK_CONFIG` to a trusted R file containing the same `list(...)`.
Run scripts from the repository root; relative configuration paths resolve there.
No machine configuration is required merely to select the default historical
`results` directory in the compiler.

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

Chat 1/2 tests require no machine-local configuration, geospatial packages,
point clouds, or LAStools installation. They use Rscript subprocesses and small
catalog/monitor stand-ins. Chat 3 additionally loads the full pinned stack, checks
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
