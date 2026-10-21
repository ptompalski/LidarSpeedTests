All content moved to:
https://ptompalski.github.io/LidarSpeedTests/

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
and error/warning diagnostics. `processx` and `future` are required alongside the
packages already loaded by setup; environment pinning remains a separate task.

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
Install the focused suite's dependencies in your R library:

```r
install.packages(c(
  "testthat", "withr", "dplyr", "readr", "tidyr", "purrr", "forcats",
  "stringr", "glue", "magrittr", "cli", "processx", "future", "fs", "ps"
))
```

Run the suite from the repository root; failures produce a nonzero exit status:

```sh
Rscript --vanilla tests/testthat.R
```

Tests are in `tests/testthat/test-*.R`; shared temporary fixtures are in
`tests/testthat/helper-fixtures.R`. To run just one chat's tests from the root:

```sh
Rscript --vanilla -e 'testthat::test_dir("tests/testthat", filter = "chat1", stop_on_failure = TRUE)'
```

The suite requires no machine-local benchmark configuration, point clouds,
geospatial packages, or LAStools installation. It uses Rscript subprocesses and
small catalog/monitor stand-ins, not real geodata processing. Unix-specific shell,
permission, and symlink checks are explicitly skipped on Windows; permission
checks also skip when the current user bypasses the tested restrictions.
