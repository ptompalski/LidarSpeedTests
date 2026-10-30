# Tool adapter interface (`lidarbench.adapter/1`)

Status: **implemented** (Phase 3, adapter interface and registry).
Code: [`src/lidarbench/execution/adapters/`](../src/lidarbench/execution/adapters/),
[`src/lidarbench/execution/plan.py`](../src/lidarbench/execution/plan.py).
Companion: [ARCHITECTURE.md §7.1](ARCHITECTURE.md#71-tool-adapter).

A tool adapter lets one piece of processing software (an R package through
`Rscript`, a Python library, a native executable, or a tool driven by a
configuration file such as PDAL) take part in benchmarks. The orchestrator stays
tool-neutral. Adding an adapter never requires editing the core.

## 1. Responsibilities

| # | Adapter answers | Method / type |
|---|---|---|
| 1 | Which tool and version is this? | `identify() -> ToolIdentity` |
| 2 | Is it installed and usable on this platform? | `probe(PlatformInfo) -> Availability` |
| 3 | Which workload IDs does it implement, and how? | `workload_support() -> list[WorkloadSupport]` |
| 4 | Which executable and argv run a workload? | `build_command(Invocation) -> CommandSpec` |
| 5 | Which environment variables and threading model? | `CommandSpec.env`, `WorkloadSupport.threading` |
| 6 | Which outputs are expected; which tool facts to record? | `expected_outputs(Invocation)`, `invocation_metadata(Invocation)`, `ToolIdentity.details` |
| 7 | Why is something unavailable or unsupported? | `Availability.reason/detail`, `WorkloadSupport.reason`, `UnsupportedCombinationError` |
| 8 | Did the finished run really succeed, and what did the tool report? (optional) | `interpret(Invocation, ProcessResult) -> Interpretation`, `AcceptedException` |

Adapters **must not**:

- launch the benchmarked process, time it, or monitor resources;
- create, clean or delete directories (the orchestrator owns `output_dir` and
  `work_dir`);
- manage campaigns, schedules or attempt records;
- compare results between tools or decide comparability.

They only *describe*. The orchestrator does everything else uniformly for all
tools, which keeps timing boundaries identical (ARCHITECTURE.md §8.4).

## 2. Lifecycle

```text
registry.discover()                          entry points "lidarbench.adapters"
  └─ adapter = registry.create(id)           no-argument constructor
       ├─ probe(platform)      ── unavailable ─▶ skipped_unavailable (+ reason)
       ├─ identify()                         recorded in campaign identity
       ├─ support_for(workload, threads)
       │     ├─ blocked        ─────────────▶ skipped_blocked (+ reason)
       │     └─ unsupported    ─────────────▶ skipped_unsupported (+ reason)
       ├─ build_command(invocation)
       │     └─ UnsupportedCombinationError ▶ skipped_unsupported (+ reason)
       ├─ expected_outputs(invocation)
       └─ invocation_metadata(invocation)
plan_invocation() validates all of the above ─▶ InvocationPlan(status="ready")
runner: write config files, launch argv (shell=False), time, monitor
       └─ interpret(invocation, process)     untimed, after a self-exited process
runner: output check, validate
```

`plan_invocation()` is the single entry point the runner uses. Missing software,
unsupported and blocked combinations become **skip plans with reasons**, matching
the `skipped_*` execution statuses of ARCHITECTURE.md §8.1. They are never
benchmark failures. Defective adapter output (relative executable, non-string
argv, output path outside `output_dir`, unexpected exception) raises
`AdapterContractError`. A `probe()` that raises or returns the wrong type yields
`skipped_unavailable` with reason `probe_failed`. `lidarbench check` contains
defects: a failing constructor, `probe` or `identify` (including a wrong return
type or non-scalar identity details) is reported as `probe_failed`, and a failing
or duplicated `workload_support` declaration is listed under the adapter's errors.

## 3. Types

All types are frozen dataclasses in `execution/adapters/base.py`.

### Availability

`available`, `reason` (`UnavailableReason`), `detail` (human text), and
`executables` (resolved absolute paths keyed by role). An unavailable result
requires a reason and a detail.

`UnavailableReason`: `not_installed`, `platform_unsupported`,
`version_unsupported`, `misconfigured`, `probe_failed`.

`probe()` must return `Availability.unavailable(...)` for missing software. It must
not raise.

### ToolIdentity

`tool`, `version` (or `None` if undetectable), `executables`, and `details`
(JSON scalars such as runtime version, package versions or build flags). Use
`launchers.probe_version_output()` to run a version command safely.

### WorkloadSupport and ThreadingSupport

`WorkloadSupport(workload_id, status, reason, threading)`. `status` is the
`implementation_status` of ARCHITECTURE.md §8.2:

| Status | Dispatched? | Reason required |
|---|---|---|
| `runnable` | yes | no |
| `diagnostic_only` | yes; can never become `comparable` | yes |
| `unsupported` | no; recorded skip | yes |
| `blocked` | no; recorded skip | yes |

Declare unsupported and blocked workloads explicitly so reports can show why.
Undeclared workload IDs resolve to `unsupported` with the reason "adapter
declares no implementation". Comparability (`matched_definition`) is declared in
workload data (Phase 4), not by adapters.

`ThreadingSupport(model, min_threads, max_threads)`. `max_threads=None` means no
adapter-imposed limit. A thread count outside the range resolves to
`unsupported` with an explicit reason.

| `ThreadingModel` | Meaning | Example |
|---|---|---|
| `single_threaded` | always one thread (`max_threads` must be 1) | synthetic worker |
| `native_threads` | the tool's own flag or setting | LAStools `-cores N` |
| `worker_processes` | the tool starts worker processes | lidR `future` multisession or mirai |
| `concurrent_files` | files processed concurrently | lasR `concurrent_files` |

### Invocation (input, built by the orchestrator)

`workload_id`, `inputs` (absolute paths), `output_dir`, `work_dir` (both absolute,
existing and attempt-owned), `threads`, and `parameters` (closed mapping of
`str | int | float | bool`, for example `height_cutoff`). `config_path(name)`
gives the absolute path where a config file will be written.

### CommandSpec (output)

| Field | Rule |
|---|---|
| `argv: tuple[str, ...]` | Non-empty. `argv[0]` is an **absolute** executable path. Every element is one argument and is never shell-parsed. No NUL characters. |
| `cwd: Path` | Absolute; normally `invocation.work_dir`. |
| `env: Mapping[str, str]` | **Delta** applied over the orchestrator's environment and recorded. The orchestrator's own environment is never changed. |
| `config_files: tuple[ConfigFile, ...]` | Text files (`name`, `content`) written by the orchestrator into `work_dir` before launch, with exclusive create and UTF-8. Names must be portable file components. |
| `accepted_exceptions: tuple[str, ...]` | Names of the `AcceptedException`s that `interpret()` may report for this command (`^[a-z][a-z0-9_]*$`). Declared before launch, recorded in `invocation.json`, and normally non-empty only under an explicit opt-in. Default empty. |

There is no shell. Paths with spaces, quotes, `;`, `$`, `%` or `*` reach the tool
unchanged, without quoting. Never build a command string.

`base.argv_value()` converts values: paths with `os.fspath`, floats with the
shortest round-trip `repr`. Booleans are rejected because their spelling is
tool-specific. `base.thread_cap_env(n)` gives the standard BLAS/OpenMP thread caps.

### Interpretation (optional, `interpret()`)

`execute_plan(..., adapter=adapter)` calls `interpret(invocation, process)`
after the timed interval, only for processes that exited by themselves (any
exit code), and before `work_dir` is cleaned up. The default returns `None`,
so the exit code alone decides. An `Interpretation` has:

| Field | Effect |
|---|---|
| `failure_reason` | Non-`None` fails a clean exit (`failure_phase = "interpretation"`). For a failed process it becomes the explanation (`failure_phase = "process"`). It can never make a failure succeed. |
| `warnings` | Appended to the attempt warnings. |
| `tool_timing: ToolTiming` | Inner workload interval from the tool's own wall clock (`start_wall_ns`, `end_wall_ns`, `clock`, `boundary`, `marks`). The orchestrator derives `workload_elapsed_ns`, `startup_overhead_ns` and `shutdown_overhead_ns` against the outer process interval. The timed interval stays the outer one. |
| `details` | JSON-ready tool facts recorded in `execution.json`. |
| `accepted_exception: AcceptedException` | A named, documented tool behaviour under an explicit opt-in: `name`, `exit_code` (the raw status it covers, `>= 0`), `policy` (the opt-in) and `evidence` (the recognized output lines). |

If `interpret()` raises, returns the wrong type or returns details that are not
JSON, the attempt fails (`adapter interpretation failed: ...`). Fail closed.

An accepted exception is the **only** way a nonzero exit can count as
`succeeded`, and the engine honours it only if all of these hold: the command
declared the name in `CommandSpec.accepted_exceptions`, `failure_reason` is
`None`, and the process exited by itself (no timeout, cancellation, signal or
engine error) with exactly `exit_code`. Otherwise the attempt fails and the
reason says why. `execution.json` and `result.json` record it as
`accepted_exception` (with `raw_exit_code`). The raw process record keeps its
own `failed` verdict and exit code, and the results table has an
`accepted_exception` column. An exception with `exit_code` 0 only labels the
attempt. Exceptions say nothing about validation or comparability.

### ArtifactSpec

`path` (absolute, inside `output_dir`), `kind` (`raster`, `point_cloud`,
`vector`, `table`, `log`, `other`), `required`, `description`. Structural
validation uses this inventory.

## 4. Launch styles

`execution/adapters/launchers.py` returns argv tuples for each style:

| Style | Helper | Shape |
|---|---|---|
| Native executable | `native_argv(exe, *args)` | `exe args…` |
| R script | `rscript_argv(rscript, script, *args)` | `Rscript --vanilla script args…` |
| Python library | `python_module_argv(python, module, *args, isolated=False)` | `python [-I] -m module args…` in a configured interpreter, never in-process |
| Config-file tool (PDAL) | `ConfigFile` + `invocation.config_path(name)` | `pdal pipeline <work_dir>/pipeline.json` |

`resolve_executable(name_or_path)` resolves through `PATH` (honouring `PATHEXT`
on Windows) and returns an absolute path or `None`. It does not follow symlinks,
so a virtual-environment interpreter keeps its environment.

## 5. Adding an adapter

1. Subclass `ToolAdapter`. Set `adapter_id` (`^[a-z][a-z0-9_-]*$`) and
   `display_name`, and implement the five abstract methods. The constructor must
   work without arguments. Read installation paths from configuration or the
   environment, not from hard-coded locations.
2. Register an entry point whose **name equals `adapter_id`**:

   ```toml
   [project.entry-points."lidarbench.adapters"]
   mytool = "my_package.adapter:MyToolAdapter"
   ```

   This works from any installed distribution, so third-party adapters need no
   change to `lidarbench` itself.
3. Re-sync (`uv sync`, or `pip install -e .`) and run `lidarbench check` to confirm
   detection, version and declared workloads.
4. Test with `plan_invocation()`, using paths that contain spaces. See
   `tests/python/adapter_fakes.py` for Rscript, native and PDAL-style examples.

Entry points that fail to import, are not `ToolAdapter` subclasses, are
abstract, or whose name differs from `adapter_id` are listed as load failures by
`lidarbench check`. They never stop other adapters from loading.

## 6. Synthetic adapter

`synthetic` (`execution/adapters/synthetic.py`) needs only a Python interpreter.
It is always registered so that the pipeline can be tested without LiDAR
software. It implements its **own** workload IDs and declares no real LiDAR
workload, so its outputs can never be mistaken for benchmark evidence.

| Workload | Status | Behaviour | Known output |
|---|---|---|---|
| `synthetic.checksum` | runnable, 1 thread | argv-only; hashes each input | `<input name>.checksum.json`: byte count and SHA-256 |
| `synthetic.hashchain` | runnable, 1 thread | config-file style (`synthetic-job.json`); iterates SHA-256 `iterations` times (default 100000, range 1–10⁸) | `hashchain.json`: input digests and final digest |
| `synthetic.blocked` | blocked | exercises `skipped_blocked` | none |
| `synthetic.unsupported` | unsupported | exercises `skipped_unsupported` | none |

Outputs are canonical JSON (sorted keys, compact, UTF-8, trailing newline) and
byte-identical for identical inputs and parameters. Golden values are pinned in
`tests/python/test_synthetic_adapter.py`. The worker
(`synthetic_worker.py`, stdlib only) exits 1 with an `ERROR:` line on failure
and performs no timing.

## 7. `lidarbench check`

```text
lidarbench check [--json] [--adapter ID ...]
```

Prints OS, release, architecture, Python, logical CPUs and memory, then for each
adapter: availability (with reason), version, executables, declared workloads
with status, threading model and thread range, and the reasons for unsupported
or blocked workloads. Plugin load failures are listed. `--json` emits a
`lidarbench.check/1` document whose adapter entries use the `lidarbench.adapter/1`
shape. Unavailable software is reported, not treated as an error; the exit code
is 0. An unknown `--adapter` ID exits with 2.

## 8. R adapters (`lidr`, `lasr`)

lidR and lasR run through the R bridge (`Rscript --vanilla bridges/r/runner.R`).
They reuse the legacy harness unchanged and pin the exact legacy calls. They
use `interpret()` to read the bridge's result record. R warnings during the
workload, R errors, missing records and version drift all fail the attempt.
They also report R's inner workload interval. See [R_BRIDGE.md](R_BRIDGE.md).

## 9. LAStools adapter (`lastools`)

`execution/adapters/lastools.py` runs the native LAStools binaries as fresh
processes from argument vectors (example:
[configs/examples/lastools-campaign.toml](../configs/examples/lastools-campaign.toml)).
Nothing is downloaded, installed or licensed.

**Discovery.** The `bin_dir` option, else `$LIDARBENCH_LASTOOLS_BIN`, else the
first `PATH` directory that holds a workload command. All commands then come
from that one directory. On Windows the commands are `<name>.exe`; elsewhere
they are the vendor's suffix-less native binaries. A missing directory is
`misconfigured`; no command is `not_installed`. Commands are never assumed to
exist: a workload whose command is missing, not executable or unidentifiable
is declared `unsupported` with the reason, and the others stay runnable.

**Version and identity.** `<command> -version` (argv, timeout
`version_timeout_s`, default 30 s) must exit 0 with exactly one
`LAStools … version <build>` line. Only the build number is kept
(`ToolIdentity.version`, `details["version.<command>"]`). The licensing part of
the banner can contain licensee details and changes between identical queries,
so it is never recorded and never quoted in errors. Mixed builds in one
directory are `misconfigured`; `expected_version` refuses other builds
(`version_unsupported`). The executables are hashed by the campaign identity.
License files are not read, hashed or recorded.

**Workloads.** Threading model `native_threads` (`-cores N`, 1 or more).

| Workload | Command | Artifact | Arguments (from `R/0_setup.R`) |
|---|---|---|---|
| `pixel_metrics_1a` | `lascanopy64` | `{stem}_avg.tif` | `-i <inputs> -step 20 -avg -odir <out> -otif -cores N -buffered 20 -use_orig_bb -height_cutoff <cutoff> -grid_ll 0 0 -no_data -9999 -no_kml` |
| `generate_DSM1` | `lasgrid64` | `{stem}.tif` | `-i <inputs> -highest -otif -odir <out> -step 1 -cores N -buffered 20 -use_orig_bb -no_data -9999 -no_kml` |

The templates are pinned in Python; a test compares them with `R/0_setup.R` and
the artifact names with the workload contracts. A changed template needs a new
workload version. Placeholders are a closed set (`{inputs}` expands to one
argument per tile, `{output}`, `{cores}`, `{height_cutoff}`). As in the legacy
harness, an `epsg` workload parameter appends `-epsg N` (metadata, no
reprojection). The adapter always appends `-fail`, so LAStools fails instead of
running unlicensed (in the legacy harness this flag came only with the EPSG).
The height cutoff is not a parameter. It is a port of
`benchmark_height_cutoff()`: the smallest header minimum Z of all inputs, minus
at least one unit, formatted like R `format(digits = 17)`. Golden values from
R are tested. Unreadable headers fail planning. The environment follows the
legacy harness: thread caps of 1, with `OMP_NUM_THREADS` and `OMP_THREAD_LIMIT`
set to N. On Windows, a command line longer than 32,766 characters is
`unsupported`, because the pinned call passes every tile with `-i`. Other
legacy LAStools tasks are declared `unsupported` with reasons.

**Interpretation (conservative).** stdout and stderr lines are read as the
legacy harness read them. The attempt fails on any of these:

1. a nonzero exit, unless it is the accepted exception below;
2. any `ERROR`/`FATAL` line, even with exit 0 (a multi-core parent can exit 0
   after a child failed);
3. an `unlicensed` notice (outputs could be perturbed);
4. any `WARNING:` line outside the accepted exception. The legacy harness kept
   these as diagnostics that blocked validation; here they fail execution;
5. a missing or empty expected artifact (hidden child failures).

Licensing lines are withheld from failure reasons; the raw logs stay in the
attempt. LAStools reports no inner timing, so `tool_timing` is `null`.

**Cutoff-warning exception.** `accept_known_cutoff_warning = true` (tool
option, default `false`, part of the campaign identity) declares
`lascanopy_all_values_above_height_cutoff` for `pixel_metrics_1a` commands
only. The full three-line block must match: a finite ordered Z range for a
supplied input, above the cutoff passed after the single `-height_cutoff`, the
same reported cutoff, and the exact `lasheight` suggestion. Raw status 1 is
accepted only if every warning is recognized and no error occurred. Status 0
with recognized warnings is labelled the same way. The recorded policy is
`lastools.accept_known_cutoff_warning`. This is a port of
`benchmark_lastools_cutoff_warnings()` (README.md, WORKLOADS.md).

**Validation.** Up to `workloads-v3` the contracts declare LAStools
`non_comparable` with `undeclared` cell ownership, so its outputs are at best
`unverified` and descriptive only. `workloads-v3` accepts the `.tfw` world file
LAStools writes beside every GeoTIFF as an ignored sidecar (v1/v2 report it as
unexpected). `workloads-v4` (kept by `workloads-v5` and `workloads-v6`, the default) declares LAStools `matched_definition` with
the upward-Y floor cell rule (`pdal_binmode_floor`, the rule PDAL binmode uses)
and, for `lascanopy -avg`, `value_precision = 0.01` (averages are written
rounded to centimetres; the tolerance is widened by 0.005). On the Task 13 pilot
pair (LAStools 261023) `lasgrid -highest` matched that reference exactly and
`lascanopy -avg` within ±0.005 m with an identical NoData mask, so LAStools
outputs are validated and functionally comparable (PILOT_REVIEW.md §14). The
declaration rests on this evidence: a different LAStools build is re-validated
by every campaign.

`generate_DEM` (workloads-v6) pins `blast2dem64 -keep_class 2 9 -elevation
-step 1 -kill 100000 -buffered 20 -use_orig_bb`. It deliberately deviates from
the legacy `-kill 100` (blast2dem's default) so that, like lasR, no long
triangle is dropped; `LastoolsWorkload.legacy_deviation` records the change, the
parity test checks the remaining arguments against `R/0_setup.R`, and the
invocation metadata reports it (`legacy_deviation`). blast2dem writes a `.kml`
despite `-no_kml`; the contract lists `*.kml` as an ignored sidecar. The
contract is structural-only and LAStools `non_comparable`/`undeclared`.

Integration tests (`tests/python/test_lastools_integration.py`) run only when
an installation is found, for example
`LIDARBENCH_LASTOOLS_BIN=/path/to/LAStools/bin uv run pytest tests/python/test_lastools_integration.py`.

## 10. PDAL adapter (`pdal`)

`execution/adapters/pdal.py` expresses the workloads with PDAL's own stages
(not a port of an R call) and runs them with the native `pdal pipeline` command
(example: [configs/examples/pdal-campaign.toml](../configs/examples/pdal-campaign.toml)).
Nothing is downloaded or installed.

**Discovery and platforms.** The `executable` option, else `$LIDARBENCH_PDAL`,
else `pdal` on `PATH` (`pdal.exe` through `PATHEXT` on Windows; activate a conda
environment to put it there). Linux, Windows and macOS are supported; other
systems are `platform_unsupported`. Probing runs `pdal --version` (must print
one `pdal x.y.z` line), `pdal --drivers --showjson` (needs `readers.las`,
`filters.merge`, `writers.gdal`) and `pdal --options writers.gdal --showjson`
(needs `binmode`, `origin_x`, `origin_y`, `width`, `height`, `output_type`,
`nodata`, `data_type`, `gdaldriver`, `override_srs`, `resolution`); it also
reads `pdal --options readers.las --showjson` to see whether `threads` exists. PDAL older
than 2.5.0 (no `binmode`), a missing option or `expected_version` mismatch is
`version_unsupported`; a missing stage is `misconfigured`. Identity records the
version, the full version string, the `pdal` and launcher Python executables
(hashed by the campaign), the discovery source and the execution mode.

**Pipelines.** One pipeline file per input tile, written by the orchestrator as
`ConfigFile`s (`pipeline-0001.json`, …) into the work directory:

```text
readers.las(tile, threads=1) [, readers.las(neighbour, threads=1)… → filters.merge]
  → writers.gdal(binmode=true, output_type=mean|max, resolution=r,
                 origin_x, origin_y, width, height,      # header bounds snapped outward
                 nodata=-9999, data_type=double, gdaldriver=GTiff
                 [, override_srs="EPSG:N"])               # optional epsg parameter
```

Neighbours are the inputs whose header bounds intersect the tile bounds grown by
the contract buffer (20 m, closed rectangle) — the validator's population rule.
The grid is fixed explicitly because PDAL otherwise sizes it from the points as
`floor(extent / r) + 1` cells. JSON is UTF-8 with paths verbatim, so spaces,
non-ASCII characters and Windows separators need no quoting. Paths containing
`#` (PDAL expands it into per-view file names) or `://` (URL) are `unsupported`.

**TIN pipeline (`generate_DEM`, workloads-v6).** A structural-only contract
(design/VALIDATION.md §1):

```text
readers.las(tile[, neighbours], threads=1[, override_srs="EPSG:N"]) [→ filters.merge]
  → filters.crop(bounds = tile header bounds grown by 20 m)
  → filters.range(limits = "Classification[2:2],Classification[9:9]")
  → filters.delaunay
  → filters.faceraster(resolution=1, origin_x, origin_y, width, height,  # snapped outward
                       nodata=-9999, max_triangle_edge_length=100)
  → writers.raster(data_type=double, nodata=-9999, gdaldriver=GTiff)
```

The crop keeps the TIN to the contract's 20 m neighbourhood instead of whole
neighbouring tiles. `writers.raster` has no `override_srs`, so the optional EPSG
is set on the readers. `filters.delaunay` cannot stream, so this workload always
runs `--nostream` whatever `execution_mode` says (metadata `execution_mode`
records the mode used, `configured_execution_mode` the option). These stages are
not required by the probe: a PDAL without them only makes `generate_DEM`
`unsupported`. `faceraster` samples the TIN at cell centres; on a planar ground
surface the real-PDAL integration test reproduces it to < 1e-6 m.

**Execution.** The benchmarked command is the stdlib launcher
`python -m lidarbench.execution.adapters.pdal_worker --job pdal-job.json`
(threading model `concurrent_files`, 1+). It runs
`pdal pipeline <pipeline> --metadata <file> [--stream|--nostream]` once per tile,
at most `threads` at a time, with argv and no shell. Each child's output goes to
a per-tile log that is echoed into the attempt's stdout. After the first failure
no further pipeline starts and running ones are terminated. The launcher writes
`pdal-run.json` (`lidarbench.pdal-run/1`: exit codes and launch/exit instants).
`execution_mode` (option, default `stream`, part of the identity) selects
`--stream` (fail if PDAL cannot stream), `standard` (`--nostream`) or `prefer`
(PDAL's choice, not recorded). Environment: thread caps of 1 and
`GDAL_NUM_THREADS=1`. Every `readers.las` stage sets `threads = 1`: its LAZ
decompression pool defaults to 7, which made a "1 CPU" attempt use ~2.9 cores
in the Task 13 pilot. The value (or "not set" for PDAL builds without the
option) is in the identity details and the invocation metadata
(`reader_threads`); the runner's CPU pinning enforces the budget regardless. The timed interval is the launcher process (Python start-up
included). `tool_timing` reports the inner interval from the first `pdal` launch
to the last exit.

**Interpretation (fail closed).** The attempt fails on a missing or inconsistent
run record, any tile that did not run or exited nonzero, any line containing
`error`/`exception`/`fatal`/`PDAL:` or `warning` in a tile log (even with exit
0), a missing or empty raster, or a missing `--metadata` record. `details`
capture every pipeline document, per-tile exit codes, instants and log digests,
and a summary of each metadata record (SHA-256, size, reader counts and bounds,
the `writers.gdal` node). Config-file digests are in `invocation.json`.

**Semantics, from the PDAL source** (`io/GDALWriter.cpp`,
`io/private/GDALGrid.cpp`, `pdal/private/Raster.hpp`; identical in 2.5.0 and
2.10):

| Topic | PDAL behaviour | Contract (`workloads-v2`) |
|---|---|---|
| Neighbourhood | Default mode adds each point to every cell whose centre is within `radius` (default `r·√2`), with optional `window_size` fill. `binmode=true` bins each point into one cell. Only binmode is used. | cell statistic |
| Cell index | `floor((X − origin_x)/r)`, `floor((Y − origin_y)/r)` in binary64, rows counted **upwards** from `origin_y` | `right_down` |
| Exact edges | rightward X, **upward** Y: a point on a horizontal edge goes north; points on the grid's north/east edge are outside this tile's raster, those on its south/west edge inside | downward Y; north edge inside, south edge outside |
| Statistic | `mean`: running (Welford) mean in double; `max`: maximum | float64 mean / max |
| Empty cells | NaN internally, written as `nodata` (double default −9999, set explicitly) | NoData |
| Band | one band per `output_type`, description `mean`/`max` | `z_mean` required only for lidR/lasR |
| Extent | as given by `origin_x/origin_y/width/height` | header bounds snapped outward |

The contract convention `pdal_binmode_floor` mirrors this arithmetic exactly
(`validation/grid.py`), so a PDAL raster is validated against a reference built
with PDAL's own edge rule, and `boundary_difference_points` measures how many
points it owns differently from the contract partition. Platforms whose
compilers fuse the LAS scale/offset multiply-add can decode a coordinate one ulp
differently. A point that close to an edge would then be binned differently and
validation would fail rather than hide it.

**Classification.** `WorkloadContract.declared_comparability()` derives one of
four classes from the declaration (never from results): `strictly_comparable`
(matched definition, contract partition), `functionally_comparable` (matched
definition, different declared edge convention), `non_comparable` and
`unsupported`.

| Workload | lidR | lasR | LAStools | PDAL |
|---|---|---|---|---|
| `pixel_metrics_1a` | functionally | strictly | non-comparable | **functionally** |
| `generate_DSM1` | functionally | strictly | non-comparable | **functionally** |
| `normalization`, `generate_DEM`, `generate_DSM2`, `pixel_metrics_2`, `tree_detection`, `complex_task1/2` | — | — | — | **unsupported** (no contract; candidate PDAL stages are not evaluated) |

PDAL computes the same population, grid and statistic, but its upward-Y edge rule
differs from the contract and from lidR's clamp and lasR's partition. PDAL is
therefore never declared identical to lidR or lasR. An attempt can still become
`comparable` only through the existing data-driven gate (validated and
`boundary_difference_points == 0`, for example when no point lies on a
horizontal cell edge). Data quantized to centimetres usually has points on 1 m
grid lines, so PDAL `generate_DSM1` timings are normally `descriptive_only`.
The adapter adds no transformation to force agreement.

Unit tests (`tests/python/test_pdal_adapter.py`) use a fake `pdal` that
reproduces the documented binmode arithmetic and also check pipeline
generation, discovery, failures and a campaign. Integration tests with a real
installation validate the actual PDAL outputs:
`LIDARBENCH_PDAL=/path/to/pdal uv run pytest tests/python/test_pdal_integration.py`.

## 11. Not yet implemented

- Real adapters `cli` and `pylib`. The modules exist as placeholders and are
  not registered.
- LAStools `normalization` and `pixel_metrics_2` (no contract yet).
- Machine-level tool configuration (installation paths outside campaign
  configurations). Adapter options currently come from `[[tools]] options`.
- Equivalence data covers only the contracts in `workloads/` (`workloads-v6` is
  the default; `workloads-v1` to `workloads-v5` are kept frozen; v1 has no PDAL
  entry). `generate_DEM` has no numerical reference (structural validation only).
