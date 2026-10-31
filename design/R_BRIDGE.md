# R bridge: lidR and lasR adapters (`lidarbench.r-bridge-*/1`)

Status: **implemented** (Phase 3, Task 7).
Code: [`execution/adapters/rscript.py`](../src/lidarbench/execution/adapters/rscript.py)
(shared base), [`lidr.py`](../src/lidarbench/execution/adapters/lidr.py),
[`lasr.py`](../src/lidarbench/execution/adapters/lasr.py),
[`bridges/r/`](../bridges/r/). Companions: [ADAPTERS.md](ADAPTERS.md),
[ARCHITECTURE.md §7.2](ARCHITECTURE.md#72-r-bridge-protocol-rscript-adapter).

lidR and lasR are two independent adapters (`lidr`, `lasr`). Each attempt is
one fresh `Rscript` process. The Python orchestrator never embeds or imports R.
It writes a JSON invocation, launches `Rscript`, times the process like any
other tool and reads a JSON result record afterwards (untimed).

## 1. Reuse of the legacy harness

The bridge does **not** reimplement algorithms. The legacy files are used
unchanged:

| Legacy piece | Use in the bridge |
|---|---|
| `R/1_functions.R` | `sys.source()`d into a private environment |
| `R/0_setup.R` `benchmark_tasks` | parsed, not run. Each cell of the `tribble()` literal is evaluated on its own in `baseenv()` to get the exact `task_call_lidR` / `task_call_lasR` string. |
| `prepare_input()` | lidR catalog configuration: buffer 20, chunk 0, alignment (0, 0), `wall_to_wall = FALSE`, `{ORIGINALFILENAME}` outputs |
| `benchmark_with_resources()` | reference-BLAS check, thread caps, `future` multisession or mirai (lidR), `concurrent_files` (lasR), cleanup |
| `benchmark_execute_call()` | evaluates the call and records the **inner** start/finish time (`Sys.time()`) |

The Python adapter pins each call string (`RWorkload.call`). The bridge
refuses to run (exit 2) when `R/0_setup.R` holds a different string, because a
changed definition needs a new workload version. A unit test checks that the
pinned strings occur verbatim in `R/0_setup.R`. A second test checks that
buffer, artifact naming and implementation status agree with the
`workloads-v1` contracts.

The lasR adapter does not load lidR. The legacy harness built a lidR catalog
only to list file names, so the bridge passes the input paths directly.

## 2. Command line and files

```text
Rscript --vanilla <project>/bridges/r/runner.R \
    --project <project> --renv on|off \
    --invocation <work_dir>/r-invocation.json \
    --result <work_dir>/r-bridge-result.json
```

The orchestrator writes `r-invocation.json` (a `ConfigFile`) with exclusive
create. `cwd` is `work_dir`. The project and renv flag are passed as arguments
because they are needed before `jsonlite` can be loaded. The result path is
passed for the same reason, so that failures before the invocation is parsed
are still reported. `bridge.R` uses base R only and has its own JSON writer.

### `lidarbench.r-bridge-invocation/1`

| Key | Meaning |
|---|---|
| `tool` | `lidR` or `lasR` (package attached with `library()`) |
| `workload_id`, `task_id` | workload contract ID = legacy task ID |
| `expected_call` | pinned call string; must equal `R/0_setup.R` |
| `output_extension` | `tif` (`fout = <output_dir>/*.tif`) |
| `buffer` | contract buffer (20.0); checked against the catalog / lasR `with` |
| `inputs`, `output_dir`, `work_dir` | absolute paths (spaces and metacharacters allowed) |
| `threads`, `threading_model` | thread budget, `worker_processes` or `concurrent_files` |
| `epsg` | optional declared horizontal CRS (`parameters.epsg`), or `null` |
| `required_packages` | must all be loadable (`lidR, future, data.table, jsonlite` / `lasR, data.table, jsonlite`) |

### `lidarbench.r-bridge-result/1`

Written atomically (`.partial` + rename) in every case the R process survives:

`status` (`succeeded` / `failed`), `phase` (`invocation`, `environment`,
`packages`, `preparation`, `resources`, `workload`, `teardown`, `complete`),
`exit_code`, `diagnostic`, `warnings` (`[{phase, message}]`), `r` (version,
platform, `startup_elapsed_s` from `proc.time()`, BLAS, LAPACK), `renv`
(enabled, version, library paths), `packages` (required package versions),
`preloaded_namespaces` (namespaces preloaded before the workload and their
versions, `null` if not installed; §7), `workload_loaded_namespaces`
(namespaces first loaded during the workload; expected empty),
`loaded_namespaces` (every loaded namespace and its version at the end),
`legacy` (files, task, call, legacy execution status), `catalog` (applied
buffer/chunking/CRS), `threads` (the thread state after setup: lidR threads,
`future` plan and workers, lasR strategy and cores, data.table threads,
thread variables), `timing_workload` (inner start/end in wall-clock ns),
`timing` (marks: `bridge_start`, `environment_ready`, `packages_loaded`,
`namespaces_preloaded`, `legacy_sourced`, `prepared`, `resources_ready`, `teardown_done`,
`bridge_end`) and `outputs`.

### Exit status

| Code | Meaning |
|---|---|
| 0 | succeeded, no R warning during the workload |
| 1 | R error during preparation, resource setup or the workload |
| 2 | invalid invocation, or `R/0_setup.R` differs from the pinned definition |
| 3 | R warning during the workload (treated as failure) |
| 4 | renv activation failed or a required package is missing |
| 5 | the result record could not be written |

## 3. Failure policy

`RBridgeAdapter.interpret()` runs untimed after the process exits. It can only
fail an attempt:

- a missing, unreadable or wrong-schema record fails the attempt; the reason
  includes the exit status meaning and the end of `stderr.log`;
- `status != succeeded` fails it with the bridge's phase and diagnostic;
- a success record combined with a non-zero exit, or a non-zero exit combined
  with a success record, fails it;
- a success record without a workload interval, or one that names another tool
  or workload, fails it;
- R or package versions that differ from the probed identity fail it, so the
  attempt cannot be credited to the wrong version;
- **any R warning during the workload fails the attempt** (exit 3). The legacy
  harness recorded such warnings as diagnostics of a successful run. The new
  adapter is stricter and records them as failures. Warnings in other phases
  (package loading, catalog construction, teardown) are kept as attempt
  warnings and do not fail the attempt.
- the single exception: the lidR option `accept_known_tin_fallback_warning`
  (default false, part of the campaign identity) accepts, for `generate_DEM`
  and `normalization` only, an exit 3 whose workload warnings *all* fully match lidR `tin()`'s
  "Interpolation of N points failed because they are too far from ground
  points. Nearest neighbour was used but interpolation is weak for those points"
  (cells or points far from any ground point take the nearest ground point's Z;
  for `normalization` this is the declared `outside_surface = "extrapolate"`). The pinned
  call must have succeeded and every other check above still applies. The
  attempt then succeeds with the declared accepted exception
  `lidr_tin_nearest_neighbour_fallback` (policy
  `lidr.accept_known_tin_fallback_warning`, raw exit 3 and the warnings recorded
  as evidence); any other or additional warning still fails it. On the Petawawa
  density-10 data 9 of 100 tiles raise it.

Every warning is listed in `execution.json` / `result.json` as
`R warning (<phase>): <message>`. The full bridge record is kept in
`execution.json` under `interpretation.details.bridge_record`.

## 4. Inputs, outputs and buffers

- Inputs are the dataset's selected files. Neighbouring files supply the 20 m
  buffer, and there is one output per input (`{stem}.tif`, required). lidR also
  writes a VRT mosaic, which validation ignores as a sidecar.
- Planning rejects inputs that are not `.las`/`.laz` and inputs whose stems
  would write the same output (`skipped_unsupported`).
- **lasR needs `.lax` indexes.** Without them, lasR writes `.lax` files next to
  the inputs, which would modify the protected dataset. The bridge refuses
  unindexed inputs before any work (exit 1, phase `preparation`). Create the
  indexes once, before the campaign is created (for example
  `lasR::exec(lasR::write_lax(), on = files)`); they become part of the dataset
  identity. COPC inputs (first VLR `copc`/1) carry their own spatial index and
  need no `.lax`; the bridge and preflight accept them as indexed. lasR, lidR
  and LAStools read COPC as LAS 1.4 (on the Petawawa density-1 pair the DEMs were
  identical to those from the original LAZ + `.lax`).
- `epsg` (optional workload parameter): lidR checks every header CRS. It refuses
  a conflicting CRS and assigns the declared CRS to the catalog, as
  `benchmark_apply_input_metadata()` does. lasR prepends `set_crs(epsg)` like the
  legacy wrapper. As in the legacy harness, it does not check header CRS
  conflicts itself.
- Any other parameter makes the combination `skipped_unsupported`.

## 5. Threads and environment

The environment delta of every command sets the legacy caps before R starts.
`OMP_NUM_THREADS`, `OPENBLAS_NUM_THREADS`, `MKL_NUM_THREADS`,
`BLIS_NUM_THREADS`, `VECLIB_MAXIMUM_THREADS`, `RCPP_PARALLEL_NUM_THREADS`,
`OMP_THREAD_LIMIT` and `OMP_MAX_ACTIVE_LEVELS` are all `1`. For lasR,
`OMP_NUM_THREADS` and `OMP_THREAD_LIMIT` equal the thread budget.
`benchmark_with_resources()` then applies the same values inside R, verifies
reference BLAS, and starts *n* `future` workers (lidR) or
`concurrent_files(n)` (lasR). The applied state is recorded in the result.

The lidR `future` backend is the tool option `future_backend`:

* `"multisession"` (the legacy harness and lidR's documented example):
  `future::plan(multisession, workers = n, rscript_startup = …)`. `plan()` returns
  once every worker runs and has executed the start-up (`lidR` loaded,
  `set_lidr_threads(1)`, `setDTthreads(1)`).
* `"mirai"` (default since workloads-v7): `future.mirai::mirai_multisession` (needs `future.mirai` and `mirai`,
  locked in `renv.lock`). `benchmark_plan_mirai()` passes the session's library
  paths to the daemons (`R_LIBS`), waits until all *n* daemons are connected and
  runs the same start-up once on each (`mirai::everywhere()`), so both backends
  start the timed call from the same state.

Both backends' workers are re-parented away from the R process but stay in its
session and process group, so they are sampled and cleaned up as part of the
attempt (EXECUTION.md §6); `wait4` accounting excludes both alike. The bridge
records `future_backend`, the plan class and the worker count. Use a separate
`[[tools]]` entry per backend, for example `id = "lidr-multisession"`,
`adapter = "lidr"`, `options = { future_backend = "multisession" }`. With
`multisession`, lidR's main R process saturates one core from about 16 workers
and limits scaling; mirai's dispatch overhead is lower (diagnostic on the
10 pts/m² Petawawa tiles, mean metric: multisession 250/201/287 s, mirai
244/153/86 s at 8/16/32 workers).

| Adapter | Threading model | Threads |
|---|---|---|
| `lidr` | `worker_processes` | 1+ |
| `lasr` | `concurrent_files` | 1+ |

## 6. renv, versions and availability

`probe()` checks, in order:

1. the bridge and legacy files exist (`misconfigured`);
2. Rscript resolves from the `rscript` option, then `$LIDARBENCH_RSCRIPT`, then
   `PATH` (`not_installed`);
3. `bridges/r/probe.R` runs and prints a `lidarbench.r-bridge-probe/1` record
   (`probe_failed` on timeout or invalid output);
4. renv activates (`misconfigured`);
5. every required package is installed and its namespace loads, so missing
   native libraries are detected (`not_installed`);
6. with renv and `require_locked_versions` (the default), R and the required
   packages equal `renv.lock` (`version_unsupported`).

renv is activated from the project directory, so the project-specific renv
library is used wherever the process runs. `identify()` records the R version,
platform, BLAS/LAPACK, renv version and library paths, the SHA-256 of
`renv.lock`, and the installed and locked version of every required package.
`Rscript`, `runner.R`, `probe.R`, `bridge.R`, `R/1_functions.R` and `R/0_setup.R`
are listed as tool executables. The campaign identity therefore hashes them, and
the runner re-checks their fingerprints before every schedule entry.

Tool options (`[[tools]] options`): `rscript`, `project` (default: the
checkout containing the package), `renv` (default `true`),
`require_locked_versions` (default `true`), `probe_timeout_s` (default 600) and,
for `lidr` only, `future_backend` (`"mirai"` default or `"multisession"`, §5).

## 7. Timing

The benchmark's timed interval is unchanged. It is the outer fresh process
(`timing_boundary = "process"`), which includes R start-up, renv activation,
package loading, namespace preloading, sourcing the legacy files, catalog setup, worker start-up, the
workload, worker shutdown and writing the result record. The bridge also
reports the legacy **inner** interval (`benchmark_execute_call()` start/finish).
`execute_plan()` derives the following, each recorded in
`execution.json` → `interpretation.tool_timing` and in `result.json` →
`tool_timing`:

- `workload_elapsed_ns`: inner interval (R `Sys.time()`);
- `startup_overhead_ns`: from process launch to the workload start;
- `shutdown_overhead_ns`: from the workload end to process exit;
- `within_outer_interval`: whether the inner interval lies inside the outer one.
  If it does not, the attempt gets a warning, because wall-clock adjustment
  makes the overheads unreliable.

**Namespace preloading.** In a fresh process the first pinned call loads some
namespaces lazily, which the legacy harness paid once per session, before
its timed calls. The bridge therefore loads them in the untimed packages
phase (mark `namespaces_preloaded`, after `packages_loaded`), with
`requireNamespace()` and startup messages suppressed. The pinned call and its
outputs are unchanged (byte-identical rasters on the pilot tiles).

| Tool | Preloaded | Measured lazy load in the first call |
|---|---|---|
| lasR | `sf`, `terra` (with their imports `units`, `classInt`, `DBI`, `e1071`, …) | ~4.8 s (GDAL/PROJ/GEOS); inner pm1a 7.8 → 3.0 s, DSM1 17.5 → 12.5 s at 1 CPU |
| lidR | `sf`, `terra` (already imported by lidR), `glue` | ~0.04 s; no measurable effect |

The bridge records the namespaces first loaded during the workload as
`workload_loaded_namespaces`; a nonempty list shows that the preload list
needs updating. lidR worker processes (2+ CPUs) load their namespaces inside
the inner interval, as in the legacy harness, which also started new workers
for every call.

At 1 CPU, lidR's first call in a fresh process is still ~1.5–2 s slower than
later calls (pilot inputs: 6.6–7.3 s vs ~4.5–4.9 s; legacy 4.69 s). This is
not namespace loading. It is mostly garbage collection while R's heap grows
from its start-up size (`gc.time()`: 2.6 s in the first call, 1.0 s and 0.5 s
in the next two). With a large initial heap (`R_VSIZE=4G R_NSIZE=20M`) the
first call takes 4.8 s and GC 0.08 s; the remaining ~0.4 s is first-use cost
(lazy-loaded function bodies, `future` globals search). The bridge does not
tune the heap, so this cost stays inside the inner interval.

The results table has the columns `tool_workload_elapsed_ns` and
`startup_overhead_ns`. The two boundaries are never mixed. Reports state which
boundary they use (ARCHITECTURE.md §8.4). Measured on the two-tile synthetic
fixture with 2 threads (Linux):

| Tool | Outer | Inner workload | Start-up overhead | of which renv + packages | of which workers |
|---|---|---|---|---|---|
| lidR | ~20 s | ~3.5 s | ~16 s | ~8 s | ~7.4 s (`future` multisession) |
| lasR | ~8 s | ~5.0 s | ~2.5 s | ~2.3 s | — |

On small inputs the outer time is dominated by start-up, so both boundaries
must be reported.

## 8. Validation and equivalence

Execution success says nothing about numerical validity. The integration test
(`tests/python/test_r_adapters_integration.py`) runs both adapters through a
campaign on the synthetic neighbouring-tile scene. It then validates the
retained outputs with the independent Python references:

- lidR is `validated`, but only descriptively: its outer-edge clamping
  convention differs from the contract partition (`boundary_difference_points > 0`);
- lasR fails strict structural validation, because it writes one extra cell on
  the east and north edges. Under `accept_known_extent_discrepancies` it is
  `accepted_exception`, with no boundary differences.

Neither result is `comparable`. Both adapters run, but they are **not**
declared equivalent.

## 9. Tests

| File | R needed | Covers |
|---|---|---|
| `test_r_adapters.py` | no | registration, declarations, contract and legacy-call pinning, argv/env/invocation with paths containing spaces and `;$`, parameter and input checks, probe outcomes, identity, interpretation of every failure mode, `execute_plan` and a campaign run with a fake `Rscript` (POSIX) |
| `test_attempt_interpretation.py` | no | generic `interpret()` phase: fail-closed defects, overhead derivation, timeouts |
| `test_r_adapters_integration.py` | yes (`integration`, `slow`) | real campaign with both tools, recorded versions/threads/timing, independent validation, lasR LAX refusal (inputs untouched), definition mismatch, real R warning (exit 3) and error (exit 1) |

## 10. Limitations

- Only `pixel_metrics_1a`, `generate_DSM1`, (workloads-v6, structural
  validation only, `non_comparable`) `generate_DEM` and (workloads-v7, LAZ
  output, structural validation only) `normalization` are runnable, with the
  legacy `R/0_setup.R` calls; lasR `normalization` is `diagnostic_only` (lasR ≤
  0.21.3 writes NaN heights as corrupt Z). Other legacy tasks are declared
  `unsupported` (no contract) or `blocked` (lasR TIN defects), with reasons.
- lidR `normalization` reads every attribute, so `readLAS` data-check warnings
  (for example invalid return numbers in the unrepaired pilot inputs) fail the
  attempt (exit 3); use repaired inputs.
- The lasR path does not check header CRS conflicts for a declared `epsg`
  (legacy behaviour).
- The fake-`Rscript` tests use a POSIX shebang script and are skipped on
  Windows. Real-R behaviour on Windows and macOS is not yet verified in CI.
- Python reads the bridge result from `work_dir`, so `interpret()` must run
  before scratch cleanup. `execute_plan()` guarantees this.
