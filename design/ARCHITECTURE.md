# LidarSpeedTests 2.0 — Architecture

Status: **proposal for review** (no framework code exists yet).
Branch: `feature/cross-platform-benchmark`, based on `dev` at `835cdf9`.
Companion documents: [../MIGRATION_PLAN.md](../MIGRATION_PLAN.md) (roadmap and
acceptance criteria) and [../AGENTS.md](../AGENTS.md) (permanent development rules).

## 1. Goals and non-goals

### Goals

1. Benchmark **any** LiDAR processing software — R packages (lidR, lasR),
   Python libraries, standalone command-line tools (LAStools, PDAL, …) — through
   one tool-neutral orchestration model.
2. Run **natively** on Linux, Windows and macOS. No WSL, containers or POSIX
   emulation are required. Platform gaps must be reported explicitly; they must not
   fail silently.
3. Use **Python** for orchestration. The Python core has no dependency on R,
   GDAL, PDAL, LAStools or any LiDAR library.
4. Preserve the evidence model of the current R harness:
   - frozen campaign identity;
   - immutable attempts;
   - balanced scheduling;
   - explicit workload contracts;
   - separation of execution, structural and numerical outcomes;
   - user-approved, frozen exception policies;
   - provenance-checked inputs;
   - guarded scratch handling.
5. Make extension a matter of adding a **plugin** (tool adapter, validator,
   monitor backend, report renderer) plus data files. Core edits should not be
   required.

### Non-goals

- Re-implementing LiDAR algorithms or numerical reference validators in Python
  only to change language. Mature R validators are reused through a bridge (§7.3).
- Rewriting or reinterpreting historical results, the published website
  (`docs/`, `gh-pages`), or prepared input data.
- Cold-cache control, CPU pinning, power management or other privileged system
  manipulation. These conditions are **recorded**, not controlled.
- Distributed or multi-machine scheduling. One campaign runs on one machine.

## 2. Review of the current repository

### 2.1 What exists

| Area | Location | Notes |
|---|---|---|
| Workload calls and contracts | `R/0_setup.R`, `benchmark_workload_contract()` in `R/1_functions.R`, `WORKLOADS.md` | `workloads-v1`: nine tasks × three tools. 17 runnable, 6 unsupported and 4 blocked combinations in the strict matrix. Status vocabulary: `matched_definition_pending_validation`, `non_comparable`, `diagnostic_only`, `blocked`, `unsupported`. |
| Configuration | `benchmark.example.R` (`dget` R list) | Contains labelled input/scratch roots, cores, tools, scheduling, monitoring, `validation_mode` and frozen opt-in policies. |
| Path and scratch safety | `benchmark_directory`, `benchmark_validate_scratch_root`, `benchmark_scratch_create/cleanup`, `benchmark_check_scratch_tree` | Scratch roots must not be a filesystem root, home or the working directory. They must not overlap protected paths. Each run gets a `0700` directory. Cleanup requires an in-memory ownership record, a device/inode/birth-time match and a symlink-free tree. |
| Process execution | `benchmark_process` (processx), `benchmark_lastools_command`, `benchmark_execute_call` | Commands are argv-only (no shell). Placeholders are templated. `ERROR`/`FATAL` log lines fail even when the exit status is 0. The known `lascanopy` cutoff-warning exception is an explicit opt-in. |
| Threading policy | `benchmark_with_resources`, `benchmark_validate_thread_runtime` | Thread env vars are capped. lidR uses a multisession × 1-thread policy, lasR uses `concurrent_files`, LAStools uses `-cores`. Reference BLAS is required. |
| Campaign identity | `benchmark_campaign_identity`, `benchmark_campaign`, `benchmark_campaign_check` | The identity covers code (git revision, dirty flag, file MD5s), renv lock, package file MD5s, native tool checksums and versions, input checksums, config, selection, schedule, OS/CPU/RAM/storage and parallelism. It is serialized to RDS and identified by MD5. |
| Immutable attempts | `benchmark_attempt_start/finish`, `benchmark_read_attempt` | Writes `started.rds`, a `completion-lock` directory, `result.pending.csv` → `result.csv` (rename) and `completed.rds` (MD5). Reading cross-checks the validation and monitoring receipts and the frozen policies. |
| Scheduling | `benchmark_scheduling_config`, `benchmark_schedule`, `benchmark_execute_schedule` | Warm-ups, then measurements. The tool order is a seeded random base permutation per task/run, rotated by repetition, and the selection order is shuffled per repetition. Cooldown is applied. The cache policy is `uncontrolled-warm` only. |
| Resource monitoring | `benchmark_monitor_collect` etc. | Linux `/proc` only, whole-machine scope, run as a background `callr` process. Samples, report and summary are bound to the attempt with checksums. `cpu_ram_usage.ps1` is a historical Windows logger. |
| Artifact validation | `benchmark_validation_manifest`, `benchmark_validate_artifacts` and helpers (~500 lines) | `validation-v1`: exact artifact inventory, raster geometry/CRS/bands, NoData masks, tool-specific edge ownership, independent references, tolerances, and `performance-only` structural mode. |
| Data preparation | `R/prepare_fresh_data.R`, `R/2_data.R`, `R/6_pilot.R`, `R/3_runBenchmarks.R` | Petawawa archive download (explicit), repair policy `fresh-petawawa-v4`, density variants, synthetic jitter, LAStools sort/index, per-tile receipts, `ready.rds`, parallel verification, and preparation lock. |
| Reporting | `R/reporting.R`, `R/5_report.R`, `*.qmd`, `_quarto.yml` | Campaign-specific report build. Selects the latest attempt per key and applies exclusion reasons. `n_comparable` is gated and `performance-only` mode exists. Reports are built in fresh directories with a checksummed `build.rds` receipt. |
| Legacy compilation | `R/4_compileResults.R` (`LIDAR_RESULTS_MODE=legacy`) | Reads historical CSVs as `legacy-unidentified`/`unverified`. |
| Tests | `tests/testthat/*` (23 test files + fixture helper, ~159 `test_that` blocks) | Synthetic fixtures; `Rscript` stands in for native tools; real pinned lidR/lasR on tiny clouds. |
| Environment | `renv.lock` (R 4.6.1, 153 packages; lidR 4.3.3, lasR 0.21.2), `.R-version`, `R/check_environment.R` (Quarto 1.10.18) | |
| Historical / protected | `docs/` (published site), `gh-pages`, `graphics/`, `_OLD/` (ignored, 94 MB), `ALS_tiles.gpkg`, `input-manifests/` | |

### 2.2 Strengths to keep

- **Evidence semantics.** These are the project's real intellectual property:
  - execution success is distinct from validation;
  - `unverified` is never promoted;
  - `accepted_exception` is never `passed`;
  - `n_comparable` requires a matched contract, passed numerical validation and
    zero boundary differences;
  - performance-only reports are descriptive.
- **Frozen opt-in policies.** Extent, surface, LAStools cutoff and diagnostic lasR
  are all part of campaign identity.
- **Conservative failure handling.** Native child failures that hide behind a
  zero parent exit status are caught through log parsing.
- **Write-once attempts with checksummed receipts.** Incomplete attempts block
  compilation instead of disappearing.
- **Scratch guards.** These include ownership, identity and symlink refusal.
- **Dataset provenance.** Preparation records the archive hash, a policy version,
  per-tile receipts and readiness, and preflight verifies them.

### 2.3 Limitations that motivate 2.0

| # | Limitation | Consequence |
|---|---|---|
| L1 | Orchestration, records and configuration are R-specific: RDS, `dget` configs and R RNG. | Python, CLI or other non-R tooling cannot read or produce campaign records. MD5 of an RDS serialization depends on the R version. |
| L2 | R tools run **in-process** in the harness session (`eval(parse(text = …))`). | Tool crashes or leaks affect the orchestrator. Timing boundaries differ between R tools (in-process) and CLI tools (subprocess). |
| L3 | `benchmark_campaign_check()` recomputes the **full identity before every schedule entry**. That includes hashing every input file and calling `git rev-parse HEAD`. | Each entry spends minutes hashing. Unrelated git operations in the checkout (commit, branch switch) abort a running campaign even when the code content is unchanged. This was observed while creating this branch; see §11. |
| L4 | Monitoring is Linux `/proc` whole-machine only. | No Windows/macOS monitoring and no process-tree attribution. |
| L5 | Timing comes from `Sys.time()` written to CSV with **second resolution** unless monitoring is on. | Sub-second precision is lost. The timing source is a wall clock, not a monotonic clock. |
| L6 | Path comparisons lowercase only on Windows. Junctions/reparse points are not considered. | macOS (case-insensitive APFS by default) and Windows junction edge cases are unguarded. |
| L7 | The scratch ownership registry is in-memory. | After a crash, orphaned scratch directories cannot be proven run-owned for safe later cleanup. |
| L8 | Tool support is hard-coded (`c("lidR","lasR","lastools")`) throughout config validation, contracts, scheduling and reporting. | Adding a tool means editing many functions. |
| L9 | Execution lock is a directory in `reports_path`. It has no PID/host metadata and no machine-wide scope. | Stale locks are hard to diagnose, and two campaigns with different report roots can run concurrently on one machine and disturb measurements. |
| L10 | No per-attempt timeout. Interrupted attempts block compilation until handled manually. | |

## 3. Architectural principles

1. **Tool-neutral core, tool-specific plugins.** The core knows workloads, runs,
   attempts, processes, files and statuses. It does not know LAS semantics,
   R or LAStools flags.
2. **Every timed workload is an external process.** The orchestrator never runs
   a benchmarked tool in-process. Python libraries run in a child Python
   interpreter, R packages in `Rscript`, CLIs directly. This gives isolation,
   uniform timing boundaries, process-tree monitoring and safe termination.
3. **Language-neutral, append-only records.** Records use UTF-8 JSON, JSON Lines
   and CSV with explicit schemas and SHA-256 digests. Any language can read them.
   Nothing is rewritten after publication.
4. **Identity by content, not by checkout state.** Campaign identity hashes the
   content that affects results. The git revision is recorded as provenance but
   is not live-rechecked (fixes L3).
5. **Four separate verdicts.** Execution, structural validation, numerical
   validation and comparability are separate fields with separate producers
   (§8). No field is derived by relabelling another.
6. **Comparability is declared and evidenced, never inferred.** Matching task
   names, flag names or successful execution never imply equivalence.
7. **Fail closed, record everything.** Unknown states are errors, skips are
   attempts, failures are retained, and missing capabilities are recorded with
   reasons.
8. **Reuse before rewrite.** Mature R validators, preparation and reporting
   logic are wrapped through a bridge. Porting is justified only when the
   orchestrator itself needs the behaviour (§10).

## 4. High-level architecture

```text
                        +----------------------------------+
   lidarbench CLI  ---> |  Orchestrator (Python, stdlib +  |
   (init/preflight/     |  psutil)                         |
    run/resume/report/  |  config | registry | campaign |  |
    doctor/verify)      |  schedule | attempt store        |
                        +---+------------+-----------+-----+
                            |            |           |
              +-------------+    +-------+----+   +--+-----------------+
              | Tool adapters|   | Monitors   |   | Validators         |
              | (plugins)    |   | (plugins)  |   | (plugins)          |
              | cli/lastools |   | psutil     |   | structural (py)    |
              | rscript      |   | whole-mach |   | r-validation-v1    |
              | python-lib   |   | proc-tree  |   |  (via R bridge)    |
              | pdal (later) |   |            |   | py-geo (optional)  |
              +------+-------+   +-----+------+   +---------+----------+
                     | subprocess      | sampler process    | subprocess
                     v                 v                    v
           Rscript / lascanopy64 /   OS counters      Rscript validate.R /
           python -m <lib runner>                     python -m <validator>
                            \            |            /
                             v           v           v
          +-----------------------------------------------------------+
          | Campaign store (filesystem, write-once):                  |
          | campaign.json, identity/, schedule.csv, events.jsonl,     |
          | attempts/<id>/{started,invocation,result,validation,      |
          | monitor}.json + logs, reports/<id>/                       |
          +-----------------------------------------------------------+
                                     |
                                     v
                   Reporting: compile -> tables (CSV/JSON) ->
                   renderers (static HTML/Markdown; optional Quarto)
```

## 5. Directory structure (target)

New code lives **beside** the legacy R harness. Nothing under `R/`, `docs/`,
`graphics/`, `_OLD/` or the historical Quarto sources moves during migration
(see §10.2 for why).

```text
LidarSpeedTests/
├── AGENTS.md                     # permanent development rules
├── MIGRATION_PLAN.md             # roadmap + acceptance criteria
├── README.md                     # updated per phase; legacy sections retained
├── WORKLOADS.md                  # legacy workloads-v1 contract (R campaign identity input!)
├── pyproject.toml                # Python package metadata (package name TBD, §13)
├── design/
│   ├── ARCHITECTURE.md           # this document
│   └── adr/                      # short architecture decision records, one per decision
├── src/lidarbench/               # Python core (no R / LiDAR imports)
│   ├── __init__.py
│   ├── __main__.py
│   ├── cli.py                    # argparse entry point
│   ├── core/
│   │   ├── canonical.py          # canonical JSON + SHA-256 digests
│   │   ├── paths.py              # path normalization, containment, file components
│   │   ├── scratch.py            # run-owned scratch lifecycle
│   │   ├── locks.py              # campaign + machine execution locks
│   │   ├── store.py              # write-once files, atomic publish, receipts
│   │   ├── clock.py              # monotonic + wall timestamps
│   │   └── platform.py           # OS/hardware facts, capability probes
│   ├── config/                   # TOML loading, schema validation, defaults
│   ├── workloads/                # workload specs, implementations, equivalence registry
│   ├── datasets/                 # manifests, LAS/LAZ header reader, provenance, verification
│   ├── campaign/                 # identity, schedule, runner, resume, events
│   ├── execution/
│   │   ├── process.py            # subprocess launch, timeout, tree termination, logs
│   │   └── adapters/             # base.py, cli.py, lastools.py, rscript.py, pylib.py
│   ├── monitoring/               # base.py, psutil_backend.py, summary.py
│   ├── validation/               # status model, structural.py, plugin protocol, r_bridge.py
│   ├── reporting/                # compile.py, summarize.py, renderers/
│   └── legacy/                   # read-only importers for exported legacy campaigns
├── bridges/
│   └── r/                        # R side of the R bridge (NOT under R/, see §10.2)
│       ├── runner.R              # executes one lidR/lasR invocation from JSON
│       ├── validate.R            # wraps validation-v1 → JSON report
│       ├── identify.R            # R/package/native-library identity → JSON
│       └── export_legacy.R       # legacy RDS campaigns/receipts → JSON (read-only)
├── workloads/
│   └── workloads-v1/             # tool-neutral specs + per-tool implementations (TOML)
├── configs/examples/             # example campaign/machine configs (TOML)
├── tests/
│   ├── python/                   # pytest suite (unit, contract, integration, platform)
│   │   └── fixtures/             # tiny synthetic LAS, fake tool executables, golden files
│   └── testthat/                 # existing R suite (unchanged) + bridge tests
├── R/                            # LEGACY R harness — frozen, still runnable
├── renv.lock, renv/, DESCRIPTION # R environment (used by legacy harness and R bridge)
├── *.qmd, _quarto.yml            # legacy campaign report sources
└── docs/, graphics/, _OLD/       # historical/published — never modified
```

## 6. Core concepts and data model

All records carry `schema` (for example `"lidarbench.attempt/1"`). Every
published file has a SHA-256 digest recorded in its parent receipt.

| Concept | Definition | Identity |
|---|---|---|
| **Workload spec** | A tool-neutral description of *what* is computed: inputs, population, algorithm, parameters, units, grid, buffer, required artifacts and timing boundary. It is versioned in a suite such as `workloads-v1`. | `suite/workload_id` + spec digest |
| **Implementation** | *How* one tool performs a workload spec: adapter, argv template or call, thread policy, artifact naming, implementation status and notes. | `suite/workload_id/tool` + digest |
| **Equivalence record** | A declared relationship between implementations, for example `matched_definition` with evidence links and validation requirements. | `suite/workload_id/group` |
| **Tool** | A configured adapter instance, for example `lastools@/opt/lastools/bin`, `lidR via Rscript 4.6.1`. | adapter name + tool identity digest |
| **Dataset** | A named, manifest-described set of input files plus provenance. | manifest digest |
| **Run** | One dimension point: dataset × cores × input storage × scratch storage (as in R `benchmark_make_runs`). | `run_id` |
| **Campaign** | A frozen selection of (workload, run) pairs × tools, with a schedule, policies and identity. | `campaign_id` (user-supplied, file-component-safe) + `identity_sha256` |
| **Schedule entry** | (workload, run, tool, phase, repetition, position). | `schedule_index` |
| **Attempt** | One execution of one schedule entry. It is immutable and may be repeated (retry). | `attempt_id` (sortable timestamp + random suffix) |
| **Report build** | A reproducible view over one campaign under one report mode. | `report_id` + build receipt |

### 6.1 Campaign store layout

```text
<reports_root>/campaigns/<campaign_id>/
├── campaign.json            # schema, campaign_id, identity_sha256, created_at, tool versions summary
├── identity/                # canonical JSON identity components (each digested in campaign.json)
│   ├── code.json            # package/bridge/workload file hashes, git revision + dirty + diff digest
│   ├── config.json          # resolved config incl. frozen policies
│   ├── selection.json
│   ├── workloads.json       # resolved specs + implementations + equivalence records
│   ├── tools.json           # adapter identities (executables, versions, R pkgs, ...)
│   ├── datasets.json        # dataset manifest digests (manifests stored alongside)
│   └── environment.json     # OS, CPU, RAM, storage mounts, Python; recorded, partly non-frozen
├── schedule.csv             # frozen schedule (digest in campaign.json)
├── events.jsonl             # append-only: campaign_started, attempt_started, attempt_published, resumed, integrity_checked, ...
├── attempts/<attempt_id>/
│   ├── started.json         # entry key, sequence number, timestamps, host, pid
│   ├── invocation.json      # resolved argv/env deltas/cwd/inputs/scratch (paths as root-label + relative)
│   ├── stdout.log, stderr.log
│   ├── execution.json       # exit status, timing, interpretation, warnings, exceptions
│   ├── validation/structural.json, validation/numerical.json
│   ├── monitor/samples.csv, monitor/summary.json
│   ├── result.json          # consolidated record (references the files above + their SHA-256)
│   └── completed.json       # receipt: sha256(result.json); written last, exclusive create
│   └── abandoned.json       # tombstone written on resume for an attempt with started.json but no completed.json
└── reports/<report_id>/     # tables/, figures/, site/, build.json
```

## 7. Interfaces

Interfaces are Python abstract base classes or `typing.Protocol`s plus versioned
JSON schemas. Plugins are discovered through one entry-point group per kind:
`lidarbench.adapters`, `lidarbench.monitors` and `lidarbench.validators`. Built-in
plugins are registered the same way. Core never imports a plugin's optional dependencies
at import time.

### 7.1 Tool adapter

**Implemented** as `lidarbench.adapter/1`. The normative contract is
[ADAPTERS.md](ADAPTERS.md). The implemented names map to the sketch below:
`probe(platform)`, `identify()`, `workload_support()` / `support_for()`
(implementation status with explicit reasons and threading ranges),
`build_command(invocation) -> CommandSpec` (argv tuple, env delta, cwd, config
files), `expected_outputs(invocation) -> list[ArtifactSpec]` and
`invocation_metadata()`. The core-side `plan_invocation()` turns these into a
ready command or a `skipped_*` plan. `interpret()` and the per-attempt timeout
arrive with process execution. The original sketch is kept for context:

```python
class ToolAdapter(Protocol):
    name: str                                  # "lastools", "rscript", "cli", "pylib", ...
    def probe(self, tool_cfg) -> Availability: ...           # installed? platform supported? reason
    def identify(self, tool_cfg) -> ToolIdentity: ...        # executables + sha256, versions, packages
    def implementation_status(self, impl) -> ImplStatus: ...  # runnable / unsupported / blocked / diagnostic_only
    def build(self, impl, invocation_ctx) -> CommandSpec: ... # argv list, env delta, cwd, timeout
    def interpret(self, impl, proc: ProcessResult) -> ExecutionOutcome: ...  # status rules, log parsing
    def artifact_specs(self, impl) -> list[ArtifactSpec]: ...  # names/kinds required per input tile
```

- `CommandSpec.argv` is always a list. There is **no shell**. Placeholders
  (`{inputs}`, `{output}`, `{cores}`, `{height_cutoff}`) are substituted by
  the core from a closed set. Unknown placeholders are errors, as in
  `benchmark_lastools_command`.
- `interpret()` owns tool-specific rules. Examples are the LAStools
  `ERROR`/`FATAL` parsing and the explicitly frozen `lascanopy` cutoff-warning
  exception. It returns `succeeded | failed` with diagnostics and any
  **named** accepted exceptions.
- Built-ins:
  - `cli`: generic argv tool with configurable success rules;
  - `lastools`: a `cli` specialization with version and banner normalization, and
    the cutoff policy;
  - `rscript`: the R bridge;
  - `pylib`: runs `python -m <module>` in a configured interpreter or virtual
    environment, so benchmarked libraries never load into the orchestrator.
- An adapter that cannot run on the current OS returns `Availability(False,
  reason)`. The runner then records a skipped attempt with that reason. It must
  never fail the whole campaign.

### 7.2 R bridge protocol (`rscript` adapter)

```text
Rscript --vanilla bridges/r/runner.R <invocation.json> <result.json>
```

- `invocation.json` contains the workload implementation (task call reused from
  `R/0_setup.R`), input file list, output directory, cores, thread policy, CRS
  declaration and policy flags.
- `runner.R` activates the project `renv` and applies the existing thread policy
  (`lidR` multisession × 1 thread, `lasR concurrent_files`, reference BLAS
  check). It builds the catalog with `prepare_input()` semantics. It times
  **only the workload call** with a high-resolution clock (`Sys.time()` with
  sub-second precision, recorded as integer nanoseconds since epoch, plus
  `proc.time()` elapsed). It writes `result.json` atomically.
- The orchestrator records **both** the outer process time (perf_counter_ns
  around the subprocess) and the inner workload time. The timing-boundary label
  says which one a report uses (§8.4).
- Bridge code lives in `bridges/r/` and **sources** functions it reuses from
  `R/1_functions.R`. It does not copy them. Early phases keep it this way to
  avoid two diverging implementations. Later phases may extract a shared R
  module after approval.

### 7.3 Validator

```python
class Validator(Protocol):
    name: str                         # "structural-v1", "r-validation-v1", ...
    level: Literal["structural", "numerical"]
    def applicable(self, impl, policy) -> bool: ...
    def validate(self, ctx: ValidationContext) -> ValidationReport: ...
```

- `structural-v1` (Python, stdlib only) checks:
  - exact per-tile primary-artifact inventory from `ArtifactSpec` (missing,
    unexpected and duplicate artifacts, halo exports);
  - nonempty files;
  - naming by input tile stem;
  - optional format-sniffing (magic bytes) for GeoTIFF, LAS/LAZ and GPKG.
- `r-validation-v1` (R bridge) runs `bridges/r/validate.R`, which calls the
  existing `benchmark_validate_artifacts()` in either `numerical` or
  `performance-only` mode. The JSON output keeps the existing fields: `status`,
  `structural_checks_passed`, per-artifact detail, `boundary_difference_points`,
  extent deltas and exception reasons.
- Future optional validators (for example `py-geo` using rasterio/laspy) live in
  optional extras. The core never requires them. A Python reimplementation of
  numerical references is acceptable only after it passes parity tests against
  `r-validation-v1` on the shared fixtures.

### 7.4 Monitor backend

```python
class MonitorBackend(Protocol):
    name: str                          # "psutil"
    scopes: frozenset[Scope]           # whole-machine, process-tree
    def probe(self) -> tuple[bool, str]: ...
    def start(self, monitor_dir, attempt_id) -> MonitorHandle: ...

class MonitorHandle(Protocol):
    def attach(self, pid, launch: Instant) -> None: ...   # never raises
    def stop(self) -> MonitorReport: ...                   # never raises
```

- It runs as a separate sampler **process** (`python -m lidarbench.monitoring.sampler`)
  with a JSON-lines handshake: `ready` on its stdout, `attach`/`stop` on its
  stdin (end of stdin also stops it, so it cannot outlive the orchestrator).
  This replaces the R ready/stop files: it needs no polling and adds no file
  latency to the attach inside the timed interval. Each scope is written to its
  own CSV (`machine-samples.csv`, `process-tree-samples.csv`) with explicit
  units and interval start/end in monotonic and wall time. Details and
  limitations: [EXECUTION.md](EXECUTION.md).
- Summary logic ports `benchmark_monitor_summary()`: time-weighted means,
  peaks, coverage fraction and `available`/`partial`.
- Scopes:
  - `whole-machine`: CPU % and RAM used/available on all three OSes;
  - `process-tree`: sum of CPU time and RSS over the child tree via
    `psutil.Process.children(recursive=True)`, now supported.
- Per-metric availability is recorded. For example, per-process I/O counters
  are unavailable on macOS, and that is recorded as `null` + reason, never 0.

### 7.5 Dataset manifest

```json
{
  "schema": "lidarbench.dataset/1",
  "dataset_id": "petawawa-fresh-v4/tile_1000_density_1",
  "root_label": "SSD",
  "relative_dir": "prepared/1_org/tile_1000_density_1",
  "files": [{"path": "298000_5093000.laz", "bytes": 0, "sha256": "...", "md5": "...",
             "las": {"version": "1.4", "point_format": 6, "points": 0,
                     "bounds": [0, 0, 0, 0, 0, 0], "scale": [0, 0, 0], "crs": "unknown|epsg:2959"},
             "companions": [{"path": "298000_5093000.lax", "sha256": "..."}]}],
  "declared_metadata": {"horizontal_epsg": 2959, "vertical_epsg": 5713, "xyz_units": "metre",
                        "confirmation": "..."},
  "provenance": {"source": "...", "archive_sha256": "...", "preparation_policy": "fresh-petawawa-v4",
                 "preparation_code": {"revision": "...", "files": {}}, "seed": 20261022,
                 "receipts": [{"path": "...", "sha256": "..."}], "readiness": {"path": "ready.rds", "sha256": "..."}},
  "license": "..."
}
```

- The LAS/LAZ public header is uncompressed in both formats. A small
  **stdlib-only** header reader in `datasets/lasheader.py` supplies counts,
  bounds, version and scale. Examples where this matters are the LAStools
  `{height_cutoff}` (minimum Z) and validation manifests. The core still needs
  no LiDAR library.
- MD5 is retained beside SHA-256 so existing R preparation receipts can be
  cross-checked.

### 7.6 Configuration (TOML)

There are two files. `tomllib` is in the standard library from Python 3.11.

- **Machine config** (not committed): input/scratch roots with labels, reports
  root, tool installations (LAStools bin, Rscript path, Python interpreters),
  workstation id, monitor settings.
- **Campaign config** (committable): selected workloads, tools, datasets, cores,
  storage pairs, scheduling, `validation_mode`, frozen opt-in policies
  (`accept_known_extent_discrepancies`, `accept_known_surface_discrepancies`,
  `accept_known_lastools_cutoff_warning`, `diagnostic_lasR`), timeout, scratch
  retention.

Unknown keys are errors. Every resolved value, including defaults, is written to
`identity/config.json`.

## 8. Status model and comparability

### 8.1 Four independent verdicts per attempt

| Field | Values | Producer |
|---|---|---|
| `execution_status` | `succeeded`, `failed`, `timed_out`, `interrupted`, `skipped_unsupported`, `skipped_blocked`, `skipped_unavailable` | adapter `interpret()` + runner |
| `structural_status` | `passed`, `failed`, `not_run` | structural validator(s) |
| `numerical_status` | `passed`, `accepted_exception`, `failed`, `unverified`, `not_applicable`, `not_run` | numerical validator per frozen `validation_mode`; `performance-only` ⇒ `unverified` |
| `comparability` | computed at report time: `comparable`, `descriptive_only`, `diagnostic_only`, `excluded` (+ reason) | reporting, from contract + equivalence registry + verdicts |

Mapping from the legacy R fields:

| Legacy R field/value | 2.0 field/value |
|---|---|
| `execution_status` | `execution_status` |
| `structural_validation_passed` / `structural_checks_passed` | `structural_status` |
| `validation_status` (`passed`, `accepted_exception`, `failed`, `unverified`, `not_run`) | `numerical_status` |
| `comparison_status` | contract-level `implementation_status` + `equivalence` |
| `n_comparable` | count of `comparability == comparable` |

### 8.2 Contract vocabulary

`implementation_status` per (workload, tool):

- `runnable`;
- `unsupported` (no call; recorded skip);
- `blocked` (candidate retained, never dispatched; reason required);
- `diagnostic_only` (explicit frozen opt-in; can never become `comparable`).

`equivalence` per workload across implementations:

- `matched_definition` (the old `matched_definition_pending_validation`);
- `non_comparable`.

The workloads-v1 matrix (17/6/4 strict, 20/6/1 with `diagnostic_lasR`) is
migrated **as data**. A parity test compares it with an R export of
`benchmark_contracts`.

### 8.3 Comparability gate (enforced in code, tested)

A timing may enter a cross-tool comparison or speedup **only if all** of these
are true:

1. Same workload suite and spec digest. Both implementations belong to one
   `matched_definition` group.
2. `execution_status == succeeded` and `numerical_status == passed`. The
   following are excluded: `accepted_exception`, `unverified`, `diagnostic_only`.
3. `boundary_difference_points == 0` (or the spec-defined equivalent).
4. Same dataset manifest digest, run dimensions (cores, storage pair) and
   campaign identity (same machine).
5. Same timing-boundary kind (§8.4).
6. The report mode is `validated`.

All other included timings are **descriptive only**. They appear in separate
tables, which are labelled. The reporting API offers no function that computes
ratios or speedups across a failed gate; attempting one raises an error. This
removes the possibility of presenting non-equivalent workloads as directly
comparable.

### 8.4 Timing

- Durations use `time.perf_counter_ns()` (monotonic). Wall timestamps use
  `time.time_ns()`. Both are stored as integers, with no second-resolution
  CSV timestamps (fixes L5).
- `timing_boundary` values:
  - `process`: outer subprocess time, including interpreter or package load;
  - `workload`: adapter-reported inner time, as in the legacy R in-process
    boundary.
- For native CLIs the two are equal by definition. Reports state the boundary,
  and comparisons never mix boundaries.
- Exclusions as in R: warm-ups, superseded retries, failures, sub-0.1 s timings
  (flagged), and validation ineligibility under the report mode.

## 9. Key mechanisms

### 9.1 Campaign identity

- **Frozen identity** = canonical JSON (sorted keys, UTF-8, `\n`, no floats
  without explicit representation) of:
  - code: SHA-256 of Python package files, `bridges/`, `workloads/`, and the
    legacy R files sourced by the bridge;
  - resolved config and policies;
  - selection;
  - schedule digest;
  - resolved workloads;
  - tool identities;
  - dataset manifest digests;
  - stable environment facts: OS family and version, CPU model, logical/physical
    cores, total RAM, storage mounts of configured roots.

  `identity_sha256` is computed over the component digests.
- **Provenance-only** (recorded, not compared on resume): git revision, branch,
  dirty flag, diff digest, hostname, boot time, free RAM, Python patch
  details.
- `run` refuses a dirty tree for frozen code paths unless `--allow-dirty` is
  given. That flag records the diff digest and marks the campaign `dirty: true`.
  It is never silent.
- **Resume** recomputes the full identity once. Mismatch ⇒ refuse with a
  component-level diff, as in `benchmark_campaign_check`.
- **Per-attempt** checks are cheap:
  - code file digests (small);
  - stat fingerprints (`size`, `mtime_ns`, `st_ino`/file index) of the attempt's
    inputs, against the frozen manifest snapshot.
- Full input rehash happens at start, at resume and at campaign end
  (`integrity_checked` event), and optionally every *N* entries. A mismatch
  stops the campaign and marks affected attempts. This fixes L3 without
  weakening the guarantee that inputs are unchanged.

### 9.2 Immutable attempts

- `started.json` is created with exclusive create (`open(..., "x")`), the
  directory first (`mkdir` fails if it exists). Attempt IDs are 26-character
  sortable IDs (time + `secrets` randomness).
- Files are written to `*.pending` then published with `os.replace` (atomic on
  POSIX and NTFS within a volume). `completed.json` is written last with
  exclusive create, after `fsync` of the file and, on POSIX, the directory.
- After completion, files are set read-only (`chmod 0o444` / Windows read-only
  attribute). This is defence-in-depth, not a security boundary.
- Readers verify every digest chain. A mismatch is an integrity error that
  blocks compilation, as in R.
- Supersession is ordered by `events.jsonl` sequence number. The R
  `started.rds` timestamp is used only for imported legacy data.
- Interrupted attempts have `started.json` without `completed.json`. On resume
  the runner writes an `abandoned.json` tombstone (exclusive create, reason,
  detected_at) and **never** edits or deletes the attempt. Reports count
  `interrupted` attempts explicitly (fixes L10).

### 9.3 Scheduling (`schedule-v2`)

- Same structure as R:
  - warm-up phase then measurement phase;
  - per (workload, run), a seeded base permutation of tools, rotated by
    repetition (Latin-square balance of tool position);
  - selection order shuffled per repetition;
  - configurable cooldown;
  - `warmups ≥ 0`, `repetitions ≥ 1`.
- RNG: an explicitly specified **SplitMix64 + Fisher–Yates** implementation in
  `campaign/schedule.py`. It is stable across Python versions and OSes, which
  `random` does not guarantee for every API. R schedules cannot be reproduced
  bit-for-bit. Legacy schedules remain in their manifests and are never
  regenerated.
- Golden schedule files plus property tests check tool-position balance,
  completeness and determinism.
- Execution is strictly **sequential**: one attempt at a time per machine,
  enforced by the machine lock (§9.6). Parallelism exists only inside a tool
  (`cores`).
- Resume: an entry is complete if a published attempt has
  `execution_status ∈ {succeeded, skipped_*}` and is acceptable under the frozen
  `validation_mode` (same rule as `benchmark_campaign_completed`). Warm-ups
  rerun on resume. Failed entries are retried only with `--retry-failed`.

### 9.4 Resource monitoring

- `psutil` is the only non-stdlib core dependency. It ships wheels for all
  three OSes.
- The sampler process is started before and stopped after the timed call,
  using the ready/attach/stop handshake and a timeout kill. Samples are bound to
  `attempt_id`, with checksums in `monitor/summary.json`.
- Attempt fields: `monitoring_status` (`disabled`, `available`, `partial`,
  `unavailable` + reason), `scope`, `platform` and coverage.
- The R logic for coverage and weighting is ported, and tests use the same
  synthetic sample tables.
- Overhead is recorded as part of the sampler's interval metadata. Whole-machine
  utilization is always labelled as including unrelated activity.

### 9.5 Safe scratch handling

The R guards are ported and extended:

- **Root validation.** Each root must:
  - exist and be writable;
  - not be a filesystem/drive/UNC share root, the home directory, the
    repository or the working directory;
  - not overlap any protected path (input roots, dataset directories, reports
    root, monitoring paths, repository, `docs/`).

  Overlap is computed on `os.path.realpath`, with a case-folding comparison
  decided **per filesystem** by probing case sensitivity (fixes L6 on macOS),
  plus `os.path.samefile` where both exist.
- **Creation.** `tempfile.mkdtemp(prefix="lidarbench-run-", dir=root)` (mode
  `0700` on POSIX). An ownership marker `.lidarbench-scratch.json` is written
  with exclusive create. It records `campaign_id`, `attempt_id`, a random token,
  creation time, host and pid. `(st_dev, st_ino)` (Windows: volume serial + file
  index) is recorded in the attempt.
- **Cleanup.** All of these must hold:
  - the in-memory owner token, the marker token and the recorded device/inode
    all match;
  - the parent is the validated root;
  - the name prefix is correct;
  - a walk with `followlinks=False` finds no symlink, junction
    (`os.path.isjunction`, Python ≥ 3.12, with a fallback via
    `FILE_ATTRIBUTE_REPARSE_POINT`) or mount point inside.

  Deletion uses `shutil.rmtree` with an `onexc`/`onerror` handler that clears
  the Windows read-only attribute only for paths inside the tree. Long paths use
  the `\\?\` prefix on Windows.
- **Orphans** (after a crash) are listed by `lidarbench scratch list`. They are
  deleted only by `lidarbench scratch clean --campaign <id> --confirm`, and only
  when the marker proves ownership (fixes L7). Nothing is ever deleted outside
  a marker-proven directory.
- **Retention.** Default is delete after validation, as in R. Optionally
  `retain_scratch = "on-failure"` with a size cap, for debugging. Retention is
  part of the frozen config.
- Adapters that may write next to inputs (for example automatic `.lax`
  creation) declare it. Preflight refuses such configurations unless the
  companions already exist, as the R lasR guard does. Input fingerprints before
  and after each attempt detect violations.

### 9.6 Locks

- **Campaign lock**: `<campaign>/.lock` (exclusive create) containing host, pid,
  process start time and command.
- **Machine lock**: in the user's runtime/cache directory
  (`platformdirs`-equivalent logic in stdlib). It prevents two benchmark
  campaigns on one machine regardless of reports root (fixes L9).
- Stale detection compares pid **and** process creation time (psutil). Stale
  locks are reported, never auto-removed. Removal requires `lidarbench lock
  release --confirm`.

### 9.7 Process execution

- `subprocess.Popen` with an argv list and `shell=False`. Executables are
  resolved to absolute paths at preflight (`shutil.which`, honouring `PATHEXT`
  on Windows) and their SHA-256 is frozen in identity.
- Process groups: POSIX uses `start_new_session=True` and kills with `killpg`.
  Windows uses `CREATE_NEW_PROCESS_GROUP` and assigns the child to a **Job
  Object** (ctypes, stdlib-only) so that the entire tree is terminated on
  timeout or interrupt. psutil tree-kill is the fallback. LAStools `-cores`
  children and R `future` workers are therefore cleaned up.
- stdout/stderr are streamed to files as raw bytes. Parsing decodes UTF-8 with
  `errors="replace"`. Logs are never held only in memory.
- Environment: the child env is the orchestrator env plus an explicit delta
  (thread caps), recorded in `invocation.json`. The orchestrator's own
  environment is never mutated (unlike `Sys.setenv` in R).
- Timeouts: configurable per workload. Expiry ⇒ tree kill ⇒ `timed_out`.
- Ctrl+C: the runner terminates the child tree, writes the attempt as
  `interrupted` (if it can) and releases locks. The next resume tombstones any
  remaining partial attempt.
- Implemented in `execution/process.py`, `execution/process_tree.py` and
  `execution/attempt.py`. The Windows child is created suspended and resumed
  only after it is in the Job Object. The timed interval ends when the OS
  reports the exit (reaper thread), not at a poll. See
  [EXECUTION.md](EXECUTION.md) for the status rules, phase boundaries and
  known limitations of process-tree monitoring.

## 10. Disposition of existing components

### 10.1 Component decisions

| Component | Decision | Rationale |
|---|---|---|
| Workload calls in `R/0_setup.R` (lidR/lasR R calls) | **Reuse** via the R bridge | Mature, pinned, validated on fixtures. Re-expressing them as data references (the call text) keeps one source of truth. |
| LAStools argv templates (`R/0_setup.R`) | **Migrate to data** (`workloads/workloads-v1/*.toml`) | Pure data. The Python `lastools` adapter needs it. A parity test compares it with an R export. |
| `benchmark_workload_contract()` matrix and notes | **Migrate to data** + parity test | It must be tool-neutral and extensible. Statuses and notes are kept verbatim. |
| `benchmark_artifact_specs()` | **Migrate to data** (`ArtifactSpec`) | Structural validation in Python needs it. The numerical part stays in R. |
| `benchmark_validate_artifacts()` and reference helpers (validation-v1) | **Reuse** via `r-validation-v1` | Most complex, best-tested and policy-laden code. Rewriting it risks silent semantic drift. |
| `performance-only` structural checks (R) | **Reuse** via the bridge now. A Python `structural-v1` covers inventory only. | Raster/LAS metadata checks need geospatial libraries, which the core must not require. |
| Config validation, path and scratch guards, file components | **Rewrite in Python (port semantics + tests)** | The orchestrator must own them. They are extended for macOS case-insensitivity, junctions, markers and long paths. |
| Campaign identity, attempts, resume, schedule | **Rewrite in Python (port semantics)** | Core orchestration. Needs language-neutral formats, content identity (L3) and portable RNG. |
| LAStools execution, log interpretation, cutoff exception, version normalization | **Rewrite in Python (port semantics + tests)** | Adapter logic. Ported regexes and the three-line warning rule are tested against the same fixtures. |
| `benchmark_height_cutoff()` | **Port** (uses the stdlib LAS header reader) | Small and deterministic. Golden values from R are used in tests. |
| Thread policy for R tools, reference-BLAS check | **Reuse** inside `bridges/r/runner.R` | R-specific. |
| Linux `/proc` monitor | **Replace** with the psutil backend. Summary math is **ported**. | Cross-platform requirement. |
| `cpu_ram_usage.ps1` | **Retain as legacy** (unused) | Historical. Superseded by psutil. |
| `R/prepare_fresh_data.R`, `R/2_data.R` (Petawawa preparation) | **Retain + wrap** (`dataset prepare` invokes the R CLI). An R exporter emits a JSON dataset manifest from existing receipts. | Mature, heavily tested and provenance-critical. It produced the current verified data. Rewriting it would invalidate the provenance chain. |
| `R/6_pilot.R`, `R/3_runBenchmarks.R` | **Retain as legacy runners** until parity is proven | In active use (a full campaign was running during this review). |
| `R/reporting.R`, `R/5_report.R`, `*.qmd` | **Retain for legacy campaigns.** **Port** the selection/exclusion/summary rules to Python reporting. | Legacy reports must remain reproducible. New reports need language-neutral inputs. |
| `R/4_compileResults.R` legacy CSV mode | **Retain as legacy** | Historical CSVs only. |
| `R/check_environment.R` | **Reuse** from `lidarbench doctor` for the R adapter | |
| `renv.lock`, `.R-version`, `DESCRIPTION` | **Retain**. They pin the R bridge environment. | R-side reproducibility. Changes require a new campaign. |
| testthat suite | **Retain and keep green**. Add bridge tests. Port the semantics of orchestration tests to pytest. | |
| `docs/`, `gh-pages`, `graphics/`, `_OLD/`, historical results | **Protected — never modified** | Published or historical record. |
| `ALS_tiles.gpkg`, `input-manifests/` | **Protected inputs** (read-only) | Preparation identity depends on them. |
| `WORKLOADS.md` | **Retain verbatim** while legacy R campaigns may resume. 2.0 workload docs live in `workloads/` and `design/`. | It is hashed into legacy R campaign identity. |

### 10.2 Why legacy R files do not move

The R identity (`benchmark_campaign_identity`) hashes `R/*.R`, `DESCRIPTION`,
`WORKLOADS.md` and `renv.lock`, and records `git rev-parse HEAD`. The legacy
report builder copies `R/reporting.R` and the `*.qmd` files from the repository
root. Moving or editing these files would block resuming or re-reporting
existing campaigns. Relocation into `legacy/` is deferred to a final phase and
needs explicit approval after all legacy campaigns are complete.

## 11. Operational lesson recorded during branch creation

The full campaign `simple-metrics-002` was running from this checkout
(`/home/ptompals/LidarSpeedTests`) while the branch was created. Committing the
previously uncommitted `dev` work changed `HEAD`. The live identity recheck
(L3) would then have aborted the run at its next entry. HEAD of that checkout
was restored, detached at `0494669` with identical working-tree content, so the
frozen identity matches again. Branch work proceeds in a separate git worktree.
The 2.0 design avoids the problem in three ways:

- content-based identity (§9.1);
- running campaigns from an installed, immutable code snapshot;
- `AGENTS.md` guidance: never change HEAD or tracked identity files in a
  checkout that an active campaign runs from.

## 12. Cross-platform strategy

| Concern | Linux | Windows | macOS | Approach |
|---|---|---|---|---|
| Python | 3.11+ | 3.11+ (python.org / py launcher) | 3.11+ | `tomllib`; tested on 3.11–3.13 |
| Paths | case-sensitive | case-insensitive, `\`, drive/UNC roots, 260-char limit, reserved names (`CON`, `NUL`, …) | usually case-insensitive APFS | `pathlib`. Records store `root_label` + POSIX relative path. Case sensitivity is probed per root. Reserved names and `<>:"/\|?*` plus control characters are rejected in file components. `\\?\` prefix for long paths. |
| Links | symlinks | symlinks, junctions, reparse points | symlinks, firmlinks | Refuse any link inside scratch. Resolve roots with `realpath` before checks. |
| Executables | exec bit | `.exe` via `PATHEXT` | exec bit, Gatekeeper quarantine (reported) | Resolve to absolute paths at preflight and hash them. |
| Process tree | `killpg` | Job Object + `CTRL_BREAK_EVENT` | `killpg` | Abstracted in `execution/process.py` |
| Monitoring | psutil | psutil | psutil (no per-process I/O) | Per-metric availability recorded |
| Atomic publish | `os.replace` + dir `fsync` | `os.replace` (retry on sharing violations from AV/indexers, bounded) | `os.replace` + `F_FULLFSYNC` optional | `core/store.py` |
| Text | UTF-8 | UTF-8 forced (`encoding=` everywhere, `newline=""` for CSV) | UTF-8 | Linted. No locale-dependent I/O. |
| R | `Rscript` on PATH or configured | `Rscript.exe` from config/registry | `Rscript` | R is optional. Adapters report unavailability. |
| LAStools | vendor Linux binaries (`*64`) | native `.exe` | depends on vendor support | Availability probe. Never assumed. |
| CI | ubuntu-latest | windows-latest | macos-latest | GitHub Actions matrix with fake tools (no LiDAR tools, no R required for core jobs). An optional R job runs bridge tests. |

## 13. Main technical decisions

| ID | Decision | Alternatives considered | Status |
|---|---|---|---|
| D1 | Python ≥ 3.11 orchestrator; core dependencies: stdlib + `psutil` only | pydantic/attrs (heavier), numpy (unneeded) | proposed |
| D2 | Every benchmarked workload runs in a child process | in-process R/Python (as legacy R) | proposed |
| D3 | JSON/JSONL/CSV records with SHA-256; canonical JSON for identity | RDS, Parquet (needs pyarrow), SQLite (single-writer but less transparent) | proposed |
| D4 | Content-addressed identity; git revision recorded but not live-checked | live HEAD check (legacy) | proposed |
| D5 | Workloads, implementations and equivalence as versioned TOML data | Python classes per workload | proposed |
| D6 | Reuse R validation-v1 and R preparation via an R bridge | full Python port now | proposed |
| D7 | `schedule-v2` SplitMix64 + Fisher–Yates; same balance structure as R | `random.Random`, numpy RNG | proposed |
| D8 | Four separate verdict fields + report-time comparability gate enforced in code | single status column | proposed |
| D9 | Plugin discovery through entry points; built-ins registered identically | hard-coded tool list (legacy) | proposed |
| D10 | Legacy R harness frozen in place; relocation only after approval | immediate move to `legacy/` | proposed |
| D11 | Package name `lidarbench`, CLI `lidarbench` | `lidarspeedtests`, `lst` | **open — needs owner decision** |
| D12 | Environment tooling: `pyproject.toml` (PEP 621) + committed `uv.lock`; uv for development (`uv sync`, `uv run`), dev tools in the PEP 735 `dev` dependency group; plain `pip install -e .` must work | Poetry, conda | **accepted** (2026-10-28) |
| D13 | Reports: Python compiles canonical tables; built-in static HTML/Markdown renderer; Quarto rendering optional | Quarto-only (requires R/Jupyter) | proposed |

## 14. Security and safety notes

- Configs are data (TOML). They are never executed. This contrasts with legacy
  `dget`, which evaluates R expressions. Workload R call text is evaluated only
  inside the R bridge, comes from the versioned `workloads/` tree, and is frozen
  in identity.
- No network access in core. Dataset downloads stay in the explicit R
  preparation CLI or a future explicit `dataset fetch` command that requires a
  flag and a size confirmation.
- No elevated privileges. No cache dropping, pinning or power changes.

## 15. Open questions for review

1. Package/CLI name (D11).
2. Default `timing_boundary` for headline tables: `process` (uniform across all
   tools) or `workload` (closest to legacy R timings)? The proposal reports both
   and uses `process` for cross-tool tables.
3. Should a Python numerical validator (rasterio/laspy) be planned for parity
   with `r-validation-v1`, or should R remain the numerical authority
   indefinitely?
4. Which additional tools should be included in the first extensibility
   milestone (for example PDAL CLI, a Python library such as `laspy`/`pdal`
   bindings)? No installation is performed without authorization.
5. Whether to relocate the legacy R harness once all legacy campaigns are
   complete (§10.2).
