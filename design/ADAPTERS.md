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
runner: write config files, launch argv (shell=False), time, monitor, validate
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
| `worker_processes` | the tool starts worker processes | lidR `future` multisession |
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

There is no shell. Paths with spaces, quotes, `;`, `$`, `%` or `*` reach the tool
unchanged, without quoting. Never build a command string.

`base.argv_value()` converts values: paths with `os.fspath`, floats with the
shortest round-trip `repr`. Booleans are rejected because their spelling is
tool-specific. `base.thread_cap_env(n)` gives the standard BLAS/OpenMP thread caps.

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
3. Reinstall (`pip install -e .`) and run `lidarbench check` to confirm
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

## 8. Not yet implemented

- Real adapters (`cli`, `lastools`, `rscript`, `pylib`, `pdal`). The modules exist
  as placeholders and are not registered.
- `interpret(ProcessResult)` (tool-specific success rules such as LAStools
  `ERROR`/`FATAL` log parsing) arrives with process execution (`execution/process.py`).
- Per-tool configuration (installation paths from the machine config) arrives with
  configuration loading. Adapter constructors currently take optional overrides only.
- Workload specs and equivalence data (`workloads/workloads-v1/*.toml`); see Phase 4.
