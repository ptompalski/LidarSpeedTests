# Campaign management (`lidarbench run`)

Status: implemented (Phase 5, Task 5). Code:
[`campaign/`](../src/lidarbench/campaign/), [`config/campaign.py`](../src/lidarbench/config/campaign.py),
[`core/store.py`](../src/lidarbench/core/store.py), [`core/locks.py`](../src/lidarbench/core/locks.py),
[`core/scratch.py`](../src/lidarbench/core/scratch.py), [`core/canonical.py`](../src/lidarbench/core/canonical.py).
Example: [`configs/examples/synthetic-campaign.toml`](../configs/examples/synthetic-campaign.toml).

This is a Python reimplementation of the legacy R campaign behaviour
(`benchmark_campaign`, `benchmark_campaign_identity`, `benchmark_schedule`,
`benchmark_execute_schedule`, `benchmark_run`, `benchmark_attempt_start/finish`,
`benchmark_read_attempt`, `benchmark_campaign_completed`, scratch guards). It
keeps the scientific and reproducibility guarantees and changes them only
where §8 says so. The R harness is unchanged.

## 1. Commands

```sh
lidarbench run CONFIG.toml --dry-run          # schedule, resume decisions, planned argv; writes nothing
lidarbench run CONFIG.toml                    # create (or resume) and execute
lidarbench resume CONFIG.toml                 # resume only; never creates a campaign
lidarbench run CONFIG.toml --tool synthetic --cpus 4 --phase measurement --limit 10
lidarbench run CONFIG.toml --retry-failed     # new attempts for failed/timed-out entries
lidarbench export WORKSPACE/campaigns/ID --format csv -o results.csv   # jsonl | csv | parquet
lidarbench report WORKSPACE/campaigns/ID      # read-only report: tables + Quarto site (REPORTING.md)
lidarbench lock status [WORKSPACE]            # workspace lock, or this user's machine lock
lidarbench lock release --confirm [WORKSPACE] # only if the owner process is gone
lidarbench scratch CONFIG.toml [--clean --confirm]   # orphaned run-owned scratch
```

Selection options (`--workload`, `--dataset`, `--tool`, `--cpus`, `--phase`,
`--limit`) restrict which **frozen** schedule rows run in this invocation
(`--limit N` counts measurement rows; their warm-ups run as well). They
never change identity or the schedule; unselected rows stay pending.

Exit codes: `0` run ended normally (attempt failures are data, not errors),
`1` configuration/identity/integrity error, `2` usage, `3` lock held,
`4` frozen material or inputs changed during the run, `130` interrupted.

## 2. Configuration (`lidarbench.campaign-config/1`, TOML)

Datasets, workloads (with parameters, datasets and timeouts), tools (adapter +
constructor options), CPU counts, repetitions, warm-ups, seed, tool order,
cooldown, cache policy, CPU pinning (`scheduling.cpu_affinity = "pinned"` (default)
or `"none"`, EXECUTION.md §7), monitoring, scratch roots/retention, validation
mode, validation settings (`[validation] suite` (default: the checkout's
`workloads/workloads-v4`), `accept_known_extent_discrepancies` (default false),
`reuse_identical_outputs` (default true), `comparability` (`strict` default or
`edge-tolerant`, ARCHITECTURE.md §8.3); VALIDATION.md §8) and integrity
options. Unknown keys and wrong types are errors with the key named.
Every default is resolved and frozen (`identity/config.json`). Configs are data;
nothing is evaluated.

A dataset is either a directory (`path`, optional `include` patterns) or a
registered manifest (`manifest = "<dataset id>"`, optional `manifest_sha256`
pin, with an optional `[data]` table for the registry and data roots). A
manifest dataset is resolved through the machine's data roots, its inputs are
the manifest's `input` files, and the campaign refuses bytes that differ from
the manifest. See [DATASETS.md §8](DATASETS.md).

The schedule is the cross product of workloads × their datasets × CPU counts
(a *unit*) × tools. A tool that cannot run a unit (unavailable, unsupported,
blocked, thread count out of range) still gets schedule rows; they are recorded
as `skipped_*` attempts with the adapter's reason, as in R.

`cache_policy` accepts only `uncontrolled-warm`. Every result carries the
policy and its statement: caches are not controlled or dropped, so timings
describe warm-cache behaviour. Any cold-cache policy is rejected.

## 3. Identity and when it is verified

| Component | Contents |
|---|---|
| `code` | lidarbench version, psutil version, SHA-256 of every package `.py` file |
| `config` | fully resolved configuration (including absolute paths and defaults) |
| `workloads` | selected workloads, parameters, effective timeouts, the workload contract (suite, key, digest; `null` without a contract), each tool's declared status/reason/threading range |
| `tools` | adapter class and module SHA-256, options, availability and reason, version, details, every executable's path/size/SHA-256 |
| `datasets` | every file under each dataset directory (relative path, size, SHA-256) and the ordered input list; for manifest datasets also the manifest reference (ID, version, kind, location, `content_sha256`, `manifest_sha256`), and the files must equal the manifest |
| `environment` | OS, release, architecture, CPU model, logical/physical CPUs, total RAM, Python implementation/version |
| `schedule` | frozen schedule, RNG description, seed, order mode, counts |

`identity_sha256 = sha256(canonical({component: sha256(canonical(component))}))`.
Canonical JSON: sorted keys, compact, UTF-8, no NaN/Inf, explicit types only.

Recorded but **not** compared (`provenance.json`): git revision, branch, dirty
flag, diff digest, host name, boot time, PID, interpreter path, free memory.
Creating a campaign from a dirty checkout requires `--allow-dirty` (recorded).
Content hashes, not git state, decide identity, so commits or branch switches
in the checkout do not abort a run (fixes R limitation L3).

| When | What is checked | Cost |
|---|---|---|
| campaign creation | full identity, **full SHA-256 of every input file** | once |
| start of every later `run`/`resume` | full identity recomputed (inputs rehashed in full) and compared per component; any difference refuses the run with a component diff and an `identity_mismatch` event | once per invocation |
| before every schedule entry (untimed) | SHA-256 of the frozen campaign records; SHA-256 of the lidarbench source files (small); stat fingerprints (size, mtime, device, inode/file index) of the entry's tool executables and dataset files; added/removed dataset files | milliseconds |
| after every attempt (untimed) | stat fingerprints of the dataset files and the list of files in each dataset directory (a tool writing into its inputs, e.g. a `.lax` index, stops the campaign) | milliseconds |
| end of a run (`verify_inputs_at_end`, default on) | full rehash of all inputs (`integrity_checked` event) | once per invocation |

Inputs are never hashed inside a timed attempt or before every entry. A file
changed in place while keeping size and mtime is only detected by the next full
rehash. Each run writes `verifications/<run_id>.json` (what was hashed, how
many bytes, which per-entry checks apply).

Changing definitions, tools, inputs or the environment therefore requires a new
`campaign_id`; the existing campaign is never modified to match.

## 4. Scheduling (`lidarbench.schedule/2`)

Same structure as R `benchmark_schedule`: all warm-ups, then measurements; the
unit order is reshuffled for every repetition.

- `balanced` (default): each unit draws one base permutation of the tools per
  phase, rotated by one position per repetition, so each tool occupies each
  position equally often (difference at most one).
- `randomized`: an independent permutation per unit and repetition.

The generator is SplitMix64 with rejection-sampled bounded integers and
Fisher–Yates shuffles, which gives the same schedule on every OS and Python
version (golden tests). R schedules cannot be reproduced bit-for-bit.

Execution is strictly sequential (one attempt at a time on the machine,
enforced by the machine lock); a tool uses only its own internal parallelism
(`threads = cpus`, with BLAS/OpenMP thread caps from the adapter), and with
`cpu_affinity = "pinned"` the whole tree is restricted to `cpus` logical CPUs,
one per physical core. After each launched attempt (and its untimed validation)
the runner pauses `cooldown_s` (interruptible).

On an interactive terminal (stderr is a TTY, `TERM` is not `dumb`; not with
`--dry-run` or `--json`) `lidarbench run` draws a live line while each attempt's
process runs ([`campaign/progress.py`](../src/lidarbench/campaign/progress.py)):
position, tool, workload, CPUs, a bar of the attempt's required expected outputs
that exist (one raster per tile for the tile workloads), elapsed time, ETA and
timeout. A daemon thread `stat`s only the declared output paths about once per
second (no reads, nothing recorded). It runs in the runner process, outside the
attempt's process tree. The count can run ahead of finished tiles for tools
that create a raster when they start a tile. Redirected output keeps the plain
log lines.

## 5. Store layout and records

```text
<workspace>/.lidarbench.lock
<workspace>/campaigns/<campaign_id>/
  campaign.json                 lidarbench.campaign/1 (receipt; written last; read-only)
  identity/<component>.json     frozen components (read-only)
  schedule.json, schedule.csv   frozen schedule
  provenance.json               not compared
  events.jsonl                  append-only, fsynced, sequence-numbered
  verifications/<run_id>.json   one per run invocation
  attempts/<attempt_id>/        26-char sortable IDs (48-bit ms time + 80 random bits)
    started.json                lidarbench.attempt-started/1
    invocation.json             resolved argv, env delta, cwd, inputs, scratch, expected outputs
    execution.json stdout.log stderr.log monitor/   (execution engine, EXECUTION.md)
    validation/report.json      lidarbench.validation-report/1 (succeeded attempts with a contract)
    result.json                 lidarbench.attempt/1 + SHA-256 of every file above
    completed.json              SHA-256 of result.json; written last
    abandoned.json              tombstone with the SHA-256 of every file left (only for
                                attempts that never completed; those files become read-only
                                and are verified on every read)
```

Write-once publishing: pending file with exclusive create → `fsync` →
`os.link` to the final name (fails if it exists) → directory `fsync`. Published
files are made read-only. Readers verify the chain (`campaign.json` → frozen
files; `completed.json` → `result.json` → every attempt file; started/result
entry against the frozen schedule) and stop with an integrity error on any
mismatch. Nothing is ever repaired or rewritten. A torn final line in
`events.jsonl` is preserved and skipped.

Every `result.json` has separate verdict fields: `execution_status`,
`artifacts_status`, `structural_status` and `numerical_status` (taken from the
attempt's validation report; `not_run` without one, `unverified` numerically in
`performance-only` mode), a `validation` summary (report path, outcome,
comparability, duration, `reused_from`), and a note that execution success does
not imply numerical validity. Validation runs after the process, before scratch
disposal, and is never timed (VALIDATION.md §8).
Comparability is decided at report time (ARCHITECTURE.md §8).

The runner passes the planning adapter to `execute_plan()`, so its optional
`interpret()` hook runs after the process (untimed, ADAPTERS.md §3). If the
adapter reports an inner workload interval (for example the R bridge,
[R_BRIDGE.md](R_BRIDGE.md) §7), `result.json` has `tool_timing` with
`workload_elapsed_ns`, `startup_overhead_ns`, `shutdown_overhead_ns` and the
marks. Otherwise `tool_timing` is `null`. `timing` remains the outer
fresh-process interval. A declared accepted exception (ADAPTERS.md §3, for
example the LAStools cutoff warning) is recorded as `accepted_exception` in
`result.json` and as its name in the results table. `exit_code` stays the raw
status. `invocation.json` lists the exceptions the command declared.

## 6. Resume, retries and interruption

Per measurement row, using only published, verified attempts:

| Attempts for the row | Action |
|---|---|
| none | `run` |
| any *eligible* (`succeeded` with all required outputs, or `skipped_*`) | `skip-complete` |
| a launched `failed`/`timed_out` attempt, or `succeeded` with missing outputs | `skip-failed`; `retry-failed` with `--retry-failed` |
| only interrupted, abandoned or pre-launch failures (scratch, inputs, monitor start, planning) | `retry-interrupted` |

Warm-ups never count as complete: in each invocation a warm-up row runs if its
unit has a measurement row to run, otherwise `skip-warmup`. Every retry is a
new attempt with `previous_attempts`; old attempts stay. The results table marks
older rows of an entry `superseded`.

Interruption: Ctrl+C/SIGTERM/SIGHUP (Windows: Ctrl+C/Ctrl+Break) cancel the
token for the whole locked run (hashing phases stop between files with exit
code 130 and release the locks); during an attempt the process tree is terminated, the attempt is published as
`interrupted`, scratch is cleaned, locks are released, `run_interrupted` is
logged and the exit code is 130. A hard kill or power loss leaves an attempt
without `completed.json`; the next run (holding the lock, so no other run can
own it) writes `abandoned.json`, logs `attempt_abandoned`, reports any orphaned
scratch, and retries the row.

## 7. Scratch and locks

Scratch (port of the R guards, ARCHITECTURE §9.5): validated roots (not a
filesystem/drive/share root, home, working directory; no overlap with datasets,
the workspace, the package or its git checkout); per attempt
`tempfile.mkdtemp` (POSIX `0700`; on Windows the root's ACL is inherited) with
`out/` and `work/`, an exclusive-create ownership marker with a random token,
and recorded device/inode. Cleanup requires the in-memory token, the marker
token, the device/inode, the validated parent and prefix, and a link-free,
mount-free tree; failures are warnings recorded in the result and the campaign
continues. Retention: `never` (default), `on-failure`, `always`. Orphans are
deleted only by `lidarbench scratch --clean --confirm` under the workspace lock,
when the marker proves ownership and the attempt already has a receipt or
tombstone. Scratch kept on purpose (`retain`, recorded in `result.json`) is
logged as `retained_scratch`, not warned about as an orphan.

Locks: `<workspace>/.lidarbench.lock` and a per-user machine lock
(`$LIDARBENCH_STATE_DIR`, else `%LOCALAPPDATA%\lidarbench`,
`~/Library/Caches/lidarbench` or `~/.local/state/lidarbench`; never a
session-scoped location, so cron, `su` and interactive sessions share it). Each records host, PID, process creation time,
command and a token. Stale locks (owner PID gone or reused) are reported with
the release command and never removed automatically. A dry run refuses to hash
inputs while the machine lock is active. The machine lock is per user: other
users' runs on the same machine are not detected.

## 7a. Results tables

`lidarbench export` verifies the campaign and writes one row per attempt with
a fixed column order (`lidarbench.results-table/1`, see
`campaign/results.py:RESULT_COLUMNS`): entry fields, attempt reason,
statuses, timing (`timing_boundary`, `elapsed_ns`; tool-reported
`tool_workload_elapsed_ns` and `startup_overhead_ns`, empty when the tool
reports no inner interval), monitoring summaries, exact CPU
(`process_cpu_seconds`, `measured_parallelism`, `cpu_budget_exceeded`,
`cpu_affinity`), artifacts/structural/numerical status, `validation_outcome`,
`validation_reused_from`, `validation_duration_ns`, output counts, `record_state`
(`completed`/`abandoned`/`incomplete`) and `superseded`. Nothing is filtered or
aggregated. CSV uses empty cells for null. Parquet needs the optional
`pyarrow` package.

## 8. Differences from the R harness

| Behaviour | R | Python |
|---|---|---|
| Identity format | RDS + MD5 (R-version dependent) | canonical JSON + SHA-256 per component |
| Identity re-check | full identity (git HEAD + every input hash) before **every** entry | full once per invocation; cheap digests/fingerprints per entry |
| Git state | live `rev-parse HEAD` in identity | provenance only; content hashes decide |
| RNG | R Mersenne-Twister | SplitMix64 + Fisher–Yates (portable, golden-tested) |
| Tool order | balanced only | `balanced` or `randomized` |
| Warm-ups on resume | all rerun | only for units with pending measurements |
| Failed entries on rerun | retried automatically | retried only with `--retry-failed`; pre-launch infrastructure failures retried automatically |
| Selection | part of identity | part of identity (config); CLI filters restrict dispatch only |
| Interrupted attempts | block compilation until handled | published `interrupted`, or tombstoned `abandoned`; counted in results |
| Locks | directory lock in reports path | workspace + per-user machine lock with owner, stale detection, explicit release |
| Scratch orphans | in-memory ownership only | on-disk marker; explicit marker-proven cleanup |
| Timing | `Sys.time()` around in-process R call | `perf_counter_ns` around a fresh process (`timing_boundary = process`); the R bridge also reports the legacy inner interval and start-up overhead (`tool_timing`) |
| R warnings during a workload | diagnostics of a successful run | attempt fails (R bridge exit 3) |
| Validation | every attempt, R reference loops | every succeeded attempt, vectorised; verdict reuse for byte-identical outputs, shared references, trusted input digests |
| Thread budget | requested (env, plan, `concurrent_files`) | requested and enforced by CPU pinning (Linux, Windows) |
| Orchestrator death | (in-process) | guardian kills the tool session (POSIX); Job Object (Windows) |

## 9. Limitations

- Resume eligibility is an execution criterion: a deterministic validation
  failure is data, not a reason to rerun.
- Identity covers the lidarbench package, adapter modules and the contracts'
  digests; per-entry checks do not re-read the contract TOML files (a changed
  contract is detected at the next run start). The R bridge files (`bridges/r/*.R`,
  `R/1_functions.R`, `R/0_setup.R`) and `renv.lock` are covered through the
  R tools' executables and identity details.
- Stat fingerprints miss in-place edits that keep size and mtime until the next
  full rehash.
- Windows scratch directories rely on the root's inherited ACL; no explicit ACL
  is set.
- Verified on Linux; Windows and macOS are type-checked (`mypy --platform`) but
  not yet run in CI.

## 10. Preflight (`lidarbench preflight`)

Code: [`campaign/preflight.py`](../src/lidarbench/campaign/preflight.py)
(closes PILOT_REVIEW.md D12).

```sh
lidarbench preflight CONFIG.toml            # grouped summary, one line per check
lidarbench preflight CONFIG.toml --json     # lidarbench.preflight/1 record
lidarbench preflight CONFIG.toml --hash     # also SHA-256 every input file
```

A read-only readiness check to run before `lidarbench run`. It never creates
the campaign, workspace or scratch directories, never writes into the workspace
or datasets, never takes a lock (locks are only inspected) and runs no
benchmarked tool beyond the adapters' `probe()`/`identify()` (as `lidarbench
check` does). Without `--hash` no input file is hashed, so it is fast.

Every check is an item `{id, group, status, message, details}` with status
`ok`, `warning`, `error` or `skipped`:

| Group | Checks |
|---|---|
| `config` | the configuration parses (otherwise the only item, exit 1) |
| `code` | git provenance; a dirty checkout warns for a new campaign (`run --allow-dirty` needed) |
| `workspace` | exists and writable, or creatable; error if it overlaps a dataset |
| `lock` | workspace and machine lock: active → error, stale/unverifiable → warning with the `lidarbench lock release` command |
| `dataset` | directory and inputs (`input_files()`), total bytes; LAS/LAZ headers readable with points; writable inputs or directory → warning (use a read-only copy); with a `lasr` tool every LAS/LAZ input needs a nonempty sibling `.lax` (error); manifest datasets: sizes and header facts (`data verify` quick mode), full SHA-256 only with `--hash` |
| `scratch` | the runner's guard (`validate_scratch_root` with the same protected paths); a missing root is an error with a `mkdir` hint; free space below max(1 GiB, 2 × inputs of the root) → warning |
| `tool` | adapter resolved with its options; version, or unavailability (warning: its entries become recorded skips); unknown adapter/options → error |
| `plan` | per tool × workload: `plan_invocation` for every dataset × CPU count with a dummy scratch path (as `run --dry-run`); `adapter_error` → error, `skipped_unavailable` → warning, only declared skips → skipped |
| `campaign` | new, or existing: attempt records verified, completed measurement entries, incomplete attempts (warning: tombstoned by the next run). `campaign.identity` compares the cheap components (code, config, workloads, tools, environment, schedule) with the frozen ones, and `datasets` only with `--hash`; any difference is an error because `run` would refuse to resume |
| `cpu` | every `cpu_counts` value ≤ CPUs available to the process (affinity mask on Linux, else `os.cpu_count()`); `cpu_affinity = "pinned"`: supported on Linux/Windows, warning on macOS; 1-min load average > 0.25 × logical CPUs or a 0.5 s CPU sample > 20 % → warning |
| `memory` | total/available; below 2 GiB available → warning |
| `monitoring` | configured backend loads and probes (as the runner does) |
| `validation` | contract suite loads; per workload the contract and, per tool, its `contract_tool`, declared implementation and `declared_comparability()`; workloads without a contract are skipped (not validated); `validation_mode = "numerical"` probes the raster and point backends (missing → error) |
| `reporting` | Quarto and Rscript discoverable (`$QUARTO`, `$LIDARBENCH_RSCRIPT`, `PATH`); missing → warning |

Exit codes: `0` no error (warnings allowed), `1` at least one error, `2` usage.
