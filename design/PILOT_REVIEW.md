# Task 13 — End-to-end pilot review

Status: pilot completed 2026-10-29 on Linux; follow-up changes and a validated
pilot in §12 (supersedes the verdict of §1 and the priorities of §10); maintainer
decisions, the edge-tolerant comparability policy and the final readiness in §13. Campaign
`pilot-t13-20261029-001`, identity
`e5c0234437b52ba03e53b97dcaa2d9a657a0da304d4b12a720f8f29fbf73a9e9`.
Example configuration: [`configs/examples/pilot-campaign.toml`](../configs/examples/pilot-campaign.toml).

**No result in this document is a performance ranking.** By decision of the
maintainer, validation was not run in this pilot (`validation_mode =
"performance-only"`). No output was numerically validated and every comparison
was refused by the comparability gate, so all timings below are descriptive
evidence about the framework, not about the relative speed of the tools.

## 1. Verdict

LidarSpeedTests 2.0 is **ready for further pilots and for a full
performance-only (descriptive) campaign of lidR, lasR, LAStools and PDAL on
Linux**. Scheduling, fresh-process execution, monitoring, immutable records,
interruption, crash recovery, resume and reporting all worked on real tools and
real data. It is **not ready for a full campaign meant to publish validated or
cross-tool results**. Four issues block that (§8): the runner does not
validate, the R timing boundaries do not reproduce the legacy boundary, the
PDAL thread budget is not enforced, and lidR's parallel workers are not
monitored. Windows and macOS have not been run.

## 2. Pilot set-up

| Item | Value |
|---|---|
| Machine | `A127637` (same machine as the legacy pilot): Linux 7.0.0-38, 2 × Xeon Gold 5218 (32 cores, 64 logical CPUs), 357 GiB RAM, NVMe `/data/nvme2`; idle (load < 0.4) before the run |
| Software | Python 3.14.4, lidarbench 2.0.0a1 at `aa603fe` (`src/lidarbench` clean); R 4.6.1 + renv; lidR 4.3.3, lasR 0.21.2; LAStools 261023 (`~/opt/lastools-261023/bin`); PDAL 2.10.2 (conda-forge, `stream` mode); Quarto 1.6.42 |
| Data | the verified two-tile Petawawa density-1 pair `298000_5093000`, `298000_5094000` (`.laz` + `.lax`, ~1.0 M points per tile, EPSG:2959). It is the same input the legacy R pilot `fresh-pilot-006` used. A read-only copy (files and directory mode `a-w`) was staged under the pilot directory, so no tool could reach the originals |
| Matrix | `pixel_metrics_1a` (mean Z, 20 m) and `generate_DSM1` (max Z, 1 m) × CPUs {1, 2} × {lidr, lasr, lastools, pdal}; 1 warm-up + 3 measured repetitions; `balanced` order, seed 20261029; cooldown 5 s; `uncontrolled-warm` cache |
| Policies | LAStools `accept_known_cutoff_warning = true` (frozen in identity). The lasR extent policy was chosen for later validation but not exercised, because validation was not run |
| Monitoring | psutil, 0.5 s interval, `required = true` |
| Scratch | separate root on the same NVMe, `retain = "on-failure"` |
| Records | `/data/nvme2/LidarSpeedTestWorkingDir/lidarbench-pilot-task13/` (`pilot.toml`, `workspace/`, run logs, `results.csv`, `report-performance-only/`, `report-validated/`, `experiments/`) |

## 3. Preflight (all passed before the pilot ran)

`lidarbench preflight` and `doctor` are still placeholders, so the preflight
was assembled from existing commands:

| Check | How | Result |
|---|---|---|
| Tool availability and versions | `lidarbench check` with the pilot options | lidr 4.3.3, lasr 0.21.2, pdal 2.10.2 available; LAStools found only through `bin_dir` (not on PATH), build 261023 |
| Data integrity | MD5 against `pilot-source-manifest.csv`; SHA-256 and MD5 against the registered manifest `petawawa-fresh-v4/tile_1000_density_1` | identical in all three places (legacy pilot inputs, preparation stage, registered manifest) |
| Input protection | read-only staged copy; `.lax` present (lasR) | ok |
| Resources | load, free disk (3.4 TB), no active legacy R campaign in the checkout | ok |
| Expected outputs and argv | `lidarbench run --dry-run [--json]` | 64 entries (16 warm-ups, 48 measurements), all `ready`; argv, thread environment and planned outputs inspected per tool |

## 4. Workflow verification

| Area | Test | Result |
|---|---|---|
| Scheduling | frozen `schedule-v2`; warm-ups before measurements; each tool takes a distinct position in each unit's three repetitions | ✅ |
| Execution | 64 / 64 attempts `succeeded` in the final run; argv without a shell; per-tool thread environment applied | ✅ |
| Accepted exceptions | 10 LAStools `pixel_metrics_1a` attempts recorded as `lascanopy_all_values_above_height_cutoff`, the declared opt-in; no other warnings | ✅ |
| Output persistence | per-output inventory with SHA-256 in `result.json`; output files deleted with scratch (`retain = on-failure`) | ✅ as designed (outputs themselves are not archived; §7) |
| Determinism | rasters byte-identical across all repetitions **and** both CPU counts for every tool (lidR `.vrt` index excluded) | ✅ |
| Monitoring | process-tree and whole-machine CSVs plus summary for every attempt, `available`; OS `rusage` in `process_accounting` | ⚠️ works, but see §6.4 (lower bounds, lidR workers invisible) |
| Immutable records | published files read-only (`campaign.json` refuses writes); digest chain verified by `export`/`report`; report checked campaign digests before and after | ✅ (minor gap for abandoned attempts, §7) |
| Input integrity | per-entry fingerprints; full rehash at end of run (`integrity_checked: match`); originals and staged copy unchanged | ✅ |
| Interruption between attempts | SIGINT during the cooldown → `run_interrupted`, exit 130, no partial attempt | ✅ |
| Interruption during an attempt | SIGINT during a lasR attempt → `killpg SIGTERM`, `complete`, no survivors; attempt published `interrupted` with a 0-byte partial output and a missing required output; exit 130; no stray processes | ✅ |
| Crash (SIGKILL of the runner during a lidR attempt) | runner exit 137; workspace and machine locks reported `stale`; `resume` refuses with exit 3 and prints the release command; after `lock release --confirm`, `resume` writes `abandoned.json`, logs `attempt_abandoned` and reports orphaned scratch | ✅ with a defect: the tool process survives (§7) |
| Resume | warm-ups rerun only for units with pending measurements; a second `resume --dry-run` gives 48 `skip-complete`, 16 `skip-warmup` and no identity differences | ✅ |
| Orphaned scratch | `lidarbench scratch --clean --confirm` removed both marker-proven orphans | ✅ |
| Export | `export --format csv|jsonl` (79 attempts over four runs: 77 succeeded (13 warm-ups from the interrupted runs + 64), 1 interrupted, 1 abandoned) | ✅ |
| Reporting | `report --render always`: tables, `summary.md`, Quarto site rendered (R 4.6.1, knitr, ggplot2); tier `execution_only`; all 24 tool pairs refused by the gate (no validation report); within-tool scaling labelled `execution_only`; `--mode validated` shows no measurements and `validation_incomplete` issues | ✅ (label issue, §7) |

Campaign timeline: created 18:14 UTC; 3 deliberately interrupted runs; final
run 18:20–18:36 UTC (64 attempts, ~15 min including cooldowns and warm-ups).

## 5. Validation

Not exercised (maintainer decision for this pilot). The runner still records
`structural_status = not_run` and `numerical_status = unverified` and writes no
`validation/*.json`. The reporting therefore classifies every measurement as
`execution_only`, and the comparability gate refused every pair with
"cross-tool comparisons require a validated report". This fail-closed
behaviour is correct. The legacy pilot `fresh-pilot-006` had the same tools on
the same bytes with lidR `passed`, lasR `accepted_exception` and LAStools
`unverified`. None of these verdicts carries over to the new campaign.

## 6. Performance observations (descriptive only)

### 6.1 Timings

Medians of three measured repetitions (seconds). *Outer* = `perf_counter_ns`
around the fresh process (the timed interval). *Inner* = the tool-reported
workload interval (R bridge `benchmark_execute_call()`, PDAL launcher). *Legacy*
= `fresh-pilot-006`, same machine and bytes, boundary "workload call only",
measured in a long-lived R session after a warm-up.

| Workload | Tool | CPUs | Outer | Inner | Start-up overhead | Legacy R |
|---|---|---|---|---|---|---|
| pixel_metrics_1a | lidR | 1 | 16.08 | 7.01 | 8.78 | 4.69 |
| pixel_metrics_1a | lidR | 2 | 22.66 | 6.18 | 15.85 | 6.93 |
| pixel_metrics_1a | lasR | 1 | 10.46 | 7.81 | 2.52 | 2.97 |
| pixel_metrics_1a | lasR | 2 | 9.11 | 6.47 | 2.51 | 1.47 |
| pixel_metrics_1a | LAStools | 1 | 3.69 | — | — | 3.88 |
| pixel_metrics_1a | LAStools | 2 | 1.88 | — | — | 2.07 |
| pixel_metrics_1a | PDAL | 1 | 1.90 | 1.77 | 0.11 | — |
| pixel_metrics_1a | PDAL | 2 | 1.02 | 0.88 | 0.11 | — |
| generate_DSM1 | lidR | 1 | 15.32 | 6.44 | 8.80 | 4.11 |
| generate_DSM1 | lidR | 2 | 22.38 | 5.58 | 15.85 | 5.49 |
| generate_DSM1 | lasR | 1 | 20.60 | 17.84 | 2.47 | 12.76 |
| generate_DSM1 | lasR | 2 | 14.49 | 11.87 | 2.51 | 6.90 |
| generate_DSM1 | LAStools | 1 | 2.34 | — | — | 2.53 |
| generate_DSM1 | LAStools | 2 | 1.20 | — | — | 1.40 |
| generate_DSM1 | PDAL | 1 | 2.00 | 1.86 | 0.11 | — |
| generate_DSM1 | PDAL | 2 | 1.06 | 0.92 | 0.11 | — |

Repetition variability is small (SD ≤ 0.43 s; CV < 3 %). The Quarto mean ± SD
figure and the per-repetition figure rendered correctly.

### 6.2 Where the R time goes (bridge marks, medians)

| Phase (outside the inner interval unless noted) | lasR | lidR 1 CPU | lidR 2 CPUs |
|---|---|---|---|
| launch → R ready | 0.28 | 0.28 | 0.28 |
| renv activation | 1.67 | 1.67 | 1.67 |
| package loading | 0.52 | 6.3 (lidR imports sf, terra, stars) | 6.2 |
| source `R/1_functions.R`, catalog set-up | 0.01 | 0.40 | 0.40 |
| thread / worker set-up (`future::multisession`) | 0.01 | 0.05 | **7.4** |
| teardown + exit | 0.13 | 0.20 | 0.64 |

### 6.3 Comparison with the legacy R implementation

* **LAStools:** the new outer times are ~0.2 s *shorter* than the legacy times
  for every unit. The legacy interval also wrapped a native process, launched
  from R via `processx`; the difference is consistent with that launch and
  collection overhead (not investigated further).
* **lasR (inner interval > legacy):** the first `lasR::exec()` in a fresh R
  process loads the `terra` and `sf` namespaces lazily (with GDAL/PROJ/GEOS),
  and that takes **~5.0 s**, inside the inner interval. Shown with the pinned
  call repeated in one process: calls 1/2/3 took 7.71/2.93/2.94 s at 1 CPU and
  6.38/1.44/1.47 s at 2 CPUs. Calls 2 and 3 equal the legacy medians (2.97 s,
  1.47 s). With `sf` and `terra` preloaded, even the first call is normal. The
  legacy harness paid this cost once, in its in-session warm-up.
* **lidR, 1 CPU:** the first call in a process costs ~1.7 s more (6.6 s vs
  4.9 s for later calls ≈ legacy 4.69 s). `future::multisession` with one
  worker runs in-process.
* **lidR, 2 CPUs:** starting the two worker R processes takes ~7 s. Both
  harnesses do this outside the inner interval, but the Python outer interval
  includes it. Inner times (5.6–6.5 s) are comparable to legacy (5.5–6.9 s).
  For two tiles, lidR at 2 CPUs is slower than at 1 CPU in both harnesses.
* Hence neither Python boundary reproduces the legacy boundary. The outer
  interval adds 2.5 s (lasR) to 16 s (lidR, 2 CPUs) of R start-up per attempt,
  and the inner interval adds a once-per-process initialisation cost (~5 s
  lasR, ~1.7 s lidR). For workloads of a few seconds this dominates, and it
  inverts the within-tool CPU scaling for lidR (scaling ratio 0.68–0.71).

### 6.4 Resources and thread budgets

Median CPU seconds of the process tree: exact OS `rusage` of waited-for
descendants vs the psutil sampled tree total (a declared lower bound).

| Workload | Tool | CPUs | rusage CPU s | sampled CPU s | Outer s |
|---|---|---|---|---|---|
| pixel_metrics_1a | PDAL | 1 | 5.44 | 2.84 | 1.90 |
| pixel_metrics_1a | PDAL | 2 | 5.49 | 2.67 | 1.02 |
| generate_DSM1 | PDAL | 1 | 5.63 | 4.19 | 2.00 |
| pixel_metrics_1a | LAStools | 1 / 2 | 3.69 / 3.71 | 3.5 / 3.0 | 3.69 / 1.88 |
| pixel_metrics_1a | lasR | 1 / 2 | 9.83 / 9.92 | 9.36 / 9.78 | 10.46 / 9.11 |
| pixel_metrics_1a | lidR | 1 / 2 | 15.71 / **11.16** | 15.07 / 10.69 | 16.08 / 22.66 |

* **PDAL exceeds its thread budget.** At "1 CPU" a PDAL attempt uses ~2.9
  cores on average (5.4 CPU-s in 1.9 s; sampled peak 531 % at 2 CPUs).
  `readers.las` defaults to `threads = 7` (decompression pool) and the adapter
  does not set it; only the number of concurrent per-tile processes is
  limited. A manual pipeline took 0.88 s with `threads = 7` vs 1.52 s with
  `threads = 1`, and still used ~1.5 cores with `threads = 1`. PDAL's 1-CPU
  timings and its scaling ratios (1.87–1.89) are therefore not single-core
  numbers.
* **lidR workers are invisible to monitoring.** `parallelly` starts the
  multisession workers (and renv starts a watchdog R process) through a shell,
  so they are reparented to PID 1. They stay in the attempt's session and
  process group, so `killpg` termination still reaches them, but they are not
  descendants. The process tree never had more than one process at 2 CPUs, so
  lidR's CPU, RSS and rusage at 2 CPUs are understated (11.2 vs 15.7 CPU-s).
* **Sampled CPU is a lower bound for short-lived children.** The report's
  `tree_cpu_seconds` comes from sampling (`is_lower_bound = true` for all 48
  measurements) and misses up to half of PDAL's CPU. `process_accounting`
  (`rusage`) is exact for waited-for descendants on POSIX, but the report does
  not show it.
* LAStools and lasR stay within their budgets (CPU-s ≈ wall × CPUs or less).
  Peak RSS (largest single process): LAStools 36 MiB, PDAL ~100–117 MiB, lasR
  ~300 MiB, lidR 0.6–0.8 GiB (main process only).

## 7. Defects and gaps found

| # | Finding | Severity | Notes |
|---|---|---|---|
| D1 | The runner never validates; no `validation/*.json` | blocker for validated campaigns | known (VALIDATION.md §8); out of scope here by decision |
| D2 | R timing boundaries ≠ legacy boundary (outer start-up 2.5–16 s; inner first-call initialisation ~5 s lasR, ~1.7 s lidR) | blocker for R timings and R-vs-native comparisons | §6.3 |
| D3 | PDAL thread budget not enforced (`readers.las threads` default 7) | blocker for PDAL timings/scaling | adapter; pipelines change ⇒ new adapter version and campaign |
| D4 | Thread budgets are configured, not enforced, for all tools (no CPU affinity/cgroup); overruns are not flagged | high | rusage CPU/wall could flag them automatically |
| D5 | lidR multisession workers and the renv watchdog escape the descendant-based tree monitor and rusage | high for resource reporting | track by session/process group on POSIX (job object on Windows) |
| D6 | After a hard kill of the runner, the tool process tree keeps running to completion, unsupervised (writes into the dead attempt's scratch) | medium | POSIX: no parent-death signal; a later `resume` could overlap an orphaned tool. Consider `PR_SET_PDEATHSIG` via a small supervisor, or killing the recorded session on resume |
| D7 | Report shows the sampled (lower-bound) tree CPU, not the exact `rusage` value | medium | report column choice |
| D8 | The legacy mean ± SD figure labels the outer process time "Workload time mean" | low | label/timing-boundary text in the staged figure |
| D9 | Abandoned attempts: `abandoned.json` lists files without digests; the never-sealed files stay writable | low (excluded from results) | add per-file SHA-256 to the tombstone |
| D10 | `retain = "on-failure"` scratch of interrupted attempts is reported as "orphaned" on every run until cleaned | low (cosmetic) | |
| D11 | Workload contracts are not part of campaign identity; outputs are inventoried but not archived, so they cannot be validated after the fact unless scratch is retained | medium | known |
| D12 | `preflight` / `doctor` are placeholders; the preflight here was manual | medium | |

Not defects: the dirty-checkout check is scoped to `src/lidarbench` (R bridge
files enter identity through executable hashes); the LAStools
`-height_cutoff 164.24000000000001` is the intended port of R
`format(digits = 17)`.

## 8. Spatial processing semantics (from the contracts; not re-validated)

| Tool | Cell ownership | Artifacts in this pilot | Declared class |
|---|---|---|---|
| lidR | `lidr_buffered_clamp` (boundary points differ from the contract partition) | one GeoTIFF per tile + `pixel_metrics.vrt` / `rasterize_canopy.vrt` | descriptive (legacy: validated) |
| lasR | `right_down`, one extra cell east and north (extent policy) | one GeoTIFF per tile | descriptive (legacy: accepted exception) |
| LAStools | `undeclared` | GeoTIFF + `.tfw` per tile (`*_avg.*` for the metric); `.tfw` is unexpected under workloads-v1/v2 | non-comparable |
| PDAL | `pdal_binmode_floor` (upward-Y edges) | one uncompressed double GeoTIFF per tile (8.0 MB for the DSM vs ~2–2.7 MB for others) | functionally comparable |

Output encodings differ (compression, data type, extra files), so the I/O
inside each timed interval differs as well. This must be considered before
any cross-tool interpretation, even of validated results.

## 9. Missing capabilities

* Runner validation integration and contract digests in identity (D1, D11).
* A decided R timing policy: which interval is primary, and how per-process
  initialisation is handled (D2).
* Thread-budget enforcement and measured-parallelism flags (D3, D4).
* Session/process-group-based monitoring and accounting (D5, D7).
* Supervisor-death handling for tool trees (D6).
* `preflight` / `doctor` commands (D12).
* Native runs on Windows and macOS (only type-checked so far).
* Output archiving for later independent validation (optional retention of
  outputs in the campaign or a content-addressed store).

## 10. Prioritized next changes

1. **Runner validation integration** (validate before scratch cleanup, write
   `validation/*.json` into the attempt digest chain, campaign→contract tool
   mapping, contract digests in identity, extent policy in config). This
   unlocks validated reports and gated comparisons.
2. **R timing boundary.** Decide the primary boundary for R tools. Recommended:
   keep the outer process interval as the timed interval. In the bridge,
   preload the namespaces that the pinned calls load lazily (`sf`, `terra`) in
   the untimed `packages` phase, so that the inner interval again matches the
   legacy definition. Report both intervals side by side. This needs a bridge
   protocol/version change and a new campaign.
3. **PDAL thread budget.** Set `readers.las` `threads` from the budget (or 1
   per concurrent file) and record it; new adapter version and campaign.
4. **Monitoring by session/process group** and exact CPU from `rusage` in
   reports. Flag attempts whose CPU-s / wall exceeds the budget.
5. **Orphan protection** after supervisor death (D6), and tombstone digests
   (D9).
6. **`lidarbench preflight`** that runs the §3 checks (tool probe with
   options, manifest/digest verification of inputs, read-only/`.lax` checks,
   load and disk, dry-run plan).
7. **Windows/macOS pilots** with the same configuration.

Then repeat this pilot with validation enabled (`validation_mode =
"numerical"`, extent policy on) and confirm the validated and accepted
evidence tiers and gated comparisons on real records before approving a full
campaign.

## 11. Reproduction

```sh
uv run lidarbench check
uv run lidarbench run PILOT/pilot.toml --dry-run --json
uv run lidarbench run PILOT/pilot.toml            # (interrupted twice on purpose)
uv run lidarbench lock status [PILOT/workspace]   # after the SIGKILL test
uv run lidarbench lock release --confirm PILOT/workspace/.lidarbench.lock
uv run lidarbench lock release --confirm
uv run lidarbench resume PILOT/pilot.toml
uv run lidarbench scratch PILOT/pilot.toml --clean --confirm
uv run lidarbench export PILOT/workspace/campaigns/pilot-t13-20261029-001 --format csv -o PILOT/results.csv
uv run lidarbench report PILOT/workspace/campaigns/pilot-t13-20261029-001 --render always -o PILOT/report-performance-only
```

The diagnostic R and PDAL experiments of §6.3–6.4 are in
`PILOT/experiments/` (outside any campaign; untimed by lidarbench).

## 12. Follow-up: changes after the pilot and the validated pilot (2026-10-29)

All §10 priorities except the Windows/macOS runs were implemented (commit
`8e79008`) and verified with a second, **validated** pilot
`pilot-t13-20261029-002-validated` (same machine, data, tools, matrix and seed;
`validation_mode = "numerical"`, `accept_known_extent_discrepancies = true`,
`cpu_affinity = "pinned"`, suite `workloads-v3`), a crash test campaign
`pilot-t13-20261029-003-killtest`, and `lidarbench preflight`
(34 checks ok before the run).

### 12.1 Status of the defects

| # | Change | Verified |
|---|---|---|
| D1 | Runner validates every succeeded attempt before scratch cleanup and seals `validation/report.json`; contracts (key, digest) frozen in identity; `[validation]` config (suite, extent policy, reuse) | 64/64 attempts with reports; reporting reads them (validated tier) |
| D2 | R bridge preloads the namespaces the pinned calls load lazily (`sf`, `terra`; lidR also `glue`) in a new untimed `namespaces_preloaded` phase; records `preloaded_namespaces` and `workload_loaded_namespaces` (empty in every run) | lasR inner times = legacy (below) |
| D3 | PDAL `readers.las threads = 1` (probed; recorded in identity and metadata) | PDAL CPU-s ≈ wall × CPUs |
| D4 | `scheduling.cpu_affinity = "pinned"` (default): whole tree pinned to `cpus` logical CPUs, one per physical core, first core reserved; results report `process_cpu_seconds`, `measured_parallelism`, `cpu_budget_exceeded` | 0 of 48 measurements over budget; pinned CPUs `1` / `1 2` recorded |
| D5 | POSIX monitoring follows the whole session (reparented `parallelly` workers, renv watchdog) | lidR 2-CPU CPU time 30.6 s (was 11.2 s) |
| D6 | Guardian process kills watched tool sessions if the orchestrator dies (POSIX; Windows already has `KILL_ON_JOB_CLOSE`) | SIGKILL of the runner during a lidR 2-CPU attempt: main R, both workers and the renv watchdog gone within 2 s; empty outputs |
| D7 | Reports use `process_cpu_seconds` (max of `rusage` and sampled tree) and list `cpu_budget_exceeded` issues | report rendered |
| D8 | The legacy figure's axis label is replaced at render time ("Process wall time mean (s; outer fresh-process interval)"); caption names the boundary; `R/reporting.R` unchanged | figure checked |
| D9 | Tombstones record the SHA-256 of every file left; those files become read-only and are verified on every read | kill test |
| D10 | Scratch retained by policy is logged `retained_scratch`, not warned as orphaned | unit test |
| D11 | Contracts in identity (done); LAStools `.tfw` accepted as sidecars in the new default suite `workloads-v3` (v2 frozen) | LAStools now `unverified`, not `validation_failed` |
| D12 | `lidarbench preflight CONFIG [--hash] [--json]` (design/CAMPAIGNS.md §10) | pilot preflight: ready |

### 12.2 Validated pilot results (descriptive; no ranking)

All 64 entries succeeded. Validation outcomes per tool (16 attempts each):
lidR `validated`, PDAL `validated` (real PDAL 2.10.2 matches the mirrored
`pdal_binmode_floor` reference), lasR `accepted_exception` (one-cell extent
policy), LAStools `unverified` (cell ownership undeclared). The validated report
includes 36 measurements (lidR, lasR, PDAL). **No cross-tool comparison passed
the gate**, by design: lidR and PDAL own boundary points differently from the
contract partition (lidR 6 points; PDAL 1 000 points for the 20 m metric and
19 721 for the 1 m DSM, upward-Y edges), and lasR is only an accepted
exception. Speedups therefore remain unavailable on this data under the current
contracts and policies.

Medians in seconds; P1 = first pilot (unpinned, before the changes), P2 =
validated pilot; *inner* = tool-reported interval.

| Workload | Tool | CPUs | Outer P1 | Outer P2 | Inner P1 | Inner P2 | Legacy R | CPU-s P2 | Parallelism P2 |
|---|---|---|---|---|---|---|---|---|---|
| pixel_metrics_1a | lasR | 1 | 10.46 | 10.44 | 7.81 | **2.97** | 2.97 | 9.75 | 0.94 |
| pixel_metrics_1a | lasR | 2 | 9.11 | 8.95 | 6.47 | **1.51** | 1.47 | 9.78 | 1.09 |
| generate_DSM1 | lasR | 1 | 20.60 | 19.92 | 17.84 | **12.48** | 12.76 | 19.31 | 0.97 |
| generate_DSM1 | lasR | 2 | 14.49 | 14.30 | 11.87 | **6.89** | 6.90 | 20.35 | 1.42 |
| pixel_metrics_1a | lidR | 1 | 16.07 | 15.26 | 7.01 | 6.61 | 4.69 | 14.92 | 0.98 |
| pixel_metrics_1a | lidR | 2 | 22.66 | 22.40 | 6.18 | 5.98 | 6.93 | 30.63 | 1.37 |
| generate_DSM1 | lidR | 1 | 15.32 | 14.78 | 6.44 | 6.13 | 4.11 | 14.42 | 0.98 |
| generate_DSM1 | lidR | 2 | 22.38 | 21.82 | 5.58 | 5.43 | 5.49 | 29.92 | 1.37 |
| pixel_metrics_1a | PDAL | 1 | 1.90 | **4.27** | 1.77 | 4.13 | — | 4.25 | 1.00 |
| pixel_metrics_1a | PDAL | 2 | 1.02 | **2.22** | 0.88 | 2.07 | — | 4.27 | 1.92 |
| generate_DSM1 | PDAL | 1 | 2.00 | **4.36** | 1.86 | 4.22 | — | 4.34 | 1.00 |
| generate_DSM1 | PDAL | 2 | 1.06 | **2.26** | 0.92 | 2.12 | — | 4.37 | 1.93 |
| pixel_metrics_1a | LAStools | 1 / 2 | 3.69 / 1.88 | 3.70 / 1.89 | — | — | 3.88 / 2.07 | 3.68 / 3.69 | 1.00 / 1.96 |
| generate_DSM1 | LAStools | 1 / 2 | 2.34 / 1.20 | 2.35 / 1.22 | — | — | 2.53 / 1.40 | 2.34 / 2.37 | 0.99 / 1.94 |

* lasR's inner interval now reproduces the legacy boundary exactly; its outer
  time is unchanged (the ~5 s namespace load moved before the inner interval).
* PDAL's first-pilot timings were not 1- or 2-core numbers; pinned and with one
  reader thread, it uses exactly its budget (parallelism 1.00 / 1.92).
* lidR at 1 CPU still exceeds legacy by ~1.7–2 s inside the inner interval.
  It is not namespace loading: most of it is garbage collection while the R heap
  grows from its start-up size (2.6 s GC in the first call vs 0.5 s in the third;
  `R_VSIZE=4G R_NSIZE=20M` removes it). Heap presizing changes R's runtime
  configuration and was **not** applied; it needs a decision (§12.5).
* LAStools is unchanged within noise; its outer times stay ~0.2 s below legacy.
* Both pilots spent the same wall time (~15 min for 64 attempts, 5 s cooldown);
  per-attempt bookkeeping including validation was 0.17 s median, ≤ 1 s.

### 12.3 Validation: what it consists of and what it costs

Per attempt: inventory of output files; complete decoding of every raster;
structural checks (format, CRS, resolution, alignment, extent, bands, NoData);
for the numerical level, decoding of the tile and every neighbour within the
20 m buffer, an independent float64 per-cell mean/max under the contract's and
the tool's cell convention, and a cell-by-cell value/NoData-mask comparison
(design/VALIDATION.md §3, §8.1). Measured here: a full validation of the pilot
pair takes ~0.2–0.8 s (~1 s cold including imports); at density 10 the 1 m DSM
takes 4.7 s and 1.4 GB.

The legacy R validation was slow because it read all point attributes, ran an
interpreted per-cell R loop over ~1 M cells per tile for 1 m rasters (and `sf`
triangulation for TIN references), re-hashed inputs and repeated everything for
every attempt. The new runner keeps every check but bounds the work:

* **verdict reuse for byte-identical outputs** (deterministic tools produce
  identical bytes across repetitions and CPU counts — true for all four tools
  here), recorded with the source attempt and report digest;
* **shared per-tile references** for all declared conventions, keyed by input
  digests, contract and grid;
* **edge-strip caching**, so each input is decoded about once per workload
  instead of once per neighbour visit;
* **trusted input digests** from the frozen identity instead of hashing every
  input twice per validation.

Pilot effect: 8 full validations (one per tool × workload) took 2.2 s in total;
the other 56 attempts reused verdicts in ~3 ms each, versus an estimated
~64 s for 64 independent validations at ~1 s each. At full scale (100 tiles × 6 densities) the cost per
tool × workload × dataset is roughly one decode of the dataset plus one
reference, instead of one per attempt.

### 12.4 Updated readiness

LidarSpeedTests 2.0 is now **ready for a full campaign on Linux that produces
validated per-tool timings** (lidR, PDAL validated; lasR as an explicit accepted
exception; LAStools descriptive), with enforced CPU budgets, complete resource
accounting and crash safety. It is **not yet able to publish cross-tool
speedups** on these workloads, because no tool pair passes the comparability
gate under the current contracts (§12.2). Windows and macOS remain unverified.

### 12.5 Remaining decisions and work (priority order)

1. **Comparability policy.** Decide whether functionally comparable pairs may
   be compared when their boundary-point differences are declared and
   reported, or whether new contract conventions are needed. Without this, no
   speedup can be published. (Maintainer decision; a new suite if contracts
   change.)
2. **lidR heap presizing** (`R_VSIZE`/`R_NSIZE`) to remove first-call GC from
   the inner interval — a runtime-configuration decision.
3. **Full-scale validation memory**: the reference cache (1 GiB) and strip
   cache should be checked on the 100-tile density-50 data before the full run
   (one tool, one workload, `--limit`).
4. Windows and macOS pilots (pinning on Windows, guardian not needed there;
   macOS without pinning).
5. Remaining contracts (TIN DEM, nine-band metrics, normalization, treetops)
   and the R `validation-v1` bridge.

## 13. Maintainer decisions and confirmation (2026-10-29)

| Open item (§12.5) | Decision | Implementation |
|---|---|---|
| 1. Comparability policy | Accept that each tool may produce slightly different output at the edges | New frozen policy `[validation] comparability = "edge-tolerant"` (default stays `strict`; commit `d729950`, ARCHITECTURE.md §8.3). It accepts only declared boundary-point ownership differences and the `accept_known_extent_discrepancies` exception; every owned cell value must still match the independent reference. Undeclared conventions (LAStools, `unverified`), other exceptions and failures stay descriptive. Accepted differences are listed per comparison (`accepted_differences`) and the policy is stated in every report. |
| 2. lidR heap presizing | Keep R's default settings | Nothing changed; the ~2 s first-call GC cost of lidR at 1 CPU stays inside its inner interval (documented in §12.2). |
| 3. Full-scale validation memory | Not needed: the full density-50 benchmark has already run on this machine | Not tested separately. Python validation keeps at most one fully decoded tile (plus a small LRU), cached edge strips and ≤ 1 GiB of references; a 50 M-point tile decodes to ~1.2 GB of float64 XYZ. |

Confirmation campaign `pilot-t13-20261029-004-edge-tolerant` (same matrix,
data, tools and seed as §12, plus `comparability = "edge-tolerant"`; preflight
ready; another interactive session used part of one core during the run, so
its timings are not used here): all 64 entries succeeded; outcomes as in §12.2.
The validated report now has **12 of 12 lidR/lasR/PDAL pairs comparable**
(2 workloads × 2 CPU counts × 3 pairs), each listing its accepted differences:

| Tool | Accepted difference (both CPU counts) |
|---|---|
| lidR | 6 points owned by a different cell than the contract partition (`lidr_buffered_clamp`) |
| lasR | accepted outward extent (one extra cell east/north) |
| PDAL | 1 000 (`pixel_metrics_1a`) / 19 721 (`generate_DSM1`) points owned differently (`pdal_binmode_floor`, upward-Y edges) |

LAStools remains descriptive only (`unverified`: its cell convention is
undeclared). Ratios exist in the pilot report's `comparisons` table; per the
project rule no ranking is published from pilot data.

**Readiness.** LidarSpeedTests 2.0 is ready, on Linux, for the full campaign
with validated per-tool timings, enforced CPU budgets, complete resource
accounting, crash safety and gated cross-tool comparisons for lidR, lasR and
PDAL under the edge-tolerant policy (LAStools descriptive). Remaining before a
cross-platform claim: Windows and macOS runs. Remaining scope: contracts for
the other legacy workloads (TIN DEM, nine-band metrics, normalization,
treetops) and the R `validation-v1` bridge. The full campaign is not started
without the maintainer's approval.
