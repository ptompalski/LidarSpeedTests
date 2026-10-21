# Development plan: one task per chat

Date: 2026-10-20
Development branch: `dev` (created from `main`)
Basis: `AI_logs/2026-10-20_code_audit.md`, checked against the current sources.

## Goal and working agreement

Produce a trustworthy, reproducible benchmark campaign and regenerate its
published results. Repair the harness before running a full campaign.

Each numbered task below is a separate chat. Start that chat with the supplied
prompt, read this plan and the audit, and implement only that task plus its
directly required tests and documentation. Record the outcome, remaining
blockers, and any changed decisions in this file before ending the chat.
Do not claim completion when external inputs or runtime checks are unavailable.

Work on `dev`; do not commit, push, download large datasets, or launch a full
benchmark campaign without an explicit request. Preserve historical results and
distinguish them from newly generated campaigns. Choose test tooling appropriate
to this script-based repository; no test suite is currently tracked.

## Chat 1: Preserve failures and represent unsupported workloads

**Scope:** `R/1_functions.R`, `R/4_compileResults.R`.

- Preserve recorded failures during compilation. Flag elapsed times below
  0.1 seconds separately rather than converting execution status.
- Treat numeric nonzero LAStools exit statuses as failures.
- Detect empty tool/task calls before execution and record an explicit
  unsupported/skipped outcome, not a successful zero-work run.
- Keep existing CSVs readable; surface errors and retain diagnostic messages.

**Done when:** focused regression checks cover failed and successful R calls,
zero/nonzero process statuses, empty calls, and short-duration records.
Failed or skipped attempts cannot become successful through compilation.

**Starter prompt:** "Implement Chat 1 from AI_logs/2026-10-20_development_plan.md:
repair failure handling and unsupported workload reporting."

## Chat 2: Make configuration, process invocation, and scratch cleanup safe

**Scope:** `R/0_setup.R`, `R/1_functions.R`, `R/3_runBenchmarks.R`.
**Depends on:** Chat 1.

- Separate machine-local paths and settings from workload definitions; validate
  input, report, scratch, and executable paths before processing.
- Replace concatenated shell commands with executable/argument handling that
  preserves paths containing spaces and captures exit status, stdout, and stderr.
- Use unique run-owned scratch directories under a validated scratch root.
  Refuse cleanup of roots, inputs, reports, or arbitrary unowned directories.
- Guarantee worker and monitoring cleanup on errors. Remove duplicate system
  information logic and pass the configured LAStools path consistently.
- Keep Windows support; make Linux execution possible without Windows drive
  assumptions. Do not silently substitute tools or enable unavailable monitoring.

**Done when:** temporary-directory and stub-process checks demonstrate quoted
paths, missing-tool errors, diagnostic capture, safe cleanup, and input protection.

**Starter prompt:** "Implement Chat 2 from the development plan: configurable
paths, reliable external processes, and run-owned scratch directories."

## Chat 3: Establish a reproducible R and native-tool environment

**Scope:** renv configuration, dependency declarations, setup documentation.
**Depends on:** Chat 2.

- Confirm the target benchmark OS and R version with the user before pinning
  them; record the distinction between this Linux checkout and historical runs.
- Initialize renv, fix the `R/*` dependency-discovery exclusion, and capture
  script and Quarto dependencies, including lasR repository provenance.
- Track the lockfile and required activation files. Document R, Quarto,
  geospatial libraries, LAStools identity/license, and restoration commands.
- Check package APIs used by the harness against the chosen versions. Replace
  fragile private helpers and printed-summary parsing with supported interfaces;
  coordinate workload-semantic changes with Chat 5.

**Done when:** required scripts load in the pinned environment, an isolated
restore succeeds, and missing external dependencies produce actionable errors.
Record any restore or compatibility blocker explicitly.

**Starter prompt:** "Implement Chat 3 from the development plan: establish renv,
pin the chosen runtime, and document native dependencies."

## Chat 4: Recover and reproduce benchmark input preparation

**Scope:** `R/2_data.R`, `ALS_tiles.gpkg`, input-data documentation/manifests.
**Depends on:** Chats 2-3.

- Establish the actual Petawawa source, tile selection, licensing, and available
  historical preparation scripts with the user. Do not invent missing URLs or
  describe a replacement workflow as the recovered historical method.
- Replace the commented workflow that expects a nonexistent `Download_LAZ`
  column with validated source metadata and resumable preparation.
- Specify thinning and artificial densification, seeds, noise parameters,
  tiling, indexing, and point-format conversion for densities 1, 2, 5, 10, 20,
  and 50 points/m2.
- Write source/output checksums and preparation metadata; provide a small
  neighboring-tile subset without committing large point clouds.

**Done when:** an approved small subset can be prepared reproducibly and
validated for tile coverage, density, point attributes, and provenance.
Full-data preparation remains a separately authorized operation.

**Starter prompt:** "Implement Chat 4 from the development plan: recover the
Petawawa input workflow and make preparation reproducible."

## Chat 5: Define and implement comparable workload contracts

**Scope:** task definitions in `R/0_setup.R`, metric helpers, methodology docs.
**Depends on:** Chat 3; use Chat 4 metadata when available.

- Specify each task's point population, original elevation versus normalized
  height, return/class filters, thresholds, units, quantile convention, and SD.
- Resolve LAStools cover and height-cutoff differences. Mark combinations
  non-comparable when equivalence cannot be established rather than presenting
  them as equivalent.
- Make buffers, raster origin/resolution/extent, bands, NoData policy, output
  formats, and tree-output persistence explicit.
- Adopt supported lasR DSM APIs where appropriate. Distinguish DSM from CHM and
  actual sequential pipelines from sums of independent task timings.
- Confirm the supported complex-task matrix and required artifacts.

**Done when:** tiny known-value fixtures demonstrate the metric definitions,
and every supported tool/task has a documented input/output contract.

**Starter prompt:** "Implement Chat 5 from the development plan: equivalent
workload definitions and supported tool/task contracts."

## Chat 6: Add campaign identity and immutable attempt records

**Scope:** runner, result schema, compiler.
**Depends on:** Chats 1-5.

- Introduce campaign, repetition, and attempt IDs; retain every attempt,
  including failed retries, instead of overwriting result CSVs.
- Record code revision/dirty state, configuration, dataset checksums, lockfile
  identity, tool versions/checksums, OS/hardware, and parallelism.
- Resume only matching campaign work; reject incompatible configurations.
- Compile an explicitly selected campaign, not independently selected package
  versions per task. Handle legacy results separately and use proper version
  comparison wherever legacy version selection remains necessary.

**Done when:** changed data/environment/configuration cannot reuse stale
results, retries retain history, and compilation cannot silently mix campaigns.

**Starter prompt:** "Implement Chat 6 from the development plan: campaign
manifests, immutable attempts, and explicit campaign selection."

## Chat 7: Validate artifacts and cross-tool numerical correctness

**Scope:** `prepare_input()`, `check_results()`, validation fixtures/reports.
**Depends on:** Chats 4-6.

- Derive expected artifacts from workload contracts and input manifests, never
  from the maximum output count observed in a set of runs.
- Validate raster geometry, bands, coverage, finite values/NoData; point-cloud
  counts and attributes; and required vector/complex-task outputs.
- Detect chunk-level failures and partial processing. Store execution and
  validation outcomes separately, with explicit diagnostic reasons.
- Apply documented per-workload tolerances on neighboring tiles, including
  buffered boundaries and empty/sparse cases.

**Done when:** missing, corrupt, partial, misaligned, or numerically incorrect
outputs are rejected; known-good fixtures pass; validation is outside timing.

**Starter prompt:** "Implement Chat 7 from the development plan: contract-based
artifact validation and small cross-tool correctness checks."

## Chat 8: Add repetitions, balanced ordering, and explicit parallelism

**Scope:** `R/3_runBenchmarks.R`, execution scheduling and configuration.
**Depends on:** Chats 6-7.

- Schedule reproducible balanced/randomized tool order with explicit repetitions
  and a recorded seed; do not run competing benchmark tools simultaneously.
- Define warm-up, warm/cold-cache policy, and timed boundaries. Do not claim
  cold-cache measurements without an implemented, verified procedure.
- Make worker and native-thread counts explicit to avoid oversubscription.
- Replace unreachable post-return sleeps with an intentional cooldown policy.

**Done when:** scheduler checks verify repetition counts, reproducible order,
balance, and thread configuration; records separate warm-ups from measurements.

**Starter prompt:** "Implement Chat 8 from the development plan: repeated,
balanced benchmark scheduling and explicit timing/parallelism policy."

## Chat 9: Make resource monitoring campaign-aware

**Scope:** `cpu_ram_usage.ps1`, monitoring integration and result ingestion.
**Depends on:** Chats 2, 6, and 8.

- Confirm whether whole-machine utilization is sufficient or process-tree
  monitoring is required, and which platforms the campaign must support.
- Store timestamps, units, sampling interval, monitoring scope, and attempt IDs;
  start and stop monitors reliably on both success and failure.
- Supply the chosen platform support and explicitly report unavailable
  monitoring. Document interference from unrelated machine activity.
- Produce deterministic CPU/RAM summaries aligned with timed intervals.

**Done when:** a short controlled run produces attributable samples and summaries
and leaves no monitor process running after an intentional workload failure.

**Starter prompt:** "Implement Chat 9 from the development plan: attributable,
reliable CPU/RAM monitoring and summary generation."

## Chat 10: Rebuild tables and figures from selected campaign records

**Scope:** `R/4_compileResults.R`, reporting scripts, `*.qmd`, `_quarto.yml`.
**Depends on:** Chats 6-9.

- Recover or replace missing generators for `CPU-RAM.rds`, `complex1.rds`,
  and figures; document replacements rather than implying historical recovery.
- Generate campaign-specific basic, complex, storage, tile-size, and utilization
  outputs where measurements exist. Clearly identify unavailable comparisons.
- Preserve failures/exclusions and report sample counts and variability;
  distinguish actual end-to-end timings from sums of independent tasks.
- Decouple rendering from benchmark setup side effects. Provide one documented
  route from raw records to intermediates, figures, tables, and the Quarto site.
- Keep historical outputs separate; do not overwrite published `docs/` merely
  to test a development render.

**Done when:** a small campaign fixture regenerates its report without manual
RDS/PNG copying, and missing inputs fail clearly or are explicitly labeled.

**Starter prompt:** "Implement Chat 10 from the development plan: reproducible
campaign compilation, figure/table generation, and Quarto reporting."

## Chat 11: Run and review a small end-to-end pilot

**Scope:** approved neighboring-tile subset and pilot artifacts.
**Depends on:** Chats 4-10.

- Run supported workload/tool combinations with a small core-count matrix and
  repetitions, including buffering and complex-output cases.
- Exercise failures, retries, resume behavior, validation, and monitoring.
- Regenerate the pilot report and review correctness, timings, variability,
  comparability, and manifest completeness before scaling.
- Record tool/license/data limitations and fix only pilot-blocking defects.

**Done when:** the pilot has traceable inputs and attempts, validated outputs,
reproducible reports, and an explicit readiness decision for a full campaign.

**Starter prompt:** "Execute Chat 11 from the development plan: run the approved
small pilot and resolve campaign-blocking defects."

## Chat 12: Freeze and execute the approved full campaign

**Scope:** campaign operation, archival, and publication.
**Depends on:** Chat 11 and explicit user approval of scope and compute/storage.

- Freeze code revision, dependencies, inputs, workload definitions, runtime,
  hardware/storage configuration, and the measurement matrix.
- Execute the approved repetitions without mid-campaign dependency changes;
  preserve failures, logs, validation reports, and monitoring data.
- Archive manifest/checksums/raw records, regenerate reports, review exclusions
  and uncertainty, then publish only after approval.
- Update methodology and restoration/rerun instructions to match actual results.

**Done when:** every published comparison traces to a complete campaign and its
recorded inputs, environment, validated attempts, and reporting pipeline.

**Starter prompt:** "Execute Chat 12 from the development plan after confirming
approval: freeze, run, archive, and prepare the full campaign for publication."

## Current handoff

- Created local branch `dev` from `main`.
- Read the existing audit and checked its key findings against repository code.
- Chat 1 implemented on `dev`; no commits, pushes, full campaigns, data downloads,
  or historical-result changes.
- `R/1_functions.R` now records `execution_status` (`succeeded`, `failed`, or
  `skipped`), `diagnostic`, and numeric `exit_status` alongside the existing
  numeric `success` flag. R errors and warnings retain their condition messages;
  nonzero process statuses fail explicitly. Successful R values are no longer
  confused with an error sentinel.
- Empty, whitespace-only, missing, and zero-length calls are recorded as
  unsupported/skipped (`success = 0`) before scratch reset, worker setup,
  monitoring, or tool execution. Their timestamps and output counts/sizes are
  unavailable, not successful zero-work measurements.
- `R/4_compileResults.R` reads mixed historical/new CSV schemas, preserves
  execution outcomes and diagnostics, and adds `short_duration` for elapsed
  times below 0.1 seconds without changing execution success. Legacy status is
  inferred only from its recorded success flag (`unknown` if missing); unavailable
  historical diagnostics are not invented. The existing output-count heuristic
  can still demote success, but now handles skipped/missing counts without
  erasing failed/skipped outcomes. Replacing that heuristic remains Chat 7.
- Added `tests/chat1_regression.R`. Passed
  `Rscript --vanilla tests/chat1_regression.R` using a session-local R library:
  successful/failed R calls for both R tools, parse errors, warning/error
  diagnostics, real Rscript subprocess exits 0/7, empty-call dispatch and CSV
  writing for all three tools, mixed legacy/new CSV compilation, short failed
  and successful records, missing outcomes, and output-count demotion.
  All six project R scripts and the regression script parsed; `git diff --check`
  passed.
- Validation environment: Linux, R 4.6.1. The required small test dependency set
  was initially absent and installed only in the session artifact library via
  `R_LIBS_USER`; no project dependency manifests or runtime pins were changed.
  The regression script uses existing project dependencies, not a new test
  framework, and writes/removes only temporary fixtures.
- No focused-check blockers remain. Real lidR/lasR/LAStools workloads, Windows
  execution, and the full configured compiler setup remain unverified: benchmark
  point clouds, the geospatial package stack, and LAStools are unavailable here.
  Subprocess checks used Rscript as a stand-in, and compiler fixtures bypassed
  machine-local/geospatial setup and plotting side effects.
- Chat 2 implemented on the existing `dev` worktree, preserving Chat 1 changes.
  No commits, pushes, data downloads, real benchmark campaigns, or historical
  result/publication changes were made.
- Added `benchmark.example.R` and ignored `benchmark.local.R`; alternatively use
  `LIDAR_BENCHMARK_CONFIG`. Machine paths, enabled tools, core counts, workstation
  label, and monitoring settings are separate from workload definitions. Paths
  resolve from the repository root. Input/report/scratch directories must already
  exist; configured trees must be safe and disjoint. The runner preflights every
  selected input and required native executable before processing. Missing tools
  fail explicitly; exact native tool names are required, with `.exe` appended
  only on Windows. No Windows drive paths remain hard-coded in setup.
- LAStools workloads now use executable/argument lists rather than shell strings;
  existing workload flags and run ordering were preserved. `processx` passes
  filenames individually and captures numeric exit status, stdout, and stderr.
  These streams survive CSV writing/compilation, including numeric-looking text;
  the two added column parsers in `R/4_compileResults.R` are the directly required
  schema follow-up. System information has one implementation and uses the same
  configured LAStools directory for its checked version query.
- Replaced shared output resets with unique, private run-owned scratch children.
  Cleanup verifies in-memory ownership, filesystem identity, canonical paths,
  protected trees, and absence of links/redirected paths before deleting anything.
  Roots, inputs, reports, arbitrary directories, changed handles, and replaced
  directories are refused. Outputs are counted before normal success/failure
  cleanup. Cleanup errors fail the attempt and retain diagnostic context; unsafe
  directories are left untouched, and scratch is retained if resource cleanup
  fails. This does not introduce campaign identity or immutable attempt history.
- Future plans are restored and lasR strategies unset on success/error paths;
  monitoring is started/stopped with owned process handles and process-tree
  cleanup, including startup failures. Windows monitoring checks its executable,
  script, output directory, and startup stdout/stderr before timing. Requesting
  the existing Windows monitor on Linux fails explicitly. No Linux monitoring
  substitute, new utilization semantics, repetitions, or cooldown policy was added.
- Added `tests/chat2_regression.R` and adapted Chat 1 checks to the explicit
  configuration/structured-call interface. Passed both scripts together with
  `options(warn = 2)`: executable/input/output paths with spaces and literal shell
  metacharacters; exits 0/7 and both diagnostic streams; missing executables and
  invalid paths; version-query stderr/failure; safe ownership/identity/link
  rejection and input checksums; real future-worker and monitor-stand-in cleanup;
  startup and cleanup failures; output accounting and retained diagnostics/scratch;
  mixed historical/new CSV compilation. All six project scripts, both regression
  scripts, and the configuration example parse; `git diff --check` passed.
- Checks ran on Linux/R 4.6.1 with the prior session-local library plus initially
  missing `processx`, `future`, and `fs`, installed only in session artifacts.
  The exact combined check was
  `Rscript --vanilla -e 'options(warn = 2); source("tests/chat1_regression.R"); source("tests/chat2_regression.R")'`
  with `R_LIBS_USER` pointing to this session's `files/r-library`. No lockfile,
  project package installation, runtime pinning, or environment-manager changes.
- No focused Chat 2 check blockers remain. Real lidR/lasR/LAStools workloads and
  full setup/system-information execution remain unverified without the point
  clouds, geospatial packages, benchmarkme, and LAStools. Native subprocess tests
  used Rscript/small shell stand-ins and catalog stand-ins, not geodata processing.
  lasR strategy cleanup was checked with stand-ins; Windows, PowerShell counters,
  and Windows filesystem behavior require native validation. Monitor lifecycle
  tests bypassed the Windows-only validator solely in isolated fixtures, not in
  production. Startup output is a health check, not utilization validation.
- Before a real run, supply machine-local paths and the exact installed native
  tool names. Dependency/runtime compatibility and restoration remain Chat 3;
  workload equivalence, campaign identity, artifact correctness, scheduling, and
  campaign-aware monitoring remain their separately planned chats.
- Next separate chat: Chat 3.
- Expected external decisions: target OS/R version (Chat 3), historical data and
  preparation sources (Chat 4), monitoring scope (Chat 9), full-run approval
  (Chat 12). Resolve each when needed rather than guessing now.
