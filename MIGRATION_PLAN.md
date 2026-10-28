# LidarSpeedTests 2.0 — Migration plan

This roadmap implements [design/ARCHITECTURE.md](design/ARCHITECTURE.md) on
branch `feature/cross-platform-benchmark`. Each phase is one reviewable task.
Each task must follow [AGENTS.md](AGENTS.md):
- add automated tests and documentation;
- leave the legacy R harness runnable;
- end with a summary of changes, tests, unresolved issues and next steps.

Do **not** start a phase until the previous phase is reviewed. Phases marked
**(authorization required)** additionally need explicit owner approval, because
they install software, use real data or run benchmarks.

## Global definition of done (every phase)

- [ ] `pytest` passes locally on Linux. The new tests cover success **and**
      failure paths.
- [ ] Code has no import of R, GDAL, PDAL, LAStools or LiDAR libraries in
      `src/lidarbench/` outside optional plugins. This is enforced by a test from
      Phase 1 onwards.
- [ ] Platform-specific code paths have tests that run on all three OSes in CI.
      If a branch is genuinely OS-only, the tests for it are explicitly skipped
      elsewhere with a reason.
- [ ] Ruff lint and format checks pass. Type hints are present on public
      interfaces.
- [ ] Documentation is updated: README section, CLI `--help`, and an ADR in
      `design/adr/` if a decision changed.
- [ ] The existing testthat suite still passes **if the phase touched any R
      file**: `Rscript --vanilla tests/testthat.R`.
- [ ] Nothing under `docs/`, `graphics/`, `_OLD/`, historical results, inputs,
      `ALS_tiles.gpkg` or `input-manifests/` changed (`git diff --stat` check).
- [ ] Nothing is pushed and no pull request is opened without approval.

---

## Phase 0 — Branch and architecture (this task)

**Deliverables:**
- `feature/cross-platform-benchmark` branch;
- `design/ARCHITECTURE.md`;
- `MIGRATION_PLAN.md`;
- `AGENTS.md`.

**Acceptance:**
- The documents have been reviewed by the owner.
- The open questions in ARCHITECTURE §15 have been answered, or deferred
  explicitly.

## Phase 1 — Python project skeleton

**Scope:**
- `pyproject.toml` (PEP 621; Python ≥ 3.11; runtime dependency `psutil`; dev
  extras: pytest, ruff, mypy optional).
- `src/lidarbench/__init__.py`, `__main__.py`, and `cli.py` (argparse) with
  `--version` and a `doctor` stub. The stub reports Python, OS, psutil, and the
  optional availability of R, Quarto and LAStools, **without failing** when
  any of them is missing.
- `tests/python/` with a conftest.
- An import-boundary test that fails if core modules import forbidden packages.
- `.github/workflows/python.yml`: an OS × Python matrix with no R or LiDAR
  tools. It is committed but not pushed.
- `.gitignore` entries for Python artifacts.

**Acceptance:**
- [ ] `pip install -e .[dev]` then `pytest` passes in a fresh virtual
      environment. The environment is created in a user-owned directory; no
      system packages are installed.
- [ ] `python -m lidarbench --version` and `lidarbench doctor` work.
      `doctor` exits 0 when R and LAStools are absent and states "unavailable"
      with reasons.
- [ ] The CI workflow is syntactically valid (validated locally, e.g. with
      `actionlint` if available, otherwise a YAML parse) and is not pushed.
- [ ] README has a short "Python framework (in development)" section; the
      legacy instructions are unchanged.

## Phase 2 — Core primitives: canonical data, paths, store, locks, scratch

**Scope:**
- `core/canonical.py`: canonical JSON and SHA-256 of objects and files
  (streaming, with MD5 as an optional extra).
- `core/paths.py`:
  - root labels;
  - root-relative POSIX paths;
  - containment and overlap with per-root case-sensitivity probing;
  - file-component validation (Windows reserved names, forbidden characters,
    control characters, `.`/`..`).
- `core/store.py`:
  - exclusive create;
  - pending → `os.replace` publish;
  - fsync;
  - read-only marking;
  - bounded retry on Windows sharing violations;
  - receipt verification.
- `core/locks.py`: campaign lock and machine lock with pid, host and create
  time; stale detection; never auto-removed.
- `core/scratch.py`: root validation, mkdtemp, ownership marker, identity
  capture, link and junction refusal, guarded rmtree, orphan listing.
- `core/clock.py`: `perf_counter_ns` and `time_ns` pairs.

**Semantics to port**, with tests mirroring
`tests/testthat/test-configuration-and-isolation.R`:
- scratch root ≠ filesystem/drive/UNC root, home or working directory;
- no overlap with protected paths;
- refusal when cleanup goes through a symlink or redirected path;
- refusal when the identity changed;
- refusal when the directory is unowned.

**Acceptance:**
- [ ] Unit tests cover:
  - overlap detection (including case variants on case-insensitive
    filesystems, which are probed);
  - symlinks (and junctions on Windows CI) inside scratch, causing refusal;
  - a marker mismatch causing refusal;
  - a replaced directory with a different inode causing refusal;
  - a read-only file inside scratch on Windows being cleaned;
  - long paths on Windows.
- [ ] The store tests show that:
  - a second publish fails;
  - a tampered file is detected;
  - a crash between pending and publish leaves no published record.
- [ ] Lock tests cover concurrent acquisition (two processes), a stale lock
      that is reported and not removed, and explicit release.
- [ ] No primitive deletes anything outside a marker-proven scratch directory.
      A test asserts this with a sentinel file next to the scratch root.

## Phase 3 — Configuration, workload registry and plugin discovery

**Scope:**
- TOML machine and campaign configs with strict schema validation (unknown keys
  are errors) and resolved-defaults output.
- Workload suite format:
  - `workloads/workloads-v1/*.toml`, containing specs, per-tool
    implementations, equivalence records and artifact specs;
  - the lidR/lasR call text and LAStools argv templates, copied verbatim from
    `R/0_setup.R`;
  - contract notes, copied verbatim from `benchmark_workload_contract()`.
- Plugin discovery through entry points (`lidarbench.plugins`) and a registry
  API.
- `bridges/r/export_contracts.R` (read-only) exports `benchmark_tasks` and
  `benchmark_contracts` as JSON. The output is stored as a golden fixture.

**Acceptance:**
- [ ] A parity test shows that the Python-loaded workloads-v1 matrix equals the
      R export exactly:
  - strict mode gives 17 runnable, 6 unsupported and 4 blocked combinations;
  - `diagnostic_lasR` gives 20/6/1;
  - statuses, notes, artifacts, bands, resolution, buffer and LAStools argv
    all match.
- [ ] Every frozen opt-in policy from `benchmark.example.R` and WORKLOADS.md is
      representable. Each defaults to `false` and is validated as a strict
      boolean.
- [ ] Invalid configs (unknown key, bad root label, relative/traversal dataset
      path, cores ≤ 0, `native_threads != 1`, unsupported `cache_policy`) are
      rejected with clear messages. This mirrors
      `benchmark_validate_config`/`benchmark_scheduling_config`.
- [ ] Example configs in `configs/examples/`, plus docs describing the
      equivalence vocabulary.
- [ ] The R export script has been run (requires the local R environment, no
      installation) **or** the fixture is marked pending with the exact
      command. The legacy R harness is unchanged.

## Phase 4 — Dataset manifests and provenance

**Scope:**
- `datasets/lasheader.py`: a stdlib LAS 1.0–1.4 public-header reader for LAS
  and LAZ. It reads version, point format, legacy and extended counts, scale,
  offset and bounds. It does not decode points.
- A manifest builder and verifier:
  - full mode (SHA-256 and MD5 of every file and companion);
  - fast mode (stat fingerprints).
- Inventory checks (exact file sets, as in `benchmark_full_inputs`).
- `bridges/r/export_preparation.R` (read-only) converts existing preparation
  receipts and `ready.rds` into a provenance JSON. A Python importer turns that
  into dataset manifests that reference the original receipts by digest.
- Port `benchmark_height_cutoff()` using header minimum Z.

**Acceptance:**
- [ ] Header-reader tests use tiny synthetic LAS files generated in the tests.
      They cover LAS 1.2 and 1.4 and a LAZ header-only fixture. Truncated,
      wrong-signature and inconsistent-count files are rejected.
- [ ] The cutoff values match golden values from `benchmark_height_cutoff()`,
      including negative and large Z.
- [ ] Verification detects:
  - modified bytes (full mode);
  - modified size or mtime (fast mode);
  - missing or extra files;
  - duplicate stems;
  - a missing `.lax` companion when it is required.
- [ ] The importer never writes into the preparation root. A test asserts the
      bytes and mtimes of the fixture root are unchanged.
- [ ] (Authorization required for real data) A read-only import of the existing
      verified preparation produces manifests whose MD5s match all R receipts.
      This is optional in this phase.

## Phase 5 — Campaign identity, schedule and attempt store

**Scope:**
- Identity components and digest:
  - frozen components versus provenance-only components (ARCHITECTURE §9.1);
  - a component-level diff on mismatch.
- `schedule-v2`:
  - SplitMix64 + Fisher–Yates;
  - warm-up and measurement phases;
  - rotated tool positions;
  - a shuffled selection order.
- The attempt lifecycle:
  - `started.json`, `invocation.json` and `result.json`;
  - `completed.json` receipts;
  - `abandoned.json` tombstones;
  - the `events.jsonl` sequence.
- Resume rules: completion predicate per frozen `validation_mode`; warm-ups
  rerun; `--retry-failed`.
- Code identity based on content digests. Dirty-tree refusal unless
  `--allow-dirty`.

**Acceptance:**
- [ ] Golden schedule tests: identical output on all three OSes and all
      supported Python versions for fixed seeds.
- [ ] Property tests:
  - each tool occupies each position equally often when the repetition count
    is a multiple of the tool count;
  - every (workload, run, tool, repetition) appears exactly once per phase;
  - `warmups = 0` is supported.
- [ ] Identity tests:
  - any change to config, policy, workload, tool executable, dataset manifest
    or code-file content ⇒ mismatch, naming the component;
  - a git commit or branch switch without content change ⇒ **no** mismatch;
  - the revision is still recorded.
- [ ] Attempt tests:
  - a double publish fails;
  - a tampered result, validation or monitor file is detected on read;
  - an incomplete attempt gets a tombstone on resume, and the original files
    are byte-identical before and after;
  - a policy mismatch causes refusal (accepted exception without opt-in,
    warning exception without opt-in, validation mode differing from the
    campaign). These cases port the checks in `benchmark_read_attempt`.
- [ ] Per-attempt checks don't rehash full inputs. A test counts hashing calls.
      A full rehash happens at start, resume and end.

## Phase 6 — Process execution and the generic CLI adapter

**Scope:**
- `execution/process.py`:
  - argv-only launch;
  - absolute executable resolution with `PATHEXT`;
  - process-group or Job Object setup;
  - streamed logs;
  - timeouts;
  - tree termination;
  - Ctrl+C handling;
  - env delta recording.
- `ToolAdapter` protocol.
- `cli` adapter with configurable success rules.
- The sequential campaign runner loop (schedule → attempt → execute →
  structural validation → cooldown).

**Acceptance:**
- [ ] Fake-tool tests (Python scripts installed as executables in a temp dir)
      cover:
  - exit 0/1/7;
  - large stdout and stderr (no deadlock);
  - non-UTF-8 bytes;
  - arguments containing spaces, `;` and quotes, passed literally (port of
    `tile;literal.laz`);
  - a missing executable, rejected at preflight;
  - an unknown placeholder, rejected.
- [ ] A timeout kills a fake tool that spawns grandchildren. A test verifies
      no surviving pids on all three OSes.
- [ ] Interrupting a running attempt yields `interrupted` or a tombstone, and
      the locks are released.
- [ ] Inner and outer timings are recorded as integers in nanoseconds. Wall
      timestamps are sub-second.
- [ ] An end-to-end mini campaign runs with fake tools on all CI OSes. It
      initializes, preflights, runs, resumes, and resumes again with no new
      work.

## Phase 7 — Resource monitoring (psutil)

**Scope:**
- Sampler process with a ready/stop handshake; whole-machine and process-tree
  scopes.
- A port of `benchmark_monitor_summary()` (weights, coverage, partial status,
  validation of sample tables).
- Per-metric availability.

**Acceptance:**
- [ ] Summary tests reuse the synthetic sample cases from
      `tests/testthat/test-monitoring.R`. Results are equal within 1e-12.
- [ ] Live tests (short, a few seconds) on all CI OSes give whole-machine
      samples, process-tree samples for a fake CPU-burning child tree, and
      coverage ≥ 0.9 of the timed interval.
- [ ] Monitor failure produces `monitoring_status = unavailable` with a reason,
      and the attempt is still recorded. Missing metrics are `null`, never 0.
- [ ] Docs list the per-OS metric availability matrix.

## Phase 8 — Validation framework and status model

**Scope:**
- Four verdict fields (ARCHITECTURE §8.1) and the legacy mapping.
- `Validator` protocol.
- `structural-v1` (inventory by tile stem, nonempty files, magic bytes,
  halo/duplicate detection).
- Policy objects for frozen opt-ins.
- `performance-only` versus `numerical` validation modes.

**Acceptance:**
- [ ] Tests port the inventory cases from `tests/testthat/test-validation.R`:
  - missing files;
  - unexpected files;
  - duplicates;
  - zero-byte files;
  - VRT and sidecar files not satisfying requirements;
  - LAStools percentile suffix spellings.
- [ ] Without a numerical validator, a structurally valid output gives
      `numerical_status = unverified` and **never** `passed`. A test asserts
      that no code path promotes it.
- [ ] `accepted_exception` is reachable only with the matching frozen policy.
      A test asserts it is never counted as `passed`.

## Phase 9 — LAStools adapter

**Scope:**
- A port of:
  - `benchmark_lastools_command` (placeholders, `-epsg`/`-fail` when
    metadata is declared);
  - `ERROR`/`FATAL` detection with a zero exit status;
  - unrecognized `WARNING:` lines as diagnostics;
  - the exact three-line `lascanopy` cutoff-warning exception, behind its
    frozen opt-in.
- Version identity with banner normalization and executable SHA-256.
- Platform availability (Linux and Windows binaries; macOS reported as
  unavailable unless the vendor binaries are configured and probe
  successfully).

**Acceptance:**
- [ ] Ported tests from `test-execution.R` and `test-policies.R` cover:
  - the complete warning block being accepted only with the opt-in, status
    0 or 1, the matching cutoff and a supplied input;
  - incomplete blocks, a different tool or task, a generic prefix, or a
    mismatched cutoff being rejected;
  - status 1 without a recognized warning failing;
  - `ERROR:` with status 0 failing.
- [ ] Argv for every workloads-v1 LAStools call equals the R-generated argv
      for identical inputs (golden fixture exported from R).
- [ ] No real LAStools run is required. An optional `requires_lastools` test
      is skipped unless explicitly enabled.

## Phase 10 — R bridge adapter (lidR, lasR)

**Scope:**
- `bridges/r/runner.R`:
  - JSON in/out;
  - renv activation;
  - reuse of `prepare_input`, the thread policy, the reference-BLAS check,
    CRS declaration and the lasR `set_crs` wrapper by sourcing `R/1_functions.R`;
  - an inner timing boundary;
  - the existing `.lax` refusal for declared inputs.
- `bridges/r/identify.R` (R version, package versions and file hashes, renv
  lock hash, sf external software).
- `rscript` adapter in Python. `doctor` integration with
  `R/check_environment.R`.

**Acceptance:**
- [ ] Protocol tests with a fake `Rscript` (Python stand-in) run on all OSes
      without R.
- [ ] `requires_r` tests run on the local Linux machine with the existing renv
      library (no installation). They run lidR and lasR basic workloads on the
      tiny synthetic fixtures from `helper-fixtures.R`/`test-workloads.R` and
      produce the same artifacts as the legacy harness.
- [ ] testthat tests for `runner.R` (malformed JSON, unknown task, blocked
      call refused, thread variables applied) are added under `tests/testthat/`
      and pass. No existing R file changes behaviour.
- [ ] The R tool identity is frozen in campaign identity. A changed package
      file ⇒ mismatch.

## Phase 11 — R numerical validator plugin (`r-validation-v1`)

**Scope:**
- `bridges/r/validate.R` wraps `benchmark_validation_manifest` and
  `benchmark_validate_artifacts` and emits a JSON report.
- A Python plugin maps the report to the verdict fields and records
  `boundary_difference_points`, extent deltas and exception reasons.

**Acceptance:**
- [ ] On the synthetic fixtures, verdicts and details equal those the legacy
      harness records, in both `numerical` and `performance-only` modes.
- [ ] The validation report is immutable and digested in the attempt. A
      tampered report is detected.
- [ ] Validation runs outside the timed interval. A test asserts that the
      timing does not include validation.

## Phase 12 — Extensibility proof: Python library and additional CLI

**Scope:**
- `pylib` adapter (`python -m <module>` in a configured interpreter).
- A toy reference workload implemented with stdlib-only Python, to prove the
  path end-to-end.
- A PDAL CLI adapter skeleton with argv templates, marked `non_comparable`
  until a contract is reviewed.

**Acceptance:**
- [ ] A new tool is added **without core edits**: one plugin module, one
      entry-point line and TOML data. A test asserts that the registry
      discovers it.
- [ ] A toy-workload campaign runs on all CI OSes.
- [ ] PDAL and other third-party tools are not installed. Real-tool tests are
      opt-in and skipped by default. (Installing them requires authorization.)
- [ ] New implementations default to `non_comparable`. A test asserts that
      adding an implementation never auto-joins an equivalence group.

## Phase 13 — Reporting

**Scope:**
- Compile campaign records into a ledger (CSV/JSON), with every attempt
  including failures, skips, tombstones and warm-ups.
- Port the `benchmark_report_prepare` rules:
  - latest attempt by event sequence;
  - exclusion reasons;
  - the sub-0.1 s flag;
  - `n_*` counters;
  - the validated and performance-only modes.
- Comparability gate (ARCHITECTURE §8.3) with descriptive versus comparable
  tables.
- A static HTML/Markdown renderer and a build receipt.
- An optional Quarto renderer (only if Quarto is present).

**Acceptance:**
- [ ] Tests port `tests/testthat/test-reporting.R` cases:
  - superseded retries;
  - a failed last retry not falling back to an earlier success;
  - warm-ups excluded;
  - single-sample SD is NA;
  - performance-only `n_comparable == 0`;
  - accepted exceptions are descriptive.
- [ ] Any attempt to compute ratios or speedups across a failed gate raises.
      Mixed timing boundaries are rejected.
- [ ] A report build fails if campaign records change during the build or the
      campaign lock is held. Builds go into fresh directories and never
      overwrite earlier ones.
- [ ] Reports never write to `docs/` or `gh-pages`. A test asserts the output
      path is inside the campaign.

## Phase 14 — Legacy interoperability (read-only)

**Scope:**
- `bridges/r/export_legacy.R` converts a legacy R campaign (manifest, attempts,
  validation and monitoring receipts) into 2.0 JSON in a **separate output
  directory**. Raw files are verified and never modified.
- A Python importer labels the result `origin = legacy-r` and maps the fields
  (ARCHITECTURE §8.1).
- The legacy CSV mode stays in R.

**Acceptance:**
- [ ] Export on fixture campaigns created by the legacy test suite round-trips.
      Python reports the same counts as `R/5_report.R` for those fixtures.
- [ ] The source campaign directory checksums are identical before and after.
- [ ] Legacy and 2.0 campaigns are never merged into one comparison table.

## Phase 15 — Cross-platform hardening

**Scope:**
- Run the full fake-tool suite on the Windows and macOS CI runners.
- Fix platform issues. Document platform caveats in the README.

**Acceptance:**
- [ ] CI is green on ubuntu, windows and macos for all supported Python
      versions. (Pushing to run CI requires approval; until then, record
      local Linux results and pending CI status.)
- [ ] A manual smoke test on native Windows and macOS (with the owner) uses
      fake tools: doctor, init, preflight, run, resume, report.
- [ ] Platform availability matrix in the README (tools, monitoring metrics,
      junction handling).

## Phase 16 — Parity pilot (authorization required)

**Scope:**
- On a staged copy of the approved two-tile pilot pair, run the legacy R
  harness and the 2.0 framework with the same workloads, tools, cores and
  policies.

**Acceptance:**
- [ ] Explicit owner authorization recorded before running.
- [ ] Execution and verdict outcomes match per (workload, tool, cores), or the
      differences are explained and approved.
- [ ] Timing differences are reported per boundary kind. No cross-framework
      speedup claims are made.
- [ ] Inputs are verified unchanged (full rehash before and after).

## Phase 17 — Documentation, deprecation and release readiness

**Scope:**
- Restructure the README: a 2.0 quick start, and the legacy harness under a
  clearly labelled section.
- User guide for adding tools and workloads.
- Mark the R runners as legacy in their help text, only if no legacy campaign
  is still active (this changes R identity). Decide on relocation per
  ARCHITECTURE §10.2.

**Acceptance:**
- [ ] A new contributor can add a CLI tool by following the docs (dry run by
      the owner).
- [ ] The owner decides on relocation, the published-site update process and
      merging. No merge, push or publish happens without approval.

---

## Sequencing and dependencies

```text
P1 → P2 → P3 → P4 → P5 → P6 → P7
                         ↘ P8 → P9
                              → P10 → P11
                   P6 + P8 → P12
              P5 + P8 → P13 → P14
         (P6..P14) → P15 → P16 → P17
```

## Risks and mitigations

| Risk | Mitigation |
|---|---|
| Changes in the main checkout break an active legacy R campaign (identity includes HEAD and R files) | Develop in a separate git worktree. Never commit or switch branches in a checkout an R campaign runs from (AGENTS.md). Phase 5 removes live HEAD checks in 2.0. |
| Semantic drift between R and Python ports | Golden fixtures exported from R. Parity tests for the contracts matrix, argv, cutoff, monitor summary and report counters. |
| Windows process-tree and file-locking edge cases | Job Objects, bounded publish retries, CI on windows-latest, a manual smoke test. |
| Over-claiming comparability | The gate is enforced in code. New implementations default to `non_comparable`. There is no speedup API across a failed gate. |
| Scope creep into re-implementing validators | R stays the numerical authority until the owner decides otherwise (ARCHITECTURE §15 Q3). |
| Data or provenance damage | Importers are read-only, inputs are fingerprinted before and after, and no downloads happen without authorization. |
