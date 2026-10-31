# LidarSpeedTests 2.0 — Development Agents and Rules

**Permanent guidelines** for contributors and agents working on LidarSpeedTests 2.0.  
**Status**: Active  
**Last updated**: 2026-10-29

## Core Principles

### 1. Separation of Concerns

- **Python orchestrator** (`src/lidarbench/`): tool-neutral, language-neutral, stdlib + minimal dependencies.
- **R bridge** (`bridges/r/`): wraps and reuses legacy R code; no diverging reimplementations without approval.
- **Tool adapters** (`src/lidarbench/execution/adapters/`): plugin-based, discoverable, platform-specific code contained.
- **Legacy harness** (`R/`, `tests/testthat/`, `*.qmd`): frozen, never moved or rewritten without explicit decision.

### 2. Cross-Platform First

- All path operations use `pathlib.Path` and `lidarbench.core.paths` utilities.
- No hard-coded paths like `/home/user`, `C:\Users`, `/tmp`, `/var`.
- Platform-specific code is guarded with `platform.system()` checks or lives in separate adapter implementations.
- Tests include `@pytest.mark.platform` markers for OS-specific behavior.
- Before merging, verify behavior on at least 2 platforms (Linux native and Windows or macOS via CI).

### 3. Write-Once Records

- All campaign records (campaign.json, attempt results, events) are created exactly once.
- Never modify or re-serialize an existing record; append-only for events and logs.
- SHA-256 digests are computed at publish time and are immutable.
- Resumption logic checks identity and skips completed attempts; it does not recompute.

### 4. No Optional Runtime Dependencies in Core

- The Python core (`src/lidarbench/`) must install with `pip install -e .` without R, GDAL, PDAL, LAStools or any LiDAR library.
- Optional dependencies (for validation, monitoring, rendering) are installed via extras or entry-point plugins.
- Probe for optional dependencies at runtime; do not fail at import time.

### 5. Tool Execution is Subprocess-Only

- No benchmarked tools run in-process.
- R packages run via `Rscript`, Python libraries via `python -m <module>`, CLIs directly.
- The orchestrator process is isolated from tool crashes and memory leaks.
- Timing boundaries are consistent: orchestrator perf_counter_ns around the subprocess.

## Development Workflow

### Branch and Commit Discipline

1. **Feature branch**: Work on `feature/cross-platform-benchmark`. Do not commit directly to `main` or `dev`.
2. **Commit messages**:
   - First line: present tense, clear intent, e.g., "Add platform detection utilities".
   - Body (if multi-line): explain "why", not "what" (what is in the diff).
   - Include Co-authored-by trailer if pair programming.
3. **No force-push**: Rebase before pushing; never `git push -f` to shared branches.
4. **Atomic commits**: Each commit should pass tests and be logically self-contained.

### Testing Requirements

- **New code has tests**: 100% test coverage of new modules and functions is the goal.
- **Environment**: Use [uv](https://docs.astral.sh/uv/). `uv sync` creates `.venv/` from the committed `uv.lock`; prefix tools with `uv run`. Plain `pip install -e .` must keep working (ARCHITECTURE.md D12).
- **Dependencies**: Change them with `uv add <pkg>` / `uv add --dev <pkg>` (or edit `pyproject.toml` and run `uv lock`), and commit `pyproject.toml` and `uv.lock` together.
- **pytest passes**: Run `uv run pytest tests/python/` before pushing.
- **Linting passes**: Run `uv run ruff check src/ tests/` and `uv run black --check src/ tests/`.
- **Type hints**: Use type hints on all public functions; `uv run mypy src/` should not complain.
- **Platform test markers**: Tests requiring specific OS use `@pytest.mark.platform` and skip gracefully on others.

### Code Style

- **Python version**: 3.11+. Use `from __future__ import annotations` for forward references.
- **Formatter**: Black (line length 100).
- **Linter**: Ruff. See `pyproject.toml` for configuration.
- **Docstrings**: Google-style (PEP 257 compatible). One-liner for simple functions, full description + Args + Returns for complex ones.
- **Comments**: Only for non-obvious logic. Code should be self-documenting.
- **Imports**: Group stdlib, third-party, then local. Use `isort` (configured in ruff).

### Documentation

- **Architecture changes**: Update [design/ARCHITECTURE.md](design/ARCHITECTURE.md) or create an ADR in `design/adr/`.
- **User-facing changes**: Update [README.md](README.md) and [MIGRATION_PLAN.md](MIGRATION_PLAN.md).
- **API changes**: Update docstrings and module `__init__.py` exports.
- **New phases or milestones**: Update [MIGRATION_PLAN.md](MIGRATION_PLAN.md) acceptance criteria.

## Key Technical Rules

### 1. Workloads and Contracts

- Workloads are defined in `workloads/<suite>/` as TOML files. `workloads-v7` (default) = `workloads-v6` (definitions unchanged) plus `normalization`, a point-cloud contract (`[point_cloud]` instead of `[raster]`: LAZ per tile, structural-only, lasR `diagnostic_only` because lasR ≤ 0.21.3 writes NaN heights as corrupt Z; design/VALIDATION.md §1). `workloads-v6` = `workloads-v5` (definitions unchanged) plus `generate_DEM`, a structural-only contract (`reference.method = "none"`, `population.points = "classes"` with classes 2/9, every implementation `non_comparable`; design/VALIDATION.md §1). `workloads-v5` = `workloads-v4` plus the lidR extent rule (`extent = "header_bounds_snapped_nearest"`, accepted only under `accept_known_extent_discrepancies`; design/VALIDATION.md §3). `workloads-v4` = `workloads-v3` (= `workloads-v2`, the frozen `workloads-v1` definitions plus PDAL, with LAStools `*.tfw` world files as ignored sidecars) plus the LAStools cell rule (`pdal_binmode_floor`, upward-Y floor) and the `value_precision = 0.01` of lascanopy averages; v1–v6 are frozen.
- A workload spec is tool-neutral; implementations are tool-specific.
- Equivalence (matched_definition, diagnostic_only, unsupported, blocked) is declared in the spec, not inferred.
- Changing a workload spec or equivalence, including adding a tool's implementation, requires a new suite version (it changes the contract digest).
- The declared four-way class (`strictly_comparable`, `functionally_comparable`, `non_comparable`, `unsupported`) is derived by `WorkloadContract.declared_comparability()`; never hard-code it elsewhere.

### 2. Campaign Identity

- Campaign identity includes: code (git revision + dirty check + file digests), renv lock, config, selection, workloads, tools, datasets, environment (OS, CPU, RAM).
- Identity is computed once at campaign creation and frozen in `campaign.json`.
- Resume checks identity; if it changes, the campaign is abandoned and a new one is created.
- Do not recompute identity before every schedule entry (this was a performance bug in the R harness; see L3 in ARCHITECTURE.md).

### 3. Scratch Directory Safety

- Scratch roots cannot be filesystem root, home directory or the current working directory.
- Each run gets a `0700` (Unix) or restricted ACL (Windows) directory owned by the benchmark user.
- Cleanup requires an in-memory ownership record, device/inode match (Unix) and symlink-free tree walk.
- Incomplete cleanup is logged as a warning; the campaign continues (do not fail hard).

### 4. Attempt Lifecycle

States: `started` → `execution` → `validation` → `result` → `completed` (with optional `abandoned` tombstone on resume).

- `started.json`: entry key, sequence number, timestamp, host, PID.
- `execution.json`: exit status, timing, interpretation, exceptions, warnings.
- `validation/*.json`: structural and numerical validation reports.
- `result.json`: consolidated record with digests of all files.
- `completed.json`: receipt (SHA-256 of result.json); written last, atomic create.

A missing `completed.json` means the attempt is incomplete; resumption skips it and retries.

### 5. R Bridge Protocol

Normative description: [design/R_BRIDGE.md](design/R_BRIDGE.md). lidR and lasR are separate adapters (`lidr`, `lasr`) on `execution/adapters/rscript.py:RBridgeAdapter`.

- The adapter writes `<work_dir>/r-invocation.json` (`lidarbench.r-bridge-invocation/1`): pinned task call, inputs, output dir, threads, buffer, optional EPSG, required packages.
- `Rscript --vanilla bridges/r/runner.R --project … --renv on|off --invocation … --result …` activates renv, sources `R/1_functions.R` (never copied), extracts the call from `R/0_setup.R` without running it, refuses a call that differs from the adapter's pinned call, preloads namespaces the call would load lazily (`sf`, `terra`, …; untimed mark `namespaces_preloaded`), sets threads via `benchmark_with_resources()`, times **only the workload** via `benchmark_execute_call()`, and writes `r-bridge-result.json` atomically.
- `interpret()` reads that record. R errors, **any R warning during the workload** (exit 3), missing/inconsistent records and version drift fail the attempt; nothing is converted to success.
- Orchestrator records both outer (perf_counter_ns around subprocess, the timed interval) and inner (R Sys.time()) times plus start-up overhead (`tool_timing`). The timing-boundary label in the report says which one is used.
- lasR inputs must have `.lax` indexes (lasR would otherwise write them into the protected input tree).
- Changing a pinned call needs a new workload version; the bridge and adapter must change together.

### 5a. LAStools Adapter

Normative description: [design/ADAPTERS.md](design/ADAPTERS.md) §9 (`execution/adapters/lastools.py`).

- Discovery: `bin_dir` option → `$LIDARBENCH_LASTOOLS_BIN` → first `PATH` directory with a workload command. `.exe` names on Windows only. Never download, install, license or hard-code LAStools locations.
- Argument templates are pinned from `R/0_setup.R` (a test parses the R file). The height cutoff is derived from the input headers, never configured. `-fail` is always appended.
- Identity keeps only the build number from `-version`. Never record, hash or quote licensing text or license files.
- `interpret()` fails on nonzero exits, `ERROR`/`FATAL`, unlicensed notices, any unrecognized `WARNING:` line and missing/empty outputs. The only accepted exception is the opt-in (`accept_known_cutoff_warning`, default false) lascanopy cutoff warning for `pixel_metrics_1a`, declared through `CommandSpec.accepted_exceptions`.

### 5b. PDAL Adapter

Normative description: [design/ADAPTERS.md](design/ADAPTERS.md) §10 (`execution/adapters/pdal.py`, launcher `pdal_worker.py`).

- Discovery: `executable` option → `$LIDARBENCH_PDAL` → `pdal` on `PATH`. Never install PDAL. Probing requires PDAL >= 2.5.0 and checks stages/options with `--drivers --showjson` and `--options writers.gdal --showjson`.
- One JSON pipeline per tile (`readers.las` tile + 20 m neighbours → `filters.merge` → `writers.gdal binmode=true`, fixed `origin_x/origin_y/width/height`, `nodata=-9999`, `double`). Never use radius mode for cell statistics.
- The stdlib launcher runs `pdal pipeline` per tile (`concurrent_files`) and writes `pdal-run.json`; `interpret()` fails on any nonzero tile, error/warning line, missing output or missing `--metadata` record.
- PDAL edge ownership is `pdal_binmode_floor` (rightward X, upward Y): `functionally_comparable`, never declared identical to lidR/lasR. Do not add transformations to force agreement.

### 6. Configuration and Validation

- Campaign configuration is TOML (human-editable, version-controlled).
- Schema validation uses stdlib frozen dataclasses with explicit checks (no Pydantic or other validation library; ARCHITECTURE.md D1). Unknown keys are errors, values are never coerced (`"3"` is not `3`), every default is resolved, and each error names the offending key (for example `scheduling.cache_policy`). See `src/lidarbench/config/campaign.py`.
- Dataset provenance (archive hash, policy version, per-tile receipts) is recorded and verified.
- Frozen opt-in policies (extent discrepancies, LAStools cutoff exception, diagnostic lasR) are part of campaign identity.

### 6a. Datasets

Normative description: [design/DATASETS.md](design/DATASETS.md) (`src/lidarbench/datasets/`).

- Recipes (`datasets/recipes/*.toml`) are written by hand; manifests (`datasets/manifests/**.json`) are only produced by `lidarbench data register`/`prepare` and never edited or rewritten. Changed bytes need a new dataset version.
- Manifests store a root label and a POSIX relative path, never an absolute path; machines map labels in a data-roots file or with `--root LABEL=PATH`.
- Keep `source`, `derived` and `fixture` data separate. Never modify, repair or reprocess original or registered data.
- The Petawawa preparation stays in `R/prepare_fresh_data.R`; Python never reimplements, simplifies or changes it. Legacy receipts are read once by `bridges/r/export_petawawa_receipts.R` (base R) and cross-checked at registration.
- Downloads only for `source` recipes whose license permits redistribution, with explicit `--accept-bytes`; never in tests.

### 7. Validator Plugin Protocol

- Validators are loaded via entry points (`lidarbench.validators`); the entry-point name equals the validator's `name`.
- Each validator declares a name, version, level and applicability (`applicable(contract)`) and reports missing optional dependencies via `probe()` (→ `unavailable`), never an import error.
- Validation runs outside timing and never resamples, crops, repairs, fills or normalizes outputs. A level may pass only if every earlier level passed (`AttemptVerdicts`). See `design/VALIDATION.md`.
- numpy/rasterio/laspy are only imported lazily inside `validation/` modules, after probing; `import lidarbench.validation` must stay stdlib-only.
- `structural-v1`: checks inventory, file existence, naming, optional format sniffing.
- `r-validation-v1`: calls existing R validation logic via bridge; supports numerical and performance-only modes.
- A missing validator does not fail the campaign; attempts are marked `unverified` or `skipped` with reason.
- The runner validates every succeeded attempt before scratch cleanup and seals `validation/report.json` in the attempt (`campaign/validation.py`). Contracts are frozen in identity (`workloads` component). Verdicts are reused only for byte-identical outputs under an identical `reuse_key` (contract, tool, inputs, policies, mode, EPSG, validator versions), and the reuse is recorded; never reuse on anything weaker.

### 7a. Reporting

Normative description: [design/REPORTING.md](design/REPORTING.md) (`src/lidarbench/reporting/`).

- Reports are read-only derivations of one verified campaign; campaign record digests are compared before and after, and the output is always a new directory (never inside the campaign, never `docs/`).
- Cross-tool ratios only through `validation.comparability.speedup()` (the gate); never compute ratios elsewhere. CPU scaling stays within one tool.
- The gate applies the campaign's frozen `[validation] comparability` policy (`strict` default; `edge-tolerant` accepts only declared boundary-point differences and the `accept_known_extent_discrepancies` exception, listed per comparison). Never relax it at report time or for `unverified` results.
- `execution_only` timings (no validation ran) appear only in performance-only reports, labelled, never in comparisons.
- The aggregation layer is stdlib-only; Quarto/R are optional, probed at run time and reported as diagnostics. Pages read the tables and never recompute inclusion.
- Historical R results are imported read-only and never mapped onto lidarbench workload contracts.

### 8. Monitor Backend Protocol

- Monitor backends are loaded via entry points (`lidarbench.monitors`).
- Default backend is `psutil` (whole-machine, process-tree sampling in separate process).
- Monitors run as independent sampler processes with a JSON-lines ready/attach/stop handshake over stdin/stdout.
- Whole-machine and process-tree samples are written to separate CSV files with interval start/end (monotonic + wall ns); missing values are empty cells, never 0.
- Monitor summary includes peak, average, per-phase breakdowns and optional custom aggregations.
- On POSIX the process tree is the tool's whole session (helpers reparented away, e.g. R `parallelly` workers, are still sampled). Reports use `process_cpu_seconds` = max(exact `rusage`, sampled tree) and flag `cpu_budget_exceeded`.

### 8a. Thread budgets and supervision

- `scheduling.cpu_affinity = "pinned"` (default) pins each attempt's whole process tree to `cpus` logical CPUs, one per physical core, first core left to the orchestrator (`execution/affinity.py`; Linux `sched_setaffinity` before exec, Windows while suspended; macOS unsupported and recorded). Adapters must still request their budget (threads, `-cores`, PDAL `readers.las threads = 1`).
- If the orchestrator dies, a guardian process (`execution/guardian.py`, POSIX) kills the watched tool sessions; on Windows the Job Object has `KILL_ON_JOB_CLOSE`.

## Reviewing and Approval

### Code Review Checklist

- [ ] Commit messages are clear and atomic.
- [ ] Docstrings and types are complete.
- [ ] Tests are included and pass.
- [ ] Linting and formatting are clean.
- [ ] Cross-platform implications are considered.
- [ ] No new hard-coded paths or OS assumptions.
- [ ] Architecture is consistent with [design/ARCHITECTURE.md](design/ARCHITECTURE.md).
- [ ] If documentation or roadmap changes, they are updated.

### Escalation Path

- **Uncertain design**: Propose an ADR or update ARCHITECTURE.md for discussion.
- **Blocked or stuck**: Document the blocker and request help in a GitHub issue.
- **Major refactor**: File an issue, outline the change in ARCHITECTURE.md or ADR, get approval before major work.

## Quick Start for Contributors

```bash
# Clone and switch to feature branch
git clone https://github.com/ptompalski/LidarSpeedTests.git
cd LidarSpeedTests
git checkout feature/cross-platform-benchmark

# Install uv once (no admin rights; see https://docs.astral.sh/uv/)
curl -LsSf https://astral.sh/uv/install.sh | sh                    # Linux/macOS
# powershell -c "irm https://astral.sh/uv/install.ps1 | iex"       # Windows

# Create .venv/ with the locked dependencies and dev tools
uv sync

# Run tests
uv run pytest tests/python/ -v

# Lint and format
uv run ruff check src/ tests/
uv run black --check src/ tests/
uv run mypy src/

# Without uv (pip >= 25.1): python -m venv .venv, activate it, then
# pip install -e . --group dev

# Make changes, commit, push
git add src/...
git commit -m "Add feature X"
git push origin feature/cross-platform-benchmark
```

## FAQ and Gotchas

**Q: Can I import R or GDAL in `src/lidarbench/`?**  
A: No. R and GDAL are optional. Use the R bridge for R code; for GDAL, wait for a future validator or tool adapter.

**Q: What if a test needs a real tool (lastools, lidR)?**  
A: Use mock fixtures. For integration tests on a developer machine, use `@pytest.mark.integration` and skip in CI unless the tool is installed.

**Q: I found a bug in the legacy R harness. Should I fix it?**  
A: No. Legacy is frozen. Document it in an issue or ADR as a rationale for Python rewrite. Fix it in the Python port if applicable.

**Q: Do I need to update the R code when I change the Python core?**  
A: Only if the R bridge protocol changes (invocation.json schema, result.json format). If so, update both `bridges/r/runner.R` and the Python side in the same commit.

**Q: How do I test platform-specific code?**  
A: Use `@pytest.mark.platform`, `platform.system()` checks, and either run the tests on the target platform or mock the platform check for local testing.

---

**Document history**:
- 2026-10-28: Initial version for Phase 1 (Package Bootstrap).
- 2026-10-28: Config validation uses strict stdlib dataclasses instead of Pydantic (Task 5; ARCHITECTURE.md D1).
- 2026-10-28: Validation framework and workload contracts (Task 6; design/VALIDATION.md).
- 2026-10-28: lidR/lasR adapters through the R bridge and the adapter `interpret()` hook (Task 7; design/R_BRIDGE.md).
- 2026-10-28: LAStools native adapter and declared accepted exceptions (Task 8; design/ADAPTERS.md §9).
- 2026-10-28: PDAL adapter, `workloads-v2` suite and declared comparability classes (Task 9; design/ADAPTERS.md §10).
- 2026-10-28: Dataset manifests, recipes, data roots and `lidarbench data` (Task 10; design/DATASETS.md).
- 2026-10-29: Reporting dataset, `lidarbench report`, Quarto site and historical import (Task 11; design/REPORTING.md).
- 2026-10-29: End-to-end pilot and readiness review (Task 13; design/PILOT_REVIEW.md).
- 2026-10-29: Runner validation with verdict reuse, `workloads-v3`/`-v4`, edge-tolerant comparability, CPU pinning, session monitoring, guardian, R namespace preload, PDAL reader threads, `lidarbench preflight` (Task 13 follow-up).

**Last reviewed**: 2026-10-29
