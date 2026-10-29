# LidarSpeedTests 2.0 — Development Agents and Rules

**Permanent guidelines** for contributors and agents working on LidarSpeedTests 2.0.  
**Status**: Active  
**Last updated**: 2026-10-28

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

- Workloads are defined in `workloads/workloads-v1/` as TOML files.
- A workload spec is tool-neutral; implementations are tool-specific.
- Equivalence (matched_definition, diagnostic_only, unsupported, blocked) is declared in the spec, not inferred.
- Changing a workload spec or equivalence requires a new version (workloads-v2).

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

- Orchestrator writes `invocation.json` with task call, inputs, outputs, cores, thread policy, CRS, flags.
- `bridges/r/runner.R` activates renv, runs the call, times **only the workload**, writes `result.json`.
- Orchestrator records both outer (perf_counter_ns around subprocess) and inner (R Sys.time()) times.
- The timing-boundary label in the report says which one is used.
- Bridge code sources (does not copy) functions from `R/1_functions.R` to avoid divergence.

### 6. Configuration and Validation

- Campaign configuration is TOML (human-editable, version-controlled).
- Schema validation uses stdlib frozen dataclasses with explicit checks (no Pydantic or other validation library; ARCHITECTURE.md D1). Unknown keys are errors, values are never coerced (`"3"` is not `3`), every default is resolved, and each error names the offending key (for example `scheduling.cache_policy`). See `src/lidarbench/config/campaign.py`.
- Dataset provenance (archive hash, policy version, per-tile receipts) is recorded and verified.
- Frozen opt-in policies (extent discrepancies, LAStools cutoff exception, diagnostic lasR) are part of campaign identity.

### 7. Validator Plugin Protocol

- Validators are loaded via entry points (`lidarbench.validators`); the entry-point name equals the validator's `name`.
- Each validator declares a name, version, level and applicability (`applicable(contract)`) and reports missing optional dependencies via `probe()` (→ `unavailable`), never an import error.
- Validation runs outside timing and never resamples, crops, repairs, fills or normalizes outputs. A level may pass only if every earlier level passed (`AttemptVerdicts`). See `design/VALIDATION.md`.
- numpy/rasterio/laspy are only imported lazily inside `validation/` modules, after probing; `import lidarbench.validation` must stay stdlib-only.
- `structural-v1`: checks inventory, file existence, naming, optional format sniffing.
- `r-validation-v1`: calls existing R validation logic via bridge; supports numerical and performance-only modes.
- A missing validator does not fail the campaign; attempts are marked `unverified` or `skipped` with reason.

### 8. Monitor Backend Protocol

- Monitor backends are loaded via entry points (`lidarbench.monitors`).
- Default backend is `psutil` (whole-machine, process-tree sampling in separate process).
- Monitors run as independent sampler processes with a JSON-lines ready/attach/stop handshake over stdin/stdout.
- Whole-machine and process-tree samples are written to separate CSV files with interval start/end (monotonic + wall ns); missing values are empty cells, never 0.
- Monitor summary includes peak, average, per-phase breakdowns and optional custom aggregations.

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

**Last reviewed**: 2026-10-28
