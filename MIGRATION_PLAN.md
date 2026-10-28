# LidarSpeedTests 2.0 — Migration Plan

**Status**: In progress  
**Version**: Task 1 (Python package bootstrap) ✅ **DONE**; Task 3 (tool adapter interface) ✅ **DONE**  
**Branch**: `feature/cross-platform-benchmark`  
**Related**: [ARCHITECTURE.md](design/ARCHITECTURE.md), [AGENTS.md](AGENTS.md)

## Executive Summary

This document defines the roadmap for migrating LidarSpeedTests from an R-only harness to a Python-orchestrated, language-agnostic benchmarking framework. The migration preserves the evidence model, operational semantics and validation logic of the current system while enabling cross-platform native execution and support for any tool—R packages, Python libraries, CLIs, LAStools, PDAL, etc.

**Core principle**: New orchestration code is written in Python. Legacy R harness, validators and reporting remain frozen. They are wrapped via an R bridge for continued use until ported.

## Roadmap

### Phase 1: Package Bootstrap ✅ **DONE**

**Objective**: Create a clean, installable Python package that is the foundation for the orchestrator.

**Deliverables**:
- [x] Python package under `src/lidarbench/` with modern `pyproject.toml`.
- [x] Entry point `lidarbench` CLI with `--help` and `version` commands.
- [x] Core modules scaffolding: config, workloads, adapters, execution, monitoring, validation, reporting, campaign, datasets, legacy.
- [x] Cross-platform utilities: path handling, platform detection, executable discovery.
- [x] pytest test suite with unit, integration and platform-specific tests.
- [x] `pip install -e .` succeeds without R, GDAL, PDAL or LAStools.
- [x] CLI works from arbitrary working directory.
- [x] Type hints and Ruff linter configuration.
- [x] ARCHITECTURE.md, MIGRATION_PLAN.md, AGENTS.md documentation.

**Acceptance Criteria** ✅:
- [x] Package installs and imports without errors.
- [x] `lidarbench --help` and `lidarbench version` work.
- [x] Placeholder commands (init, preflight, run, etc.) fail gracefully with "not yet implemented".
- [x] pytest passes all unit and integration tests.
- [x] No hard-coded paths or Unix-only assumptions.
- [x] No R dependency at installation time.

### Phase 2: Core Data Model and Store

**Objective**: Implement campaign, attempt and result records (JSON/CSV).

**Key tasks**:
- Canonical JSON serialization with SHA-256 digests.
- Campaign identity by content, not checkout state.
- Write-once attempt store with receipt verification.
- Append-only events log.
- Schedule and result CSV formats.
- Locks for campaign and machine execution.
- Scratch directory lifecycle and validation.

**Acceptance Criteria**:
- Campaign store can be read/written without R.
- Legacy R campaigns can be imported.
- All records have explicit schemas.
- Tests include round-trip serialization.

### Phase 3: Tool Adapters and Process Execution

**Objective**: Implement subprocess execution with plugin architecture.

**Progress — Task 3 (adapter interface) ✅ DONE**: language-independent adapter
contract `lidarbench.adapter/1` ([design/ADAPTERS.md](design/ADAPTERS.md)),
entry-point registry, core-side planning with explicit `skipped_unavailable` /
`skipped_unsupported` / `skipped_blocked` outcomes, argv helpers for native,
Rscript, Python-module and config-file (PDAL-style) tools, a deterministic
`synthetic` adapter, and `lidarbench check`. No real LiDAR adapters yet.

Task 3 acceptance criteria:
- [x] New adapters are added through the `lidarbench.adapters` entry point without core changes.
- [x] Missing software is reported as unavailable (with a reason), not as a benchmark failure.
- [x] Unsupported and blocked workload combinations are explicit, with reasons.
- [x] Tests cover discovery, capability reporting, argv construction and paths with spaces.

**Key tasks**:
- [x] Base adapter protocol and plugin discovery.
- Generic CLI adapter with configurable success rules.
- LAStools adapter with version/banner normalization and cutoff-warning exception.
- R bridge (Rscript) adapter for lidR/lasR.
- Python library adapter (pylib) for Python tools.
- Process launch, timeout, tree termination, log capture.
- Path templating (inputs, outputs, cores, height_cutoff).

**Acceptance Criteria**:
- Adapters are loaded via entry points.
- Platform-specific tool discovery works (which, PATH, etc.).
- Mock tools can be tested via fixtures.
- Process timeout and termination work on all platforms.

### Phase 4: Workload and Dataset Management

**Objective**: Define workloads and datasets; establish equivalence.

**Key tasks**:
- Workload spec schema (inputs, outputs, algorithm, parameters, grid, buffer, timing boundary).
- Implementation per tool (argv templates, threading policy, exception policies).
- Equivalence registry (matched_definition, diagnostic_only, unsupported, blocked).
- Dataset manifest (LAS/LAZ files, metadata, provenance).
- LAS header reader (no external dependency).
- Dataset verification and readiness receipts.

**Acceptance Criteria**:
- Workloads-v1 contracts are represented in TOML.
- Equivalence matrix can be queried programmatically.
- Dataset manifests survive round-trip.

### Phase 5: Campaign Scheduling and Execution

**Objective**: Run a campaign with scheduling, resumption and attempt tracking.

**Key tasks**:
- Campaign creation and identity freezing.
- Scheduling: warm-up runs, then measurements; per-task base permutation + rotation.
- Attempt lifecycle: started → invocation → execution → validation → result → completed.
- Resume logic: checks identity, skips completed attempts, retries failed ones.
- Resource monitoring via psutil backend (process-tree sampling in separate process).
- Tool-specific monitoring backends (stub for future distributed/hardware monitors).

**Acceptance Criteria**:
- Campaign runs on a single machine, one tool at a time.
- Scheduling matches R harness behavior (with seeded RNG).
- Resumption restarts only from failures.
- Monitoring samples are recorded with checksums.

### Phase 6: Validation and Reporting

**Objective**: Validate results and compile campaign reports.

**Key tasks**:
- Structural validator (Python): artifact inventory, file format sniffing.
- R validation bridge: call existing `benchmark_validate_artifacts()` in numerical or performance-only mode.
- Numerical, structural and comparability verdicts as separate fields.
- Report compilation: select latest attempt per key, apply exclusion reasons, compute summaries.
- Report renderers: static HTML/Markdown; optional Quarto integration.
- Legacy result importer.

**Acceptance Criteria**:
- Structural validation works on output artifacts.
- R bridge passes all existing validation tests.
- Reports produce tables (CSV/JSON) for external analysis.
- Known cross-tool differences can be documented without blocking reports.

### Phase 7: Preflight and Doctor Commands

**Objective**: Pre-run validation and diagnostic tooling.

**Key tasks**:
- Preflight: check configuration, stage inputs, verify tools, dry-run workloads.
- Doctor: probe environment, check capabilities, diagnose missing dependencies.
- Configuration validation (TOML/YAML with Pydantic).
- Dataset readiness checks.
- Tool availability and version reporting.

**Acceptance Criteria**:
- Preflight prevents misconfiguration from blocking the campaign mid-run.
- Doctor output can be captured for issue reporting.

### Phase 8: Documentation, Examples and Adoption

**Objective**: Consumable documentation and example workflows.

**Key tasks**:
- User guide (initialization, configuration, monitoring, troubleshooting).
- Developer guide (adding tools, validators, monitors, custom workloads).
- Example configurations (single machine, different core counts, datasets).
- Example command sequences (fresh data → preflight → run → report).
- Architecture decision records (ADRs).
- Migration guide for legacy campaigns.

**Acceptance Criteria**:
- First-time users can run a pilot campaign from the guide.
- New developers can add a tool adapter by following the template.

### Post-Phases: Extensions and Optimization

**Future scope** (not in 2.0 initial release):
- Distributed campaigns (multi-machine scheduling).
- Web dashboard for monitoring.
- Performance profiling and optimization.
- Additional tool adapters (PDAL, CloudCompare, etc.).
- Hardware monitoring (power, thermal, network I/O).
- Custom validators (py-geo using rasterio/laspy).
- CI/CD integration (GitHub Actions, GitLab CI).

## Key Architectural Decisions

1. **Python for orchestration, any language for tools**: The core is Python stdlib + Pydantic/psutil. Tools run as subprocesses. R code runs via Rscript. No in-process execution.

2. **Language-neutral records**: JSON, JSON Lines and CSV with explicit schemas. Any language can read them.

3. **Content-based identity**: Campaign identity is frozen from code/config/data content hashes, not live git checks. Fixes performance and safety issues in the R harness.

4. **Write-once, append-only stores**: Attempts are immutable. Events are append-only. Enables safe resumption and forensics.

5. **Separate verdicts**: Execution, structural validation, numerical validation and comparability are separate fields with separate producers.

6. **Fail closed**: Unknown states are errors. Skips are recorded as attempts. Failures are retained. Unknown capabilities are reported with reasons.

7. **Reuse before rewrite**: Mature R validators and preparation logic are wrapped via a bridge. Python reimplementation is justified only when the orchestrator itself needs the behavior.

## Testing Strategy

- **Unit tests** (pytest): Fast, isolated, no external tools.
- **Contract tests** (pytest + fixtures): Mock tools and configs; verify adapter/validator protocols.
- **Integration tests** (pytest + real tools): On a developer machine with a small prepared dataset (pilot subset).
- **Platform tests** (@pytest.mark.platform): Linux, Windows, macOS specific tests.
- **Acceptance tests** (manual): Full campaign run on target platforms; compare with legacy results.

## Migration Path for Existing Users

1. **Phase 1-2** (now): Legacy R harness remains fully functional. Python package can be installed alongside.
2. **Phase 3-5**: Python orchestrator is feature-complete for basic campaigns. Users can opt-in to try it.
3. **Phase 6-7**: Validation and reporting match R harness output. Equivalence is declared and tested. Full migration is recommended.
4. **Post-Phase 8**: Legacy harness is maintained for backward compatibility but not developed further.

## Success Criteria (Overall)

- ✅ Phase 1: Package installs and basic CLI works.
- [ ] Phase 5: A campaign can be executed end-to-end via Python.
- [ ] Phase 7: Reports from Python campaigns match legacy format.
- [ ] Phase 8: New contributors can run and extend the benchmark without R knowledge.

---

**Next Step**: Move to Phase 2 (Core Data Model and Store).
