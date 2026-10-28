# AGENTS.md — Permanent development instructions

These rules apply to every contributor and AI agent working on LidarSpeedTests
2.0. They override convenience. If a task conflicts with them, stop and ask the
owner (@ptompalski).

Read first: [design/ARCHITECTURE.md](design/ARCHITECTURE.md) and
[MIGRATION_PLAN.md](MIGRATION_PLAN.md). Do one migration phase per task, and do
not start the next phase without review.

## 1. Branch and repository hygiene

- Work only on `feature/cross-platform-benchmark` (or a branch the owner
  explicitly creates from it). Never commit to `main`, `dev` or `gh-pages`.
- Do **not** push, open pull requests, merge, rebase shared branches or
  rewrite history without explicit approval.
- Before switching branches or committing, check for uncommitted changes. Never
  discard, overwrite, stash or move someone else's work without approval.
- **Active legacy campaigns.** The legacy R harness freezes `git rev-parse HEAD`
  and the contents of `R/*.R`, `DESCRIPTION`, `WORKLOADS.md` and `renv.lock`.
  It rechecks them before every schedule entry. Before any git operation that
  changes HEAD (commit, checkout, switch, reset) or any edit of those files,
  check for running benchmark processes in that checkout:
  - `ps -eo pid,args | grep -E 'R/(3_runBenchmarks|6_pilot)\.R'`;
  - `readlink /proc/<pid>/cwd`.

  If one is running there, do not touch that checkout. Use a separate
  `git worktree`. The 2.0 development worktree is
  `/home/ptompals/LidarSpeedTests-cross-platform`.
- Include the `Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>`
  trailer on AI-authored commits.

## 2. Protected content — never modify, delete or regenerate

- Historical benchmark results and campaign directories, including legacy
  `results/`, `system_monitoring/`, and any `<reports>/campaigns/*` outside a
  test's temporary directory.
- The published website: `docs/`, the `gh-pages` branch and `graphics/`.
- Archived material: `_OLD/`.
- Input data:
  - prepared and original point clouds;
  - preparation roots and receipts;
  - `ALS_tiles.gpkg`;
  - `input-manifests/`;
  - anything under configured `input_roots` or `protected_roots` (for example
    `/data/nvme1/LidarSpeedTestData` and `/data/nvme2/LidarSpeedTestWorkingDir/*`).
- Legacy R identity files (`R/`, `DESCRIPTION`, `WORKLOADS.md`, `renv.lock`,
  `renv/`):
  - edit them only with owner approval;
  - edit them only when no legacy campaign runs from that checkout;
  - document that the edit invalidates resumption of existing legacy
    campaigns.

Importers and exporters must be read-only with respect to their sources. Tests
must assert that source bytes are unchanged.

## 3. Authorization required (ask first, every time)

- Downloading datasets or any large file.
- Installing system software, R/Python packages into shared or system
  locations, LAStools, PDAL, Quarto or compilers. A project-local Python
  virtual environment for running tests is allowed. State what is installed and
  where.
- Running benchmarks:
  - any campaign on real data;
  - pilot and parity runs;
  - full benchmarks;
  - anything that runs LAStools on real inputs.

  Tiny synthetic fixtures in temporary directories are allowed.
- Modifying inputs, preparation roots, campaign records or published output.
- Changing campaign identity semantics, workload contracts, equivalence groups,
  validation tolerances or exception policies.

## 4. Architecture rules

- **The Python core is independent.** `src/lidarbench/` (outside optional
  plugin modules) must not import or require R, GDAL, PDAL, LAStools, rasterio,
  laspy, numpy or any LiDAR library. Its runtime dependencies are the standard
  library and `psutil`. An import-boundary test enforces this; keep it passing.
- **Native cross-platform.** Code must work on Windows, Linux and macOS
  without WSL, containers, Cygwin/MSYS or a POSIX shell. In practice:
  - use `pathlib`/`os.path`;
  - use argv lists and never `shell=True`;
  - pass `encoding="utf-8"` explicitly, and `newline=""` for CSV;
  - avoid platform-specific commands; where an OS needs special handling,
    abstract it with a tested branch for each OS.
- **Benchmarked tools always run in child processes.** Never import a
  benchmarked library into the orchestrator.
- **Records are language-neutral and write-once:** JSON, JSONL or CSV with a
  schema version and SHA-256. Use exclusive create and atomic publish. Never
  edit or delete a published record. Corrections are new records (for example
  tombstones).
- **Reuse before rewrite.** Wrap mature R functionality (validation-v1,
  Petawawa preparation, thread policy, workload calls) through `bridges/r/`.
  Do not rewrite code merely to change languages. A port is justified only
  when the orchestrator needs the behaviour, and it requires parity tests
  against golden output from the R implementation.
- **Plugins, not core edits.** New tools, validators, monitors and renderers
  are plugins discovered through entry points, plus data in `workloads/`.
- **Safe scratch only.** Delete only marker-proven, run-owned scratch
  directories after validating:
  - the root;
  - the ownership token;
  - the device/inode identity;
  - the absence of links, junctions and reparse points.

  Never delete user-supplied paths, roots or anything outside scratch. Never
  follow links during deletion.
- **Locks are never auto-removed.** Report stale locks; removal requires an
  explicit, confirmed command.
- **Identity by content.** Campaign identity must change when inputs, code
  content, config, policies, workloads or tool binaries change. It must not
  change on unrelated git operations.

## 5. Evidence and reporting rules

- Keep these four verdicts separate:
  - **execution success**;
  - **structural validation**;
  - **numerical validation**;
  - **cross-tool comparability**.

  Never derive one by relabelling another. Never promote `unverified`,
  `accepted_exception` or `diagnostic_only` to `passed` or `comparable`.
- **Never present non-equivalent workloads as directly comparable.**
  Cross-tool comparisons, ratios and speedups are allowed only through the
  comparability gate (ARCHITECTURE §8.3):
  - same matched equivalence group and spec digest;
  - numerical `passed`;
  - zero boundary differences;
  - same dataset, run dimensions and machine;
  - same timing boundary;
  - validated report mode.

  Everything else is labelled descriptive.
- New tool implementations default to `non_comparable`. Joining an
  equivalence group needs a documented contract review, evidence and owner
  approval.
- Every attempt is retained, including failures, skips, timeouts,
  interruptions and warm-ups. Exclusions are explicit reasons, not omissions.
- Exception policies are explicit, default `false` and frozen in campaign
  identity. Changing one requires a new campaign.
- Do not mix legacy (R) and 2.0 campaigns, or historical results, in one
  comparison table.

## 6. Every implementation task must include

1. **Automated tests** (pytest under `tests/python/`; testthat under
   `tests/testthat/` for R bridge code):
   - cover success **and** failure paths;
   - use tiny synthetic fixtures and fake tool executables;
   - never use real data;
   - include platform-specific branches.
2. **Documentation:** README section and/or `design/` updates, CLI help, and an
   ADR in `design/adr/` when a decision changes.
3. **Validation run:** the smallest command that covers the change, plus the
   full relevant suite before handing back.
   - Python: `python -m pytest tests/python` (plus `ruff check .` and
     `ruff format --check .` once configured).
   - R, only if R files were touched: `Rscript --vanilla tests/testthat.R`.
     For a focused run, use `Rscript --vanilla -e 'source("renv/activate.R");
     testthat::test_dir("tests/testthat", filter = "<name>")'`.
4. **A protected-path check:** `git status` and `git diff --stat` show no
   changes under protected paths (§2).

## 7. End-of-task summary (mandatory)

Finish every task with:

- **Changes:** files added or modified, and the behaviour introduced.
- **Tests performed:** exact commands and results, plus anything not run and
  why (for example, Windows/macOS CI pending because pushing needs approval).
- **Unresolved issues:** known limitations, risks and open questions.
- **Recommended next steps:** normally the next migration phase. Do not start
  it.

## 8. Practical notes

- R 4.6.1 with `renv` (lidR 4.3.3, lasR 0.21.2) is the pinned R environment. The
  legacy suite runs with `Rscript --vanilla tests/testthat.R` from the
  repository root.
- Legacy workload contracts and validation semantics are specified in
  `WORKLOADS.md`. 2.0 workload data lives in `workloads/`.
- LAStools may report a varying licensing banner. Identity uses executable
  checksums and normalized version lines, not the banner.
- `lascanopy` and `blast2dem` parent processes can exit 0 when a child fails.
  Log-based `ERROR`/`FATAL` detection is mandatory for LAStools.
