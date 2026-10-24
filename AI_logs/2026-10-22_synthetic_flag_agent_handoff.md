# Agent instructions: resolve the fresh-data synthetic-flag blocker

## Superseding user decision (2026-10-23)

The user has now authorized **discarding source synthetic points** while the
rlas defect remains unfixed. The current `fresh-petawawa-v3` workflow applies
native `-drop_synthetic` filtering before R decoding, audits source/retained/
discarded counts, and requires a new root/pilot. The older preservation-only
constraints and failing-test descriptions below are historical v2 context,
not instructions to restore that policy. Prepared reads remain unfiltered and
reject synthetic diagnostics/flags. Existing v2 products, identities and the
archive are preserved; no in-place v2-to-v3 recovery is authorized.
See the 2026-10-23 development-plan follow-up and README for current behavior.
Later on 2026-10-23, full repair stopped at tile `309000_5091000` with one
negative-Z point. The user authorized removing negative-Z source points with
audits. Current policy is v4; `--resume-negative-z-full` supports a guarded
transition from the exact preceding v3 repair-stage failure, preserving old
identities, the successful pilot and completed repairs whose audits prove
negative source points did not affect earlier repair selection. See README.

## Objective and limits

Investigate and fix the synthetic-flag handling in fresh Petawawa preparation.
Deliver a verified script fix and safe manual resume instructions, or document
a precisely isolated blocker. Do not merely suppress the warning.

- Work on the existing `dev` branch. Do not commit, push or change branches.
- Preserve all existing worktree changes, including untracked files. `_OLD/`
  belongs to the user; do not modify or remove it.
- Do not run real downloads, full preparation, archive-wide scans, benchmarks
  or the full test suite (previously about 25 minutes). Use bounded tiny
  temporary fixtures only. The user runs long commands manually.
- Do not modify external data/configuration, clear LAS flags, drop flagged
  points, redownload the archive or delete completed products.
- Preserve approved repair rules, coordinate precision, density generation,
  deterministic seeds, metadata/audits and the default eight tile workers
  with one native thread per worker.
- Do not bypass frozen configuration/code/tool/runtime identity guards, relax
  point-record comparisons or automatically remove stale locks.

First read the fresh-input follow-ups at the end of
`AI_logs/2026-10-20_development_plan.md`, especially the blocked synthetic-flag
follow-up, and the fresh-data section of README. The earlier Chat 12/full
benchmark acceptance gate remains blocked and is not this task's scope.

## What happened, and what is actually known

The user manually ran:

```sh
CONFIG=/data/nvme2/LidarSpeedTestWorkingDir/fresh-data.local.R
Rscript --vanilla R/prepare_fresh_data.R --resume-parallel-full "$CONFIG"
Rscript --vanilla R/prepare_fresh_data.R --verify-full "$CONFIG"
```

The first command started eight workers, then failed:

```text
Preparation worker 41 failed
LAS read warning: There are 138 points flagged 'synthetic'.
Logs: /data/nvme2/LidarSpeedTestWorkingDir/input_data/full/execution-3882972412277e/repair
```

Verification subsequently reported `No successful readiness receipt`, which is
expected after failed preparation, not an independent verification defect.

A source LAS synthetic flag is valid metadata. The approved method preserves
existing flags. The original script incorrectly rejected every read warning,
including this informational notice.

An attempted narrow exception then exposed inconsistent counts in tiny
fixtures: a diagnostic reporting 138 versus one decoded flag, and a densified
case reporting 376 versus 239. These observations are **not proof that the
user's real data are corrupt or that an upstream library is at fault**.
The fixture, writer, decoder, counting/validation logic and object mutation
remain possible explanations. Establish independent ground truth before
deciding which layer is wrong.

## Current code and tests

Relevant files:

- `R/prepare_fresh_data.R`: `fresh_read()`, `fresh_read_las()`, `fresh_repair()`,
  `fresh_variant()`, output/index validators, process pool and recovery logic.
- `R/2_data.R`: reusable source preflight, tile preparation and complete-record
  validation; optional `read` callbacks now route fresh processing through
  the checked reader. Historical defaults remain unchanged.
- `tests/testthat/test-fresh-flags.R`: one/138 mixed flags, repair, thinning,
  densification and negative diagnostic cases.
- `tests/testthat/test-fresh-parallel-native.R`: two 225-point source fixtures,
  each with 138 mixed flags, actual worker repair/sort/index/query and resume.
- `tests/testthat/test-fresh-parallel.R`: pool limits, deterministic seeds,
  cleanup, partial-file archiving and frozen-code transition checks.
- `tests/testthat/test-fresh-data.R`, `test-chat4-data.R`,
  `test-fresh-native.R`: baseline preparation and corrupt-index regressions.

The pending `fresh_read()` collects read warnings and, for non-raw reads,
accepts only a literal synthetic diagnostic equal to the decoded logical flag
count. Other diagnostics and count mismatches remain fatal. **This is not yet
a validated production fix.** Raw reads still capture all warnings; inspect
that path too so any genuine flag discrepancy cannot be silently carried into
repair and then accepted on a later round trip.

Known-code recovery accepts the prior serial pair and initial parallel pair:

```text
R/prepare_fresh_data.R             R/2_data.R
00be03b000101fc9301c8dfef921b2bb   99a43ae69c2696ffafe37c7891b03b1b
066c8ee2c0f9d7fe0a440d3686160d53   aa12df90c7d03eaebe284d1f4ced285c
```

These are historical identities, not the current pending files' hashes.
Preserve old stage/preparation identities and completed receipts; record
authorized transitions separately. Frozen-plan comparison now accounts for
the changed `R/2_data.R` entry as well as the fresh implementation vector,
while refusing unrelated implementation changes. Do not broaden this into
arbitrary changed-code acceptance.

Latest recorded results:

| Check | Outcome |
|---|---|
| Fresh/historical baseline | 297 passed, 78.5 seconds during this follow-up |
| Pool/recovery | 49 passed, 26.0 seconds |
| Mixed flags | 25 passed, one failure, 10.1 seconds |
| Mixed-flag native workers | One passed, one failure, 17.6 seconds |

The mixed-flag suites are deliberately failing, not skipped or weakened.
Earlier all-flagged native fixtures passed but did not cover the mixed case.
Earlier exploratory raw-reader/constructor substitutions, copying and
JIT-disabled trials did not establish a reliable fix and were not retained.
Some instrumentation changed observed behavior; verify any explanation without
the instrumentation before declaring success. Avoid repeating the same broad
experiments without a concrete hypothesis and discriminating assertion.

## Required investigation and acceptance criteria

1. Reproduce the failing tiny fixtures. Establish the actual flags written
   on disk using a genuinely independent route, such as a separate decoder or
   verified legacy LAS flag-bit inspection. An uncompressed tiny LAS fixture
   can simplify inspection. Verify format/header/record layout rather than
   guessing offsets. Do not treat the warning count or a second call through
   the same reader as independent truth.
2. Track complete flag vectors before write, after independent decode, after
   the normal reader and after repair/variant generation. Match records by
   stable attributes where sorting changes row order. Test zero, one, 138 and
   all flagged points, repeated reads, and thinning/densification. Determine
   the first actual divergence; check fixture validity, aliasing/mutation and
   counting before blaming a dependency.
3. Fix the identified cause surgically. Permit only the verified informational
   synthetic notice. Preserve flag values, full point attributes, source
   encoding and audits; keep malformed/unrelated diagnostics, real QC errors,
   corrupted indices and silent scan fallbacks fatal. Do not reset flags or
   simply delete the consistency check.
4. Make both mixed-flag suites pass, then rerun the bounded baseline,
   pool/recovery and original native corrupt-index checks. Do not substitute
   all-flagged fixtures, skip the mixed case or weaken exact assertions.
5. Verify interrupted-run recovery retains completed bytes/receipts, archives
   only partial products/scratch and does not mutate frozen manifests. Keep
   eight tile workers as default. Resolve any directly coupled identity issue
   explicitly, not by asking the user to edit a receipt.
6. Update README and the development plan handoff with the actual diagnosis,
   fix, checks and remaining blockers. Remove the current do-not-resume notice
   only once the fix is validated. Stop without committing or pushing.

## Bounded checks and environment

Use the restored project environment, R 4.6.1. Dependencies are already present;
no dependency installation or restore is required to reproduce current tests.
Native binaries are under `/home/ptompals/opt/lastools/bin`.

```sh
timeout 45s Rscript --vanilla -e \
  'source("renv/activate.R"); testthat::test_dir("tests/testthat", filter = "^fresh-flags$", stop_on_failure = TRUE)'

timeout 60s env LIDAR_FRESH_LASTOOLS_PATH=/home/ptompals/opt/lastools/bin \
  Rscript --vanilla -e \
  'source("renv/activate.R"); testthat::test_dir("tests/testthat", filter = "^fresh-parallel-native$", stop_on_failure = TRUE)'

timeout 60s Rscript --vanilla -e \
  'source("renv/activate.R"); testthat::test_dir("tests/testthat", filter = "^fresh-parallel$", stop_on_failure = TRUE)'

timeout 90s Rscript --vanilla -e \
  'source("renv/activate.R"); testthat::test_dir("tests/testthat", filter = "^fresh-data$|^chat4-data$", stop_on_failure = TRUE)'

timeout 45s env LIDAR_FRESH_LASTOOLS_PATH=/home/ptompals/opt/lastools/bin \
  Rscript --vanilla -e \
  'source("renv/activate.R"); testthat::test_dir("tests/testthat", filter = "^fresh-native$", stop_on_failure = TRUE)'
```

Previous logs are outside the repository at
`/home/ptompals/.copilot/session-state/0ed64d5d-3c9a-4e98-b0a2-a78b46c66cb0/files/`:
`fresh-flags-checks.log`, `fresh-flags-native.log`,
`fresh-flags-regression.log` and `fresh-flags-resume.log`.

Only after validation, give the user this manual recovery pattern; do not
execute it yourself:

```sh
CONFIG=/data/nvme2/LidarSpeedTestWorkingDir/fresh-data.local.R
Rscript --vanilla R/prepare_fresh_data.R --resume-parallel-full "$CONFIG" && \
Rscript --vanilla R/prepare_fresh_data.R --verify-full "$CONFIG"
```

The `&&` prevents verification after preparation failure. A stale lock must
never be removed automatically: first establish that no old workers or native
children remain. No new benchmark acceptance/readiness claim follows from
data QC alone.
