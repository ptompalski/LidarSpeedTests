Work in progress: this repository is for testing the performance of LiDAR processing software packages, designed for—but not limited to—typical forestry-related workflows involving airborne laser scanning data. Currently tested packages are lidR, lasR, and LAStools.

Fresh-data preparation uses `cli` for status messages and parent-process progress
bars for the repair/check, preparation and verification phases. Progress counts completed tile
jobs (all density variants per tile during preparation), including successfully
rechecked resumed jobs. Workers keep separate logs rather than drawing competing
bars. Progress visibility follows `cli`'s terminal defaults; benchmark timing and
report output are unchanged.

Preparation and verification default to **16 tile workers**, with one native
thread per worker. Override the budget with `--cores N`; large clouds can require
several GiB per worker, so reduce it on memory-constrained machines.

To finish an existing preparation whose output files and completion receipts
are present, but whose final verification was interrupted, run:

```sh
Rscript --vanilla R/prepare_fresh_data.R --verify-full /path/to/config.R
```

Use `--verify-pilot` for the two-tile preparation. These commands acquire the
preparation lock and refuse to run alongside an active preparation. Interrupt
the old run normally (Ctrl+C) and wait for it to exit and release its lock first.
Do not remove an active lock or start another preparation.

Verification restarts its checks, not data generation: it reuses existing clouds,
indexes and receipts, checks the frozen configuration and data identities, and
performs the full per-tile QC in parallel. It permits verifier source-code changes
without rewriting historical implementation hashes or policy-transition records;
generation/resume commands retain their stricter implementation checks.
Successful verification creates a missing `ready.rds`; an existing readiness
receipt and all prepared data remain unchanged. Each `verification-*` directory
records the verifier implementation, worker logs, per-tile successes and final
status. A failed verification never creates a readiness receipt, and a hard-killed
verifier may leave no final status record.

## Benchmark pilot on prepared data

The benchmark pilot is separate from preparation's `--pilot` stage. It stages
unchanged copies of the two approved neighboring density-1 tiles and their
indexes from a verified preparation. It checks readiness, manifest identity and
completion-receipt checksums, and freezes that provenance with the campaign.
It never processes all 100 tiles or repeats data preparation.

From the repository root, initialize a **new** pilot workspace, check it, then
explicitly start the benchmark:

```sh
Rscript --vanilla R/6_pilot.R --init /path/to/fresh-data/full /path/to/new-pilot-workspace
Rscript --vanilla R/6_pilot.R --preflight /path/to/new-pilot-workspace
# Run only after the preflight succeeds:
Rscript --vanilla R/6_pilot.R --run /path/to/new-pilot-workspace fresh-pilot-001
```

Initialization creates `benchmark.R`, `scratch/` and `reports/` in the workspace.
It inherits confirmed CRS/datum metadata and the LAStools directory from the
preparation, uses the repository's benchmark defaults with the 1-/2-core matrix,
and refuses to overwrite an existing workspace. It does not stage inputs or run
workloads. Review/edit `benchmark.R` before preflight if needed.

Preflight only stages/checks inputs and checks configuration and executable
availability; it does not run workloads or establish numerical acceptance.
The pilot tests all defined tasks with lidR, lasR and LAStools at 1 and 2 cores,
with one warm-up and three measured repetitions per task/tool/core combination.
Tools run sequentially, not as 16 competing benchmark workers. The 16-worker
default applies to data preparation/verification, not this small pilot matrix.
Strict validation remains enabled: unsupported/blocked combinations are skipped,
and known cross-tool differences can fail validation; timings alone are not
acceptance. LAStools requires the appropriate installed tools and entitlement.
Results are recorded under `<workspace>/reports/campaigns/<campaign-id>`.
Use the same configuration and ID to resume an unchanged campaign; use a new
ID after changing the code, configuration or inputs.

To accept documented raster edge conventions, set
`accept_known_extent_discrepancies = TRUE` in the workspace configuration and
use a new campaign ID. This allows at most one aligned extra cell per edge,
never missing core cells or changed core values/NoData masks. Accepted R outputs
are labeled `accepted_exception`; native numerical checks remain `unverified`.
This does not enable the broader surface-support exception policy.

Native software identity records executable checksums and LAStools software
builds, not the licensing banner, which can vary between identical `-version`
queries. Licensing notices are still shown; this does not establish entitlement
or relax processing exit-status checks. In particular, the pinned `lascanopy`
can exit with status 1 when an all-point elevation metric deliberately sets its
cutoff below every input point. Such attempts remain failed, not accepted
benchmarks; do not normalize the elevations or raise the cutoff merely to hide
that warning.

Historical pilots remain supported by selecting `historical_input_root` instead
of `prepared_stage_root`; do not configure both sources. The original
`--preflight`/`--run` forms without workspace arguments still use
`LIDAR_BENCHMARK_CONFIG` and `LIDAR_CAMPAIGN_ID`.

Metrics and maximum DSM are checked against each tool's grid-edge ownership.
`boundary_difference_points` records differing point assignments; affected
attempts are descriptive only, excluded from cross-tool comparison counts.
lasR DEM uses an independent unextrapolated ground TIN and an explicit
Float32 precision budget; DEM remains non-comparable. Non-repeatable lasR
first-return TIN DSM is always skipped, including diagnostic mode. Native
warnings, errors and missing tiles remain rejected. Use a new campaign ID
after these validator changes; historical results are not rewritten.

Tests are named by responsibility (for example, workloads, validation, monitoring
and reporting). See [WORKLOADS.md](WORKLOADS.md) for current saved-data identifiers
and compatibility requirements. The optional completed-pilot review test uses
`LIDAR_PILOT_REVIEW_ROOT` to select its campaign directory.
