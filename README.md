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

Tests are named by responsibility (for example, workloads, validation, monitoring
and reporting). See [WORKLOADS.md](WORKLOADS.md) for current saved-data identifiers
and compatibility requirements. The optional completed-pilot review test uses
`LIDAR_PILOT_REVIEW_ROOT` to select its campaign directory.
