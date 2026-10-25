Work in progress: this repository is for testing the performance of LiDAR processing software packages, designed for—but not limited to—typical forestry-related workflows involving airborne laser scanning data. Currently tested packages are lidR, lasR, and LAStools.

Fresh-data preparation uses `cli` for status messages and parent-process progress
bars for the repair/check and preparation phases. Progress counts completed tile
jobs (all density variants per tile during preparation), including successfully
rechecked resumed jobs. Workers keep separate logs rather than drawing competing
bars. Progress visibility follows `cli`'s terminal defaults; benchmark timing and
report output are unchanged.

Tests are named by responsibility (for example, workloads, validation, monitoring
and reporting). See [WORKLOADS.md](WORKLOADS.md) for current saved-data identifiers
and compatibility requirements. The optional completed-pilot review test uses
`LIDAR_PILOT_REVIEW_ROOT` to select its campaign directory.
