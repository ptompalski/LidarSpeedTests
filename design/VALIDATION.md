# Output validation (`lidarbench validate`, `lidarbench.validation-report/1`)

**Status**: Task 6 — framework plus two rigorous reference implementations  
**Code**: `src/lidarbench/validation/`, `src/lidarbench/workloads/`, `workloads/workloads-v1/`  
**Related**: [ARCHITECTURE.md](ARCHITECTURE.md) §7.3 and §8, [../WORKLOADS.md](../WORKLOADS.md)

Validation decides whether the outputs of one finished attempt satisfy its
workload contract. It is independent of timed processing: it runs after the
timed interval has ended, never launches a benchmarked tool, never modifies an
output or input, and never resamples, crops, repairs, fills or normalizes a
product to make it agree.

## 1. Workload contracts (`lidarbench.workload/1`)

Contracts are TOML data in `workloads/<suite>/<id>.toml`, loaded by
`lidarbench.workloads.load_suite()` with the same strict rules as campaign
configuration: unknown keys are errors, nothing is coerced (`20` is not
`20.0`), and every error names its key. Each contract has a content digest
(`WorkloadContract.digest`). Changing a definition requires a new suite.

A contract declares, tool-neutrally: the population (all points, original Z,
finite XYZ, metre units), the tiling and buffer, the raster product
(resolution, zero origin, outward-snapped header extent, cell ownership, bands,
NoData semantics, forbidden sentinel values), the reference method and
statistic, and the numerical tolerance. Per tool it declares the implementation
status (`runnable`, `diagnostic_only`, `unsupported`, `blocked`), the
*declared* equivalence (`matched_definition` or `non_comparable`), exact
primary artifact names (`{stem}.tif`, `{stem}_avg.tif`), the tool's cell
ownership convention and whether band names are part of the contract.

Suites: `workloads-v1` (frozen, lidR/lasR/LAStools), `workloads-v2` (frozen: the
same definitions plus the PDAL implementation), `workloads-v3` (frozen:
`workloads-v2` plus LAStools `*.tfw` world files as ignored sidecars, so that
LAStools outputs are no longer `validation_failed` at the artifacts level),
`workloads-v4` (frozen: `workloads-v3` plus the LAStools declaration below) and
`workloads-v5` (the default: `workloads-v4` plus the lidR extent rule below).
An implementation may declare `value_precision` (an output quantum, for example
0.01 m when a tool rounds its values); the absolute tolerance is then widened by
half of it, the reports say so, and the value is part of the contract digest. A new tool entry
changes the contract digest, so it needs a new suite (the comparison gate
requires equal digests). Contract values come from WORKLOADS.md:

| Contract | Statistic | Grid | lidR | lasR | LAStools | PDAL (v2) |
|---|---|---|---|---|---|---|
| `pixel_metrics_1a` | mean original Z, band `z_mean` | 20 m | matched, `lidr_buffered_clamp`; v5: `extent = "header_bounds_snapped_nearest"` | matched, `right_down` | v4: matched, `pdal_binmode_floor`, `value_precision = 0.01`, `{stem}_avg.tif` (≤ v3: non-comparable, `undeclared`) | matched, `pdal_binmode_floor`, band name not required |
| `generate_DSM1` | max original Z | 1 m | matched, `lidr_buffered_clamp`; v5: `extent = "header_bounds_snapped_nearest"` | matched, `right_down` | v4: matched, `pdal_binmode_floor` (≤ v3: non-comparable, `undeclared`) | matched, `pdal_binmode_floor` |

`WorkloadContract.declared_comparability(tool)` derives the declared class
(reported under `implementation.declared_comparability`):
`strictly_comparable` (matched definition and the contract partition: lasR),
`functionally_comparable` (matched definition, other declared edge convention:
lidR, PDAL), `non_comparable` (LAStools) or `unsupported`. Only the attempt-level
gate below can make a timing `comparable`.

Both use a 20 m buffer and `|actual − reference| ≤ 2e-6 + 2e-7·|reference|`.

### Cell ownership conventions

* **`right_down`** (the contract partition): on the zero-origin grid, column
  `i` owns `i·r ≤ X < (i+1)·r` and row `j` owns `j·r < Y ≤ (j+1)·r`, on every
  edge including a tile's outer edges. Every point belongs to exactly one
  cell, so neighbouring tiles never count a point twice or drop it. This is
  the interior-edge rule of WORKLOADS.md ("rightward X, downward Y") applied
  uniformly. Observed pinned lasR behaviour matches it.
* **`lidr_buffered_clamp`**: `right_down` on interior edges, but a point
  exactly on the core raster's maximum-X / minimum-Y edge is clamped into the
  last column / bottom row when pinned lidR's buffered raster layout does not
  extend past that edge. This mirrors `benchmark_reference_cells()` in
  `R/1_functions.R` (layout edges rounded half away from zero).
* **`pdal_binmode_floor`**: PDAL `writers.gdal` with `binmode=true` (>= 2.5.0).
  Column `floor((X − xmin)/r)` and row `floor((Y − ymin)/r)` counted upwards
  from the tile grid's south edge, evaluated in binary64 exactly as PDAL does
  (`pdal/private/Raster.hpp`). On exact edges: rightward X, **upward** Y, so a
  point on a horizontal edge belongs to the cell north of it, and points on the
  grid's north edge are outside the tile's raster (design/ADAPTERS.md §10). Near
  edges the assignment follows PDAL's division rounding, not exact rationals.
* **`undeclared`**: the convention has not been established (LAStools up to `workloads-v3`).

Indices are computed with `floor`/`ceil` and then corrected by exact
comparison with the float64 edges `i·r`, so a point exactly on an edge is
assigned by the convention, never by division rounding (tested against an
exact rational oracle, including the neighbouring floats of each edge).

## 2. Levels and statuses

| Level | Question | Statuses |
|---|---|---|
| execution | Did the process run and exit successfully? (runner) | `succeeded`, `failed`, `timed_out`, `interrupted`, `skipped_unsupported`, `skipped_blocked`, `skipped_unavailable` |
| artifacts | Is every required file present, a regular non-empty file with the right format signature, completely decodable, with no unexpected product? | `passed`, `failed`, `unavailable`, `not_run` |
| structural | GeoTIFF driver, band count/names/order, numeric type, only NoData-value masking, north-up axis-aligned geometry, exact resolution, zero-origin alignment, exact expected extent, CRS identity and metre units, no infinite values | `passed`, `failed`, `unavailable`, `unsupported`, `not_run` |
| numerical | Do values and NoData masks match an independent reference within tolerance? | `passed`, `accepted_exception`, `failed`, `unverified`, `unavailable`, `unsupported`, `not_applicable`, `not_run` |
| comparability | May this timing enter a cross-tool comparison? (derived) | `comparable`, `descriptive_only`, `diagnostic_only`, `excluded` |

Meaning of the non-binary statuses:

* `unverified` — everything checkable passed, but the numerical claim cannot
  be established: `performance-only` mode, an `undeclared` tool convention, a
  `diagnostic_only` implementation, or an unknown/undeclared CRS.
* `unavailable` — a validator or optional dependency is not usable on this
  machine (for example rasterio or laspy not installed).
* `unsupported` — no implementation in the contract, or no validator provides
  the contract's reference method.
* `not_run` — a prerequisite level did not pass; the reason names it.
* `accepted_exception` — passed only under a frozen opt-in policy.

**Fail-closed invariant** (`AttemptVerdicts`, enforced at construction and
tested over every combination of statuses): a level can be `passed` or
`accepted_exception` only if execution succeeded and every earlier level
`passed`. `validated_performance` is true only when all three levels
`passed`. Outputs of failed, timed-out or interrupted executions are never
inspected.

Derived outcomes and report inclusion:

| Outcome | `validated` report | `performance-only` report |
|---|---|---|
| `validated` | included | included |
| `accepted_exception` | included, labelled | included, labelled |
| `unverified` (structure passed; numerical `unverified`/`unavailable`/`unsupported`/`not_applicable`) | excluded | included, labelled `unverified (…)` |
| `validation_failed`, `validation_incomplete`, `execution_failed`, `unsupported`, `blocked`, `unavailable` | excluded | excluded |

An attempt is `comparable` only if it is `validated`, its implementation is
`runnable` with declared `matched_definition`, `boundary_difference_points == 0`
and the mode is `numerical`. The pairwise gate
(`lidarbench.validation.comparison_gate`, `speedup`) additionally requires the
same contract and digest, equivalence group, dataset digest, campaign identity,
timing boundary and run dimensions, a `validated` report and two different
tools. `speedup()` raises `NotComparableError` across a failed gate; there is
no other ratio function.

## 3. Reference calculation (`cell-statistic/1`)

For each input tile:

1. The manifest is read with the stdlib header reader
   (`lidarbench.datasets.lasheader`): unique stems, finite non-degenerate
   header XY bounds and a non-zero count are required.
2. The population is the tile plus every tile whose header bounds intersect the
   tile bounds grown by the 20 m buffer, restricted to that closed rectangle.
   Every contributing file is decoded completely (laspy); the decoded count must
   equal the header count, uncompressed payload sizes must match the header,
   XYZ must be finite and every point must lie within its own header bounds
   (± half a coordinate unit). Input bytes are hashed (SHA-256) before decoding
   and re-checked afterwards; any change fails validation.
3. The expected grid is the tile's header XY bounds snapped outward to the
   zero-origin grid.
4. Each point is assigned to its owned core cell under the **tool's declared
   convention**; the per-cell mean (float64 accumulation) or maximum is the
   reference. Empty cells are NoData.
5. Ownership is also computed under the contract partition;
   `boundary_difference_points` counts points owned differently. For a tool
   whose convention differs, the product is additionally compared with the
   contract-partition reference and the result is recorded
   (`contract_convention_comparison`) — the discrepancy is measured, not hidden.

Comparison: NoData is read from each file's metadata, compared in the storage
type, and decoded to NaN together with stored NaN. Masks must match
**exactly**; every co-occupied value must be within tolerance. A legitimate
reference value that equals (after rounding to the storage type) the file's
NoData value or a forbidden sentinel (-9999, -1000000) fails, because NoData and
data become indistinguishable. Failures list counts, the maximum error and up to
ten example cells with coordinates.

Only decoding uses third-party code (rasterio/GDAL for GeoTIFF, laspy for
LAS/LAZ); no processing algorithm from any package is used as the authority.

### Explicit policy: `accept_known_extent_discrepancies`

Off by default. When enabled, a raster may extend **outward** by at most one
cell per edge with the full expected core present. Values are then compared
through an in-memory index window onto the expected core (no resampling, no
copy of the file), the extension is recorded with its extent delta and the best
possible numerical status is `accepted_exception`. Inward truncation, larger
extensions, shifts and different resolution or alignment still fail. Values in
the extra cells are not a claim about the contract.

The one exception to "inward truncation fails" is an implementation that
declares `extent = "header_bounds_snapped_nearest"` in its contract (lidR from
`workloads-v5`), again only under this policy. Pinned lidR builds its layout
from the buffered cloud and then crops each tile's raster to the header bounds
with `terra::crop()`, whose default `snap = "near"` rounds every bound to the
*nearest* grid line. Where a header bound lies at least half a cell inside the
outward grid line (only possible where the tile has no data to the edge, for
example at the dataset boundary) that edge cell is cut, together with the
points it would own. The rule accepts exactly that: per edge either the
contract edge or the one-cell-inward edge, the latter only where the header
bound is ≥ `r/2` inside. Values are compared on the covered cells only. The
cut is measured, never hidden: the artifact's `numerical.trimmed_extent`
records the cut cells (W/E/S/N), `dropped_points` and `occupied_cells_cut`, and
the dropped points are added to `boundary_difference_points` (they are owned
under the contract but by no cell of the product). On the Petawawa
10 pts/m² dataset (100 tiles) this cut 3,048 points in 19 occupied cells
on 9 boundary tiles.

## 4. What validation establishes, and what it does not

Each report lists these per attempt under `claims`.

**Established when the respective level passes**

| Claim | Level |
|---|---|
| Every input tile has exactly its declared primary artifact(s); each is a non-empty regular file with the correct format signature and decodes completely; nothing unexpected (halo exports, duplicates, extra primaries) is present. Declared sidecars (`*.aux.xml`, `*.vrt`) never satisfy a requirement. | artifacts |
| Each raster is a north-up GeoTIFF on the zero-origin grid with the contract resolution, the expected outward-snapped extent of its tile (or an explicitly accepted outward extension or declared nearest-snapped cut), the required band layout, NoData expressed only as a NoData value, no infinite values, and the declared CRS in metres. | structural |
| Every owned cell value and the complete NoData mask equal an independent float64 mean/max of the original Z of all points, including buffered points from neighbouring tiles, under the tool's declared cell convention, within `2e-6 + 2e-7·|reference|`. | numerical `passed` |
| The tool's cell ownership equals the contract partition for every point of these inputs (`boundary_difference_points == 0`). | numerical `passed` + boundary 0 |

**Not established (remain unverified)**

* Behaviour on inputs other than those validated: a passed fixture or dataset
  is not a general proof of equivalence or correctness.
* That every LAStools build bins by the upward-Y floor rule and rounds averages
  to 0.01 m (declared in `workloads-v4` from LAStools 261023 on the Petawawa
  pilot pair; each campaign re-validates its own build and fails it otherwise).
  Up to `workloads-v3` LAStools conventions are `undeclared` and its outputs at
  best `unverified`.
* That real PDAL bins exactly by the mirrored `pdal_binmode_floor` rule. This
  comes from the PDAL source and holds only where
  `tests/python/test_pdal_integration.py` passes against an installed PDAL.
* That pinned lidR's real clamping rule is exactly the mirrored R reference
  rule in general; it is established only where outputs pass (the real-tool
  fixture integration test passes for both workloads).
* Vertical datum and units of Z (used as supplied); data provenance; the CRS
  when none is declared (numerical status is capped at `unverified`).
* Anything about timing quality; validation never measures or adjusts timings.
* Workloads other than `pixel_metrics_1a` and `generate_DSM1` (terrain/TIN,
  nine-band metrics, normalization, treetops): no Python reference exists yet;
  they are `unsupported` here and remain with the R `validation-v1` path.
* Parity with every R `validation-v1` rule: the surface-support policy
  (`accept_known_surface_discrepancies`) is not implemented, so the Python
  validator is stricter there.

## 5. Observed results on the synthetic fixtures

`tests/python/validation_fixtures.py` writes two neighbouring 100 m tiles
(stdlib LAS writer) with points exactly on interior edges, the shared tile
edge, the south/north edges and corners; an east tile whose fractional header
minimum (X0+100.37) makes its first snapped column depend on points stored in
the west file; a deliberately empty 20 m cell; negative, zero, very large,
many-decimal and duplicate-XY Z values. `test_validation_r_integration.py`
runs the exact pinned lidR 4.3.3 / lasR 0.21.2 calls from `R/0_setup.R` on them
(untimed) and validates the persisted GeoTIFFs:

| Tool | Result | Comparability |
|---|---|---|
| lidR (both workloads) | `validated` under `lidr_buffered_clamp`; boundary points differ from the contract partition; the contract-partition diagnostic fails | `descriptive_only` |
| lasR (both workloads) | strict: structural `failed` (one extra cell east and north); with the extent policy: `accepted_exception`, `boundary_difference_points = 0` | `descriptive_only` |

These reproduce what WORKLOADS.md documents from the R validators. Unit tests
cover each failure mode with oracle-generated GeoTIFFs: tolerance edges,
float32 storage, zero-filled empty cells, missing cells, sentinel collisions,
missing neighbours, missing/truncated/extra artifacts, CRS, resolution,
alignment, extent, band names, infinite values, integer encoding, header/
payload mismatches, degenerate inputs, inputs changing during validation,
missing backends, unsupported and blocked implementations.

## 6. Usage

```sh
uv sync                                   # dev environment includes the validation extra
pip install "lidarbench[validation]"      # numpy, rasterio (bundles GDAL), laspy[lazrs]
uv run lidarbench validate --workload pixel_metrics_1a --tool lasR \
    --outputs OUT_DIR --input-dir TILES_DIR --epsg 2959 [-o report.json]
```

Exit codes: `0` validated; `3` descriptive only (accepted exception or
unverified); `1` failed, incomplete or not run; `2` usage/contract error.
Reports are written once (an existing report path is refused).
`uv run pytest tests/python -m "not integration"` skips the R-based tests.

## 7. Extending

* **New workload**: add `workloads/<suite>/<id>.toml`. If its reference method
  already exists, nothing else is needed.
* **New reference method**: implement the `ReferenceValidator` protocol
  (`name`, `version`, `level`, `applicable()`, `probe()`, `validate_artifact()`)
  and register it under the `lidarbench.validators` entry point (name = 
  validator name). Missing dependencies must be reported by `probe()`.
* **New tool convention**: add it to `CELL_OWNERSHIP` with an exact
  assignment in `validation/grid.py` and an oracle test.

## 8. Validation in the campaign runner

The runner (`campaign/validation.py`) validates every attempt whose execution
`succeeded`, after its outputs are inventoried and **before** its scratch
directory is disposed, and publishes `validation/report.json` into the attempt
so the report is covered by the attempt's digest chain (`result.json` lists it).
`result.json` then carries `artifacts_status`, `structural_status`,
`numerical_status` and a `validation` summary (outcome, comparability,
duration, `reused_from`, reference-cache hits). Validation is untimed and never
fails the campaign: an exception is recorded as a warning and the attempt keeps
`not_run` verdicts. A failed or incomplete validation retains scratch under
`retain = "on-failure"`.

* **Contract.** The campaign's `[validation] suite` (default: the checkout's
  `workloads/workloads-v4`) is frozen in identity: every workload's contract key
  and digest is part of the `workloads` component. A contract whose digest
  differs at run time is not used (the run would already refuse the changed
  identity). Workloads without a contract (synthetic) are not validated.
* **Tool mapping.** Each adapter declares `contract_tool` (`lidr`→`lidR`,
  `lasr`→`lasR`, `lastools`, `pdal`).
* **Policies and CRS.** `[validation] accept_known_extent_discrepancies` and the
  workload's `epsg` parameter.
* **Mode.** `validation_mode = "performance-only"` still checks artifacts and
  structure (no point decoding); numerical is `unverified`.
* **Comparability policy.** `[validation] comparability` (`strict` default, or
  `edge-tolerant`) is frozen in identity and applied at report time by the
  gate (ARCHITECTURE.md §8.3). It never changes a verdict: validation reports
  keep the strict attempt-level `comparability`, and `edge-tolerant` only lets
  the gate accept declared boundary-point differences and the explicit
  outward-extent exception, listing them per comparison.

### 8.1 Cost: what validation does and how it is bounded

Per attempt, validation (1) inventories the outputs (stdlib), (2) decodes every
raster completely, (3) checks structure, and (4) for the numerical level decodes
each tile plus every neighbour within the 20 m buffer, computes an independent
float64 per-cell mean/max (numpy `bincount`/`maximum.at`, vectorised) under the
contract's and the tool's cell conventions, and compares every cell and the
NoData mask. On the Task 13 pilot pair (2 × ~1 M points) a full validation takes
~1 s (≈0.25 s imports and probing); at density 10 (2 × ~10 M points) ~4.7 s and
1.4 GB RSS. Cost grows with the points decoded.

The legacy R validation was much slower because it read every attribute
(`readLAS(select = "*")`), assembled data frames with `rbind`, computed the
reference in an **interpreted per-cell R loop** (`split()` + `for` over every
cell — ~1 M cells per tile for a 1 m DSM; TIN references via `sf`
triangulation and point-in-polygon tests), re-hashed inputs, and repeated all of
it for **every attempt**, including every repetition.

The Python runner bounds the cost without weakening any check:

| Measure | Effect |
|---|---|
| **Verdict reuse** (`[validation] reuse_identical_outputs = true`, default) | A validation is a deterministic function of the output bytes, input bytes, contract, tool implementation, policies, mode, EPSG and validator versions (`reuse_key`, `lidarbench.validation-reuse-key/1`). When every output of an attempt is byte-identical (SHA-256, from the output inventory) to an attempt already validated under the same key, its verdict is reused; the report records `reuse.source_attempt_id`, the source report's SHA-256 and the statement. Published reports are indexed on resume. Repetitions and CPU counts of deterministic tools cost one comparison. Requires `integrity.record_output_digests`. |
| **Shared references** (`ReferenceCache`, 1 GiB LRU per run) | Per-tile references are keyed by contract digest, grid and the SHA-256 of every contributing input, and computed once for **all** declared conventions; tools and attempts sharing a convention reuse them. |
| **Edge strips** (`PopulationLoader`) | A neighbour whose interior does not overlap the tile contributes only points within `buffer` of its own edges; those strips are cached, so each file is decoded at most about twice instead of once per neighbour visit. |
| **Trusted input digests** | Inputs' SHA-256 come from the frozen identity (full hash at the start of the run, stat fingerprints around every entry and after validation) instead of hashing every input before and after each validation. |

Remaining cost drivers: the first validation per workload × dataset ×
convention still decodes the whole dataset (laspy/lazrs decoding is
multi-threaded and runs between attempts, before the cooldown); output rasters
are always decoded in full (structure needs them).

## 9. Not yet done

* The R `validation-v1` bridge and Python references for the remaining
  workloads (TIN surfaces, nine-band metrics, normalization, treetops).
