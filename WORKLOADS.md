# Workload contracts: workloads-v1

These definitions apply to **new attempts only**, with lidR 4.3.3 and lasR
0.21.2 from `renv.lock`. They do not reinterpret historical results.
`R/0_setup.R` contains executable calls and `benchmark_contracts`, with one row
for every task/tool, including unsupported and blocked combinations.
`benchmark_workload_contract()` in `R/1_functions.R` supplies the status,
population, target geometry, bands and required artifact patterns. New runner
CSVs carry `workload_contract`, `comparison_status`, `comparison_group` and
`comparison_note`. Execution success is **not** numerical validation.

Saved-data identifiers now describe their purpose: `workloads-v1`,
`validation-v1`, `monitoring-v1` and `reporting-v1`. Records using the previous
development-stage identifiers are not migrated or reinterpreted; regenerate
campaigns and reports for use with the current code.

## Populations, units and statistics

Inputs are the original, unnormalized point clouds (`datasets_org`), not the
unused normalized directory. XYZ must be finite. Horizontal and vertical units
must be metres for the stated 1/20 m grids, 20 m buffers, 5 m windows and
2 m thresholds to have that meaning. Original Z means the supplied elevation,
including its original datum; no CRS, datum or unit conversion is performed.
The recovered historical inputs have **unknown CRS**. These contracts do not resolve
that provenance/units blocker or assign a guessed CRS.

**Custodian-confirmed declaration (2026-10-22):** the custodian confirmed the
backup inherited EPSG:2959 (NAD83(CSRS) / UTM zone 18N), EPSG:5713 (CGVD28
height), and metre XYZ units unchanged from the identified original source.
The headers themselves remain empty. Optional `input_metadata` configuration
records this confirmation in campaign identity and validation manifests, sets
the horizontal CRS on in-memory lidR catalogs and uses supported
`lasR::set_crs()` before processing. This assigns metadata, not a coordinate
transformation or source-file rewrite. Vertical datum/units remain an explicit
source declaration, not an invented LAS header. Conflicting file CRS is refused.
Declared LAStools calls also assign the horizontal EPSG with `-epsg`, not
`-target_epsg` reprojection, and explicitly request `-fail` rather than demo
fallback. This supplies metadata, not native numerical acceptance.

Basic pixel metrics use **all points**, including ground, water, non-first
returns and points at/below zero: no return/class/height filter, pulse grouping,
deduplication or height normalization. Buffers supply neighboring points but
must not duplicate contributions to an owned output cell. The strict threshold
is `Z > 2`, not `Z >= 2`. On original elevations this is an elevation statistic,
**not canopy cover**.

The nine canonical bands, in order, are:

| Band | Definition | Units |
|---|---|---|
| `z_max` | maximum Z | m |
| `z_mean` | arithmetic mean Z | m |
| `z_sd` | sample SD, denominator n - 1 | m |
| `z_above2` | count(Z > 2) / n, all returns in the denominator | fraction, 0–1 |
| `z_p5`, `z_p25`, `z_p50`, `z_p75`, `z_p95` | R type-7 quantiles: interpolate sorted values at 1 + (n - 1)p | m |

**User-confirmed change:** use fractions rather than the old lidR percentage,
retaining native lasR operators. `metrics_multiple()` now uses these same band
names. Do not combine these results with historical `pzabove2` percentages.
The pinned lasR operator `z_above2` returns a fraction despite documentation
calling it a percentage. Tiny known-value fixtures establish its strict
threshold, sample SD and type-7 quantiles, including negative/zero elevations,
mixed returns, singleton cells and empty cells. This is not a general numerical
proof or a neighboring-tile acceptance test. lasR calculates with Float32
precision; persisted R metric comparisons allow 2e-6 on these tiny values.

Empty cells are NoData, **not zero**. A singleton has a valid max/mean/quantiles/
above-threshold fraction but NoData SD. R helpers return NA for empty cells and
reject nonfinite inputs explicitly. lasR NaN SD is interpreted as NoData.

LAStools pixel calls now request `-height_cutoff {height_cutoff}` with a
run-derived finite value strictly below **every catalog header Min.Z** (subtract
at least one coordinate unit before formatting at 17 digits). This removes the
default 1.37 cutoff, provided input headers correctly bound the payload.
`-step 20` is explicit in both calls. The complex call uses `-dns
-cover_cutoff 2 -fractions`, **not `-cov`**: all-return density rather than the
first-return cover ratio. Native SD denominator, quantile convention, exact
cutoff comparison, tile geometry and persisted outputs have not been verified
with an entitled native processing run. Both LAStools metric tasks remain
**non-comparable**, not presumed equivalent based on flag names.

## Supported matrix and required outputs

`matched` below denotes a matched target definition, not a validated result;
artifact validation is per attempt and has the boundary limitations below.
`tool-specific` means runnable but **non-comparable**.
`blocked` means deliberately empty runnable call and recorded skipped attempt;
the candidate definition is retained separately in `task_candidate_lasR`.
`unsupported` has no implemented call or required artifacts.

| Task ID | lidR | lasR | LAStools | Required primary artifacts per processed tile |
|---|---|---|---|---|
| `normalization` | tool-specific | **blocked** | tool-specific | normalized LAZ, owned points/attributes retained, Z replaced by height |
| `pixel_metrics_1a` | matched | matched | tool-specific | one single-band mean GeoTIFF |
| `pixel_metrics_2` | matched | matched | tool-specific | R: one nine-band GeoTIFF; LAStools: nine single-band metric GeoTIFFs |
| `generate_DEM` | tool-specific | tool-specific | tool-specific | one single-band original-elevation terrain GeoTIFF |
| `generate_DSM1` | matched | matched | tool-specific | one single-band original-elevation maximum GeoTIFF |
| `generate_DSM2` | matched | matched | unsupported | one single-band first-return TIN original-elevation GeoTIFF |
| `tree_detection` | tool-specific | tool-specific | unsupported | persisted GeoPackage point layer with XYZ treetop locations |
| `complex_task1` | unsupported | **blocked** | unsupported | candidate: normalized LAZ, `*_dtm.tif`, `*_chm.tif` |
| `complex_task2` | unsupported | **blocked** | unsupported | candidate: task 1 products plus `*_metrics.tif` (nine bands), `*_trees.gpkg` |

There are 18 runnable, six unsupported and three blocked combinations.
That is the **default strict matrix**. With the user-authorized
`diagnostic_lasR = TRUE`, the three blocked lasR calls become runnable
`diagnostic_only` combinations, for 21 runnable and six unsupported pairs.
They retain the NA/corrupt-elevation warning; no algorithm, point dropping,
NA filling or runtime upgrade is introduced. Their validation remains failed
when errors are found, or unverified if diagnostic inspection finds none;
they cannot become passed numerical comparisons or eligible report timings.
Artifact patterns are requirements, not the old maximum-observed-file-count
heuristic. Artifact validation checks explicit per-tile primary artifacts. In particular, LAStools
percentile suffix spelling must be confirmed against the installed distribution;
the descriptive contract uses `*_p*.tif`; validation conservatively requires
`_p5`, `_p25`, `_p50`, `_p75`, `_p95` until native spelling is verified. A different
spelling fails with explicit missing/unexpected names, never substitutes a
duplicate percentile or certifies the distribution.
Optional VRT mosaics, spatial indexes and GDAL sidecars are not substitutes for
primary artifacts. GeoPackages are written inside the run-owned output directory
by both R tools (lidR's documented sf catalog driver is explicitly GPKG, not its
pinned default Shapefile). IDs/schema/tie ordering are tool-specific.
The harness validates artifacts then removes owned scratch; this is
on-disk persistence during the attempt, **not archival retention**.

## Spatial and algorithm contracts

All tools process input-file chunks with a requested **20 m neighboring-file
buffer**; do not merge all tiles into a new single workload. lidR has chunk size
0, chunk alignment (0,0), select `*`, no reader filter, and
`opt_wall_to_wall = FALSE` so raster methods do not silently replace the buffer
with 2 m. This disables lidR's wall-to-wall assumption, not the buffer itself.
lasR calls explicitly request buffer 20 and chunk 0. Its engine treats the
buffer as at least the algorithm's required minimum. LAStools calls explicitly
request `-buffered 20`, with `-use_orig_bb` for rasters to avoid exporting the
halo. A finite local buffer is not proof of seamless or globally identical TINs.

lidR's point-cloud catalog outputs retain half-open core ownership:
`xmin <= X < xmax`, `ymin <= Y < ymax`. On the isolated synthetic tile this
excludes records exactly at its header maximum X/Y, even without a neighboring
owner. This predates the buffer change (also reproduced with wall-to-wall TRUE).
Thus lidR normalization is **not** a guarantee of retaining the entire isolated
input record count. The fixture checks retained coordinates and normalized Z
against that owned population rather than hiding the loss with a count tolerance.
Native ownership and actual tile-edge assignments remain externally unverified;
normalization remains non-comparable.

Raster grids are axis-aligned, square cells, origin **(0,0)**, pixel edges at
integer multiples of resolution. Pixel metrics have 20 m resolution, surfaces
1 m. lidR metrics explicitly set `start = c(0,0)`; surface grids and lasR use
their pinned zero-origin alignment, checked on synthetic persisted outputs.
LAStools canopy grids request `-grid_ll 0 0`; the other native grid alignments
remain unverified. Nominal per-file extents are unbuffered header XY bounds
snapped outward to the grid. Sparse/partial-coverage tiles are **not** assumed to
cover a complete 1 km square. Interpolation/masking and edge ownership may
change the actual extent/valid support; no automatic crop, resample, hole filling
or alignment repair hides these differences. The artifact validator checks geometry,
neighboring boundaries and actual coverage before an attempt is accepted.

All raster artifacts are numeric GeoTIFFs, never hillshade/RGB previews.
R complex metrics have the ordered band names above. Single-band terrain,
DSM/CHM and mean outputs represent terrain elevation, surface elevation/height
and mean respectively; GDAL band descriptions for non-metric products are
tool-specific. lidR's pinned SpatRaster catalog driver uses Float32 with
NoData -1000000; lasR writes Float32 with NoData -9999. Native calls request
`-no_data -9999` and `-no_kml`; encoding/precision still needs native verification.
Read NoData from each file's metadata into NA; never compare raw sentinels,
fill missing cells with zero or treat a missing artifact as an empty raster.
Inputs/outputs colliding with a tool's sentinel cannot be declared comparable.

| Workload | Population and algorithm details |
|---|---|
| Normalization | R terrain from classification 2 **and 9**, linear Delaunay TIN; replace retained Z with Z minus interpolated terrain, preserving owned XY/other attributes. lidR `tin()` can use its pinned default k=3 IDW extrapolation outside the hull (power 1, rmax 50). LAStools `lasheight64 -replace_z` uses its native ground/triangulation rules, not a proven class-2/9 equivalent. No clamping of negative heights. Outputs use the tool's LAS writer/quantization, not a newly imposed point-format conversion. |
| DEM | R classes 2 and 9 explicitly; linear TIN, no maximum edge trim in lasR, lidR convex terrain mask and its TIN extrapolation behavior. `blast2dem64` now explicitly keeps classes 2/9 and requests `-elevation`; native `-kill 100` retains its 100 m maximum-edge behavior. These interpolation/support differences are non-comparable. |
| DSM1 | All classes/returns, highest original Z per 1 m cell; no interpolation, subcircle spreading or NA filling. lasR uses supported `dsm(tin = FALSE)`, not deprecated `chm()`. |
| DSM2 | All **first returns**, linear TIN at 1 m, no maximum-edge trim; lidR explicitly uses `dsmtin(highest = FALSE, max_edge = 0)` to remove its default highest-per-cell preselection, matching lasR `dsm(tin = TRUE)`. Degenerate triangles/boundaries still require validation. |
| Treetops | All points, original Z, 5 m local-maximum window and minimum Z=2 explicitly. lidR window is circular. lasR window shape, equal-height ties, exact threshold and boundary behavior are not certified equivalent. This basic task does not normalize and is not comparable to normalized pipeline treetops. |

### Complex candidates and the pinned-lasR blocker

Candidate order is **terrain triangulation -> original-elevation DTM ->
normalization -> normalized LAZ -> normalized maximum CHM**, followed in task 2
by normalized metrics and persisted normalized treetops. DTM is explicitly
rasterized **before** modifying point Z. Terrain uses classes 2 and 9;
CHM uses all normalized points at 1 m without interpolation; metrics use the
same nine definitions on normalized Z at 20 m; treetops use a 5 m window,
minimum normalized height 2. All products must be retained in the attempt's
scratch directory until accounting/validation. These are actual sequential
pipelines, **not sums of independently timed basic tasks**. No equivalent
lidR or LAStools end-to-end pipeline has been implemented.

Tiny flat-ground fixtures found that pinned lasR 0.21.2 `normalize()` and
`triangulate() + transform_with()` produce NA at some exact hull/triangle
boundaries. Writing those Z values to LAZ produced quantization warnings and
invalid elevations near -2147484 instead of zero. The user approved disabling
lasR normalization and both complex calls rather than timing an R guard,
dropping points, silently substituting a method or upgrading the pinned runtime.
Their runnable calls are empty, their status is `blocked`, and the runner
records the native defect as the skip diagnostic. Candidate artifacts remain
specified but **no complex workload is currently runnable**. The regression
characterizes NA before writing corrupt output and verifies all three skips.
An authorized runtime fix plus boundary/LAZ checks is required to re-enable them.
The user subsequently authorized **diagnostic execution without that fix** through
the explicit flag above, including both complex candidates. Default calls remain
disabled unless opted in; the known defect is not resolved or waived.

Basic DSMs are **not CHMs**: they use original elevations. Only the disabled
complex candidates normalize before computing canopy height.

## Artifact validation: validation-v1

Expectations are the product of unique input tile stems and workload artifacts,
not successful peer runs. Input manifests record unbuffered header bounds/counts,
CRS and file checksums. Each R raster/vector/LAZ must use its original tile stem;
complex candidates require every explicitly suffixed product in the table.
Missing files, unexpected/duplicate primary products (including halo exports),
zero-byte products and unreadable payloads fail. VRTs/indexes/GDAL sidecars do not
satisfy a primary requirement. Zero-area/nonfinite input bounds, duplicate stems
or header/payload count mismatches fail explicitly; an empty/degenerate input
file is not silently treated as successfully processed. Empty **cells** and
zero-treetop references are valid only with readable, correctly shaped products.

Validation reads each tile and overlapping neighbors within the 20 m buffer
outside timing. It checks their payload counts and finite XYZ, builds untimed
references, and verifies input byte identity again afterwards. This may be
expensive on full inputs; no header-count-only correctness shortcut is used.
Reference metrics are independent R group reductions on cell populations;
maximum DSM is an independent per-cell maximum. Terrain/first-return TIN and
lidR normalization/trees use public lidR algorithms on an explicit buffered
in-memory cloud, without catalog chunking/output drivers. Those checks establish
catalog/persistence consistency, not an independent proof of the algorithms.

Raster geometry must exactly match the contract's outward-snapped header extent,
zero-origin resolution and CRS, with the required number/order of named metric
bands. There is no automatic cropping, alignment repair, resampling or filling.
NoData masks must match the reference **exactly**, including singleton SD and
unoccupied cells. Decoded NaN/NA is NoData; infinite values fail. Legitimate values
colliding with either configured R NoData sentinel fail rather than disappearing.
Maximum-DSM target ownership uses half-open XY cells; metric ownership uses
the pinned R raster cell lookup (rightward X, downward Y on exact cell edges).
Both operate on original Z, including buffered contributions only to owned cells.
TIN support is explicitly compared, not just the range of finite elevations.

| Product | Numerical tolerance / required agreement |
|---|---|
| Means, nine metrics, terrain, maximum/first-return TIN DSM | For each finite value: absolute error <= `2e-6 + 2e-7 * abs(reference)`; exact support mask, band order and grid |
| lidR normalized LAZ | Exact owned count, original non-Z attributes and XY after attribute-based sorting; normalized Z error <= half the output LAS Z scale + `1e-6`; no relative tolerance |
| lidR treetops | Exact point population after XYZ sorting; XYZ error <= `1e-6`; original-Z minimum 2 and core bounds; no duplicate/nonfinite points |
| Native LAStools / lasR treetops | Structural checks only; **unverified**, never passed numerical validation |
| Blocked complex candidates | Every candidate product required; always rejected as disabled, never certified or dispatched |

LAS point ordering and tree IDs are not assumed stable. Normalization uses
half-open **header** core ownership, not a loose count tolerance. GPKGs require
one valid POINT layer, matching CRS and finite XYZ; elevation may be a Z geometry
ordinate or the persisted numeric `Z` attribute, as in pinned lidR's sf driver.
Empty vector references may have zero rows but still need a readable layer/schema.
Unknown/non-metre horizontal CRS leaves otherwise correct artifacts unverified;
even a declared projected CRS does not prove vertical units or source provenance.

Any execution failure, `FALSE` pipeline result or recorded execution warning
prevents passed validation. Warnings during reference/artifact inspection are
recorded as failures rather than suppressed into success. Partial processing
cannot pass because every input tile must supply every required artifact, with
full expected support/populations. Reports retain diagnostics, byte identities,
per-artifact error/support summaries and timestamps after scratch cleanup.
The compiler preserves execution flags separately and adds `validated_success`;
old records remain unverified and no maximum-observed-count heuristic remains.

### Boundary acceptance and remaining blockers

Tiny neighboring-tile fixtures pass persisted R means/nine-band metrics, sparse
cells, lidR normalization/attributes and distinct lidR treetops. They also expose
real surface differences: on the integer-bounded fixture lidR maximum DSM has
a different occupied-cell mask, while lasR terrain/DSMs extend beyond the
outward-snapped contract maximum. Fractional header maxima can also make lidR
surface extents round inward instead of outward. These are **rejected**, not
declared equivalent or silently repaired. The contract and timed calls are not
changed to hide them. Extent/support ownership needs a separately authorized
workload correction before accepting those surface combinations on such inputs.

LAStools numerical/filename/ownership conventions and entitlement, lasR treetop
ties/ownership, exact shared-edge/degenerate TIN behavior and real-input metadata
remain uncertified. The known pinned-lasR normalization defect and both complex
blocks remain unchanged. A passed synthetic fixture is not all-input/all-tool
equivalence, nor permission for a benchmark campaign.

### User-accepted surface discrepancies

The user explicitly authorized changing acceptance rules to defer known surface
extent/support discrepancies. `accept_known_surface_discrepancies = TRUE` is
an opt-in policy frozen in campaign identity; strict behavior remains the
default. It applies only to basic R terrain/DSM products, never metrics,
normalized point clouds, vectors, LAStools or complex diagnostic products.

- lasR terrain/DSM and lidR DSM extents may differ by **at most one cell per
  edge**. CRS, resolution, alignment, bands and artifact inventory still have
  to match. Larger extent differences fail.
- R terrain/DSM NoData/occupied-cell differences are accepted and counted.
  Both products must have finite coverage in their aligned overlapping core.
  Values on shared occupied cells must still satisfy the original numerical
  tolerances; infinity, corrupt/missing products and execution/reference warnings
  still fail. If the occupied populations are entirely disjoint, the report
  explicitly records that no numerical value comparison was possible.
- Validation compares aligned overlap in memory only; it does not crop,
  resample, fill or rewrite the saved product. Reports retain extent deltas,
  differing-mask counts, compared-value counts and exception reasons.

Such attempts are **`accepted_exception`, not `passed`**. They resume and enter
descriptive timing/utilization summaries when execution and timing are otherwise
eligible; `n_accepted_exception` identifies them. `validated_success` remains
false and `n_comparable` excludes them. These are user-accepted differences,
not a claim that formerly failing surfaces are numerically equivalent.

## Checks and remaining acceptance work

Run `Rscript --vanilla tests/testthat.R`; focused workload checks use
`testthat::test_dir("tests/testthat", filter = "workloads")` after activating
the project environment. Tests use tiny temporary synthetic clouds, real
pinned R tools and persisted GeoTIFF/GPKG files. No licensed native processing,
historical point-cloud processing, preparation or benchmark campaign is launched.
The native-cutoff placeholder is tested with process stand-ins.

The matched-definition groups must not be published as universally validated
comparisons. Native LAStools conventions/entitlement, recovered-data units/
provenance and the boundary limitations above remain blockers. Campaign identity,
artifact validation and report comparison-status policies are implemented.
The legacy compiler/site must not mix these changed
workloads with historical timings or claim cross-tool equivalence.

Sources: pinned package help for `lasR::metric_engine`, `rasterize`, `dsm`,
`dtm`, `normalize`, `transform_with`, `local_maximum`, `set_exec_options`, and
lidR's algorithm/catalog-driver help; rapidlasso
[lascanopy](https://downloads.rapidlasso.de/html/lascanopy_README.html) and
[blast2dem](https://downloads.rapidlasso.de/html/blast2dem_README.html) manuals.
Observed native behavior takes precedence over ambiguous metric documentation.
