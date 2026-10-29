# Dataset management (Task 10)

**Status**: Implemented (2026-10-28) · **Code**: `src/lidarbench/datasets/` ·
**Registry**: [`datasets/`](../datasets/) · **CLI**: `lidarbench data …`

Benchmark data must be discoverable, verifiable and reusable on Linux, Windows
and macOS. A campaign on any supported OS can bind a dataset by its manifest.
The campaign then runs only if the local bytes equal the manifest exactly, so
every machine benchmarks the same input bytes.

This document is the normative description. It refines the sketch in
[ARCHITECTURE.md §7.5](ARCHITECTURE.md).

## 1. Concepts

| Term | Meaning |
|---|---|
| **Recipe** | Human-written TOML (`datasets/recipes/<id>.toml`, `lidarbench.dataset-recipe/1`). It sets the version, kind, license, declared metadata, file layout, provenance receipts, preparation method and the datasets the recipe yields. |
| **Manifest** | Generated JSON (`datasets/manifests/<dataset-id>.json`, `lidarbench.dataset/1`). It describes one directory exactly and is published once and never rewritten. |
| **Data root** | A machine-local directory named by a label, for example `petawawa-fresh`. Manifests store only `{root label, POSIX relative path}` and never an absolute path. |
| **Kind** | `source` is original data as obtained and is never prepared or modified. `derived` is produced from source data by a documented recipe. `fixture` is small synthetic test data and is never benchmark data. |
| **Role** | Every file has one role. `input` files are given to tools. `index` covers files such as `.lax`. `receipt` covers preparation receipts. `auxiliary` covers anything else that is declared. Files matched by `layout.exclude` are recorded as excluded and are not part of the dataset. |

Original source data and derived benchmark data are separate datasets, each
with its own manifest. A derived manifest names its sources in
`provenance.derived_from` (dataset ID and content digest).

## 2. Manifest `lidarbench.dataset/1`

```jsonc
{
  "schema": "lidarbench.dataset/1",
  "dataset_id": "petawawa-fresh-v4/tile_1000_density_1",   // <recipe>/<name>, lowercase
  "version": "fresh-petawawa-v4",                          // explicit dataset version
  "title": "…", "kind": "derived", "recipe": "petawawa-fresh-v4",
  "location": {"root": "petawawa-fresh", "path": "full/prepared/1_org/tile_1000_density_1"},
  "layout": {"inputs": ["*.laz"], "indexes": ["*.lax"], "receipts": ["*.laz.rds"],
             "auxiliary": [], "exclude": [], "las_headers": "required"},
  "content_sha256": "…", "file_count": 300, "total_bytes": 0,
  "files": [{"path": "297000_5094000.laz", "role": "input", "bytes": 0,
             "sha256": "…", "md5": "…",                      // md5 only to cross-check R receipts
             "las": {"version": "1.2", "point_format": 1, "compressed": true, "points": 0,
                     "scale": [0,0,0], "offset": [0,0,0], "min": [0,0,0], "max": [0,0,0],
                     "crs": "wkt|geotiff|null"}}],
  "declared_metadata": {"horizontal_epsg": 2959, "vertical_epsg": 5713,
                        "xyz_units": "metre", "confirmation": "…"},
  "license": {"name": "…", "url": "…", "attribution": "…",
              "redistribution": "permitted|permitted-with-attribution|not-permitted|unknown",
              "notes": "…"},
  "provenance": {"source_citation": "…", "source_url": "…",
                 "derived_from": [{"description": "…", "dataset_id": "…", "content_sha256": "…"}],
                 "preparation": {"engine": "r-cli", "method": "…", "…": "…",
                                 "dataset_parameters": {}},
                 "records": [{"location": {"root": "…", "path": "full/ready.rds"},
                              "bytes": 0, "sha256": "…", "md5": "…", "description": "…"}],
                 "details": {"legacy_receipts": {}}, "notes": ["…"]},
  "registration": {"lidarbench_version": "…", "registered_at": "…Z", "registered_on": "Linux"}
}
```

Rules:

- **Identity.** `content_sha256` is the canonical digest of the sorted
  `(path, role, bytes, sha256)` inventory, so it depends only on bytes and
  roles. `manifest_sha256` is the canonical digest of the whole record and can
  be pinned by a campaign. `content_sha256`, `file_count` and `total_bytes` are
  recomputed when a manifest is read, so a hand-edited inventory is rejected.
- **Strict parsing**, as for campaign configs: unknown keys are errors, values
  are never coerced, and every error names its key.
- **Portability.** Paths are POSIX relative paths. They may not contain `..`,
  drives, backslashes, Windows-reserved names (`CON`, `NUL`, `COM1`, …),
  `<>:"\|?*` or control characters, or end in a dot or space. Two paths that
  differ only in case are refused because they cannot coexist on Windows or
  macOS. Links (including Windows reparse points) and special files are
  refused.
- **LAS/LAZ header facts** come from the stdlib header reader
  (`datasets/lasheader.py`), so no LiDAR library is needed.
- **Declared metadata is declared, never inferred.** The fixtures carry no CRS.
  The Petawawa data are declared as EPSG:2959 / EPSG:5713 / metre, as confirmed
  in the R preparation configuration.
- **Write-once.** Registering the same content again is a no-op. Different
  content under an existing ID is refused, so changed data needs a new version
  or ID.

Differences from the §7.5 sketch: companions (`.lax`) are flat entries with
role `index` rather than nested; the root is a label resolved per machine; the
license and receipts are structured objects.

## 3. Data roots `lidarbench.data-roots/1`

```toml
schema = "lidarbench.data-roots/1"
[roots]
petawawa-fresh = "/data/nvme2/LidarSpeedTestWorkingDir/input_data_no_synthetic"
fixtures = "D:/lidar/fixtures"
```

Lookup order: the per-user file (`%APPDATA%\lidarbench\data-roots.toml`,
`~/Library/Application Support/lidarbench/data-roots.toml`, or
`$XDG_CONFIG_HOME|~/.config/lidarbench/data-roots.toml`). `$LIDARBENCH_DATA_ROOTS`
names a file that replaces the per-user file. `--root LABEL=PATH` overrides
any label. A resolved dataset directory must stay inside its root after links
are resolved.

## 4. Commands

| Command | Purpose |
|---|---|
| `lidarbench data list` | Registered datasets (kind, version, size, content digest, whether the root is configured here) and recipes with unregistered datasets. |
| `lidarbench data show ID [--files]` | One manifest. |
| `lidarbench data roots` | Data roots configured on this machine. |
| `lidarbench data verify ID… \| --all [--quick] [--provenance]` | Read-only verification (§5). |
| `lidarbench data register RECIPE [--dataset ID] [--legacy-export FILE \| --skip-legacy-crosscheck]` | Hash existing data and publish manifests. Needs no R and changes no data (§6). |
| `lidarbench data prepare RECIPE [--discard-interrupted] [--plan] [--run-r-stage=STAGE --r-config FILE]` | Generates `python` recipes (fixtures). For `r-cli` recipes it prints or runs the unchanged R CLI (§7). |
| `lidarbench data download RECIPE --accept-bytes N` | Explicit download of original source data, only where the terms are established (§9). |

Common options: `--registry DIR` (default `datasets/` of the checkout or
`$LIDARBENCH_DATASET_REGISTRY`), `--roots-file`, `--root LABEL=PATH`, `--json`.
Exit status: `0` ok; `1` failed or refused; `2` usage or configuration error;
`3` nothing failed but a dataset is unavailable here (root not configured).

## 5. Verification

- `full` (default): hashes every file (SHA-256, plus MD5 where the manifest
  records one) and compares the exact inventory (missing, extra, role), sizes
  and LAS header facts. Status: `verified`.
- `--quick`: compares inventory, sizes and headers without hashing. Status:
  `consistent`, never `verified`; a same-size byte change is not detected.
- `--provenance`: also checks the external receipts (`provenance.records`)
  byte for byte.
- `unavailable`: the root label is not configured on this machine. Other
  problems are `failed`.

## 6. Registration and the legacy R receipts

Registration refuses directories that show an active or interrupted
preparation: `*.lidarbench-partial-*` or `*.pending-*` entries, or the R CLI's
`.preparation-lock` in the root. It also refuses unclassified files, missing
receipts and inputs that are not readable LAS/LAZ when headers are required.

The historical preparation recorded **MD5** receipts in R `.rds` files.
`bridges/r/export_petawawa_receipts.R` reads their plain fields once with
base R (no renv or lidR needed; embedded S4 objects are never touched). It
recomputes nothing, writes nothing below the root and exports no absolute
paths. Of the LAStools banners it keeps only the build number; licensing text
is never exported. The export (`lidarbench.petawawa-receipts/1`) for the
existing full stage is committed as
[`datasets/legacy-receipts/petawawa-fresh-v4-full.json`](../datasets/legacy-receipts/petawawa-fresh-v4-full.json),
so the cross-check can be repeated without R:

```bash
Rscript --vanilla bridges/r/export_petawawa_receipts.R \
    --root /path/to/fresh-root --stage full --output new-export.json   # once, base R
lidarbench data register petawawa-fresh-v4 --root petawawa-fresh=/path/to/fresh-root \
    --legacy-export datasets/legacy-receipts/petawawa-fresh-v4-full.json
```

Cross-checks (`datasets/legacy_petawawa.py`); any disagreement refuses
registration:

- `ready.rds` status is `data-qc-passed`. The readiness policy and the
  preparation method equal the recipe method (`fresh-petawawa-v4`). A
  negative-Z transition, if present, ends at that method.
- The archive MD5 is identical in `download.rds`, `ready.rds`,
  `preparation.rds` and `stage-identity.rds`. The MD5 of the hashed
  `full/prepared/preparation.rds` bytes equals `ready.rds$preparation_md5`.
- Prepared densities: every `.laz` and `.lax` MD5 equals its per-tile receipt.
  The header point count equals the receipt's point count. Index-query
  validation is recorded as `passed`. The source MD5 equals the repair receipt.
  No file lacks a receipt.
- Repaired tiles: the tile MD5 equals the repair receipt and the preparation
  sources. Receipt MD5s equal `preparation.rds$fresh$repairs_md5`. A receipt
  without a policy field is accepted only if its audit has no negative source
  Z. This mirrors, and does not change, the R function
  `fresh_check_repair_policy()`.
- Archive: its MD5 and size equal `download.rds`.

Receipt facts (policy text, repair audit counts per action, seeds, tool builds
and MD5s, runtime versions, transitions) are copied into
`provenance.details.legacy_receipts`. MD5s are content identifiers, not
authenticity proofs. `--skip-legacy-crosscheck` registers without the
cross-check and records that in the manifest notes.

## 7. Preparation

**Python (`engine = "python"`, fixtures only).** `synthetic-tiles-v1`
(`datasets/fixtures.py`) writes LAS 1.2 point-format-1 tiles using integer
arithmetic, SplitMix64 and single correctly rounded float operations. The bytes
are therefore identical on every OS, and the committed fixture manifests verify
everywhere (tests pin this). Interruption safety:

1. An ownership marker `.<name>.lidarbench-partial-<token>.json` is published
   first.
2. Generation writes into `.<name>.lidarbench-partial-<token>/` beside the
   target.
3. The staging directory is inventoried and compared with any registered
   manifest, then renamed into place. The marker is removed, and only then is
   the manifest published.

A crash leaves a staging directory and its marker, never a partial dataset
under its final name. Later preparations refuse to continue until
`--discard-interrupted` removes the leftovers. Only leftovers with a matching
marker that contain no links or special files are removed. An existing target
is never overwritten. If it already verifies against its manifest, it is
reported as `already-prepared`.

**R (`engine = "r-cli"`).** The Petawawa preparation is **not** reproduced,
simplified or changed by lidarbench. `data prepare petawawa-fresh-v4` prints
the documented commands and dependencies.
`--run-r-stage=--verify-full --r-config cfg.R` runs one stage of
`R/prepare_fresh_data.R` unchanged from the repository root. That CLI keeps
its own identities, locks, resume rules and receipts. Register its verified
output afterwards.

## 8. Campaigns

```toml
[data]                         # optional
registry = "../datasets"       # default: checkout datasets/ or $LIDARBENCH_DATASET_REGISTRY
roots_file = "data-roots.toml" # default: $LIDARBENCH_DATA_ROOTS or the per-user file
roots = { petawawa-fresh = "/data/…" }   # inline overrides

[[datasets]]
id = "d1"
manifest = "petawawa-fresh-v4/tile_1000_density_1"
manifest_sha256 = "…"          # optional pin of the exact manifest
```

A dataset uses either `path` (unchanged behaviour and frozen record) or
`manifest`. For manifest datasets the directory is resolved through the data
roots, the inputs are the manifest's `input` files, and the campaign's full
input hash must equal the manifest exactly. Otherwise campaign creation and
`run` fail with `IdentityError`. The manifest reference (ID, version, kind,
location, `content_sha256`, `manifest_sha256`) is frozen in the `config` and
`datasets` identity components. Per-entry fingerprints and the input-tree guard
are unchanged.

## 9. Downloads

Only `source` recipes may declare `[download]`, and only when the license's
redistribution is `permitted` or `permitted-with-attribution`; this is
enforced at parse time. The URL must use HTTPS, and redirects to anything else
are refused. The user must acknowledge the exact size with `--accept-bytes`.
Transfers resume into `.<file>.lidarbench-partial-download`, which blocks
registration until it is resolved. The file is published only after its size
and checksum match. No current Petawawa recipe declares a download (§10). The
R CLI's `--download` stage remains the documented way to obtain the archive.

## 10. Licensing and provenance

| Dataset | License | Redistribution | Provenance |
|---|---|---|---|
| `petawawa-als-2012/archive` (source) | Open Government Licence – Canada, **as recorded by the preparation configuration** | `unknown`: not confirmed by lidarbench against a custodian licence statement (the NFIS portal page does not state it and the download directory is not listable) | Natural Resources Canada / Canadian Forest Service, Canadian Wood Fibre Centre; Petawawa Research Forest LiDAR 2012; `https://opendata.nfis.org/downloads/petawawa/Raster/LiDAR%202012/PRF_LiDAR2012_LAS.zip`. The archive MD5 was calculated locally with no independent checksum: it establishes byte identity, not authenticity. |
| `petawawa-fresh-v4/*` (derived) | as above, with attribution "Contains information licensed under the Open Government Licence – Canada." | `unknown` | Derived from the archive by `R/prepare_fresh_data.R` (policy `fresh-petawawa-v4`, seed 20261022, LAStools lassort/lasindex build 260914, R 4.6.1, lidR 4.3.3). Points above the source density are seeded synthetic copies (derived, not acquisition data). Readiness is data QC only, not cross-tool benchmark acceptance. |
| `synthetic-tiles-v1/*` (fixture) | Synthetic data generated by lidarbench (no third-party content) | `permitted` | `datasets/fixtures.py`. |

Confirm the Petawawa licence with the custodian (contact on the NFIS PRF page)
before publishing any copy or adding a download. lidarbench never records
LAStools licensing text, only build numbers. The tool-licence confirmation is
exported only as `tool_license_confirmed`.

Provenance history recorded in the Petawawa manifests:

- The root and stage identities were created under `fresh-petawawa-v3`. They
  moved to v4 through the documented `--resume-negative-z-full` transition
  (`negative-z-transition.rds`).
- The full stage was resumed in parallel (`parallel-transition.rds`).
- The repair receipts predate the policy field and are accepted under v4 by
  the R rule described in §6.

## 11. Registered datasets (this repository)

Registered on Linux from `/data/nvme2/LidarSpeedTestWorkingDir/input_data_no_synthetic`
(root label `petawawa-fresh`) on 2026-10-28, with the full MD5 cross-check
against the R receipts. Use `lidarbench data list` for the current digests.

| Dataset | Files | Size | Inputs |
|---|---|---|---|
| `petawawa-als-2012/archive` | 1 | 9.1 GB | `Petawawa2012.zip` |
| `petawawa-fresh-v4/repaired` | 200 | 11 GB | 100 repaired tiles (intermediate; not a benchmark input) |
| `petawawa-fresh-v4/tile_1000_density_{1,2,5,10,20,50}` | 300 each | 0.8–30 GB | 100 tiles + `.lax` + per-tile receipt |
| `synthetic-tiles-v1/{pair,grid}` | 2 / 6 | 28 KB / 337 KB | fixture tiles |

Not registered: the two-tile `pilot` stage (it is QC for the same tiles), and
the earlier recovered dataset described by the protected
`input-manifests/petawawa-recovered-pilot.csv` (legacy; superseded by the
fresh preparation).

## 12. Remaining preparation dependencies

Registering, listing, verifying and benchmarking from manifests needs only
Python (stdlib and psutil) on any OS. **Reproducing** the Petawawa preparation
still requires the original tooling:

- R 4.6.1 with the `renv.lock` library. The receipts record lidR 4.3.3,
  sf 1.1.3, GDAL 3.12.2, PROJ 9.7.1 and GEOS 3.14.1. The CLI also uses rlas,
  data.table, withr, fs, cli and callr.
- Licensed LAStools `lassort` and `lasindex`. The stage receipts record build
  260914 and the executables' MD5s.
- `curl` for the resumable archive download (9.1 GB) and the trusted R
  configuration (`fresh-data.example.R`), including the CRS/datum confirmation
  and the tool-licence confirmation.
- The protected inputs `ALS_tiles.gpkg`, `R/2_data.R`, `R/1_functions.R` and
  `renv.lock`. The receipts record their MD5s as the implementation identity.
- Linux x86_64 is the only platform on which the preparation has run.
  Byte-identical reproduction with other R, lidR or LAStools versions or other
  platforms is not established. A rerun therefore produces a new dataset
  version, which is registered and compared rather than assumed equal.

Open items: confirm the Petawawa licence; run `data verify` natively on
Windows and macOS (CI with the fixtures; real data on a workstation).
