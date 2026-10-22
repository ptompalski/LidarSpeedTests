# Trusted local R configuration. Copy outside the repository and fill all NA fields.
list(
  source_root = NA_character_, # Already repaired <tile_id>.las/.laz files.
  output_root = NA_character_, # Existing empty directory, separate from inputs.
  source_citation = "Petawawa Research Forest ALS 2012; https://opendata.nfis.org/mapserver/PRF.html",
  license = NA_character_, # Confirm license/terms and attribution, not just accessibility.
  crs = NA_integer_, # Confirm projected EPSG code in metres from source documentation.
  repaired = TRUE,
  repair_provenance = NA_character_, # Who repaired it, method/script identity, original checksums.
  lastools_path = "/home/ptompals/opt/lastools/bin",
  executables = c(sort = "lassort64", index = "lasindex64"),
  tool_license = NA_character_, # Confirm entitlement for the exact selected tools.
  tiles = c("298000_5093000", "298000_5094000"),
  seed = 20261021L
)
