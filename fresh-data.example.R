# Trusted local configuration; copy outside git. No work happens when sourced.
list(
  root = "/data/nvme2/LidarSpeedTestWorkingDir/input_data_no_synthetic",
  protected_roots = "/data/nvme1/LidarSpeedTestData",
  archive_url = paste0(
    "https://opendata.nfis.org/downloads/petawawa/Raster/",
    "LiDAR%202012/PRF_LiDAR2012_LAS.zip"
  ),
  # Set only if an independently trusted archive MD5 is available.
  # A locally calculated MD5 establishes byte identity, not source authenticity.
  archive_md5 = NULL,
  crs = 2959L,
  vertical_epsg = 5713L,
  # Record source/custodian confirmation of EPSG:2959, EPSG:5713 and metre XYZ.
  metadata_confirmation = "Piotr Tompalski",
  seed = 20261022L,
  lastools_path = "/home/ptompals/opt/lastools/bin",
  executables = c(sort = "lassort64", index = "lasindex64"),
  tool_license = "Piotr Tompalski" # Confirm entitlement for both preparation tools.
)
