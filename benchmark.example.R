list(
  # Use existing, separate directories. Relative paths resolve from the repository root.
  input_roots = c(SSD = "/path/to/input-storage"),
  scratch_roots = c(SSD = "/path/to/benchmark-scratch"),
  reports_path = "results",
  # Optional named relative paths, overriding R/0_setup.R's historical layout.
  # datasets_org = c(ALS_1000_1 = "tile_1000_density_1"),
  cores = c(1, 2, 4, 8, 16, 32),
  scheduling = list(
    repetitions = 3L, warmups = 1L, seed = 20261020L,
    cooldown_seconds = 5, cache_policy = "uncontrolled-warm",
    native_threads = 1L
  ),
  tools = c("lidR", "lasR", "lastools"),
  # For R/6_pilot.R only: select exactly one source and set cores = c(1L, 2L).
  # prepared_stage_root = "/path/to/fresh-data/full", # Requires full/ready.rds.
  # historical_input_root = "/path/to/recovered-inputs/1_org",
  # Pilot input_roots must point to <parent of reports_path>/pilot-inputs.
  # Opt-in policies are frozen in the campaign; defaults retain strict validation.
  input_metadata = NULL,
  # Confirmed historical Petawawa declaration (does not rewrite source files):
  # input_metadata = list(
  #   horizontal_epsg = 2959L, vertical_epsg = 5713L, xyz_units = "metre",
  #   confirmation = "Custodian confirmed source lineage and unchanged XYZ units/datum."
  # ),
  accept_known_surface_discrepancies = FALSE,
  # Retain the full core; allow one aligned extra cell per edge, not mask/value changes.
  accept_known_extent_discrepancies = FALSE,
  # Exact lascanopy all-values-above-cutoff warning only; raw status 0/1 is retained.
  accept_known_lastools_cutoff_warning = FALSE,
  diagnostic_lasR = FALSE,
  lastools_path = "/path/to/LAStools/bin",
  workstation_id = unname(Sys.info()[["nodename"]]),
  resource_monitoring = FALSE,
  # Freeze output checks in campaign identity; performance-only skips numerical references.
  validation_mode = "numerical",
  # Linux /proc only. Samples/reports live inside each immutable attempt.
  # process-tree is reserved for future implementation, not a supported scope.
  monitoring = list(scope = "whole-machine", interval_seconds = 1)
)
