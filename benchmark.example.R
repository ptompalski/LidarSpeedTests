list(
  # Use existing, separate directories. Relative paths resolve from the repository root.
  input_roots = c(SSD = "/path/to/input-storage"),
  scratch_roots = c(SSD = "/path/to/benchmark-scratch"),
  reports_path = "results",
  cores = c(1, 2, 4, 8, 16, 32),
  scheduling = list(
    repetitions = 3L, warmups = 1L, seed = 20261020L,
    cooldown_seconds = 5, cache_policy = "uncontrolled-warm",
    native_threads = 1L
  ),
  tools = c("lidR", "lasR", "lastools"),
  lastools_path = "/path/to/LAStools/bin",
  workstation_id = unname(Sys.info()[["nodename"]]),
  resource_monitoring = FALSE,
  # Monitoring is Windows-only and never silently enabled or substituted.
  system_monitoring_path = "system_monitoring",
  powershell = "powershell",
  monitor_script = "cpu_ram_usage.ps1"
)
