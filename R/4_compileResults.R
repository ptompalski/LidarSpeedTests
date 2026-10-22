# Select one campaign explicitly; historical CSVs require a separate legacy mode.
source("R/0_setup.R")
source("R/99_plotSettings.R")

campaign_id <- Sys.getenv("LIDAR_CAMPAIGN_ID")
legacy <- identical(Sys.getenv("LIDAR_RESULTS_MODE"), "legacy")
if (legacy && nzchar(campaign_id)) stop("Select a campaign OR legacy mode, not both.")
if (!legacy && !nzchar(campaign_id)) {
  stop("Set LIDAR_CAMPAIGN_ID, or explicitly set LIDAR_RESULTS_MODE=legacy.")
}

if (legacy) {
  files <- list.files(reports_path, full.names = TRUE, pattern = "\\.csv$")
  if (!length(files)) stop("No legacy result CSVs found in: ", reports_path)
  r <- purrr::map_dfr(files, function(path) {
    header <- names(readr::read_csv(path, n_max = 0, show_col_types = FALSE))
    if (any(c("campaign_id", "attempt_id", "identity_id") %in% header)) {
      stop("Campaign records cannot be compiled as legacy results: ", path)
    }
    spec <- readr::cols(
      .default = readr::col_character(), ...1 = readr::col_skip(),
      success = readr::col_double(), exit_status = readr::col_integer(),
      cores = readr::col_double(), density_points = readr::col_double(),
      cpu_threads = readr::col_double(), ram = readr::col_double(),
      total_point_count = readr::col_double(), density_pulses = readr::col_double(),
      file_count = readr::col_double(),
      output_file_count = readr::col_double(), start_time = readr::col_datetime(),
      finish_time = readr::col_datetime()
    )
    spec$cols <- spec$cols[intersect(names(spec$cols), header)]
    record <- readr::read_csv(path, col_types = spec, show_col_types = FALSE)
    if (nrow(readr::problems(record))) stop("Malformed legacy result CSV: ", path)
    record$legacy_source <- basename(path)
    record
  })
  # Historical runs have no campaign identity. Preserve every version/record;
  # do not select an independently "newest" tool version for each task.
  r$result_origin <- "legacy-unidentified"
  output <- "r.rds"
} else {
  r <- benchmark_read_campaign(reports_path, campaign_id)
  if (!nrow(r)) stop("Selected campaign has no completed attempts.")
  r$result_origin <- "campaign"
  output <- file.path(reports_path, "campaigns", campaign_id, "compiled.rds")
}

r <- compile_execution_outcomes(r)
r$tool <- factor(r$tool, levels = c("lidR", "lasR", "lastools"))
round_density <- function(x) ifelse(x <= 7.5, round(x), round(x / 5) * 5)
if ("density_points" %in% names(r)) {
  r <- r %>% mutate(
    density_points_rnd = round_density(density_points),
    density_labels = fct_reorder(
      paste0(round(density_points_rnd), " pts/m²"), density_points
    ),
    total_points_calc = custom_number_format(100 * 1000 * 1000 * density_points),
    data_labels = paste0(input_data_size, "B; ", total_points_calc, " pts")
  )
}

# Keep run_id and all repetition/attempt IDs; retries are never deduplicated.
if (all(c("run_id", "cores") %in% names(r))) {
  r <- r %>% separate(run_id, into = c(NA, "tile_size", NA, NA, "dir1", "dir2"),
                     remove = FALSE)
}

# The historical output-count heuristic is retained pending Chat 7.
r_expected <- r %>% group_by(tool, task_id) %>%
  dplyr::summarise(expected_output_file_count =
    if (all(is.na(output_file_count))) NA_real_
    else max(output_file_count, na.rm = TRUE), .groups = "drop")
r <- r %>% left_join(r_expected, by = c("tool", "task_id")) %>%
  mutate(success = if_else(
    !is.na(output_file_count) & !is.na(expected_output_file_count) &
      output_file_count < expected_output_file_count, 0, success))
saveRDS(r, output)
