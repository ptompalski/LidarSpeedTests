benchmark_full_plan <- function(stage_root) {
  plan <- benchmark_prepared_pilot_plan(stage_root)
  if (!identical(sort(plan$sources$tile), petawawa_tiles()) ||
      anyDuplicated(plan$densities) ||
      !setequal(plan$densities, c(1, 2, 5, 10, 20, 50))) {
    stop("Full benchmark requires all 100 approved tiles and all six prepared densities.")
  }
  plan
}

benchmark_full_datasets <- function(plan) {
  stats::setNames(paste0("tile_1000_density_", plan$densities),
                  paste0("ALS_1000_", plan$densities))
}

benchmark_full_init <- function(stage_root, workspace, lastools_path = NULL) {
  stage_root <- benchmark_directory(stage_root, "Prepared stage")
  plan <- benchmark_full_plan(stage_root)
  if (!is.character(workspace) || length(workspace) != 1L ||
      is.na(workspace) || !nzchar(trimws(workspace))) stop("Supply a new full benchmark workspace.")
  parent <- benchmark_directory(dirname(workspace), "Full workspace parent", TRUE)
  workspace <- file.path(parent, basename(workspace))
  if (benchmark_paths_overlap(stage_root, workspace)) {
    stop("Prepared stage and full benchmark workspace must be disjoint.")
  }
  if (file.exists(workspace) || isTRUE(fs::is_link(workspace))) {
    stop("Full benchmark workspace already exists; choose a new path or use --preflight.")
  }
  confirmed <- plan$fresh$config
  config <- dget("benchmark.example.R")
  config$prepared_stage_root <- stage_root
  config$datasets_org <- benchmark_full_datasets(plan)
  config$input_roots <- c(SSD = file.path(stage_root, "prepared", "1_org"))
  config$scratch_roots <- c(SSD = file.path(workspace, "scratch"))
  config$reports_path <- file.path(workspace, "reports")
  config$lastools_path <- benchmark_directory(
    if (is.null(lastools_path)) confirmed$lastools_path else lastools_path, "LAStools")
  config$input_metadata <- benchmark_input_metadata(list(
    horizontal_epsg = confirmed$crs, vertical_epsg = confirmed$vertical_epsg,
    xyz_units = "metre", confirmation = confirmed$metadata_confirmation
  ))
  if (!dir.create(workspace)) stop("Cannot create full benchmark workspace: ", workspace)
  for (name in c("scratch", "reports")) {
    if (!dir.create(file.path(workspace, name))) stop("Cannot create full benchmark directory: ", name)
  }
  path <- file.path(workspace, "benchmark.R")
  dput(config, path)
  cli::cli_alert_success("Full benchmark configuration created: {.path {path}}.")
  cli::cli_inform("Review the configuration, then run Rscript --vanilla R/3_runBenchmarks.R --preflight {shQuote(workspace)}")
  invisible(config)
}

benchmark_full_inputs <- function(config) {
  stage_root <- benchmark_directory(config$prepared_stage_root, "Prepared stage")
  plan <- benchmark_full_plan(stage_root)
  source_root <- file.path(stage_root, "prepared", "1_org")
  if (!identical(unname(config$input_roots), source_root) ||
      !identical(config$datasets_org, benchmark_full_datasets(plan))) {
    stop("Full benchmark input_roots/datasets_org must select the verified full preparation.")
  }
  records <- file.path(stage_root, c("ready.rds", "prepared/preparation.rds"))
  before <- benchmark_checksums(records)
  completed <- 0L
  total <- nrow(plan$sources) * length(plan$densities)
  progress <- cli::cli_progress_bar(name = "Check prepared tiles", total = total,
    auto_terminate = FALSE)
  on.exit(cli::cli_progress_done(id = progress,
    result = if (completed == total) "done" else "failed"), add = TRUE)
  for (dataset in config$datasets_org) {
    directory <- file.path(source_root, dataset)
    paths <- file.path(directory, paste0(plan$sources$tile, ".laz"))
    receipts <- paste0(paths, ".rds")
    expected <- c(basename(paths), basename(sub("\\.laz$", ".lax", paths)), basename(receipts))
    if (!setequal(list.files(directory, all.files = TRUE, no.. = TRUE), expected)) {
      stop("Prepared dataset inventory differs from the full manifest: ", directory)
    }
    for (i in seq_along(paths)) {
      receipt <- readRDS(receipts[i])
      if (!identical(receipt$index_validation$status, "passed") ||
          !identical(receipt$source_md5, plan$sources$md5[i]) ||
          !is.character(receipt$md5) || length(receipt$md5) != 2L || anyNA(receipt$md5) ||
          !identical(unname(tools::md5sum(c(paths[i], sub("\\.laz$", ".lax", paths[i])))),
                     receipt$md5)) {
        stop("Prepared tile checksum/completion receipt mismatch: ", paths[i])
      }
      completed <- completed + 1L
      cli::cli_progress_update(id = progress, set = completed)
    }
    records <- c(records, receipts)
  }
  provenance <- benchmark_checksums(records)
  if (!identical(benchmark_checksums(before$path), before)) {
    stop("Prepared readiness/manifest changed during preflight.")
  }
  list(stage_root = stage_root, method = plan$method, records = provenance)
}

benchmark_full_setup <- function(task = NULL) {
  config <- benchmark_validate_config(benchmark_load_config())
  setup <- new.env(parent = globalenv())
  setup$source <- function(file) sys.source(file, setup)
  sys.source("R/0_setup.R", setup)
  tasks <- setup$benchmark_tasks
  selection <- setup$benchmark_combinations
  if (!is.null(task)) {
    if (!is.character(task) || length(task) != 1L || is.na(task) || !nzchar(task)) {
      stop("Supply one task ID with --task.")
    }
    if (task == "simple_metrics_1a") task <- "pixel_metrics_1a"
    if (!task %in% tasks$task_id) {
      stop("Unknown task: ", task, ". Available tasks: ", paste(tasks$task_id, collapse = ", "))
    }
    selection <- selection[selection$task_id == task, , drop = FALSE]
    tasks <- tasks[tasks$task_id == task, , drop = FALSE]
  }
  if (!is.null(config$prepared_stage_root)) {
    cli::cli_inform("Verifying all prepared tile/index bytes against their completion receipts.")
    config$full_preparation <- benchmark_full_inputs(config)
  }
  benchmark_validate_selection(selection, tasks, setup$benchmark_runs, config)
  list(config = config, tasks = tasks,
       runs = setup$benchmark_runs, selection = selection)
}

benchmark_full_main <- function(args) {
  task <- NULL
  option <- which(args == "--task")
  if (length(option)) {
    if (length(option) != 1L || option == length(args) ||
        !nzchar(args[option + 1L]) || startsWith(args[option + 1L], "--")) {
      stop("Supply exactly one task ID after --task.")
    }
    task <- args[option + 1L]
    args <- args[-c(option, option + 1L)]
    if (length(args) && args[1] == "--init") stop("--task applies to --preflight/--run, not --init.")
  }
  if (length(args) && (!args[1] %in% c("--init", "--preflight", "--run") ||
      (args[1] == "--init" && !length(args) %in% c(3L, 4L)) ||
      (args[1] == "--preflight" && length(args) != 2L) ||
      (args[1] == "--run" && length(args) != 3L))) {
    stop("Use Rscript --vanilla R/3_runBenchmarks.R --init <prepared-stage> <new-workspace> [lastools-bin], ",
         "--preflight <workspace> [--task <task-id>], or --run <workspace> <campaign-id> [--task <task-id>].")
  }
  source("renv/activate.R")
  source("R/1_functions.R")
  source("R/6_pilot.R")
  source("R/2_data.R")
  if (length(args) && args[1] == "--init") {
    return(benchmark_full_init(args[2], args[3], if (length(args) == 4L) args[4]))
  }
  preflight <- length(args) && args[1] == "--preflight"
  if (length(args)) {
    workspace <- benchmark_directory(args[2], "Full benchmark workspace")
    variables <- c(LIDAR_BENCHMARK_CONFIG = file.path(workspace, "benchmark.R"))
    if (!preflight) variables <- c(variables, LIDAR_CAMPAIGN_ID = args[3])
    withr::local_envvar(variables)
  }
  if (!preflight) {
    id <- Sys.getenv("LIDAR_CAMPAIGN_ID")
    if (!nzchar(id)) stop("Set LIDAR_CAMPAIGN_ID or supply --run <workspace> <campaign-id>.")
    benchmark_file_component(id, "campaign_id")
  }
  cli::cli_h1("Full benchmark preflight")
  full <- benchmark_full_setup(task = task)
  if (!is.null(task)) cli::cli_inform("Selected task: {unique(full$selection$task_id)}.")
  schedule <- benchmark_schedule(full$selection, full$config$tools, full$config$scheduling)
  cli::cli_inform("{nrow(full$runs)} dataset/core/storage runs; {nrow(full$selection)} task/run combinations; {nrow(schedule)} scheduled entries including warmups and skips.")
  cli::cli_inform("Tools run sequentially. Unsupported/blocked workloads remain skipped; numerical validation policies are unchanged.")
  cli::cli_alert_success("Full benchmark preflight passed.")
  if (preflight) return(invisible(full))
  cli::cli_inform("Freezing campaign identity and input checksums; this can take several minutes.")
  campaign <- benchmark_campaign(id, full$selection, full$tasks, full$runs, full$config)
  info <- system_info()
  native <- campaign$manifest$identity$environment$native
  if (length(native)) info$lastools_version <- paste(vapply(names(native), function(name) {
    paste0(name, ": ", trimws(native[[name]]$version))
  }, character(1)), collapse = "\n")
  benchmark_execute_schedule(campaign, full$tasks, full$runs, full$config, info)
  invisible(campaign)
}

if (sys.nframe() == 0L) benchmark_full_main(commandArgs(TRUE))
