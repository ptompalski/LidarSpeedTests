metrics_multiple <- function(z) {
  if (!is.numeric(z) || any(!is.finite(z))) {
    stop("Metric elevations/heights must be finite numeric values.")
  }
  bands <- c("z_max", "z_mean", "z_sd", "z_above2",
             "z_p5", "z_p25", "z_p50", "z_p75", "z_p95")
  if (!length(z)) return(as.list(stats::setNames(rep(NA_real_, 9L), bands)))
  values <- c(max(z), mean(z), stats::sd(z), mean(z > 2),
              stats::quantile(z, c(.05, .25, .50, .75, .95),
                              type = 7, names = FALSE))
  as.list(stats::setNames(values, bands))
}

benchmark_workload_contract <- function(task_id, tool) {
  ids <- c("normalization", "pixel_metrics_1a", "pixel_metrics_2", "generate_DEM",
           "generate_DSM1", "generate_DSM2", "tree_detection",
           "complex_task1", "complex_task2")
  if (!task_id %in% ids || !tool %in% c("lidR", "lasR", "lastools")) {
    stop("Unknown workload contract: ", task_id, "/", tool)
  }
  supported <- if (startsWith(task_id, "complex_")) tool == "lasR" else
    !(tool == "lastools" && task_id %in% c("generate_DSM2", "tree_detection"))
  blocked <- tool == "lasR" &&
    task_id %in% c("normalization", "complex_task1", "complex_task2")
  implemented <- supported
  supported <- supported && !blocked
  matched <- supported && tool != "lastools" &&
    task_id %in% c("pixel_metrics_1a", "pixel_metrics_2", "generate_DSM1",
                   "generate_DSM2")
  artifacts <- switch(task_id,
    normalization = "*.laz",
    pixel_metrics_1a = if (tool == "lastools") "*_avg.tif" else "*.tif",
    pixel_metrics_2 = if (tool == "lastools")
      c("*_max.tif", "*_avg.tif", "*_std.tif", "*_dns.tif", "*_p*.tif")
      else "*.tif",
    generate_DEM = "*.tif", generate_DSM1 = "*.tif", generate_DSM2 = "*.tif",
    tree_detection = "*.gpkg",
    complex_task1 = c("*.laz", "*_dtm.tif", "*_chm.tif"),
    complex_task2 = c("*.laz", "*_dtm.tif", "*_chm.tif",
                      "*_metrics.tif", "*_trees.gpkg"))
  status <- if (blocked) "blocked" else if (!supported) "unsupported" else if (matched)
    "matched_definition_pending_validation" else "non_comparable"
  note <- if (blocked)
      "Disabled: pinned lasR 0.21.2 TIN normalization produces NA at ground hull boundaries and corrupt LAZ elevations; candidate only." else
    if (!supported) "No implemented pipeline; record an unsupported/skipped attempt." else
    if (tool == "lastools")
      "Native cutoff/filter flags are explicit; native numerical, geometry and output conventions are unverified." else
    if (startsWith(task_id, "complex_"))
      "lasR-only sequential pipeline; not equivalent to a sum of independent basic tasks." else
    if (task_id %in% c("normalization", "generate_DEM"))
      "Class 2/9 TIN; lidR extrapolation and lasR outside-hull handling differ." else
    if (task_id == "tree_detection")
      "Local maxima window/tie/boundary semantics are not established as equivalent." else
      "Same target definition; tiny fixtures do not establish neighboring-tile correctness."
  list(
    workload_contract = "chat5-v1", comparison_status = status,
    comparison_group = if (matched) paste0("chat5-v1/", task_id) else NA_character_,
    comparison_note = note, supported = supported,
    z = if (startsWith(task_id, "complex_")) "original Z for terrain; normalized Z thereafter" else "original Z",
    population = switch(task_id,
      normalization = if (tool == "lidR")
        "half-open catalog core points retained; terrain from classes 2 and 9" else
        "all points transformed; native terrain and boundary handling",
      generate_DEM = "classes 2 and 9",
      generate_DSM2 = "all first returns; no highest-per-cell preselection",
      "all points; no return/class filter"),
    buffer = 20, origin = c(0, 0),
    resolution = if (grepl("^pixel", task_id)) 20 else
      if (task_id %in% c("generate_DEM", "generate_DSM1", "generate_DSM2") ||
          startsWith(task_id, "complex_")) 1 else NA_real_,
    bands = if (task_id == "pixel_metrics_2") names(metrics_multiple(numeric())) else
      if (task_id == "pixel_metrics_1a") "z_mean" else character(),
    artifacts = if (implemented) artifacts else character(),
    validation = if (implemented) benchmark_artifact_specs(task_id, tool) else list()
  )
}

benchmark_height_cutoff <- function(ctg) {
  z <- ctg@data$Min.Z
  if (!is.numeric(z) || !length(z) || any(!is.finite(z))) {
    stop("Cannot define an all-point height cutoff without finite catalog Min.Z headers.")
  }
  minimum <- min(z)
  cutoff <- minimum - max(1, abs(minimum) * .Machine$double.eps * 4)
  if (!is.finite(cutoff) || cutoff >= minimum) stop("Cannot represent a cutoff below input Z.")
  format(cutoff, digits = 17, scientific = FALSE, trim = TRUE)
}

benchmark_directory <- function(path, label, writable = FALSE) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path) || !dir.exists(path)) {
    stop(label, " must be an existing directory: ", paste(path, collapse = ", "))
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (file.access(path, if (writable) 2 else 4) != 0) {
    stop(label, " is not ", if (writable) "writable: " else "readable: ", path)
  }
  if (.Platform$OS.type != "windows" && file.access(path, 1) != 0) {
    stop(label, " is not searchable: ", path)
  }
  path
}

benchmark_path_equal <- function(a, b) {
  if (.Platform$OS.type == "windows") {
    a <- tolower(a)
    b <- tolower(b)
  }
  identical(a, b)
}

benchmark_path_within <- function(path, parent) {
  if (.Platform$OS.type == "windows") {
    path <- tolower(path)
    parent <- tolower(parent)
  }
  identical(path, parent) || startsWith(path, paste0(sub("/+$", "", parent), "/"))
}

benchmark_paths_overlap <- function(a, b) {
  benchmark_path_within(a, b) || benchmark_path_within(b, a)
}

benchmark_validate_scratch_root <- function(root, protected_paths) {
  root <- benchmark_directory(root, "Scratch root")
  home <- normalizePath(path.expand("~"), winslash = "/", mustWork = TRUE)
  if (benchmark_path_equal(root, dirname(root)) ||
      grepl("^[A-Za-z]:/?$|^//[^/]+/[^/]+/?$", root) ||
      benchmark_path_equal(root, home) ||
      benchmark_path_equal(root, normalizePath(getwd(), winslash = "/"))) {
    stop("Scratch root cannot be a filesystem root, home, or working directory.")
  }
  benchmark_directory(root, "Scratch root", writable = TRUE)
  for (path in protected_paths) {
    path <- benchmark_directory(path, "Protected path")
    if (benchmark_paths_overlap(root, path)) {
      stop("Scratch root overlaps an input or report directory: ", path)
    }
  }
  root
}

benchmark_load_config <- function(
    path = Sys.getenv("LIDAR_BENCHMARK_CONFIG", "benchmark.local.R"),
    required = TRUE) {
  if (!file.exists(path)) {
    if (!required && !nzchar(Sys.getenv("LIDAR_BENCHMARK_CONFIG"))) return(NULL)
    stop("Missing benchmark configuration: ", path,
         ". Copy benchmark.example.R to benchmark.local.R and edit its paths.")
  }
  config <- dget(path)
  if (!is.list(config)) stop("Benchmark configuration must be a list.")
  config
}

benchmark_validate_config <- function(config) {
  if (!is.list(config)) stop("An explicit benchmark configuration is required.")
  for (field in c("input_roots", "scratch_roots")) {
    paths <- config[[field]]
    if (!is.character(paths) || !length(paths) || is.null(names(paths)) ||
        anyNA(names(paths)) || anyDuplicated(names(paths)) ||
        any(!grepl("^[A-Za-z0-9-]+$", names(paths)))) {
      stop(field, " must be directories named with unique drive labels (letters, digits, hyphens).")
    }
    config[[field]] <- vapply(paths, benchmark_directory, character(1),
                              label = field, writable = field == "scratch_roots")
  }
  config$reports_path <- benchmark_directory(config$reports_path, "Reports", TRUE)
  for (input in config$input_roots) {
    if (benchmark_paths_overlap(input, config$reports_path)) {
      stop("Reports must not overlap input directories.")
    }
  }
  if (!is.numeric(config$cores) || !length(config$cores) ||
      anyNA(config$cores) || any(!is.finite(config$cores)) ||
      any(config$cores < 1 | config$cores != floor(config$cores))) {
    stop("cores must contain positive integers.")
  }
  config$scheduling <- benchmark_scheduling_config(config$scheduling)
  if (!is.character(config$tools) || !length(config$tools) ||
      anyNA(config$tools) || anyDuplicated(config$tools) ||
      any(!config$tools %in% c("lidR", "lasR", "lastools"))) {
    stop("tools must explicitly select lidR, lasR, and/or lastools.")
  }
  if (!is.logical(config$resource_monitoring) ||
      length(config$resource_monitoring) != 1L || is.na(config$resource_monitoring)) {
    stop("resource_monitoring must be TRUE or FALSE.")
  }
  config$monitoring <- benchmark_monitoring_config(config$monitoring)
  if (config$resource_monitoring) {
    config <- benchmark_validate_monitor(config)
  }
  protected <- c(config$input_roots, config$reports_path,
                 if (config$resource_monitoring) config$system_monitoring_path)
  config$scratch_roots <- vapply(config$scratch_roots, benchmark_validate_scratch_root,
                                 character(1), protected_paths = protected)
  if ("lastools" %in% config$tools) {
    config$lastools_path <- benchmark_directory(config$lastools_path, "LAStools")
  }
  benchmark_file_component(config$workstation_id, "workstation_id")
  config
}

benchmark_scheduling_config <- function(policy = NULL) {
  defaults <- list(repetitions = 3L, warmups = 1L, seed = 20261020L,
                   cooldown_seconds = 5, cache_policy = "uncontrolled-warm",
                   native_threads = 1L)
  if (is.null(policy)) policy <- list()
  if (!is.list(policy) || (is.null(names(policy)) && length(policy) > 0L) ||
      anyNA(names(policy)) ||
      anyDuplicated(names(policy)) || any(!names(policy) %in% names(defaults))) {
    stop("scheduling must be a named list of supported policy fields.")
  }
  for (name in names(policy)) defaults[name] <- policy[name]
  for (name in c("repetitions", "warmups", "seed", "native_threads")) {
    value <- defaults[[name]]
    minimum <- if (name %in% c("warmups", "seed")) 0 else 1
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value < minimum || value != floor(value) ||
        value > .Machine$integer.max) {
      stop("scheduling$", name, " must be one integer >= ", minimum, ".")
    }
    defaults[[name]] <- as.integer(value)
  }
  if (defaults$native_threads != 1L) {
    stop("scheduling$native_threads must be 1: parallelize files, not nested native threads.")
  }
  seconds <- defaults$cooldown_seconds
  if (!is.numeric(seconds) || length(seconds) != 1L || is.na(seconds) ||
      !is.finite(seconds) || seconds < 0) {
    stop("scheduling$cooldown_seconds must be one finite nonnegative number.")
  }
  if (!identical(defaults$cache_policy, "uncontrolled-warm")) {
    stop("Only cache_policy = 'uncontrolled-warm' is implemented; cold cache is not verified.")
  }
  defaults
}

benchmark_schedule <- function(selection, tools, policy) {
  policy <- benchmark_scheduling_config(policy)
  if (!is.data.frame(selection) || !all(c("task_id", "run_id") %in% names(selection)) ||
      !nrow(selection) || anyNA(selection[c("task_id", "run_id")]) ||
      anyDuplicated(selection[c("task_id", "run_id")])) {
    stop("Schedule selection must contain unique, nonmissing task_id/run_id pairs.")
  }
  if (!length(tools) || anyNA(tools) || anyDuplicated(tools) ||
      any(!tools %in% c("lidR", "lasR", "lastools"))) stop("Invalid schedule tools.")
  selection <- as.data.frame(selection[c("task_id", "run_id")])
  selection <- selection[order(selection$task_id, selection$run_id), , drop = FALSE]
  tools <- sort(tools)
  withr::with_seed(policy$seed, {
    rows <- list()
    for (phase in c("warmup", "measurement")) {
      count <- if (phase == "warmup") policy$warmups else policy$repetitions
      bases <- lapply(seq_len(nrow(selection)), function(i) sample(tools, length(tools)))
      for (repetition in seq_len(count)) {
        for (i in sample.int(nrow(selection))) {
          order <- bases[[i]][(seq_along(tools) + repetition - 2L) %% length(tools) + 1L]
          rows[[length(rows) + 1L]] <- data.frame(
            task_id = selection$task_id[i], run_id = selection$run_id[i],
            tool = order, phase = phase, repetition_id = repetition,
            tool_position = seq_along(order), stringsAsFactors = FALSE
          )
        }
      }
    }
    schedule <- do.call(rbind, rows)
    rownames(schedule) <- NULL
    schedule$schedule_index <- seq_len(nrow(schedule))
    schedule
  }, .rng_kind = "Mersenne-Twister", .rng_normal_kind = "Inversion",
  .rng_sample_kind = "Rejection")
}

benchmark_execute_schedule <- function(campaign, tasks, runs, config, sysinfo) {
  benchmark_campaign_check(campaign, tasks, runs, config)
  schedule <- campaign$manifest$identity$schedule
  for (i in seq_len(nrow(schedule))) {
    row <- schedule[i, ]
    # Warm-ups are repeated on resume; old process/cache state is not reusable.
    flags <- stats::setNames(as.list(c("lidR", "lasR", "lastools") == row$tool),
                             c("run_lidR", "run_lasR", "run_lastools"))
    force <- stats::setNames(as.list(rep(row$phase == "warmup", 3L)),
                             c("force_rerun_lidR", "force_rerun_lasR", "force_rerun_lastools"))
    do.call(benchmark_run, c(list(
      task_id = row$task_id, run_id = row$run_id, benchmark_tasks = tasks,
      benchmark_runs = runs, config = config, sysinfo = sysinfo, campaign = campaign,
      repetition_id = row$repetition_id, phase = row$phase,
      schedule_index = row$schedule_index
    ), flags, force))
  }
  invisible(NULL)
}

benchmark_make_runs <- function(settings, config) {
  runs <- expand.grid(
    dataset = names(settings$datasets_org), cores = config$cores,
    drive_out = names(config$scratch_roots), drive_in = names(config$input_roots),
    stringsAsFactors = FALSE
  )
  runs <- runs[c("drive_in", "drive_out", "cores", "dataset")]
  runs$ID <- with(runs, paste(dataset, cores, drive_in, drive_out, sep = "_"))
  runs$dir_in_org <- file.path(
    config$input_roots[runs$drive_in], settings$datasets_org[runs$dataset]
  )
  norm <- settings$datasets_norm[runs$dataset]
  runs$dir_in_norm <- ifelse(
    is.na(norm), NA_character_, file.path(config$input_roots[runs$drive_in], norm)
  )
  runs$scratch_root <- unname(config$scratch_roots[runs$drive_out])
  runs
}

benchmark_validate_run_paths <- function(config, dir_in, scratch_root) {
  dir_in <- benchmark_directory(dir_in, "Benchmark input")
  if (!any(vapply(config$input_roots, function(root) {
    benchmark_path_within(dir_in, root)
  }, logical(1)))) {
    stop("Benchmark input is outside the configured input roots: ", dir_in)
  }
  scratch_root <- benchmark_validate_scratch_root(
    scratch_root, c(config$input_roots, config$reports_path, dir_in,
                    if (config$resource_monitoring) config$system_monitoring_path)
  )
  if (!any(vapply(config$scratch_roots, benchmark_path_equal, logical(1), b = scratch_root))) {
    stop("Run scratch root is not configured: ", scratch_root)
  }
  invisible(NULL)
}

benchmark_resolve_executable <- function(executable) {
  if (!is.character(executable) || length(executable) != 1L ||
      is.na(executable) || !nzchar(executable)) {
    stop("An executable path or name is required.")
  }
  path <- if (grepl("[/\\\\]", executable)) executable else unname(Sys.which(executable))
  if (!nzchar(path) || !file.exists(path) || dir.exists(path) ||
      (.Platform$OS.type != "windows" && file.access(path, 1) != 0)) {
    stop("Executable is missing or not executable: ", executable)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

benchmark_lastools_executable <- function(lastools_path, executable) {
  if (!is.character(executable) || length(executable) != 1L || is.na(executable) ||
      !grepl("^[A-Za-z0-9_-]+$", executable)) {
    stop("LAStools executable must be an explicit tool name, without paths or arguments.")
  }
  suffix <- if (.Platform$OS.type == "windows") ".exe" else ""
  benchmark_resolve_executable(file.path(
    benchmark_directory(lastools_path, "LAStools"), paste0(executable, suffix)
  ))
}

benchmark_process <- function(executable, args = character()) {
  executable <- benchmark_resolve_executable(executable)
  if (!is.character(args) || anyNA(args)) stop("Process arguments must be character values.")
  processx::run(executable, args, error_on_status = FALSE, cleanup_tree = TRUE)
}

.benchmark_scratch_owners <- new.env(parent = emptyenv())

benchmark_scratch_identity <- function(path) {
  as.list(fs::file_info(path, follow = FALSE)[c("device_id", "inode", "birth_time")])
}

benchmark_scratch_create <- function(root, protected_paths) {
  root <- benchmark_validate_scratch_root(root, protected_paths)
  path <- tempfile("lidar-run-", tmpdir = root)
  if (!dir.create(path, mode = "0700")) {
    stop("Could not create run-owned scratch directory: ", path)
  }
  owner <- list(path = path, root = root, protected_paths = protected_paths,
                identity = benchmark_scratch_identity(path))
  assign(path, owner, envir = .benchmark_scratch_owners)
  owner
}

benchmark_check_scratch_tree <- function(path) {
  canonical <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!benchmark_path_equal(path, canonical) || isTRUE(fs::is_link(path))) {
    stop("Refusing scratch cleanup through a symbolic link or redirected path: ", path)
  }
  if (dir.exists(path)) {
    children <- list.files(path, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    for (child in children) benchmark_check_scratch_tree(child)
  }
  invisible(NULL)
}

benchmark_scratch_cleanup <- function(owner) {
  if (!is.list(owner) || !is.character(owner$path) || length(owner$path) != 1L ||
      is.na(owner$path) || !exists(owner$path, .benchmark_scratch_owners, inherits = FALSE) ||
      !identical(owner, get(owner$path, .benchmark_scratch_owners, inherits = FALSE))) {
    stop("Refusing cleanup of an unowned scratch directory.")
  }
  root <- benchmark_validate_scratch_root(owner$root, owner$protected_paths)
  if (!benchmark_path_equal(dirname(owner$path), root) ||
      !startsWith(basename(owner$path), "lidar-run-")) {
    stop("Refusing cleanup outside the run-owned scratch directory.")
  }
  if (!identical(owner$identity, benchmark_scratch_identity(owner$path))) {
    stop("Refusing scratch cleanup: directory identity changed: ", owner$path)
  }
  benchmark_check_scratch_tree(owner$path)
  if (unlink(owner$path, recursive = TRUE) != 0L || dir.exists(owner$path)) {
    stop("Could not remove run-owned scratch directory: ", owner$path)
  }
  rm(list = owner$path, envir = .benchmark_scratch_owners)
  invisible(NULL)
}

benchmark_file_component <- function(value, label) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value) || value %in% c(".", "..") ||
      grepl("[/\\\\:<>\"|?*]|[[:cntrl:]]", value)) {
    stop(label, " must be a filename component, not a path.")
  }
  value
}

benchmark_lastools_version <- function(lastools_path) {
  version <- benchmark_process(benchmark_lastools_executable(lastools_path, "las2las"),
                               "-version")
  if (version$status != 0L) {
    stop("LAStools version query failed (", version$status, "): ",
         version$stdout, version$stderr)
  }
  text <- trimws(paste0(version$stdout, version$stderr))
  digits <- gsub("\\D", "", text)
  if (!nzchar(digits)) stop("Cannot identify LAStools version from: ", text)
  as.numeric(digits)
}

# gets info about the system, R, and the lidar related packages/software
system_info <- function(lastools_path = NULL) {
  system_details <- Sys.info()
  lastools_version <- if (is.null(lastools_path)) NA_real_ else benchmark_lastools_version(lastools_path)
  cpu_info <- benchmarkme::get_cpu()
  output <- list(
    nodename = unname(system_details["nodename"]),
    sysname = unname(system_details["sysname"]),
    release = unname(system_details["release"]),
    cpu = cpu_info$model_name,
    cpu_threads = cpu_info$no_of_cores,
    ram = unname(ps::ps_system_memory()[["total"]]),
    Rversion = R.version.string,
    lidR_version = as.character(utils::packageVersion("lidR")),
    lasR_version = as.character(utils::packageVersion("lasR")),
    lastools_version = lastools_version
  )
  
  return(output)
}


prepare_input <- function(dir_in, dir_out) {
  
  ctg <- catalog(dir_in)
  
  opt_output_files(ctg) <- paste0(dir_out, "/{ORIGINALFILENAME}")
  opt_laz_compression(ctg) <- TRUE
  opt_progress(ctg) <- FALSE
  opt_stop_early(ctg) <- TRUE
  opt_chunk_size(ctg) <- 0
  opt_chunk_buffer(ctg) <- 20
  opt_chunk_alignment(ctg) <- c(0, 0)
  # Prevent raster methods from silently replacing the requested 20 m buffer.
  opt_wall_to_wall(ctg) <- FALSE
  opt_select(ctg) <- "*"
  opt_filter(ctg) <- ""
  ctg@output_options$drivers$sf$extension <- ".gpkg"
  ctg@output_options$drivers$sf$param <- list(quiet = TRUE, driver = "GPKG")
  
  return(ctg)
  
}

# get lidar data info
benchmark_ALS_info <- function(x, dir_in) {
  total_point_count <- lidR::npoints(x)
  area <- as.numeric(sf::st_area(x))
  if (length(area) != 1L || !is.finite(area) || area <= 0) {
    stop("Catalog area must be finite and positive to calculate input densities.")
  }
  first_returns <- x[["Number.of.1st.return"]]
  if (is.null(first_returns) || anyNA(first_returns)) {
    stop("Catalog headers must contain Number.of.1st.return for pulse density.")
  }
  density_points <- total_point_count / area
  density_pulses <- sum(first_returns) / area
  file_count <- length(x[["filename"]])

  input_data_size <- sum(fs::dir_info(dir_in)$size)
  
  return(list(
    total_point_count=total_point_count, 
    density_pulses=density_pulses, 
    density_points=density_points, 
    file_count=file_count, 
    input_data_size=input_data_size)) 
  
  
}

check_results <- function(path, manifest = NULL, task_id = NULL, tool = NULL,
                          execution = NULL) {
  files <- list.files(path, full.names = TRUE, recursive = TRUE)
  files <- files[!dir.exists(files)]
  result <- list(
    output_file_count = length(files),
    output_total_size = sum(file.info(files)$size),
    validation_status = "unverified",
    validation_diagnostic = "No input/workload validation manifest supplied.",
    expected_output_file_count = NA_integer_
  )
  if (is.null(manifest)) return(result)
  report <- benchmark_validate_artifacts(path, manifest, task_id, tool, execution)
  result$validation_status <- report$status
  result$validation_diagnostic <- report$diagnostic
  result$expected_output_file_count <- report$expected_file_count
  attr(result, "validation_report") <- report
  result
}

benchmark_call_is_empty <- function(task_call) {
  length(task_call) == 0L ||
    (is.character(task_call) && length(task_call) == 1L &&
       (is.na(task_call) || !nzchar(trimws(task_call))))
}

benchmark_validate_lastools_call <- function(task_call) {
  if (!is.list(task_call) || !is.character(task_call$executable) ||
      length(task_call$executable) != 1L || is.na(task_call$executable) ||
      !is.character(task_call$args) || anyNA(task_call$args)) {
    stop("LAStools calls must be list(executable = 'tool', args = c(...)), not shell commands.")
  }
  invisible(NULL)
}

benchmark_lastools_command <- function(task_call, envir) {
  benchmark_validate_lastools_call(task_call)
  values <- list(
    "{inputs}" = get("f", envir = envir),
    "{output}" = get("dir_out", envir = envir),
    "{cores}" = as.character(get("cores", envir = envir))
  )
  if ("{height_cutoff}" %in% task_call$args) {
    values[["{height_cutoff}"]] <- get("height_cutoff", envir = envir)
  }
  args <- as.character(unlist(lapply(task_call$args, function(arg) {
    if (arg %in% names(values)) return(values[[arg]])
    if (grepl("[{}]", arg)) stop("Unknown LAStools argument placeholder: ", arg)
    arg
  }), use.names = FALSE))
  list(
    executable = benchmark_lastools_executable(
      get("lastools_path", envir = envir), task_call$executable
    ),
    args = args
  )
}

benchmark_execute_call <- function(task_call, tool, envir = parent.frame()) {
  result <- list(
    tool = tool,
    success = 0L,
    execution_status = "skipped",
    diagnostic = NA_character_,
    exit_status = NA_integer_,
    stdout = NA_character_,
    stderr = NA_character_,
    start_time = as.POSIXct(NA),
    finish_time = as.POSIXct(NA),
    validation_status = "not_run",
    validation_diagnostic = "Execution has not been validated."
  )

  if (benchmark_call_is_empty(task_call)) {
    result$diagnostic <- paste0("Unsupported workload: no ", tool, " call defined.")
    message(result$diagnostic)
    return(c(result, list(output_file_count = NA_integer_,
                          output_total_size = NA_character_)))
  }

  diagnostics <- character()
  result$start_time <- Sys.time()
  result$execution_status <- "failed"
  tryCatch(
    withCallingHandlers({
      if (tool == "lastools") {
        command <- benchmark_lastools_command(task_call, envir)
        process <- benchmark_process(command$executable, command$args)
        result$exit_status <- as.integer(process$status)
        result$stdout <- process$stdout
        result$stderr <- process$stderr
        if (process$status != 0L) {
          stop(paste0("LAStools exited with status ", process$status, ".\n",
                      process$stdout, process$stderr))
        }
      } else {
        value <- eval(parse(text = task_call), envir = envir)
        if (identical(value, FALSE)) stop("Processing returned FALSE (chunk/pipeline failure).")
      }
      result$success <- 1L
      result$execution_status <- "succeeded"
    }, warning = function(w) {
      diagnostics <<- c(diagnostics, conditionMessage(w))
    }),
    error = function(e) {
      diagnostics <<- c(diagnostics, conditionMessage(e))
      message(tool, ": ", conditionMessage(e))
    }
  )
  result$finish_time <- Sys.time()
  if (length(diagnostics)) {
    result$diagnostic <- paste(unique(diagnostics), collapse = "\n")
  }
  result
}

benchmark_validate_monitor <- function(config) {
  config$monitoring <- benchmark_monitoring_config(config$monitoring)
  if (!identical(unname(Sys.info()[["sysname"]]), "Linux") ||
      !all(file.access(c("/proc/stat", "/proc/meminfo"), 4) == 0)) {
    stop("Resource monitoring unavailable: requires Linux with readable /proc/stat and /proc/meminfo; ",
         "set resource_monitoring = FALSE to run explicitly without monitoring.")
  }
  # Samples belong to immutable attempts, not the historical shared output tree.
  config$system_monitoring_path <- config$reports_path
  config
}

benchmark_monitoring_config <- function(policy = NULL) {
  defaults <- list(scope = "whole-machine", interval_seconds = 1)
  if (is.null(policy)) policy <- list()
  if (!is.list(policy) || (length(policy) &&
      (is.null(names(policy)) || anyNA(names(policy)) || anyDuplicated(names(policy)) ||
       any(!names(policy) %in% names(defaults))))) {
    stop("monitoring must be a named list of supported policy fields.")
  }
  defaults[names(policy)] <- policy
  if (!identical(defaults$scope, "whole-machine")) {
    stop("monitoring$scope must be whole-machine; process-tree monitoring is not implemented.")
  }
  interval <- defaults$interval_seconds
  if (!is.numeric(interval) || length(interval) != 1L || is.na(interval) ||
      !is.finite(interval) || interval < 0.05 || interval > 60) {
    stop("monitoring$interval_seconds must be a finite number between 0.05 and 60.")
  }
  defaults
}

benchmark_monitor_collect <- function(path, attempt_id, interval) {
  snapshot <- function() {
    cpu <- scan(text = sub("^cpu +", "", readLines("/proc/stat", n = 1L)),
                quiet = TRUE)
    memory <- readLines("/proc/meminfo")
    memory_value <- function(key) {
      line <- grep(paste0("^", key, ":"), memory, value = TRUE)
      if (length(line) != 1L) stop("Missing Linux memory counter: ", key)
      as.numeric(strsplit(trimws(sub("^[^:]+:", "", line)), " +")[[1L]][1L]) * 1024
    }
    total <- memory_value("MemTotal")
    available <- memory_value("MemAvailable")
    if (length(cpu) < 8L || any(!is.finite(cpu)) || !is.finite(total) ||
        !is.finite(available) || total <= 0 || available < 0 || available > total) {
      stop("Invalid Linux resource counters.")
    }
    list(epoch = as.numeric(Sys.time()), elapsed = unname(proc.time()[["elapsed"]]),
         total = sum(cpu[1:8]), idle = sum(cpu[c(4, 5)]),
         ram_total = total, ram_available = available)
  }
  output <- file.path(path, "monitor-samples.csv")
  previous <- snapshot()
  file.create(file.path(path, "monitor-ready"))
  repeat {
    Sys.sleep(min(interval, 0.05))
    stopping <- file.exists(file.path(path, "monitor-stop"))
    if (unname(proc.time()[["elapsed"]]) - previous$elapsed < interval && !stopping) next
    current <- snapshot()
    ticks <- current$total - previous$total
    if (ticks <= 0 || current$elapsed <= previous$elapsed ||
        current$epoch <= previous$epoch) stop("Non-increasing resource counters or clock.")
    busy <- ticks - (current$idle - previous$idle)
    if (busy < 0 || busy > ticks) stop("Invalid Linux CPU counter delta.")
    row <- data.frame(
      attempt_id = attempt_id, scope = "whole-machine", platform = "Linux",
      timestamp_units = "seconds since Unix epoch UTC", cpu_units = "percent",
      ram_units = "bytes", sampling_interval_seconds = interval,
      interval_start_epoch = previous$epoch, interval_end_epoch = current$epoch,
      observed_interval_seconds = current$elapsed - previous$elapsed,
      cpu_percent = 100 * busy / ticks,
      ram_used_bytes = current$ram_total - current$ram_available,
      ram_available_bytes = current$ram_available, ram_total_bytes = current$ram_total
    )
    write.table(row, output, sep = ",", row.names = FALSE,
                col.names = !file.exists(output), append = file.exists(output))
    previous <- current
    if (stopping) break
  }
  invisible(NULL)
}

benchmark_start_monitor <- function(config, label, path) {
  config <- benchmark_validate_monitor(config)
  label <- benchmark_file_component(label, "Monitor label")
  path <- benchmark_directory(path, "Attempt monitoring", TRUE)
  if (length(list.files(path, "^monitor-"))) stop("Refusing to overwrite monitoring files.")
  monitor <- callr::r_bg(
    benchmark_monitor_collect,
    args = list(path = path, attempt_id = label,
                interval = config$monitoring$interval_seconds),
    stdout = file.path(path, "monitor-stdout.log"),
    stderr = file.path(path, "monitor-stderr.log"),
    supervise = TRUE
  )
  attr(monitor, "monitor_path") <- path
  ready <- FALSE
  on.exit(if (!ready) {
    monitor$kill_tree()
    monitor$wait(5000)
  })
  deadline <- Sys.time() + 10
  repeat {
    monitor$wait(20)
    if (!monitor$is_alive()) {
      monitor$get_result()
      stop("Monitoring process exited during startup.")
    }
    if (file.exists(file.path(path, "monitor-ready"))) break
    if (Sys.time() >= deadline) {
      stop("Monitoring process produced no readiness receipt; inspect ", path)
    }
  }
  ready <- TRUE
  monitor
}

benchmark_stop_monitor <- function(monitor) {
  on.exit(if (monitor$is_alive()) {
    monitor$kill_tree()
    monitor$wait(5000)
  })
  if (!monitor$is_alive()) {
    monitor$get_result()
    stop("Monitoring process exited before workload cleanup.")
  }
  if (!file.create(file.path(attr(monitor, "monitor_path"), "monitor-stop"))) {
    stop("Cannot request monitor shutdown.")
  }
  monitor$wait(5000)
  if (monitor$is_alive()) stop("Monitoring process did not stop within five seconds.")
  monitor$get_result()
  invisible(NULL)
}

benchmark_monitor_summary <- function(samples, start, finish, attempt_id) {
  numeric_fields <- c("interval_start_epoch", "interval_end_epoch",
                      "observed_interval_seconds", "cpu_percent", "ram_used_bytes",
                      "ram_available_bytes", "ram_total_bytes", "sampling_interval_seconds")
  if (!nrow(samples) || !all(c(numeric_fields, "attempt_id", "scope", "platform",
                               "timestamp_units", "cpu_units", "ram_units") %in% names(samples)) ||
      anyNA(samples) || any(samples$attempt_id != attempt_id) ||
      any(samples$scope != "whole-machine" | samples$platform != "Linux") ||
      any(samples$timestamp_units != "seconds since Unix epoch UTC" |
          samples$cpu_units != "percent" | samples$ram_units != "bytes") ||
      any(!vapply(samples[numeric_fields], is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(samples[numeric_fields]))) ||
      any(samples$interval_end_epoch <= samples$interval_start_epoch) ||
      any(samples$observed_interval_seconds <= 0) ||
      any(samples$cpu_percent < 0 | samples$cpu_percent > 100) ||
      any(samples$ram_used_bytes < 0 | samples$ram_available_bytes < 0 |
          samples$ram_total_bytes <= 0) ||
      any(samples$ram_used_bytes + samples$ram_available_bytes != samples$ram_total_bytes) ||
      length(unique(samples$sampling_interval_seconds)) != 1L ||
      any(samples$sampling_interval_seconds < 0.05 | samples$sampling_interval_seconds > 60) ||
      (nrow(samples) > 1L && any(
        samples$interval_start_epoch[-1L] != head(samples$interval_end_epoch, -1L)))) {
    stop("Malformed or unattributable monitoring samples.")
  }
  if (length(start) != 1L || length(finish) != 1L || !is.finite(start) ||
      !is.finite(finish) || finish <= start) stop("Invalid monitoring timed interval.")
  weights <- pmax(0, pmin(samples$interval_end_epoch, finish) -
                    pmax(samples$interval_start_epoch, start))
  keep <- weights > 0
  covered <- sum(weights)
  if (!any(keep)) stop("Monitoring samples do not overlap the timed workload.")
  list(
    monitoring_status = if (covered >= (finish - start) - 1e-5) "available" else "partial",
    monitoring_sample_count = sum(keep), monitoring_covered_seconds = covered,
    monitoring_coverage_fraction = min(1, covered / (finish - start)),
    cpu_mean_percent = weighted.mean(samples$cpu_percent[keep], weights[keep]),
    cpu_peak_percent = max(samples$cpu_percent[keep]),
    ram_mean_used_bytes = weighted.mean(samples$ram_used_bytes[keep], weights[keep]),
    ram_peak_used_bytes = max(samples$ram_used_bytes[keep])
  )
}

benchmark_monitor_finish <- function(monitor, result, label) {
  path <- attr(monitor, "monitor_path")
  samples_path <- file.path(path, "monitor-samples.csv")
  samples <- read.csv(samples_path, stringsAsFactors = FALSE)
  start <- as.numeric(result$start_time)
  finish <- as.numeric(result$finish_time)
  summary <- benchmark_monitor_summary(samples, start, finish, label)
  report <- list(schema = "chat9-v1", attempt_id = label,
                 timed_start_epoch = start, timed_finish_epoch = finish,
                 samples_md5 = benchmark_checksums(samples_path)$md5, summary = summary)
  report_path <- file.path(path, "monitor-report.rds")
  saveRDS(report, report_path)
  c(list(monitoring_report_md5 = benchmark_checksums(report_path)$md5,
         monitoring_timed_start_epoch = start, monitoring_timed_finish_epoch = finish), summary)
}

benchmark_cleanup <- function(actions) {
  errors <- character()
  for (action in rev(actions)) {
    tryCatch(action(), error = function(e) {
      errors <<- c(errors, conditionMessage(e))
    })
  }
  if (length(errors)) stop("Benchmark cleanup failed: ", paste(errors, collapse = "; "))
  invisible(NULL)
}

benchmark_failed_result <- function(tool, error, result = NULL) {
  if (is.null(result)) {
    result <- list(
      tool = tool, exit_status = NA_integer_, stdout = NA_character_, stderr = NA_character_,
      start_time = as.POSIXct(NA), finish_time = as.POSIXct(NA),
      output_file_count = NA_integer_, output_total_size = NA_character_,
      validation_status = "not_run",
      validation_diagnostic = "Execution/preparation failed before artifact validation."
    )
  }
  result$success <- 0L
  result$execution_status <- "failed"
  diagnostics <- c(result$diagnostic, conditionMessage(error))
  result$diagnostic <- paste(diagnostics[!is.na(diagnostics)], collapse = "\n")
  message(tool, ": ", conditionMessage(error))
  result
}

benchmark_validate_thread_runtime <- function() {
  blas <- extSoftVersion()[["BLAS"]]
  resolved <- if (is.null(blas) || is.na(blas)) "" else
    normalizePath(blas, winslash = "/", mustWork = FALSE)
  if (grepl("openblas|mkl|blis|accelerate|flexiblas", resolved, ignore.case = TRUE) ||
      !grepl("(^|/)(libblas[.]so([.][0-9]+)*|libRblas[.]so|Rblas[.]dll|libRblas[.]dylib)$",
             resolved, ignore.case = TRUE)) {
    stop("Cannot verify single-threaded BLAS: ", resolved,
         ". Use reference BLAS; environment variables cannot cap an already loaded threaded BLAS.")
  }
  invisible(NULL)
}

benchmark_with_resources <- function(tool, cores, config, label, run, monitor_path = NULL) {
  cleanup <- list()
  cleaned <- FALSE
  monitoring <- list(monitoring_status = if (config$resource_monitoring) "unavailable" else "disabled")
  on.exit(if (!cleaned) benchmark_cleanup(cleanup))
  result <- tryCatch({
    benchmark_validate_thread_runtime()
    thread_variables <- c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
                          "BLIS_NUM_THREADS", "VECLIB_MAXIMUM_THREADS",
                          "RCPP_PARALLEL_NUM_THREADS", "OMP_THREAD_LIMIT",
                          "OMP_MAX_ACTIVE_LEVELS")
    previous_environment <- Sys.getenv(thread_variables, unset = NA_character_)
    previous_threads <- if (tool == "lidR") lidR::get_lidr_threads() else NULL
    previous_data_threads <- data.table::getDTthreads()
    cleanup[[length(cleanup) + 1L]] <- function() {
      missing <- is.na(previous_environment)
      Sys.unsetenv(names(previous_environment)[missing])
      if (any(!missing)) do.call(Sys.setenv, as.list(previous_environment[!missing]))
      if (!is.null(previous_threads)) lidR::set_lidr_threads(previous_threads)
      data.table::setDTthreads(previous_data_threads)
    }
    limits <- stats::setNames(rep("1", length(thread_variables)), thread_variables)
    # Outer native parallelism gets the budget, but no nested OpenMP teams.
    if (tool %in% c("lasR", "lastools")) {
      limits[c("OMP_NUM_THREADS", "OMP_THREAD_LIMIT")] <- as.character(cores)
    }
    do.call(Sys.setenv, as.list(limits))
    data.table::setDTthreads(1L)
    if (tool == "lidR") {
      lidR::set_lidr_threads(1L)
      previous_plan <- future::plan()
      cleanup[[length(cleanup) + 1L]] <- function() future::plan(previous_plan)
      future::plan(future::multisession, workers = cores,
                   rscript_startup = quote({
                     lidR::set_lidr_threads(1L)
                     data.table::setDTthreads(1L)
                   }))
    }
    if (tool == "lasR") {
      cleanup[[length(cleanup) + 1L]] <- function() unset_parallel_strategy()
      unset_parallel_strategy()
      strategy <- concurrent_files(ncores = cores)
      withCallingHandlers(set_parallel_strategy(strategy), warning = function(w) stop(w))
      applied <- lasR::get_parallel_strategy()
      if (!identical(as.integer(applied), as.integer(strategy)) ||
          !identical(attr(applied, "strategy"), attr(strategy, "strategy"))) {
        stop("lasR did not apply the requested concurrent-files thread budget.")
      }
    }
    if (config$resource_monitoring) {
      monitor <- benchmark_start_monitor(config, label, monitor_path)
      cleanup[[length(cleanup) + 1L]] <- function() {
        benchmark_stop_monitor(monitor)
        if (!is.na(result$start_time) && !is.na(result$finish_time)) {
          monitoring <<- benchmark_monitor_finish(monitor, result, label)
        }
      }
    }
    run()
  }, error = function(e) benchmark_failed_result(tool, e))
  cleanup_error <- tryCatch({
    benchmark_cleanup(cleanup)
    NULL
  }, error = identity)
  cleaned <- TRUE
  result[names(monitoring)] <- monitoring
  if (inherits(cleanup_error, "error")) {
    result <- benchmark_failed_result(tool, cleanup_error, result)
    attr(result, "resources_cleaned") <- FALSE
  }
  result
}

compile_execution_outcomes <- function(results) {
  # Legacy CSVs have only the numeric success flag.
  if (!"execution_status" %in% names(results)) {
    results$execution_status <- NA_character_
  }
  missing_status <- is.na(results$execution_status)
  results$execution_status[missing_status] <- ifelse(
    is.na(results$success[missing_status]), "unknown",
    ifelse(results$success[missing_status] == 1, "succeeded", "failed")
  )
  if (!"diagnostic" %in% names(results)) results$diagnostic <- NA_character_
  if (!"exit_status" %in% names(results)) results$exit_status <- NA_integer_
  if (!"validation_status" %in% names(results)) results$validation_status <- "unverified"
  results$validation_status[is.na(results$validation_status)] <- "unverified"
  if (!"validation_diagnostic" %in% names(results)) {
    results$validation_diagnostic <- "Historical record: artifacts were not validated."
  }
  if (!"expected_output_file_count" %in% names(results)) {
    results$expected_output_file_count <- NA_integer_
  }
  results$validated_success <- results$execution_status == "succeeded" &
    results$success == 1 & results$validation_status == "passed"
  if (!"phase" %in% names(results)) results$phase <- "measurement"
  if (!"monitoring_status" %in% names(results)) {
    results$monitoring_status <- "unavailable-historical"
  }
  results$monitoring_status[is.na(results$monitoring_status)] <- "unavailable-historical"
  results$measurement_eligible <- results$phase == "measurement" & results$validated_success

  results$processing_time <- difftime(
    results$finish_time, results$start_time, units = "secs"
  )
  results$short_duration <- as.numeric(results$processing_time) < 0.1
  results
}

benchmark_run_tool <- function(tool, task_call, context, config, label) {
  if (benchmark_call_is_empty(task_call)) {
    return(benchmark_execute_call(task_call, tool, context))
  }
  context <- list2env(as.list(context), parent = parent.env(context))
  scratch <- benchmark_scratch_create(
    context$scratch_root,
    c(config$input_roots, config$reports_path, context$dir_in,
      if (config$resource_monitoring) config$system_monitoring_path)
  )
  cleaned <- FALSE
  on.exit(if (!cleaned) benchmark_scratch_cleanup(scratch))
  result <- NULL
  result <- tryCatch({
    context$dir_out <- scratch$path
    context$ctg <- prepare_input(context$dir_in, context$dir_out)
    context$f <- context$ctg@data$filename
    if (!length(context$f) || anyNA(context$f) || any(!file.exists(context$f)) ||
        any(file.access(context$f, 4) != 0)) {
      stop("Input catalog has no files or references missing/unreadable files.")
    }
    context$f <- normalizePath(context$f, winslash = "/", mustWork = TRUE)
    if (any(!vapply(context$f, benchmark_path_within, logical(1), parent = context$dir_in))) {
      stop("Input catalog references files outside its validated input directory.")
    }
    context$fout <- paste0(context$dir_out, "/*.", context$output_format)
    if (tool == "lastools" && "{height_cutoff}" %in% task_call$args) {
      context$height_cutoff <- benchmark_height_cutoff(context$ctg)
    }
    manifest <- if (isTRUE(context$validate_artifacts)) {
      benchmark_validation_manifest(context$ctg)
    } else NULL
    result <- benchmark_with_resources(tool, context$cores, config, label, function() {
      benchmark_execute_call(task_call, tool, context)
    }, monitor_path = context$monitor_path)
    counts <- tryCatch(
      check_results(context$dir_out, manifest, context$task_id, tool, result),
      error = function(e) {
        message("Artifact validation: ", conditionMessage(e))
        list(validation_status = "failed", validation_diagnostic = conditionMessage(e))
      }
    )
    report <- attr(counts, "validation_report")
    if (is.null(report)) {
      report <- list(schema = "chat7-v1", status = counts$validation_status,
                     diagnostic = counts$validation_diagnostic)
    }
    if (!is.null(context$validation_report)) {
      tryCatch({
        saveRDS(report, context$validation_report)
        counts$validation_report_md5 <- benchmark_checksums(context$validation_report)$md5
      }, error = function(e) {
        counts$validation_status <<- "failed"
        counts$validation_diagnostic <<- paste(
          counts$validation_diagnostic, "Cannot persist validation report:", conditionMessage(e))
        message(counts$validation_diagnostic)
      })
    }
    result[names(counts)] <- counts
    result
  }, error = function(e) benchmark_failed_result(tool, e, result))
  cleanup_error <- if (identical(attr(result, "resources_cleaned"), FALSE)) {
    simpleError(paste0("Scratch retained because resource cleanup failed: ", scratch$path))
  } else {
    tryCatch({
      benchmark_scratch_cleanup(scratch)
      NULL
    }, error = identity)
  }
  cleaned <- TRUE
  if (inherits(cleanup_error, "error")) {
    result <- benchmark_failed_result(tool, cleanup_error, result)
  }
  result
}

benchmark_selected_run <- function(task_id, run_id, benchmark_tasks, benchmark_runs) {
  task <- benchmark_tasks[benchmark_tasks$task_id == task_id, , drop = FALSE]
  run <- benchmark_runs[benchmark_runs$ID == run_id, , drop = FALSE]
  if (nrow(task) != 1L || nrow(run) != 1L) {
    stop("Each task_id and run_id must identify exactly one task and run.")
  }
  if (!is.numeric(run$cores) || length(run$cores) != 1L || is.na(run$cores) ||
      !is.finite(run$cores) || run$cores < 1 || run$cores != floor(run$cores)) {
    stop("Run cores must be a positive integer.")
  }
  input_column <- switch(
    task$data_type, datasets_org = "dir_in_org", datasets_norm = "dir_in_norm",
    stop("Unknown task data_type: ", task$data_type)
  )
  list(task = task, run = run, dir_in = run[[input_column]])
}

benchmark_validate_selection <- function(selection, tasks, runs, config) {
  config <- benchmark_validate_config(config)
  for (i in seq_len(nrow(selection))) {
    selected <- benchmark_selected_run(selection$task_id[i], selection$run_id[i], tasks, runs)
    if (!selected$run$cores %in% config$cores) stop("Run cores are not configured.")
    benchmark_validate_run_paths(config, selected$dir_in, selected$run$scratch_root)
    for (tool in config$tools) {
      call <- selected$task[[paste0("task_call_", tool)]][[1L]]
      if (tool == "lastools" && !benchmark_call_is_empty(call)) {
        benchmark_validate_lastools_call(call)
        benchmark_lastools_executable(config$lastools_path, call$executable)
      }
    }
  }
  invisible(config)
}

benchmark_run <- function(task_id, run_id, run_lidR = "lidR" %in% config$tools,
                          run_lasR = "lasR" %in% config$tools,
                          run_lastools = "lastools" %in% config$tools,
                          benchmark_tasks, benchmark_runs,
                          force_rerun_lidR = FALSE, force_rerun_lasR = FALSE,
                          force_rerun_lastools = FALSE, sysinfo,
                          resource_monitoring = config$resource_monitoring, config,
                          campaign, repetition_id = 1L, phase = "measurement",
                          schedule_index = NA_integer_) {
  config$resource_monitoring <- resource_monitoring
  config <- benchmark_validate_config(config)
  selected <- benchmark_selected_run(task_id, run_id, benchmark_tasks, benchmark_runs)
  task <- selected$task
  run <- selected$run
  if (!run$cores %in% config$cores) stop("Run cores are not configured.")
  benchmark_validate_run_paths(config, selected$dir_in, run$scratch_root)
  enabled <- c(lidR = run_lidR, lasR = run_lasR, lastools = run_lastools)
  if (!is.logical(enabled) || length(enabled) != 3L || anyNA(enabled)) {
    stop("Tool run flags must each be TRUE or FALSE.")
  }
  if (any(!names(enabled)[enabled] %in% config$tools)) {
    stop("Requested tools must be explicitly enabled in config$tools.")
  }
  force <- c(lidR = force_rerun_lidR, lasR = force_rerun_lasR,
             lastools = force_rerun_lastools)
  if (!is.logical(force) || length(force) != 3L || anyNA(force)) {
    stop("Force-rerun flags must each be TRUE or FALSE.")
  }
  for (tool in names(enabled)[enabled]) {
    call <- task[[paste0("task_call_", tool)]][[1L]]
    if (tool == "lastools" && !benchmark_call_is_empty(call)) {
      benchmark_validate_lastools_call(call)
      benchmark_lastools_executable(config$lastools_path, call$executable)
    }
  }
  if (missing(campaign)) stop("An explicit campaign is required; use benchmark_campaign().")
  benchmark_repetition_id(repetition_id)
  benchmark_campaign_check(campaign, benchmark_tasks, benchmark_runs, config)
  if (length(phase) != 1L || is.na(phase) || !phase %in% c("warmup", "measurement")) {
    stop("phase must be warmup or measurement.")
  }
  tool_position <- NA_integer_
  if (!identical(schedule_index, NA_integer_)) {
    benchmark_repetition_id(schedule_index)
    row <- campaign$manifest$identity$schedule
    row <- row[row$schedule_index == schedule_index, , drop = FALSE]
    if (nrow(row) != 1L || row$task_id != task_id || row$run_id != run_id ||
        row$phase != phase || row$repetition_id != repetition_id ||
        !identical(names(enabled)[enabled], row$tool)) {
      stop("Dispatch does not match the frozen schedule.")
    }
    tool_position <- row$tool_position
  }
  if (!any(campaign$manifest$identity$selection$task_id == task_id &
           campaign$manifest$identity$selection$run_id == run_id)) {
    stop("Task/run is not in the campaign selection.")
  }
  lock <- file.path(config$reports_path, ".benchmark-execution-lock")
  if (!dir.create(lock, showWarnings = FALSE)) {
    stop("Benchmark execution is already locked: ", lock,
         ". Do not run competing tools; inspect any stale lock after an interruption.")
  }
  on.exit(if (unlink(lock, recursive = TRUE) != 0L) {
    stop("Cannot release benchmark execution lock: ", lock)
  }, add = TRUE)
  cli::cli_h1("Benchmark run started: {task_id}, {run_id}")
  context <- new.env(parent = environment())
  context$dir_in <- benchmark_directory(selected$dir_in, "Benchmark input")
  context$scratch_root <- run$scratch_root
  context$cores <- run$cores
  context$lastools_path <- config$lastools_path
  context$output_format <- task$output_format
  context$task_id <- task_id
  context$validate_artifacts <- "workload_contract" %in% names(task)
  metadata <- NULL
  for (tool in names(enabled)[enabled]) {
    call <- task[[paste0("task_call_", tool)]][[1L]]
    parts <- list(campaign$manifest$campaign_id, task_id, run_id, tool)
    invisible(lapply(parts, benchmark_file_component, label = "Result filename"))
    label <- paste(unlist(parts), collapse = "_")
    if (!force[[tool]] &&
        benchmark_campaign_completed(campaign, task_id, run_id, tool, repetition_id, phase)) {
      cli::cli_inform("Matching campaign work is already complete: {label}")
      next
    }
    cli::cli_h2("Running {tool}")
    attempt <- benchmark_attempt_start(campaign, task_id, run_id, tool, repetition_id,
                                        phase, schedule_index, tool_position)
    context$validation_report <- file.path(attempt$path, "validation.rds")
    context$monitor_path <- attempt$path
    result <- tryCatch(
      {
        if (is.null(metadata) && !benchmark_call_is_empty(call)) {
          input_catalog <- catalog(context$dir_in)
          metadata <- benchmark_ALS_info(input_catalog, context$dir_in)
        }
        benchmark_run_tool(tool, call, context, config, attempt$record$attempt_id)
      },
      error = function(e) benchmark_failed_result(tool, e)
    )
    if (is.null(result$monitoring_status)) {
      result$monitoring_status <- if (config$resource_monitoring) "not_run" else "disabled"
    }
    common <- c(list(task_id = task_id, run_id = run_id, cores = run$cores,
                     workers = if (tool == "lastools") 1L else run$cores,
                     native_threads = if (tool == "lastools") run$cores else 1L,
                     blas_threads = 1L,
                     cache_policy = config$scheduling$cache_policy,
                     cooldown_seconds = config$scheduling$cooldown_seconds,
                     monitoring_scope = config$monitoring$scope,
                     monitoring_platform = unname(Sys.info()[["sysname"]]),
                     monitoring_interval_seconds = config$monitoring$interval_seconds,
                     monitoring_diagnostic = switch(result$monitoring_status,
                       disabled = "Monitoring explicitly disabled.",
                       not_run = "Workload did not reach monitor setup.",
                       unavailable = "Monitoring failed; see execution diagnostic and monitor logs.",
                       partial = "Samples cover only part of the timed interval.",
                       "Whole-machine utilization includes unrelated activity and monitor overhead."),
                     timing_boundary = "workload call only; includes tool I/O and persistence"),
                as.list(sysinfo), metadata)
    if ("workload_contract" %in% names(task)) {
      contract <- benchmark_workload_contract(task_id, tool)
      if (contract$comparison_status == "blocked") {
        result$diagnostic <- contract$comparison_note
      }
      common <- c(common, contract[c("workload_contract", "comparison_status",
                                    "comparison_group", "comparison_note")])
    }
    benchmark_attempt_finish(attempt, c(result, common))
    if (!is.na(result$start_time)) Sys.sleep(config$scheduling$cooldown_seconds)
    if (result$success == 1L) {
      cli::cli_alert_success("Finished in {result$finish_time-result$start_time} sec.")
    }
  }
  invisible(NULL)
}

benchmark_repetition_id <- function(value) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value < 1 || value != floor(value)) {
    stop("repetition_id must be one positive integer.")
  }
  invisible(value)
}

benchmark_checksums <- function(paths) {
  paths <- sort(unique(paths))
  if (any(!file.exists(paths)) || any(dir.exists(paths)) ||
      any(file.access(paths, 4) != 0)) stop("Cannot checksum missing/unreadable files.")
  sums <- tools::md5sum(paths)
  if (anyNA(sums)) stop("File checksum failed.")
  data.frame(path = paths, bytes = unname(file.info(paths)$size),
             md5 = unname(sums), stringsAsFactors = FALSE)
}

benchmark_object_md5 <- function(object) {
  path <- tempfile("campaign-identity-")
  on.exit(unlink(path))
  saveRDS(object, path, version = 3)
  unname(tools::md5sum(path))
}

benchmark_campaign_identity <- function(selection, tasks, runs, config, repo_root) {
  config <- benchmark_validate_config(config)
  if (!is.data.frame(selection) || !all(c("task_id", "run_id") %in% names(selection)) ||
      !nrow(selection) || anyNA(selection[c("task_id", "run_id")]) ||
      anyDuplicated(selection[c("task_id", "run_id")])) {
    stop("Campaign selection must contain unique, nonmissing task_id/run_id pairs.")
  }
  selection <- as.data.frame(selection[c("task_id", "run_id")], stringsAsFactors = FALSE)
  selection <- selection[order(selection$task_id, selection$run_id), , drop = FALSE]
  rownames(selection) <- NULL
  benchmark_validate_selection(selection, tasks, runs, config)
  tasks <- as.data.frame(tasks[tasks$task_id %in% selection$task_id, , drop = FALSE])
  tasks <- tasks[order(tasks$task_id), , drop = FALSE]
  rownames(tasks) <- NULL
  runs <- as.data.frame(runs[runs$ID %in% selection$run_id, , drop = FALSE])
  runs <- runs[order(runs$ID), , drop = FALSE]
  rownames(runs) <- NULL
  inputs <- unique(vapply(seq_len(nrow(selection)), function(i) {
    benchmark_selected_run(selection$task_id[i], selection$run_id[i], tasks, runs)$dir_in
  }, character(1)))
  datasets <- lapply(sort(inputs), function(path) {
    files <- list.files(path, full.names = TRUE, all.files = TRUE, no.. = TRUE,
                        recursive = TRUE)
    files <- files[!dir.exists(files)]
    list(directory = normalizePath(path, winslash = "/", mustWork = TRUE),
         files = benchmark_checksums(files))
  })
  git <- function(args) {
    result <- benchmark_process("git", c("-C", repo_root, args))
    if (result$status != 0L) stop("Cannot identify campaign code: ", result$stderr)
    trimws(result$stdout)
  }
  code_paths <- c(list.files(file.path(repo_root, "R"), "\\.R$", full.names = TRUE),
                  file.path(repo_root, c("DESCRIPTION", "WORKLOADS.md")))
  code <- list(
    revision = git(c("rev-parse", "HEAD")),
    dirty = nzchar(git(c("status", "--porcelain", "--untracked-files=all", "--",
                        "R", "DESCRIPTION", "WORKLOADS.md", "renv.lock"))),
    files = benchmark_checksums(code_paths)
  )
  lock_path <- file.path(repo_root, "renv.lock")
  lock <- renv::lockfile_read(lock_path)
  packages <- lapply(sort(names(lock$Packages)), function(package) {
    path <- normalizePath(find.package(package), winslash = "/", mustWork = TRUE)
    list(package = package, version = as.character(utils::packageVersion(package)),
         files = benchmark_checksums(c(
           file.path(path, "DESCRIPTION"),
           list.files(file.path(path, "R"), full.names = TRUE, recursive = TRUE),
           list.files(file.path(path, "libs"), full.names = TRUE, recursive = TRUE)
         )))
  })
  native <- list()
  if ("lastools" %in% config$tools) {
    calls <- tasks$task_call_lastools
    for (call in calls) {
      if (benchmark_call_is_empty(call)) next
      executable <- benchmark_lastools_executable(config$lastools_path, call$executable)
      version <- benchmark_process(executable, "-version")
      if (version$status != 0L || !nzchar(trimws(paste0(version$stdout, version$stderr)))) {
        stop("Native tool version query failed: ", executable, "\n",
             version$stdout, version$stderr)
      }
      native[[call$executable]] <- list(
        executable = benchmark_checksums(executable),
        version = paste0(version$stdout, version$stderr)
      )
    }
    native <- native[sort(names(native))]
  }
  storage <- ps::ps_disk_partitions(all = TRUE)
  storage$mountpoint <- chartr("\\", "/", storage$mountpoint)
  storage_paths <- c(config$input_roots, config$scratch_roots, config$reports_path,
                     vapply(datasets, `[[`, character(1), "directory"))
  storage <- storage[vapply(storage$mountpoint, function(mountpoint) {
    any(vapply(storage_paths, benchmark_path_within, logical(1), parent = mountpoint))
  }, logical(1)), , drop = FALSE]
  storage <- storage[order(storage$mountpoint, storage$device), , drop = FALSE]
  rownames(storage) <- NULL
  list(
    schema = 2L, selection = selection, tasks = tasks, runs = runs, config = config,
    schedule = benchmark_schedule(selection, config$tools, config$scheduling),
    code = code, datasets = datasets, lockfile = benchmark_checksums(lock_path),
    environment = list(
      R = R.version, R_executable = benchmark_checksums(if (.Platform$OS.type == "windows") {
        file.path(R.home("bin"), "R.exe")
      } else {
        file.path(R.home("bin"), "exec", "R")
      }),
      packages = packages, native = native, OS = Sys.info(),
      cpu = benchmarkme::get_cpu(), RAM_bytes = unname(ps::ps_system_memory()[["total"]]),
      storage_mounts = storage,
      geospatial = sf::sf_extSoftVersion(), libraries = extSoftVersion(),
      session_libraries = sessionInfo()$BLAS,
      parallelism = list(
        cores = config$cores, runs = runs[c("ID", "cores")],
        policy = "serial tools; lidR multisession x 1 native; lasR concurrent_files x 1 point thread; LAStools -cores",
        scheduling = config$scheduling,
        environment = Sys.getenv(c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS",
                                   "MKL_NUM_THREADS", "RCPP_PARALLEL_NUM_THREADS"))
      )
    )
  )
}

benchmark_campaign <- function(campaign_id, selection, tasks, runs, config,
                               repo_root = getwd()) {
  benchmark_file_component(campaign_id, "campaign_id")
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  identity <- benchmark_campaign_identity(selection, tasks, runs, config, repo_root)
  root <- file.path(identity$config$reports_path, "campaigns")
  if (!dir.exists(root) && !dir.create(root)) stop("Cannot create campaign root: ", root)
  path <- file.path(root, campaign_id)
  manifest_path <- file.path(path, "manifest.rds")
  if (dir.exists(path)) {
    if (!file.exists(manifest_path)) stop("Incomplete campaign creation: ", path)
    manifest <- readRDS(manifest_path)
    if (!identical(manifest$campaign_id, campaign_id) ||
        !identical(manifest$identity_id, benchmark_object_md5(manifest$identity)) ||
        !identical(manifest$identity, identity)) {
      stop("Incompatible campaign identity; use a new campaign_id. No work was resumed.")
    }
  } else {
    if (!dir.create(path)) stop("Cannot reserve campaign directory: ", path)
    manifest <- list(
      campaign_id = campaign_id, created_at = Sys.time(), repo_root = repo_root,
      identity_id = benchmark_object_md5(identity), identity = identity
    )
    saveRDS(manifest, manifest_path)
    if (!dir.create(file.path(path, "attempts"))) stop("Cannot create attempts directory.")
  }
  list(path = path, manifest = manifest)
}

benchmark_campaign_check <- function(campaign, tasks, runs, config) {
  stored <- readRDS(file.path(campaign$path, "manifest.rds"))
  if (!identical(stored, campaign$manifest) ||
      !identical(stored$identity_id, benchmark_object_md5(stored$identity))) {
    stop("Campaign manifest changed or is corrupt.")
  }
  current <- benchmark_campaign_identity(stored$identity$selection, tasks, runs,
                                          config, stored$repo_root)
  if (!identical(current, stored$identity)) {
    changed <- names(current)[!vapply(names(current), function(field) {
      identical(current[[field]], stored$identity[[field]])
    }, logical(1))]
    stop("Incompatible campaign identity; changed: ", paste(changed, collapse = ", "),
         ". Use a new campaign_id.\n",
         paste(all.equal(stored$identity, current), collapse = "\n"))
  }
  invisible(NULL)
}

benchmark_attempt_start <- function(campaign, task_id, run_id, tool, repetition_id,
                                    phase = "measurement", schedule_index = NA_integer_,
                                    tool_position = NA_integer_) {
  benchmark_repetition_id(repetition_id)
  if (length(phase) != 1L || is.na(phase) || !phase %in% c("warmup", "measurement")) {
    stop("phase must be warmup or measurement.")
  }
  if (!tool %in% campaign$manifest$identity$config$tools ||
      !any(campaign$manifest$identity$selection$task_id == task_id &
           campaign$manifest$identity$selection$run_id == run_id)) {
    stop("Attempt is outside the campaign selection.")
  }
  root <- file.path(campaign$path, "attempts")
  path <- tempfile("attempt-", tmpdir = root)
  if (!dir.create(path)) stop("Cannot reserve unique attempt: ", path)
  record <- list(
    campaign_id = campaign$manifest$campaign_id,
    identity_id = campaign$manifest$identity_id,
    repetition_id = repetition_id, phase = phase, schedule_index = schedule_index,
    tool_position = tool_position, attempt_id = basename(path),
    task_id = task_id, run_id = run_id, tool = tool, recorded_at = Sys.time()
  )
  saveRDS(record, file.path(path, "started.rds"))
  list(path = path, record = record)
}

benchmark_attempt_finish <- function(attempt, result) {
  path <- file.path(attempt$path, "result.csv")
  lock <- file.path(attempt$path, "completion-lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Attempt completion already reserved.")
  if (file.exists(path)) stop("Refusing to overwrite immutable attempt: ", path)
  record <- c(attempt$record, result[setdiff(names(result), names(attempt$record))])
  pending <- file.path(attempt$path, "result.pending.csv")
  write.csv(as.data.frame(record), pending, row.names = FALSE)
  if (!file.rename(pending, path)) stop("Cannot publish attempt result: ", path)
  saveRDS(benchmark_checksums(path)$md5, file.path(attempt$path, "completed.rds"))
  invisible(path)
}

benchmark_read_attempt <- function(attempt, manifest) {
  started <- readRDS(file.path(attempt, "started.rds"))
  result <- file.path(attempt, "result.csv")
  receipt <- file.path(attempt, "completed.rds")
  if (!file.exists(result) || !file.exists(receipt)) {
    stop("Incomplete attempt retained at ", attempt, "; no completion record. ",
         "Compilation is blocked rather than omitting it.")
  }
  if (!identical(readRDS(receipt), benchmark_checksums(result)$md5)) {
    stop("Attempt checksum does not match its immutable completion record: ", attempt)
  }
  spec <- readr::cols(
    .default = readr::col_character(), success = readr::col_double(),
    repetition_id = readr::col_double(), cores = readr::col_double(),
    schedule_index = readr::col_integer(), tool_position = readr::col_integer(),
    workers = readr::col_integer(), native_threads = readr::col_integer(),
    blas_threads = readr::col_integer(),
    cooldown_seconds = readr::col_double(),
    monitoring_interval_seconds = readr::col_double(),
    monitoring_timed_start_epoch = readr::col_double(),
    monitoring_timed_finish_epoch = readr::col_double(),
    monitoring_sample_count = readr::col_integer(),
    monitoring_covered_seconds = readr::col_double(),
    monitoring_coverage_fraction = readr::col_double(),
    cpu_mean_percent = readr::col_double(), cpu_peak_percent = readr::col_double(),
    ram_mean_used_bytes = readr::col_double(), ram_peak_used_bytes = readr::col_double(),
    cpu_threads = readr::col_double(), ram = readr::col_double(),
    total_point_count = readr::col_double(), density_pulses = readr::col_double(),
    file_count = readr::col_double(),
    start_time = readr::col_datetime(), finish_time = readr::col_datetime(),
    recorded_at = readr::col_datetime(), output_file_count = readr::col_double(),
    exit_status = readr::col_integer(), density_points = readr::col_double(),
    expected_output_file_count = readr::col_integer()
  )
  header <- names(readr::read_csv(
    result, n_max = 0, col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  ))
  spec$cols <- spec$cols[intersect(names(spec$cols), header)]
  record <- readr::read_csv(result, col_types = spec, show_col_types = FALSE)
  if (nrow(readr::problems(record))) stop("Malformed attempt CSV: ", result)
  if (nrow(record) != 1L || !identical(record$campaign_id, manifest$campaign_id) ||
      !identical(record$identity_id, manifest$identity_id) ||
      !identical(record$attempt_id, basename(attempt)) ||
      any(vapply(c("campaign_id", "identity_id", "attempt_id", "tool", "task_id", "run_id"),
                 function(field) !identical(record[[field]], started[[field]]), logical(1))) ||
      !identical(record$repetition_id, as.numeric(started$repetition_id)) ||
      ("phase" %in% names(started) && !identical(record$phase, started$phase)) ||
      ("schedule_index" %in% names(started) &&
         !identical(record$schedule_index, as.integer(started$schedule_index))) ||
      ("tool_position" %in% names(started) &&
         !identical(record$tool_position, as.integer(started$tool_position))) ||
      !record$tool %in% manifest$identity$config$tools ||
      !any(manifest$identity$selection$task_id == record$task_id &
           manifest$identity$selection$run_id == record$run_id)) {
    stop("Attempt identity does not match its campaign/start record: ", attempt)
  }
  benchmark_repetition_id(record$repetition_id)
  if (!"phase" %in% names(record)) record$phase <- "measurement"
  if (!"schedule_index" %in% names(record)) record$schedule_index <- NA_integer_
  if (!"tool_position" %in% names(record)) record$tool_position <- NA_integer_
  if ("validation_report_md5" %in% names(record) &&
      !is.na(record$validation_report_md5)) {
    report <- file.path(attempt, "validation.rds")
    if (!file.exists(report) ||
        !identical(benchmark_checksums(report)$md5, record$validation_report_md5)) {
      stop("Validation report checksum does not match its attempt: ", attempt)
    }
  } else if ("validation_status" %in% names(record) &&
             record$validation_status == "passed") {
    stop("Passed validation requires an immutable validation report: ", attempt)
  }
  if ("monitoring_report_md5" %in% names(record) && !is.na(record$monitoring_report_md5)) {
    report_path <- file.path(attempt, "monitor-report.rds")
    samples_path <- file.path(attempt, "monitor-samples.csv")
    if (!file.exists(report_path) ||
        !identical(benchmark_checksums(report_path)$md5, record$monitoring_report_md5)) {
      stop("Monitoring report checksum does not match its attempt: ", attempt)
    }
    report <- readRDS(report_path)
    if (!identical(report$schema, "chat9-v1") ||
        !identical(report$attempt_id, record$attempt_id) ||
        !isTRUE(all.equal(c(report$timed_start_epoch, report$timed_finish_epoch),
                         c(record$monitoring_timed_start_epoch,
                           record$monitoring_timed_finish_epoch), tolerance = 1e-14)) ||
        !file.exists(samples_path) ||
        !identical(benchmark_checksums(samples_path)$md5, report$samples_md5)) {
      stop("Monitoring samples/identity do not match their immutable report: ", attempt)
    }
    summary <- benchmark_monitor_summary(
      read.csv(samples_path, stringsAsFactors = FALSE),
      report$timed_start_epoch, report$timed_finish_epoch, record$attempt_id
    )
    if (!identical(summary, report$summary) ||
        !isTRUE(all.equal(as.list(record[names(summary)]), summary,
                          tolerance = 1e-12, check.attributes = FALSE))) {
      stop("Monitoring summary does not match its samples/attempt: ", attempt)
    }
  } else if ("monitoring_status" %in% names(record) &&
             record$monitoring_status %in% c("available", "partial")) {
    stop("Available monitoring requires an immutable monitoring report: ", attempt)
  }
  record
}

benchmark_read_campaign <- function(reports_path, campaign_id) {
  benchmark_file_component(campaign_id, "campaign_id")
  path <- file.path(reports_path, "campaigns", campaign_id)
  manifest <- readRDS(file.path(path, "manifest.rds"))
  if (!identical(manifest$campaign_id, campaign_id) ||
      !identical(manifest$identity_id, benchmark_object_md5(manifest$identity))) {
    stop("Campaign manifest changed or is corrupt.")
  }
  attempts <- list.dirs(file.path(path, "attempts"), recursive = FALSE, full.names = TRUE)
  records <- lapply(attempts, benchmark_read_attempt, manifest = manifest)
  result <- dplyr::bind_rows(records)
  if (nrow(result) && anyDuplicated(result$attempt_id)) stop("Duplicate attempt IDs.")
  result
}

benchmark_campaign_completed <- function(campaign, task_id, run_id, tool, repetition_id,
                                         phase = "measurement") {
  # An interrupted attempt must not prevent an explicit retry.
  paths <- list.files(file.path(campaign$path, "attempts"), "^completed\\.rds$",
                      recursive = TRUE, full.names = TRUE)
  any(vapply(paths, function(path) {
    record <- benchmark_read_attempt(dirname(path), campaign$manifest)
    identical(as.character(record$task_id), task_id) &&
      identical(as.character(record$run_id), run_id) &&
      identical(as.character(record$tool), tool) &&
      identical(as.character(record$phase), phase) &&
      identical(as.numeric(record$repetition_id), as.numeric(repetition_id)) &&
      (record$execution_status == "skipped" ||
         (record$execution_status == "succeeded" &&
            record$success == 1 && "validation_status" %in% names(record) &&
            record$validation_status == "passed"))
  }, logical(1)))
}

benchmark_validation_manifest <- function(ctg) {
  tiles <- ctg@data[c("filename", "Min.X", "Max.X", "Min.Y", "Max.Y")]
  tiles <- as.data.frame(tiles)
  tiles$id <- tools::file_path_sans_ext(basename(tiles$filename))
  tiles$count <- vapply(tiles$filename, function(file) {
    header <- lidR::readLASheader(file)
    as.numeric(lidR::npoints(header))
  }, numeric(1))
  bounds <- as.matrix(tiles[c("Min.X", "Max.X", "Min.Y", "Max.Y")])
  if (!nrow(tiles) || anyDuplicated(tiles$id) || any(!is.finite(bounds)) ||
      any(tiles$Min.X >= tiles$Max.X | tiles$Min.Y >= tiles$Max.Y) ||
      any(!is.finite(tiles$count) | tiles$count < 0)) {
    stop("Validation requires unique tile stems, finite nondegenerate bounds and counts.")
  }
  list(schema = "chat7-v1", tiles = tiles, crs = sf::st_crs(ctg),
       inputs = benchmark_checksums(tiles$filename), buffer = 20)
}

benchmark_artifact_specs <- function(task_id, tool) {
  metric_bands <- names(metrics_multiple(numeric()))
  raster <- function(suffix = ".tif", resolution = 1, bands = character(),
                     reference = task_id) {
    list(suffix = suffix, kind = "raster", resolution = resolution,
         bands = bands, reference = reference)
  }
  specs <- switch(task_id,
    normalization = list(list(suffix = ".laz", kind = "points")),
    pixel_metrics_1a = list(raster(
      if (tool == "lastools") "_avg.tif" else ".tif", 20, "z_mean")),
    pixel_metrics_2 = if (tool == "lastools") {
      Map(function(suffix, band) raster(suffix, 20, band),
          paste0(c("_max", "_avg", "_std", "_dns", "_p5", "_p25",
                   "_p50", "_p75", "_p95"), ".tif"), metric_bands)
    } else list(raster(".tif", 20, metric_bands)),
    generate_DEM = list(raster()),
    generate_DSM1 = list(raster()),
    generate_DSM2 = list(raster()),
    tree_detection = list(list(suffix = ".gpkg", kind = "vector")),
    complex_task1 = list(
      list(suffix = ".laz", kind = "points"),
      raster("_dtm.tif", reference = "generate_DEM"),
      raster("_chm.tif", reference = "generate_DSM1")),
    complex_task2 = list(
      list(suffix = ".laz", kind = "points"),
      raster("_dtm.tif", reference = "generate_DEM"),
      raster("_chm.tif", reference = "generate_DSM1"),
      raster("_metrics.tif", 20, metric_bands, "pixel_metrics_2"),
      list(suffix = "_trees.gpkg", kind = "vector")),
    stop("Unknown validation workload: ", task_id))
  specs
}

benchmark_validation_points <- function(manifest, tile) {
  tiles <- manifest$tiles
  b <- manifest$buffer
  selected <- tiles$Max.X >= tile$Min.X - b & tiles$Min.X <= tile$Max.X + b &
    tiles$Max.Y >= tile$Min.Y - b & tiles$Min.Y <= tile$Max.Y + b
  clouds <- lapply(which(selected), function(i) {
    cloud <- lidR::readLAS(tiles$filename[i], select = "*")
    if (is.null(cloud) || lidR::npoints(cloud) != tiles$count[i]) {
      stop("Input payload/header count mismatch: ", tiles$filename[i])
    }
    data <- as.data.frame(cloud@data)
    if (any(!is.finite(as.matrix(data[c("X", "Y", "Z")])))) {
      stop("Nonfinite input coordinates: ", tiles$filename[i])
    }
    data[data$X >= tile$Min.X - b & data$X <= tile$Max.X + b &
           data$Y >= tile$Min.Y - b & data$Y <= tile$Max.Y + b, , drop = FALSE]
  })
  data <- do.call(rbind, clouds)
  if (!nrow(data)) stop("No reference points for tile: ", tile$id)
  cloud <- lidR::LAS(data)
  sf::st_crs(cloud) <- manifest$crs
  cloud
}

benchmark_validation_grid <- function(tile, resolution, crs) {
  terra::rast(xmin = floor(tile$Min.X / resolution) * resolution,
              xmax = ceiling(tile$Max.X / resolution) * resolution,
              ymin = floor(tile$Min.Y / resolution) * resolution,
              ymax = ceiling(tile$Max.Y / resolution) * resolution,
              resolution = resolution, crs = if (is.na(crs)) "" else crs$wkt)
}

benchmark_reference_raster <- function(cloud, grid, task_id) {
  if (task_id %in% c("generate_DEM", "generate_DSM2")) {
    return(if (task_id == "generate_DEM") {
      lidR::rasterize_terrain(cloud, res = grid, algorithm = lidR::tin(),
                              use_class = c(2L, 9L), shape = "convex")
    } else {
      lidR::rasterize_canopy(cloud, res = grid,
                             algorithm = lidR::dsmtin(highest = FALSE, max_edge = 0))
    })
  }
  data <- cloud@data
  xy <- as.matrix(data[, c("X", "Y"), with = FALSE])
  if (task_id == "generate_DSM1") {
    resolution <- terra::res(grid)[1L]
    xy <- floor(xy / resolution) * resolution + resolution / 2
  }
  cell <- terra::cellFromXY(grid, xy)
  # Grid maximum edges have no owner; interior cells can use buffered points.
  extent <- as.vector(terra::ext(grid))
  cell[xy[, 1] >= extent[2] | xy[, 2] >= extent[4]] <- NA_integer_
  groups <- split(data$Z[!is.na(cell)], cell[!is.na(cell)])
  bands <- switch(task_id,
    pixel_metrics_1a = "z_mean",
    pixel_metrics_2 = names(metrics_multiple(numeric())),
    generate_DSM1 = "max",
    stop("No numerical raster reference: ", task_id))
  expected <- matrix(NA_real_, nrow = terra::ncell(grid), ncol = length(bands))
  for (id in names(groups)) {
    z <- groups[[id]]
    expected[as.integer(id), ] <- switch(task_id,
      pixel_metrics_1a = mean(z), pixel_metrics_2 = unlist(metrics_multiple(z)),
      generate_DSM1 = max(z))
  }
  grid <- terra::rast(grid, nlyrs = length(bands))
  terra::values(grid) <- expected
  names(grid) <- bands
  grid
}

benchmark_compare_values <- function(actual, expected, absolute = 2e-6, relative = 2e-7,
                                      nodata = c(-9999, -1000000)) {
  if (!identical(dim(actual), dim(expected)) || length(actual) != length(expected)) {
    stop("Numerical shape mismatch.")
  }
  if (any(is.infinite(actual)) || any(is.infinite(expected))) stop("Infinite output/reference values.")
  if (any(is.na(actual) != is.na(expected))) stop("Coverage/NoData mask mismatch.")
  valid <- !is.na(expected)
  error <- abs(actual[valid] - expected[valid])
  limit <- absolute + relative * abs(expected[valid])
  if (any(error > limit)) {
    stop("Numerical tolerance exceeded; maximum absolute error = ", max(error), ".")
  }
  if (any(expected[valid] %in% nodata)) {
    stop("Reference collides with a workload NoData sentinel.")
  }
  list(valid_values = sum(valid), max_absolute_error = if (length(error)) max(error) else 0,
       absolute_tolerance = absolute, relative_tolerance = relative)
}

benchmark_validate_raster <- function(file, spec, tile, manifest, cloud, numerical) {
  raster <- terra::rast(file)
  grid <- benchmark_validation_grid(tile, spec$resolution, manifest$crs)
  if (!terra::compareGeom(raster, grid, lyrs = FALSE, stopOnError = FALSE) ||
      any(abs(terra::origin(raster)) > 1e-8) ||
      any(abs(terra::res(raster) - spec$resolution) > 1e-8)) {
    stop("Raster CRS/extent/resolution/origin mismatch; actual extent ",
         paste(as.vector(terra::ext(raster)), collapse = ","),
         "; expected ", paste(as.vector(terra::ext(grid)), collapse = ","), ".")
  }
  layers <- max(1L, length(spec$bands))
  if (terra::nlyr(raster) != layers) stop("Raster band count mismatch.")
  if (length(spec$bands) && numerical && !identical(names(raster), spec$bands)) {
    stop("Raster band names/order mismatch.")
  }
  actual <- terra::values(raster, mat = TRUE)
  if (any(is.infinite(actual))) stop("Infinite raster values.")
  if (!numerical) {
    if (!any(is.finite(actual))) stop("Raster has no finite coverage; native support is unverified.")
    return(list(valid_values = sum(is.finite(actual))))
  }
  reference <- benchmark_reference_raster(cloud, grid, spec$reference)
  benchmark_compare_values(actual, terra::values(reference, mat = TRUE))
}

benchmark_validate_points <- function(file, cloud, tile, manifest, numerical) {
  actual <- lidR::readLAS(file, select = "*")
  if (is.null(actual)) stop("Missing/empty point-cloud payload.")
  if (!isTRUE(sf::st_crs(actual) == manifest$crs)) stop("Point-cloud CRS mismatch.")
  data <- as.data.frame(actual@data)
  if (lidR::npoints(actual) != lidR::npoints(lidR::readLASheader(file)) ||
      any(!is.finite(as.matrix(data[c("X", "Y", "Z")])))) {
    stop("Point-cloud count/nonfinite coordinate failure.")
  }
  if (any(data$X < tile$Min.X | data$X > tile$Max.X |
          data$Y < tile$Min.Y | data$Y > tile$Max.Y)) stop("Point cloud contains buffer points.")
  if (!numerical) return(list(point_count = nrow(data)))
  reference <- lidR::normalize_height(cloud, lidR::tin(), use_class = c(2L, 9L))
  expected <- as.data.frame(reference@data)
  expected <- expected[expected$X >= tile$Min.X & expected$X < tile$Max.X &
                         expected$Y >= tile$Min.Y & expected$Y < tile$Max.Y, , drop = FALSE]
  # Zref is an in-memory normalize_height helper, not an original LAS attribute.
  expected$Zref <- NULL
  if (!setequal(names(data), names(expected)) || nrow(data) != nrow(expected)) {
    stop("Owned point count/attribute schema mismatch; actual ", nrow(data),
         " [", paste(names(data), collapse = ","), "]; expected ", nrow(expected),
         " [", paste(names(expected), collapse = ","), "].")
  }
  attributes <- setdiff(names(expected), "Z")
  sorted <- function(x) x[do.call(order, x[c(attributes, "Z")]), , drop = FALSE]
  data <- sorted(data)
  expected <- sorted(expected)
  if (!isTRUE(all.equal(data[attributes], expected[attributes], check.attributes = FALSE,
                        tolerance = 0))) stop("Retained point attributes/XY mismatch.")
  scale <- actual@header@PHB[["Z scale factor"]]
  benchmark_compare_values(data$Z, expected$Z, absolute = scale / 2 + 1e-6,
                           relative = 0, nodata = numeric())
}

benchmark_validate_vector <- function(file, cloud, tile, manifest, numerical) {
  layers <- sf::st_layers(file)
  if (length(layers$name) != 1L) stop("Expected exactly one vector layer.")
  trees <- sf::st_read(file, quiet = TRUE)
  if (!isTRUE(sf::st_crs(trees) == manifest$crs) ||
      any(!sf::st_is_valid(trees)) ||
      any(as.character(sf::st_geometry_type(trees)) != "POINT") ||
      !all(grepl("Point", layers$geomtype, ignore.case = TRUE))) {
    stop("Vector CRS/geometry mismatch.")
  }
  xyz <- sf::st_coordinates(trees)
  if (!nrow(trees) && "Z" %in% names(trees) && is.numeric(trees$Z)) {
    xyz <- matrix(numeric(), ncol = 3L, dimnames = list(NULL, c("X", "Y", "Z")))
  }
  if (!"Z" %in% colnames(xyz) && "Z" %in% names(trees)) {
    xyz <- cbind(xyz, Z = trees$Z)
  }
  if (!all(c("X", "Y", "Z") %in% colnames(xyz))) stop("Treetops require XYZ coordinates.")
  xyz <- xyz[, c("X", "Y", "Z"), drop = FALSE]
  if (any(!is.finite(xyz)) || anyDuplicated(as.data.frame(xyz))) {
    stop("Nonfinite/duplicate treetop coordinates.")
  }
  if (any(xyz[, 1] < tile$Min.X | xyz[, 1] > tile$Max.X |
          xyz[, 2] < tile$Min.Y | xyz[, 2] > tile$Max.Y | xyz[, 3] < 2)) {
    stop("Treetop outside core/height contract.")
  }
  if (!numerical) return(list(tree_count = nrow(xyz)))
  reference <- lidR::locate_trees(cloud, lidR::lmf(ws = 5, hmin = 2, shape = "circular"))
  expected <- sf::st_coordinates(reference)
  if (!nrow(reference)) {
    expected <- matrix(numeric(), ncol = 3L, dimnames = list(NULL, c("X", "Y", "Z")))
  }
  if (!"Z" %in% colnames(expected) && "Z" %in% names(reference)) {
    expected <- cbind(expected, Z = reference$Z)
  }
  expected <- expected[, c("X", "Y", "Z"), drop = FALSE]
  expected <- expected[expected[, 1] >= tile$Min.X & expected[, 1] < tile$Max.X &
                         expected[, 2] >= tile$Min.Y & expected[, 2] < tile$Max.Y, , drop = FALSE]
  sorted <- function(x) {
    x <- x[order(x[, 1], x[, 2], x[, 3]), , drop = FALSE]
    dimnames(x) <- NULL
    x
  }
  benchmark_compare_values(sorted(xyz), sorted(expected), absolute = 1e-6,
                           relative = 0, nodata = numeric())
}

benchmark_validate_artifacts <- function(path, manifest, task_id, tool, execution = NULL) {
  contract <- benchmark_workload_contract(task_id, tool)
  specs <- contract$validation
  tiles <- manifest$tiles
  expected <- unlist(lapply(tiles$id, function(id) {
    vapply(specs, function(spec) paste0(id, spec$suffix), character(1))
  }), use.names = FALSE)
  report <- list(
    schema = "chat7-v1", task_id = task_id, tool = tool,
    started_at = Sys.time(),
    workload_contract = contract$workload_contract, input_manifest = manifest,
    expected_files = expected, expected_file_count = length(expected),
    artifacts = list(), status = "passed", diagnostic = NA_character_
  )
  reasons <- character()
  if (!contract$supported) reasons <- c(reasons, contract$comparison_note)
  if (!is.null(execution)) {
    if (execution$execution_status != "succeeded") reasons <- c(reasons, "Execution did not succeed.")
    if (!is.null(execution$diagnostic) && !is.na(execution$diagnostic) &&
        nzchar(execution$diagnostic)) {
      reasons <- c(reasons, paste0("Execution diagnostic requires review: ", execution$diagnostic))
    }
  }
  files <- list.files(path, full.names = TRUE, recursive = TRUE)
  primary <- files[grepl("\\.(tif|laz|las|gpkg)$", files, ignore.case = TRUE)]
  report$outputs <- benchmark_checksums(primary)
  if (!setequal(basename(primary), expected) || anyDuplicated(basename(primary)) ||
      any(dirname(primary) != path)) {
    reasons <- c(reasons, paste0(
      "Primary artifact inventory mismatch. Missing: ",
      paste(setdiff(expected, basename(primary)), collapse = ", "),
      "; unexpected: ", paste(setdiff(basename(primary), expected), collapse = ", ")))
  }
  numerical <- tool != "lastools" && !(tool == "lasR" && task_id == "tree_detection") &&
    !startsWith(task_id, "complex_")
  for (i in seq_len(nrow(tiles))) {
    tile <- tiles[i, , drop = FALSE]
    warnings <- character()
    tile_result <- tryCatch(withCallingHandlers({
      cloud <- benchmark_validation_points(manifest, tile)
      lapply(specs, function(spec) {
        file <- file.path(path, paste0(tile$id, spec$suffix))
        detail <- tryCatch({
          if (!file.exists(file) || file.info(file)$size == 0) stop("Missing/empty artifact.")
          switch(spec$kind,
            raster = benchmark_validate_raster(file, spec, tile, manifest, cloud, numerical),
            points = benchmark_validate_points(file, cloud, tile, manifest, numerical),
            vector = benchmark_validate_vector(file, cloud, tile, manifest, numerical))
        }, error = function(e) list(error = conditionMessage(e)))
        list(file = basename(file), detail = detail)
      })
    }, warning = function(w) {
      warnings <<- c(warnings, paste0(tile$id, ": validation warning: ", conditionMessage(w)))
      invokeRestart("muffleWarning")
    }), error = function(e) list(list(file = tile$id, detail = list(error = conditionMessage(e)))))
    reasons <- c(reasons, warnings)
    report$artifacts <- c(report$artifacts, tile_result)
  }
  for (artifact in report$artifacts) {
    if (!is.null(artifact$detail$error)) {
      reasons <- c(reasons, paste0(artifact$file, ": ", artifact$detail$error))
    }
  }
  if (!identical(benchmark_checksums(manifest$inputs$path), manifest$inputs)) {
    reasons <- c(reasons, "Input bytes changed during execution/validation.")
  }
  if (length(reasons)) {
    report$status <- "failed"
    report$diagnostic <- paste(unique(reasons), collapse = "\n")
    message("Artifact validation failed: ", report$diagnostic)
  } else if (!numerical) {
    report$status <- "unverified"
    report$diagnostic <- "Structural checks passed; native numerical/ownership conventions are unverified."
  } else if (is.na(manifest$crs) || !identical(manifest$crs$units_gdal, "metre")) {
    report$status <- "unverified"
    report$diagnostic <- "Numerical checks passed; input horizontal CRS/metre units are unverified."
  }
  report$finished_at <- Sys.time()
  report
}
