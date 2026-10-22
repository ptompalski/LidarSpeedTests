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
    artifacts = if (implemented) artifacts else character()
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
  if (!is.character(config$tools) || !length(config$tools) ||
      anyNA(config$tools) || anyDuplicated(config$tools) ||
      any(!config$tools %in% c("lidR", "lasR", "lastools"))) {
    stop("tools must explicitly select lidR, lasR, and/or lastools.")
  }
  if (!is.logical(config$resource_monitoring) ||
      length(config$resource_monitoring) != 1L || is.na(config$resource_monitoring)) {
    stop("resource_monitoring must be TRUE or FALSE.")
  }
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
  opt_stop_early(ctg) <- FALSE
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

check_results <- function(path) {
  
  #count number of files and total size
  file_count <- length(list.files(path))
  
  output_size <- sum(fs::dir_info(path)$size) 
  
  return(
    list(
      output_file_count = file_count,
      output_total_size = output_size
    )
  )
  
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
    finish_time = as.POSIXct(NA)
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
        eval(parse(text = task_call), envir = envir)
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
  if (.Platform$OS.type != "windows") {
    stop("Resource monitoring is Windows-only; set resource_monitoring = FALSE on this platform.")
  }
  config$system_monitoring_path <- benchmark_directory(
    config$system_monitoring_path, "Monitoring reports", TRUE
  )
  if (any(vapply(config$input_roots, benchmark_paths_overlap, logical(1),
                 b = config$system_monitoring_path))) {
    stop("Monitoring reports must not overlap input directories.")
  }
  if (!is.character(config$monitor_script) || length(config$monitor_script) != 1L ||
      is.na(config$monitor_script) || !file.exists(config$monitor_script) ||
      dir.exists(config$monitor_script)) {
    stop("Monitoring script is missing: ", config$monitor_script)
  }
  config$monitor_script <- normalizePath(config$monitor_script, winslash = "/")
  config$powershell <- benchmark_resolve_executable(config$powershell)
  config
}

benchmark_start_monitor <- function(config, label) {
  config <- benchmark_validate_monitor(config)
  label <- benchmark_file_component(label, "Monitor label")
  stdout <- file.path(config$system_monitoring_path, paste0(label, "-stdout.log"))
  stderr <- file.path(config$system_monitoring_path, paste0(label, "-stderr.log"))
  monitor <- processx::process$new(
    config$powershell,
    c("-NoProfile", "-NonInteractive", "-File", config$monitor_script,
      label, config$system_monitoring_path),
    stdout = stdout, stderr = stderr, cleanup_tree = TRUE
  )
  ready <- FALSE
  on.exit(if (!ready) {
    monitor$kill_tree()
    monitor$wait(5000)
  })
  deadline <- Sys.time() + 10
  repeat {
    monitor$wait(100)
    if (file.exists(stderr) && file.info(stderr)$size > 0) {
      stop("Monitoring process reported startup errors: ",
           paste(readLines(stderr, warn = FALSE), collapse = "\n"))
    }
    if (!monitor$is_alive()) stop("Monitoring process exited during startup.")
    if (file.exists(stdout) && file.info(stdout)$size > 0) break
    if (Sys.time() >= deadline) {
      stop("Monitoring process produced no startup output; inspect ", stdout, " and ", stderr)
    }
  }
  ready <- TRUE
  monitor
}

benchmark_stop_monitor <- function(monitor) {
  was_alive <- monitor$is_alive()
  monitor$kill_tree()
  monitor$wait(5000)
  if (monitor$is_alive()) stop("Could not stop the monitoring process tree.")
  if (!was_alive) stop("Monitoring process exited before workload cleanup.")
  invisible(NULL)
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
      output_file_count = NA_integer_, output_total_size = NA_character_
    )
  }
  result$success <- 0L
  result$execution_status <- "failed"
  diagnostics <- c(result$diagnostic, conditionMessage(error))
  result$diagnostic <- paste(diagnostics[!is.na(diagnostics)], collapse = "\n")
  message(tool, ": ", conditionMessage(error))
  result
}

benchmark_with_resources <- function(tool, cores, config, label, run) {
  cleanup <- list()
  cleaned <- FALSE
  on.exit(if (!cleaned) benchmark_cleanup(cleanup))
  result <- tryCatch({
    if (tool == "lidR") {
      previous_plan <- future::plan()
      cleanup[[length(cleanup) + 1L]] <- function() future::plan(previous_plan)
      future::plan(future::multisession, workers = cores)
    }
    if (tool == "lasR") {
      cleanup[[length(cleanup) + 1L]] <- function() unset_parallel_strategy()
      unset_parallel_strategy()
      set_parallel_strategy(concurrent_files(ncores = cores))
    }
    if (config$resource_monitoring) {
      monitor <- benchmark_start_monitor(config, label)
      cleanup[[length(cleanup) + 1L]] <- function() benchmark_stop_monitor(monitor)
    }
    run()
  }, error = function(e) benchmark_failed_result(tool, e))
  cleanup_error <- tryCatch({
    benchmark_cleanup(cleanup)
    NULL
  }, error = identity)
  cleaned <- TRUE
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
    result <- benchmark_with_resources(tool, context$cores, config, label, function() {
      benchmark_execute_call(task_call, tool, context)
    })
    counts <- check_results(context$dir_out)
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
                          campaign, repetition_id = 1L) {
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
  if (!any(campaign$manifest$identity$selection$task_id == task_id &
           campaign$manifest$identity$selection$run_id == run_id)) {
    stop("Task/run is not in the campaign selection.")
  }
  cli::cli_h1("Benchmark run started: {task_id}, {run_id}")
  context <- new.env(parent = environment())
  context$dir_in <- benchmark_directory(selected$dir_in, "Benchmark input")
  context$scratch_root <- run$scratch_root
  context$cores <- run$cores
  context$lastools_path <- config$lastools_path
  context$output_format <- task$output_format
  metadata <- NULL
  for (tool in names(enabled)[enabled]) {
    call <- task[[paste0("task_call_", tool)]][[1L]]
    parts <- list(campaign$manifest$campaign_id, task_id, run_id, tool)
    invisible(lapply(parts, benchmark_file_component, label = "Result filename"))
    label <- paste(unlist(parts), collapse = "_")
    if (!force[[tool]] &&
        benchmark_campaign_completed(campaign, task_id, run_id, tool, repetition_id)) {
      cli::cli_inform("Matching campaign work is already complete: {label}")
      next
    }
    cli::cli_h2("Running {tool}")
    attempt <- benchmark_attempt_start(campaign, task_id, run_id, tool, repetition_id)
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
    common <- c(list(task_id = task_id, run_id = run_id, cores = run$cores),
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
    schema = 1L, selection = selection, tasks = tasks, runs = runs, config = config,
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
        policy = "lidR multisession; lasR concurrent_files; LAStools -cores",
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

benchmark_attempt_start <- function(campaign, task_id, run_id, tool, repetition_id) {
  benchmark_repetition_id(repetition_id)
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
    repetition_id = repetition_id, attempt_id = basename(path),
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
    cpu_threads = readr::col_double(), ram = readr::col_double(),
    total_point_count = readr::col_double(), density_pulses = readr::col_double(),
    file_count = readr::col_double(),
    start_time = readr::col_datetime(), finish_time = readr::col_datetime(),
    recorded_at = readr::col_datetime(), output_file_count = readr::col_double(),
    exit_status = readr::col_integer(), density_points = readr::col_double()
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
      !record$tool %in% manifest$identity$config$tools ||
      !any(manifest$identity$selection$task_id == record$task_id &
           manifest$identity$selection$run_id == record$run_id)) {
    stop("Attempt identity does not match its campaign/start record: ", attempt)
  }
  benchmark_repetition_id(record$repetition_id)
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

benchmark_campaign_completed <- function(campaign, task_id, run_id, tool, repetition_id) {
  # An interrupted attempt must not prevent an explicit retry.
  paths <- list.files(file.path(campaign$path, "attempts"), "^completed\\.rds$",
                      recursive = TRUE, full.names = TRUE)
  any(vapply(paths, function(path) {
    record <- benchmark_read_attempt(dirname(path), campaign$manifest)
    identical(as.character(record$task_id), task_id) &&
      identical(as.character(record$run_id), run_id) &&
      identical(as.character(record$tool), tool) &&
      identical(as.numeric(record$repetition_id), as.numeric(repetition_id)) &&
      record$execution_status %in% c("succeeded", "skipped")
  }, logical(1)))
}
