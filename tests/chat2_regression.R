# Run from the repository root: Rscript --vanilla tests/chat2_regression.R
local({
  library(tibble)
  library(tidyr)
  stopifnot(requireNamespace("processx", quietly = TRUE),
            requireNamespace("future", quietly = TRUE),
            requireNamespace("fs", quietly = TRUE))
  repo <- getwd()
  env <- new.env()
  sys.source("R/1_functions.R", env)
  expect_error <- function(expr, text) {
    error <- tryCatch(force(expr), error = identity)
    stopifnot(inherits(error, "error"), grepl(text, conditionMessage(error), fixed = TRUE))
  }
  test_dir <- tempfile("lidar chat2 ")
  dir.create(test_dir)
  test_dir <- normalizePath(test_dir, winslash = "/")
  previous_config <- Sys.getenv("LIDAR_BENCHMARK_CONFIG", unset = NA_character_)
  old_plan <- future::plan()
  on.exit({
    future::plan(old_plan)
    if (is.na(previous_config)) Sys.unsetenv("LIDAR_BENCHMARK_CONFIG")
    else Sys.setenv(LIDAR_BENCHMARK_CONFIG = previous_config)
    unlink(test_dir, recursive = TRUE)
  })
  input <- file.path(test_dir, "input data")
  scratch <- file.path(test_dir, "scratch area")
  reports <- file.path(test_dir, "reports area")
  bin <- file.path(test_dir, "native tools")
  for (path in c(input, scratch, reports, bin)) dir.create(path)
  files <- file.path(input, c("tile one.laz", "tile;literal.laz"))
  stopifnot(all(file.create(files)))
  writeLines("input sentinel", files[1L])
  sentinel <- tools::md5sum(files)
  config <- list(
    input_roots = c(SSD = input), scratch_roots = c(SSD = scratch),
    reports_path = reports, cores = c(1L, 2L), tools = c("lidR", "lasR", "lastools"),
    lastools_path = bin, workstation_id = "fixture", resource_monitoring = FALSE
  )
  config <- env$benchmark_validate_config(config)
  config_file <- file.path(test_dir, "machine config.R")
  dput(config, config_file)
  stopifnot(identical(env$benchmark_load_config(config_file), config))
  Sys.unsetenv("LIDAR_BENCHMARK_CONFIG")
  stopifnot(is.null(env$benchmark_load_config(file.path(test_dir, "missing.R"), FALSE)))
  expect_error(env$benchmark_load_config(file.path(test_dir, "missing.R")), "Missing benchmark")
  Sys.setenv(LIDAR_BENCHMARK_CONFIG = file.path(test_dir, "missing.R"))
  expect_error(env$benchmark_load_config(required = FALSE), "Missing benchmark")
  Sys.setenv(LIDAR_BENCHMARK_CONFIG = config_file)
  for (field in c("input_roots", "scratch_roots", "reports_path", "lastools_path")) {
    invalid <- config
    invalid[[field]] <- if (field %in% c("input_roots", "scratch_roots")) {
      c(SSD = file.path(test_dir, "missing"))
    } else file.path(test_dir, "missing")
    expect_error(env$benchmark_validate_config(invalid), "existing directory")
  }
  invalid <- config
  invalid$scratch_roots <- c(SSD = input)
  expect_error(env$benchmark_validate_config(invalid), "overlaps")
  invalid$scratch_roots <- c(SSD = reports)
  expect_error(env$benchmark_validate_config(invalid), "overlaps")
  expect_error(env$benchmark_validate_scratch_root(dirname(test_dir), input), "overlaps")
  root <- normalizePath(if (.Platform$OS.type == "windows") {
    paste0(substr(test_dir, 1L, 2L), "/")
  } else "/", winslash = "/")
  expect_error(env$benchmark_validate_scratch_root(root, character()), "filesystem root")
  expect_error(env$benchmark_validate_scratch_root(path.expand("~"), character()), "home")
  expect_error(env$benchmark_validate_scratch_root(repo, character()), "working directory")
  invalid <- config
  invalid$cores <- c(0, NA_real_)
  expect_error(env$benchmark_validate_config(invalid), "positive integers")
  if (.Platform$OS.type != "windows") {
    restricted <- file.path(test_dir, "restricted directory")
    dir.create(restricted)
    Sys.chmod(restricted, "0444")
    if (file.access(restricted, 1) != 0) {
      expect_error(env$benchmark_directory(restricted, "Input"), "not searchable")
    }
    Sys.chmod(restricted, "0555")
    if (file.access(restricted, 2) != 0) {
      expect_error(env$benchmark_directory(restricted, "Reports", TRUE), "not writable")
    }
    Sys.chmod(restricted, "0700")
  }
  if (.Platform$OS.type != "windows") {
    invalid <- config
    invalid$resource_monitoring <- TRUE
    expect_error(env$benchmark_validate_config(invalid), "Windows-only")
  }
  env$benchmark_validate_run_paths(config, input, scratch)
  expect_error(env$benchmark_validate_run_paths(config, test_dir, scratch), "outside")

  owner1 <- env$benchmark_scratch_create(scratch, c(input, reports))
  owner2 <- env$benchmark_scratch_create(scratch, c(input, reports))
  stopifnot(owner1$path != owner2$path, dir.exists(owner1$path), dir.exists(owner2$path))
  writeLines("output", file.path(owner1$path, "result.tif"))
  expect_error(env$benchmark_scratch_cleanup(scratch), "unowned")
  expect_error(env$benchmark_scratch_cleanup(input), "unowned")
  expect_error(env$benchmark_scratch_cleanup(reports), "unowned")
  forged <- owner1
  forged$path <- input
  expect_error(env$benchmark_scratch_cleanup(forged), "unowned")
  arbitrary <- file.path(scratch, "pre-existing-user-directory")
  dir.create(arbitrary)
  expect_error(env$benchmark_scratch_cleanup(list(path = arbitrary)), "unowned")
  env$benchmark_scratch_cleanup(owner1)
  stopifnot(!dir.exists(owner1$path), dir.exists(owner2$path), dir.exists(arbitrary))
  env$benchmark_scratch_cleanup(owner2)
  expect_error(env$benchmark_scratch_cleanup(owner2), "unowned")
  owner <- env$benchmark_scratch_create(scratch, c(input, reports))
  original <- paste0(owner$path, "-original")
  stopifnot(file.rename(owner$path, original), dir.create(owner$path))
  expect_error(env$benchmark_scratch_cleanup(owner), "identity changed")
  stopifnot(dir.exists(owner$path))
  unlink(owner$path, recursive = TRUE)
  stopifnot(file.rename(original, owner$path))
  env$benchmark_scratch_cleanup(owner)
  if (.Platform$OS.type != "windows") {
    owner <- env$benchmark_scratch_create(scratch, c(input, reports))
    link <- file.path(owner$path, "input-link")
    stopifnot(file.symlink(input, link))
    expect_error(env$benchmark_scratch_cleanup(owner), "symbolic link")
    stopifnot(identical(tools::md5sum(files), sentinel))
    unlink(link)
    env$benchmark_scratch_cleanup(owner)
    root_link <- file.path(test_dir, "scratch-alias")
    stopifnot(file.symlink(input, root_link))
    expect_error(env$benchmark_scratch_create(root_link, c(input, reports)), "overlaps")
    unlink(root_link)
  }
  unlink(arbitrary, recursive = TRUE)

  # Rscript acts as the native executable; no LAStools or point clouds are needed.
  rscript <- file.path(R.home("bin"),
                       if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  suffix <- if (.Platform$OS.type == "windows") ".exe" else ""
  for (name in c("probe", "lascanopy", "lasheight64", "blast2dem", "lasgrid64")) {
    stopifnot(file.copy(rscript, file.path(bin, paste0(name, suffix))))
  }
  probe <- file.path(bin, paste0("probe", suffix))
  script <- file.path(test_dir, "native process fixture.R")
  writeLines(c(
    "args <- commandArgs(TRUE)",
    "saveRDS(args[-c(1L, 2L)], args[2L])",
    "output <- match('-odir', args)",
    "if (!is.na(output)) writeLines('artifact', file.path(args[output + 1L], 'result.tif'))",
    "cat('native stdout\\n')",
    "cat('native stderr\\n', file = stderr())",
    "quit(status = as.integer(args[1L]))"
  ), script)
  observed <- file.path(test_dir, "observed arguments.rds")
  special <- c("space in argument", "literal;not-a-command", "$literal",
               "double\"quote", "single'quote", "back`tick", "")
  process <- env$benchmark_process(probe, c("--vanilla", script, "0", observed, special))
  stopifnot(process$status == 0L, identical(readRDS(observed), special),
            process$stdout == "native stdout\n", process$stderr == "native stderr\n")
  expect_error(env$benchmark_process(file.path(bin, "absent-tool")), "missing or not executable")
  output <- file.path(test_dir, "output with spaces")
  dir.create(output)
  call_env <- list2env(list(
    f = files, dir_out = output,
    cores = 2L, lastools_path = bin
  ))
  task_call <- list(executable = "probe", args = c(
    "--vanilla", script, "7", observed, "-i", "{inputs}", "-odir", "{output}",
    "-cores", "{cores}"
  ))
  failed <- env$benchmark_execute_call(task_call, "lastools", call_env)
  stopifnot(
    failed$success == 0L, failed$exit_status == 7L,
    failed$stdout == "native stdout\n", failed$stderr == "native stderr\n",
    grepl("native stderr", failed$diagnostic, fixed = TRUE),
    identical(readRDS(observed), c("-i", files, "-odir", call_env$dir_out, "-cores", "2"))
  )
  missing <- task_call
  missing$executable <- "absent-tool"
  failed_missing <- env$benchmark_execute_call(missing, "lastools", call_env)
  stopifnot(failed_missing$success == 0L,
            grepl("absent-tool", failed_missing$diagnostic, fixed = TRUE))
  expect_error(env$benchmark_lastools_command("probe -i files", call_env), "not shell commands")
  invalid_call <- task_call
  invalid_call$args <- "{unknown}"
  expect_error(env$benchmark_lastools_command(invalid_call, call_env), "Unknown")
  if (.Platform$OS.type != "windows") {
    version_tool <- file.path(bin, "las2las")
    writeLines(c("#!/bin/sh", "printf 'LAStools version 240101\\n' >&2"), version_tool)
    Sys.chmod(version_tool, "0755")
    stopifnot(env$benchmark_lastools_version(bin) == 240101)
    writeLines(c("#!/bin/sh", "printf 'version query fixture failed\\n' >&2", "exit 9"),
               version_tool)
    expect_error(env$benchmark_lastools_version(bin), "version query fixture failed")
  }

  # Execute setup definitions while omitting only third-party package attachment.
  setup <- new.env()
  setup$source <- function(file) sys.source(file.path(repo, file), setup)
  setup$library <- function(...) invisible(NULL)
  sys.source("R/0_setup.R", setup)
  stopifnot(nrow(setup$benchmark_tasks) == 9L, nrow(setup$benchmark_runs) == 12L,
            "ALS_1000_1_1_SSD_SSD" %in% setup$benchmark_runs$ID,
            all(setup$benchmark_runs$scratch_root == scratch),
            identical(setup$benchmark_runs$cores, rep(c(1L, 2L), each = 6L)),
            sum(vapply(setup$benchmark_tasks$task_call_lastools,
                       env$benchmark_call_is_empty, logical(1))) == 4L)
  for (path in unique(setup$benchmark_runs$dir_in_org)) dir.create(path, recursive = TRUE)
  selection <- data.frame(task_id = "pixel_metrics_2",
                          run_id = setup$benchmark_runs$ID[1L])
  setup$benchmark_validate_selection(selection, setup$benchmark_tasks,
                                     setup$benchmark_runs, config)
  missing_runs <- setup$benchmark_runs
  missing_runs$dir_in_org[1L] <- file.path(input, "missing dataset")
  expect_error(setup$benchmark_validate_selection(
    selection, setup$benchmark_tasks, missing_runs, config
  ), "existing directory")

  # Error-path resource cleanup with a real future worker and a real monitor
  # stand-in. Production still rejects Windows monitoring requests on Linux.
  monitor_script <- file.path(test_dir, "monitor stand-in.R")
  writeLines(c("cat('ready\\n'); flush.console()", "Sys.sleep(60)"), monitor_script)
  if (.Platform$OS.type != "windows") {
    # Exercise the production startup/stop code with a native stand-in, bypassing
    # only the Windows platform validator for this isolated lifecycle fixture.
    monitor_env <- new.env()
    sys.source("R/1_functions.R", monitor_env)
    monitor_env$benchmark_validate_monitor <- function(config) config
    wrapper <- file.path(bin, "monitor stand-in")
    writeLines(c("#!/bin/sh",
                 paste("exec", shQuote(rscript), "--vanilla", shQuote(monitor_script))),
               wrapper)
    Sys.chmod(wrapper, "0755")
    monitor_output <- file.path(test_dir, "monitor logs")
    dir.create(monitor_output)
    monitor_config <- list(
      powershell = wrapper, monitor_script = monitor_script,
      system_monitoring_path = monitor_output
    )
    started <- monitor_env$benchmark_start_monitor(monitor_config, "startup")
    stopifnot(started$is_alive(),
              any(grepl("ready", readLines(file.path(monitor_output, "startup-stdout.log")))))
    monitor_env$benchmark_stop_monitor(started)
    stopifnot(!started$is_alive())
    writeLines(c("#!/bin/sh", "printf 'monitor startup fixture failed\\n' >&2", "sleep 60"),
               wrapper)
    expect_error(monitor_env$benchmark_start_monitor(monitor_config, "startup-error"),
                 "monitor startup fixture failed")
  }
  monitor <- NULL
  env$benchmark_start_monitor <- function(config, label) {
    monitor <<- processx::process$new(
      rscript, c("--vanilla", monitor_script), stdout = "|", stderr = "|",
      cleanup_tree = TRUE
    )
    monitor$poll_io(5000)
    stopifnot(monitor$is_alive(), identical(monitor$read_output_lines(), "ready"))
    monitor
  }
  future::plan(future::sequential)
  worker <- NULL
  monitored_config <- config
  monitored_config$resource_monitoring <- TRUE
  failed_resources <- env$benchmark_with_resources(
    "lidR", I(1L), monitored_config, "fixture", function() {
      pid <- future::value(future::future(Sys.getpid()))
      stopifnot(pid != Sys.getpid())
      worker <<- ps::ps_handle(pid)
      stop("intentional worker failure")
    }
  )
  stopifnot(failed_resources$success == 0L, !monitor$is_alive(),
            inherits(future::plan(), "sequential"))
  for (i in seq_len(100L)) {
    if (!ps::ps_is_running(worker)) break
    Sys.sleep(0.02)
  }
  stopifnot(!ps::ps_is_running(worker))
  env$benchmark_start_monitor <- function(...) stop("intentional startup failure")
  failed_start <- env$benchmark_with_resources(
    "lidR", I(1L), monitored_config, "fixture", function() stop("must not execute")
  )
  stopifnot(failed_start$success == 0L,
            grepl("startup failure", failed_start$diagnostic, fixed = TRUE),
            inherits(future::plan(), "sequential"))
  strategy <- FALSE
  env$unset_parallel_strategy <- function() strategy <<- FALSE
  env$concurrent_files <- function(ncores) ncores
  env$set_parallel_strategy <- function(...) strategy <<- TRUE
  failed_lasR <- env$benchmark_with_resources("lasR", 1L, config, "fixture", function() {
    stopifnot(strategy)
    stop("intentional lasR failure")
  })
  stopifnot(failed_lasR$success == 0L, !strategy)
  unset_calls <- 0L
  env$unset_parallel_strategy <- function() {
    unset_calls <<- unset_calls + 1L
    if (unset_calls > 1L) stop("intentional strategy cleanup failure")
  }
  cleanup_failed <- env$benchmark_with_resources("lasR", 1L, config, "fixture", function() failed)
  stopifnot(cleanup_failed$success == 0L, cleanup_failed$exit_status == 7L,
            cleanup_failed$stdout == "native stdout\n",
            grepl("status 7", cleanup_failed$diagnostic, fixed = TRUE),
            grepl("strategy cleanup failure", cleanup_failed$diagnostic, fixed = TRUE),
            identical(attr(cleanup_failed, "resources_cleaned"), FALSE))
  cleaned <- FALSE
  expect_error(env$benchmark_cleanup(list(
    function() cleaned <<- TRUE, function() stop("cleanup fixture")
  )), "cleanup fixture")
  stopifnot(cleaned)

  # Full dispatch, output accounting, CSV diagnostics, and scratch cleanup with
  # only catalog creation/metadata replaced by tiny stand-ins.
  catalog_class <- methods::setClass(
    "Chat2Catalog", slots = c(data = "data.frame"), where = env
  )
  env$catalog <- function(...) catalog_class(data = data.frame(filename = files))
  env$prepare_input <- function(...) env$catalog()
  env$benchmark_ALS_info <- function(...) list(density_points = 1)
  config$tools <- "lastools"
  runs <- data.frame(ID = "ALS_1000_1_1_SSD_SSD", cores = 1L,
                     dir_in_org = input, scratch_root = scratch)
  tasks <- tibble(
    task_id = "native_fixture", data_type = "datasets_org", output_format = "tif",
    task_call_lastools = list(task_call)
  )
  missing_tasks <- tasks
  missing_tasks$task_call_lastools <- list(missing)
  expect_error(env$benchmark_run(
    "native_fixture", runs$ID, benchmark_tasks = missing_tasks, benchmark_runs = runs,
    sysinfo = data.frame(lastools_version = 1), config = config
  ), "absent-tool")
  stopifnot(!length(list.files(scratch, all.files = TRUE, no.. = TRUE)),
            !length(list.files(reports)))
  env$benchmark_run(
    "native_fixture", runs$ID, benchmark_tasks = tasks, benchmark_runs = runs,
    sysinfo = data.frame(lastools_version = 1), config = config
  )
  result_file <- file.path(reports, "fixture_native_fixture_ALS_1000_1_1_SSD_SSD_1_lastools.csv")
  recorded <- read.csv(result_file)
  stopifnot(recorded$success == 0L, recorded$exit_status == 7L,
            recorded$stderr == "native stderr\n", recorded$stdout == "native stdout\n",
            recorded$output_file_count == 1,
            !length(list.files(scratch, all.files = TRUE, no.. = TRUE)),
            identical(tools::md5sum(files), sentinel))
  success_tasks <- tasks
  success_tasks$task_call_lastools[[1L]]$args[3L] <- "0"
  env$benchmark_run(
    "native_fixture", runs$ID, benchmark_tasks = success_tasks, benchmark_runs = runs,
    sysinfo = data.frame(lastools_version = 1), config = config, force_rerun_lastools = TRUE
  )
  recorded <- read.csv(result_file)
  stopifnot(recorded$success == 1L, recorded$exit_status == 0L,
            recorded$output_file_count == 1, recorded$output_total_size > 0,
            !length(list.files(scratch, all.files = TRUE, no.. = TRUE)),
            identical(tools::md5sum(files), sentinel))
  env$check_results <- function(...) stop("intentional output inspection failure")
  env$benchmark_run(
    "native_fixture", runs$ID, benchmark_tasks = tasks, benchmark_runs = runs,
    sysinfo = data.frame(lastools_version = 1), config = config, force_rerun_lastools = TRUE
  )
  recorded <- read.csv(result_file)
  stopifnot(recorded$success == 0L, recorded$exit_status == 7L,
            grepl("status 7", recorded$diagnostic, fixed = TRUE),
            grepl("output inspection failure", recorded$diagnostic, fixed = TRUE),
            !length(list.files(scratch, all.files = TRUE, no.. = TRUE)),
            identical(tools::md5sum(files), sentinel))
  env$benchmark_with_resources <- function(tool, cores, config, label, run) {
    result <- run()
    attr(result, "resources_cleaned") <- FALSE
    result
  }
  env$benchmark_run(
    "native_fixture", runs$ID, benchmark_tasks = tasks, benchmark_runs = runs,
    sysinfo = data.frame(lastools_version = 1), config = config, force_rerun_lastools = TRUE
  )
  retained <- list.files(scratch, full.names = TRUE)
  recorded <- read.csv(result_file)
  stopifnot(length(retained) == 1L, dir.exists(retained),
            recorded$success == 0L, recorded$exit_status == 7L,
            grepl("Scratch retained", recorded$diagnostic, fixed = TRUE),
            identical(tools::md5sum(files), sentinel))
  env$benchmark_scratch_cleanup(get(retained, env$.benchmark_scratch_owners))
  cat("Chat 2 regression checks passed.\n")
})
