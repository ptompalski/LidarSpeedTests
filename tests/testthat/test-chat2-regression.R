test_that("configuration loading and path validation fail explicitly for unsafe settings", {
  with_chat2_fixture({
    expect_identical(env$benchmark_load_config(config_file), config)
    Sys.unsetenv("LIDAR_BENCHMARK_CONFIG")
    expect_null(env$benchmark_load_config(file.path(test_dir, "missing.R"), FALSE))
    expect_error(env$benchmark_load_config(file.path(test_dir, "missing.R")), "Missing benchmark")
    Sys.setenv(LIDAR_BENCHMARK_CONFIG = file.path(test_dir, "missing.R"))
    expect_error(env$benchmark_load_config(required = FALSE), "Missing benchmark")
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
    withr::local_dir(repo)
    expect_error(env$benchmark_validate_scratch_root(repo, character()), "working directory")
    invalid <- config
    invalid$cores <- c(0, NA_real_)
    expect_error(env$benchmark_validate_config(invalid), "positive integers")
    invalid <- config
    invalid$resource_monitoring <- TRUE
    if (identical(unname(Sys.info()[["sysname"]]), "Linux")) {
      expect_no_error(env$benchmark_validate_config(invalid))
    } else {
      expect_error(env$benchmark_validate_config(invalid), "monitoring unavailable")
    }
    expect_no_error(env$benchmark_validate_run_paths(config, input, scratch))
    expect_error(env$benchmark_validate_run_paths(config, test_dir, scratch), "outside")
  })
})

test_that("directory validation rejects inaccessible directories", {
  skip_on_os("windows")
  with_chat2_fixture({
    restricted <- file.path(test_dir, "restricted directory")
    dir.create(restricted)
    withr::defer(Sys.chmod(restricted, "0700"))
    Sys.chmod(restricted, "0444")
    if (file.access(restricted, 1) == 0) {
      skip("This user can search directories despite restrictive permissions")
    }
    expect_error(env$benchmark_directory(restricted, "Input"), "not searchable")
    Sys.chmod(restricted, "0555")
    if (file.access(restricted, 2) == 0) {
      skip("This user can write directories despite restrictive permissions")
    }
    expect_error(env$benchmark_directory(restricted, "Reports", TRUE), "not writable")
  })
})

test_that("scratch cleanup checks ownership and filesystem identity", {
  with_chat2_fixture({
    owner1 <- env$benchmark_scratch_create(scratch, c(input, reports))
    owner2 <- env$benchmark_scratch_create(scratch, c(input, reports))
    expect_false(identical(owner1$path, owner2$path))
    expect_true(dir.exists(owner1$path))
    expect_true(dir.exists(owner2$path))
    writeLines("output", file.path(owner1$path, "result.tif"))
    for (path in c(scratch, input, reports)) {
      expect_error(env$benchmark_scratch_cleanup(path), "unowned")
    }
    forged <- owner1
    forged$path <- input
    expect_error(env$benchmark_scratch_cleanup(forged), "unowned")
    arbitrary <- file.path(scratch, "pre-existing-user-directory")
    dir.create(arbitrary)
    expect_error(env$benchmark_scratch_cleanup(list(path = arbitrary)), "unowned")
    env$benchmark_scratch_cleanup(owner1)
    expect_false(dir.exists(owner1$path))
    expect_true(dir.exists(owner2$path))
    expect_true(dir.exists(arbitrary))
    env$benchmark_scratch_cleanup(owner2)
    expect_error(env$benchmark_scratch_cleanup(owner2), "unowned")
    owner <- env$benchmark_scratch_create(scratch, c(input, reports))
    original <- paste0(owner$path, "-original")
    expect_true(file.rename(owner$path, original))
    expect_true(dir.create(owner$path))
    expect_error(env$benchmark_scratch_cleanup(owner), "identity changed")
    expect_true(dir.exists(owner$path))
    unlink(owner$path, recursive = TRUE)
    expect_true(file.rename(original, owner$path))
    env$benchmark_scratch_cleanup(owner)
    expect_identical(tools::md5sum(files), sentinel)
  })
})

test_that("scratch cleanup rejects symbolic links and protects their targets", {
  skip_on_os("windows")
  with_chat2_fixture({
    owner <- env$benchmark_scratch_create(scratch, c(input, reports))
    link <- file.path(owner$path, "input-link")
    expect_true(file.symlink(input, link))
    expect_error(env$benchmark_scratch_cleanup(owner), "symbolic link")
    expect_identical(tools::md5sum(files), sentinel)
    unlink(link)
    env$benchmark_scratch_cleanup(owner)
    root_link <- file.path(test_dir, "scratch-alias")
    expect_true(file.symlink(input, root_link))
    expect_error(env$benchmark_scratch_create(root_link, c(input, reports)), "overlaps")
    unlink(root_link)
  })
})

test_that("native processes preserve literal arguments, streams, and failure diagnostics", {
  with_chat2_fixture({
    special <- c("space in argument", "literal;not-a-command", "$literal",
                 "double\"quote", "single'quote", "back`tick", "")
    process <- env$benchmark_process(probe, c("--vanilla", script, "0", observed, special))
    expect_identical(process$status, 0L)
    expect_identical(readRDS(observed), special)
    expect_identical(process$stdout, "native stdout\n")
    expect_identical(process$stderr, "native stderr\n")
    expect_error(env$benchmark_process(file.path(bin, "absent-tool")), "missing or not executable")
    failed <- env$benchmark_execute_call(task_call, "lastools", call_env)
    expect_identical(failed$success, 0L)
    expect_identical(failed$exit_status, 7L)
    expect_identical(failed$stdout, "native stdout\n")
    expect_identical(failed$stderr, "native stderr\n")
    expect_match(failed$diagnostic, "native stderr", fixed = TRUE)
    expect_identical(readRDS(observed), c("-i", files, "-odir", call_env$dir_out, "-cores", "2"))
    failed_missing <- env$benchmark_execute_call(missing, "lastools", call_env)
    expect_identical(failed_missing$success, 0L)
    expect_match(failed_missing$diagnostic, "absent-tool", fixed = TRUE)
    expect_error(env$benchmark_lastools_command("probe -i files", call_env), "not shell commands")
    invalid_call <- task_call
    invalid_call$args <- "{unknown}"
    expect_error(env$benchmark_lastools_command(invalid_call, call_env), "Unknown")
  })
})

test_that("native version queries read stderr and reject failed queries", {
  skip_on_os("windows")
  with_chat2_fixture({
    version_tool <- file.path(bin, "las2las")
    writeLines(c("#!/bin/sh", "printf 'LAStools version 240101\\n' >&2"), version_tool)
    Sys.chmod(version_tool, "0755")
    expect_equal(env$benchmark_lastools_version(bin), 240101)
    writeLines(c("#!/bin/sh", "printf 'version query fixture failed\\n' >&2", "exit 9"),
               version_tool)
    expect_error(env$benchmark_lastools_version(bin), "version query fixture failed")
  })
})

test_that("setup definitions retain the workload matrix and validate selected inputs", {
  with_chat2_fixture({
    setup <- new.env()
    setup$source <- function(file) sys.source(file.path(repo, file), setup)
    setup$library <- function(...) invisible(NULL)
    sys.source(file.path(repo, "R/0_setup.R"), setup)
    expect_equal(nrow(setup$benchmark_tasks), 9L)
    expect_equal(nrow(setup$benchmark_runs), 12L)
    expect_true("ALS_1000_1_1_SSD_SSD" %in% setup$benchmark_runs$ID)
    expect_true(all(setup$benchmark_runs$scratch_root == scratch))
    expect_identical(setup$benchmark_runs$cores, rep(c(1L, 2L), each = 6L))
    expect_equal(sum(vapply(setup$benchmark_tasks$task_call_lastools,
                           env$benchmark_call_is_empty, logical(1))), 4L)
    for (path in unique(setup$benchmark_runs$dir_in_org)) dir.create(path, recursive = TRUE)
    task_ids <- c("pixel_metrics_1a", "pixel_metrics_2", "generate_DEM")
    calls <- setup$benchmark_tasks$task_call_lastools[
      match(task_ids, setup$benchmark_tasks$task_id)
    ]
    expect_identical(vapply(calls, function(call) call$executable, character(1)),
                     c("lascanopy64", "lascanopy64", "blast2dem64"))
    selection <- data.frame(task_id = task_ids,
                            run_id = setup$benchmark_runs$ID[1L])
    expect_no_error(setup$benchmark_validate_selection(
      selection, setup$benchmark_tasks, setup$benchmark_runs, config
    ))
    missing_runs <- setup$benchmark_runs
    missing_runs$dir_in_org[1L] <- file.path(input, "missing dataset")
    expect_error(setup$benchmark_validate_selection(
      selection, setup$benchmark_tasks, missing_runs, config
    ), "existing directory")
  })
})

test_that("monitor startup and stop manage real process lifecycles", {
  skip_if_not(identical(unname(Sys.info()[["sysname"]]), "Linux"))
  with_chat2_fixture({
    monitor_output <- file.path(test_dir, "monitor logs")
    dir.create(monitor_output)
    monitor_config <- config
    monitor_config$resource_monitoring <- TRUE
    started <- env$benchmark_start_monitor(monitor_config, "startup", monitor_output)
    withr::defer(if (started$is_alive()) started$kill_tree())
    expect_true(started$is_alive())
    expect_true(file.exists(file.path(monitor_output, "monitor-ready")))
    env$benchmark_stop_monitor(started)
    expect_false(started$is_alive())
    expect_true(file.exists(file.path(monitor_output, "monitor-samples.csv")))
    expect_error(env$benchmark_start_monitor(monitor_config, "startup", monitor_output),
                 "overwrite")
    error_output <- file.path(test_dir, "monitor error")
    dir.create(error_output)
    env$benchmark_monitor_collect <- function(...) stop("monitor startup fixture failed")
    expect_error(env$benchmark_start_monitor(monitor_config, "startup-error", error_output),
                 "monitor startup fixture failed")
  })
})

test_that("worker, monitor, and strategy cleanup retain failures and restore state", {
  skip_if_not(identical(unname(Sys.info()[["sysname"]]), "Linux"))
  with_chat2_fixture({
    old_plan <- future::plan()
    withr::defer(future::plan(old_plan))
    monitor <- NULL
    withr::defer(if (!is.null(monitor) && monitor$is_alive()) monitor$kill_tree())
    original_start <- env$benchmark_start_monitor
    env$benchmark_start_monitor <- function(config, label, path) {
      monitor <<- original_start(config, label, path)
      expect_true(monitor$is_alive())
      monitor
    }
    future::plan(future::sequential)
    worker <- NULL
    monitored_config <- config
    monitored_config$resource_monitoring <- TRUE
    monitor_output <- file.path(test_dir, "worker monitor")
    dir.create(monitor_output)
    failed_resources <- env$benchmark_with_resources(
      "lidR", I(1L), monitored_config, "fixture", function() {
        pid <- future::value(future::future(Sys.getpid()))
        expect_false(identical(pid, Sys.getpid()))
        worker <<- ps::ps_handle(pid)
        stop("intentional worker failure")
      }, monitor_path = monitor_output
    )
    expect_identical(failed_resources$success, 0L)
    expect_false(monitor$is_alive())
    expect_s3_class(future::plan(), "sequential")
    for (i in seq_len(100L)) {
      if (!ps::ps_is_running(worker)) break
      Sys.sleep(0.02)
    }
    expect_false(ps::ps_is_running(worker))
    env$benchmark_start_monitor <- function(...) stop("intentional startup failure")
    failed_start <- env$benchmark_with_resources(
      "lidR", I(1L), monitored_config, "fixture", function() stop("must not execute")
    )
    expect_identical(failed_start$success, 0L)
    expect_match(failed_start$diagnostic, "startup failure", fixed = TRUE)
    expect_s3_class(future::plan(), "sequential")
    strategy <- FALSE
    env$unset_parallel_strategy <- function() {
      strategy <<- FALSE
      lasR::unset_parallel_strategy()
    }
    env$concurrent_files <- lasR::concurrent_files
    env$set_parallel_strategy <- function(...) {
      strategy <<- TRUE
      lasR::set_parallel_strategy(...)
    }
    failed_lasR <- env$benchmark_with_resources("lasR", 1L, config, "fixture", function() {
      expect_true(strategy)
      stop("intentional lasR failure")
    })
    expect_identical(failed_lasR$success, 0L)
    expect_false(strategy)
    unset_calls <- 0L
    env$unset_parallel_strategy <- function() {
      unset_calls <<- unset_calls + 1L
      if (unset_calls > 1L) stop("intentional strategy cleanup failure")
      lasR::unset_parallel_strategy()
    }
    failed <- env$benchmark_execute_call(task_call, "lastools", call_env)
    cleanup_failed <- env$benchmark_with_resources("lasR", 1L, config, "fixture", function() failed)
    expect_identical(cleanup_failed$success, 0L)
    expect_identical(cleanup_failed$exit_status, 7L)
    expect_identical(cleanup_failed$stdout, "native stdout\n")
    expect_match(cleanup_failed$diagnostic, "status 7", fixed = TRUE)
    expect_match(cleanup_failed$diagnostic, "strategy cleanup failure", fixed = TRUE)
    expect_identical(attr(cleanup_failed, "resources_cleaned"), FALSE)
    cleaned <- FALSE
    expect_error(env$benchmark_cleanup(list(
      function() cleaned <<- TRUE, function() stop("cleanup fixture")
    )), "cleanup fixture")
    expect_true(cleaned)
  })
})

test_that("dispatch records outputs and diagnostics while protecting inputs and scratch", {
  with_chat2_fixture({
    # Replace only geospatial catalog creation and metadata with tiny stand-ins.
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
    expect_length(list.files(scratch, all.files = TRUE, no.. = TRUE), 0L)
    expect_length(list.files(reports), 0L)
    campaign <- fixture_campaign(env, tasks, runs, config)
    env$benchmark_run(
      "native_fixture", runs$ID, benchmark_tasks = tasks, benchmark_runs = runs,
      sysinfo = data.frame(lastools_version = 1), config = config, campaign = campaign
    )
    result_file <- fixture_attempt_files(reports)
    recorded <- read.csv(result_file)
    expect_equal(recorded$success, 0L)
    expect_equal(recorded$exit_status, 7L)
    expect_identical(recorded$stderr, "native stderr\n")
    expect_identical(recorded$stdout, "native stdout\n")
    expect_equal(recorded$output_file_count, 1)
    expect_length(list.files(scratch, all.files = TRUE, no.. = TRUE), 0L)
    expect_identical(tools::md5sum(files), sentinel)
    success_tasks <- tasks
    success_tasks$task_call_lastools[[1L]]$args[3L] <- "0"
    success_campaign <- fixture_campaign(env, success_tasks, runs, config, "success")
    env$benchmark_run(
      "native_fixture", runs$ID, benchmark_tasks = success_tasks, benchmark_runs = runs,
      sysinfo = data.frame(lastools_version = 1), config = config,
      campaign = success_campaign, force_rerun_lastools = TRUE
    )
    result_file <- setdiff(fixture_attempt_files(reports), result_file)
    recorded <- read.csv(result_file)
    expect_equal(recorded$success, 1L)
    expect_equal(recorded$exit_status, 0L)
    expect_equal(recorded$output_file_count, 1)
    expect_gt(recorded$output_total_size, 0)
    expect_length(list.files(scratch, all.files = TRUE, no.. = TRUE), 0L)
    expect_identical(tools::md5sum(files), sentinel)
    env$check_results <- function(...) stop("intentional output inspection failure")
    previous <- fixture_attempt_files(reports)
    env$benchmark_run(
      "native_fixture", runs$ID, benchmark_tasks = tasks, benchmark_runs = runs,
      sysinfo = data.frame(lastools_version = 1), config = config,
      campaign = campaign, force_rerun_lastools = TRUE
    )
    result_file <- setdiff(fixture_attempt_files(reports), previous)
    recorded <- read.csv(result_file)
    expect_equal(recorded$success, 0L)
    expect_equal(recorded$exit_status, 7L)
    expect_match(recorded$diagnostic, "status 7", fixed = TRUE)
    expect_match(recorded$validation_diagnostic, "output inspection failure", fixed = TRUE)
    expect_identical(recorded$validation_status, "failed")
    expect_length(list.files(scratch, all.files = TRUE, no.. = TRUE), 0L)
    expect_identical(tools::md5sum(files), sentinel)
    env$benchmark_with_resources <- function(tool, cores, config, label, run, monitor_path = NULL) {
      result <- run()
      attr(result, "resources_cleaned") <- FALSE
      result
    }
    previous <- fixture_attempt_files(reports)
    env$benchmark_run(
      "native_fixture", runs$ID, benchmark_tasks = tasks, benchmark_runs = runs,
      sysinfo = data.frame(lastools_version = 1), config = config,
      campaign = campaign, force_rerun_lastools = TRUE
    )
    result_file <- setdiff(fixture_attempt_files(reports), previous)
    retained <- list.files(scratch, full.names = TRUE)
    recorded <- read.csv(result_file)
    expect_length(retained, 1L)
    expect_true(dir.exists(retained))
    expect_equal(recorded$success, 0L)
    expect_equal(recorded$exit_status, 7L)
    expect_match(recorded$diagnostic, "Scratch retained", fixed = TRUE)
    expect_identical(tools::md5sum(files), sentinel)
    env$benchmark_scratch_cleanup(get(retained, env$.benchmark_scratch_owners))
  })
})
