test_that("R workloads preserve successes, errors, and warning diagnostics", {
  fixture <- benchmark_fixture()
  benchmark_execute_call <- fixture$env$benchmark_execute_call
  for (tool in c("lidR", "lasR")) {
    env <- new.env()
    ok <- benchmark_execute_call("answer <- 42; NULL", tool, env)
    failed <- benchmark_execute_call('stop("failure, \\"details\\"\\nsecond line")', tool)
    syntax_error <- benchmark_execute_call("1 +", tool)
    sentinel <- benchmark_execute_call('"ERROR"', tool)
    numeric_result <- benchmark_execute_call("7L", tool)
    warned <- suppressWarnings(benchmark_execute_call('warning("workload warning")', tool))
    expect_identical(ok$success, 1L)
    expect_identical(ok$execution_status, "succeeded")
    expect_equal(env$answer, 42)
    expect_identical(failed$success, 0L)
    expect_identical(failed$execution_status, "failed")
    expect_identical(failed$diagnostic, 'failure, "details"\nsecond line')
    expect_identical(syntax_error$success, 0L)
    expect_true(nzchar(syntax_error$diagnostic))
    expect_identical(sentinel$success, 1L)
    expect_identical(numeric_result$success, 1L)
    expect_identical(warned$success, 1L)
    expect_identical(warned$diagnostic, "workload warning")
  }
})

test_that("native workloads use real subprocess exit statuses", {
  fixture <- benchmark_fixture()
  benchmark_execute_call <- fixture$env$benchmark_execute_call
  # Exercise real zero/nonzero subprocess statuses without requiring LAStools.
  process_env <- new.env()
  process_env$lastools_path <- R.home("bin")
  process_env$f <- character()
  process_env$dir_out <- fixture$test_dir
  process_env$cores <- 1L
  process_call <- list(executable = "Rscript", args = c("--vanilla", "-e", "quit(status=0)"))
  process_ok <- benchmark_execute_call(process_call, "lastools", process_env)
  process_call$args[3L] <- "quit(status=7)"
  process_failed <- benchmark_execute_call(process_call, "lastools", process_env)
  expect_identical(process_ok$success, 1L)
  expect_identical(process_ok$exit_status, 0L)
  expect_identical(process_failed$success, 0L)
  expect_identical(process_failed$execution_status, "failed")
  expect_identical(process_failed$exit_status, 7L)
  expect_match(process_failed$diagnostic, "status 7", fixed = TRUE)
})

test_that("native zero statuses cannot hide child errors or warning diagnostics", {
  fixture <- benchmark_fixture()
  env <- fixture$env
  context <- list2env(list(lastools_path = R.home("bin"), f = character(),
                           dir_out = fixture$test_dir, cores = 2L))
  call <- list(executable = "Rscript", args = character())
  process <- list(status = 0L, stdout = "", stderr = "")
  env$benchmark_process <- function(...) process
  for (stream in c("stdout", "stderr")) {
    for (error in c("ERROR: directory not found.", "  FATAL error in child process.")) {
      process <- list(status = 0L, stdout = "", stderr = "")
      process[[stream]] <- paste("Starting 2 processes", error, "Sucessfully started 2 jobs", sep = "\n")
      result <- env$benchmark_execute_call(call, "lastools", context)
      expect_identical(result$success, 0L)
      expect_identical(result$execution_status, "failed")
      expect_identical(result$exit_status, 0L)
      expect_identical(result[[stream]], process[[stream]])
      expect_match(result$diagnostic, "despite exit status 0")
      expect_match(result$diagnostic, error, fixed = TRUE)
    }
    process <- list(status = 0L, stdout = "", stderr = "")
    process[[stream]] <- "WARNING: all values are above the height_cutoff."
    warned <- env$benchmark_execute_call(call, "lastools", context)
    expect_identical(warned$execution_status, "succeeded")
    expect_identical(warned$diagnostic, process[[stream]])
  }
  process <- list(status = 0L, stdout = "Input file ERROR_tile.laz\nNo errors found.", stderr = "")
  ok <- env$benchmark_execute_call(call, "lastools", context)
  expect_identical(ok$execution_status, "succeeded")
  expect_true(is.na(ok$diagnostic))
})

test_that("empty workloads are skipped without timing or output measurements", {
  benchmark_execute_call <- benchmark_fixture()$env$benchmark_execute_call
  for (tool in c("lidR", "lasR", "lastools")) {
    for (call in list("", " \n\t ", NA_character_, NULL, character())) {
      skipped <- benchmark_execute_call(call, tool)
      expect_identical(skipped$success, 0L)
      expect_identical(skipped$execution_status, "skipped")
      expect_match(skipped$diagnostic, "Unsupported workload", fixed = TRUE)
      expect_true(is.na(skipped$start_time))
      expect_true(is.na(skipped$finish_time))
      expect_true(is.na(skipped$output_file_count))
      expect_true(is.na(skipped$output_total_size))
    }
  }
})

test_that("empty-call dispatch writes skipped CSVs before preparing inputs or scratch", {
  fixture <- benchmark_fixture()
  test_dir <- fixture$test_dir
  # Run the actual dispatch/CSV-writing path with input metadata stand-ins.
  # Empty calls must return before input preparation, scratch creation, or workers.
  runner_env <- fixture$env
  runner_env$prepare_input <- function(...) stop("Input preparation must not run")
  runner_env$benchmark_scratch_create <- function(...) stop("Scratch creation must not run")
  input <- file.path(test_dir, "input")
  scratch <- file.path(test_dir, "scratch")
  reports <- file.path(test_dir, "skipped")
  for (path in c(input, scratch, reports)) dir.create(path)
  config <- list(
    input_roots = c(SSD = input), scratch_roots = c(SSD = scratch),
    reports_path = reports, cores = 1L, tools = c("lidR", "lasR", "lastools"),
    lastools_path = R.home("bin"), workstation_id = "fixture", resource_monitoring = FALSE
  )
  tasks <- data.frame(
      task_id = "unsupported", data_type = "datasets_org", output_format = "",
      task_call_lidR = "", task_call_lasR = " \n ", task_call_lastools = NA_character_
    )
  runs <- data.frame(
      ID = "ALS_1000_1_1_SSD_SSD", cores = 1,
      dir_in_org = input, scratch_root = scratch
    )
  campaign <- fixture_campaign(runner_env, tasks, runs, config)
  runner_env$benchmark_run(
    task_id = "unsupported", run_id = "ALS_1000_1_1_SSD_SSD",
    benchmark_tasks = tasks, benchmark_runs = runs, campaign = campaign,
    sysinfo = tibble(lidR_version = "1.0.0", lasR_version = "1.0.0",
                     lastools_version = 1),
    config = config
  )
  skip_files <- fixture_attempt_files(reports)
  expect_length(skip_files, 3L)
  for (file in skip_files) {
    record <- read.csv(file)
    expect_equal(record$success, 0L)
    expect_identical(record$execution_status, "skipped")
    expect_true(nzchar(record$diagnostic))
    expect_true(is.na(record$output_file_count))
  }
})

test_that("legacy compilation flags short durations without relabeling failures", {
  compile_execution_outcomes <- benchmark_fixture()$env$compile_execution_outcomes
  start <- as.POSIXct("2026-10-20 12:00:00", tz = "UTC")
  legacy <- data.frame(
    success = c(0, 1, 0, 1, NA),
    start_time = start,
    finish_time = start + c(120, 120, 0.05, 0.05, NA)
  )
  compiled_legacy <- compile_execution_outcomes(legacy)
  expect_identical(compiled_legacy$success, legacy$success)
  expect_identical(compiled_legacy$short_duration, c(FALSE, FALSE, TRUE, TRUE, NA))
  expect_identical(compiled_legacy$execution_status,
                   c("failed", "succeeded", "failed", "succeeded", "unknown"))
  expect_true(all(is.na(compiled_legacy$diagnostic)))
  expect_true(all(is.na(compiled_legacy$exit_status)))
})

test_that("the compiler preserves mixed-schema outcomes, streams, and output failures", {
  fixture_dirs <- benchmark_fixture()
  test_dir <- fixture_dirs$test_dir
  start <- as.POSIXct("2026-10-20 12:00:00", tz = "UTC")
  # Exercise the whole compiler with mixed legacy/new CSVs, bypassing only
  # machine-local/geospatial setup and plotting side effects.
  reports <- file.path(test_dir, "reports")
  dir.create(reports)
  fixture <- data.frame(
    tool = c("lidR", "lidR", "lidR", "lidR", "lasR", "lasR",
             "lastools", "lastools", "lasR", "lastools"),
    task_id = c("failed", "short_ok", "short_failed", "unknown",
                "shared", "shared", "process_failed", "process_ok", "shared", "unsupported"),
    run_id = paste0("ALS_1000_1_", c(1, 1, 1, 1, 1, 2, 1, 1, 4, 1), "_SSD_SSD"),
    cores = c(1, 1, 1, 1, 1, 2, 1, 1, 4, 1),
    success = c(0, 1, 0, NA, 1, 0, 0, 1, 1, 0),
    start_time = start,
    finish_time = start + c(1, 0.05, 0.05, 1, 1, NA, 1, 1, 1, NA),
    output_file_count = c(1, 1, 1, 1, 3, NA, 1, 1, 1, NA),
    output_total_size = "100",
    nodename = "fixture", density_points = 1, input_data_size = "100",
    lidR_version = "1.0.0", lasR_version = "1.0.0", lastools_version = 1
  )
  fixture$lasR_version[c(5, 9)] <- c("0.9.10", "0.10.0")
  for (i in seq_len(nrow(fixture))) {
    record <- fixture[i, ]
    if (i >= 5L) {
      record$execution_status <- c(
        "succeeded", "skipped", "failed", "succeeded", "succeeded", "skipped"
      )[i - 4L]
      record$diagnostic <- c(NA, "Unsupported workload: no lasR call defined.",
                             'process failure, "details"\nsecond line', NA, NA,
                             "Unsupported workload: no lastools call defined.")[i - 4L]
      record$exit_status <- c(NA_integer_, NA_integer_, 7L, 0L, NA_integer_, NA_integer_)[i - 4L]
      record$stdout <- c(NA, NA, "123", "native stdout", NA, NA)[i - 4L]
      record$stderr <- c(NA, NA, "456", "native stderr", NA, NA)[i - 4L]
    }
    write.csv(record, file.path(reports, paste0(i, ".csv")))
  }
  compiler_env <- new.env()
  compiler_env$reports_path <- reports
  plot_expressions <- parse(file.path(repo, "R/99_plotSettings.R"))
  formatter <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      identical(expr[[2L]], as.name("custom_number_format"))
  }, as.list(plot_expressions))
  expect_length(formatter, 1L)
  compiler_env$source <- function(file) {
    if (file == "R/0_setup.R") return(invisible(NULL))
    if (file == "R/99_plotSettings.R") return(eval(formatter[[1L]], compiler_env))
    sys.source(file.path(repo, file), compiler_env)
  }
  sys.source(file.path(repo, "R/1_functions.R"), compiler_env)
  withr::local_envvar(c(LIDAR_CAMPAIGN_ID = NA_character_, LIDAR_RESULTS_MODE = "legacy"))
  withr::local_dir(test_dir)
  sys.source(file.path(repo, "R/4_compileResults.R"), compiler_env)
  compiled <- readRDS("r.rds")
  compiled <- as.data.frame(compiled)[match(fixture$task_id, compiled$task_id), ]
  # The shared task has two core counts; match its skipped row explicitly.
  shared_skipped <- compiler_env$r %>%
    filter(tool == "lasR", task_id == "shared", cores == 2)
  output_failed <- compiler_env$r %>%
    filter(tool == "lasR", task_id == "shared", cores == 4)
  unsupported <- compiler_env$r %>% filter(task_id == "unsupported")
  expect_equal(nrow(compiler_env$r), nrow(fixture))
  expect_setequal(
    compiler_env$r$lasR_version[compiler_env$r$task_id == "shared"],
    c("0.9.10", "0.10.0", "1.0.0")
  )
  expect_identical(compiled$success[c(1:5, 7:8)], fixture$success[c(1:5, 7:8)])
  expect_true(compiled$short_duration[2L])
  expect_true(compiled$short_duration[3L])
  expect_false(compiled$short_duration[1L])
  expect_identical(compiled$execution_status[1L], "failed")
  expect_identical(compiled$execution_status[4L], "unknown")
  expect_equal(nrow(shared_skipped), 1L)
  expect_equal(shared_skipped$success, 0)
  expect_identical(shared_skipped$execution_status, "skipped")
  expect_true(is.na(shared_skipped$short_duration))
  expect_true(is.na(shared_skipped$expected_output_file_count))
  expect_equal(nrow(output_failed), 1L)
  expect_equal(output_failed$success, 1)
  expect_identical(output_failed$execution_status, "succeeded")
  expect_identical(output_failed$validation_status, "unverified")
  expect_false(output_failed$validated_success)
  expect_equal(nrow(unsupported), 1L)
  expect_equal(unsupported$success, 0)
  expect_identical(unsupported$execution_status, "skipped")
  expect_true(is.na(unsupported$expected_output_file_count))
  expect_equal(compiled$exit_status[7L], 7L)
  expect_equal(compiled$exit_status[8L], 0L)
  expect_identical(compiled$stdout[7L], "123")
  expect_identical(compiled$stderr[7L], "456")
  expect_identical(compiled$diagnostic[7L], 'process failure, "details"\nsecond line')
})
