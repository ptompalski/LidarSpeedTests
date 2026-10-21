# Run from the repository root: Rscript --vanilla tests/chat1_regression.R
local({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(forcats)
  library(stringr)
  library(glue)
  library(magrittr)
  stopifnot(requireNamespace("cli", quietly = TRUE))

  repo <- getwd()
  source("R/1_functions.R")
  test_dir <- tempfile("lidarspeedtests-chat1-")
  dir.create(test_dir)
  on.exit({
    setwd(repo)
    unlink(test_dir, recursive = TRUE)
  })

  for (tool in c("lidR", "lasR")) {
    env <- new.env()
    ok <- benchmark_execute_call("answer <- 42; NULL", tool, env)
    failed <- benchmark_execute_call('stop("failure, \\"details\\"\\nsecond line")', tool)
    syntax_error <- benchmark_execute_call("1 +", tool)
    sentinel <- benchmark_execute_call('"ERROR"', tool)
    numeric_result <- benchmark_execute_call("7L", tool)
    warned <- suppressWarnings(benchmark_execute_call('warning("workload warning")', tool))
    stopifnot(
      ok$success == 1L, ok$execution_status == "succeeded", env$answer == 42,
      failed$success == 0L, failed$execution_status == "failed",
      failed$diagnostic == 'failure, "details"\nsecond line',
      syntax_error$success == 0L, nzchar(syntax_error$diagnostic),
      sentinel$success == 1L, numeric_result$success == 1L,
      warned$success == 1L, warned$diagnostic == "workload warning"
    )
  }

  # Exercise real zero/nonzero subprocess statuses without requiring LAStools.
  process_env <- new.env()
  process_env$lastools_path <- R.home("bin")
  process_env$f <- character()
  process_env$dir_out <- test_dir
  process_env$cores <- 1L
  process_call <- list(executable = "Rscript", args = c("--vanilla", "-e", "quit(status=0)"))
  process_ok <- benchmark_execute_call(process_call, "lastools", process_env)
  process_call$args[3L] <- "quit(status=7)"
  process_failed <- benchmark_execute_call(process_call, "lastools", process_env)
  stopifnot(
    process_ok$success == 1L, process_ok$exit_status == 0L,
    process_failed$success == 0L, process_failed$execution_status == "failed",
    process_failed$exit_status == 7L,
    grepl("status 7", process_failed$diagnostic, fixed = TRUE)
  )

  for (tool in c("lidR", "lasR", "lastools")) {
    for (call in list("", " \n\t ", NA_character_, NULL, character())) {
      skipped <- benchmark_execute_call(call, tool)
      stopifnot(
        skipped$success == 0L, skipped$execution_status == "skipped",
        grepl("Unsupported workload", skipped$diagnostic, fixed = TRUE),
        is.na(skipped$start_time), is.na(skipped$finish_time),
        is.na(skipped$output_file_count), is.na(skipped$output_total_size)
      )
    }
  }

  # Run the actual dispatch/CSV-writing path with input metadata stand-ins.
  # Empty calls must return before input preparation, scratch creation, or workers.
  runner_env <- new.env()
  sys.source(file.path(repo, "R/1_functions.R"), runner_env)
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
  runner_env$benchmark_run(
    task_id = "unsupported", run_id = "ALS_1000_1_1_SSD_SSD",
    benchmark_tasks = data.frame(
      task_id = "unsupported", data_type = "datasets_org", output_format = "",
      task_call_lidR = "", task_call_lasR = " \n ", task_call_lastools = NA_character_
    ),
    benchmark_runs = data.frame(
      ID = "ALS_1000_1_1_SSD_SSD", cores = 1,
      dir_in_org = input, scratch_root = scratch
    ),
    sysinfo = tibble(lidR_version = "1.0.0", lasR_version = "1.0.0",
                     lastools_version = 1),
    config = config
  )
  skip_files <- list.files(reports, full.names = TRUE)
  stopifnot(length(skip_files) == 3L)
  for (file in skip_files) {
    record <- read.csv(file)
    stopifnot(record$success == 0L, record$execution_status == "skipped",
              nzchar(record$diagnostic), is.na(record$output_file_count))
  }

  start <- as.POSIXct("2026-10-20 12:00:00", tz = "UTC")
  legacy <- data.frame(
    success = c(0, 1, 0, 1, NA),
    start_time = start,
    finish_time = start + c(120, 120, 0.05, 0.05, NA)
  )
  compiled_legacy <- compile_execution_outcomes(legacy)
  stopifnot(
    identical(compiled_legacy$success, legacy$success),
    identical(compiled_legacy$short_duration, c(FALSE, FALSE, TRUE, TRUE, NA)),
    identical(compiled_legacy$execution_status,
              c("failed", "succeeded", "failed", "succeeded", "unknown")),
    all(is.na(compiled_legacy$diagnostic)), all(is.na(compiled_legacy$exit_status))
  )

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
  stopifnot(length(formatter) == 1L)
  compiler_env$source <- function(file) {
    if (file == "R/0_setup.R") return(invisible(NULL))
    if (file == "R/99_plotSettings.R") return(eval(formatter[[1L]], compiler_env))
    sys.source(file.path(repo, file), compiler_env)
  }
  setwd(test_dir)
  sys.source(file.path(repo, "R/4_compileResults.R"), compiler_env)
  compiled <- readRDS("r.rds")
  compiled <- as.data.frame(compiled)[match(fixture$task_id, compiled$task_id), ]
  # The shared task has two core counts; match its skipped row explicitly.
  shared_skipped <- compiler_env$r %>%
    filter(tool == "lasR", task_id == "shared", cores == 2)
  output_failed <- compiler_env$r %>%
    filter(tool == "lasR", task_id == "shared", cores == 4)
  unsupported <- compiler_env$r %>% filter(task_id == "unsupported")
  stopifnot(
    nrow(compiler_env$r) == nrow(fixture),
    identical(compiled$success[c(1:5, 7:8)], fixture$success[c(1:5, 7:8)]),
    compiled$short_duration[2L], compiled$short_duration[3L],
    !compiled$short_duration[1L],
    compiled$execution_status[1L] == "failed",
    compiled$execution_status[4L] == "unknown",
    nrow(shared_skipped) == 1L, shared_skipped$success == 0,
    shared_skipped$execution_status == "skipped",
    is.na(shared_skipped$short_duration),
    shared_skipped$expected_output_file_count == 3,
    nrow(output_failed) == 1L, output_failed$success == 0,
    output_failed$execution_status == "succeeded",
    nrow(unsupported) == 1L, unsupported$success == 0,
    unsupported$execution_status == "skipped",
    is.na(unsupported$expected_output_file_count),
    compiled$exit_status[7L] == 7L, compiled$exit_status[8L] == 0L,
    identical(compiled$stdout[7L], "123"), identical(compiled$stderr[7L], "456"),
    compiled$diagnostic[7L] == 'process failure, "details"\nsecond line'
  )
  cat("Chat 1 regression checks passed.\n")
})
