chat10_fixture <- function(envir = parent.frame(), accept_exceptions = FALSE) {
  f <- chat6_fixture(envir)
  env <- f$env
  f$config$accept_known_surface_discrepancies <- accept_exceptions
  sys.source(file.path(repo, "R/reporting.R"), env)
  f$tasks <- data.frame(
    task_id = c("pixel_metrics_1a", "complex_task1", "normalization"),
    data_type = "datasets_org", output_format = "tif", task_call_lidR = ""
  )
  f$runs <- rbind(f$runs, f$runs)
  f$runs$ID <- c("ALS_1000_1_1_SSD_SSD", "ALS_500_1_1_NET_SSD")
  f$campaign <- fixture_campaign(env, f$tasks, f$runs, f$config, "report-fixture")
  f$append <- function(task = "pixel_metrics_1a", run = f$runs$ID[1L],
                       repetition = 1L, seconds = 2, status = "succeeded",
                       validation = "passed", phase = "measurement", monitor = "available",
                       comparison = "matched_definition_pending_validation") {
    attempt <- env$benchmark_attempt_start(f$campaign, task, run, "lidR", repetition, phase)
    # Synthetic receipts exercise reporting/ingestion, not numerical acceptance.
    validation_path <- file.path(attempt$path, "validation.rds")
    saveRDS(list(fixture = TRUE, validation_status = validation), validation_path)
    start <- as.POSIXct("2026-10-22 12:00:00", tz = "UTC")
    result <- list(
      success = as.integer(status == "succeeded"), execution_status = status,
      diagnostic = if (status == "succeeded") NA_character_ else "synthetic failure/skip",
      start_time = start, finish_time = start + seconds,
      validation_status = validation, validation_diagnostic = "synthetic fixture receipt",
      validation_report_md5 = unname(tools::md5sum(validation_path)),
      monitoring_status = monitor, monitoring_scope = "whole-machine", monitoring_platform = "Linux",
      comparison_status = comparison, comparison_group = paste0("chat5-v1/", task),
      comparison_note = "synthetic reporting fixture, not benchmark evidence",
      workload_contract = "chat5-v1", cores = 1L
    )
    if (monitor %in% c("available", "partial")) {
      epoch <- as.numeric(start)
      samples <- data.frame(
        attempt_id = attempt$record$attempt_id, scope = "whole-machine", platform = "Linux",
        timestamp_units = "seconds since Unix epoch UTC", cpu_units = "percent",
        ram_units = "bytes", sampling_interval_seconds = min(1, seconds),
        interval_start_epoch = epoch + if (monitor == "partial") seconds / 2 else 0,
        interval_end_epoch = epoch + seconds,
        observed_interval_seconds = seconds, cpu_percent = 40,
        ram_used_bytes = 512, ram_available_bytes = 512, ram_total_bytes = 1024
      )
      write.csv(samples, file.path(attempt$path, "monitor-samples.csv"), row.names = FALSE)
      handle <- structure(list(), monitor_path = attempt$path)
      fields <- env$benchmark_monitor_finish(handle, result, attempt$record$attempt_id)
      result[names(fields)] <- fields
    }
    env$benchmark_attempt_finish(attempt, result)
    invisible(attempt)
  }
  f$read <- function() {
    r <- env$benchmark_read_campaign(f$reports, "report-fixture")
    r$report_recorded_at <- vapply(r$attempt_id, function(id) {
      as.numeric(readRDS(file.path(f$campaign$path, "attempts", id, "started.rds"))$recorded_at)
    }, numeric(1))
    env$benchmark_report_prepare(r, f$campaign$manifest)
  }
  f
}

test_that("reporting retains retries and exclusions with deterministic sample statistics", {
  f <- chat10_fixture()
  f$append(phase = "warmup", seconds = 100)
  f$append(seconds = 1, status = "failed", validation = "not_run")
  f$append(seconds = 2)
  f$append(repetition = 2L, seconds = 4)
  f$append(repetition = 3L, seconds = 8)
  f$append(repetition = 3L, status = "failed", validation = "not_run", seconds = 20)
  report <- f$read()
  data <- subset(report$summary, task_id == "pixel_metrics_1a" & run_id == f$runs$ID[1L])
  expect_equal(nrow(report$attempts), 6L)
  expect_equal(data$n_planned, 3)
  expect_equal(data$n_selected, 3)
  expect_equal(data$n_attempts, 5)
  expect_equal(data$n_warmups, 1)
  expect_equal(data$n_failed, 2)
  expect_equal(data$n_included, 2)
  expect_equal(data$n_excluded, 3)
  expect_equal(data$n_missing, 0)
  expect_equal(data$mean_seconds, 3)
  expect_equal(data$median_seconds, 3)
  expect_equal(data$sd_seconds, sqrt(2))
  expect_equal(data$min_seconds, 2)
  expect_equal(data$max_seconds, 4)
  expect_equal(data$n_comparable, 2)
  expect_equal(data$n_monitored, 2)
  expect_equal(data$cpu_mean_percent, 40)
  expect_equal(data$cpu_sd_percent, 0)
  expect_equal(data$ram_mean_used_bytes, 512)
  expect_equal(sum(report$attempts$exclusion == "superseded retry"), 2)
  expect_equal(sum(report$attempts$exclusion == "warmup"), 1)
  expect_identical(report, f$read())
  expect_true(all(report$summary$n_missing[report$summary$task_id == "normalization"] == 3))
  expect_true(all(is.na(report$summary$mean_seconds[report$summary$n_included == 0])))
})

test_that("accepted surface exceptions are descriptive measurements, not validated comparisons", {
  f <- chat10_fixture(accept_exceptions = TRUE)
  f$append(validation = "accepted_exception", seconds = 2)
  report <- f$read()
  row <- subset(report$summary, task_id == "pixel_metrics_1a" & run_id == f$runs$ID[1L])
  expect_equal(row$n_included, 1)
  expect_equal(row$n_accepted_exception, 1)
  expect_equal(row$n_comparable, 0)
  expect_false(report$attempts$validated_success)
  expect_true(report$attempts$measurement_eligible)
  expect_true(f$env$benchmark_campaign_completed(
    f$campaign, "pixel_metrics_1a", f$runs$ID[1L], "lidR", 1L))
})

test_that("short, invalid, unvalidated, partial and non-comparable results stay explicit", {
  f <- chat10_fixture()
  f$append(seconds = .05)
  f$append(repetition = 2L, validation = "failed", monitor = "disabled")
  f$append(repetition = 3L, validation = "unverified", monitor = "disabled")
  f$append(task = "normalization", comparison = "non_comparable", monitor = "partial")
  f$append(task = "complex_task1", status = "skipped", validation = "not_run",
           monitor = "not_run", comparison = "blocked")
  report <- f$read()
  metrics <- subset(report$summary, task_id == "pixel_metrics_1a" & run_id == f$runs$ID[1L])
  expect_equal(metrics$n_included, 0)
  expect_equal(metrics$n_short, 1)
  expect_equal(metrics$n_validation_failed, 1)
  expect_true(is.na(metrics$mean_seconds))
  norm <- subset(report$summary, task_id == "normalization" & run_id == f$runs$ID[1L])
  expect_equal(norm$n_included, 1)
  expect_equal(norm$n_comparable, 0)
  expect_equal(norm$n_partial_monitoring, 1)
  expect_equal(norm$n_monitored, 0)
  expect_true(is.na(norm$cpu_mean_percent))
  expect_true(is.na(norm$sd_seconds))
  expect_identical(report$attempts$success[report$attempts$short_duration], 1)
  expect_match(report$availability$status[report$availability$section == "complex"], "Unavailable")
  expect_equal(sum(report$sections$complex$n_skipped), 1)
  expect_true(all(report$sections$complex$timing_kind == "actual end-to-end workload call"))
  expect_equal(report$availability$n_configurations[report$availability$section == "storage"], 1)
  expect_equal(report$availability$n_configurations[report$availability$section == "tile_size"], 1)
  expect_match(report$availability$status[report$availability$section == "storage"],
               "only one configuration")
  expect_setequal(report$sections$tile_size$tile_size_m, c(500, 1000))
})

test_that("missing dimensions and monitoring never become invented comparisons or zeros", {
  f <- chat10_fixture()
  f$append(monitor = "disabled")
  r <- f$env$benchmark_read_campaign(f$reports, "report-fixture")
  r$run_id <- "custom"
  manifest <- f$campaign$manifest
  manifest$identity$schedule$run_id <- "custom"
  manifest$identity$runs <- manifest$identity$runs[1L, ]
  manifest$identity$runs$ID <- "custom"
  report <- f$env$benchmark_report_prepare(r, manifest)
  expect_equal(nrow(report$sections$storage), 0L)
  expect_equal(nrow(report$sections$tile_size), 0L)
  expect_equal(report$summary$n_monitored[1L], 0)
  expect_true(all(is.na(report$summary$cpu_mean_percent)))
  expect_match(report$availability$status[report$availability$section == "storage"], "Unavailable")
  expect_equal(report$attempts$timing_precision, "CSV timestamps (second resolution)")
  manifest$identity$schedule <- NULL
  manifest$identity$selection$run_id <- "custom"
  old <- f$env$benchmark_report_prepare(r, manifest)
  expect_true(all(is.na(old$summary$n_planned)))
  expect_error(f$env$benchmark_report_read(file.path(f$test_dir, "missing.rds")),
               "Missing campaign report.rds")
  expect_error(f$env$benchmark_report_prepare(r[FALSE, ], manifest), "no completed attempts")
})

test_that("raw immutable campaign fixtures regenerate all intermediates figures tables and site", {
  f <- chat10_fixture()
  f$append()
  f$append(repetition = 2L, seconds = 4)
  f$append(run = f$runs$ID[2L], seconds = 3)
  f$append(task = "complex_task1", status = "skipped", validation = "not_run",
           monitor = "not_run", comparison = "blocked")
  raw <- list.files(f$campaign$path, recursive = TRUE, full.names = TRUE)
  before <- tools::md5sum(raw)
  published <- list.files(file.path(repo, "docs"), recursive = TRUE, full.names = TRUE)
  published_before <- tools::md5sum(published)
  withr::local_envvar(c(LIDAR_BENCHMARK_CONFIG = "/nonexistent/config-must-not-load.R"))
  cli <- processx::run(f$rscript, c("--vanilla", "R/5_report.R"), wd = repo,
    env = c("current", LIDAR_REPORTS_PATH = f$reports,
            LIDAR_CAMPAIGN_ID = "report-fixture", LIDAR_RESULTS_MODE = ""),
    error_on_status = FALSE)
  expect_identical(cli$status, 0L, info = paste(cli$stdout, cli$stderr))
  output <- grep("^Campaign report: ", strsplit(cli$stdout, "\n", fixed = TRUE)[[1L]],
                 value = TRUE)
  expect_length(output, 1L)
  path <- sub("/_site/index.html$", "", sub("^Campaign report: ", "", output))
  expect_identical(tools::md5sum(raw), before)
  expect_identical(tools::md5sum(published), published_before)
  expect_true(all(file.exists(file.path(path, c(
    "report.rds", "compiled.rds", "CPU-RAM.rds", "complex1.rds", "build.rds", "render.log",
    "figures/basic.png", "figures/storage.png", "figures/tile_size.png",
    "figures/utilization.png", "figures/utilization-ram.png",
    "tables/attempts.csv", "tables/availability.csv", "tables/basic.csv",
    "_site/index.html", "_site/results_basic.html", "_site/results_complex.html",
    "_site/results_table.html", "_site/results_table_complex.html",
    "_site/results_storage.html", "_site/results_tileSize.html", "_site/results_CPU-RAM.html",
    "_site/tables/basic.csv", "_site/figures/basic.png"
  )))))
  expect_false(file.exists(file.path(path, "figures/complex.png")))
  complex_html <- paste(readLines(file.path(path, "_site/results_complex.html"), warn = FALSE),
                        collapse = "\n")
  expect_match(complex_html, "Unavailable: no eligible measurements", fixed = TRUE)
  index <- paste(readLines(file.path(path, "_site/index.html"), warn = FALSE), collapse = "\n")
  expect_match(index, "report-fixture", fixed = TRUE)
  expect_match(index, "synthetic failure/skip", fixed = TRUE)
  expect_false(grepl("graphics/result_", index, fixed = TRUE))
  expect_equal(nrow(readRDS(file.path(path, "CPU-RAM.rds"))), 6)
  expect_equal(sum(readRDS(file.path(path, "complex1.rds"))$n_included), 0)
  receipt <- readRDS(file.path(path, "build.rds"))
  expect_identical(receipt$identity_id, f$campaign$manifest$identity_id)
  expect_true(all(nzchar(receipt$sources$md5)))
  expect_identical(receipt$Quarto, "1.10.18")
  expect_true(receipt$rendered)
  second <- f$env$benchmark_build_report(f$reports, "report-fixture", repo, render = FALSE)
  expect_false(identical(path, second))
  expect_identical(readRDS(file.path(path, "report.rds")), readRDS(file.path(second, "report.rds")))
  expect_identical(tools::md5sum(raw), before)
  expect_equal(readRDS(file.path(path, "report.rds"))$availability$n_configurations[
    readRDS(file.path(path, "report.rds"))$availability$section == "storage"], 2)
})

test_that("report generation blocks missing corrupt incomplete and empty campaign records", {
  f <- chat10_fixture()
  env <- f$env
  expect_error(env$benchmark_build_report(f$reports, "report-fixture", repo, render = FALSE),
               "no completed attempts")
  expect_length(list.files(f$campaign$path, pattern = "^report-"), 0)
  attempt <- f$append()
  original <- readBin(file.path(attempt$path, "result.csv"), "raw",
                       n = file.info(file.path(attempt$path, "result.csv"))$size)
  cat("\n", file = file.path(attempt$path, "result.csv"), append = TRUE)
  expect_error(env$benchmark_build_report(f$reports, "report-fixture", repo, render = FALSE),
               "checksum")
  expect_length(list.files(f$campaign$path, pattern = "^report-"), 0)
  expect_error(env$benchmark_build_report(f$reports, "../other", repo, render = FALSE),
               "filename component")
  expect_error(env$benchmark_build_report(f$reports, "missing", repo, render = FALSE),
               "Selected campaign manifest missing")
  writeBin(original, file.path(attempt$path, "result.csv"))
  env$benchmark_attempt_start(f$campaign, "normalization", f$runs$ID[1L], "lidR", 1L)
  expect_error(env$benchmark_build_report(f$reports, "report-fixture", repo, render = FALSE),
               "Incomplete attempt retained")
})

test_that("complex summaries contain measured calls rather than sums of basic timings", {
  f <- chat10_fixture()
  f$append(seconds = 2)
  f$append(task = "complex_task1", seconds = 7, comparison = "non_comparable")
  report <- f$read()
  expect_equal(sum(report$sections$complex$n_included), 1)
  expect_equal(report$sections$complex$mean_seconds[report$sections$complex$n_included > 0], 7)
  expect_equal(sum(report$sections$complex$n_comparable), 0)
})

test_that("report snapshots reject concurrent execution and records added during generation", {
  f <- chat10_fixture()
  f$append()
  lock <- file.path(f$reports, ".benchmark-execution-lock")
  dir.create(lock)
  expect_error(f$env$benchmark_build_report(f$reports, "report-fixture", repo, render = FALSE),
               "execution is locked")
  unlink(lock, recursive = TRUE)
  plot <- f$env$benchmark_report_plot
  appended <- FALSE
  f$env$benchmark_report_plot <- function(...) {
    if (!appended) {
      appended <<- TRUE
      f$append(repetition = 2L)
    }
    plot(...)
  }
  expect_error(f$env$benchmark_build_report(f$reports, "report-fixture", repo, render = FALSE),
               "Campaign records changed during reporting")
  builds <- list.files(f$campaign$path, pattern = "^report-", full.names = TRUE)
  expect_length(builds, 1L)
  expect_false(file.exists(file.path(builds, "build.rds")))
})

test_that("the compiler reads selected campaigns without benchmark or plotting setup", {
  f <- chat10_fixture()
  f$append()
  env <- new.env(parent = globalenv())
  env$source <- function(file) {
    expect_identical(file, "R/1_functions.R")
    sys.source(file.path(repo, file), env)
  }
  withr::local_envvar(c(
    LIDAR_REPORTS_PATH = f$reports, LIDAR_CAMPAIGN_ID = "report-fixture",
    LIDAR_RESULTS_MODE = NA_character_, LIDAR_BENCHMARK_CONFIG = "/invalid/machine.R"
  ))
  sys.source(file.path(repo, "R/4_compileResults.R"), env)
  result <- readRDS(file.path(f$campaign$path, "compiled.rds"))
  expect_equal(result$campaign_id, "report-fixture")
  expect_false(exists("benchmark_config", envir = env, inherits = FALSE))
  expect_false(exists("benchmark_tasks", envir = env, inherits = FALSE))
})
