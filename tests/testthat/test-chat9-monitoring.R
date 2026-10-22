test_that("monitoring policy is explicit, validated and reserved for whole-machine Linux", {
  f <- chat6_fixture()
  env <- f$env
  expect_identical(f$config$monitoring, list(scope = "whole-machine", interval_seconds = 1))
  expect_error(env$benchmark_monitoring_config(list(scope = "process-tree")), "not implemented")
  expect_error(env$benchmark_monitoring_config(list(other = 1)), "named list")
  expect_error(env$benchmark_monitoring_config(list(scope = NULL)), "whole-machine")
  for (interval in list(NULL, NA, Inf, -1, 0, 0.01, 61, "1", c(1, 2))) {
    expect_error(env$benchmark_monitoring_config(list(interval_seconds = interval)), "finite number")
  }
  if (identical(unname(Sys.info()[["sysname"]]), "Linux")) {
    f$config$resource_monitoring <- TRUE
    validated <- env$benchmark_validate_config(f$config)
    expect_identical(validated$system_monitoring_path, f$reports)
  } else {
    f$config$resource_monitoring <- TRUE
    expect_error(env$benchmark_validate_config(f$config), "monitoring unavailable")
  }
  f$config$resource_monitoring <- FALSE
  campaign <- fixture_campaign(env, f$tasks, f$runs, f$config)
  changed <- f$config
  changed$monitoring$interval_seconds <- 2
  expect_error(env$benchmark_campaign_check(campaign, f$tasks, f$runs, changed), "config")
  env$benchmark_execute_schedule(campaign, f$tasks, f$runs, f$config, list())
  records <- env$benchmark_read_campaign(f$reports, "fixture")
  expect_true(all(records$monitoring_status == "disabled"))
  expect_true(all(records$monitoring_scope == "whole-machine"))
})

test_that("summaries deterministically clip intervals and retain units and incomplete coverage", {
  f <- benchmark_fixture()
  env <- f$env
  samples <- data.frame(
    attempt_id = "attempt-fixture", scope = "whole-machine", platform = "Linux",
    timestamp_units = "seconds since Unix epoch UTC", cpu_units = "percent",
    ram_units = "bytes", sampling_interval_seconds = 1,
    interval_start_epoch = c(100, 101, 102), interval_end_epoch = c(101, 102, 103),
    observed_interval_seconds = 1, cpu_percent = c(10, 50, 100),
    ram_used_bytes = c(100, 300, 900), ram_available_bytes = c(900, 700, 100),
    ram_total_bytes = 1000
  )
  summarize <- function(x = samples, start = 100.5, finish = 102) {
    env$benchmark_monitor_summary(x, start, finish, "attempt-fixture")
  }
  result <- summarize()
  expect_identical(result, summarize())
  expect_identical(result$monitoring_status, "available")
  expect_equal(result$monitoring_sample_count, 2)
  expect_equal(result$monitoring_covered_seconds, 1.5)
  expect_equal(result$monitoring_coverage_fraction, 1)
  expect_equal(result$cpu_mean_percent, (10 * .5 + 50) / 1.5)
  expect_equal(result$cpu_peak_percent, 50)
  expect_equal(result$ram_mean_used_bytes, (100 * .5 + 300) / 1.5)
  expect_equal(result$ram_peak_used_bytes, 300)
  partial <- summarize(start = 99, finish = 102)
  expect_identical(partial$monitoring_status, "partial")
  expect_equal(partial$monitoring_coverage_fraction, 2 / 3)
  expect_error(summarize(start = 90, finish = 99), "do not overlap")
  expect_error(summarize(start = NA), "Invalid")
  expect_error(summarize(finish = 100.5), "Invalid")
  for (field in c("attempt_id", "scope", "platform", "ram_units", "timestamp_units")) {
    invalid <- samples
    invalid[[field]][1] <- "wrong"
    expect_error(summarize(invalid), "Malformed")
  }
  for (field in c("cpu_percent", "ram_used_bytes", "observed_interval_seconds",
                  "sampling_interval_seconds", "interval_end_epoch")) {
    invalid <- samples
    invalid[[field]][1] <- -1
    expect_error(summarize(invalid), "Malformed")
  }
  invalid <- samples
  invalid$cpu_percent[1] <- 101
  expect_error(summarize(invalid), "Malformed")
  invalid <- samples
  invalid$interval_start_epoch[2] <- 100.9
  expect_error(summarize(invalid), "Malformed")
  expect_error(summarize(samples[3:1, ]), "Malformed")
  expect_error(summarize(samples[FALSE, ]), "Malformed")
})

test_that("controlled native success and failure retain attributable immutable samples with no live monitor", {
  skip_if_not(identical(unname(Sys.info()[["sysname"]]), "Linux"))
  f <- chat2_fixture()
  env <- f$env
  f$config$tools <- "lastools"
  f$config$resource_monitoring <- TRUE
  f$config$monitoring$interval_seconds <- 0.05
  catalog_class <- methods::setClass("Chat9Catalog", slots = c(data = "data.frame"), where = env)
  env$catalog <- function(...) catalog_class(data = data.frame(filename = f$files))
  env$prepare_input <- function(...) env$catalog()
  env$benchmark_ALS_info <- function(...) list()
  writeLines(c("Sys.sleep(0.2)", readLines(f$script)), f$script)
  runs <- data.frame(ID = "ALS_1000_1_1_SSD_SSD", cores = 1L,
                     dir_in_org = f$input, scratch_root = f$scratch)
  monitors <- list()
  original_start <- env$benchmark_start_monitor
  env$benchmark_start_monitor <- function(...) {
    monitor <- original_start(...)
    monitors[[length(monitors) + 1L]] <<- monitor
    monitor
  }
  withr::defer(for (monitor in monitors) if (monitor$is_alive()) monitor$kill_tree())
  for (status in c(0L, 7L)) {
    call <- f$task_call
    call$args[3L] <- as.character(status)
    tasks <- tibble(task_id = "native_fixture", data_type = "datasets_org",
                    output_format = "tif", task_call_lastools = list(call))
    id <- paste0("controlled-", status)
    campaign <- fixture_campaign(env, tasks, runs, f$config, id)
    env$benchmark_run("native_fixture", runs$ID, benchmark_tasks = tasks,
                      benchmark_runs = runs, config = f$config, campaign = campaign,
                      sysinfo = list())
    expect_false(tail(monitors, 1L)[[1L]]$is_alive())
    record <- env$benchmark_read_campaign(f$reports, id)
    expect_identical(record$success, as.numeric(status == 0L))
    expect_equal(record$exit_status, status)
    expect_identical(record$monitoring_status, "available")
    expect_gte(record$monitoring_sample_count, 2)
    expect_equal(record$monitoring_coverage_fraction, 1)
    expect_gte(record$cpu_mean_percent, 0)
    expect_lte(record$cpu_peak_percent, 100)
    expect_gt(record$ram_peak_used_bytes, 0)
    expect_identical(record$monitoring_scope, "whole-machine")
    expect_length(list.files(f$scratch, all.files = TRUE, no.. = TRUE), 0L)
    expect_false(dir.exists(file.path(f$reports, ".benchmark-execution-lock")))
    attempt <- file.path(campaign$path, "attempts", record$attempt_id)
    samples_path <- file.path(attempt, "monitor-samples.csv")
    samples <- read.csv(samples_path)
    expect_true(all(samples$attempt_id == record$attempt_id))
    expect_true(all(samples$ram_units == "bytes"))
    report_path <- file.path(attempt, "monitor-report.rds")
    report <- readRDS(report_path)
    expect_identical(report$summary, env$benchmark_monitor_summary(
      samples, report$timed_start_epoch, report$timed_finish_epoch, record$attempt_id
    ))
    expect_equal(record$monitoring_timed_finish_epoch - record$monitoring_timed_start_epoch,
                 report$timed_finish_epoch - report$timed_start_epoch, tolerance = 1e-5)
    original <- readBin(samples_path, "raw", n = file.info(samples_path)$size)
    cat("\n", file = samples_path, append = TRUE)
    expect_error(env$benchmark_read_campaign(f$reports, id), "Monitoring samples/identity")
    writeBin(original, samples_path)
    original <- readBin(report_path, "raw", n = file.info(report_path)$size)
    report$summary$cpu_mean_percent <- -1
    saveRDS(report, report_path)
    expect_error(env$benchmark_read_campaign(f$reports, id), "Monitoring report checksum")
    writeBin(original, report_path)
    expect_no_error(env$benchmark_read_campaign(f$reports, id))
  }
  expect_identical(tools::md5sum(f$files), f$sentinel)
  expect_length(monitors, 2L)
  expect_true(all(vapply(monitors, function(monitor) !monitor$is_alive(), logical(1))))
})

test_that("monitor death and unresponsive shutdown fail explicitly and reap their processes", {
  skip_if_not(identical(unname(Sys.info()[["sysname"]]), "Linux"))
  f <- chat2_fixture()
  env <- f$env
  f$config$resource_monitoring <- TRUE
  f$config$monitoring$interval_seconds <- .05
  path <- file.path(f$test_dir, "dead-monitor")
  dir.create(path)
  monitor <- NULL
  original <- env$benchmark_start_monitor
  env$benchmark_start_monitor <- function(...) {
    monitor <<- original(...)
    monitor
  }
  withr::defer(if (!is.null(monitor) && monitor$is_alive()) monitor$kill_tree())
  result <- env$benchmark_with_resources("lastools", 1L, f$config, "death", function() {
    monitor$kill_tree()
    monitor$wait(5000)
    env$benchmark_execute_call("Sys.sleep(.1)", "lidR")
  }, monitor_path = path)
  expect_identical(result$execution_status, "failed")
  expect_identical(result$monitoring_status, "unavailable")
  expect_identical(attr(result, "resources_cleaned"), FALSE)
  expect_match(result$diagnostic, "cleanup failed")
  expect_false(monitor$is_alive())
  env$benchmark_monitor_collect <- function(path, ...) {
    file.create(file.path(path, "monitor-ready"))
    Sys.sleep(60)
  }
  path <- file.path(f$test_dir, "unresponsive-monitor")
  dir.create(path)
  monitor <- original(f$config, "unresponsive", path)
  expect_error(env$benchmark_stop_monitor(monitor), "did not stop")
  expect_false(monitor$is_alive())
})
