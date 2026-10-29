# Build a small synthetic campaign of the legacy R harness (test fixture only).
#
#   Rscript --vanilla tests/python/r_legacy_campaign_fixture.R <repository> <new reports dir>
#
# Uses the harness's own campaign functions (R/1_functions.R) and the testthat
# fixture helpers, exactly like tests/testthat/test-reporting.R, then copies the
# reports directory to <new reports dir>. Synthetic receipts exercise the export
# route only; they are not benchmark evidence.
args <- commandArgs(trailingOnly = TRUE)
project <- normalizePath(args[[1]], winslash = "/")
destination <- args[[2]]
setwd(project)
suppressMessages(source("renv/activate.R"))
setwd(file.path(project, "tests", "testthat"))
suppressMessages(library(testthat))
suppressMessages(source("helper-fixtures.R"))

build <- function() {
  f <- campaign_fixture()
  env <- f$env
  f$config$validation_mode <- "numerical"
  f$tasks <- data.frame(task_id = "pixel_metrics_1a", data_type = "datasets_org",
                        output_format = "tif", task_call_lidR = "")
  campaign <- fixture_campaign(env, f$tasks, f$runs, f$config, "legacy-fixture")
  append <- function(repetition, seconds, status = "succeeded", validation = "passed",
                     phase = "measurement") {
    attempt <- env$benchmark_attempt_start(campaign, "pixel_metrics_1a", f$runs$ID[1L], "lidR",
                                           repetition, phase)
    validation_path <- file.path(attempt$path, "validation.rds")
    saveRDS(list(schema = "validation-v1", fixture = TRUE, status = validation,
                 validation_mode = "numerical", structural_checks_passed = FALSE),
            validation_path)
    start <- as.POSIXct("2026-10-22 12:00:00", tz = "UTC")
    env$benchmark_attempt_finish(attempt, list(
      success = as.integer(status == "succeeded"), execution_status = status,
      diagnostic = if (status == "succeeded") NA_character_ else "synthetic failure",
      start_time = start, finish_time = start + seconds,
      validation_status = validation, validation_diagnostic = "synthetic fixture receipt",
      validation_mode = "numerical", structural_validation_passed = FALSE,
      validation_report_md5 = unname(tools::md5sum(validation_path)),
      monitoring_status = "disabled", comparison_status = "matched_definition_pending_validation",
      comparison_group = "workloads-v1/pixel_metrics_1a", boundary_difference_points = 0L,
      workload_contract = "workloads-v1", cores = 1L, lidR_version = "4.3.3"
    ))
    Sys.sleep(0.02)
  }
  append(1L, 100, phase = "warmup")
  append(1L, 1, status = "failed", validation = "not_run")
  append(1L, 2)
  append(2L, 4)
  append(3L, 8, validation = "failed")
  if (!dir.create(destination)) stop("cannot create ", destination)
  invisible(file.copy(file.path(f$reports, "campaigns"), destination, recursive = TRUE))
}
build()
cat("ok\n")
