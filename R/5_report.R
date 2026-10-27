# Run from the repository root, independently of benchmark.local.R.
source("renv/activate.R")
source("R/1_functions.R")
source("R/reporting.R")
campaign_id <- Sys.getenv("LIDAR_CAMPAIGN_ID")
if (!nzchar(campaign_id)) stop("Set LIDAR_CAMPAIGN_ID to one explicit campaign.")
if (nzchar(Sys.getenv("LIDAR_RESULTS_MODE"))) stop("Reporting requires campaign mode, not legacy mode.")
mode <- Sys.getenv("LIDAR_REPORT_MODE", "validated")
if (!mode %in% c("validated", "performance-only")) {
  stop("LIDAR_REPORT_MODE must be validated or performance-only.")
}
path <- benchmark_build_report(Sys.getenv("LIDAR_REPORTS_PATH", "results"), campaign_id,
                               mode = mode)
cat("Campaign report: ", path, "/_site/index.html\n", sep = "")
