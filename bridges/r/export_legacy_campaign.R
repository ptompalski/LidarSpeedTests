# Export a campaign of the legacy R harness as JSON for `lidarbench report`.
#
#   Rscript --vanilla bridges/r/export_legacy_campaign.R --project <repository> \
#       --renv on|off --reports <reports_path> --campaign <campaign_id> --output <new.json>
#
# Read-only. The campaign is read with the harness's own
# benchmark_read_campaign() (R/1_functions.R, sourced, never copied), which
# verifies the manifest identity and every attempt receipt, validation report
# and monitoring report; any mismatch stops the export. Nothing below
# --reports is written. Every record column is exported verbatim (date-times as
# ISO 8601 UTC); task IDs are not mapped onto lidarbench workload contracts.
# The output (lidarbench.legacy-r-campaign/1) is consumed by
# `lidarbench report --historical <file>`. See design/REPORTING.md.
#
# Exit status: 0 exported; 1 the campaign cannot be read or verified, or it
# changed during the export; 2 usage error; 4 renv or package failure.
local({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  script <- gsub("~+~", " ", sub("^--file=", "", file_arg[[1]]), fixed = TRUE)
  source(file.path(dirname(normalizePath(script, winslash = "/")), "bridge.R"), local = TRUE)

  usage <- function(...) {
    message("export: ", ...)
    quit(save = "no", status = 2L)
  }
  options <- tryCatch(
    bridge_parse_args(commandArgs(trailingOnly = TRUE),
                      c("project", "renv", "reports", "campaign", "output")),
    error = function(e) usage(conditionMessage(e))
  )
  use_renv <- tryCatch(bridge_flag(options$renv, "renv"), error = function(e) usage(conditionMessage(e)))
  if (file.exists(options$output)) usage("refusing to overwrite ", options$output)
  project <- normalizePath(options$project, winslash = "/", mustWork = FALSE)
  if (!file.exists(file.path(project, "R", "1_functions.R"))) {
    usage("--project has no R/1_functions.R: ", project)
  }
  if (use_renv) {
    tryCatch(bridge_activate_renv(project), error = function(e) {
      message("export: renv activation failed: ", conditionMessage(e))
      quit(save = "no", status = 4L)
    })
  }
  for (package in c("readr", "dplyr")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      message("export: R package '", package, "' is not available; run renv::restore()")
      quit(save = "no", status = 4L)
    }
  }

  export <- tryCatch({
    harness <- new.env(parent = globalenv())
    sys.source(file.path(project, "R", "1_functions.R"), envir = harness)
    reports <- normalizePath(options$reports, winslash = "/", mustWork = TRUE)
    campaign <- file.path(reports, "campaigns", options$campaign)
    if (dir.exists(file.path(reports, ".benchmark-execution-lock"))) {
      stop("benchmark execution is locked; export only after execution has stopped")
    }
    raw_files <- function() c(
      file.path(campaign, "manifest.rds"),
      list.files(file.path(campaign, "attempts"), full.names = TRUE, recursive = TRUE)
    )
    before <- harness$benchmark_checksums(raw_files())
    records <- harness$benchmark_read_campaign(reports, options$campaign)
    manifest <- readRDS(file.path(campaign, "manifest.rds"))
    records <- as.data.frame(records, stringsAsFactors = FALSE)
    if (nrow(records)) {
      records$started_recorded_at <- vapply(records$attempt_id, function(id) {
        as.numeric(readRDS(file.path(campaign, "attempts", id, "started.rds"))$recorded_at)
      }, numeric(1))
    }
    for (name in names(records)) {
      if (inherits(records[[name]], "POSIXt")) {
        records[[name]] <- format(records[[name]], "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
      } else if (inherits(records[[name]], "Date") || is.factor(records[[name]])) {
        records[[name]] <- as.character(records[[name]])
      } else if (!is.atomic(records[[name]])) {
        records[[name]] <- NULL
      }
    }
    rows <- lapply(seq_len(nrow(records)), function(i) {
      as.list(records[i, , drop = FALSE])
    })
    after <- harness$benchmark_checksums(raw_files())
    if (!identical(before, after)) stop("campaign records changed during the export")
    config <- manifest$identity$config
    list(
      schema = "lidarbench.legacy-r-campaign/1",
      campaign_id = manifest$campaign_id,
      identity_id = manifest$identity_id,
      validation_mode = if (is.null(config$validation_mode)) NULL else
        as.character(config$validation_mode),
      tools = I(as.character(config$tools)),
      exported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"),
      exporter = list(r_version = R.version.string, renv = use_renv,
                      reader = "benchmark_read_campaign (R/1_functions.R)"),
      raw_files = nrow(before),
      records = rows
    )
  }, error = function(e) {
    message("export: ", conditionMessage(e))
    quit(save = "no", status = 1L)
  })

  text <- enc2utf8(paste0(bridge_json(export), "\n"))
  temporary <- paste0(options$output, ".partial")
  connection <- file(temporary, open = "wb")
  writeBin(charToRaw(text), connection)
  close(connection)
  if (!file.link(temporary, options$output)) {
    unlink(temporary)
    usage("refusing to overwrite ", options$output)
  }
  unlink(temporary)
  cat("exported ", length(export$records), " records of campaign ", export$campaign_id,
      " to ", options$output, "\n", sep = "")
})
