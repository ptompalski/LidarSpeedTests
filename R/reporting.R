# Reporting is deliberately independent of benchmark setup and machine config.
benchmark_report_prepare <- function(records, manifest) {
  if (!nrow(records)) stop("Selected campaign has no completed attempts.")
  r <- compile_execution_outcomes(records)
  defaults <- list(
    workload_contract = NA_character_, comparison_group = NA_character_,
    comparison_status = "unverified", comparison_note = "No comparability metadata.",
    boundary_difference_points = 0L,
    monitoring_scope = NA_character_, monitoring_platform = NA_character_,
    cpu_mean_percent = NA_real_, cpu_peak_percent = NA_real_,
    ram_mean_used_bytes = NA_real_, ram_peak_used_bytes = NA_real_,
    monitoring_coverage_fraction = NA_real_, monitoring_sample_count = NA_integer_,
    monitoring_timed_start_epoch = NA_real_, monitoring_timed_finish_epoch = NA_real_
  )
  for (field in setdiff(names(defaults), names(r))) r[[field]] <- defaults[[field]]
  r$elapsed_seconds <- as.numeric(r$processing_time)
  exact <- is.finite(r$monitoring_timed_start_epoch) &
    is.finite(r$monitoring_timed_finish_epoch)
  r$elapsed_seconds[exact] <- r$monitoring_timed_finish_epoch[exact] -
    r$monitoring_timed_start_epoch[exact]
  r$timing_precision <- ifelse(exact, "monitor-bound subsecond epochs",
                               "CSV timestamps (second resolution)")
  r$short_duration <- r$elapsed_seconds < 0.1
  if (!"report_recorded_at" %in% names(r)) {
    r$report_recorded_at <- as.numeric(r$recorded_at)
  }
  if (any(!is.finite(r$report_recorded_at))) stop("Missing attempt ordering timestamp.")
  r <- dplyr::arrange(r, report_recorded_at, attempt_id)
  keys <- c("task_id", "run_id", "tool", "phase", "repetition_id")
  r <- dplyr::group_by(r, dplyr::across(dplyr::all_of(keys)))
  r <- dplyr::mutate(r, selected_attempt = dplyr::row_number() == dplyr::n())
  r <- dplyr::ungroup(r)
  r$exclusion <- dplyr::case_when(
    r$phase != "measurement" ~ "warmup",
    !r$selected_attempt ~ "superseded retry",
    r$execution_status != "succeeded" | is.na(r$success) | r$success != 1 ~
      paste0("execution: ", r$execution_status),
    !r$validation_status %in% c("passed", "accepted_exception") ~
      paste0("validation: ", r$validation_status),
    !is.finite(r$elapsed_seconds) | r$elapsed_seconds < 0.1 ~
      "invalid or sub-0.1-second timing",
    TRUE ~ "included"
  )
  r$report_included <- r$exclusion == "included"
  r$comparison_eligible <- dplyr::coalesce(r$report_included &
    r$validation_status == "passed" &
    r$boundary_difference_points == 0 &
    r$comparison_status == "matched_definition_pending_validation" &
    !is.na(r$comparison_group), FALSE)
  planned <- manifest$identity$schedule
  if (is.null(planned)) {
    planned <- merge(manifest$identity$selection,
                     data.frame(tool = manifest$identity$config$tools))
    planned$repetition_id <- NA_integer_
    planned$phase <- "measurement"
  }
  planned <- planned[planned$phase == "measurement", , drop = FALSE]
  groups <- unique(planned[c("task_id", "run_id", "tool")])
  groups <- dplyr::arrange(groups, task_id, run_id, tool)
  summarize <- function(i) {
    key <- groups[i, , drop = FALSE]
    take <- r$task_id == key$task_id & r$run_id == key$run_id & r$tool == key$tool
    attempts <- r[take, , drop = FALSE]
    measured <- attempts[attempts$phase == "measurement", , drop = FALSE]
    selected <- measured[measured$selected_attempt, , drop = FALSE]
    good <- selected[selected$report_included, , drop = FALSE]
    plan <- planned[planned$task_id == key$task_id & planned$run_id == key$run_id &
                      planned$tool == key$tool, , drop = FALSE]
    n_planned <- if (all(is.na(plan$repetition_id))) NA_integer_ else
      length(unique(plan$repetition_id))
    stat <- function(x, fun) if (length(x)) fun(x) else NA_real_
    monitored <- good[good$monitoring_status == "available" &
                        !is.na(good$monitoring_scope) &
                        good$monitoring_scope == "whole-machine", , drop = FALSE]
    if (nrow(monitored) &&
        any(!is.finite(as.matrix(monitored[c("cpu_mean_percent", "cpu_peak_percent",
                                             "ram_mean_used_bytes", "ram_peak_used_bytes")])))) {
      stop("Available utilization has missing/nonfinite values.")
    }
    cbind(key, data.frame(
      n_planned = n_planned, n_attempts = nrow(measured),
      n_warmups = sum(attempts$phase == "warmup"), n_selected = nrow(selected),
      n_missing = if (is.na(n_planned)) NA_integer_ else
        sum(!unique(plan$repetition_id) %in% selected$repetition_id),
      n_included = nrow(good), n_excluded = nrow(measured) - nrow(good),
      n_failed = sum(measured$execution_status == "failed"),
      n_skipped = sum(measured$execution_status == "skipped"),
      n_validation_failed = sum(measured$validation_status == "failed"),
      n_accepted_exception = sum(good$validation_status == "accepted_exception"),
      n_short = sum(measured$short_duration, na.rm = TRUE),
      mean_seconds = stat(good$elapsed_seconds, mean),
      sd_seconds = if (nrow(good) > 1L) stats::sd(good$elapsed_seconds) else NA_real_,
      median_seconds = stat(good$elapsed_seconds, stats::median),
      min_seconds = stat(good$elapsed_seconds, min),
      max_seconds = stat(good$elapsed_seconds, max),
      n_comparable = sum(good$comparison_eligible),
      comparison_status = paste(sort(unique(attempts$comparison_status)), collapse = "; "),
      n_monitored = nrow(monitored),
      n_partial_monitoring = sum(good$monitoring_status == "partial"),
      cpu_mean_percent = stat(monitored$cpu_mean_percent, mean),
      cpu_sd_percent = if (nrow(monitored) > 1L) stats::sd(monitored$cpu_mean_percent) else NA_real_,
      cpu_peak_percent = stat(monitored$cpu_peak_percent, max),
      ram_mean_used_bytes = stat(monitored$ram_mean_used_bytes, mean),
      ram_sd_used_bytes = if (nrow(monitored) > 1L) stats::sd(monitored$ram_mean_used_bytes) else NA_real_,
      ram_peak_used_bytes = stat(monitored$ram_peak_used_bytes, max),
      timing_kind = if (startsWith(key$task_id, "complex_"))
        "actual end-to-end workload call" else "independent basic workload call"
    ))
  }
  summary <- dplyr::bind_rows(lapply(seq_len(nrow(groups)), summarize))
  runs <- manifest$identity$runs
  if (anyDuplicated(runs$ID)) stop("Duplicate run metadata.")
  summary$cores <- runs$cores[match(summary$run_id, runs$ID)]
  parts <- strsplit(summary$run_id, "_", fixed = TRUE)
  standard <- vapply(parts, function(x) {
    length(x) == 6L && x[1L] == "ALS" &&
      grepl("^[0-9]+$", x[2L]) && grepl("^[0-9]+([.][0-9]+)?$", x[3L])
  }, logical(1))
  dimension <- function(index) vapply(seq_along(parts), function(i) {
    if (standard[i]) parts[[i]][index] else NA_character_
  }, character(1))
  summary$tile_size_m <- as.numeric(dimension(2L))
  summary$nominal_density <- as.numeric(dimension(3L))
  summary$input_storage <- dimension(5L)
  summary$output_storage <- dimension(6L)
  summary$availability <- ifelse(summary$n_included > 0, "measured",
                                  "Unavailable: no eligible measurements")
  summary$comparison_status[!nzchar(summary$comparison_status)] <- "not run"
  # These are stratified descriptions, not causal speedups across different data.
  basic <- summary[!startsWith(summary$task_id, "complex_"), , drop = FALSE]
  complex <- summary[startsWith(summary$task_id, "complex_"), , drop = FALSE]
  utilization <- summary
  utilization$availability <- ifelse(utilization$n_monitored > 0, "measured",
                                     "Unavailable: no full-coverage eligible monitoring")
  sections <- list(
    basic = basic, complex = complex,
    storage = summary[!is.na(summary$input_storage) & !is.na(summary$output_storage), ],
    tile_size = summary[!is.na(summary$tile_size_m), ], utilization = utilization
  )
  availability <- dplyr::bind_rows(lapply(names(sections), function(section) {
    data <- sections[[section]]
    n <- if (section == "utilization") sum(data$n_monitored) else sum(data$n_included)
    observed <- data[data$n_included > 0, , drop = FALSE]
    variants <- switch(section,
      storage = length(unique(paste(observed$input_storage, observed$output_storage))),
      tile_size = length(unique(observed$tile_size_m)), NA_integer_)
    reason <- if (!n) "Unavailable: no eligible measurements" else
      if (!is.na(variants) && variants < 2L) "Measured; comparison unavailable: only one configuration" else
        "Measured; descriptive only, check contracts and matched inputs before comparisons"
    data.frame(section = section, n_samples = n, n_configurations = variants, status = reason)
  }))
  list(schema = "reporting-v1", manifest = manifest, attempts = r, summary = summary,
       sections = sections, availability = availability)
}

benchmark_report_plot <- function(data, section, path) {
  metric <- if (section == "utilization") "cpu_mean_percent" else "mean_seconds"
  eligible <- is.finite(data[[metric]])
  if (!any(eligible)) return(invisible(FALSE))
  data <- data[eligible, , drop = FALSE]
  data$label <- paste(data$task_id, data$run_id, data$tool, sep = " / ")
  data$label <- factor(data$label, levels = rev(unique(data$label)))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[metric]], y = label)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = if (section == "utilization")
      "Whole-machine CPU mean (%)" else "Workload time mean (seconds)",
      y = NULL, title = paste("Selected campaign:", section),
      subtitle = "Descriptive, not speedups; eligible repetitions only; bars = sample SD") +
    ggplot2::theme_minimal(base_family = "sans")
  sd <- if (section == "utilization") data$cpu_sd_percent else data$sd_seconds
  with_sd <- data[is.finite(sd), , drop = FALSE]
  with_sd$lower <- with_sd[[metric]] - sd[is.finite(sd)]
  with_sd$upper <- with_sd[[metric]] + sd[is.finite(sd)]
  if (nrow(with_sd)) plot <- plot + ggplot2::geom_errorbar(
    data = with_sd, ggplot2::aes(xmin = lower, xmax = upper), orientation = "y", width = .2
  )
  ggplot2::ggsave(path, plot, width = 12, height = max(4, nrow(data) * .28 + 2),
                  limitsize = FALSE, dpi = 120)
  if (section == "utilization") {
    ram <- data
    ram$ram_gib <- ram$ram_mean_used_bytes / 1024^3
    ram$lower <- (ram$ram_mean_used_bytes - ram$ram_sd_used_bytes) / 1024^3
    ram$upper <- (ram$ram_mean_used_bytes + ram$ram_sd_used_bytes) / 1024^3
    ram_plot <- ggplot2::ggplot(ram, ggplot2::aes(x = ram_gib, y = label)) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Whole-machine RAM used mean (GiB)", y = NULL,
                    title = "Selected campaign: RAM", subtitle = "Not process RSS; bars = sample SD") +
      ggplot2::theme_minimal(base_family = "sans")
    if (any(is.finite(ram$lower))) ram_plot <- ram_plot + ggplot2::geom_errorbar(
      data = ram[is.finite(ram$lower), ], ggplot2::aes(xmin = lower, xmax = upper),
      orientation = "y", width = .2
    )
    ggplot2::ggsave(sub("\\.png$", "-ram.png", path), ram_plot, width = 12,
                    height = max(4, nrow(data) * .28 + 2), limitsize = FALSE, dpi = 120)
  }
  invisible(TRUE)
}

benchmark_build_report <- function(reports_path, campaign_id, repo_root = getwd(),
                                   render = TRUE, quarto = Sys.getenv("QUARTO", "quarto")) {
  benchmark_file_component(campaign_id, "campaign_id")
  campaign <- file.path(reports_path, "campaigns", campaign_id)
  if (!file.exists(file.path(campaign, "manifest.rds"))) {
    stop("Selected campaign manifest missing: ", file.path(campaign, "manifest.rds"))
  }
  if (!dir.exists(file.path(campaign, "attempts"))) stop("Selected campaign attempts directory missing.")
  if (dir.exists(file.path(reports_path, ".benchmark-execution-lock"))) {
    stop("Benchmark execution is locked; build reports only after execution has stopped.")
  }
  raw_files <- function() c(
    file.path(campaign, "manifest.rds"),
    list.files(file.path(campaign, "attempts"), full.names = TRUE, recursive = TRUE)
  )
  raw_checksums <- benchmark_checksums(raw_files())
  records <- benchmark_read_campaign(reports_path, campaign_id)
  manifest <- readRDS(file.path(campaign, "manifest.rds"))
  if (nrow(records)) records$report_recorded_at <- vapply(records$attempt_id, function(id) {
    as.numeric(readRDS(file.path(campaign, "attempts", id, "started.rds"))$recorded_at)
  }, numeric(1))
  report <- benchmark_report_prepare(records, manifest)
  # Fresh, reserved build directories cannot reuse stale figures/frozen pages.
  path <- tempfile("report-", tmpdir = campaign)
  if (!dir.create(path)) stop("Cannot reserve report directory: ", path)
  for (dir in c("R", "figures", "tables")) {
    if (!dir.create(file.path(path, dir))) stop("Cannot create report subdirectory: ", dir)
  }
  saveRDS(report, file.path(path, "report.rds"))
  saveRDS(report$attempts, file.path(path, "compiled.rds"))
  saveRDS(report$sections$utilization, file.path(path, "CPU-RAM.rds"))
  saveRDS(report$sections$complex, file.path(path, "complex1.rds"))
  for (section in names(report$sections)) {
    utils::write.csv(report$sections[[section]], file.path(path, "tables", paste0(section, ".csv")),
                     row.names = FALSE, na = "")
    benchmark_report_plot(report$sections[[section]], section,
                          file.path(path, "figures", paste0(section, ".png")))
  }
  utils::write.csv(report$attempts, file.path(path, "tables", "attempts.csv"), row.names = FALSE)
  utils::write.csv(report$availability, file.path(path, "tables", "availability.csv"), row.names = FALSE)
  inputs <- c("_quarto.yml", "styles.css", "R/reporting.R", "index.qmd",
              "results_basic.qmd", "results_table.qmd", "results_complex.qmd",
              "results_table_complex.qmd", "results_storage.qmd", "results_tileSize.qmd",
              "results_CPU-RAM.qmd")
  for (file in inputs) {
    if (!file.copy(file.path(repo_root, file), file.path(path, file))) {
      stop("Cannot stage report source: ", file)
    }
  }
  source_files <- file.path(repo_root, c(inputs, "R/1_functions.R", "R/5_report.R", "renv.lock"))
  receipt <- list(
    schema = "reporting-v1", campaign_id = campaign_id, identity_id = manifest$identity_id,
    created_at = Sys.time(), sources = rbind(raw_checksums, benchmark_checksums(source_files)),
    selection_policy = "latest started.rds timestamp per phase/tool/task/run/repetition; lexical attempt ID breaks ties",
    acceptance_policy = "passed validation or frozen-policy accepted surface exceptions; exceptions are descriptive, never comparable",
    timing_policy = "monitor-bound subsecond epochs where available; otherwise second-resolution CSV timestamps",
    R = R.version.string, session = sessionInfo(), rendered = render, Quarto = NA_character_
  )
  if (render) {
    version <- processx::run(quarto, "--version", error_on_status = FALSE)
    if (version$status != 0L) stop("Cannot identify Quarto: ", version$stderr)
    receipt$Quarto <- trimws(version$stdout)
    result <- processx::run(
      quarto, c("render", "--no-cache"), wd = path, error_on_status = FALSE,
      env = c("current", QUARTO_PROJECT_DIR = "", QUARTO_PROFILE = "",
              R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)),
      echo = FALSE
    )
    writeLines(c(result$stdout, result$stderr), file.path(path, "render.log"))
    if (result$status != 0L) stop("Quarto render failed; see ", file.path(path, "render.log"),
                                 "\n", result$stderr)
    if (!file.exists(file.path(path, "_site", "index.html"))) stop("Quarto produced no site.")
  }
  if (!identical(raw_checksums, benchmark_checksums(raw_files())) ||
      dir.exists(file.path(reports_path, ".benchmark-execution-lock"))) {
    stop("Campaign records changed during reporting; discard this build and rerun after execution stops: ",
         path)
  }
  saveRDS(receipt, file.path(path, "build.rds"))
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

benchmark_report_read <- function(path = "report.rds") {
  if (!file.exists(path)) stop("Missing campaign report.rds. Run R/5_report.R; see README.md.")
  report <- readRDS(path)
  if (!identical(report$schema, "reporting-v1")) stop("Unsupported campaign report schema.")
  report
}

benchmark_report_page <- function(section, figures = TRUE) {
  report <- benchmark_report_read()
  cat("Campaign: **", report$manifest$campaign_id, "**\n\n", sep = "")
  status <- report$availability$status[report$availability$section == section]
  cat(status, "\n\n")
  cat("Times are actual workload calls, never sums of independent tasks. ",
      "Non-comparable tools remain descriptive; no cross-tool speedups are inferred. ",
      "Accepted surface exceptions are counted separately and are not numerically validated comparisons. ",
      "NA variability means fewer than two eligible samples, not zero variability.\n\n", sep = "")
  data <- report$sections[[section]]
  if (nrow(data)) {
    columns <- c("task_id", "run_id", "tool", "cores", "n_planned", "n_attempts",
                 "n_missing", "n_included", "n_excluded", "n_failed", "n_skipped",
                 "n_validation_failed", "n_accepted_exception", "n_short", "mean_seconds", "sd_seconds",
                 "median_seconds", "min_seconds", "max_seconds", "n_comparable",
                 "comparison_status", "availability")
    if (section == "utilization") columns <- c(
      "task_id", "run_id", "tool", "n_included", "n_accepted_exception", "n_monitored", "n_partial_monitoring",
      "cpu_mean_percent", "cpu_sd_percent", "cpu_peak_percent", "ram_mean_used_bytes",
      "ram_sd_used_bytes", "ram_peak_used_bytes", "availability"
    )
    print(knitr::kable(data[columns], digits = 3, format = "pipe"))
    cat("\n\n")
  }
  cat("[Download full summary](tables/", section, ".csv)\n\n", sep = "")
  if (figures) {
    image <- paste0("figures/", section, ".png")
    if (file.exists(image)) cat("![](", image, ")\n\n", sep = "")
    ram <- paste0("figures/", section, "-ram.png")
    if (file.exists(ram)) cat("![](", ram, ")\n\n", sep = "")
  }
}
