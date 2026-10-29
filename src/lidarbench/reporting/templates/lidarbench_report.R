# Helpers for the lidarbench report pages (knitr engine).
#
# Pages read only the machine-readable tables written by `lidarbench report`
# (tables/*.csv); nothing here recomputes inclusion, comparability or speedups.
# Base R + knitr; ggplot2 is optional (figures are omitted without it). When the
# legacy R/reporting.R was staged, its benchmark_report_plot() draws the
# mean +/- SD timing figure exactly as in the legacy campaign site.

lb_table <- function(name) {
  path <- file.path("tables", paste0(name, ".csv"))
  if (!file.exists(path)) return(NULL)
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "", check.names = FALSE,
                  encoding = "UTF-8")
}

lb_meta <- function(key) {
  meta <- lb_table("report_meta")
  if (is.null(meta)) return(NULL)
  meta$value[meta$key == key]
}

lb_has_ggplot <- function() requireNamespace("ggplot2", quietly = TRUE)

lb_no_figures <- function() {
  cat("_Figures omitted: the R package ggplot2 is not installed._\n\n")
}

lb_kable <- function(data, columns = NULL, digits = 3) {
  if (is.null(data) || !nrow(data)) {
    cat("_None._\n\n")
    return(invisible(FALSE))
  }
  if (!is.null(columns)) data <- data[intersect(columns, names(data))]
  print(knitr::kable(data, format = "pipe", digits = digits, row.names = FALSE))
  cat("\n\n")
  invisible(TRUE)
}

lb_download <- function(name, text = NULL) {
  cat("[", if (is.null(text)) paste0("Download ", name, ".csv") else text,
      "](tables/", name, ".csv)\n\n", sep = "")
}

lb_banner <- function() {
  mode <- lb_meta("mode")
  campaign <- lb_meta("campaign_id")
  if (length(campaign) && !is.na(campaign)) {
    cat("Campaign **", campaign, "** · identity `", substr(lb_meta("identity_sha256"), 1, 16),
        "…` · report mode **", mode, "**\n\n", sep = "")
  } else {
    cat("Report mode **", mode, "** (no lidarbench campaign selected)\n\n", sep = "")
  }
  if (identical(mode, "performance-only")) {
    cat("::: {.callout-warning}\n**Performance-only report.** Outputs may differ numerically ",
        "or algorithmically. Timings are descriptive; none establishes equivalent results ",
        "or a cross-tool speedup.\n:::\n\n", sep = "")
  }
}

lb_unit_label <- function(data) {
  paste0(data$tool, " · ", data$cpus, " CPU")
}

lb_tier_shapes <- c(validated = 16, accepted_exception = 17, unverified = 1, execution_only = 4)

lb_plot_repetitions <- function(reps, summary) {
  if (is.null(reps) || !nrow(reps)) return(invisible(FALSE))
  if (!lb_has_ggplot()) return(lb_no_figures())
  reps$unit <- lb_unit_label(reps)
  reps$panel <- paste(reps$workload, reps$dataset, sep = " · ")
  summary <- summary[is.finite(summary$median_seconds), , drop = FALSE]
  summary$unit <- lb_unit_label(summary)
  summary$panel <- paste(summary$workload, summary$dataset, sep = " · ")
  plot <- ggplot2::ggplot(reps, ggplot2::aes(x = elapsed_s, y = unit)) +
    ggplot2::geom_point(ggplot2::aes(shape = evidence_tier), alpha = 0.7, size = 2,
                        position = ggplot2::position_jitter(height = 0.15, width = 0, seed = 1)) +
    ggplot2::geom_point(data = summary, ggplot2::aes(x = median_seconds, y = unit),
                        shape = 124, size = 7, colour = "firebrick") +
    ggplot2::scale_shape_manual(values = lb_tier_shapes, drop = TRUE, name = "Evidence") +
    ggplot2::facet_wrap(~panel, scales = "free", ncol = 1) +
    ggplot2::labs(x = "Elapsed time per repetition (s, process boundary)", y = NULL,
                  subtitle = "Each point is one repetition; red bar = median") +
    ggplot2::theme_minimal(base_family = "sans")
  print(plot)
  invisible(TRUE)
}

LB_TIMING_LABEL <- "Process wall time mean (s; outer fresh-process interval)"

lb_relabel_legacy_plot <- function(f) {
  # The legacy figure labels its axis "Workload time mean", which described the
  # in-session R interval; lidarbench times the outer fresh process. Only the
  # label string is replaced; the legacy drawing code is used unchanged.
  text <- deparse(body(f), width.cutoff = 500L)
  text <- gsub("Workload time mean (seconds)", LB_TIMING_LABEL, text, fixed = TRUE)
  body(f) <- parse(text = text)[[1]]
  f
}

lb_plot_mean_sd <- function(summary, file) {
  # Reuses the legacy campaign figure (R/reporting.R) when it was staged.
  data <- summary[is.finite(summary$mean_seconds), , drop = FALSE]
  if (!nrow(data)) return(invisible(FALSE))
  if (!lb_has_ggplot()) return(lb_no_figures())
  if (!file.exists("R/reporting.R")) return(invisible(FALSE))
  env <- new.env()
  sys.source("R/reporting.R", env)
  plot_fn <- lb_relabel_legacy_plot(env$benchmark_report_plot)
  data$task_id <- data$workload
  data$run_id <- paste0(data$dataset, " / ", data$cpus, " CPU")
  dir.create("figures", showWarnings = FALSE)
  path <- file.path("figures", file)
  if (isTRUE(plot_fn(data, "timing", path))) {
    cat("![Mean ± sample SD of the outer fresh-process time (timing_boundary = process) ",
        "of included repetitions; tool-reported inner times are in the tables ",
        "(legacy campaign figure)](", path, ")\n\n", sep = "")
  }
  invisible(TRUE)
}

lb_plot_scaling <- function(scaling) {
  data <- scaling[!is.na(scaling$median_seconds), , drop = FALSE]
  if (!nrow(data)) return(invisible(FALSE))
  if (!lb_has_ggplot()) return(lb_no_figures())
  data$panel <- paste(data$workload, data$dataset, sep = " · ")
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = cpus, y = median_seconds, colour = tool)) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::scale_x_continuous(trans = "log2", breaks = sort(unique(data$cpus))) +
    ggplot2::facet_wrap(~panel, scales = "free_y", ncol = 1) +
    ggplot2::labs(x = "CPUs (threads requested)", y = "Median elapsed time (s)",
                  subtitle = "Within-tool scaling; lines connect CPU counts of one tool") +
    ggplot2::theme_minimal(base_family = "sans")
  print(plot)
  invisible(TRUE)
}

lb_plot_resources <- function(reps) {
  data <- reps[!is.na(reps$monitoring_status) & reps$monitoring_status == "available", ,
               drop = FALSE]
  if (!nrow(data)) {
    cat("_No included repetition has complete resource monitoring._\n\n")
    return(invisible(FALSE))
  }
  if (!lb_has_ggplot()) return(lb_no_figures())
  data$unit <- lb_unit_label(data)
  data$panel <- paste(data$workload, data$dataset, sep = " · ")
  data$peak_rss_gib <- data$tree_peak_rss_bytes / 1024^3
  for (spec in list(c("process_cpu_seconds",
                      "CPU time per repetition (s; max of OS rusage and sampled tree)"),
                    c("peak_rss_gib", "Process-tree peak RSS per repetition (GiB)"))) {
    values <- data[is.finite(data[[spec[1]]]), , drop = FALSE]
    if (!nrow(values)) next
    plot <- ggplot2::ggplot(values, ggplot2::aes(x = .data[[spec[1]]], y = unit)) +
      ggplot2::geom_point(alpha = 0.7, size = 2) +
      ggplot2::facet_wrap(~panel, scales = "free", ncol = 1) +
      ggplot2::labs(x = spec[2], y = NULL) +
      ggplot2::theme_minimal(base_family = "sans")
    print(plot)
  }
  invisible(TRUE)
}
