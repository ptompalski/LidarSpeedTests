source("R/0_setup.R")

benchmark_config <- benchmark_validate_config(benchmark_load_config())

# `benchmark_combinations` is a data frame consisting of all combinations of tasks_ids and run_ids.
# There maybe a lot of combinations and users may not want to run all of them. 
# Before running the benchmark a subset of the `benchmark_combinations` can be created

#e.g. single task only
B <-  benchmark_combinations %>% filter(task_id == "pixel_metrics_2")

#once checked on a single task, the entire benchmark can be run
# B <- benchmark_combinations


# Validate every selected input and executable before processing any workload.
benchmark_validate_selection(B, benchmark_tasks, benchmark_runs, benchmark_config)
campaign_id <- Sys.getenv("LIDAR_CAMPAIGN_ID")
if (!nzchar(campaign_id)) stop("Set LIDAR_CAMPAIGN_ID explicitly before running.")
campaign <- benchmark_campaign(
  campaign_id, B, benchmark_tasks, benchmark_runs, benchmark_config
)
sysinfo <- as_tibble(system_info())
native <- campaign$manifest$identity$environment$native
if (length(native)) {
  # Use the exact selected executables, not an unrelated las2las installation.
  sysinfo$lastools_version <- paste(vapply(names(native), function(name) {
    paste0(name, ": ", trimws(native[[name]]$version))
  }, character(1)), collapse = "\n")
}


#loop over all benchmark_combinations
for (i in seq_len(nrow(B))) {
  
  #get current row
  b <- B[i,]
  
  benchmark_run(task_id = b$task_id, run_id = b$run_id, 
                benchmark_tasks = benchmark_tasks, benchmark_runs = benchmark_runs,
                sysinfo = sysinfo, config = benchmark_config,
                campaign = campaign, repetition_id = 1L)
  
}
