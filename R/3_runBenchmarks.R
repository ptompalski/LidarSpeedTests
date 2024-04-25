source("R/0_setup.R")
source("R/1_functions.R")

# `benchmark_combinations` is a data frame consisting of all combinations of tasks_ids and run_ids.
# There maybe a lot of combinations and users may not want to run all of them. 
# Before running the benchmark a subset of the `benchmark_combinations` can be created

#e.g. single task only
B <-  benchmark_combinations %>% filter(task_id == "pixel_metrics_1")
B <-  benchmark_combinations %>% filter(task_id == "normalization")

#e.g. exclude datasets with 40 and 50 pts/m2
#B <-  B %>% filter(!grepl("_50_|_40_",run_id))

#exclude runs with 1 or  2 cores
B <- B %>% filter(!grepl("_1_S|_2_S",run_id)) 



#loop over all benchmark_combinations
for (i in 1:nrow(B)) {
  
  #get current row
  b <- B[i,]
  
  benchmark_run(task_id = b$task_id, run_id = b$run_id, 
                run_lidR = T,run_lasR = T, run_lastools = T,
                benchmark_tasks = benchmark_tasks, benchmark_runs = benchmark_runs)
  
}


