# compile results

source("R/0_setup.R")
source("R/99_plotSettings.R")
library(magrittr)

# read csv files in the "results" folder

# specifying column types to make everything consistent
csv_spec <- cols(
  ...1 = col_skip(),
  tool = col_character(),
  success = col_double(),
  start_time = col_datetime(format = ""),
  finish_time = col_datetime(format = ""),
  output_file_count = col_double(),
  output_total_size = col_character(),
  task_id = col_character(),
  run_id = col_character(),
  cores = col_double(),
  nodename = col_character(),
  sysname = col_character(),
  release = col_character(),
  cpu = col_character(),
  cpu_threads = col_double(),
  ram = col_double(),
  Rversion = col_character(),
  lidR_version = col_character(),
  lasR_version = col_character(),
  lastools_version = col_double(),
  total_point_count = col_double(),
  density_pulses = col_double(),
  density_points = col_double(),
  file_count = col_double(),
  input_data_size = col_character()
)

f <- list.files(reports_path, full.names = T, pattern = "csv$")

# f can be filtered to include only a subset of the results, e.g.
# f <- f[grepl(f, pattern="816")]
# f <- f[grepl(f, pattern="SSD_SSD")]


#read all csv files - this takes some time, can be probably parallelized with furrr but I was too lazy 
r <- f %>% map(~read_csv(.x, col_types = csv_spec, show_col_types = FALSE)) %>% bind_rows() 




# calculating processing time
r <- r %>% mutate(processing_time = finish_time - start_time) 

#reorder factors (tool names)
r$tool <- factor(r$tool, levels=c("lidR", "lasR", "lastools"))


# round density
round_any <- function (x, accuracy) {
  round(x/accuracy) * accuracy
}

round_density <- function(x) {
  ifelse(x <= 7.5, round_any(x, 1), round_any(x, 5))
}

r <- r %>% mutate(density_points_rnd = round_density(density_points))



# add panel labels (for plotting)
r <- 
  r %>% mutate(density_labels = fct_reorder(
    paste0(round(density_points_rnd), " pts/m²"), density_points
  )
  )

# add data labels
r <- r %>% 
  mutate(total_points_calc = 100 * 1000 * 1000 * density_points) %>%
  mutate(total_points_calc = custom_number_format(total_points_calc))

r <- r %>% mutate(data_labels = paste0(input_data_size, "B; ", total_points_calc, " pts"))

# revise `success` - if the processing time is less than 0.1 sec then change success to 0
r <- r %>% mutate(success = if_else(success==1 & as.numeric(processing_time) < 0.1, 0, 1)) #

r <- r %>% separate(run_id, into=c(NA, "tile_size", NA, NA, "dir1", "dir2"))


# # some initial filtering
# r <- r %>% filter(cores != 12)
# r <- r %>% filter(density_points_rnd != 15)
# r <- r %>% filter(density_points_rnd != 13)
# r <- r %>% filter(density_points_rnd != 30)
# r <- r %>% filter(density_points_rnd != 40)
# 
# r <- r %>% filter(task_id != "pixel_metrics_1")


#keep the newest runs only if the benchmark was re-run 

#set tool versions to NA if another tool was used
r %<>% mutate(lidR_version = if_else(tool == "lidR", lidR_version, NA))
r %<>% mutate(lasR_version = if_else(tool == "lasR", lasR_version, NA))
r %<>% mutate(lastools_version = if_else(tool == "lastools", lastools_version, NA))


lasR_newest_runs <- 
  r %>% group_by(task_id, lasR_version) %>% count() %>%
  group_by(task_id) %>%
  mutate(lasR_version_num = as.numeric(str_remove_all(lasR_version, "\\."))) %>%
  arrange(task_id, desc(lasR_version_num)) %>% 
  mutate(id = row_number()) %>%
  slice_min(id, n = 1) %>%
  select(-n, -id, -lasR_version_num)

lidR_newest_runs <- 
  r %>% group_by(task_id, lidR_version) %>% count() %>%
  group_by(task_id) %>%
  arrange(task_id, desc(lidR_version)) %>% 
  mutate(id = row_number()) %>%
  slice_min(id, n = 1) %>%
  select(-n, -id)

lastools_newest_runs <- 
  r %>% group_by(task_id, lastools_version) %>% count() %>%
  group_by(task_id) %>%
  arrange(task_id, desc(lastools_version)) %>% 
  mutate(id = row_number()) %>%
  slice_min(id, n = 1) %>%
  select(-n, -id)


r_lidR <- r %>% filter(tool == "lidR") %>% semi_join(lidR_newest_runs) 
r_lasR <- r %>% filter(tool == "lasR") %>% semi_join(lasR_newest_runs) 
r_lastools <- r %>% filter(tool == "lastools") %>% semi_join(lastools_newest_runs) 


# #some checks:
# r_lidR %>% filter(tile_size == 1000, dir1=="SSD", dir2=="SSD") %>% group_by(task_id, lidR_version, tile_size , density_points_rnd) %>% count() %>% print(n=Inf)
# r_lidR %>% group_by(task_id, lidR_version) %>% count() %>% print(n=Inf)  
# r_lasR %>% group_by(task_id, lasR_version) %>% count() %>% print(n=Inf)  
# r_lastools %>% group_by(task_id, lastools_version, nodename) %>% count() %>% print(n=Inf)  


#combine back to one df
r <- bind_rows(r_lidR, r_lasR)
r <- bind_rows(r, r_lastools)

# #some checks:
# r1 %>% filter(tile_size == 1000, dir1=="SSD", dir2=="SSD") %>% 
#   filter(task_id=="normalization") %>%
#   group_by(tool, task_id, lidR_version, tile_size , density_points_rnd) %>% count() %>% print(n=Inf)



# #check if any duplicates
r <- r %>% group_by(tool, task_id, tile_size, cores, density_points_rnd, dir1, dir2, nodename) %>%
  slice_max(finish_time)

#identify failed runs
# find out what is the expected output file count for each tool/task
r_expected <- r %>% group_by(tool, task_id) %>% 
  dplyr::summarise(expected_output_file_count = max(output_file_count))

r <- 
  r %>% left_join(r_expected) %>% 
  mutate(success = if_else(output_file_count < expected_output_file_count, 0,success))




#save 
saveRDS(r, file = "r.rds")


# r1 %>% filter(tile_size == 1000, dir1=="SSD", dir2=="SSD") %>% 
#   filter(task_id=="normalization", tool=="lidR", density_points_rnd==50) %>% View
#   
#   group_by(tool, task_id, lidR_version, tile_size , density_points_rnd) %>% count() %>% print(n=Inf)




  



