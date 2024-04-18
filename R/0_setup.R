# install.packages('lasR', repos = 'https://r-lidar.r-universe.dev')

library(lidR)
library(fs)
library(sf)
library(tidyverse)
library(future)
library(lasR)
library(callr)  
library(glue)
library(benchmarkme)


# PATHS, SETTINGS, TASKS

lastools_path <- "C:/LAStools/bin"

#benchmark results will be saved to:
reports_path <- "results"

# CPU, RAM, HDD etc usage reports will go to:
system_monitoring_path <- "system_monitoring"

#workstation id
workstation_id <- Sys.info()[4]


# settings that determine the datasets to run the functions on, how many cores to use, and 
# which drives to test.
benchmark_settings <- list(
  
  drive_in =  c(
    SSD ="D:/"
    # SSD ="N:/"
    # HDD="G:/", 
    # NET="//vic-fas2/finland/"
    ),
  
  drive_out = c(
    SSD ="D:/"
    # SSD ="N:/"
    # HDD="G:/", 
    # NET="//vic-fas1/projects_d/Tompalski/"
    ),
  
  
  # cores =  c(1, 2, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40) ,
  cores =  c(1, 2, seq(4, parallel::detectCores(), 4 )) ,
  
  #subfolder with original point cloud (i.e. not normalized)
  datasets_org = c(
    ALS_1000_1 = "benchmark_data/1_org/tile_1000_density_1",
    ALS_1000_2 = "benchmark_data/1_org/tile_1000_density_2",
    ALS_1000_5 = "benchmark_data/1_org/tile_1000_density_5",
    ALS_1000_10 = "benchmark_data/1_org/tile_1000_density_10",
    ALS_1000_20 = "benchmark_data/1_org/tile_1000_density_20",
    ALS_1000_30 = "benchmark_data/1_org/tile_1000_density_30",
    ALS_1000_40 = "benchmark_data/1_org/tile_1000_density_40",
    ALS_1000_50 = "benchmark_data/1_org/tile_1000_density_50"
    # ALS2012="prf_harmonized_data/2012_ALS/2_tiled/",
    # ALS2005="prf_harmonized_data/2005_ALS_CGVD28/2_tiled",
    # ALS2018="prf_harmonized_data/2018_SPL_CGVD28/2_tiled_adjusted"
    ), 
  
  #subfolder with normalized point clouds 
  datasets_norm = c(
    ALS_1000_1 = "benchmark_data/2_norm/tile_1000_density_1",
    ALS_1000_2 = "benchmark_data/2_norm/tile_1000_density_2",
    ALS_1000_5 = "benchmark_data/2_norm/tile_1000_density_5",
    ALS_1000_10 = "benchmark_data/2_norm/tile_1000_density_10",
    ALS_1000_20 = "benchmark_data/2_norm/tile_1000_density_20",
    ALS_1000_30 = "benchmark_data/2_norm/tile_1000_density_30",
    ALS_1000_40 = "benchmark_data/2_norm/tile_1000_density_40",
    ALS_1000_50 = "benchmark_data/2_norm/tile_1000_density_50"
    # ALS2012="prf_harmonized_data/2012_ALS/3_tiled_norm/",
    # ALS2005="prf_harmonized_data/2005_ALS_CGVD28/3_tiled_norm",
    # ALS2018="prf_harmonized_data/2018_SPL_CGVD28/3_tiled_norm"
    )
  
)


# tasks to run during the benchmark. function calls for lidR, lasR, and lastools. 
benchmark_tasks <- 
  tribble(
    ~task_id, 
    ~task_call_lidR, 
    ~task_call_lasR, 
    ~task_call_lastools,   
    ~data_type,            #original or normalized
    ~output_format,        #extension (laz or tif)
    
    "normalization", 
    "lidR::normalize_height(las = ctg, algorithm = tin())", 
    "ans = exec(pipeline = normalize() + write_las(fout), on = f, progress=TRUE)", 
    "lasheight -i {f_lastools} -odir {dir_out} -olaz -cores {cores} -replace_z -buffered 20", 
    "datasets_org",
    "laz",
    
    "pixel_metrics_1", 
    "lidR::pixel_metrics(las = ctg, ~max(Z), res = 20)", 
    "ans = exec(pipeline = rasterize(20, 'max', ofile = fout), on = f, progress=TRUE)", 
    "lascanopy -i {f_lastools} -max -odir {dir_out} -otif -cores {cores} -buffered 20", 
    "datasets_norm",
    "tif",
    
    "pixel_metrics_2", 
    "lidR::pixel_metrics(las = ctg, ~stdmetrics_z(Z), res = 20)", 
    "ans = exec(pipeline = rasterize(20, stdmetrics_z(Z), ofile = fout), on = f, progress=TRUE)", 
    "lascanopy -i {f_lastools} -max -avg -std -ske -kur -cov -b 10 20 30 40 50 60 70 80 90 -p 5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 -odir {dir_out} -otif -cores {cores} -buffered 20", 
    "datasets_norm",
    "tif"
  )

#other tasks to add:
# - generate DTM
# - generate CHM (simple max per pixel, based on tin?) #this is somewhat similar to dtm (when tin used) or pixel metrics max - simple rasterization
# lidR::rasterize_terrain




# no need to change anything below

#create dirs if not exist
if(!dir.exists(reports_path)) dir.create(reports_path)
if(!dir.exists(system_monitoring_path)) dir.create(system_monitoring_path)


benchmark_runs_org <- 
  expand_grid(drive_in=benchmark_settings$drive_in, 
              drive_out=benchmark_settings$drive_out, 
              cores=benchmark_settings$cores,
              data_path=benchmark_settings$datasets_org
  ) %>%
  mutate(dir_in_org = paste0(drive_in, data_path),
         dir_out = paste0(drive_out, "TEMP/bench_data_out")) %>%
  mutate(ID = paste(names(data_path),cores, names(drive_in), names(drive_out), sep="_")) %>%
  relocate(ID, dir_in_org, dir_out)

benchmark_runs_norm <- 
  expand_grid(drive_in=benchmark_settings$drive_in, 
              drive_out=benchmark_settings$drive_out, 
              cores=benchmark_settings$cores,
              data_path=benchmark_settings$datasets_norm
  ) %>%
  mutate(dir_in_norm = paste0(drive_in, data_path),
         dir_out = paste0(drive_out, "TEMP/bench_data_out")) %>%
  mutate(ID = paste(names(data_path),cores, names(drive_in), names(drive_out), sep="_")) %>%
  relocate(ID, dir_in_norm, dir_out)

benchmark_runs <- 
  left_join(benchmark_runs_org, 
            select(benchmark_runs_norm, ID, dir_in_norm)
  ) %>% relocate(ID, dir_in_org, dir_in_norm)


benchmark_combinations <- expand_grid(task_id = benchmark_tasks$task_id, run_id = benchmark_runs$ID)


#overwrite the benchmarkme::get_ram() function, which is currently not working on some workstations.
#corrected function below.
get_ram <- function()
{
  os = R.version$os
  ram = suppressWarnings(try(benchmarkme:::system_ram(os), silent = TRUE))
  if (inherits(ram, "try-error") || length(ram) == 0L || any(is.na(ram))) {
    message("\t Unable to detect your RAM. # nocov\n            Please raise an issue at https://github.com/csgillespie/benchmarkme")
    ram = structure(NA, class = "ram")
  }
  else {
    cleaned_ram = suppressWarnings(try(benchmarkme:::clean_ram(ram, os),
                                       silent = TRUE))
    if (inherits(cleaned_ram, "try-error") || length(ram) ==
        0L) {
      message("\t Unable to detect your RAM. # nocov\n            Please raise an issue at https://github.com/csgillespie/benchmarkme")
      ram = structure(NA, class = "ram")
    }
    else {
      ram = structure(cleaned_ram, class = "ram")
    }
  }
  return(ram)
}
