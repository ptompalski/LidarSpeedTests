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
library(magrittr)

clrs <- c("#4575b4", "#41ab5d", "#ffbc42")
names(clrs) <- c("lasR", "lidR", "lastools")


# PATHS, SETTINGS, TASKS
lastools_path <- "C:/LAStools/bin"

#benchmark results will be saved to:
reports_path <- "results"

# CPU, RAM, HDD etc usage reports will go to:
system_monitoring_path <- "system_monitoring"

#workstation id
workstation_id <- Sys.info()[4]


geomSeries <- function(base, max) {
  base^(0:floor(log(max, base)))
}

# settings that determine the datasets to run the functions on, how many cores to use, and 
# which drives to test.
benchmark_settings <- list(
  
  drive_in =  c(
    # SSD ="D:/"
    SSD ="N:/",
    HDD="G:/",
    NET="//vic-fas1/projects_d/Tompalski/"
    ),
  
  drive_out = c(
    # SSD ="D:/"
    SSD ="N:/",
    HDD="G:/",
    NET="//vic-fas1/projects_d/Tompalski/"
    ),
  
  
  # cores =  c(1, 2, 4, 8, 16, 32, 64, 128) ,
  cores =  geomSeries(2, parallel::detectCores()) ,
  # cores =  c(1, 2, 4, seq(8, parallel::detectCores(), 8 )),
  # cores =  c(1, 2, seq(4, parallel::detectCores(), 4 )) ,
  
  #subfolder with original point cloud (i.e. not normalized)
  datasets_org = c(
    ALS_1000_1 = "benchmark_data2/1_org2/tile_1000_density_1",
    ALS_1000_2 = "benchmark_data2/1_org2/tile_1000_density_2",
    ALS_1000_5 = "benchmark_data2/1_org2/tile_1000_density_5",
    ALS_1000_10 = "benchmark_data2/1_org2/tile_1000_density_10",
    ALS_1000_20 = "benchmark_data2/1_org2/tile_1000_density_20"
    # ALS_1000_50 = "benchmark_data2/1_org2/tile_1000_density_50"
    
    ), 
  
  #subfolder with normalized point clouds, currently not used
  datasets_norm = c(
    )
)


# tasks to run during the benchmark. function calls for lidR, lasR, and lastools. 
benchmark_tasks <- 
  tribble(
    ~task_id, 
    ~task_plot_title, #Task name that will be used as a plot title
    ~task_call_lidR, 
    ~task_call_lasR, 
    ~task_call_lastools,   
    ~data_type,            #original or normalized
    ~output_format,        #extension (laz or tif)
    
    "normalization", 
    "Normalization",
    "lidR::normalize_height(las = ctg, algorithm = tin())", 
    "ans = exec(pipeline = normalize() + write_las(fout), on = f, progress=TRUE)", 
    "lasheight64 -i {f_lastools} -odir {dir_out} -olaz -cores {cores} -replace_z -buffered 20", 
    "datasets_org",
    "laz",
    
    "pixel_metrics_1a", 
    "Pixel metrics (simple)",
    "lidR::pixel_metrics(las = ctg, ~mean(Z), res = 20)", 
    "ans = exec(pipeline = rasterize(20, 'z_mean', ofile = fout), on = f, progress=TRUE)",
    "lascanopy -i {f_lastools} -step 20 -avg -odir {dir_out} -otif -cores {cores} -buffered 20", 
    "datasets_org",
    "tif",
    
    
    "pixel_metrics_2", 
    "Pixel metrics (complex)",
    "pixel_metrics(las = ctg, ~metrics_multiple(Z), res = 20)", 
    "exec(pipeline = rasterize(20, c('z_max','z_mean','z_sd','z_above2','z_p5','z_p25','z_p50','z_p75','z_p95'), ofile = fout), on = f, progress=TRUE)", 
    "lascanopy -i {f_lastools} -max -avg -std -cov -p 5 25 50 75 95 -odir {dir_out} -otif -cores {cores} -buffered 20", 
    "datasets_org",
    "tif",
    
    "generate_DEM", 
    "Generate DEM",
    "rasterize_terrain(las = ctg, res = 1, algorithm = tin())", 
    "exec(pipeline = dtm(res = 1, ofile = fout), on = f, progress=TRUE)", 
    "blast2dem -i {f_lastools} -otif -odir {dir_out} -step 1 -cores {cores}", #no need to buffer the tiles
    "datasets_org",
    "tif",
    
    "generate_DSM1", #rasterizing highest point, no interpolation
    "Generate DSM (rasterize highest)",
    "rasterize_canopy(ctg, res = 1, algorithm = p2r())", 
    "exec(pipeline = chm(1, tin=FALSE, ofile = fout), on = f, progress=TRUE)", 
    "lasgrid64 -i {f_lastools} -highest -otif -odir {dir_out} -step 1 -cores {cores} -buffered 20", 
    "datasets_org",
    "tif",
    
    "generate_DSM2", #based on tin surface
    "Generate DSM (with TIN interpolation)",
    "rasterize_canopy(ctg, res = 1, algorithm = dsmtin())", 
    "exec(pipeline = chm(1, tin=TRUE, ofile = fout), on = f, progress=TRUE)", 
    "", #requires lasthin + las2dem combo
    "datasets_org",
    "tif",
    
    "tree_detection",
    "Detect treetops",
    "ttops <- locate_trees(ctg, lmf(ws = 5))",
    "exec(pipeline = local_maximum(5), on = f, progress=TRUE)",
    "", #not possible with lastools
    "datasets_org",
    ""
    
    
    
    
  )




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
