source("R/1_functions.R")

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


# Machine-local configuration is optional for reading historical reports.
# The runner requires and validates it before processing.
benchmark_config <- benchmark_load_config(required = FALSE)
reports_path <- if (is.null(benchmark_config)) "results" else benchmark_config$reports_path

benchmark_settings <- list(
  
  #subfolders with the data used in the benchmark
  datasets_org = c(
    ALS_1000_1 = "benchmark_data/1_org/tile_1000_density_1",
    ALS_1000_2 = "benchmark_data/1_org/tile_1000_density_2",
    ALS_1000_5 = "benchmark_data/1_org/tile_1000_density_5",
    ALS_1000_10 = "benchmark_data/1_org/tile_1000_density_10",
    ALS_1000_20 = "benchmark_data/1_org/tile_1000_density_20",
    ALS_1000_50 = "benchmark_data/1_org/tile_1000_density_50"
    
  ), 
  
  #subfolder with normalized point clouds, currently not used
  datasets_norm = c(
    ALS_1000_1 = "currently/not/used"
  )
)


# tasks to run during the benchmark. function calls for lidR, lasR, and lastools. 
# should not require any changes

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
    "exec(pipeline = normalize() + write_las(fout), on = f)", 
    list(executable = "lasheight64", args = c(
      "-i", "{inputs}", "-odir", "{output}", "-olaz", "-cores", "{cores}",
      "-replace_z", "-buffered", "20")),
    "datasets_org",
    "laz",
    
    "pixel_metrics_1a", 
    "Pixel metrics (simple)",
    "lidR::pixel_metrics(las = ctg, ~mean(Z), res = 20)", 
    "exec(pipeline = rasterize(20, 'z_mean', ofile = fout), on = f)",
    list(executable = "lascanopy", args = c(
      "-i", "{inputs}", "-step", "20", "-avg", "-odir", "{output}",
      "-otif", "-cores", "{cores}", "-buffered", "20")),
    "datasets_org",
    "tif",
    
    
    "pixel_metrics_2", 
    "Pixel metrics (complex)",
    "pixel_metrics(las = ctg, ~metrics_multiple(Z), res = 20)", 
    "exec(pipeline = rasterize(20, c('z_max','z_mean','z_sd','z_above2','z_p5','z_p25','z_p50','z_p75','z_p95'), ofile = fout), on = f)", 
    list(executable = "lascanopy", args = c(
      "-i", "{inputs}", "-max", "-avg", "-std", "-cov", "-p",
      "5", "25", "50", "75", "95", "-odir", "{output}", "-otif",
      "-cores", "{cores}", "-buffered", "20")),
    "datasets_org",
    "tif",
    
    "generate_DEM", 
    "Generate DEM",
    "rasterize_terrain(las = ctg, res = 1, algorithm = tin())", 
    "exec(pipeline = dtm(res = 1, ofile = fout), on = f)", 
    list(executable = "blast2dem", args = c(
      "-i", "{inputs}", "-otif", "-odir", "{output}", "-step", "1",
      "-cores", "{cores}")), # no need to buffer the tiles
    "datasets_org",
    "tif",
    
    "generate_DSM1", #rasterizing highest point, no interpolation
    "Generate DSM (rasterize highest)",
    "rasterize_canopy(ctg, res = 1, algorithm = p2r())", 
    "exec(pipeline = chm(1, tin=FALSE, ofile = fout), on = f)", 
    list(executable = "lasgrid64", args = c(
      "-i", "{inputs}", "-highest", "-otif", "-odir", "{output}", "-step", "1",
      "-cores", "{cores}", "-buffered", "20")),
    "datasets_org",
    "tif",
    
    "generate_DSM2", #based on tin surface
    "Generate DSM (with TIN interpolation)",
    "rasterize_canopy(ctg, res = 1, algorithm = dsmtin())", 
    "exec(pipeline = chm(1, tin=TRUE, ofile = fout), on = f)", 
    NULL, # requires lasthin + las2dem combo
    "datasets_org",
    "tif",
    
    "tree_detection",
    "Detect treetops",
    "ttops <- locate_trees(ctg, lmf(ws = 5))",
    "lasR::exec(pipeline = local_maximum(5, ofile = fout), on = f)",
    NULL, # not possible with lastools
    "datasets_org",
    "gpkg",
    
    
    "complex_task1",
    "Complex task 1 (Normalization, DTM, CHM)",
    "", #lidR
    'fout_laz <- paste0(dir_out, "/*.laz") 
    fout_dtm <- paste0(dir_out, "/*_dtm.tif")
    fout_chm <- paste0(dir_out, "/*_chm.tif")
    del = triangulate(filter = keep_ground())
    norm = transform_with(del)
    dtm = rasterize(1, del, ofile = fout_dtm)
    chm = rasterize(1, "max", ofile = fout_chm)
    write = write_las(ofile = fout_laz)
    pipeline = del + norm + write + dtm + chm 
    ans = exec(pipeline, on = f, progress = TRUE)', 
    NULL,
    "datasets_org",
    "",
    
    "complex_task2",
    "Complex task 2 (Normalization, DTM, CHM, pixel_metrics, tree_detection)",
    "", #lidR
    'fout_laz <- paste0(dir_out, "/*.laz") 
    fout_dtm <- paste0(dir_out, "/*_dtm.tif")
    fout_chm <- paste0(dir_out, "/*_chm.tif")
    fout_metrics <- paste0(dir_out, "/*_metrics.tif")
    del = triangulate(filter = keep_ground())
    norm = transform_with(del)
    dtm = rasterize(1, del, ofile = fout_dtm)
    chm = rasterize(1, "max", ofile = fout_chm)
    seed = local_maximum(5)
    metrics = rasterize(20, c("z_max","z_mean","z_sd","z_above2","z_p5","z_p25","z_p50","z_p75","z_p95"),ofile=fout_metrics  )
    write = write_las(ofile = fout_laz)
    pipeline = del + norm + write + dtm + chm + metrics + seed
    ans = exec(pipeline, on = f, progress = TRUE)', 
    NULL,
    "datasets_org",
    ""
    
  )



benchmark_runs <- if (is.null(benchmark_config)) {
  tibble(ID = character(), dir_in_org = character(), dir_in_norm = character(),
         scratch_root = character(), cores = integer())
} else {
  benchmark_make_runs(benchmark_settings, benchmark_config)
}


benchmark_combinations <- expand_grid(task_id = benchmark_tasks$task_id, run_id = benchmark_runs$ID)
