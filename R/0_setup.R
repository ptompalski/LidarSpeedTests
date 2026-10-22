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


# Chat 5 contracts and remaining equivalence limitations: WORKLOADS.md.

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
    "lidR::normalize_height(las = ctg, algorithm = tin(), use_class = c(2L, 9L))",
    "exec(pipeline = normalize() + write_las(fout), on = f, with = list(buffer = 20, chunk = 0, progress = FALSE))",
    list(executable = "lasheight64", args = c(
      "-i", "{inputs}", "-odir", "{output}", "-olaz", "-cores", "{cores}",
      "-replace_z", "-buffered", "20")),
    "datasets_org",
    "laz",
    
    "pixel_metrics_1a", 
    "Pixel metrics (simple)",
    "lidR::pixel_metrics(las = ctg, ~list(z_mean = mean(Z)), res = 20, start = c(0, 0))",
    "exec(pipeline = rasterize(20, 'z_mean', ofile = fout), on = f, with = list(buffer = 20, chunk = 0, progress = FALSE))",
    list(executable = "lascanopy64", args = c(
      "-i", "{inputs}", "-step", "20", "-avg", "-odir", "{output}",
      "-otif", "-cores", "{cores}", "-buffered", "20", "-use_orig_bb",
      "-height_cutoff", "{height_cutoff}", "-grid_ll", "0", "0",
      "-no_data", "-9999", "-no_kml")),
    "datasets_org",
    "tif",
    
    
    "pixel_metrics_2", 
    "Pixel metrics (complex)",
    "lidR::pixel_metrics(las = ctg, ~metrics_multiple(Z), res = 20, start = c(0, 0))",
    "exec(pipeline = rasterize(20, c('z_max','z_mean','z_sd','z_above2','z_p5','z_p25','z_p50','z_p75','z_p95'), ofile = fout), on = f, with = list(buffer = 20, chunk = 0, progress = FALSE))",
    list(executable = "lascanopy64", args = c(
      "-i", "{inputs}", "-step", "20", "-max", "-avg", "-std", "-dns", "-p",
      "5", "25", "50", "75", "95", "-odir", "{output}", "-otif",
      "-cores", "{cores}", "-buffered", "20", "-use_orig_bb",
      "-height_cutoff", "{height_cutoff}", "-cover_cutoff", "2", "-fractions",
      "-grid_ll", "0", "0", "-no_data", "-9999", "-no_kml")),
    "datasets_org",
    "tif",
    
    "generate_DEM", 
    "Generate DEM",
    "lidR::rasterize_terrain(las = ctg, res = 1, algorithm = tin(), use_class = c(2L, 9L), shape = 'convex')",
    "exec(pipeline = dtm(res = 1, ofile = fout), on = f, with = list(buffer = 20, chunk = 0, progress = FALSE))",
    list(executable = "blast2dem64", args = c(
      "-i", "{inputs}", "-otif", "-odir", "{output}", "-step", "1",
      "-cores", "{cores}", "-keep_class", "2", "9", "-elevation",
      "-kill", "100", "-buffered", "20", "-use_orig_bb",
      "-no_data", "-9999", "-no_kml")),
    "datasets_org",
    "tif",
    
    "generate_DSM1", #rasterizing highest point, no interpolation
    "Generate DSM (rasterize highest)",
    "lidR::rasterize_canopy(ctg, res = 1, algorithm = p2r(subcircle = 0, na.fill = NULL))",
    "exec(pipeline = dsm(1, tin = FALSE, ofile = fout), on = f, with = list(buffer = 20, chunk = 0, progress = FALSE))",
    list(executable = "lasgrid64", args = c(
      "-i", "{inputs}", "-highest", "-otif", "-odir", "{output}", "-step", "1",
      "-cores", "{cores}", "-buffered", "20", "-use_orig_bb",
      "-no_data", "-9999", "-no_kml")),
    "datasets_org",
    "tif",
    
    "generate_DSM2", #based on tin surface
    "Generate DSM (with TIN interpolation)",
    "lidR::rasterize_canopy(ctg, res = 1, algorithm = dsmtin(max_edge = 0, highest = FALSE))",
    "exec(pipeline = dsm(1, tin = TRUE, ofile = fout), on = f, with = list(buffer = 20, chunk = 0, progress = FALSE))",
    NULL, # requires lasthin + las2dem combo
    "datasets_org",
    "tif",
    
    "tree_detection",
    "Detect treetops",
    "lidR::locate_trees(ctg, lmf(ws = 5, hmin = 2, shape = 'circular'))",
    "lasR::exec(pipeline = local_maximum(5, min_height = 2, ofile = fout), on = f, with = list(buffer = 20, chunk = 0, progress = FALSE))",
    NULL, # not possible with lastools
    "datasets_org",
    "gpkg",
    
    
    "complex_task1",
    "Complex task 1 (Normalization, DTM, CHM)",
    "", #lidR
    'fout_laz <- paste0(dir_out, "/*.laz") 
    fout_dtm <- paste0(dir_out, "/*_dtm.tif")
    fout_chm <- paste0(dir_out, "/*_chm.tif")
    del = triangulate(filter = keep_ground_and_water())
    norm = transform_with(del)
    dtm = rasterize(1, del, ofile = fout_dtm)
    chm = rasterize(1, "max", ofile = fout_chm)
    write = write_las(ofile = fout_laz)
    pipeline = del + dtm + norm + write + chm
    ans = exec(pipeline, on = f, with = list(buffer = 20, chunk = 0, progress = FALSE))',
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
    fout_trees <- paste0(dir_out, "/*_trees.gpkg")
    del = triangulate(filter = keep_ground_and_water())
    norm = transform_with(del)
    dtm = rasterize(1, del, ofile = fout_dtm)
    chm = rasterize(1, "max", ofile = fout_chm)
    seed = local_maximum(5, min_height = 2, ofile = fout_trees)
    metrics = rasterize(20, c("z_max","z_mean","z_sd","z_above2","z_p5","z_p25","z_p50","z_p75","z_p95"),ofile=fout_metrics  )
    write = write_las(ofile = fout_laz)
    pipeline = del + dtm + norm + write + chm + metrics + seed
    ans = exec(pipeline, on = f, with = list(buffer = 20, chunk = 0, progress = FALSE))',
    NULL,
    "datasets_org",
    ""
    
  )

benchmark_tasks$workload_contract <- "chat5-v1"
blocked_lasR <- benchmark_tasks$task_id %in%
  c("normalization", "complex_task1", "complex_task2")
benchmark_tasks$task_candidate_lasR <- ifelse(
  blocked_lasR, benchmark_tasks$task_call_lasR, NA_character_
)
benchmark_tasks$task_call_lasR[blocked_lasR] <- ""
benchmark_contracts <- purrr::map_dfr(benchmark_tasks$task_id, function(task_id) {
  purrr::map_dfr(c("lidR", "lasR", "lastools"), function(tool) {
    contract <- benchmark_workload_contract(task_id, tool)
    tibble(task_id = task_id, tool = tool,
           workload_contract = contract$workload_contract,
           supported = contract$supported,
           comparison_status = contract$comparison_status,
           comparison_group = contract$comparison_group,
           comparison_note = contract$comparison_note,
           input_z = contract$z, population = contract$population,
           buffer = contract$buffer, origin = list(contract$origin),
           resolution = contract$resolution, bands = list(contract$bands),
           artifacts = list(contract$artifacts))
  })
})

benchmark_runs <- if (is.null(benchmark_config)) {
  tibble(ID = character(), dir_in_org = character(), dir_in_norm = character(),
         scratch_root = character(), cores = integer())
} else {
  benchmark_make_runs(benchmark_settings, benchmark_config)
}


benchmark_combinations <- expand_grid(task_id = benchmark_tasks$task_id, run_id = benchmark_runs$ID)
