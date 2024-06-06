# NEED TO UPDATE TO DOWNLOAD THE PETAWAWA DATA


# # script to download the data and then preprocess it to different point densities
# # note that it can take a awhile to download and then process all the data.
# # total size of the finished dataset (original+normalize, in several variants of point density)
# # is about 0.5TB
# 
# source("R/0_setup.R")
# library(curl) #fast(er) data download
# 
# #where to download the data and store the different point cloud density versions of the data?
# data_path <- "N:/benchmark_data/"
# 
# #download ALS data  ####----------------------------------------
# #(100 tiles, Ontario, 2023, single photon lidar, very dense, >50 pts/m2)
# 
# #a vector of 100 tiles with download links
# ALS_tiles <- st_read("ALS_tiles.gpkg")
# 
# # path to store the downloaded files
# ALS_path_out <- glue::glue("{data_path}1_org/raw")
# dir.create(ALS_path_out)
# 
# #download data
# urls <- ALS_tiles$Download_LAZ
# multi_download(urls = urls, destfiles = glue("{ALS_path_out}/{basename(urls)}"), resume = T)
# 
# # The downloaded SPL data is a cloud optimized point cloud (COPC). 
# # COPC are 1.4 las format files that do not require lax files for fast processing (lidR informs about this with a warning)
# # At the same time, COPC files are not that common. To make the benchmarking more
# # representative of datasets that are commonly encountered the COPC files are 
# # converted to las 1.2 format, with all VLRs removed. Lax files are then generated.
# 
# 
# # concert to 1.2
# ALS_path_out_cor <- glue::glue("{data_path}1_org/raw_corrected")
# dir.create(ALS_path_out_cor)
# system(
#   glue("{lastools_path}/las2las -set_version 1.2 -remove_all_vlrs -set_point_type 3 -set_point_size 34  -i {ALS_path_out}/*.laz -odir {ALS_path_out_cor} -olaz -cores 12")
# )
# 
# 
# # # run lassort
# # ALS_path_out_cor_sort <- glue::glue("{data_path}1_org/raw_corrected_sort")
# # dir.create(ALS_path_out_cor_sort)
# # system(
# #   glue("{lastools_path}/lassort64 -i {ALS_path_out_cor}/*.laz -odir {ALS_path_out_cor_sort} -point_source -gps_time -olaz -cores 12")
# # )
# # 
# # 
# # ### add lax index 
# # system(glue("{lastools_path}/lasindex -i {ALS_path_out_cor}/*.laz -cores 8"))
# # 
# 
# 
# 
# 
# 
# # create a catalog
# ctg <- catalog(ALS_path_out_cor)
# ctg
# plot(ctg)
# 
# opt_laz_compression(ctg) <- TRUE
# opt_progress(ctg) <- TRUE
# opt_stop_early(ctg) <- FALSE
# 
# 
# cores <- 12
# x <- paste0( "future::plan(future::multisession(workers = ", cores, "))")
# eval(parse(text = x))
# 
# ### normalize heights #### -------------------------------------------------------------------------
# 
# opt_output_files(ctg) <- paste0(data_path,"2_norm/raw/{ORIGINALFILENAME}")
# lidR::normalize_height(las = ctg, algorithm = tin())
# 
# 
# ### generate different variants of the data - different point densities and different tile sizes ####-----------
# 
# desired_densities <- c(1, 2, 5, 10, 20, 30, 40, 50)
# 
# for (desired_density in desired_densities) {
#   print(desired_density)
#   dir_out <- glue::glue("{data_path}/1_org/tile_1000_density_{desired_density}")
#   dir.create(dir_out)
#   opt_output_files(ctg) <- paste0(dir_out, "/{ORIGINALFILENAME}")
#   # #thin point cloud
#   lidR::decimate_points(ctg, algorithm = random(desired_density))
#   
# }
# 
# #need to run lassort on the thinned data!!!
# # run lassort
# 
# for (desired_density in desired_densities) {
#   print(desired_density)
#   dir_in <- glue::glue("{data_path}/1_org/tile_1000_density_{desired_density}")
#   dir_out <- glue::glue("{data_path}/1_org_ok/tile_1000_density_{desired_density}")
#   dir.create(dir_out)
# 
#   #run lassort
#   system(
#     glue("{lastools_path}/lassort64 -i {dir_in}/*.laz -odir {dir_out} -point_source -gps_time -olaz -cores 12")
#   )
#   # add index
#   system(glue("{lastools_path}/lasindex -i {dir_out}/*.laz -cores 12"))
# }
# 
# 
# 
# # ALS_path_out_cor_sort2 <- glue::glue("{dir_out}_2")
# # dir.create(ALS_path_out_cor_sort2)
# # system(
# #   glue("{lastools_path}/lassort64 -i {dir_out}/*.laz -odir {ALS_path_out_cor_sort2} -point_source -gps_time -olaz -cores 12")
# # )
# # dlist <- ALS_path_out_cor_sort2
# # lapply(dlist, #function(x) print(x))
# #        function(x) system(glue("{lastools_path}/lasindex -i {x}/*.laz -cores 12"))
# # )
# 
# 
# 
# #same for normalized data
# 
# ctg <- catalog(paste0(data_path,"2_norm/raw"))
# opt_laz_compression(ctg) <- TRUE
# opt_progress(ctg) <- TRUE
# opt_stop_early(ctg) <- FALSE
# 
# for (desired_density in desired_densities) {
#   print(desired_density)
#   dir_out <- glue::glue("{data_path}2_norm_temp/tile_1000_density_{desired_density}")
#   dir.create(dir_out)
#   
#   dir_out2 <- glue::glue("{data_path}/2_norm/tile_1000_density_{desired_density}")
#   dir.create(dir_out2)
#   
#   
#   opt_output_files(ctg) <- paste0(dir_out, "/{ORIGINALFILENAME}")
#   # #thin point cloud
#   lidR::decimate_points(ctg, algorithm = random(desired_density))
#   
#   #run lassort
#   system(
#     glue("{lastools_path}/lassort64 -i {dir_out}/*.laz -odir {dir_out2} -point_source -gps_time -olaz -cores 12")
#   )
#   # add index
#   system(glue("{lastools_path}/lasindex -i {dir_out2}/*.laz -cores 12"))
#   
#   
# }
# 
# plan(sequential())
# 
# 
# 
# 
# 
# #add lax files 
# dlist <- list.dirs(data_path)
# dlist <- dlist[grepl("tile",dlist)]
# 
# lapply(dlist, #function(x) print(x))
#        function(x) system(glue("{lastools_path}/lasindex -i {x}/*.laz -cores 12"))
# )
# 
# #######################################################
# ###compare processing time before and after lasoptimize
# 
# # run lasoptimize #lassort 
# ALS_path_out_sorted <- glue::glue("{data_path}1_org/raw_corrected_optim")
# dir.create(ALS_path_out_sorted)
# system(
#   glue("{lastools_path}/lasoptimize64 -i {ALS_path_out_cor}/*.laz -odir {ALS_path_out_sorted} -olaz -cores 12")
#   # glue("{lastools_path}/las2las -set_version 1.2 -remove_all_vlrs -set_point_type 3 -set_point_size 34  -i {ALS_path_out}/*.laz -odir {ALS_path_out_sorted} -olaz -cores 12")
# )
# 
# ### add lax index 
# system(glue("{lastools_path}/lasindex -i {ALS_path_out_cor_sort}/*.laz -cores 12"))
# 
# #thin to 10pts
# 
# # create a catalog
# ctg <- catalog(ALS_path_out_cor_sort)
# opt_laz_compression(ctg) <- TRUE
# opt_progress(ctg) <- TRUE
# opt_stop_early(ctg) <- FALSE
# 
# cores <- 12
# x <- paste0( "future::plan(future::multisession(workers = ", cores, "))")
# eval(parse(text = x))
# 
# 
# desired_density<- 10
# dir_out <- glue::glue("{data_path}/1_org/tile_1000_density_{desired_density}_sort")
# dir.create(dir_out)
# opt_output_files(ctg) <- paste0(dir_out, "/{ORIGINALFILENAME}")
# # #thin point cloud
# lidR::decimate_points(ctg, algorithm = random(desired_density))
# 
# dlist <- dir_out
# lapply(dlist, #function(x) print(x))
#        function(x) system(glue("{lastools_path}/lasindex -i {x}/*.laz -cores 12"))
# )
# 
# 
# 
# #lassort after thinning?
# 
# 
# # run lassort
# ALS_path_out_cor_sort2 <- glue::glue("{dir_out}_2")
# dir.create(ALS_path_out_cor_sort2)
# system(
#   glue("{lastools_path}/lassort64 -i {dir_out}/*.laz -odir {ALS_path_out_cor_sort2} -point_source -gps_time -olaz -cores 12")
# )
# dlist <- ALS_path_out_cor_sort2
# lapply(dlist, #function(x) print(x))
#        function(x) system(glue("{lastools_path}/lasindex -i {x}/*.laz -cores 12"))
# )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
