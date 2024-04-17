# script to download the data and then preprocess it to different point densities

source("R/0_setup.R")
library(curl) #fast(er) data download

#where to download the data and store the different point cloud density versions of the data
data_path <- "N:/benchmark_data/"

#download ALS data (100 tiles, Ontario, 2023, single photon lidar) ####----------------------------------------

#a vector of 100 tiles with download links
ALS_tiles <- st_read("ALS_tiles.gpkg")

# path to store the downloaded files
ALS_path_out <- glue::glue("{data_path}1_org/raw")
dir.create(ALS_path_out)

#download data
urls <- ALS_tiles$Download_LAZ
multi_download(urls = urls, destfiles = glue("{ALS_path_out}/{basename(urls)}"), resume = T)



### add lax index 
# system(glue("{lastools_path}/lasindex -i N:/benchmark_data/original/*.laz -cores 8"))

# create a catalog
ctg <- catalog(ALS_path_out)
plot(ctg)

opt_laz_compression(ctg) <- TRUE
opt_progress(ctg) <- TRUE
opt_stop_early(ctg) <- FALSE


cores <- 10
x <- paste0( "future::plan(future::multisession(workers = ", cores, "))")
eval(parse(text = x))


### normalize heights #### -------------------------------------------------------------------------

opt_output_files(ctg) <- paste0(data_path,"2_norm/raw/{ORIGINALFILENAME}")
lidR::normalize_height(las = ctg, algorithm = tin())



### generate different variants of the data - different point densities and different tile sizes ####-----------

desired_densities <- c(1, 2, 5, 10, 20, 30, 40, 50)



for (desired_density in desired_densities) {
  print(desired_density)
  dir_out <- glue::glue("{data_path}/1_org/tile_1000_density_{desired_density}")
  dir.create(dir_out)
  opt_output_files(ctg) <- paste0(dir_out, "/{ORIGINALFILENAME}")
  # #thin point cloud
  lidR::decimate_points(ctg, algorithm = random(desired_density))
  
}

#same for normalized data

ctg <- catalog(paste0(data_path,"2_norm/raw"))
opt_laz_compression(ctg) <- TRUE
opt_progress(ctg) <- TRUE
opt_stop_early(ctg) <- FALSE

for (desired_density in desired_densities) {
  print(desired_density)
  dir_out <- glue::glue("{data_path}2_norm/tile_1000_density_{desired_density}")
  dir.create(dir_out)
  opt_output_files(ctg) <- paste0(dir_out, "/{ORIGINALFILENAME}")
  # #thin point cloud
  lidR::decimate_points(ctg, algorithm = random(desired_density))
  
}

plan(sequential())
