
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LidarSpeedTests

This repository is dedicated to evaluating the performance of various
point cloud processing tools in handling typical airborne laser scanning
(ALS) data processing tasks. The goal is to provide users with
comparative insights into processing time, CPU, RAM, and disk usage
across different tools and configurations. More importantly, benchmark
results could help decide which processing parameters are most optimal
given the dataset and workstation characteristics.

## Project Overview

Point cloud processing tools offer a range of functionalities, often
with overlapping capabilities. Users face decisions about which tool to
use and how to configure settings for optimal performance. This project
benchmarks several tools to guide these decisions.

## Benchmarking Methodology

- **Uniform Task Performance**: Each tool is tested against standardized
  tasks to ensure comparable results.
- **Resource Utilization**: We measure and record processing time, CPU
  and RAM usage.
- **Iterative Testing**: Tools are tested under various conditions to
  assess performance impacts:
  - Different numbers of workers (CPU cores/threads)
  - Various storage devices including local (SSD, HDD) and network
    drives
  - Diverse input data characteristics

## Input Data

The benchmark uses a subset of ALS data acquired over the Peatawawa
research forest located in in Ontario, Canada. The dataset is divided
into 1 x 1 km tiles (100 tiles in total). This dataset is freely
available and can be downloaded from
<https://opendata.nfis.org/mapserver/PRF.html>.

![](graphics/data.png)

The average point density of the dataset is 13.6 pts/m². To facilitate a
comprehensive analysis, the original dataset was systematically reduced
to create variants with different densities: 1, 2, 5, 10. To simulate
dataset with higher point density, the data was artificially densified
by duplicating and then introducing a random noise to a subset of
points. This procedure was used to generate datasets with densities of
20, and 50 pts/m². Benchmarks were conducted across these modified
datasets to assess performance at varying point densities.

![](graphics/data_density_crossections.png)

## Included Software Packages

So far the benchmark focuses on lidar processing tools available in R
(`lidR`, `lasR`), as well as `lastools`.

- `lidR`: <https://github.com/r-lidar/lidR>
- `lasR`: <https://github.com/r-lidar/lasR>
- `lastools`: <https://rapidlasso.de/product-overview/>

## Benchmark Tasks

Tasks are categorized into ‘Simple’ and ‘Complex’ to differentiate
between single-step operations and multi-step processing pipelines,
respectively.

### Simple Tasks

- Ground classification
- Digital Terrain Model (DTM) generation
- Height normalization
- Point cloud metrics calculation (two scenarios: single metric,
  multiple metrics)

### Complex Tasks (in progress)

- a workflow consisting of the following tasks: Generate DTM, normalize
  heights, and calculate metrics.
- …

### Additional benchmarks:

- Influence of tile size on processing time
- Influence of drive configuration on processing time

# Results

### Generate DEM

![](graphics/result_generate_DEM_W-VIC-A127816_v2.png)

### Genenerate DSM

![](graphics/result_generate_DSM1_W-VIC-A127816_v2.png)

![](graphics/result_generate_DSM2_W-VIC-A127816_v2.png)

### Normalization

![](graphics/result_normalization_W-VIC-A127816_v2.png)

### Pixel metrics

#### Single metric

![](graphics/result_pixel_metrics_1a_W-VIC-A127816_v2.png)

#### Multiple metrics

![](graphics/result_pixel_metrics_2_W-VIC-A127816_v2.png)

### Detecting treetops

![](graphics/result_tree_detection_W-VIC-A127816_v2.png)

## Influence of tile size

This benchmark was aimed to evaluate the influence of the tile size on
processing time. In addition to the 1000 m tile size used in all
standard benchmark runs, three additional sizes were tested: 200, 500,
and 2000 m.

![](graphics/byTileSize.png) ![](graphics/byTileSizeRel.png)

## Influence of drive/storage type

This benchmark was aimed to evaluate the influence of drive
configuration on processing time. The benchmark involved reading data
from a drive, processing it, and then saving the processed data
(`normalization` task was used for this purpose). Two types of drives
were tested: an internal SSD and a network folder on a server. Four
distinct configurations were assessed: reading and writing both from the
SSD (`SSD_SSD`), reading from the SSD and writing to the network folder
(`SSD_NET`), reading from the network folder and writing to the SSD
(`NET_SSD`), and reading and writing both from the network folder
(`NET_NET`).

![](graphics/byDrives.png)

![](graphics/byDrivesRel.png)
