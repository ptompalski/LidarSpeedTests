test_that("two isolated tile workers repair and prepare deterministic native outputs", {
  bin <- Sys.getenv("LIDAR_FRESH_LASTOOLS_PATH")
  if (!nzchar(bin)) skip("Set LIDAR_FRESH_LASTOOLS_PATH for the tiny parallel native fixture.")
  if (!nzchar(Sys.which("zip"))) skip("zip is needed to create the tiny archive.")
  withr::local_dir(repo)
  directory <- withr::local_tempdir()
  env <- new.env(parent = globalenv())
  sys.source("R/prepare_fresh_data.R", env)
  root <- env$fresh_subdir(directory, "root")
  download <- env$fresh_subdir(directory, "download")
  historical <- env$fresh_subdir(directory, "historical")
  raw <- env$fresh_subdir(directory, "raw")
  scratch <- env$fresh_subdir(root, "scratch")
  folder <- env$fresh_subdir(root, "pilot")
  repaired <- env$fresh_subdir(folder, "repaired")
  prepared <- env$fresh_subdir(folder, "prepared")
  tiles <- c("298000_5093000", "298000_5094000")
  for (tile in tiles) {
    origin <- as.numeric(strsplit(tile, "_", fixed = TRUE)[[1]])
    xy <- expand.grid(X = origin[1] + seq(0, 3.5, .25),
                      Y = origin[2] + seq(0, 3.5, .25))
    las <- lidR::LAS(data.frame(
      X = xy$X, Y = xy$Y, Z = rep(100, nrow(xy)), gpstime = seq_len(nrow(xy)) / 10,
      ReturnNumber = rep(1L, nrow(xy)), NumberOfReturns = rep(1L, nrow(xy)),
      Classification = rep(2L, nrow(xy)), PointSourceID = rep(1L, nrow(xy)),
      Synthetic_flag = seq_len(nrow(xy)) <= 138L
    ))
    las@data$Z[139] <- -.01
    sf::st_crs(las) <- 2959
    lidR::writeLAS(las, file.path(raw, paste0(tile, ".las")))
  }
  archive <- file.path(download, "source.zip")
  withr::with_dir(raw, utils::zip(archive, list.files(raw), flags = "-q"))
  execution <- list(requested_cores = 2L, workers = 2L, threads_per_worker = 1L)
  jobs <- lapply(tiles, function(tile) list(phase = "repair", tile = tile,
    member = paste0(tile, ".las"), archive = archive, folder = folder,
    config = list(root = root, protected_roots = historical, crs = 2959L),
    execution = execution))
  repair_results <- env$fresh_map(jobs, 2L, env$fresh_subdir(folder, "repair-logs"))
  expect_length(unique(vapply(repair_results, `[[`, integer(1), "pid")), 2L)
  repair_paths <- file.path(repaired, paste0(tiles, ".laz"))
  repair_hashes <- env$petawawa_checksum(c(repair_paths, paste0(repair_paths, ".rds")))
  for (job in jobs) expect_identical(env$fresh_repair_tile(job)$status, "resumed")
  expect_identical(env$petawawa_checksum(c(repair_paths, paste0(repair_paths, ".rds"))),
                   repair_hashes)
  config <- list(source_root = repaired, output_root = prepared,
    source_citation = "Synthetic fixture", license = "Synthetic fixture",
    crs = 2959L, repaired = TRUE, repair_provenance = env$fresh_policy()$repair,
    lastools_path = bin, executables = c(sort = "lassort64", index = "lasindex64"),
    tool_license = "User-approved installed native tools for synthetic tests",
    tiles = tiles, seed = 20261022L)
  plan <- env$petawawa_preflight(config, read = env$fresh_read_las)
  plan$densities <- 1
  env$petawawa_initialize_plan(plan)
  dir.create(file.path(prepared, "1_org", "tile_1000_density_1"), recursive = TRUE)
  jobs <- lapply(1:2, function(i) list(phase = "prepare", plan = plan,
                                     row = plan$sources[i, ], execution = execution))
  results <- env$fresh_map(jobs, 2L, env$fresh_subdir(folder, "prepare-logs"))
  expect_length(unique(vapply(results, `[[`, integer(1), "pid")), 2L)
  for (i in 1:2) {
    source <- env$fresh_read_las(plan$sources$path[i])
    repair <- readRDS(paste0(plan$sources$path[i], ".rds"))
    expect_length(repair$raw_warnings, 0L)
    expect_equal(repair$source_filter$source_points, 225)
    expect_equal(repair$source_filter$discarded_synthetic_points, 138)
    expect_equal(repair$source_filter$retained_points, 87)
    expect_identical(sum(source$Synthetic_flag), 0L)
    expect_equal(source$gpstime, (140:225) / 10)
    removal <- repair$actions[repair$actions$action == "drop_negative_Z", ]
    expect_identical(removal$source_row, 1L)
    expect_identical(removal$source_Z, -.01)
    expect_equal(lidR::npoints(source), 86)
    seed <- config$seed + match(tiles[i], env$petawawa_tiles())
    expected <- env$fresh_variant(source, 1, seed)
    path <- file.path(prepared, "1_org", "tile_1000_density_1", paste0(tiles[i], ".laz"))
    expect_no_error(env$petawawa_validate_output(path, expected, read = env$fresh_read_las))
    record <- readRDS(paste0(path, ".rds"))
    expect_identical(record$execution, execution)
    expect_identical(record$index_validation$status, "passed")
    expect_false(any(grepl("points flagged 'synthetic'", record$summary$quality$check$messages)))
    expect_false(grepl("\\b(warning|error)\\b", paste(
      record$index_validation$process$stdout, record$index_validation$process$stderr),
      ignore.case = TRUE))
    hashes <- env$petawawa_checksum(c(path, sub("\\.laz$", ".lax", path)))
    expect_no_error(env$fresh_dispatch(jobs[[i]]))
    expect_identical(env$petawawa_checksum(c(path, sub("\\.laz$", ".lax", path))), hashes)
  }
  expect_length(list.files(scratch, all.files = TRUE, no.. = TRUE), 0L)
  expect_length(list.files(prepared, pattern = "^lidar-run-"), 0L)
})
