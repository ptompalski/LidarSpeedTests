policy_fixture <- function(envir = parent.frame()) {
  f <- benchmark_fixture(envir)
  env <- f$env
  f$input <- file.path(f$test_dir, "input")
  f$scratch <- file.path(f$test_dir, "scratch")
  f$reports <- file.path(f$test_dir, "reports")
  for (path in c(f$input, f$scratch, f$reports)) dir.create(path)
  for (i in 0:1) {
    data <- expand.grid(X = c(1, 10, 20, 30, 39) + 40 * i,
                        Y = c(1, 10, 20, 30, 39))
    data$Z <- 10
    data$Classification <- 2L
    data$ReturnNumber <- 1L
    data$NumberOfReturns <- 1L
    data <- rbind(data, data.frame(X = 12 + 40 * i, Y = 12, Z = 15,
                                   Classification = 1L, ReturnNumber = 2L,
                                   NumberOfReturns = 2L))
    lidR::writeLAS(lidR::LAS(data), file.path(f$input, paste0("tile", i, ".las")),
                   index = TRUE)
  }
  f$metadata <- list(
    horizontal_epsg = 2959L, vertical_epsg = 5713L, xyz_units = "metre",
    confirmation = "Custodian confirmed source lineage and unchanged XYZ units/datum."
  )
  f$config <- env$benchmark_validate_config(list(
    input_roots = c(SSD = f$input), scratch_roots = c(SSD = f$scratch),
    reports_path = f$reports, cores = 1L, tools = c("lidR", "lasR"),
    workstation_id = "fixture", resource_monitoring = FALSE,
    scheduling = list(cooldown_seconds = 0), input_metadata = f$metadata,
    accept_known_surface_discrepancies = TRUE, diagnostic_lasR = TRUE
  ))
  cfg <- file.path(f$test_dir, "config.R")
  dput(f$config, cfg)
  withr::local_envvar(c(LIDAR_BENCHMARK_CONFIG = cfg), .local_envir = envir)
  withr::local_dir(repo, .local_envir = envir)
  env$source <- function(file) sys.source(file.path(repo, file), env)
  suppressPackageStartupMessages(sys.source(file.path(repo, "R/0_setup.R"), env))
  withr::defer(lasR::unset_exec_option(), envir)
  f$files <- list.files(f$input, full.names = TRUE)
  f$bytes <- env$benchmark_checksums(f$files)
  f
}

policy_run <- function(f, task, tool) {
  row <- f$env$benchmark_tasks[f$env$benchmark_tasks$task_id == task, ]
  report <- tempfile(tmpdir = f$reports, fileext = ".rds")
  context <- list2env(list(
    dir_in = f$input, scratch_root = f$scratch, cores = 1L,
    output_format = row$output_format, task_id = task, validate_artifacts = TRUE,
    validation_report = report, monitor_path = NULL
  ), parent = f$env)
  result <- f$env$benchmark_run_tool(
    tool, row[[paste0("task_call_", tool)]][[1L]], context, f$config, "diagnostic")
  list(result = result, report = if (file.exists(report)) readRDS(report) else NULL)
}

test_that("confirmed input metadata is validated and applied without changing source bytes", {
  f <- policy_fixture()
  env <- f$env
  ctg <- lidR::readLAScatalog(f$input)
  expect_true(is.na(sf::st_crs(ctg)))
  declared <- env$benchmark_apply_input_metadata(ctg, f$metadata)
  expect_identical(sf::st_crs(declared)$epsg, 2959L)
  manifest <- env$benchmark_validation_manifest(declared, f$config)
  expect_identical(manifest$input_metadata, f$metadata)
  expect_true(manifest$diagnostic_lasR)
  expect_true(manifest$accept_known_surface_discrepancies)
  expect_identical(env$benchmark_checksums(f$files), f$bytes)
  expect_true(is.na(sf::st_crs(lidR::readLASheader(f$files[1L]))))
  bad <- f$metadata
  bad$horizontal_epsg <- 4326L
  expect_error(env$benchmark_input_metadata(bad), "in metres")
  bad <- f$metadata
  bad$vertical_epsg <- 2959L
  expect_error(env$benchmark_input_metadata(bad), "in metres")
  bad <- f$metadata
  bad$confirmation <- ""
  expect_error(env$benchmark_input_metadata(bad), "custodian confirmation")
  cloud <- lidR::readLAS(f$files[1L])
  sf::st_crs(cloud) <- 32618
  lidR::writeLAS(cloud, f$files[1L])
  expect_error(env$benchmark_apply_input_metadata(
    lidR::readLAScatalog(f$input), f$metadata), "conflicts")
})

test_that("known surface exceptions are opt-in and preserve numerical and structural checks", {
  f <- policy_fixture()
  for (tool in c("lidR", "lasR")) {
    run <- policy_run(f, "generate_DSM1", tool)
    expect_identical(run$result$execution_status, "succeeded")
    expect_identical(run$result$validation_status, "accepted_exception",
                     info = run$result$validation_diagnostic)
    expect_true(length(run$report$accepted_exceptions) > 0L)
    expect_identical(run$report$input_manifest$input_metadata, f$metadata)
  }
  expect_identical(f$env$benchmark_checksums(f$files), f$bytes)
  expect_setequal(list.files(f$input, full.names = TRUE), f$files)
  expect_length(list.files(f$scratch), 0L)
  f$config$accept_known_surface_discrepancies <- FALSE
  strict <- policy_run(f, "generate_DSM1", "lidR")
  expect_identical(strict$result$validation_status, "failed")
  expect_match(strict$result$validation_diagnostic, "mask mismatch")
  tile <- data.frame(Min.X = 0, Max.X = 4, Min.Y = 0, Max.Y = 4)
  cloud <- lidR::LAS(data.frame(X = c(.5, 1.5), Y = c(.5, 1.5), Z = c(2, 3)))
  sf::st_crs(cloud) <- 2959
  manifest <- list(crs = sf::st_crs(cloud), accept_known_surface_discrepancies = TRUE)
  spec <- f$env$benchmark_artifact_specs("generate_DSM1", "lidR")[[1L]]
  grid <- f$env$benchmark_validation_grid(tile, 1, manifest$crs)
  raster <- f$env$benchmark_reference_raster(cloud, grid, "generate_DSM1")
  file <- file.path(f$reports, "surface.tif")
  check <- function() f$env$benchmark_validate_raster(
    file, spec, tile, manifest, cloud, TRUE, "lidR")
  wrong <- raster
  values <- terra::values(wrong)
  values[is.finite(values)] <- values[is.finite(values)] + 1
  terra::values(wrong) <- values
  terra::writeRaster(wrong, file, overwrite = TRUE)
  expect_error(check(), "Numerical tolerance")
  terra::writeRaster(terra::shift(raster, dx = .5), file, overwrite = TRUE)
  expect_error(check(), "origin mismatch")
  terra::writeRaster(terra::extend(raster, c(0, 7, 0, 4)), file, overwrite = TRUE)
  expect_error(check(), "extent mismatch")
  wrong <- raster
  terra::values(wrong) <- NA_real_
  terra::writeRaster(wrong, file, overwrite = TRUE)
  expect_error(check(), "No finite surface coverage")
})

test_that("input declarations and diagnostic acceptance policies are frozen in campaign identity", {
  f <- policy_fixture()
  tasks <- data.frame(task_id = "unsupported", data_type = "datasets_org",
                      output_format = "", task_call_lidR = "", task_call_lasR = "")
  runs <- data.frame(ID = "pilot", dir_in_org = f$input,
                     scratch_root = f$scratch, cores = 1L)
  campaign <- fixture_campaign(f$env, tasks, runs, f$config, "policies")
  expect_identical(campaign$manifest$identity$config$input_metadata, f$metadata)
  for (field in c("diagnostic_lasR", "accept_known_surface_discrepancies", "input_metadata")) {
    changed <- f$config
    if (field == "input_metadata") {
      changed$input_metadata$confirmation <- "A different custodian declaration."
    } else changed[[field]] <- FALSE
    expect_error(f$env$benchmark_campaign_check(campaign, tasks, runs, changed),
                 "incompatible|changed|match", ignore.case = TRUE)
  }
})

test_that("normalization and both complex calls execute diagnostically without hiding failures", {
  f <- policy_fixture()
  for (task in c("normalization", "complex_task1", "complex_task2")) {
    contract <- f$env$benchmark_workload_contract(task, "lasR", TRUE)
    expect_true(contract$supported)
    expect_identical(contract$comparison_status, "diagnostic_only")
    row <- f$env$benchmark_tasks[f$env$benchmark_tasks$task_id == task, ]
    expect_true(nzchar(row$task_call_lasR))
    run <- policy_run(f, task, "lasR")
    expect_false(run$result$execution_status == "skipped")
    expect_false(run$result$validation_status %in% c("passed", "accepted_exception"),
                 info = run$result$validation_diagnostic)
    expect_true(nzchar(run$result$diagnostic) || nzchar(run$result$validation_diagnostic))
  }
  expect_identical(f$env$benchmark_checksums(f$files), f$bytes)
  expect_setequal(list.files(f$input, full.names = TRUE), f$files)
  expect_length(list.files(f$scratch), 0L)
})

test_that("declared CRS reaches persisted lasR metrics and input bytes remain unchanged", {
  f <- policy_fixture()
  run <- policy_run(f, "pixel_metrics_1a", "lasR")
  expect_identical(run$result$execution_status, "succeeded")
  expect_identical(run$result$validation_status, "passed",
                   info = run$result$validation_diagnostic)
  expect_identical(f$env$benchmark_checksums(f$files), f$bytes)
  expect_setequal(list.files(f$input, full.names = TRUE), f$files)
  index <- sub("\\.las$", ".lax", f$files[1L])
  unlink(index)
  run <- policy_run(f, "pixel_metrics_1a", "lasR")
  expect_identical(run$result$execution_status, "failed")
  expect_match(run$result$diagnostic, "existing nonempty LAX")
  expect_false(file.exists(index))
})

test_that("pilot staging copies only the frozen neighboring pair and refuses changed or extra bytes", {
  f <- benchmark_fixture()
  sys.source(file.path(repo, "R/6_pilot.R"), f$env)
  source <- file.path(f$test_dir, "historical")
  output <- file.path(f$test_dir, "output")
  dir.create(source)
  dir.create(output)
  dir.create(file.path(source, "tile_1000_density_1"))
  tiles <- c("298000_5093000", "298000_5094000")
  relative <- paste0("tile_1000_density_1/", tiles, ".laz")
  files <- file.path(source, relative)
  indexes <- sub("\\.laz$", ".lax", files)
  for (file in c(files, indexes)) writeLines(basename(file), file)
  manifest <- data.frame(density = 1L, tile = tiles, path = relative,
                         md5 = unname(tools::md5sum(files)),
                         index_md5 = unname(tools::md5sum(indexes)), las_version = "1.1")
  csv <- file.path(f$test_dir, "manifest.csv")
  write.csv(manifest, csv, row.names = FALSE)
  original <- f$env$benchmark_checksums(c(files, indexes))
  staged <- f$env$benchmark_stage_pilot_inputs(source, output, csv)
  expect_identical(f$env$benchmark_stage_pilot_inputs(source, output, csv), staged)
  expect_equal(nrow(staged$provenance$staged), 4L)
  expect_identical(f$env$benchmark_checksums(c(files, indexes)), original)
  target <- file.path(staged$root, relative[1L])
  writeLines("changed", target)
  expect_error(f$env$benchmark_stage_pilot_inputs(source, output, csv), "checksums differ")
  expect_identical(readLines(target), "changed")
  file.copy(files[1L], target, overwrite = TRUE)
  extra <- file.path(staged$root, "tile_1000_density_1", "third-tile.laz")
  writeLines("unapproved", extra)
  expect_error(f$env$benchmark_stage_pilot_inputs(source, output, csv), "Unexpected files")
  expect_error(f$env$benchmark_stage_pilot_inputs(source, source, csv), "disjoint")
})

test_that("declared native metadata assigns the CRS without transforming coordinates or permitting demo fallback", {
  with_execution_fixture({
    task_call$executable <- "lascanopy64"
    call_env$input_metadata <- list(horizontal_epsg = 2959L)
    command <- env$benchmark_lastools_command(task_call, call_env)
    expect_identical(tail(command$args, 3L), c("-epsg", "2959", "-fail"))
    expect_false("-target_epsg" %in% command$args)
    expect_false("-demo" %in% command$args)
  })
})

test_that("large sparse surface references preserve cell ownership without named-list searches", {
  f <- benchmark_fixture()
  xy <- expand.grid(X = seq(.5, 999.5), Y = seq(.5, 99.5))
  xy <- xy[seq(1L, nrow(xy), by = 2L), ]
  xy$Z <- as.double(seq_len(nrow(xy)) %% 23L)
  grid <- terra::rast(xmin = 0, xmax = 1000, ymin = 0, ymax = 100, resolution = 1)
  cells <- terra::cellFromXY(grid, as.matrix(xy[c("X", "Y")]))
  expected <- rep(NA_real_, terra::ncell(grid))
  expected[cells] <- xy$Z
  cloud <- lidR::LAS(xy)
  reference <- f$env$benchmark_reference_raster(cloud, grid, "generate_DSM1")
  expect_equal(as.vector(terra::values(reference)), expected)
  expect_equal(sum(is.finite(terra::values(reference))), 50000L)
})

test_that("buffered validation preserves sub-millimetre input coordinates instead of requantizing", {
  f <- policy_fixture()
  for (file in list.files(f$input, "\\.las$", full.names = TRUE)) {
    cloud <- lidR::readLAS(file)
    data <- as.data.frame(cloud@data)
    data$X <- data$X + .0001
    data$Y <- data$Y + .0002
    data$Z <- data$Z + .0003
    header <- cloud@header
    header@PHB[c("X scale factor", "Y scale factor", "Z scale factor")] <- rep(list(.0001), 3L)
    for (axis in c("X", "Y", "Z")) {
      offset <- header@PHB[[paste(axis, "offset")]]
      data[[axis]] <- round((data[[axis]] - offset) / .0001) * .0001 + offset
    }
    lidR::writeLAS(lidR::LAS(data, header = header), file)
  }
  catalog <- f$env$benchmark_apply_input_metadata(lidR::readLAScatalog(f$input), f$metadata)
  manifest <- f$env$benchmark_validation_manifest(catalog, f$config)
  tile <- manifest$tiles[1L, ]
  expected <- do.call(rbind, lapply(manifest$tiles$filename, function(file) {
    data <- as.data.frame(lidR::readLAS(file)@data)
    data[data$X >= tile$Min.X - 20 & data$X <= tile$Max.X + 20 &
           data$Y >= tile$Min.Y - 20 & data$Y <= tile$Max.Y + 20, c("X", "Y", "Z")]
  }))
  reference <- f$env$benchmark_validation_points(manifest, tile)
  actual <- as.data.frame(reference@data)[c("X", "Y", "Z")]
  expect_equal(unname(as.matrix(actual)), unname(as.matrix(expected)), tolerance = 1e-12)
  expect_identical(reference@header@PHB[["X scale factor"]], .0001)
  expect_identical(sf::st_crs(reference), manifest$crs)
})
