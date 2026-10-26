validation_fixture <- function(envir = parent.frame(), sparse = FALSE, fractional = FALSE) {
  fixture <- benchmark_fixture(envir)
  env <- fixture$env
  input <- file.path(fixture$test_dir, "input")
  dir.create(input)
  for (i in 0:1) {
    xy <- expand.grid(X = c(1, 10, 20, 30, 39) + 40 * i,
                      Y = c(1, 10, 20, 30, 39))
    if (fractional) {
      edge <- xy$X %in% (c(1, 39) + 40 * i)
      xy$X[edge] <- xy$X[edge] + .2
      xy$Y[xy$Y %in% c(1, 39)] <- xy$Y[xy$Y %in% c(1, 39)] + .2
    }
    if (sparse) xy <- xy[c(1, 7, 25), ]
    data <- data.frame(xy, Z = 1, Classification = 2L,
                       ReturnNumber = 1L, NumberOfReturns = 1L, Intensity = 42L)
    data <- rbind(data, data.frame(X = 12 + 40 * i, Y = 12, Z = 15,
                                   Classification = 1L, ReturnNumber = 2L,
                                   NumberOfReturns = 2L, Intensity = 99L))
    cloud <- lidR::LAS(data)
    sf::st_crs(cloud) <- 32618
    lidR::writeLAS(cloud, file.path(input, paste0("tile", i + 1L, ".las")))
  }
  env$source <- function(file) sys.source(file.path(repo, file), env)
  withr::local_dir(repo, .local_envir = envir)
  withr::local_envvar(c(LIDAR_BENCHMARK_CONFIG = NA_character_), .local_envir = envir)
  suppressPackageStartupMessages(sys.source(file.path(repo, "R/0_setup.R"), env))
  previous <- future::plan()
  future::plan(future::sequential)
  withr::defer(future::plan(previous), envir)
  withr::defer(lasR::unset_exec_option(), envir)
  lasR::set_exec_options(ncores = lasR::sequential())
  withr::local_options(list(lidR.progress = FALSE), .local_envir = envir)
  old <- get0("metrics_multiple", envir = globalenv(), inherits = FALSE)
  assign("metrics_multiple", env$metrics_multiple, globalenv())
  withr::defer({
    if (is.null(old)) rm("metrics_multiple", envir = globalenv()) else
      assign("metrics_multiple", old, globalenv())
  }, envir)
  list(env = env, root = fixture$test_dir, input = input)
}

validation_run <- function(fixture, task, tool) {
  env <- fixture$env
  output <- tempfile(paste0(task, "-", tool, "-"), tmpdir = fixture$root)
  dir.create(output)
  context <- new.env(parent = env)
  context$dir_out <- output
  context$ctg <- env$prepare_input(fixture$input, output)
  context$f <- context$ctg@data$filename
  context$fout <- file.path(output, paste0("*.", env$benchmark_tasks$output_format[
    match(task, env$benchmark_tasks$task_id)]))
  manifest <- env$benchmark_validation_manifest(context$ctg)
  call <- env$benchmark_tasks[[paste0("task_call_", tool)]][[
    match(task, env$benchmark_tasks$task_id)]]
  execution <- env$benchmark_execute_call(call, tool, context)
  expect_identical(execution$execution_status, "succeeded", info = execution$diagnostic)
  checked <- env$check_results(output, manifest, task, tool, execution)
  list(output = output, manifest = manifest, checked = checked, execution = execution)
}

test_that("metric references retain outer-edge clamping and right/down buffered ownership", {
  f <- benchmark_fixture()
  previous <- get0("metrics_multiple", envir = globalenv(), inherits = FALSE)
  assign("metrics_multiple", f$env$metrics_multiple, globalenv())
  withr::defer({
    if (is.null(previous)) rm("metrics_multiple", envir = globalenv()) else
      assign("metrics_multiple", previous, globalenv())
  })
  for (maximum in c(20, 40)) {
    data <- expand.grid(X = c(0, maximum), Y = c(0, maximum))
    data$Z <- c(1, 2, 3, 4)
    cloud <- lidR::LAS(data)
    grid <- terra::rast(xmin = 0, xmax = maximum, ymin = 0, ymax = maximum,
                        resolution = 20, crs = "")
    actual_mean <- lidR::pixel_metrics(cloud, ~list(z_mean = mean(Z)), res = 20)
    actual_multiple <- lidR::pixel_metrics(cloud, ~metrics_multiple(Z), res = 20)
    reference_mean <- f$env$benchmark_reference_raster(cloud, grid, "pixel_metrics_1a")
    reference_multiple <- f$env$benchmark_reference_raster(cloud, grid, "pixel_metrics_2")
    expect_equal(terra::values(terra::crop(actual_mean, grid)), terra::values(reference_mean))
    expect_equal(terra::values(terra::crop(actual_multiple, grid)), terra::values(reference_multiple))
  }
  cloud <- lidR::LAS(data.frame(X = rep(10, 4), Y = c(0, 20, 40, 60),
                                Z = c(10, 20, 40, 60)))
  grid <- terra::rast(xmin = 0, xmax = 20, ymin = 20, ymax = 40,
                      resolution = 20, crs = "")
  reference <- f$env$benchmark_reference_raster(cloud, grid, "pixel_metrics_1a")
  expect_equal(as.vector(terra::values(reference)), 40)
})

test_that("tool-specific metric and maximum ownership retains strict masks and interior values", {
  f <- benchmark_fixture()
  cloud <- lidR::LAS(data.frame(X = c(110, 110, 110, 130, 140),
                                Y = c(100, 120, 140, 110, 130),
                                Z = c(99, 20, 40, 30, 88)))
  sf::st_crs(cloud) <- 32618
  tile <- data.frame(Min.X = 100, Max.X = 140, Min.Y = 100, Max.Y = 140)
  manifest <- list(crs = sf::st_crs(cloud))
  for (task in c("pixel_metrics_1a", "pixel_metrics_2", "generate_DSM1")) {
    spec <- f$env$benchmark_artifact_specs(task, "lidR")[[1L]]
    grid <- f$env$benchmark_validation_grid(tile, spec$resolution, manifest$crs)
    for (tool in c("lidR", "lasR")) {
      reference <- f$env$benchmark_reference_raster(cloud, grid, task, tool)
      bottom <- terra::cellFromXY(grid, matrix(c(110, 100), ncol = 2))
      if (task == "generate_DSM1") {
        expect_equal(unname(terra::values(reference)[bottom, 1L]),
                     if (tool == "lidR") 99 else NA_real_)
      }
      file <- file.path(f$test_dir, paste0(task, "-", tool, ".tif"))
      terra::writeRaster(reference, file, overwrite = TRUE, datatype = "FLT8S")
      validate <- function() f$env$benchmark_validate_raster(
        file, spec, tile, manifest, cloud, numerical = TRUE, tool = tool)
      detail <- validate()
      expect_equal(detail$boundary_difference_points, 1L)
      expect_match(detail$boundary_convention, if (tool == "lasR") "excluded" else "clamped")
      values <- terra::values(reference)
      first <- which(is.finite(values))[1L]
      values[first] <- values[first] + 1
      terra::values(reference) <- values
      terra::writeRaster(reference, file, overwrite = TRUE, datatype = "FLT8S")
      expect_error(validate(), "Numerical tolerance exceeded")
      values[first] <- NA_real_
      terra::values(reference) <- values
      terra::writeRaster(reference, file, overwrite = TRUE, datatype = "FLT8S")
      expect_error(validate(), "mask mismatch")
    }
  }
})

test_that("buffered ownership matches lidR at positive, zero and negative grid boundaries", {
  f <- benchmark_fixture()
  for (lower in c(-40, 0, 100)) {
    cloud <- lidR::LAS(data.frame(X = lower + c(10, 10, 10, 30),
                                  Y = lower + c(0, 20, 40, 10),
                                  Z = c(99, 20, 40, 30)))
    grid <- terra::rast(xmin = lower, xmax = lower + 40,
                        ymin = lower, ymax = lower + 40, resolution = 20, crs = "")
    actual <- lidR::pixel_metrics(cloud, ~list(z_mean = mean(Z)), res = 20)
    reference <- f$env$benchmark_reference_raster(cloud, grid, "pixel_metrics_1a", "lidR")
    expect_equal(terra::values(terra::crop(actual, grid)), terra::values(reference))
  }
})

test_that("lasR terrain reference is an unrounded, unextrapolated independent plane", {
  f <- benchmark_fixture()
  xy <- data.frame(X = c(.5, 3.5, .5), Y = c(.5, .5, 3.5))
  data <- data.frame(xy, Z = c(100.001, 100.004, 100.011),
                     Classification = 2L)
  cloud <- lidR::LAS(data)
  sf::st_crs(cloud) <- 32618
  tile <- data.frame(Min.X = 0, Max.X = 4, Min.Y = 0, Max.Y = 4)
  manifest <- list(crs = sf::st_crs(cloud))
  spec <- f$env$benchmark_artifact_specs("generate_DEM", "lasR")[[1L]]
  grid <- f$env$benchmark_validation_grid(tile, 1, manifest$crs)
  reference <- f$env$benchmark_reference_raster(cloud, grid, "generate_DEM", "lasR")
  centers <- terra::xyFromCell(grid, seq_len(terra::ncell(grid)))
  inside <- centers[, 1] + centers[, 2] <= 4
  expected <- rep(NA_real_, nrow(centers))
  expected[inside] <- 100.001 + .001 * (centers[inside, 1] - .5) +
    (.01 / 3) * (centers[inside, 2] - .5)
  expect_equal(as.vector(terra::values(reference)), expected, tolerance = 1e-12)
  duplicate <- lidR::LAS(rbind(data, data[1L, ]))
  sf::st_crs(duplicate) <- manifest$crs
  expect_equal(terra::values(f$env$benchmark_reference_dem(duplicate, grid)),
               terra::values(reference))
  ambiguous <- rbind(data, transform(data[1L, ], Z = Z + 1))
  expect_error(f$env$benchmark_reference_dem(lidR::LAS(ambiguous), grid), "ambiguous duplicate XY")
  collinear <- data
  collinear$X <- c(.5, 1.5, 2.5)
  collinear$Y <- collinear$X
  expect_error(f$env$benchmark_reference_dem(lidR::LAS(collinear), grid), "nondegenerate")

  file <- file.path(f$test_dir, "terrain.tif")
  validate <- function(tool = "lasR") f$env$benchmark_validate_raster(
    file, spec, tile, manifest, cloud, numerical = TRUE, tool = tool)
  values <- terra::values(reference)
  first <- which(is.finite(values))[1L]
  limit <- 2e-6 + 4 * 2^-23 * abs(values[first])
  values[first] <- values[first] + limit / 2
  terra::values(reference) <- values
  terra::writeRaster(reference, file, overwrite = TRUE, datatype = "FLT8S")
  detail <- validate()
  expect_equal(detail$relative_tolerance, 4 * 2^-23)
  expect_match(detail$reference_method, "unextrapolated GEOS")
  values[first] <- values[first] + 2 * limit
  terra::values(reference) <- values
  terra::writeRaster(reference, file, overwrite = TRUE, datatype = "FLT8S")
  expect_error(validate(), "Numerical tolerance exceeded")
  values[first] <- NA_real_
  terra::values(reference) <- values
  terra::writeRaster(reference, file, overwrite = TRUE, datatype = "FLT8S")
  expect_error(validate(), "mask mismatch")
})

test_that("input-derived artifacts and numerical references validate neighboring R tiles", {
  fixture <- validation_fixture()
  for (task in c("pixel_metrics_1a", "pixel_metrics_2")) {
    for (tool in c("lidR", "lasR")) {
      run <- validation_run(fixture, task, tool)
      expect_identical(run$checked$validation_status, "passed",
                       info = paste(task, tool, run$checked$validation_diagnostic))
      expect_equal(run$checked$expected_output_file_count, 2L)
      expect_length(attr(run$checked, "validation_report")$artifacts, 2L)
    }
  }
})

test_that("strict surface extents are rejected while owned values and masks are validated", {
  fixture <- validation_fixture()
  for (task in c("generate_DSM1", "generate_DSM2", "generate_DEM")) {
    for (tool in c("lidR", "lasR")) {
      if (task == "generate_DSM2" && tool == "lasR") next
      run <- validation_run(fixture, task, tool)
      if (tool == "lasR") {
        expect_identical(run$checked$validation_status, "failed")
        expect_match(run$checked$validation_diagnostic, "mask mismatch|extent")
      } else {
        expect_identical(run$checked$validation_status, "passed",
                         info = run$checked$validation_diagnostic)
      }
    }
  }
})

test_that("fractional header maxima cannot silently round surface extents inward", {
  fixture <- validation_fixture(fractional = TRUE)
  for (task in c("generate_DSM1", "generate_DSM2")) {
    run <- validation_run(fixture, task, "lidR")
    expect_identical(run$checked$validation_status, "failed")
    expect_match(run$checked$validation_diagnostic, "extent")
  }
})

test_that("neighboring metric references cover unequal values on exact cell edges", {
  fixture <- validation_fixture()
  for (file in list.files(fixture$input, full.names = TRUE)) {
    cloud <- lidR::readLAS(file)
    cloud$Z <- seq_len(lidR::npoints(cloud)) / 10 - 1
    lidR::writeLAS(cloud, file)
  }
  for (tool in c("lidR", "lasR")) {
    for (task in c("pixel_metrics_1a", "pixel_metrics_2")) {
      run <- validation_run(fixture, task, tool)
      expect_identical(run$checked$validation_status, "passed",
                       info = paste(task, tool, run$checked$validation_diagnostic))
    }
  }
})

test_that("sparse cells retain exact NoData masks and singleton SD", {
  fixture <- validation_fixture(sparse = TRUE)
  for (tool in c("lidR", "lasR")) {
    run <- validation_run(fixture, "pixel_metrics_2", tool)
    expect_identical(run$checked$validation_status, "passed",
                     info = paste(tool, run$checked$validation_diagnostic))
    raster <- terra::rast(file.path(run$output, "tile1.tif"))
    values <- terra::values(raster)
    expect_true(anyNA(values[, "z_sd"]))
  }
})

test_that("missing corrupt partial shifted and wrong-band outputs are rejected independently", {
  fixture <- validation_fixture()
  env <- fixture$env
  run <- validation_run(fixture, "pixel_metrics_2", "lidR")
  file <- file.path(run$output, "tile1.tif")
  original <- terra::rast(file)
  snapshot <- terra::rast(original)
  terra::values(snapshot) <- terra::values(original)
  check <- function() env$check_results(
    run$output, run$manifest, "pixel_metrics_2", "lidR", run$execution)
  expect_identical(check()$validation_status, "passed")
  unlink(file)
  expect_match(check()$validation_diagnostic, "Missing|inventory")
  writeLines("not a GeoTIFF", file)
  expect_identical(suppressWarnings(check())$validation_status, "failed")
  write <- function(raster) terra::writeRaster(raster, file, overwrite = TRUE)
  write(snapshot[[1L]])
  expect_match(check()$validation_diagnostic, "band count")
  write(terra::shift(snapshot, dx = 1))
  expect_match(check()$validation_diagnostic, "origin|extent")
  wrong <- snapshot
  names(wrong) <- rev(names(wrong))
  write(wrong)
  expect_match(check()$validation_diagnostic, "names/order")
  wrong <- snapshot
  values <- terra::values(wrong)
  values[1L, ] <- NA_real_
  terra::values(wrong) <- values
  write(wrong)
  expect_match(check()$validation_diagnostic, "Coverage/NoData")
  wrong <- snapshot
  values <- terra::values(wrong)
  values[1L, 1L] <- values[1L, 1L] + 1
  terra::values(wrong) <- values
  write(wrong)
  expect_match(check()$validation_diagnostic, "tolerance")
  wrong <- snapshot
  values[1L, 1L] <- Inf
  terra::values(wrong) <- values
  write(wrong)
  expect_match(check()$validation_diagnostic, "Infinite")
  write(snapshot)
  expect_true(file.copy(file, file.path(run$output, "unexpected.tif")))
  expect_match(check()$validation_diagnostic, "unexpected")
  expect_identical(run$execution$execution_status, "succeeded")
})

test_that("normalization validates owned point attributes and rejects partial populations", {
  fixture <- validation_fixture()
  env <- fixture$env
  run <- validation_run(fixture, "normalization", "lidR")
  expect_identical(run$checked$validation_status, "passed",
                   info = run$checked$validation_diagnostic)
  file <- file.path(run$output, "tile1.laz")
  points <- lidR::readLAS(file)
  original <- points
  points$Z[1L] <- points$Z[1L] + 1
  lidR::writeLAS(points, file)
  check <- function() env$check_results(
    run$output, run$manifest, "normalization", "lidR", run$execution)
  expect_match(check()$validation_diagnostic, "tolerance")
  points <- original
  points$Intensity[1L] <- 5L
  lidR::writeLAS(points, file)
  expect_match(check()$validation_diagnostic, "attributes/XY")
  points <- points[-1L]
  lidR::writeLAS(points, file)
  expect_match(check()$validation_diagnostic, "count/attribute")
})

test_that("required vectors and disabled complex products cannot be replaced by sidecars", {
  fixture <- validation_fixture()
  env <- fixture$env
  run <- validation_run(fixture, "tree_detection", "lidR")
  expect_identical(run$checked$validation_status, "passed",
                   info = run$checked$validation_diagnostic)
  tree_file <- file.path(run$output, "tile1.gpkg")
  trees <- sf::st_read(tree_file, quiet = TRUE)
  trees$Z[1L] <- 1
  sf::st_write(trees, tree_file, delete_dsn = TRUE, quiet = TRUE)
  invalid <- env$check_results(run$output, run$manifest, "tree_detection", "lidR")
  expect_match(invalid$validation_diagnostic, "height contract")
  unlink(file.path(run$output, "tile1.gpkg"))
  writeLines("not a primary artifact", file.path(run$output, "tile1.vrt"))
  checked <- env$check_results(run$output, run$manifest, "tree_detection", "lidR")
  expect_identical(checked$validation_status, "failed")
  expect_match(checked$validation_diagnostic, "tile1.gpkg")
  complex <- env$check_results(run$output, run$manifest, "complex_task2", "lasR")
  expect_equal(complex$expected_output_file_count, 10L)
  expect_identical(complex$validation_status, "failed")
  expect_match(complex$validation_diagnostic, "Disabled")
  expect_match(complex$validation_diagnostic, "tile1_metrics.tif")
  native <- env$check_results(run$output, run$manifest, "pixel_metrics_2", "lastools")
  expect_equal(native$expected_output_file_count, 18L)
  expect_match(native$validation_diagnostic, "tile1_p05.tif")
  expect_match(native$validation_diagnostic, "tile1_p95.tif")
})

test_that("execution diagnostics false returns and input mutation cannot pass validation", {
  fixture <- validation_fixture()
  env <- fixture$env
  run <- validation_run(fixture, "generate_DSM1", "lasR")
  warned <- run$execution
  warned$diagnostic <- "Chunk 2 failed but engine continued"
  checked <- env$check_results(
    run$output, run$manifest, "generate_DSM1", "lasR", warned)
  expect_identical(checked$validation_status, "failed")
  expect_match(checked$validation_diagnostic, "Chunk 2")
  failed <- env$benchmark_execute_call("FALSE", "lasR")
  expect_identical(failed$execution_status, "failed")
  expect_match(failed$diagnostic, "chunk/pipeline failure")
  file <- run$manifest$tiles$filename[1L]
  points <- lidR::readLAS(file)
  points$Z[1L] <- 11
  lidR::writeLAS(points, file)
  checked <- env$check_results(
    run$output, run$manifest, "generate_DSM1", "lasR", run$execution)
  expect_identical(checked$validation_status, "failed")
  expect_match(checked$validation_diagnostic, "Input bytes changed")
})

test_that("tolerances enforce exact support and bounded Float32 error", {
  env <- benchmark_fixture()$env
  expect_equal(env$benchmark_compare_values(c(10 + 3e-6, NA), c(10, NA))$valid_values, 1)
  expect_error(env$benchmark_compare_values(10 + 1e-5, 10), "tolerance")
  expect_error(env$benchmark_compare_values(c(1, NA), c(NA, 1)), "NoData")
  expect_error(env$benchmark_compare_values(Inf, 1), "Infinite")
  expect_error(env$benchmark_compare_values(-9999, -9999), "sentinel")
  expect_equal(env$benchmark_compare_values(-9999, -9999, nodata = numeric())$valid_values, 1)
  expect_equal(env$benchmark_compare_values(numeric(), numeric())$valid_values, 0)
  expect_equal(env$benchmark_compare_values(c(NA_real_, NA_real_),
                                            c(NA_real_, NA_real_))$valid_values, 0)
})

test_that("known maximum grids and empty vector populations pass without filling NoData", {
  fixture <- benchmark_fixture()
  env <- fixture$env
  cloud <- lidR::LAS(data.frame(
    X = c(.25, 1.25, 1.75), Y = c(.25, .25, 1.25), Z = c(2, 4, 6)))
  sf::st_crs(cloud) <- 32618
  tile <- data.frame(id = "known", Min.X = 0, Max.X = 2, Min.Y = 0, Max.Y = 2)
  manifest <- list(crs = sf::st_crs(cloud))
  grid <- env$benchmark_validation_grid(tile, 1, manifest$crs)
  terra::values(grid) <- c(NA, 6, 2, 4)
  file <- file.path(fixture$test_dir, "known.tif")
  terra::writeRaster(grid, file)
  spec <- env$benchmark_artifact_specs("generate_DSM1", "lidR")[[1L]]
  checked <- env$benchmark_validate_raster(file, spec, tile, manifest, cloud, TRUE)
  expect_equal(checked$valid_values, 3L)
  expect_equal(checked$max_absolute_error, 0)
  cloud$Z <- rep(1, 3)
  empty <- sf::st_sf(Z = numeric(), geometry = sf::st_sfc(
    sf::st_point(c(0, 0)), crs = 32618)[FALSE])
  file <- file.path(fixture$test_dir, "empty.gpkg")
  sf::st_write(empty, file, quiet = TRUE)
  checked <- env$benchmark_validate_vector(file, cloud, tile, manifest, TRUE)
  expect_equal(checked$valid_values, 0L)
  grid[] <- NA_real_
  terra::writeRaster(grid, file.path(fixture$test_dir, "empty.tif"))
  expect_true(all(is.na(terra::values(terra::rast(
    file.path(fixture$test_dir, "empty.tif"))))))
})

test_that("native structural success never certifies unverified numerical conventions", {
  fixture <- validation_fixture()
  env <- fixture$env
  run <- validation_run(fixture, "pixel_metrics_1a", "lidR")
  for (id in run$manifest$tiles$id) {
    expect_true(file.rename(file.path(run$output, paste0(id, ".tif")),
                            file.path(run$output, paste0(id, "_avg.tif"))))
  }
  checked <- env$check_results(
    run$output, run$manifest, "pixel_metrics_1a", "lastools", run$execution)
  expect_equal(checked$expected_output_file_count, 2L)
  expect_identical(checked$validation_status, "unverified")
  expect_match(checked$validation_diagnostic, "native numerical/ownership")
  warned <- run$execution
  warned$exit_status <- 0L
  warned$diagnostic <- "WARNING: child process reported a cutoff warning."
  checked <- env$check_results(
    run$output, run$manifest, "pixel_metrics_1a", "lastools", warned)
  expect_identical(checked$validation_status, "failed")
  expect_match(checked$validation_diagnostic, "child process")
  accepted <- run$execution
  accepted$exit_status <- 1L
  accepted$execution_warning_exception <- "lascanopy_all_values_above_height_cutoff"
  accepted$accepted_execution_warnings <- "Recorded exact cutoff warning block."
  checked <- env$check_results(
    run$output, run$manifest, "pixel_metrics_1a", "lastools", accepted)
  expect_identical(checked$validation_status, "failed")
  expect_match(checked$validation_diagnostic, "not authorized")
  manifest <- run$manifest
  manifest$accept_known_lastools_cutoff_warning <- TRUE
  checked <- env$check_results(run$output, manifest, "pixel_metrics_1a", "lastools", accepted)
  expect_identical(checked$validation_status, "unverified")
  report <- attr(checked, "validation_report")
  expect_identical(report$exit_status, 1L)
  expect_identical(report$accepted_execution_warnings, accepted$accepted_execution_warnings)
  expect_identical(report$execution_warning_exception, accepted$execution_warning_exception)
  accepted$exit_status <- 3L
  expect_identical(env$check_results(
    run$output, manifest, "pixel_metrics_1a", "lastools", accepted)$validation_status, "failed")
  accepted$exit_status <- 1L
  unlink(file.path(run$output, paste0(manifest$tiles$id[1L], "_avg.tif")))
  missing <- env$check_results(run$output, manifest, "pixel_metrics_1a", "lastools", accepted)
  expect_identical(missing$validation_status, "failed")
  expect_match(missing$validation_diagnostic, "inventory mismatch")
})

test_that("native percentile inventory uses the observed zero-padded fifth percentile", {
  fixture <- validation_fixture()
  env <- fixture$env
  run <- validation_run(fixture, "pixel_metrics_2", "lidR")
  specs <- env$benchmark_artifact_specs("pixel_metrics_2", "lastools")
  for (id in run$manifest$tiles$id) {
    file <- file.path(run$output, paste0(id, ".tif"))
    raster <- terra::rast(file)
    for (i in seq_along(specs)) {
      terra::writeRaster(raster[[i]], file.path(run$output, paste0(id, specs[[i]]$suffix)))
    }
    unlink(file)
  }
  check <- function(execution = run$execution) env$check_results(
    run$output, run$manifest, "pixel_metrics_2", "lastools", execution)
  expect_identical(check()$validation_status, "unverified")
  failed <- run$execution
  failed$execution_status <- "failed"
  failed$diagnostic <- "LAStools exited with status 1."
  expect_identical(check(failed)$validation_status, "failed")
  expect_true(file.rename(file.path(run$output, "tile1_p05.tif"),
                          file.path(run$output, "tile1_p5.tif")))
  expect_match(check()$validation_diagnostic, "Missing: tile1_p05.tif")
  expect_match(check()$validation_diagnostic, "unexpected: tile1_p5.tif")
})

test_that("terrain references construct support from the catalog ground population", {
  fixture <- validation_fixture()
  for (file in list.files(fixture$input, full.names = TRUE)) {
    cloud <- lidR::readLAS(file)
    corners <- cloud$X %in% range(cloud$X) & cloud$Y %in% range(cloud$Y)
    cloud$Classification[corners] <- 1L
    cloud$Z[corners] <- 50
    lidR::writeLAS(cloud, file)
  }
  run <- validation_run(fixture, "generate_DEM", "lidR")
  expect_identical(run$checked$validation_status, "passed",
                   info = run$checked$validation_diagnostic)
  for (i in seq_len(nrow(run$manifest$tiles))) {
    cloud <- fixture$env$benchmark_validation_points(
      run$manifest, run$manifest$tiles[i, ], "generate_DEM", "lidR")
    expect_true(all(cloud$Classification %in% c(2L, 9L)))
  }
})

test_that("catalog references preserve primary-first reader order and half-open halos", {
  fixture <- validation_fixture()
  env <- fixture$env
  for (file in list.files(fixture$input, full.names = TRUE)) {
    cloud <- lidR::readLAS(file)
    data <- as.data.frame(cloud@data)
    duplicate <- data[1L, ]
    duplicate$Z <- 25
    data <- rbind(data, duplicate)
    if (basename(file) == "tile2.las") {
      edge <- duplicate
      edge$X <- 59
      edge$Y <- 20
      data <- rbind(data, edge)
    }
    lidR::writeLAS(lidR::LAS(data, header = cloud@header, crs = sf::st_crs(cloud)),
                   file, index = TRUE)
  }
  ctg <- env$prepare_input(fixture$input, fixture$root)
  manifest <- env$benchmark_validation_manifest(ctg)
  tile <- manifest$tiles[2L, ]
  files <- c(tile$filename, manifest$tiles$filename[1L])
  filter <- paste("-inside", tile$Min.X - 20, tile$Min.Y - 20,
                   tile$Max.X + 20, tile$Max.Y + 20)
  expected <- lidR::readLAS(files, select = "xyzr", filter = filter)
  actual <- env$benchmark_validation_points(manifest, tile, "generate_DSM2", "lidR")
  expect_identical(actual@data, expected@data)
  expect_identical(actual@header, expected@header)
  expect_true(all(actual$X < tile$Max.X + 20 & actual$Y < tile$Max.Y + 20))
  tile <- manifest$tiles[1L, ]
  actual <- env$benchmark_validation_points(manifest, tile, "generate_DSM2", "lidR")
  expect_false(any(actual$X == tile$Max.X + 20))
  run <- validation_run(fixture, "generate_DSM2", "lidR")
  expect_identical(run$checked$validation_status, "passed",
                   info = run$checked$validation_diagnostic)
  read <- lidR::readLAS
  testthat::local_mocked_bindings(readLAS = function(files, select = "*", filter = "") {
    cloud <- read(files, select = select, filter = filter)
    if (nzchar(filter)) cloud <- cloud[-1L]
    cloud
  }, .package = "lidR")
  expect_error(env$benchmark_validation_points(manifest, tile, "generate_DSM2", "lidR"),
               "differs from full input payload")
})

test_that("treetop references retain maximum boundaries and restore caller threads", {
  fixture <- validation_fixture()
  env <- fixture$env
  for (file in list.files(fixture$input, full.names = TRUE)) {
    cloud <- lidR::readLAS(file)
    corner <- cloud$X == max(cloud$X) & cloud$Y == max(cloud$Y)
    cloud$Z[corner] <- 50
    lidR::writeLAS(cloud, file)
  }
  run <- validation_run(fixture, "tree_detection", "lidR")
  expect_identical(run$checked$validation_status, "passed",
                   info = run$checked$validation_diagnostic)
  trees <- sf::st_read(file.path(run$output, "tile2.gpkg"), quiet = TRUE)
  xyz <- sf::st_coordinates(trees)
  tile <- run$manifest$tiles[2L, ]
  expect_true(any(xyz[, "X"] == tile$Max.X & xyz[, "Y"] == tile$Max.Y))
  previous <- lidR::get_lidr_threads()
  withr::defer(lidR::set_lidr_threads(previous))
  lidR::set_lidr_threads(2L)
  caller_threads <- lidR::get_lidr_threads()
  validate <- env$benchmark_validate_vector
  env$benchmark_validate_vector <- function(...) {
    expect_identical(lidR::get_lidr_threads(), 1L)
    validate(...)
  }
  checked <- env$check_results(
    run$output, run$manifest, "tree_detection", "lidR", run$execution)
  expect_identical(checked$validation_status, "passed")
  expect_identical(lidR::get_lidr_threads(), caller_threads)
  expect_error(env$benchmark_validate_artifacts(
    run$output, run$manifest, "unknown", "lidR"), "Unknown workload")
  expect_identical(lidR::get_lidr_threads(), caller_threads)
})

test_that("validation persists outside timing, protects report bytes and controls resume", {
  fixture <- validation_fixture()
  env <- fixture$env
  reports <- file.path(fixture$root, "reports")
  scratch <- file.path(fixture$root, "scratch")
  dir.create(reports)
  dir.create(scratch)
  config <- list(input_roots = c(SSD = fixture$input),
                 scratch_roots = c(SSD = scratch), reports_path = reports,
                 tools = "lidR", cores = 1L, workstation_id = "validation-fixture",
                 resource_monitoring = FALSE)
  tasks <- env$benchmark_tasks[env$benchmark_tasks$task_id == "pixel_metrics_1a", ]
  runs <- data.frame(ID = "ALS_1000_1_1_SSD_SSD", cores = 1L,
                     dir_in_org = fixture$input, scratch_root = scratch)
  campaign <- fixture_campaign(env, tasks, runs, config, "validation")
  run <- function() env$benchmark_run(
    tasks$task_id, runs$ID, benchmark_tasks = tasks, benchmark_runs = runs,
    sysinfo = list(lidR_version = "4.3.3"), config = config, campaign = campaign)
  run()
  records <- env$benchmark_read_campaign(reports, "validation")
  expect_identical(records$validation_status, "passed")
  expect_identical(records$execution_status, "succeeded")
  expect_equal(records$success, 1)
  report_file <- file.path(campaign$path, "attempts", records$attempt_id, "validation.rds")
  report <- readRDS(report_file)
  expect_gte(report$started_at, records$finish_time)
  expect_equal(report$expected_file_count, 2L)
  expect_equal(nrow(report$outputs), 2L)
  expect_length(list.files(scratch), 0L)
  run()
  expect_equal(nrow(env$benchmark_read_campaign(reports, "validation")), 1L)
  report$diagnostic <- "tampered"
  saveRDS(report, report_file)
  expect_error(env$benchmark_read_campaign(reports, "validation"), "Validation report checksum")

  tasks$task_call_lidR <- paste0(
    "writeLines('corrupt', file.path(dir_out, 'tile1.tif')); ",
    "writeLines('corrupt', file.path(dir_out, 'tile2.tif'))")
  campaign <- fixture_campaign(env, tasks, runs, config, "invalid")
  run()
  run()
  records <- env$benchmark_read_campaign(reports, "invalid")
  expect_equal(nrow(records), 2L)
  expect_true(all(records$execution_status == "succeeded"))
  expect_true(all(records$success == 1))
  expect_true(all(records$validation_status == "failed"))
  compiled <- env$compile_execution_outcomes(records)
  expect_true(all(!compiled$validated_success))
  expect_length(list.files(scratch), 0L)
})
