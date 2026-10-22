chat7_fixture <- function(envir = parent.frame(), sparse = FALSE, fractional = FALSE) {
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

chat7_run <- function(fixture, task, tool) {
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

test_that("input-derived artifacts and numerical references validate neighboring R tiles", {
  fixture <- chat7_fixture()
  for (task in c("pixel_metrics_1a", "pixel_metrics_2")) {
    for (tool in c("lidR", "lasR")) {
      run <- chat7_run(fixture, task, tool)
      expect_identical(run$checked$validation_status, "passed",
                       info = paste(task, tool, run$checked$validation_diagnostic))
      expect_equal(run$checked$expected_output_file_count, 2L)
      expect_length(attr(run$checked, "validation_report")$artifacts, 2L)
    }
  }
})

test_that("surface boundary discrepancies are rejected rather than cropped or filled", {
  fixture <- chat7_fixture()
  for (task in c("generate_DSM1", "generate_DSM2", "generate_DEM")) {
    for (tool in c("lidR", "lasR")) {
      run <- chat7_run(fixture, task, tool)
      if (task == "generate_DSM1" || tool == "lasR") {
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
  fixture <- chat7_fixture(fractional = TRUE)
  for (task in c("generate_DSM1", "generate_DSM2")) {
    run <- chat7_run(fixture, task, "lidR")
    expect_identical(run$checked$validation_status, "failed")
    expect_match(run$checked$validation_diagnostic, "extent")
  }
})

test_that("neighboring metric references cover unequal values on exact cell edges", {
  fixture <- chat7_fixture()
  for (file in list.files(fixture$input, full.names = TRUE)) {
    cloud <- lidR::readLAS(file)
    cloud$Z <- seq_len(lidR::npoints(cloud)) / 10 - 1
    lidR::writeLAS(cloud, file)
  }
  for (tool in c("lidR", "lasR")) {
    for (task in c("pixel_metrics_1a", "pixel_metrics_2")) {
      run <- chat7_run(fixture, task, tool)
      expect_identical(run$checked$validation_status, "passed",
                       info = paste(task, tool, run$checked$validation_diagnostic))
    }
  }
})

test_that("sparse cells retain exact NoData masks and singleton SD", {
  fixture <- chat7_fixture(sparse = TRUE)
  for (tool in c("lidR", "lasR")) {
    run <- chat7_run(fixture, "pixel_metrics_2", tool)
    expect_identical(run$checked$validation_status, "passed",
                     info = paste(tool, run$checked$validation_diagnostic))
    raster <- terra::rast(file.path(run$output, "tile1.tif"))
    values <- terra::values(raster)
    expect_true(anyNA(values[, "z_sd"]))
  }
})

test_that("missing corrupt partial shifted and wrong-band outputs are rejected independently", {
  fixture <- chat7_fixture()
  env <- fixture$env
  run <- chat7_run(fixture, "pixel_metrics_2", "lidR")
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
  fixture <- chat7_fixture()
  env <- fixture$env
  run <- chat7_run(fixture, "normalization", "lidR")
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
  fixture <- chat7_fixture()
  env <- fixture$env
  run <- chat7_run(fixture, "tree_detection", "lidR")
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
  expect_match(native$validation_diagnostic, "tile1_p5.tif")
  expect_match(native$validation_diagnostic, "tile1_p95.tif")
})

test_that("execution diagnostics false returns and input mutation cannot pass validation", {
  fixture <- chat7_fixture()
  env <- fixture$env
  run <- chat7_run(fixture, "generate_DSM1", "lasR")
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
  fixture <- chat7_fixture()
  env <- fixture$env
  run <- chat7_run(fixture, "pixel_metrics_1a", "lidR")
  for (id in run$manifest$tiles$id) {
    expect_true(file.rename(file.path(run$output, paste0(id, ".tif")),
                            file.path(run$output, paste0(id, "_avg.tif"))))
  }
  checked <- env$check_results(
    run$output, run$manifest, "pixel_metrics_1a", "lastools", run$execution)
  expect_equal(checked$expected_output_file_count, 2L)
  expect_identical(checked$validation_status, "unverified")
  expect_match(checked$validation_diagnostic, "native numerical/ownership")
})

test_that("validation persists outside timing, protects report bytes and controls resume", {
  fixture <- chat7_fixture()
  env <- fixture$env
  reports <- file.path(fixture$root, "reports")
  scratch <- file.path(fixture$root, "scratch")
  dir.create(reports)
  dir.create(scratch)
  config <- list(input_roots = c(SSD = fixture$input),
                 scratch_roots = c(SSD = scratch), reports_path = reports,
                 tools = "lidR", cores = 1L, workstation_id = "chat7-fixture",
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
