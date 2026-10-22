chat5_setup <- function(envir = parent.frame()) {
  withr::local_dir(repo, .local_envir = envir)
  withr::local_envvar(c(LIDAR_BENCHMARK_CONFIG = NA_character_), .local_envir = envir)
  env <- new.env(parent = globalenv())
  env$source <- function(file) sys.source(file.path(repo, file), env)
  suppressPackageStartupMessages(sys.source("R/0_setup.R", env))
  old <- get0("metrics_multiple", envir = globalenv(), inherits = FALSE)
  withr::defer({
    if (is.null(old)) rm("metrics_multiple", envir = globalenv()) else
      assign("metrics_multiple", old, envir = globalenv())
  }, envir = envir)
  assign("metrics_multiple", env$metrics_multiple, envir = globalenv())
  env
}

chat5_cloud <- function(data, envir = parent.frame()) {
  directory <- tempfile("chat5-")
  dir.create(directory)
  withr::defer(unlink(directory, recursive = TRUE), envir = envir)
  input <- file.path(directory, "input")
  dir.create(input)
  las <- lidR::LAS(data)
  sf::st_crs(las) <- 32618
  file <- file.path(input, "tile.las")
  lidR::writeLAS(las, file)
  list(directory = directory, input = input, file = file)
}

chat5_call <- function(env, fixture, task, tool) {
  output <- file.path(fixture$directory, paste(task, tool, sep = "-"))
  dir.create(output)
  context <- new.env(parent = env)
  context$dir_out <- output
  context$f <- fixture$file
  context$ctg <- env$prepare_input(fixture$input, output)
  context$fout <- file.path(output, paste0("*.", env$benchmark_tasks$output_format[
    match(task, env$benchmark_tasks$task_id)]))
  call <- env$benchmark_tasks[[paste0("task_call_", tool)]][[
    match(task, env$benchmark_tasks$task_id)]]
  result <- env$benchmark_execute_call(call, tool, context)
  expect_identical(result$execution_status, "succeeded", info = result$diagnostic)
  list(output = output, context = context)
}

test_that("metric definitions cover strict thresholds, type-7 quantiles and sparse cells", {
  env <- chat5_setup()
  z <- c(-1, 0, 2, 2, 3, 4, 10)
  expect_equal(unlist(env$metrics_multiple(z)),
               c(z_max = 10, z_mean = 20/7, z_sd = sqrt(269/21),
                 z_above2 = 3/7, z_p5 = -.7, z_p25 = 1,
                 z_p50 = 2, z_p75 = 3.5, z_p95 = 8.2))
  expect_true(all(is.na(unlist(env$metrics_multiple(numeric())))))
  singleton <- env$metrics_multiple(2)
  expect_true(is.na(singleton$z_sd))
  expect_equal(singleton$z_above2, 0)
  expect_equal(unlist(singleton[5:9]), setNames(rep(2, 5), names(singleton)[5:9]))
  expect_error(env$metrics_multiple(c(1, NA_real_)), "finite numeric")
  expect_error(env$metrics_multiple(c(1, Inf)), "finite numeric")
  expect_error(env$metrics_multiple("1"), "finite numeric")
})

test_that("every task/tool has a supported-artifact and comparability contract", {
  env <- chat5_setup()
  contracts <- env$benchmark_contracts
  expect_equal(nrow(contracts), 27L)
  expect_equal(sum(contracts$supported), 18L)
  for (i in seq_len(nrow(contracts))) {
    row <- contracts[i, ]
    task <- env$benchmark_tasks[env$benchmark_tasks$task_id == row$task_id, ]
    call <- task[[paste0("task_call_", row$tool)]][[1L]]
    expect_identical(!env$benchmark_call_is_empty(call), row$supported)
    expect_identical(length(row$artifacts[[1]]) > 0,
                     row$supported || row$comparison_status == "blocked")
    expect_identical(row$origin[[1]], c(0, 0))
    expect_equal(row$buffer, 20)
    expect_true(nzchar(row$comparison_note))
  }
  expect_true(all(contracts$comparison_status[contracts$tool == "lastools" &
                                              contracts$supported] == "non_comparable"))
  expect_true(all(is.na(contracts$comparison_group[
    contracts$comparison_status != "matched_definition_pending_validation"])))
  expect_false(any(grepl("\\bchm\\(", env$benchmark_tasks$task_call_lasR)))
  expect_equal(contracts$bands[[which(contracts$task_id == "pixel_metrics_2" &
                                      contracts$tool == "lidR")]],
               names(env$metrics_multiple(numeric())))
  expect_error(env$benchmark_workload_contract("missing", "lasR"), "Unknown workload")
})

test_that("actual R metric calls persist matching known-value bands and geometry", {
  env <- chat5_setup()
  withr::local_options(list(lidR.progress = FALSE))
  z <- c(-1, 0, 2, 2, 3, 4, 10)
  fixture <- chat5_cloud(data.frame(
    X = as.numeric(1:7), Y = as.numeric(1:7), Z = z,
    ReturnNumber = c(1L, 2L, 1L, 2L, 1L, 2L, 1L),
    NumberOfReturns = rep(2L, 7), Classification = c(2L, 9L, 1L, 1L, 1L, 1L, 1L)
  ))
  withr::defer(lasR::unset_exec_option())
  lasR::set_exec_options(ncores = lasR::sequential())
  for (task in c("pixel_metrics_1a", "pixel_metrics_2")) {
    rasters <- lapply(c("lidR", "lasR"), function(tool) {
      run <- chat5_call(env, fixture, task, tool)
      files <- list.files(run$output, "\\.tif$", full.names = TRUE)
      expect_length(files, 1L)
      terra::rast(files)
    })
    expected <- if (task == "pixel_metrics_2") unlist(env$metrics_multiple(z)) else
      c(z_mean = mean(z))
    for (r in rasters) {
      expect_equal(terra::res(r), c(20, 20))
      expect_equal(terra::origin(r), c(0, 0))
      expect_equal(unname(as.vector(terra::ext(r))), c(0, 20, 0, 20))
      expect_equal(terra::nlyr(r), length(expected))
      expect_equal(as.numeric(terra::values(r)), unname(expected), tolerance = 2e-6)
      expect_identical(names(r), names(expected))
    }
  }
})

test_that("native lasR sparse metrics use NoData SD and an above-2 fraction", {
  env <- chat5_setup()
  withr::defer(lasR::unset_exec_option())
  x <- data.frame(X = c(1, 21), Y = c(1, 21), Z = c(2, 3))
  bands <- names(env$metrics_multiple(numeric()))
  raster <- lasR::exec(lasR::rasterize(20, bands),
                      on = x, with = list(ncores = lasR::sequential(), buffer = 20,
                                          chunk = 0, progress = FALSE))
  values <- terra::values(raster)
  occupied <- !is.na(values[, "z_max"])
  expect_equal(sum(occupied), 2L)
  expect_true(all(is.na(values[, "z_sd"])))
  expect_equal(sort(values[occupied, "z_above2"]), c(0, 1))
  expect_true(all(is.na(values[!occupied, ])))
})

test_that("LAStools metrics explicitly include all Z and use density not first-return cover", {
  env <- chat5_setup()
  fixture <- chat5_cloud(data.frame(X = c(1, 2), Y = c(1, 2), Z = c(-10, 2)))
  ctg <- env$prepare_input(fixture$input, fixture$directory)
  expect_equal(lidR::opt_chunk_buffer(ctg), 20)
  expect_equal(lidR::opt_chunk_size(ctg), 0)
  expect_equal(lidR::opt_chunk_alignment(ctg), c(0, 0))
  expect_false(lidR::opt_wall_to_wall(ctg))
  expect_identical(ctg@output_options$drivers$sf$extension, ".gpkg")
  expect_lt(as.numeric(env$benchmark_height_cutoff(ctg)), -10)
  ctg@data$Min.Z <- NA_real_
  expect_error(env$benchmark_height_cutoff(ctg), "finite catalog Min.Z")
  for (task in c("pixel_metrics_1a", "pixel_metrics_2")) {
    args <- env$benchmark_tasks$task_call_lastools[[
      match(task, env$benchmark_tasks$task_id)]]$args
    expect_identical(args[match("-height_cutoff", args) + 1L], "{height_cutoff}")
    expect_identical(args[match("-step", args) + 1L], "20")
    expect_true("-use_orig_bb" %in% args)
    expect_false("-cov" %in% args)
    if (task == "pixel_metrics_2") {
      expect_true(all(c("-dns", "-fractions", "-cover_cutoff") %in% args))
      expect_identical(args[match("-cover_cutoff", args) + 1L], "2")
    }
  }
})

test_that("surface and tree calls persist explicit raster and vector products", {
  env <- chat5_setup()
  xy <- expand.grid(X = c(1, 19, 21, 39), Y = c(1, 19, 21, 39))
  data <- data.frame(xy, Z = 10, Classification = 2L,
                     ReturnNumber = 1L, NumberOfReturns = 1L)
  data$Classification[1] <- 9L
  data <- rbind(data, data.frame(X = c(10, 30, 10), Y = c(10, 30, 10),
                                 Z = c(15, 18, 30), Classification = 1L,
                                 ReturnNumber = c(1L, 1L, 2L), NumberOfReturns = 2L))
  fixture <- chat5_cloud(data)
  withr::defer(lasR::unset_exec_option())
  lasR::set_exec_options(ncores = lasR::sequential())
  for (task in c("generate_DSM1", "generate_DSM2")) {
    for (tool in c("lidR", "lasR")) {
      run <- chat5_call(env, fixture, task, tool)
      files <- list.files(run$output, "\\.tif$", full.names = TRUE)
      expect_length(files, 1L)
      raster <- terra::rast(files)
      expect_equal(terra::res(raster), c(1, 1))
      expect_equal(terra::origin(raster), c(0, 0))
      maximum <- max(terra::values(raster), na.rm = TRUE)
      if (task == "generate_DSM1") expect_equal(maximum, 30) else
        expect_lte(maximum, 18)
    }
  }
  for (tool in c("lidR", "lasR")) {
    run <- chat5_call(env, fixture, "generate_DEM", tool)
    files <- list.files(run$output, "\\.tif$", full.names = TRUE)
    expect_length(files, 1L)
    values <- terra::values(terra::rast(files))
    expect_gt(sum(is.finite(values)), 0L)
    expect_true(all(abs(values[is.finite(values)] - 10) < 1e-5))
  }
  normalized <- chat5_call(env, fixture, "normalization", "lidR")
  files <- list.files(normalized$output, "\\.laz$", full.names = TRUE)
  expect_length(files, 1L)
  points <- lidR::readLAS(files)
  owned <- data$X < max(data$X) & data$Y < max(data$Y)
  expect_equal(sort(points$Z), sort(data$Z[owned] - 10), tolerance = .0011)
  expect_equal(lidR::npoints(points), sum(owned))
  expect_equal(sort(points$X), sort(data$X[owned]))
  expect_equal(sort(points$Y), sort(data$Y[owned]))
  for (tool in c("lidR", "lasR")) {
    run <- chat5_call(env, fixture, "tree_detection", tool)
    trees <- list.files(run$output, "\\.gpkg$", full.names = TRUE)
    expect_length(trees, 1L)
    expect_gt(nrow(sf::st_read(trees, quiet = TRUE)), 0L)
    expect_length(list.files(run$output, "\\.shp$"), 0L)
  }
})

test_that("the all-Z cutoff placeholder expands without invoking native LAStools", {
  with_chat2_fixture({
    call_env$height_cutoff <- "-11"
    task_call$args <- c(task_call$args, "-height_cutoff", "{height_cutoff}")
    command <- env$benchmark_lastools_command(task_call, call_env)
    expect_identical(command$args[match("-height_cutoff", command$args) + 1L], "-11")
    result <- env$benchmark_execute_call(task_call, "lastools", call_env)
    expect_identical(result$exit_status, 7L)
    observed_args <- readRDS(observed)
    expect_identical(observed_args[match("-height_cutoff", observed_args) + 1L], "-11")
    expect_false(any(grepl("[{}]", observed_args)))
  })
})

test_that("blocked runner attempts retain the contract and native blocker diagnostic", {
  with_chat2_fixture({
    setup <- new.env()
    setup$source <- function(file) sys.source(file.path(repo, file), setup)
    setup$library <- function(...) invisible(NULL)
    sys.source(file.path(repo, "R/0_setup.R"), setup)
    run <- setup$benchmark_runs[1, ]
    dir.create(run$dir_in_org, recursive = TRUE)
    env$catalog <- function(...) stop("Blocked attempts must not read point clouds")
    env$prepare_input <- function(...) stop("Blocked attempts must not prepare inputs")
    before <- list.files(scratch, all.files = TRUE)
    env$benchmark_run(
      task_id = "normalization", run_id = run$ID,
      run_lidR = FALSE, run_lasR = TRUE, run_lastools = FALSE,
      benchmark_tasks = setup$benchmark_tasks, benchmark_runs = setup$benchmark_runs,
      sysinfo = list(lasR_version = "0.21.2"), config = config
    )
    files <- list.files(reports, "\\.csv$", full.names = TRUE)
    expect_length(files, 1L)
    result <- read.csv(files)
    expect_identical(result$execution_status, "skipped")
    expect_equal(result$success, 0)
    expect_identical(result$workload_contract, "chat5-v1")
    expect_identical(result$comparison_status, "blocked")
    expect_true(is.na(result$comparison_group))
    expect_match(result$diagnostic, "ground hull boundaries")
    expect_identical(list.files(scratch, all.files = TRUE), before)
  })
})

test_that("the pinned normalization defect is characterized and affected calls cannot run", {
  env <- chat5_setup()
  xy <- expand.grid(X = c(1, 19, 21, 39), Y = c(1, 19, 21, 39))
  data <- data.frame(xy, Z = 10, Classification = 2L)
  result <- lasR::exec(
    lasR::normalize() + lasR::callback(function(x) x, expose = "xyz",
                                      no_las_update = TRUE),
    on = data, with = list(ncores = lasR::sequential(), buffer = 20,
                          chunk = 0, progress = FALSE)
  )
  expect_true(anyNA(result$Z))
  interior <- result$X > 1 & result$X < 39 & result$Y > 1 & result$Y < 39
  expect_equal(result$Z[interior], rep(0, sum(interior)))
  for (task in c("normalization", "complex_task1", "complex_task2")) {
    row <- env$benchmark_tasks[env$benchmark_tasks$task_id == task, ]
    contract <- env$benchmark_workload_contract(task, "lasR")
    expect_identical(contract$comparison_status, "blocked")
    expect_false(contract$supported)
    expect_match(contract$comparison_note, "ground hull boundaries")
    expect_true(nzchar(row$task_candidate_lasR))
    expect_identical(env$benchmark_execute_call(row$task_call_lasR, "lasR")$execution_status,
                     "skipped")
    if (startsWith(task, "complex_")) {
      candidate <- row$task_candidate_lasR
      expect_match(candidate, "del \\+ dtm \\+ norm \\+ write \\+ chm")
      expect_match(candidate, "keep_ground_and_water")
      if (task == "complex_task2") {
        expect_match(candidate, "ofile = fout_trees", fixed = TRUE)
        expect_match(candidate, "*_metrics.tif", fixed = TRUE)
        expect_length(contract$artifacts, 5L)
      } else expect_length(contract$artifacts, 3L)
    }
  }
})
