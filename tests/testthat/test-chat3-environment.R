test_that("the runtime and complete manifest are pinned with lasR provenance", {
  withr::local_dir(repo)
  lock <- renv::lockfile_read("renv.lock")
  expect_identical(as.character(getRversion()), "4.6.1")
  expect_identical(trimws(readLines(".R-version")), lock$R$Version)
  manifest <- read.dcf("DESCRIPTION")[1L, "Imports"]
  packages <- trimws(strsplit(manifest, ",")[[1L]])
  expect_true(all(packages %in% names(lock$Packages)))
  for (package in packages) {
    expect_true(requireNamespace(package, quietly = TRUE), info = package)
    expect_equal(utils::packageVersion(package),
                 package_version(lock$Packages[[package]]$Version), info = package)
  }
  expect_identical(lock$Packages$lasR$Version, "0.21.2")
  expect_match(lock$Packages$lasR$Repository, "r_lidar|r-lidar")
  expect_identical(lock$R$Repositories$r_lidar, "https://r-lidar.r-universe.dev")
  expect_identical(lock$Packages$lasR$RemoteSha,
                   "a9ec6a5d656f413a47551a024d2e370054f28f8d")
  expect_false(any(grepl("^R/", readLines(".renvignore"))))
  found <- renv::dependencies("R/0_setup.R", progress = FALSE)
  expect_true(all(c("lidR", "lasR", "tidyverse") %in% found$Package))
})

test_that("setup and supported pipeline APIs load without running benchmarks", {
  withr::local_dir(repo)
  withr::local_envvar(c(LIDAR_BENCHMARK_CONFIG = NA_character_))
  env <- new.env(parent = globalenv())
  suppressPackageStartupMessages(sys.source("R/0_setup.R", env))
  previous_theme <- ggplot2::theme_get()
  withr::defer(ggplot2::theme_set(previous_theme))
  withr::local_options(list(
    ggplot2.discrete.colour = getOption("ggplot2.discrete.colour"),
    ggplot2.discrete.fill = getOption("ggplot2.discrete.fill")
  ))
  sys.source("R/99_plotSettings.R", env)
  expect_equal(nrow(env$benchmark_tasks), 9L)
  expect_equal(nrow(env$benchmark_runs), 0L)
  expect_true(all(vapply(list.files("R", pattern = "\\.R$", full.names = TRUE),
                        function(path) is.expression(parse(path)), logical(1))))
  expect_true(is.function(lidR::tin()))
  expect_true(is.function(lidR::p2r()))
  expect_true(is.function(lidR::dsmtin()))
  expect_true(is.function(lidR::lmf(ws = 5)))
  env$fout <- tempfile(fileext = ".tif")
  env$dir_out <- tempdir()
  env$f <- character()
  env$capture_pipeline <- function(pipeline, ...) pipeline
  for (call in env$benchmark_tasks$task_call_lasR) {
    call <- sub("(lasR::)?exec\\(", "capture_pipeline(", call)
    expect_no_warning(pipeline <- eval(parse(text = call), env))
    expect_true(inherits(pipeline, "PipelinePtr"))
  }
})

test_that("public metric operations preserve the existing threshold and quantiles", {
  env <- benchmark_fixture()$env
  z <- c(-1, 0, 2, 2, 3, 4, 10)
  result <- env$metrics_multiple(z)
  expect_equal(result$pzabove2, 100 * 3 / 7)
  expect_equal(result$zmax, max(z))
  expect_equal(result$zmean, mean(z))
  expect_equal(result$zsd, sd(z))
  expect_equal(unlist(result[paste0("zq", c(5, 25, 50, 75, 95))]),
               setNames(quantile(z, c(.05, .25, .5, .75, .95)),
                        paste0("zq", c(5, 25, 50, 75, 95))))
})

test_that("catalog metadata uses exact counts and unrounded header densities", {
  fixture <- benchmark_fixture()
  env <- fixture$env
  directory <- fixture$test_dir
  for (i in 1:2) {
    n <- 1000L + i
    las <- lidR::LAS(data.frame(
      X = seq(0, 10, length.out = n) + (i - 1) * 20,
      Y = rep(c(0, 10), length.out = n),
      Z = rep(3, n),
      ReturnNumber = rep(c(1L, 2L), length.out = n),
      NumberOfReturns = rep(2L, n)
    ))
    sf::st_crs(las) <- 32618
    lidR::writeLAS(las, file.path(directory, paste0("tile", i, ".las")))
  }
  catalog <- lidR::readLAScatalog(directory, progress = FALSE)
  result <- env$benchmark_ALS_info(catalog, directory)
  expect_equal(result$total_point_count, 2003)
  expect_equal(result$density_points, 2003 / 200)
  expect_equal(result$density_pulses, 1002 / 200)
  expect_equal(result$file_count, 2L)
  expect_equal(as.numeric(result$input_data_size),
               sum(file.info(list.files(directory, full.names = TRUE))$size))
  expect_error(env$benchmark_ALS_info(methods::new("LAScatalog"), directory),
               "area must be finite and positive")
})

test_that("system metadata uses public interfaces and RAM in bytes", {
  withr::local_dir(repo)
  env <- benchmark_fixture()$env
  info <- env$system_info()
  expect_identical(info$sysname, unname(Sys.info()["sysname"]))
  expect_identical(info$lidR_version, as.character(packageVersion("lidR")))
  expect_identical(info$lasR_version, as.character(packageVersion("lasR")))
  expect_equal(info$ram, unname(ps::ps_system_memory()[["total"]]))
  expect_true(is.finite(info$ram) && info$ram > 0)
  expect_true(is.na(info$lastools_version))
})

test_that("environment checks report missing external runtimes actionably", {
  withr::local_dir(repo)
  env <- new.env(parent = globalenv())
  sys.source("R/check_environment.R", env)
  env$loadNamespace <- function(...) stop("GDAL shared library missing")
  expect_error(env$benchmark_check_environment(),
               "GDAL shared library missing")
  rm("loadNamespace", envir = env)
  expect_error(env$benchmark_check_environment(
    file.path(tempdir(), "nonexistent-quarto")
  ), "Quarto is missing: install Quarto 1.10.18")
  info <- env$benchmark_check_environment()
  expect_identical(info$Quarto, "1.10.18")
  expect_true(all(c("GDAL", "GEOS", "PROJ") %in% names(info$geospatial)))
})
