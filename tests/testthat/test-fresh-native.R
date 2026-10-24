test_that("an isolated native indexed read avoids lassort filtered-count failures", {
  bin <- Sys.getenv("LIDAR_FRESH_LASTOOLS_PATH")
  if (!nzchar(bin)) skip("Set LIDAR_FRESH_LASTOOLS_PATH for tiny native sort/index/query checks.")
  withr::local_dir(repo)
  directory <- withr::local_tempdir()
  env <- new.env(parent = globalenv())
  sys.source("R/prepare_fresh_data.R", env)
  tools <- data.frame(
    name = c("sort", "index"),
    path = vapply(c("lassort64", "lasindex64"), function(tool) {
      env$benchmark_lastools_executable(bin, tool)
    }, character(1))
  )
  xy <- expand.grid(X = 298000 + seq(0, 3.5, .25),
                    Y = 5093000 + seq(0, 3.5, .25))
  las <- lidR::LAS(data.frame(
    X = xy$X, Y = xy$Y, Z = rep(100, nrow(xy)), gpstime = seq_len(nrow(xy)) / 10,
    ReturnNumber = rep(1L, nrow(xy)), NumberOfReturns = rep(1L, nrow(xy)),
    Classification = rep(2L, nrow(xy)), PointSourceID = rep(1L, nrow(xy))
  ))
  sf::st_crs(las) <- 2959
  source <- file.path(directory, "native-source.laz")
  sorted <- file.path(directory, "native-sorted.laz")
  bad <- file.path(directory, "native-filter-without-extra-pass.laz")
  lidR::writeLAS(las, source)
  expected <- lidR::readLAS(source)
  env$petawawa_process(tools$path[1], c(
    "-i", source, "-o", sorted, "-olaz", "-point_source", "-gps_time", "-fail"))
  env$petawawa_process(tools$path[2], c("-i", sorted))
  result <- env$benchmark_process(tools$path[1], c(
    "-i", sorted, "-inside", "298000", "5093000", "298001", "5093001",
    "-o", bad, "-olaz", "-point_source", "-gps_time", "-fail"))
  expect_true(result$status != 0L)
  expect_match(paste(result$stdout, result$stderr), "written .* points but expected")
  query <- env$fresh_validate_index(sorted, expected, tools)
  expect_identical(query$status, "passed")
  expect_true(query$points > 0L && query$points < lidR::npoints(expected))
  expect_false(grepl("\\b(warning|error)\\b",
    paste(query$process$stdout, query$process$stderr), ignore.case = TRUE))
  writeBin(c(charToRaw("LASX"), as.raw(rep(0L, 20L))), sub("\\.laz$", ".lax", sorted))
  expect_error(env$fresh_validate_index(sorted, expected, tools),
               "index query reported|Preparation process failed")
})
