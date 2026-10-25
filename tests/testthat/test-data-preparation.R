petawawa_fixture <- function(envir = parent.frame()) {
  withr::local_dir(repo, .local_envir = envir)
  directory <- withr::local_tempdir(.local_envir = envir)
  input <- file.path(directory, "source tiles")
  output <- file.path(directory, "prepared tiles")
  bin <- file.path(directory, "native tools")
  for (path in c(input, output, bin)) dir.create(path)
  env <- new.env(parent = globalenv())
  sys.source("R/2_data.R", env)
  tiles <- c("298000_5093000", "298000_5094000")
  for (tile in tiles) {
    xy <- as.numeric(strsplit(tile, "_", fixed = TRUE)[[1L]])
    las <- lidR::LAS(data.frame(
      X = xy[1L] + rep(c(0, 1, 2), 20),
      Y = xy[2L] + rep(rep(c(0, 2), each = 3), 10),
      Z = seq_len(60) / 10 + 100,
      gpstime = seq_len(60) / 100,
      Intensity = seq_len(60),
      ReturnNumber = rep(c(1L, 2L), 30),
      NumberOfReturns = rep(2L, 60),
      Classification = rep(c(1L, 2L), 30),
      PointSourceID = rep(1L, 60)
    ))
    sf::st_crs(las) <- 32618
    lidR::writeLAS(las, file.path(input, paste0(tile, ".las")))
  }
  rscript <- file.path(R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  suffix <- if (.Platform$OS.type == "windows") ".exe" else ""
  for (tool in c("lassort64", "lasindex64")) {
    expect_true(file.copy(rscript, file.path(bin, paste0(tool, suffix))))
  }
  # Stub only the native processes, not LAS sampling, IO, validation or resume.
  env$petawawa_process <- function(executable, args) {
    if (!identical(args, "-version")) {
      source <- args[match("-i", args) + 1L]
      if ("-o" %in% args) {
        destination <- args[match("-o", args) + 1L]
        las <- lidR::readLAS(source)
        rows <- order(las@data$PointSourceID, las@data$gpstime)
        lidR::writeLAS(las[rows], destination)
      } else {
        writeLines("synthetic index stand-in", sub("\\.laz$", ".lax", source))
      }
    }
    list(status = 0L, stdout = "fixture native tool", stderr = "")
  }
  config <- list(
    source_root = input, output_root = output, source_citation = "Synthetic fixture",
    license = "Synthetic fixture, no external source", crs = 32618, repaired = TRUE,
    repair_provenance = "Constructed valid return attributes", lastools_path = bin,
    executables = c(sort = "lassort64", index = "lasindex64"),
    tool_license = "Rscript stand-ins; no native geodata processing",
    tiles = tiles, seed = 20261021L
  )
  list(env = env, config = config, directory = directory)
}

test_that("recovered selection and approved pilot match the historical tile index", {
  withr::local_dir(repo)
  env <- new.env(parent = globalenv())
  expect_no_warning(sys.source("R/2_data.R", env))
  ids <- env$petawawa_tiles()
  expect_length(ids, 100L)
  expect_true(all(c("298000_5093000", "298000_5094000") %in% ids))
  expect_false(any(grepl("Download_LAZ", readLines("R/2_data.R"), fixed = TRUE)))
  expect_false(any(grepl("download.file|curl_download|system\\(",
                        readLines("R/2_data.R"))))
})

test_that("preflight requires source metadata, repaired inputs and explicit scope", {
  f <- petawawa_fixture()
  e <- f$env
  c <- f$config
  expect_identical(e$petawawa_preflight(c)$densities, c(1, 2, 5, 10, 20, 50))
  for (field in c("license", "repair_provenance", "source_citation", "tool_license")) {
    bad <- c
    bad[[field]] <- NA_character_
    expect_error(e$petawawa_preflight(bad), paste("confirmed", field))
  }
  bad <- c
  bad$crs <- 4326
  expect_error(e$petawawa_preflight(bad), "projected source CRS")
  bad <- c
  bad$repaired <- FALSE
  expect_error(e$petawawa_preflight(bad), "Already repaired")
  bad <- c
  bad$tiles <- e$petawawa_tiles()
  expect_error(e$petawawa_preflight(bad), "approved neighboring pair")
  expect_error(e$petawawa_preflight(c, full = TRUE), "all 100")
  bad <- c
  bad$output_root <- c$source_root
  expect_error(e$petawawa_preflight(bad), "disjoint")
  bad <- c
  bad$seed <- .Machine$integer.max
  expect_error(e$petawawa_preflight(bad), "seed")
  bad <- c
  bad$source_root <- file.path(f$directory, "missing")
  expect_error(e$petawawa_preflight(bad), "source_root")
  file.copy(file.path(c$source_root, paste0(c$tiles[1], ".las")),
            file.path(c$source_root, paste0(c$tiles[1], ".laz")))
  expect_error(e$petawawa_preflight(c), "exactly one")
})

test_that("seeded thinning and recovered Z-only densification preserve point attributes", {
  f <- petawawa_fixture()
  e <- f$env
  las <- lidR::readLAS(file.path(f$config$source_root, paste0(f$config$tiles[1], ".las")))
  las <- e$petawawa_validate_source(las, f$config$tiles[1], 32618)
  expect_equal(lidR::area(las), 4)
  set.seed(42)
  before <- .Random.seed
  kind <- RNGkind()
  for (density in c(1, 2, 5, 10, 20, 50)) {
    variant <- e$petawawa_variant(las, density, 20261021L)
    expect_true(identical(.Random.seed, before))
    expect_identical(RNGkind(), kind)
    expect_equal(variant@data, e$petawawa_variant(las, density, 20261021L)@data)
    expect_equal(lidR::npoints(variant), density * 4)
    expect_equal(sf::st_crs(variant), sf::st_crs(las))
    originals <- as.data.frame(las@data)
    points <- as.data.frame(variant@data)
    rows <- match(points$Intensity, originals$Intensity)
    unchanged <- setdiff(names(points), c("Z", "gpstime"))
    reference <- originals[rows, unchanged]
    rownames(reference) <- NULL
    expect_equal(points[unchanged], reference)
    dz <- points$Z - originals$Z[rows]
    dt <- points$gpstime - originals$gpstime[rows]
    if (density <= 10) {
      expect_equal(dz, rep(0, nrow(points)))
      expect_equal(dt, rep(0, nrow(points)))
    } else {
      expect_equal(sum(abs(dz) < 1e-9), 60)
      expect_true(all(round(abs(dz) * 100) %in% c(0, seq(10, 50, 5))))
      expect_equal(dt[abs(dz) > 1e-9],
                   rep(.013 / (density / 15 - 1), sum(abs(dz) > 1e-9)))
    }
  }
  expect_false(identical(e$petawawa_variant(las, 1, 1)@data,
                        e$petawawa_variant(las, 1, 2)@data))
  dense <- e$petawawa_variant(las, 50, 1)
  expect_error(e$petawawa_variant(dense, 20, 1),
               "rounded density")
  fractional <- las[-seq_len(5)]
  expect_equal(lidR::density(fractional), 13.75)
  for (density in c(20, 50)) {
    multiplier <- density / 14 - 1
    count <- 55 * (1 + floor(multiplier)) +
      round((multiplier - floor(multiplier)) * 55)
    expect_equal(lidR::npoints(e$petawawa_variant(fractional, density, 1)), count)
  }
  wrong <- las
  wrong@data$ReturnNumber[1] <- 0L
  expect_error(e$petawawa_validate_source(wrong, f$config$tiles[1], 32618),
               "not repaired")
  expect_error(e$petawawa_validate_source(las, f$config$tiles[2], 32618),
               "outside")
  expect_error(e$petawawa_validate_source(las, f$config$tiles[1], 26918),
               "CRS differs")
})

test_that("preparation persists provenance and safely resumes only matching intact outputs", {
  f <- petawawa_fixture()
  e <- f$env
  c <- f$config
  source_files <- list.files(c$source_root, full.names = TRUE)
  source_hashes <- e$petawawa_checksum(source_files)
  expect_no_warning(plan <- e$prepare_petawawa(c))
  manifest <- file.path(c$output_root, "preparation.rds")
  expect_identical(readRDS(manifest), plan)
  expect_identical(plan$sources$md5, source_hashes)
  expect_equal(nrow(plan$tools), 2L)
  expect_length(plan$implementation, 4L)
  outputs <- list.files(c$output_root, pattern = "\\.laz$", full.names = TRUE,
                         recursive = TRUE)
  expect_length(outputs, 12L)
  hashes <- e$petawawa_checksum(outputs)
  for (output in outputs) {
    record <- readRDS(paste0(output, ".rds"))
    expect_equal(record$summary$points, record$target_density * 4)
    expect_equal(record$source_area_m2, 4)
    expect_equal(record$points_per_source_m2, record$target_density)
    expect_identical(record$md5, e$petawawa_checksum(c(
      output, sub("\\.laz$", ".lax", output))))
  }
  expect_no_warning(e$prepare_petawawa(c))
  expect_identical(e$petawawa_checksum(outputs), hashes)
  expect_identical(e$petawawa_checksum(source_files), source_hashes)
  changed <- c
  changed$seed <- c$seed + 1L
  expect_error(e$prepare_petawawa(changed), "identity changed")
  writeLines("corrupt", outputs[1])
  expect_error(e$prepare_petawawa(c), "checksum mismatch")
  expect_false(any(grepl("^lidar-run-", list.files(c$output_root, all.files = TRUE))))
})

test_that("native failures, missing indexes and partial outputs cannot pass validation", {
  f <- petawawa_fixture()
  e <- f$env
  plan <- e$petawawa_preflight(f$config)
  process <- e$petawawa_process
  e$petawawa_process <- function(executable, args) {
    if ("-o" %in% args) process(executable, args) else
      list(status = 0L, stdout = "", stderr = "")
  }
  expect_error(e$petawawa_prepare_tile(plan, plan$sources[1, ], 1, 10),
               "Index was not created")
  e$petawawa_process <- function(...) stop("synthetic native failure")
  expect_error(e$petawawa_prepare_tile(plan, plan$sources[1, ], 1, 10),
               "synthetic native failure")
  expect_false(any(grepl("^lidar-run-", list.files(f$config$output_root, all.files = TRUE))))
  expect_length(list.files(f$config$output_root, pattern = "\\.laz$",
                           recursive = TRUE), 0L)
  folder <- file.path(f$config$output_root, "1_org", "tile_1000_density_1")
  output <- file.path(folder, paste0(f$config$tiles[1], ".laz"))
  writeLines("untracked output", output)
  expect_error(e$petawawa_prepare_tile(plan, plan$sources[1, ], 1, 10), "partial output")
  las <- lidR::readLAS(plan$sources$path[1])
  expect_error(e$petawawa_validate_output(plan$sources$path[2],
                                         e$petawawa_variant(las, 1, 10)),
               "count, attributes or CRS")
  modified <- lidR::LAS(as.data.frame(las@data), header = las@header,
                        crs = sf::st_crs(las))
  modified@data$Intensity[1L] <- modified@data$Intensity[1L] + 1L
  modified_path <- file.path(f$directory, "modified.las")
  lidR::writeLAS(modified, modified_path)
  expect_error(e$petawawa_validate_output(modified_path, las), "attribute changed")
  reversed_path <- file.path(f$directory, "reversed.las")
  lidR::writeLAS(las[rev(seq_len(lidR::npoints(las)))], reversed_path)
  expect_error(e$petawawa_validate_output(reversed_path, las), "not sorted")
  e$benchmark_process <- function(...) list(status = 7L, stdout = "out", stderr = "err")
  original <- new.env(parent = globalenv())
  sys.source("R/2_data.R", original)
  original$benchmark_process <- e$benchmark_process
  expect_error(original$petawawa_process("stand-in", character()), "7.*\nout\nerr")
})
