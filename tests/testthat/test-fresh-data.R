fresh_fixture <- function(envir = parent.frame()) {
  withr::local_dir(repo, .local_envir = envir)
  directory <- withr::local_tempdir(.local_envir = envir)
  env <- new.env(parent = globalenv())
  sys.source("R/prepare_fresh_data.R", env)
  original_prepare <- env$fresh_prepare
  env$fresh_prepare <- function(..., cores = 1L) original_prepare(..., cores = 1L)
  config <- dget("fresh-data.example.R")
  config$metadata_confirmation <- "Synthetic metre fixture: EPSG:2959; vertical declaration EPSG:5713."
  config$root <- file.path(directory, "fresh")
  config$protected_roots <- file.path(directory, "historical")
  dir.create(config$protected_roots)
  config$tool_license <- "Synthetic fixtures; native tool calls are stubbed."
  config$lastools_path <- file.path(directory, "bin")
  dir.create(config$lastools_path)
  executable <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  for (tool in config$executables) {
    file.copy(executable, file.path(config$lastools_path,
      paste0(tool, if (.Platform$OS.type == "windows") ".exe" else "")))
  }
  env$petawawa_process <- function(executable, args) {
    if ("--query-index" %in% args) {
      position <- match("--query-index", args)
      env$fresh_query_index_child(args[seq.int(position + 1L, length(args))])
      return(list(status = 0L, stdout = "fixture supervised query", stderr = ""))
    }
    if (!identical(args, "-version")) {
      source <- args[match("-i", args) + 1L]
      if ("-o" %in% args) {
        las <- lidR::readLAS(source)
        if ("-inside" %in% args) {
          position <- match("-inside", args)
          rectangle <- as.numeric(args[position + seq_len(4L)])
          las <- las[las$X >= rectangle[1] & las$Y >= rectangle[2] &
                       las$X <= rectangle[3] & las$Y <= rectangle[4]]
        }
        lidR::writeLAS(las[order(las$PointSourceID, las$gpstime)],
                      args[match("-o", args) + 1L])
      } else {
        las <- lidR::readLAS(source)
        lidR::writeLAS(las, source, index = TRUE)
      }
    }
    list(status = 0L, stdout = "fixture native", stderr = "")
  }
  make_las <- function(tile = "298000_5093000") {
    origin <- as.numeric(strsplit(tile, "_", fixed = TRUE)[[1]])
    xy <- expand.grid(X = seq(.0001, 4.0001, length.out = 15),
                      Y = seq(.0001, 4.0001, length.out = 15))
    template <- lidR::LAS(data.frame(X = c(0, 1, 0), Y = c(0, 0, 1),
                                    Z = c(100, 100, 100), gpstime = c(.1, .2, .3)))
    header <- template@header
    header@PHB[["X scale factor"]] <- .0001
    header@PHB[["Y scale factor"]] <- .0001
    header@PHB[["Z scale factor"]] <- .0001
    header@PHB[["X offset"]] <- origin[1]
    header@PHB[["Y offset"]] <- origin[2]
    las <- lidR::LAS(data.frame(
      X = xy$X + origin[1], Y = xy$Y + origin[2],
      Z = rep(100.0001, nrow(xy)), gpstime = seq_len(nrow(xy)) / 10,
      Intensity = rep(10L, nrow(xy)), Synthetic_flag = rep(FALSE, nrow(xy)),
      ReturnNumber = rep(1L, nrow(xy)), NumberOfReturns = rep(1L, nrow(xy)),
      Classification = rep(2L, nrow(xy)), PointSourceID = rep(1L, nrow(xy))
    ), header = header, crs = sf::st_crs(2959), check = FALSE)
    sf::st_crs(las) <- 2959
    lidR::las_quantize(las)
    lidR::las_update(las)
  }
  list(env = env, config = config, directory = directory, make_las = make_las)
}

test_that("fresh roots refuse historical, nonempty, redirected and changed configurations", {
  f <- fresh_fixture()
  e <- f$env
  config <- f$config
  bad <- config
  bad$root <- config$protected_roots
  expect_error(e$fresh_root(bad), "protected")
  bad$root <- file.path(repo, "fresh")
  expect_error(e$fresh_root(bad), "protected")
  dir.create(config$root)
  writeLines("keep me", file.path(config$root, "sentinel"))
  expect_error(e$fresh_root(config), "empty")
  expect_identical(readLines(file.path(config$root, "sentinel")), "keep me")
  unlink(file.path(config$root, "sentinel"))
  config <- e$fresh_root(config)
  expect_identical(e$fresh_root(config), config)
  bad <- config
  bad$seed <- bad$seed + 1L
  expect_error(e$fresh_root(bad), "changed")
  bad <- config
  bad$archive_url <- "https://example.org/other.zip"
  expect_error(e$fresh_root(bad), "original Petawawa")
  bad <- config
  bad$metadata_confirmation <- NA_character_
  expect_error(e$fresh_root(bad), "delivery metadata")
  bad <- config
  bad$tool_license <- NA_character_
  expect_error(e$fresh_root(bad), "tool_license")
  expect_error(e$fresh_main(character()), "Usage")
  expect_error(e$fresh_archive(config), "--download")
})

test_that("fresh repair logs removals/count changes without changing retained coordinates", {
  f <- fresh_fixture()
  e <- f$env
  las <- f$make_las()
  las@data$ReturnNumber[1] <- 0L
  las@data$gpstime[3] <- las@data$gpstime[2]
  las@data$ReturnNumber[4] <- 2L
  las@data[5, c("X", "Y", "Z")] <- las@data[6, c("X", "Y", "Z")]
  before <- data.table::copy(las@data)
  fixed <- e$fresh_repair(las, "298000_5093000", 2959)
  expect_setequal(unique(fixed$actions$action), c(
    "drop_invalid_return", "drop_duplicate_XYZ",
    "drop_repeated_pulse_return", "raise_return_count"
  ))
  expect_identical(las@data, before)
  expect_identical(fixed$las@data[, c("X", "Y", "Z"), with = FALSE],
                   before[fixed$retained_source_rows, c("X", "Y", "Z"), with = FALSE])
  expect_true(all(fixed$las$ReturnNumber >= 1L))
  expect_true(all(fixed$las$NumberOfReturns >= fixed$las$ReturnNumber))
  expect_length(fixed$qc$check$errors, 0)
  expect_length(fixed$qc$check$warnings, 0)
  expect_identical(fixed$las@header@PHB[["X scale factor"]], .0001)
  invalid <- f$make_las()
  invalid@data$Z[1] <- NA_real_
  expect_error(e$fresh_repair(invalid, "298000_5093000", 2959), "nonfinite")
  invalid <- f$make_las()
  invalid@data$X[2] <- invalid@data$X[1]
  invalid@data$Y[2] <- invalid@data$Y[1]
  invalid@data$Z[2] <- invalid@data$Z[1] + 1
  repaired <- e$fresh_repair(invalid, "298000_5093000", 2959)
  removal <- repaired$actions[repaired$actions$action == "drop_ground_XY_higher_Z", ]
  expect_identical(removal$source_row, 2L)
  expect_identical(removal$kept_source_row, 1L)
  expect_identical(removal$source_Z, invalid$Z[2])
  expect_identical(removal$kept_Z, invalid$Z[1])
  expect_false(2L %in% repaired$retained_source_rows)
  expect_length(repaired$qc$check$warnings, 0)
  invalid <- f$make_las()
  invalid@data$Classification <- rep(1L, lidR::npoints(invalid))
  expect_error(e$fresh_repair(invalid, "298000_5093000", 2959), "terrain")
})

test_that("fresh variants hit exact density counts with clean deterministic synthetic copies", {
  f <- fresh_fixture()
  e <- f$env
  las <- f$make_las()
  before <- data.table::copy(las@data)
  set.seed(71)
  seed <- .Random.seed
  kind <- RNGkind()
  for (density in c(1, 2, 5, 10, 20, 50)) {
    variant <- e$fresh_variant(las, density, 20261022)
    expect_equal(lidR::npoints(variant), round(density * lidR::area(las)))
    expect_equal(variant@data, e$fresh_variant(las, density, 20261022)@data)
    expect_identical(.Random.seed, seed)
    expect_identical(RNGkind(), kind)
    expect_no_error(e$fresh_qc(variant, "298000_5093000", 2959))
    expect_identical(variant@header@PHB[["X scale factor"]], .0001)
    if (density > lidR::density(las)) {
      expect_equal(variant@data[seq_len(nrow(before))], before)
      extra <- variant@data[-seq_len(nrow(before))]
      expect_false(any(extra$Synthetic_flag))
      expect_true(all(extra$Classification == 2L))
      expect_true(min(extra$gpstime) > max(before$gpstime))
      expect_false(anyDuplicated(variant@data[, c("X", "Y"), with = FALSE]) > 0)
      expect_true(all(round(abs(extra$Z - 100.0001) * 100) %in% seq(10, 50, 5)))
    }
  }
  expect_identical(las@data, before)
  expect_false(identical(e$fresh_variant(las, 1, 1)@data,
                         e$fresh_variant(las, 1, 2)@data))
})

test_that("negative-Z source removal is audited before ground cleanup and retains zero", {
  f <- fresh_fixture()
  source <- file.path(f$directory, "source.laz")
  lidR::writeLAS(f$make_las(), source)
  las <- f$env$fresh_read(source, raw = TRUE)$las
  las@data$Z[1:4] <- c(-.0001, 100.0001, 0, .0001)
  las@data$X[2] <- las$X[1]
  las@data$Y[2] <- las$Y[1]
  original <- data.table::copy(las@data)
  repaired <- f$env$fresh_repair(las, "298000_5093000", 2959)
  removed <- repaired$actions[repaired$actions$action == "drop_negative_Z", ]
  expect_identical(removed$source_row, 1L)
  expect_identical(removed$source_Z, -.0001)
  expect_true(is.na(removed$kept_source_row))
  expect_true(all(2:4 %in% repaired$retained_source_rows))
  expect_identical(repaired$las@data, original[repaired$retained_source_rows])
  expect_identical(las@data, original)
  expect_true(all(repaired$las$Z >= 0))
  expect_no_error(f$env$fresh_qc(repaired$las, "298000_5093000", 2959))
  path <- file.path(f$directory, "nonnegative.laz")
  lidR::writeLAS(repaired$las, path)
  expect_no_error(f$env$fresh_validate_output(path, repaired$las))
  las@data$Z <- rep(-.0001, lidR::npoints(las))
  expect_error(f$env$fresh_repair(las, "298000_5093000", 2959), "removed every point")
})

test_that("ground XY cleanup retains the measured minimum before repeated-pulse selection", {
  f <- fresh_fixture()
  las <- f$make_las()
  las@data$X[2:4] <- las$X[1]
  las@data$Y[2:4] <- las$Y[1]
  las@data$Z[2:4] <- c(99.0001, 101.0001, 102.0001)
  las@data$gpstime[2:3] <- las$gpstime[1]
  las@data$Classification[4] <- 1L
  original <- data.table::copy(las@data)
  repaired <- f$env$fresh_repair(las, "298000_5093000", 2959)
  removed <- repaired$actions[repaired$actions$action == "drop_ground_XY_higher_Z", ]
  expect_identical(removed$source_row, c(1L, 3L))
  expect_identical(removed$kept_source_row, c(2L, 2L))
  expect_identical(removed$kept_Z, rep(99.0001, 2L))
  expect_true(all(c(2L, 4L) %in% repaired$retained_source_rows))
  expect_identical(repaired$las@data[, c("X", "Y", "Z"), with = FALSE],
                   original[repaired$retained_source_rows, c("X", "Y", "Z"), with = FALSE])
  expect_identical(las@data, original)
  expect_length(repaired$qc$check$errors, 0)
  expect_length(repaired$qc$check$warnings, 0)
})

test_that("failed v1 pilot upgrade preserves archive/diagnostics and refuses completed or active work", {
  f <- fresh_fixture()
  e <- f$env
  policy <- e$fresh_policy()
  policy$version <- "fresh-petawawa-v2"
  e$fresh_policy <- function() policy
  config <- e$fresh_root(f$config)
  previous <- readRDS(file.path(config$root, "fresh-root.rds"))
  previous$policy$version <- "fresh-petawawa-v1"
  previous$policy$repair <- "Legacy v1 repair without ground XY cleanup."
  e$petawawa_save(previous, file.path(config$root, "fresh-root.rds"))
  download <- e$fresh_subdir(config$root, "download")
  archive <- file.path(download, "Petawawa2012.zip")
  writeLines("fixture archive, never downloaded", archive)
  hash <- e$petawawa_checksum(archive)
  pilot <- e$fresh_subdir(config$root, "pilot")
  e$fresh_subdir(pilot, "repaired")
  prepared <- e$fresh_subdir(pilot, "prepared")
  e$petawawa_save(list(config = previous$config, policy = previous$policy),
                  file.path(pilot, "stage-identity.rds"))
  writeLines("original diagnostic", file.path(pilot, "failure.txt"))
  expect_error(e$fresh_root(config), "policy changed")
  bad <- config
  bad$seed <- bad$seed + 1L
  expect_error(e$fresh_root(bad, upgrade_failed_pilot = TRUE), "unchanged-config")
  lock <- file.path(config$root, ".preparation-lock")
  dir.create(lock)
  expect_error(e$fresh_root(config, upgrade_failed_pilot = TRUE), "active or interrupted")
  e$fresh_release_lock(lock)
  writeLines("completed", file.path(pilot, "ready.rds"))
  expect_error(e$fresh_root(config, upgrade_failed_pilot = TRUE), "failed pilot")
  unlink(file.path(pilot, "ready.rds"))
  writeLines("partial", file.path(prepared, "partial.laz"))
  expect_error(e$fresh_root(config, upgrade_failed_pilot = TRUE), "existing repaired/prepared")
  unlink(file.path(prepared, "partial.laz"))
  expect_identical(e$fresh_root(config, upgrade_failed_pilot = TRUE), config)
  expect_identical(e$petawawa_checksum(archive), hash)
  expect_false(dir.exists(pilot))
  archived <- list.files(config$root, pattern = "^pilot-failed-v1-", full.names = TRUE)
  expect_length(archived, 1L)
  expect_identical(readLines(file.path(archived, "failure.txt")), "original diagnostic")
  expect_identical(readRDS(file.path(archived, "root-identity-v1.rds")), previous)
  expect_identical(readRDS(file.path(config$root, "fresh-root.rds"))$policy$version,
                   "fresh-petawawa-v2")
  expect_identical(e$fresh_root(config, upgrade_failed_pilot = TRUE), config)
  expect_false(dir.exists(lock))
})

test_that("current policy refuses old-policy roots and recovery without changing existing bytes", {
  f <- fresh_fixture()
  e <- f$env
  config <- e$fresh_root(f$config)
  path <- file.path(config$root, "fresh-root.rds")
  current <- readRDS(path)
  sentinel <- file.path(config$root, "existing-product")
  writeLines("preserve previous products", sentinel)
  for (version in c("fresh-petawawa-v1", "fresh-petawawa-v2")) {
    previous <- current
    previous$policy$version <- version
    previous$policy$source_filter <- NULL
    saveRDS(previous, path)
    before <- e$petawawa_checksum(c(path, sentinel))
    expect_error(e$fresh_root(config), "policy changed")
    expect_error(e$fresh_root(config, upgrade_failed_pilot = TRUE), "unchanged-config")
    expect_error(e$fresh_prepare(config, retry_failed_preparation = TRUE), "policy changed")
    expect_error(e$fresh_prepare(config, full = TRUE, resume_parallel = TRUE), "policy changed")
    expect_false(e$fresh_parallel_compatible(previous, current))
    expect_identical(e$petawawa_checksum(c(path, sentinel)), before)
    expect_false(dir.exists(file.path(config$root, ".preparation-lock")))
  }
})

test_that("ZIP inventory rejects traversal and ambiguous tile members", {
  f <- fresh_fixture()
  e <- f$env
  listing <- data.frame(Name = c("folder/298000_5093000_unixyz_c.las",
                                 "folder/298000_5094000.laz"), Length = c(100, 100))
  testthat::local_mocked_bindings(unzip = function(...) listing, .package = "utils")
  expect_equal(e$fresh_members("fixture.zip", c("298000_5093000", "298000_5094000")),
               listing$Name)
  listing$Name[1] <- "../298000_5093000.las"
  expect_error(e$fresh_members("fixture.zip", "298000_5093000"), "Unsafe")
  listing$Name <- c("298000_5093000.las", "298000_5093000.laz")
  expect_error(e$fresh_members("fixture.zip", "298000_5093000"), "exactly one")
})

fresh_fixture_archive <- function(f) {
  if (!nzchar(Sys.which("zip"))) skip("The zip executable is needed for tiny archive fixtures.")
  config <- f$env$fresh_root(f$config)
  directory <- file.path(f$directory, "original")
  dir.create(directory)
  tiles <- c("298000_5093000", "298000_5094000")
  for (tile in tiles) {
    las <- f$make_las(tile)
    lidR::writeLAS(las, file.path(directory, paste0(tile, "_unixyz_c.las")))
  }
  folder <- f$env$fresh_subdir(config$root, "download")
  archive <- file.path(folder, "Petawawa2012.zip")
  withr::with_dir(directory, utils::zip(archive, list.files(directory), flags = "-q"))
  f$env$petawawa_save(list(url = config$archive_url,
    md5 = f$env$petawawa_checksum(archive)), file.path(folder, "download.rds"))
  list(config = config, archive = archive, tiles = tiles)
}

test_that("fresh pilot prepares and verifies every density, resumes, and rejects corruption", {
  f <- fresh_fixture()
  e <- f$env
  a <- fresh_fixture_archive(f)
  expect_error(e$fresh_prepare(a$config, full = TRUE), "two-tile pilot")
  expect_false(dir.exists(file.path(a$config$root, ".preparation-lock")))
  hash <- e$petawawa_checksum(a$archive)
  expect_no_warning(plan <- e$fresh_prepare(a$config))
  folder <- file.path(a$config$root, "pilot")
  expect_true(file.exists(file.path(folder, "ready.rds")))
  expect_identical(plan$method, "fresh-petawawa-v4")
  config_file <- file.path(f$directory, "config.R")
  dput(a$config, config_file)
  expect_no_error(e$fresh_main(c("--verify-pilot", config_file)))
  outputs <- list.files(file.path(folder, "prepared"), pattern = "\\.laz$",
                        recursive = TRUE, full.names = TRUE)
  expect_length(outputs, 12L)
  hashes <- e$petawawa_checksum(outputs)
  expect_no_warning(e$fresh_prepare(a$config))
  expect_identical(e$petawawa_checksum(outputs), hashes)
  expect_error(e$fresh_prepare(a$config, retry_failed_preparation = TRUE), "ready pilot")
  repaired <- plan$sources$path
  repaired_hashes <- e$petawawa_checksum(c(repaired, paste0(repaired, ".rds")))
  unlink(file.path(folder, "ready.rds"))
  stage_file <- file.path(folder, "stage-identity.rds")
  previous <- readRDS(stage_file)
  previous$implementation[1] <- "fixture-old-query-implementation"
  e$petawawa_save(previous, stage_file)
  expect_error(e$fresh_prepare(a$config), "Stage implementation")
  changed <- previous
  changed$runtime <- list(R = "changed")
  expect_error(e$fresh_restart_failed_preparation(folder, previous, changed, a$tiles),
               "unchanged inputs")
  expect_no_warning(e$fresh_main(c("--retry-failed-preparation", config_file)))
  expect_identical(e$petawawa_checksum(c(repaired, paste0(repaired, ".rds"))), repaired_hashes)
  archived <- list.files(a$config$root, pattern = "^pilot-preparation-failed-", full.names = TRUE)
  expect_length(archived, 1L)
  archived_outputs <- list.files(file.path(archived, "prepared"), pattern = "\\.laz$",
                                  recursive = TRUE, full.names = TRUE)
  expect_identical(e$petawawa_checksum(archived_outputs), hashes)
  expect_identical(readRDS(file.path(archived, "stage-identity.rds")), previous)
  expect_no_error(e$fresh_verify(folder, a$tiles))
  expect_identical(e$petawawa_checksum(a$archive), hash)
  expect_false(dir.exists(file.path(a$config$root, ".preparation-lock")))
  expect_error(e$fresh_prepare(a$config, full = TRUE), "exactly one")
  writeLines("corrupt", outputs[1])
  expect_error(e$fresh_verify(folder, a$tiles), "checksum mismatch")
  expect_error(e$fresh_prepare(a$config), "checksum mismatch")
  expect_true(length(list.files(folder, pattern = "^preparation-failure-")) > 0L)
  index <- sub("\\.laz$", ".lax", outputs[2])
  writeLines("corrupt index", index)
  expect_error(e$fresh_index_signature(outputs[2]), "Invalid LAX")
  expect_false(dir.exists(file.path(a$config$root, ".preparation-lock")))
})

test_that("fresh download failures retain partial bytes and do not produce a success receipt", {
  f <- fresh_fixture()
  e <- f$env
  config <- e$fresh_root(f$config)
  e$benchmark_process <- function(executable, args) {
    expect_true(all(c("--fail", "--continue-at", "--location") %in% args))
    writeLines("partial", args[match("--output", args) + 1L])
    list(status = 7L, stdout = "", stderr = "fixture transfer failure")
  }
  expect_error(e$fresh_archive(config, download = TRUE), "download failed")
  folder <- file.path(config$root, "download")
  expect_true(file.exists(file.path(folder, "Petawawa2012.zip.part")))
  expect_false(file.exists(file.path(folder, "download.rds")))
  expect_identical(readRDS(file.path(folder, "transfer.rds"))$status, 7L)
  e$benchmark_process <- function(executable, args) {
    writeLines("not a zip", args[match("--output", args) + 1L])
    list(status = 0L, stdout = "", stderr = "")
  }
  config$archive_md5 <- paste(rep("0", 32), collapse = "")
  expect_error(e$fresh_archive(config, download = TRUE), "MD5 mismatch")
  expect_false(file.exists(file.path(folder, "download.rds")))
})

test_that("fresh downloads publish only a checked archive and reject changed completed bytes", {
  f <- fresh_fixture()
  e <- f$env
  a <- fresh_fixture_archive(f)
  remote <- file.path(f$directory, "fixture-remote.zip")
  file.copy(a$archive, remote)
  unlink(c(a$archive, file.path(dirname(a$archive), "download.rds")))
  e$petawawa_tiles <- function() a$tiles
  e$benchmark_process <- function(executable, args) {
    expect_identical(args[match("--continue-at", args) + 1L], "-")
    file.copy(remote, args[match("--output", args) + 1L], overwrite = TRUE)
    list(status = 0L, stdout = "", stderr = "")
  }
  expect_identical(e$fresh_archive(a$config, download = TRUE), a$archive)
  expect_false(file.exists(paste0(a$archive, ".part")))
  receipt <- readRDS(file.path(dirname(a$archive), "download.rds"))
  expect_identical(receipt$md5, e$petawawa_checksum(remote))
  expect_identical(e$fresh_archive(a$config), a$archive)
  writeLines("changed archive", a$archive)
  expect_error(e$fresh_archive(a$config), "archive changed")
})

test_that("raw read warnings are retained while prepared-data warnings are errors", {
  f <- fresh_fixture()
  las <- f$make_las()
  testthat::local_mocked_bindings(readLAS = function(...) {
    warning("fixture invalid return")
    las
  }, .package = "lidR")
  expect_identical(f$env$fresh_read("fixture", raw = TRUE)$warnings, "fixture invalid return")
  expect_error(f$env$fresh_read("fixture"), "LAS read warning: fixture invalid return")
})
