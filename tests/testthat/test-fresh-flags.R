fresh_flags_fixture <- function(envir = parent.frame(), count = 138L) {
  withr::local_dir(repo, .local_envir = envir)
  directory <- withr::local_tempdir(.local_envir = envir)
  env <- new.env(parent = globalenv())
  sys.source("R/prepare_fresh_data.R", env)
  xy <- expand.grid(X = 298000 + seq(0, 3.5, .25),
                    Y = 5093000 + seq(0, 3.5, .25))
  las <- lidR::LAS(data.frame(
    X = xy$X, Y = xy$Y, Z = rep(100, nrow(xy)), Intensity = rep(0L, nrow(xy)),
    ReturnNumber = rep(1L, nrow(xy)), NumberOfReturns = rep(1L, nrow(xy)),
    ScanDirectionFlag = rep(0L, nrow(xy)), EdgeOfFlightline = rep(0L, nrow(xy)),
    Classification = rep(2L, nrow(xy)), Synthetic_flag = seq_len(nrow(xy)) <= count,
    Keypoint_flag = rep(FALSE, nrow(xy)), Withheld_flag = rep(FALSE, nrow(xy)),
    ScanAngleRank = rep(0L, nrow(xy)), UserData = rep(0L, nrow(xy)),
    PointSourceID = rep(1L, nrow(xy)), gpstime = seq_len(nrow(xy)) / 10
  ))
  sf::st_crs(las) <- 2959
  list(env = env, directory = directory, las = las)
}

fresh_flags_disk <- function(path) {
  size <- file.info(path)$size
  stopifnot(!is.na(size), size <= 65536L)
  bytes <- readBin(path, "raw", n = size)
  uint <- function(offset, size) {
    sum(as.integer(bytes[offset + seq_len(size)]) * 256^(0:(size - 1L)))
  }
  stopifnot(length(bytes) >= 227L, rawToChar(bytes[1:4]) == "LASF",
            uint(24, 1) == 1L, uint(25, 1) == 2L, uint(94, 2) == 227L,
            uint(104, 1) == 1L, uint(105, 2) == 28L)
  offset <- uint(96, 4)
  count <- uint(107, 4)
  stopifnot(offset >= 227L, count > 0L, count <= 1000L,
            length(bytes) == offset + count * 28L)
  # LAS 1.2 format 1: classification byte 15 (bit 5), GPS time at byte 20.
  starts <- offset + (seq_len(count) - 1L) * 28L
  data.frame(
    gpstime = vapply(starts, function(start) {
      readBin(bytes[start + 21:28], "double", n = 1L, size = 8L, endian = "little")
    }, numeric(1)),
    Synthetic_flag = bitwAnd(as.integer(bytes[starts + 16L]), 32L) != 0L
  )
}

test_that("independent LAS bits establish source flags before native filtering", {
  for (count in c(0L, 1L, 138L, 225L)) {
    f <- fresh_flags_fixture(count = count)
    f$las@data$Intensity <- rep(0L, lidR::npoints(f$las))
    original <- data.table::copy(f$las@data)
    clouds <- list(source = f$las)
    for (density in c(5, 50)) {
      variant <- f$env$fresh_variant(f$las, density, 20261022L)
      step <- diff(range(f$las$gpstime)) + 1
      rows <- match(round(variant$gpstime * 10) %% round(step * 10),
                    round(f$las$gpstime * 10))
      expect_false(anyNA(rows))
      expect_identical(variant$Synthetic_flag, f$las$Synthetic_flag[rows])
      clouds[[paste0("density-", density)]] <- variant
    }
    expect_identical(f$las@data, original)
    for (name in names(clouds)) {
      las <- clouds[[name]]
      las <- las[order(las$PointSourceID, las$gpstime)]
      before <- data.table::copy(las@data)
      path <- file.path(f$directory, paste0(name, ".las"))
      lidR::writeLAS(las, path)
      disk <- fresh_flags_disk(path)
      rows <- match(disk$gpstime, las$gpstime)
      expect_false(anyNA(rows))
      expect_identical(disk$Synthetic_flag, las$Synthetic_flag[rows])
      expect_identical(las@data, before)
    }
  }
})

test_that("native filtering repeatedly retains exactly the independently unflagged records", {
  for (count in c(0L, 1L, 138L, 225L)) {
    f <- fresh_flags_fixture(count = count)
    clouds <- list(source = f$las)
    for (density in c(5, 50)) {
      clouds[[paste0("density-", density)]] <- f$env$fresh_variant(f$las, density, 20261022L)
    }
    for (name in names(clouds)) {
      path <- file.path(f$directory, paste0(name, ".las"))
      lidR::writeLAS(clouds[[name]], path)
      disk <- fresh_flags_disk(path)
      for (iteration in seq_len(5L)) {
        if (all(disk$Synthetic_flag)) {
          expect_error(f$env$fresh_read(path, raw = TRUE), "Empty/unreadable LAS")
        } else {
          expect_no_warning(read <- f$env$fresh_read(path, raw = TRUE))
          expect_length(read$warnings, 0L)
          expected <- clouds[[name]]@data[!clouds[[name]]$Synthetic_flag]
          expect_equal(read$las$gpstime, disk$gpstime[!disk$Synthetic_flag])
          expect_setequal(names(read$las@data), names(expected))
          expected <- expected[, names(read$las@data), with = FALSE]
          expect_identical(as.data.frame(read$las@data), as.data.frame(expected),
                           info = paste(name, "source flag count", count, "read", iteration))
        }
      }
    }
  }
})

test_that("filtered LAZ sources remain flag-free through repair, thinning and densification", {
  for (count in c(0L, 1L, 138L)) {
    f <- fresh_flags_fixture(count = count)
    e <- f$env
    path <- file.path(f$directory, paste0("source-", count, ".laz"))
    las <- f$las
    lidR::writeLAS(las, path)
    expect_no_warning(read <- e$fresh_read(path, raw = TRUE))
    expect_length(read$warnings, 0L)
    expect_identical(sum(read$las$Synthetic_flag), 0L)
    expect_equal(read$las$gpstime, las$gpstime[!las$Synthetic_flag])
    repaired <- e$fresh_repair(read$las, "298000_5093000", 2959)
    expect_identical(repaired$las$Synthetic_flag, read$las$Synthetic_flag)
    expect_length(repaired$qc$check$warnings, 0L)
    expect_false(any(grepl("points flagged 'synthetic'", repaired$qc$check$messages)))
    for (density in c(5, 50)) {
      expected <- e$fresh_variant(repaired$las, density, 20261022L)
      expected <- expected[order(expected$PointSourceID, expected$gpstime)]
      output <- file.path(f$directory, paste0("variant-", count, "-", density, ".laz"))
      lidR::writeLAS(expected, output)
      expect_no_warning(summary <- e$fresh_validate_output(output, expected))
      expect_identical(e$fresh_read_las(output)$Synthetic_flag, expected$Synthetic_flag)
      expect_equal(summary$points, round(density * lidR::area(repaired$las)))
    }
  }
})

test_that("synthetic diagnostics and retained flags are fatal even on raw reads", {
  f <- fresh_flags_fixture(count = 0L)
  diagnostic <- "There are 137 points flagged 'synthetic'."
  testthat::local_mocked_bindings(readLAS = function(file, filter) {
    if (nzchar(diagnostic)) warning(diagnostic)
    f$las
  }, .package = "lidR")
  expect_error(f$env$fresh_read("fixture"), "LAS read warning")
  expect_error(f$env$fresh_read("fixture", raw = TRUE), "LAS read warning")
  diagnostic <- "There are 138 points flagged 'synthetic'. Additional warning."
  expect_error(f$env$fresh_read("fixture"), "LAS read warning")
  expect_error(f$env$fresh_read("fixture", raw = TRUE), "LAS read warning")
  diagnostic <- "There are 138 points flagged 'SYNTHETIC'."
  expect_error(f$env$fresh_read("fixture", raw = TRUE), "LAS read warning")
  diagnostic <- "There are 138 points flagged 'synthetic'."
  expect_error(f$env$fresh_read("fixture", raw = TRUE), "LAS read warning")
  diagnostic <- "fixture invalid return"
  expect_error(f$env$fresh_read("fixture"), "fixture invalid return")
  expect_identical(f$env$fresh_read("fixture", raw = TRUE)$warnings,
                   diagnostic)
  diagnostic <- ""
  f$las@data$Synthetic_flag[1] <- TRUE
  expect_error(f$env$fresh_read("fixture"), "no flagged points")
  expect_error(f$env$fresh_read("fixture", raw = TRUE), "no flagged points")
  expect_error(f$env$fresh_repair(f$las, "298000_5093000", 2959), "no flagged points")
  f$las@data$Synthetic_flag[1] <- NA
  expect_error(f$env$fresh_read("fixture", raw = TRUE), "complete logical")
  f$las@data$Synthetic_flag <- NULL
  expect_error(f$env$fresh_read("fixture"), "complete logical")
  expect_error(f$env$fresh_read("fixture", raw = TRUE), "complete logical")
})

test_that("raw filtering is applied before decoding and composes with spatial filters", {
  f <- fresh_flags_fixture(count = 0L)
  filters <- character()
  testthat::local_mocked_bindings(readLAS = function(file, filter) {
    filters <<- c(filters, filter)
    f$las
  }, .package = "lidR")
  f$env$fresh_read("fixture", raw = TRUE)
  f$env$fresh_read("fixture", raw = TRUE, filter = "-inside 0 0 1 1")
  f$env$fresh_read("fixture", filter = "-inside 0 0 1 1")
  expect_identical(filters, c("-drop_synthetic", "-drop_synthetic -inside 0 0 1 1",
                              "-inside 0 0 1 1"))
})
