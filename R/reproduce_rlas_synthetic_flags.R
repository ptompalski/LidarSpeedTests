# Standalone reproducer; requires only rlas (no lidR or project scripts).
# From this checkout:
# Rscript --vanilla -e 'source("renv/activate.R"); source("R/reproduce_rlas_synthetic_flags.R")'
# Elsewhere: Rscript --vanilla reproduce_rlas_synthetic_flags.R
#
# Expected: every read returns the same 138 TRUE flags followed by 87 FALSE.
# Observed with rlas 1.9.5: some reads return only one TRUE, despite correct
# on-disk flags. This is allocation-sensitive; a clean run is not proof of a fix.
reproduce_rlas_synthetic_flags <- function() {
  cat(R.version.string, "\nrlas", as.character(utils::packageVersion("rlas")), "\n")
  path <- tempfile(fileext = ".las")
  on.exit(unlink(path), add = TRUE)

  n <- 225L
  expected <- seq_len(n) <= 138L
  points <- data.frame(
    X = 298000 + ((seq_len(n) - 1L) %% 15L) / 4,
    Y = 5093000 + ((seq_len(n) - 1L) %/% 15L) / 4,
    Z = rep(100, n),
    gpstime = seq_len(n) / 10,
    Intensity = rep(0L, n),
    ReturnNumber = rep(1L, n),
    NumberOfReturns = rep(1L, n),
    Classification = rep(2L, n),
    Synthetic_flag = expected
  )
  rlas::write.las(path, rlas::header_create(points), points)

  # Independent LAS 1.2 / format 1 decoding; offsets below are zero-based.
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  uint <- function(offset, size) {
    sum(as.integer(bytes[offset + seq_len(size)]) * 256^(0:(size - 1L)))
  }
  stopifnot(
    length(bytes) >= 227L, rawToChar(bytes[1:4]) == "LASF",
    uint(24, 1) == 1L, uint(25, 1) == 2L, uint(94, 2) == 227L,
    uint(104, 1) == 1L, uint(105, 2) == 28L,
    uint(96, 4) >= 227L, uint(107, 4) == n,
    length(bytes) == uint(96, 4) + n * 28L
  )
  starts <- uint(96, 4) + (seq_len(n) - 1L) * 28L
  disk_flags <- bitwAnd(as.integer(bytes[starts + 16L]), 32L) != 0L
  disk_times <- vapply(starts, function(start) {
    readBin(bytes[start + 21:28], "double", n = 1L, size = 8L, endian = "little")
  }, numeric(1))
  stopifnot(identical(disk_flags, expected), identical(disk_times, points$gpstime))
  cat("Independent disk check: 225 records, exactly 138 synthetic flags.\n")

  failures <- 0L
  for (iteration in seq_len(100L)) {
    diagnostics <- character()
    actual <- withCallingHandlers(rlas::read.las(path), warning = function(w) {
      diagnostics <<- c(diagnostics, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    stopifnot(nrow(actual) == n, identical(actual$gpstime, disk_times))
    if (!identical(actual$Synthetic_flag, disk_flags)) {
      failures <- failures + 1L
      if (failures <= 10L) {
        cat(sprintf("Read %d: expected 138 flags, decoded %s; vector differs.\n",
                    iteration, sum(actual$Synthetic_flag)))
        if (length(diagnostics)) cat("  Warning:", paste(diagnostics, collapse = "; "), "\n")
      }
    }
  }
  cat(sprintf("Flag-vector mismatches: %d / 100 reads.\n", failures))
  print(utils::sessionInfo())
  if (failures > 0L) stop("Reproduced: rlas returned flags that differ from the LAS bytes.")
  cat("Not reproduced in this run; the reported defect is intermittent.\n")
}

reproduce_rlas_synthetic_flags()
