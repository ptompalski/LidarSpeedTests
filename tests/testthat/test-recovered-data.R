test_that("the recovered historical pilot manifest is explicit and portable", {
  manifest <- read.csv(file.path(repo, "input-manifests/petawawa-recovered-pilot.csv"),
                       colClasses = c(las_version = "character"))
  expect_identical(names(manifest), c(
    "density", "tile", "path", "md5", "index_md5", "bytes", "points",
    "las_version", "point_format", "crs"
  ))
  expect_equal(nrow(manifest), 12L)
  expect_identical(sort(unique(manifest$density)), c(1L, 2L, 5L, 10L, 20L, 50L))
  expect_true(all(table(manifest$density) == 2L))
  expect_setequal(unique(manifest$tile), c("298000_5093000", "298000_5094000"))
  expect_false(anyDuplicated(manifest$path) > 0L)
  expect_identical(manifest$path, paste0(
    "tile_1000_density_", manifest$density, "/", manifest$tile, ".laz"
  ))
  expect_true(all(grepl("^[0-9a-f]{32}$", manifest$md5)))
  expect_true(all(grepl("^[0-9a-f]{32}$", manifest$index_md5)))
  expect_true(all(manifest$bytes > 0 & manifest$points > 0))
  expect_true(all(manifest$las_version %in% c("1.1", "1.2")))
  expect_true(all(manifest$point_format == 1L))
  expect_true(all(manifest$crs == "unknown"))
})

test_that("opt-in recovered historical inputs match their recorded bytes and headers", {
  root <- Sys.getenv("LIDAR_RECOVERED_INPUT_ROOT")
  if (!nzchar(root)) {
    skip("Set LIDAR_RECOVERED_INPUT_ROOT for read-only historical pilot checks.")
  }
  expect_true(dir.exists(root))
  manifest <- read.csv(file.path(repo, "input-manifests/petawawa-recovered-pilot.csv"),
                       colClasses = c(las_version = "character"))
  files <- file.path(root, manifest$path)
  indexes <- sub("\\.laz$", ".lax", files)
  expect_true(all(file.exists(c(files, indexes))))
  expect_identical(unname(tools::md5sum(files)), manifest$md5)
  expect_identical(unname(tools::md5sum(indexes)), manifest$index_md5)
  expect_equal(unname(file.info(files)$size), manifest$bytes)
  expect_true(all(file.info(indexes)$size > 0))
  for (i in seq_along(files)) {
    header <- lidR::readLASheader(files[i])
    phb <- header@PHB
    expect_equal(phb[["Number of point records"]], manifest$points[i])
    expect_identical(paste(phb[["Version Major"]], phb[["Version Minor"]], sep = "."),
                     manifest$las_version[i])
    expect_equal(phb[["Point Data Format ID"]], manifest$point_format[i])
    expect_true(is.na(sf::st_crs(header)))
    origin <- as.numeric(strsplit(manifest$tile[i], "_", fixed = TRUE)[[1L]])
    expect_true(phb[["Min X"]] >= origin[1L] - 1e-4 &&
                  phb[["Max X"]] <= origin[1L] + 1000 + 1e-4 &&
                  phb[["Min Y"]] >= origin[2L] - 1e-4 &&
                  phb[["Max Y"]] <= origin[2L] + 1000 + 1e-4)
  }
})
