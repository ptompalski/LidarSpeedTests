# Source from the repository root. Nothing is downloaded or processed on source().
source("R/1_functions.R", local = TRUE)

petawawa_tiles <- function(path = "ALS_tiles.gpkg") {
  tiles <- sf::st_read(path, quiet = TRUE)
  ids <- tools::file_path_sans_ext(basename(gsub("\\\\", "/", tiles$filename)))
  if (length(ids) != 100L || anyNA(ids) || anyDuplicated(ids) ||
      any(!grepl("^[0-9]{6}_[0-9]{7}$", ids))) {
    stop("ALS_tiles.gpkg must identify the approved 100 Petawawa tiles.")
  }
  sort(ids)
}

petawawa_checksum <- function(paths) {
  hashes <- unname(tools::md5sum(paths))
  if (anyNA(hashes)) stop("Cannot checksum preparation files.")
  hashes
}

petawawa_process <- function(executable, args) {
  result <- benchmark_process(executable, args)
  if (result$status != 0L) {
    stop("Preparation process failed: ", executable, " (", result$status, ")\n",
         result$stdout, "\n", result$stderr)
  }
  result
}

petawawa_preflight <- function(config, full = FALSE) {
  required <- c("source_root", "output_root", "source_citation", "license",
                "crs", "repaired", "repair_provenance", "lastools_path",
                "tool_license", "executables", "tiles", "seed")
  if (!is.list(config) || !all(required %in% names(config))) {
    stop("Supply all settings from data-preparation.example.R.")
  }
  for (field in c("source_citation", "license", "repair_provenance", "tool_license")) {
    value <- config[[field]]
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !nzchar(trimws(value))) stop("Provide confirmed ", field, ".")
  }
  if (!identical(config$repaired, TRUE)) {
    stop("Already repaired input is required; no raw-data repair is performed.")
  }
  crs <- sf::st_crs(config$crs)
  if (is.na(crs) || sf::st_is_longlat(crs) ||
      !identical(crs$units_gdal, "metre")) {
    stop("Confirm a projected source CRS in metres; do not infer it from the GeoPackage.")
  }
  approved <- petawawa_tiles()
  subset <- c("298000_5093000", "298000_5094000")
  if (!is.character(config$tiles) || !length(config$tiles) ||
      anyNA(config$tiles) || anyDuplicated(config$tiles) ||
      any(!config$tiles %in% approved)) stop("Select unique approved tile IDs.")
  if (!isTRUE(full) && !setequal(config$tiles, subset)) {
    stop("Only the approved neighboring pair is allowed without full = TRUE.")
  }
  if (isTRUE(full) && !setequal(config$tiles, approved)) {
    stop("A full preparation must explicitly select all 100 approved tiles.")
  }
  if (!is.numeric(config$seed) || length(config$seed) != 1L ||
      is.na(config$seed) || config$seed != floor(config$seed) ||
      config$seed < 0 || config$seed > .Machine$integer.max - 600L) {
    stop("seed must be a nonnegative integer below the stream offset limit.")
  }
  input <- benchmark_directory(config$source_root, "source_root")
  output <- benchmark_directory(config$output_root, "output_root", writable = TRUE)
  if (benchmark_paths_overlap(input, output)) {
    stop("Source and output trees must be disjoint; output cannot be a root, home or checkout.")
  }
  benchmark_validate_scratch_root(output, input)
  files <- list.files(input, pattern = "\\.(las|laz)$", full.names = TRUE,
                      ignore.case = TRUE)
  ids <- tools::file_path_sans_ext(basename(files))
  selected <- sort(config$tiles)
  if (any(vapply(selected, function(id) sum(ids == id) != 1L, logical(1)))) {
    stop("Supply exactly one already repaired .las/.laz named <tile_id> for each selected tile.")
  }
  files <- normalizePath(files[match(selected, ids)], winslash = "/")
  if (any(!startsWith(files, paste0(input, "/")))) {
    stop("Source tiles must not be symlinks outside source_root.")
  }
  if (!is.character(config$executables) ||
      !identical(sort(names(config$executables)), c("index", "sort")) ||
      anyNA(config$executables)) {
    stop("executables must explicitly name sort and index tools.")
  }
  tools <- vapply(config$executables[c("sort", "index")], function(tool) {
    benchmark_lastools_executable(config$lastools_path, tool)
  }, character(1))
  versions <- lapply(tools, petawawa_process, args = "-version")
  config$tiles <- selected
  config$source_root <- input
  config$output_root <- output
  config$crs <- crs$wkt
  for (i in seq_along(files)) {
    petawawa_validate_source(lidR::readLAS(files[i]), selected[i], config$crs)
  }
  list(
    schema = 1L, method = "seeded-recovered-petawawa-v1", config = config,
    recovered_scripts_md5 = c(
      "_OLD/R/2_data.R" = "b110ca6a754816d286027585b52dd09b",
      "_OLD/R/99_IncreaseDensity.R" = "9bddef7946565cfa4b5ac9c269287ec8",
      "_OLD/R/99_repairPetawawaALS2012.R" = "5fdfb399625d1ff18db9f16e7c15efe3"
    ),
    sources = data.frame(tile = selected, path = files,
                         md5 = petawawa_checksum(files)),
    densities = c(1, 2, 5, 10, 20, 50),
    parameters = list(
      thinning = "lidR::random(density, use_pulse = FALSE); round(density * source area)",
      densification = "full copies plus round(fraction * source count); source density rounded",
      z_noise_m = c(-seq(.1, .5, .05), seq(.1, .5, .05)),
      gps_time_shift = "0.013 / (target_density / round(source_density) - 1), every copy",
      rng = c("Mersenne-Twister", "Inversion", "Rejection"),
      seed_schedule = "base + 100 * zero-based density position + one-based sorted tile position",
      tiling = "Preserve supplied 1000 m source tiles and edge coverage; no XY noise or retiling",
      format = "Preserve LAS 1.1/1.2 point format 1/3, CRS, scales, offsets and attributes",
      sorting = c("-point_source", "-gps_time"),
      indexing = paste(config$executables[["index"]], ".lax; no COPC conversion"),
      checksum = "MD5 content identifiers, not authenticity verification"
    ),
    tools = data.frame(name = names(tools), path = unname(tools),
                       md5 = petawawa_checksum(tools)),
    versions = versions,
    implementation = petawawa_checksum(c(
      "R/2_data.R", "R/1_functions.R", "renv.lock", "ALS_tiles.gpkg")),
    runtime = list(R = as.character(getRversion()), platform = R.version$platform,
                   lidR = as.character(utils::packageVersion("lidR")),
                   sf = as.character(utils::packageVersion("sf")),
                   native = sf::sf_extSoftVersion())
  )
}

petawawa_validate_source <- function(las, tile, crs) {
  if (is.null(las) || !inherits(las, "LAS") || lidR::npoints(las) == 0) {
    stop("Empty or unreadable source tile: ", tile)
  }
  required <- c("X", "Y", "Z", "gpstime", "Intensity", "ReturnNumber",
                "NumberOfReturns", "Classification", "PointSourceID")
  if (!all(required %in% names(las))) stop("Missing required point attributes: ", tile)
  d <- las@data
  if (anyNA(d[, required, with = FALSE]) ||
      any(!is.finite(as.matrix(d[, c("X", "Y", "Z", "gpstime"), with = FALSE]))) ||
      any(d$ReturnNumber < 1L | d$NumberOfReturns < d$ReturnNumber |
          d$NumberOfReturns > 7L)) {
    stop("Input is not repaired/finite legacy LAS data: ", tile)
  }
  header <- las@header@PHB
  if (!header[["Point Data Format ID"]] %in% c(1L, 3L) ||
      header[["Version Major"]] != 1L || header[["Version Minor"]] > 2L) {
    stop("Require legacy LAS 1.1/1.2 with GPS time (point format 1 or 3); ",
         "conversion must be separately approved.")
  }
  if (is.na(sf::st_crs(las))) {
    sf::st_crs(las) <- sf::st_crs(crs)
  } else if (!isTRUE(sf::st_crs(las) == sf::st_crs(crs))) {
    stop("Source CRS differs from confirmed metadata: ", tile)
  }
  origin <- as.numeric(strsplit(tile, "_", fixed = TRUE)[[1L]])
  if (any(d$X < origin[1L] | d$X > origin[1L] + 1000 |
          d$Y < origin[2L] | d$Y > origin[2L] + 1000)) {
    stop("Points extend outside their named 1 km source tile: ", tile)
  }
  if (!is.finite(lidR::area(las)) || lidR::area(las) <= 0) {
    stop("Source tile must have positive spatial coverage: ", tile)
  }
  las
}

petawawa_variant <- function(las, density, seed) {
  withr::local_seed(seed, .rng_kind = "Mersenne-Twister",
                   .rng_normal_kind = "Inversion", .rng_sample_kind = "Rejection")
  if (density <= 10) {
    if (lidR::density(las) < density) stop("Source is too sparse for requested thinning.")
    result <- lidR::decimate_points(las, lidR::random(density))
  } else {
    current <- round(lidR::density(las))
    multiplier <- density / current - 1
    if (!is.finite(multiplier) || multiplier <= 0) {
      stop("Source rounded density must be positive and below densification target.")
    }
    original <- as.data.frame(las@data)
    copies <- list(original)
    repetitions <- floor(multiplier)
    partial <- round((multiplier - repetitions) * nrow(original))
    sizes <- c(rep(nrow(original), repetitions), partial)
    for (size in sizes[sizes > 0]) {
      rows <- if (size == nrow(original)) seq_len(size) else
        sample.int(nrow(original), size)
      copy <- original[rows, , drop = FALSE]
      copy$Z <- copy$Z + sample(c(-seq(.1, .5, .05), seq(.1, .5, .05)),
                                nrow(copy), replace = TRUE)
      copy$gpstime <- copy$gpstime + .013 / multiplier
      copies[[length(copies) + 1L]] <- copy
    }
    points <- do.call(rbind, copies)
    # Quantize the new Z values below before checking the persisted point records.
    result <- lidR::LAS(points, header = las@header, crs = sf::st_crs(las), check = FALSE)
  }
  if (is.null(result) || lidR::npoints(result) == 0) stop("Preparation produced no points.")
  lidR::las_quantize(result)
  result
}

petawawa_validate_output <- function(path, expected) {
  actual <- lidR::readLAS(path)
  if (is.null(actual) || lidR::npoints(actual) != lidR::npoints(expected) ||
      !identical(sort(names(actual)), sort(names(expected))) ||
      !isTRUE(sf::st_crs(actual) == sf::st_crs(expected))) {
    stop("Output count, attributes or CRS validation failed: ", path)
  }
  a <- as.data.frame(actual@data)
  b <- as.data.frame(expected@data)
  if (!identical(order(a$PointSourceID, a$gpstime), seq_len(nrow(a)))) {
    stop("Output is not sorted by point source and GPS time: ", path)
  }
  # lassort reorders points. Compare complete records in a canonical order.
  columns <- sort(names(a))
  a <- a[do.call(order, a[columns]), columns, drop = FALSE]
  b <- b[do.call(order, b[columns]), columns, drop = FALSE]
  rownames(a) <- rownames(b) <- NULL
  scales <- unlist(expected@header@PHB[c("X scale factor", "Y scale factor",
                                        "Z scale factor")])
  for (column in columns) {
    tolerance <- if (column %in% c("X", "Y", "Z")) max(scales) * 1e-6 else 1e-10
    same <- if (is.numeric(a[[column]]) && is.numeric(b[[column]])) {
      all(is.finite(a[[column]]) & is.finite(b[[column]]) &
            abs(a[[column]] - b[[column]]) <= tolerance)
    } else {
      identical(a[[column]], b[[column]])
    }
    if (!isTRUE(same)) {
      stop("Output point attribute changed: ", column, " in ", path)
    }
  }
  fields <- c("Point Data Format ID", "Version Major", "Version Minor",
              "X scale factor", "Y scale factor", "Z scale factor",
              "X offset", "Y offset", "Z offset")
  if (!identical(actual@header@PHB[fields], expected@header@PHB[fields])) {
    stop("Output LAS format/version changed: ", path)
  }
  list(points = lidR::npoints(actual), area_m2 = lidR::area(actual),
       density = lidR::density(actual), bounds = sf::st_bbox(actual),
       attributes = names(actual), crs = sf::st_crs(actual)$wkt)
}

petawawa_save <- function(value, path) {
  temporary <- tempfile(".metadata-", tmpdir = dirname(path))
  on.exit(unlink(temporary))
  saveRDS(value, temporary, version = 3)
  if (!file.rename(temporary, path)) stop("Cannot publish preparation metadata: ", path)
}

petawawa_prepare_tile <- function(plan, row, density, stream) {
  config <- plan$config
  folder <- file.path(config$output_root, "1_org",
                      paste0("tile_1000_density_", density))
  if (!dir.exists(folder) && !dir.create(folder, recursive = TRUE)) {
    stop("Cannot create preparation output directory: ", folder)
  }
  if (normalizePath(folder, winslash = "/") != folder) {
    stop("Refusing redirected output directory: ", folder)
  }
  output <- file.path(folder, paste0(row$tile, ".laz"))
  index <- sub("\\.laz$", ".lax", output)
  metadata <- paste0(output, ".rds")
  if (file.exists(metadata)) {
    record <- readRDS(metadata)
    if (!all(file.exists(c(output, index))) ||
        !identical(petawawa_checksum(c(output, index)), record$md5)) {
      stop("Resume checksum mismatch/missing output: ", output)
    }
    return(invisible(record))
  }
  if (any(file.exists(c(output, index)))) {
    stop("Untracked/partial output exists; inspect it before resuming: ", output)
  }
  owner <- benchmark_scratch_create(config$output_root, config$source_root)
  stage <- owner$path
  on.exit(benchmark_scratch_cleanup(owner))
  las <- petawawa_validate_source(lidR::readLAS(row$path), row$tile, config$crs)
  expected <- petawawa_variant(las, density, stream)
  unsorted <- file.path(stage, "variant.laz")
  sorted <- file.path(stage, "sorted.laz")
  lidR::writeLAS(expected, unsorted)
  sort_log <- petawawa_process(plan$tools$path[plan$tools$name == "sort"],
    c("-i", unsorted, "-o", sorted, "-point_source", "-gps_time", "-olaz"))
  summary <- petawawa_validate_output(sorted, expected)
  index_log <- petawawa_process(plan$tools$path[plan$tools$name == "index"],
                                 c("-i", sorted))
  staged_index <- sub("\\.laz$", ".lax", sorted)
  if (!file.exists(staged_index) || file.info(staged_index)$size == 0) {
    stop("Index was not created: ", sorted)
  }
  record <- list(tile = row$tile, target_density = density, seed = stream,
                 source_md5 = row$md5, source_area_m2 = lidR::area(las),
                 source_density = lidR::density(las), summary = summary,
                 points_per_source_m2 = summary$points / lidR::area(las),
                 source_bounds = sf::st_bbox(las),
                 md5 = petawawa_checksum(c(sorted, staged_index)),
                 processes = list(sort = sort_log, index = index_log))
  if (!file.rename(sorted, output) || !file.rename(staged_index, index)) {
    stop("Cannot publish prepared tile; inspect partial output: ", output)
  }
  petawawa_save(record, metadata)
  invisible(record)
}

prepare_petawawa <- function(config, full = FALSE) {
  plan <- petawawa_preflight(config, full)
  manifest <- file.path(plan$config$output_root, "preparation.rds")
  if (file.exists(manifest)) {
    if (!identical(readRDS(manifest), plan)) {
      stop("Preparation identity changed; use a new empty output_root.")
    }
  } else {
    if (length(list.files(plan$config$output_root, all.files = TRUE, no.. = TRUE))) {
      stop("A new preparation requires an empty output_root.")
    }
    petawawa_save(plan, manifest)
  }
  for (density in plan$densities) {
    for (i in seq_len(nrow(plan$sources))) {
      position <- match(plan$sources$tile[i], petawawa_tiles())
      stream <- config$seed + (match(density, plan$densities) - 1L) * 100L + position
      petawawa_prepare_tile(plan, plan$sources[i, ], density, stream)
    }
  }
  invisible(plan)
}
