# Explicit CLI stages only; source() defines functions without downloading data.
source("R/2_data.R", local = TRUE)

fresh_policy <- function() {
  list(
    version = "fresh-petawawa-v4",
    source_filter = "-drop_synthetic before R decoding; audit counts; row IDs refer to filtered input",
    repair = paste(
      "Preserve XYZ precision and valid return labels; drop invalid legacy returns,",
      "duplicate XYZ (first row); then retain lowest-Z class-2 ground point per XY",
      "(first-row tie), audit removals; then repeated pulse/return (highest Z, first-row tie);",
      "raise NumberOfReturns to the pulse maximum reported count/return label.",
      "Pulse key: PointSourceID + gpstime (+ ScannerChannel if present).",
      "Do not infer pulse size from tile point counts; refuse remaining QC issues."
    ),
    density = "round(target * repaired source area); sample without replacement or add copies",
    synthetic = paste(
      "Seeded XY jitter uniformly within +/-0.25 m, quantized on source grid;",
      "reject collisions/outside source bounds; Z noise +/-seq(0.10,0.50,0.05) m;",
      "separate GPS-time ranges per copy; classifications/flags retained;",
      "synthetic derivation explicitly labeled in preparation metadata, not acquisition data."
    ),
    readiness = "Data QC only; cross-tool benchmark acceptance requires a new pilot.",
    negative_z = "Drop Z < 0 before all other repairs; audit filtered-input row IDs and original Z."
  )
}

fresh_subdir <- function(root, name) {
  path <- file.path(root, name)
  if (!dir.exists(path) && !dir.create(path)) stop("Cannot create directory: ", path)
  if (isTRUE(fs::is_link(path)) ||
      !identical(normalizePath(path, winslash = "/"), path)) {
    stop("Refusing redirected directory: ", path)
  }
  path
}

fresh_release_lock <- function(lock) {
  if (isTRUE(fs::is_link(lock)) ||
      length(list.files(lock, all.files = TRUE, no.. = TRUE)) ||
      unlink(lock, recursive = TRUE) != 0L) stop("Cannot safely release preparation lock: ", lock)
}

fresh_cores <- function(cores = 16L) {
  if (!is.numeric(cores) || length(cores) != 1L || is.na(cores) ||
      !is.finite(cores) || cores < 1 || cores != floor(cores) ||
      cores > .Machine$integer.max) stop("cores must be a positive integer.")
  as.integer(cores)
}

fresh_limit_threads <- function(envir = parent.frame()) {
  benchmark_validate_thread_runtime()
  variables <- c("OMP_NUM_THREADS", "OMP_THREAD_LIMIT", "OPENBLAS_NUM_THREADS",
                 "MKL_NUM_THREADS", "BLIS_NUM_THREADS", "VECLIB_MAXIMUM_THREADS",
                 "RCPP_PARALLEL_NUM_THREADS")
  previous_dt <- data.table::getDTthreads()
  previous_lidr <- lidR::get_lidr_threads()
  withr::defer({
    data.table::setDTthreads(previous_dt)
    lidR::set_lidr_threads(previous_lidr)
  }, envir = envir)
  withr::local_envvar(stats::setNames(rep("1", length(variables)), variables),
                     .local_envir = envir)
  data.table::setDTthreads(1L)
  lidR::set_lidr_threads(1L)
}

fresh_map <- function(jobs, cores = 16L, logdir, execute = "fresh_dispatch") {
  cores <- min(fresh_cores(cores), length(jobs))
  if (!length(jobs)) return(invisible(list()))
  fresh_limit_threads()
  result <- vector("list", length(jobs))
  completed <- 0L
  phase <- jobs[[1L]]$phase
  label <- if (identical(phase, "repair")) "Repair/check tiles" else
    if (identical(phase, "verify")) "Verify tiles" else "Prepare tiles"
  progress <- cli::cli_progress_bar(name = label, total = length(jobs),
    auto_terminate = FALSE)
  on.exit(cli::cli_progress_done(id = progress,
    result = if (completed == length(jobs)) "done" else "failed"), add = TRUE)
  if (cores == 1L) {
    fun <- if (is.character(execute)) get(execute, mode = "function") else execute
    for (i in seq_along(jobs)) {
      result[i] <- list(fun(jobs[[i]]))
      completed <- completed + 1L
      cli::cli_progress_update(id = progress, set = completed)
    }
    return(invisible(result))
  }
  active <- list()
  on.exit({
    for (process in active) {
      if (process$is_alive()) process$kill_tree()
      process$wait(5000)
      if (process$is_alive()) stop("Could not stop preparation worker.")
    }
  }, add = TRUE)
  next_job <- 1L
  while (next_job <= length(jobs) || length(active)) {
    while (next_job <= length(jobs) && length(active) < cores) {
      id <- as.character(next_job)
      active[[id]] <- callr::r_bg(function(job, execute, root) {
        setwd(root)
        source("renv/activate.R")
        source("R/prepare_fresh_data.R", local = TRUE)
        fresh_limit_threads()
        fun <- if (is.character(execute)) get(execute, mode = "function") else execute
        fun(job)
      }, args = list(job = jobs[[next_job]], execute = execute,
                      root = normalizePath(".", winslash = "/")),
      libpath = .libPaths(), user_profile = FALSE, system_profile = FALSE,
      stdout = file.path(logdir, paste0("worker-", id, ".stdout")),
      stderr = file.path(logdir, paste0("worker-", id, ".stderr")),
      supervise = TRUE)
      next_job <- next_job + 1L
    }
    finished <- names(active)[!vapply(active, function(p) p$is_alive(), logical(1))]
    for (id in finished) {
      process <- active[[id]]
      process$wait(1000)
      value <- tryCatch(process$get_result(), error = function(error) {
        stop("Preparation worker ", id, " failed: ", conditionMessage(error),
             ". Logs: ", logdir, call. = FALSE)
      })
      result[as.integer(id)] <- list(value)
      active[[id]] <- NULL
      completed <- completed + 1L
      cli::cli_progress_update(id = progress, set = completed)
    }
    if (length(active)) Sys.sleep(.05)
  }
  invisible(result)
}

fresh_dispatch <- function(job) {
  if (job$phase == "repair") return(fresh_repair_tile(job))
  if (job$phase == "verify") return(fresh_verify_tile(job))
  if (job$phase != "prepare") stop("Unknown preparation worker phase.")
  for (density in job$plan$densities) {
    position <- match(job$row$tile, petawawa_tiles())
    seed <- job$plan$config$seed +
      (match(density, job$plan$densities) - 1L) * 100L + position
    petawawa_prepare_tile(job$plan, job$row, density, seed, fresh_variant,
                          fresh_validate_output, fresh_validate_index, job$execution,
                          read = fresh_read_las)
  }
  list(tile = job$row$tile, status = "completed", pid = Sys.getpid())
}

fresh_upgrade_failed_pilot <- function(root, previous, identity) {
  if (!identical(previous$config, identity$config) ||
      !identical(previous$policy$version, "fresh-petawawa-v1") ||
      !identical(identity$policy$version, "fresh-petawawa-v2") ||
      !identical(previous$policy[c("density", "synthetic", "readiness")],
                 identity$policy[c("density", "synthetic", "readiness")])) {
    stop("Only the unchanged-config v1 to v2 ground-repair upgrade is supported.")
  }
  lock <- file.path(root, ".preparation-lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Preparation is active or interrupted: ", lock)
  on.exit(fresh_release_lock(lock), add = TRUE)
  pilot <- file.path(root, "pilot")
  if (!dir.exists(pilot) || isTRUE(fs::is_link(pilot)) ||
      dir.exists(file.path(root, "full")) ||
      file.exists(file.path(pilot, "ready.rds")) ||
      !file.exists(file.path(pilot, "stage-identity.rds"))) {
    stop("Upgrade requires a failed pilot without a ready/full stage.")
  }
  for (name in c("repaired", "prepared")) {
    path <- file.path(pilot, name)
    if (isTRUE(fs::is_link(path)) ||
        length(list.files(path, recursive = TRUE, all.files = TRUE, no.. = TRUE))) {
      stop("Upgrade refuses existing repaired/prepared files; use a new root.")
    }
  }
  scratch <- file.path(root, "scratch")
  if (isTRUE(fs::is_link(scratch)) ||
      length(list.files(scratch, all.files = TRUE, no.. = TRUE))) {
    stop("Inspect retained scratch before upgrading.")
  }
  stage <- readRDS(file.path(pilot, "stage-identity.rds"))
  if (!identical(stage$config, previous$config) ||
      !identical(stage$policy, previous$policy)) stop("Failed stage identity differs from root.")
  archived <- tempfile("pilot-failed-v1-", tmpdir = root)
  if (!file.rename(pilot, archived)) stop("Cannot archive failed pilot: ", pilot)
  petawawa_save(previous, file.path(archived, "root-identity-v1.rds"))
  petawawa_save(identity, file.path(root, "fresh-root.rds"))
  cli::cli_inform("Archived failed pilot at {.path {archived}}; downloaded archive unchanged.")
}

fresh_negative_z_policy <- function(previous, current) {
  expected <- current
  expected$version <- "fresh-petawawa-v3"
  expected$negative_z <- NULL
  identical(current$version, "fresh-petawawa-v4") &&
    identical(current, fresh_policy()) && identical(previous, expected)
}

fresh_negative_z_compatible <- function(previous, current) {
  fixed <- c("config", "archive_md5", "tools_md5", "versions", "runtime")
  fresh_negative_z_policy(previous$policy, current$policy) &&
    identical(previous[fixed], current[fixed]) &&
    identical(unname(previous$implementation[1:2]),
              c("3d79583687c92ed0cd74afab1defbbcb", "b0a94be880119b1df463a011fc1b76a2")) &&
    identical(previous$implementation[2], current$implementation[2]) &&
    identical(previous$implementation[-c(1, 2)], current$implementation[-c(1, 2)])
}

fresh_negative_z_paths <- function(root) {
  file.path(root, c("fresh-root.rds", "pilot/stage-identity.rds",
                   "pilot/prepared/preparation.rds", "pilot/ready.rds",
                   "full/stage-identity.rds"))
}

fresh_upgrade_negative_z <- function(root, previous, identity) {
  if (!identical(previous$config, identity$config) ||
      !fresh_negative_z_policy(previous$policy, identity$policy)) {
    stop("Negative-Z resume requires the unchanged-config v3 to v4 policy transition.")
  }
  lock <- file.path(root, ".preparation-lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Preparation is active or interrupted: ", lock)
  on.exit(fresh_release_lock(lock), add = TRUE)
  paths <- fresh_negative_z_paths(root)
  prepared <- file.path(root, "full", "prepared")
  if (!all(file.exists(paths)) || file.exists(file.path(root, "full", "ready.rds")) ||
      length(list.files(prepared, recursive = TRUE, all.files = TRUE, no.. = TRUE))) {
    stop("Negative-Z resume requires a verified pilot and interrupted full repair, without full prepared products.")
  }
  implementation <- petawawa_checksum(c("R/prepare_fresh_data.R", "R/2_data.R",
                                        "R/1_functions.R", "renv.lock", "ALS_tiles.gpkg"))
  for (stage in paths[c(2, 5)]) {
    old <- readRDS(stage)
    candidate <- old
    candidate$policy <- identity$policy
    candidate$implementation <- implementation
    if (!identical(old$config, identity$config) ||
        !identical(old$policy, previous$policy) ||
        !fresh_negative_z_compatible(old, candidate)) {
      stop("Negative-Z resume refuses unknown stage code, configuration or policy.")
    }
  }
  petawawa_save(list(previous = previous, current = identity,
                     frozen_md5 = petawawa_checksum(paths), implementation = implementation),
                file.path(root, "negative-z-transition.rds"))
}

fresh_root <- function(config, upgrade_failed_pilot = FALSE, resume_negative_z = FALSE,
                       verification_only = FALSE) {
  if (!is.list(config) || !all(c("root", "protected_roots", "archive_url",
      "archive_md5", "crs", "vertical_epsg", "metadata_confirmation", "seed",
      "lastools_path", "executables", "tool_license") %in% names(config))) {
    stop("Supply all settings from fresh-data.example.R.")
  }
  if (!is.character(config$root) || length(config$root) != 1L ||
      is.na(config$root) || !nzchar(config$root)) stop("Supply a fresh root.")
  parent <- benchmark_directory(dirname(config$root), "Fresh root parent", TRUE)
  root <- file.path(parent, basename(config$root))
  if (isTRUE(fs::is_link(root))) stop("Refusing linked fresh root.")
  for (protected in c(normalizePath(".", winslash = "/"), path.expand("~"),
                      config$protected_roots)) {
    if (!dir.exists(protected)) stop("Protected directory must exist: ", protected)
    if (benchmark_paths_overlap(root, normalizePath(protected, winslash = "/"))) {
      stop("Fresh root overlaps a protected directory: ", protected)
    }
  }
  if (root == dirname(root)) stop("Fresh root cannot be a filesystem root.")
  crs <- sf::st_crs(config$crs)
  if (is.na(crs) || !isTRUE(crs == sf::st_crs(2959)) ||
      crs$units_gdal != "metre" ||
      !is.numeric(config$vertical_epsg) || length(config$vertical_epsg) != 1L ||
      is.na(config$vertical_epsg) || config$vertical_epsg != 5713L ||
      !is.character(config$metadata_confirmation) ||
      length(config$metadata_confirmation) != 1L ||
      is.na(config$metadata_confirmation) || !nzchar(trimws(config$metadata_confirmation))) {
    stop("Confirm EPSG:2959, EPSG:5713 vertical datum and metre delivery metadata.")
  }
  if (!is.character(config$tool_license) || length(config$tool_license) != 1L ||
      is.na(config$tool_license) || !nzchar(trimws(config$tool_license))) {
    stop("Confirm tool_license for both preparation tools before initializing the root.")
  }
  if (!is.numeric(config$seed) || length(config$seed) != 1L || is.na(config$seed) ||
      config$seed < 0 || config$seed != floor(config$seed) ||
      config$seed > .Machine$integer.max - 600L) stop("Invalid seed.")
  if (!identical(config$archive_url, paste0(
      "https://opendata.nfis.org/downloads/petawawa/Raster/",
      "LiDAR%202012/PRF_LiDAR2012_LAS.zip"))) {
    stop("Only the documented original Petawawa archive URL is supported.")
  }
  if (!is.null(config$archive_md5) && (!is.character(config$archive_md5) ||
      length(config$archive_md5) != 1L || is.na(config$archive_md5) ||
      !grepl("^[[:xdigit:]]{32}$", config$archive_md5))) stop("Invalid archive_md5.")
  config$root <- root
  if (verification_only &&
      (!dir.exists(root) || !file.exists(file.path(root, "fresh-root.rds")))) {
    stop("Verification requires an existing preparation root and identity.")
  }
  if (!dir.exists(root) && !dir.create(root)) stop("Cannot create fresh root.")
  receipt <- file.path(root, "fresh-root.rds")
  identity <- list(config = config, policy = fresh_policy())
  if (file.exists(receipt)) {
    previous <- readRDS(receipt)
    if (!identical(previous, identity)) {
      transition <- file.path(root, "negative-z-transition.rds")
      if (file.exists(transition)) {
        record <- readRDS(transition)
        if (!identical(record$previous, previous) || !identical(record$current, identity) ||
            !fresh_negative_z_policy(previous$policy, identity$policy) ||
            !identical(record$frozen_md5, petawawa_checksum(fresh_negative_z_paths(root))) ||
            (!verification_only &&
             !identical(record$implementation, petawawa_checksum(c(
               "R/prepare_fresh_data.R", "R/2_data.R", "R/1_functions.R", "renv.lock", "ALS_tiles.gpkg"))))) {
          stop("Negative-Z transition differs from frozen identities or the current policy.")
        }
      } else if (resume_negative_z) {
        fresh_upgrade_negative_z(root, previous, identity)
      } else if (upgrade_failed_pilot) {
        fresh_upgrade_failed_pilot(root, previous, identity)
      } else {
        stop("Fresh configuration/policy changed; use a new root or the documented --resume-negative-z-full for v3.")
      }
    }
  } else {
    if (length(list.files(root, all.files = TRUE, no.. = TRUE))) {
      stop("Fresh root must be empty; existing files will not be overwritten.")
    }
    petawawa_save(identity, receipt)
  }
  config
}

fresh_archive <- function(config, download = FALSE) {
  folder <- fresh_subdir(config$root, "download")
  archive <- file.path(folder, "Petawawa2012.zip")
  receipt <- file.path(folder, "download.rds")
  if (file.exists(receipt)) {
    record <- readRDS(receipt)
    if (!identical(record$url, config$archive_url) ||
        !identical(record$md5, petawawa_checksum(archive))) {
      stop("Downloaded archive changed or is missing.")
    }
    return(archive)
  }
  if (file.exists(archive)) stop("Unreceipted archive; inspect it before continuing.")
  partial <- paste0(archive, ".part")
  if (!download) stop("Run --download first; the original archive is approximately 18 GB.")
  curl <- Sys.which("curl")
  if (!nzchar(curl)) stop("Install the curl executable for resumable downloads.")
  result <- benchmark_process(curl, c(
    "--fail", "--location", "--retry", "3", "--continue-at", "-",
    "--proto", "=https", "--proto-redir", "=https",
    "--output", partial, config$archive_url
  ))
  petawawa_save(result, file.path(folder, "transfer.rds"))
  if (result$status != 0L) stop("Archive download failed; partial retained: ", result$stderr)
  if (!file.exists(partial) || file.info(partial)$size == 0) stop("Empty download.")
  hash <- petawawa_checksum(partial)
  if (!is.null(config$archive_md5) &&
      !identical(hash, tolower(config$archive_md5))) stop("Archive MD5 mismatch; partial retained.")
  fresh_members(partial, petawawa_tiles())
  if (!file.rename(partial, archive)) stop("Cannot publish downloaded archive.")
  petawawa_save(list(url = config$archive_url, md5 = hash,
    bytes = unname(file.info(archive)$size),
    authenticity = if (is.null(config$archive_md5)) "No independent checksum supplied" else
      "Compared with user-supplied checksum"), receipt)
  archive
}

fresh_members <- function(archive, tiles) {
  listing <- utils::unzip(archive, list = TRUE)
  names <- gsub("\\\\", "/", listing$Name)
  if (!length(names) || anyNA(names) || anyDuplicated(names) ||
      any(grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))", names))) {
    stop("Unsafe or ambiguous ZIP member paths.")
  }
  members <- vapply(tiles, function(tile) {
    matches <- which(grepl(paste0("^", tile, "(_[[:alnum:]_-]+)?\\.(las|laz)$"),
                           basename(names), ignore.case = TRUE))
    if (length(matches) != 1L || listing$Length[matches] <= 0) {
      stop("Archive requires exactly one nonempty original LAS/LAZ for tile ", tile)
    }
    listing$Name[matches]
  }, character(1))
  unname(members)
}

fresh_read <- function(path, raw = FALSE, filter = "") {
  warnings <- character()
  # Filter native records before rlas populates the affected Boolean vectors.
  if (raw) filter <- trimws(paste("-drop_synthetic", filter))
  las <- withCallingHandlers(lidR::readLAS(path, filter = filter), warning = function(condition) {
    warnings <<- c(warnings, conditionMessage(condition))
    invokeRestart("muffleWarning")
  })
  if (is.null(las) || lidR::npoints(las) == 0) stop("Empty/unreadable LAS: ", path)
  if (length(warnings) &&
      (!raw || any(grepl("synthetic", warnings, ignore.case = TRUE)))) {
    stop("LAS read warning: ", paste(warnings, collapse = "; "), " [", path, "]")
  }
  fresh_check_flags(las)
  list(las = las, warnings = warnings)
}

fresh_read_las <- function(path) fresh_read(path)$las

fresh_check_repair_policy <- function(receipt, path) {
  if (identical(receipt$policy, fresh_policy())) return(invisible(TRUE))
  actions <- receipt$actions
  if (!is.null(receipt$policy) || !is.data.frame(actions) ||
      !"source_Z" %in% names(actions) || !is.numeric(actions$source_Z) ||
      anyNA(actions$source_Z) || any(!is.finite(actions$source_Z)) ||
      any(actions$source_Z < 0)) {
    stop("Cannot reuse prior repair under the negative-Z policy; missing/changed policy or ",
         "audit includes negative source elevations: ", path)
  }
  invisible(TRUE)
}

fresh_check_flags <- function(las) {
  flags <- las$Synthetic_flag
  if (!is.logical(flags) || length(flags) != lidR::npoints(las) ||
      anyNA(flags) || any(flags)) {
    stop("Fresh data require complete logical synthetic flags with no flagged points.")
  }
}

fresh_qc <- function(las, tile, crs) {
  fresh_check_flags(las)
  las <- petawawa_validate_source(las, tile, crs)
  check <- lidR::las_check(las, print = FALSE)
  if (length(c(check$errors, check$warnings))) {
    stop("LAS QC failed for ", tile, ": ", paste(c(check$errors, check$warnings), collapse = "; "))
  }
  ground <- las@data[las@data$Classification %in% c(2L, 9L), c("X", "Y"), with = FALSE]
  if (nrow(unique(ground)) < 3L ||
      length(chull(ground$X, ground$Y)) < 3L) stop("Insufficient non-collinear terrain points: ", tile)
  list(check = check, points = lidR::npoints(las), area_m2 = lidR::area(las),
       bounds = sf::st_bbox(las), crs = sf::st_crs(las)$wkt)
}

fresh_repair <- function(las, tile, crs) {
  fresh_check_flags(las)
  if (!is.na(sf::st_crs(las)) && !isTRUE(sf::st_crs(las) == sf::st_crs(crs))) {
    stop("Raw source CRS conflicts with confirmed delivery metadata: ", tile)
  }
  required <- c("X", "Y", "Z", "gpstime", "PointSourceID",
                "ReturnNumber", "NumberOfReturns", "Classification")
  data <- data.table::copy(las@data)
  if (!all(required %in% names(data)) || anyNA(data) ||
      any(!is.finite(as.matrix(data[, required, with = FALSE])))) {
    stop("Missing/nonfinite raw attributes: ", tile)
  }
  actions <- list()
  rows <- seq_len(nrow(data))
  drop <- function(mask, reason, kept_source_row = rep(NA_integer_, sum(mask)),
                   kept_Z = rep(NA_real_, sum(mask))) {
    actions[[length(actions) + 1L]] <<- data.frame(
      source_row = rows[mask], action = rep(reason, sum(mask)),
      old_count = data$NumberOfReturns[mask], new_count = rep(NA_integer_, sum(mask)),
      source_Z = data$Z[mask], kept_source_row = kept_source_row, kept_Z = kept_Z
    )
    rows <<- rows[!mask]
    data <<- data[!mask]
  }
  drop(data$Z < 0, "drop_negative_Z")
  if (!nrow(data)) stop("Repair removed every point: ", tile)
  drop(data$ReturnNumber < 1L | data$ReturnNumber > 7L |
         data$NumberOfReturns < 1L | data$NumberOfReturns > 7L, "drop_invalid_return")
  drop(duplicated(data, by = c("X", "Y", "Z")), "drop_duplicate_XYZ")
  ground <- which(data$Classification == 2L)
  ranked <- ground[order(data$Z[ground], rows[ground])]
  repeated <- duplicated(data[ranked], by = c("X", "Y"))
  keepers <- ranked[!repeated]
  removed <- ranked[repeated]
  mask <- rep(FALSE, nrow(data))
  mask[removed] <- TRUE
  removed <- which(mask)
  retained <- keepers[data[keepers, c("X", "Y"), with = FALSE][
    data[removed, c("X", "Y"), with = FALSE], on = c("X", "Y"), which = TRUE]]
  drop(mask, "drop_ground_XY_higher_Z", rows[retained], data$Z[retained])
  pulse <- c("PointSourceID", "gpstime", intersect("ScannerChannel", names(data)))
  ranked <- order(-data$Z, rows)
  repeated <- duplicated(data[ranked], by = c(pulse, "ReturnNumber"))
  mask <- rep(FALSE, nrow(data))
  mask[ranked[repeated]] <- TRUE
  drop(mask, "drop_repeated_pulse_return")
  if (!nrow(data)) stop("Repair removed every point: ", tile)
  counts <- data[, .(count = max(NumberOfReturns, ReturnNumber)), by = pulse]
  new_counts <- counts[data, on = pulse]$count
  changed <- new_counts != data$NumberOfReturns
  actions[[length(actions) + 1L]] <- data.frame(
    source_row = rows[changed], action = rep("raise_return_count", sum(changed)),
    old_count = data$NumberOfReturns[changed], new_count = new_counts[changed],
    source_Z = data$Z[changed], kept_source_row = rep(NA_integer_, sum(changed)),
    kept_Z = rep(NA_real_, sum(changed))
  )
  data$NumberOfReturns <- as.integer(new_counts)
  result <- lidR::LAS(data, header = las@header, crs = sf::st_crs(las), check = FALSE)
  sf::st_crs(result) <- sf::st_crs(crs)
  result <- lidR::las_update(result)
  qc <- fresh_qc(result, tile, crs)
  list(las = result, actions = do.call(rbind, actions), qc = qc,
       retained_source_rows = rows)
}

fresh_variant <- function(las, density, seed) {
  withr::local_seed(seed, .rng_kind = "Mersenne-Twister",
                   .rng_normal_kind = "Inversion", .rng_sample_kind = "Rejection")
  original <- as.data.frame(las@data)
  count <- round(density * lidR::area(las))
  if (!is.finite(count) || count < 1 || count > .Machine$integer.max) {
    stop("Invalid target point count.")
  }
  if (count <= nrow(original)) {
    result <- las[sample.int(nrow(original), count)]
    return(lidR::las_update(result))
  }
  header <- las@header@PHB
  scales <- unlist(header[c("X scale factor", "Y scale factor", "Z scale factor")])
  offsets <- unlist(header[c("X offset", "Y offset", "Z offset")])
  bounds <- sf::st_bbox(las)
  copies <- list(original)
  occupied <- data.table::as.data.table(original[c("X", "Y")])
  step <- diff(range(original$gpstime)) + 1
  remaining <- count - nrow(original)
  iteration <- 0L
  while (remaining > 0) {
    iteration <- iteration + 1L
    n <- min(remaining, nrow(original))
    selected <- sample.int(nrow(original), n)
    copy <- original[selected, , drop = FALSE]
    pending <- seq_len(n)
    for (attempt in seq_len(50L)) {
      for (j in 1:2) {
        field <- c("X", "Y")[j]
        values <- original[[field]][selected[pending]] + runif(length(pending), -.25, .25)
        copy[[field]][pending] <- round((values - offsets[j]) / scales[j]) * scales[j] + offsets[j]
      }
      xy <- data.table::as.data.table(copy[c("X", "Y")])
      bad <- copy$X < bounds["xmin"] | copy$X > bounds["xmax"] |
        copy$Y < bounds["ymin"] | copy$Y > bounds["ymax"] |
        duplicated(xy) | !is.na(occupied[xy, on = c("X", "Y"), which = TRUE, mult = "first"])
      pending <- which(bad)
      if (!length(pending)) break
    }
    if (length(pending)) stop("Cannot create collision-free synthetic XY on source grid.")
    copy$Z <- round((copy$Z + sample(c(-seq(.1, .5, .05), seq(.1, .5, .05)),
                                    n, replace = TRUE) - offsets[3]) / scales[3]) *
      scales[3] + offsets[3]
    copy$gpstime <- copy$gpstime + step * iteration
    if (any(!is.finite(copy$gpstime)) ||
        min(copy$gpstime) <= max(copies[[length(copies)]]$gpstime)) {
      stop("Cannot assign disjoint synthetic GPS-time ranges.")
    }
    copies[[length(copies) + 1L]] <- copy
    occupied <- rbind(occupied, data.table::as.data.table(copy[c("X", "Y")]))
    remaining <- remaining - n
  }
  result <- lidR::LAS(data.table::rbindlist(copies), header = las@header,
                      crs = sf::st_crs(las), check = FALSE)
  lidR::las_update(result)
}

fresh_validate_output <- function(path, expected) {
  las <- fresh_read_las(path)
  summary <- petawawa_validate_output(path, expected, read = function(path) las)
  tile <- paste(as.integer(floor(summary$bounds[c("xmin", "ymin")] / 1000) * 1000),
                collapse = "_")
  summary$quality <- fresh_qc(las, tile, sf::st_crs(expected))
  summary
}

fresh_index_signature <- function(path) {
  index <- sub("\\.laz$", ".lax", path)
  if (!file.exists(index) || file.info(index)$size < 4 ||
      !identical(readBin(index, "raw", n = 4L), charToRaw("LASX"))) {
    stop("Invalid LAX index: ", index)
  }
  invisible(TRUE)
}

fresh_query_index_child <- function(args) {
  if (length(args) != 6L) stop("Internal index query requires input, output and four coordinates.")
  rectangle <- as.numeric(args[3:6])
  if (any(!is.finite(rectangle)) || any(rectangle[1:2] >= rectangle[3:4])) {
    stop("Invalid index query rectangle.")
  }
  if (file.exists(args[2])) stop("Index query output already exists.")
  fresh_index_signature(args[1])
  las <- fresh_read(args[1], filter = paste("-inside", paste(args[3:6], collapse = " ")))$las
  ordered <- las[order(las$PointSourceID, las$gpstime)]
  lidR::writeLAS(lidR::las_update(ordered), args[2])
  invisible(TRUE)
}

fresh_validate_index <- function(path, expected, tools) {
  fresh_index_signature(path)
  data <- expected@data
  bounds <- sf::st_bbox(expected)
  scales <- unlist(expected@header@PHB[c("X scale factor", "Y scale factor")])
  center <- c(data$X[1], data$Y[1])
  width <- c(bounds["xmax"] - bounds["xmin"], bounds["ymax"] - bounds["ymin"]) / 8
  rectangle <- c(center - width - scales * .37, center + width + scales * .37)
  reference <- expected[data$X >= rectangle[1] & data$Y >= rectangle[2] &
                          data$X <= rectangle[3] & data$Y <= rectangle[4]]
  query <- tempfile("index-query-", tmpdir = dirname(path), fileext = ".laz")
  on.exit(unlink(query))
  coordinates <- format(rectangle, digits = 17, scientific = FALSE, trim = TRUE)
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  process <- petawawa_process(rscript, c(
    "--vanilla", normalizePath("R/prepare_fresh_data.R", winslash = "/"),
    "--query-index", path, query, coordinates
  ))
  if (grepl("\\b(warning|error)\\b", paste(process$stdout, process$stderr), ignore.case = TRUE)) {
    stop("Native index query reported a warning/error: ", process$stdout, process$stderr)
  }
  summary <- petawawa_validate_output(query, reference, read = fresh_read_las)
  list(status = "passed", rectangle = rectangle, points = summary$points, process = process,
       method = "LASX signature plus isolated lidR/rlas native indexed query versus full decoded point records")
}

fresh_verify <- function(folder, tiles, cores = 16L) {
  cores <- fresh_cores(cores)
  manifest <- file.path(folder, "prepared", "preparation.rds")
  plan <- readRDS(manifest)
  ready <- file.path(folder, "ready.rds")
  if (file.exists(ready) &&
      !identical(readRDS(ready)$preparation_md5, petawawa_checksum(manifest))) {
    stop("Preparation manifest differs from readiness receipt.")
  }
  if (!identical(plan$sources$tile, sort(tiles))) stop("Prepared tile inventory differs.")
  if (!is.null(plan$fresh$reused_repairs)) {
    reuse_path <- file.path(folder, "repair-reuse.rds")
    if (!identical(petawawa_checksum(reuse_path), plan$fresh$reused_repairs)) {
      stop("Repair-reuse receipt changed.")
    }
    reuse <- readRDS(reuse_path)
    if (!identical(petawawa_checksum(reuse$previous_stage), reuse$previous_stage_md5) ||
        !identical(petawawa_checksum(reuse$previous_preparation), reuse$previous_preparation_md5)) {
      stop("Archived preparation provenance changed.")
    }
  }
  repair_receipts <- paste0(plan$sources$path, ".rds")
  if (!identical(plan$sources$path,
                 file.path(folder, "repaired", paste0(plan$sources$tile, ".laz"))) ||
      !identical(petawawa_checksum(repair_receipts), plan$fresh$repairs_md5)) {
    stop("Repair receipts or source bytes changed.")
  }
  expected_files <- c("preparation.rds", unlist(lapply(plan$densities, function(density) {
    prefix <- file.path("1_org", paste0("tile_1000_density_", density), tiles)
    c(paste0(prefix, ".laz"), paste0(prefix, ".lax"), paste0(prefix, ".laz.rds"))
  })))
  actual_files <- list.files(file.path(folder, "prepared"), recursive = TRUE,
                            all.files = TRUE)
  if (!setequal(actual_files, expected_files)) stop("Unexpected/missing prepared inventory.")
  frozen_paths <- c(manifest, repair_receipts)
  frozen_md5 <- petawawa_checksum(frozen_paths)
  logdir <- fresh_subdir(folder, basename(tempfile("verification-")))
  petawawa_save(list(started = Sys.time(), requested_cores = cores,
    workers = min(cores, length(tiles)), threads_per_worker = 1L,
    preparation_md5 = frozen_md5[1],
    implementation = petawawa_checksum(c("R/prepare_fresh_data.R", "R/2_data.R",
                                         "R/1_functions.R", "renv.lock"))),
    file.path(logdir, "started.rds"))
  completed <- FALSE
  on.exit(petawawa_save(list(status = if (completed) "completed" else "failed_or_interrupted",
    ended = Sys.time()), file.path(logdir, "finished.rds")), add = TRUE)
  cli::cli_inform("Verification: up to {min(cores, length(tiles))} tile workers; logs in {.path {logdir}}.")
  jobs <- lapply(tiles, function(tile) list(phase = "verify", tile = tile,
    folder = folder, plan = plan, logdir = logdir))
  fresh_map(jobs, cores, logdir)
  if (!identical(petawawa_checksum(frozen_paths), frozen_md5) ||
      !setequal(list.files(file.path(folder, "prepared"), recursive = TRUE,
                          all.files = TRUE), expected_files)) {
    stop("Preparation records or inventory changed during verification.")
  }
  completed <- TRUE
  invisible(TRUE)
}

fresh_verify_tile <- function(job) {
  tile <- job$tile
  plan <- job$plan
  source_md5 <- plan$sources$md5[match(tile, plan$sources$tile)]
  repaired <- file.path(job$folder, "repaired", paste0(tile, ".laz"))
  receipt <- readRDS(paste0(repaired, ".rds"))
  fresh_check_repair_policy(receipt, repaired)
  if (!identical(petawawa_checksum(repaired), receipt$md5) ||
      !identical(receipt$md5, source_md5)) stop("Repaired source checksum mismatch: ", tile)
  fresh_qc(fresh_read(repaired)$las, tile, plan$config$crs)
  for (density in plan$densities) {
    path <- file.path(job$folder, "prepared", "1_org",
                      paste0("tile_1000_density_", density), paste0(tile, ".laz"))
    record <- readRDS(paste0(path, ".rds"))
    if (!identical(petawawa_checksum(c(path, sub("\\.laz$", ".lax", path))), record$md5)) {
      stop("Prepared output/index checksum mismatch: ", path)
    }
    fresh_index_signature(path)
    if (!identical(record$index_validation$status, "passed")) stop("Missing index-query validation.")
    qc <- fresh_qc(fresh_read(path)$las, tile, plan$config$crs)
    if (qc$points != round(density * receipt$qc$area_m2) ||
        !identical(record$source_md5, source_md5)) {
      stop("Target density count/source identity mismatch: ", path)
    }
  }
  result <- list(tile = tile, status = "verified", pid = Sys.getpid(), finished = Sys.time())
  petawawa_save(result, file.path(job$logdir, paste0(tile, ".rds")))
  result
}

fresh_verify_stage <- function(config, full = FALSE, cores = 16L) {
  cores <- fresh_cores(cores)
  config <- fresh_root(config, verification_only = TRUE)
  lock <- file.path(config$root, ".preparation-lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Preparation is active or interrupted: ", lock)
  on.exit(fresh_release_lock(lock), add = TRUE)
  folder <- file.path(config$root, if (full) "full" else "pilot")
  stage_path <- file.path(folder, "stage-identity.rds")
  manifest <- file.path(folder, "prepared", "preparation.rds")
  stage <- readRDS(stage_path)
  plan <- readRDS(manifest)
  if (!identical(stage$config, config) || !identical(plan$fresh$config, config) ||
      !identical(plan$parameters, fresh_policy()) ||
      !identical(plan$fresh$archive_md5, stage$archive_md5)) {
    stop("Verification requires matching frozen configuration, current repair policy and archive identity.")
  }
  frozen_paths <- c(file.path(config$root, "fresh-root.rds"), stage_path, manifest)
  transition <- file.path(config$root, "negative-z-transition.rds")
  if (file.exists(transition)) frozen_paths <- c(frozen_paths, transition)
  frozen_md5 <- petawawa_checksum(frozen_paths)
  fresh_verify(folder, if (full) petawawa_tiles() else c("298000_5093000", "298000_5094000"),
               cores = cores)
  if (!identical(petawawa_checksum(frozen_paths), frozen_md5)) {
    stop("Frozen preparation identity changed during verification.")
  }
  ready <- file.path(folder, "ready.rds")
  if (!file.exists(ready)) {
    petawawa_save(list(status = "data-qc-passed", policy = fresh_policy(),
      archive_md5 = plan$fresh$archive_md5, preparation_md5 = petawawa_checksum(manifest)),
      ready)
  }
  cli::cli_alert_success("Data QC passed: {.path {file.path(folder, 'prepared')}}.")
  cli::cli_inform("Benchmark acceptance still requires a new pilot.")
  invisible(TRUE)
}

fresh_restart_failed_preparation <- function(folder, previous, identity, tiles) {
  fixed <- c("config", "policy", "archive_md5", "members", "tools_md5", "versions", "runtime")
  if (!identical(previous[fixed], identity[fixed])) {
    stop("Preparation retry requires unchanged inputs, repair policy, configuration, tools and runtime.")
  }
  if (file.exists(file.path(folder, "ready.rds")) ||
      dir.exists(file.path(dirname(folder), "full"))) {
    stop("Preparation retry refuses a ready pilot or full stage.")
  }
  prepared <- file.path(folder, "prepared")
  if (isTRUE(fs::is_link(prepared))) stop("Refusing redirected prepared directory.")
  if (length(list.files(prepared, pattern = "^lidar-run-", all.files = TRUE)) ||
      length(list.files(file.path(dirname(folder), "scratch"), all.files = TRUE, no.. = TRUE))) {
    stop("Inspect retained scratch before retrying preparation.")
  }
  plan <- readRDS(file.path(prepared, "preparation.rds"))
  paths <- file.path(folder, "repaired", paste0(sort(tiles), ".laz"))
  if (!identical(plan$sources$path, paths) ||
      !identical(plan$sources$tile, sort(tiles)) ||
      !identical(plan$parameters, identity$policy) ||
      !identical(plan$fresh$archive_md5, identity$archive_md5) ||
      !identical(petawawa_checksum(paths), plan$sources$md5) ||
      !identical(petawawa_checksum(paste0(paths, ".rds")), plan$fresh$repairs_md5)) {
    stop("Repair source identity/receipts changed; cannot reuse repaired tiles.")
  }
  for (i in seq_along(paths)) {
    receipt <- readRDS(paste0(paths[i], ".rds"))
    if (!identical(receipt$md5, plan$sources$md5[i]) ||
        !identical(receipt$archive_member, identity$members[i])) {
      stop("Repaired tile receipt differs from frozen source identity.")
    }
    fresh_qc(fresh_read(paths[i])$las, sort(tiles)[i], plan$config$crs)
  }
  archived <- fresh_subdir(dirname(folder), basename(tempfile("pilot-preparation-failed-")))
  petawawa_save(previous, file.path(archived, "stage-identity.rds"))
  reuse <- file.path(folder, "repair-reuse.rds")
  if (file.exists(reuse) && !file.copy(reuse, file.path(archived, "repair-reuse.rds"))) {
    stop("Cannot preserve previous repair-reuse receipt.")
  }
  if (!file.rename(prepared, file.path(archived, "prepared"))) {
    stop("Cannot archive failed preparation; existing files were not overwritten.")
  }
  petawawa_save(list(
    previous_stage = file.path(archived, "stage-identity.rds"),
    previous_preparation = file.path(archived, "prepared", "preparation.rds"),
    previous_stage_md5 = petawawa_checksum(file.path(archived, "stage-identity.rds")),
    previous_preparation_md5 = petawawa_checksum(file.path(archived, "prepared", "preparation.rds")),
    sources = plan$sources, repair_receipts_md5 = plan$fresh$repairs_md5,
    reason = "Explicit failed-preparation retry; repaired bytes/QC reverified; regenerate every variant."
  ), reuse)
  identity$reused_repairs <- petawawa_checksum(reuse)
  petawawa_save(identity, file.path(folder, "stage-identity.rds"))
  cli::cli_inform("Archived prior preparation at {.path {archived}}; repaired tiles and download unchanged.")
  identity
}

fresh_parallel_compatible <- function(previous, current) {
  if (fresh_negative_z_compatible(previous, current)) return(TRUE)
  fixed <- c("config", "policy", "archive_md5", "tools_md5", "versions", "runtime")
  known <- list(
    c("00be03b000101fc9301c8dfef921b2bb", "99a43ae69c2696ffafe37c7891b03b1b"),
    c("066c8ee2c0f9d7fe0a440d3686160d53", "aa12df90c7d03eaebe284d1f4ced285c")
  )
  identical(previous[fixed], current[fixed]) &&
    (identical(previous$implementation, current$implementation) ||
       (any(vapply(known, function(code) {
          identical(unname(previous$implementation[1:2]), code)
        }, logical(1))) &&
          identical(previous$implementation[-c(1, 2)], current$implementation[-c(1, 2)])))
}

fresh_archive_interrupted <- function(folder, tiles, execution_dir) {
  archive <- fresh_subdir(execution_dir, "interrupted")
  move <- function(path, destination) {
    benchmark_check_scratch_tree(path)
    parent <- dirname(destination)
    if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE)) stop("Cannot create recovery archive.")
    if (!file.rename(path, destination)) stop("Cannot archive interrupted path: ", path)
  }
  for (root in c(file.path(dirname(folder), "scratch"), file.path(folder, "prepared"))) {
    if (!dir.exists(root)) next
    paths <- list.files(root, pattern = "^lidar-run-", full.names = TRUE)
    for (path in paths) move(path, file.path(archive, basename(root), basename(path)))
  }
  for (tile in tiles) {
    source <- file.path(folder, "repaired", paste0(tile, ".laz"))
    if (file.exists(source) && !file.exists(paste0(source, ".rds"))) {
      move(source, file.path(archive, "repaired", basename(source)))
    }
    for (density in c(1, 2, 5, 10, 20, 50)) {
      path <- file.path(folder, "prepared", "1_org",
                        paste0("tile_1000_density_", density), paste0(tile, ".laz"))
      if (!file.exists(paste0(path, ".rds"))) {
        for (partial in c(path, sub("\\.laz$", ".lax", path))) {
          if (file.exists(partial)) move(partial,
            file.path(archive, paste0("density_", density), basename(partial)))
        }
      }
    }
  }
}

fresh_repair_tile <- function(job) {
  config <- job$config
  tile <- job$tile
  path <- file.path(job$folder, "repaired", paste0(tile, ".laz"))
  receipt <- paste0(path, ".rds")
  if (file.exists(receipt)) {
    record <- readRDS(receipt)
    fresh_check_repair_policy(record, path)
    if (!identical(record$md5, petawawa_checksum(path))) {
      stop("Repaired tile checksum mismatch: ", path)
    }
    fresh_qc(fresh_read_las(path), tile, config$crs)
    return(list(tile = tile, status = "resumed", pid = Sys.getpid()))
  }
  if (file.exists(path)) stop("Partial repaired tile; inspect before continuing: ", path)
  owner <- benchmark_scratch_create(file.path(config$root, "scratch"),
    c(dirname(job$archive), dirname(path), file.path(job$folder, "prepared"), config$protected_roots))
  tryCatch({
    extracted <- utils::unzip(job$archive, files = job$member, exdir = owner$path,
                              junkpaths = TRUE, overwrite = FALSE)
    if (length(extracted) != 1L || isTRUE(fs::is_link(extracted)) ||
        !benchmark_path_within(normalizePath(extracted, winslash = "/"), owner$path)) {
      stop("Unsafe/failed archive extraction.")
    }
    source_header <- lidR::readLASheader(extracted)
    source_points <- lidR::npoints(source_header)
    raw <- fresh_read(extracted, raw = TRUE)
    source_filter <- list(
      filter = "-drop_synthetic", source_points = source_points,
      retained_points = lidR::npoints(raw$las),
      discarded_synthetic_points = source_points - lidR::npoints(raw$las),
      row_reference = "Row IDs refer to native-filtered input, not original archive records."
    )
    if (!is.finite(source_points) || source_filter$discarded_synthetic_points < 0) {
      stop("Invalid source/filter point counts: ", tile)
    }
    petawawa_save(list(tile = tile, member = job$member, warnings = raw$warnings,
                      source_filter = source_filter),
                  file.path(job$folder, paste0(tile, "-read.rds")))
    fixed <- fresh_repair(raw$las, tile, config$crs)
    lidR::writeLAS(fixed$las, path)
    persisted <- fresh_read(path)$las
    fresh_qc(persisted, tile, config$crs)
    encoding <- c("Point Data Format ID", "Version Major", "Version Minor",
                  "X scale factor", "Y scale factor", "Z scale factor",
                  "X offset", "Y offset", "Z offset")
    if (!identical(persisted@header@PHB[encoding], raw$las@header@PHB[encoding])) {
      stop("Repair round-trip changed original coordinate encoding/LAS format.")
    }
    if (!identical(as.data.frame(persisted@data), as.data.frame(fixed$las@data))) {
      stop("Repair round-trip changed point attributes.")
    }
    petawawa_save(list(tile = tile, archive_member = job$member, policy = fresh_policy(),
      raw_md5 = petawawa_checksum(extracted), raw_warnings = raw$warnings,
      source_filter = source_filter,
      source_header = source_header, actions = fixed$actions,
      retained_source_rows = fixed$retained_source_rows, qc = fixed$qc,
      md5 = petawawa_checksum(path), execution = job$execution), receipt)
    list(tile = tile, status = "completed", pid = Sys.getpid())
  }, error = function(error) {
    petawawa_save(list(tile = tile, diagnostic = conditionMessage(error)),
                  file.path(job$folder, paste0(tile, "-failure.rds")))
    stop(error)
  }, finally = benchmark_scratch_cleanup(owner))
}

fresh_prepare <- function(config, full = FALSE, retry_failed_preparation = FALSE,
                          cores = 16L, resume_parallel = FALSE) {
  cores <- fresh_cores(cores)
  fresh_limit_threads()
  config <- fresh_root(config)
  lock <- file.path(config$root, ".preparation-lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Preparation is active or interrupted: ", lock)
  on.exit(fresh_release_lock(lock), add = TRUE)
  archive <- fresh_archive(config)
  all_tiles <- petawawa_tiles()
  pilot <- c("298000_5093000", "298000_5094000")
  if (full) {
    receipt <- file.path(config$root, "pilot", "ready.rds")
    if (!file.exists(receipt)) stop("Prepare and verify the two-tile pilot before --full.")
    fresh_verify(file.path(config$root, "pilot"), pilot, cores = cores)
  }
  tiles <- if (full) all_tiles else pilot
  members <- fresh_members(archive, tiles)
  folder <- fresh_subdir(config$root, if (full) "full" else "pilot")
  if (!is.character(config$tool_license) || length(config$tool_license) != 1L ||
      is.na(config$tool_license) || !nzchar(trimws(config$tool_license))) {
    stop("Confirm tool_license for the selected preparation tools before repair.")
  }
  if (!is.character(config$executables) ||
      !identical(sort(names(config$executables)), c("index", "sort"))) {
    stop("Explicitly select sort and index executables.")
  }
  tools <- vapply(config$executables, function(tool) {
    benchmark_lastools_executable(config$lastools_path, tool)
  }, character(1))
  stage_identity <- list(
    config = config, policy = fresh_policy(), archive_md5 = petawawa_checksum(archive),
    members = members, tools_md5 = petawawa_checksum(tools),
    versions = lapply(tools, petawawa_process, args = "-version"),
    implementation = petawawa_checksum(c("R/prepare_fresh_data.R", "R/2_data.R",
                                         "R/1_functions.R", "renv.lock", "ALS_tiles.gpkg")),
    runtime = list(R = as.character(getRversion()), lidR = as.character(utils::packageVersion("lidR")),
      rlas = as.character(utils::packageVersion("rlas")), sf = sf::sf_extSoftVersion())
  )
  stage_receipt <- file.path(folder, "stage-identity.rds")
  if (resume_parallel && file.exists(file.path(folder, "ready.rds"))) {
    stop("Parallel resume requires an interrupted full stage, not completed data.")
  }
  transition_path <- file.path(folder, "parallel-transition.rds")
  transition <- if (file.exists(transition_path)) readRDS(transition_path) else NULL
  reuse <- file.path(folder, "repair-reuse.rds")
  if (file.exists(reuse)) stage_identity$reused_repairs <- petawawa_checksum(reuse)
  parallel_allowed <- resume_parallel ||
    (!is.null(transition) && identical(transition$current, stage_identity)) ||
    (file.exists(stage_receipt) && identical(readRDS(stage_receipt), stage_identity))
  if (full) {
    pilot_identity <- readRDS(file.path(config$root, "pilot", "stage-identity.rds"))
    frozen <- c("implementation", "tools_md5", "versions", "runtime")
    if (!identical(stage_identity[frozen], pilot_identity[frozen]) &&
        !(parallel_allowed && fresh_parallel_compatible(pilot_identity, stage_identity))) {
      stop("Implementation/tools/runtime differ from the verified pilot; ",
           "use the documented --resume-parallel-full for the supported serial-to-parallel transition.")
    }
  }
  if (file.exists(stage_receipt)) {
    previous <- readRDS(stage_receipt)
    if (retry_failed_preparation) {
      if (full) stop("Failed-preparation retry is supported only for the pilot.")
      stage_identity <- fresh_restart_failed_preparation(folder, previous, stage_identity, tiles)
    } else if (!identical(previous, stage_identity)) {
      if (full && parallel_allowed && fresh_parallel_compatible(previous, stage_identity) &&
          identical(previous$members, stage_identity$members)) {
        if (!resume_parallel &&
            (is.null(transition) || !identical(transition$previous, previous))) {
          stop("Parallel transition receipt differs from the frozen stage.")
        }
        if (resume_parallel) petawawa_save(list(previous = previous, current = stage_identity),
                                           transition_path)
      } else {
        stop("Stage implementation/archive/tools/runtime changed; use a new root or the documented --retry-failed-preparation.")
      }
    }
  } else {
    if (retry_failed_preparation) stop("No existing failed preparation to retry.")
    if (length(list.files(folder, all.files = TRUE, no.. = TRUE))) {
      stop("Untracked preparation stage; use a new root.")
    }
    petawawa_save(stage_identity, stage_receipt)
  }
  repaired <- fresh_subdir(folder, "repaired")
  output <- fresh_subdir(folder, "prepared")
  scratch <- fresh_subdir(config$root, "scratch")
  execution_dir <- fresh_subdir(folder, basename(tempfile("execution-")))
  execution <- list(directory = execution_dir, requested_cores = cores,
    workers = min(cores, length(tiles)), threads_per_worker = 1L,
    implementation = stage_identity$implementation, started = Sys.time())
  petawawa_save(execution, file.path(execution_dir, "started.rds"))
  completed <- FALSE
  on.exit(petawawa_save(list(status = if (completed) "completed" else "failed_or_interrupted",
                            ended = Sys.time()), file.path(execution_dir, "finished.rds")), add = TRUE)
  if (resume_parallel) fresh_archive_interrupted(folder, tiles, execution_dir)
  cli::cli_inform("Preparation: up to {execution$workers} tile workers; one native thread each.")
  repair_logs <- fresh_subdir(execution_dir, "repair")
  repair_jobs <- lapply(seq_along(tiles), function(i) list(phase = "repair",
    config = config, archive = archive, member = members[i], tile = tiles[i],
    folder = folder, execution = execution))
  fresh_map(repair_jobs, cores, repair_logs)
  prep_config <- list(
    source_root = repaired, output_root = output,
    source_citation = paste(
      "Natural Resources Canada / Canadian Forest Service, Canadian Wood Fibre Centre;",
      "Petawawa Research Forest LiDAR 2012;", config$archive_url),
    license = paste("Open Government Licence - Canada;",
      "Contains information licensed under the Open Government Licence \u2013 Canada.",
      "https://open.canada.ca/en/open-government-licence-canada"),
    crs = config$crs, repaired = TRUE, repair_provenance = fresh_policy()$repair,
    lastools_path = config$lastools_path, executables = config$executables,
    tool_license = config$tool_license, tiles = tiles, seed = config$seed
  )
  plan <- petawawa_preflight(prep_config, full, read = fresh_read_las)
  plan$method <- fresh_policy()$version
  plan$recovered_scripts_md5 <- NULL
  plan$parameters <- fresh_policy()
  plan$fresh <- list(config = config, archive_md5 = petawawa_checksum(archive),
    implementation = petawawa_checksum(c("R/prepare_fresh_data.R", "R/2_data.R",
                                         "R/1_functions.R", "renv.lock")),
    repairs_md5 = petawawa_checksum(paste0(plan$sources$path, ".rds")))
  if (!is.null(stage_identity$reused_repairs)) {
    plan$fresh$reused_repairs <- stage_identity$reused_repairs
  }
  tryCatch({
    manifest <- file.path(output, "preparation.rds")
    if (full && parallel_allowed && file.exists(manifest)) {
      frozen_plan <- readRDS(manifest)
      comparable <- plan
      comparable$fresh$implementation <- frozen_plan$fresh$implementation
      comparable$implementation[1] <- frozen_plan$implementation[1]
      if (!identical(comparable, frozen_plan)) stop("Frozen preparation inputs/policy changed.")
      plan <- frozen_plan
    }
    petawawa_initialize_plan(plan)
    for (density in plan$densities) {
      directory <- file.path(output, "1_org", paste0("tile_1000_density_", density))
      if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE)) {
        stop("Cannot initialize density output directory.")
      }
    }
    prepare_jobs <- lapply(seq_len(nrow(plan$sources)), function(i) list(
      phase = "prepare", plan = plan, row = plan$sources[i, ], execution = execution))
    fresh_map(prepare_jobs, cores, fresh_subdir(execution_dir, "prepare"))
    fresh_verify(folder, tiles, cores = cores)
  }, error = function(error) {
    petawawa_save(list(diagnostic = conditionMessage(error)),
      tempfile("preparation-failure-", tmpdir = folder, fileext = ".rds"))
    stop(error)
  })
  petawawa_save(list(status = "data-qc-passed", policy = fresh_policy(),
    archive_md5 = plan$fresh$archive_md5,
    preparation_md5 = petawawa_checksum(file.path(output, "preparation.rds"))),
    file.path(folder, "ready.rds"))
  cli::cli_alert_success("Data QC passed: {.path {output}}.")
  cli::cli_inform("Benchmark acceptance still requires a new pilot.")
  completed <- TRUE
  invisible(plan)
}

fresh_main <- function(args) {
  if (length(args) && args[1] == "--query-index") return(fresh_query_index_child(args[-1]))
  cores <- 16L
  if (length(args) == 4L && args[3] == "--cores" && grepl("^[0-9]+$", args[4])) {
    cores <- fresh_cores(as.numeric(args[4]))
    args <- args[1:2]
  }
  if (length(args) != 2L || !args[1] %in% c("--download", "--pilot", "--full",
                                          "--verify-pilot", "--verify-full",
                                          "--upgrade-failed-pilot", "--retry-failed-preparation",
                                          "--resume-parallel-full", "--resume-negative-z-full")) {
    stop("Usage: Rscript --vanilla R/prepare_fresh_data.R <stage> /path/to/config.R; see README.")
  }
  if (args[1] %in% c("--verify-pilot", "--verify-full")) {
    return(fresh_verify_stage(dget(args[2]), full = args[1] == "--verify-full", cores = cores))
  }
  config <- fresh_root(dget(args[2]), upgrade_failed_pilot = args[1] == "--upgrade-failed-pilot",
                       resume_negative_z = args[1] == "--resume-negative-z-full")
  if (args[1] == "--resume-negative-z-full" &&
      !file.exists(file.path(config$root, "negative-z-transition.rds"))) {
    stop("Negative-Z resume requires the supported interrupted v3 root.")
  }
  if (args[1] == "--upgrade-failed-pilot") return(invisible(config))
  if (args[1] == "--download") {
    lock <- file.path(config$root, ".preparation-lock")
    if (!dir.create(lock, showWarnings = FALSE)) stop("Preparation is active or interrupted: ", lock)
    on.exit(fresh_release_lock(lock))
    return(invisible(fresh_archive(config, download = TRUE)))
  }
  fresh_prepare(config, full = args[1] %in% c("--full", "--resume-parallel-full", "--resume-negative-z-full"),
                retry_failed_preparation = args[1] == "--retry-failed-preparation",
                cores = cores,
                resume_parallel = args[1] %in% c("--resume-parallel-full", "--resume-negative-z-full"))
}

if (sys.nframe() == 0L) {
  source("renv/activate.R")
  fresh_main(commandArgs(TRUE))
}
