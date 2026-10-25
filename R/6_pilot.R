benchmark_stage_pilot_inputs <- function(source_root, output_root,
                                         manifest_path = "input-manifests/petawawa-recovered-pilot.csv",
                                         source_preparation = NULL) {
  source_root <- benchmark_directory(source_root, "Pilot source")
  output_root <- benchmark_directory(output_root, "Pilot output parent", TRUE)
  if (benchmark_paths_overlap(source_root, output_root)) {
    stop("Pilot source and output parent must be disjoint.")
  }
  manifest <- read.csv(manifest_path, colClasses = c(las_version = "character"))
  selected <- manifest[manifest$density == 1L, , drop = FALSE]
  tiles <- c("298000_5093000", "298000_5094000")
  if (nrow(selected) != 2L || !setequal(selected$tile, tiles) ||
      anyDuplicated(selected$tile) ||
      !identical(selected$path, paste0("tile_1000_density_1/", selected$tile, ".laz"))) {
    stop("Pilot requires exactly the approved neighboring pair at density 1.")
  }
  sources <- file.path(source_root, c(selected$path, sub("\\.laz$", ".lax", selected$path)))
  expected <- c(selected$md5, selected$index_md5)
  source_bytes <- benchmark_checksums(sources)
  if (!identical(unname(tools::md5sum(sources)), expected)) {
    stop("Pilot source bytes no longer match the frozen input manifest.")
  }
  root <- file.path(output_root, "pilot-inputs")
  if (!dir.exists(root) && !dir.create(root)) stop("Cannot create pilot input root.")
  root <- benchmark_directory(root, "Pilot input root", TRUE)
  if (!benchmark_path_within(root, output_root)) stop("Pilot input root redirects outside output parent.")
  dataset <- file.path(root, "tile_1000_density_1")
  if (!dir.exists(dataset) && !dir.create(dataset)) stop("Cannot create pilot dataset.")
  dataset <- benchmark_directory(dataset, "Pilot dataset", TRUE)
  if (!benchmark_path_within(dataset, root)) stop("Pilot dataset redirects outside input root.")
  destinations <- file.path(dataset, basename(sources))
  extras <- setdiff(list.files(dataset, all.files = TRUE, no.. = TRUE), basename(destinations))
  if (length(extras)) stop("Unexpected files in pilot input subset: ", paste(extras, collapse = ", "))
  for (i in seq_along(sources)) {
    if (!file.exists(destinations[i]) && !file.copy(sources[i], destinations[i], overwrite = FALSE)) {
      stop("Cannot stage approved input: ", sources[i])
    }
  }
  if (!identical(unname(tools::md5sum(destinations)), expected) ||
      !identical(benchmark_checksums(sources), source_bytes)) {
    stop("Staged/source pilot checksums differ; no files were overwritten.")
  }
  provenance <- list(
    source_root = source_root, source_manifest = benchmark_checksums(manifest_path),
    density = 1L, tiles = tiles, sources = source_bytes,
    staged = benchmark_checksums(destinations), method = "unchanged byte copies; no metadata rewrite"
  )
  if (!is.null(source_preparation)) provenance$preparation <- source_preparation
  receipt <- file.path(root, "provenance.rds")
  if (file.exists(receipt)) {
    if (!identical(readRDS(receipt), provenance)) stop("Pilot staging provenance changed.")
  } else saveRDS(provenance, receipt)
  list(root = root, provenance = provenance)
}

benchmark_prepared_pilot_plan <- function(stage_root) {
  if (dir.exists(file.path(dirname(stage_root), ".preparation-lock"))) {
    stop("Preparation is active or interrupted; release its lock before staging a pilot.")
  }
  ready_path <- file.path(stage_root, "ready.rds")
  manifest_path <- file.path(stage_root, "prepared", "preparation.rds")
  if (!file.exists(ready_path)) stop("Prepared stage has no successful readiness receipt.")
  ready <- readRDS(ready_path)
  plan <- readRDS(manifest_path)
  if (!identical(ready$status, "data-qc-passed") ||
      !identical(ready$preparation_md5, unname(tools::md5sum(manifest_path))) ||
      !identical(ready$policy, plan$parameters) ||
      !identical(ready$archive_md5, plan$fresh$archive_md5)) {
    stop("Prepared stage readiness does not match its frozen preparation manifest.")
  }
  plan
}

benchmark_stage_prepared_pilot_inputs <- function(stage_root, output_root) {
  stage_root <- benchmark_directory(stage_root, "Prepared stage")
  output_root <- benchmark_directory(output_root, "Pilot output parent", TRUE)
  if (benchmark_paths_overlap(stage_root, output_root)) {
    stop("Prepared stage and pilot output parent must be disjoint.")
  }
  plan <- benchmark_prepared_pilot_plan(stage_root)
  ready_path <- file.path(stage_root, "ready.rds")
  manifest_path <- file.path(stage_root, "prepared", "preparation.rds")
  tiles <- c("298000_5093000", "298000_5094000")
  if (!1 %in% plan$densities || anyDuplicated(plan$sources$tile) ||
      !all(tiles %in% plan$sources$tile)) {
    stop("Prepared stage must include the approved neighboring density-1 pair.")
  }
  relative <- paste0("tile_1000_density_1/", tiles, ".laz")
  source_root <- file.path(stage_root, "prepared", "1_org")
  paths <- file.path(source_root, relative)
  receipt_paths <- paste0(paths, ".rds")
  receipts <- lapply(receipt_paths, readRDS)
  for (i in seq_along(tiles)) {
    record <- receipts[[i]]
    if (!identical(record$index_validation$status, "passed") ||
        !identical(record$source_md5, plan$sources$md5[match(tiles[i], plan$sources$tile)]) ||
        !is.character(record$md5) || length(record$md5) != 2L || anyNA(record$md5)) {
      stop("Prepared pilot tile has an invalid completion receipt: ", tiles[i])
    }
    if (!identical(unname(tools::md5sum(c(paths[i], sub("\\.laz$", ".lax", paths[i])))),
                   record$md5)) {
      stop("Pilot source bytes no longer match the frozen completion receipt: ", tiles[i])
    }
  }
  preparation <- list(stage_root = stage_root, method = plan$method,
    records = benchmark_checksums(c(ready_path, manifest_path, receipt_paths)))
  manifest <- data.frame(
    density = 1L, tile = tiles, path = relative,
    md5 = vapply(receipts, function(record) record$md5[1], character(1)),
    index_md5 = vapply(receipts, function(record) record$md5[2], character(1)),
    las_version = vapply(paths, function(path) {
      header <- lidR::readLASheader(path)@PHB
      paste(header[["Version Major"]], header[["Version Minor"]], sep = ".")
    }, character(1))
  )
  rownames(manifest) <- NULL
  csv <- file.path(output_root, "pilot-source-manifest.csv")
  if (isTRUE(fs::is_link(csv))) stop("Refusing linked pilot source manifest: ", csv)
  if (file.exists(csv)) {
    if (!identical(read.csv(csv, colClasses = c(las_version = "character")), manifest)) {
      stop("Pilot source manifest changed; use a new pilot workspace.")
    }
  } else write.csv(manifest, csv, row.names = FALSE)
  staged <- benchmark_stage_pilot_inputs(source_root, output_root, csv,
                                          source_preparation = preparation)
  if (!identical(benchmark_checksums(preparation$records$path), preparation$records)) {
    stop("Prepared stage provenance changed during pilot staging.")
  }
  staged
}

benchmark_pilot_init <- function(stage_root, workspace) {
  stage_root <- benchmark_directory(stage_root, "Prepared stage")
  plan <- benchmark_prepared_pilot_plan(stage_root)
  if (!is.character(workspace) || length(workspace) != 1L ||
      is.na(workspace) || !nzchar(trimws(workspace))) stop("Supply a new pilot workspace.")
  parent <- benchmark_directory(dirname(workspace), "Pilot workspace parent", TRUE)
  workspace <- file.path(parent, basename(workspace))
  if (benchmark_paths_overlap(stage_root, workspace)) {
    stop("Prepared stage and pilot workspace must be disjoint.")
  }
  if (file.exists(workspace) || isTRUE(fs::is_link(workspace))) {
    stop("Pilot workspace already exists; use --preflight with that workspace or choose a new path.")
  }
  confirmed <- plan$fresh$config
  config <- dget("benchmark.example.R")
  config$prepared_stage_root <- stage_root
  config$input_roots <- c(SSD = file.path(workspace, "pilot-inputs"))
  config$scratch_roots <- c(SSD = file.path(workspace, "scratch"))
  config$reports_path <- file.path(workspace, "reports")
  config$cores <- c(1L, 2L)
  config$lastools_path <- benchmark_directory(confirmed$lastools_path, "LAStools")
  config$input_metadata <- benchmark_input_metadata(list(
    horizontal_epsg = confirmed$crs, vertical_epsg = confirmed$vertical_epsg,
    xyz_units = "metre", confirmation = confirmed$metadata_confirmation
  ))
  if (!dir.create(workspace)) stop("Cannot create pilot workspace: ", workspace)
  for (name in c("scratch", "reports")) {
    if (!dir.create(file.path(workspace, name))) stop("Cannot create pilot directory: ", name)
  }
  path <- file.path(workspace, "benchmark.R")
  dput(config, path)
  cli::cli_alert_success("Pilot configuration created: {.path {path}}.")
  cli::cli_inform("Next: Rscript --vanilla R/6_pilot.R --preflight {shQuote(workspace)}")
  invisible(config)
}

benchmark_pilot_setup <- function() {
  config <- benchmark_load_config()
  if (!setequal(config$cores, c(1L, 2L))) stop("Pilot core matrix must be 1 and 2.")
  prepared <- !is.null(config$prepared_stage_root)
  historical <- !is.null(config$historical_input_root)
  if (prepared == historical) {
    stop("Select exactly one pilot source: prepared_stage_root or historical_input_root.")
  }
  staged <- if (prepared) {
    benchmark_stage_prepared_pilot_inputs(config$prepared_stage_root, dirname(config$reports_path))
  } else {
    benchmark_stage_pilot_inputs(config$historical_input_root, dirname(config$reports_path))
  }
  if (!identical(unname(config$input_roots), staged$root)) {
    stop("Pilot input_roots must select only the staged pilot-inputs root.")
  }
  config$pilot_provenance <- staged$provenance
  config <- benchmark_validate_config(config)
  setup <- new.env(parent = globalenv())
  setup$source <- function(file) sys.source(file, setup)
  sys.source("R/0_setup.R", setup)
  runs <- benchmark_make_runs(list(
    datasets_org = c(ALS_1000_1 = "tile_1000_density_1"),
    datasets_norm = c(ALS_1000_1 = "unused")
  ), config)
  selection <- expand.grid(task_id = setup$benchmark_tasks$task_id, run_id = runs$ID,
                            stringsAsFactors = FALSE)
  benchmark_validate_selection(selection, setup$benchmark_tasks, runs, config)
  list(config = config, tasks = setup$benchmark_tasks, runs = runs, selection = selection)
}

benchmark_pilot_main <- function(args) {
  if (!length(args) || !args[1] %in% c("--init", "--preflight", "--run") ||
      (args[1] == "--init" && length(args) != 3L) ||
      (args[1] == "--preflight" && !length(args) %in% c(1L, 2L)) ||
      (args[1] == "--run" && !length(args) %in% c(1L, 3L))) {
    stop("Use Rscript --vanilla R/6_pilot.R --init <prepared-stage> <new-workspace>, ",
         "--preflight <workspace>, or --run <workspace> <campaign-id>; see README.md.")
  }
  source("renv/activate.R")
  source("R/1_functions.R")
  if (args[1] == "--init") return(benchmark_pilot_init(args[2], args[3]))
  if (length(args) > 1L) {
    workspace <- benchmark_directory(args[2], "Pilot workspace")
    variables <- c(LIDAR_BENCHMARK_CONFIG = file.path(workspace, "benchmark.R"))
    if (args[1] == "--run") variables <- c(variables, LIDAR_CAMPAIGN_ID = args[3])
    withr::local_envvar(variables)
  }
  if (args[1] == "--run") {
    id <- Sys.getenv("LIDAR_CAMPAIGN_ID")
    if (!nzchar(id)) stop("Set one explicit LIDAR_CAMPAIGN_ID or supply a campaign ID with --run.")
    benchmark_file_component(id, "campaign_id")
  }
  pilot <- benchmark_pilot_setup()
  cat("Pilot: 2 neighboring density-1 tiles; cores 1/2; tools ",
      paste(pilot$config$tools, collapse = ", "), ".\n", sep = "")
  if (args[1] == "--preflight") return(invisible(pilot))
  campaign <- benchmark_campaign(id, pilot$selection, pilot$tasks, pilot$runs, pilot$config)
  info <- system_info()
  native <- campaign$manifest$identity$environment$native
  if (length(native)) info$lastools_version <- paste(vapply(names(native), function(name) {
    paste0(name, ": ", trimws(native[[name]]$version))
  }, character(1)), collapse = "\n")
  benchmark_execute_schedule(campaign, pilot$tasks, pilot$runs, pilot$config, info)
  invisible(campaign)
}

if (sys.nframe() == 0L) benchmark_pilot_main(commandArgs(TRUE))
