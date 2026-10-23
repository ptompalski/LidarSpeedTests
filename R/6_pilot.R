benchmark_stage_pilot_inputs <- function(source_root, output_root,
                                         manifest_path = "input-manifests/petawawa-recovered-pilot.csv") {
  source_root <- benchmark_directory(source_root, "Historical source")
  output_root <- benchmark_directory(output_root, "Pilot output parent", TRUE)
  if (benchmark_paths_overlap(source_root, output_root)) {
    stop("Historical source and pilot output parent must be disjoint.")
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
    stop("Historical pilot bytes no longer match the frozen input manifest.")
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
  receipt <- file.path(root, "provenance.rds")
  if (file.exists(receipt)) {
    if (!identical(readRDS(receipt), provenance)) stop("Pilot staging provenance changed.")
  } else saveRDS(provenance, receipt)
  list(root = root, provenance = provenance)
}

benchmark_pilot_setup <- function() {
  config <- benchmark_load_config()
  staged <- benchmark_stage_pilot_inputs(config$historical_input_root, dirname(config$reports_path))
  if (!identical(unname(config$input_roots), staged$root)) {
    stop("Pilot input_roots must select only the staged pilot-inputs root.")
  }
  config$pilot_provenance <- staged$provenance
  config <- benchmark_validate_config(config)
  if (!setequal(config$cores, c(1L, 2L))) stop("Pilot core matrix must be 1 and 2.")
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
  if (length(args) != 1L || !args %in% c("--preflight", "--run")) {
    stop("Use Rscript --vanilla R/6_pilot.R --preflight or --run; see README.md.")
  }
  source("renv/activate.R")
  source("R/1_functions.R")
  pilot <- benchmark_pilot_setup()
  cat("Pilot: 2 neighboring density-1 tiles; cores 1/2; tools ",
      paste(pilot$config$tools, collapse = ", "), ".\n", sep = "")
  if (args == "--preflight") return(invisible(pilot))
  id <- Sys.getenv("LIDAR_CAMPAIGN_ID")
  if (!nzchar(id)) stop("Set one explicit LIDAR_CAMPAIGN_ID.")
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
