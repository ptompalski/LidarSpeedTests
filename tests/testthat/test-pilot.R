prepared_pilot_fixture <- function(envir = parent.frame()) {
  f <- benchmark_fixture(envir)
  withr::local_dir(repo, .local_envir = envir)
  e <- f$env
  sys.source("R/6_pilot.R", e)
  stage <- file.path(f$test_dir, "fresh", "full")
  output <- file.path(f$test_dir, "pilot")
  dir.create(stage, recursive = TRUE)
  dir.create(output)
  source <- file.path(stage, "prepared", "1_org", "tile_1000_density_1")
  dir.create(source, recursive = TRUE)
  tiles <- c("298000_5093000", "298000_5094000")
  paths <- file.path(source, paste0(tiles, ".laz"))
  for (i in seq_along(tiles)) {
    xy <- as.numeric(strsplit(tiles[i], "_", fixed = TRUE)[[1]])
    las <- lidR::LAS(data.frame(X = xy[1] + c(0, 1, 0),
                               Y = xy[2] + c(0, 0, 1), Z = c(100, 101, 102)))
    sf::st_crs(las) <- 2959
    lidR::writeLAS(las, paths[i], index = TRUE)
    saveRDS(list(
      md5 = unname(tools::md5sum(c(paths[i], sub("\\.laz$", ".lax", paths[i])))),
      source_md5 = paste0("source-", i), index_validation = list(status = "passed")
    ), paste0(paths[i], ".rds"))
  }
  manifest <- file.path(stage, "prepared", "preparation.rds")
  plan <- list(method = "fixture-preparation", densities = 1,
    sources = data.frame(tile = tiles, md5 = paste0("source-", seq_along(tiles))),
    parameters = list(policy = "fixture"), fresh = list(archive_md5 = "fixture-archive",
      config = list(crs = 2959L, vertical_epsg = 5713L,
        metadata_confirmation = "Synthetic fixture metadata", lastools_path = R.home("bin"))))
  saveRDS(plan, manifest)
  saveRDS(list(status = "data-qc-passed", policy = plan$parameters,
    archive_md5 = plan$fresh$archive_md5, preparation_md5 = unname(tools::md5sum(manifest))),
    file.path(stage, "ready.rds"))
  list(env = e, stage = stage, output = output, tiles = tiles, paths = paths, plan = plan)
}

test_that("prepared pilot staging copies the verified pair with stable preparation provenance", {
  f <- prepared_pilot_fixture()
  e <- f$env
  frozen <- list.files(f$stage, recursive = TRUE, full.names = TRUE)
  before <- e$benchmark_checksums(frozen)
  staged <- e$benchmark_stage_prepared_pilot_inputs(f$stage, f$output)
  expect_identical(e$benchmark_stage_prepared_pilot_inputs(f$stage, f$output), staged)
  expect_identical(e$benchmark_checksums(frozen), before)
  expect_setequal(staged$provenance$tiles, f$tiles)
  expect_equal(nrow(staged$provenance$staged), 4L)
  expect_equal(nrow(staged$provenance$preparation$records), 4L)
  expect_identical(staged$provenance$preparation$method, f$plan$method)
  expect_identical(readRDS(file.path(staged$root, "provenance.rds")), staged$provenance)
  expect_setequal(list.files(file.path(staged$root, "tile_1000_density_1")),
    c(paste0(f$tiles, ".laz"), paste0(f$tiles, ".lax")))
  writeLines("changed index", sub("\\.laz$", ".lax", f$paths[1]))
  expect_error(e$benchmark_stage_prepared_pilot_inputs(f$stage, f$output),
               "source bytes no longer match")
})

test_that("prepared pilot staging refuses missing readiness, changed manifests and active preparation", {
  f <- prepared_pilot_fixture()
  e <- f$env
  lock <- file.path(dirname(f$stage), ".preparation-lock")
  dir.create(lock)
  expect_error(e$benchmark_stage_prepared_pilot_inputs(f$stage, f$output), "active or interrupted")
  unlink(lock, recursive = TRUE)
  expect_error(e$benchmark_stage_prepared_pilot_inputs(f$stage, f$stage), "disjoint")
  ready_path <- file.path(f$stage, "ready.rds")
  ready <- readRDS(ready_path)
  unlink(ready_path)
  expect_error(e$benchmark_stage_prepared_pilot_inputs(f$stage, f$output), "no successful readiness")
  ready$preparation_md5 <- "changed"
  saveRDS(ready, ready_path)
  expect_error(e$benchmark_stage_prepared_pilot_inputs(f$stage, f$output), "frozen preparation manifest")
})

test_that("prepared pilot staging refuses incomplete tile receipts", {
  f <- prepared_pilot_fixture()
  receipt_path <- paste0(f$paths[1], ".rds")
  receipt <- readRDS(receipt_path)
  receipt$index_validation$status <- "unverified"
  saveRDS(receipt, receipt_path)
  expect_error(f$env$benchmark_stage_prepared_pilot_inputs(f$stage, f$output),
               "invalid completion receipt")
  receipt$index_validation$status <- "passed"
  receipt$source_md5 <- "different-source"
  saveRDS(receipt, receipt_path)
  expect_error(f$env$benchmark_stage_prepared_pilot_inputs(f$stage, f$output),
               "invalid completion receipt")
})

test_that("pilot setup selects fresh inputs with only the approved density and core matrix", {
  f <- prepared_pilot_fixture()
  e <- f$env
  for (name in c("reports", "scratch")) dir.create(file.path(f$output, name))
  config <- dget("benchmark.example.R")
  config$prepared_stage_root <- f$stage
  config$input_roots <- c(SSD = file.path(f$output, "pilot-inputs"))
  config$scratch_roots <- c(SSD = file.path(f$output, "scratch"))
  config$reports_path <- file.path(f$output, "reports")
  config$cores <- c(1L, 2L)
  config$tools <- "lidR"
  config_file <- file.path(f$output, "config.R")
  dput(config, config_file)
  withr::local_envvar(c(LIDAR_BENCHMARK_CONFIG = config_file))
  pilot <- e$benchmark_pilot_setup()
  expect_setequal(pilot$runs$cores, c(1L, 2L))
  expect_equal(nrow(pilot$runs), 2L)
  expect_equal(nrow(pilot$selection), 18L)
  expect_setequal(basename(pilot$runs$dir_in_org), "tile_1000_density_1")
  expect_identical(pilot$config$pilot_provenance$preparation$stage_root, f$stage)
  expect_false(pilot$config$diagnostic_lasR)
  expect_false(pilot$config$accept_known_surface_discrepancies)
  config$historical_input_root <- f$stage
  dput(config, config_file)
  expect_error(e$benchmark_pilot_setup(), "exactly one pilot source")
  config$historical_input_root <- NULL
  config$cores <- c(1L, 16L)
  dput(config, config_file)
  expect_error(e$benchmark_pilot_setup(), "core matrix must be 1 and 2")
})

test_that("pilot initialization creates configuration without staging inputs or running a campaign", {
  f <- prepared_pilot_fixture()
  e <- f$env
  workspace <- file.path(dirname(f$output), "new pilot workspace")
  config <- e$benchmark_pilot_init(f$stage, workspace)
  expect_identical(dget(file.path(workspace, "benchmark.R")), config)
  expect_identical(config$prepared_stage_root, f$stage)
  expect_identical(config$cores, c(1L, 2L))
  expect_identical(config$input_metadata$horizontal_epsg, 2959L)
  expect_identical(config$input_metadata$vertical_epsg, 5713L)
  expect_identical(config$input_metadata$confirmation, "Synthetic fixture metadata")
  expect_false(config$accept_known_surface_discrepancies)
  expect_false(config$accept_known_extent_discrepancies)
  expect_false(config$accept_known_lastools_cutoff_warning)
  expect_false(config$diagnostic_lasR)
  expect_true(dir.exists(config$reports_path))
  expect_true(dir.exists(unname(config$scratch_roots)))
  expect_false(dir.exists(unname(config$input_roots)))
  expect_length(list.files(config$reports_path), 0L)
  before <- e$benchmark_checksums(file.path(workspace, "benchmark.R"))
  expect_error(e$benchmark_pilot_init(f$stage, workspace), "already exists")
  expect_identical(e$benchmark_checksums(file.path(workspace, "benchmark.R")), before)
  expect_error(e$benchmark_pilot_init(f$stage, file.path(f$stage, "pilot")), "disjoint")
  newer <- file.path(dirname(f$output), "newer-lastools-bin")
  dir.create(newer)
  other <- e$benchmark_pilot_init(f$stage, file.path(dirname(f$output), "override"), newer)
  expect_identical(other$lastools_path, normalizePath(newer, winslash = "/"))
  expect_error(e$benchmark_pilot_init(f$stage, file.path(dirname(f$output), "missing-bin"),
                                      file.path(newer, "absent")), "LAStools must be an existing")
  expect_false(dir.exists(file.path(dirname(f$output), "missing-bin")))
})

test_that("workspace CLI preflight and run route correctly without leaking environment overrides", {
  f <- prepared_pilot_fixture()
  e <- f$env
  config <- list(tools = "lidR")
  e$benchmark_pilot_setup <- function() {
    expect_identical(Sys.getenv("LIDAR_BENCHMARK_CONFIG"), file.path(f$output, "benchmark.R"))
    list(config = config, selection = "selection", tasks = "tasks", runs = "runs")
  }
  called <- NULL
  e$benchmark_campaign <- function(id, ...) {
    called <<- id
    list(manifest = list(identity = list(environment = list(native = list()))))
  }
  e$system_info <- function() list()
  executed <- FALSE
  e$benchmark_execute_schedule <- function(...) executed <<- TRUE
  withr::local_envvar(c(LIDAR_BENCHMARK_CONFIG = "previous-config", LIDAR_CAMPAIGN_ID = "previous-id"))
  e$benchmark_pilot_main(c("--preflight", f$output))
  expect_false(executed)
  expect_null(called)
  expect_identical(Sys.getenv("LIDAR_BENCHMARK_CONFIG"), "previous-config")
  e$benchmark_pilot_main(c("--run", f$output, "new-pilot"))
  expect_true(executed)
  expect_identical(called, "new-pilot")
  expect_identical(Sys.getenv("LIDAR_BENCHMARK_CONFIG"), "previous-config")
  expect_identical(Sys.getenv("LIDAR_CAMPAIGN_ID"), "previous-id")
  for (args in list(character(), c("--init", f$stage), c("--run", f$output),
                    c("--preflight", f$output, "extra"))) {
    expect_error(e$benchmark_pilot_main(args), "Use Rscript")
  }
})
