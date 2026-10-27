full_runner_fixture <- function(envir = parent.frame()) {
  f <- benchmark_fixture(envir)
  withr::local_dir(repo, .local_envir = envir)
  e <- f$env
  for (file in c("R/6_pilot.R", "R/2_data.R", "R/3_runBenchmarks.R")) sys.source(file, e)
  stage <- file.path(f$test_dir, "prepared-data", "full")
  dir.create(file.path(stage, "prepared", "1_org"), recursive = TRUE)
  tiles <- e$petawawa_tiles()
  plan <- list(method = "full-runner-fixture", densities = c(1, 2, 5, 10, 20, 50),
    sources = data.frame(tile = tiles, md5 = paste0("source-", seq_along(tiles))),
    parameters = list(policy = "fixture"),
    fresh = list(archive_md5 = "fixture-archive", config = list(
      crs = 2959L, vertical_epsg = 5713L, metadata_confirmation = "Synthetic fixture",
      lastools_path = R.home("bin"))))
  for (dataset in e$benchmark_full_datasets(plan)) {
    directory <- file.path(stage, "prepared", "1_org", dataset)
    dir.create(directory)
    for (i in seq_along(tiles)) {
      paths <- file.path(directory, paste0(tiles[i], c(".laz", ".lax")))
      for (path in paths) writeLines("synthetic bytes, not a point cloud", path)
      saveRDS(list(source_md5 = plan$sources$md5[i],
        index_validation = list(status = "passed"), md5 = unname(tools::md5sum(paths))),
        paste0(paths[1], ".rds"))
    }
  }
  manifest <- file.path(stage, "prepared", "preparation.rds")
  saveRDS(plan, manifest)
  saveRDS(list(status = "data-qc-passed", policy = plan$parameters,
    archive_md5 = plan$fresh$archive_md5, preparation_md5 = unname(tools::md5sum(manifest))),
    file.path(stage, "ready.rds"))
  workspace <- file.path(f$test_dir, "full benchmark")
  config <- e$benchmark_full_init(stage, workspace)
  config$tools <- "lidR"
  dput(config, file.path(workspace, "benchmark.R"))
  withr::local_envvar(c(LIDAR_BENCHMARK_CONFIG = file.path(workspace, "benchmark.R")),
                     .local_envir = envir)
  list(env = e, stage = stage, workspace = workspace, config = config, plan = plan)
}

test_that("full initialization and preflight select all prepared densities, cores and tasks", {
  f <- full_runner_fixture()
  files <- list.files(f$stage, recursive = TRUE, full.names = TRUE)
  before <- tools::md5sum(files)
  full <- f$env$benchmark_full_setup()
  expect_identical(full$config$cores, c(1, 2, 4, 8, 16, 32))
  expect_identical(full$config$datasets_org, f$env$benchmark_full_datasets(f$plan))
  expect_equal(nrow(full$runs), 36L)
  expect_equal(nrow(full$selection), 324L)
  expect_setequal(full$selection$task_id, full$tasks$task_id)
  expect_equal(length(unique(full$selection$task_id)), 9L)
  expect_equal(nrow(full$config$full_preparation$records), 602L)
  expect_identical(full$config$input_metadata$vertical_epsg, 5713L)
  expect_false(dir.exists(file.path(f$workspace, "reports", "campaigns")))
  expect_false(dir.exists(file.path(f$workspace, "pilot-inputs")))
  expect_identical(tools::md5sum(files), before)
  for (task in c("pixel_metrics_1a", "simple_metrics_1a")) {
    selected <- f$env$benchmark_full_setup(task)
    expect_equal(nrow(selected$selection), 36L)
    expect_identical(unique(selected$selection$task_id), "pixel_metrics_1a")
    expect_identical(selected$tasks$task_id, "pixel_metrics_1a")
    schedule <- f$env$benchmark_schedule(selected$selection,
      c("lidR", "lasR", "lastools"), selected$config$scheduling)
    expect_equal(nrow(schedule), 432L)
  }
  expect_error(f$env$benchmark_full_setup("unknown-task"), "Unknown task.*Available tasks")
  expect_error(f$env$benchmark_full_setup(c("one", "two")), "one task ID")
  expect_error(f$env$benchmark_full_init(f$stage, f$workspace), "already exists")
  expect_error(f$env$benchmark_full_init(f$stage, file.path(f$stage, "benchmark")), "disjoint")
})

test_that("full preflight refuses changed bytes, incomplete inventory and invalid readiness", {
  f <- full_runner_fixture()
  e <- f$env
  cloud <- file.path(f$config$input_roots, f$config$datasets_org[1],
                      paste0(f$plan$sources$tile[1], ".laz"))
  writeLines("changed bytes", cloud)
  expect_error(e$benchmark_full_inputs(f$config), "checksum/completion receipt mismatch")
  unlink(cloud)
  expect_error(e$benchmark_full_inputs(f$config), "inventory differs")
  dir.create(file.path(dirname(f$stage), ".preparation-lock"))
  expect_error(e$benchmark_full_inputs(f$config), "active or interrupted")
  unlink(file.path(dirname(f$stage), ".preparation-lock"), recursive = TRUE)
  unlink(file.path(f$stage, "ready.rds"))
  expect_error(e$benchmark_full_inputs(f$config), "no successful readiness")
})

test_that("full initialization refuses a pilot subset and accepts an explicit native install", {
  f <- full_runner_fixture()
  e <- f$env
  install <- file.path(dirname(f$workspace), "new-native-bin")
  dir.create(install)
  config <- e$benchmark_full_init(f$stage, paste0(f$workspace, "-new"), install)
  expect_identical(config$lastools_path, normalizePath(install, winslash = "/"))
  plan <- f$plan
  plan$sources <- plan$sources[1:2, ]
  e$benchmark_prepared_pilot_plan <- function(...) plan
  expect_error(e$benchmark_full_plan(f$stage), "all 100 approved tiles")
  plan <- f$plan
  plan$densities <- 1
  expect_error(e$benchmark_full_plan(f$stage), "all six prepared densities")
})

test_that("full CLI preflight does not execute and run uses explicit workspace and campaign", {
  f <- benchmark_fixture()
  e <- f$env
  withr::local_dir(repo)
  sys.source("R/3_runBenchmarks.R", e)
  e$source <- function(...) invisible(NULL)
  workspace <- f$test_dir
  selected_task <- NULL
  e$benchmark_full_setup <- function(task = NULL) {
    selected_task <<- task
    expect_identical(Sys.getenv("LIDAR_BENCHMARK_CONFIG"), file.path(workspace, "benchmark.R"))
    list(config = list(tools = "lidR", scheduling = list()), tasks = "tasks",
      runs = data.frame(ID = "run"),
      selection = data.frame(task_id = "task", run_id = "run"))
  }
  id <- NULL
  e$benchmark_campaign <- function(campaign_id, ...) {
    id <<- campaign_id
    list(manifest = list(identity = list(environment = list(native = list()))))
  }
  e$system_info <- function() list()
  executed <- FALSE
  e$benchmark_execute_schedule <- function(...) executed <<- TRUE
  withr::local_envvar(c(LIDAR_BENCHMARK_CONFIG = "previous", LIDAR_CAMPAIGN_ID = "previous"))
  e$benchmark_full_main(c("--preflight", workspace))
  expect_false(executed)
  expect_null(id)
  expect_identical(Sys.getenv("LIDAR_BENCHMARK_CONFIG"), "previous")
  e$benchmark_full_main(c("--run", workspace, "full-001"))
  expect_true(executed)
  expect_identical(id, "full-001")
  expect_identical(Sys.getenv("LIDAR_CAMPAIGN_ID"), "previous")
  expect_null(selected_task)
  e$benchmark_full_main(c("--run", workspace, "simple-001", "--task", "simple_metrics_1a"))
  expect_identical(selected_task, "simple_metrics_1a")
  expect_identical(id, "simple-001")
  executed <- FALSE
  e$benchmark_full_main(c("--task", "pixel_metrics_1a", "--preflight", workspace))
  expect_identical(selected_task, "pixel_metrics_1a")
  expect_false(executed)
  expect_identical(Sys.getenv("LIDAR_BENCHMARK_CONFIG"), "previous")
  for (args in list(c("--run", workspace, "id", "--task"),
                    c("--task", "--run", workspace, "id"),
                    c("--task", "one", "--task", "two"),
                    c("--task", ""))) {
    expect_error(e$benchmark_full_main(args), "exactly one task ID")
  }
  expect_error(e$benchmark_full_main(c("--init", "stage", workspace, "--task", "one")),
               "not --init")
  expect_error(e$benchmark_full_main(c("--run", workspace)), "Use Rscript")
})
