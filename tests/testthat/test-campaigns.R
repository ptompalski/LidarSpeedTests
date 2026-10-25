test_that("campaign identity is stable and records the required provenance", {
  f <- campaign_fixture()
  env <- f$env
  campaign <- fixture_campaign(env, f$tasks, f$runs, f$config)
  identity <- campaign$manifest$identity
  again <- env$benchmark_campaign_identity(f$selection, f$tasks, f$runs, f$config, repo)
  expect_identical(identity, again, info = paste(all.equal(identity, again), collapse = "\n"))
  expect_silent(env$benchmark_campaign_check(campaign, f$tasks, f$runs, f$config))
  expect_identical(identity$datasets[[1]]$files$md5, unname(tools::md5sum(sort(f$files))))
  expect_true(nzchar(identity$code$revision))
  expect_type(identity$code$dirty, "logical")
  expect_true(all(nzchar(identity$code$files$md5)))
  expect_identical(identity$lockfile$md5, unname(tools::md5sum(file.path(repo, "renv.lock"))))
  expect_true(length(identity$environment$packages) > 100L)
  expect_true(all(c("R", "R_executable", "OS", "cpu", "RAM_bytes", "storage_mounts", "parallelism",
                    "geospatial", "libraries") %in% names(identity$environment)))
  manifest_md5 <- tools::md5sum(file.path(campaign$path, "manifest.rds"))
  resumed <- fixture_campaign(env, f$tasks, f$runs, f$config)
  expect_identical(resumed$manifest, campaign$manifest)
  expect_identical(tools::md5sum(file.path(campaign$path, "manifest.rds")), manifest_md5)
})

test_that("changed data, indexes, configuration and task definitions reject resume", {
  f <- campaign_fixture()
  env <- f$env
  campaign <- fixture_campaign(env, f$tasks, f$runs, f$config)
  writeLines("changed input", f$files[1])
  expect_error(env$benchmark_campaign_check(campaign, f$tasks, f$runs, f$config),
               "changed: datasets")
  expect_error(fixture_campaign(env, f$tasks, f$runs, f$config), "Incompatible campaign")
  writeLines("input sentinel", f$files[1])
  index <- file.path(f$input, "tile one.lax")
  writeLines("new index", index)
  expect_error(env$benchmark_campaign_check(campaign, f$tasks, f$runs, f$config), "datasets")
  unlink(index)
  config <- f$config
  config$cores <- 1L
  expect_error(env$benchmark_campaign_check(campaign, f$tasks, f$runs, config), "config")
  tasks <- f$tasks
  tasks$task_call_lidR <- "42"
  expect_error(env$benchmark_campaign_check(campaign, tasks, f$runs, f$config), "tasks")
  selection <- rbind(f$selection, f$selection)
  expect_error(env$benchmark_campaign("duplicate", selection, f$tasks, f$runs,
                                     f$config, repo), "unique")
  expect_error(env$benchmark_run(
    f$tasks$task_id, f$runs$ID, benchmark_tasks = f$tasks, benchmark_runs = f$runs,
    sysinfo = list(lidR_version = "4.3.3"), config = f$config
  ), "explicit campaign")
})

test_that("code and lockfile byte changes reject a dirty checkout's resume", {
  f <- campaign_fixture()
  env <- f$env
  checkout <- file.path(f$test_dir, "checkout")
  dir.create(checkout)
  dir.create(file.path(checkout, "R"))
  expect_true(all(file.copy(file.path(repo, c("DESCRIPTION", "WORKLOADS.md", "renv.lock")),
                            checkout)))
  code <- file.path(checkout, "R/workload.R")
  writeLines("answer <- 42", code)
  original_process <- env$benchmark_process
  env$benchmark_process <- function(executable, args = character()) {
    # Borrow the actual checkout's git revision, but hash only fixture sources.
    if (identical(executable, "git")) args[2L] <- repo
    original_process(executable, args)
  }
  campaign <- env$benchmark_campaign("code", f$selection, f$tasks, f$runs,
                                     f$config, checkout)
  expect_silent(env$benchmark_campaign_check(campaign, f$tasks, f$runs, f$config))
  writeLines("answer <- 43", code)
  expect_error(env$benchmark_campaign_check(campaign, f$tasks, f$runs, f$config), "code")
  writeLines("answer <- 42", code)
  cat("\n", file = file.path(checkout, "renv.lock"), append = TRUE)
  expect_error(env$benchmark_campaign_check(campaign, f$tasks, f$runs, f$config), "lockfile")
})

test_that("runtime thread environment and native versions/checksums reject resume", {
  f <- campaign_fixture()
  env <- f$env
  campaign <- fixture_campaign(env, f$tasks, f$runs, f$config)
  withr::with_envvar(c(OMP_NUM_THREADS = "999"), {
    expect_error(env$benchmark_campaign_check(campaign, f$tasks, f$runs, f$config),
                 "environment")
  })
  f$config$tools <- "lastools"
  f$tasks$task_call_lastools <- list(list(executable = "probe", args = character()))
  native <- fixture_campaign(env, f$tasks, f$runs, f$config, "native")
  expect_true(nzchar(native$manifest$identity$environment$native$probe$version))
  binary <- file.path(f$bin, if (.Platform$OS.type == "windows") "probe.exe" else "probe")
  con <- file(binary, "ab")
  writeBin(charToRaw("changed bytes"), con)
  close(con)
  expect_error(env$benchmark_campaign_check(native, f$tasks, f$runs, f$config),
               "environment")
  original_process <- env$benchmark_process
  env$benchmark_process <- function(executable, args = character()) {
    if (identical(args, "-version")) {
      return(list(status = 7L, stdout = "", stderr = "identity query failure"))
    }
    original_process(executable, args)
  }
  expect_error(fixture_campaign(env, f$tasks, f$runs, f$config, "query-failure"),
               "identity query failure")
  expect_false(dir.exists(file.path(f$reports, "campaigns/query-failure")))
})

test_that("software version identity excludes variable LAStools licensing notices but not build changes", {
  f <- campaign_fixture()
  env <- f$env
  f$config$tools <- "lastools"
  f$tasks$task_call_lastools <- list(list(executable = "probe", args = character()))
  license <- "non-profit"
  build <- "260914"
  original_process <- env$benchmark_process
  env$benchmark_process <- function(executable, args = character()) {
    if (identical(args, "-version")) {
      notice <- if (license == "unlicensed") "Please clarify licensing terms.\n" else ""
      return(list(status = 0L, stdout = "", stderr = paste0(notice,
        "LAStools probe (by info@rapidlasso.de) version ", build, " (", license, ")\n")))
    }
    original_process(executable, args)
  }
  campaign <- fixture_campaign(env, f$tasks, f$runs, f$config)
  expect_identical(campaign$manifest$identity$environment$native$probe$version,
                   "LAStools probe (by info@rapidlasso.de) version 260914")
  license <- "unlicensed"
  expect_no_error(env$benchmark_campaign_check(campaign, f$tasks, f$runs, f$config))
  build <- "260915"
  expect_error(env$benchmark_campaign_check(campaign, f$tasks, f$runs, f$config), "environment")
})

test_that("resume and repetitions preserve skipped, failed and successful retries", {
  f <- campaign_fixture()
  env <- f$env
  campaign <- fixture_campaign(env, f$tasks, f$runs, f$config)
  run <- function(...) env$benchmark_run(
    f$tasks$task_id, f$runs$ID, benchmark_tasks = f$tasks, benchmark_runs = f$runs,
    sysinfo = list(lidR_version = "4.3.3"), config = f$config, campaign = campaign, ...
  )
  run()
  first <- fixture_attempt_files(f$reports)
  checksum <- tools::md5sum(first)
  run()
  expect_length(fixture_attempt_files(f$reports), 1L)
  run(repetition_id = 2L)
  run(force_rerun_lidR = TRUE)
  expect_length(fixture_attempt_files(f$reports), 3L)
  expect_identical(tools::md5sum(first), checksum)
  result <- env$benchmark_read_campaign(f$reports, "fixture")
  expect_equal(sort(result$repetition_id), c(1, 1, 2))
  expect_equal(length(unique(result$attempt_id)), 3L)
  expect_true(all(result$execution_status == "skipped"))
  expect_error(run(repetition_id = 0), "positive integer")

  failed <- env$benchmark_attempt_start(campaign, f$tasks$task_id, f$runs$ID, "lidR", 3L)
  failure <- env$benchmark_execute_call('stop("retry failure")', "lidR")
  env$benchmark_attempt_finish(failed, failure)
  expect_false(env$benchmark_campaign_completed(
    campaign, f$tasks$task_id, f$runs$ID, "lidR", 3L
  ))
  expect_error(env$benchmark_attempt_finish(failed, failure), "already reserved")
  success <- env$benchmark_attempt_start(campaign, f$tasks$task_id, f$runs$ID, "lidR", 3L)
  env$benchmark_attempt_finish(success, env$benchmark_execute_call("42", "lidR"))
  expect_false(env$benchmark_campaign_completed(
    campaign, f$tasks$task_id, f$runs$ID, "lidR", 3L
  ))
  result <- env$benchmark_read_campaign(f$reports, "fixture")
  expect_equal(sum(result$execution_status == "failed"), 1L)
  expect_equal(sum(result$execution_status == "succeeded"), 1L)
  expect_true("retry failure" %in% result$diagnostic)
  execution <- 0L
  env$benchmark_run_tool <- function(...) {
    execution <<- execution + 1L
    if (execution == 1L) env$benchmark_execute_call('stop("dispatch failure")', "lidR")
    else env$benchmark_execute_call("42", "lidR")
  }
  run(repetition_id = 4L)
  run(repetition_id = 4L)
  run(repetition_id = 4L)
  expect_equal(execution, 3L)
  result <- env$benchmark_read_campaign(f$reports, "fixture")
  expect_equal(sum(result$repetition_id == 4), 3L)
  expect_true("dispatch failure" %in% result$diagnostic)
  expect_error(env$benchmark_attempt_start(campaign, "other", f$runs$ID, "lidR", 1),
               "outside")
})

test_that("compilation cannot mix campaigns, legacy records or incomplete attempts", {
  f <- campaign_fixture()
  env <- f$env
  for (id in c("one", "two")) {
    campaign <- fixture_campaign(env, f$tasks, f$runs, f$config, id)
    attempt <- env$benchmark_attempt_start(
      campaign, f$tasks$task_id, f$runs$ID, "lidR", 1L
    )
    env$benchmark_attempt_finish(attempt, env$benchmark_execute_call("", "lidR"))
  }
  result <- env$benchmark_read_campaign(f$reports, "one")
  expect_equal(nrow(result), 1L)
  expect_identical(result$campaign_id, "one")
  compiler <- new.env(parent = env)
  compiler$reports_path <- f$reports
  compiler$source <- function(...) invisible(NULL)
  withr::local_dir(f$test_dir)
  withr::local_envvar(c(LIDAR_CAMPAIGN_ID = "one", LIDAR_RESULTS_MODE = NA_character_))
  sys.source(file.path(repo, "R/4_compileResults.R"), compiler)
  compiled <- readRDS(file.path(f$reports, "campaigns/one/compiled.rds"))
  expect_identical(compiled$campaign_id, "one")
  expect_identical(compiled$attempt_id, result$attempt_id)
  expect_false(file.exists("r.rds"))
  withr::local_envvar(c(LIDAR_CAMPAIGN_ID = NA_character_))
  expect_error(sys.source(file.path(repo, "R/4_compileResults.R"), compiler), "Set LIDAR")
  withr::local_envvar(c(LIDAR_RESULTS_MODE = "legacy"))
  file.copy(fixture_attempt_files(f$reports)[1], file.path(f$reports, "mixed.csv"))
  expect_error(sys.source(file.path(repo, "R/4_compileResults.R"), compiler),
               "cannot be compiled as legacy")
  original <- fixture_attempt_files(f$reports)[1]
  record <- read.csv(original)
  record$campaign_id <- "another"
  write.csv(record, original, row.names = FALSE)
  expect_error(env$benchmark_read_campaign(f$reports, "one"), "checksum")
  campaign <- fixture_campaign(env, f$tasks, f$runs, f$config, "incomplete")
  env$benchmark_attempt_start(campaign, f$tasks$task_id, f$runs$ID, "lidR", 1L)
  expect_error(env$benchmark_read_campaign(f$reports, "incomplete"), "Incomplete attempt retained")
})

test_that("metadata failures are retained before scratch or workload execution", {
  f <- campaign_fixture()
  env <- f$env
  f$tasks$task_call_lidR <- "42"
  campaign <- fixture_campaign(env, f$tasks, f$runs, f$config)
  env$catalog <- function(...) stop("fixture metadata failure")
  env$benchmark_run_tool <- function(...) stop("Workload must not run")
  for (i in 1:2) {
    env$benchmark_run(
      f$tasks$task_id, f$runs$ID, benchmark_tasks = f$tasks, benchmark_runs = f$runs,
      sysinfo = list(lidR_version = "4.3.3"), config = f$config, campaign = campaign
    )
  }
  records <- env$benchmark_read_campaign(f$reports, "fixture")
  expect_equal(nrow(records), 2L)
  expect_true(all(records$execution_status == "failed"))
  expect_true(all(records$diagnostic == "fixture metadata failure"))
  expect_length(list.files(f$scratch, all.files = TRUE, no.. = TRUE), 0L)
})
