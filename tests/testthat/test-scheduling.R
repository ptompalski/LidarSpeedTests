test_that("balanced schedules preserve counts, seed, RNG state and position balance", {
  f <- benchmark_fixture()
  env <- f$env
  selection <- expand.grid(task_id = c("a", "b"), run_id = c("one", "two"))
  withr::local_seed(42)
  before <- .Random.seed
  for (n in 1:3) {
    tools <- c("lidR", "lasR", "lastools")[seq_len(n)]
    for (repetitions in 1:7) {
      policy <- list(repetitions = repetitions, warmups = 2L, seed = 567L)
      schedule <- env$benchmark_schedule(selection, tools, policy)
      expect_identical(.Random.seed, before)
      expect_identical(schedule, env$benchmark_schedule(selection[4:1, ], rev(tools), policy))
      expect_identical(schedule$schedule_index, seq_len(nrow(schedule)))
      expect_equal(nrow(schedule), 4 * n * (repetitions + 2))
      for (phase in c("warmup", "measurement")) {
        rows <- schedule[schedule$phase == phase, ]
        count <- if (phase == "warmup") 2 else repetitions
        counts <- table(rows$task_id, rows$run_id, rows$tool)
        expect_true(all(counts == count))
        positions <- table(rows$task_id, rows$run_id, rows$tool, rows$tool_position)
        expect_lte(max(positions) - min(positions), 1)
        groups <- split(rows$tool, interaction(rows$task_id, rows$run_id, rows$repetition_id))
        expect_true(all(vapply(groups, function(group) setequal(group, tools), logical(1))))
      }
    }
  }
  one <- env$benchmark_schedule(selection, c("lidR", "lasR", "lastools"),
                                 list(seed = 1L, warmups = 0L))
  two <- env$benchmark_schedule(selection, c("lidR", "lasR", "lastools"),
                                 list(seed = 2L, warmups = 0L))
  expect_false(identical(one, two))
  expect_true(all(one$phase == "measurement"))
  alternate <- withr::with_seed(42, env$benchmark_schedule(
    selection, c("lidR", "lasR", "lastools"), list(seed = 1L, warmups = 0L)
  ), .rng_kind = "L'Ecuyer-CMRG")
  expect_identical(one, alternate)
})

test_that("scheduling rejects invalid counts, cold-cache claims and nested threading", {
  f <- benchmark_fixture()
  env <- f$env
  for (name in c("repetitions", "warmups", "seed", "native_threads")) {
    for (value in list(NA, Inf, -1, 1.5, numeric(), "3", .Machine$integer.max + 1)) {
      policy <- stats::setNames(list(value), name)
      expect_error(env$benchmark_scheduling_config(policy), "must be one integer")
    }
  }
  expect_error(env$benchmark_scheduling_config(list(repetitions = 0)), "integer")
  expect_error(env$benchmark_scheduling_config(list(native_threads = 2)), "must be 1")
  expect_error(env$benchmark_scheduling_config(list(cache_policy = "cold")), "cold cache")
  expect_error(env$benchmark_scheduling_config(list(cooldown_seconds = -1)), "nonnegative")
  expect_error(env$benchmark_scheduling_config(list(cooldown_seconds = Inf)), "nonnegative")
  expect_error(env$benchmark_scheduling_config(list(unknown = 1)), "named list")
  expect_error(env$benchmark_scheduling_config(list(seed = NULL)), "integer")
  selection <- data.frame(task_id = "a", run_id = "b")
  expect_error(env$benchmark_schedule(rbind(selection, selection), "lidR", NULL), "unique")
  expect_error(env$benchmark_schedule(selection, c("lidR", "lidR"), NULL), "tools")
  for (blas in c("/fixture/openblas/libblas.so", "/fixture/mkl/libblas.so",
                 "/fixture/flexiblas/libblas.so", "/fixture/unknown.so", "")) {
    env$extSoftVersion <- function() c(BLAS = blas)
    expect_error(env$benchmark_validate_thread_runtime(), "Cannot verify single-threaded BLAS")
  }
})

test_that("schedule dispatch is serial, phase-specific and exactly follows the frozen rows", {
  f <- benchmark_fixture()
  env <- f$env
  schedule <- env$benchmark_schedule(
    data.frame(task_id = c("a", "b"), run_id = c("one", "two")),
    c("lidR", "lasR", "lastools"), list(repetitions = 3L, warmups = 2L)
  )
  env$benchmark_campaign_check <- function(...) invisible(NULL)
  seen <- list()
  active <- FALSE
  env$benchmark_run <- function(task_id, run_id, repetition_id, phase, schedule_index,
                                run_lidR, run_lasR, run_lastools,
                                force_rerun_lidR, force_rerun_lasR,
                                force_rerun_lastools, ...) {
    expect_false(active)
    active <<- TRUE
    on.exit(active <<- FALSE)
    flags <- c(lidR = run_lidR, lasR = run_lasR, lastools = run_lastools)
    expect_equal(sum(flags), 1)
    expect_true(all(c(force_rerun_lidR, force_rerun_lasR, force_rerun_lastools) ==
                      (phase == "warmup")))
    seen[[length(seen) + 1L]] <<- data.frame(
      task_id, run_id, tool = names(flags)[flags], phase, repetition_id, schedule_index
    )
  }
  campaign <- list(manifest = list(identity = list(schedule = schedule)))
  env$benchmark_execute_schedule(campaign, NULL, NULL, NULL, NULL)
  actual <- do.call(rbind, seen)
  expect_identical(actual, schedule[names(actual)])
})

test_that("warmups have immutable records, cannot satisfy measurements and repeat on resume", {
  f <- campaign_fixture()
  env <- f$env
  f$config$scheduling <- env$benchmark_scheduling_config(
    list(repetitions = 2L, warmups = 1L, cooldown_seconds = 0)
  )
  campaign <- fixture_campaign(env, f$tasks, f$runs, f$config)
  schedule <- campaign$manifest$identity$schedule
  expect_identical(schedule$phase, c("warmup", "measurement", "measurement"))
  env$benchmark_execute_schedule(campaign, f$tasks, f$runs, f$config, list())
  records <- env$benchmark_read_campaign(f$reports, "fixture")
  expect_equal(nrow(records), 3)
  expect_equal(sum(records$phase == "measurement"), 2)
  expect_setequal(records$schedule_index, 1:3)
  expect_true(all(records$tool_position == 1L))
  checksums <- tools::md5sum(fixture_attempt_files(f$reports))
  env$benchmark_execute_schedule(campaign, f$tasks, f$runs, f$config, list())
  records <- env$benchmark_read_campaign(f$reports, "fixture")
  expect_equal(sum(records$phase == "warmup"), 2)
  expect_equal(sum(records$phase == "measurement"), 2)
  expect_identical(tools::md5sum(names(checksums)), checksums)
  changed <- f$config
  changed$scheduling$seed <- changed$scheduling$seed + 1L
  expect_error(env$benchmark_campaign_check(campaign, f$tasks, f$runs, changed), "config")
  expect_error(env$benchmark_run(
    f$tasks$task_id, f$runs$ID, benchmark_tasks = f$tasks, benchmark_runs = f$runs,
    sysinfo = list(), config = f$config, campaign = campaign,
    schedule_index = 1L, phase = "measurement"
  ), "frozen schedule")
  other <- fixture_campaign(env, f$tasks, f$runs, f$config, "warmup-only")
  attempt <- env$benchmark_attempt_start(other, f$tasks$task_id, f$runs$ID, "lidR", 1L,
                                         phase = "warmup")
  env$benchmark_attempt_finish(attempt, env$benchmark_execute_call("", "lidR"))
  expect_false(env$benchmark_campaign_completed(other, f$tasks$task_id, f$runs$ID, "lidR", 1L))
  expect_true(env$benchmark_campaign_completed(
    other, f$tasks$task_id, f$runs$ID, "lidR", 1L, phase = "warmup"
  ))
  outcomes <- env$compile_execution_outcomes(data.frame(
    success = c(1L, 1L), execution_status = "succeeded",
    validation_status = "passed", phase = c("warmup", "measurement"),
    start_time = Sys.time(), finish_time = Sys.time()
  ))
  expect_identical(outcomes$measurement_eligible, c(FALSE, TRUE))
})

test_that("cooldown is untimed, runs on failures, skips resumed work and releases the lock", {
  f <- campaign_fixture()
  env <- f$env
  f$tasks$task_call_lidR <- "42"
  f$config$scheduling$cooldown_seconds <- 7
  campaign <- fixture_campaign(env, f$tasks, f$runs, f$config)
  env$catalog <- function(...) NULL
  env$benchmark_ALS_info <- function(...) list()
  events <- character()
  env$benchmark_run_tool <- function(...) {
    events <<- c(events, "execute")
    env$benchmark_execute_call('stop("fixture workload failure")', "lidR")
  }
  original_finish <- env$benchmark_attempt_finish
  env$benchmark_attempt_finish <- function(attempt, result) {
    events <<- c(events, "persist")
    original_finish(attempt, result)
  }
  env$Sys.sleep <- function(seconds) {
    expect_equal(seconds, 7)
    expect_true(dir.exists(file.path(f$reports, ".benchmark-execution-lock")))
    events <<- c(events, "cooldown")
  }
  run <- function() env$benchmark_run(
    f$tasks$task_id, f$runs$ID, benchmark_tasks = f$tasks, benchmark_runs = f$runs,
    sysinfo = list(), config = f$config, campaign = campaign
  )
  run()
  expect_identical(events, c("execute", "persist", "cooldown"))
  expect_false(dir.exists(file.path(f$reports, ".benchmark-execution-lock")))
  record <- env$benchmark_read_campaign(f$reports, "fixture")
  expect_equal(record$cooldown_seconds, 7)
  expect_equal(record$workers, 1)
  expect_equal(record$native_threads, 1)
  expect_match(record$timing_boundary, "workload call only")
  expect_identical(record$cache_policy, "uncontrolled-warm")
  lock <- file.path(f$reports, ".benchmark-execution-lock")
  dir.create(lock)
  expect_error(run(), "already locked")
  expect_equal(length(fixture_attempt_files(f$reports)), 1)
  unlink(lock, recursive = TRUE)
  env$Sys.sleep <- function(...) stop("cooldown failure")
  expect_error(run(), "cooldown failure")
  expect_false(dir.exists(lock))
  expect_equal(length(fixture_attempt_files(f$reports)), 2)
})

test_that("pre-scheduling immutable records remain readable without invented schedule slots", {
  f <- campaign_fixture()
  env <- f$env
  campaign <- fixture_campaign(env, f$tasks, f$runs, f$config)
  attempt <- env$benchmark_attempt_start(campaign, f$tasks$task_id, f$runs$ID, "lidR", 1L)
  env$benchmark_attempt_finish(attempt, env$benchmark_execute_call("", "lidR"))
  fields <- c("phase", "schedule_index", "tool_position")
  started <- attempt$record[setdiff(names(attempt$record), fields)]
  saveRDS(started, file.path(attempt$path, "started.rds"))
  path <- file.path(attempt$path, "result.csv")
  historical <- read.csv(path)
  write.csv(historical[setdiff(names(historical), fields)], path, row.names = FALSE)
  saveRDS(unname(tools::md5sum(path)), file.path(attempt$path, "completed.rds"))
  result <- env$benchmark_read_campaign(f$reports, "fixture")
  expect_identical(result$phase, "measurement")
  expect_identical(result$schedule_index, NA_integer_)
  expect_identical(result$tool_position, NA_integer_)
})

test_that("native thread controls apply to actual workers and restore on workload failure", {
  f <- execution_fixture()
  env <- f$env
  old_threads <- lidR::get_lidr_threads()
  old_dt <- data.table::getDTthreads()
  old_plan <- future::plan()
  withr::defer(future::plan(old_plan))
  future::plan(future::sequential)
  variables <- c("OMP_NUM_THREADS", "OMP_THREAD_LIMIT", "OPENBLAS_NUM_THREADS",
                 "MKL_NUM_THREADS", "RCPP_PARALLEL_NUM_THREADS", "OMP_MAX_ACTIVE_LEVELS")
  previous <- Sys.getenv(variables, unset = NA_character_)
  result <- env$benchmark_with_resources("lidR", I(1L), f$config, "threads", function() {
    expect_equal(lidR::get_lidr_threads(), 1L)
    expect_equal(data.table::getDTthreads(), 1L)
    worker <- future::value(future::future(list(
      pid = Sys.getpid(), lidR = lidR::get_lidr_threads(),
      dt = data.table::getDTthreads(), environment = Sys.getenv(variables)
    )))
    expect_false(identical(worker$pid, Sys.getpid()))
    expect_equal(worker$lidR, 1L)
    expect_equal(worker$dt, 1L)
    expect_true(all(worker$environment == "1"))
    stop("intentional thread fixture failure")
  })
  expect_identical(result$execution_status, "failed")
  expect_identical(Sys.getenv(variables, unset = NA_character_), previous)
  expect_equal(lidR::get_lidr_threads(), old_threads)
  expect_equal(data.table::getDTthreads(), old_dt)
  expect_s3_class(future::plan(), "sequential")
  result <- env$benchmark_with_resources("lidR", 2L, f$config, "two-workers", function() {
    expect_equal(future::nbrOfWorkers(), 2L)
    barrier <- file.path(f$test_dir, "worker-barrier")
    dir.create(barrier)
    jobs <- lapply(1:2, function(i) future::future({
      writeLines("ready", file.path(barrier, paste0(i, ".ready")))
      deadline <- Sys.time() + 20
      while (length(list.files(barrier)) < 2L) {
        if (Sys.time() > deadline) stop("Two-worker startup barrier timed out")
        Sys.sleep(0.02)
      }
      list(pid = Sys.getpid(), threads = lidR::get_lidr_threads(),
           data_threads = data.table::getDTthreads())
    }))
    workers <- lapply(jobs, future::value)
    expect_equal(length(unique(vapply(workers, `[[`, integer(1), "pid"))), 2L)
    expect_true(all(vapply(workers, `[[`, integer(1), "threads") == 1L))
    expect_true(all(vapply(workers, `[[`, integer(1), "data_threads") == 1L))
    env$benchmark_execute_call("42", "lidR")
  })
  expect_identical(result$execution_status, "succeeded")
  expect_s3_class(future::plan(), "sequential")
  env$unset_parallel_strategy <- lasR::unset_parallel_strategy
  env$set_parallel_strategy <- lasR::set_parallel_strategy
  env$concurrent_files <- lasR::concurrent_files
  result <- env$benchmark_with_resources("lasR", 2L, f$config, "lasr-threads", function() {
    expect_equal(as.integer(lasR::get_parallel_strategy()), 2L)
    expect_identical(attr(lasR::get_parallel_strategy(), "strategy"), "concurrent-files")
    expect_identical(Sys.getenv("OMP_MAX_ACTIVE_LEVELS"), "1")
    expect_identical(Sys.getenv("OMP_THREAD_LIMIT"), "2")
    env$benchmark_execute_call("42", "lasR")
  })
  expect_identical(result$execution_status, "succeeded")
  expect_null(lasR::get_parallel_strategy())
  expect_identical(Sys.getenv(variables, unset = NA_character_), previous)
  native <- env$benchmark_with_resources("lastools", 2L, f$config, "native-threads", function() {
    process <- env$benchmark_process(f$rscript, c(
      "--vanilla", "-e",
      "cat(Sys.getenv('OPENBLAS_NUM_THREADS'), Sys.getenv('OMP_MAX_ACTIVE_LEVELS'), Sys.getenv('OMP_THREAD_LIMIT'))"
    ))
    expect_identical(process$stdout, "1 1 2")
    env$benchmark_execute_call("42", "lidR")
  })
  expect_identical(native$execution_status, "succeeded")
  env$benchmark_validate_thread_runtime <- function() stop("unverified threaded BLAS")
  blocked <- env$benchmark_with_resources("lidR", 2L, f$config, "blocked", function() {
    fail("Unverified thread runtime must not execute the workload")
  })
  expect_match(blocked$diagnostic, "unverified threaded BLAS")
})
