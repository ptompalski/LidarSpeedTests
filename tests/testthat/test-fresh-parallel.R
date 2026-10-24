fresh_parallel_fixture <- function(envir = parent.frame()) {
  withr::local_dir(repo, .local_envir = envir)
  directory <- withr::local_tempdir(.local_envir = envir)
  env <- new.env(parent = globalenv())
  sys.source("R/prepare_fresh_data.R", env)
  list(env = env, directory = directory)
}

test_that("preparation defaults to eight bounded workers with explicit thread limits and stable seeds", {
  f <- fresh_parallel_fixture()
  e <- f$env
  expect_identical(e$fresh_cores(), 8L)
  for (bad in list(0, -1, 1.5, NA_real_, Inf, "8")) {
    expect_error(e$fresh_cores(bad), "positive integer")
  }
  callback <- function(job) {
    withr::local_seed(job$id)
    if (!is.null(job$barrier)) {
      writeLines(as.character(Sys.getpid()), file.path(job$barrier, paste0(job$id, ".pid")))
      deadline <- Sys.time() + 10
      while (length(list.files(job$barrier, pattern = "\\.pid$")) < 2) {
        if (Sys.time() > deadline) stop("Worker-start barrier timed out.")
        Sys.sleep(.05)
      }
    }
    started <- as.numeric(Sys.time())
    Sys.sleep(.2)
    list(random = runif(5), pid = Sys.getpid(), started = started,
         ended = as.numeric(Sys.time()), dt = data.table::getDTthreads(),
         lidr = lidR::get_lidr_threads(), omp = Sys.getenv("OMP_NUM_THREADS"))
  }
  previous_dt <- data.table::getDTthreads()
  previous_lidr <- lidR::get_lidr_threads()
  previous_omp <- Sys.getenv("OMP_NUM_THREADS", unset = NA_character_)
  set.seed(42)
  seed <- .Random.seed
  jobs <- lapply(1:2, function(id) list(id = id))
  serial <- e$fresh_map(jobs, 1L, f$directory, callback)
  barrier <- file.path(f$directory, "barrier")
  dir.create(barrier)
  for (i in 1:2) jobs[[i]]$barrier <- barrier
  parallel <- e$fresh_map(jobs, 2L, f$directory, callback)
  expect_equal(lapply(parallel, `[[`, "random"), lapply(serial, `[[`, "random"))
  expect_equal(length(unique(vapply(parallel, `[[`, integer(1), "pid"))), 2L)
  expect_true(max(vapply(parallel, `[[`, numeric(1), "started")) <
                min(vapply(parallel, `[[`, numeric(1), "ended")))
  expect_true(all(vapply(parallel, `[[`, integer(1), "dt") == 1L))
  expect_true(all(vapply(parallel, `[[`, integer(1), "lidr") == 1L))
  expect_identical(vapply(parallel, `[[`, character(1), "omp"), c("1", "1"))
  expect_identical(data.table::getDTthreads(), previous_dt)
  expect_identical(lidR::get_lidr_threads(), previous_lidr)
  expect_identical(Sys.getenv("OMP_NUM_THREADS", unset = NA_character_), previous_omp)
  expect_identical(.Random.seed, seed)
  expect_true(all(file.exists(file.path(f$directory, paste0("worker-", 1:2, ".stdout")))))
})

test_that("a worker failure stops siblings and their child processes without starting queued jobs", {
  f <- fresh_parallel_fixture()
  jobs <- lapply(1:3, function(id) list(id = id, root = f$directory))
  previous_threads <- data.table::getDTthreads()
  expect_error(f$env$fresh_map(jobs, 2L, f$directory, function(job) {
    writeLines(as.character(Sys.getpid()), file.path(job$root, paste0(job$id, ".pid")))
    if (job$id == 2) {
      child <- processx::process$new(file.path(R.home("bin"), "Rscript"),
        c("--vanilla", "-e", "Sys.sleep(10)"), cleanup = TRUE)
      writeLines(as.character(child$get_pid()), file.path(job$root, "child.pid"))
    }
    deadline <- Sys.time() + 10
    while (!file.exists(file.path(job$root, "child.pid"))) {
      if (Sys.time() > deadline) stop("Sibling-start barrier timed out.")
      Sys.sleep(.05)
    }
    if (job$id == 1) stop("intentional parallel failure")
    Sys.sleep(5)
    TRUE
  }), "intentional parallel failure")
  expect_false(file.exists(file.path(f$directory, "3.pid")))
  pids <- as.integer(unlist(lapply(list.files(f$directory, "\\.pid$", full.names = TRUE), readLines)))
  expect_true(length(pids) >= 3L)
  expect_false(any(pids %in% ps::ps_pids()))
  expect_identical(data.table::getDTthreads(), previous_threads)
})

test_that("the explicit parallel transition only accepts known interrupted implementations", {
  f <- fresh_parallel_fixture()
  base <- list(config = list(root = "fixture"), policy = f$env$fresh_policy(),
    archive_md5 = "archive", tools_md5 = "tools", versions = "versions", runtime = "runtime",
    implementation = c("new-main", "new-preparation", "functions", "lock", "tiles"))
  previous <- base
  previous$implementation[1:2] <- c("00be03b000101fc9301c8dfef921b2bb",
                                    "99a43ae69c2696ffafe37c7891b03b1b")
  expect_true(f$env$fresh_parallel_compatible(previous, base))
  parallel <- previous
  parallel$implementation[1:2] <- c("066c8ee2c0f9d7fe0a440d3686160d53",
                                   "aa12df90c7d03eaebe284d1f4ced285c")
  expect_true(f$env$fresh_parallel_compatible(parallel, base))
  for (field in c("config", "policy", "archive_md5", "tools_md5", "versions", "runtime")) {
    bad <- previous
    bad[[field]] <- "changed"
    expect_false(f$env$fresh_parallel_compatible(bad, base))
  }
  previous$implementation[1] <- "unknown-source"
  expect_false(f$env$fresh_parallel_compatible(previous, base))
})

test_that("interrupted-file recovery archives partials but preserves completed artifacts", {
  f <- fresh_parallel_fixture()
  folder <- file.path(f$directory, "full")
  dir.create(folder)
  repaired <- f$env$fresh_subdir(folder, "repaired")
  prepared <- f$env$fresh_subdir(folder, "prepared")
  scratch <- f$env$fresh_subdir(f$directory, "scratch")
  execution <- f$env$fresh_subdir(folder, "execution-fixture")
  partial <- file.path(repaired, "298000_5093000.laz")
  completed <- file.path(repaired, "298000_5094000.laz")
  writeLines("partial", partial)
  writeLines("completed", completed)
  writeLines("receipt", paste0(completed, ".rds"))
  owned <- f$env$benchmark_scratch_create(scratch, repaired)
  writeLines("retain interrupted scratch", file.path(owned$path, "output"))
  before <- f$env$petawawa_checksum(c(completed, paste0(completed, ".rds")))
  f$env$fresh_archive_interrupted(folder, c("298000_5093000", "298000_5094000"), execution)
  expect_false(file.exists(partial))
  expect_false(dir.exists(owned$path))
  expect_identical(f$env$petawawa_checksum(c(completed, paste0(completed, ".rds"))), before)
  expect_true(file.exists(file.path(execution, "interrupted", "repaired", basename(partial))))
  expect_true(file.exists(file.path(execution, "interrupted", "scratch", basename(owned$path), "output")))
})

test_that("full-stage transition preserves frozen identities and resumes interrupted preparation", {
  f <- fresh_parallel_fixture()
  e <- f$env
  root <- e$fresh_subdir(f$directory, "root")
  bin <- e$fresh_subdir(f$directory, "bin")
  archive <- file.path(f$directory, "archive.zip")
  writeLines("synthetic archive identity", archive)
  for (tool in c("sort", "index")) writeLines(tool, file.path(bin, tool))
  tiles <- c("298000_5093000", "298000_5094000")
  config <- list(root = root, lastools_path = bin, crs = 2959L, seed = 42L,
                 executables = c(sort = "sort", index = "index"), tool_license = "Fixture")
  e$fresh_root <- function(config) config
  e$fresh_archive <- function(config) archive
  e$fresh_members <- function(archive, tiles) paste0(tiles, ".las")
  e$petawawa_tiles <- function() tiles
  e$benchmark_lastools_executable <- function(path, tool) file.path(path, tool)
  e$petawawa_process <- function(...) list(status = 0L, stdout = "fixture version", stderr = "")
  e$fresh_verify <- function(...) invisible(TRUE)
  e$petawawa_preflight <- function(config, full, read = NULL) {
    paths <- file.path(config$source_root, paste0(tiles, ".laz"))
    list(config = config, densities = c(1, 2),
         implementation = e$petawawa_checksum(c("R/2_data.R", "R/1_functions.R", "renv.lock",
                                                "ALS_tiles.gpkg")),
         sources = data.frame(tile = tiles, path = paths, md5 = e$petawawa_checksum(paths)))
  }
  e$fresh_map <- function(jobs, cores, logdir) {
    for (job in jobs) {
      if (job$phase == "repair") {
        path <- file.path(job$folder, "repaired", paste0(job$tile, ".laz"))
        if (!file.exists(path)) {
          writeLines("fixture source", path)
          saveRDS(list(md5 = e$petawawa_checksum(path)), paste0(path, ".rds"))
        }
      } else {
        for (density in job$plan$densities) {
          path <- file.path(job$plan$config$output_root, "1_org",
                            paste0("tile_1000_density_", density), paste0(job$row$tile, ".laz"))
          if (!file.exists(paste0(path, ".rds"))) {
            writeLines("fixture prepared", path)
            writeLines("fixture index", sub("\\.laz$", ".lax", path))
            saveRDS(list(execution = job$execution), paste0(path, ".rds"))
          }
        }
      }
    }
    invisible(NULL)
  }
  e$fresh_prepare(config, cores = 1L)
  e$fresh_prepare(config, full = TRUE, cores = 1L)
  legacy <- c("00be03b000101fc9301c8dfef921b2bb", "99a43ae69c2696ffafe37c7891b03b1b")
  for (stage in c("pilot", "full")) {
    path <- file.path(root, stage, "stage-identity.rds")
    identity <- readRDS(path)
    identity$implementation[1:2] <- legacy
    saveRDS(identity, path)
  }
  folder <- file.path(root, "full")
  manifest <- file.path(folder, "prepared", "preparation.rds")
  plan <- readRDS(manifest)
  plan$fresh$implementation[1:2] <- legacy
  plan$implementation[1] <- legacy[2]
  saveRDS(plan, manifest)
  unlink(file.path(folder, "ready.rds"))
  partial <- file.path(folder, "prepared", "1_org", "tile_1000_density_2",
                       paste0(tiles[2], ".laz"))
  unlink(paste0(partial, ".rds"))
  preserved <- c(manifest, file.path(folder, "stage-identity.rds"),
    file.path(folder, "repaired", paste0(tiles, ".laz.rds")),
    file.path(folder, "prepared", "1_org", "tile_1000_density_1",
              paste0(tiles[1], c(".laz", ".lax", ".laz.rds"))))
  before <- e$petawawa_checksum(preserved)
  expect_error(e$fresh_prepare(config, full = TRUE), "differ from the verified pilot")
  expect_no_error(e$fresh_prepare(config, full = TRUE, cores = 2L, resume_parallel = TRUE))
  expect_identical(e$petawawa_checksum(preserved), before)
  receipt <- readRDS(file.path(folder, "parallel-transition.rds"))
  expect_identical(receipt$previous, readRDS(file.path(folder, "stage-identity.rds")))
  execution <- readRDS(paste0(partial, ".rds"))$execution
  expect_identical(execution$workers, 2L)
  expect_true(file.exists(file.path(execution$directory, "interrupted", "density_2", basename(partial))))
  expect_identical(readRDS(file.path(execution$directory, "finished.rds"))$status, "completed")
  expect_no_error(e$fresh_prepare(config, full = TRUE, cores = 1L))
  expect_error(e$fresh_prepare(config, full = TRUE, resume_parallel = TRUE), "interrupted full stage")
  changed <- plan
  changed$implementation[2] <- "changed-functions"
  saveRDS(changed, manifest)
  expect_error(e$fresh_prepare(config, full = TRUE), "Frozen preparation inputs/policy changed")
  saveRDS(plan, manifest)
  receipt$previous$runtime <- "tampered"
  saveRDS(receipt, file.path(folder, "parallel-transition.rds"))
  expect_error(e$fresh_prepare(config, full = TRUE), "transition receipt differs")
  expect_false(dir.exists(file.path(root, ".preparation-lock")))
})
