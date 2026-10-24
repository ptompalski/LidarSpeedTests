fresh_negative_z_fixture <- function(envir = parent.frame()) {
  withr::local_dir(repo, .local_envir = envir)
  directory <- withr::local_tempdir(.local_envir = envir)
  env <- new.env(parent = globalenv())
  sys.source("R/prepare_fresh_data.R", env)
  config <- dget("fresh-data.example.R")
  config$root <- file.path(directory, "root")
  config$protected_roots <- file.path(directory, "historical")
  dir.create(config$protected_roots)
  config <- env$fresh_root(config)
  identity <- readRDS(file.path(config$root, "fresh-root.rds"))
  previous <- identity
  previous$policy$version <- "fresh-petawawa-v3"
  previous$policy$negative_z <- NULL
  saveRDS(previous, file.path(config$root, "fresh-root.rds"))
  implementation <- env$petawawa_checksum(c("R/prepare_fresh_data.R", "R/2_data.R",
                                           "R/1_functions.R", "renv.lock", "ALS_tiles.gpkg"))
  implementation[1:2] <- c("3d79583687c92ed0cd74afab1defbbcb",
                            "b0a94be880119b1df463a011fc1b76a2")
  stage <- list(config = config, policy = previous$policy, archive_md5 = "archive",
                tools_md5 = "tools", versions = "versions", runtime = "runtime",
                implementation = implementation)
  for (name in c("pilot", "full")) {
    folder <- env$fresh_subdir(config$root, name)
    env$fresh_subdir(folder, "repaired")
    env$fresh_subdir(folder, "prepared")
    saveRDS(stage, file.path(folder, "stage-identity.rds"))
  }
  saveRDS(list(frozen = TRUE), file.path(config$root, "pilot", "prepared", "preparation.rds"))
  saveRDS(list(status = "data-qc-passed"), file.path(config$root, "pilot", "ready.rds"))
  list(env = env, config = config, previous = previous, current = identity, stage = stage)
}

test_that("explicit v3 negative-Z transition preserves frozen identities and completed bytes", {
  f <- fresh_negative_z_fixture()
  e <- f$env
  root <- f$config$root
  completed <- file.path(root, "full", "repaired", "completed.laz")
  writeLines("completed repaired bytes", completed)
  saveRDS(list(md5 = e$petawawa_checksum(completed)), paste0(completed, ".rds"))
  paths <- c(e$fresh_negative_z_paths(root), completed, paste0(completed, ".rds"))
  before <- e$petawawa_checksum(paths)
  expect_error(e$fresh_root(f$config), "policy changed")
  expect_identical(e$fresh_root(f$config, resume_negative_z = TRUE), f$config)
  expect_identical(e$petawawa_checksum(paths), before)
  receipt <- readRDS(file.path(root, "negative-z-transition.rds"))
  expect_identical(receipt$previous, f$previous)
  expect_identical(receipt$current, f$current)
  expect_identical(e$fresh_root(f$config), f$config)
  expect_false(dir.exists(file.path(root, ".preparation-lock")))
  current <- f$stage
  current$policy <- f$current$policy
  current$implementation[1] <- "current-negative-z-code"
  expect_true(e$fresh_parallel_compatible(f$stage, current))
  for (field in c("config", "archive_md5", "tools_md5", "versions", "runtime")) {
    bad <- current
    bad[[field]] <- "changed"
    expect_false(e$fresh_parallel_compatible(f$stage, bad))
  }
  for (position in c(2L, 3L, 4L, 5L)) {
    bad <- current
    bad$implementation[position] <- "changed"
    expect_false(e$fresh_parallel_compatible(f$stage, bad))
  }
  receipt$implementation[1] <- "changed"
  saveRDS(receipt, file.path(root, "negative-z-transition.rds"))
  expect_error(e$fresh_root(f$config), "transition differs")
  saveRDS(list(changed = TRUE), file.path(root, "full", "stage-identity.rds"))
  expect_error(e$fresh_root(f$config), "transition differs")
  expect_identical(e$petawawa_checksum(c(completed, paste0(completed, ".rds"))), tail(before, 2))
})

test_that("negative-Z transition refuses unsupported policies, code, prepared full stages and locks", {
  f <- fresh_negative_z_fixture()
  e <- f$env
  root <- f$config$root
  lock <- file.path(root, ".preparation-lock")
  dir.create(lock)
  expect_error(e$fresh_root(f$config, resume_negative_z = TRUE), "active or interrupted")
  e$fresh_release_lock(lock)
  stage_path <- file.path(root, "full", "stage-identity.rds")
  bad <- f$stage
  bad$implementation[1] <- "unknown-code"
  saveRDS(bad, stage_path)
  expect_error(e$fresh_root(f$config, resume_negative_z = TRUE), "unknown stage code")
  saveRDS(f$stage, stage_path)
  partial <- file.path(root, "full", "prepared", "preparation.rds")
  saveRDS(list(frozen = TRUE), partial)
  expect_error(e$fresh_root(f$config, resume_negative_z = TRUE), "without full prepared products")
  unlink(partial)
  ready <- file.path(root, "full", "ready.rds")
  saveRDS(list(status = "data-qc-passed"), ready)
  expect_error(e$fresh_root(f$config, resume_negative_z = TRUE), "without full prepared products")
  unlink(ready)
  bad <- f$previous
  bad$policy$version <- "fresh-petawawa-v2"
  saveRDS(bad, file.path(root, "fresh-root.rds"))
  expect_error(e$fresh_root(f$config, resume_negative_z = TRUE), "unchanged-config v3 to v4")
  expect_false(file.exists(file.path(root, "negative-z-transition.rds")))
  expect_false(dir.exists(lock))
})

test_that("old repairs with negative source removals cannot be assumed equivalent to v4", {
  f <- fresh_negative_z_fixture()
  e <- f$env
  receipt <- list(actions = data.frame(source_Z = c(100, 101)))
  expect_no_error(e$fresh_check_repair_policy(receipt, "fixture"))
  receipt$actions$source_Z[1] <- -.01
  expect_error(e$fresh_check_repair_policy(receipt, "fixture"), "audit includes negative")
  receipt$policy <- e$fresh_policy()
  expect_no_error(e$fresh_check_repair_policy(receipt, "fixture"))
  receipt$policy$version <- "unknown-policy"
  expect_error(e$fresh_check_repair_policy(receipt, "fixture"), "missing/changed policy")
  expect_error(e$fresh_check_repair_policy(list(), "fixture"), "missing/changed policy")
})

test_that("the explicit negative-Z CLI initializes the transition and forwards the worker budget", {
  f <- fresh_negative_z_fixture()
  config_path <- file.path(dirname(f$config$root), "config.R")
  dput(f$config, config_path)
  called <- NULL
  f$env$fresh_prepare <- function(...) called <<- list(...)
  expect_no_error(f$env$fresh_main(c("--resume-negative-z-full", config_path, "--cores", "3")))
  expect_identical(called$full, TRUE)
  expect_identical(called$resume_parallel, TRUE)
  expect_identical(called$cores, 3L)
  expect_true(file.exists(file.path(f$config$root, "negative-z-transition.rds")))
})

test_that("v3 repair-stage recovery completes with unchanged prior repairs and pilot identities", {
  f <- fresh_negative_z_fixture()
  e <- f$env
  root <- f$config$root
  tiles <- c("298000_5093000", "298000_5094000")
  archive <- file.path(root, "fixture.zip")
  writeLines("tiny fixture archive", archive)
  bin <- e$fresh_subdir(root, "bin")
  tools <- stats::setNames(file.path(bin, f$config$executables), names(f$config$executables))
  for (tool in tools) writeLines("fixture tool", tool)
  e$fresh_archive <- function(config) archive
  e$petawawa_tiles <- function() tiles
  e$fresh_members <- function(archive, tiles) paste0(tiles, ".las")
  e$benchmark_lastools_executable <- function(path, tool) file.path(bin, tool)
  e$petawawa_process <- function(...) list(status = 0L, stdout = "fixture version", stderr = "")
  verified <- character()
  e$fresh_verify <- function(folder, tiles) {
    verified <<- c(verified, basename(folder))
    invisible(TRUE)
  }
  for (name in c("pilot", "full")) {
    stage <- f$stage
    stage$archive_md5 <- e$petawawa_checksum(archive)
    stage$tools_md5 <- e$petawawa_checksum(tools)
    stage$versions <- lapply(tools, e$petawawa_process)
    stage$members <- paste0(tiles, ".las")
    stage$runtime <- list(R = as.character(getRversion()),
      lidR = as.character(utils::packageVersion("lidR")),
      rlas = as.character(utils::packageVersion("rlas")), sf = sf::sf_extSoftVersion())
    saveRDS(stage, file.path(root, name, "stage-identity.rds"))
  }
  repaired <- file.path(root, "full", "repaired", paste0(tiles[1], ".laz"))
  writeLines("previous completed repair", repaired)
  saveRDS(list(md5 = e$petawawa_checksum(repaired)), paste0(repaired, ".rds"))
  paths <- c(e$fresh_negative_z_paths(root), repaired, paste0(repaired, ".rds"))
  before <- e$petawawa_checksum(paths)
  e$petawawa_preflight <- function(config, full, read) {
    sources <- file.path(config$source_root, paste0(tiles, ".laz"))
    list(config = config, densities = 1,
      implementation = e$petawawa_checksum(c("R/2_data.R", "R/1_functions.R", "renv.lock",
                                             "ALS_tiles.gpkg")),
      sources = data.frame(
      tile = tiles, path = sources, md5 = e$petawawa_checksum(sources)))
  }
  e$fresh_map <- function(jobs, cores, logdir) {
    for (job in jobs) {
      if (job$phase == "repair") {
        path <- file.path(job$folder, "repaired", paste0(job$tile, ".laz"))
        if (!file.exists(path)) {
          writeLines("new repair with negative points removed", path)
          saveRDS(list(md5 = e$petawawa_checksum(path)), paste0(path, ".rds"))
        }
      } else {
        path <- file.path(job$plan$config$output_root, "1_org",
                          "tile_1000_density_1", paste0(job$row$tile, ".laz"))
        if (!file.exists(path)) {
          writeLines("prepared fixture", path)
          writeLines("index fixture", sub("\\.laz$", ".lax", path))
          saveRDS(list(execution = job$execution), paste0(path, ".rds"))
        }
      }
    }
    invisible(NULL)
  }
  e$fresh_root(f$config, resume_negative_z = TRUE)
  expect_no_error(e$fresh_prepare(f$config, full = TRUE, resume_parallel = TRUE, cores = 2L))
  expect_identical(e$petawawa_checksum(paths), before)
  expect_identical(verified, c("pilot", "full"))
  transition <- readRDS(file.path(root, "full", "parallel-transition.rds"))
  expect_identical(transition$previous$policy$version, "fresh-petawawa-v3")
  expect_identical(transition$current$policy$version, "fresh-petawawa-v4")
  plan <- readRDS(file.path(root, "full", "prepared", "preparation.rds"))
  expect_identical(plan$method, "fresh-petawawa-v4")
  expect_identical(plan$parameters, e$fresh_policy())
  expect_identical(readRDS(file.path(root, "full", "ready.rds"))$policy, e$fresh_policy())
  expect_no_error(e$fresh_prepare(f$config, full = TRUE, cores = 1L))
  expect_identical(e$petawawa_checksum(paths), before)
})
