fresh_verification_fixture <- function(envir = parent.frame()) {
  withr::local_dir(repo, .local_envir = envir)
  directory <- withr::local_tempdir(.local_envir = envir)
  env <- new.env(parent = globalenv())
  sys.source("R/prepare_fresh_data.R", env)
  config <- dget("fresh-data.example.R")
  config$root <- file.path(directory, "root")
  config$protected_roots <- file.path(directory, "historical")
  dir.create(config$protected_roots)
  config <- env$fresh_root(config)
  folder <- env$fresh_subdir(config$root, "pilot")
  repaired <- env$fresh_subdir(folder, "repaired")
  prepared <- env$fresh_subdir(folder, "prepared")
  tiles <- c("298000_5093000", "298000_5094000")
  densities <- c(1, 2)
  paths <- file.path(repaired, paste0(tiles, ".laz"))
  for (i in seq_along(tiles)) {
    origin <- as.numeric(strsplit(tiles[i], "_", fixed = TRUE)[[1]])
    xy <- expand.grid(X = origin[1] + seq(0, 3.5, .25),
                      Y = origin[2] + seq(0, 3.5, .25))
    las <- lidR::LAS(data.frame(
      xy, Z = 100, gpstime = seq_len(nrow(xy)) / 10,
      ReturnNumber = 1L, NumberOfReturns = 1L,
      Classification = 2L, PointSourceID = 1L, Synthetic_flag = FALSE
    ))
    sf::st_crs(las) <- config$crs
    lidR::writeLAS(las, paths[i])
    source <- env$fresh_read_las(paths[i])
    source_md5 <- env$petawawa_checksum(paths[i])
    env$petawawa_save(list(policy = env$fresh_policy(), md5 = source_md5,
      qc = env$fresh_qc(source, tiles[i], config$crs)), paste0(paths[i], ".rds"))
    for (density in densities) {
      output <- file.path(prepared, "1_org", paste0("tile_1000_density_", density))
      if (!dir.exists(output)) dir.create(output, recursive = TRUE)
      path <- file.path(output, paste0(tiles[i], ".laz"))
      lidR::writeLAS(env$fresh_variant(source, density, i), path, index = TRUE)
      env$petawawa_save(list(
        md5 = env$petawawa_checksum(c(path, sub("\\.laz$", ".lax", path))),
        source_md5 = source_md5, index_validation = list(status = "passed")
      ), paste0(path, ".rds"))
    }
  }
  plan <- list(method = env$fresh_policy()$version,
    config = list(crs = config$crs), parameters = env$fresh_policy(),
    sources = data.frame(tile = tiles, path = paths, md5 = env$petawawa_checksum(paths)),
    densities = densities, fresh = list(config = config, archive_md5 = "frozen-archive",
      repairs_md5 = env$petawawa_checksum(paste0(paths, ".rds"))))
  env$petawawa_save(plan, file.path(prepared, "preparation.rds"))
  env$petawawa_save(list(config = config, policy = env$fresh_policy(),
    archive_md5 = "frozen-archive", implementation = "historical-implementation"),
    file.path(folder, "stage-identity.rds"))
  list(env = env, config = config, folder = folder, tiles = tiles, plan = plan)
}

test_that("parallel verification finishes existing data without regenerating or rewriting provenance", {
  f <- fresh_verification_fixture()
  e <- f$env
  frozen <- list.files(f$config$root, recursive = TRUE, full.names = TRUE)
  before <- e$petawawa_checksum(frozen)
  expect_false(file.exists(file.path(f$folder, "ready.rds")))
  expect_true(e$fresh_verify_stage(f$config))
  expect_identical(e$petawawa_checksum(frozen), before)
  ready <- file.path(f$folder, "ready.rds")
  expect_identical(readRDS(ready)$status, "data-qc-passed")
  logs <- list.dirs(f$folder, recursive = FALSE, full.names = TRUE)
  logs <- logs[startsWith(basename(logs), "verification-")]
  expect_length(logs, 1L)
  started <- readRDS(file.path(logs, "started.rds"))
  expect_identical(started$requested_cores, 16L)
  expect_identical(started$workers, 2L)
  expect_identical(started$threads_per_worker, 1L)
  records <- lapply(file.path(logs, paste0(f$tiles, ".rds")), readRDS)
  expect_setequal(vapply(records, `[[`, character(1), "tile"), f$tiles)
  expect_true(all(vapply(records, `[[`, character(1), "status") == "verified"))
  expect_length(unique(vapply(records, `[[`, integer(1), "pid")), 2L)
  expect_false(Sys.getpid() %in% vapply(records, `[[`, integer(1), "pid"))
  expect_identical(readRDS(file.path(logs, "finished.rds"))$status, "completed")
  ready_md5 <- e$petawawa_checksum(ready)
  expect_true(e$fresh_verify_stage(f$config, cores = 1L))
  expect_identical(e$petawawa_checksum(ready), ready_md5)
  expect_identical(e$petawawa_checksum(frozen), before)
  expect_false(dir.exists(file.path(f$config$root, ".preparation-lock")))
})

test_that("verification refuses active locks and mismatched frozen configuration or policy", {
  f <- fresh_verification_fixture()
  e <- f$env
  lock <- file.path(f$config$root, ".preparation-lock")
  dir.create(lock)
  expect_error(e$fresh_verify_stage(f$config), "active or interrupted")
  expect_true(dir.exists(lock))
  e$fresh_release_lock(lock)
  stage_path <- file.path(f$folder, "stage-identity.rds")
  stage <- readRDS(stage_path)
  stage$config$seed <- stage$config$seed + 1L
  saveRDS(stage, stage_path)
  expect_error(e$fresh_verify_stage(f$config), "matching frozen configuration")
  expect_false(file.exists(file.path(f$folder, "ready.rds")))
  expect_false(dir.exists(lock))
  stage$config <- f$config
  saveRDS(stage, stage_path)
  manifest <- file.path(f$folder, "prepared", "preparation.rds")
  plan <- f$plan
  plan$parameters$version <- "unknown-policy"
  saveRDS(plan, manifest)
  expect_error(e$fresh_verify_stage(f$config), "current repair policy")
  missing <- f$config
  missing$root <- file.path(dirname(f$config$root), "missing")
  expect_error(e$fresh_root(missing, verification_only = TRUE), "existing preparation root")
  expect_false(dir.exists(missing$root))
})

test_that("parallel verification fails explicitly on corrupt data without publishing readiness", {
  f <- fresh_verification_fixture()
  e <- f$env
  output <- file.path(f$folder, "prepared", "1_org", "tile_1000_density_1",
                      paste0(f$tiles[1], ".laz"))
  writeLines("corrupt output", output)
  expect_error(e$fresh_verify_stage(f$config, cores = 2L), "checksum mismatch")
  expect_false(file.exists(file.path(f$folder, "ready.rds")))
  expect_false(dir.exists(file.path(f$config$root, ".preparation-lock")))
  logs <- list.files(f$folder, pattern = "^verification-", full.names = TRUE)
  expect_identical(readRDS(file.path(logs, "finished.rds"))$status, "failed_or_interrupted")
  expect_false(file.exists(file.path(logs, paste0(f$tiles[1], ".rds"))))
})

test_that("verification checks index validation, density counts and frozen identity changes", {
  f <- fresh_verification_fixture()
  e <- f$env
  receipt <- file.path(f$folder, "prepared", "1_org", "tile_1000_density_1",
                       paste0(f$tiles[1], ".laz.rds"))
  original <- readRDS(receipt)
  bad <- original
  bad$index_validation$status <- "unverified"
  saveRDS(bad, receipt)
  expect_error(e$fresh_verify_stage(f$config, cores = 1L), "Missing index-query validation")
  bad <- original
  bad$source_md5 <- "different-source"
  saveRDS(bad, receipt)
  expect_error(e$fresh_verify_stage(f$config, cores = 1L), "density count/source identity")
  saveRDS(original, receipt)
  e$fresh_verify <- function(...) {
    stage <- file.path(f$folder, "stage-identity.rds")
    record <- readRDS(stage)
    record$implementation <- "changed-during-verification"
    saveRDS(record, stage)
    invisible(TRUE)
  }
  expect_error(e$fresh_verify_stage(f$config), "identity changed during verification")
  expect_false(file.exists(file.path(f$folder, "ready.rds")))
})

test_that("verification CLI forwards sixteen workers by default and honors explicit budgets", {
  f <- fresh_verification_fixture()
  config <- file.path(dirname(f$config$root), "config.R")
  dput(f$config, config)
  called <- NULL
  f$env$fresh_verify_stage <- function(...) called <<- list(...)
  f$env$fresh_main(c("--verify-full", config))
  expect_identical(called$full, TRUE)
  expect_identical(called$cores, 16L)
  f$env$fresh_main(c("--verify-pilot", config, "--cores", "3"))
  expect_identical(called$full, FALSE)
  expect_identical(called$cores, 3L)
})
