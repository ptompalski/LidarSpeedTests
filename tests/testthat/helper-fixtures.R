library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(forcats)
library(stringr)
library(glue)
library(magrittr)

repo <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)

benchmark_fixture <- function(envir = parent.frame()) {
  test_dir <- tempfile("lidar fixture ")
  dir.create(test_dir)
  test_dir <- normalizePath(test_dir, winslash = "/")
  withr::defer(unlink(test_dir, recursive = TRUE), envir = envir)
  env <- new.env(parent = envir)
  sys.source(file.path(repo, "R/1_functions.R"), env)
  list(env = env, test_dir = test_dir)
}

chat2_fixture <- function(envir = parent.frame()) {
  fixture <- benchmark_fixture(envir)
  env <- fixture$env
  test_dir <- fixture$test_dir
  input <- file.path(test_dir, "input data")
  scratch <- file.path(test_dir, "scratch area")
  reports <- file.path(test_dir, "reports area")
  bin <- file.path(test_dir, "native tools")
  for (path in c(input, scratch, reports, bin)) dir.create(path)
  files <- file.path(input, c("tile one.laz", "tile;literal.laz"))
  expect_true(all(file.create(files)))
  writeLines("input sentinel", files[1L])
  sentinel <- tools::md5sum(files)
  config <- env$benchmark_validate_config(list(
    input_roots = c(SSD = input), scratch_roots = c(SSD = scratch),
    reports_path = reports, cores = c(1L, 2L), tools = c("lidR", "lasR", "lastools"),
    lastools_path = bin, workstation_id = "fixture", resource_monitoring = FALSE
  ))
  config_file <- file.path(test_dir, "machine config.R")
  dput(config, config_file)
  withr::local_envvar(c(LIDAR_BENCHMARK_CONFIG = config_file), .local_envir = envir)

  # Rscript stands in for native tools; no LAStools or point clouds are needed.
  rscript <- file.path(R.home("bin"),
                       if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  suffix <- if (.Platform$OS.type == "windows") ".exe" else ""
  for (name in c("probe", "lascanopy64", "lasheight64", "blast2dem64", "lasgrid64")) {
    expect_true(file.copy(rscript, file.path(bin, paste0(name, suffix))))
  }
  probe <- file.path(bin, paste0("probe", suffix))
  script <- file.path(test_dir, "native process fixture.R")
  writeLines(c(
    "args <- commandArgs(TRUE)",
    "saveRDS(args[-c(1L, 2L)], args[2L])",
    "output <- match('-odir', args)",
    "if (!is.na(output)) writeLines('artifact', file.path(args[output + 1L], 'result.tif'))",
    "cat('native stdout\\n')",
    "cat('native stderr\\n', file = stderr())",
    "quit(status = as.integer(args[1L]))"
  ), script)
  observed <- file.path(test_dir, "observed arguments.rds")
  output <- file.path(test_dir, "output with spaces")
  dir.create(output)
  call_env <- list2env(list(f = files, dir_out = output, cores = 2L, lastools_path = bin))
  task_call <- list(executable = "probe", args = c(
    "--vanilla", script, "7", observed, "-i", "{inputs}", "-odir", "{output}",
    "-cores", "{cores}"
  ))
  missing <- task_call
  missing$executable <- "absent-tool"
  list(
    env = env, test_dir = test_dir, input = input, scratch = scratch,
    reports = reports, bin = bin, files = files, sentinel = sentinel,
    config = config, config_file = config_file, rscript = rscript,
    probe = probe, script = script, observed = observed, call_env = call_env,
    task_call = task_call, missing = missing
  )
}

with_chat2_fixture <- function(code) {
  # Use a function frame so withr cleanup runs immediately on exit.
  envir <- environment()
  list2env(chat2_fixture(envir), envir = envir)
  eval(substitute(code), envir)
}
