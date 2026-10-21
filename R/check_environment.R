benchmark_check_environment <- function(quarto = Sys.getenv("QUARTO", "quarto")) {
  expected_r <- trimws(readLines(".R-version"))
  if (as.character(getRversion()) != expected_r) {
    stop("Use R ", expected_r, " for this project; found ", getRversion(),
         ". Install the matching R runtime before renv::restore().")
  }
  packages <- renv::lockfile_read("renv.lock")$Packages
  for (name in names(packages)) {
    tryCatch(loadNamespace(name), error = function(e) {
      stop("Cannot load R package ", name, ": ", conditionMessage(e),
           "\nRun renv::restore() and check the native prerequisites in README.md.",
           call. = FALSE)
    })
    if (utils::packageVersion(name) != package_version(packages[[name]]$Version)) {
      stop("R package ", name, " differs from renv.lock; run renv::restore().")
    }
  }
  if (!nzchar(Sys.which(quarto)) && !file.exists(quarto)) {
    stop("Quarto is missing: install Quarto 1.10.18 and put it on PATH, ",
         "or set QUARTO to its executable path. See README.md.")
  }
  version <- processx::run(quarto, "--version", error_on_status = FALSE)
  if (version$status != 0L) {
    stop("Quarto version query failed: ", version$stdout, version$stderr)
  }
  if (trimws(version$stdout) != "1.10.18") {
    stop("Use Quarto 1.10.18; found ", trimws(version$stdout), ". See README.md.")
  }
  list(
    R = R.version.string,
    OS = Sys.info()[c("sysname", "release", "machine")],
    Quarto = trimws(version$stdout),
    geospatial = sf::sf_extSoftVersion(),
    LAStools = "Not managed by renv; verify distribution, checksums and license before a run."
  )
}

if (sys.nframe() == 0L) {
  source("renv/activate.R")
  print(benchmark_check_environment())
}
