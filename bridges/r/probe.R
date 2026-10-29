# lidarbench R bridge probe: report the R runtime and package availability.
#
#   Rscript --vanilla probe.R --project <dir> --renv on|off --packages a,b,c
#
# Prints one line "LIDARBENCH_R_PROBE <json>" (schema lidarbench.r-bridge-probe/1)
# on stdout. Package namespaces are loaded (not attached) so broken native
# libraries are detected; nothing is written to disk.
local({
  # Rscript passes the script path as --file=<path> with spaces encoded as "~+~".
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  script <- gsub("~+~", " ", sub("^--file=", "", file_arg[[1]]), fixed = TRUE)
  bridge_dir <- dirname(normalizePath(script, winslash = "/"))
  source(file.path(bridge_dir, "bridge.R"), local = TRUE)

  record <- list(
    schema = "lidarbench.r-bridge-probe/1",
    r_version = as.character(getRversion()),
    r_version_string = R.version.string,
    platform = R.version$platform,
    blas = unname(extSoftVersion()[["BLAS"]]),
    lapack = tryCatch(La_library(), error = function(e) NA_character_),
    renv = list(enabled = FALSE, activated = FALSE, error = NULL),
    library_paths = NULL,
    lockfile = list(r_version = NULL, packages = structure(list(), names = character())),
    packages = structure(list(), names = character()),
    error = NULL
  )
  status <- tryCatch({
    options <- bridge_parse_args(commandArgs(trailingOnly = TRUE),
                                 c("project", "renv", "packages"))
    project <- normalizePath(options$project, winslash = "/", mustWork = TRUE)
    packages <- strsplit(options$packages, ",", fixed = TRUE)[[1]]
    record$renv$enabled <- bridge_flag(options$renv, "renv")
    if (record$renv$enabled) {
      record$renv$activated <- tryCatch({
        bridge_activate_renv(project)
        TRUE
      }, error = function(e) {
        record$renv$error <<- conditionMessage(e)
        FALSE
      })
      if (record$renv$activated && requireNamespace("renv", quietly = TRUE)) {
        lock <- renv::lockfile_read(file.path(project, "renv.lock"))
        record$lockfile$r_version <- lock$R$Version
        for (package in packages) {
          record$lockfile$packages[[package]] <- lock$Packages[[package]]$Version
        }
        record$renv$version <- bridge_package_version("renv")
      }
    }
    record$library_paths <- I(.libPaths())
    for (package in packages) {
      entry <- list(installed = bridge_package_version(package), loadable = FALSE, error = NULL)
      if (!is.na(entry$installed)) {
        entry$loadable <- tryCatch({
          suppressPackageStartupMessages(loadNamespace(package))
          TRUE
        }, error = function(e) {
          entry$error <<- conditionMessage(e)
          FALSE
        })
      }
      record$packages[[package]] <- entry
    }
    0L
  }, error = function(e) {
    record$error <<- conditionMessage(e)
    2L
  })
  cat("LIDARBENCH_R_PROBE ", bridge_json(record), "\n", sep = "")
  quit(save = "no", status = status)
})
