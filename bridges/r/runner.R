# lidarbench R bridge runner: execute one legacy lidR/lasR workload in a fresh process.
#
#   Rscript --vanilla runner.R --project <dir> --renv on|off \
#       --invocation <work_dir>/r-invocation.json --result <work_dir>/r-bridge-result.json
#
# Reuses the legacy harness unchanged: R/1_functions.R is sourced, the task
# call is extracted from R/0_setup.R (and must equal the definition pinned by
# the Python adapter), the catalog is configured by prepare_input(), threads by
# benchmark_with_resources() and the call is run and timed by
# benchmark_execute_call(). The inner interval is that function's start/finish
# time; namespaces the call would load lazily (sf, terra, ...) are preloaded
# before it. A result record (lidarbench.r-bridge-result/1) is always attempted.
#
# Exit status: 0 succeeded; 1 R error (preparation, resources or workload);
# 2 invalid invocation or definition mismatch; 3 R warning during the workload;
# 4 renv or package failure; 5 result record could not be written.
local({
  bridge_start <- Sys.time()
  startup_elapsed_s <- proc.time()[["elapsed"]]
  # Rscript passes the script path as --file=<path> with spaces encoded as "~+~".
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  script <- gsub("~+~", " ", sub("^--file=", "", file_arg[[1]]), fixed = TRUE)
  bridge_dir <- dirname(normalizePath(script, winslash = "/"))
  source(file.path(bridge_dir, "bridge.R"), local = TRUE)

  options <- tryCatch(
    bridge_parse_args(commandArgs(trailingOnly = TRUE),
                      c("project", "renv", "invocation", "result")),
    error = function(e) {
      message("lidarbench-bridge: ", conditionMessage(e))
      quit(save = "no", status = 2L)
    }
  )

  # Measured in a fresh process: namespaces loaded during the first pinned call
  # (design/R_BRIDGE.md §7). lidR already loads sf and terra with its imports.
  bridge_preload_namespaces <- list(lasR = c("sf", "terra"), lidR = c("sf", "terra", "glue"))

  phase <- "invocation"
  marks <- list(bridge_start = bridge_wall_ns(bridge_start))
  mark <- function(name) marks[[name]] <<- bridge_wall_ns()
  conditions <- list()
  record <- list(
    schema = "lidarbench.r-bridge-result/1",
    status = "failed",
    phase = phase,
    exit_code = 2L,
    tool = NULL,
    workload_id = NULL,
    diagnostic = NULL,
    warnings = list(),
    r = list(
      version = as.character(getRversion()),
      version_string = R.version.string,
      platform = R.version$platform,
      startup_elapsed_s = startup_elapsed_s,
      blas = unname(extSoftVersion()[["BLAS"]]),
      lapack = tryCatch(La_library(), error = function(e) NA_character_)
    )
  )
  fail <- function(code, ...) {
    condition <- structure(
      class = c("bridge_failure", "error", "condition"),
      list(message = paste0(...), call = NULL, code = code)
    )
    stop(condition)
  }

  outcome <- withCallingHandlers(tryCatch({
    project <- normalizePath(options$project, winslash = "/", mustWork = TRUE)
    use_renv <- bridge_flag(options$renv, "renv")

    phase <- "environment"
    record$renv <- list(enabled = use_renv, project = project)
    if (use_renv) {
      tryCatch(bridge_activate_renv(project),
               error = function(e) fail(4L, "renv activation failed: ", conditionMessage(e)))
      record$renv$version <- bridge_package_version("renv")
    }
    record$renv$library_paths <- I(.libPaths())
    mark("environment_ready")

    phase <- "invocation"
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      fail(4L, "R package 'jsonlite' is not available; run renv::restore()")
    }
    invocation <- tryCatch(
      jsonlite::fromJSON(options$invocation, simplifyVector = FALSE),
      error = function(e) fail(2L, "cannot read invocation: ", conditionMessage(e))
    )
    if (!identical(invocation$schema, "lidarbench.r-bridge-invocation/1")) {
      fail(2L, "unsupported invocation schema: ", format(invocation$schema))
    }
    tool <- invocation$tool
    if (!is.character(tool) || !tool %in% c("lidR", "lasR")) fail(2L, "unsupported tool: ", tool)
    record$tool <- tool
    record$workload_id <- invocation$workload_id
    inputs <- unlist(invocation$inputs)
    output_dir <- normalizePath(invocation$output_dir, winslash = "/", mustWork = TRUE)
    threads <- as.integer(invocation$threads)
    buffer <- as.numeric(invocation$buffer)
    if (!length(inputs) || length(threads) != 1L || is.na(threads) || threads < 1L ||
        length(buffer) != 1L || !is.finite(buffer)) {
      fail(2L, "invocation needs inputs, threads >= 1 and a finite buffer")
    }
    future_backend <- invocation$lidr_future_backend
    if (is.null(future_backend)) future_backend <- "multisession"
    if (!is.character(future_backend) || length(future_backend) != 1L ||
        !future_backend %in% c("multisession", "mirai") ||
        (!identical(tool, "lidR") && !is.null(invocation$lidr_future_backend))) {
      fail(2L, "invalid lidr_future_backend: ", format(invocation$lidr_future_backend))
    }

    phase <- "packages"
    required <- unlist(invocation$required_packages)
    missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing)) {
      fail(4L, "R package(s) not available: ", paste(missing, collapse = ", "),
           "; run renv::restore() in ", project)
    }
    suppressPackageStartupMessages(library(tool, character.only = TRUE))
    record$packages <- lapply(stats::setNames(required, required), bridge_package_version)
    mark("packages_loaded")

    # Namespaces the pinned calls would otherwise load lazily inside the inner
    # interval (lasR::exec() loads sf and terra with GDAL/PROJ/GEOS, ~5 s).
    # The legacy harness paid this once per session, before its timed calls.
    # Version null: not installed, skipped.
    preload <- bridge_preload_namespaces[[tool]]
    record$preloaded_namespaces <- lapply(stats::setNames(preload, preload), function(name) {
      if (suppressPackageStartupMessages(requireNamespace(name, quietly = TRUE))) {
        as.character(getNamespaceVersion(name))
      } else {
        NA_character_
      }
    })
    mark("namespaces_preloaded")

    phase <- "invocation"
    harness <- new.env(parent = globalenv())
    sys.source(file.path(project, "R", "1_functions.R"), envir = harness)
    tasks <- bridge_legacy_tasks(file.path(project, "R", "0_setup.R"))
    task <- tasks[[invocation$task_id]]
    if (is.null(task)) fail(2L, "R/0_setup.R defines no task ", invocation$task_id)
    call <- task[[paste0("task_call_", tool)]]
    record$legacy <- list(
      setup_file = file.path(project, "R", "0_setup.R"),
      functions_file = file.path(project, "R", "1_functions.R"),
      task_id = invocation$task_id,
      call = call
    )
    if (!is.character(call) || length(call) != 1L || !identical(call, invocation$expected_call)) {
      fail(2L, "the ", tool, " call for ", invocation$task_id, " in R/0_setup.R differs from ",
           "the definition pinned by the adapter; a changed definition needs a new workload ",
           "version")
    }
    if (!identical(task$data_type, "datasets_org") ||
        !identical(task$output_format, invocation$output_extension)) {
      fail(2L, "task ", invocation$task_id, " is not an original-Z ",
           invocation$output_extension, " workload")
    }
    mark("legacy_sourced")

    phase <- "preparation"
    missing_inputs <- inputs[!file.exists(inputs) | file.access(inputs, 4) != 0]
    if (length(missing_inputs)) {
      fail(1L, "missing or unreadable input(s): ", paste(missing_inputs, collapse = ", "))
    }
    inputs <- normalizePath(inputs, winslash = "/", mustWork = TRUE)
    epsg <- invocation$epsg
    context <- new.env(parent = globalenv())
    context$dir_out <- output_dir
    context$cores <- threads
    context$fout <- paste0(output_dir, "/*.", invocation$output_extension)
    if (tool == "lidR") {
      ctg <- harness$prepare_input(inputs, output_dir)
      if (!isTRUE(all.equal(lidR::opt_chunk_buffer(ctg), buffer)) ||
          lidR::opt_chunk_size(ctg) != 0 ||
          !identical(as.numeric(lidR::opt_chunk_alignment(ctg)), c(0, 0))) {
        fail(1L, "catalog buffer/chunking differs from the workload contract (buffer ", buffer, ")")
      }
      if (!is.null(epsg)) {
        declared <- sf::st_crs(as.integer(epsg))
        for (file in ctg@data$filename) {
          existing <- sf::st_crs(lidR::readLASheader(file))
          if (!is.na(existing) && !isTRUE(existing == declared)) {
            fail(1L, "declared EPSG:", epsg, " conflicts with the CRS of ", file)
          }
        }
        sf::st_crs(ctg) <- declared
      }
      files <- normalizePath(ctg@data$filename, winslash = "/", mustWork = TRUE)
      if (!setequal(files, inputs)) fail(1L, "catalog files differ from the invocation inputs")
      context$ctg <- ctg
      context$f <- files
      record$catalog <- list(
        buffer = lidR::opt_chunk_buffer(ctg),
        chunk_size = lidR::opt_chunk_size(ctg),
        alignment = I(as.numeric(lidR::opt_chunk_alignment(ctg))),
        wall_to_wall = lidR::opt_wall_to_wall(ctg),
        stop_early = lidR::opt_stop_early(ctg),
        output_files = lidR::opt_output_files(ctg),
        crs = if (is.null(epsg)) NULL else paste0("EPSG:", epsg)
      )
    } else {
      with <- eval(parse(text = call, keep.source = FALSE)[[1]]$with, baseenv())
      if (!is.list(with) || !isTRUE(all.equal(with$buffer, buffer)) || !identical(with$chunk, 0)) {
        fail(1L, "lasR buffer/chunking differs from the workload contract (buffer ", buffer, ")")
      }
      # lasR indexes unindexed files on the fly by writing .lax files next to
      # them, which would modify the protected input tree. COPC files carry
      # their own spatial index (first VLR "copc"/1) and need no .lax.
      bridge_is_copc <- function(path) {
        con <- file(path, "rb")
        on.exit(close(con))
        head <- readBin(con, "raw", 104L)
        if (length(head) < 104L || !identical(rawToChar(head[1:4]), "LASF")) return(FALSE)
        size <- sum(as.integer(head[95:96]) * c(1, 256))
        vlrs <- sum(as.integer(head[101:104]) * c(1, 256, 65536, 16777216))
        if (vlrs < 1 || size < 104) return(FALSE)
        rest <- readBin(con, "raw", size - 104L + 20L)
        if (length(rest) < size - 104L + 20L) return(FALSE)
        vlr <- rest[(size - 104L + 1L):(size - 104L + 20L)]
        user <- vlr[3:18]
        identical(rawToChar(user[user != as.raw(0)]), "copc") &&
          sum(as.integer(vlr[19:20]) * c(1, 256)) == 1
      }
      copc <- vapply(inputs, bridge_is_copc, logical(1))
      indexes <- sub("\\.la[sz]$", ".lax", inputs, ignore.case = TRUE)
      unindexed <- inputs[!copc & (!file.exists(indexes) | file.info(indexes)$size <= 0)]
      if (length(unindexed)) {
        fail(1L, "lasR inputs need existing nonempty LAX indexes or COPC (lasR would otherwise ",
             "write them into the protected input directory): ", paste(unindexed, collapse = ", "))
      }
      if (!is.null(epsg)) {
        declared <- as.integer(epsg)
        context$exec <- function(pipeline, on, with = NULL, ...) {
          lasR::exec(lasR::set_crs(declared) + pipeline, on = on, with = with, ...)
        }
      }
      context$f <- inputs
      record$catalog <- list(buffer = with$buffer, chunk = with$chunk,
                             progress = with$progress,
                             crs = if (is.null(epsg)) NULL else paste0("EPSG:", epsg))
    }
    mark("prepared")

    phase <- "resources"
    result <- harness$benchmark_with_resources(
      tool, threads, list(resource_monitoring = FALSE, lidr_future_backend = future_backend),
      invocation$workload_id,
      function() {
        variables <- c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
                       "BLIS_NUM_THREADS", "VECLIB_MAXIMUM_THREADS",
                       "RCPP_PARALLEL_NUM_THREADS", "OMP_THREAD_LIMIT", "OMP_MAX_ACTIVE_LEVELS")
        applied <- list(
          requested = threads,
          data_table_threads = data.table::getDTthreads(),
          environment = as.list(Sys.getenv(variables))
        )
        if (tool == "lidR") {
          applied$lidr_threads <- lidR::get_lidr_threads()
          applied$future_backend <- future_backend
          applied$future_plan <- setdiff(class(future::plan()), c("FutureStrategy", "tweaked"))[[1]]
          applied$future_workers <- as.integer(future::nbrOfWorkers())
        } else {
          strategy <- lasR::get_parallel_strategy()
          applied$lasr_strategy <- attr(strategy, "strategy")
          applied$lasr_ncores <- I(as.integer(strategy))
        }
        record$threads <<- applied
        mark("resources_ready")
        phase <<- "workload"
        namespaces_before <- loadedNamespaces()
        on.exit({
          record$workload_loaded_namespaces <<- I(sort(setdiff(loadedNamespaces(),
                                                               namespaces_before)))
          phase <<- "teardown"
        })
        harness$benchmark_execute_call(call, tool, context)
      }
    )
    mark("teardown_done")
    record$timing_workload <- list(
      start_wall_ns = bridge_wall_ns(result$start_time),
      end_wall_ns = bridge_wall_ns(result$finish_time),
      elapsed_s = as.numeric(difftime(result$finish_time, result$start_time, units = "secs"))
    )
    record$legacy$execution_status <- result$execution_status
    if (!identical(result$execution_status, "succeeded")) {
      if (is.na(result$start_time)) phase <- "resources" else phase <- "workload"
      fail(1L, result$diagnostic)
    }
    workload_warnings <- Filter(function(w) identical(w$phase, "workload"), conditions)
    if (length(workload_warnings)) {
      phase <- "workload"
      fail(3L, length(workload_warnings), " R warning(s) during the workload: ",
           paste(vapply(workload_warnings, `[[`, character(1), "message"), collapse = " | "))
    }
    phase <- "complete"
    list(status = "succeeded", code = 0L, diagnostic = NULL)
  }, bridge_failure = function(e) {
    list(status = "failed", code = e$code, diagnostic = conditionMessage(e))
  }, error = function(e) {
    list(status = "failed", code = if (phase %in% c("environment", "packages")) 4L else
           if (phase == "invocation") 2L else 1L,
         diagnostic = conditionMessage(e))
  }), warning = function(w) {
    conditions[[length(conditions) + 1L]] <<- list(phase = phase, message = conditionMessage(w))
    message("lidarbench-bridge warning [", phase, "]: ", conditionMessage(w))
    invokeRestart("muffleWarning")
  })

  mark("bridge_end")
  record$status <- outcome$status
  record$phase <- phase
  record$exit_code <- outcome$code
  record$diagnostic <- outcome$diagnostic
  record$warnings <- conditions
  record$timing <- c(list(clock = "R Sys.time()"), marks)
  if (!is.null(record$tool)) {
    record$loaded_namespaces <- bridge_loaded_versions()
    record$outputs <- I(sort(list.files(invocation$output_dir, recursive = TRUE)))
  }
  if (!is.null(outcome$diagnostic)) message("lidarbench-bridge ", phase, ": ", outcome$diagnostic)
  written <- tryCatch({
    bridge_write_json(record, options$result)
    TRUE
  }, error = function(e) {
    message("lidarbench-bridge: cannot write result record: ", conditionMessage(e))
    FALSE
  })
  quit(save = "no", status = if (written) outcome$code else 5L)
})
