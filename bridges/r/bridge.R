# Shared helpers of the lidarbench R bridge (runner.R, probe.R).
#
# Base R only: these helpers must work before renv is activated and when
# packages such as jsonlite are missing, so failures can still be reported as
# structured records. See design/R_BRIDGE.md for the protocol.

bridge_parse_args <- function(args, names) {
  values <- list()
  i <- 1L
  while (i <= length(args)) {
    key <- sub("^--", "", args[[i]])
    if (!startsWith(args[[i]], "--") || !key %in% names || i == length(args)) {
      stop("invalid bridge argument: ", args[[i]])
    }
    if (!is.null(values[[key]])) stop("duplicate bridge argument: --", key)
    values[[key]] <- args[[i + 1L]]
    i <- i + 2L
  }
  missing <- setdiff(names, names(values))
  if (length(missing)) stop("missing bridge argument(s): ", paste0("--", missing, collapse = ", "))
  values
}

bridge_flag <- function(value, name) {
  if (!value %in% c("on", "off")) stop("--", name, " must be 'on' or 'off'")
  identical(value, "on")
}

# Nanoseconds since the Unix epoch as a JSON integer (written verbatim, so the
# value is not rounded by a 15-digit double formatter).
bridge_wall_ns <- function(time = Sys.time()) {
  value <- as.numeric(time)
  if (length(value) != 1L || !is.finite(value)) return(NULL)
  structure(sprintf("%.0f", value * 1e9), class = "bridge_verbatim")
}

bridge_json_string <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  for (code in c(1:8, 11L, 12L, 14:31)) {
    x <- gsub(intToUtf8(code), sprintf("\\u%04x", code), x, fixed = TRUE)
  }
  paste0("\"", x, "\"")
}

# Minimal JSON encoder: named lists are objects, unnamed lists are arrays,
# length-one atomic vectors are scalars (wrap in I() to force an array).
bridge_json <- function(x) {
  if (inherits(x, "bridge_verbatim")) return(as.character(unclass(x)))
  if (is.null(x)) return("null")
  if (is.list(x)) {
    if (!is.null(names(x))) {
      if (!length(x)) return("{}")
      body <- vapply(seq_along(x), function(i) {
        paste0(bridge_json_string(names(x)[[i]]), ":", bridge_json(x[[i]]))
      }, character(1))
      return(paste0("{", paste(body, collapse = ","), "}"))
    }
    return(paste0("[", paste(vapply(x, bridge_json, character(1)), collapse = ","), "]"))
  }
  if (is.factor(x)) x <- as.character(x)
  values <- if (is.character(x)) {
    ifelse(is.na(x), "null", bridge_json_string(x))
  } else if (is.logical(x)) {
    ifelse(is.na(x), "null", ifelse(x, "true", "false"))
  } else if (is.integer(x)) {
    ifelse(is.na(x), "null", as.character(x))
  } else if (is.numeric(x)) {
    ifelse(is.finite(x), sprintf("%.17g", x), "null")
  } else {
    stop("cannot encode ", class(x)[[1]], " as JSON")
  }
  if (length(x) == 1L && !inherits(x, "AsIs")) return(values[[1]])
  paste0("[", paste(values, collapse = ","), "]")
}

# Write a JSON record atomically (temporary file in the same directory, then rename).
bridge_write_json <- function(record, path) {
  text <- enc2utf8(paste0(bridge_json(record), "\n"))
  temporary <- paste0(path, ".partial")
  connection <- file(temporary, open = "wb")
  on.exit(close(connection))
  writeBin(charToRaw(text), connection)
  close(connection)
  on.exit()
  if (!file.rename(temporary, path)) stop("cannot move bridge record into place: ", path)
  invisible(path)
}

bridge_activate_renv <- function(project) {
  activate <- file.path(project, "renv", "activate.R")
  if (!file.exists(activate)) stop("renv is enabled but ", activate, " does not exist")
  previous <- setwd(project)
  on.exit(setwd(previous))
  source(activate, local = new.env(parent = globalenv()))
  invisible(.libPaths())
}

bridge_package_version <- function(package) {
  tryCatch(as.character(utils::packageVersion(package)), error = function(e) NA_character_)
}

bridge_loaded_versions <- function() {
  packages <- sort(loadedNamespaces())
  stats::setNames(
    lapply(packages, function(p) as.character(getNamespaceVersion(p))),
    packages
  )
}

# Extract the benchmark task table from R/0_setup.R without evaluating the
# file: each cell of `benchmark_tasks <- tribble(...)` is a literal (string,
# NULL, list()/c() of literals) and is evaluated on its own in baseenv().
bridge_legacy_tasks <- function(setup_file) {
  expressions <- parse(setup_file, keep.source = FALSE, encoding = "UTF-8")
  for (expression in expressions) {
    if (!is.call(expression) || !as.character(expression[[1]]) %in% c("<-", "=") ||
        !identical(expression[[2]], as.name("benchmark_tasks"))) next
    table <- expression[[3]]
    if (!is.call(table) || !identical(table[[1]], as.name("tribble"))) {
      stop("benchmark_tasks in ", setup_file, " is not a tribble() literal")
    }
    cells <- as.list(table)[-1]
    is_column <- vapply(cells, function(cell) {
      is.call(cell) && identical(cell[[1]], as.name("~")) && length(cell) == 2L
    }, logical(1))
    columns <- sum(is_column)
    if (!columns || !all(is_column[seq_len(columns)]) || any(is_column[-seq_len(columns)])) {
      stop("unexpected tribble() layout in ", setup_file)
    }
    names <- vapply(cells[seq_len(columns)], function(cell) as.character(cell[[2]]), character(1))
    values <- lapply(cells[-seq_len(columns)], function(cell) eval(cell, baseenv()))
    if (length(values) %% columns != 0L) stop("ragged tribble() in ", setup_file)
    rows <- lapply(seq_len(length(values) %/% columns), function(i) {
      row <- values[(i - 1L) * columns + seq_len(columns)]
      names(row) <- names
      row
    })
    names(rows) <- vapply(rows, function(row) row$task_id, character(1))
    return(rows)
  }
  stop("benchmark_tasks <- tribble(...) not found in ", setup_file)
}
