# Runs the pinned lidR / lasR calls of two workloads on fixture tiles, untimed.
# Used only by tests/python/test_validation_r_integration.py.
#
#   Rscript run_r_workloads.R <repo> <input_dir> <output_root> <epsg>
#
# Calls are copied verbatim from R/0_setup.R; the catalog is configured by the
# harness's own prepare_input() and the declared CRS is applied like
# benchmark_apply_input_metadata() / the lasR set_crs() wrapper in R/1_functions.R.
args <- commandArgs(trailingOnly = TRUE)
repo <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
dir_in <- normalizePath(args[[2]], winslash = "/", mustWork = TRUE)
out_root <- normalizePath(args[[3]], winslash = "/", mustWork = TRUE)
epsg <- as.integer(args[[4]])

owd <- setwd(repo)
source("renv/activate.R")
setwd(owd)
suppressPackageStartupMessages({
  library(lidR)
  library(lasR)
})
harness <- new.env()
sys.source(file.path(repo, "R/1_functions.R"), harness)
lidR::set_lidr_threads(1L)

calls <- list(
  pixel_metrics_1a = list(
    lidR = "lidR::pixel_metrics(las = ctg, ~list(z_mean = mean(Z)), res = 20, start = c(0, 0))",
    lasR = "exec(pipeline = rasterize(20, 'z_mean', ofile = fout), on = f, with = list(buffer = 20, chunk = 0, progress = FALSE))"
  ),
  generate_DSM1 = list(
    lidR = "lidR::rasterize_canopy(ctg, res = 1, algorithm = p2r(subcircle = 0, na.fill = NULL))",
    lasR = "exec(pipeline = dsm(1, tin = FALSE, ofile = fout), on = f, with = list(buffer = 20, chunk = 0, progress = FALSE))"
  )
)

status <- list()
for (task in names(calls)) {
  for (tool in names(calls[[task]])) {
    dir_out <- file.path(out_root, paste0(task, "__", tool))
    dir.create(dir_out)
    ctg <- harness$prepare_input(dir_in, dir_out)
    sf::st_crs(ctg) <- sf::st_crs(epsg)
    context <- new.env(parent = globalenv())
    context$ctg <- ctg
    context$f <- ctg@data$filename
    context$fout <- paste0(dir_out, "/*.tif")
    context$exec <- function(pipeline, on, with = NULL, ...) {
      lasR::exec(lasR::set_crs(epsg) + pipeline, on = on, with = with, ...)
    }
    outcome <- tryCatch({
      eval(parse(text = calls[[task]][[tool]]), envir = context)
      "succeeded"
    }, warning = function(w) paste("warning:", conditionMessage(w)),
       error = function(e) paste("error:", conditionMessage(e)))
    status[[paste0(task, "__", tool)]] <- outcome
  }
}
writeLines(paste(names(status), unlist(status), sep = "\t"), file.path(out_root, "status.tsv"))
