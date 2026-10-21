# Run from the repository root: Rscript --vanilla tests/testthat.R
source("renv/activate.R")
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Restore the project dependencies with renv::restore(); see README.md.")
}
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
