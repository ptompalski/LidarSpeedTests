# Run from the repository root: Rscript --vanilla tests/testthat.R
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Install the test dependencies documented in README.md, including testthat.")
}
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
