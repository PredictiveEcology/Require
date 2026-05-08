# Point pak's pkgcache at a per-session writable cache BEFORE library(Require)
# loads pak. Under R CMD check (CRAN policy), pkgcache aborts if R_USER_CACHE_DIR
# is unset; without this every Require::Install() inside the test suite errors
# with "Please install pak" because pak's namespace fails to load.
if (!nzchar(Sys.getenv("R_USER_CACHE_DIR"))) {
  .ucd <- tempfile("RequireUserCache_")
  dir.create(.ucd, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(R_USER_CACHE_DIR = .ucd)
  rm(.ucd)
}

library(Require)
library(testthat)
# ProgressReporter with update_interval = 0 prints per-test_that elapsed
# (e.g. "[ 1.4s ]") into the CI log, so we can see which tests dominate
# wall time. The default reporter only prints aggregate totals which
# tells us nothing about which test_that block is slow. Once we have
# the per-test timing data we want, this can revert to the bare
# `test_check("Require")` form.
test_check("Require",
           reporter = testthat::ProgressReporter$new(update_interval = 0))
