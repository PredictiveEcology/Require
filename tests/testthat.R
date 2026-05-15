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
test_check("Require")
