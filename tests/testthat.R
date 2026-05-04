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

cat("=== testthat.R diagnostics ===\n")
cat(".libPaths():\n"); print(.libPaths())
cat("R_LIBS_USER:", Sys.getenv("R_LIBS_USER"), "\n")
cat("R_LIBS_SITE:", Sys.getenv("R_LIBS_SITE"), "\n")
cat("R_LIB_FOR_PAK:", Sys.getenv("R_LIB_FOR_PAK"), "\n")
cat("pak findable:", "pak" %in% rownames(installed.packages()), "\n")
cat("Require findable:", "Require" %in% rownames(installed.packages()), "\n")

library(Require)
library(testthat)
test_check("Require")
