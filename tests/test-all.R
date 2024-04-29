# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(Require)
library(testthat)
if (nzchar(Sys.getenv("NOT_CRAN")) && as.logical(Sys.getenv("NOT_CRAN"))) {
  clearRequirePackageCache(ask = FALSE)
  test_check("Require")

  # 2nd time should have cache setup up
  test_check("Require")

} else {
  test_check("Require")
}

