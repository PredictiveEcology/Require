runTests <- function(have, pkgs) {
  # recall LandR.CS won't be installed, also, Version number is not in place for newly installed packages
  theTest <- all(!is.na(have[installed == TRUE]$Version))
  if (!isTRUE(theTest)) browser()
  testthat::expect_true(isTRUE(theTest))
  if ("installResult" %in% colnames(have)) {
    theTest <- NROW(have[is.na(installResult) | installResult %in% "OK"]) == sum(have$installed)
    if (!isTRUE(theTest)) browser()
    testthat::expect_true(isTRUE(theTest))
  }
}


if (Require:::.isDevelVersion() && nchar(Sys.getenv("R_REQUIRE_RUN_ALL_TESTS")) == 0) {
  Sys.setenv("R_REQUIRE_RUN_ALL_TESTS" = "true")
}

isDev <- Sys.getenv("R_REQUIRE_RUN_ALL_TESTS") == "true" &&
  Sys.getenv("R_REQUIRE_CHECK_AS_CRAN") != "true"
# Actually interactive
isDevAndInteractive <- interactive() && isDev && Sys.getenv("R_REQUIRE_TEST_AS_INTERACTIVE") != "false"

if (!isDevAndInteractive) { # i.e., CRAN
  Sys.setenv(R_REQUIRE_PKG_CACHE = "FALSE")
}


withr::local_options(
  list(Require.isDev = isDev, Require.isDevAndInteractive = isDevAndInteractive),
  .local_envir = teardown_env()
)
