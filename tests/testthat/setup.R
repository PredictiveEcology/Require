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


withr::local_package("curl", lib.loc = getOption("Require.origLibPathForTests"))
# withr::local_package("openssl", lib.loc = getOption("Require.origLibPathForTests"))
# withr::local_package("googledrive", lib.loc = getOption("Require.origLibPathForTests"))
# withr::local_package("httr", lib.loc = getOption("Require.origLibPathForTests"))
# withr::local_package("rappdirs", lib.loc = getOption("Require.origLibPathForTests"))

withr::local_package("openssl", .local_envir = teardown_env())
withr::local_package("googledrive", .local_envir = teardown_env())
withr::local_package("rappdirs", .local_envir = teardown_env())
withr::local_package("httr", .local_envir = teardown_env())
withr::local_package("waldo", .local_envir = teardown_env())
withr::local_package("rematch2", .local_envir = teardown_env())
withr::local_package("diffobj", .local_envir = teardown_env())
withr::local_options(Require.RPackageCache = RequirePkgCacheDir(), .local_envir = teardown_env())

if (Sys.info()["user"] %in% "emcintir") {
  secretPath <- if (isWindows()) "c:/Eliot/.secret" else "/home/emcintir/.secret"
  options(Require.cloneFrom = Sys.getenv("R_LIBS_USER"),
          Require.origLibPathForTests = .libPaths()[1],
          gargle_oauth_email = "eliotmcintire@gmail.com",
          gargle_oauth_cache = secretPath)#, .local_envir = teardown_env())
  googledrive::drive_auth()
}


getCRANrepos(ind = 1)

if (.isDevelVersion() && nchar(Sys.getenv("R_REQUIRE_RUN_ALL_TESTS")) == 0) {
  Sys.setenv("R_REQUIRE_RUN_ALL_TESTS" = "true")
}

isDev <- Sys.getenv("R_REQUIRE_RUN_ALL_TESTS") == "true" &&
  Sys.getenv("R_REQUIRE_CHECK_AS_CRAN") != "true"
# Actually interactive
isDevAndInteractive <- interactive() && isDev && Sys.getenv("R_REQUIRE_TEST_AS_INTERACTIVE") != "false"

# try(rm(getFromCache1, getDeps1, getDepsFromCache1), silent = TRUE); i <- 0
withr::local_options(Require.verbose = ifelse(isDev, -2, -2), .local_envir = teardown_env())

if (!isDevAndInteractive) { # i.e., CRAN
  Sys.setenv(R_REQUIRE_PKG_CACHE = "FALSE")
}


withr::local_options(
  list(Require.isDev = isDev, Require.isDevAndInteractive = isDevAndInteractive),
  .local_envir = teardown_env()
)
