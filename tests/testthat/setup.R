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

suggests <- DESCRIPTIONFileDeps(system.file("DESCRIPTION", package = "Require"), which = "Suggests") |>
  extractPkgName()
# pkgsToLoad <- c("curl", "gitcreds", "httr", "openssl", "googledrive", "rappdirs", "waldo", "rematch2", "diffobj")
for (pk in suggests)
  suppressWarnings(withr::local_package(pk, .local_envir = teardown_env(), quietly = TRUE))

if (Sys.info()["user"] %in% "emcintir") {
  secretPath <- if (isWindows()) "c:/Eliot/.secret" else "/home/emcintir/.secret"
  options(Require.cloneFrom = Sys.getenv("R_LIBS_USER"),
          Require.origLibPathForTests = .libPaths()[1],
          Require.installPackagesSys = 2L,
          gargle_oauth_email = "eliotmcintire@gmail.com",
          gargle_oauth_cache = secretPath)#, .local_envir = teardown_env())
  googledrive::drive_auth()
}

getCRANrepos(ind = 1)

withr::local_options(
  list(Require.isDev = isDev, Require.isDevAndInteractive = isDevAndInteractive),
  .local_envir = teardown_env()
)


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


testWarnsInUsePleaseChange <- function(warns) {
  test <- TRUE
  if (length(warns)) {
    test <- all(grepl(paste0(msgIsInUse, "|Please change required"), warns)) # "Please change" comes with verbose >= 1
  }
  test
}
