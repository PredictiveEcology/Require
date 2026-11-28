## identify-and-defer asked "is it still missing?" by package NAME. A package installed below its floor
## (a CRAN binary, while a dependent needs the newer GitHub build) counted as done and was never
## retried, so the dependent failed with "dependency 'X' is not available". Seen in every
## PredictiveEcology module CI once a floor outran CRAN: SpaDES.tools 2.1.3 from CRAN vs
## fireSenseUtils' SpaDES.tools (>= 2.1.3.9008) (fireSense_SpreadFit #31, 2026-09-23).

tinyPkgLib <- function(version = "0.0.1") {
  src <- withr::local_tempdir(.local_envir = parent.frame())
  lib <- withr::local_tempdir(.local_envir = parent.frame())
  p <- file.path(src, "tinyFloorPkg")
  dir.create(file.path(p, "R"), recursive = TRUE)
  writeLines(c("Package: tinyFloorPkg", paste0("Version: ", version), "Title: t", "Description: t.",
               "License: MIT", "Encoding: UTF-8"), file.path(p, "DESCRIPTION"))
  writeLines("export(f)", file.path(p, "NAMESPACE"))
  writeLines("f <- function() 1", file.path(p, "R", "f.R"))
  out <- system2(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", "--no-test-load", "-l", shQuote(lib), shQuote(p)),
                 stdout = TRUE, stderr = TRUE)
  lib
}

req <- function(spec = NA_character_, ineq = NA_character_)
  data.table::data.table(Package = "tinyFloorPkg", versionSpec = spec, inequality = ineq)

test_that(".pakSatisfiedInstalled counts a package only if it meets its floor", {
  lib <- tinyPkgLib("0.0.1")
  expect_true("tinyFloorPkg" %in% Require:::.pakSatisfiedInstalled(lib, req()))            # no floor
  expect_true("tinyFloorPkg" %in% Require:::.pakSatisfiedInstalled(lib, req("0.0.1", ">=")))
  expect_false("tinyFloorPkg" %in% Require:::.pakSatisfiedInstalled(lib, req("0.0.2", ">=")))
  ## every requirement recorded for the package must hold
  both <- rbind(req("0.0.1", ">="), req("0.0.2", ">="))
  expect_false("tinyFloorPkg" %in% Require:::.pakSatisfiedInstalled(lib, both))
  expect_identical(Require:::.pakSatisfiedInstalled(withr::local_tempdir(), req()), character(0))
})

test_that("identify-and-defer retries a package installed below its floor", {
  ## The CI case: the dependency is installed, but below the floor a dependent needs, and the first
  ## (parallel) pak batch installed nothing -- as when pak abandons a batch on another package's
  ## failure. The deferral passes must still see it as missing and install it.
  lib <- tinyPkgLib("0.0.1")
  newSrc <- withr::local_tempdir()
  p2 <- file.path(newSrc, "tinyFloorPkg"); dir.create(file.path(p2, "R"), recursive = TRUE)
  writeLines(c("Package: tinyFloorPkg", "Version: 0.0.2", "Title: t", "Description: t.",
               "License: MIT", "Encoding: UTF-8"), file.path(p2, "DESCRIPTION"))
  writeLines("export(f)", file.path(p2, "NAMESPACE")); writeLines("f <- function() 2", file.path(p2, "R", "f.R"))

  asked <- new.env(); asked$refs <- character(0)
  testthat::local_mocked_bindings(pak = function(...) invisible(NULL), .package = "pak")   # the abandoned batch
  testthat::local_mocked_bindings(
    pakSerialInstall = function(pkgs, lib, repos, verbose, cranDeps = NA) {
      asked$refs <- c(asked$refs, pkgs)
      system2(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", "--no-test-load", "-l", shQuote(lib), shQuote(p2)),
              stdout = TRUE, stderr = TRUE)
      invisible(NULL)
    },
    pakResetSubprocess = function(...) invisible(NULL), .package = "Require")

  pkgDT <- data.table::data.table(
    Package = "tinyFloorPkg", packageFullName = "Owner/tinyFloorPkg@main (>= 0.0.2)",
    needInstall = Require:::.txtInstall, versionSpec = "0.0.2", inequality = ">=",
    Version = "0.0.1", LibPath = lib, installed = TRUE, installedVersionOK = FALSE,
    loadedSufficient = FALSE, repoLocation = "GitHub", Account = "Owner", Repo = "tinyFloorPkg",
    Branch = "main", GitSubFolder = NA_character_)
  suppressWarnings(Require:::pakInstallFiltered(pkgDT, libPaths = lib, repos = getOption("repos"),
                                                standAlone = TRUE, verbose = -2))
  expect_true(any(grepl("tinyFloorPkg", asked$refs)))
  expect_identical(unname(installed.packages(lib, noCache = TRUE)[, "Version"]), "0.0.2")
})

test_that("a package pak reinstalls at the same version is counted as installed; one it keeps is not", {
  ## pak reports a kept package and a same-version reinstall alike (same version, and for CRAN no
  ## commit), so pakInstallFiltered looks at whether the package's DESCRIPTION was rewritten.
  run <- function(reinstall, gh = FALSE) {
    lib <- tinyPkgLib("0.0.1")
    src <- withr::local_tempdir(.local_envir = parent.frame())
    p <- file.path(src, "tinyFloorPkg"); dir.create(file.path(p, "R"), recursive = TRUE)
    writeLines(c("Package: tinyFloorPkg", "Version: 0.0.1", "Title: t", "Description: t.",
                 "License: MIT", "Encoding: UTF-8", if (gh) "RemoteSha: new"),
               file.path(p, "DESCRIPTION"))
    writeLines("export(f)", file.path(p, "NAMESPACE")); writeLines("f <- function() 1", file.path(p, "R", "f.R"))
    ## the first pak call is the one that (re)installs or keeps the package
    testthat::local_mocked_bindings(
      pak = function(...) {
        if (reinstall) {
          Sys.sleep(1.1)
          system2(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", "--no-test-load", "-l", shQuote(lib), shQuote(p)),
                  stdout = TRUE, stderr = TRUE)
        }
        invisible(NULL)
      }, .package = "pak")
    testthat::local_mocked_bindings(pakResetSubprocess = function(...) invisible(NULL), .package = "Require")
    pkgDT <- data.table::data.table(
      Package = "tinyFloorPkg", packageFullName = if (gh) "Owner/tinyFloorPkg@main" else "tinyFloorPkg",
      needInstall = Require:::.txtInstall, versionSpec = NA_character_, inequality = NA_character_,
      Version = NA_character_, LibPath = lib, installed = FALSE, installedVersionOK = FALSE,
      loadedSufficient = FALSE, repoLocation = if (gh) "GitHub" else "CRAN",
      Account = if (gh) "Owner" else NA_character_, Repo = if (gh) "tinyFloorPkg" else NA_character_,
      Branch = if (gh) "main" else NA_character_, GitSubFolder = NA_character_)
    suppressWarnings(Require:::pakInstallFiltered(pkgDT, libPaths = lib, repos = getOption("repos"),
                                                  standAlone = TRUE, verbose = -2))$installResult
  }
  skip_if_not_installed("pak")
  expect_identical(run(reinstall = FALSE), Require:::.txtOKKept)
  expect_identical(run(reinstall = TRUE), "OK")                  # same version, CRAN
  expect_identical(run(reinstall = TRUE, gh = TRUE), "OK")       # same version, new commit
})

test_that("a kept loaded HEAD package is not flagged for restart, a reinstalled one is", {
  loadedPkg <- "data.table"   # loaded (and not a base package); the flag only needs a loaded namespace
  mk <- function(res) data.table::data.table(
    Package = loadedPkg, versionSpec = NA_character_, inequality = NA_character_,
    Version = "1.0", needInstall = Require:::.txtInstall, installResult = res, hasHEAD = TRUE)
  flag <- function(res) {
    d <- mk(res); data.table::setnames(d, "hasHEAD", Require:::hasHEADtxt)
    Require:::flagRestartForLoadedInsufficient(d)$installResult
  }
  expect_match(flag("OK"), "restart")
  expect_identical(flag(Require:::.txtOKKept), Require:::.txtOKKept)
})

test_that("installedStamps ignores chmod and hard links but sees a rewritten DESCRIPTION", {
  lib <- tinyPkgLib("0.0.1")
  before <- Require:::installedStamps(lib, "tinyFloorPkg")
  f <- file.path(lib, "tinyFloorPkg", "DESCRIPTION")
  Sys.sleep(1.1)
  Sys.chmod(f, "444")
  suppressWarnings(file.link(f, file.path(withr::local_tempdir(), "DESCRIPTION")))
  expect_identical(Require:::installedStamps(lib, "tinyFloorPkg"), before)
  Sys.chmod(f, "644")
  writeLines(readLines(f), f)
  expect_false(identical(Require:::installedStamps(lib, "tinyFloorPkg"), before))
  expect_true(is.na(Require:::installedStamps(lib, "notThere")))
})
