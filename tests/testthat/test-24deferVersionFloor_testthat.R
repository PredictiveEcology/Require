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
