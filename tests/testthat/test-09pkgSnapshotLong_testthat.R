## Install the snapshot for the running R version and check it landed exactly.
##
## The requirements live in inst/snapshots/R-<major.minor>.txt, one file per
## R version, regenerated with inst/snapshots/makeSnapshot.R; what is known
## not to build on a given R lives in inst/snapshots/knownFails.txt. Nothing
## package-specific belongs in this file: a new R version means adding a
## snapshot file, not editing a test.

snapshotForThisR <- function() {
  f <- sprintf("R-%s.%s.txt", R.version$major, sub("\\..*", "", R.version$minor))
  system.file("snapshots", f, package = "Require")
}

knownFailsForThisR <- function() {
  f <- system.file("snapshots", "knownFails.txt", package = "Require")
  if (!nzchar(f)) return(character())
  kf <- data.table::fread(f, colClasses = "character")
  if (!NROW(kf)) return(character())
  r <- getRversion()
  keep <- (is.na(kf$Rmin) | !nzchar(kf$Rmin) | r >= kf$Rmin) &
          (is.na(kf$Rmax) | !nzchar(kf$Rmax) | r <= kf$Rmax)
  kf$Package[keep]
}

test_that("a snapshot installs exactly what it pins", {
  skip_on_ci()
  skip_on_cran()
  setupInitial <- setupTest(needRequireInNewLib = FALSE)
  skip_if_offline2()

  snf <- snapshotForThisR()
  skip_if(!nzchar(snf) || !file.exists(snf),
          sprintf("no snapshot for R %s.%s in inst/snapshots", R.version$major, R.version$minor))
  pkgs <- data.table::fread(snf)[Package != "R"]   # pkgSnapshot() records the R version as a row
  knownFails <- knownFailsForThisR()
  opts <- options(repos = PEUniverseRepo()); on.exit(options(opts), add = TRUE)

  warns <- capture_warnings(
    out <- Require(packageVersionFile = snf, require = FALSE, returnDetails = TRUE)
  )
  warns <- grep("unable to translate|string.+invalid|TRE pattern compilation error",
                warns, invert = TRUE, value = TRUE)

  ## 1. only the expected kinds of warning
  if (!testWarnsInUsePleaseChange(warns) && length(warns)) {
    knownPats <- paste(c(.txtPleaseRestart, .txtPleaseChangeReqdVers, .txtMsgIsInUse,
                         .txtCouldNotBeInstalled, .txtInstallationNonZeroExit,
                         .txtInstallationPkgFailed), collapse = "|")
    unmatched <- warns[!grepl(knownPats, warns)]
    cat("\n=== test 09: unexpected warnings (", length(unmatched), " of ", length(warns), ") ===\n", sep = "")
    for (w in utils::head(unmatched, 20)) cat("  ", substr(w, 1, 200), "\n", sep = "")
  }
  expect_true(testWarnsInUsePleaseChange(warns))

  ip <- data.table::as.data.table(installed.packages(lib.loc = .libPaths()[1], noCache = TRUE))
  ## packages the test runner itself put in the library (setupTest links them in)
  runnerLibPkgs <- unique(c(
    "pak",
    extractPkgName(pkgDep("testthat", dependencies = TRUE, recursive = TRUE)$testthat),
    extractPkgName(pkgDep("devtools", dependencies = TRUE, recursive = TRUE)$devtools)))

  ## 2. everything requested is there
  expected <- setdiff(pkgs$Package, c(knownFails, .basePkgs))
  missingPackages <- setdiff(expected, ip$Package)
  expect_identical(missingPackages, character(0))

  ## 3. at exactly the requested version
  joined <- ip[pkgs, on = "Package", nomatch = NULL]
  versionProblems <- joined[Version != i.Version][!Package %in% c(runnerLibPkgs, knownFails)]
  if (NROW(versionProblems)) {
    cat("\n=== test 09: version mismatches ===\n")
    print(versionProblems[, .(Package, snapshot = i.Version, installed = Version)])
  }
  expect_true(NROW(versionProblems) == 0)

  ## 4. and nothing that was not requested: a snapshot is a closed set, so any
  ##    extra package means a dependency was pulled at some other version
  extra <- setdiff(ip$Package, c(pkgs$Package, .basePkgs, runnerLibPkgs))
  if (length(extra)) cat("\n=== test 09: unrequested packages installed: ", paste(extra, collapse = ", "), "\n")
  expect_identical(extra, character(0))
})
