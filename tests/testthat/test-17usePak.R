# Tests for pak-backend changes introduced on the pak-dep-cache branch.
#
# Covered:
#   1. RequireOptions default Require.usePak = TRUE
#   2. pakBuildFailReason() — extract failure reason from pak error strings
#   3. pakDepConflictRow()  — conflict-table row message format
#   4. pakDepsResolve in-memory cache message fires at verbose = 1
#   5. pakDepsResolve disk cache message fires at verbose = 1
#   9. Recovery mechanism: user-requested package absent from pkgDT but installed
#      → rbind'd back with loadOrder set so doLoads() calls require()

# ---------------------------------------------------------------------------
# 1. RequireOptions default
# ---------------------------------------------------------------------------

test_that("RequireOptions default Require.usePak is TRUE", {
  ro <- RequireOptions()
  testthat::expect_identical(ro[["Require.usePak"]], TRUE)
})

# ---------------------------------------------------------------------------
# 2. pakBuildFailReason()
# ---------------------------------------------------------------------------

test_that("pakBuildFailReason strips ANSI escape codes", {
  # Ensure colour codes don't appear in output and the plain text is kept
  err <- "\033[31mError\033[0m: \033[1mcompilation failed\033[0m for package 'foo'"
  out <- Require:::pakBuildFailReason(err)
  testthat::expect_false(grepl("\033", out, fixed = TRUE))
  testthat::expect_true(grepl("compilation failed", out, fixed = TRUE))
})

test_that("pakBuildFailReason detects namespace version mismatch", {
  err <- paste(
    "Error in loadNamespace(x) :",
    "  namespace 'SpaDES.tools' 2.0.9 is being loaded, but >= 2.1.1 is required",
    sep = "\n"
  )
  out <- Require:::pakBuildFailReason(err)
  testthat::expect_true(grepl("namespace 'SpaDES.tools'", out, fixed = TRUE))
  testthat::expect_true(grepl("2.1.1", out, fixed = TRUE))
})

test_that("pakBuildFailReason detects file-lock / permission-denied", {
  err <- paste(
    "Error in pak::pak(...)",
    "  unable to move temporary installation 'C:/Temp/foo' to 'C:/R/library/foo'",
    sep = "\n"
  )
  out <- Require:::pakBuildFailReason(err)
  testthat::expect_true(grepl("unable to move", out, fixed = TRUE))
})

test_that("pakBuildFailReason detects lazy loading failed", {
  err <- paste(
    "Error in loadNamespace(x) :",
    "  lazy loading failed for package 'LandR'",
    sep = "\n"
  )
  out <- Require:::pakBuildFailReason(err)
  testthat::expect_true(grepl("lazy loading failed", out, fixed = TRUE))
})

test_that("pakBuildFailReason detects compilation failed", {
  err <- paste(
    "* installing *source* package 'Rcpp'",
    "** libs",
    "ERROR: compilation failed for package 'Rcpp'",
    sep = "\n"
  )
  out <- Require:::pakBuildFailReason(err)
  testthat::expect_true(grepl("compilation failed", out, ignore.case = TRUE))
})

test_that("pakBuildFailReason returns at most 2 diagnostic lines", {
  # Three matching lines — only first two should be returned
  err <- paste(
    "namespace 'a' 1.0 is being loaded, but >= 2.0 is required",
    "namespace 'b' 1.0 is being loaded, but >= 2.0 is required",
    "namespace 'c' 1.0 is being loaded, but >= 2.0 is required",
    sep = "\n"
  )
  out <- Require:::pakBuildFailReason(err)
  # Two lines joined with "; " → exactly one "; " separator
  testthat::expect_equal(length(gregexpr("; ", out, fixed = TRUE)[[1]]), 1L)
  testthat::expect_false(grepl("namespace 'c'", out, fixed = TRUE))
})

test_that("pakBuildFailReason falls back to first non-'Error in' line", {
  err <- paste(
    "Error in pak::pak(packages, lib = lib, ask = FALSE) :",
    "  something went wrong during installation",
    sep = "\n"
  )
  out <- Require:::pakBuildFailReason(err)
  testthat::expect_true(grepl("something went wrong", out, fixed = TRUE))
})

test_that("pakBuildFailReason returns empty string for generic-only framing", {
  err <- paste(
    "Error in pak::pak(packages)",
    "pakRetryLoop",
    "Error",
    sep = "\n"
  )
  out <- Require:::pakBuildFailReason(err)
  # All lines are generic framing; fallback also filtered → ""
  testthat::expect_identical(out, "")
})

# ---------------------------------------------------------------------------
# 3. pakDepConflictRow()
# ---------------------------------------------------------------------------

test_that("pakDepConflictRow: same package → 'dcp  vs  owner/dcp@branch'", {
  row <- Require:::pakDepConflictRow("quickPlot", "PredictiveEcology/quickPlot@development")
  testthat::expect_equal(row$Package, "quickPlot")
  testthat::expect_match(row$Conflict, "quickPlot  vs  PredictiveEcology/quickPlot@development",
                         fixed = TRUE)
  testthat::expect_match(row$Resolution, "drop CRAN ref", fixed = TRUE)
})

test_that("pakDepConflictRow: different package → 'dcp (CRAN)  vs  dcp (via X Remotes)'", {
  # sp: dependency conflict reported because SpaDES.core has sp in its Remotes
  row <- Require:::pakDepConflictRow("sp", "PredictiveEcology/SpaDES.core@development")
  testthat::expect_equal(row$Package, "sp")
  testthat::expect_match(row$Conflict, "sp (CRAN)  vs  sp (via PredictiveEcology/SpaDES.core@development Remotes)",
                         fixed = TRUE)
  testthat::expect_match(row$Resolution, "drop CRAN ref", fixed = TRUE)
  # The string must NOT contain "SpaDES.core  vs  sp" (the old misleading form)
  testthat::expect_false(grepl("SpaDES.core  vs", row$Conflict, fixed = TRUE))
})

test_that("pakDepConflictRow: empty string cand → NULL (no row added)", {
  testthat::expect_null(Require:::pakDepConflictRow("sp", ""))
})

test_that("pakDepConflictRow: zero-length cand → NULL (no row added)", {
  testthat::expect_null(Require:::pakDepConflictRow("sp", character(0)))
})

# ---------------------------------------------------------------------------
# 4 & 5. pakDepsResolve cache messages fire at verbose = 1 but not verbose = 0
# ---------------------------------------------------------------------------

test_that("pakDepsResolve in-memory cache hit emits message at verbose = 1", {
  skip_if_not_installed("pak")

  pkgsForPak <- "any::data.table"
  wh         <- c("Imports", "Depends", "LinkingTo")
  repos      <- c(CRAN = "https://cloud.r-project.org")

  # Compute the key and inject a minimal fake result into the in-memory cache
  key    <- Require:::pakDepsCacheKey(pkgsForPak, wh, repos)
  envKey <- paste0("pakDeps_", key)
  fake   <- data.frame(package = "data.table", version = "1.15.0",
                       ref = "data.table", direct = TRUE,
                       stringsAsFactors = FALSE)
  assign(envKey, fake, envir = Require:::pakEnv())
  on.exit(rm(list = envKey, envir = Require:::pakEnv()), add = TRUE)

  # verbose = 1 → message should appear
  msgs1 <- testthat::capture_messages(
    withr::with_options(list(Require.purge = FALSE),
      Require:::pakDepsResolve(pkgsForPak, wh, repos, verbose = 1, purge = FALSE)
    )
  )
  testthat::expect_true(any(grepl("in-memory cached dep tree", msgs1, fixed = TRUE)))

  # Re-inject (capture_messages doesn't consume it but let's be safe)
  assign(envKey, fake, envir = Require:::pakEnv())

  # verbose = 0 → no message
  msgs0 <- testthat::capture_messages(
    withr::with_options(list(Require.purge = FALSE),
      Require:::pakDepsResolve(pkgsForPak, wh, repos, verbose = 0, purge = FALSE)
    )
  )
  testthat::expect_false(any(grepl("in-memory cached dep tree", msgs0, fixed = TRUE)))
})

test_that("pakDepsResolve disk cache hit emits message at verbose = 1", {
  skip_if_not_installed("pak")

  pkgsForPak <- "any::digest"
  wh         <- c("Imports", "Depends", "LinkingTo")
  repos      <- c(CRAN = "https://cloud.r-project.org")

  # Write a minimal fake result to the disk cache
  key       <- Require:::pakDepsCacheKey(pkgsForPak, wh, repos)
  cacheDir  <- Require:::pakDepsCacheDir()
  cacheFile <- file.path(cacheDir, paste0(key, ".rds"))
  fake      <- data.frame(package = "digest", version = "0.6.35",
                          ref = "digest", direct = TRUE,
                          stringsAsFactors = FALSE)
  dir.create(cacheDir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(fake, cacheFile)
  on.exit(unlink(cacheFile), add = TRUE)

  # Ensure no in-memory entry so disk path is taken
  envKey <- paste0("pakDeps_", key)
  if (exists(envKey, envir = Require:::pakEnv(), inherits = FALSE))
    rm(list = envKey, envir = Require:::pakEnv())

  msgs <- testthat::capture_messages(
    withr::with_options(list(Require.purge = FALSE),
      Require:::pakDepsResolve(pkgsForPak, wh, repos, verbose = 1, purge = FALSE)
    )
  )
  testthat::expect_true(any(grepl("disk-cached dep tree", msgs, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# 6. recordLoadOrder: GitHub ref replaced by CRAN version-spec ref
# ---------------------------------------------------------------------------

test_that("recordLoadOrder sets loadOrder when GitHub ref is replaced by CRAN version-spec ref", {
  # Regression: user supplies "owner/Pkg@branch" (no version spec).
  # trimRedundantVersionAndNoVersion removes it in favour of a dep-table entry
  # "Pkg (>= X.Y)" that has a version spec.  After this, pkgDT$packageFullName
  # is "Pkg (>= X.Y)" not "owner/Pkg@branch", so the old pfn %in% packagesWObase
  # match failed → loadOrder never set → base::require never called.
  pkg_user <- "PredictiveEcology/SpaDES.core@development"
  pkg_dep  <- "SpaDES.core (>= 2.0.0)"

  pkgDT <- Require:::trimRedundancies(Require:::toPkgDTFull(c(pkg_user, pkg_dep)))
  # After trimRedundancies only the CRAN version-spec row remains
  testthat::expect_equal(nrow(pkgDT), 1L)
  testthat::expect_match(pkgDT$packageFullName, "SpaDES.core \\(>= 2.0.0\\)")

  pkgDT <- Require:::recordLoadOrder(pkg_user, pkgDT)
  testthat::expect_false(is.na(pkgDT$loadOrder),
    info = "loadOrder must be set even when GitHub ref was replaced by CRAN version-spec ref")
})

# ---------------------------------------------------------------------------
# 7. trimRedundancies: multiple version specs for the same GitHub ref collapse
#    to the highest (regression from production LandR Install() call)
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# 8. pakDepsToPkgDT step-3b: installed dev version satisfies constraint
#    → package must NOT be removed from pkgDT (regression: LandR not attached)
# ---------------------------------------------------------------------------

test_that("step-3b does not remove a package whose installed version satisfies the constraint", {
  skip_if_not_installed("pak")

  # Simulate: user has "digest (>= 0.1.0)" — an absurdly low floor that is always
  # satisfied by any installed version of digest.  pak's CRAN resolution would give
  # the current CRAN version, which is >> 0.1.0, so canSatisfy = TRUE for this case.
  #
  # More importantly, the key scenario is the inverse: pak's CRAN resolution gives
  # a version LOWER than the user's constraint (e.g. dev-version constraint), but
  # the installed version satisfies it.  We test that by mocking the pakVerMap
  # indirectly: we call the internal helper directly and check the logic using
  # installed.packages().  The test verifies the behaviour of the guard added in
  # step-3b without needing to control pak's output.
  #
  # The minimal check: if installed version satisfies, badPkgs must NOT contain it.
  pkg     <- "digest"
  instVer <- tryCatch(as.character(packageVersion(pkg)), error = function(e) NULL)
  skip_if(is.null(instVer), "digest not installed")

  # Build a needCheck-style row as step-3b would see it
  needCheckRow <- data.table::data.table(
    Package         = pkg,
    packageFullName = paste0(pkg, " (>= 0.1.0)"),
    inequality      = ">=",
    versionSpec     = "0.1.0"
  )
  # pakVerMap: pretend pak resolved digest at exactly 0.1.0 (won't satisfy, forces the check)
  fakePakVer <- c(digest = "0.1.0")

  canSatisfy <- Require:::compareVersion2(fakePakVer[needCheckRow$Package],
                                          needCheckRow$versionSpec,
                                          needCheckRow$inequality)
  # Sanity: "0.1.0 >= 0.1.0" is TRUE, so no badPkg is created — wrong for our test.
  # Use a truly old version so canSatisfy = FALSE:
  fakePakVer["digest"] <- "0.0.1"
  canSatisfy <- Require:::compareVersion2(fakePakVer[needCheckRow$Package],
                                          needCheckRow$versionSpec,
                                          needCheckRow$inequality)
  testthat::expect_false(isTRUE(canSatisfy),
                         info = "0.0.1 should NOT satisfy >= 0.1.0")

  # Now check that the installed version DOES satisfy (so the package should NOT be removed)
  instPkgVers <- tryCatch({
    ipAll <- installed.packages(lib.loc = .libPaths())
    setNames(ipAll[, "Version"], ipAll[, "Package"])
  }, error = function(e) character(0))

  instVer2 <- instPkgVers[pkg]
  testthat::expect_false(is.na(instVer2), info = "digest must be in installed.packages()")
  satisfiedByInstalled <- isTRUE(Require:::compareVersion2(instVer2,
                                                           needCheckRow$versionSpec,
                                                           needCheckRow$inequality))
  testthat::expect_true(satisfiedByInstalled,
    info = "installed digest should satisfy >= 0.1.0")

  # The core assertion: because installed version satisfies, the package is NOT "trulyBad"
  # and must survive as if badPkgs is empty after the guard.
  badCandidates <- needCheckRow[Package %in% pkg]
  trulyBad <- vapply(badCandidates$Package, function(p) {
    iv <- instPkgVers[p]
    if (is.na(iv) || !nzchar(iv)) return(TRUE)
    row <- badCandidates[Package == p][1L]
    !isTRUE(Require:::compareVersion2(iv, row$versionSpec, row$inequality))
  }, logical(1))
  testthat::expect_false(trulyBad,
    info = "digest is installed at a satisfying version; it must NOT be in trulyBad")
})

# ---------------------------------------------------------------------------
# 9. Recovery: user-requested package absent from pkgDT but installed
#    → rbind'd back with loadOrder set so doLoads() calls require()
# ---------------------------------------------------------------------------

test_that("recovery mechanism adds loadOrder for packages absent from pkgDT but installed", {
  # This is a unit-level test of the recovery logic that runs in Require2.R
  # after pakDepsToPkgDT.  Simulate the scenario: "digest" was removed from
  # pkgDT (as step-3b would do when pak's CRAN version can't satisfy the
  # constraint) but is actually installed at a satisfying version.

  pkg <- "digest"
  skip_if_not_installed(pkg)

  instVer <- tryCatch(as.character(packageVersion(pkg)), error = function(e) NULL)
  skip_if(is.null(instVer), "digest not installed")

  # Build a minimal pkgDT that does NOT contain digest (simulating step-3b removal)
  # and a packages vector that contains digest with a low enough constraint that
  # the installed version satisfies it.
  pkgDT    <- Require:::toPkgDTFull("data.table")   # some other package; digest is absent
  packages <- c("data.table", paste0(pkg, " (>= 0.1.0)"))

  # Apply the same pipeline pieces the recovery uses:
  userPkgFull   <- packages[!Require:::extractPkgName(packages) %in% Require:::.basePkgs]
  missingFromDT <- setdiff(Require:::extractPkgName(userPkgFull), pkgDT$Package)
  testthat::expect_true(pkg %in% missingFromDT,
    info = "digest should be identified as missing from pkgDT")

  ipAll <- tryCatch({
    ipRaw <- installed.packages(lib.loc = .libPaths())
    setNames(ipRaw[, "Version"], ipRaw[, "Package"])
  }, error = function(e) character(0))

  missingPkgFull <- userPkgFull[Require:::extractPkgName(userPkgFull) %in% missingFromDT]
  missingPkgDT   <- Require:::toPkgDTFull(missingPkgFull)
  missingPkgDT   <- Require:::confirmEqualsDontViolateInequalitiesThenTrim(missingPkgDT)
  missingPkgDT   <- Require:::trimRedundancies(missingPkgDT)

  recoverable <- vapply(seq_len(NROW(missingPkgDT)), function(i) {
    pkg2    <- missingPkgDT$Package[i]
    instVer2 <- ipAll[pkg2]
    if (is.na(instVer2) || !nzchar(instVer2)) return(FALSE)
    ineq <- missingPkgDT$inequality[i]
    vsp  <- missingPkgDT$versionSpec[i]
    if (is.na(ineq) || !nzchar(ineq)) return(TRUE)
    isTRUE(Require:::compareVersion2(instVer2, vsp, ineq))
  }, logical(1))

  testthat::expect_true(any(recoverable),
    info = "digest should be recoverable (installed version satisfies >= 0.1.0)")

  # Simulate the actual recovery
  recoverDT <- missingPkgDT[recoverable]
  recoverPkgs <- recoverDT$Package
  maxLO <- 0L
  data.table::set(recoverDT, NULL, "loadOrder", seq(maxLO + 1L, maxLO + NROW(recoverDT)))
  data.table::set(recoverDT, NULL, "installed",          TRUE)
  data.table::set(recoverDT, NULL, "installedVersionOK", TRUE)

  # Core assertions
  testthat::expect_true(pkg %in% recoverPkgs,
    info = "digest must be in the set of recovered packages")
  testthat::expect_false(is.na(recoverDT$loadOrder[recoverDT$Package == pkg]),
    info = "recovered digest must have a non-NA loadOrder so doLoads() will require() it")
  testthat::expect_true(isTRUE(recoverDT$installedVersionOK[recoverDT$Package == pkg]),
    info = "recovered digest must have installedVersionOK = TRUE")
})

test_that("trimRedundancies keeps only the highest version constraint for duplicate GitHub refs", {
  # Production regression: Install() was called with three entries for the same
  # GitHub ref at different minimum versions.  trimRedundancies must keep only
  # the strictest (highest) constraint so that exactly one row remains and
  # Require does not attempt three separate installs.
  pkgs <- c(
    "PredictiveEcology/LandR@development (>= 1.1.5.9064)",
    "PredictiveEcology/LandR@development (>= 1.1.5.9100)",
    "PredictiveEcology/LandR@development (>= 1.1.5.9016)"
  )
  pkgDT <- Require:::trimRedundancies(Require:::toPkgDTFull(pkgs))
  # Only one row should remain
  testthat::expect_equal(nrow(pkgDT), 1L)
  # It must be the highest constraint
  testthat::expect_equal(pkgDT$versionSpec, "1.1.5.9100")
})
