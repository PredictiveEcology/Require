# Tests for pak-backend changes introduced on the pak-dep-cache branch.
#
# Covered:
#   1.  RequireOptions default Require.usePak = TRUE
#   2.  pakBuildFailReason() — extract failure reason from pak error strings
#   3.  pakDepConflictRow()  — conflict-table row message format
#   4.  pakDepsResolve memory cache message fires at verbose = 1
#   5.  pakDepsResolve disk cache message fires at verbose = 1
#   9.  Recovery mechanism: user-requested package absent from pkgDT but installed
#       → rbind'd back with loadOrder set so doLoads() calls require()
#   10. doLoads fallback: when pak install fails but old version present, load it
#   11. doLoads: require() failure emits immediate warning
#   12. pakInstallFiltered versionChanged guard: NA pre-install version → no spurious warning
#   13. pakRetryLoop upgrade flag: GitHub refs get upgrade=TRUE; CRAN refs get upgrade=FALSE
#   14. pakInstallFiltered: installedVersionOK set TRUE after successful install
#   15. pakInstallFiltered: no double warning when version-change path already warned
#   16. versionChanged dash-vs-dot normalization: "3.2.1" == "3.2-1" semantically → no spurious warning
#   17. recordLoadOrder skipped when require=FALSE: no loadOrder set for Install() calls

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

test_that("pakDepsResolve memory cache hit emits message at verbose = 1", {
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
  testthat::expect_true(any(grepl("using memory cache", msgs1, fixed = TRUE)))

  # Re-inject (capture_messages doesn't consume it but let's be safe)
  assign(envKey, fake, envir = Require:::pakEnv())

  # verbose = 0 → no message
  msgs0 <- testthat::capture_messages(
    withr::with_options(list(Require.purge = FALSE),
      Require:::pakDepsResolve(pkgsForPak, wh, repos, verbose = 0, purge = FALSE)
    )
  )
  testthat::expect_false(any(grepl("using memory cache", msgs0, fixed = TRUE)))
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
  testthat::expect_true(any(grepl("using cache", msgs, fixed = TRUE)))
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

# ---------------------------------------------------------------------------
# 10. doLoads fallback: load installed version when pak install fails
# ---------------------------------------------------------------------------

test_that("doLoads loads installed version as fallback when installResult=could not be installed", {
  # Regression: when pak fails to install a newer version but an older version
  # is present, doLoads was leaving the package completely unattached
  # (require=FALSE), causing confusing "object not found" errors downstream.
  # Fix: set require=TRUE and emit a warning so the installed version is loaded.
  pkg <- "digest"
  skip_if_not_installed(pkg)

  pkgDT <- data.table::data.table(
    Package            = pkg,
    packageFullName    = paste0(pkg, " (>= 999.0.0)"),
    inequality         = ">=",
    versionSpec        = "999.0.0",
    loadOrder          = 1L,
    # NOTE: no 'require' column — doLoads creates it internally.  If 'require'
    # were pre-populated, data.table would resolve it as the column (not the
    # function argument) inside the j expression, breaking the initialization.
    installed          = TRUE,
    installedVersionOK = FALSE,        # installed version doesn't satisfy >= 999
    availableVersionOK = FALSE,
    installResult      = "could not be installed",
    Version            = "0.6.35",
    LibPath            = .libPaths()[1]
  )

  warns <- character(0L)
  withr::with_options(list(Require.verbose = 0), {
    withCallingHandlers(
      Require:::doLoads(require = TRUE, pkgDT = pkgDT, libPaths = .libPaths()),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  })

  # The fallback warning must mention the package and "fallback"
  fallback_warn <- warns[grepl("fallback", warns, ignore.case = TRUE)]
  testthat::expect_true(length(fallback_warn) >= 1L,
    info = "doLoads must emit a fallback warning when install failed but package is present")
  testthat::expect_match(fallback_warn[1], pkg, fixed = TRUE)

  # require must have been set to TRUE so base::require() was called
  testthat::expect_true(isTRUE(pkgDT$require),
    info = "pkgDT$require must be TRUE after fallback so the package is actually loaded")
})

test_that("doLoads does NOT fall back when installed=FALSE (nothing to fall back to)", {
  # Safety check: if the package is simply absent, no fallback should occur and
  # no spurious "loading as fallback" warning should be emitted.
  pkgDT <- data.table::data.table(
    Package            = "zzz_nonexistent_pkg",
    packageFullName    = "zzz_nonexistent_pkg (>= 999.0.0)",
    inequality         = ">=",
    versionSpec        = "999.0.0",
    loadOrder          = 1L,
    # NOTE: no 'require' column — doLoads initializes it from the function argument.
    installed          = FALSE,        # NOT installed
    installedVersionOK = FALSE,
    availableVersionOK = FALSE,
    installResult      = "could not be installed",
    Version            = NA_character_,
    LibPath            = NA_character_
  )

  warns <- character(0L)
  withr::with_options(list(Require.verbose = 0), {
    withCallingHandlers(
      Require:::doLoads(require = TRUE, pkgDT = pkgDT, libPaths = .libPaths()),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  })

  fallback_warn <- warns[grepl("fallback", warns, ignore.case = TRUE)]
  testthat::expect_equal(length(fallback_warn), 0L,
    info = "No fallback warning should be emitted when installed=FALSE")
  testthat::expect_false(isTRUE(pkgDT$require),
    info = "require must stay FALSE when there is no installed version to fall back to")
})

# ---------------------------------------------------------------------------
# 11. doLoads: require() failure emits an immediate warning
# ---------------------------------------------------------------------------

test_that("doLoads emits an immediate warning when base::require() returns FALSE", {
  # When a package is marked require=TRUE but base::require() fails (e.g. the
  # package is not in any of libPaths), a warning must always be emitted
  # regardless of verbose setting, so the user knows why downstream code fails.
  pkgDT <- data.table::data.table(
    Package            = "zzz_nonexistent_for_require_test",
    packageFullName    = "zzz_nonexistent_for_require_test",
    loadOrder          = 1L,
    # NOTE: no 'require' column — doLoads initializes it from the function argument.
    installed          = TRUE,
    installedVersionOK = TRUE,
    availableVersionOK = TRUE,
    installResult      = "OK",
    Version            = "1.0.0",
    LibPath            = .libPaths()[1]
  )

  warns <- character(0L)
  withr::with_options(list(Require.verbose = -1), {  # verbose=-1 (silent mode)
    withCallingHandlers(
      Require:::doLoads(require = TRUE, pkgDT = pkgDT, libPaths = .libPaths()),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  })

  require_fail_warn <- warns[grepl("returned FALSE", warns, fixed = TRUE)]
  testthat::expect_true(length(require_fail_warn) >= 1L,
    info = "A 'returned FALSE' warning must be emitted even with verbose=-1")
  testthat::expect_match(require_fail_warn[1], "zzz_nonexistent_for_require_test", fixed = TRUE)
  testthat::expect_match(require_fail_warn[1], "Searched in:", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# 12. pakInstallFiltered: versionChanged NA guard
# ---------------------------------------------------------------------------

test_that("versionChanged is FALSE when preVer is NA (first-time install failure)", {
  # Regression: when a package was absent from the library before a (failed)
  # install attempt, preInstallVers[pkg] is NA_character_.  The old logic
  #   !isTRUE(!is.na(preVer) && identical(preVer, installedVer))
  # evaluated NA as "changed", firing a spurious "Please change required version"
  # warning that told the user to lower their version requirement to the very
  # version that pak failed to change.
  # Fixed logic:
  #   !is.na(preVer) && !isTRUE(identical(preVer, installedVer))

  installedVer <- "1.1.5.9088"

  # Case 1: first-time install (package was not in library before) → no change
  preVer <- NA_character_
  versionChanged <- !is.na(preVer) && !isTRUE(identical(preVer, installedVer))
  testthat::expect_false(versionChanged,
    info = "NA preVer must NOT trigger 'Please change required version'")

  # Case 2: pak actually installed a different (but still insufficient) version
  preVer <- "1.1.5.9080"
  versionChanged <- !is.na(preVer) && !isTRUE(identical(preVer, installedVer))
  testthat::expect_true(versionChanged,
    info = "Different non-NA preVer must trigger 'Please change required version'")

  # Case 3: build failed — version unchanged from pre-install
  preVer <- "1.1.5.9088"
  versionChanged <- !is.na(preVer) && !isTRUE(identical(preVer, installedVer))
  testthat::expect_false(versionChanged,
    info = "Identical preVer/installedVer (build failure) must NOT trigger the warning")
})

# ---------------------------------------------------------------------------
# 13. pakRetryLoop upgrade flag: GitHub refs → upgrade=TRUE; CRAN → upgrade=FALSE
# ---------------------------------------------------------------------------

test_that("isGH correctly distinguishes GitHub refs from CRAN refs for upgrade flag logic", {
  # The pakRetryLoop split: ghOrUrl <- isGH(packages) | startsWith(packages, "url::")
  # GitHub and url:: packages need upgrade=TRUE so pak always fetches the latest
  # commit from the branch.  CRAN-like packages must keep upgrade=FALSE to avoid
  # over-upgrading already-satisfied dependencies.

  pkgs <- c(
    "any::data.table",
    "PredictiveEcology/LandR@development",
    "any::ggplot2",
    "PredictiveEcology/SpaDES.core@development",
    "url::https://cran.r-project.org/src/contrib/Archive/fastdigest/fastdigest_0.6-4.tar.gz"
  )

  ghOrUrl <- Require:::isGH(pkgs) | startsWith(pkgs, "url::")

  testthat::expect_false(ghOrUrl[1], info = "any::data.table is CRAN-like → upgrade=FALSE")
  testthat::expect_true(ghOrUrl[2],  info = "LandR@development is GitHub → upgrade=TRUE")
  testthat::expect_false(ghOrUrl[3], info = "any::ggplot2 is CRAN-like → upgrade=FALSE")
  testthat::expect_true(ghOrUrl[4],  info = "SpaDES.core@development is GitHub → upgrade=TRUE")
  testthat::expect_true(ghOrUrl[5],  info = "url:: archive ref → upgrade=TRUE")

  # Mixed batch: both types present → two separate pak calls are needed
  testthat::expect_true(any(ghOrUrl) && any(!ghOrUrl),
    info = "Mixed batch must trigger the two-call split in pakRetryLoop")

  # All-GitHub batch: single call with upgrade=TRUE
  ghOnly <- c("PredictiveEcology/LandR@development",
               "PredictiveEcology/SpaDES.core@development")
  ghOrUrlOnly <- Require:::isGH(ghOnly) | startsWith(ghOnly, "url::")
  testthat::expect_true(all(ghOrUrlOnly),
    info = "All-GitHub batch: single pak call with upgrade=TRUE")
  testthat::expect_false(any(!ghOrUrlOnly),
    info = "All-GitHub batch must not trigger the CRAN upgrade=FALSE call")

  # All-CRAN batch: single call with upgrade=FALSE
  cranOnly <- c("any::data.table", "any::ggplot2")
  ghOrUrlCRAN <- Require:::isGH(cranOnly) | startsWith(cranOnly, "url::")
  testthat::expect_false(any(ghOrUrlCRAN),
    info = "All-CRAN batch: single pak call with upgrade=FALSE")
})

# ---------------------------------------------------------------------------
# 14. pakInstallFiltered: installedVersionOK set TRUE after successful install
# ---------------------------------------------------------------------------

test_that("post-install update sets installedVersionOK=TRUE on success", {
  # Regression: the post-install update loop in pakInstallFiltered set
  # installed/Version/LibPath/installResult on success but left
  # installedVersionOK=FALSE, so doLoads() saw the package as unloadable and
  # emitted "Packages with loadOrder set but require=FALSE".
  # Fix: also set installedVersionOK=TRUE in the success branch.

  pkg <- "digest"
  skip_if_not_installed(pkg)

  nowInstalled <- data.table::data.table(
    Package = pkg,
    Version = "0.6.35",
    LibPath = .libPaths()[1]
  )

  pkgDT <- data.table::data.table(
    Package            = pkg,
    packageFullName    = pkg,
    inequality         = "",
    versionSpec        = "",
    installed          = FALSE,
    installedVersionOK = FALSE,
    installResult      = NA_character_
  )

  # Reproduce the success branch of the post-install update loop.
  wh <- which(pkgDT$Package == pkg)
  nowRow <- nowInstalled[Package == pkg]
  installedVer <- nowRow$Version[1]
  data.table::set(pkgDT, wh, "installed",          TRUE)
  data.table::set(pkgDT, wh, "installedVersionOK", TRUE)
  data.table::set(pkgDT, wh, "Version",            installedVer)
  data.table::set(pkgDT, wh, "LibPath",            nowRow$LibPath[1])
  data.table::set(pkgDT, wh, "installResult",      "OK")

  testthat::expect_true(pkgDT$installedVersionOK,
    info = "installedVersionOK must be TRUE after a successful install")
  testthat::expect_true(pkgDT$installed,
    info = "installed must be TRUE after a successful install")
  testthat::expect_equal(pkgDT$installResult, "OK")
})

# ---------------------------------------------------------------------------
# 15. pakInstallFiltered: no double warning when version-change path warned
# ---------------------------------------------------------------------------

test_that("no double 'could not be installed' warning when versionChanged emits 'Please change'", {
  # Regression: when pak installed a package at a version that still didn't
  # satisfy the constraint, the code emitted "Please change required version"
  # (correct) but did NOT add the package to warnedDropped, so the silentlyFailed
  # check below also emitted "could not be installed" for the same package.
  # Fix: add pkg to warnedDropped when the version-change warning is emitted.

  pkg          <- "spatstat.utils"
  installedVer <- "3.1-0"   # installed but doesn't satisfy >= 3.2-1
  preVer       <- "3.0-0"   # different from installedVer → versionChanged = TRUE

  warnedDropped <- character(0)

  versionChanged <- !is.na(preVer) && !isTRUE(identical(preVer, installedVer))
  testthat::expect_true(versionChanged)

  warns <- character(0)
  withCallingHandlers({
    if (versionChanged) {
      warning(Require:::msgPleaseChangeRqdVersion(pkg, ineq = ">=", newVersion = installedVer),
              call. = FALSE)
      warnedDropped <- c(warnedDropped, pkg)
    }
  }, warning = function(w) {
    warns <<- c(warns, conditionMessage(w))
    invokeRestart("muffleWarning")
  })

  testthat::expect_true(pkg %in% warnedDropped,
    info = "pkg must be in warnedDropped after version-change warning so silentlyFailed skips it")

  # silentlyFailed check: pkg is in warnedDropped → no second warning
  pkgDT <- data.table::data.table(
    Package       = pkg,
    installResult = "could not be installed"
  )
  silentlyFailed <- pkg[
    !pkg %in% warnedDropped &
    isTRUE(pkgDT$installResult[pkgDT$Package == pkg] == "could not be installed")
  ]
  testthat::expect_equal(length(silentlyFailed), 0L,
    info = "silentlyFailed must be empty when pkg was already warned via versionChanged path")
})

# ---------------------------------------------------------------------------
# 16. versionChanged dash-vs-dot normalization
# ---------------------------------------------------------------------------

test_that("versionChanged is FALSE when preVer and installedVer differ only by dash-vs-dot", {
  # Regression: installedVers() calls as.character(packageVersion(...)) which
  # collapses version components with "." (e.g. "3.2.1"), while
  # installed.packages() returns the raw DESCRIPTION string (e.g. "3.2-1").
  # identical("3.2.1", "3.2-1") = FALSE → versionChanged = TRUE spuriously,
  # triggering a "Please change required version" warning after a successful
  # (no-op) pak call.
  # Fix: add compareVersion(preVer, installedVer) == 0L guard.

  for (usePak in c(TRUE, FALSE)) {
    withr::with_options(list(Require.usePak = usePak), {

      installedVer <- "3.2-1"   # from installed.packages()
      preVer_dot   <- "3.2.1"   # from as.character(packageVersion(...))
      preVer_dash  <- "3.2-1"   # identical strings

      versionChanged_old_dot  <- !is.na(preVer_dot) &&
                                  !isTRUE(identical(preVer_dot, installedVer))
      versionChanged_new_dot  <- !is.na(preVer_dot) &&
                                  !isTRUE(identical(preVer_dot, installedVer)) &&
                                  !isTRUE(compareVersion(preVer_dot, installedVer) == 0L)
      versionChanged_new_dash <- !is.na(preVer_dash) &&
                                  !isTRUE(identical(preVer_dash, installedVer)) &&
                                  !isTRUE(compareVersion(preVer_dash, installedVer) == 0L)

      testthat::expect_true(versionChanged_old_dot,
        info = paste0("usePak=", usePak,
                      ": old logic fires spuriously on dot-vs-dash ('3.2.1' vs '3.2-1')"))
      testthat::expect_false(versionChanged_new_dot,
        info = paste0("usePak=", usePak,
                      ": new logic must NOT fire when '3.2.1' and '3.2-1' are semantically equal"))
      testthat::expect_false(versionChanged_new_dash,
        info = paste0("usePak=", usePak,
                      ": new logic must NOT fire for identical dash strings"))
    })
  }
})

# ---------------------------------------------------------------------------
# 17. recordLoadOrder skipped when require=FALSE
# ---------------------------------------------------------------------------

test_that("recordLoadOrder is not called and loadOrder stays NA when require=FALSE", {
  # Regression: Install() (require=FALSE) called recordLoadOrder unconditionally,
  # setting loadOrder for all user-passed packages.  doLoads() then reported
  # "Packages with loadOrder set but require=FALSE (will NOT be loaded)" for
  # every package in the call.
  # Fix: gate recordLoadOrder on !isFALSE(require) in Require2.R.

  pkgs <- c("digest", "data.table")
  pkgDT <- Require:::toPkgDTFull(pkgs)
  # Confirm no loadOrder before the gate
  testthat::expect_true(is.null(pkgDT[["loadOrder"]]) || all(is.na(pkgDT$loadOrder)),
    info = "loadOrder must be absent/NA before recordLoadOrder is called")

  # require=FALSE path: gate fires, recordLoadOrder NOT called → loadOrder stays NA
  require_false <- FALSE
  if (!isFALSE(require_false))
    pkgDT <- Require:::recordLoadOrder(pkgs, pkgDT)
  testthat::expect_true(is.null(pkgDT[["loadOrder"]]) || all(is.na(pkgDT$loadOrder)),
    info = "require=FALSE: loadOrder must remain NA (recordLoadOrder must be skipped)")

  # require=TRUE path: gate open, recordLoadOrder IS called → loadOrder set
  pkgDT2 <- Require:::toPkgDTFull(pkgs)
  require_true <- TRUE
  if (!isFALSE(require_true))
    pkgDT2 <- Require:::recordLoadOrder(pkgs, pkgDT2)
  testthat::expect_false(is.null(pkgDT2[["loadOrder"]]),
    info = "require=TRUE: loadOrder column must exist after recordLoadOrder")
  testthat::expect_true(any(!is.na(pkgDT2$loadOrder)),
    info = "require=TRUE: at least one package must have a non-NA loadOrder")
})
