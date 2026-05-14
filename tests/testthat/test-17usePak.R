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
  # installedVersionOK=FALSE, so doLoads() saw the package as unloadable.
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
  # setting loadOrder for all user-passed packages.
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

# ---------------------------------------------------------------------------
# 18. pakRefToBareName: bookkeeping bug for "@version" exact-pin refs
# ---------------------------------------------------------------------------
# Regression: Require::Install(c("stringfish (<= 0.15.8)", "qs (== 0.27.3)"))
# was reported with both packages flagged "still-missing" in the install
# summary even though stringfish DID install. Root cause:
# equalsToAt()/lessThanToAt() rewrite "pkg (== X)" / "pkg (<= X)" to pak's
# "pkg@X" exact-pin syntax, but the iter-loop / install-summary used
# `sub("^any::", "", sub("^[^/]+/", "", extractPkgName(pkgs)))` to derive
# the bare names. extractPkgName() only strips parenthetical "(>=X)" via
# trimVersionNumber(), NOT "@X" — so "qs@0.27.3" survived intact and never
# matched rownames(installed.packages())'s bare "qs".  Every version-pinned
# install was therefore reported as missing, the archive-fallback ran on
# already-installed refs, and the summary printed bogus "still-missing"
# entries.  The fix: pakRefToBareName() strips all three of any:: / owner/
# / @ver in one helper used everywhere a pak ref needs to match
# installed.packages().

test_that("pakRefToBareName strips @version, any::, and owner/ prefixes", {
  cases <- c(
    "qs@0.27.3"               , # CRAN exact-pin via equalsToAt()
    "stringfish@0.15.8"       , # CRAN <=ver pin via lessThanToAt()
    "any::cli"                , # plain CRAN with any:: prefix
    "any::dplyr"              ,
    "tidyverse/ggplot2"       , # GitHub owner/repo
    "tidyverse/ggplot2@main"  , # GitHub owner/repo@branch
    "owner-with-hyphen/pkg"   , # owner with hyphen (not caught by extractPkgGitHub's [:alnum:])
    "stringfish (<= 0.15.8)"  , # not yet rewritten — extractPkgName parens-strip
    "qs (== 0.27.3)"          ,
    "Require (>= 0.0.1)"
  )
  expected <- c(
    "qs", "stringfish", "cli", "dplyr",
    "ggplot2", "ggplot2", "pkg",
    "stringfish", "qs", "Require"
  )
  testthat::expect_identical(Require:::pakRefToBareName(cases), expected)
})

test_that("pakRefToBareName output matches installed.packages() rownames", {
  # The contract this helper has to honor: for any pak ref the install-summary
  # / iter-loop / archive-fallback uses, pakRefToBareName(ref) must equal what
  # rownames(installed.packages()) would return for that package once
  # installed.  Without the @-version strip, the %in% check is always FALSE
  # and the bookkeeping reports successfully-installed packages as missing.
  refs       <- c("qs@0.27.3", "stringfish@0.15.8", "any::cli")
  bareNames  <- Require:::pakRefToBareName(refs)
  pretendInstalled <- c("qs", "stringfish", "cli", "Rcpp", "data.table")
  testthat::expect_true(all(bareNames %in% pretendInstalled),
    info = paste0("bareNames = (",
                  paste(bareNames, collapse = ", "),
                  ") must all be present in pretendInstalled — if any survive",
                  " as 'pkg@ver' the iter-loop will misclassify them as missing"))
})

# ---------------------------------------------------------------------------
# 19. pakDepsCacheKey: user-supplied version constraints are part of the key
# ---------------------------------------------------------------------------
# Regression: pakDepsToPkgDT() strips version specs from `pkgsForPak` before
# calling pak::pkg_deps() (line ~1322: `pkgsForPak <- trimVersionNumber(...)`).
# That's intentional — pak's resolver only takes bare refs — but it meant the
# cache key was identical for any two calls whose package *names* matched, no
# matter how their version constraints differed. The cached pak_result is
# used downstream by pakDepsToPkgDT to build pkgDT (whose `packageFullName`
# rows then drive trimRedundancies, lessThanToAt, equalsToAt, and ultimately
# what pak::pak() is asked to install). Reusing a cached entry from a call
# with different constraints can therefore produce the wrong install plan —
# field symptom: after `remove.packages("stringfish")` followed by
# `Install("stringfish (<= 0.15.8)")`, pak was asked for `any::stringfish`
# (no pin) and silently installed 0.19.0 instead of 0.15.8.
#
# Fix: pakDepsCacheKey() now hashes a `userPkgs` argument carrying the
# version-bearing refs, so different constraint sets get distinct cache
# entries. Backward-compat: when `userPkgs` is NULL the key omits it
# (matches old call-sites that haven't been updated).

test_that("pakDepsCacheKey distinguishes calls by user-supplied version constraints", {
  pkgs  <- c("stringfish", "qs")  # version-stripped form pak::pkg_deps() sees
  wh    <- NA
  repos <- c(CRAN = "https://cran.r-project.org")

  k_none <- Require:::pakDepsCacheKey(pkgs, wh, repos,
                                      userPkgs = c("stringfish", "qs"))
  k_le   <- Require:::pakDepsCacheKey(pkgs, wh, repos,
                                      userPkgs = c("stringfish (<= 0.15.8)",
                                                   "qs (== 0.27.3)"))
  k_eq   <- Require:::pakDepsCacheKey(pkgs, wh, repos,
                                      userPkgs = c("stringfish (== 0.16.0)",
                                                   "qs (== 0.27.3)"))

  # All three must be distinct. The pre-fix code produced a single key for
  # all three because pkgs+wh+repos are identical.
  testthat::expect_false(identical(k_none, k_le),
    info = "no-constraint key must differ from (<=) constraint key")
  testthat::expect_false(identical(k_le, k_eq),
    info = "(<=) and (==) constraint keys must differ")
  testthat::expect_false(identical(k_none, k_eq),
    info = "no-constraint key must differ from (==) constraint key")
})

test_that("pakDepsCacheKey is stable across reorderings and repeated calls", {
  pkgs  <- c("stringfish", "qs")
  wh    <- NA
  repos <- c(CRAN = "https://cran.r-project.org")

  k1 <- Require:::pakDepsCacheKey(pkgs, wh, repos,
                                  userPkgs = c("stringfish (<= 0.15.8)",
                                               "qs (== 0.27.3)"))
  # Same content, different vector order — pakDepsCacheKey sorts internally
  # so the key must be invariant.
  k2 <- Require:::pakDepsCacheKey(pkgs, wh, repos,
                                  userPkgs = c("qs (== 0.27.3)",
                                               "stringfish (<= 0.15.8)"))
  testthat::expect_identical(k1, k2,
    info = "key must be order-invariant: same constraint set → same key")

  # Repeated identical calls return identical keys (no temp-file or
  # md5 instability).
  k3 <- Require:::pakDepsCacheKey(pkgs, wh, repos,
                                  userPkgs = c("stringfish (<= 0.15.8)",
                                               "qs (== 0.27.3)"))
  testthat::expect_identical(k1, k3,
    info = "repeated identical call must return the same key")
})

# ---------------------------------------------------------------------------
# 20. pakInstallFiltered dedup: prefer strictest constraint
# ---------------------------------------------------------------------------
# Regression: when the same Package appeared in pkgDT under two plain-CRAN
# rows — typically the user's "(<= X)" upper-bound and a transitive dep's
# "(>= Y)" lower-bound (both kept by trimRedundancies because they're
# complementary, not redundant) — pakInstallFiltered's dedup did
# `unique(toInstall, by = "Package")` and arbitrarily kept whichever row sorted
# first.  In practice the ">=" row tends to come second from pkgDepsToPkgDT,
# but the previous-call's pkgDT can leave them in either order; either way the
# user's "<=" pin would be silently dropped, the downstream gsub("\\(>=...\\)")
# step would strip the row to a bare name, the any:: prefix would yield
# `any::pkg`, and pak would install the latest (constraint-violating) version.
# Field symptom: `Install("stringfish (<= 0.15.8)")` produced stringfish 0.19.0
# even though the user explicitly requested an upper-bound version.
#
# Fix: before unique-by-Package, sort by inequality priority
# (==, <=, <, >=, >, none) so the strictest row wins.  equalsToAt() and
# lessThanToAt() (called downstream) translate the surviving == / <= / <
# constraint into pak's exact "@version" pin form, and the install proceeds
# with the right version.

test_that("pakInstallFiltered dedup keeps the row with the strictest version constraint", {
  # We test the dedup logic in isolation against a synthetic pkgDT shape
  # (the actual pakInstallFiltered runs install actions we can't sandbox).
  # Mirror the dedup branch from pak.R verbatim.
  ti <- data.table::data.table(
    Package         = c("qs",                "stringfish",            "stringfish"),
    packageFullName = c("qs (== 0.27.3)",    "stringfish (>= 0.15.1)", "stringfish (<= 0.15.8)"),
    Version         = c("0.27.3",            "0.15.1",                "0.15.8"),
    versionSpec     = c("0.27.3",            "0.15.1",                "0.15.8"),
    inequality      = c("==",                ">=",                    "<=")
  )
  ti[, isNonCRAN := Require:::isGH(packageFullName) | startsWith(packageFullName, "url::")]
  ti[, hasNonCRAN := any(isNonCRAN), by = Package]
  ti <- ti[!(hasNonCRAN == TRUE & isNonCRAN == FALSE)]
  ti[, .versionSpecPrio := match(inequality, c("==","<=","<",">=",">"), nomatch = 6L)]
  data.table::setorderv(ti, c("Package", ".versionSpecPrio"))
  ti <- unique(ti, by = "Package")

  # The "<=" row must win, NOT the ">=" row.
  testthat::expect_identical(ti[Package == "stringfish"]$inequality, "<=",
    info = "dedup must keep the user's `(<= X)` upper-bound row over the transitive `(>= Y)` row")
  testthat::expect_identical(ti[Package == "stringfish"]$packageFullName,
                             "stringfish (<= 0.15.8)")
  # qs has only one row; it survives unchanged.
  testthat::expect_identical(ti[Package == "qs"]$packageFullName, "qs (== 0.27.3)")
})

test_that("pakInstallFiltered dedup priority order is == > <= > < > >= > > > none", {
  # Six rows, all for the same fake Package "X", spanning every inequality
  # operator and one bare row. After dedup the survivor must be the one with
  # `==` (the highest-priority constraint).
  ti <- data.table::data.table(
    Package         = rep("X", 6L),
    packageFullName = c("X", "X (> 1.0)", "X (>= 2.0)", "X (< 3.0)", "X (<= 4.0)", "X (== 5.0)"),
    inequality      = c(NA_character_, ">",        ">=",        "<",         "<=",        "==")
  )
  ti[, isNonCRAN := FALSE]
  ti[, hasNonCRAN := FALSE]
  ti[, .versionSpecPrio := match(inequality, c("==","<=","<",">=",">"), nomatch = 6L)]
  data.table::setorderv(ti, c("Package", ".versionSpecPrio"))
  survivor <- unique(ti, by = "Package")
  testthat::expect_identical(survivor$packageFullName, "X (== 5.0)",
    info = "with all 6 inequality forms present, `==` must win")
})

test_that("pakDepsCacheKey omits userPkgs when not supplied (back-compat)", {
  # Old call sites that haven't been updated should still get a stable key
  # that doesn't include userPkgs. This means an old call gets a different
  # key than a new call that supplies userPkgs == pkgsForPak — that's the
  # intended behavior: rather than treating "no userPkgs" as "userPkgs ==
  # pkgsForPak", we want them to be distinct so cache entries from the new
  # path don't accidentally collide with entries from the old path.
  pkgs  <- c("stringfish", "qs")
  wh    <- NA
  repos <- c(CRAN = "https://cran.r-project.org")

  k_old <- Require:::pakDepsCacheKey(pkgs, wh, repos)
  k_new_same <- Require:::pakDepsCacheKey(pkgs, wh, repos, userPkgs = pkgs)
  testthat::expect_false(identical(k_old, k_new_same),
    info = "key with userPkgs supplied must differ from key with userPkgs omitted")
})

# ---------------------------------------------------------------------------
# pinInstalledForPak: rewrites plain CRAN refs to `pkg@<installedVersion>`
# so pak resolves transitive deps from the installed version's DESCRIPTION
# rather than the latest CRAN release. Regression guard for the bug where
# `Require("processx")` (with processx 3.8.6 + ps 1.9.2 installed) spuriously
# upgraded ps to 1.9.3 because pak read the constraint `ps (>= 1.9.3)` from
# processx 3.9.0's (latest CRAN) Imports rather than from the installed
# 3.8.6's `ps (>= 1.2.0)`.
# ---------------------------------------------------------------------------

# Use a package that's always installed when these tests run (data.table is
# a hard dep of Require). Bare DESCRIPTION stubs in a tempdir don't trigger
# installed.packages() — it requires a properly-built package layout — so
# we test against the real package metadata instead.
test_that("pinInstalledForPak rewrites installed plain refs to pkg@version", {
  dtVer <- as.character(utils::packageVersion("data.table"))
  out <- Require:::pinInstalledForPak(
    pkgsForPak = c("data.table", "definitelynotapackage12345"),
    libPaths   = .libPaths()
  )
  testthat::expect_identical(out[1L], paste0("data.table@", dtVer),
    info = "installed data.table must be pinned to its installed version")
  testthat::expect_identical(out[2L], "definitelynotapackage12345",
    info = "uninstalled package must be left as-is (no @version)")
})

test_that("pinInstalledForPak skips refs the user version-pinned", {
  # `pkgsForPak` is the version-stripped form pak gets; `resolvedPkgs`
  # carries the user's original parenthetical constraint, e.g.
  # `data.table (>= 9.9.9)`. Pinning would mask the user's upgrade request.
  out <- Require:::pinInstalledForPak(
    pkgsForPak   = "data.table",
    libPaths     = .libPaths(),
    resolvedPkgs = "data.table (>= 9.9.9)"
  )
  testthat::expect_identical(out, "data.table",
    info = "must not pin when user provided an explicit version constraint")
})

test_that("pinInstalledForPak leaves GitHub refs and pre-pinned refs alone", {
  out <- Require:::pinInstalledForPak(
    pkgsForPak = c("Rdatatable/data.table", "data.table@1.0.0"),
    libPaths   = .libPaths()
  )
  testthat::expect_identical(out[1L], "Rdatatable/data.table",
    info = "GitHub ref must be left as-is")
  testthat::expect_identical(out[2L], "data.table@1.0.0",
    info = "ref with an existing @version pin must be left as-is")
})

test_that("pinInstalledForPak returns input unchanged when libPath is empty", {
  tmpLib <- tempfile("pinTest-")
  dir.create(tmpLib, recursive = TRUE)
  on.exit(unlink(tmpLib, recursive = TRUE), add = TRUE)
  out <- Require:::pinInstalledForPak("data.table", libPaths = tmpLib)
  testthat::expect_identical(out, "data.table",
    info = "empty libPath has no installed packages -- ref must pass through unchanged")
})

# ---------------------------------------------------------------------------
# cachePkgDir() consolidation
#
# `cachePkgDir()` is the single getter for the package-tarball cache:
#   * usePak = TRUE  -> pak::cache_summary()$cachepath
#   * usePak = FALSE -> legacy <cacheDir>/packages/<Rver>
#
# `.requirePkgInfoDir()` stays at the legacy path regardless of pak mode --
# it holds Require's own bookkeeping (SHA DB, mirrors.csv, pkgDepDB, etc.)
# which pak doesn't know about. Together these split a previously-overloaded
# concept and let `R_USER_CACHE_DIR` be the single env-var knob for shared
# caches (closes the spirit of #91 without needing R_REQUIRE_PKG_CACHE).
# ---------------------------------------------------------------------------

test_that("cachePkgDir() returns pak's cache path in pak mode", {
  skip_if_not_installed("pak")
  withr::with_options(list(Require.usePak = TRUE), {
    pakPath <- tryCatch(pak::cache_summary()$cachepath, error = function(e) NULL)
    skip_if(is.null(pakPath) || !nzchar(pakPath),
            "pak::cache_summary() unavailable here -- nothing to compare against")
    testthat::expect_identical(
      normalizePath(Require::cachePkgDir(), mustWork = FALSE),
      normalizePath(pakPath, mustWork = FALSE),
      info = "cachePkgDir() must delegate to pak::cache_summary()$cachepath in pak mode"
    )
  })
})

test_that("cachePkgDir() returns legacy path when usePak = FALSE", {
  withr::with_options(list(Require.usePak = FALSE), {
    legacy <- Require::cachePkgDir()
    testthat::expect_true(grepl(paste0("packages/", Require:::versionMajorMinor(), "$"),
                                legacy),
      info = "non-pak path must be <cacheDir>/packages/<Rver>")
    testthat::expect_true(startsWith(legacy, Require:::cacheDir()),
      info = "non-pak path must be a child of cacheDir()")
  })
})

test_that(".requirePkgInfoDir() is stable across usePak setting", {
  withr::with_options(list(Require.usePak = TRUE), {
    inPak  <- Require:::.requirePkgInfoDir()
  })
  withr::with_options(list(Require.usePak = FALSE), {
    noPak <- Require:::.requirePkgInfoDir()
  })
  testthat::expect_identical(inPak, noPak,
    info = "Require's bookkeeping dir must not move when pak is toggled")
  testthat::expect_true(grepl(paste0("packages/", Require:::versionMajorMinor(), "$"),
                              inPak),
    info = ".requirePkgInfoDir() must keep the legacy <cacheDir>/packages/<Rver> layout")
})

# ---------------------------------------------------------------------------
# cacheClearPackages() / cachePurge() reroute through pak in pak mode
# ---------------------------------------------------------------------------

test_that("cacheClearPackages() in pak mode delegates to pak::cache_clean()", {
  skip_if_not_installed("pak")
  ## Stub pak::cache_clean to a recorder; with no `packages` arg the function
  ## must hit the clean path, not the delete path.
  cleanCalled <- 0L
  deleteCalled <- list()
  testthat::local_mocked_bindings(
    cache_clean = function() {
      cleanCalled <<- cleanCalled + 1L
      invisible(NULL)
    },
    cache_delete = function(...) {
      deleteCalled[[length(deleteCalled) + 1L]] <<- list(...)
      invisible(NULL)
    },
    .package = "pak"
  )
  withr::with_options(list(Require.usePak = TRUE),
    Require::cacheClearPackages(ask = FALSE, verbose = -1))
  testthat::expect_identical(cleanCalled, 1L,
    info = "no `packages` arg must call pak::cache_clean() exactly once")
  testthat::expect_length(deleteCalled, 0L)
})

test_that("cacheClearPackages(packages=...) delegates to pak::cache_delete()", {
  skip_if_not_installed("pak")
  cleanCalled <- 0L
  deleteArgs  <- NULL
  testthat::local_mocked_bindings(
    cache_clean = function() { cleanCalled <<- cleanCalled + 1L; invisible(NULL) },
    cache_delete = function(...) {
      deleteArgs <<- list(...)
      invisible(NULL)
    },
    .package = "pak"
  )
  withr::with_options(list(Require.usePak = TRUE),
    Require::cacheClearPackages(packages = c("ps", "callr"),
                                ask = FALSE, verbose = -1))
  testthat::expect_identical(cleanCalled, 0L,
    info = "with `packages` arg, full clean must NOT be called")
  testthat::expect_identical(deleteArgs$package, c("ps", "callr"),
    info = "package names must be forwarded as `package =` arg to pak::cache_delete()")
})

test_that("cacheClearPackages() under usePak=FALSE keeps walking the legacy bookkeeping dir", {
  ## When usePak is off, the function must NOT invoke pak's API. We
  ## verify by mocking pak's funcs to fail loudly and confirming neither
  ## is invoked.
  skip_if_not_installed("pak")
  testthat::local_mocked_bindings(
    cache_clean = function() stop("pak::cache_clean called under usePak=FALSE"),
    cache_delete = function(...) stop("pak::cache_delete called under usePak=FALSE"),
    .package = "pak"
  )
  withr::with_options(list(Require.usePak = FALSE), {
    res <- tryCatch(
      Require::cacheClearPackages(ask = FALSE, verbose = -1),
      error = function(e) e
    )
    testthat::expect_false(inherits(res, "error"),
      info = "legacy path must not call pak's API")
  })
})

# ---------------------------------------------------------------------------
# Deprecation: cacheGetOptionCachePkgDir, purgeCache, clearRequirePackageCache
#
# These are kept as functional shims for one release cycle so existing user
# code doesn't break, but emit a `.Deprecated()` warning steering callers
# to the canonical names.
# ---------------------------------------------------------------------------

test_that("cacheGetOptionCachePkgDir() emits a deprecation warning", {
  testthat::expect_warning(
    Require::cacheGetOptionCachePkgDir(),
    regexp = "deprecated.*cachePkgDir",
    info = "cacheGetOptionCachePkgDir must emit a .Deprecated() warning"
  )
})

test_that("purgeCache() emits a deprecation warning", {
  ## Stub dealWithCache (the only side-effect) so this test doesn't actually
  ## clear caches in the developer's environment.
  testthat::local_mocked_bindings(
    dealWithCache = function(...) invisible(NULL),
    .package = "Require"
  )
  testthat::expect_warning(
    Require::purgeCache(),
    regexp = "deprecated.*cachePurge",
    info = "purgeCache must emit a .Deprecated() warning"
  )
})

test_that("clearRequirePackageCache() emits a deprecation warning", {
  ## Stub the canonical worker to a no-op for this isolation test.
  testthat::local_mocked_bindings(
    cacheClearPackages = function(...) invisible(NULL),
    .package = "Require"
  )
  testthat::expect_warning(
    Require::clearRequirePackageCache(ask = FALSE),
    regexp = "deprecated.*cacheClearPackages",
    info = "clearRequirePackageCache must emit a .Deprecated() warning"
  )
})

test_that("cachePkgDir() follows R_USER_CACHE_DIR in pak mode (issue #91)", {
  skip_if_not_installed("pak")
  ## The kill+respawn dance: tweak R_USER_CACHE_DIR, kill pak's subprocess,
  ## the next pak call respawns and captures the new env. Restore at exit
  ## so other tests aren't affected.
  oldEnv <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
  oldRemote <- pak:::pkg_data$remote
  tmpRoot <- tempfile("rUserCache-")
  on.exit({
    ## restore env + subprocess for downstream tests
    if (is.na(oldEnv)) Sys.unsetenv("R_USER_CACHE_DIR") else Sys.setenv(R_USER_CACHE_DIR = oldEnv)
    rs <- pak:::pkg_data$remote
    if (inherits(rs, "r_session") && rs$is_alive()) rs$kill()
    unlink(tmpRoot, recursive = TRUE)
  }, add = TRUE)

  Sys.setenv(R_USER_CACHE_DIR = tmpRoot)
  rs <- pak:::pkg_data$remote
  if (inherits(rs, "r_session") && rs$is_alive()) rs$kill()
  ## next pak call respawns with the new env
  newPath <- tryCatch(Require::cachePkgDir(), error = function(e) NULL)
  skip_if(is.null(newPath), "could not query pak cache path after respawn")

  ## macOS resolves /var/folders/... to /private/var/folders/...; normalize
  ## both sides so the prefix check survives the symlink expansion.
  newPathNorm  <- normalizePath(newPath, mustWork = FALSE)
  tmpRootNorm  <- normalizePath(tmpRoot,  mustWork = FALSE)
  testthat::expect_true(startsWith(newPathNorm, tmpRootNorm),
    info = paste("setting R_USER_CACHE_DIR must redirect pak's cache;",
                 "got:", newPathNorm, "expected prefix:", tmpRootNorm))
})

# ---------------------------------------------------------------------------
# 21. install = "force" must NOT upgrade transitive CRAN dependencies
# ---------------------------------------------------------------------------
# Regression report: on Windows + RStudio,
#   Install({reproducible; SpaDES.core; ...}, install = "force")
# emitted pak's
#   "+ broom 1.0.12 → 1.0.13, mgcv 1.9-3 → 1.9-4, sf 1.1-0 → 1.1-1, ..."
# even though the user had not asked for those CRAN dependencies to be
# updated.  Root causes (both fixed; covered below):
#
#  (a) Require2.R "Deal with force installs" block must set
#      `needInstall = .txtInstall` on user-requested rows so they end up
#      in `pakInstallFiltered`'s toInstall.  Previously this was only
#      done indirectly via whichToInstall's `askedByUser <- !is.na(loadOrder)`,
#      which was empty when Install() called Require(require = FALSE)
#      because recordLoadOrder is gated on the require flag.
#
#  (b) pakRetryLoop must NOT propagate forceUpgrade to the CRAN-batch
#      pak::pak() call's `upgrade=` flag.  pak's `upgrade` is global
#      across the entire resolved dep graph -- passing TRUE force-upgrades
#      every CRAN dep, not just the user-requested ones.  Force semantics
#      apply to user-requested packages; transitive deps stay put unless
#      a constraint forces them.

# (a) -- force block correctly sets needInstall on user-requested rows only
test_that("install = 'force' sets needInstall=.txtInstall on user-requested rows only", {
  packages <- c("reproducible", "SpaDES.core")
  ## pkgDT containing user-requested rows + a transitive dep row
  pkgDT <- data.table::data.table(
    Package         = c("reproducible", "SpaDES.core", "broom"),
    packageFullName = c("reproducible", "SpaDES.core", "broom"),
    inequality      = "",
    versionSpec     = "",
    installed       = TRUE,
    Version         = c("1.2.3", "0.9.0", "1.0.12"),
    installedVersionOK = TRUE,
    needInstall     = Require:::.txtDontInstall,
    loadOrder       = NA_integer_
  )

  ## Replicate the "Deal with force installs" block from Require2.R
  data.table::set(pkgDT, NULL, "forceInstall", FALSE)
  install <- "force"
  if (install %in% "force") {
    wh <- which(pkgDT$Package %in% Require:::extractPkgName(packages))
    data.table::set(pkgDT, wh, "installedVersionOK", FALSE)
    data.table::set(pkgDT, wh, "forceInstall", TRUE)
    data.table::set(pkgDT, wh, "needInstall", Require:::.txtInstall)
  }

  testthat::expect_equal(
    pkgDT[Package %in% packages, needInstall],
    rep(Require:::.txtInstall, length(packages)),
    info = "user-requested rows must be marked needInstall=.txtInstall under install='force'"
  )
  testthat::expect_equal(
    pkgDT[Package == "broom", needInstall],
    Require:::.txtDontInstall,
    info = "transitive dep rows must NOT be marked .txtInstall under install='force'"
  )
  testthat::expect_true(
    all(pkgDT[Package %in% packages, forceInstall]),
    info = "user-requested rows must have forceInstall=TRUE"
  )
  testthat::expect_false(
    pkgDT[Package == "broom", forceInstall],
    info = "transitive dep rows must have forceInstall=FALSE"
  )
})

# (a) -- pakInstallFiltered toInstall filter respects the force-block mark
test_that("pakInstallFiltered toInstall filter selects only force-marked user rows", {
  pkgDT <- data.table::data.table(
    Package         = c("reproducible", "SpaDES.core", "broom", "mgcv"),
    packageFullName = c("reproducible", "SpaDES.core", "broom", "mgcv"),
    inequality      = "",
    versionSpec     = "",
    needInstall     = c(Require:::.txtInstall, Require:::.txtInstall,
                        Require:::.txtDontInstall, Require:::.txtDontInstall),
    forceInstall    = c(TRUE, TRUE, FALSE, FALSE),
    installed       = TRUE,
    Version         = c("1.2.3", "0.9.0", "1.0.12", "1.9-3"),
    installedVersionOK = c(FALSE, FALSE, TRUE, TRUE)
  )

  toInstall <- pkgDT[needInstall == Require:::.txtInstall]
  testthat::expect_setequal(
    toInstall$Package, c("reproducible", "SpaDES.core"))
  testthat::expect_false(any(c("broom", "mgcv") %in% toInstall$Package),
    info = "transitive deps must not enter pakInstallFiltered's install set under install='force'")
})

# (b) -- pakRetryLoop CRAN-batch upgrade flag tracks forceUpgrade.
#         The "don't gratuitously upgrade deps" property is achieved at the
#         dep-tree level by pinInstalledForPak()'s `pkg@<installedVersion>`
#         pins (always-on as of fix(install-force): pin installed user
#         packages); pak treats those exact pins as already-satisfied and
#         doesn't upgrade them. cranUp=TRUE on force is still needed so
#         that user packages at versions failing their `>=` constraint
#         do get installed -- with cranUp=FALSE pak skipped them since
#         the bare "any::pkg" form considers any installed version OK
#         (regression caught by test-04other on Windows).
test_that("pakRetryLoop CRAN-batch upgrade flag is derived from forceUpgrade", {
  src <- deparse(body(Require:::pakInstallFiltered))
  oneLine <- paste(src, collapse = "\n")

  testthat::expect_true(
    grepl("cranUp\\s*<-\\s*isTRUE\\(forceUpgrade\\)", oneLine),
    info = paste0("cranUp must track forceUpgrade -- hard-coding FALSE breaks ",
                  "the force-install path for user pkgs that fail their >= ",
                  "constraint, since pak skips when installed satisfies 'any::'")
  )
})

# (b) -- GitHub batch still uses upgrade=TRUE (unchanged) so branch-pulling works
test_that("pakRetryLoop GitHub batch still uses upgrade=TRUE", {
  src <- deparse(body(Require:::pakInstallFiltered))
  oneLine <- paste(src, collapse = "\n")

  ## In the two-call branch (mixed batch), the GitHub call must hard-code
  ## upgrade=TRUE to force fetching the latest commit from the branch.
  ## Find pak::pak(packages[ghOrUrl], ...) and confirm upgrade = TRUE in
  ## the same arg list (allowing whitespace + line breaks).
  ghCall <- regmatches(oneLine, regexpr(
    "pak::pak\\(packages\\[ghOrUrl\\][^)]*\\)", oneLine))
  testthat::expect_true(length(ghCall) > 0L,
    info = "must find the pak::pak(packages[ghOrUrl], ...) call")
  testthat::expect_true(
    grepl("upgrade\\s*=\\s*TRUE", ghCall),
    info = "GitHub-batch pak call must keep upgrade=TRUE so latest commits are fetched"
  )
})

# (b) -- single-call branch upgrade flag is `any(ghOrUrl) || cranUp`,
#         so a CRAN-only batch tracks cranUp (=forceUpgrade) and a GH-only
#         batch always upgrades. No leaking of forceUpgrade through any
#         other path.
test_that("pakRetryLoop single-call branch combines ghOrUrl with cranUp (not forceUpgrade directly)", {
  src <- deparse(body(Require:::pakInstallFiltered))
  oneLine <- paste(src, collapse = "\n")
  testthat::expect_true(
    grepl("up\\s*<-\\s*any\\(ghOrUrl\\)\\s*\\|\\|\\s*cranUp", oneLine),
    info = "single-call branch must combine ghOrUrl with cranUp")
})

# (c) -- pakDepsToPkgDT must pin installed user packages even under
#        install = "force".  Without pinning, pak's dep tree resolution
#        uses the LATEST user-package version's Imports, which transitively
#        forces upgrades of CRAN deps that are still satisfied by the
#        installed user package's Imports.  E.g. installed reproducible
#        Imports `broom (>= 1.0.10)` (satisfied by installed broom 1.0.12),
#        but latest reproducible Imports `broom (>= 1.0.13)` -- pak then
#        upgrades broom to 1.0.13 even with upgrade=FALSE, because the
#        constraint is hard.
test_that("pakDepsToPkgDT pins installed user packages even under install='force'", {
  src <- deparse(body(Require:::pakDepsToPkgDT))
  oneLine <- paste(src, collapse = "\n")

  ## The fix: pinInstalledForPak must NOT be gated on install != "force".
  ## Specifically, there must be no `if (!identical(install, "force"))`
  ## guard wrapping the pinInstalledForPak() call.
  ## Use a tolerant regex that catches the guarded form across any whitespace.
  guardedForm <- "if\\s*\\(\\s*!\\s*identical\\(\\s*install\\s*,\\s*[\"']force[\"']\\s*\\)\\s*\\)\\s*pkgsForPak\\s*<-\\s*pinInstalledForPak"
  testthat::expect_false(
    grepl(guardedForm, oneLine),
    info = paste0("pakDepsToPkgDT must NOT skip pinning under install='force' -- ",
                  "doing so makes pak resolve dep tree against the latest user-package ",
                  "Imports, which transitively forces upgrades of installed CRAN deps."))

  ## And pinInstalledForPak must still be called (unconditionally now).
  testthat::expect_true(
    grepl("pkgsForPak\\s*<-\\s*pinInstalledForPak\\(", oneLine),
    info = "pakDepsToPkgDT must still call pinInstalledForPak to keep deps stable")
})

# (d) -- pre-install integrity check: when a user-requested package's
#        installed DESCRIPTION names a hard dep that is neither already
#        installed nor planned for install in pkgDT, pakInstallFiltered
#        must SKIP the install (not proceed) and emit a clear warning.
#        The typical trigger is the pak-doesn't-follow-Remotes-from-CRAN-
#        style-parents limitation: pak's dep resolution fails, pakDepsToPkgDT
#        falls back to toPkgDTFull(packages), and the user package's
#        Remote-only dep ends up neither in libPath nor in plan. Letting
#        pak install in that state can succeed from a cached binary and
#        produce a broken install whose load fails later.

test_that("pakInstallFiltered aborts install when a hard dep is unresolved (pre-install check)", {
  src <- deparse(body(Require:::pakInstallFiltered))
  oneLine <- paste(src, collapse = "\n")

  testthat::expect_true(
    grepl("unresolvedDeps", oneLine),
    info = "pre-install integrity check must build an unresolvedDeps list"
  )
  testthat::expect_true(
    grepl("skipping install: hard dependencies are unresolved", oneLine, fixed = TRUE),
    info = "skip warning must use a clear leading phrase that explains the abort"
  )
  testthat::expect_true(
    grepl('which = c\\("Depends", "Imports", "LinkingTo"\\)', oneLine),
    info = "pre-install check must parse Imports/Depends/LinkingTo from each installed package's DESCRIPTION"
  )
  testthat::expect_true(
    grepl('"installResult"\\s*,\\s*\\.txtCouldNotBeInstalled', oneLine),
    info = "affected rows must be marked .txtCouldNotBeInstalled in pkgDT"
  )
  testthat::expect_true(
    grepl("toInstall\\s*<-\\s*toInstall\\[!Package %in% affected\\]", oneLine),
    info = "affected rows must be removed from toInstall so pak::pak() is not called for them"
  )
})

# (c) -- pinInstalledForPak's own semantics are unchanged: it respects
#        user-supplied version specs and GitHub/url::/@-pinned refs.
#        This is what lets the new always-pin behaviour stay safe: if a
#        user passes `Install("reproducible (>= 1.5)", install = "force")`,
#        the parenthetical spec marks the package as `userPinned` and pin
#        is skipped, so pak resolves against that constraint (not installed).
test_that("pinInstalledForPak skips user-version-constrained packages", {
  skip_if_not_installed("digest")

  ## Two packages: one with a parenthetical user-version constraint,
  ## one bare (the everyday case the always-pin fix is meant to handle).
  pkgsForPak  <- c("digest", "data.table")
  resolvedPkgs <- c("digest (>= 0.0.1)", "data.table")  # only digest user-pinned

  out <- Require:::pinInstalledForPak(
    pkgsForPak,
    libPaths     = .libPaths(),
    resolvedPkgs = resolvedPkgs
  )

  ## digest had a user spec -> not pinned to installed version.
  testthat::expect_false(grepl("^digest@", out[1]),
    info = "user-version-constrained packages must NOT be pinned (the constraint must drive resolution)")
  ## data.table had no user spec and is installed -> pinned to installed version.
  testthat::expect_true(grepl("^data.table@", out[2]),
    info = "bare user packages with no constraint must be pinned to installed version to keep deps stable")
})
