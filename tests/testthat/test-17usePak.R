# Tests for pak-backend changes introduced on the pak-dep-cache branch.
#
# Covered:
#   1. RequireOptions default Require.usePak = TRUE
#   2. pakBuildFailReason() — extract failure reason from pak error strings
#   3. pakDepConflictRow()  — conflict-table row message format
#   4. pakDepsResolve in-memory cache message fires at verbose = 1
#   5. pakDepsResolve disk cache message fires at verbose = 1

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
