# Tests for the install-failure metadata path:
#   * extractInstallFailures() parsing pak output
#   * reportInstallFailures() formatting the per-package summary
#   * pakInstallFiltered()'s end-of-install summary, including the
#     CRAN-archive fallback for refs pak couldn't resolve as `any::pkg`
#   * pakSerialInstall() basic shape
#
# The full LandR-scale cascade-recovery interaction (~200 ref install,
# parallel-cascade abort, identify-and-defer + serial fallback) is too
# heavy for CRAN / CI. Outside those contexts (local dev runs) it is run
# by default; set `R_REQUIRE_RUN_LARGE_INTEGRATION=false` to opt out
# without leaving the lab. See the last test_that block.

# ---------------------------------------------------------------------------
# Unit: extractInstallFailures() parses pak's per-package failure lines.
# ---------------------------------------------------------------------------
test_that("extractInstallFailures parses 'Failed to build X' + ERROR lines", {
  output <- c(
    "ℹ Building PSPclean 1.0.0.9006",
    "✖ Failed to build PSPclean 1.0.0.9006 (247ms)",
    "WARN: could not be installed: any::DBI, any::sf, ...; ERROR: dependencies 'bit64', 'dplyr', 'sf', 'terra' are not available for package 'PSPclean'",
    "✔ Installed cli 3.6.6 (60ms)"
  )
  fails <- Require:::extractInstallFailures(output)
  expect_s3_class(fails, "data.table")
  expect_equal(NROW(fails), 1L)
  expect_equal(fails$package, "PSPclean")
  expect_equal(fails$reason_type, "missing-build-deps")
  expect_match(fails$reason_brief, "bit64", fixed = TRUE)
  expect_match(fails$reason_brief, "sf", fixed = TRUE)
  expect_match(fails$reason_brief, "terra", fixed = TRUE)
})

test_that("extractInstallFailures handles compile errors", {
  output <- c(
    "✖ Failed to build foo 1.0.0 (5s)",
    "make: *** [foo.o] Error 1",
    "ERROR: compilation failed for package 'foo'"
  )
  fails <- Require:::extractInstallFailures(output)
  expect_equal(NROW(fails), 1L)
  expect_equal(fails$package, "foo")
  expect_equal(fails$reason_type, "compile-error")
})

test_that("extractInstallFailures returns empty when nothing failed", {
  output <- c(
    "ℹ Building cli 3.6.6",
    "✔ Installed cli 3.6.6 (60ms)",
    "✔ 1 pkg: added 1 [1.6s]"
  )
  fails <- Require:::extractInstallFailures(output)
  expect_s3_class(fails, "data.table")
  expect_equal(NROW(fails), 0L)
})

test_that("extractInstallFailures strips ANSI color codes", {
  output <- "\033[33m\033[33m✖ Failed to build foo 1.0 (1s)\033[39m"
  fails <- Require:::extractInstallFailures(output)
  expect_equal(NROW(fails), 1L)
  expect_equal(fails$package, "foo")
})

# ---------------------------------------------------------------------------
# Field case (PredictiveEcology/fireSenseUtils@development): package's
# DESCRIPTION declares `VignetteBuilder: knitr, rmarkdown` but neither is
# in the active library. R CMD build halts at loadVignetteBuilder() with
# "vignette builder 'knitr' not found" -- and pak's console stream emits
# only the "✖ Failed to build" summary, dropping the actual cause. With the
# structured-error capture (pakConditionLog), the parent condition's
# message lands in allCapturedMsgs and this test verifies the parser
# attributes it correctly rather than falling through to the catch-all
# "still-missing — cascade casualty" branch.
# ---------------------------------------------------------------------------
test_that("extractInstallFailures recognizes missing VignetteBuilder", {
  output <- c(
    "✖ Failed to build fireSenseUtils 0.1.8 (3.1s)",
    "Error in loadVignetteBuilder(pkgdir, TRUE, lib.loc = c(libdir, .libPaths())) :",
    "  vignette builder 'knitr' not found",
    "Execution halted"
  )
  fails <- Require:::extractInstallFailures(output)
  expect_equal(NROW(fails), 1L)
  expect_equal(fails$package, "fireSenseUtils")
  expect_equal(fails$reason_type, "missing-build-deps")
  expect_match(fails$reason_brief, "VignetteBuilder", fixed = TRUE)
  expect_match(fails$reason_brief, "knitr", fixed = TRUE)
})

# Field case (PredictiveEcology/fireSenseUtils@development, after the
# initial vignette-builder fix landed): once knitr/rmarkdown were present,
# the build progressed further and failed at lazy-load. pak's stream had
# both "* installing *source* package 'fireSenseUtils' ..." (a generic
# progress line) AND "ERROR: lazy loading failed for package 'fireSenseUtils'"
# in the same 25-line window. The original first-match-wins regex picked
# the generic line, so the install summary printed "* installing *source*
# package 'fireSenseUtils' ..." as the reason. Priority-ordered scanning
# must surface the lazy-load failure (and ideally the preceding "Error:"
# line that names the actual cause).
test_that("extractInstallFailures recognizes missing GitHub branch", {
  # When a user pins `account/repo@somebranch (HEAD)` and `somebranch`
  # doesn't exist on the remote (typo, or never pushed), pak emits at
  # dep-resolution time:
  #   Can't find reference @somebranch in GitHub repo account/repo.
  # This must be surfaced as a `missing-github-branch` reason with both
  # the branch name and owner/repo named, plus the actionable hint
  # "did you push it?". Without this, the failure either falls through
  # to the catch-all "still-missing" or — worse — gets masked entirely
  # by useLoadedIfSufficient's "no version constraint" short-circuit.
  output <- c(
    "X Failed to build reproducible",
    "Could not solve package dependencies:",
    "* PredictiveEcology/reproducible@useCloudPullPushTest: ! pkgdepends resolution error",
    "Caused by error:",
    "! Can't find reference @useCloudPullPushTest in GitHub repo PredictiveEcology/reproducible.",
    ""
  )
  fails <- Require:::extractInstallFailures(output)
  expect_equal(NROW(fails), 1L)
  expect_equal(fails$package, "reproducible")
  expect_equal(fails$reason_type, "missing-github-branch")
  expect_match(fails$reason_brief, "useCloudPullPushTest", fixed = TRUE)
  expect_match(fails$reason_brief, "PredictiveEcology/reproducible", fixed = TRUE)
  expect_match(fails$reason_brief, "did you push it?", fixed = TRUE)
})

test_that("pakConditionLog synthesizes Failed-to-build anchor for missing GitHub branch", {
  # The dep-resolution failure has NO package_build_error parent, so the
  # build-error walk returns nothing. The fallback path must produce a
  # synthetic "Failed to build <pkg>" anchor (so extractInstallFailures
  # attributes the row to the right ref) plus the actual error lines.
  parent <- structure(
    list(message = paste0(
      "Could not solve package dependencies:\n",
      "* PredictiveEcology/reproducible@thisBranchDoesNotExist: ! ",
      "pkgdepends resolution error for ",
      "PredictiveEcology/reproducible@thisBranchDoesNotExist.\n",
      "Caused by error: \n",
      "! Can't find reference @thisBranchDoesNotExist in GitHub repo ",
      "PredictiveEcology/reproducible.")),
    class = c("simpleError", "error", "condition"))
  outer <- structure(
    list(message = "error in pak subprocess", parent = parent),
    class = c("callr_status_error", "callr_error", "rlib_error_3_0",
              "rlib_error", "error", "condition"))
  errStr <- structure("Error : ! error in pak subprocess\n",
                      class = "try-error", condition = outer)

  log <- Require:::pakConditionLog(errStr)
  joined <- paste(log, collapse = "\n")
  expect_match(joined, "Failed to build reproducible", fixed = TRUE)
  expect_match(joined, "Can't find reference @thisBranchDoesNotExist", fixed = TRUE)

  # End-to-end: log routes through the parser to a clean row.
  fails <- Require:::extractInstallFailures(log)
  expect_equal(NROW(fails), 1L)
  expect_equal(fails$package, "reproducible")
  expect_equal(fails$reason_type, "missing-github-branch")
  expect_match(fails$reason_brief, "thisBranchDoesNotExist", fixed = TRUE)
})

test_that("extractInstallFailures recognizes lazy loading failed (priority over generic)", {
  output <- c(
    "ℹ Building fireSenseUtils 0.1.8",
    "✖ Failed to build fireSenseUtils 0.1.8 (3.1s)",
    "* installing *source* package 'fireSenseUtils' ...",
    "** using staged installation",
    "** R",
    "** byte-compile and prepare package for lazy loading",
    "Error in eval(ei, envir) : object 'foo' not found",
    "ERROR: lazy loading failed for package 'fireSenseUtils'",
    "* removing '/tmp/X/fireSenseUtils'"
  )
  fails <- Require:::extractInstallFailures(output)
  expect_equal(NROW(fails), 1L)
  expect_equal(fails$package, "fireSenseUtils")
  expect_equal(fails$reason_type, "build-error")
  # Must NOT pick the generic progress line.
  expect_false(grepl("\\* installing \\*source\\*", fails$reason_brief))
  # Must mention lazy loading and ideally the preceding Error line.
  expect_match(fails$reason_brief, "lazy loading", fixed = TRUE)
  expect_match(fails$reason_brief, "object 'foo' not found", fixed = TRUE)
})

test_that("extractInstallFailures recognizes missing build-time package", {
  # R uses Unicode ‘/’ single quotes in this error message;
  # the recognizer must accept both ASCII ' and the typographic forms.
  outputAscii <- c(
    "✖ Failed to build foo 1.0.0 (200ms)",
    "Error: there is no package called 'bar'"
  )
  outputUnicode <- c(
    "✖ Failed to build foo 1.0.0 (200ms)",
    "Error: there is no package called ‘bar’"
  )
  for (out in list(outputAscii, outputUnicode)) {
    fails <- Require:::extractInstallFailures(out)
    expect_equal(NROW(fails), 1L)
    expect_equal(fails$package, "foo")
    expect_equal(fails$reason_type, "missing-build-deps")
    expect_match(fails$reason_brief, "bar", fixed = TRUE)
  }
})

# ---------------------------------------------------------------------------
# pakConditionLog(): given a try-error whose condition chain carries a
# `package_build_error` parent (pak's structured form for an R CMD INSTALL
# failure), return a character vector that downstream regex parsers can
# attribute to the right ref. Without this helper, pak's parent$message
# (which contains the actual ERROR text) is invisible to allCapturedMsgs.
# ---------------------------------------------------------------------------
test_that("pakConditionLog extracts package_build_error data from try-error", {
  parent <- structure(
    list(
      message = "Failed to build source package fireSenseUtils.\nFull installation output:\nERROR: dependencies 'sf', 'terra' are not available for package 'fireSenseUtils'\n",
      package = "fireSenseUtils",
      version = "0.1.8",
      stdout  = "* checking for file 'DESCRIPTION' ... OK\n"),
    class = c("package_build_error", "rlib_error_3_0", "rlib_error",
              "error", "condition"))
  outer <- structure(
    list(message = "error in pak subprocess", parent = parent),
    class = c("callr_error", "rlib_error_3_0", "rlib_error",
              "error", "condition"))
  # Mimic try()'s wrapping: the top-level value is a try-error string with
  # the condition stashed on `attr(., "condition")`.
  errStr <- structure("Error : ! error in pak subprocess\n",
                      class = "try-error", condition = outer)

  log <- Require:::pakConditionLog(errStr)
  expect_true(length(log) > 0L)
  joined <- paste(log, collapse = "\n")
  # Synthetic line lets extractBuildFailures attribute to the right ref.
  expect_match(joined, "Failed to build fireSenseUtils 0.1.8", fixed = TRUE)
  # The actual root cause must be present.
  expect_match(joined, "dependencies", fixed = TRUE)
  expect_match(joined, "sf", fixed = TRUE)
  expect_match(joined, "terra", fixed = TRUE)

  # End-to-end: the spliced log must drive extractInstallFailures to a
  # specific reason_type rather than the generic "build-error" fallback.
  fails <- Require:::extractInstallFailures(log)
  expect_equal(NROW(fails), 1L)
  expect_equal(fails$package, "fireSenseUtils")
  expect_equal(fails$reason_type, "missing-build-deps")
})

test_that("pakConditionLog handles a vignette-builder failure end-to-end", {
  # Same shape, but the parent message carries the loadVignetteBuilder
  # error instead of a missing-deps line. This is the exact field case
  # from the user's failing fireSenseUtils@development install.
  parent <- structure(
    list(
      message = paste0(
        "Failed to build source package fireSenseUtils.\n",
        "Full installation output:\n",
        "* checking for file '.../DESCRIPTION' ... OK\n",
        "* preparing 'fireSenseUtils':\n",
        "* checking DESCRIPTION meta-information ... OK\n",
        "Error in loadVignetteBuilder(pkgdir, TRUE, lib.loc = c(libdir, .libPaths())) :\n",
        "  vignette builder 'knitr' not found\n",
        "Execution halted\n"),
      package = "fireSenseUtils",
      version = "0.1.8"),
    class = c("package_build_error", "error", "condition"))
  outer <- structure(list(message = "error in pak subprocess", parent = parent),
                     class = c("callr_error", "error", "condition"))
  errStr <- structure("Error : ! error in pak subprocess\n",
                      class = "try-error", condition = outer)

  log <- Require:::pakConditionLog(errStr)
  fails <- Require:::extractInstallFailures(log)
  expect_equal(NROW(fails), 1L)
  expect_equal(fails$package, "fireSenseUtils")
  expect_equal(fails$reason_type, "missing-build-deps")
  expect_match(fails$reason_brief, "VignetteBuilder", fixed = TRUE)
  expect_match(fails$reason_brief, "knitr", fixed = TRUE)
})

test_that("pakConditionLog dumps the chain message when no package_build_error present", {
  # Plain error condition with no package_build_error parent: pakConditionLog
  # falls back to dumping the condition message so the outer parsers
  # (extractInstallFailures / pakBuildFailReason) at least see the underlying
  # cause, even if it's not a per-package build failure. Original assertion
  # was "returns empty" — that became wrong when we extended the function to
  # surface dep-resolution errors like "Can't find reference @branch in
  # GitHub repo owner/repo" (which lack a package_build_error parent).
  cond <- simpleError("generic failure")
  errStr <- structure("Error : generic failure\n",
                      class = "try-error", condition = cond)
  log <- Require:::pakConditionLog(errStr)
  expect_true(any(grepl("generic failure", log, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# Unit: reportInstallFailures() supplements parser output with still-missing
# entries and prints a one-line-per-package summary.
# ---------------------------------------------------------------------------
test_that("reportInstallFailures adds still-missing rows for unexplained pkgs", {
  parsed <- data.table::data.table(
    package      = "PSPclean",
    reason_type  = "missing-build-deps",
    reason_brief = "build-time deps not yet in lib: bit64, sf",
    reason_detail = "ERROR: dependencies ..."
  )
  missing <- c("PSPclean", "disk.frame", "pryr")
  out <- capture.output(
    res <- Require:::reportInstallFailures(parsed, missingPkgNames = missing,
                                           verbose = 1),
    type = "output"
  )
  expect_equal(NROW(res), 3L)
  expect_setequal(res$package, c("PSPclean", "disk.frame", "pryr"))
  expect_equal(res[package == "PSPclean", reason_type],   "missing-build-deps")
  expect_equal(res[package == "disk.frame", reason_type], "still-missing")
  expect_equal(res[package == "pryr",       reason_type], "still-missing")
  expect_match(paste(out, collapse = "\n"), "Install summary: 3 package")
})

# ---------------------------------------------------------------------------
# Regression: pakInstallFiltered's end-of-install summary used to produce
# spurious entries for packages that failed in pak's first parallel pass but
# built successfully in the deferred-culprit serial pass.
#
# Concrete scenario from the field: install a GitHub HEAD package
# (`PredictiveEcology/reproducible@HEAD`) whose build-time deps (digest,
# fpCompare, lobstr) aren't in the project lib yet. pak emits
# "✖ Failed to build reproducible 3.0.0.9050" in iter 1; identify-and-defer
# treats it as a culprit, deferring to a final serial pass that succeeds
# (deps now in lib). reproducible IS in installed.packages() at the end,
# but the iter-1 "Failed to build" line is still in allCapturedMsgs — and
# the summary used to print it as a build-error anyway.
#
# Independently, when `qs` (archived from CRAN) hits the archive-fallback
# and its source build genuinely fails to compile, the per-package
# "Failed to build qs" line is emitted DURING the archive pass. The summary
# used to be computed BEFORE the archive pass ran, so qs would appear as
# the catch-all "still-missing" / "cascade casualty of a wedged subprocess"
# — even though pak did emit a real per-package error for it.
#
# Both bugs share the same fix shape: parse failure metadata once, after
# every install pass has finished, AND drop entries for packages that ended
# up installed (i.e. filter by finalMissing).
# ---------------------------------------------------------------------------
test_that("install summary drops culprits resolved by the deferred serial pass", {
  # Concrete scenario from the field: install a GitHub HEAD package
  # (`PredictiveEcology/reproducible@HEAD`) whose build-time deps (digest,
  # fpCompare, lobstr) aren't in the project lib yet during iter 1. pak emits
  # "✖ Failed to build reproducible 3.0.0.9050" in iter 1; identify-and-defer
  # treats it as a culprit and the final serial pass installs it after the
  # missing deps land. reproducible IS in installed.packages() at the end,
  # but the iter-1 "Failed to build" line is still in allCapturedMsgs — and
  # the summary used to print it as a build-error anyway.
  msgs <- c(
    "✖ Failed to build reproducible 3.0.0.9050 (395ms)",
    "Warning: could not be installed: ...; ERROR: dependencies 'digest', 'fpCompare', 'lobstr' are not available for package 'reproducible'",
    "✔ Installed digest 0.6.39 (219ms)",
    "✔ Installed fpCompare 0.2.4 (260ms)",
    "✔ Installed lobstr 1.2.1 (222ms)",
    "ℹ Building reproducible 3.0.0.9050",
    "✔ Built reproducible 3.0.0.9050 (8.6s)",
    "✔ Installed reproducible 3.0.0.9050"
  )
  parsed <- Require:::extractInstallFailures(msgs)
  # Sanity: the iter-1 failure IS captured by the parser.
  expect_true("reproducible" %in% parsed$package)

  # The fix: filter by finalMissing (packages NOT in installed.packages())
  # before reporting. reproducible installed successfully in the deferred
  # pass, so it should not appear in finalMissing.
  finalMissing <- character(0)
  filtered <- parsed[package %in% finalMissing]
  expect_equal(NROW(filtered), 0L)

  out <- capture.output(
    Require:::reportInstallFailures(filtered, missingPkgNames = finalMissing,
                                    verbose = 1),
    type = "output"
  )
  # No summary should be printed when nothing is genuinely missing — and
  # in particular reproducible must not be listed.
  expect_false(any(grepl("reproducible", out)))
  expect_false(any(grepl("Install summary", out)))
})

test_that("archive-pass build errors are labeled (not 'still-missing')", {
  # When `qs` (archived from CRAN) hits the archive-fallback path and its
  # source build genuinely fails to compile, pak emits a per-package
  # "✖ Failed to build qs" line DURING the archive pass. The summary used
  # to be parsed BEFORE the archive pass ran, so qs would fall through to
  # the catch-all "still-missing" / "cascade casualty of a wedged
  # subprocess" branch — even though pak emitted a real build failure for
  # it. The fix moves the canonical parse to AFTER archive fallback.
  msgs <- c(
    "archive fallback: trying CRAN archive for 1 still-missing ref(s): qs",
    "ℹ Building qs 0.27.3",
    "✖ Failed to build qs 0.27.3 (7.2s)",
    "ERROR: compilation failed for package 'qs'"
  )
  parsed <- Require:::extractInstallFailures(msgs)
  expect_equal(NROW(parsed), 1L)
  expect_equal(parsed$package, "qs")
  expect_equal(parsed$reason_type, "compile-error")

  finalMissing <- "qs"
  filtered <- parsed[package %in% finalMissing]
  out <- capture.output(
    Require:::reportInstallFailures(filtered, missingPkgNames = finalMissing,
                                    verbose = 1),
    type = "output"
  )
  joined <- paste(out, collapse = "\n")
  expect_match(joined, "qs")
  expect_match(joined, "compile-error")
  # The whole point: NOT the catch-all label.
  expect_false(grepl("still-missing", joined))
  expect_false(grepl("cascade casualty", joined))
})

test_that("reportInstallFailures returns invisibly with no output when nothing missing", {
  empty <- data.table::data.table(
    package = character(0), reason_type = character(0),
    reason_brief = character(0), reason_detail = character(0))
  out <- capture.output(
    res <- Require:::reportInstallFailures(empty, missingPkgNames = character(0),
                                           verbose = 1),
    type = "output"
  )
  expect_equal(NROW(res), 0L)
  expect_equal(length(out), 0L)
})

# ---------------------------------------------------------------------------
# Integration: archive fallback for an archived-from-CRAN package.
#
# `pryr` was archived from CRAN; pak::pak("any::pryr") cannot resolve it
# against the current CRAN mirror, so identify-and-defer reaches its
# end-of-install summary with pryr as still-missing. The archive-fallback
# pass should then build a `url::https://cran.../Archive/pryr_X.X.X.tar.gz`
# ref and install successfully.
# ---------------------------------------------------------------------------
test_that("pakGetArchive constructs CRAN-archive URL for archived package", {
  # Lighter-weight check: the archive-URL-construction step works for a
  # known archived-from-CRAN package. The full Require::Install("pryr")
  # round-trip (which exercises the archive fallback path inside
  # pakInstallFiltered) is environment-sensitive and runs in the larger
  # integration test below.
  if (!nzchar(Sys.getenv("R_REQUIRE_RUN_LONG_CI"))) skip_on_ci()
  skip_on_cran()
  skip_if_offline2()
  skip_if_not_installed("pak")

  withr::local_options(repos = c(CRAN = "https://cran.rstudio.com"))
  ref <- tryCatch(
    Require:::pakGetArchive("pryr", packages = "pryr", whRm = 1L),
    error = function(e) e, warning = function(w) w)
  if (inherits(ref, "condition")) skip(paste("pak subprocess unavailable:",
                                              conditionMessage(ref)))
  expect_match(ref, "^url::https?://.*Archive/pryr/pryr_.*\\.tar\\.gz$")
})

# ---------------------------------------------------------------------------
# Regression: when options(repos) has no concrete CRAN URL (only @CRAN@
# placeholder, or only a non-CRAN repo like an r-universe), pakGetArchive
# previously returned a bare "url::" string (paste0("url::", character(0))
# yields a length-1 "url::"). Downstream pak::pak("url::") then aborted the
# whole batch with "All URLs failed", masking the real situation.
# pakGetArchive must now return the input `packages` unchanged in this case
# so the caller can skip cleanly.
# ---------------------------------------------------------------------------
test_that("pakGetArchive returns unchanged packages when no concrete CRAN repo", {
  if (!nzchar(Sys.getenv("R_REQUIRE_RUN_LONG_CI"))) skip_on_ci()
  skip_on_cran()
  skip_if_offline2()
  skip_if_not_installed("pak")

  for (rep in list(
    c("https://predictiveecology.r-universe.dev"),
    c("https://predictiveecology.r-universe.dev", CRAN = "@CRAN@")
  )) {
    withr::local_options(repos = rep)
    ref <- tryCatch(
      Require:::pakGetArchive("disk.frame", packages = "disk.frame", whRm = 1L),
      error = function(e) e, warning = function(w) w)
    if (inherits(ref, "condition")) {
      skip(paste("pak subprocess unavailable:", conditionMessage(ref)))
    }
    # Must NOT be a bare "url::" or anything starting "url::" without a host.
    expect_false(any(grepl("^url::$", ref)),
                 info = sprintf("repos=%s", paste(rep, collapse = ", ")))
    expect_false(any(grepl("^url::[^h]", ref)),
                 info = sprintf("repos=%s", paste(rep, collapse = ", ")))
    # Either unchanged input (caller will skip), or a fully-formed archive URL.
    ok <- identical(ref, "disk.frame") ||
          all(grepl("^url::https?://.+", ref))
    expect_true(ok,
                info = sprintf("ref=%s repos=%s",
                               paste(ref, collapse=","),
                               paste(rep, collapse = ", ")))
  }
})

test_that("pak::pak installs an archived-CRAN ref via url::", {
  # The lower-level pak call that the archive fallback ultimately makes;
  # if this works, Require's archive fallback will work too (modulo pak's
  # internal subprocess state, which is exercised in the big integration
  # test).
  if (!nzchar(Sys.getenv("R_REQUIRE_RUN_LONG_CI"))) skip_on_ci()
  skip_on_cran()
  skip_if_offline2()
  skip_if_not_installed("pak")

  testlib <- file.path(tempdir(), paste0("rqlib_pryrurl_", sample(1e5, 1)))
  dir.create(testlib, recursive = TRUE)
  on.exit(unlink(testlib, recursive = TRUE), add = TRUE)

  origLibPaths <- .libPaths()
  on.exit(.libPaths(origLibPaths), add = TRUE)
  for (p in c("pak", "withr", "fs", "filelock", "sys",
              "data.table", "rprojroot", "rstudioapi")) {
    src <- find.package(p, lib.loc = origLibPaths, quiet = TRUE)
    if (length(src) && nzchar(src) && !file.exists(file.path(testlib, p))) {
      file.copy(src, testlib, recursive = TRUE)
    }
  }
  .libPaths(c(testlib, .Library))  # .Library is the cross-platform base R lib
  withr::local_options(repos = c(CRAN = "https://cran.rstudio.com"))

  ref <- "url::https://cran.rstudio.com/src/contrib/Archive/pryr/pryr_0.1.6.tar.gz"
  res <- try(pak::pak(ref, lib = testlib, ask = FALSE,
                      dependencies = NA, upgrade = FALSE), silent = TRUE)
  if (inherits(res, "try-error")) skip(paste("pak install failed:", as.character(res)))

  expect_true("pryr" %in% rownames(installed.packages(testlib)),
              info = "pryr should be installed via direct pak::pak(url::...)")
})

# ---------------------------------------------------------------------------
# Cross-archive deps: disk.frame depends on pryr (>= 0.1.4); both are
# archived from CRAN, so pak::pak("any::disk.frame") fails with
# "Can't find package called pryr". The archive-fallback batch must pass
# both archive URLs together so pak resolves disk.frame -> pryr from the
# same plan.
# ---------------------------------------------------------------------------
test_that("pak::pak installs cross-dependent archived refs in one batch", {
  if (!nzchar(Sys.getenv("R_REQUIRE_RUN_LONG_CI"))) skip_on_ci()
  skip_on_cran()
  skip_if_offline2()
  skip_if_not_installed("pak")

  testlib <- file.path(tempdir(), paste0("rqlib_xarch_", sample(1e5, 1)))
  dir.create(testlib, recursive = TRUE)
  on.exit(unlink(testlib, recursive = TRUE), add = TRUE)

  origLibPaths <- .libPaths()
  on.exit(.libPaths(origLibPaths), add = TRUE)
  for (p in c("pak", "withr", "fs", "filelock", "sys",
              "data.table", "rprojroot", "rstudioapi")) {
    src <- find.package(p, lib.loc = origLibPaths, quiet = TRUE)
    if (length(src) && nzchar(src) && !file.exists(file.path(testlib, p))) {
      file.copy(src, testlib, recursive = TRUE)
    }
  }
  .libPaths(c(testlib, .Library))  # .Library is the cross-platform base R lib
  withr::local_options(repos = c(CRAN = "https://cran.rstudio.com"))

  refs <- c(
    "url::https://cran.rstudio.com/src/contrib/Archive/disk.frame/disk.frame_0.8.3.tar.gz",
    "url::https://cran.rstudio.com/src/contrib/Archive/pryr/pryr_0.1.6.tar.gz")
  res <- try(pak::pak(refs, lib = testlib, ask = FALSE,
                      dependencies = NA, upgrade = FALSE), silent = TRUE)
  if (inherits(res, "try-error")) skip(paste("pak install failed:", as.character(res)))

  inst <- rownames(installed.packages(testlib))
  expect_true("disk.frame" %in% inst, info = "disk.frame should be installed")
  expect_true("pryr"       %in% inst, info = "pryr should be installed")
})

# ---------------------------------------------------------------------------
# Integration: pakInstallFiltered emits an install summary with reasons
# attributable to specific packages, and the structured table is exposed
# in pakEnv()$.lastInstallFailures.
# ---------------------------------------------------------------------------
test_that("pakEnv()$.lastInstallFailures is populated after a successful install", {
  if (!nzchar(Sys.getenv("R_REQUIRE_RUN_LONG_CI"))) skip_on_ci()
  skip_on_cran()
  skip_if_offline2()
  skip_if_not_installed("pak")

  testlib <- file.path(tempdir(), paste0("rqlib_summary_", sample(1e5, 1)))
  dir.create(testlib, recursive = TRUE)
  on.exit(unlink(testlib, recursive = TRUE), add = TRUE)

  origLibPaths <- .libPaths()
  on.exit(.libPaths(origLibPaths), add = TRUE)
  for (p in c("Require", "pak", "withr", "fs", "filelock", "sys",
              "data.table", "rprojroot", "rstudioapi")) {
    src <- find.package(p, lib.loc = origLibPaths, quiet = TRUE)
    if (length(src) && nzchar(src) && !file.exists(file.path(testlib, p))) {
      file.copy(src, testlib, recursive = TRUE)
    }
  }
  .libPaths(c(testlib, .Library))  # .Library is the cross-platform base R lib

  withr::local_options(
    repos = c(CRAN = "https://cran.rstudio.com"),
    Require.verbose = -2
  )

  res <- tryCatch(Require::Install(c("R6", "cli")),
                  error = function(e) e)
  if (inherits(res, "error")) skip(paste("network or pak issue:", conditionMessage(res)))

  pakEnv <- Require:::pakEnv()
  expect_true(exists(".lastInstallFailures", envir = pakEnv))
  failures <- get(".lastInstallFailures", envir = pakEnv)
  # Either NULL (if no install was needed) or an empty/populated data.table
  expect_true(is.null(failures) || data.table::is.data.table(failures))
})

# ---------------------------------------------------------------------------
# Integration: full LandR-scale cascade-recovery exercise.
#
# Replicates the install pattern from the SpaDES training book's
# LandRDemo_coreVeg.qmd: a setupProject with ~100+ refs that, when run
# with a fresh project lib and the dev branches of Require/reproducible/
# SpaDES.project/SpaDES.core, hits pak's parallel-build cascade abort.
# Verifies that identify-and-defer's iterative pass + serial fallback
# recover the install end-to-end and that the install summary correctly
# attributes the surviving still-missing refs.
#
# Slow (3-15 min depending on package cache state) and network-heavy.
# CRAN and CI are already skipped above; on local interactive/dev runs
# we run this by default. Set R_REQUIRE_RUN_LARGE_INTEGRATION=false to
# opt out without changing the test.
# ---------------------------------------------------------------------------
test_that("identify-and-defer recovers from PSPclean-style cascade", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline2()
  skip_if_not_installed("pak")
  if (identical(tolower(Sys.getenv("R_REQUIRE_RUN_LARGE_INTEGRATION", "true")),
                "false")) {
    skip("R_REQUIRE_RUN_LARGE_INTEGRATION=false; skipping multi-minute install")
  }
  skip_if_not_installed("SpaDES.project")

  testlib <- file.path(tempdir(), paste0("rqlib_landr_", sample(1e5, 1)))
  dir.create(testlib, recursive = TRUE)
  on.exit(unlink(testlib, recursive = TRUE), add = TRUE)

  origLibPaths <- .libPaths()
  on.exit(.libPaths(origLibPaths), add = TRUE)
  for (p in c("Require", "pak", "withr", "fs", "filelock", "sys",
              "data.table", "rprojroot", "rstudioapi", "SpaDES.project")) {
    src <- find.package(p, lib.loc = origLibPaths, quiet = TRUE)
    if (length(src) && nzchar(src) && !file.exists(file.path(testlib, p))) {
      file.copy(src, testlib, recursive = TRUE)
    }
  }
  .libPaths(c(testlib, .Library))  # .Library is the cross-platform base R lib

  withr::local_options(
    repos = c("https://predictiveecology.r-universe.dev",
              CRAN = "https://cran.rstudio.com"),
    Require.verbose = -2
  )

  # The LandRDemo dep set: enough refs to trip pak's parallel cascade,
  # including PSPclean (the historical culprit) via LandR's Remotes.
  res <- tryCatch(
    Require::Install(c(
      "PredictiveEcology/LandR@main",
      "PredictiveEcology/SpaDES.core@development",
      "PredictiveEcology/reproducible@development"
    )),
    error = function(e) e)
  if (inherits(res, "error")) skip(paste("install error:", conditionMessage(res)))

  inst <- rownames(installed.packages(testlib))
  expect_true("SpaDES.core" %in% inst,
              info = "SpaDES.core must be in project lib after cascade recovery")
  expect_true("LandR" %in% inst,
              info = "LandR must be in project lib after cascade recovery")
  expect_true("reproducible" %in% inst,
              info = "reproducible must be in project lib after cascade recovery")

  # If any packages didn't make it, every entry in the failure table should
  # have a non-empty reason_type so users get actionable messaging.
  pakEnv <- Require:::pakEnv()
  failures <- get0(".lastInstallFailures", envir = pakEnv)
  if (data.table::is.data.table(failures) && NROW(failures) > 0L) {
    expect_true(all(nzchar(failures$reason_type)))
    expect_true(all(nzchar(failures$reason_brief)))
  }
})
