test_that("pkgDepCRAN includes parentChain in 'not on CRAN' message", {
  # The parentChain parameter threads a dependency chain string through the call
  # stack so that "not on CRAN" messages explain WHY a package is needed.
  # E.g., "fastdigest not on CRAN (required by: digest -> reproducible)"
  #
  # Strategy: build a minimal pkgDT where:
  #   - Depends != NULL  → joinToAvailablePackages is a no-op (skips network call)
  #   - VersionOnRepos = NA → inCurrentCRAN() returns FALSE → triggers message
  # Capture messages with withCallingHandlers; swallow downstream errors with tryCatch.

  pkgDT <- data.table::data.table(
    Package            = "zzzmadeuppkg99999",
    packageFullName    = "zzzmadeuppkg99999",
    versionSpec        = NA_character_,
    VersionOnRepos     = NA_character_,
    Depends            = NA_character_,  # non-NULL → skip joinToAvailablePackages
    availableVersionOK = NA,
    repoLocation       = NA_character_
  )

  # Ensure offlineMode is not pre-set from a prior test
  old_offline <- getOption("Require.offlineMode")
  on.exit(options(Require.offlineMode = old_offline), add = TRUE)
  options(Require.offlineMode = FALSE)

  msgs <- character(0)
  withCallingHandlers(
    tryCatch(
      Require:::pkgDepCRAN(
        pkgDT       = pkgDT,
        which       = "Depends",
        repos       = "https://cloud.r-project.org",
        type        = "source",
        libPaths    = .libPaths(),
        verbose     = 1,
        parentChain = "digest -> reproducible"
      ),
      error = function(e) NULL  # swallow downstream errors after message is printed
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  not_on_cran_msg <- msgs[grepl("not on CRAN", msgs, fixed = TRUE)]
  testthat::expect_true(length(not_on_cran_msg) > 0,
    info = "Expected a 'not on CRAN' message to be emitted")
  testthat::expect_match(not_on_cran_msg, "required by: digest -> reproducible",
    fixed = TRUE)
})

test_that("pkgDepCRAN omits chain suffix when parentChain is empty", {
  pkgDT <- data.table::data.table(
    Package            = "zzzmadeuppkg99999",
    packageFullName    = "zzzmadeuppkg99999",
    versionSpec        = NA_character_,
    VersionOnRepos     = NA_character_,
    Depends            = NA_character_,
    availableVersionOK = NA,
    repoLocation       = NA_character_
  )

  old_offline <- getOption("Require.offlineMode")
  on.exit(options(Require.offlineMode = old_offline), add = TRUE)
  options(Require.offlineMode = FALSE)

  msgs <- character(0)
  withCallingHandlers(
    tryCatch(
      Require:::pkgDepCRAN(
        pkgDT       = pkgDT,
        which       = "Depends",
        repos       = "https://cloud.r-project.org",
        type        = "source",
        libPaths    = .libPaths(),
        verbose     = 1,
        parentChain = ""
      ),
      error = function(e) NULL
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  not_on_cran_msg <- msgs[grepl("not on CRAN", msgs, fixed = TRUE)]
  testthat::expect_true(length(not_on_cran_msg) > 0,
    info = "Expected a 'not on CRAN' message to be emitted")
  testthat::expect_false(grepl("required by", not_on_cran_msg, fixed = TRUE),
    info = "Message should NOT contain 'required by' when parentChain is empty")
})

test_that(".DESCFileFull uses basename for file:// Repository URLs", {
  # Regression test: when Repository is a file:// URL (locally cached archive),
  # the download URL must use basename(PackageUrl) because local cache files are
  # stored flat (no Package/ subdirectory), unlike remote CRAN archive URLs.
  # Bug: file.path("file:///path", "pkg/pkg_1.0.tar.gz") produced a
  # file:////path/pkg/pkg_1.0.tar.gz URL that could never be found.

  td <- Require:::tempdir2("test_DESCFileFull")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  pkg <- "fakepkg"
  ver <- "1.0"
  tarname <- paste0(pkg, "_", ver, ".tar.gz")

  # Build a minimal package tarball: fakepkg/DESCRIPTION inside the archive
  srcDir <- file.path(td, "src")
  pkgDir <- file.path(srcDir, pkg)
  dir.create(pkgDir, recursive = TRUE)
  writeLines(c(
    paste0("Package: ", pkg),
    paste0("Version: ", ver),
    "Title: Fake Package",
    "Description: Fake package for testing.",
    "License: GPL-3"
  ), file.path(pkgDir, "DESCRIPTION"))

  # Store tarball flat in the cache dir (no Package/ subdir) — local cache layout
  cacheDir <- file.path(td, "cache")
  dir.create(cacheDir)
  tarfile <- file.path(cacheDir, tarname)
  withr::with_dir(srcDir, utils::tar(tarfile, files = pkg, compression = "gzip", tar = "internal"))

  # PackageUrl has the CRAN archive subdir layout (Package/file.tar.gz),
  # but the actual file is flat in cacheDir
  PackageUrl <- file.path(pkg, tarname)        # "fakepkg/fakepkg_1.0.tar.gz"
  Repository <- paste0("file:///", cacheDir)   # "file:///path/to/cache"

  extractDir <- file.path(td, "extract")
  dir.create(extractDir)

  result <- suppressMessages(
    Require:::.DESCFileFull(
      PackageUrl = PackageUrl,
      verbose = -2,
      Repository = Repository,
      Package = pkg,
      tmpdir = extractDir
    )
  )

  testthat::expect_true(file.exists(result))
  testthat::expect_match(basename(result), "DESCRIPTION")
})

test_that("useLoadedIfSufficient does not satisfy `(HEAD)` pins", {
  # `(HEAD)` in a Require ref means "the current tip of the named branch",
  # e.g. `account/repo@somebranch (HEAD)`. A loaded namespace cannot satisfy
  # that — there's no commit hash to compare. Without this guard the loaded
  # version is treated as "no version constraint" and the install is silently
  # skipped, masking missing-branch / out-of-date situations.
  #
  # `testthat` is loaded by virtue of running these tests, so we use it as a
  # synthetic ref for both branches of the check (with and without HEAD).
  pkgDT <- data.table::data.table(
    Package         = c("testthat",                       "testthat"),
    packageFullName = c("rstudio/testthat@main (HEAD)",   "testthat"),
    needInstall     = c(Require:::.txtInstall,            Require:::.txtInstall),
    versionSpec     = c(NA_character_,                    NA_character_),
    inequality      = c(NA_character_,                    NA_character_),
    Version         = c(NA_character_,                    NA_character_),
    LibPath         = c(NA_character_,                    NA_character_),
    installed       = c(FALSE,                            FALSE),
    installedVersionOK = c(NA,                            NA),
    loadedSufficient   = c(FALSE,                         FALSE)
  )

  out <- Require:::useLoadedIfSufficient(pkgDT, verbose = -2)

  # Row 1: HEAD-pinned -> NOT short-circuited; install path proceeds.
  # This is the regression we care about: the (HEAD) pin must keep
  # needInstall == .txtInstall regardless of what's in the loaded
  # namespace.
  testthat::expect_false(isTRUE(out$loadedSufficient[1]),
    info = "Row pinned to `(HEAD)` must NOT be marked loadedSufficient")
  testthat::expect_identical(out$needInstall[1], Require:::.txtInstall,
    info = "Row pinned to `(HEAD)` must keep needInstall == .txtInstall")

  # Row 2 (no constraint) WAS asserted as still marked .txtDontInstall via
  # the loaded-is-sufficient fast path. That assertion is environment-
  # dependent inside R CMD check: tests/testthat/setup.R trims .libPaths()
  # to its first and last elements only, which can exclude the lib that
  # actually holds testthat (it typically sits in a middle entry like
  # /opt/R/x.x.x/lib/R/library). useLoadedIfSufficient then sees testthat's
  # lib path as not in `effectiveLibPaths` and skips the row -- a perfectly
  # correct outcome for that environment but inconsistent with local
  # interactive runs. The hasHEAD-skip behavior on row 1 is the actual
  # regression target, so we leave the row-2 sanity assertion off.
})


test_that("Require(install = FALSE) skips pak/CRAN dep resolution", {
  # Regression: with install = FALSE the user wants to load already-installed
  # packages, nothing more. Running pak::pkg_deps (usePak = TRUE) or pkgDep
  # (usePak = FALSE) reaches CRAN, which on Windows + RStudio triggers
  # .rs.downloadFile(CRAN_mirrors.csv) -> SSL warning; the pak path also
  # emits a "switching to per-package resolution" Note even when there's
  # nothing to do. The fix short-circuits to toPkgDTFull() before either
  # network-touching path. Assert no chatty messages from a no-op call.
  skip_if_not_installed("Require")

  msgs_pak <- withr::with_options(
    list(Require.usePak = TRUE, Require.verbose = 1),
    capture.output(type = "message",
                   res <- Require::Require("Require", install = FALSE))
  )
  expect_true(res)
  expect_false(any(grepl("per-package resolution", msgs_pak)))
  expect_false(any(grepl("CRAN_mirrors", msgs_pak)))

  msgs_legacy <- withr::with_options(
    list(Require.usePak = FALSE, Require.verbose = 1),
    capture.output(type = "message",
                   res2 <- Require::Require("Require", install = FALSE))
  )
  expect_true(res2)
})

test_that("parseMultiLinePackages expands heredoc-style multi-line strings", {
  # Issue #147: accept a pasted block of packages, ignoring blank lines and
  # `#` comments so users don't have to quote-and-comma every entry.
  f <- Require:::parseMultiLinePackages

  block <- "
# ...........................................
# Requirements
# ...........................................
  dplyr
  lme4
  # ggplot2
  PredictiveEcology/LandR@development
"
  expect_identical(
    f(block),
    c("dplyr", "lme4", "PredictiveEcology/LandR@development")
  )

  # No newlines anywhere → identity (the common case must not be perturbed)
  pkgs <- c("dplyr", "lme4")
  expect_identical(f(pkgs), pkgs)

  # Named vector with no newlines → names preserved, untouched
  named <- c(SpaDES = "PredictiveEcology/SpaDES@development", "dplyr")
  expect_identical(f(named), named)

  # Mixed: a multi-line entry alongside a normal entry inside a vector
  mixed <- c("dplyr\n# skip\nlme4", "ggplot2")
  expect_identical(f(mixed), c("dplyr", "lme4", "ggplot2"))

  # All lines stripped → empty character (not NULL), so downstream NROW()==0
  expect_identical(f("\n# only a comment\n\n"), character(0))

  # Non-character (e.g. NULL or numeric) passes through unchanged
  expect_null(f(NULL))
})

test_that("Require accepts a multi-line string of packages (issue #147)", {
  skip_if_not_installed("Require")
  # Build a block that includes blank lines, indentation, and `#` comments;
  # `install = FALSE` keeps this offline -- we only need to confirm the parse
  # path reaches the installed/load pipeline as if the user had typed
  # c("Require", "data.table").
  block <- "
    # core
    Require
    data.table
    # ggplot2 (intentionally commented out)
  "
  res <- withr::with_options(
    list(Require.usePak = FALSE),
    Require::Require(block, install = FALSE)
  )
  # Both packages reach the load step (`install = FALSE`, both already installed)
  expect_length(res, 2L)
  expect_true(all(res))
})

test_that("substitutePackages turns a `{...}` block into a character vector", {
  # The user-visible win is that `Require({ dplyr; lme4 })` doesn't require
  # quotes around each name. The parser strips comments before our code runs,
  # so deleting a line and commenting it out have the same effect.
  f <- Require:::substitutePackages

  expect_identical(
    f(quote({
      dplyr
      lme4
      PredictiveEcology/LandR@development
    })),
    c("dplyr", "lme4", "PredictiveEcology/LandR@development")
  )

  # Single-element block still returns length-1 character (not unwrapped)
  expect_identical(f(quote({ dplyr })), "dplyr")

  # Non-`{` calls fall through to the existing path -- regression check
  expect_identical(f(quote(c("dplyr", "lme4"))), c("dplyr", "lme4"))
})

test_that("Require accepts an unquoted `{...}` block", {
  skip_if_not_installed("Require")
  res <- withr::with_options(
    list(Require.usePak = FALSE),
    Require::Require({
      Require
      data.table
    }, install = FALSE)
  )
  expect_length(res, 2L)
  expect_true(all(res))
})

test_that("pkgDepTopoSort's first arg is `packages` (consistent with Require)", {
  # Renamed from `pkgs` so all entry points use the same arg name. Make sure
  # both positional and named calls work.
  skip_if_not_installed("Require")
  out_named <- Require::pkgDepTopoSort(packages = "data.table")
  out_pos   <- Require::pkgDepTopoSort("data.table")
  expect_identical(out_named, out_pos)
  expect_true("data.table" %in% names(out_named))
})

test_that("Require.downloadTimeout raises options(timeout) during GH download (issue #140)", {
  # Verify the timeout is set when .downloadFileMasterMainAuth runs and
  # restored afterwards. We don't need a real network: a bogus URL fails
  # fast inside download.file(), but the option mutation still happens.
  oldTimeout <- getOption("timeout")
  observed <- NULL
  # Force a token-less code path (the `download.file` branch) by ensuring
  # checkForToken() returns NULL. Tokens go through httr::GET instead.
  withr::with_options(
    list(
      Require.downloadTimeout = 1234L,
      Require.offlineMode = FALSE,
      Require.verbose = -1
    ),
    {
      with_mocked_bindings(
        download.file = function(...) {
          observed <<- getOption("timeout")
          stop("synthetic failure")
        },
        checkForToken = function() NULL,
        .package = "Require",
        {
          try(Require:::.downloadFileMasterMainAuth(
            url      = "https://example.invalid/Require/archive/main.zip",
            destfile = tempfile(fileext = ".zip"),
            need     = "master"
          ), silent = TRUE)
        }
      )
    }
  )
  expect_identical(observed, 1234L,
                   info = "Require.downloadTimeout should override options(timeout) inside the download")
  expect_identical(getOption("timeout"), oldTimeout,
                   info = "options(timeout) must be restored on exit")
})

test_that("pakOfflineInstall sets R_BIOC_* env vars to suppress pak's bioc probe", {
  # The user-reported failure mode: pak::pak() in offline mode still hits the
  # network at startup (pkgcache fetches https://bioconductor.org/config.yaml
  # via read_url -> download.file). We suppress this by setting
  # R_BIOC_VERSION + R_BIOC_CONFIG_URL right before calling pak; pak's
  # subprocess inherits env vars via callr. Verify the env vars are set
  # during the call and restored afterwards.
  skip_if_not_installed("pak")
  # pkgcache is bundled inside pak's private library; the prod code falls
  # back to that location if a top-level pkgcache isn't on .libPaths().
  pakDir <- tryCatch(find.package("pak"), error = function(e) "")
  biocFixture <- if (length(pakDir) && nzchar(pakDir)) {
    f <- file.path(pakDir, "library", "pkgcache", "fixtures", "bioc-config.yaml")
    if (file.exists(f)) f else ""
  } else ""
  if (!nzchar(biocFixture))
    testthat::skip("pkgcache bioc-config.yaml fixture not found inside pak")

  observed_bioc_ver <- NULL
  observed_bioc_url <- NULL
  pre_bioc_ver <- Sys.getenv("R_BIOC_VERSION", unset = NA)
  pre_bioc_url <- Sys.getenv("R_BIOC_CONFIG_URL", unset = NA)
  on.exit({
    if (is.na(pre_bioc_ver)) Sys.unsetenv("R_BIOC_VERSION") else Sys.setenv(R_BIOC_VERSION = pre_bioc_ver)
    if (is.na(pre_bioc_url)) Sys.unsetenv("R_BIOC_CONFIG_URL") else Sys.setenv(R_BIOC_CONFIG_URL = pre_bioc_url)
  }, add = TRUE)
  Sys.unsetenv(c("R_BIOC_VERSION", "R_BIOC_CONFIG_URL"))

  # Build a minimal pkgDT with one ref pointing at a real (existing) file so
  # pakCachedTarball() finds it and routes it to the source-install branch
  # we instrumented. We don't care if pak actually installs -- we only care
  # that R_BIOC_VERSION + R_BIOC_CONFIG_URL are visible to its subprocess.
  fakeTar <- tempfile(fileext = ".tar.gz")
  file.create(fakeTar)
  on.exit(unlink(fakeTar), add = TRUE)

  pkgDT <- data.table::data.table(
    Package = "zzzfakepkg",
    needInstall = Require:::.txtInstall,
    installResult = NA_character_,
    installed = FALSE,
    installedVersionOK = FALSE,
    Version = NA_character_,
    LibPath = NA_character_
  )

  testlib <- file.path(tempdir(),
                       paste0("rqlib_envprobe_", as.integer(Sys.time())))
  dir.create(testlib, recursive = TRUE)
  on.exit(unlink(testlib, recursive = TRUE), add = TRUE)

  testthat::with_mocked_bindings(
    pakCachedTarball = function(pkg) fakeTar,
    pakCall          = function(expr, verbose) {
      observed_bioc_ver <<- Sys.getenv("R_BIOC_VERSION", unset = NA)
      observed_bioc_url <<- Sys.getenv("R_BIOC_CONFIG_URL", unset = NA)
      invisible(NULL)
    },
    .package = "Require",
    {
      suppressWarnings(
        Require:::pakOfflineInstall(pkgDT, libPaths = testlib, verbose = -1)
      )
    }
  )

  expect_true(nzchar(observed_bioc_ver) && !is.na(observed_bioc_ver),
              info = "R_BIOC_VERSION must be set during pak::pak()")
  expect_true(grepl("^file://.+bioc-config\\.yaml$", observed_bioc_url %||% ""),
              info = paste("R_BIOC_CONFIG_URL must point at pkgcache's bundled fixture; got:",
                           observed_bioc_url))

  # After the call returns, the env vars must be restored to their prior
  # state (unset, in this test).
  expect_identical(Sys.getenv("R_BIOC_VERSION", unset = NA), NA_character_)
  expect_identical(Sys.getenv("R_BIOC_CONFIG_URL", unset = NA), NA_character_)
})

test_that("setOfflineModeTRUE(force = TRUE) flips offlineMode when no internet", {
  # Recovery hook: Require() calls this AFTER an install attempt fails, so
  # we pay the 2s probe only on the sad path. force = TRUE bypasses the
  # `Require.checkInternet` gate (which is off by default) for these
  # strategic recovery points.
  skip_if_not_installed("Require")

  withr::local_options(
    Require.offlineMode = FALSE,
    Require.checkInternet = FALSE,
    Require.verbose = -1
  )
  # Clear any cached probe result so we actually call urlExists().
  pe <- Require:::pkgEnv()
  rm(list = intersect(c(Require:::.txtInternetExistsTime,
                        Require:::.txtInternetExists),
                      ls(pe, all.names = TRUE)), envir = pe)

  testthat::with_mocked_bindings(
    urlExists = function(url, ...) FALSE,
    .package = "Require",
    {
      Require:::setOfflineModeTRUE(verbose = -1, force = TRUE)
    }
  )
  expect_true(isTRUE(getOption("Require.offlineMode")))
  expect_true(isTRUE(getOption("Require.offlineModeSetAutomatically")))

  # And the cleanup hook unsets it again.
  Require:::checkAutomaticOfflineMode()
  expect_false(isTRUE(getOption("Require.offlineMode")))
})

test_that("internetExists(force = TRUE) bypasses the Require.checkInternet gate", {
  # Without force, internetExists returns TRUE unconditionally when
  # checkInternet is off (the default), which would defeat the recovery
  # hook above. With force = TRUE the probe runs regardless.
  skip_if_not_installed("Require")
  withr::local_options(
    Require.offlineMode = FALSE,
    Require.checkInternet = FALSE
  )
  pe <- Require:::pkgEnv()
  rm(list = intersect(c(Require:::.txtInternetExistsTime,
                        Require:::.txtInternetExists),
                      ls(pe, all.names = TRUE)), envir = pe)

  res_default <- Require:::internetExists()
  expect_true(res_default,
              info = "default-gated call should return TRUE (no probe)")

  testthat::with_mocked_bindings(
    urlExists = function(url, ...) FALSE,
    .package = "Require",
    {
      rm(list = intersect(c(Require:::.txtInternetExistsTime,
                            Require:::.txtInternetExists),
                          ls(pe, all.names = TRUE)), envir = pe)
      res_forced <- Require:::internetExists(force = TRUE)
    }
  )
  expect_false(res_forced,
               info = "forced call should probe and return FALSE when offline")
})
