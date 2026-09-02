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


test_that("allInPakCache refuses the shortcut when any row has a (HEAD) pin", {
  # `(HEAD)` means "the current tip of the branch" -- which can only be
  # resolved online. Even if a cached tarball for the package exists, we
  # have no way of knowing it represents the *current* tip, so the
  # shortcut must decline and let pak's resolver hit the network.
  skip_if_not_installed("pak")
  skip_if_not_installed("data.table")

  fakeTar <- tempfile(fileext = ".tar.gz"); file.create(fakeTar)
  on.exit(unlink(fakeTar), add = TRUE)

  fakeCache <- data.frame(
    package  = "fpCompare",
    version  = "0.2.4",
    platform = "source",
    fullpath = fakeTar,
    stringsAsFactors = FALSE
  )

  pkgDT_plain <- data.table::data.table(
    Package         = "fpCompare",
    packageFullName = "PredictiveEcology/fpCompare@development",
    needInstall     = Require:::.txtInstall,
    versionSpec     = NA_character_,
    inequality      = NA_character_
  )
  pkgDT_head <- data.table::data.table(
    Package         = "fpCompare",
    packageFullName = "PredictiveEcology/fpCompare@development (HEAD)",
    needInstall     = Require:::.txtInstall,
    versionSpec     = NA_character_,
    inequality      = NA_character_
  )

  testthat::with_mocked_bindings(
    cache_list = function(...) fakeCache,
    .package = "pak",
    {
      expect_true(Require:::allInPakCache(pkgDT_plain),
                  info = "plain GitHub ref + cached tarball -> shortcut OK")
      expect_false(Require:::allInPakCache(pkgDT_head),
                   info = "(HEAD) pin -> must go online to resolve current tip")
    }
  )
})

test_that("allInPakCache honours version constraints", {
  # Cache-shortcut gate: a cached version that doesn't satisfy the user's
  # version constraint should NOT count as "in cache" -- we must go
  # online to look for a satisfying build. Verify via mocked
  # `pak::cache_list` returning a stale version.
  skip_if_not_installed("pak")
  skip_if_not_installed("data.table")

  fakeTar <- tempfile(fileext = ".tar.gz"); file.create(fakeTar)
  on.exit(unlink(fakeTar), add = TRUE)

  fakeCache <- data.frame(
    package  = "dplyr",
    version  = "1.2.1",
    platform = "source",
    fullpath = fakeTar,
    stringsAsFactors = FALSE
  )

  pkgDT_unconstrained <- data.table::data.table(
    Package     = "dplyr",
    needInstall = Require:::.txtInstall,
    versionSpec = NA_character_,
    inequality  = NA_character_
  )
  pkgDT_satisfied <- data.table::data.table(
    Package     = "dplyr",
    needInstall = Require:::.txtInstall,
    versionSpec = "1.0.0",
    inequality  = ">="
  )
  pkgDT_unsatisfied <- data.table::data.table(
    Package     = "dplyr",
    needInstall = Require:::.txtInstall,
    versionSpec = "2.0.0",
    inequality  = ">="
  )

  testthat::with_mocked_bindings(
    cache_list = function(...) fakeCache,
    .package = "pak",
    {
      expect_true(Require:::allInPakCache(pkgDT_unconstrained),
                  info = "no constraint -> cached 1.2.1 is enough")
      expect_true(Require:::allInPakCache(pkgDT_satisfied),
                  info = "1.2.1 satisfies >= 1.0.0 -> cache is enough")
      expect_false(Require:::allInPakCache(pkgDT_unsatisfied),
                   info = "1.2.1 does NOT satisfy >= 2.0.0 -> must go online")
    }
  )
})

test_that("pakOfflineInstall routes .zip/.tgz binaries through local:: refs", {
  # Regression on Windows: with PPM single-arch binaries cached
  # (`x86_64-w64-mingw32`), pak's resolver picks the CRAN multi-arch URL
  # (`i386+x86_64-w64-mingw32`) as canonical, misses the cache, and
  # re-downloads. Routing `.zip` / `.tgz` files through `local::<file>`
  # makes pak install the binary directly with no resolver involvement.
  # The vignette-rebuild problem only applies to `.tar.gz`, so those keep
  # the bare-ref pak path.
  skip_if_not_installed("pak")
  skip_if_not_installed("data.table")

  testlib <- file.path(tempdir(),
                       paste0("rqlib_local_zip_", as.integer(Sys.time())))
  dir.create(testlib, recursive = TRUE)
  on.exit(unlink(testlib, recursive = TRUE), add = TRUE)

  zipPath <- tempfile(fileext = ".zip")
  tgzPath <- tempfile(fileext = ".tgz")
  tarPath <- tempfile(fileext = ".tar.gz")
  file.create(c(zipPath, tgzPath, tarPath))
  on.exit(unlink(c(zipPath, tgzPath, tarPath)), add = TRUE)

  pkgDT <- data.table::data.table(
    Package         = c("pkgZip", "pkgTgz", "pkgTar"),
    packageFullName = c("pkgZip", "pkgTgz", "pkgTar"),
    needInstall     = Require:::.txtInstall,
    installResult   = NA_character_,
    installed       = FALSE,
    installedVersionOK = FALSE,
    Version         = NA_character_,
    LibPath         = NA_character_
  )

  captured_refs <- NULL
  testthat::with_mocked_bindings(
    pakCachedTarball = function(pkg, ...) {
      switch(pkg,
        pkgZip = list(path = zipPath, is_binary = TRUE),
        pkgTgz = list(path = tgzPath, is_binary = TRUE),
        pkgTar = list(path = tarPath, is_binary = FALSE),
        NULL)
    },
    pakCall = function(expr, verbose) {
      cl <- substitute(expr)
      captured_refs <<- eval(cl[[2L]], envir = parent.frame())
      invisible(NULL)
    },
    .package = "Require",
    {
      suppressWarnings(suppressMessages(
        Require:::pakOfflineInstall(pkgDT, libPaths = testlib, verbose = -1)
      ))
    }
  )

  expect_identical(captured_refs[1], paste0("local::", zipPath),
                   info = ".zip must be passed to pak as local::<file>")
  expect_identical(captured_refs[2], paste0("local::", tgzPath),
                   info = ".tgz must be passed to pak as local::<file>")
  expect_identical(captured_refs[3], "pkgTar",
                   info = ".tar.gz must be passed as a bare ref (no local::)")
})

test_that("pakOfflineInstall pins source .tar.gz refs to the cached version", {
  # Regression: snapshot install of `fpCompare (==0.2.2)` was getting
  # fpCompare 0.2.4 because the parenthetical constraint was stripped
  # to a bare `fpCompare` ref and pak then installed the latest CRAN
  # version. Fix: when `pakCachedTarball()` returns a `version`, the
  # source-tarball ref becomes `pkg@<version>` so pak resolves to
  # exactly the cached version. GitHub `account/repo@SHA` refs are
  # preserved separately.
  skip_if_not_installed("pak")
  skip_if_not_installed("data.table")

  testlib <- file.path(tempdir(),
                       paste0("rqlib_pin_version_", as.integer(Sys.time())))
  dir.create(testlib, recursive = TRUE)
  on.exit(unlink(testlib, recursive = TRUE), add = TRUE)

  tarPath <- tempfile(fileext = ".tar.gz"); file.create(tarPath)
  on.exit(unlink(tarPath), add = TRUE)

  pkgDT <- data.table::data.table(
    Package         = c("fpCompare", "PredictiveEcology/fpCompare@SHA",
                        "withVersion"),
    packageFullName = c("fpCompare (==0.2.2)",
                        "PredictiveEcology/fpCompare@abc123",
                        "withVersion"),  # unconstrained, fallback path
    needInstall     = Require:::.txtInstall,
    installResult   = NA_character_,
    installed       = FALSE,
    installedVersionOK = FALSE,
    Version         = NA_character_,
    LibPath         = NA_character_
  )

  captured_refs <- NULL
  testthat::with_mocked_bindings(
    pakCachedTarball = function(pkg, ...) {
      switch(pkg,
        fpCompare = list(path = tarPath, is_binary = FALSE,
                         version = "0.2.2"),
        `PredictiveEcology/fpCompare@SHA` =
          list(path = tarPath, is_binary = FALSE, version = "0.2.2"),
        withVersion = list(path = tarPath, is_binary = FALSE,
                           version = "1.2.3"),
        NULL)
    },
    pakCall = function(expr, verbose) {
      cl <- substitute(expr)
      captured_refs <<- eval(cl[[2L]], envir = parent.frame())
      invisible(NULL)
    },
    .package = "Require",
    {
      suppressWarnings(suppressMessages(
        Require:::pakOfflineInstall(pkgDT, libPaths = testlib, verbose = -1)
      ))
    }
  )

  expect_identical(captured_refs[1], "fpCompare@0.2.2",
                   info = paste("CRAN-style `pkg (==X)` must become",
                                "`pkg@X` (preserves the pin); got:",
                                captured_refs[1]))
  expect_identical(captured_refs[2], "PredictiveEcology/fpCompare@abc123",
                   info = paste("GitHub `account/repo@SHA` must be preserved;",
                                "got:", captured_refs[2]))
  expect_identical(captured_refs[3], "withVersion@1.2.3",
                   info = paste("unconstrained CRAN ref must pin to the",
                                "cached version; got:", captured_refs[3]))
})

test_that("pakOfflineInstall strips parenthetical version specs before pak", {
  # Regression: pak rejects Require-internal refs of the form
  # `pkg (>= 1.3.2)` with "Cannot parse package: glue (>= 1.3.2)". The
  # offline install path passes `packageFullName` to pak::pak() but must
  # first strip the parenthetical constraint -- pak understands `pkg@ver`
  # exact pins but not parenthetical inequalities.
  skip_if_not_installed("pak")
  skip_if_not_installed("data.table")

  testlib <- file.path(tempdir(),
                       paste0("rqlib_parsable_refs_", as.integer(Sys.time())))
  dir.create(testlib, recursive = TRUE)
  on.exit(unlink(testlib, recursive = TRUE), add = TRUE)

  fakeTar <- tempfile(fileext = ".tar.gz"); file.create(fakeTar)
  on.exit(unlink(fakeTar), add = TRUE)

  pkgDT <- data.table::data.table(
    Package         = "glue",
    packageFullName = "glue (>= 1.3.2)",  # the form that broke pak
    needInstall     = Require:::.txtInstall,
    installResult   = NA_character_,
    installed       = FALSE,
    installedVersionOK = FALSE,
    Version         = NA_character_,
    LibPath         = NA_character_
  )

  captured_refs <- NULL
  testthat::with_mocked_bindings(
    pakCachedTarball = function(pkg, ...) list(path = fakeTar, is_binary = TRUE),
    pakCall = function(expr, verbose) {
      # Capture the refs argument from pak::pak's unevaluated call.
      cl <- substitute(expr)
      captured_refs <<- eval(cl[[2L]], envir = parent.frame())
      invisible(NULL)
    },
    .package = "Require",
    {
      suppressWarnings(suppressMessages(
        Require:::pakOfflineInstall(pkgDT, libPaths = testlib, verbose = -1)
      ))
    }
  )

  # `%||%` only became base in R 4.4 -- inline the fallback so this test
  # runs on oldrel-3 (R 4.3.x).
  refsForCheck <- if (is.null(captured_refs)) "" else captured_refs
  expect_false(any(grepl("\\(", refsForCheck)),
               info = paste("ref passed to pak::pak must not carry the",
                            "parenthetical version constraint; got:",
                            paste(captured_refs, collapse = ", ")))
  expect_identical(captured_refs, "glue",
                   info = paste("expected bare ref 'glue'; got:",
                                paste(captured_refs, collapse = ", ")))
})

test_that("useLoadedIfSufficient refuses to short-circuit when files were removed", {
  # Regression on Windows: in the same R session, after `remove.packages()`
  # the namespace stays in `loadedNamespaces()` and `system.file(package=p)`
  # still returns the path where the package WAS installed. The previous
  # logic (loaded + libPath-in-effective) marked the row as
  # `loadedSufficient = TRUE`, install was skipped, and downstream
  # installed.packages() then warned about missing DESCRIPTIONs.
  #
  # The fix: also verify the DESCRIPTION actually exists on disk inside one
  # of the effective lib paths. Here we point `libPaths` at a fresh tempdir
  # and mock `system.file` so the libPath-membership check passes -- this
  # exercises ONLY the new disk-presence check.
  testlib <- file.path(tempdir(),
                       paste0("rqlib_loaded_removed_", as.integer(Sys.time())))
  dir.create(testlib, recursive = TRUE)
  on.exit(unlink(testlib, recursive = TRUE), add = TRUE)

  pkgDT <- data.table::data.table(
    Package          = "testthat",
    packageFullName  = "testthat",
    needInstall      = Require:::.txtInstall,
    versionSpec      = NA_character_,
    inequality       = NA_character_,
    Version          = NA_character_,
    LibPath          = NA_character_,
    installed        = FALSE,
    installedVersionOK = NA,
    loadedSufficient = FALSE
  )

  # Make system.file(package = "testthat") report testlib so the libPath
  # check inside useLoadedIfSufficient passes. The DESCRIPTION file at
  # file.path(testlib, "testthat", "DESCRIPTION") deliberately does NOT
  # exist -- that's the regression scenario.
  testthat::local_mocked_bindings(
    system.file = function(..., package = NULL) {
      if (!is.null(package) && identical(package, "testthat")) {
        return(file.path(testlib, "testthat"))
      }
      args <- list(..., package = package)
      do.call(base::system.file, args[!vapply(args, is.null, logical(1))])
    },
    .package = "base"
  )

  out <- Require:::useLoadedIfSufficient(pkgDT, libPaths = testlib, verbose = -2)

  expect_false(isTRUE(out$loadedSufficient[1]),
               info = paste("Row whose namespace is loaded but DESCRIPTION",
                            "is missing from disk must NOT be marked loadedSufficient"))
  expect_identical(out$needInstall[1], Require:::.txtInstall,
                   info = "Row must remain flagged for reinstall")
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
  #
  # `standAlone = FALSE` is needed when this test runs under covr (or any
  # fresh-install scenario, e.g. CI test-coverage on macOS): covr installs
  # Require into a private tempdir, but Require's deps (data.table) sit in
  # the runner's site-library. The default `Require.standAlone = TRUE`
  # would constrain `libPaths` to that tempdir, hide data.table, mark it for
  # reinstall, hit the "Can't install Require dependency" guard, and finally
  # make `require()` return FALSE -- failing `all(res)` despite the parser
  # working correctly. Forcing the shared-libs mode keeps the test focused
  # on the parser, which is what issue #147 was about.
  block <- "
    # core
    Require
    data.table
    # ggplot2 (intentionally commented out)
  "
  res <- withr::with_options(
    list(Require.usePak = FALSE, Require.standAlone = FALSE),
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
  # See multi-line-string test above for why `Require.standAlone = FALSE` is
  # set: same fresh-install-libPath interaction.
  res <- withr::with_options(
    list(Require.usePak = FALSE, Require.standAlone = FALSE),
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

test_that("pakCachedTarball routes Linux PPM binaries through binary install", {
  # Regression for the user-reported Linux failure: PPM binary tarballs
  # share the bare `pkg_ver.tar.gz` filename with their source counterparts.
  # The old filename heuristic misclassified them as source, fed them to
  # pak as `local::<file>` refs, and pak then tried to R-CMD-BUILD them
  # offline (which rebuilds vignettes and fails). The fix uses
  # pak::cache_list()'s `platform` column instead, which is authoritative.
  skip_if_not_installed("pak")
  skip_if_not_installed("data.table")

  # Build a fake pak cache_list with both a source AND a Linux PPM binary
  # for the same package + version. Use the running R's arch so the
  # is_binary predicate matches.
  arch <- R.version$arch
  ppmPlatform <- paste0(arch, "-pc-linux-gnu-ubuntu-24.04")

  srcPath <- tempfile(pattern = "src_", fileext = ".tar.gz")
  binPath <- tempfile(pattern = "bin_", fileext = ".tar.gz")
  file.create(srcPath); file.create(binPath)
  on.exit(unlink(c(srcPath, binPath)), add = TRUE)
  # Make the binary newer so it would win the mtime tiebreak; the platform
  # filter should pick it regardless of mtime, but this also exercises the
  # mtime path within the binary subset.
  Sys.setFileTime(binPath, Sys.time())
  Sys.setFileTime(srcPath, Sys.time() - 3600)

  fakeCache <- data.frame(
    package  = c("dplyr", "dplyr"),
    version  = c("1.2.1", "1.2.1"),
    platform = c("source", ppmPlatform),
    fullpath = c(srcPath,  binPath),
    stringsAsFactors = FALSE
  )

  testthat::with_mocked_bindings(
    cache_list = function(...) fakeCache,
    .package = "pak",
    {
      out <- Require:::pakCachedTarball("dplyr")
    }
  )

  expect_type(out, "list")
  expect_identical(out$path, binPath,
                   info = "must prefer the platform-matching binary over source")
  expect_true(out$is_binary,
              info = "must report is_binary = TRUE for the PPM binary")

  # Now drop the binary row and confirm source is correctly detected
  testthat::with_mocked_bindings(
    cache_list = function(...) fakeCache[1, , drop = FALSE],
    .package = "pak",
    {
      out2 <- Require:::pakCachedTarball("dplyr")
    }
  )
  expect_identical(out2$path, srcPath)
  expect_false(out2$is_binary)
})

test_that("pakOfflineInstall distinguishes 'not in cache' from 'install failed'", {
  # The old single-warning text ("offline mode and not in pak cache") was
  # actively misleading when packages were in the cache but the install
  # step failed. Split into two warnings so the user knows which case
  # they're in.
  skip_if_not_installed("pak")
  skip_if_not_installed("data.table")

  testlib <- file.path(tempdir(),
                       paste0("rqlib_split_warn_", as.integer(Sys.time())))
  dir.create(testlib, recursive = TRUE)
  on.exit(unlink(testlib, recursive = TRUE), add = TRUE)

  fakeTar <- tempfile(fileext = ".tar.gz"); file.create(fakeTar)
  on.exit(unlink(fakeTar), add = TRUE)

  pkgDT <- data.table::data.table(
    Package = c("inCache", "notInCache"),
    needInstall = Require:::.txtInstall,
    installResult = NA_character_,
    installed = FALSE,
    installedVersionOK = FALSE,
    Version = NA_character_,
    LibPath = NA_character_
  )

  warnings_seen <- character()
  # Pretend pak ran but installed nothing -- so the ground-truth check
  # finds the supposedly-cached pkg still missing on disk -- the "install
  # failed" branch we want to exercise.
  testthat::with_mocked_bindings(
    pakCachedTarball = function(pkg, ...) {
      if (pkg == "inCache")    list(path = fakeTar, is_binary = FALSE)
      else                     NULL
    },
    pakCall = function(expr, verbose) invisible(NULL),
    .package = "Require",
    {
      withCallingHandlers(
        suppressMessages(
          Require:::pakOfflineInstall(pkgDT, libPaths = testlib, verbose = -1)
        ),
        warning = function(w) {
          warnings_seen <<- c(warnings_seen, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
    }
  )

  expect_true(any(grepl("notInCache", warnings_seen) &
                  grepl("not in pak cache", warnings_seen, fixed = TRUE)),
              info = "expected a 'not in pak cache' warning naming notInCache")
  expect_true(any(grepl("inCache", warnings_seen) &
                  grepl("tarball was in pak cache but offline install failed",
                        warnings_seen, fixed = TRUE)),
              info = "expected a separate 'install failed' warning naming inCache")
})

test_that("extractMissingSysreqs parses pak's 'Missing N system packages' block", {
  # Regression for the infinite-retry symptom: when pak reports missing
  # system packages, identify-and-defer's dep resolver re-includes the
  # failing pkg in every retry plan (because dependents still reference
  # it), so the loop ping-pongs forever. Detecting the block lets the
  # loop terminate with an actionable error.
  output <- c(
    "  + fs          2.1.0  [bld][cmp] + ✖ cmake, ✔ make, ✖ libuv1-dev",
    "✖ Missing 2 system packages. You'll probably need to install them manually:",
    "+ cmake       - fs",
    "+ libuv1-dev  - fs",
    "i No downloads are needed, 80 pkgs (51.17 MB) are cached"
  )
  out <- Require:::extractMissingSysreqs(output)
  expect_named(out, c("fs", "fs"))
  expect_setequal(unname(out), c("cmake", "libuv1-dev"))

  # Multi-pkg form: one sysreq needed by several packages
  output2 <- c(
    "✖ Missing 1 system packages. You'll probably need to install them manually:",
    "+ libssl-dev  - curl, openssl"
  )
  out2 <- Require:::extractMissingSysreqs(output2)
  expect_named(out2, c("curl", "openssl"))
  expect_true(all(unname(out2) == "libssl-dev"))

  # No block -> empty
  expect_identical(Require:::extractMissingSysreqs("nothing relevant"), character(0))
  expect_identical(Require:::extractMissingSysreqs(character(0)), character(0))
})

test_that("whIsOfficialCRANrepo does not leak 'cannot open file' warning when .mirrors.csv is absent", {
  # Regression: when the cached .mirrors.csv is absent AND download.file fails
  # (offline / fresh cache), `try(read.csv(...), silent = TRUE)` swallowed the
  # error but `file()` signals a "cannot open file" warning before erroring,
  # which escaped try(). That warning was caught by callers' withCallingHandlers
  # (e.g. SpaDES.project::setupProject) whose handlers probed the call stack
  # for `pkgDT` -- a variable that doesn't exist on the pak code path --
  # crashing with: Error in get(obj, envir = env, inherits = FALSE).

  tmpCache <- tempfile("requireMirrorsTest")
  dir.create(tmpCache)
  on.exit(unlink(tmpCache, recursive = TRUE), add = TRUE)

  caught <- character()
  withr::with_envvar(
    c(R_REQUIRE_CACHE = tmpCache),
    {
      with_mocked_bindings(
        download.file = function(...) stop("simulated offline"),
        .package = "Require",
        {
          withCallingHandlers(
            try(Require:::whIsOfficialCRANrepo(
              currentRepos = c(CRAN = "https://cloud.r-project.org")
            ), silent = TRUE),
            warning = function(w) {
              caught <<- c(caught, conditionMessage(w))
              invokeRestart("muffleWarning")
            }
          )
        }
      )
    }
  )

  expect_false(
    any(grepl("cannot open", caught)),
    info = paste("Leaked warnings:", paste(caught, collapse = "; "))
  )
})

test_that("isBinaryCRANRepo() default arg is resilient when 'CRAN' is not a name in options(repos)", {
  # Regression: the default arg `curCRANRepo = getOption("repos")[["CRAN"]]`
  # threw "subscript out of bounds" on a named character vector when no
  # element was named "CRAN" -- triggered when caller code rebuilt
  # options("repos") in a way that dropped names, e.g.
  #   unique(c("https://predictiveecology.r-universe.dev", getOption("repos")))
  # which on a fresh session yields an unnamed character vector.

  withr::with_options(
    list(repos = c(
      "https://predictiveecology.r-universe.dev",
      "https://cloud.r-project.org"
    )),  # deliberately unnamed -- no "CRAN" name present
    {
      # Pre-fix: this errored with "subscript out of bounds".
      # Post-fix: returns without erroring; value is FALSE/NA depending on platform.
      expect_no_error(out <- Require:::isBinaryCRANRepo())
      # The result must not be TRUE -- there is no recognised CRAN entry to classify.
      expect_false(isTRUE(any(out)))
    }
  )

  # Sanity: when CRAN *is* named, the function still works (no regression).
  withr::with_options(
    list(repos = c(CRAN = "https://cloud.r-project.org")),
    {
      expect_no_error(Require:::isBinaryCRANRepo())
    }
  )
})

test_that(".pakNoCopyPkgs() includes pak, callr, processx, cli", {
  out <- Require:::.pakNoCopyPkgs()
  expect_true(all(c("pak", "callr", "processx", "cli") %in% out),
              info = paste("got:", paste(out, collapse = ", ")))
})

test_that(".preferGHrefDedup() collapses multi-form refs preferring GH > CRAN", {
  # Regression: user lists of the form
  #   c("PredictiveEcology/reproducible@development",
  #     "PredictiveEcology/reproducible",
  #     "reproducible")
  # have no versionSpec on any row, so trimRedundancies() can't dedup them.
  # All three survived into pakOfflineInstall and poisoned the pak batch
  # with `reproducible@<v>: Conflicts with reproducible@<v>`.
  refs <- c("PredictiveEcology/reproducible@development",
            "PredictiveEcology/reproducible",
            "reproducible",
            "data.table")
  expect_identical(
    Require:::.preferGHrefDedup(refs),
    c("PredictiveEcology/reproducible@development", "data.table"))

  # No GH ref present: keep first occurrence per package.
  expect_identical(
    Require:::.preferGHrefDedup(c("reproducible", "reproducible@1.2.3", "data.table")),
    c("reproducible", "data.table"))

  # Multiple GH refs for same package: keep the first (typically the most
  # specific @branch/@SHA form -- user-input order wins).
  expect_identical(
    Require:::.preferGHrefDedup(c("acct/pkg@dev", "acct/pkg", "pkg")),
    "acct/pkg@dev")

  # No-op when there are no duplicates.
  refs2 <- c("data.table", "fpCompare", "digest")
  expect_identical(Require:::.preferGHrefDedup(refs2), refs2)

  # Empty / single inputs return unchanged.
  expect_identical(Require:::.preferGHrefDedup(character(0)), character(0))
  expect_identical(Require:::.preferGHrefDedup("data.table"), "data.table")
})

test_that(".isSessionLibPath() distinguishes real project libs from ephemeral tempdirs", {
  # A path that IS in .libPaths() must be recognised as a session lib path.
  expect_true(Require:::.isSessionLibPath(.libPaths()[1]))
  # A fresh tempdir is NOT in .libPaths() and must be rejected.
  td <- tempfile("notASessionLib")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  expect_false(Require:::.isSessionLibPath(td))
  # Empty / NULL inputs are FALSE.
  expect_false(Require:::.isSessionLibPath(""))
  expect_false(Require:::.isSessionLibPath(character(0)))
  expect_false(Require:::.isSessionLibPath(NULL))
})

test_that(".pakNeedsReinstall() flags pak-not-on-.libPaths and forceReinstall opt-in", {
  # Not reachable anywhere on .libPaths()
  expect_match(Require:::.pakNeedsReinstall(character(0)), "not available on .libPaths")
  expect_match(Require:::.pakNeedsReinstall(""),           "not available on .libPaths")
  expect_match(Require:::.pakNeedsReinstall(NULL),         "not available on .libPaths")

  # Reachable on .libPaths() + no force => no install
  expect_identical(Require:::.pakNeedsReinstall("/some/path/pak"), "")
  expect_identical(Require:::.pakNeedsReinstall("/some/path/pak",
                                                 forceReinstall = FALSE), "")

  # forceReinstall opt-in (Require.forcePakReinstall) => reinstall
  expect_match(
    Require:::.pakNeedsReinstall("/some/path/pak", forceReinstall = TRUE),
    "forcePakReinstall = TRUE")
})

test_that("a damaged-pak error is detected through pak's wrapper and annotated", {
  broken <- simpleError("Native call to processx_exec failed: Command '' not found")
  expect_true(Require:::isPakBrokenInstallError(broken))
  expect_false(Require:::isPakBrokenInstallError(simpleError("some other failure")))
  expect_false(Require:::isPakBrokenInstallError(NULL))

  ## pak nests the subprocess's real error inside its own wrapper condition,
  ## so conditionMessage() alone does not see the signature.
  nested <- simpleError("! error in pak subprocess")
  nested$parent <- broken
  expect_false(grepl("processx_exec", conditionMessage(nested), fixed = TRUE))
  expect_true(Require:::isPakBrokenInstallError(nested))

  ## pakCall() appends the remedy and keeps pak's own condition intact
  e <- tryCatch(Require:::pakCall(stop(broken), verbose = -2),
                error = function(e) e)
  expect_s3_class(e, "simpleError")
  expect_match(conditionMessage(e), "processx_exec", fixed = TRUE)
  expect_match(conditionMessage(e), "Require.forcePakReinstall = TRUE", fixed = TRUE)

  ## an unrelated pak error is re-raised untouched
  e2 <- tryCatch(Require:::pakCall(stop(simpleError("plain failure")), verbose = -2),
                 error = function(e) e)
  expect_identical(conditionMessage(e2), "plain failure")
})

test_that("the pak-not-on-.libPaths hint names the cause, not the metadata cache", {
  ## Regression: this branch used to replace the error with
  ## "Try running: pak::meta_clean()", which addresses the metadata cache --
  ## not the library path that actually causes it.
  expect_match(Require:::.txtPakNotOnLibPaths, ".libPaths()", fixed = TRUE)
  expect_match(Require:::.txtPakNotOnLibPaths, "setLibPaths(", fixed = TRUE)
  expect_match(Require:::.txtPakBrokenInstall, "Require.forcePakReinstall = TRUE",
               fixed = TRUE)
})

test_that("linkOrCopyPackageFiles() excludes pak/callr/processx/cli even when asked", {
  # Regression: SpaDES.project::setupPackages (pre-pak) used to file-copy
  # the system lib into the project lib; pak's embedded callr/processx
  # native helpers don't survive that on Windows and the resulting pak
  # install dies inside processx with `Command '' not found` on the next
  # subprocess spawn. linkOrCopyPackageFiles() must therefore drop those
  # packages from the copy list so the install machinery installs them
  # fresh instead.

  # Build a synthetic installed.packages() matrix with Built that satisfies
  # Require:::correctBuilt() for the current R version (so cantClone()
  # treats every row as clone-eligible by default).
  rvDot <- Require:::RversionDot()  # e.g. "4.5."
  built <- paste0(rvDot, "0; ; ;")
  pkgs  <- c("data.table", "pak", "callr", "processx", "cli", "rlang")
  ip <- cbind(
    Package          = pkgs,
    Version          = rep("1.0.0", length(pkgs)),
    NeedsCompilation = rep("no", length(pkgs)),  # all clone-eligible by NeedsCompilation
    Built            = rep(built, length(pkgs))
  )
  rownames(ip) <- pkgs

  passedThrough <- NULL
  testthat::with_mocked_bindings(
    linkOrCopyPackageFilesInner = function(Packages, fromLib, toLib) {
      passedThrough <<- Packages
      invisible()
    },
    .package = "Require",
    {
      Require:::linkOrCopyPackageFiles(
        Packages = pkgs,
        fromLib  = tempdir(),
        toLib    = tempdir(),
        ip       = ip
      )
    }
  )

  expect_false(is.null(passedThrough),
               info = "linkOrCopyPackageFilesInner mock was never invoked")
  # pak / callr / processx / cli must NOT be cloned
  expect_false(any(c("pak", "callr", "processx", "cli") %in% passedThrough),
               info = paste("got passed through:", paste(passedThrough, collapse = ", ")))
  # The non-pak rows still go through normally
  expect_true(all(c("data.table", "rlang") %in% passedThrough))
})

test_that("ensurePakInProjectLib() is a no-op when Require.usePak is FALSE", {
  installCalled <- FALSE
  withr::with_options(
    list(Require.usePak = FALSE),
    {
      testthat::with_mocked_bindings(
        install.packages = function(...) { installCalled <<- TRUE; invisible() },
        .package = "utils",
        {
          Require:::ensurePakInProjectLib(tempfile("noPak"))
        }
      )
    }
  )
  expect_false(installCalled,
               info = "install.packages should never be called when usePak = FALSE")
})

test_that("Require.forcePakReinstall = TRUE forces reinstall even when pak is in projLib", {
  # When the user explicitly opts in via Require.forcePakReinstall (escape
  # hatch for "pak files in projLib but secretly broken from a file-copy
  # bootstrap"), ensurePakInProjectLib must call install.packages even if
  # find.package() reports pak is already there.
  installCalled <- FALSE
  installLib    <- NULL
  withr::with_options(
    list(Require.usePak = TRUE, Require.forcePakReinstall = TRUE),
    {
      testthat::with_mocked_bindings(
        install.packages = function(pkgs, lib, ...) {
          installCalled <<- TRUE
          installLib    <<- lib
          invisible()
        },
        .package = "utils",
        {
          testthat::with_mocked_bindings(
            find.package = function(...) "/fake/projLib/pak",  # pak IS in projLib
            .package = "base",
            {
              suppressMessages(Require:::ensurePakInProjectLib(
                projLib = "/fake/projLib",
                repos   = c(CRAN = "https://cloud.r-project.org"),
                verbose = -1))
            }
          )
        }
      )
    }
  )
  expect_true(installCalled,
              info = "forcePakReinstall = TRUE should trigger install.packages even if pak is in projLib")
  expect_identical(installLib, "/fake/projLib")
})

test_that("ensurePakInProjectLib() releases pak before install: unload + pakResetSubprocess", {
  # Case 4: pak is already loaded in the parent session (user ran pak::pak()
  # directly before Require::Install was called, or some other path triggered
  # pak loading). On Windows the loaded DLL blocks install.packages from
  # writing -- so ensurePakInProjectLib must first kill pak's r_session
  # (pakResetSubprocess) and unloadNamespace("pak") to release the lock.

  resetCalled  <- FALSE
  unloadCalled <- FALSE
  installCalled <- FALSE

  testthat::with_mocked_bindings(
    # Simulate "pak is loaded" -> "pak is unloaded after unloadNamespace"
    loadedNamespaces = local({
      seenUnload <- FALSE
      function() {
        if (seenUnload) character(0) else "pak"
      }
    }),
    unloadNamespace = function(...) { unloadCalled <<- TRUE; invisible() },
    .package = "base",
    {
      # Replace these *after* the outer mocks so we can capture them
      testthat::with_mocked_bindings(
        pakResetSubprocess = function() { resetCalled <<- TRUE },
        .package = "Require",
        {
          testthat::with_mocked_bindings(
            install.packages = function(...) { installCalled <<- TRUE; invisible() },
            .package = "utils",
            {
              testthat::with_mocked_bindings(
                find.package = function(...) character(0),  # pak NOT in projLib -> needs install
                .package = "base",
                {
                  # Flip the loadedNamespaces() return to "" after unloadNamespace runs.
                  # We do this by augmenting the unloadNamespace mock to flip a flag
                  # the loadedNamespaces() mock reads. Simpler: just call ensurePakInProjectLib
                  # and assert pakResetSubprocess + unloadNamespace + install.packages all fired.
                  ok <- tryCatch(
                    suppressMessages(Require:::ensurePakInProjectLib(
                      projLib = "/fake/projLib",
                      repos   = c(CRAN = "https://cloud.r-project.org"),
                      verbose = -1)),
                    error = function(e) e
                  )
                }
              )
            }
          )
        }
      )
    }
  )

  expect_true(resetCalled,
              info = "pakResetSubprocess() must be called when pak is loaded, to kill the r_session that holds the DLL")
  expect_true(unloadCalled,
              info = "unloadNamespace('pak') must be called to release the namespace + DLL")
})

test_that("ensurePakInProjectLib() stops with restart message if pak can't be unloaded", {
  # If pak is loaded and our two-step release (pakResetSubprocess +
  # unloadNamespace) fails to actually unload pak, ensurePakInProjectLib
  # must stop() with an actionable "please RESTART R" message rather than
  # silently no-op'ing and letting the user hit the same processx_exec
  # failure mid-install.

  testthat::with_mocked_bindings(
    loadedNamespaces = function() "pak",      # always reports pak is loaded
    unloadNamespace  = function(...) invisible(),  # pretends to unload but doesn't
    .package = "base",
    {
      testthat::with_mocked_bindings(
        pakResetSubprocess = function() invisible(),
        .package = "Require",
        {
          testthat::with_mocked_bindings(
            find.package = function(...) character(0),  # needs reinstall
            .package = "base",
            {
              expect_error(
                suppressMessages(Require:::ensurePakInProjectLib(
                  projLib = "/fake/projLib",
                  repos   = c(CRAN = "https://cloud.r-project.org"),
                  verbose = -1)),
                regexp = "RESTART R"
              )
            }
          )
        }
      )
    }
  )
})

test_that(".pakDepsInvalidateLast() removes the stashed key + on-disk cache file", {
  # Regression: a user upgraded Require to a version that added an
  # Imports package (e.g. processx) but the pak dep-resolution cache
  # from before the upgrade still represented the old dep graph. Pak
  # tried to build a package that needed the new dep, failed with
  # `missing-build-deps`, and the next Require call served the same
  # stale plan -- same failure, ad infinitum. .pakDepsInvalidateLast()
  # wipes the entry pakDepsResolve() most recently used so the next
  # call re-resolves.

  pe <- Require:::pakEnv()
  fakeKey <- "pakDepsTest_abc123"
  fakeEnvKey <- paste0("pakDeps_", fakeKey)

  # Seed the stash + in-memory cache + a real on-disk cache file
  assign(".lastPakDepsKey", fakeKey, envir = pe)
  assign(fakeEnvKey, list(package = "fake"), envir = pe)
  cacheDir  <- Require:::pakDepsCacheDir()
  dir.create(cacheDir, recursive = TRUE, showWarnings = FALSE)
  cacheFile <- file.path(cacheDir, paste0(fakeKey, ".rds"))
  saveRDS(list(package = "fake"), cacheFile)
  on.exit({
    rm(list = intersect(c(".lastPakDepsKey", fakeEnvKey), ls(envir = pe)),
       envir = pe)
    if (file.exists(cacheFile)) unlink(cacheFile)
  }, add = TRUE)

  expect_true(exists(fakeEnvKey, envir = pe, inherits = FALSE))
  expect_true(file.exists(cacheFile))

  invalidated <- Require:::.pakDepsInvalidateLast()
  expect_true(isTRUE(invalidated),
              info = "invalidator must report it found and cleared a key")
  expect_false(exists(fakeEnvKey, envir = pe, inherits = FALSE),
               info = "in-memory cache entry must be gone after invalidate")
  expect_false(file.exists(cacheFile),
               info = "on-disk cache file must be gone after invalidate")
  expect_false(exists(".lastPakDepsKey", envir = pe, inherits = FALSE),
               info = ".lastPakDepsKey stash must be cleared to prevent double-invalidation")
})

test_that(".pakDepsInvalidateLast() is a no-op when no key was stashed", {
  pe <- Require:::pakEnv()
  ## Defensive: ensure no stash exists
  if (exists(".lastPakDepsKey", envir = pe, inherits = FALSE))
    rm(".lastPakDepsKey", envir = pe)
  expect_false(Require:::.pakDepsInvalidateLast(),
               info = "must return FALSE when nothing to invalidate")
})

test_that(".pinSurvivorToMinimumAfterExactReject() pins survivor when an == was rejected", {
  # User listed `stringfish (==0.17.0)` AND another package required
  # `stringfish (>= 0.18.0)`. Old behaviour: drop the ==, keep the >=
  # as-is, pak fetches CRAN's LATEST (e.g. 0.19.0). New behaviour: when
  # the rejected constraint was an exact `==X` pin, pin the surviving
  # `>=Y`/`>Y` row to `==Y` so we install the minimum that satisfies
  # the floor -- as close to the user's original `==X` as we can get.
  pkgDT <- data.table::data.table(
    Package         = "stringfish",
    packageFullName = "stringfish (>= 0.18.0)",
    versionSpec     = "0.18.0",
    inequality      = ">="
  )
  rmRows <- data.table::data.table(Package = "stringfish", inequality = "==")
  out <- Require:::.pinSurvivorToMinimumAfterExactReject(pkgDT, rmRows)
  expect_identical(out$inequality, "==")
  expect_identical(out$versionSpec, "0.18.0")
  expect_identical(out$packageFullName, "stringfish (== 0.18.0)")
})

test_that(".pinSurvivorToMinimumAfterExactReject() is a no-op when no == was rejected", {
  # Pure `>=Y` conflict (no exact pin involved) should NOT be pinned --
  # absent a user-supplied `==` we have no reason to narrow.
  pkgDT <- data.table::data.table(
    Package         = "stringfish",
    packageFullName = "stringfish (>= 0.18.0)",
    versionSpec     = "0.18.0",
    inequality      = ">="
  )
  rmRows <- data.table::data.table(
    Package = "stringfish", inequality = ">=") # rejected was also a >=, not ==
  before <- data.table::copy(pkgDT)
  Require:::.pinSurvivorToMinimumAfterExactReject(pkgDT, rmRows)
  expect_identical(pkgDT, before)
})

test_that(".pinSurvivorToMinimumAfterExactReject() handles empty/malformed inputs", {
  pkgDT <- data.table::data.table(
    Package = "stringfish", packageFullName = "stringfish",
    versionSpec = NA_character_, inequality = NA_character_)
  expect_silent(Require:::.pinSurvivorToMinimumAfterExactReject(
    pkgDT, data.table::data.table()))
  expect_silent(Require:::.pinSurvivorToMinimumAfterExactReject(
    pkgDT, NULL))
})

test_that("pak is pinned to source only when the caller actually asked for it", {
  ## Require()/Install() default `type = getOption("pkgType")`, which is
  ## "source" on Linux. Keying the pin on the value alone therefore fired on
  ## every Linux install: pak resolved the whole tree as source, so every
  ## already-installed *binary* dependency stopped matching and was replanned
  ## as a source rebuild -- a plan full of `cli 3.6.6 -> 3.6.6 [bld][cmp]`.
  ## Measured on one ref whose deps were all installed: 23 same-version
  ## rebuilds and 103.8s forced, versus "kept 20, added 1" and 2.6s not forced.
  withr::local_options(list(pkg.platforms = NULL))

  ## defaulted type -- must NOT pin, whatever the platform default happens to be
  expect_null(Require:::forcePakSourceIfRequested("source", typeExplicit = FALSE))
  expect_null(getOption("pkg.platforms"))

  ## explicitly requested source -- must pin, and hand back the old value so
  ## the caller's on.exit() can restore it
  old <- Require:::forcePakSourceIfRequested("source", typeExplicit = TRUE)
  expect_identical(getOption("pkg.platforms"), "source")
  expect_true(is.list(old) && "pkg.platforms" %in% names(old))
  options(old)
  expect_null(getOption("pkg.platforms"))

  ## an explicit non-source type never pins
  expect_null(Require:::forcePakSourceIfRequested("binary", typeExplicit = TRUE))
  expect_null(getOption("pkg.platforms"))
})

test_that(".pakDropUnchangedFailures re-attempts only when something new is installed", {
  ## #190: identify-and-defer ran several phases, each deciding independently
  ## what to attempt, so a ref that cannot build was handed to pak once per
  ## phase against an identical installed set. Retrying is only useful when a
  ## dependency has landed since the last attempt.
  memo <- Require:::.pakFailMemo()
  refs <- c("any::Deriv", "any::car")
  inst1 <- c("cli", "rlang")

  ## nothing recorded yet -> everything is attempted
  expect_identical(
    Require:::.pakDropUnchangedFailures(memo, refs, inst1, verbose = -2), refs)

  Require:::.pakRecordFailures(memo, refs, inst1)

  ## same installed set -> both dropped
  expect_identical(
    Require:::.pakDropUnchangedFailures(memo, refs, inst1, verbose = -2),
    character(0))

  ## the comparison is set-wise, not order-sensitive
  expect_identical(
    Require:::.pakDropUnchangedFailures(memo, refs, rev(inst1), verbose = -2),
    character(0))

  ## one new package anywhere makes them eligible again
  expect_identical(
    Require:::.pakDropUnchangedFailures(memo, refs, c(inst1, "glue"), verbose = -2),
    refs)

  ## a ref that never failed is never dropped
  expect_identical(
    Require:::.pakDropUnchangedFailures(memo, "any::brandNew", inst1, verbose = -2),
    "any::brandNew")

  ## empty in, empty out
  expect_identical(
    Require:::.pakDropUnchangedFailures(memo, character(0), inst1, verbose = -2),
    character(0))
})

test_that("a `(HEAD)` CRAN ref does not force a reinstall of what is current", {
  ## Install() must not reinstall a package it already has -- that is the point
  ## of the package. `(HEAD)` used to force installedVersionOK = FALSE on every
  ## row carrying it, so updatePackages() (which tags every installed CRAN
  ## package `pkg (HEAD)`) asked for the whole library back: 170 same-version
  ## rebuilds where base::update.packages() correctly found 3.
  ##
  ## Nothing downstream corrected it under the default Require.usePak = TRUE:
  ## the HEAD -> dontInstall comparison lives in doDownloads(), on the legacy
  ## non-pak path only.
  mk <- function(pkg, instVer, availVer, repoLoc) {
    data.table::data.table(
      Package = pkg, Version = instVer, VersionOnRepos = availVer,
      packageFullName = paste0(pkg, " (HEAD)"), repoLocation = repoLoc,
      versionSpec = "HEAD", inequality = "")
  }

  dt <- data.table::rbindlist(list(
    mk("current",  "1.2.0", "1.2.0", "CRAN"),          # installed == newest
    mk("ahead",    "1.3.0", "1.2.0", "CRAN"),          # installed newer than repo
    mk("stale",    "1.0.0", "1.2.0", "CRAN"),          # repo has something newer
    mk("unknown",  "1.0.0", NA_character_, "CRAN"),    # cannot settle -> install
    mk("noSHA",    "1.0.0", "1.0.0", "GitHub")         # no local SHA -> install
  ))

  out <- Require:::whichToInstall(dt, install = TRUE, verbose = -2)
  needInstall <- setNames(out$needInstall, out$Package)

  expect_identical(unname(needInstall[["current"]]), Require:::.txtDontInstall)
  expect_identical(unname(needInstall[["ahead"]]),   Require:::.txtDontInstall)
  expect_identical(unname(needInstall[["stale"]]),   Require:::.txtInstall)
  expect_identical(unname(needInstall[["unknown"]]), Require:::.txtInstall)
  ## a GitHub row with no local SHA cannot be settled -> keep the old answer
  expect_identical(unname(needInstall[["noSHA"]]),   Require:::.txtInstall)
})

test_that("a `(HEAD)` GitHub ref is settled by SHA, not reinstalled blindly", {
  ## HEAD means "the newest available" for both CRAN-alikes and Git; the Git
  ## half is a SHA comparison. alreadyExistingDESCFile() is the existing
  ## implementation -- doDownloads() uses it -- but that is the legacy non-pak
  ## path, so under Require.usePak = TRUE every GitHub HEAD ref reinstalled
  ## unconditionally. Reuse, not a second implementation.
  remoteSHA <- strrep("a", 40)
  lib <- tempfile("headlib"); dir.create(lib)
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  mkInstalled <- function(pkg, sha) {
    dir.create(file.path(lib, pkg), recursive = TRUE, showWarnings = FALSE)
    writeLines(c(paste0("Package: ", pkg), "Version: 1.0.0",
                 paste0("GithubSHA1: ", sha)),
               file.path(lib, pkg, "DESCRIPTION"))
  }
  mkInstalled("atHead", remoteSHA)
  mkInstalled("behind", strrep("b", 40))

  mkGH <- function(pkg) data.table::data.table(
    Package = pkg, Version = "1.0.0", VersionOnRepos = NA_character_,
    packageFullName = paste0("acct/", pkg, "@main (HEAD)"),
    repoLocation = "GitHub", versionSpec = "HEAD", inequality = "",
    Account = "acct", Repo = pkg, Branch = "main")
  dt <- data.table::rbindlist(list(mkGH("atHead"), mkGH("behind")))

  testthat::with_mocked_bindings(
    getSHAfromGitHubMemoise = function(...) remoteSHA,
    {
      out <- Require:::whichToInstall(dt, install = TRUE, verbose = -2,
                                      libPaths = lib)
      needInstall <- setNames(out$needInstall, out$Package)
      ## local SHA == branch HEAD -> nothing to do
      expect_identical(unname(needInstall[["atHead"]]), Require:::.txtDontInstall)
      ## local SHA differs -> install
      expect_identical(unname(needInstall[["behind"]]), Require:::.txtInstall)
    },
    .package = "Require")

  ## an unreachable GitHub must not be read as "up to date"
  testthat::with_mocked_bindings(
    getSHAfromGitHubMemoise = function(...) stop("no network"),
    {
      out <- Require:::whichToInstall(dt, install = TRUE, verbose = -2,
                                      libPaths = lib)
      expect_true(all(out$needInstall == Require:::.txtInstall))
    },
    .package = "Require")
})
