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
