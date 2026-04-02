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
