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
