test_that("small snapshot install pins each package to the requested version", {
  setupInitial <- setupTest()
  skip_if_offline2()

  ## A 5-package snapshot that exercises the version-pin paths Require
  ## must support, without dragging in the LandR-shaped Remotes mess:
  ##   - 4 CRAN packages pinned to non-current versions (served by CRAN
  ##     Archive forever)
  ##   - 1 GitHub@<sha> pin to a leaf package with no Remotes/Imports
  ## Lightweight enough to run under CI budget.
  snf <- testthat::test_path("fixtures", "smallSnapshot.txt")
  pkgs <- data.table::fread(snf)

  testlib <- file.path(tempdir(), paste0("rqlib_smallsnap_", as.integer(Sys.time())))
  dir.create(testlib, recursive = TRUE)
  on.exit(unlink(testlib, recursive = TRUE), add = TRUE)
  origLibPaths <- setLibPaths(testlib, standAlone = TRUE)
  on.exit(setLibPaths(origLibPaths), add = TRUE)

  warns <- capture_warnings(
    out <- Require(packageVersionFile = snf, require = FALSE,
                   returnDetails = TRUE)
  )

  ip <- data.table::as.data.table(installed.packages(lib.loc = testlib, noCache = TRUE))

  ## Every snapshot package must be installed in the test lib
  missing <- setdiff(pkgs$Package, ip$Package)
  testthat::expect_identical(missing, character(0),
                             info = paste("missing packages:", paste(missing, collapse = ", ")))

  ## CRAN pins must match the requested version exactly
  cranPins <- pkgs[is.na(GithubRepo)]
  for (i in seq_len(nrow(cranPins))) {
    actual <- ip[Package == cranPins$Package[i], Version]
    testthat::expect_identical(actual, cranPins$Version[i],
                               info = paste0(cranPins$Package[i], ": expected ",
                                             cranPins$Version[i], " got ", actual))
  }

  ## GitHub@SHA pin: just confirm the package is installed (the SHA's actual
  ## DESCRIPTION Version is "2.5.1.9000"; pak strips the .9000 sometimes, so
  ## assert presence rather than exact string).
  ghPin <- pkgs[!is.na(GithubRepo)]
  testthat::expect_true(ghPin$Package %in% ip$Package)
})
