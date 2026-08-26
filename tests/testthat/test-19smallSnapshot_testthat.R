test_that("small snapshot install pins each package to the requested version", {
  ## CRAN policy: tests may only download/install packages listed in Suggests.
  ## The fixture pins crayon / futile.logger / assertthat / R6 (and their
  ## deps), which are not Require Suggests -- so this is CI-only.
  skip_on_cran()
  setupInitial <- setupTest()
  skip_if_offline2()

  ## A 5-package snapshot that exercises the version-pin paths Require
  ## must support, without dragging in the LandR-shaped Remotes mess:
  ##   - 4 CRAN packages pinned to non-current versions (served by CRAN
  ##     Archive forever). None needs compilation, and neither does anything
  ##     they pull in: old packages that need a compiler are their own beast,
  ##     and the answer there is "ask for a newer version", not something
  ##     Require tries to solve.
  ##   - futile.logger imports lambda.r + futile.options, and lambda.r imports
  ##     formatR, so the set still exercises *transitive* resolution -- 2
  ##     direct deps resolving to 3 -- without dragging in a compiler.
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

  ## Once a session has hit "Please restart R" -- a namespace was loaded before
  ## a satisfying version was installed, and R cannot hot-swap it -- nothing
  ## afterwards installs reliably. In a full-suite run that state is inherited
  ## from earlier tests, and asserting on pins then reports a session problem as
  ## a pinning failure. Every other install test in this suite defers to
  ## testWarnsInUsePleaseChange() for exactly this; these two did not.
  skip_if(!testWarnsInUsePleaseChange(warns),
          paste("session cannot install reliably:",
                paste(utils::head(warns, 2), collapse = " | ")))

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
