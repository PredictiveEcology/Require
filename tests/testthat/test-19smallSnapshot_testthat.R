test_that("small snapshot install pins each package to the requested version", {
  ## CRAN policy: tests may only download/install packages listed in Suggests.
  ## The fixture pins packages that are not Require Suggests -- so this is
  ## CI-only.
  skip_on_cran()
  setupInitial <- setupTest()
  skip_if_offline2()

  ## A 5-package snapshot that exercises the version-pin paths Require
  ## must support:
  ##   - 4 CRAN packages pinned to non-current versions (served by CRAN
  ##     Archive forever). None needs compilation, and neither does anything
  ##     they pull in: old packages that need a compiler are their own beast,
  ##     and the answer there is "ask for a newer version", not something
  ##     Require tries to solve.
  ##   - transitive resolution is exercised twice: futile.logger imports
  ##     lambda.r + futile.options, and lambda.r imports formatR; itertools
  ##     imports iterators, which is *also* pinned explicitly, so a pinned
  ##     dependency has to be honoured as a dependency too.
  ##   - 1 GitHub@<sha> pin to a leaf package with no Remotes.
  ##
  ## Every pin MUST be a package the test stack never loads. That is a hard
  ## requirement, not a preference: R cannot hot-swap a loaded namespace, so
  ## pinning a package testthat has loaded asks for a downgrade that cannot
  ## happen, and the pin silently does not land.
  ##
  ## Getting this wrong is subtle, because the offending namespaces load
  ## *lazily* -- so the test passes in isolation and fails in a full-suite
  ## run, on some machines only. An earlier version of this fixture pinned
  ## crayon, prettyunits and praise; all three are in the dependency closure
  ## of testthat/devtools/pak (praise is a declared testthat Import, loaded
  ## for the encouragement message on success), and unlinking `testlib` also
  ## pulled praise out from under testthat mid-run.
  ##
  ## To re-verify the current picks, check that none is in the closure:
  ##   ap <- available.packages()
  ##   cl <- tools::package_dependencies(c("testthat", "devtools", "pak",
  ##           "pkgload", "data.table", "cli", "covr", "withr"), db = ap,
  ##           recursive = TRUE, which = c("Depends", "Imports", "LinkingTo"))
  ##   pkgs$Package %in% unique(c(unlist(cl), names(cl)))   # must be all FALSE
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

  ## Require installs into .libPaths()[1], which setLibPaths() may have made an
  ## R-version subfolder of testlib (it appends the version when interactive()).
  ip <- data.table::as.data.table(installed.packages(lib.loc = .libPaths()[1], noCache = TRUE))

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
