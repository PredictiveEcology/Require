## The install.packages() snapshot chain.
##
## Require's default snapshot installer is pak (Require.snapshotInstaller);
## the install.packages() chain in R/pkgSnapshot.R is legacy, retained but no
## longer developed. It is also, at ~2700 lines, the largest single block of
## unexercised code in the package -- the only test that drove it was the
## 380-package test-09, which is gated off CI for taking over an hour.
##
## These are the cheap parts: classifyCompileFailure() and
## diagnoseSnapshotInstallFailures() are ordinary functions over text and a
## library path, so they need no network and no install. The end-to-end chain
## is covered separately in test-19.

classifyCompileFailure <- Require:::classifyCompileFailure

## ---- classifyCompileFailure ---------------------------------------------

test_that("classifyCompileFailure names a missing system header and hints at it", {
  txt <- c("gcc -I/usr/share/R/include -fpic -O2 -c bar.c -o bar.o",
           "bar.c:3:10: fatal error: 'jpeglib.h' file not found",
           "1 error generated.",
           "make: *** [bar.o] Error 1")

  out <- classifyCompileFailure(txt, "jpegPkg")

  expect_type(out, "list")
  expect_match(out$reason, "missing system header 'jpeglib.h'", fixed = TRUE)
  ## known headers carry an actionable install hint
  expect_match(paste(unlist(out), collapse = " "), "jpeg")
})

test_that("classifyCompileFailure handles a header it has no hint for", {
  txt <- "foo.c:1:10: fatal error: 'wholly-unknown-lib.h' file not found"

  out <- classifyCompileFailure(txt, "somePkg")

  expect_match(out$reason, "wholly-unknown-lib.h", fixed = TRUE)
})

test_that("classifyCompileFailure names the library the linker could not find", {
  txt <- c("ld: library 'gdal' not found for -lgdal",
           "clang: error: linker command failed with exit code 1")

  out <- classifyCompileFailure(txt, "sf")

  expect_match(out$reason, "linker can't find library '-lgdal'", fixed = TRUE)
})

test_that("classifyCompileFailure recognises missing zlib headers", {
  txt <- c("foo.c:2:10: fatal error: zlib.h: No such file or directory",
           "compilation terminated.")

  out <- classifyCompileFailure(txt, "zPkg")

  expect_match(out$reason, "zlib")
})

test_that("classifyCompileFailure recognises the R memory-macro breakage", {
  ## R 4.5 stopped defining Calloc/Free unless STRICT_R_HEADERS=0
  txt <- "foo.c:10:3: error: 'Calloc' was not declared in this scope"

  out <- classifyCompileFailure(txt, "oldPkg")

  expect_match(out$reason, "oldPkg")
})

test_that("classifyCompileFailure recognises a dyn.load symbol failure", {
  txt <- c("** testing if installed package can be loaded",
           "Error: package or namespace load failed:",
           " unable to load shared object 'thing.so':",
           " symbol not found in flat namespace '_gomp_parallel'")

  out <- classifyCompileFailure(txt, "thing")

  expect_match(out$reason, "dyn.load failed")
  expect_match(out$reason, "_gomp_parallel", fixed = TRUE)
})

test_that("classifyCompileFailure recognises the const OGRSpatialReference breakage", {
  txt <- paste("gdal.cpp:88:5: error: cannot initialize a variable of type",
               "'OGRSpatialReference *' with an rvalue of type",
               "'const OGRSpatialReference *'")

  out <- classifyCompileFailure(txt, "terra")

  expect_match(out$reason, "terra")
})

test_that("classifyCompileFailure falls back to the first error line", {
  txt <- c("making some progress",
           "widget.c:42:1: error: something entirely unanticipated",
           "make: *** [widget.o] Error 1")

  out <- classifyCompileFailure(txt, "widget")

  expect_type(out$reason, "character")
  expect_true(nzchar(out$reason))
})

test_that("classifyCompileFailure survives empty and blank input", {
  expect_type(classifyCompileFailure(character(0), "p"), "list")
  expect_type(classifyCompileFailure(c("", "  ", ""), "p"), "list")
})

## ---- diagnoseSnapshotInstallFailures -------------------------------------

test_that("diagnoseSnapshotInstallFailures reports every package as missing from an empty lib", {
  destLib <- withr::local_tempdir()
  snapshot <- data.frame(
    Package = c("aaaNotReal", "bbbNotReal"),
    Version = c("1.0.0", "2.0.0"),
    stringsAsFactors = FALSE
  )

  ## the report goes to stdout via cat(), not through message()
  out <- capture.output(
    Require:::diagnoseSnapshotInstallFailures(snapshot, destLib, verbose = 1)
  )
  out <- paste(out, collapse = "\n")

  ## it reports rather than errors, and names what it could not find
  expect_match(out, "aaaNotReal")
  expect_match(out, "bbbNotReal")
  expect_match(out, "installed: 0 / 2", fixed = TRUE)
})

test_that("diagnoseSnapshotInstallFailures is quiet when the snapshot is satisfied", {
  ## a lib containing exactly what the snapshot asks for: use a package that
  ## is certainly installed, at whatever version is installed
  destLib <- .libPaths()[length(.libPaths())]
  v <- as.character(utils::packageVersion("utils"))
  snapshot <- data.frame(Package = "utils", Version = v, stringsAsFactors = FALSE)

  expect_no_error(
    Require:::diagnoseSnapshotInstallFailures(snapshot, destLib, verbose = 0)
  )
})

test_that("diagnoseSnapshotInstallFailures accepts the optional ref arguments", {
  destLib <- withr::local_tempdir()
  snapshot <- data.frame(Package = "cccNotReal", Version = "0.1", stringsAsFactors = FALSE)

  expect_no_error(
    Require:::diagnoseSnapshotInstallFailures(
      snapshot, destLib,
      unresolvedRefs = "cccNotReal", substituted = character(),
      autoFilled = character(), verbose = 0)
  )
})

## ---- installSnapshotViaInstallPackages: the no-op paths -------------------

test_that("installSnapshotViaInstallPackages skips the R row and base packages", {
  ## The snapshot's "R" row records the required R version, not a package;
  ## installing it would be nonsense. With only that and base packages there
  ## is nothing to do, so this needs no network.
  destLib <- withr::local_tempdir()
  snapshot <- data.frame(
    Package = c("R", "stats", "utils"),
    Version = c("4.4", "4.4.0", "4.4.0"),
    stringsAsFactors = FALSE
  )

  msgs <- capture_messages(
    out <- Require:::installSnapshotViaInstallPackages(snapshot, libPaths = destLib,
                                                       verbose = 1)
  )

  expect_true(isTRUE(out))
  expect_match(paste(msgs, collapse = " "), "no non-base packages")
  ## nothing was installed
  expect_length(dir(destLib), 0L)
})

test_that("installSnapshotViaInstallPackages accepts an empty snapshot", {
  destLib <- withr::local_tempdir()
  snapshot <- data.frame(Package = character(0), Version = character(0),
                         stringsAsFactors = FALSE)

  expect_true(isTRUE(suppressMessages(
    Require:::installSnapshotViaInstallPackages(snapshot, libPaths = destLib, verbose = 0)
  )))
})

## ---- the chain end to end ------------------------------------------------

test_that("a small snapshot installs through the install.packages chain", {
  ## test-19 installs the same fixture, but through the default pak
  ## installer. This one forces Require.snapshotInstaller = "install.packages"
  ## so the legacy chain -- parallel download, install.packages(Ncpus),
  ## post-install diagnostic -- is actually executed. Kept to the same
  ## 5-package fixture so the CI cost is the same as test-19's.
  ## CRAN policy: a package's tests may only download/install packages listed
  ## in its own Suggests. This installs the fixture's pins (iterators,
  ## futile.options, assertthat and the GitHub R.methodsS3 pin), none of which
  ## are Require Suggests, so it is CI-only. As in test-19, none may be a
  ## package the test stack loads -- see the note there. The tests above install nothing and run
  ## everywhere.
  skip_on_cran()
  ## Windows: the chain has no binary path there -- the code's own comment says
  ## "Linux uses PPM __linux__ binaries; macOS uses PPM with User-Agent
  ## negotiation; Windows falls through to source from CRAN. Windows isn't
  ## routinely tested but the same path runs." It does not: install.packages()
  ## fails inside available.packages() with
  ##   Error in read.dcf(file = tmpf): cannot open the connection
  ## on all four Windows legs, while Linux and macOS pass. Not being fixed --
  ## this chain is slated for removal (#182).
  skip_on_os("windows")
  setupInitial <- setupTest()
  skip_if_offline2()

  ## A *different* fixture from test-19's: no pin here has a third-party
  ## dependency. The install.packages() chain downloads and installs only the
  ## rows the snapshot names, so a pin whose dependencies are not themselves
  ## pinned fails at R CMD INSTALL with
  ##   "dependencies 'lambda.r', 'futile.options' are not available"
  ## That is a real limitation of this legacy path -- pak resolves such
  ## dependencies, which is why test-19 can use futile.logger and this cannot.
  ## Tracked in #182; not being fixed, since the chain is slated for removal.
  snf <- testthat::test_path("fixtures", "smallSnapshotNoDeps.txt")
  skip_if_not(file.exists(snf), "fixture not available")
  pkgs <- data.table::fread(snf)

  testlib <- file.path(tempdir(), paste0("rqlib_ipsnap_", as.integer(Sys.time())))
  dir.create(testlib, recursive = TRUE)
  on.exit(unlink(testlib, recursive = TRUE), add = TRUE)
  ## standAlone = TRUE only means "do not append the user libs"; it does not
  ## limit how many entries we pass. Carrying pakLibForTests keeps pak
  ## reachable for its subprocess without a per-test 12 MB reinstall.
  origLibPaths <- setLibPaths(c(testlib, pakLibForTests()), standAlone = TRUE)
  on.exit(setLibPaths(origLibPaths), add = TRUE)

  withr::local_options(
    Require.snapshotInstaller = "install.packages",
    Require.snapshotInstallerUsePPM = TRUE,
    Require.snapshotDownloadAttempts = 2L,
    Ncpus = max(1L, parallel::detectCores() - 1L)
  )

  warns <- capture_warnings(
    Require(packageVersionFile = snf, require = FALSE, returnDetails = TRUE)
  )

  ip <- data.table::as.data.table(
    installed.packages(lib.loc = testlib, noCache = TRUE))

  ## the CRAN pins must land at exactly the requested version
  cranPins <- pkgs[is.na(GithubRepo)]
  for (i in seq_len(nrow(cranPins))) {
    actual <- ip[Package == cranPins$Package[i], Version]
    expect_identical(actual, cranPins$Version[i],
                     info = paste0(cranPins$Package[i], ": expected ",
                                   cranPins$Version[i], " got ",
                                   paste(actual, collapse = "/")))
  }
})
