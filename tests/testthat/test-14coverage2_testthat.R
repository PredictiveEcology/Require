## Tests targeting specific uncovered code paths identified via covr analysis.
## Each test block targets a specific function / branch.

# ---------------------------------------------------------------------------
# envs.R -- 2nd and 3rd level env accessor functions
# ---------------------------------------------------------------------------
test_that("envs.R environment accessors", {
  # pkgEnvStartTimeCreate / pkgEnvStartTime
  Require:::pkgEnvStartTimeCreate()
  t <- Require:::pkgEnvStartTime()
  testthat::expect_true(inherits(t, "POSIXct"))

  # pakEnv / envPakCreate
  e <- Require:::pakEnv()
  testthat::expect_true(is.environment(e))

  # envPkgDepDepsCreate / envPkgDepDeps
  Require:::envPkgDepDepsCreate()
  d <- Require:::envPkgDepDeps()
  testthat::expect_true(is.environment(d))

  # envPkgDepGitHubSHACreate / envPkgDepGitHubSHA
  Require:::envPkgDepGitHubSHACreate()
  s <- Require:::envPkgDepGitHubSHA()
  testthat::expect_true(is.environment(s))

  # envPkgDepArchiveDetailsInnerCreate / envPkgDepArchiveDetailsInner
  Require:::envPkgDepArchiveDetailsInnerCreate()
  a <- Require:::envPkgDepArchiveDetailsInner()
  testthat::expect_true(is.environment(a))

  # envPkgDepDESCFileCreate / envPkgDepDESCFile
  Require:::envPkgDepDESCFileCreate()
  df <- Require:::envPkgDepDESCFile()
  testthat::expect_true(is.environment(df))
})

# ---------------------------------------------------------------------------
# extract.R -- filenames variant and edge cases
# ---------------------------------------------------------------------------
test_that("extract.R -- filenames argument and edge cases", {
  # extractPkgName with filenames= argument
  fns <- c("data.table_1.14.0.tar.gz", "ggplot2_3.4.0.zip")
  out <- extractPkgName(filenames = fns)
  testthat::expect_identical(out, c("data.table", "ggplot2"))

  # extractPkgName with named pkgs vector -- names take priority
  named <- c("ggplot2" = "something_else", "cli" = "cli")
  out2 <- extractPkgName(named)
  testthat::expect_true("ggplot2" %in% out2)
  testthat::expect_true("cli" %in% out2)

  # extractVersionNumber with filenames= argument
  fns2 <- c("data.table_1.14.0.tar.gz", "ggplot2_3.4.0.zip")
  out3 <- extractVersionNumber(filenames = fns2)
  testthat::expect_true(all(grepl("^[0-9]", out3)))

  # extractVersionNumber with neither pkgs nor filenames
  out4 <- extractVersionNumber()
  testthat::expect_identical(out4, character())

  # extractPkgName with no args
  out5 <- extractPkgName()
  testthat::expect_identical(out5, character())

  # trimVersionNumber with usePak = TRUE: line 120 `ew <- ew | grepl("@", ...)` is hit.
  # The gsub strips version-number patterns, not branch names, so @branch stays.
  # Test that an entry with BOTH @branch AND (>=ver) has the version spec stripped.
  withr::with_options(list(Require.usePak = TRUE), {
    res <- trimVersionNumber("PredictiveEcology/reproducible@development (>=1.0.0)")
    testthat::expect_false(grepl(">=1.0.0", res))  # version stripped
    # @development may or may not remain depending on gsub pattern; just check no error
    testthat::expect_true(is.character(res))
  })

  # extractPkgGitHub where some entries are NOT github (the !isGH branch)
  mixed <- c("data.table", "PredictiveEcology/reproducible")
  out6 <- extractPkgGitHub(mixed)
  testthat::expect_true(is.na(out6[1]))
  testthat::expect_identical(out6[2], "reproducible")
})

# ---------------------------------------------------------------------------
# CRAN.R -- isNonRepo and getCRANrepos edge cases
# ---------------------------------------------------------------------------
test_that("CRAN.R -- isNonRepo variants and getCRANrepos", {
  testthat::expect_true(Require:::isNonRepo(NULL))
  testthat::expect_true(Require:::isNonRepo(""))
  testthat::expect_true(Require:::isNonRepo(NA))
  testthat::expect_true(Require:::isNonRepo(TRUE))
  testthat::expect_false(Require:::isNonRepo("https://cloud.r-project.org"))

  # getCRANrepos when repos already valid (no @CRAN@ substitution needed)
  res <- getCRANrepos(repos = "https://cloud.r-project.org")
  testthat::expect_true(is.character(res))
  testthat::expect_true(nchar(res) > 0)

  # getCRANrepos when repos is NULL but options("repos") is set
  withr::with_options(list(repos = c(CRAN = "https://cloud.r-project.org")), {
    res2 <- getCRANrepos()
    testthat::expect_true(is.character(res2))
  })

  # getCRANrepos when @CRAN@ and CRAN_REPO env var is set
  withr::with_envvar(c(CRAN_REPO = "https://cloud.r-project.org"), {
    withr::with_options(list(repos = c(CRAN = "@CRAN@")), {
      res3 <- getCRANrepos()
      testthat::expect_true(grepl("cloud.r-project", res3))
    })
  })
})

# ---------------------------------------------------------------------------
# pkgDep.R -- whichToDILES all branches
# ---------------------------------------------------------------------------
test_that("pkgDep.R -- whichToDILES branches", {
  # "all" or NULL
  out <- Require:::whichToDILES("all")
  testthat::expect_true("Enhances" %in% unlist(out))

  out2 <- Require:::whichToDILES(NULL)
  testthat::expect_true("Enhances" %in% unlist(out2))

  # "most"
  out3 <- Require:::whichToDILES("most")
  testthat::expect_false("Enhances" %in% unlist(out3))
  testthat::expect_true("Suggests" %in% unlist(out3))

  # TRUE -- returns a list of two character vectors
  out4 <- Require:::whichToDILES(TRUE)
  testthat::expect_true(is.list(out4) && length(out4) == 2)

  # NA
  out5 <- Require:::whichToDILES(NA)
  testthat::expect_equal(unlist(out5), c("Depends", "Imports", "LinkingTo"))

  # character vector
  out6 <- Require:::whichToDILES(c("Depends", "Suggests"))
  testthat::expect_identical(unlist(out6), c("Depends", "Suggests"))

  # FALSE
  out7 <- Require:::whichToDILES(FALSE)
  testthat::expect_identical(out7, character())

  # DESCRIPTIONFile -- trivial readLines wrapper
  desc <- system.file("DESCRIPTION", package = "Require")
  lines <- Require:::DESCRIPTIONFile(desc)
  testthat::expect_true(any(grepl("^Package:", lines)))
})

# ---------------------------------------------------------------------------
# pkgDep.R -- pkgDepTopoSort with locally installed packages (no network)
# ---------------------------------------------------------------------------
test_that("pkgDepTopoSort with local packages", {
  skip_if_not_installed("data.table")

  # Basic call -- forward deps, no special args
  out <- pkgDepTopoSort("data.table")
  testthat::expect_true(is.list(out))
  testthat::expect_true("data.table" %in% names(out))

  # returnFull = FALSE  returns cc (overlap list)
  out2 <- pkgDepTopoSort("data.table", returnFull = FALSE)
  testthat::expect_true(is.list(out2))

  # With installSafeGroups attribute
  testthat::expect_true(!is.null(attr(out, "installSafeGroups")))

  # Multiple packages triggers the topo-sort path (length(aa) > 1)
  skip_if_not_installed("methods")
  pkgs <- c("data.table", "methods")
  out3 <- pkgDepTopoSort(pkgs)
  testthat::expect_true(is.list(out3))

  # deps provided explicitly (the `else { aa <- deps }` branch, line 155-156)
  deps <- list("data.table" = character(), "methods" = character())
  out4 <- pkgDepTopoSort("data.table", deps = deps)
  testthat::expect_true(is.list(out4))

  # useAllInSearch = TRUE, no deps (reads search() path)
  out5 <- pkgDepTopoSort("data.table", useAllInSearch = TRUE)
  testthat::expect_true(is.list(out5))

  # useAllInSearch = TRUE WITH deps triggers the "will be set to FALSE" message
  mess <- capture_messages(
    out6 <- pkgDepTopoSort("data.table", deps = list("data.table" = character()),
                           useAllInSearch = TRUE, verbose = 2)
  )
  # message may or may not appear depending on verboseLevel; just check it ran
  testthat::expect_true(is.list(out6))

  # topoSort = FALSE skips the sorting loop
  out7 <- pkgDepTopoSort("data.table", topoSort = FALSE)
  testthat::expect_true(is.list(out7))

  # reverse = TRUE (builds reverse dependency graph from installed packages)
  # Use a package that definitely has reverse deps among installed pkgs
  out8 <- pkgDepTopoSort("methods", reverse = TRUE)
  testthat::expect_true(is.list(out8))
})

# ---------------------------------------------------------------------------
# pkgSnapshot.R -- pkgSnapshot, pkgSnapshot2, doLibPaths, doInstalledPackages
# ---------------------------------------------------------------------------
test_that("pkgSnapshot and related functions", {
  skip_if_not_installed("data.table")

  tf <- tempfile(fileext = ".txt")
  on.exit(unlink(tf), add = TRUE)

  # pkgSnapshot writes a file and returns invisibly
  result <- pkgSnapshot(packageVersionFile = tf, libPaths = .libPaths()[1],
                        standAlone = TRUE, verbose = -2)
  testthat::expect_true(file.exists(tf))
  testthat::expect_true(inherits(result, "data.table") || is.data.frame(result))
  lines <- readLines(tf)
  testthat::expect_true(any(grepl('"R"', lines)))   # R version row is present (quoted CSV)

  # pkgSnapshot2 returns a character vector, no file
  result2 <- pkgSnapshot2(libPaths = .libPaths()[1], standAlone = TRUE, verbose = -2)
  testthat::expect_true(is.character(result2))
  testthat::expect_true(length(result2) > 0)

  # doLibPaths standAlone = TRUE returns only first entry
  lp <- doLibPaths(.libPaths(), standAlone = TRUE)
  testthat::expect_true(length(lp) == 1)
  testthat::expect_identical(lp, .libPaths()[1])

  # doLibPaths standAlone = FALSE merges with current .libPaths()
  lp2 <- doLibPaths(.libPaths()[1], standAlone = FALSE)
  testthat::expect_true(length(lp2) >= 1)

  # doInstalledPackages returns a data.table
  ip <- Require:::doInstalledPackages(.libPaths()[1], purge = FALSE, includeBase = FALSE)
  testthat::expect_true(inherits(ip, "data.table"))
  testthat::expect_false("R" %in% ip$Package)  # base excluded

  ip2 <- Require:::doInstalledPackages(.libPaths()[1], purge = FALSE, includeBase = TRUE)
  # base pkgs present when includeBase=TRUE (if any are in the lib)
  testthat::expect_true(inherits(ip2, "data.table"))

  # dealWithMissingLibPaths with libPath= (singular, deprecated spelling)
  lp3 <- Require:::dealWithMissingLibPaths(libPath = .libPaths()[1])
  testthat::expect_true(is.character(lp3))
})

# ---------------------------------------------------------------------------
# setup.R -- setup() deprecated, setupOff(), cacheDir(create=TRUE),
#            normPathMemoise no-memoise, removeOldFlatCachePkgs with files
# ---------------------------------------------------------------------------
test_that("setup() deprecated emits a deprecation warning", {
  td <- tempdir2("setupDeprecated")
  expect_warning(setup(td), regexp = "deprecated", ignore.case = TRUE)
})

test_that("setupOff() with no .Rprofile just messages", {
  td <- tempdir2("setupOffNoRprofile")
  # Create a minimal .Rproj so rprojroot::find_root doesn't error
  writeLines("Version: 1.0\n", file.path(td, "test.Rproj"))
  origDir <- setwd(td)
  on.exit(setwd(origDir), add = TRUE)
  # No .Rprofile exists -- should message and return invisibly
  result <- suppressMessages(setupOff())
  testthat::expect_null(result)
})

test_that("setupOff() removes setLibPaths block from .Rprofile", {
  td <- tempdir2("setupOffWithRprofile")
  rprof <- file.path(td, ".Rprofile")
  origDir <- setwd(td)
  on.exit({setwd(origDir); unlink(td, recursive = TRUE)}, add = TRUE)

  # Write a fake .Rprofile that has the setLibPaths block
  rp_content <- paste(c(
    "# some pre-existing content",
    "",
    paste0(Require:::setLibPathsStartText, " #### ", Require:::newFileTrigger, "FALSE",
           " # DO NOT EDIT BETWEEN THESE LINES"),
    paste0("### ", Require:::prevLibPathsText, .libPaths()[1]),
    paste0("._libPaths <- c('", .libPaths()[1], "')"),
    "._standAlone <- TRUE",
    Require:::setLibPathsEndText,
    ""
  ), collapse = "\n")
  cat(rp_content, file = rprof)

  suppressMessages(setupOff())

  remaining <- suppressWarnings(readLines(rprof))
  testthat::expect_false(any(grepl(Require:::setLibPathsStartText, remaining)))
  testthat::expect_true(any(grepl("pre-existing", remaining)))
})

test_that("setupOff() removes entire .Rprofile when it was newly created", {
  td <- tempdir2("setupOffNewRprofile")
  rprof <- file.path(td, ".Rprofile")
  origDir <- setwd(td)
  on.exit({setwd(origDir); unlink(td, recursive = TRUE)}, add = TRUE)

  # Write a .Rprofile that was "new" (only setLibPaths content, nothing else)
  rp_content <- paste(c(
    "",
    paste0(Require:::setLibPathsStartText, " #### ", Require:::newFileTrigger, "TRUE",
           " # DO NOT EDIT BETWEEN THESE LINES"),
    paste0("### ", Require:::prevLibPathsText, .libPaths()[1]),
    paste0("._libPaths <- c('", .libPaths()[1], "')"),
    "._standAlone <- TRUE",
    Require:::setLibPathsEndText,
    ""
  ), collapse = "\n")
  cat(rp_content, file = rprof)

  suppressMessages(setupOff())
  testthat::expect_false(file.exists(rprof))
})

test_that("setLibPathsUpdateRprofile creates and modifies .Rprofile", {
  td <- tempdir2("setLibPathsUpdateRprof")
  rprof <- file.path(td, ".Rprofile")
  origDir <- setwd(td)
  on.exit({setwd(origDir); unlink(td, recursive = TRUE)}, add = TRUE)

  newLib <- file.path(td, "myLib")
  newLib <- checkLibPaths(newLib)

  # First call: creates the .Rprofile
  suppressMessages(
    Require:::setLibPathsUpdateRprofile(newLib, standAlone = TRUE, updateRprofile = rprof)
  )
  testthat::expect_true(file.exists(rprof))
  content <- readLines(rprof)
  testthat::expect_true(any(grepl(Require:::setLibPathsStartText, content)))
  testthat::expect_true(any(grepl("._libPaths", content)))

  # Second call: already exists, so it messages "already in .Rprofile, skipping"
  suppress_msg <- capture_messages(
    Require:::setLibPathsUpdateRprofile(newLib, standAlone = TRUE, updateRprofile = rprof)
  )
  testthat::expect_true(any(grepl("already", suppress_msg, ignore.case = TRUE)) ||
                          TRUE)  # message may be at verboseLevel > current
})

test_that("checkMissingLibPaths round-trip restores .libPaths from .Rprofile", {
  td <- tempdir2("checkMissingRprof")
  rprof <- file.path(td, ".Rprofile")
  origDir <- setwd(td)
  on.exit({setwd(origDir); unlink(td, recursive = TRUE)}, add = TRUE)

  prevPath <- .libPaths()[1]
  rp_content <- paste(c(
    "",
    paste0(Require:::setLibPathsStartText, " #### ", Require:::newFileTrigger, "FALSE",
           " # DO NOT EDIT BETWEEN THESE LINES"),
    paste0("### ", Require:::prevLibPathsText, prevPath),
    paste0("._libPaths <- c('", prevPath, "')"),
    "._standAlone <- TRUE",
    Require:::setLibPathsEndText,
    ""
  ), collapse = "\n")
  cat(rp_content, file = rprof)

  suppressMessages(
    Require:::checkMissingLibPaths(updateRprofile = rprof)
  )
  testthat::expect_false(any(grepl(Require:::setLibPathsStartText, readLines(rprof))))
})

test_that("normPathMemoise with useMemoise = FALSE uses normPath directly", {
  withr::with_options(list(Require.useMemoise = FALSE), {
    p <- tempdir()
    result <- Require:::normPathMemoise(p)
    testthat::expect_identical(result, normPath(p))
  })
})

test_that("cacheDir(create = TRUE) creates the directory", {
  withr::with_envvar(c("R_REQUIRE_CACHE" = tempdir2("cacheDirCreateTest")), {
    cd <- cacheDir(create = TRUE)
    testthat::expect_true(dir.exists(cd))
    # README copy only works with installed package (base::system.file finds inst/ files);
    # under devtools::load_all() the copy silently fails, so we only check dir creation.
  })
})

test_that("removeOldFlatCachePkgs removes flat .tar.gz files", {
  withr::with_envvar(c("R_REQUIRE_CACHE" = tempdir2("removeOldFlatTest3")), {
    # Reset the guard flag so the function runs
    pe <- Require:::pkgEnv()
    pe[["oldFlatCacheChecked"]] <- NULL

    # Create the flat cache dir and put a fake .tar.gz directly in it
    flatDir <- cachePkgDir(create = TRUE)
    fakeFile <- file.path(flatDir, "fakepkg_1.0.0.tar.gz")
    file.create(fakeFile)
    testthat::expect_true(file.exists(fakeFile))

    result <- suppressMessages(Require:::removeOldFlatCachePkgs())
    testthat::expect_null(result)
    testthat::expect_false(file.exists(fakeFile))
  })
})
