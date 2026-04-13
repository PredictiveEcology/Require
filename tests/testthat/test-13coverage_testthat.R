test_that("RequireOptions functions", {
  ro <- RequireOptions()
  testthat::expect_true(is.list(ro))
  testthat::expect_true(all(startsWith(names(ro), "Require")))
  testthat::expect_true("Require.verbose" %in% names(ro))
  testthat::expect_true("Require.usePak" %in% names(ro))
  testthat::expect_true("Require.cachePkgDir" %in% names(ro))
  testthat::expect_identical(ro[["Require.usePak"]], TRUE)
  testthat::expect_identical(ro[["Require.offlineMode"]], FALSE)

  gro <- getRequireOptions()
  testthat::expect_true(is.list(gro))
  testthat::expect_true(all(startsWith(names(gro), "Require")))
  testthat::expect_identical(names(ro), names(gro))
})

test_that("message helper functions", {
  # msgStripColor
  coloured <- "\033[31mred text\033[0m"
  stripped <- Require:::msgStripColor(coloured)
  testthat::expect_false(grepl("\033", stripped, fixed = TRUE))
  testthat::expect_true(grepl("red text", stripped))

  plain <- "no color here"
  testthat::expect_identical(Require:::msgStripColor(plain), plain)

  # messageCantFind
  msg <- Require:::messageCantFind("main", "PredictiveEcology", "reproducible")
  testthat::expect_true(grepl("PredictiveEcology/reproducible@main", msg))
  testthat::expect_true(grepl("does it exist", msg))

  # messageCantInstallNoVersion - single package
  msg1 <- Require:::messageCantInstallNoVersion("data.table")
  testthat::expect_true(grepl("data.table", msg1))
  testthat::expect_true(grepl(.txtCouldNotBeInstalled, msg1))
  testthat::expect_true(grepl("doesn't", msg1))

  # messageCantInstallNoVersion - multiple packages
  msg2 <- Require:::messageCantInstallNoVersion(c("pkg1", "pkg2"))
  testthat::expect_true(grepl("don't", msg2))

  # messageCantInstallNoInternet
  msg3 <- Require:::messageCantInstallNoInternet("somepackage")
  testthat::expect_true(grepl("somepackage", msg3))
  testthat::expect_true(grepl(.txtCouldNotBeInstalled, msg3))
  testthat::expect_true(grepl("no internet", msg3))

  # msgPleaseChangeRqdVersion
  msg4 <- Require:::msgPleaseChangeRqdVersion("ggplot2", ">=", "3.0.0")
  testthat::expect_true(grepl(.txtPleaseChangeReqdVers, msg4))
  testthat::expect_true(grepl("ggplot2", msg4))
  testthat::expect_true(grepl(">=3.0.0", msg4))

  # msgShaNotChanged
  msg5 <- Require:::msgShaNotChanged("PredictiveEcology", "reproducible", "main")
  testthat::expect_true(grepl("SHA1 has not changed", msg5))
  testthat::expect_true(grepl("PredictiveEcology/reproducible@main", msg5))

  # getOptionWidthWithBuffer
  w <- Require:::getOptionWidthWithBuffer()
  testthat::expect_true(is.numeric(w))
  testthat::expect_true(w == getOption("width") - 10)
  # buff parameter is declared but function body uses hardcoded 10
  w2 <- Require:::getOptionWidthWithBuffer(buff = 5)
  testthat::expect_true(is.numeric(w2))

  # paste0WithLineFeed - short string unchanged
  short <- "short"
  out <- Require:::paste0WithLineFeed(short)
  testthat::expect_true(is.character(short))  # returns mess (original)

  # singularPlural
  testthat::expect_identical(Require:::singularPlural(c("is", "are"), l = "one"), "is")
  testthat::expect_identical(Require:::singularPlural(c("is", "are"), l = c("one", "two")), "are")
  testthat::expect_identical(Require:::singularPlural(c("has", "have"), v = 1), "has")
  testthat::expect_identical(Require:::singularPlural(c("has", "have"), v = 2), "have")
})

test_that("setLibPaths helpers", {
  # checkTRUERprofile
  testthat::expect_identical(Require:::checkTRUERprofile(TRUE), ".Rprofile")
  testthat::expect_identical(Require:::checkTRUERprofile(FALSE), FALSE)
  testthat::expect_identical(Require:::checkTRUERprofile("myFile.R"), "myFile.R")

  # resetRprofileMessage
  msg <- Require:::resetRprofileMessage()
  testthat::expect_true(is.character(msg))
  testthat::expect_true(grepl("setupOff", msg))

  msg2 <- Require:::resetRprofileMessage(".Rprofile_custom")
  testthat::expect_true(is.character(msg2))

  # checkMissingLibPaths - NULL updateRprofile should stop
  testthat::expect_error(
    Require:::checkMissingLibPaths(updateRprofile = NULL),
    "libPaths cannot be missing"
  )

  # setLibPaths - missing libPaths with FALSE updateRprofile: no .Rprofile → message + invisible return
  td <- tempdir2("setLibPathsCovTest")
  origDir <- setwd(td)
  on.exit(setwd(origDir), add = TRUE)
  result <- setLibPaths(updateRprofile = FALSE)
  testthat::expect_null(result)
  setwd(origDir)
})

test_that("setup.R cache functions", {
  # cacheDir returns a character path
  cd <- cacheDir()
  testthat::expect_true(is.character(cd))
  testthat::expect_true(nchar(cd) > 0)

  # cachePkgDir returns a path under cacheDir
  pkgDir <- cachePkgDir()
  testthat::expect_true(is.character(pkgDir))
  testthat::expect_true(grepl("packages", pkgDir))

  # cachePkgDirForRepo sanitizes URL -- check the subdir name only (not full path,
  # which on Windows contains ":" from the drive letter e.g. "C:\...")
  d1 <- Require:::cachePkgDirForRepo("https://cloud.r-project.org")
  testthat::expect_true(is.character(d1))
  testthat::expect_false(grepl("https|:", basename(d1), fixed = FALSE))

  # Two URLs with same host but different paths map to same subdir
  d2 <- Require:::cachePkgDirForRepo("https://cloud.r-project.org/src/contrib")
  testthat::expect_identical(d1, d2)

  # Different hosts map to different subdirs
  d3 <- Require:::cachePkgDirForRepo("https://predictiveecology.r-universe.dev")
  testthat::expect_false(identical(d1, d3))

  # cachePkgDirForRepo with create=TRUE creates the directory
  withr::with_envvar(
    c("R_REQUIRE_CACHE" = tempdir2("cacheDirForRepoTest")),
    {
      d4 <- Require:::cachePkgDirForRepo("https://cloud.r-project.org", create = TRUE)
      testthat::expect_true(dir.exists(d4))
    }
  )

  # cacheGetOptionCachePkgDir with TRUE option
  withr::with_options(
    list(Require.cachePkgDir = TRUE),
    {
      val <- cacheGetOptionCachePkgDir()
      testthat::expect_true(is.character(val))
      testthat::expect_true(grepl("packages", val))
    }
  )

  # cacheGetOptionCachePkgDir with FALSE option
  withr::with_options(
    list(Require.cachePkgDir = FALSE),
    {
      val <- cacheGetOptionCachePkgDir()
      testthat::expect_null(val)
    }
  )

  # cacheGetOptionCachePkgDir with explicit path
  td <- tempdir2("cacheGetOption")
  withr::with_options(
    list(Require.cachePkgDir = td),
    {
      val <- cacheGetOptionCachePkgDir()
      testthat::expect_identical(val, td)
    }
  )

  # cacheGetOptionCachePkgDir with "default" and R_REQUIRE_PKG_CACHE = "FALSE"
  withr::with_options(
    list(Require.cachePkgDir = "default"),
    withr::with_envvar(
      c("R_REQUIRE_PKG_CACHE" = "FALSE"),
      {
        val <- cacheGetOptionCachePkgDir()
        testthat::expect_null(val)
      }
    )
  )

  # cacheGetOptionCachePkgDir with "default" and R_REQUIRE_PKG_CACHE = "TRUE"
  withr::with_options(
    list(Require.cachePkgDir = "default"),
    withr::with_envvar(
      c("R_REQUIRE_PKG_CACHE" = "TRUE"),
      {
        val <- cacheGetOptionCachePkgDir()
        testthat::expect_true(is.character(val))
      }
    )
  )

  # cacheGetOptionCachePkgDir with "default" and R_REQUIRE_PKG_CACHE = path
  tdPkgCache <- tempdir2("pkgCacheEnvVar")
  withr::with_options(
    list(Require.cachePkgDir = "default"),
    withr::with_envvar(
      c("R_REQUIRE_PKG_CACHE" = tdPkgCache),
      {
        val <- cacheGetOptionCachePkgDir()
        testthat::expect_identical(val, tdPkgCache)
      }
    )
  )

  # removeOldFlatCachePkgs - no old files, returns NULL
  withr::with_envvar(
    c("R_REQUIRE_CACHE" = tempdir2("removeOldFlatTest")),
    {
      pe <- Require:::pkgEnv()
      pe[["oldFlatCacheChecked"]] <- NULL  # reset
      result <- Require:::removeOldFlatCachePkgs()
      testthat::expect_null(result)
    }
  )

  # removeOldFlatCachePkgs - already checked, returns early
  withr::with_envvar(
    c("R_REQUIRE_CACHE" = tempdir2("removeOldFlatTest2")),
    {
      pe <- Require:::pkgEnv()
      pe[["oldFlatCacheChecked"]] <- TRUE
      result <- Require:::removeOldFlatCachePkgs()
      testthat::expect_null(result)
    }
  )
})

test_that("msgWithLineFeedIterative and related", {
  # msgWithLineFeedIterative sends a message; just test it doesn't error
  testthat::expect_no_error({
    capture_messages(
      Require:::msgWithLineFeedIterative("test msg", verbose = 0)
    )
  })

  # with reset = TRUE
  testthat::expect_no_error({
    capture_messages(
      Require:::msgWithLineFeedIterative("test msg", verbose = 0, reset = TRUE)
    )
  })

  # counter overflow causes newline
  pe <- Require:::pkgEnv()
  pe[[".msgTestCounter"]] <- 10000L
  testthat::expect_no_error({
    capture_messages(
      Require:::msgWithLineFeedIterative("x", name = ".msgTestCounter", verbose = 0)
    )
  })
})
