test_that("test 3", {

  setupInitial <- setupTest()
    # on.exit(endTest(setupInitial))

  isDev <- getOption("Require.isDev")
  # Test misspelled
  out <- capture.output(type = "message", lala <- Require("data.tt", verbose = 1))
  testthat::expect_true(any(grepl("could not be installed", out))) # {out, "simpleWarning")})

  # for coverages that were missing
  pkgDTEmpty <- Require:::toPkgDT(character())
  out <- Require:::installedVers(pkgDTEmpty) #


  pkgDep("data.table", purge = FALSE)
  if (!isDev) {
    pkgDep("data.table", purge = TRUE)
  }
  # pkgDep2("Require")


  pkgDepTopoSort(c("data.table"), useAllInSearch = TRUE)
  pkgDepTopoSort(c("data.table"), useAllInSearch = TRUE, deps = "Require")
  pkgDepTopoSort(c("Require", "data.table"))
  pkgDepTopoSort(c("Require", "data.table"),
                 useAllInSearch = TRUE,
                 deps = "Require", returnFull = FALSE, reverse = TRUE
  )


  if (Sys.info()["user"] == "emcintir") {
    options(
      "Require.RPackageCache" = TRUE,
      "Require.unloadNamespaces" = FALSE
    )
  }
  Require("data.table",
          install = "force", require = FALSE, libPaths = tempdir2("other"),
          quiet = !(getOption("Require.verbose") >= 1)
  )
  suppressWarnings(Require("Require",
                           install = "force", require = FALSE,
                           libPaths = tempdir2("other")
  ))

  pkg <- c("data.table", "data.table")
  pkgDT <- Require:::toPkgDT(pkg)
  data.table::set(pkgDT, NULL, "installFrom", "CRAN")
  data.table::set(pkgDT, NULL, "installed", FALSE)
  data.table::set(pkgDT, NULL, "installResult", TRUE)

  data.table::set(pkgDT, NULL, "versionSpec", NA)

  out <- detachAll("data.table",
                   dontTry = dontDetach())
  testthat::expect_true({
    isTRUE(out["data.table"] == 1)
  })

  warn <- tryCatch(Require:::warningCantInstall("devtolls"), warning = function(w) w$message)
  testthat::expect_true({
    grepl("you will likely", warn)
  })

  origLP <- setLibPaths(tempdir2("other"), updateRprofile = FALSE)
  warn <- tryCatch(Require("data.table", require = FALSE), warning = function(w) w$message)
  silent <- setLibPaths(origLP, updateRprofile = FALSE)

  # Test the setLibPaths with changed .Rprofile
  origDir <- setwd(tempdir2("other"))
  setLibPaths("newProjectLib", updateRprofile = TRUE) # set a new R package library locally
  setLibPaths() # reset it to original
  setwd(origDir)

  ## setup
  # setupTestDir <- normPath(tempdir2("setupTests"))
  # ccc <- checkPath(file.path(setupTestDir, ".cache"), create = TRUE)
  # out2222 <- capture.output(setup(setupTestDir, RPackageCache = ccc))
  # testthat::expect_true(identical(getOption("Require.RPackageCache"), ccc)) ## TODO: warnings in readLines() cannot open DESCRIPTION file
  # out2222 <- capture.output(setupOff())
  # Require:::messageVerbose("This is getOption('Require.RPackageCache'): ", Require:::getOptionRPackageCache(),
  #                verboseLevel = 0)
  # RPackageCacheSysEnv <- Sys.getenv("R_REQUIRE_PKG_CACHE")
  # if (identical(RPackageCacheSysEnv, "FALSE") ) {
  #   testthat::expect_true(identical(NULL, getOptionRPackageCache()))
  # } else {
  #   if (!(is.null(Require:::getOptionRPackageCache()) || Require:::getOptionRPackageCache() == "FALSE"))
  #     testthat::expect_true(identical(normPath(Require:::getOptionRPackageCache()), normPath(Require::RequirePkgCacheDir())))
  # }

  # reset options after setupOff()
  # secondTry <- normPath(file.path(setupTestDir, ".cacheSecond"))
  # opt22 <- options("Require.RPackageCache" = secondTry)
  # ccc <- checkPath(secondTry, create = TRUE)
  # out2222 <- capture.output(setup(setupTestDir, RPackageCache = ccc)) ## TODO: warnings in file() cannot open DESCRIPTION files
  # testthat::expect_true(identical(Require:::getOptionRPackageCache(), ccc))
  # out2222 <- capture.output(setupOff())
  # testthat::expect_true(identical(Require:::getOptionRPackageCache(), secondTry)) # BECAUSE THIS IS A MANUAL OVERRIDE of options; doesn't return Sys.getenv

  ooo <- options(Require.RPackageCache = TRUE)
  testthat::expect_true(identical(getOptionRPackageCache(), RequirePkgCacheDir()))
  ooo <- options(Require.RPackageCache = FALSE)
  testthat::expect_true(identical(getOptionRPackageCache(), NULL))
  ooo <- options(Require.RPackageCache = tempdir())
  testthat::expect_true(identical(getOptionRPackageCache(), tempdir()))
  ooo <- options(Require.RPackageCache = "default")
  RPackageCacheSysEnv <- Sys.getenv("R_REQUIRE_PKG_CACHE")
  if (identical(RPackageCacheSysEnv, "FALSE")) {
    testthat::expect_true(identical(NULL, getOptionRPackageCache()))
  } else {
    if (!(is.null(Require:::getOptionRPackageCache()) || Require:::getOptionRPackageCache() == "FALSE")) {
      testthat::expect_true(identical(normPath(Require:::getOptionRPackageCache()), normPath(Require::RequirePkgCacheDir())))
    }
  }

  ro <- RequireOptions()
  gro <- getRequireOptions()

  testthat::expect_true(is.list(ro))
  testthat::expect_true(all(startsWith(names(ro), "Require")))
  testthat::expect_true(is.list(gro))
  testthat::expect_true(all(startsWith(names(gro), "Require")))

  # Ensure the "which" for pkgDep are working correctly
  wh <- expand.grid(suggests = c(TRUE, FALSE), depends = c(TRUE, FALSE), imports = c(TRUE, FALSE), linkingTo = c(TRUE, FALSE))
  utilsOut <- list()
  for (i in c("Suggests", "Imports", "Depends", "LinkingTo")) {
    lala <- strsplit(gsub("\n", "", utils::packageDescription("Require", fields = i)), ",")[[1]]
    utilsOut[[i]] <- gsub(x = lala, " ", "") # remove spaces
  }


  out2 <- by(wh, seq(NROW(wh)), function(wh1Row) {
    out <- do.call(pkgDep, append(list("Require"), as.list(wh1Row[1, , drop = TRUE])))[[1]]
    o2 <- tools::toTitleCase(names(wh1Row)[unlist(wh1Row)])
    if (length(o2)) {
      pkgs <- gsub(x = unname(unlist(utilsOut[o2])), " ", "") # remove spaces
      out <- gsub(x = out, " ", "") # remove spaces
      out <- setdiff(out, grep("R\\(.+", pkgs, value = TRUE, invert = TRUE))
    }
    setdiff(out, "remotes") # remotes was removed in version 0.2.6.9020
  })
  testArgs <- all(c("Require", "data.table") %in% extractPkgName(unname(unlist(as.list(out2)))))
  testthat::expect_true(isTRUE(testArgs))

  if (isDev) {
    # this was a bug created a warning when there was a package not on CRAN, but there
    #   were multiple repos; ffbase is no longer on CRAN
    # can't quiet this down on linux because ffbase is not binary but rest are ...
    #  install.packages won't do both types quiet = TRUE for some reason
    #mess1 <- capture.output(
    #  type = "message",
    #  mess <- capture_messages(
        warns <- capture_warnings(
          #withCallingHandlers(
          Install("ffbase", # verbose = 0,
                  repos = c(RSPM = urlForPositPACKAGES, CRAN = "https://cloud.r-project.org"
                  ))
          #)
          )
    #)
    expect_identical(character(0), warns)
    # testthat::expect_true(
    #   !grepl("number of items to replace is not a multiple of replacement length",
    #          warns))


  }

  if (isWindows()) {
    # test the new approach that installs outside R session -- is fine on Linux-alikes
    withr::local_options(Require.installPackagesSys = FALSE)
    Require("fpCompare (<0.2.4)", install = "force")
    packageVersion("fpCompare")
    warns <- capture_warnings(Require("fpCompare (>=0.2.4)", install = "force"))
    packageVersion("fpCompare")
    withr::local_options(Require.installPackagesSys = TRUE)
    mess <- capture_messages(Require("fpCompare (>=0.2.4)", install = "force"))
    warnsAfter <- capture_warnings(packageVersion("fpCompare"))
    expect_true(grepl(msgIsInUse, warns))
    expect_false(isTRUE(grepl(msgIsInUse, warnsAfter)))
    detach("package:fpCompare", unload = TRUE)
  }

  if (FALSE) {
    pkgs <- c("fpCompare", "rlang", "cli", "crayon", "stringr", "lobstr")
    a <- unique(extractPkgName(unlist(unname(pkgDep(pkgs)))))
    clearRequirePackageCache(a, ask = F)
    library(sys); library(waldo)
    setLibPaths(tempdir2(.rndstr(1)))
    try(remove.packages(a))
    options(Require.installPackagesSys = 1L)
    ipBefore <- installed.packages(lib.loc = .libPaths()[1], noCache = TRUE)
    system.time(Install(pkgs, verbose = 1))
    # system.time(install.packages(pkgs))
    ipAfter <- installed.packages(lib.loc = .libPaths()[1], noCache = TRUE)
  }

  ooo <- options(Require.RPackageCache = NULL)
  testthat::expect_true(identical(getOptionRPackageCache(), NULL))
  options(ooo)

})
