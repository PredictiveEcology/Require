test_that("test 3", {

  setupInitial <- setupTest()
    # on.exit(endTest(setupInitial))

  isDev <- getOption("Require.isDev")
  # Test misspelled

  warns <- capture_warnings(
    err <- try(silent = TRUE,
               out <- capture.output(type = "message",
                                     lala <- Require("data.tt")
               )
    )
  )
  test <- testWarnsInUsePleaseChange(warns)

  if (!getOption("Require.usePak", TRUE))
    testthat::expect_true(any(grepl("could not be installed", warns))) # {out, "simpleWarning")})

  # for coverages that were missing
  pkgDTEmpty <- Require:::toPkgDT(character())
  out <- Require:::installedVers(pkgDTEmpty) #


  pkgDep("data.table", purge = FALSE)
  if (!isDev) {
    pkgDep("data.table", purge = TRUE)
  }

  if (isTRUE(tryCatch(packageVersion("fpCompare"), error = function(e) "0.0.0") < "0.2.5")) {
    if (isDev) {
      Require::Install(c("fpCompare (>= 0.2.4)", "PredictiveEcology/fpCompare@development (>= 0.2.4.9000)"),
                       install= "force", libPaths = .libPaths()[1])
      expect_true(packVer("fpCompare", lib.loc = .libPaths()[1]) > "0.2.4")
    }
  }

  # pkgDep2("Require")


  if (!getOption("Require.usePak", TRUE)) {
    pkgDepTopoSort(c("data.table"), useAllInSearch = TRUE)
    pkgDepTopoSort(c("data.table"), useAllInSearch = TRUE, deps = "Require")
    pkgDepTopoSort(c("Require", "data.table"))
    pkgDepTopoSort(c("Require", "data.table"),
                   useAllInSearch = TRUE,
                   deps = "Require", returnFull = FALSE, reverse = TRUE
    )
  }

  if (Sys.info()["user"] == "emcintir") {
    options(
      "Require.cachePkgDir" = TRUE,
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

  if (!getOption("Require.usePak", TRUE)) {
    out <- detachAll("data.table",
                     dontTry = dontDetach())
    testthat::expect_true({
      isTRUE(out["data.table"] == 1)
    })
  }

  warn <- tryCatch(Require:::warningCantInstall("devtools"), warning = function(w) w$message)
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
  # testthat::expect_true(identical(getOption("Require.cachePkgDir"), ccc)) ## TODO: warnings in readLines() cannot open DESCRIPTION file
  # out2222 <- capture.output(setupOff())
  # Require:::messageVerbose("This is getOption('Require.cachePkgDir'): ", Require:::cacheGetOptionCachePkgDir(),
  #                verboseLevel = 0)
  # RPackageCacheSysEnv <- Sys.getenv("R_REQUIRE_PKG_CACHE")
  # if (identical(RPackageCacheSysEnv, "FALSE") ) {
  #   testthat::expect_true(identical(NULL, cacheGetOptionCachePkgDir()))
  # } else {
  #   if (!(is.null(Require:::cacheGetOptionCachePkgDir()) || Require:::cacheGetOptionCachePkgDir() == "FALSE"))
  #     testthat::expect_true(identical(normPath(Require:::cacheGetOptionCachePkgDir()), normPath(Require::cachePkgDir())))
  # }

  # reset options after setupOff()
  # secondTry <- normPath(file.path(setupTestDir, ".cacheSecond"))
  # opt22 <- options("Require.cachePkgDir" = secondTry)
  # ccc <- checkPath(secondTry, create = TRUE)
  # out2222 <- capture.output(setup(setupTestDir, RPackageCache = ccc)) ## TODO: warnings in file() cannot open DESCRIPTION files
  # testthat::expect_true(identical(Require:::cacheGetOptionCachePkgDir(), ccc))
  # out2222 <- capture.output(setupOff())
  # testthat::expect_true(identical(Require:::cacheGetOptionCachePkgDir(), secondTry)) # BECAUSE THIS IS A MANUAL OVERRIDE of options; doesn't return Sys.getenv

  ooo <- options(Require.cachePkgDir = TRUE)
  testthat::expect_true(identical(cacheGetOptionCachePkgDir(), cachePkgDir()))
  ooo <- options(Require.cachePkgDir = FALSE)
  testthat::expect_true(identical(cacheGetOptionCachePkgDir(), NULL))
  ooo <- options(Require.cachePkgDir = tempdir())
  testthat::expect_true(identical(cacheGetOptionCachePkgDir(), tempdir()))
  ooo <- options(Require.cachePkgDir = "default")
  RPackageCacheSysEnv <- Sys.getenv("R_REQUIRE_PKG_CACHE")
  if (identical(RPackageCacheSysEnv, "FALSE")) {
    testthat::expect_true(identical(NULL, cacheGetOptionCachePkgDir()))
  } else {
    if (!(is.null(Require:::cacheGetOptionCachePkgDir()) || Require:::cacheGetOptionCachePkgDir() == "FALSE")) {
      testthat::expect_true(identical(normPath(Require:::cacheGetOptionCachePkgDir()), normPath(Require::cachePkgDir())))
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


  if (getRversion() >= "4.3.0") { # R 4.2.X and lower don't exist on PEuniverse so this test fails
    opts <- options(repos = PEUniverseRepo()); on.exit(options(opts), add = TRUE)
    out2 <- by(wh, seq(NROW(wh)), function(wh1Row) {
      out <- do.call(pkgDep, append(list("Require"), as.list(wh1Row[1, , drop = TRUE])))[[1]]
      o2 <- tools::toTitleCase(names(wh1Row)[unlist(wh1Row)])
      if (length(o2)) {
        pkgs <- gsub(x = unname(unlist(utilsOut[o2])), " ", "") # remove spaces
        out <- gsub(x = out, " ", "") # remove spaces
        # out <- setdiff(out, grep("R\\(.+", pkgs, value = TRUE, invert = TRUE))
      }
      setdiff(out, "remotes") # remotes was removed in version 0.2.6.9020
    })
    localDeps <- DESCRIPTIONFileDeps(system.file("DESCRIPTION", package = "Require"),
                                     which = c("Suggests", "Imports", "Depends"))
    locals <- setdiff(extractPkgName(localDeps), .basePkgs)
    testArgs <- setdiff(locals, unique(extractPkgName(unname(unlist(as.list(out2))))))
    # not sure why roxygen2 was not in it before; fpCompare is new, not yet on PEUniverse
    testArgs <- setdiff(testArgs, c("roxygen2", "rmarkdown", "fpCompare"))
    testthat::expect_identical(testArgs, character())
  }
  if (isDev) {
    # this was a bug created a warning when there was a package not on CRAN, but there
    #   were multiple repos; ffbase is no longer on CRAN
    # can't quiet this down on linux because ffbase is not binary but rest are ...
    #  install.packages won't do both types quiet = TRUE for some reason
    warns1 <- capture_warnings(
      Install("ff", # verbose = 0,
              repos = c(RSPM = urlForPositPACKAGES, CRAN = "https://cloud.r-project.org"
              ))
    )
    expect_identical(character(0), warns1)
  }

  if (isWindows()) {
    # test the new approach that installs outside R session -- is fine on Linux-alikes
    withr::local_options(Require.installPackagesSys = FALSE)
    ver <- "0.2.4"; ineq <- "<"

    Install(paste0("fpCompare (", ineq, ver, ")"), install = "force")
    ip <- installed.packages(noCache = TRUE) |> as.data.table()
    expect_true(compareVersion2(ip[Package %in% "fpCompare"]$Version, ver, inequality = ineq))


    #packageVersion("fpCompare") # doesn't update immediately
    ineq <- ">="
    warns <- capture_warnings(
      Install(paste0("fpCompare (", ineq, ver, ")"), install = "force"))
    ip <- installed.packages(noCache = TRUE) |> as.data.table()
    expect_true(compareVersion2(ip[Package %in% "fpCompare"]$Version, ver, inequality = ineq))

    # Require("fpCompare (>=0.2.4)", install = "force"))
    # packageVersion("fpCompare")
    if (!getOption("Require.usePak", TRUE)) {
      withr::local_options(Require.installPackagesSys = TRUE)
      mess <- capture_messages(Require("fpCompare (>=0.2.4)", install = "force", require = FALSE))
      warnsAfter <- capture_warnings(packageVersion("fpCompare"))
      # expect_true(grepl(.txtMsgIsInUse, warns))
      expect_false(isTRUE(grepl(.txtMsgIsInUse, warnsAfter)))
    }
    warns <- capture_warnings( # fpCompare namespace cannot be unloaded: cannot open file?
                               #  and also restarting interuupted promise evaluation
      try(detach("package:fpCompare", unload = TRUE), silent = TRUE) # some are not attaching
    )
  }

  if (FALSE) {
    pkgs <- c("fpCompare", "rlang", "cli", "crayon", "stringr", "lobstr")
    a <- unique(extractPkgName(unlist(unname(pkgDep(pkgs)))))
    cacheClearPackages(a, ask = FALSE)
    library(sys); library(waldo)
    setLibPaths(tempdir3())
    try(remove.packages(a))
    options(Require.installPackagesSys = 1L)
    ipBefore <- installed.packages(lib.loc = .libPaths()[1], noCache = TRUE)
    system.time(Install(pkgs, verbose = 1))
    # system.time(install.packages(pkgs))
    ipAfter <- installed.packages(lib.loc = .libPaths()[1], noCache = TRUE)
  }

  if (FALSE) { # benchmark pak and Require
    library(Require)
    options(Require.installPackagesSys = 2, Require.cloneFrom = Sys.getenv("R_LIBS_USER"))
    setLinuxBinaryRepo()
    pkgsKeep <- c('rlang', "R6", "cli", "withr", "magrittr", "data.table")
    Install(pkgsKeep)
    pkgs <- c("PredictiveEcology/fpCompare", "PredictiveEcology/reproducible@modsForLargeArchives",
              "rlang", "purrr", "ggplot2")
    a <- pkgDep(pkgs)
    N = 4
    # st <- list()
    st[["Require"]] <- system.time(replicate(N, {
      try(remove.packages(setdiff(extractPkgName(unname(unlist(a))), pkgsKeep)))
      cacheClearPackages(ask = FALSE)
      Install(pkgs)
    }))
    st[["pak"]] <- system.time(replicate(N, {
      try(remove.packages(setdiff(extractPkgName(unname(unlist(a))), pkgsKeep)))
      pak::cache_clean()
      pak::pkg_install(pkgs, ask = FALSE)
    }))
    Map(x = st, function(x) x[[3]]/N)
    #      min        lq      mean   median       uq      max neval
    #13.759182 13.873962 13.999130 13.98874 14.11910 14.24947     3
    # 7.367775  8.914831  9.495963 10.46189 10.56006 10.65823     3
  }

  if (getRversion() >= "4.3.0") { # R 4.2.x and below can't seem to build many of the PE ecosystem from src
    # Mistakenly have a partial repos, i.e., without getOption("repos") -- This failed previously Jul 2, 2024
    dir44 <- tempdir2(.rndstr(1))
    silence <- dir.create(dir44, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(dir44, recursive = TRUE), add = TRUE)
    warns <- capture_warnings(
      Require::Install("LandR", repos = "predictiveecology.r-universe.dev", libPaths = dir44,
                       standAlone = TRUE)
    )
    test <- testWarnsInUsePleaseChange(warns)
    expect_true(test)
  }


  ooo <- options(Require.cachePkgDir = NULL)
  testthat::expect_true(identical(cacheGetOptionCachePkgDir(), NULL))
  options(ooo)

})
