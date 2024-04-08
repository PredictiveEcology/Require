test_that("test 5", {

  setupInitial <- setupTest()
  # on.exit(endTest(setupInitial))

  isDev <- getOption("Require.isDev")
  isDevAndInteractive <- getOption("Require.isDevAndInteractive")

  if (isDevAndInteractive) {
    tmpdir <- file.path(tempdir2(paste0("RequireTmp", sample(1e5, 1))))

    dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
    # repo <- chooseCRANmirror(ind = 1)

    pkgDepTest1 <- Require::pkgDep("Require", includeSelf = FALSE)
    pkgDepTest2 <- Require::pkgDep2(c("Require"), # simplify = FALSE,
                                    # which = c("Depends", "Imports"),
                                    includeSelf = FALSE)
    orig <- Require::setLibPaths(tmpdir, standAlone = TRUE, updateRprofile = FALSE)
    # origDir <- setwd("~/GitHub/")

    # theDir <- Require:::rpackageFolder(getOptionRPackageCache())
    #
    # if (!is.null(theDir)) {
    #   localBins <- dir(theDir, pattern = "data.table|remotes")
    #   localBinsFull <- dir(theDir, full.names = TRUE, pattern = "data.table|remotes")
    #
    #   # localBins <- dir(getOption("Require.RPackageCache"), pattern = "data.table|remotes")
    #   # localBinsFull <- dir(getOption("Require.RPackageCache"), full.names = TRUE, pattern = "data.table|remotes")
    #   #
    #   vers <- gsub("^[^_]+\\_(.+)", "\\1", basename(localBins))
    #   vers <- gsub("^([^_]+)_+.+$", "\\1", vers)
    #   vers <- gsub("^([[:digit:]\\.-]+)\\.[[:alpha:]]{1,1}.+$", "\\1", vers)
    #
    #   localBinsOrd <- order(do.call(c, lapply(vers, packageVersion)), decreasing = TRUE)
    #   localBins <- localBins[localBinsOrd]
    #   localBinsFull <- localBinsFull[localBinsOrd]
    #   dups <- duplicated(gsub("(.+)\\_.+", "\\1", localBins))
    #   localBins <- localBins[!dups]
    #   localBinsFull <- localBinsFull[!dups]
    #   if (any(grepl("tar.gz", localBinsFull))) {
    #     localBinsFull <- grep("linux-gnu", localBinsFull, value = TRUE)
    #   }
    #   # THere might be more than one version
    #   dts <- grep("data.table", localBinsFull, value = TRUE)[1]
    #   rems <- grep("remotes", localBinsFull, value = TRUE)[1]
    #   localBinsFull <- c(dts, rems)
    # } else {
    #   localBinsFull <- NULL
    # }
    # THere might be more than one version
    # dts <- grep("data.table", localBinsFull, value = TRUE)[1]
    # # rems <- grep("remotes", localBinsFull, value = TRUE)[1]
    # # localBinsFull <- c(dts, rems)
    # localBinsFull <- dts
    #
    # Rpath <- Sys.which("Rscript")
    # if (length(localBinsFull) == 2) {
    #   if (Require:::isWindows()) {
    #     system(paste0(
    #       Rpath, " -e \"install.packages(c('", localBinsFull[1], "', '", localBinsFull[2],
    #       "'), quiet = TRUE, type = 'binary', lib ='", .libPaths()[1], "', repos = NULL)\""
    #     ), wait = TRUE)
    #   } else {
    #     system(paste0(
    #       Rpath, " -e \"install.packages(c('", localBinsFull[1], "', '", localBinsFull[2],
    #       "'), quiet = TRUE, lib ='", .libPaths()[1], "', repos = NULL)\""
    #     ), wait = TRUE)
    #   }
    # } else {
    #   system(paste0(
    #     Rpath, " -e \"install.packages(c('data.table'), lib ='", .libPaths()[1],
    #     "', quiet = TRUE, repos = '", getOption("repos")[["CRAN"]], "')\""
    #   ), wait = TRUE)
    # }

    # if (is.null(getOption("Require.Home"))) stop("Must define options('Require.Home' = 'pathToRequirePkgSrc')")
    # Require:::installRequire(getOption("Require.Home"))

    # system(paste0("R CMD INSTALL --library=", .libPaths()[1], " Require"), wait = TRUE)
    # setwd(origDir)

    # on.exit({
    #   message(".libPaths during packagesLong: ", paste(.libPaths(), collapse = "; "))
    #   Require::setLibPaths(orig, updateRprofile = FALSE)
    # })

    testthat::expect_true({
      length(pkgDepTest1) == 1
    })
    testthat::expect_true({
      any(sort(pkgDepTest1[[1]]) %in% c("data.table (>= 1.10.4)"))
    })

    testthat::expect_true({
      length(pkgDepTest2[[1]]) == 1
    })
    testthat::expect_true({
      all(sort(names(pkgDepTest2$Require)) == sort(pkgDepTest1$Require))
    })

    tmpdirForPkgs <- gsub(".+ ([[:digit:]]\\.[[:digit:]])\\.[[:digit:]].+", "\\1", R.version.string)
    pkgsInstalled <- dir(tmpdirForPkgs, full.names = TRUE)
    RequireDeps <- c(
      "data.table", "utils", "callr", "cli", "covr",
      "crayon", "desc", "digest", "DT", "ellipsis", "BH", "units",
      "git2r", "glue", "httr", "jsonlite", "memoise", "pkgbuild", "pkgload",
      "rcmdcheck", "rlang", "roxygen2", "rstudioapi", "rversions",
      "sessioninfo", "stats", "testthat", "tools", "usethis", "utils", "withr", "Require"
    )
    pkgsToRm <- setdiff(
      sample(basename(pkgsInstalled), min(length(pkgsInstalled), 5)),
      RequireDeps
    )
    out <- unlink(pkgsToRm, recursive = TRUE)

        pkgs <- list(
      c(
        "LearnBayes (<=4.0.4)", "tinytest (<= 1.0.3)", "glmm (<=1.4.3)",
        "SpaDES.tools (>=2.0.5)", "terra (>=1.7-71)",
        "reproducible (>=2.0.2)", "PredictiveEcology/reproducible@development (>=2.0.0)", # Until reproducible 2.0.2 is on CRAN
        "achubaty/amc@development", "PredictiveEcology/LandR@development (>=0.0.1)",
        "PredictiveEcology/LandR@development (>=0.0.2)", "ianmseddy/LandR.CS (<=0.0.1)"
      ),
      c(
        "PredictiveEcology/SpaDES.core@development (>=1.1.2)",
        "PredictiveEcology/map@development (>= 4.0.9)",
        "achubaty/amc@development (>=0.1.5)", "data.table (>=100.0)",
        "tinytest (>=1.3.1)", "PredictiveEcology/LandR@development (>= 1.0.2)",
        "versions (>=0.3)",
        "fastdigest (>=0.0.0.9)", "PredictiveEcology/map@development (>= 0.1.0.9)",
        "achubaty/amc@development (>=0.0.0.9)", "data.table (>=0.0.0.9)",
        "PredictiveEcology/LandR@development(>= 0.0.0.9)", "fastdigest (>=1000.0.0.8)",
        "fastdigest", "quickPlot", "testthat",
        "PredictiveEcology/map@development (>= 0.0.0.9)",
        "PredictiveEcology/map@development (>= 0.0.0.10)",
        "PredictiveEcology/map@development (>= 1111.0.9)",
        "PredictiveEcology/map@master (>= 0.0.0.9)",
        "PredictiveEcology/map@master (>= 0.0.0.10)"
      ),
      c(
        "PredictiveEcology/SpaDES.core@development (>=1.1.2)",
        "PredictiveEcology/map@development (>= 5.0.0.9)",
        "achubaty/amc@development (>=0.1.5)",
        "data.table (>=100.0)",
        paste0("tinytest (>=1.3.1)"),
        "PredictiveEcology/LandR@development (>= 1.0.2)"
      ),
      c(
        "fastdigest (>=0.0.0.9)",
        "PredictiveEcology/map@development (>= 0.0.0.9)",
        "achubaty/amc@development (>=0.0.0.9)",
        "data.table (>=0.0.0.9)",
        paste0("tinytest (>=1.3.1)"),
        "PredictiveEcology/LandR@development(>= 0.0.0.9)"
      ),
      # Multiple conflicting version numbers, and with NO version number
      c("fastdigest (>=0.0.0.8)", "fastdigest (>=0.0.0.9)", "fastdigest"), # "quickPlot", "testthat"),
      c("fastdigest (>=1000.0.0.8)", "fastdigest (>=0.0.0.9)", "fastdigest"),
      #          "quickPlot", "testthat"),
      c(
        "fastdigest (>=0.0.0.9)",
        "PredictiveEcology/map@development (>= 0.0.0.9)",
        "PredictiveEcology/map@development (>= 0.0.0.10)",
        "PredictiveEcology/map@development (>= 110.0.9)",
        "achubaty/amc@development (>=0.0.0.9)",
        "data.table (>=0.0.0.9)",
        paste0("tinytest (>=1.3.1)"),
        "PredictiveEcology/LandR@development(>= 0.0.0.9)"
      ),
      "LearnBayes (>=1000.3.1)",
      c("LearnBayes (>=1.0.1)", "fpCompare"),
      "LearnBayes (>=2.15.1)",
      c("r-forge/mumin/pkg", MuMIn = "r-forge/mumin/pkg", "A3")
    )
    #   options("reproducible.Require.install" = TRUE)

    i <- 0
    pkg <- pkgs[[i + 1]] # redundant, but kept for interactive use
    # }
    for (pkg in pkgs) {
      # out <- unloadNSRecursive(n = 1)
      i <- i + 1
      Require:::messageVerbose(paste0("\033[32m", i, ": ", paste0(Require::extractPkgName(pkg), collapse = comma), "\033[39m"),
                               verboseLevel = 0
      )
      # if (i == 11) ._Require_0 <<- 1
      pkg <- omitPkgsTemporarily(pkg)

      (outFromRequire <- Require(pkg, standAlone = FALSE, require = FALSE)) |>
        capture_warnings() -> warns

      test <- testWarnsInUsePleaseChange(warns)
      expect_true(test)

      # Rerun it to get output table, but capture messages for quiet; should be no installs
      (out <- Require(pkg, standAlone = FALSE, require = FALSE, returnDetails = TRUE)) |>
        capture_warnings() -> warns

      test <- testWarnsInUsePleaseChange(warns)
      expect_true(test)

      testthat::expect_true(
        all.equal(out, outFromRequire, check.attributes = FALSE)
      )
      have <- attr(out, "Require")
      have <- have[!Package %in% c("Require", "testthat")] # these don't have Version number because they may be load_all'd
      pkgsToTest <- unique(Require::extractPkgName(pkg))
      names(pkgsToTest) <- pkgsToTest
      runTests(have, pkg)
      endTime <- Sys.time()
    }

    # Use a mixture of different types of "off CRAN"
    if (!isMacOSX()) {
      pkgs <- c("knn", "gdalUtils", "ggplot2 (==3.3.4)", "silly1", "SpaDES.core")
      pkgsClean <- extractPkgName(pkgs)
      lala <- suppressWarnings(capture.output(suppressMessages(remove.packages(pkgsClean))))

      # ERROR: dependency 'Require' is not available for package 'SpaDES.core' --> doesn't show up
      acceptableFails <- c("Require", "SpaDES.core")
      warns <- capture_warnings(# package 'Require' is in use and will not be installed
        out22 <- Require(pkgs, require = FALSE, returnDetails = TRUE)
      )
      ip <- installed.packages() ## silly1 won't be installed

      # depending on whether SpaDES.core gets installed... could be poss1 or poss2
      poss1 <- identical(setdiff(pkgsClean, ip[, "Package"]), pkgs[[4]])  # installs SpaDES.core anyway

      # This one fails to install SpaDES.core because already loaded
      poss2 <- sum(pkgsClean %in% ip[, "Package"]) ==
        length(pkgsClean) - length(acceptableFails) ## TODO: fails on macOS
      expect_true(poss1 || poss2)
    }

    ## Test Install and also (HEAD)
    capted1 <- capture.output(
      type = "message",
      out1 <- Install("PredictiveEcology/fpCompare@development (HEAD)", verbose = 5, returnDetails = TRUE) # will install
    )
    capted2 <- capture.output(
      type = "message",
      out2 <- Install("PredictiveEcology/fpCompare@development (HEAD)", verbose = 5, returnDetails = TRUE) # will install
    )
    theGrep1 <- "Installing from"
    theGrep2 <- "SHA1 has not"
    testthat::expect_true(isTRUE(sum(grepl(theGrep1, capted1)) == 1))
    testthat::expect_true(isTRUE(sum(grepl(theGrep2, capted2)) == 1))

    # two sources, where both are OK; use CRAN by preference
    if (!isMacOSX()) {
      lala <- suppressWarnings(capture.output(suppressMessages(
        remove.packages("SpaDES.core")))) ## TODO: fails on macOS
      suppressWarnings(
        out <- Require(c("PredictiveEcology/SpaDES.core@development (>=1.1.2)",
                         "SpaDES.core (>=1.0.0)"),
                       require = FALSE, returnDetails = TRUE
        )
      )
      out2 <- attr(out, "Require")
      # try(unlink(dir(RequirePkgCacheDir(), pattern = "SpaDES.core", full.names = TRUE)))
      testthat::expect_true(out2[Package == "SpaDES.core"]$installFrom %in% c("CRAN", "Local"))
      # if (isWindows())
      testthat::expect_true(out2[Package == "SpaDES.core"]$installed)

    }
   }

})
