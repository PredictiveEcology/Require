thisFilename <- "test-5packagesLong.R"
startTime <- Sys.time()
message("\033[32m --------------------------------- Starting ",thisFilename,"  at: ",format(startTime),"---------------------------\033[39m")
origLibPathsAllTests <- .libPaths()

if (interactive()) {
  library(testit)
  library(Require)
  Sys.setenv("R_REMOTES_UPGRADE" = "never")
  Sys.setenv('CRANCACHE_DISABLE' = TRUE)
  outOpts <- options(#"Require.verbose" = 1,
                     "Require.persistentPkgEnv" = TRUE,
                     "install.packages.check.source" = "never",
                     "install.packages.compile.from.source" = "never",
                     "Require.unloadNamespaces" = FALSE)
  if (Sys.info()["user"] == "achubaty") {
    outOpts2 <- options("Require.Home" = "~/GitHub/PredictiveEcology/Require")
  } else {
    outOpts2 <- options("Require.Home" = "~/GitHub/Require")
  }
  tmpdir <- file.path(tempdir2("other"), paste0("RequireTmp", sample(1e5, 1)))

  suppressWarnings(dir.create(tmpdir))
  # repo <- chooseCRANmirror(ind = 1)


  pkgDepTest1 <- Require::pkgDep("Require")
  pkgDepTest2 <- Require::pkgDep2("Require")
  orig <- Require::setLibPaths(tmpdir, standAlone = TRUE, updateRprofile = FALSE)
  origDir <- setwd("~/GitHub/");

  theDir <- Require:::rpackageFolder(getOptionRPackageCache())
  if (!is.null(theDir)) {
    localBins <- dir(theDir, pattern = "data.table|remotes")
    localBinsFull <- dir(theDir, full.names = TRUE, pattern = "data.table|remotes")

    # localBins <- dir(getOption("Require.RPackageCache"), pattern = "data.table|remotes")
    # localBinsFull <- dir(getOption("Require.RPackageCache"), full.names = TRUE, pattern = "data.table|remotes")
    #
    vers <- gsub("^[^_]+\\_(.+)", "\\1", basename(localBins))
    vers <- gsub("^([^_]+)_+.+$", "\\1", vers)
    vers <- gsub("^([[:digit:]\\.-]+)\\.[[:alpha:]]{1,1}.+$", "\\1", vers)

    localBinsOrd <- order(package_version(vers), decreasing = TRUE)
    localBins <- localBins[localBinsOrd]
    localBinsFull <- localBinsFull[localBinsOrd]
    dups <- duplicated(gsub("(.+)\\_.+", "\\1", localBins))
    localBins <- localBins[!dups]
    localBinsFull <- localBinsFull[!dups]
    if (any(grepl("tar.gz", localBinsFull))) {
      localBinsFull <- grep("linux-gnu", localBinsFull, value = TRUE)
    }
    # THere might be more than one version
    dts <- grep("data.table", localBinsFull, value = TRUE)[1]
    rems <- grep("remotes", localBinsFull, value = TRUE)[1]
    localBinsFull <- c(dts, rems)

  } else {
    localBinsFull <- NULL
  }
  # THere might be more than one version
  dts <- grep("data.table", localBinsFull, value = TRUE)[1]
  #rems <- grep("remotes", localBinsFull, value = TRUE)[1]
  #localBinsFull <- c(dts, rems)
  localBinsFull <- dts

  Rpath <- Sys.which("Rscript")
  if (length(localBinsFull) == 2) {
    if (Require:::isWindows())
      system(paste0(Rpath, " -e \"install.packages(c('", localBinsFull[1], "', '", localBinsFull[2],
                    "'), quiet = TRUE, type = 'binary', lib ='", .libPaths()[1], "', repos = NULL)\""), wait = TRUE)
    else
      system(paste0(Rpath, " -e \"install.packages(c('", localBinsFull[1], "', '", localBinsFull[2],
                    "'), quiet = TRUE, lib ='", .libPaths()[1], "', repos = NULL)\""), wait = TRUE)
  } else {
    system(paste0(Rpath, " -e \"install.packages(c('data.table'), lib ='", .libPaths()[1],
                  "', quiet = TRUE, repos = '", getOption('repos')[["CRAN"]], "')\""), wait = TRUE)
  }

  if (is.null(getOption("Require.Home"))) stop("Must define options('Require.Home' = 'pathToRequirePkgSrc')")
  Require:::installRequire(getOption("Require.Home"))

  # system(paste0("R CMD INSTALL --library=", .libPaths()[1], " Require"), wait = TRUE)
  setwd(origDir)

  on.exit({
    message(".libPaths during packagesLong: ", .libPaths())
    Require::setLibPaths(orig, updateRprofile = FALSE)
    })

  testit::assert({length(pkgDepTest1) == 1})
  testit::assert({sort(pkgDepTest1[[1]]) == c("data.table (>= 1.10.4)")})

  testit::assert({length(pkgDepTest2) == 1})
  testit::assert({sort(names(pkgDepTest2)) == sort(pkgDepTest1$Require)})

  tmpdirForPkgs <- gsub(".+ ([[:digit:]]\\.[[:digit:]])\\.[[:digit:]].+", "\\1", R.version.string)
  pkgsInstalled <- dir(tmpdirForPkgs, full.names = TRUE)
  RequireDeps <- c("data.table", "utils", "callr", "cli", "covr",
                   "crayon", "desc", "digest", "DT", "ellipsis", "BH", "units",
                   "git2r", "glue", "httr", "jsonlite", "memoise", "pkgbuild", "pkgload",
                   "rcmdcheck", "remotes", "rlang", "roxygen2", "rstudioapi", "rversions",
                   "sessioninfo", "stats", "testthat", "tools", "usethis", "utils", "withr", "Require")
  pkgsToRm <- setdiff(sample(basename(pkgsInstalled), min(length(pkgsInstalled), 5)),
                      RequireDeps)
  out <- unlink(pkgsToRm, recursive = TRUE)

  runTests <- function(have, pkgs) {
    # recall LandR.CS won't be installed, also, Version number is not in place for newly installed packages
    testit::assert({all(!is.na(have[installed == TRUE]$Version))})
    out <- try(testit::assert({
      all(have[loadOrder > 0 & (correctVersion == TRUE | hasVersionSpec == FALSE)]$loadOrder > 0)
    }))
    if (is(out, "try-error")) stop("Error 855; please contact developer")
    couldHaveLoaded <- gsub(".*\\<mumin\\>.*", "MuMIn", unique(pkgs))
    # couldHaveLoaded <- setdiff(unique(Require:::extractPkgName(pkgs)) , "mumin")

    actuallyLoaded <- if ("correctVersionAvail" %in% colnames(have)) {
      didntLoad <- have[packageFullName %in% couldHaveLoaded & correctVersionAvail  == FALSE]
      # didntLoad <- have[Package %in% couldHaveLoaded & correctVersionAvail == FALSE]
      setdiff(couldHaveLoaded, didntLoad$packageFullName)
    } else {
      couldHaveLoaded
    }

    theTest <- isTRUE(all.equal(unique(sort(extractPkgName(actuallyLoaded))),
                                sort(unique(have[loadOrder > 0]$Package))))
    browser(expr = !theTest)
    testit::assert({isTRUE(theTest)})
  }
  unloadNSRecursive <- function(packages, n = 0) {
    if (!missing(packages)) {
      out <- packages
      browser(expr = "bitops" %in% packages)
    } else {
      out <- data.table::as.data.table(.installed.pkgs(which = character()))[[1]]
      names(out) <- out
      out <- lapply(out, isNamespaceLoaded)
      out <- unlist(out)
      out <- out[out]
      keepLoaded1 <- c("Require", "testit", "base64enc", "RCurl", "dismo", "units",
                       "fastmatch", "raster", "Rcpp", "rstudioapi", "crayon", "data.table",
                       "tools", "utils", "versions", "fastdigest",
                       "grDevices", "methods", "stats", "graphics", "dplyr", "stringi")
      keepLoaded = unique(c(keepLoaded1, dir(tail(.libPaths(),1))))
      out <- names(out)
      names(out) <- out
      out <- unique(setdiff(out, keepLoaded))
    }
    if (length(out) > 0) {

      names(out) <- out
      out1 <- lapply(out, function(pInner) {
        names(pInner) <- pInner
        if (isNamespaceLoaded(pInner)) {
          out <- tryCatch(unloadNamespace(pInner), error = function(x) FALSE)
          if (is.null(out)) out <- TRUE
          out
        }
      })

      out2 <- unlist(out1)
      if (sum(out2) > 0) {
        out3 <- out2[out2]
        sam <- sample(names(out3), size = n)
        if (n > 0) {
          message("removing ", paste(sam, collapse = ", "))

          files <- dir(.libPaths()[1], recursive = TRUE, full.names = TRUE)
          origDir <- Require::normPath(file.path(.libPaths()[1]))
          origDirWithPkg <- file.path(origDir, sam)
          files <- grep(Require::normPath(origDirWithPkg), files, value = TRUE)
          td <- Require::normPath(file.path(tempdir2("other"), .rndstr(1, 6)))
          newFiles <- gsub(origDir, td, files)
          dirs <- unique(dirname(files))
          newDirs <- gsub(origDir, td, dirs)
          Require::checkPath(td, create = TRUE)
          #dir.create(dirname(td))
          #dir.create(td)
          out <- lapply(newDirs, dir.create)

          outFC <- file.copy(files, newFiles)
          if (any(outFC == FALSE)) {
            file.copy(newFiles, files)
          }
          unlink1 <- unlink(file.path(origDir, sam), recursive = TRUE)
          if (isTRUE(any(file.exists(files)))) {
            suppressWarnings({
              out <- lapply(dirs, dir.create, recursive = TRUE)
            })
            out <- file.copy(newFiles, files)
            message("Actually, not deleting ", sam)
          } else {
            browser(expr = sam %in% dir(.libPaths()[1]))
            out11 <- utils::capture.output({
              out <- utils::capture.output({
                utils::remove.packages(sam)
              }, type = "message")
            }, type = "output")
          }
        }
      }
    } else {
      out2 <- out
    }
    return(out2)
  }

  pkgs <- list(c("LearnBayes (<=4.0.4)", "tinytest (<= 1.0.3)", "glmm (<=1.3.0)",
                 "achubaty/amc@development", "PredictiveEcology/LandR@development (>=0.0.1)",
                 "PredictiveEcology/LandR@development (>=0.0.2)", "ianmseddy/LandR.CS (<=0.0.1)"),
               c("SpaDES.core (>=0.9)",
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
               c("SpaDES.core (>=0.9)",
                 "PredictiveEcology/map@development (>= 5.0.0.9)",
                 "achubaty/amc@development (>=0.1.5)",
                 "data.table (>=100.0)",
                 paste0("tinytest (>=1.3.1)"),
                 "PredictiveEcology/LandR@development (>= 1.0.2)"),
               c("fastdigest (>=0.0.0.9)",
                 "PredictiveEcology/map@development (>= 0.0.0.9)",
                 "achubaty/amc@development (>=0.0.0.9)",
                 "data.table (>=0.0.0.9)",
                 paste0("tinytest (>=1.3.1)"),
                 "PredictiveEcology/LandR@development(>= 0.0.0.9)"),
               # Multiple conflicting version numbers, and with NO version number
               c("fastdigest (>=0.0.0.8)", "fastdigest (>=0.0.0.9)", "fastdigest"), #"quickPlot", "testthat"),
               c("fastdigest (>=1000.0.0.8)", "fastdigest (>=0.0.0.9)", "fastdigest"),
               #          "quickPlot", "testthat"),
               c("fastdigest (>=0.0.0.9)",
                 "PredictiveEcology/map@development (>= 0.0.0.9)",
                 "PredictiveEcology/map@development (>= 0.0.0.10)",
                 "PredictiveEcology/map@development (>= 110.0.9)",
                 "achubaty/amc@development (>=0.0.0.9)",
                 "data.table (>=0.0.0.9)",
                 paste0("tinytest (>=1.3.1)"),
                 "PredictiveEcology/LandR@development(>= 0.0.0.9)"),
               "LearnBayes (>=1000.3.1)",
               c("LearnBayes (>=1.0.1)", "fpCompare"),
               "LearnBayes (>=2.15.1)",
               c("rforge/mumin/pkg", MuMIn = "rforge/mumin/pkg", "A3")
  )
  #   options("reproducible.Require.install" = TRUE)

  i <- 0
  pkg <- pkgs[[i + 1]] # redundant, but kept for interactive use
  #}
  for (pkg in pkgs) {
    # out <- unloadNSRecursive(n = 1)
    i <- i + 1
    print(paste0(i, ": ", paste0(Require::extractPkgName(pkg), collapse = ", ")))
    #if (i == 11) ._Require_0 <<- 1
    outFromRequire <- Require(pkg, standAlone = FALSE, require = FALSE)
    out <- Require(pkg, require = FALSE)
    testit::assert({all.equal(outFromRequire, out)})
    have <- attr(out, "Require")
    pkgsToTest <- unique(Require::extractPkgName(pkg))
    names(pkgsToTest) <- pkgsToTest
    # suppressWarnings(normalRequire <- unlist(lapply(pkgsToTest,
    #                                function(p) tryCatch(require(p, character.only = TRUE),
    #                                                     error = function(x) FALSE))))
    # out2 <- out
    # out2 <- out2[names(out2) %in% names(normalRequire)]
    # whMatch <- match(names(normalRequire), names(out2))
    # whMatch <- whMatch[!is.na(whMatch)]
    # out2 <- out2[whMatch]
    # have2 <- have[loadOrder > 0]
    # normalRequire2 <- if (NROW(have2))
    #   normalRequire[have2$Package]
    # else
    #   normalRequire

    # browser(expr = all(unique(Require:::extractPkgName(pkg)) %in% "fastdigest"))
    # if (length(out2)) {
    #   out2 <- out2[out2]
    #   normalRequire2 <- normalRequire2[!is.na(normalRequire2)][normalRequire2]
    #   browser(expr = !all(out2[order(names(out2))] == normalRequire2[order(names(normalRequire2))]))
    #   testit::assert({all(out2[order(names(out2))] == normalRequire2[order(names(normalRequire2))])})
    #   runTests(have, pkg)
    # } else {
    #   # TODO: what goes here?
    # }
    # suppressWarnings(rm(outFromRequire, out, have, normalRequire))
    # if (any("tinytest" %in% Require::extractPkgName(pkg))) {
    #   try(unloadNamespace("LearnBayes"))
    #   try(unloadNamespace("tinytest"))
    #   try(remove.packages(c("tinytest", "LearnBayes")))
    # }
  }

  # Use a mixture of different types of "off CRAN"
  pkgs <- c("ggplot", "gdalUtils", "ggplot2 (==3.3.4)", "silly1", "SpaDES.core")
  pkgsClean <- extractPkgName(pkgs)
  lala <- capture.output(suppressMessages(remove.packages(pkgsClean)))
  Require(pkgs, verbose = -1, require = FALSE)
  ip <- installed.packages()
  testit::assert(sum(pkgsClean %in% ip[, "Package"]) == length(pkgsClean) - 1) # silly1 won't be installed



  # two sources, where both are OK; use CRAN by preference
  out <- capture.output(remove.packages("SpaDES.core"))
  out <- Require(c("PredictiveEcology/SpaDES.core (>= 1.0.0)", "SpaDES.core (>=1.0.0)"),
                 require = FALSE, verbose = 2)
  out2 <- attr(out, "Require")
  try(unlink(dir(RequirePkgCacheDir(), pattern = "SpaDES.core", full.names = TRUE)))
  testit::assert(out2[Package == "SpaDES.core"]$installFrom == "CRAN")
  testit::assert(out2[Package == "SpaDES.core"]$installed)


  unlink(tmpdir, recursive = TRUE)
  options(outOpts)
  if (exists("outOpts2")) options(outOpts2)
  if (!identical(origLibPathsAllTests, .libPaths()))
    Require::setLibPaths(origLibPathsAllTests, standAlone = TRUE, exact = TRUE)
}

# unlink(tmpdir, recursive = TRUE)
endTime <- Sys.time()
message("\033[32m ----------------------------------",thisFilename, ": ", format(endTime - startTime)," \033[39m")
