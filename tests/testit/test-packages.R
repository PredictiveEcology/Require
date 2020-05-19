isInteractive <- function() TRUE
chooseCRANmirror2 <- function() {
  repos <- NULL
  repos2 <- "https://cloud.r-project.org"
  repos["CRAN"] <- repos2
  options("repos" = repos)
  repos
}
available.packagesCRAN <- function(repos) {
  out <- load(system.file(".cacheAvailablePackages.rda", package = "Require"))
  get(out, inherits = FALSE)
}
assignInNamespace("available.packagesCRAN", available.packagesCRAN, ns = "Require")
assignInNamespace("chooseCRANmirror2", chooseCRANmirror2, ns = "Require")
assignInNamespace("isInteractive", isInteractive, ns = "Require")

repos <- Require:::getCRANrepos()
testit::assert(is.character(repos))
options("repos" = repos) # shouldn't be necessary now
options("Require.purge" = TRUE)

# Failure on Travis:
# cannot open file 'startup.Rs': No such file or directory
# suggested solution https://stackoverflow.com/a/27994299/3890027
Sys.setenv("R_TESTS" = "")
Sys.setenv("R_REMOTES_UPGRADE" = "never")

library(testit)
tmpdir <- if (Sys.info()["user"] != "emcintir") {
  file.path(tempdir(), paste0("RequireTmp", sample(1e5, 1)))
} else {
  "~/TempLib5"
}
suppressWarnings(dir.create(tmpdir))
oldLibPaths <- .libPaths()
on.exit(Require::setLibPaths(oldLibPaths))
pkgDepTest1 <- Require::pkgDep("Require")

Require::setLibPaths(oldLibPaths)
###
#if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
if (interactive()) {
  pkgDepTest2 <- Require::pkgDep2("Require")
  Require::setLibPaths(tmpdir, standAlone = TRUE)

  testit::assert(length(pkgDepTest1) == 1)
  testit::assert(sort(pkgDepTest1[[1]]) == c("data.table (>= 1.10.4)", "remotes"))

  testit::assert(length(pkgDepTest2) == 2)
  testit::assert(sort(names(pkgDepTest2)) == sort(pkgDepTest1$Require))

  pkgsInstalled <- dir(tmpdir, full.names = TRUE)
  RequireDeps <- c("data.table", "remotes", "utils", "callr", "cli", "covr",
                   "crayon", "desc", "digest", "DT", "ellipsis", "BH", "units",
                   "git2r", "glue", "httr", "jsonlite", "memoise", "pkgbuild", "pkgload",
                   "rcmdcheck", "remotes", "rlang", "roxygen2", "rstudioapi", "rversions",
                   "sessioninfo", "stats", "testthat", "tools", "usethis", "utils",
                   "withr")
  pkgsToRm <- setdiff(sample(basename(pkgsInstalled), min(length(pkgsInstalled), 5)), RequireDeps)
  message("Deleting: ", paste(basename(pkgsToRm), collapse = ", "))
  out <- unlink(pkgsToRm, recursive = TRUE)

  runTests <- function(have, pkgs) {
    # recall LandR.CS won't be installed, also, Version number is not in place for newly installed packages
    testit::assert(all(!is.na(have[installed == TRUE]$Version)))
    testit::assert(all(have[toLoad == TRUE & (correctVersion == TRUE | hasVersionSpec == FALSE)]$toLoad))
    couldHaveLoaded <- setdiff(unique(Require:::extractPkgName(pkgs)) , "mumin")
    actuallyLoaded <- if ("correctVersionAvail" %in% colnames(have)) {
      didntLoad <- have[Package %in% couldHaveLoaded & correctVersionAvail == FALSE]
      setdiff(couldHaveLoaded, didntLoad$Package)
    } else {
      couldHaveLoaded
    }

    theTest <- isTRUE(all.equal(sort(actuallyLoaded), sort(have[toLoad == TRUE]$Package)))
    browser(expr = !theTest)
  }
  unloadNSRecursive <- function(packages, n = 0) {
    if (!missing(packages)) {
      out <- packages
      browser(expr = "bitops" %in% packages)
    } else {
      out <- data.table::as.data.table(installed.packages(noCache = TRUE))[[1]]
      names(out) <- out
      out <- lapply(out, isNamespaceLoaded)
      out <- unlist(out)
      out <- out[out]
      keepLoaded1 <- c("Require", "testit", "base64enc", "RCurl", "dismo", "units",
                       "fastmatch", "raster", "Rcpp", "rstudioapi", "crayon", "data.table",
                       "remotes", "tools", "utils", "versions", "fastdigest",
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
          td <- Require::normPath(file.path(tempdir(), Require::rndstr(1, 6)))
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
            suppressWarnings(out <- lapply(dirs, dir.create, recursive = TRUE))
            out <- file.copy(newFiles, files)
            message("Actually, not deleting ", sam)
          } else {
            browser(expr = sam %in% dir(.libPaths()[1]))
            out11 <- capture.output({
              out <- capture.output(remove.packages(sam), type = "message")
            }, type = "output")
          }
        }
      }
    } else {
      out2 <- out
    }
    return(out2)
  }

  pkgs <- list(c("Holidays (<=1.0.4)", "TimeWarp (<= 1.0.3)", "glmm (<=1.3.0)",
                 "achubaty/amc@development", "PredictiveEcology/LandR@development (>=0.0.1)",
                 "PredictiveEcology/LandR@development (>=0.0.2)", "ianmseddy/LandR.CS (<=0.0.1)"),
               c("SpaDES.core (>=0.9)",
                 "PredictiveEcology/map@development (>= 4.0.9)",

                 "achubaty/amc@development (>=0.1.5)", "data.table (>=100.0)",
                 "digest (>=0.6.23)", "PredictiveEcology/LandR@development (>= 1.0.2)",
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
                 paste0("digest (>=0.6.25)"),
                 "PredictiveEcology/LandR@development (>= 1.0.2)"),
               c("fastdigest (>=0.0.0.9)",
                 "PredictiveEcology/map@development (>= 0.0.0.9)",
                 "achubaty/amc@development (>=0.0.0.9)",
                 "data.table (>=0.0.0.9)",
                 paste0("digest (>=0.6.25)"),
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
                 paste0("digest (>=0.6.25)"),
                 "PredictiveEcology/LandR@development(>= 0.0.0.9)"),
               "Holidays (>=1000.3.1)",
               c("Holidays (>=1.0.1)", "fpCompare"),
               "Holidays (>=1.3.1)",
               c("rforge/mumin/pkg", MuMIn = "rforge/mumin/pkg", "PredictiveEcology/LandR",
                 "PredictiveEcology/LandR@development", "A3")
  )
  #   options("reproducible.Require.install" = TRUE)
  options("Require.verbose" = TRUE)

  i <- 0
  pkg <- pkgs[[1]] # redundant, but kept for interactive use
  for (pkg in pkgs) {
    # out <- unloadNSRecursive(n = 1)
    i <- i + 1
    print(paste0(i, ": ", paste0(Require::extractPkgName(pkg), collapse = ", ")))
    outFromRequire <- Require::Require(pkg, repos = repo, standAlone = FALSE)
    out <- Require::Require(pkg)
    testit::assert(all.equal(outFromRequire, out))
    have <- attr(out, "Require")
    pkgsToTest <- unique(Require::extractPkgName(pkg))
    names(pkgsToTest) <- pkgsToTest
    normalRequire <- unlist(lapply(pkgsToTest,
                                   function(p) tryCatch(require(p, character.only = TRUE),
                                                        error = function(x) FALSE)))
    out2 <- out
    out2 <- out2[names(out2) %in% names(normalRequire)]
    whMatch <- match(names(normalRequire), names(out2))
    whMatch <- whMatch[!is.na(whMatch)]
    out2 <- out2[whMatch]
    have2 <- have[toLoad == TRUE]
    normalRequire2 <- if (NROW(have2))
      normalRequire[have2$Package]
    else
      normalRequire

    # browser(expr = all(unique(Require:::extractPkgName(pkg)) %in% "fastdigest"))
    if (length(out2)) {
      out2 <- out2[out2]
      normalRequire2 <- normalRequire2[!is.na(normalRequire2)]
      browser(expr = !all(out2[order(names(out2))] == normalRequire2[order(names(normalRequire2))]))
      testit::assert(all(out2[order(names(out2))] == normalRequire2[order(names(normalRequire2))]))
      runTests(have, pkg)
    } else {

    }
    suppressWarnings(rm(outFromRequire, out, have, normalRequire))
    if (any("TimeWarp" %in% Require::extractPkgName(pkg))) {
      unloadNamespace("Holidays")
      unloadNamespace("TimeWarp")
      remove.packages(c("Holidays", "TimeWarp"))
    }
  }
}

dir1 <- tempdir2("test1")
options("Require.verbose" = TRUE)
out <- Require::Require("TimeWarp (<= 2.3.1)", standAlone = TRUE, libPaths = dir1)
testit::assert(data.table::is.data.table(attr(out, "Require")))
ip <- as.data.table(installed.packages(lib.loc = dir1))[[1]]
testit::assert(isTRUE(out))
testit::assert("TimeWarp" %in% ip)
detach("package:TimeWarp", unload = TRUE)

# Try older version
dir2 <- tempdir2("test2")
pvWant <- "1.0-7"
inst <- Require::Require(paste0("TimeWarp (<=",pvWant,")"), standAlone = TRUE, libPaths = dir2)
pv <- packageVersion("TimeWarp")
testit::assert(pv == pvWant)
detach("package:TimeWarp", unload = TRUE)

# Try github
dir3 <- tempdir2("test3")
inst <- Require::Require("PredictiveEcology/Require", install = "force",
                         require = FALSE,
                         standAlone = FALSE, libPaths = dir3)
pkgs <- c("data.table", "remotes", "Require")
ip <- as.data.table(installed.packages(lib.loc = dir3))[[1]]
testit::assert(isTRUE(all.equal(sort(pkgs),
                                sort(ip))))


# Try github with version
dir4 <- tempdir2("test4")
mess <- capture.output({
  inst <- Require::Require("PredictiveEcology/Require (>=2.0.0)",
                           require = FALSE,
                           standAlone = FALSE, libPaths = dir4)
}, type = "message")
testit::assert(isFALSE(inst))
testit::assert(length(mess) > 0)
testit::assert(sum(grepl("could not be installed", mess)) == 1)

unlink(dirname(dir3), recursive = TRUE)
