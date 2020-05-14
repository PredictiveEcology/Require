tmpdir <- if (Sys.info()["user"] != "emcintir") {
  file.path(tempdir(), paste0("RequireTmp", sample(1e5, 1)))
} else {
  "c:/Eliot/TempLib5"
}
suppressWarnings(dir.create(tmpdir))
oldLibPaths <- .libPaths()
on.exit(.libPaths(oldLibPaths))
.libPaths(tmpdir)

pkgDepTest1 <- Require::pkgDep("Require")
testit::assert(length(pkgDepTest1) == 1)
testit::assert(sort(pkgDepTest1[[1]]) == c("data.table (>= 1.10.4)", "remotes", "utils"))

pkgDepTest2 <- Require::pkgDep2("Require")
testit::assert(length(pkgDepTest2) == 3)
testit::assert(sort(names(pkgDepTest2)) == sort(pkgDepTest1$Require))

if (!identical(Sys.getenv("NOT_CRAN"), "true")) {

  pkgsInstalled <- dir(tmpdir, full.names = TRUE)
  RequireDeps <- c("data.table", "remotes", "utils")
  pkgsToRm <- setdiff(sample(pkgsInstalled, 5), RequireDeps)
  message("Deleting: ", paste(basename(pkgsToRm), collapse = ", "))
  out <- unlink(pkgsToRm, recursive = TRUE)

  runTests <- function(have) {
    testit::assert(all(!is.na(have[installed == TRUE]$Version)))
    testit::assert(all(have[hasVersionSpec == TRUE & correctVersion == TRUE]$toLoad))
    if (any(have$installFrom == "Fail", na.rm = TRUE))
      testit::assert(!all(have[installFrom == "Fail" & correctVersionAvail == FALSE]$toLoad))
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
            out11 <- capture.output(out <- capture.output(remove.packages(sam), type = "message"),
                                    type = "output")
          }
        }
      }
    } else {
      out2 <- out
    }
    return(out2)
  }




  pkgs <- list(c("Holidays (<=1.0.4)", "TimeWarp (<= 1.0.3)", "achubaty/amc@development", "PredictiveEcology/LandR (>=0.0.1)",
                 "PredictiveEcology/LandR (>=0.0.2)", "ianmseddy/LandR.CS (<=0.0.1)"),
               c("SpaDES.core (>=0.9)",
                 "PredictiveEcology/map@development (>= 4.0.9)",

                 "achubaty/amc@development (>=0.1.5)", "data.table (>=100.0)",
                 "digest (>=0.6.23)", "PredictiveEcology/LandR@development (>= 1.0.2)", "versions (>=0.3)",
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
                 paste0("digest (>=", packageVersion("digest"),")"),
                 "PredictiveEcology/LandR@development (>= 1.0.2)"),
               c("fastdigest (>=0.0.0.9)",
                 "PredictiveEcology/map@development (>= 0.0.0.9)",
                 "achubaty/amc@development (>=0.0.0.9)",
                 "data.table (>=0.0.0.9)",
                 paste0("digest (>=", packageVersion("digest"),")"),
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
                 paste0("digest (>=", packageVersion("digest"),")"),
                 "PredictiveEcology/LandR@development(>= 0.0.0.9)"),
               "Holidays (>=1000.3.1)",
               c("Holidays (>=1.0.1)", "fpCompare"),
               "Holidays (>=1.3.1)"
  )
  #   options("reproducible.Require.install" = TRUE)
  Sys.setenv("R_REMOTES_UPGRADE" = "never")
  options("Require.verbose" = TRUE)

  i <- 0
  pkg <- pkgs[[1]]
  for (pkg in pkgs) {
    # out <- unloadNSRecursive(n = 1)
    i <- i + 1
    print(paste0(i, ": ", paste0(Require::extractPkgName(pkg), collapse = ", ")))
    # if (i == 7) stop()
    outFromRequire <- Require::Require(pkg, repos = repo, standAlone = FALSE)
    out <- Require::Require(pkg)
    testit::assert(all.equal(outFromRequire, out))
    have <- Require::Require(pkg, install = FALSE, require = FALSE)
    pkgsToTest <- unique(Require::extractPkgName(pkg))
    names(pkgsToTest) <- pkgsToTest
    normalRequire <- unlist(lapply(pkgsToTest,
                                   function(p) tryCatch(require(p, character.only = TRUE),
                                                        error = function(x) FALSE)))
    out2 <- out
    out2 <- out2[names(out2) %in% names(normalRequire)]
    #out1 <- out1[Package %in% names(normalRequire)]
    out2 <- out2[match(names(normalRequire), names(out2))]
    have <- attr(have, "Require")
    have2 <- have[toLoad == TRUE]
    normalRequire2 <- if (NROW(have2))
      normalRequire[have2$Package]
    else
      normalRequire

    out2 <- out2[out2]
    browser(expr = !all(out2 == normalRequire2))
    testit::assert(all(out2 == normalRequire2))
    runTests(have)
    suppressWarnings(rm(outFromRequire, out, have, normalRequire))
    if (any("TimeWarp" %in% Require::extractPkgName(pkg))) {
      unloadNamespace("Holidays")
      unloadNamespace("TimeWarp")
      remove.packages(c("Holidays", "TimeWarp"))
    }
  }
}

out <- Require::Require("Holiday (<= 2.3.1)", standAlone = TRUE, libPaths = tempdir())
assert(attr(out, "Require"))
