

if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  RequireDeps <- c("data.table", "remotes", "tools", "utils", "versions", "methods",
                   "stats", "grDevices", "graphics")
  helpers <- dir(pattern = "helper", full.names = TRUE)
  out <- lapply(helpers, source, local = environment())

  # testInitOut <- testInit()
  tmpdir <- if (Sys.info()["user"] != "emcintir") {
    file.path(tempdir(), Require:::rndstr(1,6))
  } else {
    "c:/Eliot/TempLib5"
  }

  Require::checkPath(tmpdir, create = TRUE)
  oldLibPaths <- .libPaths()
  on.exit(.libPaths(oldLibPaths))
  .libPaths(tmpdir)

  runTests <- function(have) {
    testit::assert(all(!is.na(have[installed == TRUE]$Version)))
    testit::assert(all(have[hasVersionSpec == TRUE & correctVersion == TRUE]$toLoad))
    if (any(have$installFrom == "Fail", na.rm = TRUE))
      testit::assert(!all(have[installFrom == "Fail" & correctVersionAvail == FALSE]$toLoad))
  }
  unloadNSRecursive <- function(packages, n = 0) {
    out <- data.table::as.data.table(installed.packages(noCache = TRUE))[[1]]
    names(out) <- out
    out <- lapply(out, isNamespaceLoaded)
    out <- unlist(out)
    out <- out[out]
    keepLoaded1 <- c("Require", "base64enc", "RCurl", "fastmatch", "raster", "Rcpp", "rstudioapi", c("crayon", "data.table", "remotes", "tools", "utils", "versions",
                                                                                                     "grDevices", "methods", "stats", "graphics"))
    keepLoaded = unique(c(keepLoaded1, dir(tail(.libPaths(),1))))
    out <- names(out)
    names(out) <- out
    out <- unique(setdiff(out, keepLoaded))
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
      if (sum(!out2) > 0) {
        out3 <- out2[out2]
        sam <- sample(names(out3), size = n)
        if (n > 0) {
          message(crayon::green("removing ", paste(sam, collapse = ", ")))

          browser()
          files <- dir(.libPaths()[1], recursive = TRUE, full.names = TRUE)
          grep(sam, files, value = TRUE)
          td <- file.path(tempdir(), "tmp11")
          Require::checkPath(td, create = TRUE)
          file.copy(files, td)
          unlink1 <- unlink(files)
          if (isTRUE(file.exists(files))) {
            dirName <- file.path(.libPaths()[1], sam)
            file.copy(files, dirName)
          }
          browser(expr = sam %in% dir(.libPaths()[1]))
          out <- capture.output(remove.packages(sam), type = "message")
          if (sam %in% dir(.libPaths()[1])) {
            file.copy(dir(td, recursive = TRUE, full.names = TRUE), file.path(.libPaths()[1], sam))
          }
          files1 <- dir(.libPaths()[1], pattern = sam, recursive = TRUE, full.names = TRUE)
          file.copy()

          dirToRm <- grep(sam, dir(.libPaths()[1], full.names = TRUE), value = TRUE)
          unlink(td, recursive = TRUE)
        }
      }
    } else {
      out2 <- out
    }
    return(out2)
  }




  pkgs <- list(c("bitops (<=1.0-5)", "Holidays (>=0.0.1)", "achubaty/amc@development", "PredictiveEcology/LandR (>=0.0.1)",
                 "PredictiveEcology/LandR (>=0.0.2)", "Holidays (>=0.0.2)", "ianmseddy/LandR.CS (<=0.0.1)"),
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

  i <- 0
  pkg <- pkgs[[1]]
  for (pkg in pkgs) {
    out <- unloadNSRecursive(n = 1)
    i <- i + 1
    print(paste0(i, ": ", paste0(Require::extractPkgName(pkg), collapse = ", ")))
    # if (i == 7) stop()
    outFromRequire <- Require::Require2(pkg, repos = repo, standAlone = FALSE)
    out <- Require::Require2(pkg)
    testit::assert(all.equal(outFromRequire, out))
    have <- Require::Require2(pkg, install = FALSE, require = FALSE)
    pkgsToTest <- unique(Require::extractPkgName(pkg))
    names(pkgsToTest) <- pkgsToTest
    normalRequire <- unlist(lapply(pkgsToTest,
                                   function(p) tryCatch(require(p, character.only = TRUE),
                                                        error = function(x) FALSE)))
    out <- out[names(out) %in% names(normalRequire)]
    #out1 <- out1[Package %in% names(normalRequire)]
    out <- out[match(names(normalRequire), names(out))]
    browser(expr = !all(out == normalRequire))
    testit::assert(all(out == normalRequire))
    have <- attr(have, "Require")
    runTests(have)
    suppressWarnings(rm(outFromRequire, out, have, normalRequire))
  }
}
