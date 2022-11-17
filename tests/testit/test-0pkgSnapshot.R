setupInitial <- setupTest()

library(Require)
srch <- search()
anyNamespaces <- srch[!gsub("package:", "", srch) %in%
  c("Require", "data.table", Require:::.basePkgs, ".GlobalEnv", "tools:rstudio", "Autoloads", "testit")]
if (length(anyNamespaces) > 0) stop("Please restart R before running this test")
library(testit)

quiet <- !(getOption("Require.verbose") >= 1)

tmpdir <- tempdir2(Require:::.rndstr(1))
tmpdir2 <- tempdir2(Require:::.rndstr(1))
created <- dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
pkgVF <- file.path(tmpdir, "packageVersions.txt")
setLibPaths(tmpdir, standAlone = TRUE)
tmpdirActual <- .libPaths()[1] # setLibPaths postpends the R version
Require(c("remotes (==2.4.1)", "testit (==0.12)"), require = FALSE, quiet = quiet)

setLibPaths(tmpdir2, standAlone = TRUE)
tmpdir2Actual <- .libPaths()[1] # setLibPaths postpends the R version
if (isDev) {
  Require(c("covr (==3.6.0)"), require = FALSE, quiet = quiet)
} else {
  Require(c("crayon"), require = FALSE, quiet = quiet)
}

.libPaths(c(tmpdirActual, tmpdir2Actual))
# .libPaths(c(tmpdir, tmpdir2))
aa <- pkgSnapshot(packageVersionFile = pkgVF, libPaths = .libPaths()[1:2])

bb <- list()
for (lp in unique(aa$LibPath)) {
  pack <- aa$Package[aa$LibPath == lp]
  pack <- sample(pack, size = min(10, length(pack)))
  if (!all(pack %in% Require:::.basePkgs)) {
    deps <- pkgDep(pack, recursive = TRUE)
    lens <- lengths(deps)
    haveFewDeps <- order(lens)
    deps <- deps[haveFewDeps]
    wh <- min(4, max(which(cumsum(lengths(deps) + 1) < 10)))
    deps <- deps[seq(wh)]
    pkgs <- c(names(deps), Require::extractPkgName(unname(unlist(deps))))
    bb[[lp]] <- aa[Package %in% pkgs & LibPath == lp]
  }
}
bb <- data.table::rbindlist(bb)
data.table::fwrite(x = bb, file = pkgVF)

if (file.exists(pkgVF)) {
  fileNames <- list()
  baseFN <- "packageVersions"
  tmpLibPath <- tempdir2(paste(sample(LETTERS, size = 6), collapse = ""))
  fileNames[["fn0"]][["lp"]] <- file.path(tmpLibPath)
  fileNames[["fn0"]][["txt"]] <- pkgVF

  try(setLibPaths(origLibPaths[[1]], updateRprofile = FALSE), silent = TRUE)

  origLibPaths <- setLibPaths(paste0(fileNames[["fn0"]][["lp"]]), updateRprofile = FALSE)

  theDir <- Require:::rpackageFolder(getOptionRPackageCache())
  if (!is.null(theDir)) {
    localBins <- dir(theDir, pattern = "data.table")
    localBinsFull <- dir(theDir, full.names = TRUE, pattern = "data.table")
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
    # There might be more than one version
    dts <- grep("data.table", localBinsFull, value = TRUE)[1]
    # rems <- grep("remotes", localBinsFull, value = TRUE)[1]
    localBinsFull <- na.omit(c(dts)) # , rems))
    # dts <- grep("data.table", localBinsFull, value = TRUE)[1]
    # localBinsFull <- dts
  } else {
    localBinsFull <- NULL
  }

  ## There might be more than one version
  Rpath <- Sys.which("Rscript")
  if (length(localBinsFull) == 1) { # already have the binary in the Cache
    if (Require:::isWindows()) {
      system(paste0(
        Rpath, " -e \"install.packages(c('", localBinsFull[1],
        "'), quiet = ", quiet, ", type = 'binary', lib = '", .libPaths()[1], "', repos = NULL)\""
      ), wait = TRUE)
    } else {
      system(paste0(
        Rpath, " -e \"install.packages(c('", localBinsFull[1],
        "'), quiet = ", quiet, ", lib = '", .libPaths()[1], "', repos = NULL)\""
      ), wait = TRUE)
    }
  } else {
    # For some reason, when using Rscript, the RStudio Package Manager repository the Rscript install doesn't use binary
    pkg <- "data.table"
    withCallingHandlers(
      install.packages(pkg,
        lib = .libPaths()[1],
        quiet = quiet, repos = getOption("repos")[["CRAN"]]
      ),
      warning = function(w) {
        system(paste0(
          Rpath, " -e \"install.packages(c('", pkg, "'), lib ='",
          .libPaths()[1], "', quiet = ", quiet, ", repos = '", getOption("repos")[["CRAN"]], "')\""
        ), wait = TRUE)
        invokeRestart("muffleWarning")
      }
    )
  }

  if (is.null(getOption("Require.Home"))) stop("Must define options('Require.Home' = 'pathToRequirePkgSrc')")
  Require:::installRequire(getOption("Require.Home"))

  out <- Require(packageVersionFile = fileNames[["fn0"]][["txt"]], standAlone = TRUE)

  # Test
  there <- data.table::fread(fileNames[["fn0"]][["txt"]])
  unique(there, by = "Package")
  here <- pkgSnapshot(file.path(tempdir2("test"), "packageVersionsEliot.txt"), libPaths = .libPaths())
  anyMissing <- there[!here, on = c("Package", "Version")]
  anyMissing <- anyMissing[!Package %in% c("Require", getFromNamespace(".basePkgs", "Require"))]
  anyMissing <- anyMissing[!is.na(GithubRepo)] # fails due to "local install"
  anyMissing <- anyMissing[GithubUsername != "PredictiveEcology"] # even though they have GitHub info,
  # they are likely missing because of the previous line of local installs
  if (Require:::isWindows()) {
    anyMissing <- anyMissing[!Package %in% "littler"]
  }
  # here[!there, on = "Package"]
  if (NROW(anyMissing) != 0) stop("Error 832; please contact developer")
  testit::assert(NROW(anyMissing) == 0)
}
endTest(setupInitial)
