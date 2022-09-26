thisFilename <- "test-0pkgSnapshot.R"
startTimeAll <- startTime <- Sys.time()
tdOuter <- tempdir2("tests")
try(saveRDS(startTimeAll, file = file.path(tdOuter, "startTimeAll")), silent = TRUE)
message("\033[32m --------------------------------- Starting ", thisFilename, "  at: ",
        format(startTime),"---------------------------\033[39m")
messageVerbose("\033[34m getOption('Require.verbose'): ", getOption("Require.verbose"), "\033[39m", verboseLevel = -1)

library(Require)
srch <- search()
anyNamespaces <- srch[!gsub("package:", "", srch) %in%
                        c("Require", Require:::.basePkgs, ".GlobalEnv", "tools:rstudio", "Autoloads", "testit")]
if (length(anyNamespaces) > 0) stop("Please restart R before running this test")
library(testit)
origLibPathsAllTests <- .libPaths()
tmpdir <- checkPath(tempdir2("testingA"), create = TRUE)
pkgVF <- file.path(tmpdir, "packageVersions.txt")
aa <- pkgSnapshot(packageVersionFile = pkgVF)
bb <- list()
for (lp in unique(aa$LibPath)) {
  pack <- aa$Package[aa$LibPath == lp]
  if (!all(pack %in% Require:::.basePkgs)) {
    deps <- pkgDep(pack, recursive = TRUE)
    haveNoDeps <- unlist(lapply(deps, length)) == 2
    sam <- sample(sum(haveNoDeps), pmin(sum(haveNoDeps), 3))
    pkgs <- c(names(deps[haveNoDeps][sam]), Require::extractPkgName(unname(unlist(deps[haveNoDeps][sam]))))
    bb[[lp]] <- aa[Package %in% pkgs & LibPath == lp]
  }
}
bb <- data.table::rbindlist(bb)
data.table::fwrite(x = bb, file = pkgVF)

outOpts <- options(
  install.packages.check.source = "never",
  install.packages.compile.from.source = "never",
  Require.persistentPkgEnv = TRUE,
  Require.unloadNamespaces = TRUE)
if (Sys.info()["user"] == "achubaty") {
  outOpts2 <- options("Require.Home" = "~/GitHub/PredictiveEcology/Require")
} else {
  outOpts2 <- options("Require.Home" = "~/GitHub/Require")
}

if (file.exists(pkgVF)) {
  fileNames <- list()
  baseFN <- "packageVersions"
  tmpLibPath <- tempdir2(paste(sample(LETTERS, size = 6), collapse = ""))
  fileNames[["fn0"]][["lp"]] <- file.path(tmpLibPath)
  fileNames[["fn0"]][["txt"]] <- file.path(tmpdir, paste0(baseFN, ".txt"))

  try(setLibPaths(origLibPaths[[1]], updateRprofile = FALSE), silent = TRUE)
  Sys.setenv("R_REMOTES_UPGRADE" = "never")
  Sys.setenv('CRANCACHE_DISABLE' = TRUE)

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
    localBinsFull <- na.omit(c(dts))#, rems))
  } else {
    localBinsFull <- NULL
  }

  ## There might be more than one version
  dts <- grep("data.table", localBinsFull, value = TRUE)[1]
  localBinsFull <- dts
  Rpath <- Sys.which("Rscript")
  quiet <- !(getOption("Require.verbose") >= 1)
  if (length(localBinsFull) == 1) {
    if (Require:::isWindows())
      system(paste0(Rpath, " -e \"install.packages(c('", localBinsFull[1],
                    "'), quiet = ", quiet,", type = 'binary', lib = '", .libPaths()[1], "', repos = NULL)\""), wait = TRUE)
    else
      system(paste0(Rpath, " -e \"install.packages(c('", localBinsFull[1],
                    "'), quiet = ", quiet,", lib = '", .libPaths()[1], "', repos = NULL)\""), wait = TRUE)
  } else {
    system(paste0(Rpath, " -e \"install.packages(c('data.table'), lib ='",
                  .libPaths()[1], "', quiet = ", quiet,", repos = '", getOption('repos')[["CRAN"]], "')\""), wait = TRUE)
  }

  if (is.null(getOption("Require.Home"))) stop("Must define options('Require.Home' = 'pathToRequirePkgSrc')")
  Require:::installRequire(getOption("Require.Home"))

  st <- try(system.time({out <- Require(packageVersionFile = fileNames[["fn0"]][["txt"]])}))

  # Test
  there <- data.table::fread(fileNames[["fn0"]][["txt"]])
  unique(there, by = "Package")
  here <- pkgSnapshot(file.path(tempdir2("test"), "packageVersionsEliot.txt"), libPaths = .libPaths())
  anyMissing <- there[!here, on = c("Package", "Version")]
  anyMissing <- anyMissing[!Package %in% c("Require", getFromNamespace(".basePkgs", "Require"))]
  anyMissing <- anyMissing[!is.na(GithubRepo)] # fails due to "local install"
  anyMissing <- anyMissing[GithubUsername != "PredictiveEcology"] # even though they have GitHub info,
  # they are likely missing because of the previous line of local installs
  if (Require:::isWindows())
    anyMissing <- anyMissing[!Package %in% "littler"]
  # here[!there, on = "Package"]
  if (NROW(anyMissing) != 0) stop("Error 832; please contact developer")
  testit::assert(NROW(anyMissing) == 0)
}
if (!identical(origLibPathsAllTests, .libPaths()))
  Require::setLibPaths(origLibPathsAllTests, standAlone = TRUE, exact = TRUE)

pkgGrep <- paste0(unlist(lapply(strsplit(names(out), " "), `[`, 1)), ".*[.]tar[.]gz", collapse = "|")
pkgs2rm <- dir(path = getOption("Require.Home"), pattern = pkgGrep, full.names = TRUE)
unlink(pkgs2rm)

options(outOpts)
if (exists("outOpts2")) options(outOpts2)

endTime <- Sys.time()
message("\033[32m ----------------------------------", thisFilename, ": ", format(endTime - startTime), " \033[39m")
