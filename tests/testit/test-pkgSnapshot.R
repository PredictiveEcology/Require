if (!exists("forceRun")) forceRun <- FALSE
if (interactive() && forceRun) {
  library(Require)
  srch <- search()
  anyNamespaces <- srch[!gsub("package:", "", srch) %in% 
                          c("Require", Require:::.basePkgs, ".GlobalEnv", "tools:rstudio", "Autoloads")]
  if (length(anyNamespaces) > 0) stop("Please restart R before running this test")
  origLibPathsAllTests <- .libPaths()
  aa <- pkgSnapshot()
  if (file.exists("packageVersions.txt")) {
    fileNames <- list()
    baseFN <- "packageVersions"
    tmpLibPath <- tempdir2(paste(sample(LETTERS, size = 6), collapse = ""))
    fileNames[["fn0"]][["lp"]] <- file.path(tmpLibPath)
    fileNames[["fn0"]][["txt"]] <- paste0(baseFN, ".txt")
    try(setLibPaths(origLibPaths[[1]], updateRprofile = FALSE), silent = TRUE)
    Sys.setenv("R_REMOTES_UPGRADE" = "never")
    Sys.setenv('CRANCACHE_DISABLE' = TRUE)
    outOpts <- options("Require.persistentPkgEnv" = TRUE,
                       "install.packages.check.source" = "never",
                       "install.packages.compile.from.source" = "never",
                       "Require.unloadNamespaces" = TRUE)
    if (Sys.info()["user"] == "emcintir") {
      outOpts2 <- options("Require.Home" = "~/GitHub/Require",
                          "Require.RPackageCache" = "~/._RPackageCache/")
    } else {
      outOpts2 <- options("Require.Home" = "~/GitHub/Require")
    }
    origLibPaths <- setLibPaths(paste0(fileNames[["fn0"]][["lp"]]), updateRprofile = FALSE)
    
    theDir <- Require:::rpackageFolder(getOption("Require.RPackageCache"))
    localBins <- dir(theDir, pattern = "data.table|remotes")
    localBinsFull <- dir(theDir, full.names = TRUE, pattern = "data.table|remotes")
    vers <- gsub("^[^_]+\\_(.+)", "\\1", basename(localBins))
    vers <- gsub("^([^_]+)_+.+$", "\\1", vers)
    vers <- gsub("^([[:digit:]\\.-]+)\\.[[:alpha:]]{1,1}.+$", "\\1", vers)
    
    # vers <- gsub("^.+\\_(.+)\\.[[:alnum:]]+", "\\1", basename(localBins))
    
    localBinsOrd <- order(package_version(vers), decreasing = TRUE)
    localBins <- localBins[localBinsOrd]
    localBinsFull <- localBinsFull[localBinsOrd]
    dups <- duplicated(gsub("(.+)\\_.+", "\\1", localBins))
    localBins <- localBins[!dups]
    localBinsFull <- localBinsFull[!dups]
    if (any(grepl("tar.gz", localBinsFull))) {
      localBinsFull <- grep("linux-gnu", localBinsFull, value = TRUE)
    }
    if (length(localBinsFull) == 2) {
      if (Require:::isWindows())
        system(paste0("Rscript -e \"install.packages(c('",localBinsFull[1],"', '",localBinsFull[2],"'), type = 'binary', lib ='",.libPaths()[1],"', repos = NULL)\""), wait = TRUE)
      else 
        system(paste0("Rscript -e \"install.packages(c('",localBinsFull[1],"', '",localBinsFull[2],"'), lib ='",.libPaths()[1],"', repos = NULL)\""), wait = TRUE)
    } else {
      system(paste0("Rscript -e \"install.packages(c('data.table', 'remotes'), lib ='",.libPaths()[1],"', repos = '",getOption('repos')[["CRAN"]],"')\""), wait = TRUE)
    }
    
    # oldDir <- getwd()
    if (is.null(getOption("Require.Home"))) stop("Must define options('Require.Home' = 'pathToRequirePkgSrc')")
    Require:::installRequire(getOption("Require.Home"))
    # on.exit(setwd(oldDir))
    # system(paste0("R CMD INSTALL --library=", .libPaths()[1], " Require"), wait = TRUE)
    #setwd(oldDir)
    
    st <- system.time(out <- Require(packageVersionFile = fileNames[["fn0"]][["txt"]])   )
    print(st)
    
    # Test
    there <- data.table::fread(fileNames[["fn0"]][["txt"]])
    unique(there, by = "Package")
    here <- pkgSnapshot("packageVersionsEliot.txt", libPaths = .libPaths())
    anyMissing <- there[!here, on = c("Package", "Version")]
    anyMissing <- anyMissing[!Package %in% c("Require", getFromNamespace(".basePkgs", "Require"))]
    anyMissing <- anyMissing[!is.na(GithubRepo)] # fails due to "local install"
    anyMissing <- anyMissing[GithubUsername != "PredictiveEcology"] # even though they have GitHub info,
    # they are likely missing because of the previous line of local installs 
    if (Require:::isWindows())
      anyMissing <- anyMissing[!Package %in% "littler"]
    # here[!there, on = "Package"]
    if (NROW(anyMissing) != 0) browser()
    testit::assert(NROW(anyMissing) == 0)
    
  }
  if (!identical(origLibPathsAllTests, .libPaths()))
    Require::setLibPaths(origLibPathsAllTests, standAlone = TRUE, exact = TRUE)
  options(outOpts)
  options(outOpts2)
} else {
  message("Please restart R, then run test-pkgSnapshot manually:\n",
          "forceRun <- TRUE; source('tests/testit/test-pkgSnapshot.R', echo=TRUE)")
}
