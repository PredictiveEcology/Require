forceRun <- FALSE
if (interactive() && forceRun) {
  library(Require)
  origLibPathsAllTests <- .libPaths()
  # devtools::load_all("~/GitHub/Require")
  # library(profvis)
  aa <- pkgSnapshot()
  # googledrive::drive_download(googledrive::as_id("1Yo_7nuIn580rKqBCeycssVBOoe_qb7oY"))
  if (file.exists("packageVersions.txt")) {
    fileNames <- list()
    Sys.setenv('CRANCACHE_DISABLE' = TRUE)
    baseFN <- "packageVersions"
    tmpLibPath <- tempdir2(paste(sample(LETTERS, size = 6), collapse = ""))
    fileNames[["fn0"]][["lp"]] <- file.path(tmpLibPath)
    fileNames[["fn0"]][["txt"]] <- paste0(baseFN, ".txt")
    try(setLibPaths(origLibPaths[[1]], updateRprofile = FALSE), silent = TRUE)
    options("Require.persistentPkgEnv" = TRUE,
            "Require.Home" = "~/GitHub/Require",
            "Require.RPackageCache" = "~/._RPackageCache/",
            "install.packages.check.source" = "never",
            "install.packages.compile.from.source" = "never",
            "Require.unloadNamespaces" = TRUE)
    origLibPaths <- setLibPaths(paste0(fileNames[["fn0"]][["lp"]]), updateRprofile = FALSE)
    
    theDir <- Require:::rpackageFolder(getOption("Require.RPackageCache"))
    localBins <- dir(theDir, pattern = "data.table|remotes")
    localBinsFull <- dir(theDir, full.names = TRUE, pattern = "data.table|remotes")
    vers <- gsub("^.+\\_(.+)\\.[[:alnum:]]+", "\\1", basename(localBins))
    localBinsOrd <- order(package_version(vers), decreasing = TRUE)
    localBins <- localBins[localBinsOrd]
    localBinsFull <- localBinsFull[localBinsOrd]
    dups <- duplicated(gsub("(.+)\\_.+", "\\1", localBins))
    localBins <- localBins[!dups]
    localBinsFull <- localBinsFull[!dups]
    if (length(localBinsFull) == 2) {
      system(paste0("Rscript -e \"install.packages(c('",localBinsFull[1],"', '",localBinsFull[2],"'), type = 'binary', lib ='",.libPaths()[1],"', repos = NULL)\""), wait = TRUE)
    } else {
      system(paste0("Rscript -e \"install.packages(c('data.table', 'remotes'), lib ='",.libPaths()[1],"', repos = '",getOption('repos')[["CRAN"]],"')\""), wait = TRUE)
    }
    
    # oldDir <- getwd()
    Require:::installRequire("~/GitHub/Require")
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
    if (Require:::isWindows())
      anyMissing <- anyMissing[!Package %in% "littler"]
    # here[!there, on = "Package"]
    if (NROW(anyMissing) != 0) browser()
    testit::assert(NROW(anyMissing) == 0)
  
  }
  if (!identical(origLibPathsAllTests, .libPaths()))
    Require::setLibPaths(origLibPathsAllTests, standAlone = TRUE, exact = TRUE)
} else {
  message("Please run test-pkgSnapshot manually")
}