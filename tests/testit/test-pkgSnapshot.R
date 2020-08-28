if (interactive()) {
  try(setLibPaths(origLibPaths, standAlone = TRUE), silent = TRUE)
  devtools::load_all("~/GitHub/Require")
  aa <- pkgSnapshot()
  # googledrive::drive_download(googledrive::as_id("1Yo_7nuIn580rKqBCeycssVBOoe_qb7oY"))
  # library(Require)
  if (file.exists("packageVersions.txt")) {
    fileNames <- list()
    baseFN <- "packageVersions"
    tmpLibPath <- "~/tmpLibPath1"
    fileNames[["fn0"]][["lp"]] <- file.path(tmpLibPath)
    fileNames[["fn0"]][["txt"]] <- paste0(baseFN, ".txt")
    try(setLibPaths(origLibPaths[[1]]))
    options("Require.persistentPkgEnv" = TRUE, 
            "Require.RPackageCache" = "~/._RPackageCache/", 
            "install.packages.check.source" = "never", 
            "install.packages.compile.from.source" = "never",
            "Require.unloadNamespaces" = FALSE)
    origLibPaths <- setLibPaths(paste0(fileNames[["fn0"]][["lp"]]))
    Require(packageVersionFile = fileNames[["fn0"]][["txt"]])   
    
    # Test
    there <- data.table::fread(fileNames[["fn0"]][["txt"]])
    unique(there, by = "Package")
    here <- pkgSnapshot("packageVersionsEliot.txt", libPaths = .libPaths())
    anyMissing <- there[!here, on = c("Package", "Version")]
    anyMissing <- anyMissing[!Package %in% c("Require", .basePkgs)]
    if (tolower(Sys.info()["sysname"]) == "windows")
      anyMissing <- anyMissing[!Package %in% "littler"]
    # here[!there, on = "Package"]
    testit::assert(NROW(anyMissing) == 0)
    
    
  } else {
    stop("Requires manual intervention in test-pkgSnapshot.R")
  }
}