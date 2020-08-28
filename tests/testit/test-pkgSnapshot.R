if (interactive()) {
  # googledrive::drive_download(googledrive::as_id("1Yo_7nuIn580rKqBCeycssVBOoe_qb7oY"))
  # library(Require)
  if (file.exists("packageVersions.txt")) {
    fileNames <- list()
    baseFN <- "packageVersions"
    fileNames[["fn0"]][["lp"]] <- file.path("~", baseFN)
    fileNames[["fn0"]][["txt"]] <- paste0(baseFN, ".txt")
    try(setLibPaths(origLibPaths[[1]]))
    options("Require.persistentPkgEnv" = TRUE, "Require.RPackageCache" = "~/._RPackageCache/", warn = 0,
            install.packages.check.source = "never", "install.packages.compile.from.source" = "never")
    origLibPaths <- list()
    origLibPaths[[1]] <- setLibPaths(paste0(fileNames[["fn0"]][["lp"]], "_AA"))
    Require(packageVersionFile = fileNames[["fn0"]][["txt"]])   
    
    # Test
    there <- data.table::fread(fileNames[["fn0"]][["txt"]])
    unique(there, by = "Package")
    here[!there, on = "Package"]
    there[!here, on = "Package"]
    
  } else {
    stop("Requires manual intervention in test-pkgSnapshot.R")
  }
}