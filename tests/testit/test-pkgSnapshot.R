if (interactive()) {
  fileNames <- list()
  baseFN <- "packageTati"
  fileNames[["fn0"]][["lp"]] <- file.path("~", baseFN)
  fileNames[["fn0"]][["txt"]] <- paste0(baseFN, ".txt")
  there <- data.table::fread(fileNames[["fn0"]][["txt"]])
  out <- split(there, there$LibPath)
  theSeq <- seq(length(out))
  try(setLibPaths(origLibPaths[[1]]))
  for (i in theSeq) {
    fnHere <- paste0("fn", i)
    fileNames[[fnHere]][["lp"]] <- file.path("~", paste0(baseFN, "C", i))
    fileNames[[fnHere]][["txt"]] <- paste0(baseFN, i, ".txt")
  }
  #fileNames[["fn2"]][["lp"]] <- file.path("~", paste0(baseFN, "2"))
  #fileNames[["fn2"]][["txt"]] <- paste0(baseFN, "2.txt")
  
  for (i in theSeq)
    data.table::fwrite(out[[i]], fileNames[[paste0("fn", i)]][["txt"]])
  
  options("Require.persistentPkgEnv" = TRUE, "Require.RPackageCache" = "~/._RPackageCache/", warn = 0,
          install.packages.check.source = "never", "install.packages.compile.from.source" = "never")
  # setwd("~/GitHub/Require")
  #library(devtools)
  #devtools::load_all("~/GitHub/Require")#, INSTALL_opts = c("--no-multiarch"))
  #devtools::install("~/GitHub/Require", INSTALL_opts = c("--no-multiarch"))
  #library(Require)
  #library(googledrive)
  #drive_download(as_id("1uauMLbqexwtAwLvxFaOOioI-YQUYXJv"), path = fileNames[["fn0"]][["txt"]], overwrite = TRUE)
  #._installCRAN_0 <- 1
  #debug(updateInstalled)
  origLibPaths <- list()
  #for (fnInd in rev(theSeq) ) {
  #  origLibPaths[[fnInd]] <- setLibPaths(fileNames[[paste0("fn", fnInd)]][["lp"]], standAlone = if (fnInd == 1) FALSE else TRUE) # start with an empty folder for new library
  #  Require(packageVersionFile = fileNames[[paste0("fn", fnInd)]][["txt"]])   
  #}
  origLibPaths[[1]] <- setLibPaths(paste0(fileNames[["fn0"]]$lp, "_AA"))
  Require(packageVersionFile = fileNames[["fn0"]][["txt"]])   
  
  #Require(c("PredictiveEcology/fireSenseUtils@b9f22782bea1a1836775f8529bf6b5ad14834aeb", 
  #          "tati-micheletti/usefulFuns@3bea6af03ba572af7a86ae30a4c0e8a11eb13345"), require = FALSE)
}