thisFilename <- "test-8Modules.R"
startTime <- Sys.time()
message("\033[32m --------------------------------- Starting ",thisFilename,"  at: ",format(startTime),"---------------------------\033[39m")
Require:::messageVerbose("\033[34m getOption('Require.verbose'): ", getOption("Require.verbose"), "\033[39m", verboseLevel = 0)
origLibPathsAllTests <- .libPaths()

if (interactive()) {
  library(testit)
  library(Require)
  Sys.setenv("R_REMOTES_UPGRADE" = "never")
  Sys.setenv('CRANCACHE_DISABLE' = TRUE)
  outOpts <- options("Require.persistentPkgEnv" = TRUE,
                     "install.packages.check.source" = "never",
                     "install.packages.compile.from.source" = "never",
                     "Require.unloadNamespaces" = FALSE)
  projectDir <- "~/Testing11"
  pkgDir <- file.path(projectDir, "EliotTest4.2")
  setLibPaths(pkgDir, standAlone = TRUE)
  dir.create(pkgDir, showWarnings = FALSE, recursive = TRUE)
  origDir <- setwd(projectDir)
  modulePath <- "m"

  # Install 3 packages that are needed for subsequent module and package installations
  Require("PredictiveEcology/SpaDES.project@transition",
                   upgrade = FALSE, require = FALSE)
  # setLinuxBinaryRepo() # OK to run on any system

  # Install modules
  SpaDES.project::getModule(modulePath = modulePath,
                            c("PredictiveEcology/Biomass_speciesData@master",
                              "PredictiveEcology/Biomass_borealDataPrep@master",
                              "PredictiveEcology/Biomass_core@master",
                              "CeresBarros/Biomass_validationKNN@master",
                              "PredictiveEcology/Biomass_speciesParameters@LandRPub"))

  outs <- SpaDES.project::packagesInModules(modulePath = modulePath)
  Require::Require(c(unname(unlist(outs)),
                     "PredictiveEcology/SpaDES.experiment@development",
                     "devtools", "ggspatial", "ggpubr", "cowplot"),
                   require = FALSE, standAlone = TRUE)
  setLibPaths(origLibPathsAllTests)
  setwd(origDir)
}

try(startTimeAll <- readRDS(file = file.path(tdOuter, "startTimeAll")), silent = TRUE) # doesn't seem to keep globals from other scripts; recreate here
unlink(tempdir2(), recursive = TRUE)
endTime <- Sys.time()
message("\033[32m ----------------------------------",thisFilename, ": ", format(endTime - startTime)," \033[39m")
try(message("\033[32m ----------------------------------All Tests: ",format(endTime - startTimeAll)," \033[39m"), silent = TRUE)
