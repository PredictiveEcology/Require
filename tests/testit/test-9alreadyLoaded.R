thisFilename <- "test-9alreadyLoaded.R"
startTime <- Sys.time()
message("\033[32m --------------------------------- Starting ",thisFilename,"  at: ",format(startTime),"---------------------------\033[39m")
Require:::messageVerbose("\033[34m getOption('Require.verbose'): ", getOption("Require.verbose"), "\033[39m", verboseLevel = 0)
origLibPathsAllTests <- .libPaths()

# This error doesn't occur on Linux
if (interactive() && Require:::isWindows()) {
  library(testit)
  library(Require)
  Sys.setenv("R_REMOTES_UPGRADE" = "never")
  Sys.setenv('CRANCACHE_DISABLE' = TRUE)
  outOpts <- options("Require.persistentPkgEnv" = TRUE,
                     "install.packages.check.source" = "never",
                     "install.packages.compile.from.source" = "never",
                     "Require.unloadNamespaces" = FALSE)
  projectDir <- tempdir2(Require:::.rndstr(1))
  pkgDir <- file.path(projectDir, "R")
  setLibPaths(pkgDir, standAlone = TRUE)
  dir.create(pkgDir, showWarnings = FALSE, recursive = TRUE)
  origDir <- setwd(projectDir)



  # Trying to install a package whose dependency is a loadedNamespace, but that is not available
  ####
  out6 <- capture.output(type = "message",
                         out <- suppressWarnings(Require("whisker",
                                                         require = TRUE,
                                                         standAlone = TRUE))) # warnings would be if whisker is used by e.g., devtools
  out3 <- capture.output(type = "message",
                         out5 <- capture.output(out4 <- suppressWarnings(
                           remove.packages("whisker")), silent = TRUE))
  warns <- list()
  mess <- capture.output(type = "message",
                         out2 <- withCallingHandlers(
                           Require("PredictiveEcology/SpaDES.project@development", require = FALSE, verbose = 2)
                           , warning = function(w) {
                             warns <<- appendToWarns(w$message, warns)
                             invokeRestart("muffleWarning")
                           })
  )
  testit::assert(any(grepl("is in use", warns)))
  testit::assert(any(grepl("is in use", mess)))



  ####
  setLibPaths(origLibPathsAllTests)
  setwd(origDir)
}

try(startTimeAll <- readRDS(file = file.path(tdOuter, "startTimeAll")), silent = TRUE) # doesn't seem to keep globals from other scripts; recreate here
unlink(tempdir2(), recursive = TRUE)
endTime <- Sys.time()
message("\033[32m ----------------------------------",thisFilename, ": ", format(endTime - startTime)," \033[39m")
try(message("\033[32m ----------------------------------All Tests: ",format(endTime - startTimeAll)," \033[39m"), silent = TRUE)
