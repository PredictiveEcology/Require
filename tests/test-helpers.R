setupTest <- function(verbose = getOption("Require.verbose")) {
  opts <- options()

  Sys.setenv("R_REMOTES_UPGRADE" = "never")
  Sys.setenv("CRANCACHE_DISABLE" = TRUE)
  outOpts <- options(
    "Require.persistentPkgEnv" = TRUE,
    "install.packages.check.source" = "never",
    "install.packages.compile.from.source" = "never",
    "Require.unloadNamespaces" = TRUE)

  if (Sys.info()["user"] == "achubaty") {
    outOpts2 <- options("Require.Home" = "~/GitHub/PredictiveEcology/Require")
  } else {
    outOpts2 <- options("Require.Home" = "~/GitHub/Require")
  }

  libPath <- .libPaths()
  origWd <- getwd()
  thisFilename <- getInStack("r")
  env <- whereInStack("ee")
  startTime <- Sys.time()
  Require:::messageVerbose("\033[32m --------------------------------- Starting ",
                           thisFilename,"  at: ",format(startTime),"---------------------------\033[39m",
                           verbose = verbose, verboseLevel = 0)
  Require:::messageVerbose("\033[34m getOption('Require.verbose'): ", getOption("Require.verbose"), "\033[39m", verboseLevel = 0)
  Require:::messageVerbose("\033[34m getOption('repos'): ", paste(getOption("repos"), collapse = ", "), "\033[39m", verboseLevel = 0)
  return(list(startTime = startTime, thisFilename = thisFilename, libPath = libPath, origWd = origWd, opts = opts))
}

endTest <- function(setupInitial, verbose = getOption("Require.verbose")) {

  currOptions <- options()
  changedOptions <- setdiff(currOptions, setupInitial$opts)
  toRevert <- setupInitial$opts[names(changedOptions)]
  names(toRevert) <- names(changedOptions)
  if (any(grepl("datatable.alloccol", names(toRevert)))) browser()
  options(toRevert)


  thisFilename <- setupInitial$thisFilename
  endTime <- Sys.time()
  ee <- getInStack("ee")
  ee[[thisFilename]] <- format(endTime - setupInitial$startTime)
  Require:::messageVerbose("\033[32m ----------------------------------",
                           thisFilename, ": ", ee[[thisFilename]], " \033[39m",
                           verboseLevel = -1, verbose = verbose)
  .libPaths(setupInitial$libPath)
  setwd(setupInitial$origWd)
}

whereInStack <- function(obj) {
  for (i in 1:sys.nframe()) {
    fn <- get0(obj, sys.frame(-i), inherits = FALSE)
    if (!is.null(fn)) break
  }
  return(sys.frame(-i))
}

getInStack <- function(obj) {
  env <- whereInStack(obj)
  return(get(obj, envir = env, inherits = FALSE))
}

