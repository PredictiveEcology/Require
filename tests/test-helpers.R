setupTest <- function(verbose = getOption("Require.verbose")) {
  opts <- options()

  # cannot open file 'startup.Rs': No such file or directory
  # suggested solution https://stackoverflow.com/a/27994299/3890027
  Sys.setenv("R_TESTS" = "")

  Sys.setenv("R_REMOTES_UPGRADE" = "never")
  Sys.setenv("CRANCACHE_DISABLE" = TRUE)
  outOpts <- options(
    # "Require.persistentPkgEnv" = TRUE,
    install.packages.check.source = "never",
    install.packages.compile.from.source = "never",
    Ncpus = 2L,
    Require.unloadNamespaces = TRUE
  )

  if (Sys.info()["user"] == "achubaty") {
    outOpts2 <- options("Require.Home" = "~/GitHub/PredictiveEcology/Require")
  } else {
    outOpts2 <- options("Require.Home" = "~/GitHub/Require")
  }

  libPath <- .libPaths()
  origWd <- getwd()
  thisFilename <- Require:::getInStack("r")
  env <- Require:::whereInStack("ee")
  startTime <- Sys.time()
  Require:::messageVerbose("\033[32m --------------------------------- Starting ",
    thisFilename, "  at: ", format(startTime, digits = 2),
    "---------------------------\033[39m",
    verbose = verbose, verboseLevel = 0
  )
  Require:::messageVerbose("\033[34m getOption('Require.verbose'): ",
    getOption("Require.verbose"), "\033[39m",
    verboseLevel = 0
  )
  Require:::messageVerbose("\033[34m getOption('repos'): ",
    paste(getOption("repos"), collapse = ", "), "\033[39m",
    verboseLevel = 0
  )
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
  ee <- Require:::getInStack("ee")
  ee[[thisFilename]] <- format(endTime - setupInitial$startTime, digits = 2)
  Require:::messageVerbose("\033[32m ----------------------------------",
    thisFilename, ": ", ee[[thisFilename]], " \033[39m",
    verboseLevel = -1, verbose = verbose
  )
  .libPaths(setupInitial$libPath)
  setwd(setupInitial$origWd)
}
