setupTest <- function(verbose = getOption("Require.verbose"), envir = parent.frame()) {
  opts <- options()
  requireNamespace("waldo")
  # requireNamespace("diffob")

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

  if (Sys.info()["user"] %in% "emcintir")
    withr::local_options(Require.cloneFrom = Sys.getenv("R_LIBS_USER"), .local_envir = envir)
  withr::local_options(Require.RPackageCache = RequirePkgCacheDir(), .local_envir = envir)


  if (Sys.info()["user"] == "achubaty") {
    outOpts2 <- options("Require.Home" = "~/GitHub/PredictiveEcology/Require")
  } else {
    outOpts2 <- options("Require.Home" = "~/GitHub/Require")
  }

  libPath <- .libPaths()
  setLibPaths(tempdir2(.rndstr()))
  origWd <- getwd()
  # thisFilename <- Require:::getInStack("r")
  # env <- Require:::whereInStack("ee")
  startTime <- Sys.time()
  # Require:::messageVerbose(Require:::green(" --------------------------------- Starting ",
  #   thisFilename, "  at: ", format(startTime, digits = 2),
  #   "---------------------------"),
  #   verbose = verbose, verboseLevel = 0
  # )
  Require:::messageVerbose(Require:::blue(" getOption('Require.verbose'): ",
    getOption("Require.verbose")),
    verboseLevel = 0
  )
  Require:::messageVerbose(Require:::blue(" getOption('repos'): ",
    paste(getOption("repos"), collapse = ", ")),
    verboseLevel = 0
  )
  return(list(startTime = startTime, # thisFilename = thisFilename,
              libPath = libPath, origWd = origWd, opts = opts))
}

endTest <- function(setupInitial, verbose = getOption("Require.verbose")) {
  currOptions <- options()
  changedOptions <- setdiff(currOptions, setupInitial$opts)
  toRevert <- setupInitial$opts[names(changedOptions)]
  names(toRevert) <- names(changedOptions)
  if (any(grepl("datatable.alloccol", names(toRevert)))) browser()
  options(toRevert)


  # thisFilename <- setupInitial$thisFilename
  endTime <- Sys.time()
  # ee <- Require:::getInStack("ee")
  # ee[[thisFilename]] <- format(endTime - setupInitial$startTime, digits = 2)
  # Require:::messageVerbose("\033[32m ----------------------------------",
  #   thisFilename, ": ", ee[[thisFilename]], " \033[39m",
  #   verboseLevel = -1, verbose = verbose
  # )
  .libPaths(setupInitial$libPath)
  setwd(setupInitial$origWd)
}


omitPkgsTemporarily <- function(pkgs) {
  if (getRversion() < "4.2") {
    pkgs <- grep("mumin", pkgs, invert = TRUE, value = TRUE) # MuMIn requires R >= 4.2
    pkgs <- grep("LandR", pkgs, invert = TRUE, value = TRUE) # LandR requires R >= 4.2
    pkgs <- grep("fireSenseUtils", pkgs, invert = TRUE, value = TRUE) # LandR requires R >= 4.2
  }
  # while not on CRAN
  pkgs <- grep("^SpaDES.core", pkgs, invert = TRUE, value = TRUE) # not on CRAN
#   pkgs <- grep("SpaDES.experiment", pkgs, invert = TRUE, value = TRUE) # file.move is not exported from reproducible
  pkgs
}
