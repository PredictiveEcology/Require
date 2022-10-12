library(testit)
suppressPackageStartupMessages(library(Require)) # this will trigger data.table options to be set so that we have them part of our "before" snapshot
envOrig <- Sys.getenv()

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


if (length(strsplit(packageDescription("Require")$Version, "\\.")[[1]]) > 3) {
  Sys.setenv("RunAllRequireTests"="yes")
}
.isDevTest <- Sys.getenv("RunAllRequireTests") == "yes"
.isDevTestAndInteractive <- FALSE#interactive() && .isDevTest

# Put this in a function so we can wrap it in a try, so that we can revert everything
runTests <- function() {
  ## force tests to be executed if in dev release which we define as
  ## having a sub-release, eg 0.9.15.5 is one whereas 0.9.16 is not
  ee <- new.env()
  origOptions <- options()

  startTimeAll <- startTime <- Sys.time()
  tdOuter <- Require::tempdir2("tests")
  try(saveRDS(startTimeAll, file = file.path(tdOuter, "startTimeAll")), silent = TRUE)

  optsListNew <- list()
  verbosity <- if (.isDevTestAndInteractive) 1 else -1
  if (!.isDevTestAndInteractive)
    options(Require.RPackageCache = NULL)

  if (!startsWith(getOption("repos")[[1]], "http")) # deal with @CRAN@
    origRepos <- options(repos = c(CRAN = Require:::srcPackageURLOnCRAN))

  optsListPrevLinux <- if (.isDevTestAndInteractive) Require::setLinuxBinaryRepo() else NULL

  optsListNew <- Require::modifyList2(optsListNew, list(Require.verbose = verbosity),
                                      keep.null = TRUE)
  optsNcpus <- options("Ncpus" = 4)
  optsListPrev <- options(optsListNew)
  # optsListNew <- modifyList2(optsListNew, options("repos"))
  origLibPaths <- .libPaths()
  origWd <- getwd()

  on.exit({

    endTime <- Sys.time()
    try(Require:::messageVerbose("\033[32m ----------------------------------All Tests: ",format(endTime - startTimeAll)," \033[39m",
                                 verbose = getOption("Require.verbose"), verboseLevel = 0),
        silent = TRUE)

    fis <- ls(ee)
    names(fis) <- basename(fis)
    times <- lapply(fis, function(fi) ee[[fi]])
    times <- data.table::data.table(times)
    data.table::set(times, NULL, "file", names(fis))
    Require:::messageDF(times, verboseLevel = 0, verbose = getOption("Require.verbose"))


    # six things to clean up: libPaths, wd, options, cached pkgs, 5: envir vars; tempdir2
    # 1
    .libPaths(origLibPaths)

    # 2
    setwd(origWd)

    # 3
    currOptions <- options()
    changedOptions <- setdiff(currOptions, origOptions)
    toRevert <- origOptions[names(changedOptions)]
    names(toRevert) <- names(changedOptions)
    options(toRevert)

    # 4
    if (!.isDevTest) {
      try(unlink(RequireCacheDir(create = FALSE), recursive = TRUE))
    }

    # 5
    envCur <- Sys.getenv()
    envNeedRm <- envCur[!names(envCur) %in% names(envOrig)]
    envNeedRevert <- envCur[match(names(envOrig), names(envCur) )] != envOrig
    if (any(envNeedRevert)) {
      envNeedRevert <- envOrig[envNeedRevert]
    }
    Sys.unsetenv(names(envNeedRm))

    # 6
    unlink(Require::tempdir2(), recursive = TRUE)

    Require:::messageVerbose("Done tests", verboseLevel = -2, verbose = verbosity)

  }, add = TRUE)

  # The test
  test_pkg("Require")

}

runTests()
