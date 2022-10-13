library(testit)
suppressPackageStartupMessages(library(Require)) # this will trigger data.table options to be set so that we have them part of our "before" snapshot
envOrig <- Sys.getenv()

source(dir(pattern = "test-helpers.R", recursive = TRUE, full.names = TRUE))


if (length(strsplit(packageDescription("Require")$Version, "\\.")[[1]]) > 3) {
  Sys.setenv("RunAllRequireTests"="yes")
}

# Can emulate CRAN by setting Sys.setenv("Require.checkAsCRAN" = "true"); source()
if (identical(Sys.getenv("Require.checkAsCRAN"), "true")) {
  .isDevTest <- FALSE
  .isDevTestAndInteractive <- .isDevTest
} else {
  .isDevTest <- Sys.getenv("RunAllRequireTests") == "yes"
  .isDevTestAndInteractive <- interactive() && .isDevTest
}
if (isFALSE(.isDevTest)) try(unlink(RequireCacheDir(create = FALSE), recursive = TRUE))

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
                                 verbose = getOption("Require.verbose"), verboseLevel = -1),
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
    print("envNeedRevert")
    if (any(na.omit(envNeedRevert))) {
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
