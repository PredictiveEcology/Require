# Sys.setenv("Require.checkAsCRAN" = "true")

checks <- list()
checks$start <- list()
checks$start[["getwd"]] <- getwd()
checks$start[["libPaths"]] <- .libPaths()
checks$start[["envVars"]] <- Sys.getenv()
envOrig <- checks$start[["envVars"]]

if (length(strsplit(packageDescription("Require")$Version, "\\.")[[1]]) > 3) {
  Sys.setenv("RequireRunAllTests"="yes")
}
if (identical(Sys.getenv("Require.checkAsCRAN"), "true")) {
  .isDevTest <- FALSE
  .isDevTestAndInteractive <- .isDevTest
} else {
  .isDevTest <- Sys.getenv("RequireRunAllTests") == "yes"
  .isDevTestAndInteractive <- interactive() && .isDevTest
}
if (!.isDevTestAndInteractive) # i.e., CRAN
  Sys.setenv(Require.RPackageCache = "FALSE")

suppressPackageStartupMessages(library(Require)) # this will trigger data.table options to be set so that we have them part of our "before" snapshot
library(testit)

# These are only defined sufficiently to be undone *after* loading the package
checks$start[["options"]] <- options()
origOptions <- checks$start[["options"]]
checks$start[["cacheDir"]] <- dir(Require::RequireCacheDir(), recursive = TRUE)
checks$start[["tempdir2"]] <- dir(Require::tempdir2(), recursive = TRUE)

# helper files for this test
source(dir(pattern = "test-helpers.R", recursive = TRUE, full.names = TRUE))

# Can emulate CRAN by setting Sys.setenv("Require.checkAsCRAN" = "true"); source()
# if (isFALSE(.isDevTest)) {
#   try(unlink(RequireCacheDir(), recursive = TRUE))
# }

# Put this in a function so we can wrap it in a try, so that we can revert everything
runTests <- function(checks) {
  ## force tests to be executed if in dev release which we define as
  ## having a sub-release, eg 0.9.15.5 is one whereas 0.9.16 is not
  ee <- new.env()

  startTimeAll <- startTime <- Sys.time()
  tdOuter <- Require::tempdir2("tests")
  try(saveRDS(startTimeAll, file = file.path(tdOuter, "startTimeAll")), silent = TRUE)

  optsListNew <- list()
  verbosity <- if (.isDevTestAndInteractive) 1 else -2
    # options(Require.RPackageCache = FALSE)

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
    currOptions <- options()
    toRevert <- Require:::setdiffList(currOptions, origOptions)
    toRevert <- toRevert[grep("^datatable", names(toRevert), value = TRUE, invert = TRUE)] # don't revert data.table options

    envCur <- Sys.getenv()
    envNeedRm <- envCur[!names(envCur) %in% names(envOrig)]

    checks$prior <- list()
    checks$prior[["getwd"]] <- getwd()
    checks$prior[["options"]] <- options()
    checks$prior[["libPaths"]] <- .libPaths()
    checks$prior[["cacheDir"]] <- dir(Require::RequireCacheDir(), recursive = TRUE)
    checks$prior[["envVars"]] <- Sys.getenv()
    checks$prior[["tempdir2"]] <- dir(Require::tempdir2(), recursive = TRUE)

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
    # 1 libPaths
    .libPaths(origLibPaths)

    # 2 wd
    setwd(origWd)

    # 3 options
    options(toRevert)

    # 4 Cache
    if (!.isDevTest) {
      if (length(checks$start[["cacheDir"]]) == 0)
        try(unlink(checks$prior[["cacheDir"]], recursive = TRUE))
      else
        try(unlink(setdiff(checks$start[["cacheDir"]], checks$prior[["cacheDir"]])))
    }

    # 5 Sys.env
    ma <- match(names(envOrig), names(envCur) )
    envNeedRevert <- envCur[ma] != envOrig
    if (any(na.omit(envNeedRevert))) {
      envNeedRevert <- envOrig[envNeedRevert]
    }
    Sys.unsetenv(names(envNeedRm))

    # 6 tempdir2
    unlink(Require::tempdir2(), recursive = TRUE)

    checks$post <- list()
    checks$post[["getwd"]] <- getwd()
    checks$post[["options"]] <- options()
    checks$post[["libPaths"]] <- .libPaths()
    checks$post[["cacheDir"]] <- dir(Require::RequireCacheDir(FALSE), recursive = TRUE)
    checks$post[["envVars"]] <- Sys.getenv()
    checks$post[["tempdir2"]] <- dir(Require::tempdir2(), recursive = TRUE)

    Require:::messageVerbose("Done tests", verboseLevel = -2, verbose = verbosity)

    if (.isDevTestAndInteractive)
      saveRDS(checks, file = ".checks.RDS")

    # Check everything is reset to original
    nam <- names(checks$start)
    compare <- lapply(nam, function(x) {
      a <- checks$start[[x]]
      if (!is.null(names(a))) a <- a[order(names(a))]
      b <- checks$post[[x]]
      if (!is.null(names(b))) b <- b[order(names(b))]
      ae <- all.equal(a, b)
      if (!isTRUE(ae)) {
        print(paste(x, ae))
        print("##################")
        print(checks$start[[x]])
        print("##################")
        print(checks$post[[x]])
        print("##################")
      }
      ae
      })
    theTest <- all(unlist(compare))
    if (!isTRUE(theTest)) browser()
    testit::assert()

  }, add = TRUE)

  # The test
  test_pkg("Require")

}

runTests(checks)

