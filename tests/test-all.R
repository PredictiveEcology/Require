checks <- list()
checks$start <- list()
checks$start[["getwd"]] <- getwd()
checks$start[["libPaths"]] <- .libPaths()
checks$start[["envVars"]] <- Sys.getenv()
envOrig <- checks$start[["envVars"]]

if (length(strsplit(packageDescription("Require")$Version, "\\.")[[1]]) > 3) {
  Sys.setenv("Require_RunAllTests"="yes")
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

source(dir(pattern = "test-helpers.R", recursive = TRUE, full.names = TRUE))



# Can emulate CRAN by setting Sys.setenv("Require.checkAsCRAN" = "true"); source()
if (isFALSE(.isDevTest)) try(unlink(RequireCacheDir(), recursive = TRUE))

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
    changedOptions1 <- setdiff(names(currOptions), names(origOptions)) # new option
    changedOptions2 <- setdiff(names(origOptions), names(currOptions)) # option set to NULL
    changedOptions3 <- vapply(names(origOptions), FUN.VALUE = logical(1), function(nam)
                              identical(currOptions[nam], origOptions[nam]), USE.NAMES = TRUE) # changed values of existing
    changedOptions3 <- origOptions[names(changedOptions3[!changedOptions3])]
    toRevert1 <- mapply(x = changedOptions1, function(x) NULL, USE.NAMES = TRUE)
    toRevert2 <- origOptions[changedOptions2]
    toRevert3 <- origOptions[names(changedOptions3)]
    toRevert <- modifyList2(toRevert1, toRevert2, toRevert3)
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
      try(unlink(RequireCacheDir(), recursive = TRUE))
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
    compare <- lapply(nam, function(x)
      all.equal(checks$start[[x]], checks$post[[x]]))
    testit::assert(all(unlist(compare)))

  }, add = TRUE)

  # The test
  test_pkg("Require")

}

runTests(checks)

