# Sys.setenv("Require.checkAsCRAN" = "true") # set this to test as if it is CRAN (short and silent)
# Sys.setenv("Require.testAsInteractive" = "false") # set this to test as if GitHub Actions (short and loud)
# THIS IS USING THE MECHANISM FOR CRAN THAT IF THE VERSION NUMBER IS NOT A DEV VERSION (E.G., .9000) THEN IT IS RUN AS CRAN
# source("tests/test-all.R") # run this for tests; set neither of above 2 for "long" testing
checks <- list()
checks$start <- list()
checks$start[["getwd"]] <- getwd()
checks$start[["libPaths"]] <- .libPaths()
checks$start[["envVars"]] <- Sys.getenv()
envOrig <- checks$start[["envVars"]]

if (length(strsplit(packageDescription("Require")$Version, "\\.")[[1]]) > 3) {
  Sys.setenv("RequireRunAllTests"="yes")
}
# GitHub Actions, R CMD check locally
isDev <- Sys.getenv("RequireRunAllTests") == "yes" && Sys.getenv("Require.checkAsCRAN") != "true"
# Actually interactive
isDevAndInteractive <- interactive() && isDev && Sys.getenv("Require.testAsInteractive") != "false"

if (!isDevAndInteractive) # i.e., CRAN
  Sys.setenv(Require.RPackageCache = "FALSE")

suppressPackageStartupMessages(library(Require)) # this will trigger data.table options to be set so that we have them part of our "before" snapshot
library(testit)

# These are only defined sufficiently to be undone *after* loading the package
checks$start[["options"]] <- options()
origOptions <- checks$start[["options"]]
checks$start[["cacheDir"]] <- dir(Require::RequireCacheDir(FALSE), recursive = TRUE)
checks$start[["tempdir2"]] <- dir(Require::tempdir2(), recursive = TRUE)

# helper files for this test
source(dir(pattern = "test-helpers.R", recursive = TRUE, full.names = TRUE))

# Can emulate CRAN by setting Sys.setenv("Require.checkAsCRAN" = "true"); source()
# if (isFALSE(isDev)) {
#   try(unlink(RequireCacheDir(), recursive = TRUE))
# }

# Put this in a function so we can wrap it in a try, so that we can revert everything
#runTests <- function(checks) {
## force tests to be executed if in dev release which we define as
## having a sub-release, eg 0.9.15.5 is one whereas 0.9.16 is not
ee <- new.env()

optsBioC <- options(BIOCONDUCTOR_USE_CONTAINER_REPOSITORY = FALSE,
                    BiocManager.check_repositories = FALSE)

startTimeAll <- startTime <- Sys.time()
tdOuter <- Require::tempdir2("tests")
try(saveRDS(startTimeAll, file = file.path(tdOuter, "startTimeAll")), silent = TRUE)

optsListNew <- list()
verbosity <- if (isDevAndInteractive) 2 else if (isDev) 2 else -2
# options(Require.RPackageCache = FALSE)

if (!startsWith(getOption("repos")[[1]], "http")) # deal with @CRAN@
  origRepos <- options(repos = c(CRAN = Require:::srcPackageURLOnCRAN))

 optsListPrevLinux <- if (isDevAndInteractive) Require::setLinuxBinaryRepo() else NULL

optsListNew <- Require::modifyList2(optsListNew, list(Require.verbose = verbosity),
                                    keep.null = TRUE)
optsNcpus <- options("Ncpus" = 4)
optsListPrev <- options(optsListNew)
# optsListNew <- modifyList2(optsListNew, options("repos"))
origLibPaths <- .libPaths()
origWd <- getwd()


# The test
try(test_pkg("Require")) # not sure if this works with try


if (!isDevAndInteractive) # i.e., CRAN
  unlink(tools::R_user_dir("Require", "cache"), recursive = TRUE)
#runTests(checks)

currOptions <- options()
toRevert <- Require::setdiffNamed(currOptions, origOptions, missingFill = NULL)
toRevert <- toRevert[grep("^datatable", names(toRevert), value = TRUE, invert = TRUE)] # don't revert data.table options

envCur <- Sys.getenv()
envNeedRm <- envCur[!names(envCur) %in% names(envOrig)]

checks$prior <- list()
checks$prior[["getwd"]] <- getwd()
checks$prior[["options"]] <- options()
checks$prior[["libPaths"]] <- .libPaths()
checks$prior[["cacheDir"]] <- dir(Require::RequireCacheDir(create = FALSE), recursive = TRUE)
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

# 4 Cache -- remove everything that was added in this test
if (!isDev) {
  if (length(checks$start[["cacheDir"]]) == 0)
    try(unlink(checks$prior[["cacheDir"]], recursive = TRUE))
  else
    try(unlink(setdiff(checks$start[["cacheDir"]], checks$prior[["cacheDir"]])))
}

# 5 Sys.env
envVarsChanged <- c("CRANCACHE_DISABLE", "R_REMOTES_UPGRADE", "Require.RPackageCache",
                    "RequireRunAllTests", "R_TESTS")
envVarsChangedNeedUnset <- names(envCur)[names(envCur) %in% envVarsChanged]
if (length(envVarsChangedNeedUnset))
  Sys.unsetenv(envVarsChangedNeedUnset)

if (FALSE) { # This will try to unset all env vars; this is too broad as there seem to be many that are set by GA
  ma <- match(names(envOrig), names(envCur) )
  envNeedRevert <- envCur[ma] != envOrig
  if (any(na.omit(envNeedRevert))) {
    envNeedRevert <- envOrig[envNeedRevert]
  }
  Sys.unsetenv(names(envNeedRm))
}

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

if (!isDev) {
  unlink(Require::tempdir2(), recursive = TRUE)
  unlink(dir(dirname(tools::R_user_dir("Require", "cache")), full.names = T), recursive = T)
}
# Check everything is reset to original
if (FALSE) {
  if (isDev) {
    nam <- names(checks$start)
    hashes <- "########################################################################"
    compare <- lapply(nam, function(x) {
      a <- checks$start[[x]]
      if (!is.null(names(a))) a <- a[order(names(a))]
      b <- checks$post[[x]]
      if (!is.null(names(b))) b <- b[order(names(b))]
      ae <- all.equal(a, b)
      if (!isTRUE(ae)) {
        print(paste(x, hashes))
        try(print(setdiffNamed(a, b, missingFill = NULL)), silent = TRUE)
        try(print(setdiffNamed(a, b, missingFill = "")), silent = TRUE)
      }
      ae
    })
    # theTest <- all(unlist(compare))
  }
}
# if (!isTRUE(theTest)) browser()
# testit::assert()
