# Sys.setenv("_R_CHECK_THINGS_IN_OTHER_DIRS_" = "true") ## check if things left behind
# Sys.setenv("R_REQUIRE_RUN_ALL_TESTS" = "true") # use this if Version is a release version, but want to run as Dev
# Sys.setenv(R_REQUIRE_CHECK_AS_CRAN = "true") # set this to test as if it is CRAN (short and silent)
# Sys.setenv(R_REQUIRE_TEST_AS_INTERACTIVE = "false") # set this to test as if GitHub Actions (short and loud)
# THIS IS USING THE MECHANISM FOR CRAN THAT IF THE VERSION NUMBER IS NOT A DEV VERSION (E.G., .9000) THEN IT IS RUN AS CRAN
# source("tests/test-all.R") # run this for tests; set neither of above 2 for "long" testing
########## THe next 3 lines are good to test for CRAN
# Sys.setenv("_R_CHECK_THINGS_IN_OTHER_DIRS_" = "TRUE") ## check if things left behind
# Sys.setenv(R_REQUIRE_CHECK_AS_CRAN = "true") # set this to test as if it is CRAN (short and silent)
# devtools::check(args = c('--as-cran','--run-dontrun','--no-clean','--run-donttest'))

optNcpus <- options(Ncpus = 2L)

checks <- list()
checks$start <- list()
checks$start[["getwd"]] <- getwd()
checks$start[["libPaths"]] <- .libPaths()
checks$start[["envVars"]] <- Sys.getenv()
envOrig <- checks$start[["envVars"]]

if (Require:::.isDevelVersion() && nchar(Sys.getenv("R_REQUIRE_RUN_ALL_TESTS")) == 0) {
  Sys.setenv("R_REQUIRE_RUN_ALL_TESTS" = "true")
}
# GitHub Actions, R CMD check locally
isDev <- Sys.getenv("R_REQUIRE_RUN_ALL_TESTS") == "true" &&
  Sys.getenv("R_REQUIRE_CHECK_AS_CRAN") != "true"
# Actually interactive
isDevAndInteractive <- interactive() && isDev && Sys.getenv("R_REQUIRE_TEST_AS_INTERACTIVE") != "false"

if (!isDevAndInteractive) { # i.e., CRAN
  Sys.setenv(R_REQUIRE_PKG_CACHE = "FALSE")
}

suppressPackageStartupMessages(library(Require)) # this will trigger data.table options to be set so that we have them part of our "before" snapshot
library(testit)

# These are only defined sufficiently to be undone *after* loading the package
checks$start[["options"]] <- options()
origOptions <- checks$start[["options"]]
checks$start[["cacheDir"]] <- dir(Require::RequireCacheDir(FALSE), recursive = TRUE)
checks$start[["tempdir2"]] <- dir(Require::tempdir2(create = FALSE), recursive = TRUE)

# helper files for this test
source(dir(pattern = "test-helpers.R", recursive = TRUE, full.names = TRUE))

# Can emulate CRAN by setting Sys.setenv(R_REQUIRE_CHECK_AS_CRAN = "true"); source()
# if (isFALSE(isDev)) {
#   try(unlink(RequireCacheDir(), recursive = TRUE))
# }

# Put this in a function so we can wrap it in a try, so that we can revert everything
# runTests <- function(checks) {
## force tests to be executed if in dev release which we define as
## having a sub-release, eg 0.9.15.5 is one whereas 0.9.16 is not
ee <- new.env()

optsBioC <- options(
  BIOCONDUCTOR_USE_CONTAINER_REPOSITORY = FALSE,
  BiocManager.check_repositories = FALSE
)

startTimeAll <- startTime <- Sys.time()
tdOuter <- Require::tempdir2("tests")
try(saveRDS(startTimeAll, file = file.path(tdOuter, "startTimeAll")), silent = TRUE)

optsListNew <- list()
verbosity <- if (isDevAndInteractive) 2 else if (isDev) 2 else -2
# options(Require.RPackageCache = FALSE)

if (!startsWith(getOption("repos")[[1]], "http")) { # deal with @CRAN@
  origRepos <- options(repos = c(CRAN = Require:::srcPackageURLOnCRAN))
}

optsListPrevLinux <- if (isDevAndInteractive) Require::setLinuxBinaryRepo() else NULL

optsListNew <- Require::modifyList2(optsListNew, list(Require.verbose = verbosity),
  keep.null = TRUE
)
optsListPrev <- options(optsListNew)
# optsListNew <- modifyList2(optsListNew, options("repos"))
origLibPaths <- .libPaths()
origWd <- getwd()

# The test
try(test_pkg("Require")) # not sure if this works with try

if (!isDevAndInteractive || !isDev) { # i.e., CRAN
  Require:::.cleanup(list())
}
# runTests(checks)

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
checks$prior[["tempdir2"]] <- dir(Require::tempdir2(create = FALSE), recursive = TRUE)

endTime <- Sys.time()
try(
  Require:::messageVerbose("\033[32m ----------------------------------All Tests: ",
    format(endTime - startTimeAll), " \033[39m",
    verbose = getOption("Require.verbose"), verboseLevel = -1
  ),
  silent = TRUE
)

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
options(optNcpus)

# 4 Cache -- remove everything that was added in this test
if (!isDev) {
  if (length(checks$start[["cacheDir"]]) == 0) {
    try(unlink(checks$prior[["cacheDir"]], recursive = TRUE))
  } else {
    try(unlink(setdiff(checks$start[["cacheDir"]], checks$prior[["cacheDir"]])))
  }
}

# 5 Sys.env
envVarsChanged <- c(
  "CRANCACHE_DISABLE", "R_REMOTES_UPGRADE", "R_REQUIRE_PKG_CACHE",
  "R_REQUIRE_RUN_ALL_TESTS", "R_TESTS"
)
envVarsChangedNeedUnset <- names(envCur)[names(envCur) %in% envVarsChanged]
if (length(envVarsChangedNeedUnset)) {
  Sys.unsetenv(envVarsChangedNeedUnset)
}

## try to unset all env vars; this is too broad as there seem to be many that are set by GitHub Actions
if (FALSE) {
  ma <- match(names(envOrig), names(envCur))
  envNeedRevert <- envCur[ma] != envOrig
  if (any(na.omit(envNeedRevert))) {
    envNeedRevert <- envOrig[envNeedRevert]
  }
  Sys.unsetenv(names(envNeedRm))
}

# 6 tempdir2
unlink(Require::tempdir2(create = FALSE), recursive = TRUE)

checks$post <- list()
checks$post[["getwd"]] <- getwd()
checks$post[["options"]] <- options()
checks$post[["libPaths"]] <- .libPaths()
checks$post[["cacheDir"]] <- dir(Require::RequireCacheDir(FALSE), recursive = TRUE)
checks$post[["envVars"]] <- Sys.getenv()
checks$post[["tempdir2"]] <- dir(Require::tempdir2(create = FALSE), recursive = TRUE)

Require:::messageVerbose("Done tests", verboseLevel = -2, verbose = verbosity)

if (!isDev) {
  Require:::.cleanup(list())
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

if (FALSE) { # this is good to ensure all folders are empty
  list(
    dir(tools::R_user_dir("Require", which = "cache")),
    dir(dirname(tools::R_user_dir("Require", which = "cache"))),
    dir(dirname(dirname(tools::R_user_dir("Require", which = "cache"))), pattern = "^R$"),
    dir(tools::R_user_dir("Require", which = "config")),
    dir(tools::R_user_dir("Require", which = "data")),
    dir(tempdir(), pattern = "^Require$", full.names = T),
    dir(dirname(Require:::defaultCacheDirOld), pattern = "Require", full.names = TRUE),
    dir(dirname(Require:::defaultCacheDirOld), full.names = TRUE, pattern = "^R$|Require"),
    # dir(Require::tempdir2(create = FALSE), recursive = TRUE),
    setdiff(dir(getwd()), c(
      "codecov.yml", "CONTRIBUTING.md", "cran-comments.md", "DESCRIPTION",
      "inst", "man", "NAMESPACE", "NEWS.md", "R", "README.md", "Require.Rproj",
      "revdep", "tests", "docs", "CRAN-SUBMISSION"
    ))
  )
}
