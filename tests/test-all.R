library(testit)
library(Require)
optsListNew <- list()
optsListPrev <- list()

verbosity <- if (interactive()) 1 else -1
optsListPrevLinux <- if (interactive()) setLinuxBinaryRepo() else NULL

optsListNew <- modifyList2(optsListNew, list(Require.verbose = verbosity))
if (is.null(getOption("repos")))
  optsListNew <- modifyList2(optsListNew, list(repos = c(CRAN = "https://cloud.r-project.org")))
optsListPrev <- options(optsListNew)
optsListNew <- modifyList2(optsListNew, options("repos"))
optsListPrev <- modifyList2(optsListPrev, optsListPrevLinux)
#unlink(dir(getOptionRPackageCache(), full.names = TRUE), recursive = TRUE)
#test_pkg("Require")
# options(optsListPrev)
