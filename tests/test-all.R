library(testit)
library(Require)
optsListNew <- list()
optsListPrev <- list()

verbosity <- if (interactive()) 1 else 2
optsListPrevLinux <- if (interactive()) setLinuxBinaryRepo() else NULL

optsListNew <- modifyList2(optsListNew, list(Require.verbose = verbosity))

if (!startsWith(getOption("repos")[[1]], "http")) # deal with @CRAN@
  optsListNew <- modifyList2(optsListNew,
                             list(repos = c(CRAN = Require:::srcPackageURLOnCRAN)))
optsListPrev <- options(optsListNew)
optsListNew <- modifyList2(optsListNew, options("repos"))
optsListPrev <- modifyList2(optsListPrev, optsListPrevLinux)
# unlink(dir(getOptionRPackageCache(), full.names = TRUE), recursive = TRUE)
test_pkg("Require")
options(optsListPrev)
