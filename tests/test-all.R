library(testit)
library(Require)
verbosity <- if (interactive()) 1 else -1
opts <- options(Require.verbose = verbosity)
unlink(dir(getOptionRPackageCache(), full.names = TRUE), recursive = TRUE)
# optsCRAN <- options(repos = c(CRAN = "https://muug.ca/mirror/cran"))
# optsCRAN <- options(repos = c(CRAN = "https://cloud.r-project.org"))
test_pkg("Require")
options(opts)
