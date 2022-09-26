library(testit)
library(Require)
opts <- options(Require.verbose = 2#,
                #Require.RPackageCache = FALSE
                )
unlink(dir(getOptionRPackageCache(), full.names = TRUE), recursive = TRUE)
# optsCRAN <- options(repos = c(CRAN = "https://muug.ca/mirror/cran"))
# optsCRAN <- options(repos = c(CRAN = "https://cloud.r-project.org"))
test_pkg("Require")
options(opts)
