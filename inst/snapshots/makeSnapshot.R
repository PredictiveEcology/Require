## Regenerate the snapshot test-09 installs for the running R version:
##
##   Rscript inst/snapshots/makeSnapshot.R        # writes inst/snapshots/R-<major.minor>.txt
##
## Installs `pkgs` into a fresh standAlone library with the current Require,
## then records what landed with pkgSnapshot(). Nothing else is edited by hand:
## the test asserts against this file, and knownFails.txt lists what is known
## not to build on a given R version.
##
## The set is small on purpose -- one of each kind of thing Require has to get
## right -- not a copy of a real project library:
pkgs <- c(
  ## pure R, no dependencies
  "praise", "crayon", "R6", "whisker", "ini", "assertthat",
  ## short dependency chains (install levels)
  "rprojroot", "here",
  "R.methodsS3", "R.oo", "R.utils",
  ## a longer chain with several levels
  "cli", "glue", "rlang", "lifecycle", "vctrs", "pillar", "magrittr", "tibble",
  "generics", "tidyselect", "withr", "dplyr",
  ## compiled
  "Rcpp", "data.table", "digest", "fs", "jsonlite", "yaml", "fastmap", "fastmatch",
  "ps", "processx", "callr", "bit", "bit64", "stringi", "stringr",
  ## LinkingTo chain (header-only BH is also the version pak's solver refuses to pin)
  "BH", "sitmo", "dqrng",
  ## SystemRequirements
  "curl", "openssl", "xml2", "png",
  ## versions that are no longer current: exercises the CRAN Archive path
  "bitops (==1.0-7)", "backports (==1.4.1)",
  ## GitHub at a commit, and an r-universe repository
  "MangoTheCat/visualTest@9b835a7",
  "fpCompare"
)
rUniverse <- "https://predictiveecology.r-universe.dev"

options(repos = c(PE = rUniverse, getOption("repos")), Require.verbose = 1)
lib <- file.path(tempdir(), "snapshotLib"); dir.create(lib, showWarnings = FALSE)
Require::Install(pkgs, libPaths = lib, standAlone = TRUE)
out <- file.path("inst", "snapshots", sprintf("R-%s.%s.txt", R.version$major, sub("\\..*", "", R.version$minor)))
Require::pkgSnapshot(out, libPaths = lib, standAlone = TRUE)
message("wrote ", out, ": ", nrow(data.table::fread(out)), " rows")
