thisFilename <- "test-4other.R"
startTime <- Sys.time()
message("\033[32m --------------------------------- Starting ",thisFilename,"  at: ",format(startTime),"---------------------------\033[39m")
library(Require)
origLibPathsAllTests <- .libPaths()
Sys.setenv("R_REMOTES_UPGRADE" = "never")
Sys.setenv('CRANCACHE_DISABLE' = TRUE)
outOpts <- options(#"Require.verbose" = FALSE,
                   "Require.persistentPkgEnv" = TRUE,
                   "install.packages.check.source" = "never",
                   "install.packages.compile.from.source" = "never",
                   "Require.unloadNamespaces" = TRUE)
if (Sys.info()["user"] == "achubaty") {
  outOpts2 <- options("Require.Home" = "~/GitHub/PredictiveEcology/Require")
} else {
  outOpts2 <- options("Require.Home" = "~/GitHub/Require")
}

# Test misspelled
out <- capture.output(type = "message", Require("data.tt", verbose = 1))
testit::assert(any(grepl("could not be installed", out)))#{out, "simpleWarning")})

# for coverages that were missing
pkgDTEmpty <- Require:::toPkgDT(character())
out <- Require:::installedVers(pkgDTEmpty) #

# test warn missing
out <- Require:::updateInstalled(pkgDTEmpty, installPkgNames = "package")

pkgDep("data.table", purge = FALSE)
pkgDep("data.table", purge = TRUE)
pkgDep2("Require")


pkgDepTopoSort(c("data.table"), useAllInSearch = TRUE)
pkgDepTopoSort(c("data.table"), useAllInSearch = TRUE, deps = "Require")
pkgDepTopoSort(c("Require", "data.table"))
pkgDepTopoSort(c("Require", "data.table"), useAllInSearch = TRUE,
               deps = "Require", returnFull = FALSE, reverse = TRUE)

Require:::pkgDepCRAN("Require", keepVersionNumber = TRUE, purge = TRUE)


if (Sys.info()["user"] == "emcintir2") {
  options("Require.RPackageCache" = TRUE,
          "Require.unloadNamespaces" = FALSE)
}
Require("data.table", install = "force", require = FALSE, libPaths = tempdir2("other"))
suppressWarnings(Require("Require", install = "force", require = FALSE,
                         libPaths = tempdir2("other")))

pkg <- c("data.table", "data.table")
pkgDT <- Require:::toPkgDT(pkg)
data.table::set(pkgDT, NULL, "installFrom", "CRAN")
data.table::set(pkgDT, NULL, "installed", FALSE)
data.table::set(pkgDT, NULL, "installResult", TRUE)
Require:::rmDuplicatePkgs(pkgDT)

data.table::set(pkgDT, NULL, "versionSpec", NA)
Require:::rmDuplicatePkgs(pkgDT)

out <- detachAll("data.table", dontTry = "testit")
testit::assert({isTRUE(out['data.table'] == 1)})

warn <- tryCatch(Require:::warningCantInstall("devtolls"), warning = function(w) w$message)
testit::assert({grepl("you will likely", warn)})

origLP <- setLibPaths(tempdir2("other"), updateRprofile = FALSE)
warn <- tryCatch(Require("data.table"), warning = function(w) w$message)
setLibPaths(origLP, updateRprofile = FALSE)

# Test the setLibPaths with changed .Rprofile
origDir <- setwd(tempdir2("other"))
setLibPaths("newProjectLib", updateRprofile = TRUE) # set a new R package library locally
setLibPaths() # reset it to original
setwd(origDir)

if (!identical(origLibPathsAllTests, .libPaths())) {
  Require::setLibPaths(origLibPathsAllTests, standAlone = TRUE, exact = TRUE)
}

options(outOpts)
if (exists("outOpts2")) options(outOpts2)
## setup
# assign("aaaa", 1, envir = .GlobalEnv)
options(RequireOptions())
setupTestDir <- normPath(tempdir2("setupTests"))
ccc <- checkPath(file.path(setupTestDir, ".cache"), create = TRUE)
setup(setupTestDir, RPackageCache = ccc)
testit::assert(identical(getOption("Require.RPackageCache"), ccc)) ## TODO: warnings in readLines() cannot open DESCRIPTION file
setupOff()
message("This is getOption('Require.RPackageCache'): ", Require:::getOptionRPackageCache())
RPackageCacheSysEnv <- Sys.getenv("Require.RPackageCache")
if (identical(RPackageCacheSysEnv, "FALSE")) {
  testit::assert(identical(NULL, getOptionRPackageCache()))
} else {
  testit::assert(identical(normPath(Require:::getOptionRPackageCache()), normPath(Require::RequirePkgCacheDir())))
}

# reset options after setupOff()
secondTry <- normPath(file.path(setupTestDir, ".cacheSecond"))
opt22 <- options("Require.RPackageCache" = secondTry)
ccc <- checkPath(secondTry, create = TRUE)
setup(setupTestDir, RPackageCache = ccc) ## TODO: warnings in file() cannot open DESCRIPTION files
testit::assert(identical(Require:::getOptionRPackageCache(), ccc))
setupOff()
testit::assert(identical(Require:::getOptionRPackageCache(), secondTry)) # BECAUSE THIS IS A MANUAL OVERRIDE of options; doesn't return Sys.getenv

ooo <- options(Require.RPackageCache = TRUE)
testit::assert(identical(getOptionRPackageCache(), RequirePkgCacheDir()))
ooo <- options(Require.RPackageCache = FALSE)
testit::assert(identical(getOptionRPackageCache(), NULL))
ooo <- options(Require.RPackageCache = tempdir())
testit::assert(identical(getOptionRPackageCache(), tempdir()))
ooo <- options(Require.RPackageCache = "default")
RPackageCacheSysEnv <- Sys.getenv("Require.RPackageCache")
if (identical(RPackageCacheSysEnv, "FALSE")) {
  testit::assert(identical(NULL, getOptionRPackageCache()))
} else {
  testit::assert(identical(normPath(Require:::getOptionRPackageCache()), normPath(Require::RequirePkgCacheDir())))
}
ooo <- options(Require.RPackageCache = NULL)
testit::assert(identical(getOptionRPackageCache(), NULL))


options(opt22)
endTime <- Sys.time()
message("\033[32m ----------------------------------",thisFilename, ": ", format(endTime - startTime)," \033[39m")
