origLibPathsAllTests <- .libPaths()
Sys.setenv("R_REMOTES_UPGRADE" = "never")
Sys.setenv('CRANCACHE_DISABLE' = TRUE)
outOpts <- options("Require.persistentPkgEnv" = TRUE,
                   "install.packages.check.source" = "never",
                   "install.packages.compile.from.source" = "never",
                   "Require.unloadNamespaces" = TRUE)
if (Sys.info()["user"] == "emcintir") {
  outOpts2 <- options("Require.Home" = "~/GitHub/Require",
                      "Require.RPackageCache" = "~/._RPackageCache/")
} else {
  outOpts2 <- options("Require.Home" = "~/GitHub/PredictiveEcology/Require")
}

# Test misspelled
out <- tryCatch(Require("data.tt"), warning = function(w) w)
testit::assert({is(out, "simpleWarning")})

# for coverages that were missing
pkgDTEmpty <- Require:::toPkgDT(character())
out <- Require:::installedVers(pkgDTEmpty) # 

# test warn missing
out <- Require:::updateInstalled(pkgDTEmpty, installPkgNames = "package") 

pkgDep("data.table", purge = FALSE)
pkgDep("data.table", purge = TRUE)
pkgDep2("Require")


pkgDepTopoSort(c("data.table", "remotes"), useAllInSearch = TRUE)
pkgDepTopoSort(c("data.table", "remotes"), useAllInSearch = TRUE, deps = "Require")
pkgDepTopoSort(c("Require", "data.table", "remotes"))
pkgDepTopoSort(c("Require", "data.table", "remotes"), useAllInSearch = TRUE, 
               deps = "Require", returnFull = FALSE, reverse = TRUE)

Require:::pkgDepCRAN("Require", keepVersionNumber = TRUE, purge = TRUE)


options("Require.RPackageCache" = "~/._RPackageCache/",
        "Require.unloadNamespaces" = FALSE)
Require("data.table", install = "force", require = FALSE, libPaths = tempdir2(basename(tempdir())))
suppressWarnings(Require("Require", install = "force", require = FALSE,
                         libPaths = tempdir2(basename(tempdir()))))

pkg <- c("data.table", "data.table")
pkgDT <- Require:::toPkgDT(pkg)
data.table::set(pkgDT, NULL, "installFrom", "CRAN")
data.table::set(pkgDT, NULL, "installed", FALSE)
data.table::set(pkgDT, NULL, "installResult", TRUE)
Require:::rmDuplicatePkgs(pkgDT)

data.table::set(pkgDT, NULL, "versionSpec", NA)
Require:::rmDuplicatePkgs(pkgDT)

out <- detachAll("data.table")
testit::assert({isTRUE(out['data.table'] == 1)})

warn <- tryCatch(Require:::warningCantInstall("devtolls"), warning = function(w) w$message)
testit::assert({grepl("you will likely", warn)})

origLP <- setLibPaths(tempdir2(basename(tempdir())), updateRprofile = FALSE)
warn <- tryCatch(Require("data.table"), warning = function(w) w$message)
setLibPaths(origLP, updateRprofile = FALSE)

# Test the setLibPaths with changed .Rprofile
origDir <- setwd(tempdir())
setLibPaths("newProjectLib", updateRprofile = TRUE) # set a new R package library locally
setLibPaths() # reset it to original
setwd(origDir)

if (!identical(origLibPathsAllTests, .libPaths()))
  Require::setLibPaths(origLibPathsAllTests, standAlone = TRUE, exact = TRUE)
options(outOpts)
options(outOpts2)

## setup
options(RequireOptions())
setupTestDir <- normPath(tempdir2("setupTests"))
ccc <- checkPath(file.path(setupTestDir, ".cache"), create = TRUE)
setup(setupTestDir, RPackageCache = ccc)
testit::assert(identical(getOption("Require.RPackageCache"), ccc))
setupOff()
message("This is getOption('Require.RPackageCache')", getOption("Require.RPackageCache"))
message("This is NULL")
testit::assert(identical(getOption("Require.RPackageCache"), NULL))

# reset options after setupOff()
secondTry <- normPath(file.path(setupTestDir, ".cacheSecond"))
opt22 <- options("Require.RPackageCache" = secondTry)
ccc <- checkPath(secondTry, create = TRUE)
setup(setupTestDir, RPackageCache = ccc)
testit::assert(identical(getOption("Require.RPackageCache"), ccc))
setupOff()
testit::assert(identical(getOption("Require.RPackageCache"), secondTry))
options(opt22)

