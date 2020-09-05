# Test misspelled
out <- tryCatch(Require("data.tt"), warning = function(w) w)
testit::assert(is(out, "simpleWarning"))

# for coverages that were missing
pkgDTEmpty <- toPkgDT(character())
out <- installedVers(pkgDTEmpty) # 

# test warn missing
out <- updateInstalled(pkgDTEmpty, installPkgNames = "package") 

pkgDep("data.table", purge = FALSE)
pkgDep("data.table", purge = TRUE)
pkgDep2("Require")


pkgDepTopoSort(c("data.table", "remotes"), useAllInSearch = TRUE)
pkgDepTopoSort(c("data.table", "remotes"), useAllInSearch = TRUE, deps = "Require")
pkgDepTopoSort(c("Require", "data.table", "remotes"))
pkgDepTopoSort(c("Require", "data.table", "remotes"), useAllInSearch = TRUE, 
               deps = "Require", returnFull = FALSE, reverse = TRUE)

pkgDepCRAN("Require", keepVersionNumber = TRUE, purge = TRUE)


options("Require.RPackageCache" = tempdir2(basename(tempdir())),
        "Require.unloadNamespaces" = FALSE)
Require("data.table", install = "force", require = FALSE, libPaths = tempdir2(basename(tempdir())))
suppressWarnings(Require("Require", install = "force", require = FALSE, libPaths = tempdir2(basename(tempdir()))))

pkg <- c("data.table", "data.table")
pkgDT <- toPkgDT(pkg)
set(pkgDT, NULL, "installFrom", "CRAN")
set(pkgDT, NULL, "installed", FALSE)
set(pkgDT, NULL, "installResult", TRUE)
rmDuplicatePkgs(pkgDT)

set(pkgDT, NULL, "versionSpec", NA)
rmDuplicatePkgs(pkgDT)

out <- detachAll("data.table")
testit::assert(isTRUE(out == 1))

warn <- tryCatch(warningCantInstall("devtolls"), warning = function(w) w$message)
testit::assert(grepl("you will likely", warn))