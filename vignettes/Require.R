## ----eval=FALSE---------------------------------------------------------------
# library(Require)
# Require::Install(
#   c("PredictiveEcology/reproducible@development (HEAD)",
#     "PredictiveEcology/SpaDES.core@development (>=2.0.5.9004)"))

## ----eval=FALSE,message=FALSE-------------------------------------------------
# Require::Install("knn")
# 
# try(pak::pkg_install(c("knn")))

## ----eval=FALSE,message=TRUE--------------------------------------------------
# library(Require)
# # Fails because of a) packages taken off CRAN & multiple GitHub branches requested within the nested dependencies
# pkgs <- c("reproducible", "PredictiveEcology/SpaDES@development")
# dirTmp <- tempdir2(sub = "first")
# .libPaths(dirTmp)
# install.packages("pak") # need this in the library; can't use personal library version
# try(pak::pkg_install(pkgs))
# # ✔ Loading metadata database ... done
# # Error : ! error in pak subprocess
# # Caused by error:
# # ! Could not solve package dependencies:
# # * reproducible: dependency conflict
# # * PredictiveEcology/SpaDES@development: Can't install dependency PredictiveEcology/reproducible@development (>=  2.0.10)
# # * PredictiveEcology/reproducible@development: Conflicts with reproducible
# pkgsAny <- c("any::reproducible", "PredictiveEcology/SpaDES@development")
# try(pak::pkg_install(pkgsAny))
# 
# # Fine
# dirTmp <- tempdir2(sub = "second")
# .libPaths(dirTmp)
# Require::Install(pkgs)

## ----eval=FALSE,message=TRUE--------------------------------------------------
# # Fails
# try(pk <- pak::pak(c("PredictiveEcology/LandR@development", "PredictiveEcology/LandR@main")))
# # Error : ! error in pak subprocess
# # Caused by error:
# # ! Could not solve package dependencies:
# # * PredictiveEcology/LandR@development: Conflicts with PredictiveEcology/LandR@main
# # * PredictiveEcology/LandR@main: Conflicts with PredictiveEcology/LandR@development
# 
# # Fine -- takes in order, so main first in this example
# rq <- Require::Install(c("PredictiveEcology/LandR@main", "PredictiveEcology/LandR@development"))
# 
# # Fine -- takes by version requirement, so takes development,
# #    which is the only one that fulfills requirement on Jul 25, 2024
# rq <- Require::Install(c("PredictiveEcology/LandR@main", "PredictiveEcology/LandR@development (>=1.1.5)"))
# 

## ----eval=FALSE,message=FALSE-------------------------------------------------
# try(gg <- pak::pkg_deps("PredictiveEcology/LandR@development", dependencies = TRUE))
# ff <- Require::pkgDep("PredictiveEcology/LandR@development", dependencies = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# # The following has no version specifications,
# #   so CRAN version will be installed or none installed if already installed
# Require::Install(c("PredictiveEcology/reproducible@development", "reproducible"))
# 
# # The following specifies "HEAD" after the Github package name. This means the
# #   tip of the development branch of reproducible will be installed if not already installed
# Require::Install(c("PredictiveEcology/reproducible@development (HEAD)", "reproducible"))
# 
# # The following specifies "HEAD" after the package name. This means the
# #   tip of the development branch of reproducible
# Require::Install(c("PredictiveEcology/reproducible@development", "reproducible (HEAD)"))
# 
# # Not a problem because version number specifies
# Require::Install(c("PredictiveEcology/reproducible@modsForLargeArchives (>=2.0.10.9010)",
#                    "PredictiveEcology/reproducible (>= 2.0.10)"))
# 
# # Even if branch does not exist, if later version requirement specifies a different branch, no error
# Require::Install(c("PredictiveEcology/reproducible@modsForLargeArchives (>=2.0.10.9010)",
#                    "PredictiveEcology/reproducible@validityTest (>= 2.0.9)"))

## ----eval=FALSE---------------------------------------------------------------
# ## FAILS - can't specify version requirements
# try(pak::pkg_install(
#     c("PredictiveEcology/reproducible@modsForLargeArchives (>=2.0.10.9010)",
#       "PredictiveEcology/reproducible (>= 2.0.10)")))

## ----eval=FALSE---------------------------------------------------------------
# # In this example, it is `terra` that generally needs to be installed from source on Linux
# if (Require:::isUbuntuOrDebian()) {
#   Require::setLinuxBinaryRepo()
#   pkgs <- c("terra", "PSPclean")
#   pkgFullName <- "ianmseddy/PSPclean@development"
#   try(remove.packages(pkgs))
#   pak::cache_delete() # make sure a locally built one is not present in the cache
#   try(pak::pkg_install(pkgFullName))
#   # ✔ Loading metadata database ... done
#   #
#   # → Will install 2 packages.
#   # → Will download 2 packages with unknown size.
#   # + PSPclean   0.1.4.9005 [bld][cmp][dl] (GitHub: fed9253)
#   # + terra      1.7-71     [dl] + ✔ libgdal-dev, ✔ gdal-bin, ✔ libgeos-dev, ✔ libproj-dev, ✔ libsqlite3-dev
#   # ✔ All system requirements are already installed.
#   #
#   # ℹ Getting 2 pkgs with unknown sizes
#   # ✔ Got PSPclean 0.1.4.9005 (source) (43.29 kB)
#   # ✔ Got terra 1.7-71 (x86_64-pc-linux-gnu-ubuntu-22.04) (4.24 MB)
#   # ✔ Downloaded 2 packages (4.28 MB) in 2.9s
#   # ✔ Installed terra 1.7-71  (61ms)
#   # ℹ Packaging PSPclean 0.1.4.9005
#   # ✔ Packaged PSPclean 0.1.4.9005 (420ms)
#   # ℹ Building PSPclean 0.1.4.9005
#   # ✖ Failed to build PSPclean 0.1.4.9005 (3.7s)
#   # Error:
#   # ! error in pak subprocess
#   # Caused by error in `stop_task_build(state, worker)`:
#   # ! Failed to build source package PSPclean.
#   # Type .Last.error to see the more details.
# 
# 
#   # Works fine because the `sourcePkgs()`
# 
#   try(remove.packages(pkgs)) # uninstall to make sure it is a clean install for this test
#   Require::cacheClearPackages(pkgs, ask = FALSE) # remove any existing local packages
#   Require::Install(pkgFullName)
# }

## ----eval=FALSE---------------------------------------------------------------
# depPak <- pak::pkg_deps("PredictiveEcology/LandR@LandWeb")
# depRequire <- Require::pkgDep("PredictiveEcology/LandR@LandWeb") # Slightly different default in Require
# 
# # Same
# pakDepsClean <- setdiff(Require::extractPkgName(depPak$ref), Require:::.basePkgs)
# requireDepsClean <- setdiff(Require::extractPkgName(depRequire[[1]]), Require:::.basePkgs)
# setdiff(pakDepsClean, requireDepsClean)
# setdiff(requireDepsClean, pakDepsClean) # does not report "RcppArmadillo", "RcppEigen", "cpp11" which are LinkingTo
# 

## ----eval=FALSE---------------------------------------------------------------
# gg <- pak::pkg_deps("PredictiveEcology/LandR@development", dependencies = TRUE)
# # Error:
# # ! error in pak subprocess
# # Caused by error:
# # ! Could not solve package dependencies:
# # * PredictiveEcology/LandR@development: Can't install dependency BioSIM
# # * BioSIM: Can't find package called BioSIM.
# # Type .Last.error to see the more details.
# ff <- Require::pkgDep("PredictiveEcology/LandR@development", dependencies = TRUE)
# # $`PredictiveEcology/LandR@development`
# #  [1] "BH"                                                      "BIEN"
# #  [3] "BioSIM"                                                  "DBI (>= 0.8)"
# #  [5] "Deriv"                                                   "ENMeval"
# #  ...

