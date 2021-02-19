utils::globalVariables(c(
  "..colsToKeep", "..colsToKeep2", "..keepCols", "Account", "archiveSource", "AvailableVersion",
  "bothDepAndOrig", "Branch", "compareVersionAvail", "correctVersion", "correctVersionAvail",
  "DESCFile", "depOrOrig", "download.file", "fullGit", "githubPkgName", "GitSubFolder",
  "hasSubFolder", "hasVersionSpec", "inequality", "installed", "isGH", "isInteractive",
  "libPaths", "LibPath", "loaded", "needInstall", "OlderVersionsAvailable", "OlderVersionsAvailableCh",
  "Package", "packageFullName", "packagesRequired", "PackageUrl", "pkgDepTopoSort",
  "Repo", "repoLocation", "RepoWBranch", "loadOrder", "Version", "versionSpec","detached", "."
))

#' Repeatability-safe install and load packages, optionally with specific versions
#'
#' This is an "all in one" function that will run \code{install.packages} for
#' CRAN packages, \code{remotes::install_github} for \url{https://github.com/} packages and
#' will install specific versions of each package if versions are specified either
#' via an (in)equality (e.g., \code{"Holidays (>=1.0.0)"} or \code{"Holidays (==1.0.0)"} for 
#' an exact version) or with a
#' \code{packageVersionFile}. If \code{require = TRUE}, the default, 
#' the function will then run \code{require} on all
#' named packages that satisfy their version requirements. If packages are already installed
#' (\code{packages} supplied), and their optional version numbers are satisfied,
#' then the "install" component will be skipped.
#'
#' \code{standAlone} will either put the \code{Require}d packages and their
#' dependencies \emph{all} within the \code{libPaths} (if \code{TRUE}) or if
#' \code{FALSE} will only install packages and their dependencies that are
#' otherwise not installed in \code{.libPaths()[1]}, i.e., the current active
#' R package directory. Any packages or dependencies that are not yet installed will
#' be installed in \code{libPaths}. 
#' 
#' @section GitHub Package:
#' Follows \code{remotes::install_github} standard as this is what is used internally.
#' As with \code{remotes::install_github}, it is not possible to specify a past
#' version of a GitHub package, without supplying a SHA that had that package
#' version. Similarly, if a developer does a local install e.g., via 
#' \code{devtools::install}, of an active project, this package will not be able 
#' know of the GitHub state, and thus \code{pkgSnapshot} will not be able to 
#' recover this state as there is no SHA associated with a local
#' installation. Use \code{Require} or \code{install_github} to create
#' a record of the GitHub state.
#'
#' @section Package Snapshots:
#' To build a snapshot of the desired packages and their versions, first run
#' \code{Require} with all packages, then \code{pkgSnapshot}. If a
#' \code{libPaths} is used, it must be used in both functions.
#'
#' @section Mutual Dependencies:
#' This function works best if all required packages are called within one
#' \code{Require} call, as all dependencies can be identified together, and all
#' package versions will be addressed (if there are no conflicts), 
#' allowing a call to \code{\link{pkgSnapshot}} to take a snapshot or "record" of 
#' the current collection of packages and versions.
#'
#' @section Local Cache of Packages:
#' When installing new packages, `Require` will put all source and binary files
#' in an R-version specific subfolder of 
#' \code{getOption("Require.RPackageCache")} whose default is `NULL`, meaning 
#' \emph{do not cache packages locally},
#' and will reuse them if needed. To turn
#' on this feature, set \code{options("Require.RPackageCache" = "someExistingFolder")}.
#'
#' @note
#' For advanced use and diagnosis, the user can set \code{verbose = TRUE} or
#' \code{1} or \code{2} (or via \code{options("Require.verbose")}). This will
#' attache an attribute \code{attr(obj, "Require")} to the output of this
#' function.
#'
#' @param install Logical or "force". If \code{FALSE}, this will not try to
#'   install anything. If \code{"force"}, then it will force installation of
#'   requested packages, mimicking a call to e.g., \code{install.packages}. If
#'   \code{TRUE}, the default, then this function will try to install any
#'   missing packages or dependencies.
#' @param require Logical. If \code{TRUE}, the default, then the function will
#'   attempt to call \code{require} on all requested \code{packages}, possibly
#'   after they are installed.
#' @param packages Character vector of packages to install via
#'   \code{install.packages}, then load (i.e., with \code{library}). If it is
#'   one package, it can be unquoted (as in \code{require}). In the case of a
#'   GitHub package, it will be assumed that the name of the repository is the
#'   name of the package. If this is not the case, then pass a named character
#'   vector here, where the names are the package names that could be different
#'   than the GitHub repository name.
#' @param packageVersionFile If provided, then this will override all
#'   \code{install.package} calls with \code{versions::install.versions}
#' @param libPaths The library path (or libraries) where all packages should be
#'   installed, and looked for to load (i.e., call \code{library}). This can be
#'   used to create isolated, stand alone package installations, if used with
#'   \code{standAlone = TRUE}. Currently, the path supplied here will be
#'   prepended to \code{.libPaths()} (temporarily during this call) to
#'   \code{Require} if \code{standAlone = FALSE} or will set (temporarily)
#'        \code{.libPaths()} to \code{c(libPaths, tail(libPaths(), 1)} to keep base packages.
#' @param repos The remote repository (e.g., a CRAN mirror), passed to either
#'              \code{install.packages}, \code{install_github} or \code{installVersions}.
#' @param install_githubArgs List of optional named arguments, passed to \code{install_github}.
#' @param install.packagesArgs List of optional named arguments, passed to \code{install.packages}.
#' @param standAlone Logical. If \code{TRUE}, all packages will be installed to and loaded from
#'                   the \code{libPaths} only. If \code{FALSE}, then \code{libPath} will
#'                   be prepended to \code{.libPaths()} during the \code{Require} call,
#'                   resulting in shared packages, i.e.,
#'                   it will include the user's default package folder(s).
#'                   This can be create dramatically faster
#' installs if the user has a substantial number of the packages already in
#' their personal library. Default \code{FALSE} to minimize package installing.
#' @param purge Logical. Should all caches be purged Default is 
#'   \code{getOption("Require.purge", FALSE)}.
#'   There is a lot of internal caching of results throughout 
#'   the \code{Require} package. These help with speed and reduce calls to internet sources.
#'   However, sometimes these caches must be purged. The cached values are renewed 
#'   when found to be too old, with the age limit. 
#'   This maximum age can be set in seconds with the environment variable 
#'   \code{R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE}, or if unset, 
#'   defaults to 3600  (one hour -- see
#'   \code{\link[utils]{available.packages}}).
#'   
#'   Internally, there are calls to \code{available.packages}
#' @param verbose Numeric. If \code{1} (less) or \code{2} (more), there will be
#'   a data.table with many details attached to the output
#' @param ... Passed to \emph{all} of \code{install_github},
#'   \code{install.packages}, and \code{remotes::install_version}, i.e., the
#'   function will error if all of these functions can not use the ... argument.
#'   Good candidates are e.g., \code{type} or \code{dependencies}. This can be
#'   used with \code{install_githubArgs} or \code{install.packageArgs} which
#'   give individual options for those 2 internal function calls.
#'
#' @export
#' @importFrom data.table data.table as.data.table setDT set is.data.table
#'   rbindlist
#' @importFrom data.table  :=  .I .SD setnames setorderv
#' @importFrom remotes install_github install_version
#' @importFrom utils available.packages capture.output compareVersion
#' @importFrom utils install.packages packageVersion
#'
#' @examples
#' \dontrun{
#' # simple usage, like conditional install.packages then library
#' library(Require)
#' Require("stats") # analogous to require(stats), but it checks for
#'                  #   pkg dependencies, and installs them, if missing
#' tempPkgFolder <- file.path(tempdir(), "Packages")
#'
#' # use standAlone, means it will put it in libPaths, even if it already exists
#' #   in another local library (e.g., personal library)
#' Require("crayon", libPaths = tempPkgFolder, standAlone = TRUE)
#'
#' # make a package version snapshot of installed packages
#' packageVersionFile <- "_.packageVersionTest.txt"
#' (pkgSnapshot(libPath = tempPkgFolder, packageVersionFile, standAlone = TRUE))
#'
#' # Restart R -- to remove the old temp folder (it disappears with restarting R)
#' library(Require)
#' tempPkgFolder <- file.path(tempdir(), "Packages")
#' packageVersionFile <- "_.packageVersionTest.txt"
#' # Reinstall and reload the exact version from previous
#' Require(packageVersionFile = packageVersionFile, libPaths = tempPkgFolder, standAlone = TRUE)
#'
#' # Create mismatching versions -- desired version is older than current installed
#' # This will try to install the older version, overwriting the newer version
#' desiredVersion <- data.frame(instPkgs="crayon", instVers = "1.3.2", stringsAsFactors = FALSE)
#' write.table(file = packageVersionFile, desiredVersion, row.names = FALSE)
#' newTempPkgFolder <- file.path(tempdir(), "Packages2")
#'
#' # Note this will install the 1.3.2 version (older that current on CRAN), but
#' #   because crayon is still loaded in memory, it will return TRUE, using the current version
#' #   of crayon. To start using the older 1.3.2, need to unload or restart R
#' Require("crayon", packageVersionFile = packageVersionFile,
#'         libPaths = newTempPkgFolder, standAlone = TRUE)
#'
#' # restart R again to get access to older version
#' # run again, this time, correct "older" version installs in place of newer one
#' library(Require)
#' packageVersionFile <- "_.packageVersionTest.txt"
#' newTempPkgFolder <- file.path(tempdir(), "Packages3")
#' Require("crayon", packageVersionFile = packageVersionFile,
#'         libPaths = newTempPkgFolder, standAlone = TRUE)
#'
#' # Mutual dependencies, only installs once -- e.g., httr
#' tempPkgFolder <- file.path(tempdir(), "Packages")
#' Require(c("cranlogs", "covr"), libPaths = tempPkgFolder, standAlone = TRUE)
#'
#' ##########################################################################################
#' # Isolated projects -- Just use a project folder and pass to libPaths or set .libPaths() #
#' ##########################################################################################
#' # GitHub packages -- restart R because crayon is needed
#' library(Require)
#' ProjectPackageFolder <- file.path(tempdir(), "ProjectA")
#' #  THIS ONE IS LARGE -- > 100 dependencies -- use standAlone = FALSE to
#' #    reuse already installed packages --> this won't allow as much control
#' #    of package versioning
#' Require("PredictiveEcology/SpaDES@development",
#'         libPaths = ProjectPackageFolder, standAlone = FALSE)
#'
#' # To keep totally isolated: use standAlone = TRUE
#' #   --> setting .libPaths() directly means standAlone is not necessary; it will only
#' #   use .libPaths()
#' library(Require)
#' ProjectPackageFolder <- file.path("~", "ProjectA")
#' setLibPaths(ProjectPackageFolder)
#' Require("PredictiveEcology/SpaDES@development") # the latest version on GitHub
#' Require("PredictiveEcology/SpaDES@23002b2a92a92df4ccba7f51cdd82798800b2fa7") 
#'                               # a specific commit (by using the SHA)
#'
#'
#' ############################################################################
#' # Mixing and matching GitHub, CRAN, with and without version numbering
#' ############################################################################
#' # Restart R -- when installing/loading packages, start fresh
#' pkgs <- c("Holidays (<=1.0.4)", "TimeWarp (<= 1.0.3)", "glmm (<=1.3.0)",
#'           "achubaty/amc@development", "PredictiveEcology/LandR@development (>=0.0.1)",
#'           "PredictiveEcology/LandR@development (>=0.0.2)", "ianmseddy/LandR.CS (<=0.0.1)")
#' Require::Require(pkgs)
#'
#' ############################################################################
#' # Using libPaths -- This will only be used inside this function;
#' # To change .libPaths() for the whole session use a manually call to
#' # setLibPaths(newPath) first
#' ############################################################################
#' Require::Require("SpaDES", libPaths = "~/TempLib2", standAlone = FALSE)
#'
#' ############################################################################
#' # Persistent separate packages
#' ############################################################################
#' setLibPaths("~/TempLib2", standAlone = TRUE)
#' Require::Require("SpaDES") # not necessary to specify standAlone here because .libPaths are set
#' 
#' ############################################################################
#' # Installing on many machines that are connected by a shared drive
#' ############################################################################
#' options("Require.RPackageCache" = "~/binaryRPackages") # create binaries on the fly. 
#'                                                        # Put thes in a shared location.
#' # May need to install Require in main user library before setting library paths for project
#' if (!require("Require")) install.packages("Require") 
#' setLibPaths("./packages") # not shared location for library path; no longer using main user lib
#' Require::Require(packageVersionFile = "./packageVersions.txt",
#'                  standAlone = TRUE)
#' 
#' }
#'
Require <- function(packages, packageVersionFile,
                    libPaths, # nolint
                    install_githubArgs = list(),
                    install.packagesArgs = list(),
                    standAlone = getOption("Require.standAlone", FALSE),
                    install = getOption("Require.install", TRUE),
                    require = getOption("Require.require", TRUE),
                    repos = getOption("repos"),
                    purge = getOption("Require.purge", FALSE),
                    verbose = getOption("Require.verbose", FALSE),
                    ...) {
  
  origDTThreads <- data.table::setDTthreads(1)
  on.exit(data.table::setDTthreads(origDTThreads))
  
  purge <- dealWithCache(purge)
  doDeps <- if (!is.null(list(...)$dependencies)) list(...)$dependencies else NA
  which <- whichToDILES(doDeps)
  
  libPaths <- checkLibPaths(libPaths = libPaths)
  # if (missing(libPaths))
  #   libPaths <- .libPaths()
  suppressMessages(origLibPaths <- setLibPaths(libPaths, standAlone))
  
  if (!missing(packageVersionFile)) {
    packages <- data.table::fread(packageVersionFile)
    # if  (!any(packages$Package == "Require")) {
    #   # Doesn't list Require
    #   packages <- rbindlist(list(packages, 
    #                              data.table(Package = "Require", LibPath = .libPaths()[1], 
    #                                         Version = as.character(packageVersion("Require")))), 
    #                         fill = TRUE, use.names = TRUE)
    # }
    packages <- packages[!packages$Package %in% .basePkgs]
    uniqueLibPaths <- unique(packages$LibPath)
    if (length(uniqueLibPaths) > 1) {
      dt <- data.table(libPathInSnapshot = uniqueLibPaths, 
                       newLibPaths = normPath(c(libPaths[1], 
                                                file.path(libPaths[1], 
                                                          gsub(":", "", uniqueLibPaths[-1])))))
      message("packageVersionFile is covering more than one library; installing packages in reverse order; ",
              "also -- .libPaths() will be altered to be\n")
      messageDF(dt)
      callArgs <- as.list(match.call())[-1]
      out <- Map(lib = rev(dt$libPathInSnapshot), 
                 newLib = rev(dt$newLibPaths), function(lib, newLib) {
                   tf <- tempfile2("RequireSnapshot")
                   packages <- packages[packages$LibPath == lib]
                   data.table::fwrite(packages, file = tf)
                   callArgs[["packageVersionFile"]] <- tf
                   callArgs[["libPaths"]] <- newLib
                   callArgs[["standAlone"]] <- TRUE
                   out <- do.call(Require, args = callArgs)
                 })
      out <- unlist(out)
      setLibPaths(dt$newLibPaths, standAlone = TRUE)
      message(" to echo the multiple paths in ", packageVersionFile)
      
      if (isTRUE(require)) {
        return(out)
      } else {
        return(invisible(out))
      }
      packages[, LibPath := .libPaths()[1]]  
    }
    if (NROW(packages)) {
      set(packages, NULL, "Package", paste0(packages$Package, " (==", packages$Version, ")"))
    } else {
      character()
    }
    if (any(grep("github", tolower(colnames(packages))))) {
      haveGit <- nchar(packages[["GithubSHA1"]]) > 0
      if (sum(haveGit, na.rm = TRUE)) {
        packages[haveGit, `:=`(Package = paste0(GithubUsername, "/", GithubRepo, "@", GithubSHA1) )]
      }
    }
    packages <- packages$Package
    which <- NULL
    install_githubArgs[c("dependencies", "upgrade")] <- list(FALSE, FALSE)
    install.packagesArgs["dependencies"] = FALSE
    require <- FALSE
    oldEnv <- Sys.getenv("R_REMOTES_UPGRADE")
    Sys.setenv(R_REMOTES_UPGRADE = "never")
    on.exit({Sys.setenv("R_REMOTES_UPGRADE" = oldEnv)}, add = TRUE)
    message("Using ", packageVersionFile, "; setting `require = FALSE`")
  }
  on.exit({suppressMessages(setLibPaths(origLibPaths, standAlone = TRUE, exact = TRUE))}, 
          add = TRUE)
  
  # Rm base packages -- this will happen in getPkgDeps if that is run
  if (NROW(packages)) {
    
    # Some package names are not derived from their GitHub repo names -- user can supply named packages
    origPackagesHaveNames <- nchar(names(packages)) > 0
    if (any(origPackagesHaveNames))
      packages <- packages[order(names(packages), decreasing = TRUE)]
    dups <- duplicated(packages)
    packages <- packages[!dups] # (unique removes names) sometimes people pass identical packages -- only <10s microseconds
    origPackagesHaveNames <- nchar(names(packages)) > 0 # redo -- changed order
    packagesOrig <- packages
    packageNamesOrig <- packages
    
    if (any(origPackagesHaveNames))
      packageNamesOrig[origPackagesHaveNames] <- packagesOrig[origPackagesHaveNames]
    packagesOrder <- seq(packagesOrig)
    names(packagesOrder) <- extractPkgName(packageNamesOrig)
    
    if (length(which) && (isTRUE(install) || identical(install, "force"))) {
      packages <- getPkgDeps(packages, which = which, purge = purge)
    }
    
    
    # Create data.table of Require workflow
    if (is(packages, "list")) packages <- unlist(packages, recursive = FALSE)
    
    pkgDT <- toPkgDT(packages, deepCopy = TRUE)
    # identify the packages that were asked by user to load -- later dependencies will be in table too
    # some cases, original was without version, but due to a dependency that does have a version, 
    # it is no longer the same as orig package name
    pkgDT[packageFullName %in% unique(packageNamesOrig) | Package %in% unique(packageNamesOrig), 
          packagesRequired := packagesOrder[match(Package, names(packagesOrder))]]
    pkgDT[, loadOrder := packagesRequired] # this will start out as loadOrder = TRUE, but if install fails, will turn to FALSE
    
    if (any(origPackagesHaveNames))
      pkgDT[packageFullName %in% packagesOrig[origPackagesHaveNames],
            Package := names(packagesOrig[origPackagesHaveNames])]
    
    # Join installed with requested
    pkgDT <- installedVers(pkgDT)
    pkgDT <- pkgDT[, .SD[1], by = "packageFullName"] # remove duplicates
    pkgDT[, `:=`(installed = !is.na(Version), loaded = FALSE)]
    
    if (length(packages)) {
      if (isTRUE(install) || identical(install, "force")) {
        pkgDT <- parseGitHub(pkgDT)
        pkgDT <- getPkgVersions(pkgDT, install = install)
        pkgDT <- getAvailable(pkgDT, purge = purge, repos = repos)
        pkgDT <- installFrom(pkgDT, purge = purge, repos = repos)
        pkgDT <- rmDuplicatePkgs(pkgDT)
        
        # Remove base packages from the installation step
        pkgDT <- pkgDT[Package %in% .basePkgs, needInstall := NA]
        
        if (any(!is.na(pkgDT$needInstall))) {
          install.packagesArgs["INSTALL_opts"] <- unique(c('--no-multiarch', install.packagesArgs[["INSTALL_opts"]]))
          install_githubArgs["INSTALL_opts"] <- unique(c('--no-multiarch', install_githubArgs[["INSTALL_opts"]]))
          if (is.null(list(...)$destdir) && (isTRUE(install) || identical(install, "force"))) {
            if (!is.null(rpackageFolder(getOption("Require.RPackageCache")))) {
              ip <- .installed.pkgs()
              isCranCacheInstalled <- any(grepl("crancache", ip[, "Package"])) && identical(Sys.getenv("CRANCACHE_DISABLE"), "")
              if (isTRUE(isCranCacheInstalled)) {
                message("Package crancache is installed and option('Require.RPackageCache') is set; it is unlikely that both are needed. ",
                        "turning off crancache with Sys.setenv('CRANCACHE_DISABLE' = TRUE). ",
                        "To use only crancache's caching mechanism, set both:", 
                        "\noptions('Require.RPackageCache' = NULL)\n",
                        "Sys.setenv('CRANCACHE_DISABLE' = '')")
                Sys.setenv('CRANCACHE_DISABLE' = TRUE)
              }
              
              checkPath(rpackageFolder(getOption("Require.RPackageCache")), create = TRUE)
              install.packagesArgs["destdir"] <- paste0(gsub("/$", "", rpackageFolder(getOption("Require.RPackageCache"))), "/")
              if (getOption("Require.buildBinaries", TRUE)) {
                # if (isWindows() && getOption("Require.buildBinaries", TRUE)) {
                  install.packagesArgs[["INSTALL_opts"]] <- unique(c('--build', install.packagesArgs[["INSTALL_opts"]]))
              }
              
              install_githubArgs["destdir"]<- install.packagesArgs["destdir"]
            }
          }
        }
        
        pkgDT <- doInstalls(pkgDT, install_githubArgs = install_githubArgs,
                            install.packagesArgs = install.packagesArgs,
                            install = install, ...)
      }
      if ("detached" %in% colnames(pkgDT)) {
        unloaded <- pkgDT[!is.na(detached)]
        if (NROW(unloaded)) {
          reloaded <- lapply(unloaded[detached == 2]$Package, loadNamespace)
          relibraried <- lapply(unloaded[detached == 3]$Package, require, character.only = TRUE)
          message("Attempting to reload namespaces that were detached: ", paste(unloaded[detached == 2]$Package, collapse = ", "))
          message("Attempting to reattach to the search path: ", paste(unloaded[detached == 2]$Package, collapse = ", "))
        }
      }
      if (isTRUE(require)) {
        pkgDT <- doLoading(pkgDT, ...)
      }
    }
    out <- pkgDT[packagesRequired > 0]$loaded
    # outOrder <- pkgDT[packagesRequired > 0]$packagesRequired
    names(out) <- pkgDT[packagesRequired > 0]$packageFullName
    
    # put back in original order
    outOrder <- match(pkgDT[packagesRequired > 0]$packageFullName, packagesOrig)
    orderNAs <- is.na(outOrder)
    if (any(orderNAs))
      outOrder[orderNAs] <- match( pkgDT[packagesRequired > 0][orderNAs]$Package, packagesOrig)
    out <- out[order(outOrder)]
    
    if (verbose > 0) {
      if (verbose < 2) {
        colsToKeep <- c("packageFullName", "Package", "installed", "loadOrder", "loaded", "installFrom", 
                        "Version", "repoLocation", "correctVersion", "hasVersionSpec",
                        "correctVersionAvail")
        colsToKeep <- intersect(colsToKeep, colnames(pkgDT))
        pkgDT <- pkgDT[, ..colsToKeep]
      }
      
      attr(out, "Require") <- pkgDT[]
    }
    stillNeeded <- if (!is.null(pkgDT$installResult)) if (any(grep("No available", pkgDT$installResult))) {
      pkgDT[installed == FALSE, list(Package, packageFullName, installResult)]
    } else {
      pkgDT[0]
    }
    if (NROW(stillNeeded)) { 
      message("Several packages are not on CRAN, its archives (for this OS), or don't have GitHub tracking ",
              "information and thus will not be installed. ",
              "These may have been installed locally from source, or are on another ",
              "repository system, such as BioConductor:")
      messageDF(stillNeeded[, list(Package, packageFullName, installResult)]) 
    }
    notCorrectly <- pkgDT$installed == FALSE & pkgDT$needInstall == TRUE
    if (sum(notCorrectly)) {
      message("The following packages did not get installed correctly. Try to rerun Require call again without any changes...")
      colsToKeep2 <- c("packageFullName", "Package", "LibPath", "Version", 
                       "repoLocation", "installFrom", "installResult")
      messageDF(pkgDT[notCorrectly == TRUE, ..colsToKeep2])
      nonZeroExit <- grepl("had non-zero", pkgDT[notCorrectly == TRUE, ..colsToKeep2]$installResult)
      if (any(nonZeroExit)) {
        nonZ <- pkgDT[notCorrectly == TRUE, ..colsToKeep2]
        message("It may be necessary to simply run:\ninstall.packages(c('",paste(nonZ$Package, collapse = "', '"),"'))",
                "\nbut this will cause a different version to be installed.")
        if (!missing(packageVersionFile)) {
          message("If packages are installed as per above, you may wish to rerun pkgSnapshot('",packageVersionFile,"') to update with the new version")
        }
      }
      
    } else {
      if (!is.null(pkgDT$needInstall)) {
        nas <- is.na(pkgDT$needInstall)
        if (!all(nas)) {
          allCorrect <- pkgDT$needInstall[!nas] == TRUE & pkgDT$installed[!nas] == FALSE
          if (length(allCorrect)) {
            allInstalled <- pkgDT[!nas][allCorrect]
            if (NROW(allInstalled) == 0)
              message("All packages appear to have installed correctly")
          }
        }
      }
    }
  } else {
    out <- logical()
  }
  
  if (isTRUE(require)) {
    return(out)
  } else {
    return(invisible(out))
  }
}

