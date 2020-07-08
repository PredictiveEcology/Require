utils::globalVariables(c(
  "..colsToKeep", "..keepCols", "Account", "archiveSource", "AvailableVersion",
  "bothDepAndOrig", "Branch", "compareVersionAvail", "correctVersion", "correctVersionAvail",
  "DESCFile", "depOrOrig", "download.file", "fullGit", "githubPkgName", "GitSubFolder",
  "hasSubFolder", "hasVersionSpec", "inequality", "installed", "isGH", "isInteractive",
  "libPaths", "loaded", "needInstall", "OlderVersionsAvailable", "OlderVersionsAvailableCh",
  "Package", "packageFullName", "packagesRequired", "PackageUrl", "pkgDepTopoSort",
  "Repo", "repoLocation", "RepoWBranch", "toLoad", "Version", "versionSpec"
))

#' Repeatability-safe install and load packages, optionally with specific versions
#'
#' This is an "all in one" function that will run \code{install.packages} for
#' CRAN packages, \code{remotes::install_github} for \url{https://github.com/} packages and
#' will install specific versions of each package if versions are specified either
#' via an inequality (e.g., \code{"Holidays (>=1.0.0)"}) or with a
#' \code{packageVersionFile}. The function will then run \code{require} on all
#' named packages that satisfy their version requirements. If packages are already installed
#' (\code{packages} supplied), and their optional version numbers are satisfied,
#' then the "install" component will be skipped.
#'
#' \code{standAlone} will either put the \code{Require}d packages and their
#' dependencies \emph{all} within the \code{libPaths} (if \code{TRUE}) or if
#' \code{FALSE} will only install packages and their dependencies that are
#' otherwise not installed in \code{.libPaths()}, i.e., the personal or base
#' library paths. Any packages or dependencies that are not yet installed will
#' be installed in \code{libPaths}. Importantly, a small hidden file (named
#' \code{._packageVersionsAuto.txt}) will be saved in \code{libPaths} that will
#' store the \emph{information} about the packages and their dependencies, even
#' if the version used is located in \code{.libPaths()}, i.e., not the
#' \code{libPaths} provided. This hidden file will be used if a user runs
#' \code{pkgSnapshot}, enabling a new user to rebuild the entire dependency
#' chain, without having to install all packages in an isolated directory (as
#' does \pkg{packrat}). This will save potentially a lot of time and disk space,
#' and yet maintain reproducibility. \emph{NOTE}: since there is only one hidden
#' file in a \code{libPaths}, any call to \code{pkgSnapshot} will make a snapshot
#' of the most recent call to \code{Require}.
#'
#' To build a snapshot of the desired packages and their versions, first run
#' \code{Require} with all packages, then \code{pkgSnapshot}. If a
#' \code{libPaths} is used, it must be used in both functions.
#'
#' This function works best if all required packages are called within one
#' \code{Require} call, as all dependencies can be identified together, and all
#' package versions will be saved automatically (with \code{standAlone = TRUE}
#' or \code{standAlone = FALSE}), allowing a call to \code{pkgSnapshot} when a
#' more permanent record of versions can be made.
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
#' @param purge Logical. Internally, there are calls to \code{available.packages}
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
#' Require("PredictiveEcology/SpaDES@development")
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
#' Require::Require("SpaDES") # not necessary to specifify standAlone here because .libPaths are set
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
  browser(expr = exists("._Require_0"))
  if (!missing(packageVersionFile)) {
    packages <- data.table::fread(packageVersionFile)
    packages <- if (NROW(packages)) {
      paste0(packages$instPkgs, " (==", packages$instVers, ")")
    } else {
      character()
    }
  }

  doDeps <- if (!is.null(list(...)$dependencies)) list(...)$dependencies else NA
  which <- whichToDILES(doDeps)

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
    packageNamesOrig[origPackagesHaveNames] <- names(packagesOrig)[origPackagesHaveNames]
  packagesOrder <- seq(packagesOrig)
  names(packagesOrder) <- extractPkgName(packageNamesOrig)

  if (missing(libPaths))
    libPaths <- .libPaths()
  origLibPaths <- setLibPaths(libPaths, standAlone)
  on.exit({setLibPaths(origLibPaths)}, add = TRUE)

  browser(expr = exists("._Require_1"))
  if (length(which) && (isTRUE(install) || identical(install, "force"))) {
    packages <- getPkgDeps(packages, which = which, purge = purge)
  }


  # Create data.table of Require workflow
  if (is(packages, "list")) packages <- unlist(packages, recursive = FALSE)
  pkgDT <- data.table(Package = extractPkgName(packages), packageFullName = c(packages))

  # identify the packages that were asked by user to load -- later dependencies will be in table too

  pkgDT[Package %in% unique(extractPkgName(packageNamesOrig)),
        packagesRequired := packagesOrder[match(Package, names(packagesOrder))]]
  pkgDT[, toLoad := packagesRequired] # this will start out as toLoad = TRUE, but if install fails, will turn to FALSE

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
      pkgDT <- installFrom(pkgDT)
      pkgDT <- doInstalls(pkgDT, install_githubArgs = install_githubArgs,
                          install.packagesArgs = install.packagesArgs,
                          install = install, ...)
    }
    if (isTRUE(require))
      pkgDT <- doLoading(pkgDT, ...)
  }
  out <- pkgDT[packagesRequired > 0]$loaded
  names(out) <- pkgDT[packagesRequired > 0]$Package
  if (verbose > 0) {
    if (verbose < 2) {
      colsToKeep <- intersect(colsToKeep, colnames(pkgDT))
      pkgDT <- pkgDT[, ..colsToKeep]
    }

    attr(out, "Require") <- pkgDT[]
  }
  return(out)
}
