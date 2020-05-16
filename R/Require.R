if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".SD", ":=", "Account", "AvailableVersion", "Branch", "Package",
                           "Repo", "RepoWBranch", "Version", "compareVersionAvail", "correctVersion",
                           "correctVersionAvail", "download.file", "fullGit", "githubPkgName",
                           "hasVersionSpec", "inequality", "installed", "isGH", "packageFullName",
                           "versionSpec", "..colsToKeep", "..keepCols", "DESCFile", "OlderVersionsAvailable",
                           "OlderVersionsAvailableCh", "PackageUrl", "archiveSource", "isInteractive",
                           "libPaths", "needInstall", "pkgDepTopoSort", "repoLocation", "toLoad"))
}

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
#' dependencies \emph{all} within the libPaths (if \code{TRUE}) or if
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
#' For advanced use and diagnosis, the user can set \code{options("Require.verbose")}
#' and there will be an attribute \code{attr(obj, "Require")} attached to the output
#' of this function.
#'
#' @param install Logical or "force". If \code{FALSE}, this will not try to install anything.
#'        If \code{"force"}, then it will force installation of requested packages,
#'        mimicking a call to e.g., \code{install.packages}. If \code{TRUE}, the default,
#'        then this
#'        function will try to install any missing packages or dependencies.
#' @param require Logical. If \code{TRUE}, the default, then the function will attempt
#'        to call \code{require} on all requested \code{packages}, possibly after they
#'        are installed.
#' @param packages Character vector of packages to install via
#'        \code{install.packages}, then load (i.e., with \code{library}). If it is
#'        one package, it can be unquoted (as in \code{require}). In the case of a GitHub
#'        package, it will be assumed that the name of the repository is the name of the
#'        package. If this is not the case, then pass a named character vector here, where
#'        the names are the package names that could be different than the GitHub repository
#'        name.
#' @param packageVersionFile If provided, then this will override all \code{install.package}
#'        calls with \code{versions::install.versions}
#' @param libPaths The library path (or libraries) where all packages should be installed,
#'        and looked for to load (i.e., call \code{library}). This can be used to create
#'        isolated, stand alone package installations, if used with \code{standAlone = TRUE}.
#'        Currently, the path supplied here will be prepended to \code{.libPaths()} unless
#'        \code{standAlone = TRUE}.
#' @param repos The remote repository (e.g., a CRAN mirror), passed to either
#'              \code{install.packages}, \code{install_github} or \code{installVersions}.
#' @param install_githubArgs List of optional named arguments, passed to \code{install_github}.
#' @param install.packagesArgs List of optional named arguments, passed to \code{install.packages}.
#' @param standAlone Logical. If \code{TRUE}, all packages will be installed to and loaded from
#'                   the \code{libPaths} only. If \code{FALSE}, then \code{libPath} will
#'                   be prepended to \code{.libPaths()}, resulting in shared packages, i.e.,
#'                   it will include the user's default package folder(s).
#'                   This can be create dramatically faster
#'                   installs if the user has a substantial number of the packages already in their
#'                   personal library. In the case of \code{TRUE}, there will be a hidden file
#'                   place in the \code{libPaths} directory that lists all the packages
#'                   that were needed during the \code{Require} call. Default \code{FALSE} to
#'                   minimize package installing.
#' @param ... Passed to \emph{all} of \code{install_github}, \code{install.packages}, and
#'   \code{remotes::install_version}, i.e., the function will error if all of these functions
#'   can not use the ... argument. Good candidates are e.g., \code{type} or \code{dependencies}.
#'   This can be used with \code{install_githubArgs} or \code{install.packageArgs} which
#'   give individual options for those 2 internal function calls.
#' @export
#' @importFrom remotes install_github install_version
#' @importFrom data.table data.table as.data.table setDT set is.data.table rbindlist
#' @importFrom data.table setnames setorderv := .SD .I
#' @importFrom utils install.packages capture.output assignInMyNamespace available.packages
#' @importFrom utils compareVersion installed.packages
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
#'                  libPaths = ProjectPackageFolder, standAlone = FALSE)
#'
#' # To keep totally isolated: use standAlone = TRUE
#' #   --> setting .libPaths() directly means standAlone is not necessary; it will only
#' #   use .libPaths()
#' library(Require)
#' ProjectPackageFolder <- file.path("~", "ProjectA")
#' .libPaths(ProjectPackageFolder)
#' Require("PredictiveEcology/SpaDES@development")
#'
#'
#' # If creating an iso
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
                    ...){

  doDeps <- if (!is.null(list(...)$dependencies)) list(...)$dependencies else NA
  which <- whichToDILES(doDeps)

  origPackagesHaveNames <- nchar(names(packages)) > 0
  if (any(origPackagesHaveNames))
    packages <- packages[order(names(packages), decreasing = TRUE)]
  packages <- packages[!duplicated(packages)] # (unique removes names) sometimes people pass identical packages -- only <10s microseconds
  packagesOrig <- packages
  if (!missing(packageVersionFile)) {
    packages <- data.table::fread(packageVersionFile)
    packages <- paste0(packages$instPkgs, " (==", packages$instVers, ")")
  }
  if (missing(libPaths))
    libPaths <- .libPaths()
  origLibPaths <- setLibPaths(libPaths, standAlone)
  on.exit({.libPaths(origLibPaths)}, add = TRUE)


  browser(expr = exists("._Require_1"))
  if (length(which) && isTRUE(install)) {
    packages <- getPkgDeps(packages, which = which, purge = purge)
  }
  pkgDT <- data.table(Package = extractPkgName(packages), packageFullName = c(packages))
  if (any(origPackagesHaveNames))
    pkgDT[packageFullName %in% packagesOrig[origPackagesHaveNames], Package := names(packagesOrig[origPackagesHaveNames])]

  installedPkgsCurrent <- installed.packages(noCache = isTRUE(purge))
  installedPkgsCurrent <- as.data.table(installedPkgsCurrent[, c("Package", "LibPath", "Version"), drop = FALSE])

  # Join installed with requested
  pkgDT <- installedPkgsCurrent[pkgDT, on = "Package"]
  pkgDT[, installed := !is.na(Version)]

  if (length(packages) && (isTRUE(install) || isTRUE(require))) {
    if (isTRUE(install)) {
      pkgDT <- parseGitHub(pkgDT)
      pkgDT <- getPkgVersions(pkgDT, install = install)
      if (NROW(pkgDT[correctVersion == FALSE | is.na(correctVersion)]))
        pkgDT <- getAvailable(pkgDT, purge = purge)
      pkgDT <- installFrom(pkgDT)

      if (any(!pkgDT$installed | NROW(pkgDT[correctVersion == FALSE]) > 0) && (isTRUE(install) || install == "force")) {
        pkgDT <- doInstalls(pkgDT, install_githubArgs = install_githubArgs,
                            install.packagesArgs = install.packagesArgs, ...)
      }
      pkgDT[Package %in% unique(extractPkgName(packagesOrig)), packagesRequired := TRUE]
      pkgDT[, toLoad := packagesRequired]
      # pkgDT[, toLoad := correctVersion == TRUE]
      pkgDT[toLoad == TRUE, toLoad := is.na(installFrom) | installFrom != "Fail"]
      packages <- pkgDT[toLoad == TRUE]$Package
    }
    names(packages) <- packages
    if (isTRUE(require)) {
      postLoad <- doLoading(packages, ...)
    } else {
      postLoad <- list(out = lapply(packages, function(x) FALSE), out2 = character())
    }
    out <- unlist(postLoad$out)
    if (is.null(out)) out <- character()
    pkgDT[, loaded := (pkgDT$Package %in% names(out) & toLoad == TRUE)]
    out <- pkgDT[packagesRequired == TRUE]$loaded
    names(out) <- pkgDT[packagesRequired == TRUE]$Package
  } else {
    out <- rep(FALSE, length(packages))
    names(out) <- packages
  }
  if (getOption("Require.verbose", FALSE)) {
    colsToKeep <- intersect(colsToKeep, colnames(pkgDT))
    pkgDT <- pkgDT[, ..colsToKeep]

    attr(out, "Require") <- pkgDT[]
  }
  return(out)
}

parseGitHub <- function(pkgDT) {
  if (!is.data.table(pkgDT))
    pkgDT <- data.table(Package = extractPkgName(pkgDT), packageFullName = c(pkgDT))
  pkgDT[, githubPkgName := extractPkgGitHub(packageFullName)]
  isGH <- !is.na(pkgDT$githubPkgName)
  if (is.null(pkgDT$repoLocation)) {
    pkgDT[isGH, repoLocation := "GitHub"]
    pkgDT[!isGH, repoLocation := "CRAN"]
  }

  if (any(pkgDT$repoLocation == "GitHub")) {
    isGitHub <- pkgDT$repoLocation == "GitHub"
    pkgDT[isGitHub, fullGit := trimVersionNumber(packageFullName)]
    pkgDT[isGitHub, Account := gsub("^(.*)/.*$", "\\1", fullGit)]
    pkgDT[isGitHub, RepoWBranch := gsub("^(.*)/(.*)@*.*$", "\\2", fullGit)]
    pkgDT[isGitHub, hasSubFolder := grepl("/", pkgDT[isGitHub]$Account)]
    if (any(pkgDT$hasSubFolder, na.rm = TRUE)) { # fix both Account and RepoWBranch
      hasSubFold <- pkgDT$hasSubFolder
      subFoldIndices <- seq_len(NROW(pkgDT[hasSubFold]))
      pkgDT[hasSubFold, Account := gsub("^(.*)/(.*)$", "\\1", Account)]
      pkgDT[hasSubFold, RepoWBranch := gsub(paste0("^",Account,"/"), "", fullGit), by = subFoldIndices]
      pkgDT[hasSubFold, GitSubFolder := strsplit(pkgDT[hasSubFold]$RepoWBranch, split = "/|@")[[1]][2],
            by = subFoldIndices]
      pkgDT[hasSubFold, RepoWBranch := gsub(paste0("/",GitSubFolder), "", RepoWBranch), by = subFoldIndices]
    }
    pkgDT[isGitHub, Repo := gsub("^(.*)@(.*)$", "\\1", RepoWBranch)]
    pkgDT[isGitHub, Branch := "master"]
    pkgDT[isGitHub & grepl("@", RepoWBranch), Branch := gsub("^.*@(.*)$", "\\1", RepoWBranch)]
    set(pkgDT, NULL, c("RepoWBranch", "fullGit"), NULL)
  }
  pkgDT[]
}

#' @importFrom data.table setorderv
getPkgVersions <- function(pkgDT, install) {
  if (!is.data.table(pkgDT))
    pkgDT <- data.table(Package = extractPkgName(pkgDT), packageFullName = c(pkgDT))

  pkgDT[, hasVersionSpec := grepl(.grepVersionNumber, packageFullName)]

  if (any(pkgDT$hasVersionSpec)) {
    # pkgDT <- pkgDT[hasVersionSpec == TRUE, versionSpec := gsub(grepExtractPkgs, "\\2", packageFullName)]
    pkgDT <- pkgDT[hasVersionSpec == TRUE, versionSpec := extractVersionNumber(packageFullName)]
    pkgDT[hasVersionSpec == TRUE, inequality := extractInequality(packageFullName)]


    # pkgDTNoMV[repoLocation == "GitHub", correctVersionAvailGH := TRUE]
    # setnames(pkgDTNoMV, old = colsToKeep, new = newColNames)
    pkgDT[hasVersionSpec == TRUE & grepl("<", inequality), versionSpec := as.character(min(package_version(versionSpec))),
          by = "Package"]

    setorderv(pkgDT, c("Package", "versionSpec"), order = -1L)

    # any duplicates with different minimum version number to be dealt with here --> only those with > in their inequality
    pkgDT[hasVersionSpec == TRUE, versionSpec := as.character(max(package_version(versionSpec))), by = "Package"]
    # pkgDT_maxVersion <- pkgDT[hasVersionSpec == TRUE & grepl(">", inequality),
    #                           list(versionSpec = as.character(max(package_version(versionSpec)))),
    #                           by = "Package"]
    # pkgDT <- rbindlist(list(pkgDT[hasVersionSpec == FALSE], pkgDT[pkgDT_maxVersion, on = c("Package", "versionSpec")]))
    setorderv(pkgDT, c("Package", "hasVersionSpec"), order = -1L)
    # pkgDT <- pkgDT[, .SD[1], by = c("Package", "inequality")]

    if ("Version" %in% colnames(pkgDT)) {
      pkgDT[!is.na(Version), compareVersion := .compareVersionV(Version, versionSpec)]
      pkgDT[!is.na(Version) & hasVersionSpec == TRUE, correctVersion := .evalV(.parseV(text = paste(compareVersion, inequality, "0")))]
      pkgDT[hasVersionSpec == FALSE, correctVersion := NA]
      # put FALSE at top of each package -- then take the first one, so we will know if all inequalities are satisfied
      setorderv(pkgDT, c("Package", "correctVersion"), order = 1L)
      pkgDT <- pkgDT[, .SD[1], by = c("Package")]
    }
  } else {
    pkgDT[ , correctVersion := NA]
  }
  if (isTRUE(install == "force")) {
    pkgDT[, correctVersion := FALSE]
  }
  pkgDT
}

#' @importFrom utils download.file tail
getAvailable <- function(pkgDT, purge = FALSE) {
  whNotCorrect <- pkgDT[, .I[hasVersionSpec == TRUE & (correctVersion == FALSE | is.na(correctVersion))]]
  if (NROW(whNotCorrect)) {

    notCorrectVersions <- pkgDT[whNotCorrect]

    # do CRAN first
    if (any(notCorrectVersions$repoLocation == "CRAN")) {
      cachedAvailablePackages <- if (!exists("cachedAvailablePackages", envir = .pkgEnv) || isTRUE(purge)) {
        cap <- available.packages()
        assign("cachedAvailablePackages", cap, envir = .pkgEnv)
        cap
      } else {
        .pkgEnv$cachedAvailablePackages
      }
      cachedAvailablePackages <- as.data.table(cachedAvailablePackages[, c("Package", "Version")])
      setnames(cachedAvailablePackages, "Version", "AvailableVersion")
      notCorrectVersions <- cachedAvailablePackages[notCorrectVersions, on = "Package"]
      notCorrectVersions[repoLocation != "GitHub" & is.na(AvailableVersion), AvailableVersion := "10000000"]
      notCorrectVersions[repoLocation != "GitHub",
                         compareVersionAvail := .compareVersionV(AvailableVersion, versionSpec)]
      notCorrectVersions[repoLocation != "GitHub",
                         correctVersionAvail :=
                           .evalV(.parseV(text = paste(compareVersionAvail, inequality, "0")))]
    }

    # do Older Versions

    needOlder <- notCorrectVersions$correctVersionAvail == FALSE & grepl("==|<=|<", notCorrectVersions$inequality)
    needOlderNotGH <- needOlder & notCorrectVersions$repoLocation != "GitHub"
    if (any(needOlderNotGH)) {
      if (FALSE) {
        oldAvailableVersions <- notCorrectVersions[repoLocation != "GitHub" & needOlder]
        oldAvailableVersions[, archiveSource := "CRANArchive"]
        oldAvailableVersions[, OlderVersionsAvailable := extractVersionNumber(packageFullName)] # Assuming it is available!!!
        oldAvailableVersions[, correctVersionAvail := TRUE]

      } else {
        oldAvailableVersions <- if (!exists("oldAvailableVersions", envir = .pkgEnv) || isTRUE(purge)) {
          pkg <- notCorrectVersions[repoLocation != "GitHub" & needOlder]$Package
          names(pkg) <- pkg
          ava <- lapply(pkg, function(p) {
                          as.data.table(archiveVersionsAvailable(p, repos = getCRANrepos()), keep.rownames = "PackageUrl")
                        })
          assign("oldAvailableVersions", ava, envir = .pkgEnv)
          ava
          # versions::available.versions(notCorrectVersions[repoLocation != "GitHub" & needOlder]$Package)
        } else {
          .pkgEnv$oldAvailableVersions
        }
        oldAvailableVersions <- rbindlist(oldAvailableVersions, idcol = "Package")
        # delete unwanted columns
        set(oldAvailableVersions, NULL, c("size", "isdir", "mode", "mtime",
                                          "ctime", "atime", "uid", "gid", "uname", "grname"),
            NULL)
        setDT(oldAvailableVersions)
        oldAvailableVersions[, OlderVersionsAvailable := gsub(".*_(.*)\\.tar\\.gz", "\\1", PackageUrl)]
        needOlderDT <- notCorrectVersions[needOlder & repoLocation != "GitHub"]
        oldAvailableVersions[, OlderVersionsAvailableCh := as.character(package_version(OlderVersionsAvailable))]

        oldAvailableVersions <- needOlderDT[oldAvailableVersions, on = c("Package"), roll = TRUE]
        oldAvailableVersions[, compareVersionAvail := .compareVersionV(OlderVersionsAvailableCh, versionSpec)]
        oldAvailableVersions[, correctVersionAvail :=
                               .evalV(.parseV(text = paste(compareVersionAvail, inequality, "0")))]
        if (any(oldAvailableVersions$correctVersionAvail)) {
          oldAvailableVersions[correctVersionAvail == TRUE, archiveSource := "CRANArchive"]
          oldAvailableVersions <- oldAvailableVersions[correctVersionAvail == TRUE & archiveSource == "CRANArchive"]
          oldAvailableVersions <- oldAvailableVersions[!is.na(archiveSource)]
          oldAvailableVersions[, repoLocation := archiveSource]
          setorderv(oldAvailableVersions, "OlderVersionsAvailableCh", order = -1L)
        }

        oldAvailableVersions <- oldAvailableVersions[, if (NROW(.SD) == 0) .SD else .SD[1], by = "Package"]
        set(oldAvailableVersions, NULL, c("OlderVersionsAvailableCh"), NULL)
      }
      notCorrectVersions <- rbindlist(list(notCorrectVersions[!(repoLocation != "GitHub" & needOlder)],
                                           oldAvailableVersions), fill = TRUE, use.names = TRUE)
    }
    # do GitHub second
    if (any(notCorrectVersions$repoLocation == "GitHub")) {
      babab <<- 1
      notCorrectVersions <- getGitHubDESCRIPTION(notCorrectVersions)
      notCorrectVersions[repoLocation == "GitHub", AvailableVersion := DESCRIPTIONFileVersion(DESCFile)]
      notCorrectVersions[repoLocation == "GitHub", compareVersionAvail := .compareVersionV(AvailableVersion, versionSpec)]
      notCorrectVersions[repoLocation == "GitHub", correctVersionAvail :=
                           .evalV(.parseV(text = paste(compareVersionAvail, inequality, "0")))]
      set(notCorrectVersions, NULL, c("url", "DESCFile"), NULL)

    }
    pkgDT <- rbindlist(list(pkgDT[!correctVersion == FALSE | hasVersionSpec == FALSE],
                            notCorrectVersions), fill = TRUE, use.names = TRUE)
  } else {
    pkgDT[, correctVersionAvail := NA]
  }

  pkgDT
}

installFrom <- function(pkgDT) {
  pkgDT <- pkgDT[correctVersion == FALSE | is.na(correctVersion) & installed == FALSE, needInstall := TRUE]
  if (NROW(pkgDT[needInstall == TRUE])) {
    pkgDT[needInstall == TRUE & # installed == FALSE &
            (correctVersionAvail == TRUE | is.na(correctVersionAvail)) & repoLocation == "CRAN",
          installFrom := repoLocation]
    pkgDT[needInstall == TRUE & # installed == FALSE &
            (correctVersionAvail == TRUE | is.na(correctVersionAvail)) & repoLocation == "GitHub",
          installFrom := repoLocation]
    pkgDT[needInstall == TRUE & # installed == FALSE &
            correctVersionAvail == FALSE,
          installFrom := "Fail"]
    if ("OlderVersionsAvailable" %in% colnames(pkgDT)) {
      pkgDT[needInstall == TRUE & # installed == FALSE &
              (correctVersionAvail == TRUE) &
              repoLocation == "CRANArchive", installFrom := "CRANArchive"]
      # pkgDT[needInstall == TRUE & # installed == FALSE &
      #         (correctVersionAvail == TRUE) &
      #         repoLocation == "Versions", installFrom := repoLocation]
    }
  } else {
    pkgDT[, installFrom := NA_character_]
  }

  pkgDT
}

DESCRIPTIONFileVersion <- function(file) {
  out <- lapply(file, function(f) {
    lines <- readLines(f);
    Sys.setlocale(locale = "C") # required to deal with non English characters in Author names
    on.exit(Sys.setlocale(locale = ""))
    vers_line <- lines[grep("^Version: *", lines)] # nolint
    gsub("Version: ", "", vers_line)
  })
  unlist(out)
}

setLibPaths <- function(libPaths = .libPaths(), standAlone = FALSE) {
  out <- unlist(lapply(libPaths, checkPath, create = TRUE))
  libPaths <- normalizePath(libPaths, winslash = "/") # the system call requires this

  origLibPaths <- .libPaths()
  if (standAlone) {
    .libPaths(c(libPaths, tail(.libPaths(), 1)))
  } else {
    if (!all(normPath(libPaths) %in% normPath(.libPaths())))
      .libPaths(c(libPaths, .libPaths()))
  }
  return(invisible(origLibPaths))
}

extractVersionNumber <- function(packageFullName) {
  gsub(grepExtractPkgs, "\\2", packageFullName)
}


#' @rdname extractPkgName
#' @export
extractInequality <- function(pkgs) {
  gsub(grepExtractPkgs, "\\1", pkgs)
}


#' @rdname extractPkgName
#' @export
extractPkgGitHub <- function(pkgs) {
  unlist(lapply(strsplit(trimVersionNumber(pkgs), split = "/|@"), function(x) x[2]))
  #sapply(strsplit(sapply(strsplit(pkgs, split = "/"),
  #                       function(x) x[2]), split = "@"), function(x) x[1])
}

#' Extract info from package character strings
#'
#' Cleans a character vector of non-package name related information (e.g., version)
#'
#' @param pkgs A character string vector of packages with or without GitHub path or versions
#' @return Just the package names without extraneous info.
#' @export
#' @examples
#' @rdname extractPkgName
#' extractPkgName("Require (>=0.0.1)")
extractPkgName <- function(pkgs) {
  pkgNames <- trimVersionNumber(pkgs)
  withGitName <- extractPkgGitHub(pkgNames)
  isNAWithGitName <- is.na(withGitName)
  if (any(!isNAWithGitName)) {
    stripped <- unlist(lapply(strsplit(pkgNames[!isNAWithGitName], split = "/|@"), function(x) x[2]))
    pkgNames[!isNAWithGitName] <- stripped
  }
  pkgNames
}

trimVersionNumber <- function(packages) {
  out <- gsub(.grepVersionNumber, "", packages)
  gsub("\n|\t", "", out)
}

.grepVersionNumber <- " *\\(.*"

grepExtractPkgs <- ".*\\((<*>*=*) *(.*)\\)"

.compareVersionV <- Vectorize(compareVersion)
.evalV <- Vectorize(eval, vectorize.args = "expr")
.parseV <- Vectorize(parse, vectorize.args = "text")


#'
#' @export
getGitHubPackageVersion <- function(pkgDT) {
  if (!is.data.table(pkgDT))
    pkgDT <- data.table(Package = extractPkgName(pkgDT), packageFullName = c(pkgDT))
  pkgDT <- getGitHubDESCRIPTION(pkgDT$packageFullName)
  pkgDT[repoLocation == "GitHub", AvailableVersion := DESCRIPTIONFileVersion(DESCFile),
        by = c("Package", "Branch")]
  pkgDT[]
}

getGitHubDESCRIPTION <- function(pkg) {
  pkgDT <- parseGitHub(pkg)
  names(pkg) <- extractPkgName(pkg)
  pkgDT[repoLocation == "GitHub",
        url := {
          if (hasSubFolder) {
            Branch <- paste0(Branch, "/", GitSubFolder)
          }
          file.path("https://raw.githubusercontent.com", Account,
                         Repo, Branch, "DESCRIPTION", fsep = "/")
        }, by = "Package"]

  checkPath(dirname(tempfile()), create = TRUE)
  pkgDT[repoLocation == "GitHub", {
    DESCFile := {
      destFile <- tempfile()
      download.file(url, destFile, overwrite = TRUE, quiet = TRUE) ## TODO: overwrite?
      destFile
    }
  }, by = c("Package", "Branch")]
  pkgDT[]
}

# getPkgVersions(pkgs1)

updateInstalled <- function(pkgDT, installPkgNames, warn) {
  if (missing(warn)) warn <- warnings()
  warnOut <- unlist(lapply(installPkgNames, function(ip) grepl(ip, names(warn))))
  if (any(!warnOut) | length(warnOut) == 0) {
    pkgDT[Package %in% installPkgNames, installed := TRUE]
  }
  pkgDT[]
}

doInstalls <- function(pkgDT, install_githubArgs, install.packagesArgs, ...) {
  toInstall <- pkgDT[installed == FALSE  | !correctVersion]
  if (any(!toInstall$installFrom %in% "Fail")) {
    if (any("CRAN" %in% toInstall$installFrom)) {
      installPkgNames <- toInstall[installFrom == "CRAN"]$Package
      names(installPkgNames) <- installPkgNames
      dots <- list(...)
      dots$dependencies <- FALSE

      out <- do.call(install.packages, append(append(list(installPkgNames), install.packagesArgs), dots))
      # out <- install.packages(installPkgNames)
      warn <- warnings()
      pkgDT <- updateInstalled(pkgDT, installPkgNames, warn)
      permDen <- grepl("Permission denied", names(warn))
      packagesDen <- gsub("^.*[\\/](.*).dll.*$", "\\1", names(warn))
      if (any(permDen)) {
        stopMess <- character()
        if (any(pkgDT[Package %in% packagesDen]$installFrom == "CRAN"))
          stopMess <- c(stopMess, paste0("Due to permission denied, you will have to restart R, and reinstall:\n",
                                         "------\n",
                                         #"install.packages(c('",paste(pkgs, collapse = ", "),"'), lib = '",libPaths[1],"')",
                                         "install.packages(c('", paste(packagesDen, collapse = "', '"), "'), lib = '",libPaths[1],"')"))
        if (any(pkgDT[Package %in% packagesDen]$installFrom == "GitHub"))
          stopMess <- c(stopMess, paste0("Due to permission denied, you will have to restart R, and reinstall:\n",
                                         "------\n", "remotes::install_github(c('",
                                         paste0(trimVersionNumber(pkgDT[Package %in% packagesDen]$packageFullName),
                                                collapse = "', '"), "'), lib = '",libPaths[1],"')"))
        stop(stopMess)

      }
    }
    if (any("GitHub" %in% toInstall$installFrom)) {
      gitPkgNames <- toInstall[installFrom == "GitHub"]
      out5 <- install_githubV(gitPkgNames, install_githubArgs = install_githubArgs, ...)
      pkgDT <- updateInstalled(pkgDT, gitPkgNames$Package, warnings())
    }
    # if (any("Versions" %in% toInstall$installFrom)) {
    #   installPkgNames <- toInstall[installFrom == "Versions"]$Package
    #   out <- versions::install.versions(toInstall[installFrom == "Versions"]$Package,
    #                                     versions = toInstall[installFrom == "Versions"]$OlderVersionsAvailable)
    #   pkgDT <- updateInstalled(pkgDT, installPkgNames, warnings())
    # }
    CRANArchive <- "CRANArchive"
    if (any(CRANArchive %in% toInstall$installFrom)) {
      message("installing older versions is still experimental and may cause package version conflicts")
      installPkgNames <- toInstall[installFrom == CRANArchive]$Package
      names(installPkgNames) <- installPkgNames
      installVersions <- toInstall[installFrom == CRANArchive]$OlderVersionsAvailable
      out <- Map(p = installPkgNames, v = installVersions, function(p, v, ...) {
        do.call(remotes::install_version, list(package = p, version = v, ...))
      })
      pkgDT <- updateInstalled(pkgDT, installPkgNames, warnings())
    }
  }
  if (NROW(pkgDT[installFrom == "Fail"])) {
    keepCols <- c("packageFullName", "installed", "correctVersion", "AvailableVersion")
    message("The following packages could not be installed because could not find a correct version")
    messageDF(pkgDT[installFrom == "Fail", ..keepCols])
    pkgDTFail <- pkgDT[installFrom == "Fail"]
  }
  pkgDT
}

doLoading <- function(toLoadPkgs, ...) {
  outMess <- capture.output(
    out <- lapply(toLoadPkgs, require, character.only = TRUE),
    type = "message")
  warn <- warnings()
  grep3a <- "no package called"
  grep3b <- "could not be found"
  missingDeps <- grepl(paste0(grep3a, "|", grep3b), outMess)

  grep2 <- "package or namespace load failed for"
  grep1 <- "onLoad failed in loadNamespace"
  otherErrors <- grepl(paste(grep1, "|", grep2), outMess)
  toInstall <- character()
  if (any(otherErrors) || any(missingDeps)) {
    if (any(otherErrors)) {
      error1 <- grepl(.libPaths()[1], outMess)
      error1Val <- gsub(paste0("^.*",.libPaths()[1], "\\/(.*)", "\\/.*$"), "\\1", outMess[error1])
      packageNames <- unique(unlist(lapply(strsplit(error1Val, "\\/"), function(x) x[[1]])))
      error2 <- grepl("no such symbol", outMess)
      if (any(error2)) {
        pkgs <- paste(packageNames, collapse = "', '")
        stop("Can't install ", pkgs, "; you will likely need to restart R and run:\n",
             "-----\n",
             "install.packages(c('",paste(pkgs, collapse = ", "),"'), lib = '",libPaths[1],"')",
             "\n-----\n...before any other packages get loaded")
      }
      error3 <- grepl("is being loaded, but", outMess)
      packageNames <- gsub(paste0("^.*namespace.{2,2}(.*)’ .*$"), "\\1", outMess[error3])
      if (any(error3)) {
        pkgs <- paste(packageNames, collapse = "', '")
        stop("Can't install ", pkgs, "; you will likely need to restart R and run:\n",
             "-----\n", "install.packages(c('", paste(pkgs,
                                                      collapse = ", "), "'), lib = '", .libPaths()[1],
             "')", "\n-----\n...before any other packages get loaded")
      }
    }
    doDeps <- if (!is.null(list(...)$dependencies)) list(...)$dependencies else TRUE
    if (any(missingDeps) && doDeps) {
      browser()
      grep3a_1 <- paste0(".*",grep3a,".{2}(.*).{1}")
      packageNames <- character()
      if (any(grepl(grep3a, outMess[missingDeps])))
        packageNames <- unique(gsub(grep3a_1, "\\1", outMess[missingDeps]))
      grep3b_1 <- paste0(".*package.{2}(.*).{2}required.*$")
      if (any(grepl(grep3b, outMess[missingDeps])))
        packageNames <- unique(c(packageNames,
                                 unique(gsub(grep3b_1, "\\1", outMess[missingDeps]))))
      toInstall <- c(toInstall, packageNames)
      outMess <- grep(grep2, outMess, value = TRUE, invert = TRUE)
      outMess <- grep(grep3a, outMess, value = TRUE, invert = TRUE)
      outMess <- grep(grep3b, outMess, value = TRUE, invert = TRUE)
    }

    out2 <- Require(unique(toInstall), ...)
    out2 <- unlist(out2)
    names(out2) <- unique(toInstall)

    out <- c(out, out2)
  } else {
    out2 <- character()
  }
  message(paste0(outMess, collapse = "\n"))
  list(out = out, out2 = out2)

}

archiveVersionsAvailable <- function(package, repos) {
  for (repo in repos) {
    if (length(repos) > 1)
      message("Trying ", repo)
    archive <- tryCatch({
      con <- gzcon(url(sprintf("%s/src/contrib/Meta/archive.rds",
                               repo), "rb"))
      on.exit(close(con))
      readRDS(con)
    }, warning = function(e) list(), error = function(e) list())
    info <- archive[[package]]
    if (!is.null(info)) {
      info$repo <- repo
      return(info)
    }
  }
  stop(sprintf("couldn't find package '%s'", package))
}

install_githubV <- function(gitPkgNames, install_githubArgs, ...) {
  dots <- list(...)
  dots$dependencies <- NA # This is NA, which under normal circumstances should be irrelevant
                          #  but there are weird cases where the internals of Require don't get correct
                          #  version of dependencies e.g., achubaty/amc@development says "reproducible" on CRAN
                          #  which has R.oo
  gitPkgs <- trimVersionNumber(gitPkgNames$packageFullName)
  names(gitPkgs) <- gitPkgs
  isTryError <- unlist(lapply(gitPkgs, is, "try-error"))
  attempts <- rep(0, length(gitPkgs))
  names(attempts) <- gitPkgs
  while(length(gitPkgs)) {
    gitPkgDeps2 <- gitPkgs[unlist(lapply(seq_along(gitPkgs), function(ind) {
      all(!extractPkgName(names(gitPkgs))[-ind] %in% extractPkgName(gitPkgs[[ind]]))
    }))]
    outRes <- lapply(gitPkgDeps2, function(p) {
      try(do.call(remotes::install_github, append(append(list(p),
                                                         install_githubArgs), dots))
      )})
    attempts[names(outRes)] <- attempts[names(outRes)] + 1
    if (any(attempts > 1)) {
      failedAttempts <- attempts[attempts > 1]
      outRes[attempts > 1] <- "Failed"
    }
    isTryError <- unlist(lapply(outRes, is, "try-error"))
    gitPkgs1 <- gitPkgs[!names(gitPkgs) %in% names(outRes)[!isTryError]]
    if (identical(gitPkgs1, gitPkgs)) {
      failedAttempts <- names(gitPkgs)
      gitPkgs <- character()
    }
    gitPkgs <- gitPkgs1
  }
  outRes

}

getPkgDeps <- function(packages, which, purge = getOption("Require.purge", FALSE)) {
  pkgs <- trimVersionNumber(packages)
  out1 <- pkgDep(pkgs, recursive = TRUE, which = which, purge = purge)
  out1 <- unique(unname(unlist(out1)))
  out2 <- c(out1, pkgs)
  out3 <- c(out1, packages)
  dt <- data.table(github = extractPkgGitHub(out2), Package = out2,
                   depOrOrig = c(rep("dep", length(out1)), rep("orig", length(pkgs))),
                   fullPackageName = out3)
  set(dt, NULL, "origOrder", seq_along(dt$github))
  dt[, bothDepAndOrig := length(depOrOrig) > 1, by = "Package"]
  dt[bothDepAndOrig == TRUE, depOrOrig := "both"]


  if ("github" %in% colnames(dt))
    setorderv(dt, na.last = TRUE, "github") # keep github packages up at top -- they take precedence
  haveVersion <- dt[Package != fullPackageName] # have no version number or are github
  haveNoVersion <- dt[Package == fullPackageName] # have no version number or are github
  dt <- rbindlist(list(haveVersion, haveNoVersion[!Package %in% haveVersion$Package][!duplicated(Package)]))
  # dt <- dt[!duplicated(Package)]
  setorderv(dt, "origOrder")
  # set(dt, NULL, c("github", "origOrder", "bothDepAndOrig"), NULL)
  dt$fullPackageName
}

colsToKeep <- c("Package", "loaded", "LibPath", "Version", "packageFullName",
                "installed", "repoLocation", "correctVersion", "correctVersionAvail",
                "toLoad", "hasVersionSpec")
