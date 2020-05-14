if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".SD", ":=", "Account", "AvailableVersion", "Branch", "Package",
                           "Repo", "RepoWBranch", "Version", "compareVersionAvail", "correctVersion",
                           "correctVersionAvail", "download.file", "fullGit", "githubPkgName",
                           "hasVersionSpec", "inequality", "installed", "isGH", "packageFullName",
                           "versionSpec", "..colsToKeep", "..keepCols", ".I", "DESCFile", "OlderVersionsAvailable",
                           "OlderVersionsAvailableCh", "PackageUrl", "archiveSource", "isInteractive",
                           "libPaths", "needInstall", "pkgDepTopoSort", "repoLocation", "toLoad"))
}

#' Repeatability-safe install and load packages, optionally with specific versions
#'
#' This is an "all in one" function that will run \code{install.packages} for
#' CRAN packages, \code{remotes::install_github} for \url{https://github.com/} packages and
#' will install specific versions of each package if there is a
#' \code{packageVersionFile} supplied. Plus, when \code{packages} is provided as
#' a character vector, or a \code{packageVersionFile} is supplied, all package
#' dependencies will be first assessed for \code{unique(dependencies)} so the
#' same package is not installed multiple times. Finally \code{library} is
#' called on the \code{packages}. If packages are already installed
#' (\code{packages} supplied), and their version numbers are exact (when
#' \code{packageVersionFile} is supplied), then the "install" component will be
#' skipped very quickly with a message.
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
#' @note This function will use \code{memoise} internally to determine the
#'   dependencies of all \code{packages}. This will speed up subsequent calls to
#'   \code{Require} dramatically. However, it will not take into account version
#'   numbers for this memoised step. #If package versions are updated manually by
#'   #the user, then this cached element should be wiped, using \code{forget =
#'   #TRUE}.
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
#'        one package, it can be unquoted (as in \code{require})
#' @param packageVersionFile If provided, then this will override all \code{install.package}
#'        calls with \code{versions::install.versions}
#' @param libPaths The library path (or libraries) where all packages should be installed,
#'        and looked for to load (i.e., call \code{library}). This can be used to create
#'        isolated, stand alone package installations.
#' @param repos The remote repository (e.g., a CRAN mirror), passed to either
#'              \code{install.packages}, \code{install_github} or \code{installVersions}.
#' @param install_githubArgs List of optional named arguments, passed to \code{install_github}.
#' @param install.packagesArgs List of optional named arguments, passed to \code{install.packages}.
#' @param standAlone Logical. If \code{TRUE}, all packages will be installed to and loaded from
#'                   the \code{libPaths} only. If \code{FALSE}, then the full \code{.libPaths()} will
#'                   be used to find the correct versions. This can be create dramatically faster
#'                   installs if the user has a substantial number of the packages already in their
#'                   personal library. In the case of \code{TRUE}, there will be a hidden file
#'                   place in the \code{libPaths} directory that lists all the packages
#'                   that were needed during the \code{Require} call. Default \code{FALSE} to
#'                   minimize package installing.
#' @param ... Passed to \emph{all} of \code{install_github}, \code{install.packages}, and
#'   \code{remotes::install_version}, i.e., the function will error if all of these functions
#'   can not use the ... argument. Good candidates are e.g., \code{type}, \code{dependencies}. This
#'   can be used with \code{install_githubArgs} or \code{install.packageArgs} which give individual
#'   options for those 2 internal function calls.
#' @export
#' @importFrom remotes install_github install_version
#' @importFrom data.table data.table as.data.table setDT set is.data.table rbindlist
#' @importFrom data.table setnames setorderv := .SD
#' @importFrom utils install.packages capture.output assignInMyNamespace available.packages
#' @importFrom utils compareVersion installed.packages
#' @examples
#' \dontrun{
#' # simple usage, like conditional install.packages then library
#' library(Require)
#' Require("stats") # analogous to require(stats), but slower because it checks for
#'                  #   pkg dependencies, and installs them, if missing
#' tempPkgFolder <- file.path(tempdir(), "Packages")
#'
#' # use standAlone, means it will put it in libPaths, even if it already exists
#' #   in another local library (e.g., personal library)
#' Require("crayon", libPaths = tempPkgFolder, standAlone = TRUE)
#'
#' # make a package version snapshot
#' packageVersionFile <- file.path(tempPkgFolder, ".packageVersion.txt")
#' pkgSnapshot(libPaths=tempPkgFolder, packageVersionFile)
#'
#' # confirms that correct version is installed
#' Require("crayon", packageVersionFile = packageVersionFile)
#'
#' # Create mismatching versions -- desired version is older than current installed
#' # This will try to install the older version, overwriting the newer version
#' desiredVersion <- data.frame(instPkgs="crayon", instVers = "1.3.2", stringsAsFactors = FALSE)
#' write.table(file = packageVersionFile, desiredVersion, row.names = FALSE)
#' # won't work because newer crayon is loaded
#' Require("crayon", packageVersionFile = packageVersionFile)
#'
#' # unload it first
#' detach("package:crayon", unload = TRUE)
#'
#' # run again, this time, correct "older" version installs in place of newer one
#' Require("crayon", packageVersionFile = packageVersionFile)
#'
#' # Mutual dependencies, only installs once -- e.g., httr
#' Require(c("cranlogs", "covr"), libPaths = tempPkgFolder)
#' }
#'
Require <- function(packages, packageVersionFile,
                    libPaths, # nolint
                    install_githubArgs = list(),       # nolint
                    install.packagesArgs = list(),
                    standAlone = FALSE,      # nolint
                    install = getOption("Require.install", TRUE),
                    require = getOption("Require.require", TRUE),
                    repos = getOption("repos"),
                    ...){

  if (missing(libPaths))
    libPaths <- .libPaths()
  origLibPaths <- setLibPaths(libPaths, standAlone)
  on.exit({.libPaths(origLibPaths)}, add = TRUE)

  pkgDT <- data.table(Package = extractPkgName(packages), packageFullName = c(packages))

  if (length(packages)) {

    installedPkgsCurrent <- installed.packages(noCache = TRUE)
    installedPkgsCurrent <- as.data.table(installedPkgsCurrent[, c("Package", "LibPath", "Version"), drop = FALSE])

    # Join installed with requested
    pkgDT <- installedPkgsCurrent[pkgDT, on = "Package"]
    pkgDT[, installed := !is.na(Version)]

    pkgDT <- parseGitHub(pkgDT)
    pkgDT <- getPkgVersions(pkgDT, install = install)
    if (NROW(pkgDT[correctVersion == FALSE | is.na(correctVersion)]))
      pkgDT <- getAvailable(pkgDT)
    pkgDT <- installFrom(pkgDT)
    if (any(!pkgDT$installed | NROW(pkgDT[correctVersion == FALSE]) > 0) && (isTRUE(install) || install == "force")) {
      pkgDT <- doInstalls(pkgDT, install_githubArgs = install_githubArgs,
                          install.packagesArgs = install.packagesArgs, ...)
    }
    pkgDT[, toLoad := is.na(installFrom) | installFrom != "Fail"]

    toLoadPkgs <- pkgDT[toLoad == TRUE]$Package
    names(toLoadPkgs) <- toLoadPkgs
    if (isTRUE(require)) {
      postLoad <- doLoading(toLoadPkgs)
    } else {
      postLoad <- list(out = lapply(toLoadPkgs, function(x) FALSE), out2 = character())
    }
    loaded <- unlist(postLoad$out)
    if (is.null(loaded)) loaded <- character()
    pkgDTPlusDeps <- data.table(Package = names(postLoad$out), loaded = loaded)
    pkgDT <- pkgDTPlusDeps[pkgDT, on = "Package"]
    depInstalled <- pkgDTPlusDeps[!pkgDT, on = "Package"]
    pkgDT[, loaded := (loaded == TRUE & toLoad == TRUE)]
    # pkgDT[, installed := (loaded == TRUE)]
    out <- pkgDT$loaded
    names(out) <- pkgDT$Package
    if (any(!pkgDTPlusDeps$Package %in% pkgDT$Package)) {
      depDT <- attr(postLoad$out2, "Require")
      pkgDT <- rbindlist(list(pkgDT, depDT), use.names = TRUE, fill = TRUE)
    }
  } else {
    out <- logical()
  }
  if (getOption("Require.verbose", FALSE)) {
    colsToKeep <- c("Package", "loaded", "LibPath", "Version", "packageFullName",
                    "installed", "repoLocation", "correctVersion", "correctVersionAvail",
                    "toLoad", "hasVersionSpec")
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
  pkgDT[isGH, repoLocation := "GitHub"]
  pkgDT[!isGH, repoLocation := "CRAN"]

  if (any(pkgDT$repoLocation == "GitHub")) {
    isGitHub <- pkgDT$repoLocation == "GitHub"
    pkgDT[isGitHub, fullGit := trimVersionNumber(packageFullName)]
    pkgDT[isGitHub, Account := gsub("^(.*)/.*$", "\\1", fullGit)]
    pkgDT[isGitHub, RepoWBranch := gsub("^(.*)/(.*)@*.*$", "\\2", fullGit)]
    pkgDT[isGitHub, Repo := gsub("^(.*)@(.*)$", "\\1", RepoWBranch)]
    pkgDT[isGitHub, Branch := "master"]
    pkgDT[isGitHub & grepl("@", RepoWBranch), Branch := gsub("^.*@(.*)$", "\\1", RepoWBranch)]
    pkgDT[isGitHub, Package := githubPkgName]
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
    pkgDT[hasVersionSpec == TRUE & grepl(">", inequality), versionSpec := as.character(max(package_version(versionSpec))), by = "Package"]
    pkgDT_maxVersion <- pkgDT[hasVersionSpec == TRUE, list(versionSpec = as.character(max(package_version(versionSpec)))),
                              by = "Package"]
    pkgDT <- rbindlist(list(pkgDT[hasVersionSpec == FALSE], pkgDT[pkgDT_maxVersion, on = c("Package", "versionSpec")]))
    setorderv(pkgDT, c("Package", "hasVersionSpec"), order = -1L)
    pkgDT <- pkgDT[, .SD[1], by = c("Package", "inequality")]

    if ("Version" %in% colnames(pkgDT)) {
      pkgDT[!is.na(Version), compareVersion := .compareVersionV(Version, versionSpec)]
      pkgDT[!is.na(Version) & hasVersionSpec == TRUE, correctVersion := .evalV(.parseV(text = paste(compareVersion, inequality, "0")))]
      pkgDT[hasVersionSpec == FALSE, correctVersion := NA]
      pkgDT <- pkgDT[, .SD[1], by = "Package"]
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
getAvailable <- function(pkgDT) {
  whNotCorrect <- pkgDT[, .I[hasVersionSpec == TRUE & (correctVersion == FALSE | is.na(correctVersion))]]
  if (NROW(whNotCorrect)) {
    #if (!is.memoised(available.packagesMem)) {
    #  assignInMyNamespace("available.packagesMem", memoise(available.packages, ~timeout(360))) # nolint
    #}

    notCorrectVersions <- pkgDT[whNotCorrect]

    # do CRAN first
    if (any(notCorrectVersions$repoLocation == "CRAN")) {
      #cachedAvailablePackages <- available.packagesMem()
      cachedAvailablePackages <- if (!exists("cachedAvailablePackages", envir = .pkgEnv)) {
        available.packages()
      } else {
        .pkgEnv$cachedAvailablePackages
      }
      assign("cachedAvailablePackages", cachedAvailablePackages, envir = .pkgEnv)
      cachedAvailablePackages <- as.data.table(cachedAvailablePackages[, c("Package", "Version")])
      setnames(cachedAvailablePackages, "Version", "AvailableVersion")
      notCorrectVersions <- cachedAvailablePackages[notCorrectVersions, on = "Package"]
      notCorrectVersions[repoLocation != "GitHub",
                         compareVersionAvail := .compareVersionV(AvailableVersion, versionSpec)]
      notCorrectVersions[repoLocation != "GitHub",
                         correctVersionAvail := .evalV(.parseV(text = paste(compareVersionAvail, inequality, "0")))]
    }

    # do Older Versions
    needOlder <- grepl("==|<=|<", notCorrectVersions$inequality)
    needOlderNotGH <- needOlder & notCorrectVersions$repoLocation != "GitHub"
    if (any(needOlderNotGH)) {
      message("installing older versions is still experimental and may cause package version conflicts")
      if (FALSE) {
        oldAvailableVersions <- notCorrectVersions[repoLocation != "GitHub" & needOlder]
        oldAvailableVersions[, archiveSource := "CRANArchive"]
        oldAvailableVersions[, OlderVersionsAvailable := extractVersionNumber(packageFullName)] # Assuming it is available!!!
        oldAvailableVersions[, correctVersionAvail := TRUE]

      } else {
        oldAvailableVersions <- if (!exists("oldAvailableVersions", envir = .pkgEnv)) {
          pkg <- notCorrectVersions[repoLocation != "GitHub" & needOlder]$Package
          names(pkg) <- pkg
          ava <- lapply(pkg, function(p) {
                          as.data.table(archiveVersionsAvailable(p, repos = getCRANrepos()), keep.rownames = "PackageUrl")
                        })
          # versions::available.versions(notCorrectVersions[repoLocation != "GitHub" & needOlder]$Package)
        } else {
          .pkgEnv$oldAvailableVersions
        }
        assign("oldAvailableVersions", oldAvailableVersions, envir = .pkgEnv)
        oldAvailableVersions <- rbindlist(oldAvailableVersions, idcol = "Package")
        # delete unwanted columns
        set(oldAvailableVersions, NULL, c("size", "isdir", "mode", "mtime",
                                          "ctime", "atime", "uid", "gid", "uname", "grname"),
            NULL)
        setDT(oldAvailableVersions)
        oldAvailableVersions[, OlderVersionsAvailable := gsub(".*_(.*)\\.tar\\.gz", "\\1", PackageUrl)]
        needOlderDT <- notCorrectVersions[needOlder & repoLocation != "GitHub"]
        # oldAvailableVersions <- oldAvailableVersions[available == TRUE]
        oldAvailableVersions[, OlderVersionsAvailableCh := as.character(package_version(OlderVersionsAvailable))]

        oldAvailableVersions <- needOlderDT[oldAvailableVersions, on = c("Package"), roll = TRUE]
        oldAvailableVersions[, compareVersionAvail := .compareVersionV(OlderVersionsAvailableCh, versionSpec)]
        oldAvailableVersions[, correctVersionAvail :=
                               .evalV(.parseV(text = paste(compareVersionAvail, inequality, "0")))]
        if (!all(oldAvailableVersions$correctVersionAvail)) {
          oldAvailableVersions[correctVersionAvail == TRUE, archiveSource := "CRANArchive"]
          #oldAvailableVersions[is.na(archiveSource) & correctVersionAvail == TRUE, archiveSource := "CRANArchive", by = "Package"]
          oldAvailableVersions <- oldAvailableVersions[!is.na(archiveSource)]
          oldAvailableVersions[, repoLocation := archiveSource]
        }

        oldAvailableVersions <- oldAvailableVersions[, if (NROW(.SD) == 0) .SD else .SD[1], by = "Package"]
        set(oldAvailableVersions, NULL, c("OlderVersionsAvailableCh"), NULL)
      }
      notCorrectVersions <- rbindlist(list(notCorrectVersions[!(repoLocation != "GitHub" & needOlder)],
                                           oldAvailableVersions), fill = TRUE, use.names = TRUE)
    }
    #notCorrectVersions[repoLocation != "GitHub",
    #                   correctVersionAvail := .evalV(.parseV(text = paste(compareVersionAvail, inequality, "0")))]
    # do GitHub second
    if (any(notCorrectVersions$repoLocation == "GitHub")) {
      notCorrectVersions[repoLocation == "GitHub",
                         url := file.path("https://raw.githubusercontent.com", Account,
                                          Repo, Branch, "DESCRIPTION", fsep = "/")
                         ]
      # ua <- httr::user_agent(getOption("reproducible.useragent"))
      checkPath(dirname(tempfile()), create = TRUE)
      notCorrectVersions[repoLocation == "GitHub", {
        AvailableVersion := {
          destFile <- tempfile()
          #suppressWarnings(
            #httr::GET(
          download.file(url, destFile, overwrite = TRUE, quiet = TRUE) ## TODO: overwrite?
          DESCRIPTIONFileVersion(destFile)
        }
      }, by = c("Package", "Branch")]

      notCorrectVersions[repoLocation == "GitHub", compareVersionAvail := .compareVersionV(AvailableVersion, versionSpec)]
      notCorrectVersions[repoLocation == "GitHub", correctVersionAvail :=
                           .evalV(.parseV(text = paste(compareVersionAvail, inequality, "0")))]
      set(notCorrectVersions, NULL, c("url"), NULL)

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
  lines <- readLines(file);
  Sys.setlocale(locale = "C") # required to deal with non English characters in Author names
  on.exit(Sys.setlocale(locale = ""))
  vers_line <- lines[grep("^Version: *", lines)] # nolint
  gsub("Version: ", "", vers_line)
}

setLibPaths <- function(libPaths = .libPaths(), standAlone = FALSE) {
  out <- lapply(libPaths, checkPath, create = TRUE)
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
extractInequality <- function(packageFullName) {
  gsub(grepExtractPkgs, "\\1", packageFullName)
}


extractPkgGitHub <- function(pkgs) {
  unlist(lapply(strsplit(trimVersionNumber(pkgs), split = "/|@"), function(x) x[2]))
  #sapply(strsplit(sapply(strsplit(pkgs, split = "/"),
  #                       function(x) x[2]), split = "@"), function(x) x[1])
}

#' @export
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
  gsub(.grepVersionNumber, "", packages)
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
        url := file.path("https://raw.githubusercontent.com", Account,
                         Repo, Branch, "DESCRIPTION", fsep = "/")
        ]

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
      out <- do.call(install.packages, append(list(installPkgNames, ...), install.packagesArgs))
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
      installPkgNames <- toInstall[installFrom == "GitHub"]

      out <- do.call(remotes::install_github,
                     append(list(trimVersionNumber(installPkgNames$packageFullName), ...),
                          install_githubArgs))
      # out <- remotes::install_github(trimVersionNumber(installPkgNames$packageFullName))

      pkgDT <- updateInstalled(pkgDT, installPkgNames$Package, warnings())
    }
    # if (any("Versions" %in% toInstall$installFrom)) {
    #   installPkgNames <- toInstall[installFrom == "Versions"]$Package
    #   out <- versions::install.versions(toInstall[installFrom == "Versions"]$Package,
    #                                     versions = toInstall[installFrom == "Versions"]$OlderVersionsAvailable)
    #   pkgDT <- updateInstalled(pkgDT, installPkgNames, warnings())
    # }
    CRANArchive <- "CRANArchive"
    if (any(CRANArchive %in% toInstall$installFrom)) {
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

doLoading <- function(toLoadPkgs) {
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
    }
    if (any(missingDeps)) {
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

    out2 <- Require(unique(toInstall))
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
