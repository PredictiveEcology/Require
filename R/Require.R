if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".SD", ":=", "Account", "AvailableVersion", "Branch", "Package",
                           "Repo", "RepoWBranch", "Version", "compareVersionAvail", "correctVersion",
                           "correctVersionAvail", "download.file", "fullGit", "githubPkgName",
                           "hasVersionSpec", "inequality", "installed", "isGH", "packageFullName",
                           "versionSpec"))
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
#' @param packages Character vector of packages to install via
#'        \code{install.packages}, then load (i.e., with \code{library}). If it is
#'        one package, it can be unquoted (as in \code{require})
#' @param packageVersionFile If provided, then this will override all \code{install.package}
#'        calls with \code{versions::install.versions}
#' @param libPaths The library path where all packages should be installed, and looked for to load
#'        (i.e., call \code{library})
#' @param repos The remote repository (e.g., a CRAN mirror), passed to either
#'              \code{install.packages}, \code{install_github} or \code{installVersions}.
#' @param install_githubArgs List of optional named arguments, passed to \code{install_github}.
#' @param install.packagesArgs List of optional named arguments, passed to \code{install.packages}.
#' @param standAlone Logical. If \code{TRUE}, all packages will be installed and loaded strictly
#'                   from the \code{libPaths} only. If \code{FALSE}, all \code{.libPaths} will
#'                   be used to find the correct versions. This can be create dramatically faster
#'                   installs if the user has a substantial number of the packages already in their
#'                   personal library. In the case of \code{TRUE}, there will be a hidden file
#'                   place in the \code{libPaths} directory that lists all the packages
#'                   that were needed during the \code{Require} call. Default \code{FALSE} to
#'                   minimize package installing.
#' @export
#' @importFrom remotes install_github
#' @importFrom data.table data.table as.data.table setDT set is.data.table rbindlist
#' @importFrom data.table setnames setorderv := .SD
#' @importFrom utils install.packages capture.output assignInMyNamespace available.packages
#' @importFrom utils compareVersion installed.packages
#' @examples
#' \dontrun{
#' # simple usage, like conditional install.packages then library
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
                    repos = getOption("repos")){

  if (missing(libPaths))
    libPaths <- .libPaths()
  origLibPaths <- setLibPaths(libPaths, standAlone)
  on.exit({.libPaths(origLibPaths)}, add = TRUE)

  pkgDT <- data.table(Package = extractPkgName(packages), packageFullName = c(packages))

  pkgDT <- parseGitHub(pkgDT)
  pkgDT <- getPkgVersions(pkgDT)

  if (any(pkgDT$hasVersionSpec)) {
    installedPkgsCurrent <- installed.packages()
    installedPkgsCurrent <- as.data.table(installedPkgsCurrent[, c("Package", "LibPath", "Version"), drop = FALSE])

    # Join installed with requested
    pkgDT <- installedPkgsCurrent[pkgDT, on = "Package"]
    pkgDT[, installed := !is.na(Version)]
    if (any(!pkgDT$correctVersion))
      pkgDT <- getAvailable(pkgDT)
    pkgDT <- installFrom(pkgDT)
    if (any(!pkgDT$installed & pkgDT$correctVersionAvail)) {
      if (any(!pkgDT$isGH)) {
        out <- install.packages(pkgDT[isGH == FALSE]$Package)
      }
      if (any(pkgDT$isGH)) {
        out <- remotes::install_github(pkgDT[isGH == TRUE]$packageFullName)

      }
    }
  }

  out <- lapply(pkgDT$Package, require, character.only = TRUE)


}

parseGitHub <- function(pkgDT) {
  if (!is.data.table(pkgDT))
    pkgDT <- data.table(Package = extractPkgName(pkgDT), packageFullName = c(pkgDT))
  pkgDT[, githubPkgName := extractPkgGitHub(packageFullName)]
  pkgDT[, isGH := !is.na(githubPkgName)]
  if (any(pkgDT$isGH)) {
    isGitHub <- pkgDT$isGH == TRUE
    pkgDT[isGitHub, fullGit := trimVersionNumber(packageFullName)]
    pkgDT[isGitHub, Account := gsub("^(.*)/.*$", "\\1", fullGit)]
    pkgDT[isGitHub, RepoWBranch := gsub("^(.*)/(.*)@*.*$", "\\2", fullGit)]
    pkgDT[isGitHub, Repo := gsub("^(.*)@(.*)$", "\\1", RepoWBranch)]
    pkgDT[isGitHub, Branch := "master"]
    pkgDT[isGitHub & grepl("@", RepoWBranch), Branch := gsub("^.*@(.*)$", "\\1", RepoWBranch)]
    pkgDT[isGitHub, Package := githubPkgName]
    set(pkgDT, NULL, c("RepoWBranch", "fullGit"), NULL)
  }
  pkgDT
}

#' @importFrom data.table setorderv
getPkgVersions <- function(pkgDT) {
  if (!is.data.table(pkgDT))
    pkgDT <- data.table(Package = extractPkgName(pkgDT), packageFullName = c(pkgDT))

  pkgDT[, hasVersionSpec := grepl(.grepVersionNumber, packageFullName)]

  if (any(pkgDT$hasVersionSpec)) {
    pkgDT <- pkgDT[hasVersionSpec == TRUE, versionSpec := gsub(grepExtractPkgs, "\\2", packageFullName)]
    pkgDT[hasVersionSpec == TRUE, inequality := gsub(grepExtractPkgs, "\\1", packageFullName)]


    # pkgDTNoMV[isGH == TRUE, correctVersionAvailGH := TRUE]
    # setnames(pkgDTNoMV, old = colsToKeep, new = newColNames)

    # any duplicates with different minimum version number to be dealt with here
    pkgDT_maxVersion <- pkgDT[hasVersionSpec == TRUE, list(versionSpec = as.character(max(package_version(versionSpec)))),
                              by = "Package"]
    pkgDT <- rbindlist(list(pkgDT[hasVersionSpec == FALSE], pkgDT[ pkgDT_maxVersion, on = c("Package", "versionSpec")]))
    setorderv(pkgDT, c("Package", "hasVersionSpec"), order = -1L)
    pkgDT <- pkgDT[, .SD[1], by = "Package"]

    pkgDT[, compareVersion := .compareVersionV(Version, versionSpec)]
    pkgDT[hasVersionSpec == TRUE, correctVersion := .evalV(.parseV(text = paste(compareVersion, inequality, "0")))]
    pkgDT[hasVersionSpec == FALSE, correctVersion := TRUE]
  }
  pkgDT
}

#' @importFrom utils download.file tail
getAvailable <- function(pkgDT) {
  if (any(!pkgDT$correctVersion)) {#} && sum(notCorrectVersions$installed) > 0) {
    #if (!is.memoised(available.packagesMem)) {
    #  assignInMyNamespace("available.packagesMem", memoise(available.packages, ~timeout(360))) # nolint
    #}

    notCorrectVersions <- pkgDT[correctVersion == FALSE]

    # do CRAN first
    if (any(!notCorrectVersions$isGH)) {
      browser()
      #apm <- available.packagesMem()
      apm <- available.packages()
      apm <- as.data.table(apm[, c("Package", "Version")])
      setnames(apm, "Version", "AvailableVersion")
      notCorrectVersions <- apm[notCorrectVersions, on = "Package"]
      notCorrectVersions[isGH == FALSE,
                         compareVersionAvail := .compareVersionV(AvailableVersion, versionSpec)]
      notCorrectVersions[isGH == FALSE,
                         correctVersionAvail := .evalV(.parseV(text = paste(compareVersionAvail, inequality, "0")))]
    }

    # do Older Versions
    needOlder <- grepl("==|<=|<", notCorrectVersions$inequality)
    needOlderOnGH <- notCorrectVersions$needOlder & notCorrectVersions$isGH == FALSE
    if (any(needOlderOnGH)) {
      message("installing older versions is still experimental and may cause package version conflicts")
      out <- versions::available.versions(notCorrectVersions[isGH == FALSE & needOlder]$Package)
      out <- rbindlist(out, idcol = "Package")
      setDT(out)
      setnames(out, old = "version", new = "Version")
      needOlderDT <- notCorrectVersions[needOlder & isGH == FALSE]
      out1 <- out[needOlderDT, on = c("Package", "Version")]
      browser()
    }
    notCorrectVersions[isGH == FALSE,
                       correctVersionAvail := .evalV(.parseV(text = paste(compareVersionAvail, inequality, "0")))]
    # do GitHub second
    if (any(notCorrectVersions$isGH)) {
      notCorrectVersions[isGH == TRUE,
                         url := file.path("https://raw.githubusercontent.com", Account,
                                          Repo, Branch, "DESCRIPTION", fsep = "/")
                         ]
      # ua <- httr::user_agent(getOption("reproducible.useragent"))
      notCorrectVersions[isGH == TRUE, {
        AvailableVersion := {
          destFile <- tempfile()
          #suppressWarnings(
            #httr::GET(
          download.file(url, destFile, overwrite = TRUE, quiet = TRUE) ## TODO: overwrite?
          DESCRIPTIONFileVersion(destFile)
        }
      }, by = c("Package", "Branch")]

      notCorrectVersions[isGH == TRUE, compareVersionAvail := .compareVersionV(AvailableVersion, versionSpec)]
      notCorrectVersions[isGH == TRUE, correctVersionAvail :=
                           .evalV(.parseV(text = paste(compareVersionAvail, inequality, "0")))]
      set(notCorrectVersions, NULL, c("url"), NULL)

    }
  }
  pkgDT <- rbindlist(list(pkgDT[correctVersion == TRUE], notCorrectVersions), fill = TRUE, use.names = TRUE)

  pkgDT
}

installFrom <- function(pkgDT) {
  if (any(!pkgDT$correctVersion)) {
    if (NROW(pkgDT[correctVersionAvail == TRUE])) {
      pkgDT[installed == FALSE & correctVersionAvail == TRUE & isGH == FALSE, installFrom := "CRAN"]
      pkgDT[installed == FALSE & correctVersionAvail == TRUE & isGH == TRUE, installFrom := "GitHub"]
      pkgDT[installed == FALSE & correctVersionAvail == FALSE, installFrom := "Fail"]
    } else {
      pkgDT[, installFrom := NA_character_]
    }
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
    # browser()
    if (!all(normPath(libPaths) %in% normPath(.libPaths())))
      .libPaths(c(libPaths, .libPaths()))
  }
  return(invisible(origLibPaths))
}

extractPkgGitHub <- function(pkgs) {
  unlist(lapply(strsplit(trimVersionNumber(pkgs), split = "/|@"), function(x) x[2]))
  #sapply(strsplit(sapply(strsplit(pkgs, split = "/"),
  #                       function(x) x[2]), split = "@"), function(x) x[1])
}

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

