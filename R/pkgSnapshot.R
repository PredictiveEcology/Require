#' Take a snapshot of all the packages and version numbers
#'
#' This can be used later by \code{installVersions} to install or re-install the correct versions.
#'
#' @export
#' @param packageVersionFile A filename to save the packages and their currently
#'        installed version numbers. Defaults to \code{".packageVersions.txt"}.
#'        If this is specified to be \code{NULL}, the function will return the exact
#'        \code{Require} call needed to install all the packages at their current
#'        versions. This can be useful to add to a script to allow for reproducibility of
#'        a script.
#' @param libPaths The path to the local library where packages are installed.
#'        Defaults to the \code{.libPaths()[1]}.
#' @param exact Logical. If \code{TRUE}, the default, then for GitHub packages, it
#'        will install the exact SHA, rather than the head of the account/repo@branch. For
#'        CRAN packages, it will install the exact version. If \code{FALSE}, then GitHub
#'        packages will identify their branch if that had been specified upon installation, 
#'        not a SHA. If the package had been installed with reference to a SHA, then it
#'        will return the SHA as it does not know what branch it came from. 
#'        Similarly, CRAN packages will
#'        report their version and specify with a \code{>=}, allowing a subsequent user
#'        to install with a minimum version number, as opposed to an exact version number.
#' @details
#' A file is written with the package names and versions of all packages within \code{libPaths}.
#' This can later be passed to \code{Require}.
#'
#' @inheritParams Require
#' @importFrom utils write.table
#' @importFrom data.table fwrite
#' @examples
#' pkgSnapFile <- tempfile()
#' pkgSnapshot(pkgSnapFile, .libPaths()[1])
#' data.table::fread(pkgSnapFile)
#'
#' \dontrun{
#'
#' # An example to move this file to a new computer
#' library(Require)
#' setLibPaths(.libPaths()[1])  # this will only do a snapshot of the main user library
#' fileName <- "packageSnapshot.txt"
#' pkgSnapshot(fileName)
#' # Get file on another computer -- via email, slack, cloud, etc.
#' # library(googledrive)
#' # (out <- googledrive::drive_upload(fileName)) # copy the file id to clipboard
#' 
#' # On new machine 
#' fileName <- "packageSnapshot.txt"
#' library(Require)
#' # get the file from email, slack, cloud etc.
#' # library(googledrive)
#' # drive_download(as_id(PASTE-THE-FILE-ID-HERE), path = fileName)
#' setLibPaths("~/RPackages") # start with an empty folder for new 
#'                            # library to minimize package version conflicts
#' Require(packageVersionFile = fileName)
#' 
#' # Passing NULL --> results in output to console with exact Require call to 
#' #   achieve the packages installations
#' pkgSnapshot(NULL, libPaths = .libPaths()[1], exact = FALSE)
#' 
#' # Or shunt it to a file
#' sink("packages2.R")
#' pkgSnapshot(NULL, libPaths = .libPaths()[1])
#' sink()
#' 
#' # Will show "minimum package version"
#' pkgSnapshot(NULL, libPaths = .libPaths()[1], exact = FALSE)
#' }
#'
pkgSnapshot <- function(packageVersionFile = "packageVersions.txt", libPaths, standAlone = FALSE,
                        purge = getOption("Require.purge", FALSE), exact = TRUE) {
  if (missing(libPaths))
    libPaths <- .libPaths()
  origLibPaths <- suppressMessages(setLibPaths(libPaths, standAlone))
  on.exit({suppressMessages(setLibPaths(origLibPaths, standAlone = TRUE))}, add = TRUE)

  ip <- as.data.table(.installed.pkgs(lib.loc = libPaths, which = character(), other = "GitHubSha",
                                      purge = purge))
  if (is.null(packageVersionFile)) {
    # aa <- pkgDep("SpaDES", recursive = T)
    tmpPkgSnapshotFile <- ".tmppackageVersions.txt"
    on.exit({try(unlink(tmpPkgSnapshotFile), silent = TRUE)}, add = TRUE)
    co <- suppressMessages(pkgSnapshot(tmpPkgSnapshotFile, libPaths = libPaths, standAlone = standAlone,
                purge = purge))
    cc <- data.table::fread(tmpPkgSnapshotFile)
    # cc <- bb[bb$Package %in% extractPkgName(aa$SpaDES) & bb$LibPath == bb$LibPath[1],]
    if (isTRUE(exact)) {
      ref <- cc$GithubSHA1
      dd <- paste0(ifelse(!is.na(cc$GithubRepo), paste0(cc$GithubUsername, "/", cc$GithubRepo, "@", ref), 
                          paste0(cc$Package, " (==", cc$Version, ")")))
    } else {
      ref <- cc$GithubRef
      dd <- paste0(ifelse(!is.na(cc$GithubRepo), paste0(cc$GithubUsername, "/", cc$GithubRepo, "@", ref), 
                          paste0(cc$Package, " (>=", cc$Version, ")")))
    }
    ee <- paste0("Require(c('", paste(dd, collapse = "',\n'"), "'), require = FALSE, dependencies = FALSE, upgrades = FALSE)")
    cat(ee)
    # cat(ee, file = "packages.R")
    # source("packages.R")
  } else {
    fwrite(ip, file = packageVersionFile, row.names = FALSE, na = NA)
    message("package version file saved in ",packageVersionFile)    
  }

  return(invisible(ip))
}

