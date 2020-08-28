#' Take a snapshot of all the packages and version numbers
#'
#' This can be used later by \code{installVersions} to install or re-install the correct versions.
#'
#' @export
#' @param packageVersionFile A filename to save the packages and their currently
#'        installed version numbers. Defaults to \code{".packageVersions.txt"}.
#' @param libPaths The path to the local library where packages are installed.
#'        Defaults to the \code{.libPaths()[1]}.
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
#' }
#'
pkgSnapshot <- function(packageVersionFile = "packageVersions.txt", libPaths, standAlone = FALSE) {
  browser(expr = exists("aaaa"))
  if (missing(libPaths))
    libPaths <- .libPaths()
  origLibPaths <- suppressMessages(setLibPaths(libPaths, standAlone))
  on.exit({suppressMessages(setLibPaths(origLibPaths, standAlone = TRUE))}, add = TRUE)

  ip <- as.data.table(.installed.pkgs(lib.loc = libPaths, which = character(), other = "GitHubSha"))
  fwrite(ip, file = packageVersionFile, row.names = FALSE, na = NA)
  message("package version file saved in ",packageVersionFile)
  return(invisible(ip))
}

