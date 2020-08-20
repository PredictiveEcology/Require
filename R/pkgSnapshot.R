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
pkgSnapshot <- function(packageVersionFile = "packageVersions.txt", libPaths, standAlone = FALSE) {
  browser(expr = exists("aaaa"))
  if (missing(libPaths))
    libPaths <- .libPaths()
  origLibPaths <- setLibPaths(libPaths, standAlone)
  on.exit({.libPaths(origLibPaths)}, add = TRUE)

  ip <- as.data.table(.installed.pkgs(lib.loc = libPaths, which = character(), other = "GitHubSha"))
  # browser()
  # instPkgs <- ip$Package
  # instVers <- ip$Version
  # GitHub <- ip
  # names(instPkgs) <- instPkgs
  # names(instVers) <- instPkgs
  # names(instShas) <- instPkgs
  # df <- data.frame(names(instVers), unlist(instVers), stringsAsFactors = FALSE, row.names = NULL)
  # colnames(df) <- c("instPkgs", "instVers")
  # if (sum(nchar(instShas)) > 0)
  #   df <- data.frame(df, "instShas" = unlist(instShas))
  fwrite(ip, file = packageVersionFile, row.names = FALSE)
  message("package version file saved in ",packageVersionFile)
  return(invisible(ip))
}

