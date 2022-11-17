#' Take a snapshot of all the packages and version numbers
#'
#' This can be used later by `Require` to install or re-install the correct versions. See examples.
#'
#' @details
#' A file is written with the package names and versions of all packages within `libPaths`.
#' This can later be passed to `Require`.
#'
#' `pkgSnapshot2` returns a vector of package names and versions, with no file output. See
#' examples.
#'
#' @return
#' Will both write a file, and (invisibly) return a vector of packages with the
#' version numbers. This vector can be used directly in `Require`, though it should likely
#' be used with `require = FALSE` to prevent attaching all the packages.
#'
#' @param packageVersionFile A filename to save the packages and their currently
#'        installed version numbers. Defaults to `"packageVersions.txt"`.
#'        If this is specified to be `NULL`, the function will return the exact
#'        `Require` call needed to install all the packages at their current
#'        versions. This can be useful to add to a script to allow for reproducibility of
#'        a script.
#' @param libPaths The path to the local library where packages are installed.
#'        Defaults to the `.libPaths()[1]`.
#' @param exact Logical. If `TRUE`, the default, then for GitHub packages, it
#'        will install the exact SHA, rather than the head of the `account/repo@branch`. For
#'        CRAN packages, it will install the exact version. If `FALSE`, then GitHub
#'        packages will identify their branch if that had been specified upon installation,
#'        not a SHA. If the package had been installed with reference to a SHA, then it
#'        will return the SHA as it does not know what branch it came from.
#'        Similarly, CRAN packages will report their version and specify with a `>=`,
#'        allowing a subsequent user
#'        to install with a minimum version number, as opposed to an exact version number.
#'
#' @export
#' @inheritParams Require
#' @inheritParams pkgDep
#' @importFrom utils write.table
#' @importFrom data.table fwrite
#' @examples
#' \dontrun{
#' if (Require:::.runLongExamples()) {
#'   opts <- Require:::.setupExample()
#'   # install one archived version so that below does something interesting
#'
#'   libForThisEx <- tempdir2("Example")
#'   Require("crayon (==1.5.1)", libPaths = libForThisEx, require = FALSE)
#'   # Normal use -- using the libForThisEx for example;
#'   #    normally libPaths would be omitted to get all
#'   #    packages in user or project library
#'   tf <- tempfile()
#'
#'   # writes to getOption("Require.packageVersionFile")
#'   # within project; also returns a vector
#'   # of packages with version
#'   pkgs <- pkgSnapshot(
#'     packageVersionFile = tf,
#'     libPaths = libForThisEx
#'   )
#'
#'   # Now move this file to another computer e.g. by committing in git,
#'   #   emailing, googledrive
#'   #   on next computer/project
#'   Require(packageVersionFile = tf, libPaths = libForThisEx)
#'
#'   # Using pkgSnapshot2 to get the vector of packages and versions
#'
#'   tf <- tempfile()
#'   pkgs <- pkgSnapshot2(
#'     packageVersionFile = tf,
#'     libPaths = libForThisEx
#'   )
#'   Require(pkgs, require = FALSE) # will install packages from previous line
#'   # (likely want require = FALSE
#'   #  and not load them all)
#'
#'   Require:::.cleanup(opts)
#'   unlink(getOption("Require.packageVersionFile"))
#' }
#' }
#'
#' @rdname pkgSnapshot
pkgSnapshot <-
  function(packageVersionFile = getOption("Require.packageVersionFile"),
           libPaths = .libPaths(),
           standAlone = FALSE,
           purge = getOption("Require.purge", FALSE),
           exact = TRUE,
           includeBase = FALSE,
           verbose = getOption("Require.verbose")) {
    libPaths <- checkLibPaths(libPaths = libPaths)
    libPaths <- doLibPaths(libPaths, standAlone)

    ip <- doInstalledPackages(libPaths, purge, includeBase)

    fwrite(ip,
      file = packageVersionFile,
      row.names = FALSE,
      na = NA
    )
    messageVerbose(
      "package version file saved in ",
      packageVersionFile,
      verbose = verbose,
      verboseLevel = 1
    )

    return(invisible(ip))
  }

#' @rdname pkgSnapshot
#' @export
pkgSnapshot2 <-
  function(packageVersionFile = getOption("Require.packageVersionFile"),
           libPaths,
           standAlone = FALSE,
           purge = getOption("Require.purge", FALSE),
           exact = TRUE,
           includeBase = FALSE,
           verbose = getOption("Require.verbose")) {
    libPaths <- doLibPaths(libPaths, standAlone)

    ip <- doInstalledPackages(libPaths, purge, includeBase)

    if (isTRUE(exact)) {
      ref <- ip$GithubSHA1
      ineq <- "=="
    } else {
      ref <- ip$GithubRef
      ineq <- ">="
    }
    thePkgAndVers <- paste0(ifelse(
      !is.na(ip$GithubRepo),
      paste0(ip$GithubUsername, "/", ip$GithubRepo, "@", ref),
      # github
      paste0(ip$Package, " (", ineq, ip$Version, ")") # cran
    ))
    thePkgAndVers
  }


doLibPaths <- function(libPaths, standAlone) {
  if (missing(libPaths)) {
    libPaths <- .libPaths()
  }
  if (isTRUE(standAlone)) {
    libPaths <- libPaths[1]
  }
  libPaths
}

doInstalledPackages <- function(libPaths, purge, includeBase) {
  ip <-
    as.data.table(
      .installed.pkgs(
        lib.loc = libPaths,
        which = character(),
        other = "GitHubSha",
        purge = purge
      )
    )
  if (isFALSE(includeBase)) {
    ip <- ip[!Package %in% .basePkgs]
  }

  ip
}
