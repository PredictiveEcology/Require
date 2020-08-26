#' \code{Require} options
#'
#' These provide top-level, powerful settings for a comprehensive
#' reproducible workflow. To see defaults, run \code{RequireOptions()}.
#' See Details below.
#'
#' @export
#' @details
#'
#' Below are options that can be set with \code{options("Require.xxx" = newValue)},
#' where \code{xxx} is one of the values below, and \code{newValue} is a new value to
#' give the option. Sometimes these options can be placed in the user's \code{.Rprofile}
#' file so they persist between sessions.
#'
#' The following options are likely of interest to most users:
#' \describe{
#'   \item{\code{RPackageCache}}{
#'     Default: \code{NULL}. If a folder is provided, then binary and source packages will
#'       be cached here. Subsequent downloads of same package will use local copy.
#'   }
#'   \item{\code{buildBinarieis}}{
#'     Default: \code{TRUE}. Only relevant on *nix systems and if
#'     \code{getOption("Require.RPackageCache")} is set to a  path. If \code{TRUE}, then
#'        \code{Require} will pass \code{INSTALL_OPTS = "--build"}, meaning the
#'        package binary will be built and then saved in the
#'        \code{getOption("Require.RPackageCache")}. This means that subsequent installs
#'        of this package on this or identical system will be faster.
#'   }
#'   \item{\code{persistentPkgEnv}}{
#'     Default: \code{FALSE}. (ADVANCED USE) Require stashes a lot of information in a
#'     hidden environment, located at \code{Require:::.pkgEnv}. This gets reset at each
#'     restart of R and each reload of Require. To make the stashes more persistent, 
#'     set this option to \code{TRUE}. A file will be placed at 
#'     \code{file.path("~", "._Require_pkgEnv.rdata")}, which will be restored at package load
#'   }

#' }
#'
RequireOptions <- function() {
  list(Require.RPackageCache = NULL, # "~/._RPackageCache" # nolint
       Require.buildBinaries = TRUE,
       Require.persistentPkgEnv = FALSE
  )
}
