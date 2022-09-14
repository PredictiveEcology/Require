#' `Require` options
#'
#' These provide top-level, powerful settings for a comprehensive
#' reproducible workflow.
#' See Details below.
#'
#' \describe{
#'   \item{`RequireOptions()`}{prints the default values of package options set at startup,
#'   which may have been changed (e.g., by the user) during the current session.}
#'   \item{`getRequireOptions()`}{prints the current values of package options.}
#' }
#'
#' @export
#' @details
#'
#' Below are options that can be set with `options("Require.xxx" = newValue)`,
#' where `xxx` is one of the values below, and `newValue` is a new value to
#' give the option. Sometimes these options can be placed in the user's `.Rprofile`
#' file so they persist between sessions.
#'
#' The following options are likely of interest to most users:
#' \describe{
#'   \item{`RPackageCache`}{
#'     Default: `getOptionRPackageCache()`, which must be either a path or a logical.
#'     This can be set using an environment
#'     variable e.g. `Sys.setenv(Require.RPackageCache = "somePath")`, or
#'     `Sys.setenv(Require.RPackageCache = "TRUE")`; if that is not
#'     set, then an either a path or logical option (`options(Require.RPackageCache = "somePath")`
#'     or `options(Require.RPackageCache = TRUE)`). If `TRUE`, the default
#'     folder location `RequirePkgCacheDir()` will be used.
#'     If this is `TRUE` or a path is provided,
#'     then binary and source packages will be cached here. Subsequent downloads
#'     of same package will use local copy.
#'     Default is to have packages not be cached locally so each install of the same version will
#'     be from the original source, e.g., CRAN, GitHub.
#'   }
#'   \item{`buildBinaries`}{
#'     Default: `TRUE`. Only relevant on *nix systems and if
#'     `getOption("Require.RPackageCache")` is set to a  path. If `TRUE` or a
#'        valid path, then
#'        `Require` will pass `INSTALL_OPTS = "--build"`, meaning the
#'        package binary will be built and then saved in the
#'        `getOption("Require.RPackageCache")`. This means that subsequent installs
#'        of this package on this or identical system will be faster.
#'   }
#'   \item{`persistentPkgEnv`}{
#'     Default: `FALSE`. (ADVANCED USE) `Require` stashes a lot of information in a
#'     hidden environment, located at `Require:::.pkgEnv`. This gets reset at each
#'     restart of R and each reload of Require. To make the stashes more persistent,
#'     set this option to `TRUE`. A file will be placed at
#'     `file.path("~", "._Require_pkgEnv.rdata")`, which will be restored at package load
#'   }
#'   \item{`purge`}{
#'     Default: `FALSE`. If set to (almost) all internal caches used by `Require`
#'     will be deleted and rebuilt. This should not generally be necessary as it will
#'     automatically be deleted after (by default) 1 hour (set via
#'     `R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE` environment variable in seconds)
#'   }
#'   \item{`setupVerbose`}{
#'     Default: `TRUE`. Logical. Once `setup` is called, there are several important
#'     changes that are made to the user's experience. For beginners with `Require`,
#'     the messages that are written are important to see. However, these can be turned off
#'     setting this to `FALSE`
#'   }
#'   \item{`unloadNamespaces`}{
#'     Default: `TRUE`. (ADVANCED USE) `Require` will attempt to detach and unload
#'     packages that conflict with the requested package installing via `Require`.
#'     This can be complicated, resulting in broken states that can only be recovered
#'     by restarting R. Default is to attempt to do this. `FALSE` will not attempt
#'     to do this. User must deal with inability to install packages due to package already
#'     being loaded.
#'   }
#'   \item{`verbose`}{
#'     Default: `0`. During a `Require`, there is a lot of information collected
#'     and used. With `verbose` set to `1` or `2`, more of this information
#'     will be reported as an attribute attached to the return object of `Require`.
#'     This may help diagnosing problems.
#'   }
#'
#' }
#'
#' @rdname RequireOptions
RequireOptions <- function() {
  list(Require.buildBinaries = TRUE,
       Require.persistentPkgEnv = FALSE, # TRUE
       Require.RPackageFolders = NULL,
       Require.RPackageCache = getOptionRPackageCache(),
       Require.standAlone = TRUE,
       Require.unloadNamespaces = FALSE,
       Require.updateRprofile = FALSE,
       # Require.useCranCache = NULL,
       Require.verbose = 0
  )
}

#' @export
#' @rdname RequireOptions
getRequireOptions <- function() {
  opts <- names(RequireOptions())
  vals <- lapply(opts, getOption)
  names(vals) <- opts

  return(invisible(vals))
}

# old code ------------------------------------------------------------------------------------

#   \item{\code{useCranCache}}{
#     Default: \code{NULL}. Experimental. If \code{TRUE}, a user can try
#     to use the same cache folder as the crancache package (for binaries only). .
#   }
