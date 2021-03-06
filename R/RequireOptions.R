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
#'       be cached here. Subsequent downloads of same package will use local copy. Default
#'       is to have packages not be cached locally so each install of the same version will
#'       be from the original source, e.g., CRAN, GitHub.
#'   }
#'   \item{\code{buildBinaries}}{
#'     Default: \code{TRUE}. Only relevant on *nix systems and if
#'     \code{getOption("Require.RPackageCache")} is set to a  path. If \code{TRUE}, then
#'        \code{Require} will pass \code{INSTALL_OPTS = "--build"}, meaning the
#'        package binary will be built and then saved in the
#'        \code{getOption("Require.RPackageCache")}. This means that subsequent installs
#'        of this package on this or identical system will be faster.
#'   }
#'   \item{\code{persistentPkgEnv}}{
#'     Default: \code{FALSE}. (ADVANCED USE) \code{Require} stashes a lot of information in a
#'     hidden environment, located at \code{Require:::.pkgEnv}. This gets reset at each
#'     restart of R and each reload of Require. To make the stashes more persistent, 
#'     set this option to \code{TRUE}. A file will be placed at 
#'     \code{file.path("~", "._Require_pkgEnv.rdata")}, which will be restored at package load
#'   }
#'   \item{\code{purge}}{
#'     Default: \code{FALSE}. If set to (almost) all internal caches used by \code{Require}
#'     will be deleted and rebuilt. This should not generally be necessary as it will 
#'     automatically be deleted after (by default) 1 hour (set via 
#'     \code{R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE} environment variable in seconds)
#'   }
#'   \item{\code{setupVerbose}}{
#'     Default: \code{TRUE}. Logical. Once \code{setup} is called, there are several important
#'     changes that are made to the user's experience. For beginners with \code{Require},
#'     the messages that are written are important to see. However, these can be turned off
#'     setting this to \code{FALSE}
#'   }
#'   \item{\code{unloadNamespaces}}{
#'     Default: \code{TRUE}. (ADVANCED USE) \code{Require} will attempt to detach and unload
#'     packages that conflict with the requested package installing via \code{Require}.
#'     This can be complicated, resulting in broken states that can only be recovered
#'     by restarting R. Default is to attempt to do this. \code{FALSE} will not attempt
#'     to do this. User must deal with inability to install packages due to package already 
#'     being loaded.
#'   }
#'   \item{\code{verbose}}{
#'     Default: \code{0}. During a \code{Require}, there is a lot of information collected 
#'     and used. With \code{verbose} set to \code{1} or \code{2}, more of this information
#'     will be reported as an attribute attached to the return object of \code{Require}.
#'     This may help diagnosing problems.
#'   }
#'   
#' }
#'
RequireOptions <- function() {
  list(Require.buildBinaries = TRUE,
       Require.persistentPkgEnv = FALSE, # TRUE
       Require.RPackageFolders = NULL, #"~/._RPackageCache", # nolint
       Require.RPackageCache = NULL, #"~/._RPackageCache", # nolint
       Require.setupVerbose = TRUE,
       Require.standAlone = TRUE, 
       Require.unloadNamespaces = TRUE,
       Require.updateRprofile = FALSE,
       # Require.useCranCache = NULL,
       Require.verbose = 0
  )
}

#   \item{\code{useCranCache}}{
#     Default: \code{NULL}. Experimental. If \code{TRUE}, a user can try 
#     to use the same cache folder as the crancache package (for binaries only). .
#   }
