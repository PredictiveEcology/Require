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
#' }
#'
RequireOptions <- function() {
  list(Require.RPackageCache = "~/._RPackageCache" # nolint
  )
}
