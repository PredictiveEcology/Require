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
#'   \item{\code{ask}}{
#'     Default: \code{TRUE}. Used in \code{\link{clearCache}} and \code{\link{keepCache}}.
#'   }
#' }
#'
#' @section Advanced:
#' The following options are likely not needed by a user.
#' \describe{
#'   \item{\code{cloudChecksumsFilename}}{
#'     Default: \code{file.path(dirname(.reproducibleTempCacheDir()), "checksums.rds")}.
#'     Used in \code{\link{cloudCache}}
#'   }
#' }
RequireOptions <- function() {
  list(Require.RPackageCache = "~/._RPackageCache" # nolint
  )
}
