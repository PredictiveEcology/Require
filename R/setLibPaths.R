#' Set \code{.libPaths}
#'
#' This will set the \code{.libPaths()} by either adding a new path to
#' it if \code{standAlone = FALSE}, or will concatenate
#' \code{c(libPath, tail(.libPaths(), 1))} if \code{standAlone = TRUE}.
#'
#' @details
#' This was taken from https://stackoverflow.com/a/36873741/3890027 . Another
#' possible solution to this strange issue is here:
#' This code was taken from \url{https://milesmcbain.xyz/hacking-r-library-paths/}.
#' This 2nd one is currently not used as it appeared not to work on Travis
#'
#'
#' @param libPaths A new path to append to, or replace all existing user
#'   components of \code{.libPath()}
#' @param algo Numeric, either 1 or 2, indicating which underlying algorithm
#'   to use to set \code{.libPaths()}. \code{1} uses a defunct \code{.lib.loc}
#'   and \code{2} uses a shim environment. Default is \code{2}. Both appear to
#'   work on all systems tested.
#' @inheritParams Require
#' @return
#' As when setting \code{options}, this will return the previous state of
#' \code{.libPaths()} allowing the user to reset easily.
#' @export
#' @examples
#' \dontrun{
#' orig <- setLibPaths("~/newProjectLib") # will only have 2 paths
#' .libPaths() # see the 2 paths
#' setLibPaths(orig) # reset
#' .libPaths() # see the 2 original paths back
#'
#' # will have 2 or more paths
#' setLibPaths("~/newProjectLib", standAlone = FALSE) # will have 2 or more paths
#'
#' }
setLibPaths <- function(libPaths, standAlone = TRUE, algo = 2) {
  oldLibPaths <- .libPaths()
  libPaths <- checkPath(normPath(libPaths), create = TRUE)#, mustWork = TRUE)

  if (algo == 1) {
    if (isTRUE(standAlone)) {
      assign(".lib.loc", unique(c(libPaths, tail(.libPaths(), 1))),
             envir = environment(.libPaths))
    } else {
      assign(".lib.loc", unique(c(libPaths, .libPaths())),
             envir = environment(.libPaths))
    }
  } else if (algo == 2) {

    shim_fun <- .libPaths
    shim_env <- new.env(parent = environment(shim_fun))
    if (isTRUE(standAlone)) {
      shim_env$.Library <- tail(.libPaths(), 1)
    } else {
      shim_env$.Library <- .libPaths()
    }
    shim_env$.Library.site <- character()

    environment(shim_fun) <- shim_env
    shim_fun(unique(libPaths))
  }
  return(oldLibPaths)
}
