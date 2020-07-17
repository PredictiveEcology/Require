#' Set \code{.libPaths}
#'
#' This will set the \code{.libPaths()} by either adding a new path to
#' it if \code{standAlone = FALSE}, or will concatenate
#' \code{c(libPath, tail(.libPaths(), 1))} if \code{standAlone = TRUE}.
#'
#' @details
#' This code was modified from \url{https://github.com/milesmcbain}.
#' A different, likely non-approved by CRAN approach that also works is here:
#' \url{https://stackoverflow.com/a/36873741/3890027}.
#'
#' @param libPaths A new path to append to, or replace all existing user
#'   components of \code{.libPath()}
#' @inheritParams Require
#' @return
#' The main point of this function is to set \code{.libPaths()}, which
#' will be changed as a side effect of this function.
#' As when setting \code{options}, this will return the previous state of
#' \code{.libPaths()} allowing the user to reset easily.
#'
#' @export
#' @examples
#' \dontrun{
#' orig <- setLibPaths("~/newProjectLib") # will only have 2 paths,
#'                                        # this and the last one in .libPaths()
#' .libPaths() # see the 2 paths
#' setLibPaths(orig) # reset
#' .libPaths() # see the 2 original paths back
#'
#' # will have 2 or more paths
#' setLibPaths("~/newProjectLib", standAlone = FALSE) # will have 2 or more paths
#'
#' }
setLibPaths <- function(libPaths, standAlone = TRUE) {
  oldLibPaths <- .libPaths()
  libPaths <- checkPath(normPath(libPaths), create = TRUE)#, mustWork = TRUE)

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
  return(oldLibPaths)
}
