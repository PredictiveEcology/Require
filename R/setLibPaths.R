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
#' @inheritParams Require
#' @export
#' @examples
#' \dontrun{
#' setLibPaths("~/newProjectLib") # will only have 2 paths
#' setLibPaths("~/newProjectLib", standAlone = FALSE) # will have 2 or more paths
#'
#' }
setLibPaths <- function(libPaths, standAlone = TRUE) {
  oldLibPaths <- .libPaths()
  libPaths <- checkPath(normPath(libPaths), create = TRUE)#, mustWork = TRUE)

  #shim_fun <- .libPaths
  #shim_env <- new.env(parent = environment(shim_fun))
  if (isTRUE(standAlone)) {
    assign(".lib.loc", unique(c(libPaths, tail(.libPaths(), 1))),
           envir = environment(.libPaths))
    #shim_env$.Library <- tail(.libPaths(), 1)
  } else {
    assign(".lib.loc", unique(c(libPaths, .libPaths())),
           envir = environment(.libPaths))
    #shim_env$.Library <- .libPaths()
  }
  #shim_env$.Library.site <- character()

  #environment(shim_fun) <- shim_env
  #shim_fun(unique(libPaths))

  return(oldLibPaths)
}
