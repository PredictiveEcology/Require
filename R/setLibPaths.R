#' Set .libPaths
#'
#' This will set the .libPaths() by either adding a new path to
#' it if \code{standAlone = FALSE}, or will concatenate
#' \code{c(libPath, tail(.libPaths(), 1))} if \code{standAlone = TRUE}.
#'
#' @details
#' This code was taken from \url{https://milesmcbain.xyz/hacking-r-library-paths/}
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
