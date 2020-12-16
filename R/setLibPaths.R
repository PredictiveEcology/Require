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
#' @param RprofileToUpdate Logical or Character string. If \code{TRUE}, then
#'   this function will put several lines of code in the default \code{~/.Rprofile} 
#'   file setting up the package libraries for this and all future sessions. If
#'   a character string, then this should be the path to an \code{.Rprofile} file.
#'   To reset back 
#'   to normal, run \code{setLibPaths(RprofileToUpdate = TRUE)} without a libPath.
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
setLibPaths <- function(libPaths, standAlone = TRUE, RprofileToUpdate = NULL) {
  oldLibPaths <- .libPaths()
  if (missing(libPaths)) {
    return(checkMissingLibPaths(libPaths, RprofileToUpdate))
  } ## End missing
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
  message(".libPaths() is now: ", paste(.libPaths(), collapse = ", "))
  if (!is.null(RprofileToUpdate)) {
    setLibPathsUpdateRprofile(libPaths, standAlone, RprofileToUpdate)
  }
  return(invisible(oldLibPaths))
}

setLibPathsUpdateRprofile <- function(libPaths, standAlone = TRUE, RprofileToUpdate = NULL) {
  if (any(grepl("setLibPaths start", readLines("~/.Rprofile")))) {
    message("There is already a setLibPaths in the .Rprofile, skipping")
  } else {
    bodyFn <- format(body(Require::setLibPaths))
    lineWCheckPath <- grepl("checkPath.normPath", bodyFn)
    bodyFn[lineWCheckPath] <- "    if (!dir.exists(libPaths)) dir.create(libPaths)"
    lineWReturn <- grepl("return.*oldLibPaths", bodyFn)
    bodyFn <- bodyFn[!lineWReturn] 
    bodyFn <- gsub("tail", "utils::tail", bodyFn)
    bodyFn <- gsub("shim_env", ".shim_env", bodyFn)
    bodyFn <- gsub("shim_fun", ".shim_fun", bodyFn)
    bodyFn <- gsub("oldLibPaths", ".oldLibPaths", bodyFn)
    lineWMissing <- which(grepl("missing.libPaths", bodyFn))
    bodyFn <- bodyFn[-(lineWMissing:(lineWMissing+2))] 
    lineWRprofileToUpdate <- which(grepl("is\\.null\\(RprofileToUpdate\\)", bodyFn))
    bodyFn <- bodyFn[-(lineWRprofileToUpdate:(lineWRprofileToUpdate+2))] 
    bodyFn <- gsub("\\<standAlone\\>", "._standAlone", bodyFn)
    bodyFn <- gsub("\\.libPaths", "origDotlibPaths", bodyFn)
    bodyFn <- gsub("\\<libPaths\\>", "._libPaths", bodyFn)
    bodyFn <- gsub("origDotlibPaths", ".libPaths", bodyFn)
    bodyFn <- c("\n#### setLibPaths start #### DO NOT EDIT BETWEEN THESE LINES",
                "### DELETE THESE LINES BELOW TO RESTORE STANDARD R Package LIBRARY", 
                paste0("._libPaths <- '", libPaths, "'"), 
                paste0("._standAlone <- ", standAlone), 
                bodyFn, 
                resetRprofileMessage(RprofileToUpdate),
                "#### setLibPaths end ####")
    message("Updating ", RprofileToUpdate, "; this will set new libPaths for R packages even after restarting R")
    cat(bodyFn, file = "~/.Rprofile", append = TRUE, sep = "\n")
  }
  
}

checkMissingLibPaths <- function(libPaths, RprofileToUpdate = NULL) {
  if (!is.null(RprofileToUpdate)) {
    if (isTRUE(RprofileToUpdate)) RprofileToUpdate <- "~/.Rprofile"
    ll <- readLines(RprofileToUpdate)
    bounds <- which(grepl("#### setLibPaths", ll))
    if (length(bounds)) {
      message("removing custom libPaths in .Rprofile")
      if (identical("", ll[bounds[1] - 1])) {
        bounds[1] <- bounds[1] - 1
      }
      ll <- ll[-(bounds[1]:bounds[2])]
      writeLines(ll, con = RprofileToUpdate)
    } else {
      message("There was no custom libPaths setting in .Rprofile; nothing changed")
    }
    return(invisible())
  }
  stop("libPaths cannot be missing; please supply a folder to install R packages to")
}

resetRprofileMessage <- function(RprofileToUpdate = "~/.Rprofile") {
  paste0("message(\"To reset libPaths to default, type: \nRequire::setLibPaths(RprofileToUpdate = '",RprofileToUpdate,"')\") ")
}