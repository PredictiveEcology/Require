#' Set `.libPaths`
#'
#' This will set the `.libPaths()` by either adding a new path to it if
#' `standAlone = FALSE`, or will concatenate `c(libPath, tail(.libPaths(), 1))`
#' if `standAlone = TRUE`. Currently, the default is to make this new
#' `.libPaths()` "sticky", meaning it becomes associated with the current
#' directory even through a restart of R. It does this by adding and/updating
#' the \file{.Rprofile} file in the current directory. If this current directory
#' is a project, then the project will have the new `.libPaths()` associated
#' with it, even through an R restart.
#'
#' @details This details of this code were modified from
#'   <https://github.com/milesmcbain>. A different, likely non-approved by CRAN
#'   approach that also works is here:
#'   <https://stackoverflow.com/a/36873741/3890027>.
#'
#' @param libPaths A new path to append to, or replace all existing user
#'   components of `.libPath()`
#' @param updateRprofile Logical or Character string. If `TRUE`, then this
#'   function will put several lines of code in the current directory's
#'   `.Rprofile` file setting up the package libraries for this and future
#'   sessions. If a character string, then this should be the path to an
#'   `.Rprofile` file. To reset back to normal, run `setLibPaths()` without a
#'   `libPath`. Default: `getOption("Require.updateRprofile", FALSE)`, meaning
#'   `FALSE`, but it can be set with an option or within a single call.
#' @param exact Logical. This function will automatically append the R version
#'   number to the `libPaths` to maintain separate R package libraries for each
#'   R version on the system. There are some cases where this behaviour is not
#'   desirable. Set `exact` to `TRUE` to override this automatic appending and
#'   use the exact, unaltered `libPaths`. Default is `FALSE`
#' @inheritParams Require
#' @return The main point of this function is to set `.libPaths()`, which will
#'   be changed as a side effect of this function. As when setting `options`,
#'   this will return the previous state of `.libPaths()` allowing the user to
#'   reset easily.
#'
#' @export
#' @importFrom utils compareVersion
#' @inheritParams Require
#' @examples
#' \dontrun{
#' if (Require:::.runLongExamples()) {
#'   opts <- Require:::.setupExample()
#'   origDir <- setwd(tempdir())
#'   td <- tempdir()
#'   setLibPaths(td) # set a new R package library locally
#'   setLibPaths() # reset it to original
#'   setwd(origDir)
#'   # Using standAlone = FALSE means that newly installed packages
#'   #   will be installed
#'   #   in the new package library, but loading packages can come
#'   #   from any of the ones listed in .libPaths()
#'
#'   # will have 2 or more paths
#'   otherLib <- file.path(td, "newProjectLib")
#'   setLibPaths(otherLib, standAlone = FALSE)
#'   # Can restart R, and changes will stay
#'
#'   # remove the custom .libPaths()
#'   setLibPaths() # reset to previous; remove from .Rprofile
#'   # because libPath arg is empty
#'
#'   Require:::.cleanup(opts)
#'   unlink(otherLib, recursive = TRUE)
#' }
#' }
#'
setLibPaths <- function(libPaths, standAlone = TRUE,
                        updateRprofile = getOption("Require.updateRprofile", FALSE),
                        exact = FALSE, verbose = getOption("Require.verbose")) {
  oldLibPaths <- .libPaths()
  if (missing(libPaths)) {
    return(checkMissingLibPaths(libPaths, updateRprofile))
  } ## End missing
  libPaths <- checkLibPaths(libPaths, exact = exact)

  if (!is.null(updateRprofile)) {
    setLibPathsUpdateRprofile(libPaths, standAlone, updateRprofile)
  }
  gte4.1 <- isTRUE(getRversion() >= "4.1")
  if (gte4.1) { # now correct behaviour; remaining parts unnecessary
    ## to avoid triggering warning on R < 4.1
    do.call(.libPaths, list(
      new = libPaths,
      if (gte4.1) include.site <- !standAlone
    ))
    return(oldLibPaths)
  }

  # libPaths <- checkPath(normPath(libPaths), create = TRUE)#, mustWork = TRUE)
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
  return(invisible(oldLibPaths))
}

setLibPathsUpdateRprofile <- function(libPaths, standAlone = TRUE, updateRprofile = NULL,
                                      verbose = getOption("Require.verbose")) {
  updateRprofile <- checkTRUERprofile(updateRprofile)
  if (is.character(updateRprofile)) {
    newFile <- FALSE
    if (!file.exists(updateRprofile)) {
      newFile <- TRUE
      file.create(updateRprofile)
      if (file.exists(".gitignore")) {
        if (!isTRUE(any(grepl(".Rprofile", readLines(".gitignore"))))) {
          cat(".Rprofile\n", file = ".gitignore", append = TRUE)
        }
      }
    }
    if (any(grepl(setLibPathsStartText, readLines(".Rprofile")))) {
      messageVerbose(alreadyInRprofileMessage, verbose = verbose, verboseLevel = 1)
    } else {
      bodyFn <- format(body(Require::setLibPaths))
      lineWCheckPath <- grepl("checkPath.normPath|checkLibPaths", bodyFn)
      bodyFn[lineWCheckPath] <- "    if (!dir.exists(libPaths[1])) dir.create(libPaths[1], recursive = TRUE)"
      lineWReturn <- grepl("return.*oldLibPaths", bodyFn)
      bodyFn <- bodyFn[!lineWReturn]
      bodyFn <- gsub("tail", "utils::tail", bodyFn)
      bodyFn <- gsub("shim_env", ".shim_env", bodyFn)
      bodyFn <- gsub("shim_fun", ".shim_fun", bodyFn)
      bodyFn <- gsub("oldLibPaths", ".oldLibPaths", bodyFn)
      lineWMissing <- which(grepl("missing.libPaths", bodyFn))
      bodyFn <- bodyFn[-(lineWMissing:(lineWMissing + 2))]
      lineWRprofileToUpdate <- which(grepl("is\\.null\\(updateRprofile\\)", bodyFn))
      bodyFn <- bodyFn[-(lineWRprofileToUpdate:(lineWRprofileToUpdate + 2))]
      bodyFn <- gsub("\\<standAlone\\>", "._standAlone", bodyFn)
      bodyFn <- gsub("\\.libPaths", "origDotlibPaths", bodyFn)
      bodyFn <- gsub("\\<libPaths\\>", "._libPaths", bodyFn)
      bodyFn <- gsub("origDotlibPaths", ".libPaths", bodyFn)
      bodyFn <- c(
        paste0(
          "\n", setLibPathsStartText, " #### ", newFileTrigger, newFile,
          " # DO NOT EDIT BETWEEN THESE LINES"
        ),
        "### DELETE THESE LINES BELOW TO RESTORE STANDARD R Package LIBRARY",
        paste0("### ", prevLibPathsText, paste(.libPaths(), collapse = ", ")),
        paste0("._libPaths <- c('", paste(libPaths, collapse = "', '"), "')"),
        paste0("._standAlone <- ", standAlone),
        bodyFn,
        if (verbose >= 1) {
          resetRprofileMessage(updateRprofile)
        },
        paste0(commentCharsForSetLibPaths, "end ####")
      )
      messageVerbose("Updating ", updateRprofile,
        "; this will set new libPaths for R packages even after restarting R",
        verbose = verbose, verboseLevel = 1
      )
      cat(bodyFn, file = ".Rprofile", append = TRUE, sep = "\n")
    }
  }
}

checkMissingLibPaths <- function(libPaths, updateRprofile = NULL, verbose = getOption("Require.verbose")) {
  if (!is.null(updateRprofile)) {
    if (updateRprofile == FALSE && missing(libPaths)) {
      updateRprofile <- TRUE
    }
    updateRprofile <- checkTRUERprofile(updateRprofile)
    noChange <- FALSE
    if (is.character(updateRprofile)) {
      if (file.exists(updateRprofile)) {
        ll <- readLines(updateRprofile)
        bounds <- which(grepl("#### setLibPaths", ll))
        if (length(bounds)) {
          messageVerbose("removing custom libPaths in .Rprofile",
            verbose = verbose, verboseLevel = 1
          )
          if (identical("", ll[bounds[1] - 1])) {
            bounds[1] <- bounds[1] - 1
          }
          newFileLine <- grepl(newFileTrigger, ll)
          newFile <- gsub(paste0(".*", newFileTrigger, "([[:alpha:]]+) .*"), "\\1", ll[newFileLine])
          wasNew <- as.logical(newFile)
          prevLines <- grepl(prevLibPathsText, ll)
          prevLibPaths <- strsplit(gsub(paste0(".*", prevLibPathsText), "", ll[prevLines]), split = ", ")[[1]]
          .libPaths(prevLibPaths)

          # needs to be NEW and starts on 2nd line
          if (isTRUE(wasNew) && which(newFileLine) == 2) {
            file.remove(updateRprofile)
          } else {
            ll <- ll[-(bounds[1]:bounds[2])]
            writeLines(ll, con = updateRprofile)
          }
        } else {
          noChange <- TRUE
        }
      } else {
        noChange <- TRUE
      }
      if (isTRUE(noChange)) {
        messageVerbose("There was no custom libPaths setting in .Rprofile; nothing changed",
          verbose = verbose, verboseLevel = 0
        )
      }

      return(invisible())
    }
  }
  stop("libPaths cannot be missing; please supply a folder to install R packages to")
  return(invisible())
}

resetRprofileMessage <- function(updateRprofile = ".Rprofile") {
  paste0("message(\"To reset libPaths to previous state, run: Require::setupOff() (or delete section in .Rprofile file)\") ")
}

checkTRUERprofile <- function(updateRprofile) {
  if (isTRUE(updateRprofile)) updateRprofile <- ".Rprofile"
  updateRprofile
}

prevLibPathsText <- "Previous .libPaths: "
commentCharsForSetLibPaths <- "#### setLibPaths "
setLibPathsStartText <- paste0(commentCharsForSetLibPaths, "start")
newFileTrigger <- "New File:"
alreadyInRprofileMessage <- "There is already a setLibPaths in the .Rprofile, skipping"
