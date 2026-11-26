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
#' @examplesIf Require:::.runLongExamples()
#' opts <- Require:::.setupExample()
#' origDir <- setwd(tempdir())
#' td <- tempdir()
#' setLibPaths(td) # set a new R package library locally
#' setLibPaths() # reset it to original
#' setwd(origDir)
#' # Using standAlone = FALSE means that newly installed packages
#' #   will be installed
#' #   in the new package library, but loading packages can come
#' #   from any of the ones listed in .libPaths()
#'
#' # will have 2 or more paths
#' otherLib <- file.path(td, "newProjectLib")
#' setLibPaths(otherLib, standAlone = FALSE)
#' # Can restart R, and changes will stay
#'
#' # remove the custom .libPaths()
#' setLibPaths() # reset to previous; remove from .Rprofile
#' # because libPath arg is empty
#'
#' Require:::.cleanup(opts)
#' unlink(otherLib, recursive = TRUE)
#'
## The code that setLibPathsUpdateRprofile() writes into .Rprofile (setLibPathsBlockCode) does what
## the body below does; change one and change the other.
setLibPaths <- function(libPaths, standAlone = TRUE,
                        updateRprofile = getOption("Require.updateRprofile", FALSE),
                        exact = FALSE, verbose = getOption("Require.verbose")) {
  oldLibPaths <- .libPaths()
  if (missing(libPaths)) {
    return(checkMissingLibPaths(libPaths, updateRprofile))
  } ## End missing
  libPaths <- checkLibPaths(libPaths, exact = exact)

  if (!is.null(updateRprofile)) {
    setLibPathsUpdateRprofile(libPaths, standAlone, updateRprofile, verbose = verbose)
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
    libPathsLine <- paste0("._libPaths <- c('", paste(libPaths, collapse = "', '"), "')")
    standAloneLine <- paste0("._standAlone <- ", standAlone)
    old <- removeSetLibPathsBlock(updateRprofile)
    prevLibPathsLine <- NULL
    sameBlock <- FALSE
    if (old$found) {
      sameBlock <- libPathsLine %in% old$block && standAloneLine %in% old$block
      if (!sameBlock) {
        ## the project's library changed (e.g. a new folder or R version): replace the old block,
        ## keeping what it recorded as the libPaths to go back to
        prevLibPathsLine <- old$prevLibPathsLine
        newFile <- newFile || old$newFile
        writeLines(old$lines, con = updateRprofile)
        messageVerbose("The .Rprofile already had a different setLibPaths; replacing it with ",
                       paste(libPaths, collapse = ", "),
                       verbose = verbose, verboseLevel = 0)
      }
    }
    if (sameBlock) {
      messageVerbose(alreadyInRprofileMessage, verbose = verbose, verboseLevel = 1)
    } else {
      bodyFn <- c(
        paste0(
          "\n", setLibPathsStartText, " #### ", newFileTrigger, newFile,
          " # DO NOT EDIT BETWEEN THESE LINES"
        ),
        "### DELETE THESE LINES BELOW TO RESTORE STANDARD R Package LIBRARY",
        if (is.null(prevLibPathsLine) || is.na(prevLibPathsLine)) {
          paste0("### ", prevLibPathsText, paste(.libPaths(), collapse = ", "))
        } else {
          prevLibPathsLine
        },
        libPathsLine,
        standAloneLine,
        setLibPathsBlockCode,
        if (verbose >= 1) {
          resetRprofileMessage(updateRprofile)
        },
        paste0(setLibPathsEndText)
      )
      messageVerbose("Updating ", normPath(updateRprofile),
        "; this will set new libPaths for R packages even after restarting R",
        verbose = verbose, verboseLevel = 1
      )
      cat(bodyFn, file = updateRprofile, append = TRUE, sep = "\n")
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
        old <- removeSetLibPathsBlock(updateRprofile)
        if (old$found) {
          messageVerbose("removing custom libPaths in .Rprofile",
            verbose = verbose, verboseLevel = 1
          )
          prevLibPaths <- strsplit(sub(paste0(".*", prevLibPathsText), "", old$prevLibPathsLine),
                                   split = ", ")[[1]]
          .libPaths(prevLibPaths)

          if (old$newFile) {
            file.remove(updateRprofile)
          } else {
            writeLines(old$lines, con = updateRprofile)
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

## Finds the setLibPaths block in an .Rprofile file and returns the file's lines without it
## (and without the blank line written before it). `newFile` is TRUE if the block's own
## "New File:" flag says setLibPaths created the file and nothing else is left in it.
removeSetLibPathsBlock <- function(updateRprofile) {
  ll <- readLines(updateRprofile)
  start <- grep(setLibPathsStartText, ll)[1]
  out <- list(found = !is.na(start), lines = ll, block = character(), prevLibPathsLine = NA_character_,
              newFile = FALSE)
  if (!out$found) return(out)
  end <- grep(setLibPathsEndText, ll)
  end <- c(end[end >= start], start)[1]
  out$block <- ll[start:end]
  if (start > 1 && identical("", ll[start - 1])) start <- start - 1
  out$lines <- ll[-(start:end)]
  out$prevLibPathsLine <- grep(prevLibPathsText, out$block, value = TRUE)[1]
  out$newFile <- any(grepl(paste0(newFileTrigger, "TRUE"), out$block)) && !any(nzchar(out$lines))
  out
}

resetRprofileMessage <- function(updateRprofile = ".Rprofile") {
  paste0("message(\"To reset libPaths to previous state, run: Require::setupOff() (or delete section in .Rprofile file)\") ")
}

checkTRUERprofile <- function(updateRprofile) {
  if (isTRUE(updateRprofile)) updateRprofile <- ".Rprofile"
  updateRprofile
}

## The code in the .Rprofile block that sets the library, after the `._libPaths` and `._standAlone`
## lines. A fixed template, not deparsed from setLibPaths(): a block cut out of deparsed source by
## line counts broke whenever setLibPaths() changed (#234 left a stray `}` and R failed to start).
setLibPathsBlockCode <- c(
  "{",
  "    .oldLibPaths <- .libPaths()",
  "    if (!dir.exists(._libPaths[1])) dir.create(._libPaths[1], recursive = TRUE)",
  "    gte4.1 <- isTRUE(getRversion() >= \"4.1\")",
  "    if (gte4.1) {",
  "        do.call(.libPaths, list(new = ._libPaths, include.site = !._standAlone))",
  "    }",
  "    .shim_fun <- .libPaths",
  "    .shim_env <- new.env(parent = environment(.shim_fun))",
  "    if (isTRUE(._standAlone)) {",
  "        .shim_env$.Library <- utils::tail(.libPaths(), 1)",
  "    } else {",
  "        .shim_env$.Library <- .libPaths()",
  "    }",
  "    .shim_env$.Library.site <- character()",
  "    environment(.shim_fun) <- .shim_env",
  "    .shim_fun(unique(._libPaths))",
  "    message(\".libPaths() is now: \", paste(.libPaths(), collapse = \", \"))",
  "}"
)

prevLibPathsText <- "Previous .libPaths: "
commentCharsForSetLibPaths <- "#### setLibPaths "
setLibPathsStartText <- paste0(commentCharsForSetLibPaths, "start")
setLibPathsEndText <- paste0(commentCharsForSetLibPaths, "end")
newFileTrigger <- "New File:"
alreadyInRprofileMessage <- "There is already a setLibPaths in the .Rprofile, skipping"
