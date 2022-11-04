################################################################################
#' Normalize filepath
#'
#' Checks the specified filepath for formatting consistencies:
#'  1) use slash instead of backslash;
#'  2) do tilde etc. expansion;
#'  3) remove trailing slash.
#'
#' @param path A character vector of filepaths.
#'
#' @return Character vector of cleaned up filepaths.
#'
#' @export
#' @rdname normPath
#'
#' @example inst/examples/example_checkPath.R
#'
setGeneric("normPath", function(path) {
  standardGeneric("normPath")
})

#' @export
#' @rdname normPath
setMethod("normPath",
          signature(path = "character"),
          definition = function(path) {
            if (length(path) > 0) {
              nas <- is.na(path)
              if (any(!nas)) {
                path[!nas] <- normalizePath(path[!nas], winslash = "/", mustWork = FALSE)
              }
              if (any(nas)) {
                path[nas] <- NA_character_
              }

              # Eliot changed this Sept 24, 2019 because weird failures with getwd()
              # in non-interactive testing
              path <- unlist(path)
              if (!is.null(path)) {
                path <- gsub("\\\\", "//", path)
                path <- gsub("//", "/", path)
                hasDotStart <- startsWith(path, "./")
                if (isTRUE(any(hasDotStart)))
                  path[hasDotStart] <- gsub("^[.]/", paste0(getwd(), "/"), path[hasDotStart])
                path <- gsub("/$", "", path) # nolint
              }
            }
            return(path)
})

#' @export
#' @rdname normPath
setMethod("normPath",
          signature(path = "list"),
          definition = function(path) {
            return(normPath(unlist(path)))
})

#' @export
#' @rdname normPath
setMethod("normPath",
          signature(path = "NULL"),
          definition = function(path) {
            return(character(0))
})

#' @export
#' @rdname normPath
setMethod("normPath",
          signature(path = "missing"),
          definition = function() {
            return(character(0))
})

#' @export
#' @rdname normPath
setMethod("normPath",
          signature(path = "logical"),
          definition = function(path) {
            return(NA_character_)
})

################################################################################
#' Check directory path
#'
#' Checks the specified path to a directory for formatting consistencies,
#' such as trailing slashes, etc.
#'
#' @note This will not work for paths to files.
#' To check for existence of files, use [file.exists()].
#' To normalize a path to a file, use [normPath()] or [normalizePath()].
#'
#' @param path A character string corresponding to a directory path.
#'
#' @param create A logical indicating whether the path should
#' be created if it does not exist. Default is `FALSE`.
#'
#' @return Character string denoting the cleaned up filepath.
#'
#' @seealso [file.exists()], [dir.create()].
#'
#' @export
#' @rdname checkPath
#'
#' @example inst/examples/example_checkPath.R
#'
setGeneric("checkPath", function(path, create) {
  standardGeneric("checkPath")
})

#' @export
#' @rdname checkPath
setMethod(
  "checkPath",
  signature(path = "character", create = "logical"),
  definition = function(path, create) {
    if (isTRUE(all(is.na(path)))) {
      stop("Invalid path: cannot be NA.")
    } else {
      path <- normPath(path) # this is necessary to cover Windows double slash used on non-Windows
      dirsThatExist <- dir.exists(path)
      if (any(!dirsThatExist)) {
        isExistingFile <- file.exists(path)
        if (all(isExistingFile)) {
          messageVerbose("That path is an existing file(s)", verboseLevel = 0,
                         verbose = getOption("Require.verbose"))
        } else {
          if (create == TRUE) {
            lapply(path[!dirsThatExist[!isExistingFile]], function(pth) {
              dir.create(file.path(pth), recursive = TRUE, showWarnings = FALSE)
            })
          } else {
            stop(paste("Specified path", normPath(path), "does not exist.",
                       "Create it and try again."))
          }
        }
      }
      if (Sys.info()[["sysname"]] == "Darwin")
        path <- normPath(path) # ensure path re-normalized after creation

      return(path)
    }
  })

#' @export
#' @rdname checkPath
setMethod("checkPath",
          signature(path = "character", create = "missing"),
          definition = function(path) {
            return(checkPath(path, create = FALSE))
})

#' @export
#' @rdname checkPath
setMethod("checkPath",
          signature(path = "NULL", create = "ANY"),
          definition = function(path) {
            stop("Invalid path: cannot be NULL.")
})

#' @export
#' @rdname checkPath
setMethod("checkPath",
          signature(path = "missing", create = "ANY"),
          definition = function() {
            stop("Invalid path: no path specified.")
})

#' @keywords internal
.rndstr <- function(n = 1, len = 8) {
  unlist(lapply(character(n), function(x) {
    x <- paste0(sample(c(0:9, letters, LETTERS), size = len, replace = TRUE), collapse = "")
  }))
}

#' Use message to print a clean square data structure
#'
#' Sends to `message`, but in a structured way so that a data.frame-like can
#' be cleanly sent to messaging.
#'
#' @param df A data.frame, data.table, matrix
#' @param round An optional numeric to pass to `round`
#' @inheritParams Require
#'
#' @importFrom data.table is.data.table as.data.table
#' @importFrom utils capture.output
#' @rdname messageVerbose
messageDF <- function(df, round, verbose = getOption("Require.verbose"),
                      verboseLevel = 1) {
  if (is.matrix(df))
    df <- as.data.frame(df)
  if (!is.data.table(df)) {
    df <- as.data.table(df)
  }
  if (!missing(round)) {
    isNum <- sapply(df, is.numeric)
    isNum <- colnames(df)[isNum]
    for (Col in isNum) {
      set(df, NULL, Col, round(df[[Col]], round))
    }
  }
  out <- lapply(capture.output(df), function(x) {
      messageVerbose(x, verbose = verbose, verboseLevel = verboseLevel)
  })
}

#' Make a temporary (sub-)directory
#'
#' Create a temporary subdirectory in `.RequireTempPath()`, or a
#' temporary file in that temporary subdirectory.
#'
#' @param sub Character string, length 1. Can be a result of
#'   `file.path("smth", "smth2")` for nested temporary sub
#'   directories.
#' @param tempdir Optional character string where the temporary dir should be placed.
#'   Defaults to `.RequireTempPath()`
#' @seealso [tempfile2()]
#' @export
tempdir2 <- function(sub = "", tempdir = getOption("Require.tempPath", .RequireTempPath())) {
  checkPath(normPath(file.path(tempdir, sub)), create = TRUE)
}

#' Make a temporary subfile in a temporary (sub-)directory
#'
#' @param ... passed to `tempfile`, e.g., `fileext`
#'
#' @seealso [tempdir2()]
#' @inheritParams tempdir2
#' @param ... passed to `tempfile`, e.g., `fileext`
#' @export
tempfile2 <- function(sub = "",
                      tempdir = getOption("Require.tempPath", .RequireTempPath()),
                      ...) {
  normPath(file.path(tempdir2(sub = sub, tempdir = tempdir), basename(tempfile(...))))
}

.RequireTempPath <- function() normPath(file.path(tempdir(), "Require"))

#' Invert a 2-level list
#'
#' This is a simple version of `purrr::transpose`, only for lists with 2 levels.
#'
#' @param l A list with 2 levels. If some levels are absent, they will be `NULL`
#'
#' @return A list with 2 levels deep, inverted from `l`
#'
#' @export
#' @examples
#' # create a 2-deep, 2 levels in first, 3 levels in second
#' a <- list(a = list(d = 1, e = 2:3, f = 4:6), b = list(d = 5, e = 55))
#' invertList(a) # creates 2-deep, now 3 levels outer --> 2 levels inner
invertList <- function(l) {
  indices <- list(names(l), names(l[[1]]))
  names(indices[[1]]) <- indices[[1]]
  names(indices[[2]]) <- indices[[2]]
  lapply(indices[[2]], function(i) {
    lapply(indices[[1]], function(j) l[[j]][[i]])
  })
}

#' `modifyList` for multiple lists
#'
#' @description
#' This calls [`utils::modifyList`] iteratively using
#' [`base::Reduce`], so it can handle >2 lists.
#' The subsequent list elements that share a name will override
#' previous list elements with that same name.
#' It also will handle the case where any list is a `NULL`. Note:
#' default `keep.null = TRUE`, which is different than `modifyList`
#'
#' @details
#' More or less a convenience around
#' `Reduce(modifyList, list(...))`, with some checks, and the addition of
#' `keep.null = TRUE` by default.
#' @inheritParams utils::modifyList
#'
#' @export
#' @param ... One or more named lists.
#' @importFrom utils modifyList
#' @examples
#' modifyList2(list(a = 1), list(a = 2, b = 2))
#' modifyList2(list(a = 1), NULL, list(a = 2, b = 2))
#' modifyList2(list(a = 1), list(x = NULL), list(a = 2, b = 2), list(a = 3, c = list(1:10)))
modifyList2 <- function(..., keep.null = FALSE) {
  dots <- list(...)
  if (length(dots) > 0) {
    dots <- dots[!unlist(lapply(dots, is.null))]
    areLists <- unlist(lapply(dots, is, "list"))
    if (all(areLists)) {
      # Create a function where I can pass `keep.null` and also pass to Reduce, which is binary only
      ml <- function(x, val) {
        modifyList(x, val, keep.null = keep.null)
      }
      dots <- Reduce(ml, dots)
    } else {
      if (all(!areLists)) {
        out <- Reduce(c, dots)
        dots <- out[!duplicated(names(out), out)]
      } else {
        stop("All elements must be named lists or named vectors")
      }
    }
  }

  # do.call(Reduce, alist(modifyList, dots)) # can't keep nulls with this approach
  dots
}

#' @note `modifyList3` retains the original behaviour of `modifyList2` (prior to Oct 2022);
#' however, it cannot retain `NULL` values in lists.
#'
#' @export
#' @rdname modifyList2
modifyList3 <- function(..., keep.null = TRUE) {
  dots <- list(...)
  dots <- dots[!unlist(lapply(dots, is.null))]
  do.call(Reduce, alist(modifyList, dots)) # can't keep nulls with this approach
}

#' Create link to file, falling back to making a copy if linking fails.
#'
#' First try to create a hardlink to the file.
#' If that fails, try a symbolic link (symlink) before falling back to copying the file.
#' "File" here can mean a file or a directory.
#'
#' @rdname linkOrCopy
#' @param from,to character vectors, containing file names or paths.
#' @param allowSymlink Logical. If `FALSE`, the default, then it will try `file.link` first, then
#'   `file.copy`, omitting the `file.symlink` step
#'
linkOrCopy <- function(from, to, allowSymlink = FALSE) {
  res <- suppressWarnings(file.link(from, to)) ## try hardlink
  if (any(!res)) {
    if (allowSymlink) {
      res[!res] <- suppressWarnings(file.symlink(from[!res], to[!res]))
    }
    if (any(!res)) {
      res[!res] <- suppressWarnings(
        file.copy(from[!res], to[!res], recursive = TRUE)) ## finally, copy the file
    }
  }
  return(invisible(res))
}

#' `fileRenameOrMove` is like `file.rename`, but will work across disks.
#' @rdname linkOrCopy
fileRenameOrMove <- function(from, to) {
  res <- suppressWarnings(file.rename(from, to)) ## try hardlink
  if (any(!res)) {
    res[!res] <- suppressWarnings(
      file.copy(from[!res], to[!res])) ## finally, copy the file
    unlink(from[!res])
  }
  return(invisible(res))
}

timestamp <- function() {
  format(Sys.time(), "%Y%m%d%H%M%S")
}


#' Similar to base::message, but with an verbosity threshold
#'
#' This will only show a message if the value of `verbose` is greater than
#' the `verboseLevel`.
#'
#' @rdname messageVerbose
#' @inheritParams base::message
#' @inheritParams Require
#' @param verboseLevel A numeric indicating what verbose threshold (level) above which
#'   this message will show.
messageVerbose <- function(..., verbose = getOption("Require.verbose"),
                           verboseLevel = 1) {
  if (verbose >= verboseLevel)
    message(...)
}


#' @rdname messageVerbose
#' @inheritParams Require
#' @param pre A single text string to paste before the counter
#' @param post A single text string to paste after the counter
#' @param counter An integer indicating which iteration is being done
#' @param total An integer indicating the total number to be done.
#' @param minCounter An integer indicating the minimum (i.e,. starting value)
messageVerboseCounter <- function(pre = "", post = "", verbose = getOption("Require.verbose"),
                                  verboseLevel = 1, counter = 1,
                                  total = 1, minCounter = 1) {
  total <- max(counter, total)
  minCounter <- min(minCounter, counter)
  mess <- paste0(paddedFloatToChar(counter, padL = nchar(total), pad = " ")
                 , " of ", total)
  numCharsNeeded <- nchar(mess) + 1
  messWithPrePost <- paste0(pre, mess, post)
  if (counter == minCounter) {
    messageVerbose(rep(" ", numCharsNeeded), verbose = verbose, verboseLevel = verboseLevel)
  }
  messageVerbose(rep("\b", numCharsNeeded),  messWithPrePost,
                 verbose = verbose, verboseLevel = verboseLevel)
}

# This environment variable "R_TESTS" is set during testing, and it points to a file
#   called Startup.Rs that is placed in the .libPaths(). If the .libPaths() is changed
#   during the testing, then that file will not be found, and install.packages will
#   fail to install a package with an error of source file not found. See:
#   https://github.com/HenrikBengtsson/startup/issues/19
#   The env variable is set here:
# https://github.com/wch/r-source/blob/8b6429feb661b02e2b2b6df1757b31cf1250a33e/
#   src/library/tools/R/testing.R#L472-Lundefined
R_TESTSomit <- function() {
  origR_TESTS <- Sys.getenv("R_TESTS")
  if (!identical(origR_TESTS, "")) {
    Sys.setenv("R_TESTS" = "")
  }
  return(origR_TESTS)
}


#' Like `setdiff`, but takes into account names
#'
#' This will identify the elements in `l1` that are not in `l2`. If `missingFill`
#' is provided, then elements that are in `l2`, but not in `l1` will be returned,
#' assigning `missingFill` to their values. This might be `NULL` or `""`, i.e., some
#' sort of empty value. This function will work on named lists, named vectors and likely
#' on other named classes.
#'
#' @return
#' A vector or list of the elements in `l1` that are not in `l2`, and optionally the
#' elements of `l2` that are not in `l1`, with values set to `missingFill`
#'
#' @details
#' There are 3 types of differences that might occur with named elements: 1. a new
#' named element, 2. an removed named element, and 3. a modified named element. This function
#' captures all of these. In the case of unnamed elements, e.g., `setdiff`, the first
#' two are not seen as differences, if the values are not different.
#'
#' @param l1 A named list or named vector
#' @param l2 A named list or named vector (must be same class as `l1`)
#' @param missingFill A value, such as `NULL` or `""` or `"missing"` that will be
#'   given to the elements returned, that are in `l2`, but not in `l1`
#'
#' @export
setdiffNamed <- function(l1, l2, missingFill) {
  changed1 <- setdiff(names(l2), names(l1)) # new option
  changed2 <- setdiff(names(l1), names(l2)) # option set to NULL
  changed3 <- vapply(names(l1), FUN.VALUE = logical(1), function(nam)
    identical(l2[nam], l1[nam]), USE.NAMES = TRUE) # changed values of existing
  changed3 <- l1[names(changed3[!changed3])]
  dif <- list()
  if (!missing(missingFill)) {
    dif[[1]] <- mapply(x = changed1, function(x) missingFill, USE.NAMES = TRUE)
  }
  dif[[2]] <- l1[changed2]
  dif[[3]] <- l1[names(changed3)]
  dif <- do.call(modifyList2, dif)
  dif
}

whereInStack <- function(obj) {
  for (i in 1:sys.nframe()) {
    fn <- get0(obj, sys.frame(-i), inherits = FALSE)
    if (!is.null(fn)) break
  }
  return(sys.frame(-i))
}

getInStack <- function(obj) {
  env <- whereInStack(obj)
  return(get(obj, envir = env, inherits = FALSE))
}

