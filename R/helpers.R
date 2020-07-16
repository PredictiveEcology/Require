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
              path <- lapply(path, function(x) {
                if (is.na(x)) {
                  NA_character_
                } else {
                  normalizePath(x, winslash = "/", mustWork = FALSE)
                }
              })
              # Eliot changed this Sept 24, 2019 because weird failures with getwd()
              # in non-interactive testing
              path <- unlist(path)
              if (!is.null(path)) {
                hasDotStart <- startsWith(path, ".")
                if (isTRUE(any(hasDotStart)))
                  path[hasDotStart] <- gsub("^[.]", paste0(getwd()), path[hasDotStart])
                path <- gsub("\\\\", "//", path)
                path <- gsub("//", "/", path)
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

################################################################################
#' Check directory path
#'
#' Checks the specified path to a directory for formatting consistencies,
#' such as trailing slashes, etc.
#'
#' @note This will not work for paths to files.
#' To check for existence of files, use \code{\link{file.exists}}.
#' To normalize a path to a file, use \code{\link{normPath}} or \code{\link{normalizePath}}.
#'
#' @param path A character string corresponding to a directory path.
#'
#' @param create A logical indicating whether the path should
#' be created if it does not exist. Default is \code{FALSE}.
#'
#' @return Character string denoting the cleaned up filepath.
#'
#' @seealso \code{\link{file.exists}}, \code{\link{dir.create}}.
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
    # if (length(path) != 1) {
    #   stop("path must be a character vector of length 1.")
    # } else {
    if (isTRUE(all(is.na(path)))) {
      stop("Invalid path: cannot be NA.")
    } else {

      path <- normPath(path) # this is necessary to cover Windows double slash used on non-Windows

      dirsThatExist <- dir.exists(path)
      if (any(!dirsThatExist)) {
        isExistingFile <- file.exists(path)
        if (all(isExistingFile)) {
          message("That path is an existing file(s)")
        } else {
          if (create == TRUE) {
            lapply(path[!dirsThatExist[!isExistingFile]], function(pth) {
              dir.create(file.path(pth), recursive = TRUE, showWarnings = FALSE)
            })
          } else {
            stop(paste("Specified path", normPath(path), "doesn't exist.",
                       "Create it and try again."))
          }
        }
      }
      if (Sys.info()[["sysname"]] == "Darwin")
        path <- normPath(path) # ensure path re-normalized after creation

      return(path)
    }
    #}
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
#' Sends to \code{message}, but in a structured way so that a data.frame-like can
#' be cleanly sent to messaging.
#'
#' @param df A data.frame, data.table, matrix
#' @param round An optional numeric to pass to \code{round}
#' @importFrom data.table is.data.table as.data.table
#' @importFrom utils capture.output
messageDF <- function(df, round) {#}, colour = NULL) {
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
    #if (!is.null(colour)) {
    #  message(getFromNamespace(colour, ns = "crayon")(x))
    #} else {
      message(x)
    #}
  })
}

#' Make a temporary (sub-)directory
#'
#' Create a temporary subdirectory in \code{.RequireTempPath()}, or a
#' temporary file in that temporary subdirectory.
#'
#' @param sub Character string, length 1. Can be a result of
#'   \code{file.path("smth", "smth2")} for nested temporary sub
#'   directories.
#' @param tempdir Optional character string where the temporary dir should be placed.
#'   Defaults to \code{.RequireTempPath()}
#' @seealso \code{\link{tempfile2}}
#' @export
tempdir2 <- function(sub = "", tempdir = getOption("Require.tempPath", .RequireTempPath())) {
  checkPath(normPath(file.path(tempdir, sub)), create = TRUE)
}

#' Make a temporary subfile in a temporary (sub-)directory
#'
#' @param ... passed to \code{tempfile}, e.g., \code{fileext}
#'
#' @seealso \code{\link{tempdir2}}
#' @inheritParams tempdir2
#' @param ... passed to \code{tempfile}, e.g., \code{fileext}
#' @export
tempfile2 <- function(sub = "",
                      tempdir = getOption("Require.tempPath", .RequireTempPath()),
                      ...) {
  normPath(file.path(tempdir2(sub = sub, tempdir = tempdir), basename(tempfile(...))))
}

.RequireTempPath <- function() normPath(file.path(tempdir(), "Require"))

#' Invert a 2-level list
#'
#' This is a simple version of \code{purrr::transpose}, only for lists with 2 levels.
#'
#' @param l A list with 2 levels. If some levels are absent, they will be \code{NULL}
#'
#' @return A list with 2 levels deep, inverted from \code{l}
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
