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
setMethod(
  "normPath",
  signature(path = "character"),
  definition = function(path) {
    if (length(path) > 0) {
      nas <- is.na(path)
      if (any(!nas)) {
        path[!nas] <-
          normalizePath(path[!nas], winslash = "/", mustWork = FALSE)
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
        if (isTRUE(any(hasDotStart))) {
          path[hasDotStart] <-
            gsub("^[.]/", paste0(getwd(), "/"), path[hasDotStart])
        }
        path <- gsub("/$", "", path) # nolint
      }
    }
    return(path)
  }
)

#' @export
#' @rdname normPath
setMethod(
  "normPath",
  signature(path = "list"),
  definition = function(path) {
    return(normPath(unlist(path)))
  }
)

#' @export
#' @rdname normPath
setMethod(
  "normPath",
  signature(path = "NULL"),
  definition = function(path) {
    return(character(0))
  }
)

#' @export
#' @rdname normPath
setMethod(
  "normPath",
  signature(path = "missing"),
  definition = function() {
    return(character(0))
  }
)

#' @export
#' @rdname normPath
setMethod(
  "normPath",
  signature(path = "logical"),
  definition = function(path) {
    return(NA_character_)
  }
)

#' Check directory path
#'
#' Checks the specified path to a directory for formatting consistencies,
#' such as trailing slashes, etc.
#'
#' @note This will not work for paths to files.
#' To check for existence of files, use `file.exists()`.
#' To normalize a path to a file, use `normPath()` or `normalizePath()`.
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
      path <-
        normPath(path) # this is necessary to cover Windows
      # double slash used on non-Windows
      dirsThatExist <- dir.exists(path)
      if (any(!dirsThatExist)) {
        isExistingFile <- file.exists(path)
        if (all(isExistingFile)) {
          messageVerbose(
            "That path is an existing file(s)",
            verboseLevel = 0,
            verbose = getOption("Require.verbose")
          )
        } else {
          if (create == TRUE) {
            lapply(path[!dirsThatExist[!isExistingFile]], function(pth) {
              dir.create(file.path(pth),
                         recursive = TRUE,
                         showWarnings = FALSE
              )
            })
          } else {
            stop(
              paste(
                "Specified path",
                normPath(path),
                "does not exist.",
                "Create it and try again."
              )
            )
          }
        }
      }
      if (SysInfo[["sysname"]] == "Darwin") {
        path <-
          normPath(path)
      } # ensure path re-normalized after creation

      return(path)
    }
  }
)

#' @export
#' @rdname checkPath
setMethod(
  "checkPath",
  signature(path = "character", create = "missing"),
  definition = function(path) {
    return(checkPath(path, create = FALSE))
  }
)

#' @export
#' @rdname checkPath
setMethod(
  "checkPath",
  signature(path = "NULL", create = "ANY"),
  definition = function(path) {
    stop("Invalid path: cannot be NULL.")
  }
)

#' @export
#' @rdname checkPath
setMethod(
  "checkPath",
  signature(path = "missing", create = "ANY"),
  definition = function() {
    stop("Invalid path: no path specified.")
  }
)

#' @keywords internal
.rndstr <- function(n = 1, len = 8) {
  unlist(lapply(character(n), function(x) {
    a <- c(0:9, letters, LETTERS)
    b <- sample(a, size = len, replace = TRUE)
    paste0(b, collapse = "")
    # x <-
    #   paste0(sample(
    #     c(0:9, letters, LETTERS),
    #     size = len,
    #     replace = TRUE
    #   ), collapse = "")
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
#' @export
#' @rdname messageVerbose
messageDF <-
  function(df,
           round,
           verbose = getOption("Require.verbose"),
           verboseLevel = 1) {
    if (is.matrix(df)) {
      df <- as.data.frame(df)
    }
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
#' @param tempdir Optional character string where the
#'   temporary dir should be placed. Defaults to `.RequireTempPath()`
#' @param create Logical. Should the directory be created. Default `TRUE`
#' @seealso [tempfile2()]
#' @export
tempdir2 <- function(sub = "",
                     tempdir = getOption("Require.tempPath", .RequireTempPath()),
                     create = TRUE) {
  np <- normPath(file.path(tempdir, sub))
  if (isTRUE(create)) {
    checkPath(np, create = TRUE)
  }
  np
}

tempdir3 <- function(sub = "Require") {
  nd <- tempdir() |> file.path(sub, basename(tempfile("tmpdir")))
  made <- dir.create(nd, showWarnings = FALSE, recursive = TRUE)
  nd
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

.RequireTempPath <- function() {
  normPath(file.path(tempdir(), "Require"))
}

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
    lapply(indices[[1]], function(j) {
      l[[j]][[i]]
    })
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
#' modifyList2(
#'   list(a = 1), list(x = NULL), list(a = 2, b = 2),
#'   list(a = 3, c = list(1:10))
#' )
modifyList2 <- function(..., keep.null = FALSE) {
  dots <- list(...)
  if (length(dots) > 0) {
    dots <- dots[!unlist(lapply(dots, is.null))]
    areLists <- unlist(lapply(dots, is, "list"))
    if (all(areLists)) {
      # Create a function where I can pass `keep.null` and also pass
      #   to Reduce, which is binary only
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

#' @note `modifyList3` retains the original behaviour of `modifyList2` (prior to
#'   Oct 2022); however, it cannot retain `NULL` values in lists.
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
#' First try to create a hardlink to the file. If that fails, try a symbolic
#' link (symlink) before falling back to copying the file. "File" here can mean
#' a file or a directory.
#'
#' @rdname linkOrCopy
#' @param from,to character vectors, containing file names or paths.
#' @param allowSymlink Logical. If `FALSE`, the default, then it will try
#'   `file.link` first, then `file.copy`, omitting the `file.symlink` step
#'
linkOrCopy <- function(from, to, allowSymlink = FALSE) {
  res <- suppressWarnings(file.link(from, to)) ## try hardlink
  if (any(!res)) {
    if (allowSymlink) {
      res[!res] <- suppressWarnings(file.symlink(from[!res], to[!res]))
    }
    if (any(!res)) {
      ## finally, copy the file
      res[!res] <-
        suppressWarnings(file.copy(from[!res], to[!res], recursive = TRUE))
    }
  }
  return(invisible(res))
}

#' `fileRenameOrMove` is like `file.rename`, but will work across disks.
#' @rdname linkOrCopy
fileRenameOrMove <- function(from, to) {
  du <- unique(dirname(to))
  checkPath(du, create = TRUE) # somewhat slow because `normPath`
  res <- suppressWarnings(file.rename(from, to)) ## try hardlink
  if (any(!res)) {
    ## finally, copy the file
    res[!res] <-
      suppressWarnings(file.copy(from[!res], to[!res], overwrite = TRUE))
    unlink(from[!res])
  }
  return(invisible(res))
}

timestamp <- function() {
  format(Sys.time(), "%Y%m%d%H%M%S")
}

#' Similar to base::message, but with an verbosity threshold
#'
#' This will only show a message if the value of `verbose` is greater than the
#' `verboseLevel`. This is mostly useful for developers of code who want to give
#' users of their code easy access to how verbose their code will be. A developer
#' of a function will place this `messageVerbose` internally, setting the `verboseLevel`
#' according to how advanced they may want the message to be. `1` is a reasonable
#' default for standard use, `0` would be for "a very important message for all users",
#' `2` or above would be increasing levels of details for e.g., advanced use.
#' If a user sets to `-1` with this numeric approach, they can avoid all messaging.
#'
#' @rdname messageVerbose
#' @inheritParams base::message
#' @inheritParams Require
#' @param verboseLevel A numeric indicating what verbose threshold (level) above
#'   which this message will show.
#' @export
#' @return
#' Used for side effects, namely messaging that can be turned on or off with different
#' numeric values of `verboseLevel`. A user sets the `verboseLevel` for a particular
#' message.
messageVerbose <-
  function(...,
           verbose = getOption("Require.verbose"),
           verboseLevel = 1) {
    if (verbose >= verboseLevel) {
      message(...)
    }
  }

#' @rdname messageVerbose
#' @inheritParams Require
#' @param pre A single text string to paste before the counter
#' @param post A single text string to paste after the counter
#' @param counter An integer indicating which iteration is being done
#' @param total An integer indicating the total number to be done.
#' @param minCounter An integer indicating the minimum (i.e,. starting value)
messageVerboseCounter <-
  function(pre = "",
           post = "",
           verbose = getOption("Require.verbose"),
           verboseLevel = 1,
           counter = 1,
           total = 1,
           minCounter = 1) {
    total <- max(counter, total)
    minCounter <- min(minCounter, counter)
    mess <-
      paste0(
        paddedFloatToChar(counter, padL = nchar(total), pad = " "),
        " of ", total
      )
    numCharsNeeded <- nchar(mess) + 1
    messWithPrePost <- paste0(pre, mess, post)
    if (counter == minCounter) {
      messageVerbose(rep(" ", numCharsNeeded),
                     verbose = verbose,
                     verboseLevel = verboseLevel
      )
    }
    messageVerbose(
      rep("\b", numCharsNeeded),
      messWithPrePost,
      verbose = verbose,
      verboseLevel = verboseLevel
    )
  }

#' This environment variable "R_TESTS" is set during testing, and it points to a file
#' called `Startup.Rs` that is placed in the `.libPaths()`.
#' If the `.libPaths()` is changed during the testing,
#' then that file will not be found, and install.packages will
#' fail to install a package with an error of source file not found.
#' See: <https://github.com/HenrikBengtsson/startup/issues/19>.
#'
#' The environment variable is set here:
#' `https://github.com/wch/r-source/blob/8b6429feb661b02e2b2b6df1757b31cf1250a33e/src/library/tools/R/testing.R#L472-Lundefined`
#'
#' @keywords internal
R_TESTSomit <- function() {
  origR_TESTS <- Sys.getenv("R_TESTS")
  if (!identical(origR_TESTS, "")) {
    Sys.setenv(R_TESTS = "")
  }
  return(origR_TESTS)
}


#' Like `setdiff`, but takes into account names
#'
#' This will identify the elements in `l1` that are not in `l2`. If
#' `missingFill` is provided, then elements that are in `l2`, but not in `l1`
#' will be returned, assigning `missingFill` to their values. This might be
#' `NULL` or `""`, i.e., some sort of empty value. This function will work on
#' named lists, named vectors and likely on other named classes.
#'
#' @return A vector or list of the elements in `l1` that are not in `l2`, and
#' optionally the elements of `l2` that are not in `l1`, with values set to
#' `missingFill`
#'
#' @details There are 3 types of differences that might occur with named
#' elements: 1. a new named element, 2. an removed named element, and 3. a
#' modified named element. This function captures all of these. In the case of
#' unnamed elements, e.g., `setdiff`, the first two are not seen as differences,
#' if the values are not different.
#'
#' @param l1 A named list or named vector
#' @param l2 A named list or named vector (must be same class as `l1`)
#' @param missingFill A value, such as `NULL` or `""` or `"missing"` that will
#'   be given to the elements returned, that are in `l2`, but not in `l1`
#'
#' @export
setdiffNamed <- function(l1, l2, missingFill) {
  changed1 <- setdiff(names(l2), names(l1)) # new option
  changed2 <- setdiff(names(l1), names(l2)) # option set to NULL
  changed3 <-
    vapply(names(l1), FUN.VALUE = logical(1), function(nam) {
      identical(l2[nam], l1[nam])
    }, USE.NAMES = TRUE) # changed values of existing
  changed3 <- l1[names(changed3[!changed3])]
  dif <- list()
  if (!missing(missingFill)) {
    dif[[1]] <-
      mapply(x = changed1, function(x) {
        missingFill
      }, USE.NAMES = TRUE)
  }
  dif[[2]] <- l1[changed2]
  dif[[3]] <- l1[names(changed3)]
  dif <- do.call(modifyList2, dif)
  dif
}

whereInStack <- function(obj) {
  for (i in 1:sys.nframe()) {
    fn <- get0(obj, sys.frame(-i), inherits = FALSE)
    if (!is.null(fn)) {
      break
    }
  }
  return(sys.frame(-i))
}

getInStack <- function(obj) {
  env <- whereInStack(obj)
  return(get(obj, envir = env, inherits = FALSE))
}

SysInfo <-
  Sys.info() # do this on load; nothing can change, so repeated calls are a waste

.setupExample <- function() {
  l <- list()
  l$opts <- options(
    Ncpus = 2L,
    Require.RequirePkgCache = FALSE
  ) ## TODO: use e.g., `tempdir2("examples")`
  # l$libOrig <- setLibPaths(tempdir3())
  l
}

.cleanup <- function(opts = list()) {
  unlink(file.path(tempdir(), "Require"), recursive = TRUE)
  unlink(Require::tempdir2(create = FALSE), recursive = TRUE)
  cacheClearPackages(
    ask = FALSE,
    Rversion = versionMajorMinor(),
    verbose = FALSE
  )
  # It appears that _R_CHECK_THINGS_IN_OTHER_DIRS_ detects both the R and the Require
  #   dirs, even though the R is not affiliated specifically with Require
  #   Nevertheless, if there is no other folder than "Require" in the "R" dir
  #   it will delete both the Require subdir and the R dir in the
  #   so that _R_CHECK_THINGS_IN_OTHER_DIRS_ shows nothing; but if there is another
  #   subdir in the R dir, it won't delete the R dir
  filesOuter <- dir(dirname(dirname(tools::R_user_dir(
    "Require", "cache"
  ))), full.names = TRUE, pattern = "^R$")
  filesOneIn <- dir(dirname(tools::R_user_dir(
    "Require", "cache"
  )), full.names = TRUE)
  unlink(filesOneIn, recursive = TRUE)
  if (length(filesOuter) == 1 && length(filesOneIn) <= 1) {
    unlink(filesOuter, recursive = TRUE)
  }
  # setLibPaths(opts$libOrig)
  options(opts$opts)
}

#' @importFrom utils packageDescription
.isDevelVersion <- function() {
  length(strsplit(packageDescription("Require")$Version, "\\.")[[1]]) > 3
}

.runLongTests <- function() {
  .isDevelVersion() || Sys.getenv("R_REQUIRE_RUN_ALL_TESTS") == "true"
}

.runLongExamples <- function() {
  .isDevelVersion() ||
    Sys.getenv("R_REQUIRE_RUN_ALL_EXAMPLES") == "true"
}




doCranCacheCheck <- function(localFiles, verbose = getOption("Require.verbose")) {
  if (getOption("Require.useCranCache", FALSE)) {
    pe <- pkgEnv()
    if (is.null(pe[["crancacheCheck"]])) {
      pe[["crancacheCheck"]] <- TRUE
      crancache <- crancacheFolder()
      if (dir.exists(crancache)) {
        ccFiles <- dir(crancache, full.names = TRUE, recursive = TRUE)
        ccFiles <- grep("PACKAGES", ccFiles, invert = TRUE, value = TRUE)
        alreadyThere <- basename(ccFiles) %in% basename(localFiles)
        if (any(!alreadyThere)) {
          ccFiles <- ccFiles[!alreadyThere]
          toFiles <- file.path(cacheGetOptionCachePkgDir(), basename(ccFiles))
          linked <- linkOrCopy(ccFiles, toFiles)
          messageVerbose(blue("crancache had some packages; creating link or copy in Require Cache"),
                         verbose = verbose, verboseLevel = 1
          )
          localFiles <- dir(cacheGetOptionCachePkgDir(), full.names = TRUE)
        }
      }
    }
  }
  return(localFiles)
}


# library(rversions)
# dput(tail(r_versions(), 12))
rversionHistory <- as.data.table(
  structure(list(
    version = c("4.0.5", "4.1.0", "4.1.1", "4.1.2",
                "4.1.3", "4.2.0", "4.2.1", "4.2.2", "4.2.3", "4.3.0", "4.3.1",
                "4.3.2"),
    date = structure(c(1617174315, 1621321522, 1628579106,
                       1635753912, 1646899538, 1650611141, 1655967933, 1667203554, 1678867561,
                       1682060774, 1686899167, 1698739662), class = c("POSIXct", "POSIXt"
                       ), tzone = "UTC"),
    nickname = c("Shake and Throw", "Camp Pontanezen",
                 "Kick Things", "Bird Hippie", "One Push-Up", "Vigorous Calisthenics",
                 "Funny-Looking Kid", "Innocent and Trusting", "Shortstop Beagle",
                 "Already Tomorrow", "Beagle Scouts", "Eye Holes")),
    row.names = 122:133, class = "data.frame")
)

crancacheFolder <- function() {
  crancache <- file.path(dirname(dirname(tools::R_user_dir("Require", "cache"))), "R-crancache")
}


colr <- function(..., digit = 32) paste0("\033[", digit, "m", paste0(...), "\033[39m")
purple <- function(...) colr(..., digit = "38;5;129m")
black <- function(...) colr(..., digit = 30)
green2 <- function(...) colr(..., digit = 38)
cyan <- function(...) colr(..., digit = 29)
red <- function(...) colr(..., digit = 31)
green <- function(...) colr(..., digit = 32)
yellow <- function(...) colr(..., digit = 33)
blue <- function(...) colr(..., digit = 34)
turquoise <- function(...) colr(..., digit = 36)
greyLight <- function(...) colr(..., digit = 90)

