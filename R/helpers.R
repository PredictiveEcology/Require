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
    x <-
      paste0(sample(
        c(0:9, letters, LETTERS),
        size = len,
        replace = TRUE
      ), collapse = "")
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

.RequireTempPath <-
  function() {
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
#' `verboseLevel`.
#'
#' @rdname messageVerbose
#' @inheritParams base::message
#' @inheritParams Require
#' @param verboseLevel A numeric indicating what verbose threshold (level) above
#'   which this message will show.
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
  options(
    Ncpus = 2L,
    Require.RequirePkgCache = FALSE
  ) ## TODO: use e.g., `tempdir2("examples")`
}

.cleanup <- function(opts = list()) {
  unlink(Require::tempdir2(create = FALSE), recursive = TRUE)
  clearRequirePackageCache(
    ask = FALSE,
    Rversion = rversion(),
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
  options(opts)
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
    if (is.null(.pkgEnv[["crancacheCheck"]])) {
      .pkgEnv[["crancacheCheck"]] <- TRUE
      crancache <- crancacheFolder()
      if (dir.exists(crancache)) {
        ccFiles <- dir(crancache, full.names = TRUE, recursive = TRUE)
        ccFiles <- grep("PACKAGES", ccFiles, invert = TRUE, value = TRUE)
        alreadyThere <- basename(ccFiles) %in% basename(localFiles)
        if (any(!alreadyThere)) {
          ccFiles <- ccFiles[!alreadyThere]
          toFiles <- file.path(getOptionRPackageCache(), basename(ccFiles))
          linked <- linkOrCopy(ccFiles, toFiles)
          messageVerbose(blue("crancache had some packages; creating link or copy in Require Cache"),
            verbose = verbose, verboseLevel = 1
          )
          localFiles <- dir(getOptionRPackageCache(), full.names = TRUE)
        }
      }
    }
  }
  return(localFiles)
}


rversionHistory <- as.data.table(
  structure(list(
    version = c(
      "0.60", "0.61", "0.61.1", "0.61.2",
      "0.61.3", "0.62", "0.62.1", "0.62.2", "0.62.3", "0.62.4", "0.63",
      "0.63.1", "0.63.2", "0.63.3", "0.64", "0.64.1", "0.64.2", "0.65",
      "0.65.1", "0.90", "0.90.1", "0.99", "1.0", "1.0.1", "1.1", "1.1.1",
      "1.2", "1.2.1", "1.2.2", "1.2.3", "1.3", "1.3.1", "1.4", "1.4.1",
      "1.5.0", "1.5.1", "1.6.0", "1.6.1", "1.6.2", "1.7.0", "1.7.1",
      "1.8.0", "1.8.1", "1.9.0", "1.9.1", "2.0.0", "2.0.1", "2.1.0",
      "2.1.1", "2.2.0", "2.2.1", "2.3.0", "2.3.1", "2.4.0", "2.4.1",
      "2.5.0", "2.5.1", "2.6.0", "2.6.1", "2.6.2", "2.7.0", "2.7.1",
      "2.7.2", "2.8.0", "2.8.1", "2.9.0", "2.9.1", "2.9.2", "2.10.0",
      "2.10.1", "2.11.0", "2.11.1", "2.12.0", "2.12.1", "2.12.2", "2.13.0",
      "2.13.1", "2.13.2", "2.14.0", "2.14.1", "2.14.2", "2.15.0", "2.15.1",
      "2.15.2", "2.15.3", "3.0.0", "3.0.1", "3.0.2", "3.0.3", "3.1.0",
      "3.1.1", "3.1.2", "3.1.3", "3.2.0", "3.2.1", "3.2.2", "3.2.3",
      "3.2.4", "3.2.5", "3.3.0", "3.3.1", "3.3.2", "3.3.3", "3.4.0",
      "3.4.1", "3.4.2", "3.4.3", "3.4.4", "3.5.0", "3.5.1", "3.5.2",
      "3.5.3", "3.6.0", "3.6.1", "3.6.2", "3.6.3", "4.0.0", "4.0.1",
      "4.0.2", "4.0.3", "4.0.4", "4.0.5", "4.1.0", "4.1.1", "4.1.2",
      "4.1.3", "4.2.0", "4.2.1", "4.2.2"
    ),
    date = structure(
      c(
        881225278,
        882709762, 884392315, 889903555, 894095897, 897828980, 897862405,
        900069225, 904294939, 909144521, 910967839, 912776788, 916059350,
        920644034, 923491181, 926083543, 930918195, 935749769, 939211984,
        943273514, 945260947, 949922690, 951814523, 955701858, 961058601,
        966329658, 976875565, 979553881, 983191405, 988284587, 993206462,
        999261952, 1008756894, 1012391855, 1020074486, 1024312833, 1033466791,
        1036146797, 1042212874, 1050497887, 1055757279, 1065611639, 1069416021,
        1081766198, 1087816179, 1096899878, 1100528190, 1113863193, 1119259633,
        1128594134, 1135074921, 1145875040, 1149150333, 1159870504, 1166435363,
        1177407703, 1183029426, 1191402173, 1196086444, 1202469005, 1208850329,
        1214207072, 1219654436, 1224494641, 1229936597, 1239957168, 1246018257,
        1251102154, 1256547742, 1260786504, 1271923881, 1275293425, 1287132117,
        1292490724, 1298632039, 1302683487, 1310117828, 1317366356, 1320048549,
        1324541418, 1330503010, 1333091765, 1340348984, 1351235476, 1362126509,
        1364973156, 1368688293, 1380093069, 1394093553, 1397113870, 1404976269,
        1414743092, 1425888740, 1429168413, 1434611704, 1439536398, 1449735188,
        1457597745, 1460649578, 1462259608, 1466493698, 1477901595, 1488788191,
        1492758885, 1498806251, 1506582275, 1512029105, 1521101067, 1524467078,
        1530515071, 1545293080, 1552291489, 1556262303, 1562310303, 1576137903,
        1582963516, 1587711934, 1591427116, 1592809519, 1602313524, 1613376313,
        1617174315, 1621321522, 1628579106, 1635753912, 1646899538, 1650611141,
        1655967933, 1667203554
      ),
      class = c("POSIXct", "POSIXt"), tzone = "UTC"
    ),
    nickname = c(
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA, "Great Pumpkin", "December Snowflakes",
      "Gift-Getting Season", "Easter Beagle", "Roasted Marshmallows",
      "Trick or Treat", "Security Blanket", "Masked Marvel", "Good Sport",
      "Frisbee Sailing", "Warm Puppy", "Spring Dance", "Sock it to Me",
      "Pumpkin Helmet", "Smooth Sidewalk", "Full of Ingredients",
      "World-Famous Astronaut", "Fire Safety", "Wooden Christmas-Tree",
      "Very Secure Dishes", "Very, Very Secure Dishes", "Supposedly Educational",
      "Bug in Your Hair", "Sincere Pumpkin Patch", "Another Canoe",
      "You Stupid Darkness", "Single Candle", "Short Summer", "Kite-Eating Tree",
      "Someone to Lean On", "Joy in Playing", "Feather Spray",
      "Eggshell Igloo", "Great Truth", "Planting of a Tree", "Action of the Toes",
      "Dark and Stormy Night", "Holding the Windsock", "Arbor Day",
      "See Things Now", "Taking Off Again", "Bunny-Wunnies Freak Out",
      "Lost Library Book", "Shake and Throw", "Camp Pontanezen",
      "Kick Things", "Bird Hippie", "One Push-Up", "Vigorous Calisthenics",
      "Funny-Looking Kid", "Innocent and Trusting"
    )
  ), row.names = c(
    NA,
    -129L
  ), class = "data.frame")
)

crancacheFolder <- function() {
  crancache <- file.path(dirname(dirname(tools::R_user_dir("Require", "cache"))), "R-crancache")
}
