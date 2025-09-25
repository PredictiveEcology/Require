#' Get cache directory paths
#'
#' Get the path to the cache directories associated with the `Require` package:
#' `cacheDir` and `cachePkgDir`, the latter is intended to be a sub-directory of `cacheDir`.
#' If `create = TRUE`, will ensure the directory path is created,
#' and a `README` file will be placed in that directory.
#'
#' @return character, specifying the path to the cache directory,
#'   or `NULL` indicating a cache is not in use.
#'   If `create = TRUE`, will create the directory at that path as a side effect.
#'
#' @details
#' These can be set using the environment variables `R_REQUIRE_CACHE` and `R_REQUIRE_PKG_CACHE`.
#' If you set only `R_REQUIRE_CACHE`, then both the package cache and cache directories will be set,
#' with the package cache as a sub-directory.
#' You can, however, set them independently, if you set both environment variables,
#' although doing so is less well-tested.
#' The package cache can also be set via an option `Require.cachePkgDir` (see [RequireOptions]).
#'
#' `cacheDir()` is used to get the current cache directory, checking the value of `R_REQUIRE_CACHE`,
#' which is used if set. If not set, will use the default cache directory via `cacheDefaultDir()`.
#'
#' @inheritParams checkPath
#'
#' @inheritParams Require
#'
#' @export
#' @rdname cacheDir
cacheDir <- function(create, verbose = getOption("Require.verbose")) {
  if (missing(create)) {
    create <- FALSE
  } # !is.null(cacheGetOptionCachePkgDir())

  ## OLD: was using cache dir following OS conventions used by rappdirs package:
  ##   rappdirs::user_cache_dir(appName)
  ## CURRENT: using cache dir following conventions used by tools::R_user_dir
  ##   tools::R_user_dir("appName", "cache")

  # browser()
  cacheDir <- if (nzchar(Sys.getenv("R_REQUIRE_CACHE"))) {
    Sys.getenv("R_REQUIRE_CACHE")
  } else {
    defaultCacheDirectory <- cacheDefaultDir()
    if (!is.null(defaultCacheDirOld)) {
      # solaris doesn't have this set
      if (dir.exists(defaultCacheDirOld)) {
        oldLocs <- dir(defaultCacheDirOld, full.names = TRUE, recursive = TRUE)
        if (length(oldLocs) > 0) {
          messageVerbose(
            "Require has changed default package cache folder from\n",
            defaultCacheDirOld,
            "\nto \n",
            defaultCacheDirectory,
            ". \nThere are packages ",
            "in the old Cache, moving them now..."
          )
          checkPath(defaultCacheDirectory, create = TRUE)
          dirs <- unique(dirname(oldLocs))
          newdirs <- gsub(defaultCacheDirOld, defaultCacheDirectory, dirs)
          lapply(newdirs, checkPath, create = TRUE)
          fileRenameOrMove(oldLocs, gsub(defaultCacheDirOld, defaultCacheDirectory, oldLocs))
          unlink(defaultCacheDirOld, recursive = TRUE)
        }
      }
    }
    defaultCacheDirectory
  }

  cacheDir <- normPathMemoise(cacheDir)

  if (isTRUE(create)) {
    cacheDir <- checkPath(cacheDir, create = create)
    readme <- file.path(cacheDir, "README")
    if (!file.exists(readme)) {
      if (isTRUE(create)) {
        file.copy(base::system.file("cache-README", package = "Require"), readme)
      }
    }
  }

  return(cacheDir)
}

#' @details
#' `cacheDefaultDir()` is simply a wrapper around [tools::R_user_dir()], used to identify the
#' OS-specific user cache directory.
#'
#' @importFrom tools R_user_dir
#' @export
#' @rdname cacheDir
cacheDefaultDir <- function() {
  tools::R_user_dir("Require", which = "cache") |> normalizePath(mustWork = FALSE)
}

appName <- "R-Require"

defaultCacheDirOld <- switch(
  SysInfo[["sysname"]],
  Darwin = file.path("~", "Library", "Caches", appName) |> normalizePath(mustWork = FALSE),
  Linux = file.path("~", ".cache", appName) |> normalizePath(mustWork = FALSE),
  Windows = file.path("C:", "Users", SysInfo[["user"]], "AppData", "Local", ".cache", appName) |>
    normalizePath(mustWork = FALSE)
)

#' @details
#'
#' `cachePkgDir()` is used to get the default package cache path, not the currently set path.
#' I.e., it does not check the values of the `Require.cachePkgDir` option, nor the
#' `R_REQUIRE_PKG_CACHE` environment variable.
#'
#' @export
#' @rdname cacheDir
cachePkgDir <- function(create) {
  if (missing(create)) {
    create <- FALSE
  }

  pkgCacheDir <- file.path(cacheDefaultDir(), "packages") |>
    rPkgDir(exact = FALSE) |>
    normPathMemoise()

  if (isTRUE(create)) {
    pkgCacheDir <- checkPath(pkgCacheDir, create = TRUE)
  }

  ## TODO: prompt the user ONCE about using this cache dir, and save their choice
  ##       - remind them how to change this, and make sure it's documented!

  return(pkgCacheDir)
}

#' @details
#' `cacheGetOptionCachePkgDir()` is used to get the current package cache path.
#' It first checks whether the option `Require.cachePkgDir` is set (see [RequireOptions]).
#' This option can take one of several values:
#' - `TRUE`, `"true"`, `"TRUE"` mean use the default via `cachePkgDir()`;
#' - `FALSE`, `"false"`, `"FALSE"` mean do not use the cache (function will return `NULL`);
#' - `"default"` will check whether the `R_REQUIRE_PKG_CACHE` environment variable is set
#'   and defines a path. If so, that path is used (postpended with the R version).
#'   If not set, it uses the default package cache path via `cachePkgDir()`.
#'
#' @export
#' @rdname cacheDir
cacheGetOptionCachePkgDir <- function() {
  curVal <- getOption("Require.cachePkgDir") ## "default" set by default

  stopifnot(is.null(curVal) || is.logical(curVal) || is.character(curVal))

  ## TODO: deal with situation where R_REQUIRE_CACHE is set but not R_REQUIRE_PKG_CACHE
  if (!is.null(curVal)) {
    curValLogical <- as.logical(curVal) ## will treat "TRUE", "false", etc. as logical
    if (isTRUE(curValLogical)) {
      curVal <- cachePkgDir(create = FALSE) ## default cache package directory
    } else if (isFALSE(curValLogical)) {
      curVal <- NULL
    } else {
      ## curVal should be non-NULL and non-logical value;
      ## presumably a character string with value "default" or a directory path.
      fromEnvVars <- Sys.getenv("R_REQUIRE_PKG_CACHE")

      if (identical("default", curVal)) {
        if (nchar(fromEnvVars) == 0) {
          curVal <- cachePkgDir(create = FALSE) ## default cache package directory
        } else {
          curVal <- rPkgDir(fromEnvVars, exact = FALSE)
        }
      } else {
        curVal <- rPkgDir(fromEnvVars, exact = FALSE)
      }
    }
  }

  if (!is.null(curVal)) {
    curVal <- normPathMemoise(curVal) |> checkPath(create = TRUE)
  }

  return(curVal)
}

#' Append the R version to a directory path
#'
#' Used primarily with `cacheGetOptionCachePkgDir`.
#'
#' @param path character specifying a directory path
#'
#' @param exact logical indicating whether to use `path` as-is (i.e., without appending)
#'
#' @keywords internal
rPkgDir <- function(path = cacheGetOptionCachePkgDir(), exact = FALSE) {
  if (is.null(path)) {
    return(NULL)
  }

  if (isTRUE(exact)) {
    return(path)
  }

  path <- path[1]
  if (
    normPathMemoise(path) %in%
      normPathMemoise(strsplit(Sys.getenv("R_LIBS_SITE"), split = ":")[[1]])
  ) {
    path
  } else {
    if (!endsWith(path, versionMajorMinor())) {
      path <- file.path(path, versionMajorMinor())
    } else {
      path
    }
  }

  return(path)
}

RequireGitHubCacheDir <- function(create) {
  if (missing(create)) {
    create <- FALSE
  }
  ghPkgCacheDir <- file.path(cacheDir(create), .txtGitHub) |> normPathMemoise()

  if (isTRUE(create)) {
    ghPkgCacheDir <- checkPath(ghPkgCacheDir, create = TRUE)
  }

  ## TODO: prompt the user ONCE about using this cache dir, and save their choice
  ##       - remind them how to change this, and make sure it's documented!

  return(ghPkgCacheDir)
}

normPathMemoise <- function(d) {
  pe <- pkgEnv()
  if (getOption("Require.useMemoise", TRUE)) {
    fnName <- "normPath"
    if (!exists(fnName, envir = pe, inherits = FALSE)) {
      assign(fnName, newEmptyEnv(), envir = pe)
    }
    fnEnv <- get(fnName, envir = pe)
    ret <- Map(di = d, function(di) {
      if (!exists(di, envir = fnEnv, inherits = FALSE)) {
        assign(di, normPath(di), envir = fnEnv)
      }
      fnEnv[[di]]
    })
    ret <- unlist(ret) |> unname()
  } else {
    ret <- normPath(d)
  }

  return(ret)
}

#' Setup a project library, cache, options
#'
#' `setup` and `setupOff` are currently deprecated.
#' These may be re-created in a future version.
#' In its place, a user can simply put `.libPaths(libs, include.site = FALSE)`
#' in their `.Rprofile` file, where `libs` is the directory where the packages
#' should be installed and should be a folder with the R version number, e.g.,
#' derived by using `checkLibPaths(libs)`.
#'
#' @param newLibPaths Same as `RPackageFolders`. This is for more consistent
#'   naming with `Require(..., libPaths = ...)`.
#' @param RPackageFolders One or more folders where R packages are
#'   installed to and loaded from. In the case of more than one
#'   folder provided, installation will only happen in the first one.
#'
#' @param RPackageCache See `?RequireOptions`.
#'
#' @inheritParams setLibPaths
#' @inheritParams Require
#'
#' @export
#' @rdname setup
#'
setup <- function(
  newLibPaths,
  RPackageFolders, # = getOption("Require.RPackageFolders", "R"),
  RPackageCache = cacheGetOptionCachePkgDir(),
  standAlone = getOption("Require.standAlone", TRUE),
  verbose = getOption("Require.verbose")
) {
  if (missing(newLibPaths)) {
    if (missing(RPackageFolders)) {
      newLibPaths <- "R"
    } else {
      newLibPaths <- RPackageFolders
    }
  }
  newLibPaths <- normPath(newLibPaths)
  newLibPaths <- checkLibPaths(newLibPaths)
  .Deprecated(
    msg = paste0(
      "setup is deprecated; to get approximately the same functionality, ",
      "please put a line like\n",
      ".libPaths('",
      newLibPaths,
      "', include.site = ",
      !standAlone,
      ")",
      "\nin your .Rprofile file"
    )
  )
  return(invisible())
}

#' @rdname setup
#' @inheritParams Require
#' @export
#' @param removePackages Deprecated. Please remove packages manually from `.libPaths()`
setupOff <- function(removePackages = FALSE, verbose = getOption("Require.verbose")) {
  updateRprofile <- checkTRUERprofile(TRUE)
  if (!file.exists(updateRprofile)) {
    # not in current dir
    ## 1. Check project
    possDirs <- c(rprojroot::find_root(rprojroot::is_rstudio_project), "~")
    for (i in 1:2) {
      possDir <- possDirs[i]
      possFile <- file.path(possDir, updateRprofile)
      if (file.exists(possFile)) {
        updateRprofile <- possFile
        break
      }
    }
  }

  if (file.exists(updateRprofile)) {
    rproflines <- readLines(updateRprofile)
    start <- grep(setLibPathsStartText, rproflines)
    end <- grep(setLibPathsEndText, rproflines)
    newFile <- any(grepl(paste0(setLibPathsStartText, ".+New File:TRUE"), rproflines))
    if (length(start)) {
      rproflines <- rproflines[-(start:end)]
      if (length(rproflines) <= 1 && all(nchar(rproflines) == 0) && isTRUE(newFile)) {
        unlink(updateRprofile)
        messageVerbose(
          "removing the .Rprofile file, which had been created with ",
          "setLibPaths(updateRprofile = TRUE)"
        )
      } else {
        cat(rproflines, file = updateRprofile)
        messageVerbose("Setting .libPaths() has been removed from the .Rprofile file")
      }
    } else {
      messageVerbose("Require::setLibPaths was not run to change the .Rprofile file; nothing to do")
    }
  } else {
    messageVerbose("No .Rprofile file; nothing to do")
  }
  return(invisible())
}


#' Setup for binary Linux repositories
#'
#' Enable use of binary package builds for Linux from the RStudio Package
#' Manager repo. This will set the `repos` option, affecting the current R
#' session. It will put this `binaryLinux` in the first position. If the
#' `getOption("repos")` is `NULL`, it will put `backupCRAN` in second position.
#'
#' @param binaryLinux A CRAN repository serving binary Linux packages.
#' @param backupCRAN If there is no CRAN repository set
#'
#' @importFrom utils read.csv
#' @export
setLinuxBinaryRepo <- function(binaryLinux = urlForArchivedPkgs, backupCRAN = srcPackageURLOnCRAN) {
  if (isUbuntuOrDebian()) {
    if (!grepl("R Under development", R.version.string) && getRversion() >= "4.1") {
      if (is.null(names(backupCRAN))) {
        names(backupCRAN) <- rep("CRAN", length(backupCRAN))
      }

      repo <- c(CRAN = positBinaryRepos())

      currentRepos <- getOption("repos")
      insertBefore <- 1 # put first, unless otherwise
      if (!is.null(currentRepos)) {
        isCRAN <- whIsOfficialCRANrepo(currentRepos, srcPackageURLOnCRAN)
        # mirrorsLocalFile <- file.path(dirname(cacheGetOptionCachePkgDir()), ".mirrors.csv")
        # if (!file.exists(mirrorsLocalFile))
        #   download.file("https://cran.r-project.org/CRAN_mirrors.csv",
        #                 destfile = mirrorsLocalFile, quiet = TRUE)
        # a <- read.csv(mirrorsLocalFile)
        # b <- a[1,]
        # b$URL = "https://cran.rstudio.com/"
        # a <- rbind(a, b)
        # isCRAN <- lapply(gsub("https://", "", currentRepos),
        #        grep, x = gsub("https://", "", a$URL), value = TRUE)
        insertBefore <- which(lengths(isCRAN) > 0)
        repos <- c(repo, currentRepos)
        if (isTRUE(insertBefore > 1)) {
          # could have no CRAN official mirror
          repos <- c(
            currentRepos[seq(1, insertBefore - 1)],
            repo,
            currentRepos[seq(insertBefore, length(currentRepos))]
          )
        }
      } else {
        repos <- c(repo, backupCRAN)
      }

      repos <- repos[!duplicated(repos)]
      options(repos = repos)
    }
  }
}

whIsOfficialCRANrepo <- function(
  currentRepos = getOption("repos"),
  backupCRAN = srcPackageURLOnCRAN
) {
  mirrorsLocalFile <- file.path(dirname(cacheGetOptionCachePkgDir()), ".mirrors.csv")
  dir.create(dirname(mirrorsLocalFile), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(mirrorsLocalFile)) {
    download.file(
      "https://cran.r-project.org/CRAN_mirrors.csv",
      destfile = mirrorsLocalFile,
      quiet = TRUE
    )
  }
  a <- read.csv(mirrorsLocalFile)
  b <- a[1, ]
  b$URL <- "https://cran.rstudio.com/"
  a <- rbind(a, b)
  isCRAN <- lapply(
    gsub("https://", "", currentRepos),
    grep,
    x = gsub("https://", "", a$URL),
    value = TRUE
  )
  isCRAN
}

positBinaryRepos <- function(binaryLinux = urlForArchivedPkgs) {
  repo <- character()
  if (isUbuntuOrDebian()) {
    repo <- paste0(binaryLinux, "__linux__/", debianUbuntuRelease(), "/latest")
  }
  repo
}

debianUbuntuRelease <- function() {
  system("lsb_release -cs", intern = TRUE)
}
