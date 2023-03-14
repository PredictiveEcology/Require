#' Path to (package) cache directory
#'
#' Sets (if `create = TRUE`) or gets the cache
#' directory associated with the `Require` package.
#' @return
#' If `!is.null(getOptionRPackageCache())`, i.e., a cache path exists,
#' the cache directory will be created,
#'   with a README placed in the folder. Otherwise, this function will just
#'   return the path of what the cache directory would be.
#'
#' @details
#' To set a different directory than the default, set the system variable:
#' `R_USER_CACHE_DIR = "somePath"` and/or `R_REQUIRE_PKG_CACHE = "somePath"`
#' e.g., in `.Renviron` file or `Sys.setenv()`. See Note below.
#' @inheritParams checkPath
#' @export
#' @rdname RequireCacheDir
RequireCacheDir <- function(create) {
  if (missing(create)) {
    create <- FALSE
  } # !is.null(getOptionRPackageCache())

  ## OLD: was using cache dir following OS conventions used by rappdirs package:
  ##   rappdirs::user_cache_dir(appName)
  ## CURRENT: using cache dir following conventions used by tools::R_user_dir
  ##   tools::R_user_dir("appName", "cache")

  cacheDir <- if (nzchar(Sys.getenv("R_USER_CACHE_DIR"))) {
    Sys.getenv("R_USER_CACHE_DIR")
  } else {
    defaultCacheDirectory <- defaultCacheDir()
    if (!is.null(defaultCacheDirOld)) { # solaris doesn't have this set
      if (dir.exists(defaultCacheDirOld)) {
        oldLocs <- dir(defaultCacheDirOld, full.names = TRUE, recursive = TRUE)
        if (length(oldLocs) > 0) {
          message(
            "Require has changed default package cache folder from\n",
            defaultCacheDirOld, "\nto \n", defaultCacheDirectory, ". \nThere are packages ",
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
        file.copy(system.file("cache-README", package = "Require"), readme)
      }
    }
  }

  return(cacheDir)
}

normPathMemoise <- function(d) {
  if (getOption("Require.useMemoise", TRUE)) {
    fnName <- "normPath"
    if (!exists(fnName, envir = .pkgEnv, inherits = FALSE)) {
      .pkgEnv[[fnName]] <- new.env()
    }
    ret <- Map(di = d, function(di) {
      if (!exists(di, envir = .pkgEnv[[fnName]], inherits = FALSE)) {
        .pkgEnv[[fnName]][[di]] <- normPath(di)
      }
      .pkgEnv[[fnName]][[di]]
    })
    ret <- unlist(ret)
  } else {
    ret <- normPath(d)
  }

  return(ret)
}

#' @export
#' @rdname RequireCacheDir
#'
#' @note
#' Currently, there are 2 different Cache directories used by Require:
#' `RequireCacheDir` and `RequirePkgCacheDir`. The `RequirePkgCacheDir`
#'  is intended to be a sub-directory of the `RequireCacheDir`. If you set
#'  `Sys.setenv("R_USER_CACHE_DIR" = "somedir")`, then both the package cache
#'  and cache dirs will be set, with the package cache a sub-directory. You can, however,
#'  set them independently, if you set `"R_USER_CACHE_DIR"` and `"R_REQUIRE_PKG_CACHE"`
#'  environment variable. The package cache can also be set with
#'  `options("Require.RPackageCache" = "somedir")`.
RequirePkgCacheDir <- function(create) {
  if (missing(create)) {
    create <- FALSE # !is.null(getOptionRPackageCache())
  }
  pkgCacheDir <- normPathMemoise(file.path(RequireCacheDir(create), "packages", rversion()))
  if (isTRUE(create)) {
    pkgCacheDir <- checkPath(pkgCacheDir, create = TRUE)
  }

  ## TODO: prompt the user ONCE about using this cache dir, and save their choice
  ##       - remind them how to change this, and make sure it's documented!

  return(pkgCacheDir)
}

#' Get the option for `Require.RPackageCache`
#'
#' First checks if an environment variable `Require.RPackageCache`
#' is set and defines a path.
#' If not set, checks whether the `options("Require.RPackageCache")` is set.
#' If a character string, then it returns that.
#' If `TRUE`, then use `RequirePkgCacheDir()`. If `FALSE`
#' then returns `NULL`.
#'
#' @export
getOptionRPackageCache <- function() {
  curVal <- getOption("Require.RPackageCache")
  try <- 1
  while (try < 3) {
    if (isTRUE(curVal)) {
      curVal <- RequirePkgCacheDir(FALSE)
      break
    } else if (isFALSE(curVal)) {
      curVal <- NULL
      break
    } else {
      if (identical("default", curVal)) {
        fromEnvVars <- Sys.getenv("R_REQUIRE_PKG_CACHE")
        if (nchar(fromEnvVars) == 0) {
          curVal <- RequirePkgCacheDir(FALSE)
          break
        } else {
          try <- try + 1
          curVal <- fromEnvVars
          if (identical("TRUE", curVal)) {
            curVal <- TRUE
          } else if (identical("FALSE", curVal)) {
            curVal <- NULL
          } else {
            break
          }
        }
      } else {
        break
      }
    }
  }
  # if (!is.null(curVal)) {
  #   checkPath(curVal, create = TRUE)
  # }
  curVal
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
setup <- function(newLibPaths,
                  RPackageFolders, # = getOption("Require.RPackageFolders", "R"),
                  RPackageCache = getOptionRPackageCache(),
                  standAlone = getOption("Require.standAlone", TRUE),
                  verbose = getOption("Require.verbose")) {
  if (missing(newLibPaths)) {
    if (missing(RPackageFolders)) {
      newLibPaths <- "R"
    } else {
      newLibPaths <- RPackageFolders
    }
  }
  newLibPaths <- normPath(newLibPaths)
  newLibPaths <- checkLibPaths(newLibPaths)
  .Deprecated(msg = paste0(
    "setup is deprecated; to get approximately the same functionality, ",
    "please put a line like\n",
    ".libPaths('", newLibPaths, "', include.site = ", !standAlone, ")",
    "\nin your .Rprofile file"
  ))
  return(invisible())
}

#' @rdname setup
#' @inheritParams Require
#' @export
#' @param removePackages Deprecated. Please remove packages manually from
#'        the .libPaths()
setupOff <- function(removePackages = FALSE, verbose = getOption("Require.verbose")) {
  updateRprofile <- checkTRUERprofile(TRUE)
  if (file.exists(updateRprofile)) {
    rproflines <- readLines(updateRprofile)
    start <- grep(setLibPathsStartText, rproflines)
    end <- grep(setLibPathsEndText, rproflines)
    newFile <- any(grepl(paste0(setLibPathsStartText, ".+New File:TRUE"), rproflines))
    if (length(start)) {
      rproflines <- rproflines[-(start:end)]
      if (length(rproflines) <= 1 && all(nchar(rproflines) == 0) && isTRUE(newFile)) {
        unlink(updateRprofile)
        messageVerbose("removing the .Rprofile file, which had been created with ",
                       "setLibPaths(updateRprofile = TRUE)")
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
#' @export
setLinuxBinaryRepo <- function(binaryLinux = "https://packagemanager.posit.co/",
                               backupCRAN = srcPackageURLOnCRAN) {
  if (SysInfo["sysname"] == "Linux" && grepl("Ubuntu", utils::osVersion)) {
    if (!grepl("R Under development", R.version.string) && getRversion() >= "4.1") {
      repo <- c(
        CRAN =
          paste0(binaryLinux, "all/__linux__/", linuxRelease(), "/latest")
      )
      if (!is.null(getOption("repos"))) {
        backupCRAN <- getOption("repos")
      }
      if (is.null(names(backupCRAN))) names(backupCRAN) <- "CRAN"
      repo <- c(repo, backupCRAN)
      repo <- repo[!duplicated(repo)]
      options(repos = repo)
    }
  }
}

linuxRelease <- function() {
  system("lsb_release -cs", intern = TRUE)
}

appName <- "R-Require"

#' @importFrom tools R_user_dir
defaultCacheDir <- function() {
  normalizePath(tools::R_user_dir("Require", which = "cache"), mustWork = FALSE)
}

defaultCacheDirOld <- switch(SysInfo[["sysname"]],
  Darwin = normalizePath(file.path("~", "Library", "Caches", appName), mustWork = FALSE),
  Linux = normalizePath(file.path("~", ".cache", appName), mustWork = FALSE),
  Windows = normalizePath(file.path("C:", "Users", SysInfo[["user"]], "AppData", "Local", ".cache", appName), mustWork = FALSE)
)
