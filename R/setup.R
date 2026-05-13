#' Path to (package) cache directory
#'
#' Sets (if `create = TRUE`) or gets the cache
#' directory associated with the `Require` package.
#' @return
#' If `!is.null(cacheGetOptionCachePkgDir())`, i.e., a cache path exists,
#' the cache directory will be created,
#'   with a README placed in the folder. Otherwise, this function will just
#'   return the path of what the cache directory would be.
#'
#' @details
#' To set a different directory than the default, set the system variable:
#' `R_REQUIRE_CACHE = "somePath"` and/or `R_REQUIRE_PKG_CACHE = "somePath"`
#' e.g., in `.Renviron` file or `Sys.setenv()`. See Note below.
#' @inheritParams checkPath
#' @inheritParams Require
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

  cacheDir <- if (nzchar(Sys.getenv("R_REQUIRE_CACHE"))) {
    Sys.getenv("R_REQUIRE_CACHE")
  } else {
    defaultCacheDirectory <- cacheDefaultDir()
    if (!is.null(defaultCacheDirOld)) { # solaris doesn't have this set
      if (dir.exists(defaultCacheDirOld)) {
        oldLocs <- dir(defaultCacheDirOld, full.names = TRUE, recursive = TRUE)
        if (length(oldLocs) > 0) {
          messageVerbose(
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
        file.copy(base::system.file("cache-README", package = "Require"), readme)
      }
    }
  }

  return(cacheDir)
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
    ret <- unlist(ret)
  } else {
    ret <- normPath(d)
  }

  return(ret)
}

#' @export
#' @rdname cacheDir
#'
#' @note
#' `cachePkgDir()` is the single source of truth for "where Require/pak puts
#' downloaded package tarballs".
#'
#' * With `getOption("Require.usePak", TRUE)` (the default): a thin wrapper
#'   over `pak::cache_summary()$cachepath`. The directory is owned and managed
#'   by pak/pkgcache; the location is controlled by the `R_USER_CACHE_DIR`
#'   environment variable (pak's standard knob -- read at pak's subprocess
#'   spawn time). When `R_USER_CACHE_DIR` is unset, pak resolves it via
#'   `tools::R_user_dir("pkgcache", "cache")`, so by default the path lives
#'   as a sibling of Require's own scratch dir (`R_REQUIRE_CACHE`).
#'
#' * With `usePak = FALSE` (legacy non-pak path): `<cacheDir>/packages/<Rver>`,
#'   driven by `R_REQUIRE_CACHE`.
#'
#' To share a cache across machines or R versions, set `R_USER_CACHE_DIR` in
#' `.Renviron`. The previously-supported `R_REQUIRE_PKG_CACHE` env var and
#' `Require.cachePkgDir` option are deprecated -- use `R_USER_CACHE_DIR`.
cachePkgDir <- function(create) {
  if (missing(create)) {
    create <- FALSE
  }

  usePak <- isTRUE(getOption("Require.usePak", TRUE))
  if (usePak && requireNamespace("pak", quietly = TRUE)) {
    ## pak/pkgcache owns the directory: location resolved via R_USER_CACHE_DIR
    ## (captured at the subprocess's spawn time). pak creates the dir itself
    ## on first write, so `create = TRUE` is a no-op here.
    pakPath <- tryCatch(pak::cache_summary()$cachepath,
                        error = function(e) NULL)
    if (!is.null(pakPath) && nzchar(pakPath))
      return(normPathMemoise(pakPath))
    ## Fall through to legacy path if pak's cache_summary failed (e.g. under
    ## R CMD check where R_USER_CACHE_DIR is unset and pkgcache aborts).
  }

  pkgCacheDir <- normPathMemoise(file.path(cacheDir(create), "packages", versionMajorMinor()))
  if (isTRUE(create)) {
    pkgCacheDir <- checkPath(pkgCacheDir, create = TRUE)
  }
  return(pkgCacheDir)
}

## Internal: where Require's own bookkeeping files live (SHA DB,
## DESCRIPTION cache, pkgDepDB, mirrors.csv, available.packages cache).
## NOT the package tarball cache -- that's `cachePkgDir()`, which in pak
## mode is pak's own cache directory. Historically these files lived next
## to the tarballs at `<cacheDir>/packages/<Rver>`; this helper keeps them
## there so existing on-disk state is preserved across the upgrade that
## repurposed `cachePkgDir()` as a pak wrapper.
.requirePkgInfoDir <- function(create = FALSE) {
  d <- normPathMemoise(file.path(cacheDir(create), "packages", versionMajorMinor()))
  if (isTRUE(create))
    d <- checkPath(d, create = TRUE)
  d
}

removeOldFlatCachePkgs <- function(verbose = getOption("Require.verbose")) {
  pe <- pkgEnv()
  if (!is.null(pe[["oldFlatCacheChecked"]])) return(invisible(NULL))
  pe[["oldFlatCacheChecked"]] <- TRUE

  ## Migration helper for #143's flat-cache layout: tarballs that used to
  ## live directly in Require's `<cacheDir>/packages/<Rver>` before the
  ## repos-specific-subdir rework. After `cachePkgDir()` was repurposed
  ## as a pak wrapper, the flat dir is the bookkeeping dir, not pak's
  ## cache.
  flatDir <- .requirePkgInfoDir()
  if (!dir.exists(flatDir)) return(invisible(NULL))

  # Package files are directly in the flat dir (not in subdirs); subdirs belong to the new scheme
  allEntries <- dir(flatDir, full.names = TRUE)
  pkgPat <- "\\.tar\\.gz$|\\.zip$|\\.tgz$"
  oldFiles <- allEntries[!dir.exists(allEntries) & grepl(pkgPat, allEntries)]

  if (length(oldFiles)) {
    messageVerbose(
      "Removing ", length(oldFiles), " package file(s) from old flat cache location ",
      "(", flatDir, "); they will be re-downloaded into repos-specific subdirectories.",
      verbose = verbose, verboseLevel = 1
    )
    unlink(oldFiles)
  }
  invisible(NULL)
}

cachePkgDirForRepo <- function(repos, create = FALSE) {
  # Normalize to just protocol+host so that "https://cloud.r-project.org/src/contrib"
  # and "https://cloud.r-project.org" map to the same cache subdirectory.
  # Lives under Require's bookkeeping dir (legacy non-pak download path);
  # pak's own cache has its own per-repo layout we don't touch.
  normalized <- sub("^(https?://[^/]+).*", "\\1", repos)
  sanitized <- gsub("https|[:/]", "", normalized)
  d <- file.path(.requirePkgInfoDir(), sanitized)
  if (isTRUE(create)) {
    d <- vapply(d, checkPath, character(1), create = TRUE)
  }
  d
}

RequireGitHubCacheDir <- function(create) {
  if (missing(create)) {
    create <- FALSE
  }
  pkgCacheDir <- normPathMemoise(file.path(cacheDir(create), .txtGitHub))
  if (isTRUE(create)) {
    pkgCacheDir <- checkPath(pkgCacheDir, create = TRUE)
  }

  ## TODO: prompt the user ONCE about using this cache dir, and save their choice
  ##       - remind them how to change this, and make sure it's documented!

  return(pkgCacheDir)
}
#' Get the option for `Require.cachePkgDir` (deprecated)
#'
#' @description
#' Deprecated in favour of [cachePkgDir()], which is now the single getter
#' for the package-tarball cache (wraps `pak::cache_summary()$cachepath`
#' under `usePak = TRUE`). Honoured for one release cycle to preserve
#' behaviour when `options("Require.cachePkgDir")` or `R_REQUIRE_PKG_CACHE`
#' are user-set; otherwise just delegates to `cachePkgDir(FALSE)`.
#'
#' First checks if an environment variable `R_REQUIRE_PKG_CACHE` is
#' set and defines a path.
#' If not set, checks whether the `options("Require.cachePkgDir")` is set.
#' If a character string, then it returns that.
#' If `TRUE`, then use `cachePkgDir()`. If `FALSE`
#' then returns `NULL`.
#'
#' @export
cacheGetOptionCachePkgDir <- function() {
  .Deprecated("cachePkgDir", package = "Require",
              msg = paste0(
                "cacheGetOptionCachePkgDir() is deprecated; use cachePkgDir() ",
                "instead. To redirect pak's package cache, set R_USER_CACHE_DIR ",
                "in your .Renviron (not R_REQUIRE_PKG_CACHE)."
              ))
  curVal <- getOption("Require.cachePkgDir")
  try <- 1
  while (try < 3) {
    if (isTRUE(curVal)) {
      curVal <- cachePkgDir(FALSE)
      break
    } else if (isFALSE(curVal)) {
      curVal <- NULL
      break
    } else {
      if (identical("default", curVal)) {
        fromEnvVars <- Sys.getenv("R_REQUIRE_PKG_CACHE")
        if (nchar(fromEnvVars) == 0) {
          curVal <- cachePkgDir(FALSE)
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
                  RPackageCache = cacheGetOptionCachePkgDir(),
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
#' @param removePackages Deprecated. Please remove packages manually from `.libPaths()`
setupOff <- function(removePackages = FALSE, verbose = getOption("Require.verbose")) {
  updateRprofile <- checkTRUERprofile(TRUE)
  if (!file.exists(updateRprofile)) { # not in current dir
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
#' @importFrom utils read.csv
#' @export
setLinuxBinaryRepo <- function(binaryLinux = urlForArchivedPkgs,
                               backupCRAN = srcPackageURLOnCRAN) {
  if (isUbuntuOrDebian()) {
    if (!grepl("R Under development", R.version.string) && getRversion() >= "4.1") {
      if (is.null(names(backupCRAN))) names(backupCRAN) <- rep("CRAN", length(backupCRAN))

      repo <- c(
        CRAN =
          positBinaryRepos()
      )

      currentRepos <- getOption("repos")
      insertBefore <- 1 # put first, unless otherwise
      if (!is.null(currentRepos)) {
        isCRAN <- whIsOfficialCRANrepo(currentRepos, srcPackageURLOnCRAN)
        # mirrorsLocalFile <- file.path(cachePkgDir(), ".mirrors.csv")
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
        if (isTRUE(insertBefore > 1)) { # could have no CRAN official mirror
          repos <- c(currentRepos[seq(1, insertBefore - 1)] ,
                     repo,
                     currentRepos[seq(insertBefore, length(currentRepos))])
        }
      } else {
        repos <- c(repo, backupCRAN)
      }

      repos <- repos[!duplicated(repos)]
      options(repos = repos)
    }
  }
}

whIsOfficialCRANrepo <- function(currentRepos = getOption("repos"), backupCRAN = srcPackageURLOnCRAN) {
  ## mirrors.csv is Require-internal bookkeeping; lives next to other
  ## Require state, not in pak's tarball cache.
  mirrorsLocalFile <- file.path(.requirePkgInfoDir(create = TRUE), ".mirrors.csv")

  for (attempt in 1:3) {
    if (!file.exists(mirrorsLocalFile))
      download.file("https://cran.r-project.org/CRAN_mirrors.csv",
                    destfile = mirrorsLocalFile, quiet = TRUE)
    a <- try(read.csv(mirrorsLocalFile), silent = TRUE)
    if (!is(a, "try-error"))
      break
    unlink(mirrorsLocalFile)
    if (attempt == 2) {
      # https://stackoverflow.com/a/76684292/3890027
      enableSSLWorkaround()
    }
    if (attempt == 3) {
      optsHere <- options(download.file.method = "curl")
      on.exit(options(optsHere))
    }
  }
  b <- a[1, ]
  b$URL <- "https://cran.rstudio.com/"
  a <- rbind(a, b)
  isCRAN <- lapply(gsub("https://", "", currentRepos),
                   grep, x = gsub("https://", "", a$URL), value = TRUE)
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

#' The default cache directory for Require Cache
#'
#' A wrapper around `tools::R_user_dir("Require", which = "cache")` that
#' creates the directory, if it does not exist.
#'
#' @return The default cache directory
#'
#' @importFrom tools R_user_dir
#' @export
cacheDefaultDir <- function() {
  normalizePath(tools::R_user_dir("Require", which = "cache"), mustWork = FALSE)
}

appName <- "R-Require"

defaultCacheDirOld <- switch(SysInfo[["sysname"]],
  Darwin = normalizePath(file.path("~", "Library", "Caches", appName), mustWork = FALSE),
  Linux = normalizePath(file.path("~", ".cache", appName), mustWork = FALSE),
  Windows = normalizePath(file.path("C:", "Users", SysInfo[["user"]], "AppData", "Local", ".cache", appName), mustWork = FALSE)
)
