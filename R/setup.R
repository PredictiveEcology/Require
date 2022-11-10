#' Path to (package) cache directory
#'
#' Sets or gets the cache directory associated with the `Require` package.
#' @return
#' If `!is.null(getOptionRPackageCache())`, i.e., a cache path exists,
#' the cache directory will be created,
#'   with a README placed in the folder. Otherwise, this function will just
#'   return the path of what the cache directory would be.
#' @inheritParams checkPath
#' @export
#' @rdname RequireCacheDir
RequireCacheDir <- function(create) {
  if (missing(create))
    create <- FALSE # !is.null(getOptionRPackageCache())

  ## use cache dir following OS conventions used by rappdirs package:
  ## rappdirs::user_cache_dir(appName)

  cacheDir <- if (nzchar(Sys.getenv("R_USER_CACHE_DIR"))) {
    Sys.getenv("R_USER_CACHE_DIR")
  } else {
    if (!is.null(defaultCacheDirOld)) { # solaris doesn't have this set
      if (dir.exists(defaultCacheDirOld)) {
        oldLocs <- dir(defaultCacheDirOld, full.names = TRUE, recursive = TRUE)
        if (length(oldLocs) > 0) {
          message("Require has changed default package cache folder from\n",
                  defaultCacheDirOld, "\nto \n", defaultCacheDir, ". \nThere are packages ",
                  "in the old Cache, moving them now...")
          checkPath(defaultCacheDir, create = TRUE)
          dirs <- unique(dirname(oldLocs))
          newdirs <- gsub(defaultCacheDirOld, defaultCacheDir, dirs)
          lapply(newdirs, checkPath, create = TRUE)
          fileRenameOrMove(oldLocs, gsub(defaultCacheDirOld, defaultCacheDir, oldLocs))
          unlink(defaultCacheDirOld, recursive = TRUE)
        }
      }
    }
    defaultCacheDir
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
    if (!exists(fnName, envir = .pkgEnv, inherits = FALSE))
      .pkgEnv[[fnName]] <- new.env()
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
RequirePkgCacheDir <- function(create) {
  if (missing(create)) {
    create <- FALSE # !is.null(getOptionRPackageCache())
  }
  pkgCacheDir <- normPathMemoise(file.path(RequireCacheDir(create), "packages", rversion()))
  if (isTRUE(create))
    pkgCacheDir <- checkPath(pkgCacheDir, create = TRUE)

  ## TODO: prompt the user ONCE about using this cache dir, and save their choice
  ##       - remind them how to change this, and make sure it's documented!

  return(pkgCacheDir)
}

#' Get the option for `Require.RPackageCache`
#'
#' First checks if an environment variable `Require.RPackageCache` is set and defines a path.
#' If not set, checks whether the `options("Require.RPackageCache")` is set.
#' If a character string, then it returns that.
#' If `TRUE`, then use `RequirePkgCacheDir()`. If `FALSE` then returns `NULL`.
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
        if (nchar(fromEnvVars) == 0  ) {
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
#' This can be placed as the first line of any/all scripts and it will
#' be create a reproducible, self-contained project with R packages.
#' Some of these have direct relationships with `RequireOptions`
#' and arguments in `setLibPaths` and `Require`.
#'
#' @param RPackageFolders One or more folders where R packages are
#'   installed to and loaded from. In the case of more than one
#'   folder provided, installation will only happen in the first one.
#'
#' @param RPackageCache See `?RequireOptions`.
#'
#' @param buildBinaries See `?RequireOptions`.
#'
#' @inheritParams setLibPaths
#' @inheritParams Require
#'
#' @export
#' @rdname setup
#'
#' @examples
#' \dontrun{
#' opts <- options("Require.RequirePkgCache" = FALSE) # don't use cache for examples
#' # Place this as the first line of a project
#' Require::setup()
#'
#' # To turn it off and return to normal
#' Require::setupOff()
#' options(opts) # replace original value for the cache option
#' }
#'
setup <- function(RPackageFolders = getOption("Require.RPackageFolders", "R"),
                  RPackageCache = getOptionRPackageCache(),
                  buildBinaries = getOption("Require.buildBinaries", TRUE),
                  standAlone = getOption("Require.standAlone", TRUE),
                  verbose = getOption("Require.verbose")) {
  RPackageFolders <- checkPath(RPackageFolders, create = TRUE)
  RPackageCache <- checkPath(RPackageCache, create = TRUE)
  copyRequireAndDeps(RPackageFolders, verbose = verbose)

  newOpts <- list("Require.RPackageCache" = RPackageCache,
                  "Require.buildBinaries" = buildBinaries)#,
                  #"Require.useCranCache" = usingCranCache)
  opts <- options(newOpts)
  co <- capture.output(type = "message",
                       setLibPaths(RPackageFolders, standAlone = standAlone,
                                   updateRprofile = TRUE))
  if (!any(grepl(alreadyInRprofileMessage, co)))
    silence <- lapply(co, messageVerbose, verbose = verbose, verboseLevel = 1)

  ro <- RequireOptions()
  roNames <- names(newOpts)
  names(roNames) <- roNames
  nonStandardOpt <- !unlist(lapply(roNames, function(optNam) identical(ro[[optNam]], opts[[optNam]])))
  nonStandardOpt[] <- TRUE # OVERRIDE -- MAKE THEM ALL EXPLICIT IN .Rprofile
  if (any(nonStandardOpt)) {
    setLibPathsUpdateRprofile(.libPaths()[1])
    rp <- readLines(".Rprofile")
    lineWithPrevious <- grepl("### Previous", rp)
    if (any(lineWithPrevious)) {
      lineWithPrevious <- which(lineWithPrevious)
      post <- seq(length(rp) - lineWithPrevious) + lineWithPrevious
      pre <- seq(lineWithPrevious)
      nameNonStandards <- names(nonStandardOpt)[nonStandardOpt]
      optsToAdd <- unlist(lapply(nameNonStandards, function(nns) {
        paste0("### Previous option: ", nns, " = ", opts[[nns]])
      }))
      newOptsToAdd <- unlist(lapply(nameNonStandards, function(nns) {
        paste0("options('", nns, "' = '", newOpts[[nns]], "')")
      }))
      newRP <- c(rp[pre], optsToAdd, newOptsToAdd, rp[post])
      cat(newRP, file = ".Rprofile", sep = "\n")
    }
  }
}

#' @rdname setup
#' @inheritParams Require
#' @export
#' @param removePackages Logical. If `TRUE`, then all packages that
#'   were installed in the custom library will be deleted when `setupOff`
#'   is run. The default is `FALSE`, and when `TRUE` is selected,
#'   and it is an interactive session, the user will be prompted to confirm
#'   deletions.
setupOff <- function(removePackages = FALSE, verbose = getOption("Require.verbose")) {

  lps <- .libPaths()
  if (file.exists(".Rprofile")) {
    rp <- readLines(".Rprofile")
    lineWithPrevious <- grepl("### Previous option", rp)

    #options(RequireOptions())
    #options(getOptionRPackageCache()) # This one may have a Sys.getenv that is different
    if (any(lineWithPrevious)) {
      lineWithPrevious <- which(lineWithPrevious)
      silence <- lapply(lineWithPrevious, function(lwp) {
        opt <- gsub("### Previous option: ", "", rp[lwp])
        opt <- strsplit(opt, " = ")[[1]]
        newOpt <- list(opt[2])
        names(newOpt) <- opt[1]
        options(newOpt)
      })
    }
    setLibPaths()
    if (isTRUE(removePackages)) {
      if (interactive() && (verbose >= 1)) { # don't even ask if verbose is low
        messageVerbose("You have requested to remove all packages in ", lps[1],
                       verbose = verbose, verboseLevel = 1)
        out <- readline("Is this correct? Y (delete all) or N (do not delete all)")
        if (identical(tolower(out), "n"))
          removePackages <- FALSE
      }
      if (isTRUE(removePackages))
        unlink(lps[1], recursive = TRUE)
    }
  } else {
    messageVerbose("Project is not setup yet; nothing to do",
                   verbose = verbose, verboseLevel = 0)
  }
}

copyRequireAndDeps <- function(RPackageFolders, verbose = getOption("Require.verbose")) {
  lps <- .libPaths()
  names(lps) <- lps
  pkgs <- c("Require", "data.table")
  for (pkg in pkgs) {
    theNewPath <- file.path(rpackageFolder(RPackageFolders), pkg)
    newPathExists <- dir.exists(theNewPath)
    for (lp in lps) {
      thePath <- file.path(lp, pkg)
      pkgInstalledAlready <- dir.exists(thePath)
      if (isTRUE(pkgInstalledAlready)) {
        fromFiles <- dir(thePath, recursive = TRUE, full.names = TRUE)
        if (!newPathExists) {
          messageVerbose("Placing copy of ", pkg, " in ", RPackageFolders,
                  verbose = verbose, verboseLevel = 2)
          dirs <- unique(dirname(fromFiles))
          dirs <- gsub(thePath, theNewPath, dirs)
          lapply(dirs, checkPath, create = TRUE)
        }

        toFiles <- gsub(thePath, theNewPath, fromFiles)

        if (newPathExists) {
          newPathVersion <- DESCRIPTIONFileVersionV(file.path(theNewPath, "DESCRIPTION"))
          oldPathVersion <- DESCRIPTIONFileVersionV(file.path(thePath, "DESCRIPTION"))
          comp <- compareVersion(newPathVersion, oldPathVersion)
          if (comp > -1) break
          messageVerbose("Updating version of ", pkg, " in ", RPackageFolders,
                  verbose = verbose, verboseLevel = 1)
          unlink(toFiles)
        }
        linkOrCopy(fromFiles, toFiles)
        break
      }
    }
  }
}

#' Setup for binary Linux repositories
#'
#' Enable use of binary package builds for Linux from the RStudio Package Manager repo.
#' This will set the `repos` option, affecting the current R session. It will put this
#' `binaryLinux` in the first position. If the `getOption("repos")` is `NULL`, it will
#' put `backupCRAN` in second position.
#'
#' @param binaryLinux A CRAN repository serving binary Linux packages.
#' @param backupCRAN If there is no CRAN repository set
#'
#' @export
setLinuxBinaryRepo <- function(binaryLinux = "https://packagemanager.rstudio.com/",
                               backupCRAN = srcPackageURLOnCRAN) {
  if (SysInfo["sysname"] == "Linux" && grepl("Ubuntu", utils::osVersion)) {
    if (!grepl("R Under development", R.version.string) && getRversion() >= "4.1") {
      repo <- c(CRAN =
                  paste0(binaryLinux, "all/__linux__/", system("lsb_release -cs", intern = TRUE), "/latest"))
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

putFile <- function(from, to, overwrite) {
  if (file.exists(to)) {
    if (isTRUE(overwrite)) {
      res0 <- file.copy(to, paste0(to, ".bak.", timestamp()))
      res1 <- file.copy(from, to)
    } else {
      message("file ", to, " exists but overwrite not TRUE. Not overwriting.")
    }
  } else {
    res1 <- file.copy(from, to)
  }
}

appName <- "R-Require"

#' @importFrom tools R_user_dir
defaultCacheDir <- normalizePath(tools::R_user_dir("Require", which = "cache"), mustWork = FALSE)

defaultCacheDirOld <- switch(
  SysInfo[["sysname"]],
  Darwin = normalizePath(file.path("~", "Library", "Caches", appName), mustWork = FALSE),
  Linux = normalizePath(file.path("~", ".cache", appName), mustWork = FALSE),
  Windows = normalizePath(file.path("C:", "Users", SysInfo[["user"]], "AppData", "Local", ".cache", appName), mustWork = FALSE)
)
