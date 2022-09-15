
#' Path to (package) cache directory
#'
#' @export
#' @rdname RequireCacheDir
RequireCacheDir <- function() {
  appName <- "R-Require"

  ## use cache dir following OS conventions used by rappdirs package:
  ## rappdirs::user_cache_dir(appName)

  cacheDir <- if (nzchar(Sys.getenv("R_USER_CACHE_DIR"))) {
    Sys.getenv("R_USER_CACHE_DIR")
  } else {
    switch(
      Sys.info()[["sysname"]],
      Darwin = file.path("~", "Library", "Caches", appName),
      Linux = file.path("~", ".cache", appName),
      Windows = file.path("C:", "Users", Sys.info()[["user"]], "AppData", "Local", ".cache", appName)
    )
  }
  cacheDir <- checkPath(cacheDir, create = TRUE)

  readme <- file.path(cacheDir, "README")
  if (!file.exists(readme)) {
    file.copy(system.file("cache-README", package = "Require"), readme)
  }

  return(cacheDir)
}

#' @export
#' @rdname RequireCacheDir
RequirePkgCacheDir <- function() {
  pkgCacheDir <- checkPath(file.path(RequireCacheDir(), "packages", rversion()), create = TRUE)

  ## TODO: prompt the user ONCE about using this cache dir, and save their choice
  ##       - remind them how to change this, and make sure it's documented!

  return(pkgCacheDir)
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
#' # Place this as the first line of a project
#' Require::setup()
#'
#' # To turn it off and return to normal
#' Require::setupOff()
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
    options(RequireOptions())
    options(getOptionRPackageCache()) # This one may have a Sys.getenv that is different
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
    message("Project is not setup yet; nothing to do")
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
          message("Placing copy of ", pkg, " in ", RPackageFolders,
                  verbose = verbose, verboseLevel = 1)
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
          message("Updating version of ", pkg, " in ", RPackageFolders,
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
#' Enable use of binary package builds for Linux from the Rstudio Package Manager repo.
#' This will set the `repos` option, affecting the current R session.
#'
#' @param binaryLinux A CRAN repository serving binary Linux packages.
#'
#' @export
setLinuxBinaryRepo <- function(binaryLinux = "https://packagemanager.rstudio.com/") {
  if (Sys.info()["sysname"] == "Linux" && grepl("Ubuntu", utils::osVersion)) {
    if (!grepl("R Under development", R.version.string) && getRversion() >= "4.1") {
      options(
        repos = c(
          paste0(binaryLinux, "all/__linux__/", system("lsb_release -cs", intern = TRUE), "/latest")
        )
      )
    }
  }
}

#' Use cache for R package compilation
#'
#' Simple [`ccache`](https://ccache.dev/) configuration for compiling R packages on Linux,
#' based on <http://dirk.eddelbuettel.com/blog/2017/11/27/#011_faster_package_installation_one>.
#'
#' @note This is typically run once per user, per machine to configure the cache.
#'
#' @param overwrite Logical. Should the existing configuration be overwritten?
#'                  For safety, a backup copy of the old configuration is created.
#'
#' @return Invoked for the side effect of copying files needed to configure `ccache` for R packages.
#'
#' @author Dirk Eddelbuettel and Alex Chubaty
#' @export
#'
#' @examples
#' \dontrun{
#'  useLinuxSourceCache()
#' }
useLinuxSourceCache <- function(overwrite = FALSE) {
  if (identical(Sys.info()[["sysname"]], "Linux")) {
    hasccache <- nzchar(Sys.which("ccache"))
    if (isTRUE(hasccache)) {
      confFile <- file.path("~", ".ccache", "ccache.conf")
      putFile(system.file("dotccache/ccache.conf", package = "Require"), confFile, overwrite)

      makevarsFile <- file.path("~", ".R", "Makevars")
      putFile(system.file("dotR/Makevars", package = "Require"), makevarsFile, overwrite)
    } else {
      warning("'ccache' not found. Is it installed? Try e.g., 'sudo apt install ccache'.")
    }
  } else {
    message("Setting up ccache for R package compilation is currently only supported on Linux.")
  }

  invisible()
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
