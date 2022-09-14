
#' Path to (package) cache directory
#'
#' @export
#' @rdname RequireCacheDir
RequireCacheDir <- function() {
  ## cache dirs based on rappdirs::user_cache_dir()
  appName <- "R-Require"
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
  checkPath(cacheDir, create = TRUE)
}

#' @export
#' @rdname RequireCacheDir
RequirePkgCacheDir <- function() {
  checkPath(file.path(RequireCacheDir(), "packages", rversion()), create = TRUE)
}

#' Setup a project library, cache, options
#'
#' This can be placed as the first line of any/all scripts and it will
#' be create a reproducible, self-contained project with R packages.
#' Some of these have direct relationships with `RequireOptions`
#' and arguments in `setLibPaths` and `Require`.
#' @param RPackageFolders One or more folders where R packages are
#'   installed to and loaded from. In the case of more than one
#'   folder provided, installation will only happen in the first one.
#' @param RPackageCache See `?RequireOptions`.
#' @param buildBinaries See `?RequireOptions`.
#' @inheritParams setLibPaths
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
                  standAlone = getOption("Require.standAlone", TRUE)) {
  RPackageFolders <- checkPath(RPackageFolders, create = TRUE)
  RPackageCache <- checkPath(RPackageCache, create = TRUE)
  copyRequireAndDeps(RPackageFolders)

  newOpts <- list("Require.RPackageCache" = RPackageCache,
                  "Require.buildBinaries" = buildBinaries)#,
                  #"Require.useCranCache" = usingCranCache)
  opts <- options(newOpts)
  co <- capture.output(type = "message",
                       setLibPaths(RPackageFolders, standAlone = standAlone,
                                   updateRprofile = TRUE))
  if (!any(grepl(alreadyInRprofileMessage, co)))
    if (getOption("Require.setupVerbose", TRUE))
      silence <- lapply(co, message)
  ro <- RequireOptions()
  roNames <- names(newOpts)
  names(roNames) <- roNames
  nonStandardOpt <- !unlist(lapply(roNames, function(optNam) identical(ro[[optNam]], opts[[optNam]])))
  if (any(nonStandardOpt)) {
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
#' @export
#' @param removePackages Logical. If `TRUE`, then all packages that
#'   were installed in the custom library will be deleted when `setupOff`
#'   is run. The default is `FALSE`, and when `TRUE` is selected,
#'   and it is an interactive session, the user will be prompted to confirm
#'   deletions.
setupOff <- function(removePackages = FALSE) {
  lps <- .libPaths()
  if (file.exists(".Rprofile")) {
    rp <- readLines(".Rprofile")
    lineWithPrevious <- grepl("### Previous option", rp)
    options(RequireOptions())
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
      if (interactive() && getOption("Require.setupVerbose", TRUE) ) {
        message("You have requested to remove all packages in ", lps[1])
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

copyRequireAndDeps <- function(RPackageFolders) {
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
          if (getOption("Require.setupVerbose", TRUE))
            message("Placing copy of ", pkg, " in ", RPackageFolders)
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
          if (getOption("Require.setupVerbose", TRUE))
            message("Updating version of ", pkg, " in ", RPackageFolders)
          unlink(toFiles)
        }
        file.link(fromFiles, toFiles)
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

