#' Setup a project library, cache, options
#' 
#' This can be placed as the first line of any/all scripts and it will
#' be create a reproducible, self-contained project with R packages. 
#' Some of these have direct relationships with \code{RequireOptions}
#' and arguments in \code{setLibPaths} and \code{Require}.
#' @param RPackageFolders One or more folders where R packages are 
#'   installed to and loaded from. In the case of more than one 
#'   folder provided, installation will only happen in the first one.
#' @param RPackageCache See \code{?RequireOptions}.
#' @param buildBinaries See \code{?RequireOptions}.
#' @inheritParams setLibPaths
#' @export
#' @rdname setup
#' 
#' @examples 
#' # Place this as the first line of a project
#' \dontrun{
#' Require::setup()
#' 
#' # To turn it off and return to normal
#' Require::setupOff()
#' }
#'   
setup <- function(RPackageFolders = getOption("RPackageFolders", "R"), 
                  RPackageCache = getOption("RPackageCache", "~/.cache/R/RequirePkgCache"), 
                  buildBinaries = getOption("buildBinaries", TRUE), 
                  standAlone = getOption("standAlone", TRUE)) {
  RPackageFolders <- checkPath(RPackageFolders, create = TRUE)
  RPackageCache <- checkPath(RPackageCache, create = TRUE)
  # hasCranCache <- any(dir.exists(file.path(.libPaths(), "crancache")))
  # usingCranCache <- FALSE
  # 
  # if (hasCranCache && !isFALSE(getOption("Require.useCranCache"))) { # if NULL or TRUE go here
  #   rvn <- paste0(R.version$major, '.', strsplit(R.version$minor, split = '\\.')[[1]][1])
  #   os <- tolower(.Platform$OS.type)
  #   extra <- file.path('cran','bin', os,'contrib', rvn, fsep = "/")
  #   usingCranCache <- !endsWith(RPackageCache, extra)
  #   if (usingCranCache) {
  #     if (interactive()) {
  #       message("crancache is installed; would you like to have Require and ",
  #               "crancache share the cache? If N, then Require will use ",
  #               RPackageCache)
  #       useSameCache <- readline("Use same cache? (Y or N)")
  #       if (identical(tolower(useSameCache), "y")) {
  #         stop(paste0("To use crancache cached packages, please rerun:\n",
  #                     "setup(RPackageCache = normalizePath(file.path(crancache::get_cache_dir(),'",extra,"'),
  #                            winslash = '/'))"))
  #       }
  #     }
  #   } else {
  #     usingCranCache <- TRUE
  #   }
  # }
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
#' @param removePackages Logical. If \code{TRUE}, then all packages that
#'   were installed in the custom library will be deleted when \code{setupOff}
#'   is run. The default is \code{FALSE}, and when \code{TRUE} is selected,
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
  pkgs <- c("Require", "remotes", "data.table")
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