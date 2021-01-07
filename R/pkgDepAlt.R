utils::globalVariables(c(
  "..whichAll", "destFile", "filepath", "pkg"
))

#' @description 
#' 
#' \code{pkgDepAlt} is a newer, still experimental approach to \code{pkgDep}, which has different
#' internal algorithms. With current testing, it appears to be slightly more accurate for
#' (some unknown, as of yet) edge cases. One known case is when the a \code{package} is
#' installed locally package, but is 
#' not the version that is requested with \code{pkgDep}, the function will default to the
#' local, installed, and incorrect package dependencies. \code{pkgDepAlt} gets this case 
#' correct. This function may eventually replace \code{pkgDep}.
#' 
#' @export
#' @rdname pkgDep
pkgDepAlt <- function(packages, libPath = .libPaths(),
                   which = c("Depends", "Imports", "LinkingTo", "Remotes"), recursive = FALSE,
                   depends, imports, suggests, linkingTo, enhances, remotes,
                   repos = getOption("repos"),
                   keepVersionNumber = TRUE, includeBase = FALSE,
                   sort = TRUE, purge = getOption("Require.purge", FALSE)) {
  
  origDTThreads <- data.table::setDTthreads(1)
  on.exit(data.table::setDTthreads(origDTThreads))

  purge <- dealWithCache(purge)
  
  pkgDT <- list(packageFullName = packages, Package = extractPkgName(packages))
  pkgDT <- setDT(pkgDT)
  if (!includeBase) pkgDT <- pkgDT[!pkgDT$Package %in% .basePkgs]
  
  
  if (!includeBase) packages <- packages[!packages %in% .basePkgs]
  #if (any(!missing(depends), !missing(linkingTo), !missing(imports), !missing(suggests))) {
  if (!missing(depends)) {wh <- "Depends"; if (isTRUE(depends)) which <- unique(c(which, wh)) else setdiff(which, wh)}
  if (!missing(imports)) {wh <- "Imports"; if (isTRUE(imports)) which <- unique(c(which, wh)) else setdiff(which, wh)}
  if (!missing(suggests)) {wh <- "Suggests"; if (isTRUE(suggests)) which <- unique(c(which, wh)) else setdiff(which, wh)}
  if (!missing(linkingTo)) {wh <- "LinkingTo"; if (isTRUE(linkingTo)) which <- unique(c(which, wh)) else setdiff(which, wh)}
  
  which <- whichToDILES(which)
  whichLC <- tolower(which[[1]])
  maxIterations <- if (isTRUE(recursive)) Inf else 1
  
  # Initiate pkgDTDeps
  i <- 1
  
  pkgDTDeps <- list()
  pkgDTDeps[[i]] <- pkgDT
  set(pkgDTDeps[[i]], NULL, "PackageTopLevel", pkgDTDeps[[i]]$packageFullName)
  set(pkgDTDeps[[i]], NULL, "PackageVersion", concatPkgVersion(pkgDTDeps[[i]]$Package, pkgDTDeps[[i]]$Version))
  set(pkgDTDeps[[i]], NULL, "which", "TopLevel")
  set(pkgDTDeps[[i]], NULL, "Package", extractPkgName(pkgDTDeps[[i]]$packageFullName))
  set(pkgDTDeps[[i]], NULL, "hasVersionSpec", grepl(.grepVersionNumber, pkgDTDeps[[i]]$packageFullName))
  
  pkgDTComplete <- list()
  pkgDTComplete[[i]] <- data.table::copy(pkgDTDeps[[i]] )
  set(pkgDTComplete[[i]], NULL, "Package", NULL)
  
  final <- list()
  
  # If a complete version previous, then use it
  namSR <- saveNameOuter(pkgDTDeps[[i]]$packageFullName, recursive, which) 
  names(namSR) <- namSR
  stashed_recursive <- lapply(namSR, function(x) {
    get0(x, envir = .pkgEnv[["pkgDep"]][["deps"]])
  })
  stillNeed_recursive <- unlist(lapply(stashed_recursive, is.null))
  if (sum(stillNeed_recursive) > 0) {
    if (sum(!stillNeed_recursive) > 0) {
      pkgDTDeps[[i]] <- pkgDTDeps[[i]][stillNeed_recursive]
      pkgDTComplete[[i]] <- pkgDTComplete[[i]][stillNeed_recursive]
    }
  } else {
    pkgDTDeps[[i]] <- pkgDTDeps[[i]][0]
    pkgDTComplete[[i]] <- pkgDTComplete[[i]][0]
  }
  
  while ((NROW(pkgDTDeps[[i]]) & recursive) || (recursive == FALSE & NROW(pkgDTDeps[[i]]) == 1)) {
    pkgDTSrc <- list()
    browser(expr = exists("._pkgDep3_2"))
    pfn <- pkgDTDeps[[i]]$packageFullName
    names(pfn) <- pfn
    stashed <- lapply(pfn, function(x) {
      get0(paste0(x), envir = .pkgEnv[["pkgDep"]][["deps"]])
    })
    stillNeed <- unlist(lapply(stashed, is.null))
    if (any(!stillNeed))
      names(stashed)[!stillNeed] <- concatPkgVersion(
        names(stashed)[!stillNeed], pkgDTDeps[[i]][!stillNeed]$PackageTopLevel)
    if (sum(stillNeed) > 0) {
      if (sum(!stillNeed) > 0) {
        pkgDTDeps[[i]] <- pkgDTDeps[[i]][stillNeed]
      }
    } else {
      pkgDTDeps[[i]] <- pkgDTDeps[[i]][0]
    }
    
    deps <- list()
    
    if (NROW(pkgDTDeps[[i]])) {
      pkgDTDeps[[i]] <- installedVers(pkgDTDeps[[i]])
      pkgDTDeps[[i]] <- parseGitHub(pkgDTDeps[[i]])
      pkgDTDeps[[i]] <- getPkgVersions(pkgDTDeps[[i]], install = TRUE)
      pkgDTDeps[[i]] <- getAvailable(pkgDTDeps[[i]], purge = FALSE, repos = repos)
      
      # Check Local -- either correct version available or no version spec & installed
      locals <- (pkgDTDeps[[i]]$correctVersion == TRUE | 
                   (is.na(pkgDTDeps[[i]]$correctVersion) & !is.na(pkgDTDeps[[i]]$Version))) & 
        pkgDTDeps[[i]]$repoLocation != "GitHub"
      pkgDTSrc[["Local"]] <- pkgDTDeps[[i]][locals]
      if (NROW(pkgDTSrc[["Local"]])) {
        desc_paths <- file.path(pkgDTSrc[["Local"]]$LibPath, pkgDTSrc[["Local"]]$Package, "DESCRIPTION")
        names(desc_paths) <- concatPkgVersion(pkgDTSrc[["Local"]]$packageFullName, 
                                              pkgDTSrc[["Local"]]$PackageTopLevel)
        deps[["Local"]] <- DESCRIPTIONFileDepsV(desc_paths, keepVersionNumber = TRUE, purge = FALSE, 
                                                which = whichAllGitHub,
                                                keepSeparate = TRUE)
      } 
      
      pkgDTOther <- pkgDTDeps[[i]][!locals | is.na(locals)]
      if (NROW(pkgDTOther)) {
        pkgDTSrc[["GitHub"]] <- pkgDTOther[pkgDTOther$repoLocation == "GitHub"]
        if (NROW(pkgDTSrc[["GitHub"]])) {
          pkgDTSrc[["GitHub"]] <- getGitHubDESCRIPTION(pkgDTSrc[["GitHub"]], purge = FALSE)
          desc_paths <- pkgDTSrc[["GitHub"]]$DESCFile
          bnDESCFile <- basename(desc_paths)
          names(desc_paths) <- concatPkgVersion(pkgDTSrc[["GitHub"]]$packageFullName,
                                                pkgDTSrc[["GitHub"]]$PackageTopLevel)
          deps[["GitHub"]] <- DESCRIPTIONFileDepsV(desc_paths, which = whichAllGitHub, purge = FALSE,
                                                   keepSeparate = TRUE)
          pkgDTSrc[["GitHub"]] <- getGitHubNamespace(pkgDTSrc[["GitHub"]], purge = FALSE)
          depsGitHub2 <- NAMESPACEFileDepsV(pkgDTSrc[["GitHub"]]$NAMESPACE, purge = FALSE)
          deps[["GitHub"]] <- Map(d1 = deps[["GitHub"]], d2 = depsGitHub2, function(d2, d1) {
            d1$imports <- union(d2, d1$imports)
            # if (!includeBase)
            #   d1$imports <- setdiff(d1$imports, .basePkgs)
            d1
          })
        }
        
        pkgDTSrc[["CRAN"]] <- pkgDTOther[pkgDTOther$repoLocation == "CRAN"]
        if (NROW(pkgDTSrc[["CRAN"]])) {
          pkgs <- pkgDTSrc[["CRAN"]]$packageFullName
          names(pkgs) <- concatPkgVersion(pkgDTSrc[["CRAN"]]$packageFullName,
                                          pkgDTSrc[["CRAN"]]$PackageTopLevel)
          deps[["CRAN"]] <- pkgDepCRAN2(packageFullName = pkgs, which = whichAll,
                                        Package = pkgDTSrc[["CRAN"]]$Package, 
                                        PackageTopLevel = pkgDTSrc[["CRAN"]]$PackageTopLevel,
                                        keepVersionNumber = TRUE, repos = getOption("repos"),
                                        purge = FALSE,
                                        keepSeparate = TRUE) 
        }
        
        #############
        pkgDTSrc[["Archive"]] <- pkgDTOther[pkgDTOther$repoLocation == "Archive"]
        if (NROW(pkgDTSrc[["Archive"]])) {
          # ava <- archiveVersionsAvailable(pkgDTSrc[["Archive"]]$Package, repos = repos)
          deps[["Archive"]] <- pkgDepArchive(pkgDTSrc[["Archive"]], repos = repos, purge = FALSE)
        }
      }
    }
    depsList <- do.call(c, unname(deps))
    if (!is.null(depsList))
      MapOut <- Map(deps = depsList, nam = paste0(gsub("^(.+)\\___.*", "\\1", names(depsList))),
                    function(deps, nam) {
                      if (!exists(nam, envir = .pkgEnv[["pkgDep"]][["deps"]])) {
                        assign(nam, deps, envir = .pkgEnv[["pkgDep"]][["deps"]])
                      }
                    })
    if (any(!stillNeed)) {
      depsList <- append(depsList, stashed[!stillNeed])
    }
    # message(sum(!stillNeed), " not rerun, out of ", length(stillNeed))
    pkgDTDeps[[i + 1]] <- rbindlist(
      Map(x = depsList, PackageTopLevel = gsub("^.+\\___(.*)", "\\1", names(depsList)),
          function(x, PackageTopLevel) {
            nams <- names(x)
            nams <- match(tolower(which[[1]]), tolower(nams))
            l <- list(packageFullName = unlist(x[nams], recursive = FALSE))
            l[["PackageTopLevel"]] = rep(PackageTopLevel, length(l[["packageFullName"]]))
            l[["which"]] = rep(names(x[nams]), unlist(lapply(x[nams], length)))
            l
          }
      ), 
      idcol = "PackageVersion")
    
    # Clean up -- R, \n\t
    if (NROW(pkgDTDeps[[i + 1]]) > 0) { 
      pkgDTDeps[[i + 1]] <- cleanUp(pkgDTDeps[[i + 1]], includeBase = includeBase)
    }
    
    if (NROW(pkgDTDeps[[i + 1]])) {
      # Put "remotes" first, because it has more information than all others
      if (length(unique(pkgDTDeps[[i + 1]]$which)) > 1) {
        set(pkgDTDeps[[i + 1]], NULL, "whichFac", 
            factor(pkgDTDeps[[i + 1]]$which, 
                   levels = c("remotes", "depends", "imports", "suggests", "linkingto", "enhances")))
        setorderv(pkgDTDeps[[i + 1]], "whichFac")
        set(pkgDTDeps[[i + 1]], NULL, "whichFac", NULL)
      }
      
      # if there are duplicates within a 
      set(pkgDTDeps[[i + 1]], NULL, "hasVersionSpec", grepl(.grepVersionNumber, pkgDTDeps[[i + 1]]$packageFullName))
      
      pkgDTDeps[[i + 1]] <- keepOnlyMaxVersion(pkgDTDeps[[i + 1]])
    }
    pkgDTComplete[[i + 1]] <- data.table::copy(pkgDTDeps[[i + 1]])
    
    if (NROW(pkgDTComplete[[i + 1]]) == 0)  {
      break
    }
    # Deal with duplicates
    
    # alreadyDone <- pkgDTDeps[[i + 1]]$packageFullName %in% 
    #   unlist(lapply(pkgDTDeps[seq(i)], function(x) x$packageFullName))
    
    # if (any(alreadyDone)) { 
    #   pkgDTDeps[[i + 1]] <- pkgDTDeps[[i + 1]][alreadyDone == FALSE]
    # } else if (all(alreadyDone)) { 
    #   pkgDTDeps[[i + 1]] <- pkgDTDeps[[i + 1]][0] 
    # }
    # pkgDTDeps[[i + 1]] <- pkgDTDeps[[i + 1]][!duplicated(pkgDTDeps[[i + 1]]$Package)] # remove imports where there is remotes
    i <- i + 1
    
  }
  
  if (NROW(pkgDTComplete[-1])) {
    ll <- rbindlist(pkgDTComplete, use.names = TRUE, fill = TRUE)
    ll <- keepOnlyMaxVersion(ll)
    
    setkeyv(ll, "PackageTopLevel")
    wh <- which(is.na(ll$Package))
    set(ll, wh, "Package", extractPkgName(ll$packageFullName[wh]))
    ll1 <- unique(ll, by = c("PackageTopLevel", "Package"))
    ll2 <- ll1[base::order(ll1$Package)]
    final <- split(ll2$packageFullName, ll2$PackageTopLevel)
    final <- Map(pkg = final, nam = names(final), function(pkg, nam) setdiff(pkg, nam))
  }
  if (sum(stillNeed_recursive) > 0) {
    final2Save <- final
    names(final2Save) <- saveNameOuter(names(final2Save), recursive, which) 
    list2env(final2Save, envir = .pkgEnv[["pkgDep"]][["deps"]])
  }
  
  if (sum(!stillNeed_recursive) > 0) {
    names(stashed_recursive)[!stillNeed_recursive] <- 
      gsub(paste0("^(.+)\\_", recursive, "\\_.+"), "\\1", names(stashed_recursive)[!stillNeed_recursive])
    final[names(stashed_recursive)[!stillNeed_recursive]] <- stashed_recursive[!stillNeed_recursive]
  }
  final <- final[match(packages, names(final))]
  if (!includeBase)
    final <- lapply(final, function(x) x[!x %in% .basePkgs])
  
  final[]
  
}


NAMESPACEFileDeps <- function(desc_paths, purge = getOption("Require.purge", FALSE)) {
  rr <- readLines(desc_paths)
  depsFromNamespace <- gsub(", except.*(\\))$", "\\1", rr)
  depsFromNamespace <- unique(gsub("^import.*\\((.+)\\,.*$", "\\1", 
                                   grep("^import", depsFromNamespace, value = TRUE)))
  depsFromNamespace <- unique(gsub("^import\\((.+)\\)", "\\1", depsFromNamespace))
  depsFromNamespace <- gsub(",.*", "", depsFromNamespace)
  depsFromNamespace <- gsub("\\\"", "", depsFromNamespace)
}

NAMESPACEFileDepsV <- Vectorize(NAMESPACEFileDeps, SIMPLIFY = FALSE, vectorize.args = "desc_paths")

pkgDepCRAN2 <- function(packageFullName, which = c("Depends", "Imports", "LinkingTo"),
                       #recursive = FALSE,
                       Package, PackageTopLevel,
                       keepVersionNumber = TRUE, repos = getOption("repos"),
                       purge = getOption("Require.purge", FALSE), 
                       keepSeparate = TRUE) {
  capFull <- available.packagesCached(repos = repos, purge = purge)
  deps <- pkgDepCRANInner2(capFull, which = which, 
                          packageFullName = packageFullName, Package = Package,
                          PackageTopLevel = PackageTopLevel,
                          keepVersionNumber = keepVersionNumber, keepSeparate = keepSeparate)
}

pkgDepArchive <- function(pkgDT, repos, keepVersionNumber = TRUE, 
                          purge = getOption("Require.purge", FALSE)) {
  objsExist <- unlist(lapply(
    pkgDT$packageFullName, function(pfn) exists(pfn, envir = .pkgEnv[["pkgDep"]][["deps"]])))
  Package <- pkgDT$Package
  packageTD <- file.path(tempdir2(paste(collapse = "", sample(LETTERS, 6))), Package)
  DESCRIPTIONpaths <- file.path(packageTD, "DESCRIPTION")
  pkgDTNeedNew <- pkgDT[objsExist == FALSE] 
  if (NROW(pkgDTNeedNew)) {
    message("available.packages() does not have correct information on package dependencies for ",
            paste(Package, collapse = ", "), 
            " because they are Archive versions; downloading their respective tar.gz files")
    pkgFilename <- paste0(pkgDTNeedNew$Package, "_", 
                          pkgDTNeedNew$OlderVersionsAvailable, ".tar.gz")
    packageURL <- file.path(pkgDTNeedNew$Package, pkgFilename)
    srcContrib <- "src/contrib"
    deps <- Map(pkgURL = packageURL, pkgTD = packageTD, 
        function(pkgURL, pkgTD) {
          url <- file.path(repos, srcContrib, "/Archive", pkgURL) 
          url2 <- file.path(repos, srcContrib, basename(pkgURL))#https://cran.r-project.org/src/contrib/foreign_0.8-80.tar.gz) 
          tf <- tempfile()
          haveFile <- suppressWarnings(tryCatch(download.file(url, tf, quiet = TRUE), error = function(x) 
            tryCatch(download.file(url2, tf, quiet = TRUE), error = function(y) FALSE)))
          if (!file.exists(tf))
            browser()
          untar(tarfile = tf, exdir = dirname(pkgTD)) # untaring has package name
          filesToDel <- dir(pkgTD, recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
          filesToDel <- filesToDel[grep("^DESCRIPTION$", basename(filesToDel), invert = TRUE)]
          unlink(filesToDel, recursive = TRUE)  
        })
  }
  deps <- lapply(DESCRIPTIONpaths, function(Dp) {
    needed <- if (file.exists(Dp))
      DESCRIPTIONFileDeps(Dp, which = whichAll, keepVersionNumber = keepVersionNumber,
                          purge = purge, keepSeparate = TRUE)  
    else {
      character()
      message(pkg, " dependencies not found on CRAN; perhaps incomplete description? On GitHub?")
    }
    needed
  })
  
  names(deps) <- concatPkgVersion(pkgDT$packageFullName, pkgDT$PackageTopLevel)
  deps
  

}
  
# whichAll <- c("Depends", "Imports")
whichAll <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
whichAllGitHub <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances", "Remotes")
  
pkgDepCRANInner2 <- function(ap, which, packageFullName, Package, PackageTopLevel,
                             keepVersionNumber,
                             keepSeparate = FALSE) {
  # MUCH faster to use base "ap$Package %in% packageFullName" than data.table internal "Package %in% packageFullName"
  dups <- duplicated(Package)
  packageFullName <- packageFullName[!dups]
  Package <- Package[!dups]
  PackageTopLevel <- PackageTopLevel[!dups]
  
  if (missing(Package))
    Package <- trimVersionNumber(packageFullName)
  if (isFALSE(keepVersionNumber)) {
    packageFullName <- Package
  }
  P <- Package
  apShort <- ap[ap$Package %in% P]
  keep <- match(apShort$Package, P)
  keep <- keep[!is.na(keep)]
  Package1 <- P[keep]
  packageFullName <- packageFullName[keep]
  PackageTopLevel <- PackageTopLevel[keep]
  
  names(which) <- which
  names(Package1) <- names(packageFullName)
  deps <- lapply(Package1, function(p) {
    l <- as.list(apShort[apShort$Package == p, ..whichAll])
    l <- lapply(l, function(i)
      strsplit(i, split = "(, {0,1})|(,\n)")[[1]]
    )
  })
  if (isFALSE(keepSeparate))
    deps <- lapply(deps, function(d) {
      dd <- unname(unlist(d, recursive = FALSE))
      dd <- dd[!is.na(dd)]
    })
  anyMissing <- P[!P %in% ap$Package]
  if (length(anyMissing))
    deps[anyMissing] <- lapply(anyMissing, function(am) 
      list(Depends = NA, Imports = NA, Suggests = NA, LinkingTo = NA, Enhances = NA))
  
  deps
}

cleanUp <- function(pkgDT, includeBase = FALSE) {
  pkgDT <- pkgDT[which(!is.na(pkgDT$packageFullName))]
  whWeird <- grep(.grepTabCR, pkgDT$packageFullName)
  set(pkgDT, whWeird, "packageFullName", gsub(.grepTabCR, "", pkgDT$packageFullName[whWeird]))
  Rs <- try(which(startsWith(pkgDT$packageFullName, "R")))
  if (is(Rs, "try-error")) browser()
  if (length(Rs)) {
    drop1 <- grep("^R[\\( ]", pkgDT$packageFullName[Rs])
    Rs <- Rs[drop1]
    pkgDT <- pkgDT[-Rs]
  }
  whTooManySpaces <- grep(" {2,}", pkgDT$packageFullName)
  set(pkgDT, whTooManySpaces, "packageFullName", gsub(" {2,}", " ", pkgDT$packageFullName[whTooManySpaces]))
  set(pkgDT, NULL, "Package", extractPkgName(pkgDT$packageFullName))
  if (!includeBase) 
    pkgDT <- pkgDT[!pkgDT$Package %in% .basePkgs]
  pkgDT
  # pkgDT <- pkgDT[!duplicated(pkgDT$packageFullName)]
}

concatPkgVersion <- function(...) {
  do.call(paste, args = list(..., sep = "___"))
}

keepOnlyMaxVersion <- function(PDT) {
  
  hasVersionSpec1 <- PDT$hasVersionSpec
  if (any(hasVersionSpec1)) {
    set(PDT, which(hasVersionSpec1), "versionSpec", 
        extractVersionNumber(PDT$packageFullName[hasVersionSpec1]))
    # browser(expr = any(startsWith(PDT$Package , "Rcpp")))
    
    set(PDT, NULL, "origOrder", seq(NROW(PDT)))
    order1 <- c(which(!hasVersionSpec1),
                which(hasVersionSpec1)[order(numeric_version(PDT$versionSpec[hasVersionSpec1]), decreasing = TRUE)])
    PDT <- PDT[order1]
    keeper <- PDT[, .I[1], by = c("PackageTopLevel", "Package")]$V1
    PDT <- PDT[keeper]
    setorderv(PDT, "origOrder")
     
    # PDT[hasVersionSpec1,
    #     hasMaxVersion := {
    #       if (.N > 1) {
    #         pvs <- numeric_version(versionSpec)
    #         .I[pvs %in% max(pvs)][1]
    #       } else {
    #         .I
    #       }
    #     },
    #     by = c("PackageTopLevel", "Package")]
    # keeper <- unique(PDT[ , if (any(hasVersionSpec)) hasMaxVersion[hasVersionSpec == TRUE][1] else .I[1],
    #                       by = c("Package", "PackageTopLevel")]$V1)
    # if (any(hasVersionSpec1))
    #   set(PDT, NULL, "hasMaxVersion", NULL)
    # PDT <- PDT[keeper]
  }
  PDT
}

dealWithCache <- function(purge, checkAge = TRUE) {
  if (!isTRUE(purge) && isTRUE(checkAge)) {
    purgeDiff <- as.numeric(Sys.getenv("R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE"))
    if (is.null(.pkgEnv[["startTime"]])) {
      purge = TRUE
    } else {
      purgeDiff <- if (identical(purgeDiff, "")  || is.na(purgeDiff)) 3600 else purgeDiff
      autoPurge <- purgeDiff < as.numeric(difftime(Sys.time(), .pkgEnv[["startTime"]], units = "sec")) 
      purge <- purge || autoPurge
    }
  }
  
  if (isTRUE(purge) || is.null(.pkgEnv[["pkgDep"]])) {
    .pkgEnv[["pkgDep"]] <- new.env(parent = emptyenv())
    .pkgEnv[["startTime"]] <- Sys.time()
  }
  
  if (is.null(.pkgEnv[["pkgDep"]][["deps"]]) || purge) .pkgEnv[["pkgDep"]][["deps"]] <- new.env(parent = emptyenv())
  if (is.null(.pkgEnv[["pkgDep"]][["DESCRIPTIONFile"]]) || purge) 
    .pkgEnv[["pkgDep"]][["DESCRIPTIONFile"]] <- new.env(parent = emptyenv())
  
  purge
}

.grepTooManySpaces <- " {2,}"
.grepTabCR <- "\n|\t"

saveNameOuter <- function(package, recursive, which) {
  paste0(package, "_", recursive, "_",  paste(which[[1]], collapse = "_"))
}
