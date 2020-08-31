pkgDep3 <- function(packages, libPath = .libPaths(),
                   which = c("Depends", "Imports", "LinkingTo"), recursive = FALSE,
                   depends, imports, suggests, linkingTo, enhances,
                   repos = getOption("repos"),
                   keepVersionNumber = TRUE, includeBase = FALSE,
                   sort = TRUE, purge = getOption("Require.purge", FALSE)) {
  

  pkgDT <- list(packageFullName = packages, Package = extractPkgName(packages))
  pkgDT <- setDT(pkgDT)
  if (!includeBase) pkgDT <- pkgDT[!pkgDT$Package %in% .basePkgs]
  
  if (any(!missing(depends), !missing(linkingTo), !missing(imports), !missing(suggests))) {
    message("Please use 'which' instead of 'imports', 'suggests', 'depends' and 'linkingTo'")
    if (!missing(depends)) depends <- TRUE
    if (!missing(imports)) imports <- TRUE
    if (!missing(suggests)) suggests <- TRUE
    if (!missing(linkingTo)) linkingTo <- TRUE
  }
  which <- whichToDILES(which)
  
  maxIterations <- if (isTRUE(recursive)) Inf else 1
  
  iterations <- 0
  pkgDTDeps <- list()
  pkgDTComplete <- list()
  i <- 1
  pkgDTDeps[[i]] <- pkgDT
  set(pkgDTDeps[[i]], NULL, "PackageTopLevel", pkgDTDeps[[i]]$Package)
  set(pkgDTDeps[[i]], NULL, "PackageVersion", concatPkgVersion(pkgDTDeps[[i]]$Package, pkgDTDeps[[i]]$Version))
  pkgDTComplete[[i]] <- data.table::copy(pkgDTDeps[[i]] )
  set(pkgDTComplete[[i]], NULL, "Package", NULL)
  
  while(NROW(pkgDTDeps[[i]])) {
    pkgDTs <- list()
    if (NROW(pkgDTDeps[[i]])) {
      pkgDTDeps[[i]] <- installedVers(pkgDTDeps[[i]])
      pkgDTDeps[[i]] <- parseGitHub(pkgDTDeps[[i]])
      pkgDTDeps[[i]] <- getPkgVersions(pkgDTDeps[[i]], install = TRUE)
      pkgDTDeps[[i]] <- getAvailable(pkgDTDeps[[i]], purge = purge, repos = repos)
      #pkgDTDeps[[i]] <- installFrom(pkgDTDeps[[i]], purge = purge, repos = repos)
    }
    
    # Check Local -- either correct version available or no version spec & installed
    locals <- pkgDTDeps[[i]]$correctVersion == TRUE | 
      (is.na(pkgDTDeps[[i]]$correctVersion) & !is.na(pkgDTDeps[[i]]$Version))
    pkgDTs[["Local"]] <- pkgDTDeps[[i]][locals]
    deps <- list()
    if (NROW(pkgDTs[["Local"]])) {
      desc_paths <- file.path(pkgDTs[["Local"]]$LibPath, pkgDTs[["Local"]]$Package, "DESCRIPTION")
      names(desc_paths) <- paste0(pkgDTs[["Local"]]$Package, "_", pkgDTs[["Local"]]$Version)
      deps[["Local"]] <- DESCRIPTIONFileDepsV(desc_paths, keepVersionNumber = TRUE, purge = purge, 
                                              which = whichAllGitHub,
                                              keepSeparate = TRUE)
    } 
    
    pkgDTOther <- pkgDTDeps[[i]][!locals | is.na(locals)]
    if (NROW(pkgDTOther)) {
      pkgDTs[["GitHub"]] <- pkgDTOther[pkgDTOther$repoLocation == "GitHub"]
      if (NROW(pkgDTs[["GitHub"]])) {
        pkgDTs[["GitHub"]] <- getGitHubDESCRIPTION(pkgDTs[["GitHub"]], purge = purge)
        desc_paths <- pkgDTs[["GitHub"]]$DESCFile
        bnDESCFile <- basename(desc_paths)
        names(desc_paths) <- paste0(pkgDTs[["GitHub"]]$Package, "_", pkgDTs[["GitHub"]]$AvailableVersion)
        deps[["GitHub"]] <- DESCRIPTIONFileDepsV(desc_paths, which = whichAllGitHub, purge = purge,
                                                 keepSeparate = TRUE)
        pkgDTs[["GitHub"]] <- getGitHubNamespace(pkgDTs[["GitHub"]], purge = purge)
        depsGitHub2 <- NAMESPACEFileDepsV(pkgDTs[["GitHub"]]$NAMESPACE, purge = purge)
        deps[["GitHub"]] <- Map(d1 = deps[["GitHub"]], d2 = depsGitHub2, function(d2, d1) {
          d1$imports <- setdiff(union(d2, d1$imports), .basePkgs)
          d1
        })
      }
      
      pkgDTs[["CRAN"]] <- pkgDTOther[pkgDTOther$repoLocation == "CRAN"]
      if (NROW(pkgDTs[["CRAN"]])) {
        pkgs <- pkgDTs[["CRAN"]]$packageFullName
        names(pkgs) <- paste0(pkgDTs[["CRAN"]]$Package, "_", "00Latest")
        deps[["CRAN"]] <- pkgDepCRAN2(packageFullName = pkgs, which = whichAll,
                                      Package = pkgDTs[["CRAN"]]$Package, 
                                      PackageTopLevel = pkgDTs[["CRAN"]]$PackageTopLevel,
                                      keepVersionNumber = TRUE, repos = getOption("repos"),
                                      purge = getOption("Require.purge", FALSE),
                                      keepSeparate = TRUE) 
        pp <- gsub("^(.+)\\_.+", "\\1", names(deps[["CRAN"]]))
        deps[["CRAN"]] <- deps[["CRAN"]][match(pkgDTs[["CRAN"]]$Package, pp)]
        pkgDTs[["CRAN"]]$Package
        
        # names(deps[["CRAN"]]) <- paste0(pkgDTs[["CRAN"]]$Package, "_", "00Latest")
      }
      
      #############
      pkgDTs[["Archive"]] <- pkgDTOther[pkgDTOther$repoLocation == "Archive"]
      if (NROW(pkgDTs[["Archive"]])) {
        ava <- archiveVersionsAvailable(pkgDTs[["Archive"]]$Package, repos = repos)
        deps[["Archive"]] <- pkgDepArchive(pkgDTs[["Archive"]], repos = repos, purge = purge)
      }
    }

    depsList <- do.call(c, unname(deps))
    pkgDTDeps[[i+1]] <- rbindlist(
      Map(x = depsList, PackageTopLevel = unlist(lapply(pkgDTs, function(x) x$PackageTopLevel)),
          function(x, PackageTopLevel) {
      nams <- names(x)
      nams <- match(tolower(nams), c("depends", "imports", "linkingto", "remotes"))
      l <- list(packageFullName = unlist(x[nams], recursive = FALSE))
      l[["PackageTopLevel"]] = rep(PackageTopLevel, length(l[["packageFullName"]]))
      l
      }
      
    ), 
    idcol = "PackageVersion")
    
    # Clean up -- R, \n\t
    pkgDTDeps[[i+1]] <- cleanUp(pkgDTDeps[[i+1]])
    
    if (!("PackageTopLevel" %in% colnames(pkgDTDeps[[i]])))
      set(pkgDTDeps[[i+1]], NULL, "PackageTopLevel", 
          gsub("^(.+)\\_[[:digit:]].+$", "\\1", pkgDTDeps[[i+1]]$PackageVersion))
    
    pkgDTComplete[[i+1]] <- pkgDTDeps[[i+1]]
    pkgDTDeps[[i+1]] <- pkgDTDeps[[i+1]][!duplicated(pkgDTDeps[[i+1]]$packageFullName)]
    
    
    alreadyDone <- pkgDTDeps[[i+1]]$packageFullName %in% 
      unlist(lapply(pkgDTDeps[seq(i)], function(x) x$packageFullName))
    set(pkgDTDeps[[i+1]], NULL, "Package", extractPkgName(pkgDTDeps[[i+1]]$packageFullName))
    
    if (any(!alreadyDone))
      # if (all(alreadyDone))
        pkgDTDeps[[i+1]] <- pkgDTDeps[[i+1]][alreadyDone == FALSE]
    else 
      pkgDTDeps[[i+1]] <- pkgDTDeps[[i+1]][0]
    i <- i + 1
    
  }
  ll <- rbindlist(pkgDTComplete, use.names = TRUE)
  setkeyv(ll, "PackageTopLevel")
  set(ll, NULL, "Package", extractPkgName(ll$packageFullName))
  ll1 <- unique(ll, by = c("PackageTopLevel", "Package"))
  bb <- split(ll1, ll1$PackageTopLevel)
  
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
  # pkgName <- extractPkgName(pkg)
  Package <- pkgDT$Package
  packageTD <- tempdir2(Package)
  # packageTD <- file.path(td, Package)
  dirsExist <- dir.exists(packageTD)
  pkgDTNeedNew <- if (isTRUE(purge)) pkgDT else pkgDT[dirsExist == FALSE] 
  if (NROW(pkgDTNeedNew)) {
    message("available.packages() doesn't have correct information on package dependencies for ",
            paste(Package, collapse = ", "), 
            " because they are Archive versions; downloading tar.gz")
    # verNum <- extractVersionNumber(pkg)
    # verNum <- pkgDTNeedNew$OlderVersionsAvailable
    #if (is.na(verNum)) {
    #  dt <- as.data.table(archiveVersionsAvailable(Package, repos = repos), keep.rownames = "packageURL")
    #  packageURL <- tail(dt$packageURL, 1)
    #} else {
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
  deps <- lapply(packageTD, function(pkgTD) {
    needed <- if (dir.exists(pkgTD))
      DESCRIPTIONFileDeps(file.path(pkgTD, "DESCRIPTION"), 
                          which = whichAll, keepVersionNumber = keepVersionNumber,
                          purge = purge, keepSeparate = TRUE)  
    else {
      character()
      message(pkg, " dependencies not found on CRAN; perhaps incomplete description? On GitHub?")
    }
  })
  
  names(deps) <- concatPkgVersion(pkgDT$Package, pkgDT$OlderVersionsAvailable)
  deps
  

}
  
whichAll <- c("Depends", "Imports")
#whichAll <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
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

cleanUp <- function(pkgDT) {
  pkgDT <- pkgDT[which(!is.na(pkgDT$packageFullName))]
  whWeird <- grep("\n|\t", pkgDT$packageFullName)
  set(pkgDT, whWeird, "packageFullName", gsub("\n|\t", "", pkgDT$packageFullName[whWeird]))
  Rs <- which(startsWith(pkgDT$packageFullName, "R"))
  if (length(Rs)) {
    drop1 <- grep("^R[\\( ]", pkgDT$packageFullName[Rs])
    Rs <- Rs[drop1]
    pkgDT <- pkgDT[-Rs]
  }
  whTooManySpaces <- grep(" {2,}", pkgDT$packageFullName)
  set(pkgDT, whTooManySpaces, "packageFullName", gsub(" {2,}", " ", pkgDT$packageFullName[whTooManySpaces]))
  pkgDT
  # pkgDT <- pkgDT[!duplicated(pkgDT$packageFullName)]
}

concatPkgVersion <- function(Package, Version) {
  paste0(Package, "_", Version) 
}