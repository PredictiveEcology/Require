pkgDep2 <- function(packages, libPath = .libPaths(),
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
  
  if (NROW(packages)) {
    
    # Some package names are not derived from their GitHub repo names -- user can supply named packages
    # origPackagesHaveNames <- nchar(names(packages)) > 0
    # if (any(origPackagesHaveNames))
    #   packages <- packages[order(names(packages), decreasing = TRUE)]
    # dups <- duplicated(packages)
    # packages <- packages[!dups] # (unique removes names) sometimes people pass identical packages -- only <10s microseconds
    # origPackagesHaveNames <- nchar(names(packages)) > 0 # redo -- changed order
    # packagesOrig <- packages
    # packageNamesOrig <- packages
    # 
    # if (any(origPackagesHaveNames))
    #   packageNamesOrig[origPackagesHaveNames] <- names(packagesOrig)[origPackagesHaveNames]
    # packagesOrder <- seq(packagesOrig)
    # names(packagesOrder) <- extractPkgName(packageNamesOrig)
    
    browser(expr = exists("._Require_1"))
    
    # Create data.table of Require workflow
    # if (is(packages, "list")) packages <- unlist(packages, recursive = FALSE)
    # 
    # pkgDT <- toPkgDT(packages)
    # identify the packages that were asked by user to load -- later dependencies will be in table too
    # pkgDT[Package %in% unique(extractPkgName(packageNamesOrig)),
    #       packagesRequired := packagesOrder[match(Package, names(packagesOrder))]]
    # 
    # if (any(origPackagesHaveNames))
    #   pkgDT[packageFullName %in% packagesOrig[origPackagesHaveNames],
    #         Package := names(packagesOrig[origPackagesHaveNames])]
    

    if (NROW(pkgDT)) {
      pkgDT <- installedVers(pkgDT)
      pkgDT <- parseGitHub(pkgDT)
      pkgDT <- getPkgVersions(pkgDT, install = TRUE)
      pkgDT <- getAvailable(pkgDT, purge = purge, repos = repos)
      #pkgDT <- installFrom(pkgDT, purge = purge, repos = repos)
    }}
  
  #
  # Check Local -- either correct version available or no version spec & installed
  locals <- pkgDT$correctVersion == TRUE | 
    (is.na(pkgDT$correctVersion) & !is.na(pkgDT$Version))
  pkgDTLocal <- pkgDT[locals]
  if (NROW(pkgDTLocal)) {
    desc_paths <- file.path(pkgDTLocal$LibPath, pkgDTLocal$Package, "DESCRIPTION")
    names(desc_paths) <- paste0(pkgDTLocal$Package, "_", pkgDTLocal$Version)
    depsLocal <- DESCRIPTIONFileDepsV(desc_paths, keepVersionNumber = TRUE, purge = purge, 
                                which = whichAll,
                                keepSeparate = TRUE)
  } 
  pkgDTOther <- pkgDT[!locals | is.na(locals)]
  if (NROW(pkgDTOther)) {
    pkgDTGitHub <- pkgDTOther[pkgDTOther$repoLocation == "GitHub"]
    pkgDTGitHub <- getGitHubDESCRIPTION(pkgDTGitHub, purge = purge)
    depsGitHub <- DESCRIPTIONFileDepsV(pkgDTGitHub$DESCFile, which = whichAll, purge = purge,
                                      keepSeparate = TRUE)
    pkgDTGitHub <- getGitHubNamespace(pkgDTGitHub, purge = purge)
    depsGitHub2 <- NAMESPACEFileDepsV(pkgDTGitHub$NAMESPACE, purge = purge)
    depsGitHub <- Map(d1 = depsGitHub, d2 = depsGitHub2, function(d2, d1) {
      d1$imports <- setdiff(union(d2, d1$imports), .basePkgs)
      d1
    })
    
    pkgDTCRAN <- pkgDTOther[pkgDTOther$repoLocation == "CRAN"]
    depsCRAN <- pkgDepCRAN2(packageFullName = pkgDTCRAN$packageFullName, which = whichAll,
                Package = pkgDTCRAN$Package, 
               keepVersionNumber = TRUE, repos = getOption("repos"),
               purge = getOption("Require.purge", FALSE),
               keepSeparate = TRUE) 
    
    #############
    pkgDTArchive <- pkgDTOther[pkgDTOther$repoLocation == "Archive"]
    ava <- archiveVersionsAvailable(pkgDTArchive$Package, repos = repos)
    depsArchive <- pkgDepArchive(pkgDTArchive, repos = repos, purge = purge)
    browser()
    deps <- c(depsLocal, depsGitHub, depsCRAN, depsArchive)
    
    #############
    depsGitHub <- Map(d1 = depsGitHub, d2 = depsGitHub2, function(d2, d1) {
      d1$imports <- setdiff(union(d2, d1$imports), .basePkgs)
      d1
    })
    
    
    pkgDT2 <- data.table(packageFullName = setdiff(union(depsGitHub2, depsGitHub), .basePkgs))
    # needed <- setdiff(union(depsFromNamespace, needed), .basePkgs)
    if (NROW(pkgDT2)) {
      pkgDT2[, isGitPkg := grepl("^.+/(.+)@+.*$", packageFullName)]
      setorderv(pkgDT2, "isGitPkg", order = -1)
      pkgDT2[, Package := extractPkgName(packageFullName)]
      pkgDT2[, dup := duplicated(Package)]
      pkgDT2 <- pkgDT2[dup == FALSE]
      differences <- setdiff(pkgDT2$Package, extractPkgName(needed))
      if (length(differences)) {
        message(" (-- The DESCRIPTION file for ", pkg, " is incomplete; there are missing imports:\n",
                paste(differences, collapse = ", "), " --) ")
      }
      pkgDT2[, github := extractPkgGitHub(packageFullName)]
    }
    needed <- pkgDT2$packageFullName
    
    #############
    
    pkgDTGitHub <- getGitHubNamespace(pkgDTGitHub)
  }
  #if ()
  
  browser()
  
  DepsList <- pkgDepInner(pkgDT[correctVersion != FALSE]$Package, 
                          libPath, which = c("Depends", "Imports", "LinkingTo", "Suggests"), 
                          keepVersionNumber = TRUE,
                          purge = purge, repos = repos)
  Deps <- unlist(DepsList, use.names = FALSE)
  Package <- unlist(Map(pkg = DepsList, name = names(DepsList), function(pkg, name) {
    rep(name, length(pkg))
  }), use.names = FALSE)
  pkgDTDeps <- list()
  i <- 1
  pkgDTDeps[[i]] <- data.table(Package, Deps)
  if (recursive) {
    i <- i + 1
    pkgDTDeps[[i]] <- pkgDTDeps[!pkgDTDeps$Deps %in% pkgDTDeps$Package]
    return(pkgDT)
  }
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
                       Package, 
                       keepVersionNumber = TRUE, repos = getOption("repos"),
                       purge = getOption("Require.purge", FALSE), 
                       keepSeparate = TRUE) {
  capFull <- available.packagesCached(repos = repos, purge = purge)
  deps <- pkgDepCRANInner2(capFull, which = which, 
                          packageFullName = packageFullName, Package = Package,
                          keepVersionNumber = keepVersionNumber, keepSeparate = keepSeparate)
}

pkgDepArchive <- function(pkgDT, repos, keepVersionNumber = TRUE, 
                          purge = getOption("Require.purge", FALSE)) {
  # pkgName <- extractPkgName(pkg)
  Package <- pkgDT$Package
  packageTD <- tempdir2(Package)
  # packageTD <- file.path(td, Package)
  dirsExist <- dir.exists(packageTD)
  pkgDTNeedNew <- pkgDT[dirsExist == FALSE]
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
  
  names(deps) <- paste0(pkgDT$Package, "_", pkgDT$OlderVersionsAvailable)
  deps
  

}
  
whichAll <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
  
pkgDepCRANInner2 <- function(ap, which, packageFullName, Package, keepVersionNumber,
                             keepSeparate = FALSE) {
  # MUCH faster to use base "ap$Package %in% packageFullName" than data.table internal "Package %in% packageFullName"
  if (missing(Package))
    Package <- trimVersionNumber(packageFullName)
  if (isFALSE(keepVersionNumber)) {
    packageFullName <- Package
  }
  browser()
  P <- Package
  ap <- ap[ap$Package %in% P]
  keep <- match(ap$Package, P)
  keep <- keep[!is.na(keep)]
  Package1 <- P[keep]
  packageFullName <- packageFullName[keep]
  ap <- ap[order(Package1)]
  
  names(which) <- which
  names(Package1) <- Package1
  deps <- lapply(Package1, function(p) {
    l <- as.list(ap[1, ..whichAll])
    l <- lapply(l, function(i)
      strsplit(i, split = "(, {0,1})|(,\n)")[[1]]
    )
  })
  if (isFALSE(keepSeparate))
    deps <- lapply(deps, function(d) {
      dd <- unname(unlist(d, recursive = FALSE))
      dd <- dd[!is.na(dd)]
    })
  # deps <- lapply(ss, function(x) unname(unlist(lapply(deps, function(y) y[[x]]))))
  deps
}
