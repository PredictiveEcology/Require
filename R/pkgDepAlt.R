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

  # pkgDT <- list(packageFullName = packages, Package = extractPkgName(packages))
  # pkgDT <- setDT(pkgDT)
  # if (!includeBase) pkgDT <- pkgDT[!pkgDT$Package %in% .basePkgs]
  #

  if (!includeBase) packages <- packages[!packages %in% .basePkgs]
  #if (any(!missing(depends), !missing(linkingTo), !missing(imports), !missing(suggests))) {
  if (!missing(depends)) {wh <- "Depends"; if (isTRUE(depends)) which <- unique(c(which, wh)) else setdiff(which, wh)}
  if (!missing(imports)) {wh <- "Imports"; if (isTRUE(imports)) which <- unique(c(which, wh)) else setdiff(which, wh)}
  if (!missing(suggests)) {wh <- "Suggests"; if (isTRUE(suggests)) which <- unique(c(which, wh)) else setdiff(which, wh)}
  if (!missing(linkingTo)) {wh <- "LinkingTo"; if (isTRUE(linkingTo)) which <- unique(c(which, wh)) else setdiff(which, wh)}

  which <- whichToDILES(which)
  # whichLC <- tolower(which[[1]])
  names(packages) <- packages
  isGHs <- !is.na(extractPkgGitHub(packages))
  doneI <- 0
  df <- data.frame(pkgNameFull = packages, package = extractPkgName(packages), isGH = isGHs,
                   depth = doneI, parent = "",
                   versionNumber = extractVersionNumber(packages))
  done <- character()
  ip <- dir(.libPaths())
  while (NROW(df) > 0) {
    doneI <- doneI + 1
    # pkgNameFull <- df$pkgNameFull[1]
    if (!df$package[1] %in% done) {
      if (df$package[1] %in% .basePkgs) {
        if (includeBase)
          done <- c(done, df$package[1])
        df <- df[-1, ]
      } else {
        if (df$isGH[1]) {
          ss <- getGitHubDESCRIPTION(df$pkgNameFull[1])      # ss <- Require:::getGitHubDESCRIPTION(pkg)$DESCFile
          pdl <-  DESCRIPTIONFileDeps(ss$DESCFile, which = unlist(which))
        } else {
          canDoLocal <- FALSE
          if (df$package[1] %in% ip) {
            canDoLocal <- TRUE
            ss <- system.file(package = df$package[1], "DESCRIPTION")
            ss1 <- file.path(ss)
          }

          needCompareVersion <- if (is.na(df$versionNumber[1])) FALSE else TRUE

          if (needCompareVersion && canDoLocal) {
            vers <- DESCRIPTIONFileVersionV(ss1)
            comp <- compareVersion(df$versionNumber[1], vers)
          } else {
            comp <- 1
          }
          if (comp <= 0 || !needCompareVersion) {
            ss2 <- gsub("DESCRIPTION", "NAMESPACE", ss)
            bb <- DESCRIPTIONFileDeps(ss1)
            aa <- NAMESPACEFileDeps(ss2)
            together <- c(extractPkgName(bb), aa)
            dups <- duplicated(together)
            pdl <- c(bb, aa)[!dups]
          } else {

            pdl <-  pkgDepCRAN(df$package[1], which = setdiff(unlist(which), "Remotes"))[[1]]
          }

      }
        done <- c(done, df$package[1])
        if (length(pdl) > 0) {
          isGH <- !is.na(extractPkgGitHub(pdl))
          # names(isGH) <- pdl
          df2 <- data.frame(pkgNameFull = pdl, package = extractPkgName(pdl), isGH = isGH, depth = doneI, parent = df$package[1],
                            versionNumber = extractVersionNumber(pdl))
          df <- rbind(df[-1, ], df2)
        } else {
          df <- df[-1, ]
        }
      }
    } else {
      df <- df[-1, ]
    }

  }
  return(done)
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
    .pkgEnv[["pkgDep"]] <- newPkgDepEnv()
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

newPkgDepEnv <- function() new.env(parent = emptyenv())
