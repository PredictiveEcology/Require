utils::globalVariables(
  c("Additional_repositories", "githubPkgName",
    "localBranch", "localFiles", "localRepo", "installedSha", "localUsername",
    "newPackageFullName")
)

#' Determine package dependencies
#'
#' This will first look in local filesystem (in `.libPaths()`) and will use a
#' local package to find its dependencies. If the package does not exist
#' locally, including whether it is the correct version, then it will look in
#' (currently) `CRAN` and its archives (if the current `CRAN` version is not the
#' desired version to check). It will also look on `GitHub` if the package
#' description is of the form of a GitHub package with format
#' `account/repo@branch` or `account/repo@commit`. For this, it will attempt to
#' get package dependencies from the GitHub \file{DESCRIPTION} file. This is
#' intended to replace `tools::package_dependencies` or `pkgDep` in the
#' \pkg{miniCRAN} package, but with modifications to allow multiple sources to
#' be searched in the same function call.
#'
#' @note `tools::package_dependencies` and `pkgDep` will differ under the
#'   following circumstances: \enumerate{ \item GitHub packages are not detected
#'   using `tools::package_dependencies`; \item `tools::package_dependencies`
#'   does not detect the dependencies of base packages among themselves, *e.g.*,
#'   `methods` depends on `stats` and `graphics`. }
#'
#' @inheritParams Require
#' @param which a character vector listing the types of dependencies, a subset
#'   of `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.
#'   Character string `"all"` is shorthand for that vector, character string
#'   `"most"` for the same vector without `"Enhances"`.
#' @param depends Logical. Include packages listed in "Depends". Default `TRUE`.
#' @param imports Logical. Include packages listed in "Imports". Default `TRUE`.
#' @param suggests Logical. Include packages listed in "Suggests". Default
#'   `FALSE`.
#' @param linkingTo Logical. Include packages listed in "LinkingTo". Default
#'   `TRUE`.
#' @param recursive Logical. Should dependencies of dependencies be searched,
#'   recursively. NOTE: Dependencies of suggests will not be recursive. Default
#'   `TRUE`.
#' @param keepVersionNumber Logical. If `TRUE`, then the package dependencies
#'   returned will include version number. Default is `FALSE`
#' @param libPath A path to search for installed packages. Defaults to
#'   `.libPaths()`
#' @param sort Logical. If `TRUE`, the default, then the packages will be sorted
#'   alphabetically. If `FALSE`, the packages will not have a discernible order
#'   as they will be a concatenation of the possibly recursive package
#'   dependencies.
#' @param includeBase Logical. Should R base packages be included, specifically,
#'   those in `tail(.libPath(), 1)`
#' @param includeSelf Logical. If `TRUE`, the default, then the dependencies
#'   will include the package itself in the returned list elements, otherwise,
#'   only the "dependencies"
#' @param Additional_repositories Logical. If `TRUE`, then `pkgDep` will return
#'   a list of `data.table` objects (instead of character vectors)
#'   with a column `packageFullName` and possibly a second column `Additional_repositories`,
#'   which may have been specified in a `DESCRIPTION` file. NOTE: THIS ALTERS
#'   THE OUTPUT CLASS
#' @inheritParams Require
#'
#' @export
#' @rdname pkgDep
#'
#' @examples
#' \dontrun{
#' if (Require:::.runLongExamples()) {
#'   opts <- Require:::.setupExample()
#'
#'   pkgDep("tidyverse", recursive = TRUE)
#'
#'   # GitHub, local, and CRAN packages
#'   pkgDep(c("PredictiveEcology/reproducible", "Require", "plyr"))
#'
#'   Require:::.cleanup(opts)
#' }
#' }
pkgDep <- function(packages,
                   libPath = .libPaths(),
                   which = c("Depends", "Imports", "LinkingTo"),
                   recursive = FALSE,
                   depends,
                   imports,
                   suggests,
                   linkingTo,
                   repos = getOption("repos"),
                   keepVersionNumber = TRUE,
                   includeBase = FALSE,
                   includeSelf = TRUE,
                   sort = TRUE,
                   simplify = TRUE,
                   purge = getOption("Require.purge", FALSE),
                   verbose = getOption("Require.verbose"),
                   type = getOption("pkgType"),
                   Additional_repositories = FALSE) {
  purge <- dealWithCache(purge)
  checkAutomaticOfflineMode() # This will turn off offlineMode if it had been turned on automatically

  if (!includeBase) {
    packages <- packages[!packages %in% .basePkgs]
  }
  deps <- NULL
  if (length(packages)) {
    which <- depsImpsSugsLinksToWhich(depends, imports, suggests, linkingTo, which)

    if (length(which) < length(packages))
      which <- rep(which, length.out = length(packages))
    # Only deal with first one of "which"...
    whichCat <- paste(sort(which[[1]]), collapse = "_")

    deps <- getPkgDeps(packages, recursive = recursive, repos = repos, verbose = verbose,
                       type = type, which = which, includeBase = includeBase,
                       includeSelf = includeSelf)
  }

  # a <- deps$`PredictiveEcology/LandR@LandWeb`
  # b <- split(a, f = c(a$parentPackage))
  # d <- data.table(deps = b, packageFullName = names(b))

  # deps <- lapply(deps, function(d) {
  #   dups <- duplicated(d$Package)
  #   d[!dups]
  # })
  deps <- lapply(deps, trimRedundancies) # faster to only do this once
  deps <- lapply(deps, setorderv, cols = ".depth")

  # keep only some columns
  keepCols <- c("Package", "packageFullName", "Version", "versionSpec", "inequality",
                "githubPkgName", "repoLocation", ".depth", "parentPackage", "which")

  a <- try(deps <- lapply(deps, function(dep) dep[, ..keepCols]))
  if (is(a, "try-error")) browser()

  if (keepVersionNumber %in% FALSE) {
    deps <- Map(pkgFN = deps, function(pkgFN) pkgFN[["Package"]])
  } else if (simplify %in% TRUE) {
    # collapse or not to list of character vector
    deps <- Map(pkgFN = deps, function(pkgFN) pkgFN[["packageFullName"]])
  }

  # This is to remove "R" ... but for now keep it
  # deps <- lapply(deps, function(dep) {
  #   hasR <- dep$Package == "R"
  #   if (any(hasR)) dep <- dep[-which(hasR)]
  #   dep
  # })

  deps
}

getPkgDeps <- function(packages, outerPackages = NULL, recursive, which, repos, type, includeBase,
                       includeSelf, verbose, .depth = 0, .counter) {

  if (.depth == 0) {
    outerPackages <- packages
  }

  deps <- NULL
  if (NROW(packages) > 0) {
    if (is.data.table(packages))
      packages <- packages$packageFullName
    pkgDT <- parseGitHub(packages)
    pkgDT <- updatePackagesWithNames(pkgDT, packages)
    pkgDT <- parsePackageFullname(pkgDT, sorted = FALSE)
    pkgDT <- trimRedundancies(pkgDT, repos = repos, purge = FALSE) # changes order

    ap <- getAvailablePackagesIfNeeded(pkgDT, repos, purge = FALSE, verbose, type)

    recTF <- c("FALSE", "TRUE") #paste0("Recursive", c("TRUE", "FALSE"))
    rec <- if (recursive %in% TRUE) c(recTF[1], recTF[2]) else recTF[2] # need to do both if TRUE
    alreadySaved <- list()
    for (recursiveUsed in rec) {
      pkgDT <- saveNamesForCache(pkgDT, which, recursive = recursiveUsed, ap = ap,
                                 repos = repos, type = type, verbose = verbose - 1)
      sns <- pkgDT$sn[!is.na(pkgDT$sn)]
      alreadySaved[[recursiveUsed]] <- sns %in% names(pkgDepDepsEnv())

      names(alreadySaved[[recursiveUsed]]) <- sns

      if (all(alreadySaved[[recursiveUsed]]))
        break
    }

    if (any(alreadySaved[[recursiveUsed]])) {
      wh <- alreadySaved[[recursiveUsed]]
      whSaved <- pkgDT$sn %in% names(wh)[wh]
      pkgDTSaved <- pkgDT[whSaved]
      deps <- Map(nam = pkgDTSaved$packageFullName, obj = pkgDTSaved$sn,
                  f = function(nam, obj) get0(obj, envir = pkgDepDepsEnv()))
      pkgDT <- pkgDT[-which(whSaved)]
    }

    if (!includeBase) {
      isBase <- pkgDT$Package %in% .basePkgs
      if (any(isBase))
        pkgDT <- pkgDT[-which(isBase)]
    }
    if (NROW(pkgDT) > 0) {
      if (.depth == 0) {
        num <- NROW(unique(pkgDT$Package)) # unique b/c can have src and bin listed here if they are different on CRAN
        messageVerbose("Determining dependencies of ", num, singularPlural(c(" package", " packages"), v = num),
                       verbose = verbose)
      }

      depsNew <- getPkgDepsNonRecursive(pkgDT, outerPackages, which, repos, type,
                                        includeBase = includeBase, ap = ap, verbose = verbose,
                                        .depth = .depth)

      # Because we aren't trimRedundancies, multiple versions of e.g., Rcpp (>= 0.11.0) and Rcpp (>= 1.0-10) both get downloaded
      # also, it will say later that crayon is not on CRAN!



      # assignPkgDepDTtoSaveNames(pkgDT$sn, depsNew)
      deps <- append(deps, depsNew) # add the ones recovered from saved

      if (recursive %in% TRUE) {
        if (.depth == 0) {
          messageVerbose("  Determining recursive dependencies", verbose = verbose)
        }
        deps <- getPkgDepsMap(deps, outerPackages = outerPackages, recursive,  repos, which, type,  libPaths,
                              includeBase = includeBase, includeSelf = includeSelf, verbose,
                              .depth = .depth)
      }
    }
    if (.depth == 0) {
      if (recursive %in% TRUE) {
        if (any(!alreadySaved[[recTF[1]]])) {

          if (exists("pkgDTSaved", inherits = FALSE)) {
            pkgDT <- rbindlist(list(pkgDT, pkgDTSaved))
          }

          whDepsArePkgDT <- match(pkgDT$packageFullName, names(deps))
          deps <- deps[whDepsArePkgDT]
          alreadySavedNow <- pkgDT$sn %in% names(pkgDepDepsEnv())
          if (any(!alreadySavedNow)) # this is for the FALSEs ... it will be no new assignments unless a partial update
            assignPkgDepDTtoSaveNames(pkgDT$sn[!alreadySavedNow], deps[whDepsArePkgDT][!alreadySavedNow])

          # Now for saving recursive = TRUE
          pkgDT2 <- saveNamesForCache(pkgDT, which, recursive = "TRUE",
                                      ap = ap, repos = repos, type = type,
                                      verbose = verbose - 1)
          alreadySavedNow <- pkgDT2$sn %in% names(pkgDepDepsEnv())
          if (any(!alreadySavedNow)) {
            whDepsArePkgDT <- match(pkgDT2$packageFullName, names(deps))
            assignPkgDepDTtoSaveNames(pkgDT2$sn[!alreadySavedNow], deps[whDepsArePkgDT][!alreadySavedNow])
          }
        }
      }

      # reorder to initial order; if there were redundancies, it would create NAs; thus na.omit
      deps <- deps[na.omit(match(packages, names(deps)))]

      deps <- Map(dep = deps, function(dep) {
        pp <- c("packageFullName", "parentPackage")
        for (pk in pp)
          set(dep, NULL, pk, cleanPkgs(dep[[pk]]))
        dep
      })
      if (includeBase %in% FALSE) {
        deps <- Map(dep = deps, function(dep) {
          dep[!dep$Package %in% .basePkgs]
        })
      }
      if (includeSelf %in% TRUE) {
        deps <- Map(dep = deps, self = names(deps), function(dep, self) {
          rbindlist(list(toPkgDepDT(self), dep), fill = TRUE, use.names = TRUE)
        })
      }
    }
  }

  deps
}


getPkgDepsMap <- function(deps, outerPackages, recursive, repos, which, type, libPaths,
                          includeBase, includeSelf, verbose, .depth = 0) {
  ndeps <- sapply(deps, NROW)
  noDeps <- ndeps == 0
  if (any(!noDeps)) {
    out <- Map(packages = deps[!noDeps], f = getPkgDeps, .counter = seq_along(deps[!noDeps]),
               MoreArgs = list(outerPackages = outerPackages, recursive = recursive, which = which, repos = repos,
                               type = type, includeBase = includeBase,
                               includeSelf = includeSelf,
                               .depth = .depth + 1, verbose = verbose))
    out2 <- try(lapply(out, rbindlist, fill = TRUE, use.names = TRUE))
    out3 <- Map(na = names(out2), function(na) {
      rr <- rbindlist(list(deps[[na]], out2[[na]]), use.names = TRUE, fill = TRUE)
      # rr <- trimRedundancies(rr, purge = FALSE, repos = repos, libPaths = libPaths,
      #                        type = type)
    })

    if (any(noDeps)) {
      deps <- append(out3, deps[noDeps])
    } else {
      deps <- out3
    }
  }
  deps
}


getPkgDepsNonRecursive <- function(pkgDT, outerPackages, which, repos, type, includeBase, ap, verbose,
                                   .depth) {
  hasRasDep <- pkgDT$Package == "R"
  if (isTRUE(any(hasRasDep)))
    pkgDT <- pkgDT[-which(hasRasDep)]
  pkgDTList <- split(pkgDT, by = "repoLocation")
  pkgDTDep <- list()
  if (!is.null(pkgDTList[["CRAN"]])) {
    num <- NROW(unique(pkgDTList$CRAN$Package)) # can have src and bin listed
    messageVerbose("  ", num, " ", singularPlural(c("package", "packages"), v = num),
                   " on CRAN", verbose = verbose)
    pkgDTDep[["CRAN"]] <- pkgDepCRAN(pkgDT = pkgDTList$CRAN, which = which[[1]],
                                          repos = repos, type = type, ap = ap)
  }
  if (!is.null(pkgDTList[["GitHub"]])) {
    messageVerbose("  ", NROW(pkgDTList$GitHub), " packages on GitHub", verbose = verbose)

    pkgDTDep[["GitHub"]] <- pkgDepGitHub(pkgDT = pkgDTList$GitHub, which = which[[1]],
                                         includeBase = includeBase, verbose = verbose)
  }

  depsNew <- unlist(unname(pkgDTDep), recursive = FALSE)
  depsNew <- lapply(depsNew, function(depsInner) {
    # Remove "R" as a dependency package
    hasR <- "R" == depsInner$Package
    if (any(hasR))
      depsInner <- depsInner[which(!hasR)]
    set(depsInner, NULL, ".depth", .depth)
    depsInner
  })
  depsNew <- Map(depsInner = depsNew, nam = names(depsNew), function(depsInner, nam)
    set(depsInner, NULL, "parentPackage", nam))
  depsNew <- depsNew[match(unique(pkgDT$packageFullName), names(depsNew))] # return to original order
  depsNew
}

pkgDepCRAN <- function(pkgDT, which, repos, type, ap, verbose) {
  fillDefaults(pkgDep)

  if (!is.data.table(pkgDT))
    pkgDT <- toPkgDT(pkgDT) |> parsePackageFullname()
  needCheckCRAN_AP <- FALSE
  if (is.null(pkgDT$Depends)) needCheckCRAN_AP <- TRUE else {
    if (all(is.na(pkgDT$Depends))) needCheckCRAN_AP <- TRUE
  }
  if (needCheckCRAN_AP) {

    pkgDT <- joinToAvailablePackages(ap, pkgDT, repos, type, which, verbose)
    # if (is.null(ap))
    #   ap <- getAvailablePackagesIfNeeded(pkgDT$Package, repos = repos, purge = FALSE, verbose = verbose, type = type)
    # keepNames <- c("Package", "packageFullName", setdiff(colnames(pkgDT), colnames(ap)))
    #
    # if (any(grepl("raster", pkgDT$Package))) browser()
    # setnames(ap, old = "Version", new = "VersionOnRepos")
    # pkgDT <- ap[pkgDT[, ..keepNames], on = "Package"] # there will be packageFullName from both, the i. will be what is on CRAN
    # # if (any(pkgDT$packageFullName == pkgDT$i.packageFullName, na.rm = TRUE))
    # #   pkgDT[packageFullName == i.packageFullName, VersionOnRepos := Version]
    # set(pkgDT, NULL, "packageFullName", pkgDT$i.packageFullName) # keep the needed version, not the one on CRAN
    # set(pkgDT, NULL, "i.packageFullName", NULL)
    # # setnames(pkgDT, old = "Version", new = "VersionOnRepos")

  }

  hasVersionNum <- !is.na(pkgDT$versionSpec) | !is.na(pkgDT$VersionOnRepos)
  set(pkgDT, NULL, "availableVersionOK", NA) # default
  pkgDT <- availableVersionOK(pkgDT)
  if (any(hasVersionNum)) {
    pkgDTVerNums <- split(pkgDT, f = hasVersionNum)
    # setnames(pkgDTVerNums$`TRUE`, old = "Version", "VersionOnRepos")
    pkgDTVerNums$`TRUE` <- availableVersionOK(pkgDTVerNums$`TRUE`)
    # setnames(pkgDTVerNums$`TRUE`, old = "VersionOnRepos", "Version")
    set(pkgDTVerNums$`TRUE`, NULL, "keep", seq(NROW(pkgDTVerNums$`TRUE`)))
    dups <- duplicated(pkgDTVerNums$`TRUE`$Package)
    pkgDTVerNums$`TRUE`[which(dups), keep := if (any(availableVersionOK)) .I[availableVersionOKthisOne][1] else .I, by = "Package"]
    pkgDTVerNums$`TRUE` <- pkgDTVerNums$`TRUE`[na.omit(unique(pkgDTVerNums$`TRUE`$keep))]
    set(pkgDTVerNums$`TRUE`, NULL, "keep", NULL) # remove "keep" column; no longer needed
    pkgDT <- rbindlist(pkgDTVerNums, fill = TRUE)

    #, availableVersionOK := compareVersion2(Version, versionSpec, inequality)]
  } else {
    set(pkgDT, NULL, "availableVersionOK", TRUE)
  }
  inCurrentCRAN <- !is.na(pkgDT[!Package %in% .basePkgs]$VersionOnRepos) # this is "removed and gone"

  if (isTRUE(any(!pkgDT$availableVersionOK %in% TRUE)))
    inCurrentCRAN <- inCurrentCRAN | pkgDT$availableVersionOK

  if (sum(inCurrentCRAN)) {
    if (any(inCurrentCRAN)) {
      num <- NROW(unique(pkgDT$Package[inCurrentCRAN]))
      messageVerbose("  Done for ", num, " ", singularPlural(c("package", "packages"), v = num),
                     " currently on CRAN!", verbose = verbose)
    }
  }

  if (any(!inCurrentCRAN)) {
    set(pkgDT, which(!inCurrentCRAN), "repoLocation", "Archive")
    pkgDTList <- split(pkgDT, by = "repoLocation")
    dups <- duplicated(pkgDTList$Archive$Package) # b/c will hvae src and bin --> not needed any more
    pkgDTList$Archive <- pkgDTList$Archive[which(!dups)]
    num <- NROW(pkgDTList$Archive$Package)
    messageVerbose("  ", num, " ", singularPlural(c("package has", "packages have"), v = num),
                   " been archived...", verbose = verbose)
    pkgDTList <- getArchiveDESCRIPTION(pkgDTList, repos, which, verbose, purge = FALSE)
    wcr <- whichCatRecursive(list(which), recursive = FALSE)
    whHadArchive <- which(!is.na(pkgDTList$Archive$VersionOnRepos))
    set(pkgDTList$Archive, whHadArchive, "sn",
        assignPkgDTtoSaveNames(pkgDTList$Archive[whHadArchive], which, verbose, wcr))

    messageVerbose("    Done package archives!", verbose = verbose)
    pkgDT <- rbindlist(pkgDTList, fill = TRUE, use.names = TRUE)
  }


  pkgDT[is.na(versionSpec), availableVersionOK := TRUE]
  browser()
  pkgDT[!is.na(versionSpec), availableVersionOK := compareVersion2(Version, versionSpec, inequality)]
  for (co in which)
    set(pkgDT, which(is.na(pkgDT[[co]])), co, "")

  pkgDT <- installed.packagesDeps(pkgDT, which)
  deps1 <- list()
  for (cn in which)
    deps1[[cn]] <- depsWithCommasToVector(pkgDT$packageFullName, pkgDT[[cn]])
  deps2 <- invertList(deps1)
  deps3 <- depsAddWhichCol(deps2)
  depsAll2 <- Map(toPkgDepDT, deps3, verbose = verbose)
  deps3
}


#' @inheritParams Require
pkgDepGitHub <- function(pkgDT, which, includeBase = FALSE, verbose = getOption("Require.verbose")) {
  pkg <- masterMainToHead(pkgDT$packageFullName)

  localVersionOK <- pkgDT$installedVersionOK
  pkgDepDTOuter <- data.table(packageFullName = character())

  if (any(!localVersionOK %in% TRUE)) {
    pkgDTNotLocal <- getGitHubDESCRIPTION(pkgDT[!localVersionOK %in% TRUE], purge = FALSE)
  }
  if (any(localVersionOK %in% TRUE)) {
    pkgDT[localVersionOK %in% TRUE, DESCFile := base::system.file("DESCRIPTION", package = Package), by = "Package"]
    if (exists("pkgDTNotLocal", inherits = FALSE))
      pkgDT <- rbindlist(list(pkgDT[localVersionOK %in% TRUE], pkgDTNotLocal), fill = TRUE, use.names = TRUE)
  } else {
    pkgDT <- pkgDTNotLocal
  }

  hasVersionNum <- pkgDT$hasVers # grep(grepExtractPkgs, pkgDT$packageFullName)
  set(pkgDT, NULL, "availableVersionOK", NA)
  if (any(hasVersionNum)) {
    if (!isTRUE(getOption("Require.offlineMode"))) {
      pkgDT[hasVersionNum, VersionOnRepos := DESCRIPTIONFileVersionV(DESCFile, purge = FALSE)]
      if (is.null(pkgDT$versionSpec)) {
        pkgDT[hasVersionNum, versionSpec := extractVersionNumber(packageFullName)]
        pkgDT[hasVersionNum, inequality := extractInequality(packageFullName)]
      }
      pkgDT[hasVersionNum, availableVersionOK := compareVersion2(VersionOnRepos, versionSpec, inequality)]
    }
  }

  pkgDepDTOuter <- updateWithRemotesNamespaceAddRepos2(pkgDT, which, purge = FALSE, includeBase, verbose = verbose)

  pkgDepDTOuter
}



#' The `packages` argument may have up to 4 pieces of information for GitHub
#' packages: name, repository, branch, version. For CRAN-alikes, it will only
#' be 2 pieces: name, version. There can also be an inequality or equality, if
#' there is a version.
#'
#' @param packages A vector of GitHub or CRAN-alike packages. These can include
#'   package name (required) and optionally repository, branch/sha and/or version.
#' @param ap is used for CRAN-alikes to get version number, if not part of `packages`
#' @param repos is used for `ap`.
#'
#' @details
#' If version is not supplied, it will take the local, installed version, if it
#' exists. Otherwise, it is assumed that the HEAD is desired.
#' The function will find it in the `ap` or on `github.com`. For github packages,
#' this is obviously a slow step, which can be accelerated if user supplies a sha
#' or a version e.g., saveNamesForCache("PredictiveEcology/LandR@development (==1.0.2)")
#'
#' @return
#' A (named) vector of SaveNames, which is a concatenation of the 2 or 4 elements
#' above, plus the `which` and the `recursive`.
saveNamesForCache <- function(packages, which, recursive, ap, type = type, repos, verbose) {
  fillDefaults(pkgDep)
  if (missing(which))
    which <- eval(formals(pkgDep)[["which"]])
  whichCatRecursive <- whichCatRecursive(which, recursive)
  if (is.data.table(packages)) {
    isGH <- if (is.null(packages$Account)) {
      rep(FALSE, NROW(packages))
    } else {
      !is.na(packages$Account)
    }
  } else {
    isGH <- isGitHub(packages)
  }
  set(packages, NULL, "ord", seq(NROW(packages)))
  if (any(!isGH)) {
    pkgDT <- saveNamesNonGH(packages[!isGH], ap, repos, type = type, verbose,
                            which = which[[1]], whichCatRecursive)
  }
  if (any(isGH)) {
    pkgDT1 <- saveNamesGH(packages[isGH], ap, verbose, which = which[[1]], whichCatRecursive)
    if (exists("pkgDT", inherits = FALSE)) {
      pkgDT <- rbindlist(list(pkgDT, pkgDT1), use.names = TRUE, fill = TRUE)
    } else {
      pkgDT <- pkgDT1
    }
  }
  set(pkgDT, NULL, "saveNamesLabel", saveNamesLabel(pkgDT))
  newOrd <- order(pkgDT$ord)
  pkgDT <- pkgDT[newOrd]
  return(pkgDT)
}


saveNamesNonGH <- function(packages, ap, repos, verbose, type, which,
                           whichCatRecursive, doSave = TRUE) {
  pkgDT1 <- toPkgDT(packages)
  needVersions <- FALSE
  if ("Version" %in% colnames(pkgDT1)) {
    haveVersions <- is.na(pkgDT1$Version)
    if (any(haveVersions)) needVersions <- TRUE
  } else {
    needVersions <- TRUE
  }
  if (needVersions) {
    if (endsWith(whichCatRecursive, "FALSE")) { # this is joining the ap, so can only be recursive = FALSE
      pkgDT1 <- joinToAvailablePackages(ap, pkgDT1, repos, type, which, verbose)

      # if (missing(ap))
      #   ap <- getAvailablePackagesIfNeeded(pkgDT1, repos, purge = FALSE, verbose, type = type)
      # set(ap, NULL, "packageFullName", paste0(ap$Package, " (==", ap$Version, ")"))
      # keep <- c("Package", "packageFullName", "Version", "Repository", which)
      # ap <- ap[ap$Package %in% pkgDT1$Package][, ..keep]
      # setnames(ap, old = "Version", new = "VersionOnRepos")
      # cn <- setdiff(colnames(ap), c("Package", "packageFullName", which))
      # onlyKeep <- c(setdiff(colnames(pkgDT1), cn))
      # pkgDT1 <- ap[pkgDT1[, ..onlyKeep], on = c("Package", "packageFullName")] # ap[pkgDT1, on = "Package"]
      # browser()
      # dups <- duplicated(pkgDT1, by = c("Package", "VersionOnRepos"))
      # if (any(dups))
      #   pkgDT1 <- pkgDT1[!dups]
    }
  }

  # set(pkgDT1, NULL, "sn", saveNameConcat(pkgDT1, whichCatRecursive))
  # pkgDT1[, sn := saveNameConcat(.SD, whichCatRecursive)]

  if (endsWith(whichCatRecursive, "FALSE")) { # this is joining the ap, so can only be recursive = FALSE
    if (isTRUE(doSave)) {
      # could be archived from CRAN
      onCRAN <- if (!is.null(pkgDT1$VersionOnRepos)) !is.na(pkgDT1$VersionOnRepos) else rep(FALSE, NROW(pkgDT1))
      whOnCRAN <- which(onCRAN)
      if (any(onCRAN)) {
        set(pkgDT1, whOnCRAN, "sn",
            assignPkgDTtoSaveNames(pkgDT1[whOnCRAN], which, verbose, whichCatRecursive))
      }
    }
  }

  pkgDT1[]
}

saveNamesGH <- function(packages, ap, verbose, which, whichCatRecursive, doSave = TRUE) {
  pkgDT <- parseGitHub(packages)
  pkgDT <- installedVersionOKPrecise(pkgDT)
  pkgDT <- parsePackageFullname(pkgDT, sorted = FALSE) # this sorted previously; now no
  # pkgDT <- whichToInstall(pkgDT, install = TRUE)

  installedOK <- pkgDT$installedVersionOK
  installedNotOK <- !installedOK
  shas <- character(NROW(pkgDT))
  set(pkgDT, NULL, "shas", shas)
  areNULLs <- rep(TRUE, NROW(pkgDT))
  if (isTRUE(any(installedNotOK))) {
    versionInPkgEnv <-
      getVersionOptionPkgEnv(paste0(pkgDT$Account[installedNotOK], "_", pkgDT$Package[installedNotOK]),
                             verNum = pkgDT$versionSpec[installedNotOK],
                             inequ = pkgDT$inequality[installedNotOK])
    if (!is.null(versionInPkgEnv)) {
      areNULLs <- sapply(versionInPkgEnv, is.null)
    }
    if (any(areNULLs) || length(areNULLs) == 0) {
      installedNoOKAndNoPkgEnv <- installedNotOK[areNULLs]
      installedNoOKAndNoPkgEnvWh <- which(installedNoOKAndNoPkgEnv)
      for (i in 1:2) {
        brLocals <- if (i == 1) pkgDT$Branch[installedNoOKAndNoPkgEnv] else rep("main", length(installedNoOKAndNoPkgEnv))
        haveLocalSHAfull <- grepl("^[[:alnum:]]{40}$", pkgDT$installedSha) # full length
        haveLocalSHA <- haveLocalSHAfull & !installedNoOKAndNoPkgEnv # full length
        if (!missing(ap)) {
          ghshacolname <- "GithubSHA1"
          if (ghshacolname %in% colnames(ap)) {
            isGHinAP <- !is.na(ap[[ghshacolname]])
            shaOuts <- pkgDT[installedNoOKAndNoPkgEnvWh][ap[isGHinAP], on = c("Branch" = ghshacolname)]$Branch
            haveLocalSHA <- shaOuts %in% brLocals
          }
        }
        installedNoOKAndNoPkgEnv <- installedNoOKAndNoPkgEnv & !haveLocalSHA
        installedNoOKAndNoPkgEnvWh <- which(installedNoOKAndNoPkgEnv)
        if (length(installedNoOKAndNoPkgEnvWh)) {
          shaOuts <- try(
            Map(
              repo = pkgDT$Repo[installedNoOKAndNoPkgEnvWh],
              acct = pkgDT$Account[installedNoOKAndNoPkgEnvWh],
              br = pkgDT$Branch[installedNoOKAndNoPkgEnvWh],
              verbose = verbose,
              getSHAfromGitHubMemoise
            )
          )
          pkgDT[installedNoOKAndNoPkgEnvWh, shas := unlist(shaOuts)]
        }
        if (sum(installedNoOKAndNoPkgEnv %in% FALSE)) {
          pkgDT[which(installedNoOKAndNoPkgEnv %in% FALSE), shas := installedSha]
        }
        needVersion <- needVersions(pkgDT)
        if (any(needVersion)) {
          descs <- getGitHubDESCRIPTION(pkgDT$packageFullName[needVersion])
          pkgDT[descs, descFiles := DESCFile,  on = "packageFullName"]
          pkgDT[!is.na(descFiles), Version := DESCRIPTIONFileVersionV(descFiles)]
        }
        break

      }
    }
  }
  pkgDT[, sn := saveNameConcat(.SD, whichCatRecursive)]

  if (isTRUE(doSave)) {
    deps2 <- DESCRIPTIONFileDepsV(pkgDT$descFiles, keepSeparate = T)
    deps3 <- depsAddWhichCol(deps2)
    assignPkgDepDTtoSaveNames(sn = pkgDT$sn, pkgDepDT = deps3)
  }

  pkgDT[]
}

saveNameConcat <- function(pkgDT, whichCatRecursive) {
  p <- pkgDT
  if ("Account" %in% colnames(pkgDT)) {
    # faster than using data.table
    out <- with(p, ifelse(is.na(Account),
                          saveNameConcatNonGH(Package, Version),
                          saveNameConcatGH(Package, Version, Account, Repo, shas)))
  } else {
    # non github
    out <- saveNameConcatNonGH(p$Package, p$Version)
  }
  paste(out, whichCatRecursive, sep = sepForSaveNames)
}

saveNameConcatGH <- function(Package, Version, Account, Repo, shas)
  paste(Package, Version, Account, Repo, shas, sep = sepForSaveNames)

saveNameConcatNonGH <- function(Package, Version)
  paste(Package, Version, sep = sepForSaveNames)

sepForSaveNames <- "__"

saveNamesLabel <- function(pkgDT) {
  if (!is.null(pkgDT$Account)) {
    isGH <- !is.na(pkgDT$Account)
  } else {
    isGH <- rep(FALSE, NROW(pkgDT))
  }
  set(pkgDT, NULL, "label", pkgDT$Package)
  set(pkgDT, which(isGH), "label",
      paste0(pkgDT$Account[isGH], "/",
             pkgDT$Repo[isGH], "@",
             pkgDT$Branch[isGH]))

  paste0(pkgDT$label, " (==", pkgDT$Version, ")")
}


needVersions <- function(pkgDT) {
  if ("Version" %in% colnames(pkgDT)) {
    needVersions <- is.na(pkgDT$Version)
  } else {
    needVersions <- rep(TRUE, NROW(pkgDT))
  }
  needVersions
}


updateWithRemotesNamespaceAddRepos2 <- function(pkgDT, which, purge, includeBase, verbose) {

  out <- pkgDT[, list(packageFullName = packageFullName, lis = {
    needed <- DESCRIPTIONFileDeps(DESCFile, which = which, purge = purge, keepSeparate = TRUE)
    neededAdditionalRepos <- DESCRIPTIONFileOtherV(DESCFile, other = "Additional_repositories")
    neededRemotes <- DESCRIPTIONFileDeps(DESCFile, which = "Remotes", purge = purge)
    pfn <- gsub("(@).+( *)", paste0("\\1", shas, "\\2"), packageFullName)
    # Change branch to use sha
    uwrnar(needed = needed, neededRemotes, installedVersionOK, Package,
           pfn, neededAdditionalRepos, shas = shas, includeBase, verbose)
  }), by = "packageFullName"]

  out1 <- out$lis
  names(out1) <- out$packageFullName
  return(out1)
  neededV <-
    try(DESCRIPTIONFileDepsV(pkgDT$DESCFile, which = which, purge = purge))
  if (is(neededV, "try-error")) {
    unlink(pkgDT$DESCFile)
    unlink(pkgDT$destFile)
    set(pkgDT, NULL, c("DESCFile", "destFile"), NULL)
    browserDeveloper(paste0("A problem occurred installing ", pkgDT$packageFullName, ". Does it exist?",
                            "\nTo confirm whether it exists, try browsing to ",
                            file.path("https://github.com", pkgDT$Account, pkgDT$Package, "tree", pkgDT$Branch),
                            "\nIf it does exist, try rerunning with `purge = TRUE`",
                            "\nIf this error is inaccurate, and the problem persists, ",
                            "please contact developers with error code 949"))
  }

  neededAdditionalReposV <- DESCRIPTIONFileOtherV(pkgDT$DESCFile, other = "Additional_repositories")

  neededRemotesV <-
    DESCRIPTIONFileDepsV(pkgDT$DESCFile, which = "Remotes", purge = purge)
  names(neededV) <- pkgDT$packageFullName

  Map(
    needed = neededV, neededRemotes = neededRemotesV,
    localVersionOK = pkgDT$installedVersionOK,
    neededAdditionalRepos = neededAdditionalReposV,
    localPackageName = pkgDT$Package,
    packageFullName = pkgDT$packageFullName,
    sha = pkgDT$shas,
    MoreArgs = list(includeBase = includeBase, verbose = verbose,
                    pkgDT = pkgDT),
    uwrnar
  )
}


uwrnar <- function(needed, neededRemotes, installedVersionOK, Package,
                   # Repo, Account, Branch, hasSubFolder,
                   packageFullName, neededAdditionalRepos, shas,
                   includeBase, verbose) {
  neededOrig <- needed
  if (!is.null(unlist(neededOrig))) {
    needed <- unname(unlist(needed))
    neededRemotesName <- extractPkgName(neededRemotes)
    neededName <- extractPkgName(needed)
    needSomeRemotes <- neededName %in% neededRemotesName
    if (any(needSomeRemotes)) {
      hasVersionNum <- grep(grepExtractPkgs, needed[needSomeRemotes])
      if (length(hasVersionNum)) {
        neededInRemotesWVersion <- needed[needSomeRemotes][hasVersionNum]
        vn <- extractVersionNumber(neededInRemotesWVersion)
        ineq <- extractInequality(neededInRemotesWVersion)
        neededPkgsInRemotes <-
          extractPkgName(neededInRemotesWVersion)
        inequWVN <- paste0(" (", ineq, " ", vn, ")")
        remotes <- neededRemotes[neededRemotesName %in% neededName]
        whNeedVN <-
          match(neededPkgsInRemotes, extractPkgName(remotes))
        remotesWVN <- remotes[whNeedVN]
        remotesWVN <- paste0(remotesWVN, inequWVN)
        remotesWoVN <-
          if (length(neededRemotes) != length(whNeedVN)) {
            remotes[-whNeedVN]
          } else {
            NULL
          }
        remotesAll <- c(remotesWVN, remotesWoVN)
      } else {
        remotesAll <- neededRemotes[neededRemotesName %in% neededName]
      }
      needed <- c(needed[!needSomeRemotes], remotesAll)
    }

    # Check NAMESPACE too -- because imperfect DESCRIPTION files
    if (isTRUE(installedVersionOK)) {
      namespaceFile <-
        base::system.file("NAMESPACE", package = Package)
    } else {
      namespaceFile <-
        getGitHubNamespace(packageFullName)[["NAMESPACE"]]
    }
    if (is.null(namespaceFile)) {
      depsFromNamespace <- NULL
    } else {
      rr <- readLines(namespaceFile)
      depsFromNamespace <- gsub(", except.*(\\))$", "\\1", rr)
      depsFromNamespace <-
        unique(gsub(
          "^import.*\\((.+)\\,.*$",
          "\\1",
          grep("^import", depsFromNamespace, value = TRUE)
        ))
      depsFromNamespace <-
        unique(gsub("^import\\((.+)\\)", "\\1", depsFromNamespace))
      depsFromNamespace <- gsub(",.*", "", depsFromNamespace)
      depsFromNamespace <- gsub("\\\"", "", depsFromNamespace)

      bp <- if (isTRUE(includeBase)) {
        NULL
      } else {
        .basePkgs
      }

    }
    # needed could have version number specification
    neededReally <- union(needed, depsFromNamespace)
    neededReally <- neededReally[!extractPkgName(neededReally) %in% bp]

    pkgDepDT <- toPkgDepDT(neededReally, needed,
                           Package, verbose)
    if (!is.na(neededAdditionalRepos))
      pkgDepDT[, Additional_repositories := neededAdditionalRepos]

    neededWithWhich <- unlist(neededOrig, use.names = TRUE)
    neededWithWhich <- list(Package = extractPkgName(unname(neededWithWhich)),
                            packageFullName = neededWithWhich, which = names(neededWithWhich)) |>
      setDT()
    set(neededWithWhich, NULL, "which", gsub("[[:digit:]]", "", neededWithWhich[["which"]]))
    pkgDepDT <- neededWithWhich[pkgDepDT, on = "Package"]
  } else {
    pkgDepDT <- toPkgDepDT(NULL, needed, Package, verbose)
    set(pkgDepDT, NULL, "which", character())
  }

  list(pkgDepDT)
}

fillDefaults <- function(fillFromFn, envir = parent.frame()) {
  i <- 0
  sf <- sys.frames()
  len <- length(sf)
  for (en in rev(sf)) {
    i  = i + 1
    if (identical(en, envir))
      break
  }
  whFrame <- len - i + 1
  # whFrame <- which(sapply(sys.frames(), function(en) identical(en, envir)))
  mc <- match.call(sys.function(whFrame), call = sys.call(whFrame))
  used <- names(mc)[-1]
  forms <- formalArgs(eval(as.list(mc[1])[[1]]))
  # miss <- setdiff(forms, used)
  miss <- forms[!forms %in% used]
  for (m in miss) {
    defVal <- eval(formals(fillFromFn)[[m]])
    assign(m, defVal, envir = parent.frame())
  }
}




getArchiveDESCRIPTION <- function(pkgDTList, repos, purge = FALSE, which, verbose) {
  pkgDTList <- downloadArchive(pkgDTList, repos = repos, purge = purge, verbose = verbose)

  tmpdir <- Require::tempdir2(.rndstr())
  if (any(!is.na(pkgDTList$Archive$PackageUrl))) {
    pkgDTList$Archive[!is.na(PackageUrl), DESCFileFull := {
      tf <- file.path(Require:::RequirePkgCacheDir(), basename(PackageUrl))
      if (length(tf) > 1) browser()
      out <- if (file.exists(tf)) { NULL } else {
        try(download.file(
          url = file.path(Repository, basename(PackageUrl)),
          destfile = tf), silent = TRUE)
      }
      if (is(out, "try-error")) {
        message(out)
        out <- NA
      } else {
        DESCFile <- file.path(Package, "DESCRIPTION")
        untar(tf, files = DESCFile, exdir = tmpdir)
        out <- file.path(tmpdir, DESCFile)
      }
      out
    }, by = "packageFullName"]

    gotDESC <- !is.na(pkgDTList$Archive$DESCFileFull)
    deps <- DESCRIPTIONFileDepsV(
      pkgDTList$Archive[gotDESC]$DESCFileFull,
      which = which, keepSeparate = TRUE)
    names(deps) <- pkgDTList$Archive$packageFullName[gotDESC]
    deps <- lapply(deps, function(x) {
      unlist(lapply(x, function(y) paste(y, collapse = ", ")))
    })
    deps <- do.call(rbind, deps)
    # deps <- invertList(deps)
    # deps <- as.data.table(deps)
    for (co in colnames(deps))
      set(pkgDTList$Archive, which(gotDESC), co, deps[, co])
  }
  pkgDTList
}


installed.packagesDeps <- function(ip, which) {
  if (missing(ip))
    ip <- .installed.pkgs()

  if (!is.data.table(ip))
    ip <- as.data.table(ip)

  ip[, deps := do.call(paste, append(.SD, list(sep = ", "))), .SDcols=which]

  # remove trailing and initial commas
  set(ip, NULL, "deps", gsub("(, )+$", "", ip$deps))
  set(ip, NULL, "deps", gsub("^(, )+", "", ip$deps))
  # remove middle empty commas
  set(ip, NULL, "deps", gsub(", ,", ",", ip$deps))
  # remove empty, but no space
  set(ip, NULL, "deps", gsub("(,,)+", ",", ip$deps))
}

depsWithCommasToVector <- function(packageFullName, depsWithCommas) {
  Map(pkgFN = packageFullName, x = depsWithCommas, function(pkgFN, x) {
    out <- strsplit(x, split = "(, {0,1})|(,\n)")[[1]]
    out <- out[!is.na(out)]
    out <-
      grep(.grepR, out, value = TRUE, invert = TRUE)
    out
  })
}


cleanPkgs <- function(packageFullName) {
  pkgsCleaned <- gsub(.grepTooManySpaces, " ", packageFullName)
  pkgsCleaned <- gsub(.grepTabCR, "", pkgsCleaned)
  gsub("(>|=|<)([[:digit:]])", "\\1 \\2", pkgsCleaned)
}


assignPkgDTtoSaveNames <- function(pkgDT, which, verbose, whichCatRecursive, versionCol = "VersionOnRepos") {
  deps1 <- list()
  # packageFullName1 <- paste0(pkgDT$Package, " (==", pkgDT[[versionCol]], ")")
  packageFullName <- pkgDT$packageFullName
  # if (!identical(packageFullName1, packageFullName)) browser()
  for (cn in which)
    deps1[[cn]] <- depsWithCommasToVector(packageFullName, pkgDT[[cn]])
  deps2 <- invertList(deps1)
  dep3 <- lapply(deps2, function(d) unique(unname(unlist(d))))
  depsAll <- Map(toPkgDepDT, dep3, verbose = verbose)
  versionColForConcat <- "Version"
  if (!identical(versionColForConcat, versionCol)) {
    setnames(pkgDT, old = versionCol, new = versionColForConcat)
    on.exit(setnames(pkgDT, new = versionCol, old = versionColForConcat))
  }
  actualSN <- saveNameConcat(pkgDT, whichCatRecursive)
  assignPkgDepDTtoSaveNames(actualSN, pkgDepDT = depsAll)
  actualSN
}

whichCatRecursive <- function(which, recursive) {
  whichCat <- paste(sort(which[[1]]), collapse = "_")
  if (is.logical(recursive))
    recursive <- as.character(recursive)
  if (!startsWith(recursive, "Recursive"))
    recursive <- paste0("Recursive", recursive)
  paste(whichCat, recursive, sep = "")[1]
}


depsAddWhichCol <- function(deps2) {
  deps3 <- lapply(deps2, function(dep) {
    dep1 <- unlist(dep, use.names = TRUE)
    dep2 <- list(Package = extractPkgName(unname(dep1)),
      packageFullName = dep1,
      which = names(dep1)) |>
      setDT()
    set(dep2, NULL, "which", gsub("[[:digit:]]", "", dep2[["which"]]))
    dep2
  })
}


joinToAvailablePackages <- function(ap, pkgDT1, repos, type, which, verbose) {
  if (missing(ap))
    ap <- getAvailablePackagesIfNeeded(pkgDT1, repos, purge = FALSE, verbose, type = type)
  set(ap, NULL, "packageFullName", paste0(ap$Package, " (==", ap$Version, ")"))
  keep <- c("Package", "packageFullName", "Version", "Repository", which)
  ap <- ap[ap$Package %in% pkgDT1$Package][, ..keep]
  setnames(ap, old = "Version", new = "VersionOnRepos")
  cn <- setdiff(colnames(ap), c("Package", "packageFullName", which))
  onlyKeep <- c(setdiff(colnames(pkgDT1), cn))
  pkgDT1 <- ap[pkgDT1[, ..onlyKeep], on = c("Package", "packageFullName")] # ap[pkgDT1, on = "Package"]
  dups <- duplicated(pkgDT1, by = c("Package", "VersionOnRepos"))
  if (any(dups))
    pkgDT1 <- pkgDT1[!dups]
  pkgDT1
}
