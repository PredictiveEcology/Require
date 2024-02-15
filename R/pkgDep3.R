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
                   sort = TRUE,
                   purge = getOption("Require.purge", FALSE),
                   verbose = getOption("Require.verbose"),
                   includeSelf = TRUE,
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
                       type = type, which = which, purge = purge, includeBase = includeBase,
                       includeSelf = includeSelf)
  }

  # collapse or not to list of character vector
  if (!isTRUE(Additional_repositories)) {
    deps <- Map(pkgFN = deps, function(pkgFN) pkgFN[["packageFullName"]])
  }


  deps
}

getPkgDeps <- function(packages, recursive, which, repos, type, purge, includeBase,
                       includeSelf, verbose, .inner = FALSE) {

  deps <- NULL
  if (NROW(packages) > 0) {
    if (is.data.table(packages))
      packages <- packages$packageFullName
    pkgDT <- parseGitHub(packages)
    pkgDT <- updatePackagesWithNames(pkgDT, packages)
    pkgDT <- parsePackageFullname(pkgDT, sorted = FALSE)
    pkgDT <- trimRedundancies(pkgDT, repos = repos, purge = FALSE) # changes order

    ap <- getAvailablePackagesIfNeeded(pkgDT, repos, purge = FALSE, verbose, type)

    recTF <- paste0("Recursive", c("TRUE", "FALSE"))
    rec <- if (recursive %in% TRUE) c(recTF[1], recTF[2]) else recTF[2] # need to do both if TRUE
    alreadySaved <- list()
    for (recursiveUsed in rec) {
      pkgDT <- saveNamesForCache(pkgDT, which, recursive = recursiveUsed, ap = ap,
                                 repos = repos, type = type, verbose = verbose - 1)
      alreadySaved[[recursiveUsed]] <- pkgDT$sn %in% names(pkgDepDepsEnv())
      names(alreadySaved[[recursiveUsed]]) <- pkgDT$sn
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
      depsNew <- getPkgDepsNonRecursive(pkgDT, which, repos, type, purge,
                                        includeBase = includeBase, ap = ap, verbose = verbose)
      if (isTRUE(any(sapply(depsNew, function(d) any(nchar(d$Package) == 0))))) browser()
      assignPkgDepDTtoSaveNames(pkgDT$sn, depsNew)
      deps <- append(deps, depsNew) # add the ones recovered from saved

      if (recursive %in% TRUE) {
        deps <- getPkgDepsMap(deps, recursive,  repos, which, type, purge, libPaths,
                              includeBase = includeBase, includeSelf = includeSelf, verbose)
      }
    }
    if (.inner %in% FALSE) {
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
      deps <- deps[match(packages, names(deps))] # reorder to initial order

      deps <- Map(dep = deps, function(dep) {
        set(dep, NULL, "packageFullName", {
          pkgsCleaned <- gsub(.grepTooManySpaces, " ", dep$packageFullName)
          gsub(.grepTabCR, "", pkgsCleaned)
        })
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


getPkgDepsMap <- function(deps, recursive, repos, which, type, purge, libPaths,
                          includeBase, includeSelf, verbose) {
  ndeps <- sapply(deps, NROW)
  noDeps <- ndeps == 0
  out <- Map(packages = deps[!noDeps], f = getPkgDeps,
             MoreArgs = list(recursive = recursive, which = which, repos = repos,
                             type = type, purge = purge, includeBase = includeBase,
                             includeSelf = includeSelf,
                             .inner = TRUE, verbose = verbose))
  out2 <- try(lapply(out, rbindlist, fill = TRUE, use.names = TRUE))
  out3 <- Map(na = names(out2), function(na) {
    rr <- rbindlist(list(deps[[na]], out2[[na]]), use.names = TRUE, fill = TRUE)
    rr <- trimRedundancies(rr, purge = purge, repos = repos, libPaths = libPaths,
                           type = type)
  })

  if (any(noDeps)) {
    deps <- append(out3, deps[noDeps])
  } else {
    deps <- out3
  }
}


getPkgDepsNonRecursive <- function(pkgDT, which, repos, type, purge, includeBase, ap, verbose) {
  pkgDTList <- split(pkgDT, by = "repoLocation")
  pkgDTDep <- list()
  if (!is.null(pkgDTList[["CRAN"]])) {
    pkgDTDep[["CRAN"]] <- pkgDepCRAN3(pkgDT = pkgDTList$CRAN, which = which[[1]],
                                          repos = repos, type = type, purge = purge, ap = ap)
  }
  if (!is.null(pkgDTList[["GitHub"]]))
    pkgDTDep[["GitHub"]] <- pkgDepGitHub(pkgDT = pkgDTList$GitHub, which = which[[1]],
                                         purge = purge, includeBase = includeBase, verbose = verbose)

  depsNew <- unlist(unname(pkgDTDep), recursive = FALSE)
  depsNew <- depsNew[match(pkgDT$packageFullName, names(depsNew))] # return to original order
  depsNew
}

pkgDepCRAN3 <- function(pkgDT, which, repos, purge, type, ap,
           verbose = getOption("Require.verbose")) {

    if (is.null(pkgDT$Depends)) {
      if (missing(ap))
        ap <- getAvailablePackagesIfNeeded(pkgDT$Package, repos = repos, purge = purge, verbose = verbose, type = type)
      keepNames <- c("Package", setdiff(colnames(pkgDT), colnames(ap)))
      pkgDT <- ap[pkgDT[, ..keepNames], on = "Package"]
    }

    pkgDT[is.na(versionSpec), availableVersionOK := TRUE]
    pkgDT[!is.na(versionSpec), availableVersionOK := compareVersion2(Version, versionSpec, inequality)]
    for (co in which)
      set(pkgDT, which(is.na(pkgDT[[co]])), co, "")

    pkgDT[, deps := do.call(paste, append(.SD, list(sep = ", "))), .SDcols=which]

    # remove trailing and initial commas
    set(pkgDT, NULL, "deps", gsub("(, )+$", "", pkgDT$deps))
    set(pkgDT, NULL, "deps", gsub("^(, )+", "", pkgDT$deps))
    # remove middle empty commas
    set(pkgDT, NULL, "deps", gsub(", ,", ",", pkgDT$deps))

    deps <- Map(pkgFN = pkgDT$packageFullName, x = pkgDT$deps, function(pkgFN, x) {
      out <- strsplit(x, split = "(, {0,1})|(,\n)")[[1]]
      out <- out[!is.na(out)]
      out <-
        grep(.grepR, out, value = TRUE, invert = TRUE)
      out
    })
    depsAll <- Map(toPkgDepDT, deps, verbose = verbose)
    depsAll
  }
#


#' @inheritParams Require
pkgDepGitHub <- function(pkgDT, which, purge, includeBase = FALSE, verbose = getOption("Require.verbose")) {
    pkg <- masterMainToHead(pkgDT$packageFullName)

    localVersionOK <- pkgDT$installedVersionOK
    pkgDepDTOuter <- data.table(packageFullName = character())

    if (any(!localVersionOK %in% TRUE)) {
      pkgDTNotLocal <- getGitHubDESCRIPTION(pkgDT[!localVersionOK %in% TRUE], purge = purge)
    }
    if (any(localVersionOK %in% TRUE)) {
      pkgDT[localVersionOK %in% TRUE, DESCFile := system.file("DESCRIPTION", package = Package), by = "Package"]
      if (exists("pkgDTNotLocal", inherits = FALSE))
        pkgDT <- rbindlist(list(pkgDT[localVersionOK %in% TRUE], pkgDTNotLocal), fill = TRUE, use.names = TRUE)
    } else {
      pkgDT <- pkgDTNotLocal
    }

    hasVersionNum <- pkgDT$hasVers # grep(grepExtractPkgs, pkgDT$packageFullName)
    set(pkgDT, NULL, "availableVersionOK", NA)
    if (any(hasVersionNum)) {
      if (!isTRUE(getOption("Require.offlineMode"))) {
        pkgDT[hasVersionNum, VersionOnRepos := DESCRIPTIONFileVersionV(DESCFile, purge = purge)]
        if (is.null(pkgDT$versionSpec)) {
          pkgDT[hasVersionNum, versionSpec := extractVersionNumber(packageFullName)]
          pkgDT[hasVersionNum, inequality := extractInequality(packageFullName)]
        }
        pkgDT[hasVersionNum, availableVersionOK := compareVersion2(VersionOnRepos, versionSpec, inequality)]
      }
    }


    if (any(!pkgDT$availableVersionOK %in% FALSE)) {
      pkgDepDTOuter <- updateWithRemotesNamespaceAddRepos2(pkgDT, which, purge, includeBase, verbose = verbose)
    }

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
saveNamesForCache3 <- function(packages, which, recursive, ap, type = type, repos, verbose) {
  if (missing(which))
    which <- eval(formals(pkgDep)[["which"]])
  whichCat <- paste(sort(which[[1]]), collapse = "_")

  if (missing(recursive))
    recursive <- formals(pkgDep)[["recursive"]]
  if (!startsWith(recursive, "Recursive"))
    recursive <- paste0("Recursive", recursive)
  whichCatRecursive <- paste(whichCat, recursive, sep = "")[1]
  if (is.data.table(packages)) {
    isGH <- if (is.null(packages$Account)) {
      rep(FALSE, NROW(packages))
    } else {
      !is.na(packages$Account)
    }
  } else {
    isGH <- isGitHub(packages)
  }
  ord <- seq(NROW(packages))
  if (any(!isGH)) {
    pkgDT <- saveNamesNonGH(packages, isGH, ord, ap, repos, type = type, verbose, whichCatRecursive)
  }
  if (any(isGH)) {
    pkgDT1 <- saveNamesGH(packages, isGH, ord, ap, verbose, whichCatRecursive)
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

saveNamesNonGH <- function(packages, isGH, ord, ap, repos, verbose, type, whichCatRecursive) {
  pkgDT1 <- toPkgDT(packages[!isGH])
  set(pkgDT1, NULL, "ord", ord[!isGH])
  needVersions <- FALSE
  if ("Version" %in% colnames(pkgDT1)) {
    haveVersions <- is.na(pkgDT1$Version)
    if (any(haveVersions)) needVersions <- TRUE
  } else {
    needVersions <- TRUE
  }
  if (needVersions) {
    if (missing(ap))
      ap <-
        getAvailablePackagesIfNeeded(pkgDT1, repos, purge = FALSE, verbose, type = type)
    pkgDT1 <- ap[ap$Package %in% pkgDT1$Package][pkgDT1, on = "Package"] # ap[pkgDT1, on = "Package"]
  }

  set(pkgDT1, NULL, "sn", saveNameConcat(pkgDT1, whichCatRecursive))
  # pkgDT1[, sn := saveNameConcat(.SD, whichCatRecursive)]
  dups <- duplicated(pkgDT1$sn)
  if (any(dups))
    pkgDT1 <- pkgDT1[!dups]
  pkgDT1[]
}

saveNamesGH <- function(packages, isGH, ord, ap, verbose, whichCatRecursive) {
  pkgDT <- parseGitHub(packages[isGH])
  set(pkgDT, NULL, "ord", ord[isGH])
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
      installedNoOKAndNoPkgEnv <- which(installedNotOK[areNULLs])
      for (i in 1:2) {
        brLocals <- if (i == 1) pkgDT$Branch[installedNoOKAndNoPkgEnv] else rep("main", length(installedNoOKAndNoPkgEnv))
        haveLocalSHAfull <- grepl("^[[:alnum:]]{40}$", pkgDT$installedSha)
        haveLocalSHA <- haveLocalSHAfull[installedNoOKAndNoPkgEnv]
        if (!missing(ap)) {
          ghshacolname <- "GithubSHA1"
          if (ghshacolname %in% colnames(ap)) {
            isGHinAP <- !is.na(ap[[ghshacolname]])
            shaOuts <- pkgDT[installedNoOKAndNoPkgEnv][ap[isGHinAP], on = c("Branch" = ghshacolname)]$Branch
            haveLocalSHA <- shaOuts %in% brLocals
          }
        }
        installedNoOKAndNoPkgEnv <- installedNoOKAndNoPkgEnv[!haveLocalSHA]
        if (length(installedNoOKAndNoPkgEnv)) {
          shaOuts <- try(
            Map(
              repo = pkgDT$Repo[installedNoOKAndNoPkgEnv],
              acct = pkgDT$Account[installedNoOKAndNoPkgEnv],
              br = pkgDT$Branch[installedNoOKAndNoPkgEnv],
              verbose = verbose,
              getSHAfromGitHubMemoise
            )
          )
          pkgDT[installedNoOKAndNoPkgEnv, shas := unlist(shaOuts)]
        }
        wh <- which(haveLocalSHAfull)
        pkgDT[wh, shas := installedSha]
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
    needed <- DESCRIPTIONFileDeps(DESCFile, which = which, purge = purge)
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

  browser()
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
      system.file("NAMESPACE", package = Package)
  } else {
    namespaceFile <-
      getGitHubNamespace(packageFullName)[["NAMESPACE"]]
  }
  if (is.null(namespaceFile)) {
    needed <- NULL
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

    # needed could have version number specification
    neededReally <- union(needed, depsFromNamespace)
    neededReally <- neededReally[!extractPkgName(neededReally) %in% bp]

    pkgDepDT <- toPkgDepDT(neededReally, needed,
                           Package, verbose)
    if (!is.na(neededAdditionalRepos))
      pkgDepDT[, Additional_repositories := neededAdditionalRepos]
  }
  list(pkgDepDT)
}
