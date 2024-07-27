utils::globalVariables(
  c("Additional_repositories", "githubPkgName",
    "localBranch", "localFiles", "localRepo", "installedSha", "localUsername",
    "newPackageFullName", "DESCFileFull", "correctVersionInCache", "descFiles",
    "..keep", "..keepShort", "..onlyKeep", "..keep", "..keepCols", "..keepColsOuter",
    "shas")
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
#' @param libPaths A path to search for installed packages. Defaults to
#'   `.libPaths()`
#' @param sort Logical. If `TRUE`, the default, then the packages will be sorted
#'   alphabetically. If `FALSE`, the packages will not have a discernible order
#'   as they will be a concatenation of the possibly recursive package
#'   dependencies.
#' @param includeBase Logical. Should R base packages be included, specifically,
#'   those in `tail(.libPaths(), 1)`
#' @param includeSelf Logical. If `TRUE`, the default, then the dependencies
#'   will include the package itself in the returned list elements, otherwise,
#'   only the "dependencies"
#' @param simplify Logical or numeric. If `TRUE` (or > 0), the default,
#'   the return object is "just" a character vector of package names
#'   (with version requirements). If `FALSE` (or `0`),
#'   then a `data.table` will be returned with 4 columns,
#'   `Package`, `packageFullName`, `parentPackage` (the package name for which the
#'   given line entry is a dependency; will be "user" if it was user supplied)
#'   and `deps`, which is a list of `data.table`s
#'   of all dependencies. If a negative number, then it will return a similar `data.table`
#'   as with `FALSE`, however, duplications in the recursive package dependencies
#'   are left intact.
#' @param Additional_repositories Logical. If `TRUE`, then `pkgDep` will return
#'   a list of `data.table` objects (instead of character vectors)
#'   with a column `packageFullName` and possibly a second column `Additional_repositories`,
#'   which may have been specified in a `DESCRIPTION` file. NOTE: THIS ALTERS
#'   THE OUTPUT CLASS
#' @param ... Currently only `dependencies` as an alternative to `which`. If specified,
#'   then `which` will be ignored.
#'
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
                   libPaths,
                   which = c("Depends", "Imports", "LinkingTo"),
                   recursive = TRUE,
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
                   Additional_repositories = FALSE, ...) {

  libPaths <- dealWithMissingLibPaths(libPaths, ...)

  doDeps <- if (!is.null(list(...)$dependencies)) list(...)$dependencies else NULL
  if (!is.null(doDeps))
    which <- whichToDILES(doDeps)

  purge <- purgePkgDep(purge)
  # purge <- dealWithCache(purge)
  checkAutomaticOfflineMode() # This will turn off offlineMode if it had been turned on automatically

  # deps <- packages
  if (!includeBase) {
    packages <- packages[!packages %in% .basePkgs]
  }
  if (length(packages)) {
    which <- depsImpsSugsLinksToWhich(depends, imports, suggests, linkingTo, which)
    if (getOption("Require.usePak", TRUE)) {
      if (!requireNamespace("pak")) stop("Please install pak")
      log <- tempfile2(fileext = ".txt")
      withCallingHandlers(
        deps <- pakPkgDep(packages, which, simplify, includeSelf, includeBase, keepVersionNumber,
                          verbose = verbose)
        , message = function(m) {
          if (verbose > 1)
            cat(m$message, file = log, append = TRUE)
          if (verbose < 1)
            invokeRestart("muffleMessage")
        }
      )
    } else {

      # Only deal with first one of "which"...
      if (is.list(which)) which <- which[[1]]

      deps <- getPkgDeps(packages, parentPackage = "user", recursive = recursive, repos = repos, verbose = verbose,
                         type = type, which = which, includeBase = includeBase, libPaths = libPaths,
                         includeSelf = includeSelf, grandp = "aboveuser")
      depsCol <- "deps"
      if (!identical(which, character())) { # when which = character(), results in a NULL list element: [[1]] NULL
        set(deps, NULL, depsCol, lapply(deps[[deps(recursive)]], rbindlistRecursive))
      }
    keepCols <- c("Package", "packageFullName", "Version", "versionSpec", "inequality",
                "githubPkgName", "repoLocation", ".depth", "which", "parentPackage")
    dotDepth <- ".depth"

    set(deps, NULL, #whHasDeps,
        depsCol,
        Map(dep = deps[[depsCol]], self = deps[["packageFullName"]],
            # Map(dep = deps[[depsCol]][whHasDeps], self = deps[["packageFullName"]][whHasDeps],
            function(dep, self) {
              if (!is.null(dep)) {

                  rmCols <- intersect(colnames(dep), c(deps(TRUE), deps(FALSE)))
                  if (length(rmCols))
                    set(dep, NULL, rmCols, NULL)
                  # if (dotDepth %in% colnames(dep) && !all(diff(dep[[dotDepth]]) >= 0)) {
                  #   if (!exists("sorted")) sorted <<- 0
                  #   sorted <<- sorted + 1
                  #   setorderv(dep, cols = dotDepth)
                  # } else {
                  #   if (!exists("unsorted")) unsorted <<- 0
                  #   unsorted <<- unsorted + 1
                  # }

                  keepCols <- intersect(colnames(dep), c(keepCols, .txtGitHubParsedCols))
                  if (simplify >= 0)
                    dep <- dep[!duplicated(dep[["Package"]])]
                  set(dep, NULL, c("packageFullName", "parentPackage"),
                      list(cleanPkgs(dep[["packageFullName"]]), cleanPkgs(dep[["parentPackage"]])))
                  dep <- rmBase(includeBase, dep)
                  dep <- dep[, ..keepCols] # includeSelf added some cols
                  # setnames(dep, old = depsCol, new = "deps")
                }
                dep <- addSelf(includeSelf, dep, self)
                if (!is.null(dep))
                  setorderv(dep, cols = c("Package", intersect(dotDepth, colnames(dep))))# <- dep[order(dep$Package)]
                dep
              }))

      keepColsOuter <- c("Package", "packageFullName", "parentPackage", depsCol)
      deps <- deps[, ..keepColsOuter]
      # trimRedundancies is better for following the deps, but it will fail to keep original
      #   user request. Use only duplicated instead ... to keep user order

      if (keepVersionNumber %in% FALSE) {
        deps <- Map(pkgFN = deps$deps, function(pkgFN) pkgFN[["Package"]])
      } else if (simplify %in% TRUE) {
        # collapse or not to list of character vector
        pfn <- deps$packageFullName
        deps <- lapply(deps[[depsCol]], function(x) x[["packageFullName"]])
        names(deps) <- pfn
      }
    }
  } else {
    deps <- toPkgDepDT(packages)
    # names(deps) <- unlist(deps)
  }

  return(deps)
}

getPkgDeps <- function(pkgDT, parentPackage, recursive, which, repos, type, includeBase,
                       includeSelf, libPaths, verbose, .depth = 0, .counter, grandp) {

  if (is.list(which)) which <- which[[1]]

  cols <- c("red", "blue", "yellow", "green", "black", "turquoise", "cyan")

  lim <- 5
  if (isNotNULLAnd(.depth, .depth <= lim))
    messageVerbose(get(cols[.depth + 1])(paste(rep("  ", .depth), collapse = ""),
                                         cleanPkgs(parentPackage)),
                   verbose = .depth <= (lim ) && verbose >= 3)
  deps <- NULL
  snTr <- sn(TRUE)
  snFa <- sn(FALSE)
  depTr <- deps(TRUE)
  depFa <- deps(FALSE)
  caFa <- cached(FALSE)
  caTr <- cached(TRUE)

  if (NROW(pkgDT) > 0) { # this will skip any pkgDT that has no deps, breaks out of recursion
    if (is(pkgDT, "list")) pkgDT <- pkgDT[[1]]
    pkgDT <- toPkgDTFull(pkgDT)
    pkgDT <- rmRifInPackageCol(pkgDT)
    isInBase <- pkgDT$Package %in% .basePkgs
    if (!all(isInBase)) {
      pkgDTBase <- splitKeepOrderAndDTIntegrity(pkgDT, splitOn = isInBase)
      if (NROW(pkgDTBase[["FALSE"]])) {
        pkgDTBase[["FALSE"]] <- getDeps(pkgDTBase[["FALSE"]], which, recursive = recursive,
                                        repos = repos, type = type, libPaths = libPaths, verbose = verbose)
        hasDeps <- sapply(pkgDTBase$`FALSE`[[depFa]], NROW) > 0
        if (any(hasDeps)) {
          if (recursive %in% TRUE && any(pkgDTBase[["FALSE"]]$cachedTRUE %in% FALSE)) {
            which <- setdiff(which, "Suggests")
            pkgDTNeedRecursive <-
              splitKeepOrderAndDTIntegrity(pkgDTBase[["FALSE"]],
                                           splitOn = pkgDTBase[["FALSE"]]$cachedTRUE %in% FALSE)

            pkgDTNeedRecursive[["TRUE"]][[depFa]] <-
              # out <-
              Map(pkgDT = pkgDTNeedRecursive[["TRUE"]][[depFa]],
                  pp = pkgDTNeedRecursive[["TRUE"]]$packageFullName,
                  .counter = seq(NROW(pkgDTNeedRecursive[["TRUE"]])),
                  function(pkgDT, pp, .counter, grandp)
                    getPkgDeps(pkgDT, parentPackage = pp,
                               grandp = parentPackage,
                               .counter = .counter,
                               recursive = recursive, which = which, repos = repos,
                               type = type, includeBase = includeBase,
                               includeSelf = includeSelf,
                               libPaths = libPaths,
                               .depth = .depth + 1, verbose = verbose))


            pkgDTBase[["FALSE"]] <- appendRecursiveToDeps(pkgDTNeedRecursive, caTr, depFa, caFa, snFa, depTr, snTr)
            hasRecursiveTRUE <- !is.na(pkgDTBase[["FALSE"]][[snTr]])
            whHasRecursiveTRUE <- which(hasRecursiveTRUE)
            whNotHasRecTRUE <- which(!hasRecursiveTRUE)

            if (length(whNotHasRecTRUE)) {
              if (!is.null(pkgDTBase[["FALSE"]][[snFa]]))
                set(pkgDTBase[["FALSE"]], whNotHasRecTRUE, snTr,
                    pkgDTBase[["FALSE"]][[snFa]][whNotHasRecTRUE])
              if (!is.null(pkgDTBase[["FALSE"]][[depFa]]))
                set(pkgDTBase[["FALSE"]], whNotHasRecTRUE, depTr,
                    pkgDTBase[["FALSE"]][[depFa]][whNotHasRecTRUE])
            }
            # noSnTr <- which(is.na(pkgDTBase[["FALSE"]][[snTr]]))
            # set(pkgDTBase[["FALSE"]],  NULL, snTr, gsub("FALSE$", "TRUE", pkgDTBase[["FALSE"]][[snTr]]))
            # if (length(whHasRecursiveTRUE)) {
            #   set(pkgDTBase[["FALSE"]],  NULL, snTr,
            #       gsub("FALSE$", "TRUE", pkgDTBase[["FALSE"]][[snTr]]))
            # }
            # }
            #   len <- length(pkgDTBase[["FALSE"]][[depTr]][whHasRecursiveTRUE])
            # moveDeps <- FALSE
            # if (len < 16) moveDeps <- is.null(unlist(moveDeps))
            #
            # if (!moveDeps) {
            #   set(pkgDTBase[["FALSE"]], whHasRecursiveTRUE, depTr, pkgDTBase[["FALSE"]][[depTr]][whHasRecursiveTRUE])
            # } else {
            #   set(pkgDTBase[["FALSE"]], NULL, depTr, lapply(seq(NROW(pkgDTBase[["FALSE"]])), function(x) emptyDT()))
            # }

            isNULL <- sapply(pkgDTBase[["FALSE"]][[depTr]][whHasRecursiveTRUE], is.null)
            if (any(!isNULL)) {
              assignPkgDTdepsToSaveNames(pkgDTBase[["FALSE"]], recursive = TRUE)#,
                                         #rowsToUpdate = whHasRecursiveTRUE[!isNULL])
              # if (!all(sapply(seq(NROW(pkgDT))[!is.na(pkgDT$snFALSE)], function(i)
              #   isTRUE(all.equal(pkgDT[[depTr]][[i]], get(pkgDT[[snTr]][i], envir = envPkgDepDeps()))))))
            }
            #}
          }
        } else {
          keep <- match(colnames(pkgDTBase[["FALSE"]]), c(depFa, caFa)) |> na.omit()
          setnames(pkgDTBase[["FALSE"]],
                   old = c(depFa, caFa)[keep],
                   new = c(depTr, caTr)[keep])

        }
      }
      pkgDT <- rbindlistNULL(pkgDTBase, fill = TRUE, use.names = TRUE)

    }

  }
  pkgDT <- addDepthAndParentPkg(pkgDT, nam = parentPackage, .depth)

  return(pkgDT)
}


#' The `packages` argument may have up to 4 pieces of information for GitHub
#' packages: name, repository, branch, version. For CRAN-alikes, it will only
#' be 2 pieces: name, version. There can also be an inequality or equality, if
#' there is a version.
#'
#' @param repos is used for `ap`.
#'
#' @details
#' If version is not supplied, it will take the local, installed version, if it
#' exists. Otherwise, it is assumed that the HEAD is desired.
#' The function will find it in the `ap` or on `github.com`. For github packages,
#' this is obviously a slow step, which can be accelerated if user supplies a sha
#' or a version e.g., getDeps("PredictiveEcology/LandR@development (==1.0.2)")
#' @inheritParams pkgDep
#' @param pkgDT A `pkgDT` object e.g., from `toPkgDT`
#' @return
#' A (named) vector of SaveNames, which is a concatenation of the 2 or 4 elements
#' above, plus the `which` and the `recursive`.
getDeps <- function(pkgDT, which, recursive, type = type, repos, libPaths, verbose) {
  fillDefaults(pkgDep)

  for (tf in c(TRUE, FALSE))
    if (is.null(pkgDT[[cached(tf)]]))
      set(pkgDT, NULL, cached(tf), FALSE)
  set(pkgDT, which(pkgDT$Package %in% .basePkgs), cached(FALSE), TRUE)
  set(pkgDT, which(pkgDT$Package %in% .basePkgs), cached(TRUE), TRUE)

  if (getOption("Require.useCache", TRUE)) {
    pkgDT <- getFromCache(pkgDT, which, recursive)
  }

  if (any(!pkgDT[[cached(recursive)]] %in% TRUE) &&
      sum(!pkgDT[["Package"]][!pkgDT[[cached(recursive)]]] %in% .basePkgs)) {

    recursiveHere <- FALSE
    set(pkgDT, NULL, "ord", seq(NROW(pkgDT)))
    pkgDTCached <- splitKeepOrderAndDTIntegrity(pkgDT, pkgDT[[cached(recursiveHere)]])
    messageVerbose("Not in Cache: ", paste(unique(pkgDTCached[["FALSE"]]$Package), collapse = comma),
                   verbose = verbose, verboseLevel = 3)
    whichCatRecursive <- whichCatRecursive(which, recursiveHere)
    isGH <- !is.na(pkgDTCached[["FALSE"]]$githubPkgName)
    pkgDTNonGH <- pkgDTGH <- NULL
    if (!all(pkgDTCached[["FALSE"]]$Package %in% .basePkgs)) {
      if (any(!isGH)) {
        pkgDTNonGH <- getDepsNonGH(pkgDTCached[["FALSE"]][!isGH], repos, type = type, verbose,
                                   which = which, whichCatRecursive, libPaths = libPaths)
      }
      if (any(isGH)) {
        pkgDTGH <- getDepsGH(pkgDTCached[["FALSE"]][isGH], verbose, which = which, whichCatRecursive,
                             libPaths = libPaths)
      }
      pkgDTCached[["FALSE"]] <- rbindlistNULL(list(pkgDTNonGH, pkgDTGH), use.names = TRUE, fill = TRUE)
    }
    pkgDT <- rbindlist(pkgDTCached, fill = TRUE, use.names = TRUE)
    pkgDT <- pkgDT[order(pkgDT$ord)]
  }

  return(pkgDT)
}





pkgDepCRAN <- function(pkgDT, which, repos, type, libPaths, verbose) {
  fillDefaults(pkgDep)

  num <- NROW(unique(pkgDT$Package)) # can have src and bin listed
  messageVerbose("  ", num, " ", singularPlural(c("package", "packages"), v = num),
                 " on CRAN", verbose = verbose, verboseLevel = 3)

  if (!is.data.table(pkgDT))
    pkgDT <- toPkgDT(pkgDT) |> parsePackageFullname()

  pkgDT <- #try(
    joinToAvailablePackages(pkgDT, repos, type, which, verbose)
    # )
  # if (is(pkgDT2, "try-error")) {
  #   o <- options()
  #   sc <- sys.calls()
  #   out <- mget(ls())
  #   save(out, file = "/home/emcintir/tmp/out.rda")
  #   stop()
  # }
  # pkgDT <- pkgDT2

  needsVersionCheck <- !is.na(pkgDT$versionSpec) # | !is.na(pkgDT$VersionOnRepos)
  set(pkgDT, NULL, "availableVersionOK", NA) # default

  if (any(needsVersionCheck)) {
    pkgDT <- availableVersionOK(pkgDT)

    # NOT SURE ABOUT THIS -- SHOULDN"T BE NECESSARY TO PICK THE availableVersionOKthisOne
    # pkgDTVerNums <- split(pkgDT, f = needsVersionCheck)
    # # setnames(pkgDTVerNums$`TRUE`, old = "Version", "VersionOnRepos")
    # pkgDTVerNums$`TRUE` <- availableVersionOK(pkgDTVerNums$`TRUE`)
    # # setnames(pkgDTVerNums$`TRUE`, old = "VersionOnRepos", "Version")
    # set(pkgDTVerNums$`TRUE`, NULL, "keep", seq(NROW(pkgDTVerNums$`TRUE`)))
    # dups <- duplicated(pkgDTVerNums$`TRUE`$Package)
    # pkgDTVerNums$`TRUE`[which(dups), keep := if (any(availableVersionOK)) .I[availableVersionOKthisOne][1] else .I, by = "Package"]
    # pkgDTVerNums$`TRUE` <- pkgDTVerNums$`TRUE`[na.omit(unique(pkgDTVerNums$`TRUE`$keep))]
    # set(pkgDTVerNums$`TRUE`, NULL, "keep", NULL) # remove "keep" column; no longer needed
    # pkgDT <- rbindlist(pkgDTVerNums, fill = TRUE)

    #, availableVersionOK := compareVersion2(Version, versionSpec, inequality)]
  } else {
    set(pkgDT, NULL, "availableVersionOK", TRUE)
  }

  inCurrentCRAN <- inCurrentCRAN(pkgDT, verbose)

  if (any(!inCurrentCRAN)) {
    set(pkgDT, which(!inCurrentCRAN), "repoLocation", "Archive")
    pkgDTList <- split(pkgDT, by = "repoLocation")
    dups <- duplicated(pkgDTList$Archive$Package) # b/c will hvae src and bin --> not needed any more
    pkgDTList$Archive <- pkgDTList$Archive[which(!dups)]
    num <- NROW(pkgDTList$Archive$Package)
    messageVerbose(paste(pkgDTList$Archive$packageFullName, collapse = comma), " ",
                   "not on CRAN; checking CRAN archives ... ", verbose = verbose)
    pkgDTList <- getArchiveDESCRIPTION(pkgDTList, repos, which, libPaths = libPaths, verbose, purge = FALSE)
    wcr <- whichCatRecursive(which, recursive = FALSE)
    hadArchive <- !is.na(pkgDTList$Archive$VersionOnRepos)
    whHadArchive <- which(hadArchive)
    didntFindOnArchives <- is.na(pkgDTList[["Archive"]]$DESCFileFull)
    if (any(didntFindOnArchives)) {
      messageVerbose(red("   Did not find archives of: ",
                     paste(pkgDTList[["Archive"]]$packageFullName[didntFindOnArchives], collapse = comma),
                     "\n   --> Maybe version misspecified or were they installed locally?"))

    }

    messageVerbose("    Done evaluating archives!", verbose = verbose)
    pkgDT <- rbindlistNULL(pkgDTList, fill = TRUE, use.names = TRUE)
  }

  pkgDT[]
}


#' @inheritParams Require
pkgDepGitHub <- function(pkgDT, which, includeBase = FALSE, libPaths, verbose = getOption("Require.verbose")) {

  messageVerbose("  ", NROW(pkgDT), " packages on GitHub", verbose = verbose)

  pkg <- masterMainToHead(pkgDT$packageFullName)

  localVersionOK <- pkgDT$installedVersionOK
  if (is.null(localVersionOK)) {
    pkgDT <- installedVersionOKPrecise(pkgDT, libPaths = libPaths)
    localVersionOK <- pkgDT$installedVersionOK
  }

  if (any(!localVersionOK %in% TRUE)) {
    pkgDTNotLocal <- dlGitHubDESCRIPTION(pkgDT[!localVersionOK %in% TRUE], purge = FALSE)
  }
  if (any(localVersionOK %in% TRUE)) {
    # pkgDT[localVersionOK %in% TRUE, DESCFile := base::system.file("DESCRIPTION", package = Package), by = "Package"]
    pkgDT[localVersionOK %in% TRUE, DESCFile := localFiles, by = "Package"]

    if (exists("pkgDTNotLocal", inherits = FALSE)) {
      pkgDT <- rbindlist(list(pkgDT[localVersionOK %in% TRUE], pkgDTNotLocal), fill = TRUE, use.names = TRUE)
      setorderv(pkgDT, "ord")
    }

  } else {
    pkgDT <- pkgDTNotLocal
  }

  hasVersionNum <- grep(grepExtractPkgs, pkgDT$packageFullName)# pkgDT$hasVers # grep(grepExtractPkgs, pkgDT$packageFullName)
  isHEAD <- grep("(HEAD)", pkgDT$packageFullName)
  if (length(isHEAD)) {
    set(pkgDT, isHEAD, "availableVersionOK", TRUE)
    hasVersionNum <- setdiff(hasVersionNum, isHEAD)
  }
  set(pkgDT, NULL, "availableVersionOK", NA)
  if (length(hasVersionNum)) {
    if (!isTRUE(getOption("Require.offlineMode"))) {
      pkgDT[hasVersionNum, VersionOnRepos := DESCRIPTIONFileVersionV(DESCFile, purge = FALSE)]
      if (is.null(pkgDT$versionSpec)) {
        pkgDT[hasVersionNum, versionSpec := extractVersionNumber(packageFullName)]
        pkgDT[hasVersionNum, inequality := extractInequality(packageFullName)]
      }
      isHEAD <- pkgDT$versionSpec %in% "HEAD"
      if (any(isHEAD)) {
        pkgDT[which(isHEAD), availableVersionOK := TRUE]
        hasVersionNum <- hasVersionNum[which(!isHEAD)]
      }
      pkgDT[hasVersionNum, availableVersionOK := compareVersion2(VersionOnRepos, versionSpec, inequality)]
    }
  }

  pkgDepDTOuter <- updateWithRemotesNamespaceAddRepos2(pkgDT, which, purge = FALSE, includeBase, verbose = verbose)

  pkgDepDTOuter
}


getDepsNonGH <- function(pkgDT, repos, verbose, type, which,
                           whichCatRecursive, libPaths, doSave = TRUE) {
  pkgDT <- toPkgDTFull(pkgDT)

  pkgDT <- pkgDepCRAN(pkgDT, which = which, repos = repos, type = type, libPaths = libPaths, verbose = verbose)

  if (endsWith(whichCatRecursive, "FALSE")) { # this is joining the ap, so can only be recursive = FALSE
    if (isTRUE(doSave)) {
      # could be archived from CRAN
      onCRAN <- if (!is.null(pkgDT$VersionOnRepos)) !is.na(pkgDT$VersionOnRepos) else rep(FALSE, NROW(pkgDT))
      whOnCRAN <- which(onCRAN)
      if (any(onCRAN)) {
        pkgDT <- assignPkgDTtoSaveNames(pkgDT, rowsToUpdate = whOnCRAN, which, verbose, whichCatRecursive)
      }
    }
  }

  pkgDT[]
}

getDepsGH <- function(pkgDT, verbose, which, whichCatRecursive, libPaths, doSave = TRUE) {
  if (!is.data.table(pkgDT)) {
    pkgDT <- toPkgDTFull(pkgDT)
  }

  pkgDT <- installedVersionOKPrecise(pkgDT, libPaths = libPaths)
  pkgDT <- parsePackageFullname(pkgDT, sorted = FALSE) # this sorted previously; now no
  # pkgDT <- whichToInstall(pkgDT, install = TRUE)

  # if (isTRUE(any(!pkgDT$installedVersionOK) && any(duplicated(pkgDT[["Package"]])))) {
  #   # This is helpful if there are many branches of the same package, this can shorten the time,
  #   #   but also, if a package branch is defunct, another branch's min version requirements
  #   #   may make that defunct branch obsolete, so don't check it
  #   pkgDT <- trimRedundancies(pkgDT) #
  # }
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
        installedNoOKAndNoPkgEnv <- installedNoOKAndNoPkgEnv & !haveLocalSHA
        installedNoOKAndNoPkgEnvWh <- which(installedNoOKAndNoPkgEnv)
        if (length(installedNoOKAndNoPkgEnvWh)) {
          shaOuts <- #try(
            Map(
              repo = pkgDT$Repo[installedNoOKAndNoPkgEnvWh],
              acct = pkgDT$Account[installedNoOKAndNoPkgEnvWh],
              br = pkgDT$Branch[installedNoOKAndNoPkgEnvWh],
              verbose = verbose,
              getSHAfromGitHubMemoise
            )
          # )
          if (is(shaOuts, "try-error"))
            if (any(grepl(messageCantFind("|", "|", "|"), shaOuts)))
              stop(shaOuts)
          pkgDT[installedNoOKAndNoPkgEnvWh, shas := unlist(shaOuts)]
        }
        if (sum(installedNoOKAndNoPkgEnv %in% FALSE)) {
          pkgDT[which(installedNoOKAndNoPkgEnv %in% FALSE), shas := installedSha]
        }
        needVersion <- needVersions(pkgDT)
        # if (any(needVersion)) {
          descs <- dlGitHubDESCRIPTION(pkgDT)#$packageFullName)
          pkgDT[descs, descFiles := DESCFile,  on = "packageFullName"]
          wh <- which(!is.na(pkgDT$descFiles))
          pkgDT[wh, Version := DESCRIPTIONFileVersionV(descFiles)]
          possPkgUpdate <- DESCRIPTIONFileOtherV(pkgDT$descFiles[wh], other = "Package")
          correctPkgName <- possPkgUpdate == pkgDT$Package[wh]
          if (any(correctPkgName %in% FALSE)) {
            set(pkgDT, wh, "Package", possPkgUpdate)
            set(pkgDT, wh, "githubPkgName", possPkgUpdate)
          }
        # }
        break

      }
    }
  }

  dups <- duplicated(pkgDT$packageFullName)
  if (any(dups))
    pkgDT <- pkgDT[!dups]

  # next changes order
  out <- pkgDepGitHub(pkgDT = pkgDT, which = which,
                      includeBase = FALSE, libPaths = libPaths, verbose = verbose)
  rec <- FALSE # this function is only one time through
  set(pkgDT, NULL, deps(FALSE), unname(out))

  # rec <- recursiveType(whichCatRecursive)
  snHere <- sn(rec)
  set(pkgDT, NULL, snHere, saveNameConcat(pkgDT, whichCatRecursive))

  if (isTRUE(doSave)) {
    pkgDT <- assignPkgDTdepsToSaveNames(pkgDT = pkgDT, recursive = rec)
  }

  pkgDT[]
}

saveNameConcat <- function(pkgDT, whichCatRecursive = NULL, versionCol = c("Version", "versionSpec"), shasCol = "shas") {
  p <- pkgDT

  if (is.null(p$repoLocation)) {
    isGH <- isGitHub(p$packageFullName)
  } else {
    isGH <- p$repoLocation %in% .txtGitHub
  }
  if (any(isGH)) {
    p <- toPkgDTFull(p)
  }
  if (length(versionCol) > 1)
    versionCol <- intersect(versionCol, colnames(pkgDT))[1]
  if (is.null(pkgDT[[shasCol]]))
    shasCol <- "Branch"

  out <- with(p, ifelse(isGH,
                        saveNameConcat2(argsCh = saveNameOrderGH(versionCol, shasCol)),
                        saveNameConcat2(argsCh = saveNameOrderNonGH(versionCol))))
                        # saveNameConcatGH(Package, get(versionCol), Account, Repo, Branch, get(shasCol)),
                        # saveNameConcatNonGH(Package, get(versionCol))))

  isCRAN <- p$repoLocation %in% "CRAN"
  val <- p$repoLocation
  if (any(isCRAN))
    val[isCRAN] <- gsub(":|/", "_", p$Repository[isCRAN])
  paste(out, val, whichCatRecursive, sep = sepForSaveNames)
}

saveNameConcat2 <- function(argsCh, envir = parent.frame()) {
  mget(argsCh, envir = envir) |>
    append(list(paste, sep = sepForSaveNames)) |>
    do.call(what = Map) |>
    unlist() |>
    unname()
  # unname(unlist(do.call(Map, append(mget(argsCh, envir = envir), list(paste, sep = sepForSaveNames)))))
}


saveNameOrderNonGH <- function(Version = "Version") {
  c("Package", Version)
}
saveNameOrderGH <- function(Version, shas = "shas") {
  c(saveNameOrderNonGH(Version), "Account", "Repo", "Branch", shas)
}

sepForSaveNames <- "__"
sepForWhich <- "_"

# saveNamesLabel <- function(pkgDT) {
#   if (!is.null(pkgDT$Account)) {
#     isGH <- !is.na(pkgDT$Account)
#   } else {
#     isGH <- rep(FALSE, NROW(pkgDT))
#   }
#   set(pkgDT, NULL, "label", pkgDT$Package)
#   set(pkgDT, which(isGH), "label",
#       paste0(pkgDT$Account[isGH], "/",
#              pkgDT$Repo[isGH], "@",
#              pkgDT$Branch[isGH]))
#
#   paste0(pkgDT$label, " (==", pkgDT$Version, ")")
# }


needVersions <- function(pkgDT) {
  if ("Version" %in% colnames(pkgDT)) {
    needVersions <- is.na(pkgDT$Version)
  } else {
    needVersions <- rep(TRUE, NROW(pkgDT))
  }
  needVersions
}


updateWithRemotesNamespaceAddRepos2 <- function(pkgDT, which, purge, includeBase, verbose) {
  if (!"shas" %in% colnames(pkgDT))
    set(pkgDT, NULL, "shas", pkgDT$Branch)

  out <- pkgDT[, list(# packageFullName = packageFullName,
                      lis = {
    allDeps <- DESCRIPTIONFileDeps(DESCFile, which = c("Depends", "Imports", "Suggests", "LinkingTo"),
                                  purge = purge, keepSeparate = TRUE)
    needed <- allDeps[which]
    notNeeded <- allDeps[setdiff(names(allDeps), which)]
    neededAdditionalRepos <- DESCRIPTIONFileOtherV(DESCFile, other = "Additional_repositories")
    neededRemotes <- DESCRIPTIONFileDeps(DESCFile, which = "Remotes", purge = purge)
    pfn <- gsub("(@).+( *)", paste0("\\1", shas, "\\2"), packageFullName)
    # Change branch to use sha
    uwrnar(needed = needed, notNeeded = notNeeded, neededRemotes, installedVersionOK, Package,
           pfn, neededAdditionalRepos, shas = shas, includeBase, localFiles = localFiles, verbose)
  }), by = "packageFullName"]

  out1 <- out$lis
  names(out1) <- out$packageFullName
  return(out1)
  # neededV <-
  #   try(DESCRIPTIONFileDepsV(pkgDT[["DESCFile"]], which = which, purge = purge))
  # if (is(neededV, "try-error")) {
  #   unlink(pkgDT[["DESCFile"]])
  #   unlink(pkgDT$destFile)
  #   set(pkgDT, NULL, c("DESCFile", "destFile"), NULL)
  #   browserDeveloper(paste0("A problem occurred installing ", pkgDT$packageFullName, ". Does it exist?",
  #                           "\nTo confirm whether it exists, try browsing to ",
  #                           file.path("https://github.com", pkgDT$Account, pkgDT$Package, "tree", pkgDT$Branch),
  #                           "\nIf it does exist, try rerunning with `purge = TRUE`",
  #                           "\nIf this error is inaccurate, and the problem persists, ",
  #                           "please contact developers with error code 949"))
  # }
  #
  # neededAdditionalReposV <- DESCRIPTIONFileOtherV(pkgDT[["DESCFile"]], other = "Additional_repositories")
  #
  # neededRemotesV <-
  #   DESCRIPTIONFileDepsV(pkgDT[["DESCFile"]], which = "Remotes", purge = purge)
  # names(neededV) <- pkgDT$packageFullName
  #
  # Map(
  #   needed = neededV, neededRemotes = neededRemotesV,
  #   localVersionOK = pkgDT$installedVersionOK,
  #   neededAdditionalRepos = neededAdditionalReposV,
  #   localPackageName = pkgDT$Package,
  #   packageFullName = pkgDT$packageFullName,
  #   sha = pkgDT$shas,
  #   MoreArgs = list(includeBase = includeBase, verbose = verbose,
  #                   pkgDT = pkgDT),
  #   uwrnar
  # )
}


uwrnar <- function(needed, notNeeded, neededRemotes, installedVersionOK, Package,
                   # Repo, Account, Branch, hasSubFolder,
                   packageFullName, neededAdditionalRepos, shas,
                   includeBase, localFiles, verbose) {
  neededOrig <- needed
  if (!is.null(unlist(neededOrig))) {
    needed <- unname(unlist(needed))
    neededRemotesName <- extractPkgName(neededRemotes)
    neededName <- extractPkgName(needed)
    notNeededName <- c(extractPkgName(unname(unlist(notNeeded))))

    needSomeRemotesNames <- setdiff(neededRemotesName, notNeededName)
    needSomeRemotes <- neededName %in% needSomeRemotesNames
    notNeededRemotes <- setdiff(neededRemotesName, neededName)
    if (length(notNeededName)) {
      somehowMissing <- setdiff(neededRemotesName, c(notNeededRemotes, needSomeRemotes))
      # needSomeRemotes <- neededName %in% neededRemotesName
      dontMatch <- neededRemotes[match(somehowMissing, neededRemotesName)]
      if (length(dontMatch)) { # These either are missing from Depends/Imports/Suggests but are in Remotes
        #  or else the package name doesn't match the GitHub repo name e.g., BioSIM = RNCan/BioSimClient_R
        #  Need to try to figure out which it is for the dontMatch
        Packages <- Map(packageFullName = trimVersionNumber(dontMatch), function(packageFullName) {
          descFiles <- dlGitHubDESCRIPTION(packageFullName)
          DESCRIPTIONFileOtherV(descFiles[["DESCFile"]], "Package")
        })
        whPackages <- match(extractPkgName(needed), unname(unlist(Packages))) |> na.omit()
        Packages <- Packages[whPackages]

        RepoNotPkgName <- !mapply(pack = Packages, nam = names(Packages), function(pack, nam) {
          extractPkgGitHub(nam) == pack
        })

        if (any(RepoNotPkgName)) {
          addToNeededName <- neededName %in% unname(Packages[RepoNotPkgName])
          needSomeRemotes[addToNeededName] <- TRUE
          neededName <- unique(c(neededName, extractPkgName(names(Packages)[RepoNotPkgName])))
        }


      }
    }
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
      # namespaceFile <- base::system.file("NAMESPACE", package = Package)
      namespaceFile <- file.path(dirname(localFiles), "NAMESPACE")
    } else {
      namespaceFile <-
        dlGitHubNamespace(packageFullName)[["NAMESPACE"]]
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



    }
    bp <- if (isTRUE(includeBase)) {
      NULL
    } else {
      .basePkgs
    }
    # needed could have version number specification
    neededReally <- union(needed, depsFromNamespace)
    neededReally <- neededReally[!extractPkgName(neededReally) %in% bp]
    neededReally <- rmRifInPackageCol(neededReally)

    pkgDepDT <- toPkgDepDT(neededReally, needed,
                           Package, verbose)

    if (exists("Packages", inherits = FALSE)) {
      whOverride <- match(names(Packages)[RepoNotPkgName], pkgDepDT$packageFullName)
      set(pkgDepDT, whOverride, "Package", unname(unlist(Packages[RepoNotPkgName])))
    }
    if (!is.na(neededAdditionalRepos))
      pkgDepDT[, Additional_repositories := neededAdditionalRepos]

    neededWithWhich <- unlist(neededOrig, use.names = TRUE)
    neededWithWhich <- list(Package = extractPkgName(unname(neededWithWhich)),
                            # packageFullName = neededWithWhich,
                            which = names(neededWithWhich)) |>
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




#' @importFrom utils download.file untar
#' @include messages.R
getArchiveDESCRIPTION <- function(pkgDTList, repos, purge = FALSE, which, libPaths, verbose = getOption("Require.cloneFrom")) {

  tmpdir <- tempdir3() # faster than tempdir2
  on.exit({
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)
  pkgDTList$Archive <- identifyLocalFiles(pkgDTList$Archive, repos = repos, purge = purge,
                                          libPaths = libPaths, verbose = verbose)
  haveLocal2 <- pkgDTList$Archive[["haveLocal"]] %in% .txtLocal
  if (any(haveLocal2))
    pkgDTList$Archive[, PackageUrl := file.path(Package, basename(localFile))]
  if (!all(haveLocal2)) {
    # downloadArchive takes a list with an element called "Archive" so can't split on "Archive" here
    # noLocals <- pkgDTList$Archive$haveLocal2 %in% TRUE
    txtArchiveHaveLocal <- "ArchiveHaveLocal"
    if (any(haveLocal2)) {
      pkgDTList[[txtArchiveHaveLocal]] <- pkgDTList$Archive[which(haveLocal2), ]
    }
    pkgDTList$Archive <- pkgDTList$Archive[which(!haveLocal2), ]
    if (!is.null(pkgDTList$Archive)) {
      pkgDTList <- downloadArchive(pkgDTList, repos = repos, purge = purge,
                                   tmpdir = tmpdir, verbose = verbose)
    }
    if (!is.null(pkgDTList[[txtArchiveHaveLocal]])) {
      pkgDTList$Archive <-
        rbindlist(pkgDTList[c("Archive", txtArchiveHaveLocal)],
                  fill = TRUE, use.names = TRUE)
      pkgDTList[[txtArchiveHaveLocal]] <- NULL
    }
  }

  # tmpdir <- tempdir3()
  # pkgDTList <- downloadArchive(pkgDTList, repos = repos, purge = purge,
  #                              tmpdir = tmpdir, verbose = verbose)

  if (any(!is.na(pkgDTList$Archive$PackageUrl))) {
    pkgDTList$Archive[!is.na(PackageUrl), DESCFileFull := {
      .DESCFileFull(PackageUrl, verbose, Repository, Package, tmpdir)
      }, by = "packageFullName"]

    gotDESC <- !is.na(pkgDTList$Archive$DESCFileFull)
    whGotDESC <- which(gotDESC)

    VoR <- DESCRIPTIONFileOtherV(pkgDTList$Archive[whGotDESC]$DESCFileFull, "Version")
    pkgDTList$Archive[whGotDESC, VersionOnRepos := VoR]
    deps <- DESCRIPTIONFileDepsV(
      pkgDTList$Archive[whGotDESC]$DESCFileFull,
      which = which, keepSeparate = TRUE)
    names(deps) <- pkgDTList$Archive$packageFullName[whGotDESC]
    deps <- lapply(deps, function(x) {
      unlist(lapply(x, function(y) paste(y, collapse = comma)))
    })
    deps <- do.call(rbind, deps)
    # deps <- invertList(deps)
    # deps <- as.data.table(deps)
    for (co in colnames(deps))
      set(pkgDTList$Archive, whGotDESC, co, deps[, co])
  }
  pkgDTList
}


installed.packagesDeps <- function(ip, libPaths, which) {
  if (missing(ip))
    ip <- .installed.pkgs(lib.loc = libPaths, which = which)

  if (!is.data.table(ip))
    ip <- as.data.table(ip)

  ip[, deps := do.call(paste, append(.SD, list(sep = comma))), .SDcols=which]

  # remove trailing and initial commas
  set(ip, NULL, "deps", gsub("(, )+$", "", ip$deps))
  set(ip, NULL, "deps", gsub("^(, )+", "", ip$deps))
  # remove middle empty commas
  set(ip, NULL, "deps", gsub(", ,", ",", ip$deps))
  # remove empty, but no space
  set(ip, NULL, "deps", gsub("(,,)+", ",", ip$deps))
}

depsWithCommasToVector <- function(packageFullName, depsWithCommas) {
  # packageFullName is used to name the list
  ll <- strsplit(depsWithCommas, split = "(, {0,1})|(,\n)")
  names(ll) <- packageFullName
  ll <- lapply(ll, grep, pattern = .grepR, value = TRUE, invert = TRUE)
  ll <- lapply(ll, function(x) x[!is.na(x)])
  ll
}


cleanPkgs <- function(packageFullName, colsToClean = c("packageFullName", "parentPackage")) {
  if (is(packageFullName, "list")) {
    if (is.data.table(packageFullName[[1]])) {
      deps <- Map(pfn = packageFullName, function(pfn) {
        for (pk in colsToClean)
          set(pfn, NULL, pk, cleanPkgs(pfn[[pk]]))
        pfn
      })
      return(deps)
    } else {
      return(cleanPkgs(packageFullName))
    }
  }
  pkgsCleaned <- gsub(.grepTooManySpaces, " ", packageFullName)
  pkgsCleaned <- gsub(.grepTabCR, "", pkgsCleaned)
  pkgsCleaned <- gsub("(\\S)\\(", "\\1 (", pkgsCleaned) # missing space before (
  gsub("(>|=|<)([[:digit:]])", "\\1 \\2", pkgsCleaned)
}


assignPkgDTtoSaveNames <- function(pkgDT, rowsToUpdate = seq_len(NROW(pkgDT)), which, verbose, whichCatRecursive,
                                   versionCol = "VersionOnRepos") {
  if (NROW(pkgDT[rowsToUpdate])) {
    deps3 <- depsWithCommasToPkgDT(pkgDT[rowsToUpdate], which)
    depsAll <- Map(toPkgDepDT, deps3, verbose = verbose)

    actualSN <- saveNameConcat(pkgDT[rowsToUpdate], whichCatRecursive, versionCol = versionCol)

    recursive <- recursiveType(whichCatRecursive)
    set(pkgDT, rowsToUpdate, sn(recursive), actualSN)
    set(pkgDT, rowsToUpdate, deps(recursive), unname(depsAll))

    # "cachedFALSE is FALSE, but the depsFALSE is already there!"

    pkgDT <- assignPkgDTdepsToSaveNames(pkgDT, rowsToUpdate, recursive = recursive)
  }
  pkgDT
}

sn <- function(recursive)
  .suffix("sn", recursive)

deps <- function(recursive)
  .suffix("deps", recursive)

cached <- function(recursive)
  .suffix("cached", recursive)

recursiveType <- function(x)
  as.logical(strsplit(x, split = "Recursive")[[1]][2])

.suffix <- function(x, suff)
  paste0(x, suff)

whichCatRecursive <- function(which, recursive) {
  whichCat <- paste(sort(which), collapse = sepForWhich)

  if (is.logical(recursive))
    recursive <- as.character(recursive)
  if (!startsWith(recursive, "Recursive"))
    recursive <- paste0("Recursive", recursive)
  paste(whichCat, recursive, sep = "")[1]
}


depsListToPkgDTWithWhichCol <- function(deps2) {
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


#' Join a data.table with a `Package` column to `available.packages`
#'
#' Will join `available.packages()` with `pkgDT`, if `pkgDT` does not already have
#' a column named `Depends`, which would be an indicator that this had already
#' happened.
#' @inheritParams getDeps
#'
#' @return
#' The returned `data.table` will have most of the columns from
#' `available.packages` appended to the `pkgDT`, including `Depends`, `Imports`,
#' `Suggests`. It will change the column name that is normally
#'   returned from `available.packages` as `Version` to `VersionOnRepos`.
joinToAvailablePackages <- function(pkgDT, repos, type, which, verbose) {

  if (is.null(pkgDT[["Depends"]])) {
    if (any(!pkgDT$Package %in% .basePkgs)) {

      ap <- available.packagesCached(repos, purge = FALSE, verbose = verbose, type = type)
      # set(ap, NULL, "packageFullName", paste0(ap$Package, " (==", ap$Version, ")"))
      keep <- c("Package", "Version", "Repository", which)
      ap <- ap[ap$Package %in% pkgDT$Package][, ..keep]
      setnames(ap, old = "Version", new = "VersionOnRepos")
      cn <- setdiff(colnames(ap), c("Package", "packageFullName", which))
      onlyKeep <- c(setdiff(colnames(pkgDT), cn))

      # DON"T JOIN ON packageFullName because we want to know "what is on CRAN", not whether it is exact
      dups <- duplicated(ap$Package)
      pkgDT <- ap[!dups][pkgDT[, ..onlyKeep], on = c("Package")]

      # pkgDT <- ap[pkgDT[, ..onlyKeep], on = c("Package")] # ap[pkgDT, on = "Package"]
      # # typeIsDiffVers <- pkgDT[, .N, by = "Package"]
      # dups <- duplicated(pkgDT, by = c("Package")) # prev included "VersionOnRepos"; but this was always a diff version by "type"
      # if (any(dups))
      #   pkgDT <- pkgDT[!dups]
    }
  }
  pkgDT
}


rmRifInPackageCol <- function(pkgDT) {
  if (is(pkgDT, "list")) {
    out <- lapply(pkgDT, rmRifInPackageCol)
    return(out)
  }
  if (is.data.table(pkgDT)) {
    hasRasDep <- pkgDT[["Package"]] == "R"
    if (isTRUE(any(hasRasDep)))
      pkgDT <- pkgDT[-which(hasRasDep)]
    out <- pkgDT[]
  } else {
    out <- grep(.grepR, pkgDT, value = TRUE, invert = TRUE)
  }
  out
}



inCurrentCRAN <- function(pkgDT, verbose) {
  inCurrentCRAN <- !is.na(pkgDT$VersionOnRepos[!pkgDT$Package %in% .basePkgs]) # this is "removed and gone"
  if (isTRUE(any(!pkgDT$availableVersionOK %in% TRUE)))
    inCurrentCRAN <- inCurrentCRAN & pkgDT$availableVersionOK

  if (sum(inCurrentCRAN)) {
    if (any(inCurrentCRAN)) {
      num <- NROW(unique(pkgDT$Package[inCurrentCRAN]))
      messageVerbose("  Done for ", num, " ", singularPlural(c("package", "packages"), v = num),
                     " currently on CRAN!", verbose = verbose, verboseLevel = 3)
    }
  }
  inCurrentCRAN
}

assignPkgDTdepsToSaveNames <- function(pkgDT, rowsToUpdate = seq(NROW(pkgDT)), recursive) {
  sn <- sn(recursive)
  deps <- deps(recursive)

  deps1 <- pkgDT[[deps]][rowsToUpdate]
  names(deps1) <- pkgDT[[sn]][rowsToUpdate]
  isDT <- unlist(lapply(deps1, is.data.table))
  if (!all(isDT)) {
    deps1[which(!isDT)] <- lapply(deps1[which(!isDT)], rbindlistRecursive)
  }
  list2env(deps1, envir = envPkgDepDeps())
  set(pkgDT, rowsToUpdate, cached(recursive), TRUE)

  pkgDT[]
}



addDepthAndParentPkg <- function(deps, nam, .depth) {
  if (!is.null(deps))
    if (!is.null(nam)) {
      if (is(deps, "list")) {
        return(Map(deps = deps, nam = names(deps), addDepthAndParentPkg, .depth = .depth))
      }

      set(deps, NULL, "parentPackage", nam)
      deps <- rmRifInPackageCol(deps)
      set(deps, NULL, ".depth", .depth)
    }
  deps
}


reorderTo <- function(deps, packages) {
  deps[na.omit(match(packages, names(deps)))]
}

depsWithCommasToPkgDT <- function(pkgDT, which) {
  packageFullName <- pkgDT$packageFullName
  deps1 <- list()
  for (cn in which)
    deps1[[cn]] <- depsWithCommasToVector(packageFullName, pkgDT[[cn]])
  if (sum(lengths(deps1))) {
    deps2 <- invertList(deps1)
    deps1 <- depsListToPkgDTWithWhichCol(deps2)
  }
  deps1

}


#' Recursive function to remove `.basePkgs`
#'
#' @param deps Either a list of dependencies, a data.table of dependencies with
#'   a column `Package` or a vector of dependencies.
#' @param includeBase Logical. If `FALSE`, the default, then base packages will
#'   be removed.
rmBase <- function(includeBase = formals(pkgDep)[["includeBase"]], deps) {
  if (includeBase %in% FALSE) {
    if (is(deps, "list")) {
      deps <- lapply(deps, rmBase, includeBase = includeBase)
    } else {
      if (NROW(deps)) {
        if (is(deps, "data.table")) {
          deps <- deps[!deps$Package %in% .basePkgs]

        } else {
          deps <- deps[!extractPkgName(deps) %in% .basePkgs]
        }
      }
    }
  }
  return(deps)
}

addSelf <- function(includeSelf, deps, self) {
  if (includeSelf %in% TRUE) {
    if (is(deps, "list")) {
      deps <- Map(dep1 = deps, self = self, function(dep1, self) {
        addSelf(includeSelf, dep1, self)
      })
    } else {
      deps <- rbindlist(list(toPkgDepDT(self), deps), fill = TRUE, use.names = TRUE)
    }
  }
  return(deps)
}


toPkgDTFull <- function(pkgDT) {
  if (!is.data.table(pkgDT)) {
    packagesNames <- names(pkgDT)
    pkgDT <- toPkgDT(pkgDT)
  }
  if (is.null(pkgDT$versionSpec))
    pkgDT <- parsePackageFullname(pkgDT, sorted = FALSE)
  if (is.null(pkgDT$Account))
    pkgDT <- parseGitHub(pkgDT)

  if (exists("packagesNames", inherits = FALSE))
    pkgDT <- updatePackagesWithNames(pkgDT, packagesNames)

  # set(pkgDT, NULL, "topLevelOrder", seq(NROW(pkgDT)))

  pkgDT
}

#' `split` for a data.table that keeps integrity of a column of lists of data.table objects
#'
#' `data.table::split` does 2 bad things:
#'  1. reorders if using `f`
#'  2. destroys the integrity of a column that is a list of data.tables, when using `by`
#' So, to keep order, need `by`, but to keep integrity, need `f`. This function
#' @inheritParams getDeps
#' @param splitOn Character vector passed to `data.table::split(..., f = splitOn)`
#' @return
#' A list of `data.table` objects of `length(unique(splitOn))`.
splitKeepOrderAndDTIntegrity <- function(pkgDT, splitOn) {
  # keeps order and integrity.
  # split reorders!! careful
  # The data.table "by" doesn't work -- the list of data.tables in the deps*** column doesn't work
  # pkgDTHaveDeps <- pkgDT[whHasDeps]
  # pkgDTHaveDeps <- split(pkgDTHaveDeps, by = "packageFullName", sorted = FALSE)
  # splitOn -- using the split.data.table with `f` treats as a factor, which can get reordered inadvertently

  # splitOn <- pkgDT$packageFullName[whHasDeps]
  splitOn <- factor(splitOn, levels = unique(splitOn)) # must keep order
  split(pkgDT, f = splitOn)
  # pkgDTHaveDeps <- split(pkgDT[whHasDeps], f = splitOn)
}

getFromCache <- function(pkgDT, which, recursive) {

  curCache <- names(envPkgDepDeps())
  # must be correct which & recursive too
  curCache <- grep(whichCatRecursive(which, ""), curCache, value = TRUE)
  maybeHaveCache <- Map(nam = pkgDT$packageFullName, sw = paste0(pkgDT$Package, sepForSaveNames),
                        function(nam, sw) which(startsWith(prefix = sw, curCache)))
  maybeHaveCacheCurCache <- unlist(maybeHaveCache)
  maybeHaveCacheDT <- list(Package = rep(pkgDT$Package, lengths(maybeHaveCache)),
                             packageFullName = rep(names(maybeHaveCache), lengths(maybeHaveCache)),
                           rowNum = maybeHaveCacheCurCache) |> setDT()

  if (any(maybeHaveCacheDT$packageFullName %in% pkgDT$packageFullName)) {
    lst <- sapply(maybeHaveCacheDT, is.list)
    keep <- setdiff(colnames(maybeHaveCacheDT), names(lst)[which(lst)])
    dups <- duplicated(maybeHaveCacheDT[, ..keep])
    if (any(dups)) {
      maybeHaveCacheDT <- maybeHaveCacheDT[which(!dups)]
    }

    keep <- intersect(c("Package", "packageFullName", deps(FALSE)), colnames(pkgDT))
    keepShort <- keep[1:2]
    dups <- duplicated(pkgDT[, ..keepShort])
    pkgDT2 <- if (any(dups)) { pkgDT[which(!dups)] } else { pkgDT }

    maybeHaveCacheDT <- pkgDT2[, ..keep][maybeHaveCacheDT, on = c("Package", "packageFullName")]

    if (recursive %in% TRUE) {
      pkgDT <- getDepsFromCache(pkgDT, maybeHaveCacheDT, recursive = TRUE, curCache)
    }
    # if (!allNotNull(pkgDT[[cached(TRUE)]]))
      pkgDT <- getDepsFromCache(pkgDT, maybeHaveCacheDT, recursive = FALSE, curCache)

  }
  pkgDT
}


getDepsFromCache <- function(pkgDT, maybeHaveCacheDT, recursive, curCache) {

  # if (!allNotNull(pkgDT[[cached(TRUE)]])) {

    # if (recursive %in% FALSE && !allNotNull(pkgDT[[cached(TRUE)]])) {
    if (is.null(pkgDT[[cached(recursive)]])) {
      alreadyCached <- rep(FALSE, NROW(pkgDT))
    } else {
      alreadyCached <- pkgDT[[cached(recursive)]] %in% TRUE
    }

    if (any(alreadyCached)) {
      toRm <- maybeHaveCacheDT$packageFullName %in% pkgDT$packageFullName[alreadyCached]
      maybeHaveCacheDT <- maybeHaveCacheDT[!toRm]
    }

    haveInCache <- endsWith(curCache[maybeHaveCacheDT$rowNum], as.character(recursive))

    if (isTRUE(any(haveInCache))) {
      isRecursiveTxt <- paste0("isRecursive", recursive)

      set(maybeHaveCacheDT, NULL, isRecursiveTxt, haveInCache)
      maybeHaveCacheDTRec <- maybeHaveCacheDT[which(haveInCache)]
      set(maybeHaveCacheDTRec, NULL, "curCache", curCache[maybeHaveCacheDTRec$rowNum])

      # quick for CRAN

      getFromCurCachePoss <- grepl(paste0(sepForSaveNames, "(CRAN)", sepForSaveNames, paste0(".+", recursive, "$")),
                                   maybeHaveCacheDTRec$curCache)
      # slow for GitHub
      if (any(getFromCurCachePoss %in% FALSE)) {
        maybeHaveCacheDTRec <- toPkgDTFull(maybeHaveCacheDTRec)
        if (any(!is.na(maybeHaveCacheDTRec$githubPkgName))) {
          colsForComparison <- intersect(c("Package", "Account", "Repo", "Branch"),
                                         saveNameOrderGH(Version = "Version"))
          colsForComparison <- c(colsForComparison, "repoLocation")
          wh <- maybeHaveCacheDTRec[
            , list(wh = all(greplV(unname(unlist(mget(colsForComparison))), curCache)))
            , by = seq(NROW(maybeHaveCacheDTRec))]$wh
          # wh <- with(maybeHaveCacheDTRec[, ..colsForComparison], {
          #   greplV(c(Package, Account, Repo, Branch, repoLocation),
          #                                        maybeHaveCacheDTRec$curCache)
          #   })
          getFromCurCachePoss <- getFromCurCachePoss | wh %in% TRUE
        }
      }

      versions <- sapply(strsplit(maybeHaveCacheDTRec$curCache, sepForSaveNames), function(x) x[2])
      set(maybeHaveCacheDTRec, NULL, "version", versions)
      haveCache <- getFromCurCachePoss
      CRANvOKbcNeedLatest <- is.null(maybeHaveCacheDTRec$inequality)
      if (CRANvOKbcNeedLatest %in% FALSE)
        CRANvOKbcNeedLatest <- is.na(maybeHaveCacheDTRec$inequality)
      if (isTRUE(any(!CRANvOKbcNeedLatest))) {
        #  This is anything that has an inequality or is NOT on CRAN
        isGitHead <- maybeHaveCacheDTRec$versionSpec %in% "HEAD" & maybeHaveCacheDTRec$repoLocation %in% "GitHub"
        if (any(isGitHead)) {
          maybeHaveCacheDTRec[which(isGitHead), correctVersionInCache := FALSE] # force re-check
          # hasVersionNum <- hasVersionNum[!isGitHead]
          CRANvOKbcNeedLatest[!isGitHead] <- CRANvOKbcNeedLatest[!isGitHead]
        }
        maybeCRANOKNeedVerCheck <- CRANvOKbcNeedLatest %in% FALSE

        # correctVersionInCache -- if versionSpec is satisfied
        if (sum(maybeCRANOKNeedVerCheck)) {

          whMaybeCRANOK <- which(maybeCRANOKNeedVerCheck)
          isHEAD <- maybeHaveCacheDTRec$versionSpec[whMaybeCRANOK] %in% "HEAD"
          if (isTRUE(any(isHEAD %in% TRUE))) {
            maybeHaveCacheDTRec[whMaybeCRANOK[isHEAD], correctVersionInCache := FALSE]
          }
          if (isTRUE(any(isHEAD %in% FALSE))) {
            maybeHaveCacheDTRec[whMaybeCRANOK[isHEAD %in% FALSE],
                                correctVersionInCache := compareVersion2(version, versionSpec, inequality)]
          }

        }

        # correctVersionInCache -- if CRANvOKbcNeedLatest
        if (sum(CRANvOKbcNeedLatest %in% TRUE))
          set(maybeHaveCacheDTRec, which(CRANvOKbcNeedLatest), "correctVersionInCache", TRUE)

        correctV <- maybeHaveCacheDTRec[["correctVersionInCache"]] %in% TRUE
        haveCache <- correctV | is.na(maybeHaveCacheDTRec[["correctVersionInCache"]])
        correct <- correctV | CRANvOKbcNeedLatest
        maybeHaveCacheDTRec <- maybeHaveCacheDTRec[which(correct)]
        getFromCurCachePoss <- correct[which(correct)]
      }

      if (any(haveCache)) {

        set(maybeHaveCacheDTRec, NULL, "toGet", getFromCurCachePoss)

        whHaveCacheDT <- which(maybeHaveCacheDTRec[["toGet"]])
        whHaveCachePkgDT <-
          match(maybeHaveCacheDTRec[["packageFullName"]][whHaveCacheDT], pkgDT[["packageFullName"]])

        if (recursive %in% FALSE) {
          noColYet <- pkgDT[whHaveCachePkgDT][[deps(recursive)]]
          if (is.null(noColYet)) {
            a <- mget(maybeHaveCacheDTRec$curCache[whHaveCacheDT], envir = envPkgDepDeps())
            addEmpty <- FALSE
            if (length(a) <= 2) addEmpty <- is.null(unlist(a))
            if (addEmpty)
              a <- lapply(seq(NROW(pkgDT)), function(x) emptyDT())
            set(pkgDT, whHaveCachePkgDT, deps(recursive), unname(a))
          } else {
            # a <- lapply(whHaveCachePkgDT, function(x) data.table())#list(Package = NULL)
          }
        } else {
          objs <- mget(maybeHaveCacheDTRec$curCache[whHaveCacheDT], envir = envPkgDepDeps()) |>
            unname()
          # if (!identical(objs, pkgDT[[deps(recursive)]][whHaveCachePkgDT])) {
          set(pkgDT, whHaveCachePkgDT, deps(recursive), objs)
          # }
        }

        set(pkgDT, whHaveCachePkgDT, cached(recursive), TRUE)
        set(pkgDT, whHaveCachePkgDT, sn(recursive), maybeHaveCacheDTRec$curCache[whHaveCacheDT])
      }
    }
  # }
  pkgDT
}

emptyDT <- function()
  list(Package = character(), packageFullName = character(), which = character()) |> setDT()

grepV <- Vectorize(grep, vectorize.args = "pattern", SIMPLIFY = TRUE)
greplVint <- Vectorize(grepl, vectorize.args = "pattern", SIMPLIFY = TRUE)
greplV <- function(pattern, x, ...) {
  ww <- greplVint(pattern, x, ...)
  if (!is.matrix(ww)) ww <- t(as.matrix(ww))
  ww
}

allNotNull <- function(x) {
  ret <- FALSE
  if (!is.null(x))
    ret <- all(x %in% TRUE)
  ret
}

isNotNULLAnd <- function(x, cond) {
  ret <- FALSE
  if (!is.null(x))
    ret <- eval(cond)
  ret
}



appendRecursiveToDeps <- function(pkgDT, caTr, depFa, caFa, snFa, depTr, snTr) {
  if (!is.null(pkgDT[["TRUE"]][[depTr]]))
    set(pkgDT[["TRUE"]], NULL, depTr, NULL)

  if (!is.null(pkgDT[["TRUE"]][[caTr]]))
    set(pkgDT[["TRUE"]], NULL, caTr, NULL)

  if (!is.null(pkgDT[["TRUE"]][[snTr]]))
    set(pkgDT[["TRUE"]], NULL, snTr, NULL)

  chColNames <- c(depFa, caFa, snFa) %in% colnames(pkgDT[["TRUE"]])
  if (any(chColNames)) {
    setnames(pkgDT[["TRUE"]],
             old = c(depFa, caFa, snFa)[chColNames],
             new = c(depTr, caTr, snTr)[chColNames])

    set(pkgDT[["TRUE"]],  NULL, snTr, gsub("FALSE$", "TRUE", pkgDT[["TRUE"]][[snTr]]))
  }

  needFlatten <- sapply(pkgDT[["TRUE"]][[depTr]],
                        function(x) !is.null(x[[depTr]]))
  if (sum(needFlatten)) {
    whNeedFlatten <- which(needFlatten)
    set(pkgDT[["TRUE"]], whNeedFlatten, depTr,
        lapply(whNeedFlatten,
               function(ind) {
                 rbindlistRecursive(
                   append(list(pkgDT[["TRUE"]][[depTr]][[ind]]),
                        pkgDT[["TRUE"]][[depTr]][[ind]][[depTr]])
                   )
                 })

    )

  }


  pkgDT <- rbindlist(pkgDT, fill = TRUE)
  if (!is.null(pkgDT[[depFa]]))
    set(pkgDT, NULL, depFa, NULL) # remove non-recursive column for clearning memory
  if (!is.null(pkgDT[[snFa]]))
    set(pkgDT, NULL, snFa, NULL) # remove non-recursive column for clearning memory
  if (!is.null(pkgDT[[caFa]]))
    set(pkgDT, NULL, caFa, NULL) # remove non-recursive column for clearning memory
  pkgDT[]
}


#' @include pkgDep.R
RequireDependencies <- function(libPaths = .libPaths()) {
  localRequireDir <- file.path(libPaths, "Require")
  de <- dir.exists(localRequireDir)
  RequireDeps <- character()
  if (any(de)) {
    localRequireDir <- localRequireDir[de][1]
    # The next line is `try` because during R CMD check, the folder is there, but no DESCRIPTION file yet
    RequireDeps <- try(DESCRIPTIONFileDeps(file.path(localRequireDir, "DESCRIPTION")), silent = TRUE)
  }
  if (is(RequireDeps, "try-error") || all(de %in% FALSE)) {
    # if the package is loaded to memory from a different .libPaths() that is no longer on the current .libPaths()
    #  then the next line will work to find it
    deps <- packageDescription("Require", lib.loc = NULL, fields = "Imports")
    if (nzchar(deps)) {
      RequireDeps <- depsWithCommasToVector("Require", depsWithCommas = deps)
    } else {
      RequireDeps <- pkgDep("Require", simplify = TRUE, verbose = 0)
    }
  }
  RequireDeps <- unique(c("Require", unlist(RequireDeps)))
  # RequireDeps <- c(RequireDeps, "sys")
  RequireDeps
}


.RequireDependencies <- character()
.RequireDependenciesNoBase <- character()

.DESCFileFull <- function(PackageUrl, verbose, Repository, Package, tmpdir) {
  tf <- file.path(cachePkgDir(), basename(PackageUrl))
  rmEmptyFiles(tf)
  out <- if (file.exists(tf)) { NULL } else {
    # This section should only happen if Require.installPackageSys < 1
    for (i in 1:2) { # can be flaky -- try 2x
      inn <- try(download.file(quiet = verbose <= 0 || verbose >= 5,
                               url = file.path(Repository, basename(PackageUrl)),
                               destfile = tf), silent = TRUE)
      if (!is(inn, "try-error"))
        break
    }
    inn
  }
  if (is(out, "try-error")) {
    messageVerbose(out, verbose = verbose)
    out <- NA
  } else {
    DESCFile <- file.path(Package, "DESCRIPTION")
    untar(tf, files = DESCFile, exdir = tmpdir)
    out <- file.path(tmpdir, DESCFile)
  }
  out
}
