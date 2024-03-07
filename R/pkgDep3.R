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
                   libPath = .libPaths(),
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

  doDeps <- if (!is.null(list(...)$dependencies)) list(...)$dependencies else NULL
  if (!is.null(doDeps))
    which <- whichToDILES(doDeps)

  purge <- dealWithCache(purge)
  checkAutomaticOfflineMode() # This will turn off offlineMode if it had been turned on automatically

  deps <- list(packages)
  if (!includeBase) {
    packages <- packages[!packages %in% .basePkgs]
  }
  if (length(packages)) {
    which <- depsImpsSugsLinksToWhich(depends, imports, suggests, linkingTo, which)

    # if (length(which) < length(packages))
    #   which <- rep(which, length.out = length(packages))
    # Only deal with first one of "which"...
    if (is.list(which)) which <- which[[1]]

    # whichCat <- paste(sort(which[[1]]), collapse = "_")

    deps <- getPkgDeps(packages, parentPackage = NULL, recursive = recursive, repos = repos, verbose = verbose,
                       type = type, which = which, includeBase = includeBase,
                       includeSelf = includeSelf)

    depsCol <- "deps"
    set(deps, NULL, depsCol, lapply(deps[[deps(recursive)]], rbindlistRecursive))

    # keepCols <- c("Package", "packageFullName", "Version", "versionSpec", "inequality",
    #              "githubPkgName", "repoLocation", ".depth", "which", "parentPackage")
    # whHasDeps <- which(sapply(deps[[depsCol]], NROW) > 0) + 1 # (self)
    set(deps, NULL, #whHasDeps,
        depsCol,
        Map(dep = deps[[depsCol]], self = deps[["packageFullName"]],
            # Map(dep = deps[[depsCol]][whHasDeps], self = deps[["packageFullName"]][whHasDeps],
            function(dep, self) {
              if (!is.null(dep)) {
                dotDepth <- ".depth"
                if (dotDepth %in% colnames(dep))
                  setorderv(dep, cols = dotDepth)
                set(dep, NULL, intersect(colnames(dep), c(deps(TRUE), deps(FALSE))), NULL)
                # keepCols <- c(keepCols, intersect(colnames(dep), .txtGitHubParsedCols))
                dep <- dep[!duplicated(dep[["Package"]])]
                set(dep, NULL, c("packageFullName", "parentPackage"),
                    list(cleanPkgs(dep[["packageFullName"]]), cleanPkgs(dep[["parentPackage"]])))
                dep <- rmBase(includeBase, dep)
                # dep <- dep[, ..keepCols] # includeSelf added some cols
                # setnames(dep, old = depsCol, new = "deps")
              }
              dep <- addSelf(includeSelf, dep, self)
              dep <- dep[order(dep$Package)]
              dep
            }))

    keepColsOuter <- c("Package", "packageFullName", depsCol)
    # deps <- deps[, ..keepColsOuter]
    # trimRedundancies is better for following the deps, but it will fail to keep original
    #   user request. Use only duplicated instead ... to keep user order

    if (keepVersionNumber %in% FALSE) {
      deps <- Map(pkgFN = deps$deps, function(pkgFN) pkgFN[["Package"]])
    } else if (simplify %in% TRUE) {
      # collapse or not to list of character vector
      pfn <- deps$packageFullName
      deps <- Map(pkgFN = deps$deps, function(pkgFN) pkgFN[["packageFullName"]])
      names(deps) <- pfn
    }
  } else {
    names(deps) <- unlist(deps)
  }

  return(deps)
}

getPkgDeps <- function(pkgDT, parentPackage, recursive, which, repos, type, includeBase,
                       includeSelf, verbose, .depth = 0, .counter) {

  if (is.list(which)) which <- which[[1]]

  messageVerbose(paste(rep("  ", .depth), collapse = ""), cleanPkgs(parentPackage),
                 verbose = .depth == 1 && verbose >= 1)
  deps <- NULL
  if (NROW(pkgDT) > 0) { # this will skip any pkgDT that has no deps, breaks out of recursion
    if (is(pkgDT, "list")) pkgDT <- pkgDT[[1]]
    # if (.depth == 4) browser()
    # if (i == 1) browser()
    pkgDT <- toPkgDTFull(pkgDT)
    # if (any(pkgDT$packageFullName %in% "Rcpp (>= 0.12.18)")) browser()
    pkgDT <- rmRifInPackageCol(pkgDT)

    if (NROW(pkgDT[Package %in% "BioSIM"])) browser()

    pkgDT <- saveNamesForCache(pkgDT, which, recursive = FALSE,
                               repos = repos, type = type, verbose = verbose)

    if (recursive %in% TRUE) {
      which <- setdiff(which, "Suggests")
      pkgDT <- getPkgDepsMap(pkgDT, recursive, parentPackage = parentPackage, repos, which, type,  libPaths,
                             includeBase = includeBase, includeSelf = includeSelf, verbose,
                             .depth = .depth)

      whHasRecursiveTRUE <- which(!is.na(pkgDT[[sn(FALSE)]]))
      if (length(whHasRecursiveTRUE)) {
        set(pkgDT, whHasRecursiveTRUE, sn(TRUE),
            gsub("FALSE$", "TRUE", pkgDT[[sn(FALSE)]][whHasRecursiveTRUE]))
        # moveDeps <- pkgDT[[deps(FALSE)]][whHasRecursiveTRUE]
        # if (is.null(unlist(moveDeps))) {
        #   lst <- pkgDT[[deps(FALSE)]]
        #   lst[whHasRecursiveTRUE] <- lapply(whHasRecursiveTRUE, function(x)
        #     list(Package = character(), packageFullName = character()) |> setDT())
        #   pkgDT[[deps(FALSE)]][whHasRecursiveTRUE] <- lapply(whHasRecursiveTRUE, function(x) list(Package = character(), packageFullName = character()) |> setDT())
        # }

        moveDeps <- pkgDT[[deps(FALSE)]][whHasRecursiveTRUE]
        if (!is.null(unlist(moveDeps))) {
          set(pkgDT, whHasRecursiveTRUE, deps(TRUE), pkgDT[[deps(FALSE)]][whHasRecursiveTRUE])
        } else {
          set(pkgDT, NULL, deps(TRUE), lapply(seq(NROW(pkgDT)), function(x) emptyDT()))
          #  list(Package = character(), packageFullName = character(), which = character()) |> setDT()))
        }

        isNULL <- sapply(pkgDT[[deps(FALSE)]][whHasRecursiveTRUE], is.null)
        if (any(!isNULL))
          assignPkgDTdepsToSaveNames(pkgDT, rowsToUpdate = whHasRecursiveTRUE[!isNULL], TRUE)
      }

    }
  }
  pkgDT <- addDepthAndParentPkg(pkgDT, nam = parentPackage, .depth)

  return(pkgDT)
#
#     recTF <- c("FALSE", "TRUE") #paste0("Recursive", c("TRUE", "FALSE"))
#     rec <- if (recursive %in% TRUE) c(recTF[1], recTF[2]) else recTF[2] # need to do both if TRUE
#     alreadySaved <- list()
#     for (recursiveUsed in rec) {
#       pkgDT <- saveNamesForCache(pkgDT, which, recursive = recursiveUsed,
#                                  repos = repos, type = type, verbose = verbose - 1)
#       sns <- pkgDT$sn[!is.na(pkgDT$sn)]
#       alreadySaved[[recursiveUsed]] <- sns %in% names(envPkgDepDeps())
#
#       names(alreadySaved[[recursiveUsed]]) <- sns
#
#       if (all(alreadySaved[[recursiveUsed]]))
#         break
#     }
#
#     pkgDT <- rmBase(pkgDT, includeBase = includeBase)
#
#     if (any(alreadySaved[[recursiveUsed]])) {
#       wh <- alreadySaved[[recursiveUsed]]
#       whSaved <- pkgDT$sn %in% names(wh)[wh]
#       pkgDTSaved <- pkgDT[whSaved]
#       # messageVerbose("  ", NROW(pkgDTSaved), " packages already saved in Cache", verbose = verbose)
#       deps <- Map(nam = pkgDTSaved$packageFullName, obj = pkgDTSaved$sn,
#                   f = function(nam, obj) get0(obj, envir = envPkgDepDeps()))
#       set(pkgDT, which(whSaved), .suffix("deps", recursiveUsed), deps)
#       deps <- addDepthAndParentPkg(pkgDT$deps, .depth)
#       pkgDT <- pkgDT[-which(whSaved)]
#     }
#
#     if (NROW(pkgDT) > 0) {
#       if (.depth == 0) {
#         outerPackages <- pkgDT$packageFullName # outerPackages <- packages # packages has "named" vector, i.e., BioSIM
#       }
#
#       .depsNew <- getPkgDepsNonRecursive(pkgDT, outerPackages, which, repos, type,
#                                         includeBase = includeBase, verbose = verbose)
#       .depsNew <- addDepthAndParentPkg(.depsNew, .depth)
#       set(pkgDT, NULL, "deps", .depsNew)
#
#       # Because we aren't trimRedundancies, multiple versions of e.g., Rcpp (>= 0.11.0) and Rcpp (>= 1.0-10) both get downloaded
#       # deps <- append(deps, depsNew) # add the ones recovered from saved
#     }
#
#     if (recursive %in% TRUE && !is.null(deps)) {
#       if (.depth == 0) {
#         outerPackages <- names(deps) # outerPackages <- packages # packages has "named" vector, i.e., BioSIM
#       }
#
#       deps <- getPkgDepsMap(deps, outerPackages = outerPackages, recursive,  repos, which, type,  libPaths,
#                             includeBase = includeBase, includeSelf = includeSelf, verbose,
#                             .depth = .depth)
#     }
#
#     if (.depth == 0) {
#       if (recursive %in% TRUE) {
#         if (any(!alreadySaved[[recTF[1]]])) {
#
#           if (exists("pkgDTSaved", inherits = FALSE)) {
#             pkgDT <- rbindlist(list(pkgDT, pkgDTSaved))
#           }
#
#           whDepsArePkgDT <- match(pkgDT$packageFullName, names(deps))
#           deps <- deps[whDepsArePkgDT]
#           alreadySavedNow <- pkgDT$sn %in% names(envPkgDepDeps())
#           if (any(!alreadySavedNow)) {# this is for the FALSEs ... it will be no new assignments unless a partial update
#             pkgDT3 <- assignPkgDTdepsToSaveNames(pkgDT[!alreadySavedNow], deps[whDepsArePkgDT][!alreadySavedNow])
#           }
#
#           # Now for saving recursive = TRUE
#           pkgDT2 <- saveNamesForCache(pkgDT, which, recursive = "TRUE",
#                                       repos = repos, type = type,
#                                       verbose = verbose - 1)
#           alreadySavedNow <- pkgDT2$sn %in% names(envPkgDepDeps())
#           if (any(!alreadySavedNow)) {
#             whDepsArePkgDT <- match(pkgDT2$packageFullName, names(deps))
#             pkgDT4 <- assignPkgDTdepsToSaveNames(pkgDT2[!alreadySavedNow], deps[whDepsArePkgDT][!alreadySavedNow])
#           }
#         }
#       }
#
#       # reorder to initial order; if there were redundancies, it would create NAs; thus na.omit
#       deps <- reorderTo(deps, packages)
#       deps <- cleanPkgs(deps)
#       deps <- rmBase(includeBase, deps)
#       deps <- addSelf(includeSelf, deps)
#     }
#
#
#   deps
}

getPkgDepsMap <- function(pkgDT, parentPackage, recursive, repos, which, type, libPaths,
                          includeBase, includeSelf, verbose, .depth = 0) {

  if (.depth == 0) {
    messageVerbose("  Determining recursive dependencies", verbose = verbose)
  }
  ndeps <- sapply(pkgDT[[deps(FALSE)]], NROW)
  noDeps <- ndeps == 0
  hasDeps <- !noDeps
  whHasDeps <- which(hasDeps)
  if (any(pkgDT$packageFullName %in% "Rcpp (>= 0.12.18)")) browser()

  if (any(hasDeps)) {
    depsToDo <- pkgDT[[deps(FALSE)]][hasDeps]
    col <- deps(FALSE)
    pkgDTHaveDeps <- pkgDT[whHasDeps]

    # can't use data.table "by" because I need to change .SD
    out <- Map(pkgDT = pkgDTHaveDeps$depsFALSE, nam = pkgDTHaveDeps$packageFullName,
        .counter = seq(NROW(pkgDTHaveDeps)), function(pkgDT, nam, .counter)
      getPkgDeps(pkgDT, parentPackage = nam,
                 .counter = .counter,
                 recursive = recursive, which = which, repos = repos,
                 type = type, includeBase = includeBase,
                 includeSelf = includeSelf,
                 .depth = .depth + 1, verbose = verbose))

    # out <- by(pkgDT, INDICES = pkgDTHaveDeps$packageFullName, FUN = function(pkgDTInner, nam, .counter)
    #   getPkgDeps(pkgDTInner[[deps(FALSE)]],
    #              parentPackage = pkgDT,
    #              .counter = .counter,
    #              recursive = recursive, which = which, repos = repos,
    #              type = type, includeBase = includeBase,
    #              includeSelf = includeSelf,
    #              .depth = .depth + 1, verbose = verbose))
    # )
    # pkgDTHaveDepsList <- splitKeepOrderAndDTIntegrity(pkgDTHaveDeps, pkgDTHaveDeps$packageFullName)
    # out <- Map(pkgDT = pkgDTHaveDepsList, nam = names(pkgDTHaveDepsList), .counter = seq(NROW(pkgDTHaveDepsList)),
    #            function(pkgDT, nam, .counter)
    #              getPkgDeps(pkgDT[[deps(FALSE)]],
    #                         parentPackage = nam,
    #                         .counter = .counter,
    #                         recursive = recursive, which = which, repos = repos,
    #                         type = type, includeBase = includeBase,
    #                         includeSelf = includeSelf,
    #                         .depth = .depth + 1, verbose = verbose))

    set(pkgDT, whHasDeps, deps(FALSE), lapply(seq(NROW(pkgDTHaveDeps)), function(ind)
      append(list(out[[ind]]), # pkgDT$depsFALSE[whHasDeps[ind]],
              list(unname(out)[[ind]]$depsFALSE))
    ))
  }
  pkgDT[]
}


# getPkgDepsNonRecursive <- function(pkgDT, outerPackages, which, repos, type, includeBase, verbose) {
#
#   if (verbose >= 1) {
#     num <- NROW(unique(pkgDT$Package)) # unique b/c can have src and bin listed here if they are different on CRAN
#     messageVerbose("Determining dependencies of ", num,
#                    singularPlural(c(" package", " packages"), v = num),
#                    verbose = verbose)
#   }
#
#   pkgDT <- rmRifInPackageCol(pkgDT)
#   pkgDTList <- split(pkgDT, by = "repoLocation")
#   pkgDTDep <- list()
#
#   if (!is.null(pkgDTList[["CRAN"]])) {
#     pkgDTDep[["CRAN"]] <- pkgDepCRAN(pkgDT = pkgDTList$CRAN, which = which[[1]],
#                                           repos = repos, type = type)
#   }
#   if (!is.null(pkgDTList[[.txtGitHub]])) {
#     pkgDTDep[[.txtGitHub]] <- pkgDepGitHub(pkgDT = pkgDTList$GitHub, which = which[[1]],
#                                          includeBase = includeBase, verbose = verbose)
#   }
#
#   depsNew <- unlist(unname(pkgDTDep), recursive = FALSE)
#   depsNew
# }

pkgDepCRAN <- function(pkgDT, which, repos, type, verbose) {
  fillDefaults(pkgDep)

  num <- NROW(unique(pkgDT$Package)) # can have src and bin listed
  messageVerbose("  ", num, " ", singularPlural(c("package", "packages"), v = num),
                 " on CRAN", verbose = verbose - 1)

  if (!is.data.table(pkgDT))
    pkgDT <- toPkgDT(pkgDT) |> parsePackageFullname()

  pkgDT <- joinToAvailablePackages(pkgDT, repos, type, which, verbose)

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
    messageVerbose("  ", num, " ", singularPlural(c("package has", "packages have"), v = num),
                   " been archived...", verbose = verbose)
    pkgDTList <- getArchiveDESCRIPTION(pkgDTList, repos, which, verbose, purge = FALSE)
    wcr <- whichCatRecursive(list(which), recursive = FALSE)
    hadArchive <- !is.na(pkgDTList$Archive$VersionOnRepos)
    whHadArchive <- which(hadArchive)
    # pdt <- split(pkgDTList$Archive, f = hadArchive)
    # pdt[["TRUE"]] <- assignPkgDTtoSaveNames(pdt[["TRUE"]], which = which,
    #                               verbose = verbose, whichCatRecursive = wcr)
    # pkgDTList$Archive <- rbindlistNULL(pdt, fill = TRUE)
    didntFindOnArchives <- is.na(pkgDTList[["Archive"]]$DESCFileFull)
    if (any(didntFindOnArchives)) {
      messageVerbose(red("   Did not find archives of: ",
                     paste(pkgDTList[["Archive"]]$packageFullName[didntFindOnArchives], collapse = ", "),
                     "\n   --> Maybe version misspecified or were they installed locally?"))

    }

    messageVerbose("    Done package archives!", verbose = verbose)
    pkgDT <- rbindlistNULL(pkgDTList, fill = TRUE, use.names = TRUE)
  }

  # for (co in which)
  #   set(pkgDT, which(is.na(pkgDT[[co]])), co, "")
  #
  # pkgDT <- installed.packagesDeps(pkgDT, which)
  # # deps1 <- list()
  # # for (cn in which)
  # #   deps1[[cn]] <- depsWithCommasToVector(pkgDT$packageFullName, pkgDT[[cn]])
  # # deps2 <- invertList(deps1)
  # # deps3 <- depsListToPkgDTWithWhichCol(deps2)
  # #
  #
  # deps3 <- depsWithCommasToPkgDT(pkgDT, which)
  # Map(toPkgDepDT, deps3, verbose = verbose)
  pkgDT[]
}


#' @inheritParams Require
pkgDepGitHub <- function(pkgDT, which, includeBase = FALSE, verbose = getOption("Require.verbose")) {

  messageVerbose("  ", NROW(pkgDT), " packages on GitHub", verbose = verbose - 2)

  pkg <- masterMainToHead(pkgDT$packageFullName)

  localVersionOK <- pkgDT$installedVersionOK
  if (is.null(localVersionOK)) {
    pkgDT <- installedVersionOKPrecise(pkgDT)
    localVersionOK <- pkgDT$installedVersionOK
  }

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

  hasVersionNum <- grep(grepExtractPkgs, pkgDT$packageFullName)# pkgDT$hasVers # grep(grepExtractPkgs, pkgDT$packageFullName)
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



#' The `packages` argument may have up to 4 pieces of information for GitHub
#' packages: name, repository, branch, version. For CRAN-alikes, it will only
#' be 2 pieces: name, version. There can also be an inequality or equality, if
#' there is a version.
#'
#' @param packages A vector of GitHub or CRAN-alike packages. These can include
#'   package name (required) and optionally repository, branch/sha and/or version.
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
saveNamesForCache <- function(pkgDT, which, recursive, type = type, repos, verbose) {
  fillDefaults(pkgDep)

  set(pkgDT, NULL, "cached", FALSE)
  set(pkgDT, which(pkgDT$Package %in% .basePkgs), "cached", TRUE)

  pkgDT <- getFromCache(pkgDT, which, recursive)

  if (any(!pkgDT[["cached"]] %in% TRUE) &&
      sum(!pkgDT[["Package"]][!pkgDT[["cached"]]] %in% .basePkgs)) {
    set(pkgDT, NULL, "ord", seq(NROW(pkgDT)))
    pkgDTCached <- splitKeepOrderAndDTIntegrity(pkgDT, pkgDT$cached)
    messageVerbose("Not in Cache: ", paste(pkgDTCached[["FALSE"]]$Package, collapse = ", "), verbose = verbose)
    whichCatRecursive <- whichCatRecursive(which, recursive)
    isGH <- !is.na(pkgDTCached[["FALSE"]]$githubPkgName)
    pkgDTNonGH <- pkgDTGH <- NULL
    if (!all(pkgDTCached[["FALSE"]]$Package %in% .basePkgs)) {
      if (any(!isGH)) {
        pkgDTNonGH <- getDepsNonGH(pkgDTCached[["FALSE"]][!isGH], repos, type = type, verbose,
                                     which = which, whichCatRecursive)
      }
      if (any(isGH)) {
        pkgDTGH <- getDepsGH(pkgDTCached[["FALSE"]][isGH], verbose, which = which, whichCatRecursive)
      }
      pkgDTCached[["FALSE"]] <- rbindlistNULL(list(pkgDTNonGH, pkgDTGH), use.names = TRUE, fill = TRUE)
    }
    pkgDT <- rbindlist(pkgDTCached, fill = TRUE, use.names = TRUE)
    pkgDT <- pkgDT[order(pkgDT$ord)]
  }
  return(pkgDT)
}


getDepsNonGH <- function(pkgDT, repos, verbose, type, which,
                           whichCatRecursive, doSave = TRUE) {
  pkgDT <- toPkgDTFull(pkgDT)

  pkgDT <- pkgDepCRAN(pkgDT, which = which, repos = repos, type = type, verbose = verbose)
  # needVersions <- FALSE
  # if ("VersionOnRepos" %in% colnames(pkgDT)) {
  #   haveVersions <- is.na(pkgDT$VersionOnRepos)
  #   if (any(haveVersions)) needVersions <- TRUE
  # } else {
  #   needVersions <- TRUE
  # }
  # if (needVersions) {
  #   if (endsWith(whichCatRecursive, "FALSE")) { # this is joining the ap, so can only be recursive = FALSE
  #     pkgDT <- joinToAvailablePackages(pkgDT, repos, type, which, verbose)
  #   }
  # }

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

getDepsGH <- function(pkgDT, verbose, which, whichCatRecursive, doSave = TRUE) {
  if (!is.data.table(pkgDT)) {
    pkgDT <- toPkgDTFull(pkgDT)
  }

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
        # if (!missing(ap)) {
        #   ghshacolname <- "GithubSHA1"
        #   if (ghshacolname %in% colnames(ap)) {
        #     isGHinAP <- !is.na(ap[[ghshacolname]])
        #     shaOuts <- pkgDT[installedNoOKAndNoPkgEnvWh][ap[isGHinAP], on = c("Branch" = ghshacolname)]$Branch
        #     haveLocalSHA <- shaOuts %in% brLocals
        #   }
        # }
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
        # if (any(needVersion)) {
          descs <- getGitHubDESCRIPTION(pkgDT$packageFullName)
          pkgDT[descs, descFiles := DESCFile,  on = "packageFullName"]
          pkgDT[!is.na(descFiles), Version := DESCRIPTIONFileVersionV(descFiles)]
        # }
        break

      }
    }
  }

  dups <- duplicated(pkgDT$packageFullName)
  if (any(dups))
    pkgDT <- pkgDT[!dups]
  out <- pkgDepGitHub(pkgDT = pkgDT, which = which,
                      includeBase = TRUE, verbose = verbose)
  set(pkgDT, NULL, deps(FALSE), unname(out))

  rec <- recursiveType(whichCatRecursive)
  snHere <- sn(rec)
  set(pkgDT, NULL, snHere, saveNameConcat(pkgDT, whichCatRecursive))

  # deps2 <- DESCRIPTIONFileDepsV(pkgDT$descFiles, keepSeparate = TRUE)
  # deps3 <- depsListToPkgDTWithWhichCol(deps2)
  # deps3 <- rmRifInPackageCol(deps3)
  # deps4 <- Map(toPkgDepDT, deps3)
  # # pkgDT[, depsFALSE := unname(deps4)]
  # set(pkgDT, NULL, deps(rec), unname(deps4))
  # # set(pkgDT, NULL, snHere, pkgDT[[snHere]])

  if (isTRUE(doSave)) {
    pkgDT <- assignPkgDTdepsToSaveNames(pkgDT = pkgDT, recursive = rec)
  }

  pkgDT[]
}

saveNameConcat <- function(pkgDT, whichCatRecursive, versionCol = "Version", shasCol = "shas") {
  p <- pkgDT

  isGH <- p$repoLocation %in% .txtGitHub
  out <- with(p, ifelse(isGH,
                        saveNameConcatGH(Package, get(versionCol), Account, Repo, get(shasCol)),
                        saveNameConcatNonGH(Package, get(versionCol))))

  paste(out, p$repoLocation, whichCatRecursive, sep = sepForSaveNames)
}

saveNameConcatGH <- function(Package, Version, Account, Repo, shas)
  paste(Package, Version, Account, Repo, shas, sep = sepForSaveNames)

saveNameConcatNonGH <- function(Package, Version)
  paste(Package, Version, sep = sepForSaveNames)

sepForSaveNames <- "__"

saveNamesLabel <- function(pkgDT) {
  browser() #  This is currently unused as of last time
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
  if (!"shas" %in% colnames(pkgDT))
    set(pkgDT, NULL, "shas", pkgDT$Branch)

  out <- pkgDT[, list(# packageFullName = packageFullName,
                      lis = {
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
    dontMatch <- neededRemotes[!neededRemotesName %in% neededName]
    if (length(dontMatch)) { # These either are missing from Depends/Imports/Suggests but are in Remotes
      #  or else the package name doesn't match the GitHub repo name e.g., BioSIM = RNCan/BioSimClient_R
      #  Need to try to figure out which it is for the dontMatch
      Packages <- Map(packageFullName = trimVersionNumber(dontMatch), function(packageFullName) {
        descFiles <- getGitHubDESCRIPTION(packageFullName)
        DESCRIPTIONFileOtherV(descFiles$DESCFile, "Package")
      })
      RepoNotPkgName <- !mapply(pack = Packages, nam = names(Packages), function(pack, nam) {
        extractPkgGitHub(nam) == pack
      })

      if (any(RepoNotPkgName)) {
        addToNeededName <- neededName %in% unname(Packages[RepoNotPkgName])
        needSomeRemotes[addToNeededName] <- TRUE
        neededName <- unique(c(neededName, extractPkgName(names(Packages)[RepoNotPkgName])))
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




getArchiveDESCRIPTION <- function(pkgDTList, repos, purge = FALSE, which, verbose = getOption("Require.cloneFrom")) {
  pkgDTList <- downloadArchive(pkgDTList, repos = repos, purge = purge, verbose = verbose)

  tmpdir <- Require::tempdir2(.rndstr())
  if (any(!is.na(pkgDTList$Archive$PackageUrl))) {
    pkgDTList$Archive[!is.na(PackageUrl), DESCFileFull := {
      tf <- file.path(Require:::RequirePkgCacheDir(), basename(PackageUrl))
      out <- if (file.exists(tf)) { NULL } else {
        try(download.file(
          url = file.path(Repository, basename(PackageUrl)),
          destfile = tf), silent = TRUE)
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

    pkgDT <- assignPkgDTdepsToSaveNames(pkgDT, rowsToUpdate, recursive = recursive)
  }
  pkgDT
}

sn <- function(recursive)
  .suffix("sn", recursive)

deps <- function(recursive)
  .suffix("deps", recursive)

recursiveType <- function(x)
  as.logical(strsplit(x, split = "Recursive")[[1]][2])

.suffix <- function(x, suff)
  paste0(x, suff)

whichCatRecursive <- function(which, recursive) {
  whichCat <- paste(sort(which), collapse = "_")
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
#' Will join `available.packages()` with `pkgDT`, if `pkgDT` doesn't already have
#' a column named `Depends`, which would be an indicator that this had already
#' happened.
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
    out <- grep(.grepR, pkgDT, value = TRUE, invert = T)
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
                     " currently on CRAN!", verbose = verbose - 2)
    }
  }
  inCurrentCRAN
}

assignPkgDTdepsToSaveNames <- function(pkgDT, rowsToUpdate = seq(NROW(pkgDT)), recursive) {
  sn <- sn(recursive)
  deps <- deps(recursive)

  deps1 <- pkgDT[[deps]][rowsToUpdate]
  names(deps1) <- pkgDT[[sn]][rowsToUpdate]
  list2env(deps1, envir = envPkgDepDeps())
  set(pkgDT, rowsToUpdate, "cached", TRUE)

  pkgDT[]
}



addDepthAndParentPkg <- function(deps, nam, .depth) {
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
    pkgDT <- parsePackageFullname(pkgDT, sorted = FALSE)
    pkgDT <- parseGitHub(pkgDT)
    pkgDT <- updatePackagesWithNames(pkgDT, packagesNames)
    set(pkgDT, NULL, "topLevelOrder", seq(NROW(pkgDT)))
  }
  pkgDT
}

#' `split` for a data.table that keeps integrity of a column of lists of data.table objects
#'
#' `data.table::split` does 2 bad things:
#'  1. reorders if using `f`
#'  2. destroys the integrity of a column that is a list of data.tables, when using `by`
#' So, to keep order, need `by`, but to keep integrity, need `f`. This function
#' @return
#' A list of `data.table` objects of `length(unique(splitOn))`.
splitKeepOrderAndDTIntegrity <- function(pkgDT, splitOn) {
  # keeps order and integrity.
  # split reorders!! careful
  # The data.table "by" doesn't work -- the list of data.tables doesn't work
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
  # must be correctx which & recursive too
  curCache <- grep(whichCatRecursive(which, recursive), curCache, value = TRUE)
  # if (!is.null(pkgDT$snNeeded))
  maybeHaveCache <- Map(nam = pkgDT$packageFullName, sw = paste0(pkgDT$Package, sepForSaveNames),
                        function(nam, sw) which(startsWith(prefix = sw, curCache)))
  maybeHaveCacheCurCache <- unlist(maybeHaveCache)
  maybeHaveCacheDT <- list(packageFullName = rep(names(maybeHaveCache), lengths(maybeHaveCache)),
                           rowNum = maybeHaveCacheCurCache) |> setDT()
  # set(pkgDT, NULL, "cached", TRUE)

  # if (NROW(maybeHaveCacheDT)) {
  if (any(maybeHaveCacheDT$packageFullName %in% pkgDT$packageFullName)) {
    # rowToFillIn <- if (is.null(pkgDT[[deps(FALSE)]])) seq(NROW(pkgDT)) else which(sapply(pkgDT[[deps(FALSE)]], NROW) == 0)
    # depsList <- lapply(rowToFillIn, function(x) data.table(Package = character(0)))
    # set(pkgDT, rowToFillIn, deps(FALSE), depsList)
    # pkgDTPoss <- pkgDT[which(pkgDT$Package %in% names(maybeHaveCache))]
    # wantLatestCRAN <- is.na(pkgDTPoss$versionSpec) | startsWith(prefix = ">", pkgDTPoss$inequality)# == ">="
    # if (any(wantLatestCRAN)) {

    lst <- sapply(maybeHaveCacheDT, is.list)
    keep <- setdiff(colnames(maybeHaveCacheDT), names(lst)[which(lst)])
    dups <- duplicated(maybeHaveCacheDT[, ..keep])
    if (any(dups)) {
      maybeHaveCacheDT <- maybeHaveCacheDT[which(!dups)]
    }

    maybeHaveCacheDT <- pkgDT[maybeHaveCacheDT, on = "packageFullName"]

    pkgDT <- getDepsFromCache(pkgDT, maybeHaveCacheDT, recursive = TRUE, curCache)
    pkgDT <- getDepsFromCache(pkgDT, maybeHaveCacheDT, recursive = FALSE, curCache)

    if (isTRUE(!pkgDT[Package %in% "Rcpp"]$cached %in% TRUE)) browser()
    # if (FALSE) {
    #
    #   isRecursiveTRUE <- endsWith(curCache[maybeHaveCacheDT$rowNum], "TRUE")
    #
    #   if (any(isRecursiveTRUE)) {
    #     set(maybeHaveCacheDT, NULL, "isRecursiveTRUE", isRecursiveTRUE)
    #     maybeHaveCacheDTRecTRUE <- maybeHaveCacheDT[isRecursiveTRUE]
    #     set(maybeHaveCacheDTRecTRUE, NULL, "curCache", curCache[maybeHaveCacheDTRecTRUE$rowNum])
    #     # curCachePoss <- curCache[maybeHaveCacheDT$rowNum[which(maybeHaveCacheDT$isRecursiveTRUE)]]
    #
    #     getFromCurCachePoss <- grepl(paste0(sepForSaveNames, "(CRAN)", sepForSaveNames, ".+TRUE$"),
    #                                  maybeHaveCacheDTRecTRUE$curCache)
    #     set(maybeHaveCacheDTRecTRUE, NULL, "toGet", getFromCurCachePoss)
    #
    #     whHaveCacheDT <- which(maybeHaveCacheDTRecTRUE$toGet)
    #     whHaveCachePkgDT <- match(maybeHaveCacheDTRecTRUE$Package[whHaveCacheDT], pkgDT$Package)
    #     a <- mget(maybeHaveCacheDTRecTRUE$curCache[whHaveCacheDT], envir = envPkgDepDeps())
    #     if (!identical(unname(a), pkgDT[[deps(FALSE)]][whHaveCachePkgDT])) {
    #       set(pkgDT, whHaveCachePkgDT, deps(TRUE), unname(a))
    #     }
    #     set(pkgDT, whHaveCachePkgDT, "cached", TRUE)
    #     set(pkgDT, whHaveCachePkgDT, sn(TRUE), maybeHaveCacheDTRecTRUE$curCache[whHaveCacheDT])
    #     pkgsWithRecursiveTRUE <- maybeHaveCacheDT$Package[match(maybeHaveCacheDT$rowNum[getFromCurCachePoss],
    #                                                             maybeHaveCacheDT$rowNum)]
    #     # This changes the "maybes" becaues some are now gotten
    #     maybeHaveCacheDT <- maybeHaveCacheDT[!maybeHaveCacheDT$Package %in% pkgsWithRecursiveTRUE]
    #   }
    #   isRecursiveFALSE <- endsWith(curCache[maybeHaveCacheDT$rowNum], "FALSE")
    #   if (any(isRecursiveFALSE)) {
    #
    #     set(maybeHaveCacheDT, NULL, "isRecursiveFALSE", isRecursiveFALSE)
    #     maybeHaveCacheDTRecFALSE <- maybeHaveCacheDT[isRecursiveFALSE]
    #     set(maybeHaveCacheDTRecFALSE, NULL, "curCache", curCache[maybeHaveCacheDTRecFALSE$rowNum])
    #
    #     ###
    #     getFromCurCachePoss <- grepl(paste0(sepForSaveNames, "(CRAN)", sepForSaveNames, ".+TRUE$"),
    #                                  maybeHaveCacheDTRecFALSE$curCache)
    #     set(maybeHaveCacheDTRecFALSE, NULL, "toGet", getFromCurCachePoss)
    #
    #     whHaveCacheDT <- which(maybeHaveCacheDTRecFALSE$toGet)
    #     whHaveCachePkgDT <- match(maybeHaveCacheDTRecFALSE$Package[whHaveCacheDT], pkgDT$Package)
    #
    #     # maybeHaveCacheCurCache <- maybeHaveCacheCurCache[isRecursiveFALSE]
    #
    #     # getThese <- grep(paste0(sepForSaveNames, "(CRAN)", sepForSaveNames, ".+FALSE$"),
    #     #                  curCache[maybeHaveCacheCurCache])
    #     # whHaveCache <- match(maybeHaveCacheDT$Package[getThese], pkgDT$Package)
    #     noColYet <- pkgDT[whHaveCachePkgDT][[deps(FALSE)]]
    #     if (is.null(noColYet)) {
    #       a <- mget(maybeHaveCacheDTRecFALSE$curCache[whHaveCacheDT], envir = envPkgDepDeps())
    #       # a <- mget(curCache[maybeHaveCacheCurCache][getThese], envir = envPkgDepDeps())
    #       set(pkgDT, whHaveCachePkgDT, deps(FALSE), unname(a))
    #     } else {
    #       a <- lapply(whHaveCachePkgDT, function(x) emptyDT())#list(Package = NULL)
    #     }
    #     # set(pkgDT, whHaveCache,
    #     #     # unname(maybeHaveCacheCurCache[getThese]),
    #     #     deps(FALSE),
    #     #     unname(mget(curCache[maybeHaveCacheCurCache][getThese], envir = envPkgDepDeps())))
    #     set(pkgDT, whHaveCachePkgDT, "cached", TRUE)
    #     set(pkgDT, whHaveCachePkgDT, sn(FALSE), curCache[maybeHaveCacheCurCache][getThese])
    #     # }
    #     # if (any(!wantLatestCRAN)) {
    #     # Here, we may not want latest CRAN, so we need to check Archives for this package
    #     # isRecursiveTRUE <- endsWith(curCache[maybeHaveCacheCurCache], "TRUE")
    #     # if (any(isRecursiveTRUE)) {
    #     #   a <- mget(curCache[maybeHaveCacheCurCache][isRecursiveTRUE], envir = envPkgDepDeps())
    #     #   getThese <- grep(paste0(sepForSaveNames, "(CRAN)", sepForSaveNames, ".+TRUE$"),
    #     #                    curCache[maybeHaveCacheDT$rowNum])
    #     #   whHaveCache <- match(maybeHaveCacheDT$Package[getThese], pkgDT$Package)
    #     #   set(pkgDT, whHaveCache, deps(TRUE), unname(a))
    #     # }
    #     # get0(pkgDTPoss)
    #   }
    #
    # }
  }
  pkgDT
}


getDepsFromCache <- function(pkgDT, maybeHaveCacheDT, recursive, curCache) {
  haveInCache <- endsWith(curCache[maybeHaveCacheDT$rowNum], as.character(recursive))

  if (isTRUE(any(haveInCache))) {
    isRecursiveTxt <- paste0("isRecursive", recursive)

    set(maybeHaveCacheDT, NULL, isRecursiveTxt, haveInCache)
    maybeHaveCacheDTRec <- maybeHaveCacheDT[which(haveInCache)]
    set(maybeHaveCacheDTRec, NULL, "curCache", curCache[maybeHaveCacheDTRec$rowNum])
    # maybeHaveCacheDTRec <- pkgDT[maybeHaveCacheDTRec, on = "Package"]
    # curCachePoss <- curCache[maybeHaveCacheDT$rowNum[which(maybeHaveCacheDT$haveInCache)]]
    getFromCurCachePoss <- grepl(paste0(sepForSaveNames, "(CRAN)", sepForSaveNames, paste0(".+", recursive, "$")),
                                 maybeHaveCacheDTRec$curCache)

    versions <- sapply(strsplit(maybeHaveCacheDTRec$curCache, sepForSaveNames), function(x) x[2])
    set(maybeHaveCacheDTRec, NULL, "version", versions)
    # maybeHaveCacheDTRec1 <- pkgDT[, c("inequality", "versionSpec", "Package")][maybeHaveCacheDTRec, on = "Package"]
    # maybeHaveCacheDTRec <- maybeHaveCacheDTRec1
    haveCache <- getFromCurCachePoss
    CRANvOKbcNeedLatest <- is.na(maybeHaveCacheDTRec$inequality)
    if (isTRUE(any(!CRANvOKbcNeedLatest))) {
      isGitHead <- maybeHaveCacheDTRec$versionSpec %in% "HEAD" & maybeHaveCacheDTRec$repoLocation %in% "GitHub"
      if (any(isGitHead)) {
        maybeHaveCacheDTRec[which(isGitHead), correctVersionInCache := FALSE] # force re-check
        # hasVersionNum <- hasVersionNum[!isGitHead]
        CRANvOKbcNeedLatest <- CRANvOKbcNeedLatest[!isGitHead]
      }
      if (length(CRANvOKbcNeedLatest))
        maybeHaveCacheDTRec[CRANvOKbcNeedLatest %in% FALSE,
                            correctVersionInCache := compareVersion2(version, versionSpec, inequality)]
      haveCache <- maybeHaveCacheDTRec[["correctVersionInCache"]] |
                         is.na(maybeHaveCacheDTRec[["correctVersionInCache"]])
      correctV <- maybeHaveCacheDTRec[["correctVersionInCache"]]
      correct <- correctV | CRANvOKbcNeedLatest
      maybeHaveCacheDTRec <- maybeHaveCacheDTRec[which(correct)]
      getFromCurCachePoss <- correct[which(correct)]
    }

    if (any(haveCache)) {

      set(maybeHaveCacheDTRec, NULL, "toGet", getFromCurCachePoss)

      whHaveCacheDT <- which(maybeHaveCacheDTRec[["toGet"]])
      whHaveCachePkgDT <- match(maybeHaveCacheDTRec[["Package"]][whHaveCacheDT], pkgDT[["Package"]])

      if (recursive %in% FALSE) {
        noColYet <- pkgDT[whHaveCachePkgDT][[deps(recursive)]]
        if (is.null(noColYet)) {
          a <- mget(maybeHaveCacheDTRec$curCache[whHaveCacheDT], envir = envPkgDepDeps())
          if (is.null(unlist(a)))
            a <- lapply(seq(NROW(pkgDT)), function(x) emptyDT())
          # a <- mget(curCache[maybeHaveCacheCurCache][getThese], envir = envPkgDepDeps())
          set(pkgDT, whHaveCachePkgDT, deps(recursive), unname(a))
        } else {
          # a <- lapply(whHaveCachePkgDT, function(x) data.table())#list(Package = NULL)
        }
      } else {
        a <- mget(maybeHaveCacheDTRec$curCache[whHaveCacheDT], envir = envPkgDepDeps())
        if (!identical(unname(a), pkgDT[[deps(recursive)]][whHaveCachePkgDT])) {
          set(pkgDT, whHaveCachePkgDT, deps(recursive), unname(a))
        }
      }

      set(pkgDT, whHaveCachePkgDT, "cached", TRUE)
      set(pkgDT, whHaveCachePkgDT, sn(recursive), maybeHaveCacheDTRec$curCache[whHaveCacheDT])
    }
  }
  pkgDT
}

emptyDT <- function()
  list(Package = character(), packageFullName = character(), which = character()) |> setDT()
