utils::globalVariables(
  c(
    "atLeastOneWithVersionSpec",
    "Current",
    "GithubRepo", "GithubSHA1", "GithubUsername",
    "hasVers",
    "localBranch", "localFiles", "localRepo", "localSha", "localUsername",
    "maxVersionSpec",
    "mtime",
    "newPackageFullName",
    "PackageTrimmed"
  )
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
                   type = getOption("pkgType")) {
  purge <- dealWithCache(purge)
  checkAutomaticOfflineMode() # This will turn off offlineMode if it had been turned on automatically

  if (!includeBase) {
    packages <- packages[!packages %in% .basePkgs]
  }
  which <- depsImpsSugsLinksToWhich(depends, imports, suggests, linkingTo, which)

  # Only deal with first one of "which"... deal with second later
  whichCat <- paste(sort(which[[1]]), collapse = "_")
  if (length(packages)) {
    # trim redundancies
    pkgDT <- parseGitHub(packages)  #[isGH])
    pkgDT <- updatePackagesWithNames(pkgDT, packages)
    pkgDT <- parsePackageFullname(pkgDT)
    pkgDT <- trimRedundancies(pkgDT, repos = repos, purge = FALSE)
    packages <- pkgDT$packageFullName

    ap <-
      getAvailablePackagesIfNeeded(packages, repos, purge = FALSE, verbose, type)
    saveNames <-
      saveNamesForCache(packages, which, recursive = recursive, ap = ap, verbose = verbose)
    packagesNoVersion <- trimVersionNumber(packages)
    saveNamesDT <-
      data.table(saveNames, packages, packagesNoVersion, names = names(saveNames))
    dups <- duplicated(saveNamesDT$saveNames)
    saveNamesDT <- saveNamesDT[dups %in% FALSE]
    saveNames <- saveNames[!dups] # keep this object because it has names, which are used for lapply/Map
    if (isTRUE(purge)) {
      whExist <- unlist(lapply(saveNames, exists, envir = .pkgEnv))
      if (any(whExist)) {
        suppressWarnings(rm(list = saveNames[whExist], envir = .pkgEnv))
      }
    }
    # this will be short because saveNamesDT$saveNames has no version numbers if they are "inequalities" i.e,. >= or <=
    neededFull1 <-
      lapply(saveNames, get0, envir = .pkgEnv$pkgDep$deps)
    saveNamesOrig <- names(neededFull1)
    out111 <-
      try(set(saveNamesDT, NULL, "saveNamesOrig", saveNamesOrig))
    if (is(out111, "try-error")) {
      browserDeveloper("Error 918; please contact developer")
    }
    # Expand it back out to full length

    needGet <- unlist(lapply(neededFull1, is.null))
    packageFullNamesToGet <- names(needGet)[needGet]
    if (any(needGet)) {
      fn <- pkgDepDBFilename()
      if (length(fn)) {
        # user may not be using Cache
        if (file.exists(fn)) {
          # may be first time through, so there would be none yet
          if (purge) {
            unlink(fn)
          } else {
            savedNeededFull1 <- readRDS(fn)
            ord <- match(saveNamesDT$saveNames, names(savedNeededFull1))
            have <- savedNeededFull1[ord]
            names(have) <- saveNamesDT$names
            toGet <- packageFullNamesToGet
            toGet <- toGet[toGet %in% names(have)]
            neededFull1[toGet] <- have[toGet]
            needGet <- unlist(lapply(neededFull1, is.null))
            messageVerbose(
              "Using file-backed cache; to refresh this, set purge = TRUE",
              verbose = verbose,
              verboseLevel = 2
            )
          }
        }
      }
    }

    Npackages <- NROW(packageFullNamesToGet)
    messageIfGTN <- Npackages > 5

    if (any(needGet)) {
      NpackagesGitHub <- sum(!is.na(extractPkgGitHub(packageFullNamesToGet)))
      NpackagesCRAN <- Npackages - NpackagesGitHub
      if (messageIfGTN) {
        messageVerbose(
          "Getting dependencies for ",
          if (NpackagesCRAN > 0) {
            paste0(NpackagesCRAN, " packages on CRAN")
          },
          if (NpackagesGitHub > 0) {
            paste0("; ", NpackagesGitHub, " packages on GitHub")
          },
          verbose = verbose,
          verboseLevel = 0
        )
      }
      ap <-
        getAvailablePackagesIfNeeded(packages[needGet], repos, purge = FALSE, verbose, type)
      neededFull <-
        try(pkgDepInnerMemoise(
          packages = packageFullNamesToGet,
          libPath = libPath,
          which = which[[1]],
          keepVersionNumber = keepVersionNumber,
          purge = FALSE,
          repos = repos,
          verbose = verbose,
          includeBase = includeBase,
          type = type,
          ap = ap
        ))
      if (is(neededFull, "try-error")) {
        browserDeveloper("Error 2343")
      }
      purge <- FALSE # whatever it was, it was done in line above
      theNulls <-
        unlist(lapply(neededFull, function(x) {
          is.null(x) || length(x) == 0
        }))
      neededFull2 <- neededFull[!theNulls]
      NpackagesRecursive <- NROW(neededFull2)
      if (messageIfGTN) {
        nchars <- rep(" ", 2 * nchar(NpackagesRecursive) + 5)
        messageVerbose(
          "... ",
          NpackagesRecursive,
          " of these have recursive dependencies",
          nchars,
          verbose = verbose,
          verboseLevel = 0
        )
      }
      if (NROW(neededFull2)) {
        if (recursive) {
          which <- tail(which, 1)[[1]] # take the last of the list of which
          which <-
            setdiff(which, "Suggests") # Suggests is never recursive
          ap <-
            getAvailablePackagesIfNeeded(unlist(neededFull2), repos, purge = FALSE, verbose, type)
          neededFull2 <-
            Map(
              needed = neededFull2, counter = seq_along(neededFull2),
              function(needed, counter) {
                i <- 1
                pkgsNew <- list()
                pkgsNew[[i]] <- needed
                while (length(unlist(pkgsNew[[i]])) > 0) {
                  i <- i + 1
                  pkgsToLookupFull <-
                    pkgsNew[[i - 1]]
                  names(pkgsToLookupFull) <-
                    pkgsToLookupFull
                  pkgsNew[[i]] <-
                    lapply(pkgsToLookupFull, function(needed) {
                      unique(unlist({
                        a <- pkgDepInnerMemoise(
                          packages = needed,
                          libPath = libPath,
                          which = which,
                          keepVersionNumber = keepVersionNumber,
                          purge = FALSE,
                          repos = repos,
                          verbose = verbose,
                          includeBase = includeBase,
                          ap = ap
                        )
                        if (is(a, "try-error")) {
                          browserDeveloper("Error 23")
                        }

                        a
                      }))
                    })
                  prevIndices <- 1:(i - 1)
                  curPkgs <- unlist(pkgsNew[[i]])
                  prevPkgs <-
                    unlist(pkgsNew[prevIndices])
                  dt <-
                    data.table(
                      Package = c(prevPkgs, curPkgs),
                      Current = c(rep(FALSE, length(
                        prevPkgs
                      )), rep(TRUE, length(
                        curPkgs
                      )))
                    )
                  set(
                    dt,
                    NULL,
                    "PackageTrimmed",
                    extractPkgName(dt$Package)
                  )
                  set(
                    dt,
                    NULL,
                    "versionSpec",
                    extractVersionNumber(dt$Package)
                  )
                  set(dt, NULL, "hasVers", !is.na(dt$versionSpec))
                  hasV <- dt$hasVers == TRUE
                  dt[, `:=`(
                    atLeastOneWithVersionSpec = any(hasVers),
                    Current = all(Current == TRUE)
                  ), by = "PackageTrimmed"]
                  dt <-
                    dt[!(dt$atLeastOneWithVersionSpec == TRUE &
                      dt$hasVers == FALSE)] # remove cases where no version spec >1 case

                  versionSpecNA <-
                    is.na(dt$versionSpec)
                  dt1 <- dt[versionSpecNA == FALSE]

                  if (NROW(dt1)) {
                    dt1 <- dt1[!duplicated(dt1$Package)]
                    ord <-
                      order(package_version(dt1$versionSpec),
                        decreasing = TRUE
                      )
                    dt1 <- dt1[ord]
                  }
                  dt2 <-
                    if (all(versionSpecNA)) {
                      dt
                    } else {
                      dt[versionSpecNA]
                    }
                  dt <-
                    if (NROW(dt1)) {
                      rbindlist(list(dt1, dt2),
                        use.names = TRUE,
                        fill = TRUE
                      )
                    } else {
                      dt2
                    }

                  dups <-
                    duplicated(dt$PackageTrimmed)
                  dt3 <-
                    if (any(dups)) {
                      dt[!dups]
                    } else {
                      dt
                    }

                  curTrue <- dt3$Current == TRUE
                  dt4 <-
                    if (any(curTrue)) {
                      if (all(curTrue)) {
                        dt3
                      } else {
                        dt3[curTrue]
                      }
                    } else {
                      dt3[0]
                    }
                  pkgsNew <- list()
                  pkgsNew[[i - 1]] <-
                    dt3[dt3$Current == FALSE]$Package
                  pkgsNew[[i]] <- dt4$Package
                }
                needed <- unique(unlist(pkgsNew))
                if (messageIfGTN) {
                  messageVerboseCounter(
                    counter = counter,
                    total = NpackagesRecursive,
                    verbose = verbose,
                    verboseLevel = 0
                  )
                }
                needed
              }
            )
        }
      }
      # Remove "R"
      neededFull2 <- append(neededFull2, neededFull[theNulls])

      newOnes <- saveNamesDT$names %in% names(neededFull)

      neededFull2 <- lapply(neededFull2, function(n) {
        n <- grep(.grepR, n, value = TRUE, invert = TRUE)
        n <- gsub(.grepTabCR, "", n)
        n <- gsub(.grepTooManySpaces, "", n)
        n <- gsub(paste0("([[:alnum:]]{1})(\\()"), "\\1 \\2", n)
        n
      })

      # Add self to vector
      Map(sn = saveNamesDT$saveNames[newOnes], n = saveNamesDT$names[newOnes], function(sn, n) {
        assign(sn, neededFull2[[n]], envir = .pkgEnv[["pkgDep"]][["deps"]])
      })
      neededFull1 <- append(neededFull1[!needGet], neededFull2)
    }


    if (isTRUE(sort)) {
      neededFull1 <- lapply(neededFull1, function(x) {
        sort(x)
      })
    }
    # Put the package *without* its inequality (because they aren't there) in the first slot
    nam1 <-
      saveNamesDT$packages[match(names(neededFull1), saveNamesDT$saveNamesOrig)]
    names(neededFull1) <- nam1
    neededFull1 <- prependSelf(neededFull1, includeSelf)
    nam1 <-
      saveNamesDT$saveNamesOrig[match(names(neededFull1), saveNamesDT$packages)]
    names(neededFull1) <- nam1

    # neededFull1 <- Map(p = neededFull1, n = names(neededFull1), function(p, n)
    #   unique(rmExtraSpaces(c(if (isTRUE(includeSelf)) n else character(), p))))

    # file-backed cache

    if (any(needGet)) {
      fn <- pkgDepDBFilename()
      if (length(fn)) {
        # ie. using Cache mechanism
        saveNamesOrdered <- saveNames[names(neededFull1)]
        saveNeededFull1 <- neededFull1
        names(saveNeededFull1) <- saveNamesOrdered
        if (file.exists(fn)) {
          prev <- readRDS(fn)
          saveNeededFull1 <- c(saveNeededFull1, prev)
          saveNeededFull1 <-
            saveNeededFull1[!duplicated(names(saveNeededFull1))]
        }
        checkPath(dirname(fn), create = TRUE)
        saveRDS(saveNeededFull1, file = fn)
      }
    }
    saveGitHubSHAsToDisk()

    if (isFALSE(keepVersionNumber)) {
      neededFull1 <- lapply(neededFull1, trimVersionNumber)
    }
    if (!isTRUE(includeBase)) {
      neededFull1 <- lapply(neededFull1, setdiff, .basePkgs)
    }
    if (messageIfGTN) {
      messageVerbose("\b Done!", verbose = verbose, verboseLevel = 0)
    }

    # put original inequalities back
    aa <-
      neededFull1[match(saveNamesDT$saveNamesOrig, names(neededFull1))]

    names(aa) <- saveNamesDT$packages
    neededFull1 <- aa

    # Put the package *with* its inequality in the first slot
    neededFull1 <- prependSelf(neededFull1, includeSelf)
  } else {
    neededFull1 <- list()
  }
  neededFull1
}

#' @importFrom utils untar download.file
#' @inheritParams Require
pkgDepInner <- function(packages,
                        libPath,
                        which,
                        keepVersionNumber,
                        purge = getOption("Require.purge", FALSE),
                        repos = repos,
                        includeBase = FALSE,
                        verbose = getOption("Require.verbose"),
                        type = getOption("pkgType"),
                        ap) {
  names(packages) <- packages
  pkgsNoVersion <- extractPkgName(packages)
  # purge <- dealWithCache(purge, checkAge = TRUE)
  if (!isTRUE(includeBase)) {
    isBase <- pkgsNoVersion %in% .basePkgs
    packagesToCheck <- packages[!isBase]
    pkgsNoVersionToCheck <- pkgsNoVersion[!isBase]
    packagesBase <- packages[isBase]
    packagesBase <- lapply(packagesBase, function(x) {
      character()
    })
  } else {
    packagesToCheck <- packages
    pkgsNoVersionToCheck <- pkgsNoVersion
  }

  needed <- Map( # desc_path = desc_paths,
    pkg = packagesToCheck,
    pkgNoVersion = pkgsNoVersionToCheck,
    function( # desc_path,
             pkg, pkgNoVersion) {
      pkgDT <- parseGitHub(pkg, verbose = verbose)
      if ("GitHub" %in% pkgDT$repoLocation) {
        needed <-
          try(getGitHubDepsMemoise(
            pkg = pkg,
            pkgDT = pkgDT,
            which = which,
            purge = FALSE,
            includeBase = includeBase
          ))
        if (is(needed, "try-error")) {
          browserDeveloper("Error 357; please contact developer")
        }
      } else {
        if (isFALSE(getOption("Require.offlineMode", FALSE))) {
          for (attmpt in 1:2) {
            needed <- unique(unname(unlist(
              pkgDepCRANMemoise(
                pkg = pkg,
                pkgsNoVersion = pkgNoVersion,
                which = which,
                keepVersionNumber = keepVersionNumber,
                purge = FALSE,
                repos = repos,
                verbose = verbose,
                type = type,
                ap = ap
              )
            )))
            # null means not in ap; attmpt is only try 1x after purging, but 3rd condition -->
            #    if the pkg is ap, then it is a version problem, so no purging will help.
            if (is.null(needed) && attmpt < 2 && !(pkgNoVersion %in% ap$Package)) {
              ap <-
                available.packagesCached(
                  repos = repos,
                  purge = TRUE,
                  verbose = verbose,
                  type = type
                )
            } else {
              break
            }
          }


          if (is.null(needed)) {
            # essesntially, failed
            pkgName <- extractPkgName(pkg)
            pkgPrint <-
              if (any(grepl("==NA", pkg))) {
                pkgName
              } else {
                pkg
              }
            inequ <- extractInequality(pkg)
            if (!inequ %in% "==") {
              pkg <- gsub(inequ, "==", pkg)
            }
            td <- tempdir2(pkg) #  use the version number for td
            packageTD <- file.path(td, pkgName)
            if (!dir.exists(packageTD)) {
              verNum <- extractVersionNumber(pkg)
              noSpecNeeded <- is.na(verNum) || identical(verNum, "NA")
              if (noSpecNeeded) {
                localInstallOK <- installedVers(pkgName)
                if (isTRUE(localInstallOK$installed %in% TRUE)) {
                  packageTD <- system.file(package = pkgName)
                }
              }

              if (!dir.exists(packageTD)) {
                if (noSpecNeeded) {
                  ava <- archiveVersionsAvailable(pkgName, repos = repos)
                  dt <- if (is(ava, "list")) {
                    rbindlist(lapply(ava, as.data.table, keep.rownames = "packageURL"))
                  } else {
                    as.data.table(ava, keep.rownames = "packageURL")
                  }
                  colNamesToSortOn <- intersect(colnames(dt), "mtime")
                  if (length(colNamesToSortOn)) {
                    data.table::setorderv(dt, "mtime")
                  } # order it so last one is the most recent one
                  packageURL <-
                    if (NROW(dt)) {
                      tail(dt$packageURL, 1)
                    } else {
                      character()
                    }
                } else {
                  pkgFilename <- paste0(pkgName, "_", verNum, ".tar.gz")
                  packageURL <- file.path(pkgName, pkgFilename)
                  dt <- numeric()
                }
                if (!is.null(packageURL)) {
                  if (endsWith(packageURL, "tar.gz")) {
                    messageVerbose(
                      "available.packages() does not have correct information on package dependencies for ",
                      pkgPrint,
                      "; checking CRAN archives",
                      verbose = verbose,
                      verboseLevel = 1
                    )
                    for (repo in repos) {
                      url <- getArchiveURL(repo, packageURL)
                      url2 <-
                        file.path(contrib.url(repo), basename(packageURL))
                      tf <- tempfile()
                      if (isFALSE(getOption("Require.offlineMode", FALSE))) {
                        haveFile <-
                          suppressWarnings(tryCatch(
                            download.file(url, tf, quiet = TRUE),
                            error = function(x) {
                              tryCatch(
                                download.file(url2, tf, quiet = TRUE),
                                error = function(y) {
                                  FALSE
                                }
                              )
                            }
                          ))
                      }
                      if (file.exists(tf)) {
                        untar(tarfile = tf, exdir = td)
                        filesToDel <-
                          dir(
                            packageTD,
                            recursive = TRUE,
                            full.names = TRUE,
                            include.dirs = TRUE
                          )
                        filesToDel <-
                          filesToDel[grep("^DESCRIPTION$",
                                          basename(filesToDel),
                                          invert = TRUE
                          )]
                        unlink(filesToDel, recursive = TRUE)
                        break
                      }
                    }
                  }
                }
              }
            }
            needed <- if (dir.exists(packageTD)) {
              DESCRIPTIONFileDeps(
                file.path(packageTD, "DESCRIPTION"),
                which = which,
                keepVersionNumber = keepVersionNumber,
                purge = FALSE
              )
            } else {
              messageVerbose(
                pkgPrint,
                " dependencies not found on CRAN; perhaps incomplete description? On GitHub?",
                verbose = verbose,
                verboseLevel = 1
              )
              character()
            }
          }
          purge <<- FALSE
          needed
        } else {
          needed <- NULL
          needed
        }
      }
      # } else {
      #  needed <- DESCRIPTIONFileDeps(desc_path, which = which, keepVersionNumber = keepVersionNumber)
      # }
      sn <-
        saveNamesForCache(pkg,
          which = list(which),
          recursive = FALSE,
          verbose = verbose,
          ap = ap
        )
      .pkgEnv[["pkgDep"]][["deps"]][[sn]] <- needed
      needed
    }
  )
  if (!isTRUE(includeBase)) {
    needed1 <- append(needed, packagesBase)
    needed <- needed1[match(names(packages), names(needed1))]
  }

  needed
}

getDescPath <- function(packages, libPath) {
  lapply(packages, function(pkg) {
    dp <- sprintf("%s/%s/DESCRIPTION", libPath, pkg) # nolint
    fe <- file.exists(dp)
    dp[fe][1] # take first file that exists
  })
}

#' @description
#' `pkgDep2` is a convenience wrapper of `pkgDep` that "goes one level in",
#' i.e., the first order dependencies, and runs the `pkgDep` on those.
#' @rdname pkgDep
#' @export
#' @param sorted Logical. If `TRUE`, the default, the packages will be sorted in
#'   the returned list from most number of dependencies to least.
#' @examples
#' \dontrun{
#' if (Require:::.runLongExamples()) {
#'   opts <- Require:::.setupExample()
#'
#'   pkgDep2("reproducible")
#'   # much bigger one
#'   pkgDep2("tidyverse")
#'
#'   Require:::.cleanup(opts)
#' }
#' }
pkgDep2 <- function(packages,
                    recursive = TRUE,
                    which = c("Depends", "Imports", "LinkingTo"),
                    depends,
                    imports,
                    suggests,
                    linkingTo,
                    repos = getOption("repos"),
                    sorted = TRUE,
                    purge = getOption("Require.purge", FALSE),
                    includeSelf = TRUE,
                    verbose = getOption("Require.verbose")) {
  if (length(packages) > 1) stop("packages should be length 1")
  deps <- pkgDep(
    packages,
    recursive = FALSE,
    which = which,
    depends = depends,
    imports = imports,
    suggests = suggests,
    linkingTo = linkingTo,
    repos = repos,
    includeSelf = includeSelf
  )
  deps <- lapply(deps, function(d) setdiff(d, packages))[[1]]

  a <-
    lapply(
      deps,
      pkgDep,
      depends = depends,
      imports = imports,
      suggests = suggests,
      linkingTo = linkingTo,
      repos = repos,
      recursive = recursive,
      includeSelf = includeSelf,
      verbose = verbose - 1
    )
  a <- unlist(a, recursive = FALSE)
  if (sorted) {
    ord <- order(sapply(a, function(x) {
      length(x)
    }), decreasing = TRUE)
    a <- a[ord]
  }
  return(a)
}

#' @importFrom utils compareVersion
#' @inheritParams Require
pkgDepCRAN <-
  function(pkg,
           which = c("Depends", "Imports", "LinkingTo"),
           # recursive = FALSE,
           pkgsNoVersion,
           keepVersionNumber = TRUE,
           repos = getOption("repos"),
           purge = getOption("Require.purge", FALSE),
           verbose = getOption("Require.verbose"),
           type = getOption("pkgType"),
           ap) {

    needAp <- TRUE # covers the missing(ap) case
    if (!missing(ap)) needAp <- is.null(ap)

    if (needAp) {
      ap <-
        available.packagesCached(
          repos = repos,
          purge = TRUE,
          verbose = verbose,
          type = type
        )
    }

    deps <- pkgDepCRANInner(
        ap,
        which = which,
        pkgs = pkg,
        pkgsNoVersion = pkgsNoVersion,
        keepVersionNumber = keepVersionNumber,
        verbose = verbose
      )
    deps
  }

#' Reverse package depends
#'
#' This is a wrapper around `tools::dependsOnPkgs`,
#' but with the added option of `sorted`, which
#' will sort them such that the packages at the top will have
#' the least number of dependencies that are in `pkgs`.
#' This is essentially a topological sort, but it is done
#' heuristically. This can be used to e.g., `detach` or
#' `unloadNamespace` packages in order so that they each
#' of their dependencies are detached or unloaded first.
#' @param pkgs A vector of package names to evaluate their
#'   reverse depends (i.e., the packages that *use* each
#'   of these packages)
#' @param deps An optional named list of (reverse) dependencies.
#'   If not supplied, then `tools::dependsOnPkgs(..., recursive = TRUE)`
#'   will be used
#' @param topoSort Logical. If `TRUE`, the default, then
#'   the returned list of packages will be in order with the
#'   least number of dependencies listed in `pkgs` at
#'   the top of the list.
#' @param reverse Logical. If `TRUE`, then this will use `tools::pkgDependsOn`
#'   to determine which packages depend on the `pkgs`
#' @param useAllInSearch Logical. If `TRUE`, then all non-core
#' R packages in `search()` will be appended to `pkgs`
#' to allow those to also be identified
#' @param returnFull Logical. Primarily useful when `reverse = TRUE`.
#'   If `TRUE`, then then all installed packages will be searched.
#'   If `FALSE`, the default, only packages that are currently in
#'   the `search()` path and passed in `pkgs` will be included
#'   in the possible reverse dependencies.
#'
#' @inheritParams Require
#' @export
#' @rdname pkgDep
#' @return
#' A possibly ordered, named (with packages as names) list where list elements
#' are either full reverse depends.
#'
#' @examples
#' \dontrun{
#' if (Require:::.runLongExamples()) {
#'   opts <- Require:::.setupExample()
#'
#'   pkgDepTopoSort(c("Require", "data.table"), reverse = TRUE)
#'
#'   Require:::.cleanup(opts)
#' }
#' }
#'
pkgDepTopoSort <-
  function(pkgs,
           deps,
           reverse = FALSE,
           topoSort = TRUE,
           libPath = .libPaths(),
           useAllInSearch = FALSE,
           returnFull = TRUE,
           recursive = TRUE,
           purge = getOption("Require.purge", FALSE),
           which = c("Depends", "Imports", "LinkingTo"),
           type = getOption("pkgType"),
           verbose = getOption("Require.verbose")) {
    if (isTRUE(useAllInSearch)) {
      if (missing(deps)) {
        a <- search()
        a <- setdiff(a, .defaultPackages)
        a <- gsub("package:", "", a)
        pkgs <- unique(c(pkgs, a))
      } else {
        messageVerbose(
          "deps is provided; useAllInSearch will be set to FALSE",
          verbose = verbose,
          verboseLevel = 2
        )
      }
    }

    names(pkgs) <- pkgs
    if (missing(deps)) {
      aa <- if (isTRUE(reverse)) {
        ip <- .installed.pkgs()
        deps <- lapply(ip[, "Dependencies"], extractPkgName)
        names(deps) <- ip[, "Package"]
        names(pkgs) <- pkgs
        deps <- deps[order(names(deps))]
        revDeps <-
          lapply(pkgs, function(p) {
            names(unlist(
              lapply(deps, function(d) {
                if (isTRUE(any(p %in% d))) {
                  TRUE
                } else {
                  NULL
                }
              })
            ))
          })
        if (recursive) {
          revDeps <- lapply(revDeps, function(p) {
            if (!is.null(p)) {
              used <- p
              repeat ({
                r <-
                  unique(unlist(lapply(p, function(p1) {
                    names(unlist(
                      lapply(deps, function(d) {
                        if (isTRUE(any(
                          p1 %in% d
                        ))) {
                          TRUE
                        } else {
                          NULL
                        }
                      })
                    ))
                  })))
                used <- unique(c(r, used))
                if (length(r) == 0) {
                  break
                }
                p <- r
              })
            } else {
              used <- NULL
            }
            sort(used)
          })
        }
      } else {
        pkgDep(
          pkgs,
          recursive = TRUE,
          purge = purge,
          libPath = libPath,
          which = which,
          verbose = verbose,
          includeSelf = FALSE
        )
      }
    } else {
      aa <- deps
    }
    bb <- list()

    aa <- checkCircular(aa)
    cc <- lapply(aa, function(x) {
      character()
    })
    dd <- lapply(cc, function(x) {
      0
    })


    if (length(aa) > 1) {
      lengths <- lengths(aa)
      aa <- aa[order(lengths)]
      cc <- cc[order(lengths)]
      dd <- lapply(cc, function(x) {
        0
      })

      ddIndex <- 0
      priorsBeingInstalled <- priorsAlreadyInstalled <- character()

      if (isTRUE(topoSort)) {
        notInOrder <- TRUE
        isCorrectOrder <- logical(length(aa))
        i <- 1
        newOrd <- numeric(0)
        for (i in seq_along(aa)) {
          dif <- setdiff(seq_along(aa), newOrd)
          pkgNameNames <- extractPkgName(names(aa))
          for (j in dif) {
            pkgName <- extractPkgName(aa[[j]])
            overlapFull <- pkgName %in% pkgNameNames[-i]
            overlap <- pkgName %in% pkgNameNames[dif]
            overlapPkgs <- pkgName[overlapFull]
            isCorrectOrder <- !any(overlap)
            if (isCorrectOrder) {
              cc[j] <- list(overlapPkgs)
              priorsBeingInstalled <-
                vapply(dd, function(x) {
                  if (is.numeric(x)) {
                    x == ddIndex
                  } else {
                    FALSE
                  }
                }, logical(1))
              priorsBeingInstalled <-
                extractPkgName(names(priorsBeingInstalled)[priorsBeingInstalled])
              overlapPkgsAdditional <-
                intersect(overlapPkgs, priorsBeingInstalled)
              if (length(overlapPkgsAdditional)) {
                ddIndex <- ddIndex + 1
                priorsAlreadyInstalled <-
                  vapply(dd, function(x) {
                    if (is.numeric(x)) {
                      x < ddIndex
                    } else {
                      FALSE
                    }
                  }, logical(1))
                priorsAlreadyInstalled <-
                  extractPkgName(names(priorsAlreadyInstalled)[priorsAlreadyInstalled])
              }
              dd[j] <- list(ddIndex)

              newOrd <- c(newOrd, j)
              # i <- i + 1
              break
            }
          }
        }
        aa <- aa[newOrd]
        cc <- cc[newOrd]
        dd <- dd[newOrd]
      }
    }

    out <- if (isTRUE(returnFull)) {
      aa
    } else {
      cc
    }
    attr(out, "installSafeGroups") <- dd

    return(out)
  }

.defaultPackages <-
  c(
    ".GlobalEnv",
    "tools:rstudio",
    "package:stats",
    "package:graphics",
    "package:grDevices",
    "package:utils",
    "package:datasets",
    "package:methods",
    "Autoloads",
    "package:base",
    "devtools_shims"
  )


#' @inheritParams Require
pkgDepCRANInner <-
  function(ap,
           which,
           pkgs,
           pkgsNoVersion,
           keepVersionNumber,
           keepSeparate = FALSE,
           verbose = getOption("Require.verbose")) {
    # MUCH faster to use base "ap$Package %in% pkgs" than data.table internal "Package %in% pkgs"
    if (missing(pkgsNoVersion)) {
      pkgsNoVersion <- trimVersionNumber(pkgs)
    }
    if (isFALSE(keepVersionNumber)) {
      pkgs <- pkgsNoVersion
    }
    depsOut <- list()
    if (pkgsNoVersion %in% ap$Package) {
      # is it on CRAN

      if (length(pkgsNoVersion) > 1) {
        # the next %in% used to be a `match`, which didn't work when there were 2 matches; which Windows now has with bin and src package options
        browserDeveloper("Error number 555; please contact developers")
      }

      ap <-
        ap[ap$Package %in% pkgsNoVersion] # gets only this package, but not necessarily in correct order
      # ap <- ap[match( pkgsNoVersion, ap$Package)]
      ap[, versionSpec := extractVersionNumber(pkgs)]
      ap[!is.na(versionSpec), ineq := extractInequality(pkgs)]
      ap[is.na(versionSpec), availableVersionOK := TRUE]
      b <-
        try(ap[!is.na(versionSpec), availableVersionOK := compareVersion2(Version, versionSpec, ineq)])
      if (is(b, "try-error")) {
        browserDeveloper("Error 818; please contact developers")
      }
      apList <- split(ap, by = "availableVersionOK")
      ap <- apList[["TRUE"]]

      if (!is.null(ap)) {
        keep <- match(ap$Package, pkgsNoVersion)
        keep <- keep[!is.na(keep)]
        pkgsNoVersion1 <- pkgsNoVersion[keep]
        pkgs <- pkgs[keep]
        ap <- ap[order(pkgsNoVersion1)]

        names(which) <- which
        deps <- lapply(which, function(i) {
          lapply(ap[[i]], function(x) {
            out <- strsplit(x, split = "(, {0,1})|(,\n)")[[1]]
            out <- out[!is.na(out)]
            out <-
              grep(.grepR, out, value = TRUE, invert = TRUE) # remove references to R
          })
        })

        ss <- seq_along(pkgsNoVersion1)
        names(ss) <- pkgsNoVersion1
        if (isFALSE(keepSeparate)) {
          deps <-
            lapply(ss, function(x) {
              unname(unlist(lapply(deps, function(y) {
                y[[x]]
              })))
            })
        }
        depsOut <- modifyList2(depsOut, deps, keep.null = TRUE)
      }
      if (!is.null(apList[["FALSE"]])) {
        depsFALSE <- Map(x = pkgs, function(x) {
          NULL
        }, USE.NAMES = TRUE)
        depsOut <- modifyList2(depsOut, depsFALSE, keep.null = TRUE)
      }
    }

    depsOut
  }

DESCRIPTIONFileDeps <-
  function(desc_path,
           which = c("Depends", "Imports", "LinkingTo"),
           keepVersionNumber = TRUE,
           purge = getOption("Require.purge", FALSE),
           keepSeparate = FALSE) {
    if (!exists("pkgDep", envir = .pkgEnv)) {
      .pkgEnv[["pkgDep"]] <- newPkgDepEnv()
    }
    if (!exists("DESCRIPTIONFile", envir = .pkgEnv[["pkgDep"]])) {
      .pkgEnv[["pkgDep"]][["DESCRIPTIONFile"]] <- newPkgDepEnv()
    }
    objName <- if (length(desc_path) == 1) {
      paste0(
        desc_path,
        paste0(collapse = "_", which, "_", keepVersionNumber)
      )
    } else {
      grepPackage <- "Package *: *"
      grepVersion <- "Version *: *"
      suppressWarnings({
        pkg <-
          gsub(
            grepPackage,
            "",
            grep(grepPackage, desc_path, value = TRUE)
          )
      })
      suppressWarnings({
        vers <-
          gsub(
            grepVersion,
            "",
            grep(grepVersion, desc_path, value = TRUE)
          )
      })
      paste(pkg,
        vers,
        sep = "_",
        paste0(which, "_", keepVersionNumber, collapse = "_")
      )
    }
    if (isTRUE(length(objName) > 1) ||
      isTRUE(any(!exists(objName, envir = .pkgEnv[["pkgDep"]][["DESCRIPTIONFile"]]))) ||
      isTRUE(purge)) {
      if (is.null(desc_path)) {
        needed <- NULL
      } else {
        lines <- if (length(desc_path) == 1) {
          try(readLines(desc_path))
        } else {
          lines <- desc_path
        }
        if (is(lines, "try-error")) {
          stop("Cannot read the file: ", desc_path)
        }
        Sys.setlocale(locale = "C") # required to deal with non English characters in Author names
        on.exit(Sys.setlocale(locale = ""))
        sl <- list()
        sl[["depends"]] <- grep("^Depends: *", lines) # nolint
        sl[["suggests"]] <- grep("^Suggests: *", lines) # nolint
        sl[["imports"]] <- grep("^Imports: *", lines) # nolint
        sl[["linkingto"]] <- grep("^LinkingTo: *", lines) # nolint
        sl[["remotes"]] <- grep("^Remotes: *", lines) # nolint
        sl[["colon"]] <- grep(": *", lines) # nolint

        which <-
          paste0(toupper(substr(which, 1, 1)), substr(which, 2, 1e4))
        which <- unique(which)
        whichLower <- tolower(which)
        needed <-
          Map(whLower = whichLower, wh = which, function(whLower, wh) {
            if (length(sl[[whLower]])) {
              whEnd <- which(sl[["colon"]] %in% sl[[whLower]]) + 1
              whEnd <- if (length(sl[["colon"]]) < whEnd) {
                length(lines)
              } else {
                (sl[["colon"]][whEnd] - 1)
              }
              allLines <- sl[[whLower]]:whEnd # nolint
              needs <- paste(lines[allLines], collapse = "")
              needs <- gsub(paste0(wh, ": *"), "", needs)
              needs <- strsplit(needs, split = ", *")
              needs <-
                gsub(" *$", "", needs[[1]]) # remove trailing spaces
              if (isFALSE(keepVersionNumber)) {
                needs <- trimVersionNumber(needs)
              }
              needs
            }
          })
        if (length(objName) == 1) {
          assign(objName, needed, envir = .pkgEnv[["pkgDep"]][["DESCRIPTIONFile"]])
        }
      }
    } else {
      needed <-
        get(objName, envir = .pkgEnv[["pkgDep"]][["DESCRIPTIONFile"]])
    }
    if (!keepSeparate) {
      needed <- unname(unlist(needed))
      needed <- grep("^R[\\( ]", needed, value = TRUE, invert = TRUE)
    }
    needed
  }

DESCRIPTIONFile <- function(desc_path) {
  lines <- readLines(desc_path)
}

whichToDILES <- function(which) {
  if (identical("all", which) || is.null(which)) {
    which <-
      list(c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"))
  } else if (identical("most", which)) {
    which <- list(c("Depends", "Imports", "LinkingTo", "Suggests"))
  } else if (isTRUE(which)) {
    which <- list(
      c("Depends", "Imports", "LinkingTo", "Suggests"),
      c("Depends", "Imports", "LinkingTo")
    )
  } else if (any(is.na(which))) {
    which <- list(c("Depends", "Imports", "LinkingTo"))
  } else if (is.character(which)) {
    which <- list(which)
  } else if (isFALSE(which)) {
    which <- character()
  }
  which
}

.installed.pkgs <-
  function(lib.loc = .libPaths(),
           which = c("Depends", "Imports", "LinkingTo"),
           other = NULL,
           purge = getOption("Require.purge", FALSE)) {
    purge <- dealWithCache(purge)
    if (!is.null(other)) {
      if (any(grepl("github", tolower(other)))) {
        other <-
          c("GithubRepo", "GithubUsername", "GithubRef", "GithubSHA1")
      }
    }

    out <- lapply(lib.loc, function(path) {
      dirs <- dir(path, full.names = TRUE)
      areDirs <- dir.exists(dirs)
      dirs <- dirs[areDirs]
      files <- file.path(dirs, "DESCRIPTION")
      filesExist <- file.exists(files)
      files <- files[filesExist]
      names(files) <- basename(dirs[filesExist])
      desc_lines <-
        lapply(files, function(file) {
          DESCRIPTIONFile(file)
        })
      versions <- DESCRIPTIONFileVersionV(desc_lines, purge = FALSE)
      deps <- if (length(which)) {
        lapply(desc_lines, function(lines) {
          DESCRIPTIONFileDeps(lines, which = which, purge = purge)
        })
      } else {
        NULL
      }
      if (!is.null(other)) {
        names(other) <- other
        others <-
          lapply(other, function(oth) {
            DESCRIPTIONFileOtherV(desc_lines, other = oth)
          })
        # shas <- DESCRIPTIONFileOtherV(desc_lines)
      }
      mat <-
        cbind(
          "Package" = dirs[filesExist],
          "Version" = versions,
          "Depends" = deps
        )
      if (!is.null(other)) {
        others <- lapply(others, function(co) {
          if (!is(co, "character")) {
            as.character(co)
          } else {
            co
          }
        })
        othersDF <- as.data.frame(others, stringsAsFactors = FALSE)
        mat <- cbind(mat, othersDF, stringsAsFactors = FALSE)
      }
      mat
    })
    lengths <- unlist(lapply(out, function(x) {
      NROW(x)
    }))
    out <- do.call(rbind, out)
    ret <-
      cbind(
        "Package" = basename(unlist(out[, "Package"])),
        "LibPath" = rep(lib.loc, lengths),
        "Version" = out[, "Version"]
      )
    if (length(which)) {
      ret <-
        cbind(ret,
          "Dependencies" = out[, "Depends"],
          stringsAsFactors = FALSE
        )
    }
    if (NROW(ret)) {
      if (!is.null(other)) {
        ncolBefore <- NCOL(ret)
        ret <- cbind(ret, out[, other], stringsAsFactors = FALSE)
      }
    }
    ret
  }

.basePkgs <-
  # unlist(.installed.pkgs(tail(.libPaths(), 1))[, "Package"])
  c(
    "base",
    "boot",
    "class",
    "cluster",
    "codetools",
    "compiler",
    "datasets",
    "foreign",
    "graphics",
    "grDevices",
    "grid",
    "KernSmooth",
    "lattice",
    "MASS",
    "Matrix",
    "methods",
    "mgcv",
    "nlme",
    "nnet",
    "parallel",
    "rpart",
    "spatial",
    "splines",
    "stats",
    "stats4",
    "survival",
    "tcltk",
    "tools",
    "translations",
    "utils"
  )

DESCRIPTIONFileDepsV <-
  Vectorize(DESCRIPTIONFileDeps,
    vectorize.args = "desc_path",
    SIMPLIFY = FALSE
  )

#' Package dependencies when one or more packages removed
#'
#' This is primarily for package developers. It allows the testing of what the
#' recursive dependencies would be if a package was removed from the immediate
#' dependencies.
#' @param pkg A package name to be testing the dependencies
#' @param depsRemoved A vector of package names who are to be "removed" from the
#'   `pkg` immediate dependencies
#' @export
#' @return A list with 3 named lists `Direct`, `Recursive` and `IfRemoved`.
#' `Direct` will show the top level direct dependencies, either `Remaining` or
#' `Removed`. `Recursive` will show the full recursive dependencies, either
#' `Remaining` or `Removed`. `IfRemoved` returns all package dependencies that
#' are removed for each top level dependency. If a top level dependency is not
#' listed in this final list, then it means that it is also a recursive
#' dependency elsewhere, so its removal has no effect.
#' @inheritParams Require
#' @examples
#' \dontrun{
#' if (Require:::.runLongExamples()) {
#'   opts <- Require:::.setupExample()
#'
#'   pkgDepIfDepRemoved("reproducible", "data.table")
#'
#'   Require:::.cleanup(opts)
#' }
#' }
#'
pkgDepIfDepRemoved <-
  function(pkg = character(),
           depsRemoved = character(),
           verbose = getOption()) {
    if (length(pkg)) {
      p2 <- pkgDep2(pkg, recursive = TRUE)
      p2 <-
        Map(p = p2, nam = names(p2), function(p, nam) {
          setdiff(p, nam)
        })
      names(p2) <- extractPkgName(names(p2))
      p1 <- names(p2) # just the immediate
      p3 <- p2
      p2All <-
        unique(extractPkgName(c(names(p2), unname(unlist(
          p2
        )))))
      p3[depsRemoved] <- NULL
      p3All <-
        unique(extractPkgName(c(names(p3), unname(unlist(
          p3
        )))))
      removed <- sort(setdiff(p2All, p3All))
      left <- sort(p3All)

      removedDirect <- intersect(p1, depsRemoved)
      allInRecursive <- unique(extractPkgName(unname(unlist(p2))))
      direct <- p1
      bestToRemove <- setdiff(direct, allInRecursive)
      bestToRemove <- p2[bestToRemove]
      bestToRemove <-
        Map(
          btr = bestToRemove, top = names(bestToRemove),
          function(btr, top) {
            p4 <- p2
            p4[top] <- NULL
            setdiff(
              extractPkgName(unique(c(
                names(p4), unname(unlist(p4))
              ))),
              extractPkgName(c(top, btr))
            )
          }
        )
      leftDirect <- names(p3)
      pkg <-
        list(
          Direct = list(
            Remaining = leftDirect,
            Removed = removedDirect,
            NRemaining = length(leftDirect),
            NRemoved = length(removedDirect)
          ),
          Recursive = list(
            "Remaining" = left,
            "Removed" = removed,
            NRemaining = length(left),
            NRemoved = length(removed)
          ),
          IfRemoved = bestToRemove
        )
    }
    return(pkg)
  }

checkCircular <- function(aa) {
  circular <- lapply(names(aa), function(outer) {
    circularWh <- extractPkgName(aa[[outer]]) %in% extractPkgName(outer)
    circ <- if (any(circularWh)) {
      aa[[outer]][circularWh]
    } else {
      NULL
    }
  })

  if (length(unlist(circular))) {
    for (i in which(!unlist(lapply(circular, is.null)))) {
      aa[[i]] <- setdiff(aa[[i]], circular[[i]])
    }
  }
  aa
}

#' @inheritParams Require
getGitHubDeps <-
  function(pkg,
           pkgDT,
           which,
           purge,
           verbose = getOption("Require.verbose"),
           includeBase = FALSE) {
    pkg <- masterMainToHead(pkg)

    localVersionOK <- installedVersionOKPrecise(pkgDT)

    if (isTRUE(localVersionOK)) {
      pkgDT[, DESCFile := system.file("DESCRIPTION", package = pkgDT$Package)]
    } else {
      pkgDT <- getGitHubDESCRIPTION(pkgDT, purge = purge)
    }
    hasVersionNum <- grep(grepExtractPkgs, pkgDT$packageFullName)
    set(pkgDT, NULL, "availableVersionOK", NA)
    if (length(hasVersionNum)) {
      VersionOnRepos <-
        DESCRIPTIONFileVersionV(pkgDT$DESCFile, purge = purge)
      pkgDT[hasVersionNum, versionSpec := extractVersionNumber(packageFullName)]
      pkgDT[hasVersionNum, inequality := extractInequality(packageFullName)]
      pkgDT[hasVersionNum, availableVersionOK := compareVersion2(VersionOnRepos, pkgDT$versionSpec, pkgDT$inequality)]
    }

    if (any(!pkgDT$availableVersionOK %in% FALSE)) {
      needed <-
        try(DESCRIPTIONFileDeps(pkgDT$DESCFile, which = which, purge = purge))
      if (is(needed, "try-error")) {
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
      neededRemotes <-
        DESCRIPTIONFileDeps(pkgDT$DESCFile, which = "Remotes", purge = purge)
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
      if (isTRUE(localVersionOK)) {
        namespaceFile <-
          system.file("NAMESPACE", package = pkgDT$Package)
      } else {
        namespaceFile <-
          getGitHubNamespace(pkgDT$packageFullName)$NAMESPACE
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

        pkgDT2 <-
          data.table(packageFullName = setdiff(union(needed, depsFromNamespace), bp))
        if (NROW(pkgDT2)) {
          pkgDT2[, isGitPkg := grepl("^.+/(.+)@+.*$", packageFullName)]
          setorderv(pkgDT2, "isGitPkg", order = -1)
          pkgDT2[, Package := extractPkgName(packageFullName)]

          # Here, GitHub package specification in a DESCRIPTION file Remotes section
          #   won't have version numbering --> Need to merge the two fields
          pkgDT2[, Version := extractVersionNumber(packageFullName)]
          if (any(!is.na(pkgDT2$Version))) {
            pkgDT2[!is.na(Version), inequality := extractInequality(packageFullName)]
            pkgDT2[, Version := {
              if (all(is.na(Version))) {
                NA_character_
              } else {
                as.character(max(as.package_version(Version[!is.na(Version)])))
              }
            }, by = "Package"]
            pkgDT2[, inequality := {
              if (all(is.na(inequality))) {
                NA_character_
              } else {
                inequality[!is.na(inequality)][[1]]
              }
            }, by = "Package"]
            pkgDT2[, github := extractPkgGitHub(packageFullName)]
            if (any(pkgDT2$isGitPkg == TRUE &
              !is.na(pkgDT2$Version))) {
              pkgDT2[isGitPkg == TRUE & !is.na(Version), newPackageFullName :=
                ifelse(
                  is.na(extractVersionNumber(packageFullName)),
                  paste0(packageFullName, " (", inequality, Version, ")"),
                  NA
                )]
              whGitNeedVersion <- !is.na(pkgDT2$newPackageFullName)
              if (any(whGitNeedVersion)) {
                pkgDT2[whGitNeedVersion == TRUE, packageFullName := newPackageFullName]
              }
            }
          }
          dup <- duplicated(pkgDT2, by = c("Package", "Version"))
          pkgDT2 <- pkgDT2[dup == FALSE]
          differences <-
            setdiff(pkgDT2$Package, extractPkgName(needed))
          if (length(differences)) {
            messageVerbose(
              " (-- The DESCRIPTION file for ",
              pkg,
              " is incomplete; there are missing imports:\n",
              paste(differences, collapse = ", "),
              " --) ",
              verbose = verbose,
              verboseLevel = 1
            )
          }
        }
        needed <- pkgDT2$packageFullName
      }
    } else {
      needed <- character() # ""#NA_character_
    }
    needed
  }

purgeBasedOnTimeSinceCached <- function(savedTime) {
  purgeDiff <-
    as.numeric(Sys.getenv("R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE"))
  if (is.null(savedTime)) {
    purge <- TRUE
  } else {
    purgeDiff <-
      if (identical(purgeDiff, "") ||
          is.na(purgeDiff)) {
        defaultCacheAgeForPurge
      } else {
        purgeDiff
      }
    purge <-
      purgeDiff < as.numeric(difftime(Sys.time(), savedTime, units = "sec"))
  }
  purge
}

defaultCacheAgeForPurge <- 3600

dealWithCache <- function(purge,
                          checkAge = TRUE,
                          repos = getOption("repos")) {
  if (isTRUE(getOption("Require.offlineMode", FALSE))) {
    purge <- FALSE
    checkAge <- FALSE
  }

  if (!isTRUE(purge) && isTRUE(checkAge)) {
    # purgeDiff <-
    #   as.numeric(Sys.getenv("R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE"))
    # if (is.null(.pkgEnv[["startTime"]])) {
    #   purge <- FALSE
    # } else {
    #   purgeDiff <-
    #     if (identical(purgeDiff, "") ||
    #         is.na(purgeDiff)) {
    #       3600
    #     } else {
    #       purgeDiff
    #     }
    #   autoPurge <-
    #     purgeDiff < as.numeric(difftime(Sys.time(), .pkgEnv[["startTime"]], units = "sec"))
    #   purge <- purge || autoPurge
    # }
    purgeBasedOnTime <- purgeBasedOnTimeSinceCached(.pkgEnv[["startTime"]])
    purge <- purge || purgeBasedOnTime
  }

  if (isTRUE(purge) || is.null(.pkgEnv[["pkgDep"]])) {
    .pkgEnv[["pkgDep"]] <- newPkgDepEnv()
    .pkgEnv[["startTime"]] <- Sys.time()
  }
  if (isTRUE(purge)) {

    # getSHAFromGItHubMemoise
    SHAfile <- getSHAFromGitHubDBFilename()
    if (isTRUE(file.exists(SHAfile)))
      unlink(SHAfile)

    fn <-
      availablePackagesCachedPath(repos = repos, type = c("source", "binary"))
    fExists <- file.exists(fn)
    if (any(fExists)) {
      unlink(fn[fExists])
    }

    # This is for pkgDep
    fn <- pkgDepDBFilename()
    unlink(fn)
  }

  if (exists(getSHAfromGitHubObjName, envir = .pkgEnv, inherits = FALSE) && purge)
    rm(list = getSHAfromGitHubObjName, envir = .pkgEnv)
  if (is.null(.pkgEnv[["pkgDep"]][["deps"]]) ||
      purge) {
    .pkgEnv[["pkgDep"]][["deps"]] <- new.env(parent = emptyenv())
  }
  if (is.null(.pkgEnv[["pkgDep"]][["DESCRIPTIONFile"]]) || purge) {
    .pkgEnv[["pkgDep"]][["DESCRIPTIONFile"]] <-
      new.env(parent = emptyenv())
  }

  purge
}

.grepTooManySpaces <- " {2,}"
.grepTabCR <- "\n|\t"

newPkgDepEnv <- function() {
  new.env(parent = emptyenv())
}

masterMainToHead <- function(gitRepo) {
  masterOrMain <- "@(master|main) *"
  areMasterOrMain <- grepl(masterOrMain, gitRepo)
  if (any(areMasterOrMain)) {
    masterOrMainNoSpace <- "@(master|main)"
    gitRepo[areMasterOrMain] <-
      gsub(paste0("(", masterOrMainNoSpace, ")"), "@HEAD", gitRepo[areMasterOrMain])
  }
  return(gitRepo)
}

################################################################################
#' Convert numeric to character with padding
#'
#' This will pad floating point numbers, right or left. For integers, either class
#' integer or functionally integer (e.g., 1.0), it will not pad right of the decimal.
#' For more specific control or to get exact padding right and left of decimal,
#' try the \code{stringi} package. It will also not do any rounding. See examples.
#'
#' @param x numeric. Number to be converted to character with padding
#'
#' @param padL numeric. Desired number of digits on left side of decimal.
#'              If not enough, \code{pad} will be used to pad.
#'
#' @param padR numeric. Desired number of digits on right side of decimal.
#'              If not enough, \code{pad} will be used to pad.
#'
#' @param pad character to use as padding (\code{nchar(pad) == 1} must be \code{TRUE}).
#'   Currently, can be only `"0"` or `" "` (i.e., space).
#'
#' @return Character string representing the filename.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @rdname paddedFloatToChar
#'
#' @examples
#' paddedFloatToChar(1.25)
#' paddedFloatToChar(1.25, padL = 3, padR = 5)
#' paddedFloatToChar(1.25, padL = 3, padR = 1) # no rounding, so keeps 2 right of decimal
paddedFloatToChar <-
  function(x,
           padL = ceiling(log10(x + 1)),
           padR = 3,
           pad = "0") {
    if (!pad %in% c("0", " ")) {
      stop("pad must be either '0' or ' '")
    }
    xf <- x %% 1
    numDecimals <- nchar(gsub("(.*)(\\.)|([0]*$)", "", xf))

    newPadR <-
      ifelse(abs(xf - 0) < sqrt(.Machine$double.eps),
        0,
        pmax(numDecimals, padR)
      )
    string <-
      paste0(
        paste0("%", pad),
        padL + newPadR + 1 * (newPadR > 0),
        ".",
        newPadR,
        "f"
      )
    xFCEnd <- sprintf(string, x)
    return(xFCEnd)
  }


saveNamesForCache <- function(packages, which, recursive, ap, verbose) {
  isGH <- isGitHub(packages)
  if (any(isGH)) {
    pkgDT <- parseGitHub(packages[isGH])
    pkgDT <- installedVersionOKPrecise(pkgDT)
    pkgDT <- parsePackageFullname(pkgDT, sorted = FALSE) # this sorted previously; now no
    # pkgDT <- whichToInstall(pkgDT, install = TRUE)

    installedOK <- pkgDT$installedVersionOK
    installedNotOK <- !installedOK
    shas <- character(NROW(pkgDT))
    if (isTRUE(any(installedNotOK))) {
      for (i in 1:2) {
        brLocals <- if (i == 1) pkgDT$Branch[installedNotOK] else "main"
        shaOuts <- try(
          Map(
            repo = pkgDT$Repo[installedNotOK],
            acct = pkgDT$Account[installedNotOK],
            br = brLocals,
            verbose = verbose,
            getSHAfromGitHubMemoise
          )
        )
        if (all(!is(shaOuts, "try-error"))) {
          if (i == 2)
            warning(pkgDT[installedNotOK]$packageFullName,
                    " could not be found. Was the branch deleted? ",
                    "Assessing package dependencies in ",
                    "main branch of that/those repository/repositories. ")
          shas[installedNotOK] <- shaOuts
          break
        }
      }
    }

    if (isTRUE(any(installedOK))) {
      withCallingHandlers(
        shas[installedOK] <-
          DESCRIPTIONFileOtherV(
            file.path(pkgDT$LibPath[installedOK], pkgDT$Package[installedOK], "DESCRIPTION"),
            other = "GithubSHA1"),
        warning = function(w) {
          warning(w)
          browserDeveloper("Error 44322; please contact developer")
          invokeRestart("muffleWarning")
        })
    }

    names(shas) <- pkgDT$packageFullName # need to keep order correct
  }

  hasIneq <- grepl("==|>|<", packages[!isGH])
  inequ <- extractInequality(packages[!isGH][hasIneq])
  hasNoEquality <- grep("^(==)", inequ, invert = TRUE)
  packagesSaveNames <- packages

  if (any(isGH)) {
    hasAt <- grepl("@", packagesSaveNames[isGH])
    if (any(hasAt %in% FALSE)) {
      packagesSaveNames[isGH][hasAt %in% FALSE] <-
        gsub(
          "(.+/.+)( \\()|(\\()(.+\\))",
          "\\1@HEAD (\\4",
          packagesSaveNames[isGH][hasAt %in% FALSE]
        )
    }
    psnSplits <- strsplit(packagesSaveNames[isGH], "@")
    packagesSaveNamesGH <-
      mapply(psnSplit = psnSplits, sha = shas, function(psnSplit, sha) {
        paste0(psnSplit[1], "@", sha)
      }, SIMPLIFY = TRUE)
    packagesSaveNames[isGH] <- packagesSaveNamesGH
  }

  if (sum(hasIneq)) {
    verNum <- extractVersionNumber(packagesSaveNames[!isGH][hasIneq])
    # hasIneq <- !is.na(inequ)
    psnNoVersion <- trimVersionNumber(packagesSaveNames[!isGH][hasIneq])
    packagesSaveNames[!isGH][hasIneq] <- psnNoVersion
    versions <- ap$Version[match(packagesSaveNames[!isGH][hasIneq], ap$Package)]
    if (any(hasIneq)) {
      okVers <- compareVersion2(versions, inequality = inequ, verNum)
      #if (all(!is.na(okVers))) {
      if (any(okVers %in% TRUE)) {
        out <- try(packagesSaveNames[!isGH][hasIneq][okVers %in% TRUE] <-
          paste0(
            packagesSaveNames[!isGH][hasIneq][okVers %in% TRUE], " (==",
            versions[okVers %in% TRUE],
            ")"
          ))
        if (is(out, "try-error")) browserDeveloper("Error 7788; please contact developer")
      }
      if (any(okVers %in% FALSE))
        packagesSaveNames[!isGH][hasIneq][okVers %in% FALSE] <-
          paste0(
            packagesSaveNames[!isGH][hasIneq][okVers %in% FALSE], " (==",
            verNum[okVers %in% FALSE],
            ")"
          )
    }
    #}
  }
  noIneq <- hasIneq %in% FALSE
  if (any(noIneq)) {
    versions <- ap$Version[match(packagesSaveNames[!isGH][noIneq], ap$Package)]
    # if versions has NA then it means it is not on CRAN; maybe other causes as well
    packagesSaveNames[!isGH][noIneq] <-
      paste0(
        packagesSaveNames[!isGH][noIneq], " (==",
        versions,
        ")"
      )
  }


  whichCat <- paste(sort(which[[1]]), collapse = "_")
  saveNames <-
    paste(packagesSaveNames,
      paste(whichCat, "recursive", recursive, sep = "_")[1],
      sep = "_"
    )
  saveNames <- gsub("[[:punct:]]| ", "_", saveNames)
  names(saveNames) <- packagesSaveNames
  saveNames
}

pkgDepCRANMemoise <- function(...) {
  if (getOption("Require.useMemoise", TRUE)) {
    dots <- list(...)
    if (!exists("pkgDepCRAN", envir = .pkgEnv, inherits = FALSE)) {
      .pkgEnv$pkgDepCRAN <- new.env()
    }
    ret <- NULL
    if (!exists(dots$pkgsNoVersion,
      envir = .pkgEnv$pkgDepCRAN,
      inherits = FALSE
    )) {
      .pkgEnv$pkgDepCRAN[[dots$pkgsNoVersion]] <- list()
    } else {
      whIdent <-
        unlist(lapply(.pkgEnv$pkgDepCRAN[[dots$pkgsNoVersion]], function(x) {
          identical(x$input, dots)
        }))
      if (any(whIdent)) {
        ret <-
          .pkgEnv$pkgDepCRAN[[dots$pkgsNoVersion]][[which(whIdent)]]$output
      }
    }
    if (is.null(ret)) {
      ret <- pkgDepCRAN(...)
      .pkgEnv$pkgDepCRAN[[dots$pkgsNoVersion]] <-
        list(.pkgEnv$pkgDepCRAN[[dots$pkgsNoVersion]], list(input = dots, output = ret))
    }
  } else {
    ret <- pkgDepCRAN(...)
  }

  return(ret)
}

pkgDepInnerMemoise <- function(...) {
  if (getOption("Require.useMemoise", TRUE)) {
    dots <- list(...)
    if (isTRUE(dots$purge) &&
      exists("pkgDepInner", envir = .pkgEnv)) {
      rm(list = "pkgDepInner", envir = .pkgEnv)
    }
    if (!exists("pkgDepInner", envir = .pkgEnv, inherits = FALSE)) {
      .pkgEnv$pkgDepInner <- new.env()
    }
    ret <- NULL
    ss <- match.call(definition = pkgDepInner)
    packages <- eval(ss$packages, envir = parent.frame())
    if (length(packages) > 1) {
      dots2 <- dots
      dots2[[1]] <- NULL
      dots2 <- modifyList2(dots2, list(purge = FALSE))
      if (!is.null(names(packages))) {
        names(packages) <- NULL
      }
      ret <-
        lapply(packages, function(p) {
          do.call(pkgDepInnerMemoise, append(list(packages = p), dots2))
        })
      ret <- unlist(ret, recursive = FALSE)
    } else {
      if (!exists(packages,
        envir = .pkgEnv$pkgDepInner,
        inherits = FALSE
      )) {
        .pkgEnv$pkgDepInner[[packages]] <- list()
      } else {
        whIdent <-
          unlist(lapply(.pkgEnv$pkgDepInner[[packages]], function(x) {
            identical(x$input, dots)
          }))
        if (any(whIdent)) {
          ret <-
            .pkgEnv$pkgDepInner[[packages]][[which(whIdent)]]$output
        }
      }
      if (is.null(ret)) {
        ret <- pkgDepInner(...)
        .pkgEnv$pkgDepInner[[packages]] <-
          list(.pkgEnv$pkgDepInner[[packages]], list(input = dots, output = ret))
      }
    }
  } else {
    ret <- pkgDepInner(...)
  }

  return(ret)
}

getGitHubDepsMemoise <- function(...) {
  if (getOption("Require.useMemoise", TRUE)) {
    dots <- list(...)
    if (!exists("getGitHubDeps", envir = .pkgEnv, inherits = FALSE)) {
      .pkgEnv$getGitHubDeps <- new.env()
    }
    ret <- NULL
    ss <- match.call(definition = getGitHubDeps)
    pkg <- eval(ss$pkg, envir = parent.frame())
    if (!exists(pkg, envir = .pkgEnv$getGitHubDeps, inherits = FALSE)) {
      .pkgEnv$getGitHubDeps[[pkg]] <- list()
    } else {
      whIdent <-
        unlist(lapply(.pkgEnv$getGitHubDeps[[pkg]], function(x) {
          identical(x$input, dots)
        }))
      if (any(whIdent)) {
        ret <- .pkgEnv$getGitHubDeps[[pkg]][[which(whIdent)]]$output
      }
    }
    if (is.null(ret)) {
      inputs <- data.table::copy(dots)
      ret <- getGitHubDeps(...)
      .pkgEnv$getGitHubDeps[[pkg]] <-
        list(.pkgEnv$getGitHubDeps[[pkg]], list(input = inputs, output = ret))
    }
  } else {
    ret <- getGitHubDeps(...)
  }

  return(ret)
}

pkgDepTopoSortMemoise <- function(...) {
  if (getOption("Require.useMemoise", TRUE)) {
    dots <- list(...)
    fnName <- "pkgDepTopoSort"
    fn <- eval(parse(text = fnName))
    if (!exists(fnName, envir = .pkgEnv, inherits = FALSE)) {
      .pkgEnv[[fnName]] <- new.env()
    }
    ret <- NULL
    ss <- match.call(definition = fn)
    pkg <- eval(ss$pkg, envir = parent.frame())
    hash <-
      sum(as.integer(serialize(
        object = pkg, ascii = T, NULL
      )))
    hash <- as.character(hash)
    if (!exists(hash, envir = .pkgEnv[[fnName]], inherits = FALSE)) {
      .pkgEnv[[fnName]][[hash]] <- list()
    } else {
      whIdent <-
        unlist(lapply(.pkgEnv[[fnName]][[hash]], function(x) {
          identical(x$input, dots)
        }))
      if (any(whIdent)) {
        ret <- .pkgEnv[[fnName]][[hash]][[which(whIdent)]]$output
      }
    }
    if (is.null(ret)) {
      inputs <- data.table::copy(dots)
      ret <- fn(...)
      .pkgEnv[[fnName]][[hash]] <-
        list(.pkgEnv[[fnName]][[hash]], list(input = inputs, output = ret))
    }
  } else {
    ret <- fn(...)
  }

  return(ret)
}

pkgDepDBFilename <- function() {
  if (!is.null(getOptionRPackageCache())) {
    file.path(RequirePkgCacheDir(), "pkgDepDB.rds")
  } # returns NULL if no Cache used
}

isAre <- function(l, v) {
  isare <- c("is", "are")
  if (!missing(l)) {
    out <- isare[(length(l) > 1) + 1]
  }
  if (!missing(v)) {
    out <- isare[(v > 1) + 1]
  }
  out
}

prependSelf <- function(deps, includeSelf) {
  if (isTRUE(includeSelf)) {
    deps <- Map(pkgs = deps, nam = names(deps), function(pkgs, nam) {
      depsInner <- pkgs
      alreadyHasSelf <- identical(extractPkgName(pkgs[1]), extractPkgName(nam))
      # alreadyHasSelf <- startsWith(pkgs[1], trimVersionNumber(nam))
      if (!isTRUE(alreadyHasSelf)) {
        depsInner <- c(nam, pkgs)
      }

      return(depsInner)
    })
  }
  deps
}

getAvailablePackagesIfNeeded <-
  function(packages, repos, purge, verbose, type) {
    isCRAN <- parseGitHub(packages)[["repoLocation"]] %in% "CRAN"
    if (any(isCRAN %in% TRUE)) {
      ap <-
        available.packagesCached(
          repos = repos,
          purge = purge,
          verbose = verbose,
          type = type
        )
    } else {
      ap <- NULL
    }
    ap
  }

#' Clear Require Cache elements
#'
#' @param packages Either missing or a character vector of package names
#'   (currently cannot specify version number) to remove from the local Require
#'   Cache.
#' @param ask Logical. If `TRUE`, then it will ask user to confirm
#' @param Rversion An R version (major dot minor, e.g., "4.2"). Defaults to
#'   current R version.
#' @param clearCranCache Logical. If `TRUE`, then this will also clear the
#'   local `crancache` cache, which is only relevant if
#'   `options(Require.useCranCache = TRUE)`, i.e., if `Require` is using the
#'   `crancache` cache also
#' @export
#' @inheritParams Require
#' @rdname clearRequire
clearRequirePackageCache <- function(packages,
                                     ask = interactive(),
                                     Rversion = rversion(),
                                     clearCranCache = FALSE,
                                     verbose = getOption("Require.verbose")) {
  out <- RequirePkgCacheDir(create = FALSE)
  if (!identical(Rversion, rversion())) {
    out <- file.path(dirname(out), Rversion)
  }
  if (getOption("Require.useCranCache")) {
    crancache <- crancacheFolder()
    toDelete <- dir(crancache, recursive = TRUE, full.names = TRUE)
    if (length(toDelete) && isFALSE(clearCranCache)) {
      messageVerbose(blue(
        "crancache is being used because options(Require.useCranCache = TRUE); ",
        "however, clearCranCache is FALSE. This means that packages from ",
        "crancache will continue to re-populate the Require Cache. ",
        "To remove all local packages, set clearCranCache in this ",
        "function to TRUE"
      ))
    }
    if (isTRUE(clearCranCache)) {
      if (!missing(packages)) {
        pkgNamesInFiles <- extractPkgName(filenames = basename(toDelete))
        present <- pkgNamesInFiles %in% packages
        toDelete <- toDelete[present]
      }
      if (length(toDelete)) {
        unlink(toDelete)
      }
    }
  }
  proceed <- TRUE
  indivFiles <- dir(out, full.names = TRUE)
  isFile <- !dir.exists(indivFiles)
  indivFiles <- indivFiles[isFile]
  if (missing(packages)) {
    toDelete <-
      indivFiles # don't delete whole dir because has available.packages too; not to delete
    forMess <-
      paste0("all ", length(indivFiles), " cached packages in ", out)
  } else {
    if (length(indivFiles)) {
      pkgNamesInFiles <- extractPkgName(filenames = basename(indivFiles))
      toDelete <- indivFiles[pkgNamesInFiles %in% packages]
      forMess <- paste(sort(basename(toDelete)), collapse = ",\n")
    } else {
      toDelete <- character()
    }
  }
  if (length(toDelete)) {
    if (isTRUE(ask)) {
      message("Are you sure you would like to remove\n", forMess, "\n?")
      askResult <- readline("(n or anything else for yes) ")
      if (startsWith(tolower(askResult), "n")) {
        proceed <- FALSE
      }
    }

    if (isTRUE(proceed)) {
      messageVerbose("Clearing: \n",
                     forMess,
                     verbose = verbose,
                     verboseLevel = 1
      )
      unlink(toDelete, recursive = TRUE)

      # This purges all the pkgDep stuff
      SHAfile1 <- getSHAFromGitHubDBFilename()
      if (isTRUE(file.exists(SHAfile1))) # can be character() ifgetOptionRPackageCache is NULL/FALSE
        unlink(SHAfile1)

    } else {
      message("Aborting")
    }
  } else {
    messageVerbose("Nothing to clear in Cache", verbose = verbose, verboseLevel = 1)
  }
}


depsImpsSugsLinksToWhich <- function(depends, imports, suggests, linkingTo, which) {
  if (!missing(depends)) {
    wh <-
      "Depends"
    which <- if (isTRUE(depends)) {
      unique(c(which, wh))
    } else {
      setdiff(which, wh)
    }
  }
  if (!missing(imports)) {
    wh <-
      "Imports"
    which <- if (isTRUE(imports)) {
      unique(c(which, wh))
    } else {
      setdiff(which, wh)
    }
  }
  if (!missing(suggests)) {
    wh <-
      "Suggests"
    which <- if (isTRUE(suggests)) {
      unique(c(which, wh))
    } else {
      setdiff(which, wh)
    }
  }
  if (!missing(linkingTo)) {
    wh <-
      "LinkingTo"
    which <- if (isTRUE(linkingTo)) {
      unique(c(which, wh))
    } else {
      setdiff(which, wh)
    }
  }
  whichToDILES(which)
}


installedVersionOKPrecise <- function(pkgDT) {
  pkgDT[, localFiles := system.file("DESCRIPTION", package = Package), by = "Package"]
  fe <- nzchar(pkgDT$localFiles)
  if (any(fe)) {
    pkgDT[fe, localRepo := DESCRIPTIONFileOtherV(localFiles, "RemoteRepo")]
    pkgDT[fe, localUsername := DESCRIPTIONFileOtherV(localFiles, "RemoteUsername")]
    pkgDT[fe, localBranch := DESCRIPTIONFileOtherV(localFiles, "RemoteRef")]
    pkgDT[fe, localSha := DESCRIPTIONFileOtherV(localFiles, "RemoteSha")]
  } else {
    pkgDT[fe, localRepo := NA]
    pkgDT[fe, localUsername := NA]
    pkgDT[fe, localBranch := NA]
    pkgDT[fe, localSha := NA]
  }

  pkgDT <- installedVers(pkgDT)

  if (isTRUE(any(pkgDT$installed %in% TRUE))) {
    brOrSha <- pkgDT$Branch == pkgDT$localBranch |
      pkgDT$Branch == pkgDT$localSha

    installedNotOK <- pkgDT$Account != pkgDT$localUsername |
      pkgDT$Repo != pkgDT$localRepo |
      brOrSha %in% FALSE

  } else {
    installedNotOK <- NA
  }
  set(pkgDT, NULL, "installedVersionOK", installedNotOK %in% FALSE)
  pkgDT
}
