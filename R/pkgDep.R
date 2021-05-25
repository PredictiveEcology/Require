utils::globalVariables(c(
  "PackageTrimmed", "hasVers", "atLeastOneWithVersionSpec", "maxVersionSpec", "Current",
  "GithubRepo", "GithubSHA1", "GithubUsername", "mtime", "newMtime", "newPackageFullName"
  #, "..keepCols3"
))

#' Determine package dependencies
#'
#' This will first look in local filesystem (in \code{.libPaths()}) and will use
#' a local package to find its dependencies. If the package does not exist locally,
#' including whether it is the correct version, then it will look in (currently)
#' \code{CRAN} and its archives (if the current \code{CRAN} version is not
#' the desired version to check). It will also look on \code{GitHub} if the
#' package description is of the form of a GitHub package with format
#' \code{account/repo@branch} or \code{account/repo@commit}. For this,
#' it will attempt to get package dependencies from
#' the GitHub \file{DESCRIPTION} file.
#' This is intended to replace \code{tools::package_dependencies} or
#' \code{pkgDep} in the \pkg{miniCRAN} package, but with modifications to allow
#' multiple sources to be searched in the same function call.
#'
#' @note \code{tools::package_dependencies} and \code{pkgDep} will differ under the following
#' circumstances:
#' \enumerate{
#'   \item GitHub packages are not detected using \code{tools::package_dependencies};
#'   \item \code{tools::package_dependencies} does not detect the dependencies of base packages
#'     among themselves, \emph{e.g.}, \code{methods} depends on \code{stats} and \code{graphics}.
#' }
#'
#' @inheritParams Require
#' @param which a character vector listing the types of dependencies, a subset of
#'   \code{c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")}.
#'   Character string \code{"all"} is shorthand for that vector,
#'   character string \code{"most"} for the same vector without \code{"Enhances"}.
#' @param depends Logical. Include packages listed in "Depends". Default \code{TRUE}.
#' @param imports Logical. Include packages listed in "Imports". Default \code{TRUE}.
#' @param suggests Logical. Include packages listed in "Suggests". Default \code{FALSE}.
#' @param enhances Logical. Include packages listed in "Enhances". Default \code{FALSE}.
#' @param remotes Logical. Include packages listed in "Remotes". This is only relevant for GitHub packages.
#'   Default \code{TRUE}.
#' @param linkingTo Logical. Include packages listed in "LinkingTo". Default \code{TRUE}.
#' @param recursive Logical. Should dependencies of dependencies be searched, recursively.
#'                  NOTE: Dependencies of suggests will not be recursive. Default \code{TRUE}.
#' @param keepVersionNumber Logical. If \code{TRUE}, then the package dependencies returned
#'   will include version number. Default is \code{FALSE}
#' @param libPath A path to search for installed packages. Defaults to \code{.libPaths()}
#' @param sort Logical. If \code{TRUE}, the default, then the packages will be sorted alphabetically.
#'        If \code{FALSE}, the packages will not have a discernible order as they will be a
#'        concatenation of the possibly recursive package dependencies.
#' @param includeBase Logical. Should R base packages be included, specifically, those in
#'   \code{tail(.libPath(), 1)}
#'
#' @export
#' @rdname pkgDep
#'
#' @examples
#' \dontrun{
#'   pkgDep("Require")
#'   pkgDep("Require", keepVersionNumber = FALSE) # just names
#'   pkgDep("PredictiveEcology/reproducible") # GitHub
#'   pkgDep("PredictiveEcology/reproducible", recursive = TRUE) # GitHub
#'   pkgDep(c("PredictiveEcology/reproducible", "Require")) # GitHub package and local packages
#'   pkgDep(c("PredictiveEcology/reproducible", "Require", "plyr")) # GitHub, local, and CRAN packages
#' }
pkgDep <- function(packages, libPath = .libPaths(),
                   which = c("Depends", "Imports", "LinkingTo"), recursive = FALSE,
                   depends, imports, suggests, linkingTo,
                   repos = getOption("repos"),
                   keepVersionNumber = TRUE, includeBase = FALSE,
                   sort = TRUE, purge = getOption("Require.purge", FALSE)) {

  purge <- dealWithCache(purge)

  if (!includeBase) packages <- packages[!packages %in% .basePkgs]
  #if (any(!missing(depends), !missing(linkingTo), !missing(imports), !missing(suggests))) {
  if (!missing(depends)) {wh <- "Depends"; if (isTRUE(depends)) which <- unique(c(which, wh)) else setdiff(which, wh)}
  if (!missing(imports)) {wh <- "Imports"; if (isTRUE(imports)) which <- unique(c(which, wh)) else setdiff(which, wh)}
  if (!missing(suggests)) {wh <- "Suggests"; if (isTRUE(suggests)) which <- unique(c(which, wh)) else setdiff(which, wh)}
  if (!missing(linkingTo)) {wh <- "LinkingTo"; if (isTRUE(linkingTo)) which <- unique(c(which, wh)) else setdiff(which, wh)}

  which <- whichToDILES(which)

  # Only deal with first one of "which"... deal with second later
  whichCat <- paste(sort(which[[1]]), collapse = "_")
  if (length(packages)) {
    saveNames <- paste(packages, paste(whichCat, "recursive", recursive, sep = "_")[1], sep = "_")
    saveNames <- gsub("[[:punct:]]| ", "_", saveNames)
    names(saveNames) <- packages
    if (isTRUE(purge)) {
      whExist <- unlist(lapply(saveNames, exists, envir = .pkgEnv))
      if (any(whExist))
        suppressWarnings(rm(list = saveNames[whExist], envir = .pkgEnv))
    }
    neededFull1 <- lapply(saveNames, get0, envir = .pkgEnv)
    needGet <- unlist(lapply(neededFull1, is.null))

    if (any(needGet)) {
      neededFull <- pkgDepInner(packages[needGet], libPath, which[[1]], keepVersionNumber,
                                purge = purge, repos = repos)
      purge <- FALSE # whatever it was, it was done in line above
      theNulls <- unlist(lapply(neededFull, function(x) is.null(x) || length(x) == 0))
      neededFull2 <- neededFull[!theNulls]
      if (NROW(neededFull2)) {

        if (recursive) {
          which <- tail(which, 1)[[1]] # take the last of the list of which
          neededFull2 <- lapply(neededFull2, function(needed) {
            i <- 1
            pkgsNew <- list()
            pkgsNew[[i]] <- needed
            while (length(unlist(pkgsNew[[i]])) > 0) {
              i <- i + 1
              pkgsNew[[i]] <- lapply(trimVersionNumber(pkgsNew[[i - 1]]), function(needed) {
                unique(unlist(pkgDepInner(needed, libPath, which, keepVersionNumber,
                                          purge = purge, repos = repos)))
              })
              prevIndices <- 1:(i - 1)
              curPkgs <- unlist(pkgsNew[[i]])
              prevPkgs <- unlist(pkgsNew[prevIndices])
              dt <- data.table(Package = c(prevPkgs, curPkgs),
                               Current = c(rep(FALSE, length(prevPkgs)), rep(TRUE, length(curPkgs))))
              # rdtOrig <- data.table::copy(dt)
              # if (TRUE) {
              #   dt[, PackageTrimmed := extractPkgName(Package)]
              #   dt[, versionSpec := extractVersionNumber(Package)]
              #   dt[, hasVers := !is.na(versionSpec)]
              #   dt[hasVers == TRUE, inequality := extractInequality(Package)]
              #   dt[hasVers == FALSE, versionSpec := NA]
              #   dt[, atLeastOneWithVersionSpec := any(hasVers), by = "PackageTrimmed"]
              #   dt[, Current := all(Current == TRUE), by = "PackageTrimmed"] # don't need to redo depdencies of one that already did it
              #   dt <- dt[!(atLeastOneWithVersionSpec == TRUE & hasVers == FALSE)] # remove cases where no version spec >1 case
              #   keepCols3 <- c("PackageTrimmed", "Package", "Current",
              #                  "hasVers", "inequality", "atLeastOneWithVersionSpec", "versionSpec")
              #
              #   versionSpecNA <- is.na(dt$versionSpec)
              #   dt1 <- dt[versionSpecNA == FALSE, ..keepCols3]
              #   if (NROW(dt1)) {
              #     ord <- order(package_version(dt1$versionSpec), decreasing = TRUE)
              #     dt1 <- dt1[ord]
              #     dt1 <- dt1[!duplicated(dt1$PackageTrimmed)]
              #   }
              #
              #   dt2 <- dt[versionSpecNA]
              #
              #   dt <- rbindlist(list(dt1, dt2), use.names = TRUE, fill = TRUE)
              #   dt3 <- dt[!duplicated(dt$PackageTrimmed)]
              #   dt4 <- dt3[Current == TRUE]
              #   dt4Stable <- data.table::copy(dt4)
              # #} else {
              #dt <- data.table::copy(rdtOrig)

              set(dt, NULL, "PackageTrimmed", extractPkgName(dt$Package))
              set(dt, NULL, "versionSpec", extractVersionNumber(dt$Package))
              set(dt, NULL, "hasVers", !is.na(dt$versionSpec))
              hasV <- dt$hasVers == TRUE
              # set(dt, which(hasV), "inequality", extractInequality(dt$Package[hasV]))
              # set(dt, which(!hasV), "versionSpec", NA)
              # browser(expr = any(duplicated(dt$PackageTrimmed)))
              dt[, `:=`(atLeastOneWithVersionSpec = any(hasVers),
                        Current = all(Current == TRUE)), by = "PackageTrimmed"]
              dt <- dt[!(dt$atLeastOneWithVersionSpec == TRUE & dt$hasVers == FALSE)] # remove cases where no version spec >1 case

              versionSpecNA <- is.na(dt$versionSpec)
              #keepCols3 <- c("PackageTrimmed", "Package", "Current",
              #               "hasVers", #"inequality",
              #               "atLeastOneWithVersionSpec", "versionSpec")
              # keepCols3 <- intersect(colnames(dt), keepCols3)

              #if (length(setdiff(keepCols3, colnames(dt))))
                # dt1 <- dt[versionSpecNA == FALSE, ..keepCols3]
              dt1 <- dt[versionSpecNA == FALSE]

              if (NROW(dt1)) {
                dt1 <- dt1[!duplicated(dt1$PackageTrimmed)]
                ord <- order(package_version(dt1$versionSpec), decreasing = TRUE)
                dt1 <- dt1[ord]
              }
              dt2 <- if (all(versionSpecNA)) dt else dt[versionSpecNA]
              dt <- if (NROW(dt1)) rbindlist(list(dt1, dt2), use.names = TRUE, fill = TRUE) else dt2

              dups <- duplicated(dt$PackageTrimmed)
              dt3 <- if (any(dups)) dt[!dups] else dt

              curTrue <- dt3$Current == TRUE
              dt4 <- if (any(curTrue)) if (all(curTrue)) dt3 else dt3[curTrue] else dt3[0]
              # if (!(identical(dt4[, list(PackageTrimmed, Current, Package, hasVers, atLeastOneWithVersionSpec, versionSpec)],
              #                 dt4Stable[, list(PackageTrimmed, Current, Package, hasVers, atLeastOneWithVersionSpec, versionSpec)])))
              #   browser()
              #dt4 <- rdt4
              #}
              pkgsNew <- list()
              pkgsNew[[i - 1]] <- dt3[dt3$Current == FALSE]$Package
              pkgsNew[[i]] <- dt4$Package
            }
            needed <- unique(unlist(pkgsNew))
          })
        }
      }
      # Remove "R"
      neededFull2 <- append(neededFull2, neededFull[theNulls])
      neededFull2 <- lapply(neededFull2, function(needed) {
        grep(.grepR, needed, value = TRUE, invert = TRUE)
      })

      newOnes <- names(saveNames) %in% names(neededFull)
      Map(sn = saveNames[newOnes], n = names(saveNames)[newOnes], function(sn, n) {
        assign(sn, neededFull2[[n]], envir = .pkgEnv)
      })
      neededFull1 <- append(neededFull1[!needGet], neededFull2)
    }

    if (isTRUE(sort))
      neededFull1 <- lapply(neededFull1, function(x) sort(x))
    if (isFALSE(keepVersionNumber)) {
      neededFull1 <- lapply(neededFull1, trimVersionNumber)
    }
    if (!isTRUE(includeBase)) {
      neededFull1 <- lapply(neededFull1, setdiff, .basePkgs)
    }
  } else {
    neededFull1 <- list()
  }
  neededFull1
}

#' @importFrom utils untar
pkgDepInner <- function(packages, libPath, which, keepVersionNumber,
                        purge = getOption("Require.purge", FALSE),
                        repos = repos, includeBase = FALSE) {
  names(packages) <- packages
  pkgsNoVersion <- extractPkgName(packages)
  if (!isTRUE(includeBase)) {
    isBase <- pkgsNoVersion %in% .basePkgs
    packagesToCheck <- packages[!isBase]
    pkgsNoVersionToCheck <- pkgsNoVersion[!isBase]
    packagesBase <- packages[isBase]
    packagesBase <- lapply(packagesBase, function(x) character())
  } else {
    packagesToCheck <- packages
    pkgsNoVersionToCheck <- pkgsNoVersion
  }

  desc_paths <- getDescPath(packagesToCheck, libPath)
  needed <- Map(desc_path = desc_paths, pkg = packagesToCheck,
                pkgNoVersion = pkgsNoVersionToCheck,
                function(desc_path, pkg, pkgNoVersion) {
    if (!file.exists(desc_path)) {
      pkgDT <- parseGitHub(pkg)
      if ("GitHub" %in% pkgDT$repoLocation) {
        which <- c(which, "Remotes")

        pkgDT <- getGitHubDESCRIPTION(pkgDT, purge = purge)
        needed <- DESCRIPTIONFileDeps(pkgDT$DESCFile, which = which, purge = purge)
        #if (FALSE) {
        # Check NAMESPACE too -- because imperfect DESCRIPTION files
        rr <- readLines(getGitHubNamespace(pkgDT$packageFullName)$NAMESPACE)
        depsFromNamespace <- gsub(", except.*(\\))$", "\\1", rr)
        depsFromNamespace <- unique(gsub("^import.*\\((.+)\\,.*$", "\\1",
                                         grep("^import", depsFromNamespace, value = TRUE)))
        depsFromNamespace <- unique(gsub("^import\\((.+)\\)", "\\1", depsFromNamespace))
        depsFromNamespace <- gsub(",.*", "", depsFromNamespace)
        depsFromNamespace <- gsub("\\\"", "", depsFromNamespace)
        pkgDT2 <- data.table(packageFullName = setdiff(union(depsFromNamespace, needed), .basePkgs))
        # needed <- setdiff(union(depsFromNamespace, needed), .basePkgs)
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
              if (all(is.na(Version))) NA_character_ else as.character(max(as.package_version(Version[!is.na(Version)])))
            }, by = "Package"]
            pkgDT2[, inequality := {
              if (all(is.na(inequality))) NA_character_ else inequality[!is.na(inequality)][[1]]
            }, by = "Package"]
            pkgDT2[, github := extractPkgGitHub(packageFullName)]
            if (any(pkgDT2$isGitPkg == TRUE & !is.na(pkgDT2$Version))) {
              pkgDT2[isGitPkg == TRUE & !is.na(Version), newPackageFullName:=
                       ifelse (is.na(extractVersionNumber(packageFullName)),
                         paste0(packageFullName, " (", inequality, Version, ")"), NA) ]
              whGitNeedVersion <- !is.na(pkgDT2$newPackageFullName)
              if (any(whGitNeedVersion)) {
                pkgDT2[whGitNeedVersion == TRUE, packageFullName := newPackageFullName]
              }
            }
          }
          dup <- duplicated(pkgDT2, by = c("Package", "Version"))
          pkgDT2 <- pkgDT2[dup == FALSE]
          differences <- setdiff(pkgDT2$Package, extractPkgName(needed))
          if (length(differences)) {
            message(" (-- The DESCRIPTION file for ", pkg, " is incomplete; there are missing imports:\n",
                    paste(differences, collapse = ", "), " --) ")
          }

        }
        needed <- pkgDT2$packageFullName
        #}
        #pkgDT2 <- data.table(Package = needed, github = extractPkgGitHub(needed))
        #if ("github" %in% colnames(pkgDT2)) {
        #  setorderv(pkgDT2, "github", na.last = TRUE)
        #}
        #needed <- pkgDT2[!duplicated(extractPkgName(pkgDT2$Package))]$Package
      } else {
        needed <- unique(unname(unlist(pkgDepCRAN(pkg,
                                                  pkgsNoVersion = pkgNoVersion,
                                           which = which,
                                           keepVersionNumber = keepVersionNumber,
                                           purge = purge,
                                           repos = repos))))

        if (is.null(needed)) { # essesntially, failed
          pkgName <- extractPkgName(pkg)
          td <- tempdir2(pkgName)
          packageTD <- file.path(td, pkgName)
          if (!dir.exists(packageTD)) {
            message("available.packages() does not have correct information on package dependencies for ", pkgName,
                    "; downloading tar.gz")
            verNum <- extractVersionNumber(pkg)
            if (is.na(verNum)) {
              ava <- archiveVersionsAvailable(pkgName, repos = repos)
              dt <- if (is(ava, "list"))
                rbindlist(lapply(ava, as.data.table, keep.rownames = "packageURL"))
              else
                as.data.table(ava, keep.rownames = "packageURL")
              packageURL <- if (NROW(dt)) tail(dt$packageURL, 1) else character()

            } else {
              pkgFilename <- paste0(pkgName, "_", verNum, ".tar.gz")
              packageURL <- file.path(pkgName, pkgFilename)
              dt <- numeric()
            }
            if (!is.null(packageURL)) {
              if (endsWith(packageURL, "tar.gz")) {
                srcContrib <- "src/contrib"
                url <- file.path(repos, srcContrib, "/Archive", packageURL)
                url2 <- file.path(repos, srcContrib, basename(packageURL))
                tf <- tempfile()
                haveFile <- suppressWarnings(tryCatch(download.file(url, tf, quiet = TRUE), error = function(x)
                  tryCatch(download.file(url2, tf, quiet = TRUE), error = function(y) FALSE)))
                if (file.exists(tf)) {
                  untar(tarfile = tf, exdir = td)
                  filesToDel <- dir(packageTD, recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
                  filesToDel <- filesToDel[grep("^DESCRIPTION$", basename(filesToDel), invert = TRUE)]
                  unlink(filesToDel, recursive = TRUE)
                }
              }
            }
          }
          needed <- if (dir.exists(packageTD))
            DESCRIPTIONFileDeps(file.path(packageTD, "DESCRIPTION"),
                                which = which, keepVersionNumber = keepVersionNumber,
                                purge = purge)
          else {
            character()
            message(pkg, " dependencies not found on CRAN; perhaps incomplete description? On GitHub?")
          }
        }
        purge <<- FALSE
        needed
      }
    } else {
      needed <- DESCRIPTIONFileDeps(desc_path, which = which, keepVersionNumber = keepVersionNumber)
    }
    needed
  })
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
#' \code{pkgDep2} is a convenience wrapper of \code{pkgDep} that "goes one level in",
#' i.e., the first order dependencies, and runs the \code{pkgDep} on those.
#' @rdname pkgDep
#' @export
#' @param sorted Logical. If \code{TRUE}, the default, the packages will be sorted in
#'   the returned list from most number of dependencies to least.
#' @examples
#' \dontrun{
#'   pkgDep2("Require")
#'   # much bigger one
#'   pkgDep2("reproducible")
#' }
pkgDep2 <- function(packages, recursive = TRUE,
                    which = c("Depends", "Imports", "LinkingTo"),
                    depends, imports, suggests, linkingTo,
                    repos = getOption("repos"),
                    sorted = TRUE, purge = getOption("Require.purge", FALSE)) {
  a <- lapply(pkgDep(packages, recursive = FALSE, which = which, depends = depends,
                     imports = imports, suggests = suggests, linkingTo = linkingTo,
                     repos = repos)[[1]],
              pkgDep,
              depends = depends, imports = imports, suggests = suggests, linkingTo = linkingTo,
              repos = repos, recursive = recursive
  )
  a <- unlist(a, recursive = FALSE)
  if (sorted) {
    ord <- order(sapply(a, function(x) length(x)), decreasing = TRUE)
    a <- a[ord]
  }
  return(a)
}

#' @importFrom utils compareVersion
pkgDepCRAN <- function(pkg, which = c("Depends", "Imports", "LinkingTo"),
                       #recursive = FALSE,
                       pkgsNoVersion,
                       keepVersionNumber = TRUE, repos = getOption("repos"),
                       purge = getOption("Require.purge", FALSE)) {
  capFull <- available.packagesCached(repos = repos, purge = purge)
  # cachedAvailablePackages <- available.packagesCached(repos = repos, purge = purge)
  #
  # capFull <- as.data.table(cachedAvailablePackages)
  deps <- pkgDepCRANInner(capFull, which = which, pkgs = pkg, pkgsNoVersion = pkgsNoVersion,
                          keepVersionNumber = keepVersionNumber)
  # if (recursive) {
  #   i <- 1
  #   pkgsNew <- list()
  #   pkgsNew[[i]] <- deps
  #   while (length(unlist(pkgsNew[[i]])) > 0) {
  #     i <- i + 1
  #     pkgsNew[[i]] <- lapply(pkgsNew[[i-1]], function(deps) {
  #       deps1 <- pkgDepCRANInner(capFull, which = which, pkgs = deps, keepVersionNumber = keepVersionNumber)
  #       unique(unlist(deps1))
  #     })
  #   }
  #   ss <- seq_along(deps)
  #   names(ss) <- names(deps)
  #   deps <- lapply(ss, function(x) {
  #     out <- unique(unname(unlist(lapply(pkgsNew, function(y) y[[x]]))))
  #     if (isFALSE(keepVersionNumber))
  #       out <- trimVersionNumber(out)
  #     out
  #     })
  # }
  deps
}


#' Reverse package depends
#'
#' This is a wrapper around \code{tools::dependsOnPkgs},
#' but with the added option of \code{sorted}, which
#' will sort them such that the packages at the top will have
#' the least number of dependencies that are in \code{pkgs}.
#' This is essentially a topological sort, but it is done
#' heuristically. This can be used to e.g., \code{detach} or
#' \code{unloadNamespace} packages in order so that they each
#' of their dependencies are detached or unloaded first.
#' @param pkgs A vector of package names to evaluate their
#'   reverse depends (i.e., the packages that \emph{use} each
#'   of these packages)
#' @param deps An optional named list of (reverse) dependencies.
#'   If not supplied, then \code{tools::dependsOnPkgs(..., recursive = TRUE)}
#'   will be used
#' @param topoSort Logical. If \code{TRUE}, the default, then
#'   the returned list of packages will be in order with the
#'   least number of dependencies listed in \code{pkgs} at
#'   the top of the list.
#' @param reverse Logical. If \code{TRUE}, then this will use \code{tools::pkgDependsOn}
#'   to determine which packages depend on the \code{pkgs}
#' @param useAllInSearch Logical. If \code{TRUE}, then all non-core
#' R packages in \code{search()} will be appended to \code{pkgs}
#' to allow those to also be identified
#' @param returnFull Logical. Primarily useful when \code{reverse = TRUE}.
#'   If \code{TRUE}, then then all installed packages will be searched.
#'   If \code{FALSE}, the default, only packages that are currently in
#'   the \code{search()} path and passed in \code{pkgs} will be included
#'   in the possible reverse dependencies.
#'
#' @export
#' @rdname pkgDep
#' @return
#' A possibly ordered, named (with packages as names) list where list elements
#' are either full reverse depends.
#'
#' @examples
#' \dontrun{
#' pkgDepTopoSort(c("Require", "data.table"), reverse = TRUE)
#' }
pkgDepTopoSort <- function(pkgs, deps, reverse = FALSE, topoSort = TRUE,
                           useAllInSearch = FALSE,
                           returnFull = TRUE, recursive = TRUE,
                           purge = getOption("Require.purge", FALSE)) {

  if (isTRUE(useAllInSearch)) {
    if (missing(deps)) {
      a <- search()
      a <- setdiff(a, .defaultPackages)
      a <- gsub("package:", "", a)
      pkgs <- unique(c(pkgs, a))
    } else {
      message("deps is provided; useAllInSearch will be set to FALSE")
    }
  }

  names(pkgs) <- pkgs
  if (missing(deps)) {
    aa <- if (isTRUE(reverse)) {
      ip <- .installed.pkgs()
      deps <- lapply(ip[,"Dependencies"], extractPkgName)
      names(deps) <- ip[, "Package"]
      names(pkgs) <- pkgs
      deps <- deps[order(names(deps))]
      revDeps <- lapply(pkgs, function(p) names(unlist(lapply(deps, function(d) if (isTRUE(any(p %in% d))) TRUE else NULL))))
      if (recursive) {
        revDeps <- lapply(revDeps, function(p) {
          if (!is.null(p)) {
            used <- p
            repeat({
              r <- unique(unlist(lapply(p, function(p1)  names(unlist(lapply(deps, function(d) if (isTRUE(any(p1 %in% d))) TRUE else NULL))))))
              used <- unique(c(r, used))
              if (length(r) == 0)
                break
              p <- r
            })
          } else {
            used <- NULL
          }
          sort(used)
        })

      }
    } else {
      pkgDep(pkgs, recursive = TRUE, purge = purge)
    }
    # testVal <- lapply(pkgs, function(p) sort(tools::dependsOnPkgs(p, recursive = TRUE)))
    # if (!identical(sort(unlist(testVal)), sort(unlist(aa)))) {
    #   cat(unlist(testVal), file = "c:/Eliot/tmp/test.txt")
    #   cat("-----------------", file = "c:/Eliot/tmp/test.txt", append = TRUE)
    #   cat(unlist(aa), file = "c:/Eliot/tmp/test.txt", append = TRUE)
    #   stop("new recursive reverse dependencies is not correct")
    # }


  }
  else
    aa <- deps
  bb <- list()
  cc <- lapply(pkgs, function(x) character())

  firsts <- unlist(lapply(aa, function(ps) {
    if (length(ps) == 0 || all(ps %in% .basePkgs))
      "first"
    else
      "later"
  }))
  aaa <- split(aa, firsts)
  aa <- aaa$later
  if (length(aa) > 1) {
    lengths <- unlist(lapply(aa, length))
    aa <- aa[order(lengths)]
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
            # bb[names(aa)[j]] <- list(overlapPkgs)
            cc[j] <- list(overlapPkgs)
            newOrd <- c(newOrd, j)
            i <- i + 1
            break
          }
        }
      }
      aa <- aa[newOrd]
      cc <- cc[newOrd]
    }
  }

  out <- if (isTRUE(returnFull)) {
    aa
  } else {
    cc
  }
  out <- append(aaa$first, out)

  return(out)
}

.defaultPackages <- c(".GlobalEnv", "tools:rstudio", "package:stats", "package:graphics",
                      "package:grDevices", "package:utils", "package:datasets", "package:methods",
                      "Autoloads", "package:base", "devtools_shims")


pkgDepCRANInner <- function(ap, which, pkgs, pkgsNoVersion, keepVersionNumber,
                            keepSeparate = FALSE) {
  # MUCH faster to use base "ap$Package %in% pkgs" than data.table internal "Package %in% pkgs"
  if (missing(pkgsNoVersion))
    pkgsNoVersion <- trimVersionNumber(pkgs)
  if (isFALSE(keepVersionNumber)) {
    pkgs <- pkgsNoVersion
  }
  ap <- ap[ap$Package %in% pkgsNoVersion]
  keep <- match(ap$Package, pkgsNoVersion)
  keep <- keep[!is.na(keep)]
  pkgsNoVersion1 <- pkgsNoVersion[keep]
  pkgs <- pkgs[keep]
  ap <- ap[order(pkgsNoVersion1)]

  names(which) <- which
  deps <- lapply (which, function(i) {
    lapply(ap[[i]], function(x) {
      out <- strsplit(x, split = "(, {0,1})|(,\n)")[[1]]
      out <- out[!is.na(out)]
      out <- grep(.grepR, out, value = TRUE, invert = TRUE) # remove references to R
    })
  })

  ss <- seq_along(pkgsNoVersion1)
  names(ss) <- pkgsNoVersion1
  if (isFALSE(keepSeparate))
    deps <- lapply(ss, function(x) unname(unlist(lapply(deps, function(y) y[[x]]))))
  deps
}

DESCRIPTIONFileDeps <- function(desc_path, which = c("Depends", "Imports", "LinkingTo"),
                                keepVersionNumber = TRUE,
                                purge = getOption("Require.purge", FALSE),
                                keepSeparate = FALSE) {
  objName <- if (length(desc_path) == 1)
    paste0(desc_path, paste0(collapse = "_", which, "_", keepVersionNumber))
  else {
    grepPackage <- "Package *: *"
    grepVersion <- "Version *: *"
    suppressWarnings(pkg <- gsub(grepPackage, "", grep(grepPackage, desc_path, value = TRUE)))
    suppressWarnings(vers <- gsub(grepVersion, "", grep(grepVersion, desc_path, value = TRUE)))
    paste(pkg, vers, sep = "_", paste0(which, "_", keepVersionNumber, collapse = "_"))
  }
  if (isTRUE(length(objName) > 1) ||
      isTRUE(any(!exists(objName, envir = .pkgEnv[["pkgDep"]][["DESCRIPTIONFile"]]))) ||
      isTRUE(purge)) {
    lines <- if (length(desc_path) == 1)
      try(readLines(desc_path))
    else
      lines <- desc_path
    if (is(lines, "try-error")) browser()
    Sys.setlocale(locale = "C") # required to deal with non English characters in Author names
    on.exit(Sys.setlocale(locale = ""))
    sl <- list()
    sl[["depends"]] <- grep("^Depends: *", lines) # nolint
    sl[["suggests"]] <- grep("^Suggests: *", lines) # nolint
    sl[["imports"]] <- grep("^Imports: *", lines) # nolint
    sl[["linkingto"]] <- grep("^LinkingTo: *", lines) # nolint
    sl[["remotes"]] <- grep("^Remotes: *", lines) # nolint
    sl[["colon"]] <- grep(": *", lines) # nolint

    which <- paste0(toupper(substr(which, 1, 1)), substr(which, 2, 1e4))
    which <- unique(which)
    whichLower <- tolower(which)
    needed <- Map(whLower = whichLower, wh = which, function(whLower, wh) {
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
        needs <- gsub(" *$", "", needs[[1]]) # remove trailing spaces
        if (isFALSE(keepVersionNumber))
          needs <- trimVersionNumber(needs)
        needs
      }
    })
    if (length(objName) == 1)
      assign(objName, needed, envir = .pkgEnv[["pkgDep"]][["DESCRIPTIONFile"]])
  } else {
    needed <- get(objName, envir = .pkgEnv[["pkgDep"]][["DESCRIPTIONFile"]])
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
    which <- list(c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"))
  } else if (identical("most", which)) {
    which <- list(c("Depends", "Imports", "LinkingTo", "Suggests"))
  } else if (isTRUE(which)) {
    which <- list(c("Depends", "Imports", "LinkingTo", "Suggests"),
                  c("Depends", "Imports", "LinkingTo"))
  } else if (any(is.na(which))) {
    which <- list(c("Depends", "Imports", "LinkingTo"))
  } else if (is.character(which)) {
    which <- list(which)
  } else if (isFALSE(which)) {
    which <- character()
  }
  which
}

.installed.pkgs <- function(lib.loc = .libPaths(), which = c("Depends", "Imports", "LinkingTo"), other = NULL,
                            purge = getOption("Require.purge", FALSE)) {
  purge <- dealWithCache(purge)
  if (!is.null(other))
    if (any(grepl("github", tolower(other)))) {
      other <- c("GithubRepo", "GithubUsername", "GithubRef", "GithubSHA1")
    }

  out <- lapply(lib.loc, function(path) {
    dirs <- dir(path, full.names = TRUE)
    areDirs <- dir.exists(dirs)
    dirs <- dirs[areDirs]
    files <- file.path(dirs, "DESCRIPTION")
    filesExist <- file.exists(files)
    files <- files[filesExist]
    desc_lines <- lapply(files, function(file) DESCRIPTIONFile(file))
    versions <- DESCRIPTIONFileVersionV(desc_lines, purge = FALSE)
    deps <- if (length(which)) lapply(desc_lines, function(lines)
      DESCRIPTIONFileDeps(lines, which = which, purge = purge)) else NULL
    if (!is.null(other)) {
      names(other) <- other
      others <- lapply(other, function(oth) DESCRIPTIONFileOtherV(desc_lines, other = oth))
      # shas <- DESCRIPTIONFileOtherV(desc_lines)
    }
    mat <- cbind("Package" = dirs[filesExist], "Version" = versions, "Depends" = deps)
    if (!is.null(other)) {
      mat <- cbind(mat, as.data.frame(others, stringsAsFactors = FALSE), stringsAsFactors = FALSE)
    }
    mat
  })
  lengths <- unlist(lapply(out, function(x) NROW(x)))
  out <- do.call(rbind, out)
  ret <- cbind("Package" = basename(unlist(out[, "Package"])), "LibPath" = rep(lib.loc, lengths),
        "Version" = out[, "Version"])
  if (length(which))
    ret <- cbind(ret, "Dependencies" = out[, "Depends"], stringsAsFactors = FALSE)
  if (!is.null(other)) {
    ncolBefore <- NCOL(ret)
    ret <- cbind(ret, out[, other], stringsAsFactors = FALSE)
  }
  ret
}

.basePkgs <- # unlist(.installed.pkgs(tail(.libPaths(), 1))[, "Package"])
  c("base", "boot", "class", "cluster", "codetools", "compiler",
    "datasets", "foreign", "graphics", "grDevices", "grid", "KernSmooth",
    "lattice", "MASS", "Matrix", "methods", "mgcv", "nlme", "nnet",
    "parallel", "rpart", "spatial", "splines", "stats", "stats4",
    "survival", "tcltk", "tools", "translations", "utils")

DESCRIPTIONFileDepsV <- Vectorize(DESCRIPTIONFileDeps, vectorize.args = "desc_path",
                                  SIMPLIFY = FALSE)

#' Package dependencies when one or more packages removed
#'
#' This is primarily for package developers.
#' It allows the testing of what the recursive dependencies would be if a package was removed
#' from the immediate dependencies.
#' @param pkg A package name to be testing the dependencies
#' @param depsRemoved A vector of package names who are to be "removed"
#'   from the \code{pkg} immediate dependencies
#' @export
#' @return
#' A character vector of the packages that would removed from recursive dependencies
#' of \code{pkg}
#' if \code{depsRemoved} were removed from first level dependencies
#' @examples \dontrun{
#' pkgDepIfDepRemoved("Require", "remotes")
#' }
pkgDepIfDepRemoved <- function(pkg = character(), depsRemoved = character()) {
  if (length(pkg)) {
    p2 <- pkgDep2(pkg, recursive = TRUE)
    p1 <- sort(extractPkgName(pkgDep(pkg, recursive = TRUE)[[1]]))
    p2Clean <- sort(
      unique(
        extractPkgName(
          setdiff(
            c(extractPkgName(names(p2)),
              unlist(unname(p2[!extractPkgName(names(p2)) %in% depsRemoved]))),
            depsRemoved))))
    pkg <- paste(paste(setdiff(p1, p2Clean), collapse = ", "))
  }
  return(pkg)
}
