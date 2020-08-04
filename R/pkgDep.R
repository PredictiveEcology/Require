utils::globalVariables(c(
  "PackageTrimmed", "hasVers", "atLeastOneWithVersionSpec", "maxVersionSpec", "Current"
))

#' Determine package dependencies
#'
#' This will first look in local filesystem (in \code{.libPaths()}), then
#' \code{CRAN}. If the package is in the form of a GitHub package with format
#' \code{account/repo@branch}, it will attempt to get package dependencies from
#' the GitHub \file{DESCRIPTION} file.
#' Currently, it will not find \code{Remotes}.
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
#' pkgDep("Require")
#' \dontrun{
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

  if (any(!missing(depends), !missing(linkingTo), !missing(imports), !missing(suggests))) {
    message("Please use 'which' instead of 'imports', 'suggests', 'depends' and 'linkingTo'")
    if (!missing(depends)) depends <- TRUE
    if (!missing(imports)) imports <- TRUE
    if (!missing(suggests)) suggests <- TRUE
    if (!missing(linkingTo)) linkingTo <- TRUE
  }
  which <- whichToDILES(which)

  # Only deal with first one of "which"... deal with second later
  whichCat <- paste(sort(which[[1]]), collapse = "_")
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
    theNulls <- unlist(lapply(neededFull, is.null))
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
            dt[, PackageTrimmed := extractPkgName(Package)]
            dt[, versionSpec := extractVersionNumber(Package)]
            dt[, hasVers := !is.na(versionSpec)]
            dt[hasVers == TRUE, inequality := extractInequality(Package)]
            dt[hasVers == FALSE, versionSpec := NA]
            dt[, atLeastOneWithVersionSpec := any(hasVers), by = "PackageTrimmed"]
            dt[, Current := all(Current == TRUE), by = "PackageTrimmed"] # don't need to redo depdencies of one that already did it
            dt <- dt[!(atLeastOneWithVersionSpec == TRUE & hasVers == FALSE)] # remove cases where no version spec >1 case
            #setorderv(dt, "PackageTrimmed", na.last = TRUE)

            dt1 <- dt[!is.na(versionSpec), list(Package, Current, hasVers, inequality,
                                                atLeastOneWithVersionSpec,
                                                versionSpec = as.character(package_version(versionSpec)),
                                                maxVersionSpec = as.character(max(package_version(versionSpec)))),
                      by = "PackageTrimmed"]
            dt1 <- dt1[versionSpec == maxVersionSpec]
            dt1 <- dt1[, lapply(.SD, function(x) x[1]), by = "PackageTrimmed"]
            dt2 <- dt[is.na(versionSpec)]
            dt <- rbindlist(list(dt1, dt2), use.names = TRUE, fill = TRUE)
            dt3 <- dt[!duplicated(dt$PackageTrimmed)]
            dt4 <- dt3[Current == TRUE]
            pkgsNew <- list()
            pkgsNew[[i - 1]] <- dt3[Current == FALSE]$Package
            pkgsNew[[i]] <- dt4$Package
          }
          needed <- unique(unlist(pkgsNew))
        })
      }
    }
    # Remove "R"
    neededFull2 <- lapply(neededFull2, function(needed)
      grep("^\\<R\\>", needed, value = TRUE, invert = TRUE))

    Map(sn = saveNames, n = names(saveNames), function(sn, n) {
      assign(sn, neededFull2[[n]], envir = .pkgEnv)
    })
    neededFull1 <- append(neededFull1[!needGet], neededFull2)
  }

  if (isTRUE(sort))
    neededFull1 <- lapply(neededFull1, function(x) sort(x))
  if (isFALSE(keepVersionNumber))
    neededFull1 <- lapply(neededFull1, trimVersionNumber)
  if (!isTRUE(includeBase)) {
    neededFull1 <- lapply(neededFull1, setdiff, .basePkgs)
  }
  neededFull1
}

pkgDepInner <- function(packages, libPath, which, keepVersionNumber,
                        purge = getOption("Require.purge", FALSE),
                        repos = repos) {
  names(packages) <- packages
  desc_paths <- getDescPath(packages, libPath)

  needed <- Map(desc_path = desc_paths, pkg = packages, function(desc_path, pkg) {
    browser(expr = exists("aaaaa"))
    if (!file.exists(desc_path)) {
      pkgDT <- parseGitHub(pkg)
      if ("GitHub" %in% pkgDT$repoLocation) {
        which <- c(which, "Remotes")
        pkgDT <- getGitHubDESCRIPTION(pkgDT)
        needed <- DESCRIPTIONFileDeps(pkgDT$DESCFile, which = which)
        pkgDT2 <- data.table(Package = needed, github = extractPkgGitHub(needed))
        if ("github" %in% colnames(pkgDT2)) {
          setorderv(pkgDT2, "github", na.last = TRUE)
        }
        needed <- pkgDT2[!duplicated(extractPkgName(pkgDT2$Package))]$Package
      } else {
        needed <- unname(unlist(pkgDepCRAN(pkg,
                                           which = which,
                                           keepVersionNumber = keepVersionNumber,
                                           purge = purge,
                                           repos = repos)))
        purge <<- FALSE
        needed
      }
    } else {
      needed <- DESCRIPTIONFileDeps(desc_path, which = which, keepVersionNumber = keepVersionNumber)
    }
    needed
  })
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
#' pkgDep2("Require")
#' \dontrun{
#' # much bigger one
#' pkgDep2("reproducible")
#' }
pkgDep2 <- function(packages, recursive = TRUE,
                    which = c("Depends", "Imports", "LinkingTo"),
                    depends, imports, suggests, linkingTo,
                    repos = getOption("repos"),
                    sorted = TRUE) {
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
                       keepVersionNumber = TRUE, repos = getOption("repos"),
                       purge = getOption("Require.purge", FALSE)) {
  cachedAvailablePackages <- available.packagesCached(repos = repos, purge = purge)

  capFull <- as.data.table(cachedAvailablePackages)
  deps <- pkgDepCRANInner(capFull, which = which, pkgs = pkg,
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
#' @param returnFull Logical. If \code{TRUE}, then the full reverse
#'   dependencies will be returned; if \code{FALSE}, the default,
#'   only the reverse dependencies that are found within the \code{pkgs}
#'   (and \code{search()} if \code{useAllInSearch = TRUE}) will be returned.
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
pkgDepTopoSort <- function(pkgs, deps, reverse = FALSE, topoSort = TRUE, useAllInSearch = FALSE,
                           returnFull = TRUE, recursive = TRUE) {

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
      pkgDep(pkgs, recursive = TRUE)
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

  if (isTRUE(topoSort)) {
    notInOrder <- TRUE
    isCorrectOrder <- logical(length(aa))
    i <- 1
    newOrd <- numeric(0)
    for (i in seq_along(aa)) {
      dif <- setdiff(seq_along(aa), newOrd)
      for (j in dif) {
        overlapFull <- aa[[j]] %in% names(aa)[-i]
        overlap <- aa[[j]] %in% names(aa)[dif]
        overlapPkgs <- aa[[j]][overlapFull]
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
  out <- if (isTRUE(returnFull)) {
    aa
  } else {
    cc
  }
  return(out)
}

.defaultPackages <- c(".GlobalEnv", "tools:rstudio", "package:stats", "package:graphics",
                      "package:grDevices", "package:utils", "package:datasets", "package:methods",
                      "Autoloads", "package:base", "devtools_shims")


pkgDepCRANInner <- function(ap, which, pkgs, keepVersionNumber) {
  # MUCH faster to use base "ap$Package %in% pkgs" than data.table internal "Package %in% pkgs"
  pkgsNoVersion <- trimVersionNumber(pkgs)
  if (isFALSE(keepVersionNumber)) {
    pkgs <- trimVersionNumber(pkgs)
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
      out <- grep("^\\<R\\>", out, value = TRUE, invert = TRUE) # remove references to R
    })
  })

  ss <- seq_along(pkgsNoVersion1)
  names(ss) <- pkgsNoVersion1
  deps <- lapply(ss, function(x) unname(unlist(lapply(deps, function(y) y[[x]]))))
}

DESCRIPTIONFileDeps <- function(desc_path, which = c("Depends", "Imports", "LinkingTo"),
                                keepVersionNumber = TRUE) {
  lines <- readLines(desc_path)
  Sys.setlocale(locale = "C") # required to deal with non English characters in Author names
  on.exit(Sys.setlocale(locale = ""))
  sl <- list()
  sl[["depends"]] <- grep("^Depends: *", lines) # nolint
  sl[["suggests"]] <- grep("^Suggests: *", lines) # nolint
  sl[["imports"]] <- grep("^Imports: *", lines) # nolint
  sl[["linkingto"]] <- grep("^LinkingTo: *", lines) # nolint
  sl[["remotes"]] <- grep("^Remotes: *", lines) # nolint
  sl[["colon"]] <- grep(": *", lines) # nolint

  whichLower <- tolower(which)
  needed <- Map(whLower = whichLower, wh = which, function(whLower, wh) {
    if (length(sl[[whLower]])) {
      allLines <- sl[[whLower]]:(sl[["colon"]][which(sl[["colon"]] %in% sl[[whLower]]) + 1] - 1) # nolint
      needs <- paste(lines[allLines], collapse = "")
      needs <- gsub(paste0(wh, ": *"), "", needs)
      needs <- strsplit(needs, split = ", *")
      needs <- gsub(" *$", "", needs[[1]]) # remove trailing spaces
      if (isFALSE(keepVersionNumber))
        needs <- trimVersionNumber(needs)
      needs
    }
  })
  needed <- unname(unlist(needed))
  needed <- grep("^R[\\( ]", needed, value = TRUE, invert = TRUE)

  needed
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

.installed.pkgs <- function(lib.loc = .libPaths(), which = c("Depends", "Imports", "LinkingTo")) {
  out <- lapply(lib.loc, function(path) {
    dirs <- dir(path, full.names = TRUE)
    areDirs <- dir.exists(dirs)
    dirs <- dirs[areDirs]
    files <- file.path(dirs, "DESCRIPTION")
    filesExist <- file.exists(files)
    files <- files[filesExist]
    versions <- unlist(lapply(files, function(file) DESCRIPTIONFileVersion(file)))
    deps <- if (length(which)) lapply(files, function(file) DESCRIPTIONFileDeps(file, which = which)) else NULL
    cbind("Package" = dirs[filesExist], "Version" = versions, "Depends" = deps)
  })
  lengths <- unlist(lapply(out, function(x) NROW(x)))
  out <- do.call(rbind, out)
  ret <- cbind("Package" = basename(unlist(out[, "Package"])), "LibPath" = rep(lib.loc, lengths),
        "Version" = out[, "Version"])
  if (length(which))
    ret <- cbind(ret, "Dependencies" = out[, "Depends"])
  ret
}

.basePkgs <- unlist(.installed.pkgs(tail(.libPaths(), 1))[, "Package"])

