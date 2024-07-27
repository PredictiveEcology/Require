utils::globalVariables(
  c("Additional_repositories", "githubPkgName",
    "localBranch", "localFiles", "localRepo", "installedSha", "localUsername",
    "newPackageFullName", "..keepNames")
)


#' Reverse package depends
#'
#' This is a wrapper around `tools::dependsOnPkgs`,
#' but with the added option of `topoSort`, which
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
pkgDepTopoSort <- function(pkgs,
                           deps,
                           reverse = FALSE,
                           topoSort = TRUE,
                           libPaths,
                           useAllInSearch = FALSE,
                           returnFull = TRUE,
                           recursive = TRUE,
                           purge = getOption("Require.purge", FALSE),
                           which = c("Depends", "Imports", "LinkingTo"),
                           type = getOption("pkgType"),
                           verbose = getOption("Require.verbose"), ...) {

  libPaths <- dealWithMissingLibPaths(libPaths, ...)

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
        ip <- .installed.pkgs(lib.loc = libPaths, which = which, collapse = TRUE) # need all installed packages
        ip <- installed.packagesDeps(ip, libPaths = libPaths, which = which[1])
        deps <- depsWithCommasToVector(ip$Package, ip$deps)
         deps <- lapply(deps, extractPkgName)
        # names(deps) <- ip[, "Package"]
        # names(pkgs) <- pkgs
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

                r <- setdiff(r, used) # addresses circularity

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
          libPaths = libPaths,
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


# @inheritParams Require
# pkgDepCRANInner <-
#   function(ap,
#            which,
#            pkgDT,
#            repos,
#            purge,
#            type,
#            # pkgsNoVersion,
#            # keepVersionNumber,
#            # keepSeparate = FALSE,
#            verbose = getOption("Require.verbose")) {
#
#     if (is.null(pkgDT$Depends)) {
#       if (missing(ap))
#         ap <- getAvailablePackagesIfNeeded(pkgDT$Package, repos = repos, purge = purge, verbose = verbose, type = type)
#       keepNames <- c("Package", setdiff(colnames(pkgDT), colnames(ap)))
#       pkgDT <- ap[pkgDT[, ..keepNames], on = "Package"]
#     }
#
#     pkgDT[is.na(versionSpec), availableVersionOK := TRUE]
#     pkgDT[!is.na(versionSpec), availableVersionOK := compareVersion2(Version, versionSpec, inequality)]
#     for (co in which)
#       set(pkgDT, which(is.na(pkgDT[[co]])), co, "")
#
#     pkgDT[, deps := do.call(paste, append(.SD, list(sep = comma))), .SDcols=which]
#
#     # remove trailing and initial commas
#     set(pkgDT, NULL, "deps", gsub("(, )+$", "", pkgDT$deps))
#     set(pkgDT, NULL, "deps", gsub("^(, )+", "", pkgDT$deps))
#
#     deps <- Map(pkgFN = pkgDT$packageFullName, x = pkgDT$deps, function(pkgFN, x) {
#       out <- strsplit(x, split = "(, {0,1})|(,\n)")[[1]]
#       out <- out[!is.na(out)]
#       out <-
#         grep(.grepR, out, value = TRUE, invert = TRUE)
#       out
#     })
#     depsAll <- Map(toPkgDepDT, deps, verbose = verbose)
#     depsAll
#   }

DESCRIPTIONFileDeps <-
  function(desc_path,
           which = c("Depends", "Imports", "LinkingTo"),
           keepVersionNumber = TRUE,
           purge = getOption("Require.purge", FALSE),
           keepSeparate = FALSE) {
    if (is.null(pkgDepEnv())) {
      envPkgDepCreate()
    }
    if (is.null(envPkgDepDESCFile())) {
      envPkgDepDESCFileCreate()
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
      isTRUE(any(!exists(objName, envir = envPkgDepDESCFile()))) ||
      isTRUE(purge)) {
      if (is.null(desc_path)) {
        needed <- NULL
      } else {
        lines <- if (length(desc_path) == 1) {
          # linesAll <- lapply(desc_path, read.dcf)
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
        sl[["Depends"]] <- grep("^Depends: *", lines) # nolint
        sl[["Suggests"]] <- grep("^Suggests: *", lines) # nolint
        sl[["Imports"]] <- grep("^Imports: *", lines) # nolint
        sl[["Linkingto"]] <- grep("^LinkingTo: *", lines) # nolint
        sl[["Remotes"]] <- grep("^Remotes: *", lines) # nolint
        sl[["colon"]] <- grep(": *", lines) # nolint
        # sl[["Additional_repositories"]] <- grep("^Additional_repositories: *", lines) # nolint

        which <-
          paste0(toupper(substr(which, 1, 1)), substr(which, 2, 1e4))
        which <- unique(which)
        whichLower <- tolower(which)
        needed <-
          Map(#whLower = whichLower,
              # wh = which, function(whLower, wh) {
                wh = which, function(wh) {
            if (length(sl[[wh]])) {
              whEnd <- which(sl[["colon"]] %in% sl[[wh]]) + 1
              whEnd <- if (length(sl[["colon"]]) < whEnd) {
                length(lines)
              } else {
                (sl[["colon"]][whEnd] - 1)
              }
              allLines <- sl[[wh]]:whEnd # nolint
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
          assign(objName, needed, envir = envPkgDepDESCFile())
        }
      }
    } else {
      needed <-
        get(objName, envir = envPkgDepDESCFile())
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

#' Partial alternative (faster) to `installed.packages`
#'
#' This reads the DESCRIPTION files only, so can only access fields that are
#' available in the DESCRIPTION file. This is different than `installed.packages`
#' which has many other fields, like "Built", "NeedsCompilation" etc. If those
#' fields are needed, then this function will return an empty column in the returned
#' character matrix.
#' @inheritParams utils::installed.packages
#' @inheritParams pkgDep
#' @param other Can supply other fields; the only benefit here is that a user
#'   can specify `"github"` (lower case) and it will automatically add
#'   c("GithubRepo", "GithubUsername", "GithubRef", "GithubSHA1",
#'   "GithubSubFolder") fields
#' @param collapse Logical. If `TRUE` then the dependency fields will be collapsed;
#'   if `FALSE` (default) then the `which` fields will be kept separate.
#' @param packages Character vector. If `NULL` (default), then all installed packages
#'   are searched for. If a character vector is supplied, then it will only return
#'   information about those packages (and is thus faster to execute).
.installed.pkgs <-
  function(lib.loc = .libPaths(),
           which = c("Depends", "Imports", "LinkingTo"),
           other = NULL,
           purge = getOption("Require.purge", FALSE),
           packages = NULL,
           collapse = FALSE) {
    purge <- dealWithCache(purge)
    if (!is.null(other)) {
      hasGit <- grepl("github", tolower(other))
      if (any(hasGit)) {
        other <- c(
          c("GithubRepo", "GithubUsername", "GithubRef", "GithubSHA1",
            "GithubSubFolder"),
          other[!hasGit])
      }
    }

    out <- lapply(lib.loc, function(path) {
      dirs <- dir(path, full.names = TRUE)
      # from pak -- makes a _cache which isn't relevant here
      dirs <- dirs[!endsWith(dirs, suffix = "_cache")]
      mat <- NULL
      if (length(dirs)) {
        if (!is.null(packages)) {
          keeps <- basename(dirs) %in% packages
          dirs <- dirs[keeps]
        }
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
          names(which) <- which
          deps <- lapply(desc_lines, function(lines) {
            if (isTRUE(collapse))
              Map(a = "Depends", function(a) paste(DESCRIPTIONFileDeps(lines, which = which, purge = purge), collapse = comma))
            else
              lapply(which, function(wh)
                paste(DESCRIPTIONFileDeps(lines, which = wh, purge = purge), collapse = comma)
            )
          })
          if (length(deps))
            deps <- invertList(deps)
          deps
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
            "Version" = versions
          )
        if (!is.null(deps)) {
          cn <- c(colnames(mat), names(deps))
          mat <- cbind(mat, matrix(unlist(deps), ncol = length(deps)))
          colnames(mat) <- cn
        }
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
      }
      mat
    })

    lengths <- unlist(lapply(out, function(x) {
      NROW(x)
    }))

    out <- do.call(rbind, out)
    colNames <- c("Package", "Version", "LibPath", which, other)
    if (is.null(out)) {
      out <- matrix(data = character(), ncol = 3 + length(which) + length(other))
      colnames(out) <- colNames
    } else {
      out[, "Package"] <- basename(out[, "Package"])
      out <- cbind(out, "LibPath" = rep(lib.loc, lengths), stringsAsFactors = FALSE)

      dups <- duplicated(out[, "Package"]) # means installed in >1 .libPaths()
      out <- out[!dups, ]
    }
    colNames <- intersect(colNames, colnames(out))
    out <- out[, colNames]
    # ret <-
    #   cbind(
    #     "Package" = basename(unlist(out[, "Package"])),
    #     "LibPath" = rep(lib.loc, lengths),
    #     "Version" = out[, "Version"]
    #   )
    # if (length(which)) {
    #   ret <-
    #     cbind(ret,
    #       "Dependencies" = out[, "Depends"],
    #       stringsAsFactors = FALSE
    #     )
    # }
    # if (NROW(ret)) {
    #   if (!is.null(other)) {
    #     ncolBefore <- NCOL(ret)
    #     ret <- cbind(ret, out[, other], stringsAsFactors = FALSE)
    #   }
    # }
    out
  }

.basePkgs <-
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

# @inheritParams Require
# getGitHubDeps <-
#   function(pkg,
#            pkgDT,
#            which,
#            purge,
#            verbose = getOption("Require.verbose"),
#            includeBase = FALSE) {
#     pkg <- masterMainToHead(pkgDT$packageFullName)
#
#     localVersionOK <- pkgDT$installedVersionOK
#     pkgDepDTOuter <- data.table(packageFullName = character())
#
#     if (any(!localVersionOK %in% TRUE)) {
#       pkgDTNotLocal <- dlGitHubDESCRIPTION(pkgDT[!localVersionOK %in% TRUE], purge = purge)
#     }
#     if (any(localVersionOK %in% TRUE)) {
#       pkgDT[localVersionOK %in% TRUE, DESCFile := base::system.file("DESCRIPTION", package = Package), by = "Package"]
#       if (exists("pkgDTNotLocal", inherits = FALSE))
#         pkgDT <- rbindlist(list(pkgDT[localVersionOK %in% TRUE], pkgDTNotLocal), fill = TRUE, use.names = TRUE)
#     } else {
#       pkgDT <- pkgDTNotLocal
#     }
#
#     hasVersionNum <- pkgDT$hasVers # grep(grepExtractPkgs, pkgDT$packageFullName)
#     set(pkgDT, NULL, "availableVersionOK", NA)
#     if (any(hasVersionNum)) {
#       if (!isTRUE(getOption("Require.offlineMode"))) {
#         pkgDT[hasVersionNum, VersionOnRepos := DESCRIPTIONFileVersionV(DESCFile, purge = purge)]
#         if (is.null(pkgDT$versionSpec)) {
#           pkgDT[hasVersionNum, versionSpec := extractVersionNumber(packageFullName)]
#           pkgDT[hasVersionNum, inequality := extractInequality(packageFullName)]
#         }
#         pkgDT[hasVersionNum, availableVersionOK := compareVersion2(VersionOnRepos, versionSpec, inequality)]
#       }
#     }
#
#
#     if (any(!pkgDT$availableVersionOK %in% FALSE)) {
#       neededV <-
#         try(DESCRIPTIONFileDepsV(pkgDT$DESCFile, which = which, purge = purge))
#       if (is(neededV, "try-error")) {
#         unlink(pkgDT$DESCFile)
#         unlink(pkgDT$destFile)
#         set(pkgDT, NULL, c("DESCFile", "destFile"), NULL)
#         browserDeveloper(paste0("A problem occurred installing ", pkgDT$packageFullName, ". Does it exist?",
#                                 "\nTo confirm whether it exists, try browsing to ",
#                                 file.path("https://github.com", pkgDT$Account, pkgDT$Package, "tree", pkgDT$Branch),
#                                 "\nIf it does exist, try rerunning with `purge = TRUE`",
#                                 "\nIf this error is inaccurate, and the problem persists, ",
#                                 "please contact developers with error code 949"))
#       }
#
#       neededAdditionalReposV <- DESCRIPTIONFileOtherV(pkgDT$DESCFile, other = "Additional_repositories")
#
#       neededRemotesV <-
#         DESCRIPTIONFileDepsV(pkgDT$DESCFile, which = "Remotes", purge = purge)
#       names(neededV) <- pkgDT$packageFullName
#
#       pkgDepDTOuter <- updateWithRemotesNamespaceAddRepos(neededV, neededRemotesV, pkgDT, neededAdditionalReposV, includeBase, verbose)
#     }
#
#     pkgDepDTOuter
#   }

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

#' Purge everything in the Require cache
#'
#' Require uses caches for local Package saving, local caches of `available.packages`,
#' local caches of GitHub (e.g., `"DESCRIPTION"`) files, and some function calls
#' that are cached. This function clears all of them.
#'
#' @inheritParams Require
#' @return Run for its side effect, namely, all cached objects are removed.
#'
#' @export
cachePurge <- function(packages = FALSE,
                       repos = getOption("repos")) {
  if (isTRUE(packages))
    Require::cacheClearPackages(ask = FALSE)
  dealWithCache(TRUE, repos = repos)
}

#' @rdname cachePurge
#' @export
purgeCache <- cachePurge

dealWithCache <- function(purge,
                          checkAge = TRUE,
                          repos = getOption("repos")) {
  if (isTRUE(getOption("Require.offlineMode"))) {
    purge <- FALSE
    checkAge <- FALSE
  }

  purgeBasedOnTime <- FALSE
  if (!isTRUE(purge) && isTRUE(checkAge)) {
    ee <- pkgEnvStartTime()
    purgeBasedOnTime <- if (is.null(ee)) FALSE else purgeBasedOnTimeSinceCached(ee)
  }
  purge <- purge || purgeBasedOnTime

  if (is.null(pkgDepEnv()) ) {
    # if (is.null(pkgDepEnv()) || purge) {
    envPkgDepCreate()
  }
  if (purge) {
    pkgEnvStartTimeCreate()
  }
  if (purge) {
    unlink(dir(RequireGitHubCacheDir(), full.names = TRUE))
    # getSHAFromGItHubMemoise
    SHAfile <- getSHAFromGitHubDBFilename()
    if (isTRUE(file.exists(SHAfile)))
      unlink(SHAfile)

    purgeAvailablePackages(repos, purge)
    # fn <- availablePackagesCachedPath(repos = repos, type = c("source", "binary"))
    # fExists <- file.exists(fn)
    # if (any(fExists)) {
    #   unlink(fn[fExists])
    # }

    # This is for pkgDep
    fn <- pkgDepDBFilename()
    unlink(fn)
  }

  if (is.null(envPkgDepGitHubSHA()))
    envPkgDepGitHubSHACreate()

  if (is.null(envPkgDepDeps()))
    envPkgDepDepsCreate()

  if (is.null(envPkgDepDESCFile()))
    envPkgDepDESCFileCreate()

  if (is.null(envPkgDepArchiveDetailsInner()))
    envPkgDepArchiveDetailsInnerCreate()

  purgePkgDep(purge)

  purge
}

.grepTooManySpaces <- " {2,}"
.grepTabCR <- "\n|\t"



purgePkgDep <- function(purge) {
  if (isTRUE(purge)) {
    envPkgDepCreate()
    envPkgDepGitHubSHACreate()
    envPkgDepDepsCreate()
    envPkgDepDESCFileCreate()
    envPkgDepArchiveDetailsInnerCreate()
  }
  purge
}

#' This converts master or main to HEAD for a git repo
#'
#' This will also convert a git repo with nothing after the @ to @HEAD
#' @param gitRepo A git repository of the form account/repo with optional @branch
#'    or @sha or @tag
#' @return The git repository with @HEAD if it had @master, @main or no @.
masterMainToHead <- function(gitRepo) {
  masterOrMain <- "@(master|main) *"
  areMasterOrMain <- grepl(masterOrMain, gitRepo)
  if (any(areMasterOrMain)) {
    masterOrMainNoSpace <- "@(master|main)"
    gitRepo[areMasterOrMain] <-
      gsub(paste0("(", masterOrMainNoSpace, ")"), "@HEAD", gitRepo[areMasterOrMain])
  }

  noAt <- !grepl("@", gitRepo)
  if (any(noAt)) {
    gitRepo[noAt] <- paste0(gitRepo[noAt], "@HEAD")
  }
  return(gitRepo)
}

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



pkgDepTopoSortMemoise <- function(...) {
  if (getOption("Require.useMemoise", TRUE)) {
    dots <- list(...)
    fnName <- "pkgDepTopoSort"
    fn <- eval(parse(text = fnName))
    pe <- pkgEnv()
    if (!exists(fnName, envir = pe, inherits = FALSE)) {
      pe[[fnName]] <- new.env()
    }
    ret <- NULL
    ss <- match.call(definition = fn)
    pkg <- eval(ss$pkg, envir = parent.frame())
    hash <-
      sum(as.integer(serialize(
        object = pkg, ascii = TRUE, NULL
      )))
    hash <- as.character(hash)
    if (!exists(hash, envir = pe[[fnName]], inherits = FALSE)) {
      pe[[fnName]][[hash]] <- list()
    } else {
      whIdent <-
        unlist(lapply(pe[[fnName]][[hash]], function(x) {
          identical(x$input, dots)
        }))
      if (any(whIdent)) {
        ret <- pe[[fnName]][[hash]][[which(whIdent)]]$output
      }
    }
    if (is.null(ret)) {
      inputs <- data.table::copy(dots)
      ret <- fn(...)
      pe[[fnName]][[hash]] <-
        list(pe[[fnName]][[hash]], list(input = inputs, output = ret))
    }
  } else {
    ret <- fn(...)
  }

  return(ret)
}

pkgDepDBFilename <- function() {
  if (!is.null(cacheGetOptionCachePkgDir())) {
    file.path(cachePkgDir(), "pkgDepDB.rds")
  } # returns NULL if no Cache used
}

isAre <- function(l, v) {
  singularPlural(c("is", "are"), l, v)
}

hasHave <- function(l, v) {
  singularPlural(c("has", "have"), l, v)
}

singularPlural <- function(singPlur, l, v) {
  if (!missing(l)) {
    out <- singPlur[(length(l) > 1) + 1]
  }
  if (!missing(v)) {
    out <- singPlur[(v > 1) + 1]
  }
  out
}


prependSelf <- function(deps, includeSelf) {
  if (isTRUE(includeSelf)) {
    deps <- Map(pkgDepDT = deps, nam = names(deps), function(pkgDepDT, nam) {
      depsInner <- pkgDepDT
      alreadyHasSelf <- identical(extractPkgName(pkgDepDT$packageFullName[1]), extractPkgName(nam))
      # alreadyHasSelf <- startsWith(pkgDepDT[1], trimVersionNumber(nam))
      if (!isTRUE(alreadyHasSelf)) {
        depsInner <- rbindlist(list(toPkgDepDT(nam), pkgDepDT), fill = TRUE, use.names = TRUE)
      }

      return(depsInner)
    })
  }
  deps
}

getAvailablePackagesIfNeeded <-
  function(packages, repos, purge, verbose, type) {
    if (is.data.table(packages)) {
      pkgDT <- packages
    } else {
      pkgDT <- parseGitHub(packages)
    }
    isCRAN <- pkgDT[["repoLocation"]] %in% "CRAN"

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
cacheClearPackages <- function(packages,
                                     ask = interactive(),
                                     Rversion = versionMajorMinor(),
                                     clearCranCache = FALSE,
                                     verbose = getOption("Require.verbose")) {
  out <- cachePkgDir(create = FALSE)
  if (!identical(Rversion, versionMajorMinor())) {
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
      # toDelete <- indivFiles[pkgNamesInFiles %in% packages]
      toDelete <- unlist(grepV(packages, indivFiles, value = TRUE))
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


#' @export
#' @rdname clearRequire
clearRequirePackageCache <- cacheClearPackages

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


installedVersionOKPrecise <- function(pkgDT, libPaths) {
  # pkgload steals system.file but fails under some conditions, not sure what...
  withCallingHandlers(
    pkgDT[, localFiles := base::system.file("DESCRIPTION", package = Package), by = "Package"]
    , warning = function(w) {
      if (isTRUE(any(grepl("cannot open compressed file", w$message))))
        invokeRestart("muffleWarning")
    })
  fe <- nzchar(pkgDT$localFiles)
  if (any(fe)) {
    pkgDT[fe, localRepo := DESCRIPTIONFileOtherV(localFiles, "RemoteRepo")]
    pkgDT[fe, localUsername := DESCRIPTIONFileOtherV(localFiles, "RemoteUsername")]
    pkgDT[fe, localBranch := DESCRIPTIONFileOtherV(localFiles, "RemoteRef")]
    pkgDT[fe, installedSha := DESCRIPTIONFileOtherV(localFiles, "RemoteSha")]
  }
  if (any(!fe)) {
    pkgDT[!fe, localRepo := NA]
    pkgDT[!fe, localUsername := NA]
    pkgDT[!fe, localBranch := NA]
    pkgDT[!fe, installedSha := NA]
  }

  pkgDT <- installedVers(pkgDT, libPaths = libPaths)

  if (isTRUE(any(pkgDT$installed %in% TRUE))) {
    brOrSha <- pkgDT$Branch == pkgDT$localBranch |
      pkgDT$Branch == pkgDT$installedSha

    installedNotOK <- pkgDT$Account != pkgDT$localUsername |
      pkgDT$Repo != pkgDT$localRepo |
      brOrSha %in% FALSE

  } else {
    installedNotOK <- NA
  }
  set(pkgDT, NULL, "installedVersionOK", installedNotOK %in% FALSE)
  pkgDT
}


toPkgDepDT <- function(packageFullName, neededFromDESCRIPTION, pkg, verbose) {
  if (missing(pkg))
    pkg <- packageFullName

  pkgDepDT <- toPkgDT(packageFullName)
  if (missing(neededFromDESCRIPTION)) {
    neededFromDESCRIPTION <- pkgDepDT$packageFullName
  }
  if (NROW(pkgDepDT)) {
    pkgDepDT <- parsePackageFullname(pkgDepDT, sorted = FALSE)
    pkgDepDT <- parseGitHub(pkgDepDT)
    set(pkgDepDT, NULL, "isGitPkg", !is.na(pkgDepDT$githubPkgName))
    # pkgDepDT[, isGitPkg := !is.na(pkgDepDT$githubPkgName)]
    # pkgDepDT <- trimRedundancies(pkgDepDT, repos = repos, purge = FALSE)
    setorderv(pkgDepDT, "isGitPkg", order = -1)
    # pkgDepDT[, Package := extractPkgName(packageFullName)]
    # set(pkgDepDT, NULL, "versionSpec", extractVersionNumber(packageFullName))

    # Here, GitHub package specification in a DESCRIPTION file Remotes section
    #   won't have version numbering --> Need to merge the two fields



    # This shouldn't be "Version" but "versionSpec" from here
    if (any(!is.na(pkgDepDT[["versionSpec"]]))) {
      whHasVersion <- which(!is.na(pkgDepDT[["versionSpec"]]))
      set(pkgDepDT, whHasVersion, "inequality",
          extractInequality(pkgDepDT$packageFullName[whHasVersion]))
      set(pkgDepDT, NULL, "github", extractPkgGitHub(pkgDepDT$packageFullName))
      if (any(pkgDepDT$isGitPkg == TRUE &
              !is.na(pkgDepDT[["versionSpec"]]))) {
        pkgDepDT[isGitPkg == TRUE & !is.na(versionSpec), newPackageFullName :=
                   ifelse(
                     is.na(extractVersionNumber(packageFullName)),
                     paste0(packageFullName, " (", inequality, versionSpec, ")"),
                     NA
                   )]
        whGitNeedVersion <- !is.na(pkgDepDT$newPackageFullName)
        if (any(whGitNeedVersion)) {
          pkgDepDT[whGitNeedVersion == TRUE, packageFullName := newPackageFullName]
        }
      }
    }
    dup <- duplicated(pkgDepDT, by = c("Package", "versionSpec"))
    pkgDepDT <- pkgDepDT[dup == FALSE]
    differences <-
      setdiff(setdiff(pkgDepDT$Package, .basePkgs), extractPkgName(neededFromDESCRIPTION))
    if (length(differences)) {
      messageVerbose(
        " (-- The DESCRIPTION file for ",
        pkg,
        " is incomplete; there are missing imports: ",
        paste(differences, collapse = comma),
        " --) ",
        verbose = verbose,
        verboseLevel = 1
      )
    }
  }
  pkgDepDT
}


getAvailablePackagesCheckAdditRepos <- function(pkgDepDTList2, pkgDepDT, repos, verbose, type, ap = NULL) {
#
  anyNewAdditionalRepositories <-
    unlist(lapply(pkgDepDTList2, function(dt)
      if (is.null(dt$Additional_repositories)) NULL else unique(dt$Additional_repositories)))
  if (!is.null(anyNewAdditionalRepositories) || is.null(ap)) {


    isDT <- vapply(pkgDepDTList2, is.data.table, FUN.VALUE = logical(1))
    if (any(isDT)) {
      if (any(!isDT))
        pkgDepDTList2 <- pkgDepDTList2[isDT]
      if (missing(pkgDepDT))
        pkgDepDT <- rbindlist(pkgDepDTList2, fill = TRUE, use.names = TRUE)
      additionalRepos <- lapply(pkgDepDTList2, function(DT)
        if (!is.null(DT$Additional_repositories)) unique(DT$Additional_repositories) else NULL)
      uniqueAdditionalRepos <- vapply(additionalRepos, function(ar) paste(sort(ar), collapse = "__"), FUN.VALUE = character(1))
      uniques <- as.integer(factor(paste0(seq(uniqueAdditionalRepos), uniqueAdditionalRepos)))
      hasAdditionalRepos <- vapply(additionalRepos, function(ar) NROW(ar) > 0, FUN.VALUE = logical(1))
      if (sum(hasAdditionalRepos)) {
        ap <- lapply(uniques, function(un) {
          DT <- rbindlist(pkgDepDTList2[un], fill = TRUE, use.names = TRUE)
          repos <- c(repos, na.omit(unique(DT$Additional_repositories)))
          needPurge <- FALSE
          for (i in 1:2) {
            ap <-
              getAvailablePackagesIfNeeded(DT, repos, purge = needPurge, verbose, type)
            if (!is.null(ap)) {
              repo1 <- noHttp(repos)
              haveRepos <- unlist(lapply(repo1, function(re) any(startsWith(noHttp(unique(ap$Repository)), re))))
              needPurge <- !all(haveRepos)
            }
            if (needPurge %in% FALSE)
              break
          }
          ap
        })
        ap <- unique(rbindlist(ap, use.names = TRUE, fill = TRUE))
      } else {
        if (is.null(ap))
          ap <-
            getAvailablePackagesIfNeeded(pkgDepDT, repos, purge = FALSE, verbose, type)
      }
    }
  }
  ap
}



getArchiveDetailsInnerMemoise <- function(...) {
  if (getOption("Require.useMemoise", TRUE)) {
    dots <- list(...)
    if (is.null(envPkgDepArchiveDetailsInner())) {
      envPkgDepArchiveDetailsInnerCreate()
    }
    ret <- NULL
    ss <- match.call(definition = getArchiveDetailsInner)
    Package <- eval(ss$Package, envir = parent.frame())
    if (!exists(Package, envir = envPkgDepArchiveDetailsInner(), inherits = FALSE)) {
      assign(Package, list(), envir = envPkgDepArchiveDetailsInner())
      # envPkgDepArchiveDetailsInner()[[Package]] <- list()
    } else {
      if (length(envPkgDepArchiveDetailsInner()[[Package]]) > 0) {
        # This is trigger
        prevInOuts <- envPkgDepArchiveDetailsInner()[[Package]][[2]]
        whIdent <- identical(prevInOuts$input, dots[[2]])
        if (any(whIdent)) {
          ret <- prevInOuts$output
        }
      }

    }
    if (is.null(ret)) {
      # inputs <- dots[[2]]
      inputs <- data.table::copy(dots[[2]])  # just take ava argument -- it has everything that is relevant
      ret <- getArchiveDetailsInner(...)
      assign(Package,
             list(envPkgDepArchiveDetailsInner()[[Package]], list(input = inputs, output = ret)),
             envir = envPkgDepArchiveDetailsInner())
      # envPkgDepArchiveDetailsInner()[[Package]] <-
      #   list(envPkgDepArchiveDetailsInner()[[Package]], list(input = inputs, output = ret))
    }
  } else {
    ret <- getArchiveDetailsInner(...)
  }

  return(ret)
}

noHttp <- function(url) {
  gsub("^http.*://", "", url)
}


getVersionOptionPkgEnv <- function(psnNoVersion, verNum, inequ) {
  pat <- paste0(psnNoVersion)
  pat <- gsub("[[:punct:]]| ", "_", pat)
  nams <- names(as.list(envPkgDepDeps()))
  if (!is.null(nams)) {
    versions <- Map(pa = pat, ver = verNum, ineq = inequ, #MoreArgs = list(nam = nams),
                function(pa, ver, ineq) {
                  sw <- startsWith(prefix = pa, nams)
                  if (any(sw)) {
                    # if (all(sw %in% FALSE))
                    #   sw <- grep(pa, nams)
                    nams <- nams[sw]
                    # nams <- nams[have]

                    poss <- get0(nams, envir = envPkgDepDeps())
                    versions <- extractVersionNumber(names(poss))
                    # have <- ls(envPkgDepDeps(), pattern = pat) # TOO SLOW APPARENTLY
                    # versions <- gsub(paste0("^", pa, "\\_\\_*([[:alnum:]]{40})|[(.+)\\_\\_+].+$"), "\\1", nams)
                    if (!is.null(ineq)) {
                      if (!is.na(ineq)) {
                        if (length(versions) > 0) {
                          okVers <- compareVersion2(gsub("_", "\\.", versions),
                                                    versionSpec = ver, inequality = ineq)
                          if (any(!okVers)) {
                            versions[!okVers] <- ""
                          }
                        }
                      }
                    }
                    noSavedOption <- nchar(versions) == 0
                    if (!is.null(ver))
                      if (any(noSavedOption))
                        versions[noSavedOption] <- verNum[noSavedOption]
                    if (length(versions) > 1)
                      versions <- tail(versions, 1)
                    versions
                  } else {
                    NULL
                  }
                })
  } else {
    versions <- NULL
  }

  versions

}




#' @description
#' `pkgDep2` is a convenience wrapper of `pkgDep` that "goes one level in",
#' i.e., the first order dependencies, and runs the `pkgDep` on those.
#' @rdname pkgDep
#' @export
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
pkgDep2 <- function(...) {
  dots <- list(...)
  dots$recursive <- FALSE
  forms <- formals(pkgDep)
  if (is.null(dots$simplify)) dots$simplify <- forms$simplify
  # dots$simplify <- FALSE
  deps <- do.call(pkgDep, dots)
  dots[[1]] <- NULL
  dots$recursive <- TRUE
  if (isTRUE(dots$simplify %in% FALSE)) {
    deps1 <- lapply(deps$deps, function(dep) do.call(pkgDep, append(list(dep$packageFullName), dots)))
  } else {
    deps1 <- lapply(deps, function(dep) do.call(pkgDep, append(list(dep), dots)))
  }
  deps1
}




purgeAvailablePackages <- function(repos, purge = FALSE) {
  if (isTRUE(purge)) {
    fn <- availablePackagesCachedPath(repos = repos, type = c("source", "binary"))
    fExists <- file.exists(fn)
    if (any(fExists)) {
      unlink(fn[fExists])
    }
  }
  purge
}

