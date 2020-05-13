#' Determine package dependencies, first looking at local filesystem
#'
#' This is intended to replace \code{tools::package_dependencies} or
#' \code{pkgDep} in the \pkg{miniCRAN} package, but with modifications for speed.
#' It will first check local package directories in \code{libPath}, and it if
#' the function cannot find the packages there, then it will use
#' \code{tools::package_dependencies}.
#'
#' @note \code{package_dependencies} and \code{pkgDep} will differ under the following
#' circumstances:
#' \enumerate{
#'   \item GitHub packages are not detected using \code{tools::package_dependencies};
#'   \item \code{tools::package_dependencies} does not detect the dependencies of base packages
#'     among themselves, \emph{e.g.}, \code{methods} depends on \code{stats} and \code{graphics}.
#' }
#'
#' @inheritParams Require
#' @param depends Logical. Include packages listed in "Depends". Default \code{TRUE}.
#' @param imports Logical. Include packages listed in "Imports". Default \code{TRUE}.
#' @param suggests Logical. Include packages listed in "Suggests". Default \code{FALSE}.
#' @param linkingTo Logical. Include packages listed in "LinkingTo". Default \code{TRUE}.
#' @param recursive Logical. Should dependencies of dependencies be searched, recursively.
#'                  NOTE: Dependencies of suggests will not be recursive. Default \code{TRUE}.
#' @param keepVersionNumber Logical. If \code{TRUE}, then the package dependencies returned
#'   will include version number. Default is \code{FALSE}
#' @param refresh There is an internal type of caching. If the results are wrong, likely
#'   set \code{refresh = TRUE}.
#' @export
#' @rdname pkgDep
#'
#' @examples
#' pkgDep("Require")
pkgDep <- function(packages, libPath = .libPaths(),
                   which = c("Depends", "Imports", "LinkingTo"), recursive = FALSE,
                   depends, imports, suggests, linkingTo,
                   repos = getCRANrepos(),
                   keepVersionNumber = TRUE) {

  if (any(!missing(depends), !missing(linkingTo), !missing(imports), !missing(suggests))) {
    message("Please use 'which' instead of 'imports', 'suggests', 'depends' and 'linkingTo'")
    if (!missing(depends)) {
      depends <- TRUE
    }
    if (!missing(imports)) {
      imports = TRUE
    }
    if (!missing(suggests)) {
      suggests <- TRUE
    }
    if (!missing(linkingTo)) {
      linkingTo <- TRUE
    }
  }

  names(packages) <- packages
  desc_paths <- lapply(packages, function(pkg) {
    dp <- sprintf("%s/%s/DESCRIPTION", libPath, pkg) # nolint
    fe <- file.exists(dp)
    dp[fe][1] # take first file that exists
  })

  Map(desc_path = desc_paths, pkg = packages, function(desc_path, pkg) {
    if (!file.exists(desc_path)) {
      pkgDT <- parseGitHub(pkg)
      if ("GitHub" %in% pkgDT$repoLocation) {
        pkgDT <- getGitHubDESCRIPTION(pkgDT)
        needed <- DESCRIPTIONFileDeps(pkgDT$DESCFile)
      } else if (any("CRAN" %in% pkgDT$repoLocation)) {
        needed <- unname(unlist(pkgDepCRAN(pkg, recursive = recursive, which = which, keepVersionNumber = keepVersionNumber)))
      } else {
        stop("Can only get package dependencies from CRAN and GitHub")
      }

    } else {
      needed <- DESCRIPTIONFileDeps(desc_path, which = which, keepVersionNumber = keepVersionNumber)
    }
    needed
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
#' pkgDep2("reproducible")
pkgDep2 <- function(packages, recursive = TRUE,
                    which = c("Depends", "Imports", "LinkingTo"),
                    depends, imports, suggests, linkingTo,
                    repos = getOption("repos"), refresh = FALSE,
                    verbose = getOption("reproducible.verbose"),
                    sorted = TRUE) {
  a <- lapply(pkgDep(packages, recursive = FALSE, which = which, depends = depends, imports = imports, suggests = suggests,
                     linkingTo = linkingTo)[[1]],
              recursive = recursive,
              pkgDep, depends = depends, imports = imports, suggests = suggests,
              linkingTo = linkingTo
  )
  a <- unlist(a, recursive = FALSE)
  if (sorted) {
    ord <- order(sapply(a, function(x) length(x)), decreasing = TRUE)
    a <- a[ord]
  }
  return(a)
}

pkgDepCRAN <- function(pkg, which = c("Depends", "Imports", "LinkingTo"), recursive = FALSE,
                       keepVersionNumber = TRUE, repos = getCRANrepos()) {
  cachedAvailablePackages <- if (!exists("cachedAvailablePackages", envir = .pkgEnv)) {
    aa <- available.packages(repos = repos)
    assign("cachedAvailablePackages", aa, envir = .pkgEnv)
    aa
  } else {
    .pkgEnv$cachedAvailablePackages
  }

  capFull <- as.data.table(cachedAvailablePackages)
  deps <- pkgDepCRANInner(capFull, which = which, pkgs = pkg, keepVersionNumber = keepVersionNumber)
  if (recursive) {
    i <- 1
    pkgsNew <- list()
    pkgsNew[[i]] <- deps
    while (length(unlist(pkgsNew[[i]])) > 0) {
      i <- i + 1
      pkgsNew[[i]] <- lapply(pkgsNew[[i-1]], function(deps) {
        deps1 <- pkgDepCRANInner(capFull, which = which, pkgs = deps, keepVersionNumber = keepVersionNumber)
        unique(unlist(deps1))
      })
    }
    ss <- seq_along(deps)
    names(ss) <- names(deps)
    deps <- lapply(ss, function(x) {
      out <- unique(unname(unlist(lapply(pkgsNew, function(y) y[[x]]))))
      if (isFALSE(keepVersionNumber))
        out <- trimVersionNumber(out)
      out
      })
  }
  deps
}

pkgDepCRANInner <- function(ap, which, pkgs, keepVersionNumber) {
  # MUCH faster to use base "ap$Package %in% pkgs" than data.table internal "Package %in% pkgs"
  pkgsNoVersion <- trimVersionNumber(pkgs)
  if (isFALSE(keepVersionNumber))
    pkgs <- trimVersionNumber(pkgs)
  ap <- ap[ap$Package %in% pkgsNoVersion]
  keep <- na.omit(match(ap$Package, pkgsNoVersion))
  pkgsNoVersion1 <- pkgsNoVersion[keep]
  pkgs <- pkgs[keep]
  ap <- ap[order(pkgsNoVersion1)]

  names(which) <- which
  deps <- lapply (which, function(i) {
    lapply(ap[[i]], function(x) {
      out <- strsplit(x, split = "(, {0,1})|(,\n)")[[1]]
      out <- na.omit(out)
      out <- grep("^\\<R\\>", out, value = TRUE, invert = TRUE) # remove references to R
    })
  })

  ss <- seq_along(pkgsNoVersion1)
  names(ss) <- pkgsNoVersion1
  deps <- lapply(ss, function(x) unname(unlist(lapply(deps, function(y) y[[x]]))))

}






pkgDepOld <- function(packages, libPath, recursive = TRUE, depends = TRUE,
                      imports = TRUE, suggests = FALSE, linkingTo = TRUE,
                      topoSort = FALSE, repos = getOption("repos"), refresh = FALSE,
                      verbose = getOption("Require.verbose", FALSE),
                      keepVersionNumber = FALSE) {
  if (all(c(!depends, !imports, !suggests, !linkingTo))) {
    names(packages) <- packages
    needed <- lapply(packages, function(x) character())
    return(needed)
  }
  typeString <- paste("depends"[depends], "imports"[imports],
                      "suggests"[suggests], "linkingTo"[linkingTo], sep = "_")
  if (isTRUE(refresh)) {
    .pkgEnv$.depsAll[["recursive"]][[typeString]] <- NULL
    .pkgEnv$.depsAll[["nonRecursive"]][[typeString]] <- NULL
  }

  if (missing(libPath) || is.null(libPath)) {
    libPath <- .libPaths()#[1L]
  }

  if (length(libPath) > 1) {
    ans <- list()
    # Using loop next allows the ability to break out of search
    #  if initial .libPaths have the package
    for (lp in libPath) {
      # message("  Searching in ", lp)
      ans1 <- pkgDepOld(packages, lp, recursive = recursive,
                        depends = depends, imports = imports, suggests = suggests,
                        linkingTo = linkingTo,
                        refresh = FALSE)
      ans <- append(ans, list(ans1))
      if (all(unlist(lapply(ans, function(x) all(unlist(lapply(x, is.character))))))) {
        break
      }
    }
    if (length(packages) == 1) {
      ans <- list(ans)
    } else {
      #invert the list, so by package name
      ans <- lapply(names(ans[[1]]), function(nam) {
        ll2 <- lapply(ans, function(x) x[[nam]])
      })
    }
    names(ans) <- packages

    ll2 <- lapply(ans, function(x) {
      ll1 <- unique(na.omit(unlist(x)))
      attr(ll1, "na.action") <- NULL
      attr(ll1, "class") <- NULL
      ll1
    })

    # package_dependencies and pkgDepOld will differ under the following circumstances
    # 1. github packages are not detected using tools::package_dependencies
    # 2. package_dependencies does not detect the dependencies of base packages,
    #    e.g,. methods depends on stats and graphics
    notInstalled <- unlist(lapply(ll2, function(y) length(y) == 0 & is.logical(y)))
    ll2[notInstalled] <- NA
    if (any(notInstalled)) {

      paste(names(ll2[notInstalled]), collapse = ", ")
      repos <- getCRANrepos(repos)

      #if (!is.memoised(available.packagesMem)) {
      #  assignInMyNamespace("available.packagesMem", memoise(available.packages, ~timeout(360))) # nolint
      #}

      parentFramePackages <- tryCatch(get("packages", envir = parent.frame()), error = function(x) NULL)

      if (!is.null(parentFramePackages))
        message(paste(parentFramePackages, collapse = ", "), " dependencies: ")
      message("  ", paste(names(ll2[notInstalled]), collapse = ", "),
              " not installed locally; check for dependencies on CRAN")
      #availPackagesDb <- available.packagesMem(repos = repos)
      availPackagesDb <- available.packages(repos = repos)

      ll3 <- pkgDepCRAN(names(ll2[notInstalled]), # db = availPackagesDb,
                        recursive = recursive)
      #ll3 <- package_dependencies(names(ll2[notInstalled]), db = availPackagesDb,
      #                               recursive = recursive)
      #ll3 <- package_dependenciesMem(names(ll2[notInstalled]), db = availPackagesDb,
      #                               recursive = recursive)
      if (recursive) {
        .pkgEnv$.depsAll[["recursive"]][[typeString]] <- append(.pkgEnv$.depsAll[["recursive"]][[typeString]], ll3)
      } else {
        .pkgEnv$.depsAll[["nonRecursive"]][[typeString]] <- append(.pkgEnv$.depsAll[["nonRcursive"]][[typeString]], ll3)
      }
      # the previous line will miss base packages
      ll3 <- lapply(ll3, function(x) {
        unique(c(x, unlist(pkgDepOld(x, libPath = unique(c(libPath, .libPaths())),
                                     recursive = recursive,
                                     depends = depends, imports = imports, suggests = FALSE, # don't propagate suggests
                                     linkingTo = linkingTo,
                                     refresh = FALSE))))
      })

      ll2[notInstalled] <- ll3
    }
    if (isTRUE(topoSort)) {
      browser(expr = exists("._pkgDep_1", envir = .GlobalEnv))
      needed <- names(ll2)
      names(needed) <- needed
      ll2 <- pkgDepTopoSort(needed, deps = ll2)
    }

    return(ll2)
  }

  if (length(packages) > 1) {
    if (length(packages) == length(libPath)) {
      ans <- lapply(seq_along(packages), function(x) pkgDepOld(packages[x], libPath[x],
                                                               recursive = recursive,
                                                               depends = depends, imports = imports, suggests = suggests,
                                                               linkingTo = linkingTo,
                                                               refresh = FALSE))
    } else {
      ans <- lapply(packages, pkgDepOld, libPath, recursive = recursive,
                    depends = depends, imports = imports, suggests = suggests,
                    linkingTo = linkingTo,
                    refresh = FALSE)
    }
    names(ans) <- packages
    return(ans)
  } else if (length(packages) == 0)  {
    return(character())
  }

  if (recursive) {
    if (isTRUE(packages %in% names(.pkgEnv$.depsAll[["recursive"]][[typeString]]))) {
      if (!is.null(.pkgEnv$.depsAll[["recursive"]][[typeString]][[packages]])) {
        return(.pkgEnv$.depsAll[["recursive"]][[typeString]][[packages]])
      }
    }
  } else {
    if (isTRUE(packages %in% names(.pkgEnv$.depsAll[["nonRecursive"]][[typeString]]))) {
      if (!is.null(.pkgEnv$.depsAll[["nonRecursive"]][[typeString]][[packages]]))
        return(.pkgEnv$.depsAll[["nonRecursive"]][[typeString]][[packages]])
    }
  }

  desc_path <- sprintf("%s/%s/DESCRIPTION", libPath, packages) # nolint
  if (!file.exists(desc_path)) {
    return(NA)
  } else {
    lines <- readLines(desc_path)
    Sys.setlocale(locale = "C") # required to deal with non English characters in Author names
    on.exit(Sys.setlocale(locale = ""))
    deps_line <- grep("^Depends: *", lines) # nolint
    sugg_line <- grep("^Suggests: *", lines) # nolint
    imports_line <- grep("^Imports: *", lines) # nolint
    linkingTo_line <- grep("^LinkingTo: *", lines) # nolint
    colon_line <- grep(": *", lines) # nolint

    needed <- character()
    if (imports) {
      if (length(imports_line)) {
        imports_lines <- imports_line:(colon_line[which(colon_line %in% imports_line) + 1] - 1) # nolint
        imprts <- paste(lines[imports_lines], collapse = "")
        imprts <- gsub("Imports: ", "", imprts)
        imprts <- strsplit(imprts, split = ", *")
        needed <- c(needed, imprts[[1]])
      }}

    if (depends) {
      if (length(deps_line)) {
        deps_lines <- deps_line:(colon_line[which(colon_line %in% deps_line) + 1] - 1) # nolint
        deps <- paste(lines[deps_lines], collapse = "")
        deps <- gsub("Depends: ", "", deps)
        deps <- strsplit(deps, split = ", *")
        needed <- c(needed, deps[[1]])
      }}

    if (suggests) {
      if (length(sugg_line)) {
        sugg_lines <- sugg_line:(colon_line[which(colon_line %in% sugg_line) + 1] - 1) # nolint
        sugg <- paste(lines[sugg_lines], collapse = "")
        sugg <- gsub("Suggests: ", "", sugg)
        sugg <- strsplit(sugg, split = ", *")
        needed <- c(needed, sugg[[1]])
      }}

    if (linkingTo) {
      if (length(linkingTo_line)) {
        linkingTo_lines <- linkingTo_line:(colon_line[which(colon_line %in% linkingTo_line) + 1] - 1) # nolint
        link <- paste(lines[linkingTo_lines], collapse = "")
        link <- gsub("LinkingTo: ", "", link)
        link <- strsplit(link, split = ", *")
        needed <- c(needed, link[[1]])
      }}

    needed <- grep("^R[\\( ]", needed, value = TRUE, invert = TRUE)

    if (length(needed)) {
      # hasVersionNumber <- regmatches(needed, gregexpr(pattern = "(?<=\\().*?(?=\\))",
      #                                                 needed, perl = TRUE))[[1]]
      hasVersionNumber <- unlist(lapply(needed, function(x) {
        regmatches(x, gregexpr(pattern = "(?<=\\().*?(?=\\))", x, perl = TRUE))[[1]]
      }))
      if (!isTRUE(keepVersionNumber))
        if (length(hasVersionNumber)) {
          for (pat in hasVersionNumber) {
            needed <- sub(pattern = paste0("\\(", pat, "\\)"), needed, replacement = "")
          }
          needed <- gsub(needed, pattern = " *", replacement = "")
        }
    }

    if (recursive) {
      # note that recursive searching must search in all libPaths, not just current one
      # like miniCRAN::pkgDep not recursive on Suggests
      if (verbose) message(packages)
      needed <- unique(needed)
      namesSP <- names(.pkgEnv$.depsAll[["nonRecursive"]][[typeString]])
      oldNeeded <- character()
      if (!is.null(namesSP)) {
        oldNeeded <- unlist(needed[needed %in% namesSP])
        needed <- needed[!needed %in% namesSP]
      }
      if (verbose) {
        if (length(needed) > 0)
          message("      Recursive: ", paste(needed, collapse = ","))
        if (length(oldNeeded) > 0)
          message("        Skipped: ", paste(oldNeeded, collapse = ","))
      }
      .packages <- list(character())
      names(.packages) <- packages
      names(needed) <- needed
      .needed <- lapply(needed, function(x) NULL)
      .pkgEnv$.depsAll[["nonRecursive"]][[typeString]] <- c(.pkgEnv$.depsAll[["nonRecursive"]][[typeString]], .needed)
      .pkgEnv$.depsAll[["nonRecursive"]][[typeString]][[packages]] <- unique(c(needed, oldNeeded))
      .pkgEnv$.depsAll[["recursive"]][[typeString]] <- c(.pkgEnv$.depsAll[["recursive"]][[typeString]], .needed)

      if (length(needed) > 0) {
        uniqueLibPaths <- unique(c(libPath, .libPaths()))
        needed2 <- pkgDepOld(needed, libPath = uniqueLibPaths, recursive = recursive,
                             depends = depends, imports = imports, suggests = FALSE,
                             linkingTo = linkingTo,
                             refresh = FALSE)
        needed <- na.omit(unique(c(needed, unlist(needed2)))) # collapses recursive on non-recursive
      }
      if (length(oldNeeded) > 0) { # just because we don't need to find its depenencies, doesn't mean it isn't needed
        needed <- unique(c(needed, oldNeeded, unlist(.pkgEnv$.depsAll[["recursive"]][[typeString]][oldNeeded])))
      }
      .pkgEnv$.depsAll[["recursive"]][[typeString]][[packages]] <- needed # recursive
      attr(needed, "na.action") <- NULL
      attr(needed, "class") <- NULL
      #}
    }
    return(needed)
  }
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
  sl[["colon"]] <- grep(": *", lines) # nolint

  whichLower <- tolower(which)
  needed1 <- Map(whLower = whichLower, wh = which, function(whLower, wh) {
    if (length(sl[[whLower]])) {
      allLines <- sl[[whLower]]:(sl[["colon"]][which(sl[["colon"]] %in% sl[[whLower]]) + 1] - 1) # nolint
      needs <- paste(lines[allLines], collapse = "")
      needs <- gsub(paste0(wh, ": *"), "", needs)
      needs <- strsplit(needs, split = ", *")
      if (isFALSE(keepVersionNumber))
        needs[[1]] <- trimVersionNumber(needs[[1]])
      needs[[1]]
    }
  })
  needed1 <- unname(unlist(needed1))


  needed <- character()
  deps_line <- grep("^Depends: *", lines) # nolint
  sugg_line <- grep("^Suggests: *", lines) # nolint
  imports_line <- grep("^Imports: *", lines) # nolint
  linkingTo_line <- grep("^LinkingTo: *", lines) # nolint
  colon_line <- grep(": *", lines) # nolint
  if ("imports" %in% whichLower) {
    if (length(imports_line)) {
      imports_lines <- imports_line:(colon_line[which(colon_line %in% imports_line) + 1] - 1) # nolint
      imprts <- paste(lines[imports_lines], collapse = "")
      imprts <- gsub("Imports: *", "", imprts)
      imprts <- strsplit(imprts, split = ", *")
      if (isFALSE(keepVersionNumber))
        imprts[[1]] <- trimVersionNumber(imprts[[1]])
      needed <- c(needed, imprts[[1]])
    }}

  if ("depends" %in% whichLower) {
    if (length(deps_line)) {
      deps_lines <- deps_line:(colon_line[which(colon_line %in% deps_line) + 1] - 1) # nolint
      deps <- paste(lines[deps_lines], collapse = "")
      deps <- gsub("Depends: *", "", deps)
      deps <- strsplit(deps, split = ", *")
      if (isFALSE(keepVersionNumber))
        deps[[1]] <- trimVersionNumber(deps[[1]])
      needed <- c(needed, deps[[1]])
    }}

  if ("suggests" %in% whichLower) {
    if (length(sugg_line)) {
      sugg_lines <- sugg_line:(colon_line[which(colon_line %in% sugg_line) + 1] - 1) # nolint
      sugg <- paste(lines[sugg_lines], collapse = "")
      sugg <- gsub("Suggests: *", "", sugg)
      sugg <- strsplit(sugg, split = ", *")
      if (isFALSE(keepVersionNumber))
        sugg[[1]] <- trimVersionNumber(sugg[[1]])
      needed <- c(needed, sugg[[1]])
    }}

  if ("linkingto" %in% whichLower) {
    if (length(linkingTo_line)) {
      linkingTo_lines <- linkingTo_line:(colon_line[which(colon_line %in% linkingTo_line) + 1] - 1) # nolint
      link <- paste(lines[linkingTo_lines], collapse = "")
      link <- gsub("LinkingTo: *", "", link)
      link <- strsplit(link, split = ", *")
      if (isFALSE(keepVersionNumber))
        link[[1]] <- trimVersionNumber(link[[1]])
      needed <- c(needed, link[[1]])
    }}

  needed <- grep("^R[\\( ]", needed, value = TRUE, invert = TRUE)
  needed1 <- grep("^R[\\( ]", needed1, value = TRUE, invert = TRUE)
  stopifnot(identical(sort(needed), sort(needed1)))

  needed
}
