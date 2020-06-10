#' Determine package dependencies
#'
#' This will first look in local filesystem (in \code{.libPaths()}), then
#' \code{CRAN}. If the package is in the form of a GitHub package with format
#' Account/Repo@branch, it will attempt to get package dependencies from the
#' GitHub DESCRIPTION file. Currently, it will not find \code{Remotes}.
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
#' pkgDep("Require", keepVersionNumber = FALSE) # just names
#' \dontrun{
#'   pkgDep("PredictiveEcology/reproducible") # GitHub
#'   pkgDep("PredictiveEcology/reproducible", recursive = TRUE) # GitHub
#'   pkgDep(c("PredictiveEcology/reproducible", "Require")) # GitHub package and local packages
#'   pkgDep(c("PredictiveEcology/reproducible", "Require", "plyr")) # GitHub, local, and CRAN packages
#' }
pkgDep <- function(packages, libPath = .libPaths(),
                   which = c("Depends", "Imports", "LinkingTo"), recursive = FALSE,
                   depends, imports, suggests, linkingTo,
                   repos = getCRANrepos(),
                   keepVersionNumber = TRUE, includeBase = FALSE,
                   sort = TRUE, purge = getOption("Require.purge", FALSE)) {

  if (any(!missing(depends), !missing(linkingTo), !missing(imports), !missing(suggests))) {
    message("Please use 'which' instead of 'imports', 'suggests', 'depends' and 'linkingTo'")
    if (!missing(depends)) depends <- TRUE
    if (!missing(imports)) imports = TRUE
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

    neededFull <- pkgDepInner(packages[needGet], libPath, which[[1]], keepVersionNumber, purge = purge)
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
            pkgsNew[[i]] <- lapply(trimVersionNumber(pkgsNew[[i-1]]), function(needed) {
              unique(unlist(pkgDepInner(needed, libPath, which, keepVersionNumber, purge = purge)))
            })
            pkgsNew[[i]] <- unique(unlist(lapply(pkgsNew[[i]], trimVersionNumber)))
            pkgsNew[[i]] <- setdiff(pkgsNew[[i]], unlist(pkgsNew[1:(i-1)])) # don't redo ones that are already in the list
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
                        purge = getOption("Require.purge", FALSE)) {
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
      } else { #if (any("CRAN" %in% pkgDT$repoLocation)) {
        needed <- unname(unlist(pkgDepCRAN(pkg, #recursive = FALSE,
                                           which = which, keepVersionNumber = keepVersionNumber,
                                           purge = purge)))
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
                     imports = imports, suggests = suggests,
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

pkgDepCRAN <- function(pkg, which = c("Depends", "Imports", "LinkingTo"), #recursive = FALSE,
                       keepVersionNumber = TRUE, repos = getCRANrepos(),
                       purge = getOption("Require.purge", FALSE)) {
  cachedAvailablePackages <- if (!exists("cachedAvailablePackages", envir = .pkgEnv) || isTRUE(purge)) {
    isOldMac <- (Sys.info()[["sysname"]] == "Darwin" &&
                    compareVersion(as.character(getRversion()), "4.0.0") < 0)
    aa <- available.packages(repos = repos, ignore_repo_cache = isOldMac)
    assign("cachedAvailablePackages", aa, envir = .pkgEnv)
    aa
  } else {
    get("cachedAvailablePackages", envir = .pkgEnv, inherits = FALSE)
  }

  capFull <- as.data.table(cachedAvailablePackages)
  deps <- pkgDepCRANInner(capFull, which = which, pkgs = pkg, keepVersionNumber = keepVersionNumber)
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

.installed.pkgs <- function(lib.loc = .libPaths()) {
  out <- lapply(lib.loc, function(path) {
    dirs <- dir(path, full.names = TRUE)
    areDirs <- dir.exists(dirs)
    dirs <- dirs[areDirs]
    files <- file.path(dirs, "DESCRIPTION")
    filesExist <- file.exists(files)
    files <- files[filesExist]
    versions <- unlist(lapply(files, function(file) DESCRIPTIONFileVersion(file)))
    cbind("Package" = dirs[filesExist], "Version" = versions)
  })
  c("Package", "LibPath", "Version")
  lengths <- unlist(lapply(out, function(x) NROW(x)))
  out <- do.call(rbind, out)
  cbind("Package" = basename(unlist(out[, "Package"])), "LibPath" = rep(lib.loc, lengths),
        "Version" = out[, "Version"])
}

.basePkgs <- unlist(.installed.pkgs(tail(.libPaths(), 1))[, "Package"])

