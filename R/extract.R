#' Extract info from package character strings
#'
#' Cleans a character vector of non-package name related information (e.g., version)
#'
#' @param pkgs A character string vector of packages with or without GitHub path or versions
#' @return Just the package names without extraneous info.
#' @seealso \code{\link{trimVersionNumber}}
#' @export
#' @rdname extractPkgName
#' @examples
#' extractPkgName("Require (>=0.0.1)")
extractPkgName <- function(pkgs) {
  hasNamesAny <- !is.null(names(pkgs))
  if (hasNamesAny) {
    browser(expr = exists("aaaaa"))
    hasNames <- nchar(names(pkgs)) > 0
    pkgs[hasNames] <- names(pkgs)[hasNames]
    pkgs <- unname(pkgs)
  }

  pkgNames <- trimVersionNumber(pkgs)
  withGitName <- extractPkgGitHub(pkgNames)
  isNAWithGitName <- is.na(withGitName)
  if (any(!isNAWithGitName)) {
    stripped <- unlist(lapply(strsplit(pkgNames[!isNAWithGitName], split = "/|@"), function(x) x[2]))
    pkgNames[!isNAWithGitName] <- stripped
  }
  pkgNames
}

#' @rdname extractPkgName
#' @export
#' @examples
#' extractVersionNumber(c("Require (<=0.0.1)", "PredictiveEcology/Require@development (<=0.0.4)"))
extractVersionNumber <- function(pkgs) {
  if (!missing(pkgs)) {
    hasVersionNum <- grepl(grepExtractPkgs, pkgs, perl = FALSE)
    out <- rep(NA, length(pkgs))
    out[hasVersionNum] <- gsub(grepExtractPkgs, "\\2", pkgs[hasVersionNum], perl = FALSE)
  } else {
    out <- character()
  }
  out
}

#' @rdname extractPkgName
#' @export
#' @examples
#' extractInequality("Require (<=0.0.1)")
extractInequality <- function(pkgs) {
  gsub(grepExtractPkgs, "\\1", pkgs, perl = FALSE)
}

#' @rdname extractPkgName
#' @export
#' @examples
#' extractPkgGitHub("PredictiveEcology/Require")
extractPkgGitHub <- function(pkgs) {
  isGH <- grepl("/", pkgs, perl = FALSE)
  if (any(isGH)) {
    a <- trimVersionNumber(pkgs[isGH])
    a <- strsplit(a, split = "/|@")
    a <- lapply(a, function(x) x[2])
    pkgs[isGH] <- unlist(a)
    if (any(!isGH))
      pkgs[!isGH] <- NA
  } else {
    pkgs <- rep(NA, length(pkgs))
  }
  pkgs
  #unlist(lapply(strsplit(trimVersionNumber(pkgs), split = "/|@"), function(x) x[2]))
}

#' Trim version number off a compound package name
#'
#' The resulting string(s) will have only name (including github.com repository if it exists).
#'
#' @inheritParams extractPkgName
#'
#' @rdname trimVersionNumber
#' @seealso \code{\link{extractPkgName}}
#' @export
#' @examples
#' trimVersionNumber("PredictiveEcology/Require (<=0.0.1)")
trimVersionNumber <- function(pkgs) {
  if (!is.null(pkgs)) {
    nas <- is.na(pkgs)
    if (any(!nas)) {
      ew <- endsWith(pkgs[!nas], ")")
      if (any(ew)) {
        pkgs[!nas][ew] <- gsub(paste0("\n|\t|",.grepVersionNumber), "", pkgs[!nas][ew])
      }
    }
    pkgs
  }
}

.grepVersionNumber <- " *\\(.*"

grepExtractPkgs <- ".*\\([ \n\t]*(<*>*=*)[ \n\t]*(.*)\\)"

.grepR <- "^ *R( |\\(|$)"
