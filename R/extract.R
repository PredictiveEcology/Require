#' Extract info from package character strings
#'
#' Cleans a character vector of non-package name related information (e.g., version)
#'
#' @param pkgs A character string vector of packages with or without GitHub path or versions
#' @return Just the package names without extraneous info.
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
#' extractVersionNumber("Require (<=0.0.1)")
extractVersionNumber <- function(pkgs) {
  gsub(grepExtractPkgs, "\\2", pkgs)
}

#' @rdname extractPkgName
#' @export
#' @examples
#' extractInequality("Require (<=0.0.1)")
extractInequality <- function(pkgs) {
  gsub(grepExtractPkgs, "\\1", pkgs)
}

#' @rdname extractPkgName
#' @export
#' @examples
#' extractPkgGitHub("PredictiveEcology/Require")
extractPkgGitHub <- function(pkgs) {
  unlist(lapply(strsplit(trimVersionNumber(pkgs), split = "/|@"), function(x) x[2]))
}

#' @rdname extractPkgName
#' @export
#' @examples
#' trimVersionNumber("PredictiveEcology/Require (<=0.0.1)")
trimVersionNumber <- function(pkgs) {
  out <- gsub(.grepVersionNumber, "", pkgs)
  gsub("\n|\t", "", out)
}

.grepVersionNumber <- " *\\(.*"

grepExtractPkgs <- ".*\\([ \n]*(<*>*=*)[ \n]*(.*)\\)"

shortRVersion <- function(vers) {
  paste0(strsplit(as.character(vers), "[.]")[[1]][1:2], collapse = ".")
}
