#' Extract info from package character strings
#'
#' Cleans a character vector of non-package name related information (e.g., version)
#'
#' @param pkgs A character string vector of packages with or without GitHub path or versions
#' @return Just the package names without extraneous info.
#' @seealso [trimVersionNumber()]
#' @export
#' @rdname extractPkgName
#' @examples
#' extractPkgName("Require (>=0.0.1)")
extractPkgName <- function(pkgs, filenames) {
  if (!missing(pkgs)) {
    hasNamesAny <- !is.null(names(pkgs))
    if (hasNamesAny) {
      browser(expr = exists("aaaaa"))
      hasNames <- nchar(names(pkgs)) > 0
      pkgs[hasNames] <- names(pkgs)[hasNames]
      pkgs <- unname(pkgs)
    }

    pkgNames <- trimVersionNumber(pkgs)
    gitPkgs <- extractPkgGitHub(pkgNames)
    whGitPkgs <- is.na(gitPkgs)

    if (any(!whGitPkgs)) {
      pkgNames[!whGitPkgs] <- gitPkgs[!whGitPkgs]
    }
  } else {
    if (!missing(filenames)) {
      fnsSplit <- strsplit(filenames, "_")
      out <- unlist(lapply(fnsSplit, function(x) x[[1]]))
      out2 <- strsplit(out, split = "-")
      pkgNames <- unlist(Map(len = pmax(1, lengths(out2) - 1), pkg = out2, function(len, pkg) pkg[len]))
    } else {
      pkgNames <- character()
    }
  }

  pkgNames
}

#' @rdname extractPkgName
#' @param filenames Can be supplied instead of `pkgs` if it is a filename e.g., a
#'   .tar.gz or .zip that was downloaded from CRAN.
#' @export
#' @examples
#' extractVersionNumber(c(
#'   "Require (<=0.0.1)",
#'   "PredictiveEcology/Require@development (<=0.0.4)"
#' ))
extractVersionNumber <- function(pkgs, filenames) {
  if (!missing(pkgs)) {
    hasVersionNum <- grepl(grepExtractPkgs, pkgs, perl = FALSE)
    out <- rep(NA, length(pkgs))
    out[hasVersionNum] <- gsub(grepExtractPkgs, "\\2", pkgs[hasVersionNum], perl = FALSE)
  } else {
    if (!missing(filenames)) {
      fnsSplit <- strsplit(filenames, "_")
      out <- unlist(lapply(fnsSplit, function(x) gsub("\\.zip|\\.tar\\.gz|\\.tgz", "", x[[2]])))
    } else {
      out <- character()
    }
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
  isGH <- grepl("/|@", pkgs, perl = FALSE)
  if (any(isGH)) {
    a <- trimVersionNumber(pkgs[isGH])
    hasRepo <- grepl("/", a)
    hasBranch <- grepl("@", a)
    a <- strsplit(a, split = "/|@")
    a <- Map(x = a, hasRep = hasRepo, function(x, hasRep) x[1 + hasRep])
    pkgs[isGH] <- unlist(a)
    if (any(!isGH)) {
      pkgs[!isGH] <- NA
    }
  } else {
    pkgs <- rep(NA, length(pkgs))
  }
  pkgs
  # unlist(lapply(strsplit(trimVersionNumber(pkgs), split = "/|@"), function(x) x[2]))
}

#' Trim version number off a compound package name
#'
#' The resulting string(s) will have only name (including github.com repository if it exists).
#'
#' @inheritParams extractPkgName
#'
#' @rdname trimVersionNumber
#' @seealso [extractPkgName()]
#' @export
#' @examples
#' trimVersionNumber("PredictiveEcology/Require (<=0.0.1)")
trimVersionNumber <- function(pkgs) {
  if (!is.null(pkgs)) {
    nas <- is.na(pkgs)
    if (any(!nas)) {
      ew <- endsWith(pkgs[!nas], ")")
      if (any(ew)) {
        pkgs[!nas][ew] <- gsub(paste0("\n|\t|", .grepVersionNumber), "", pkgs[!nas][ew])
      }
    }
    pkgs
  }
}

rmExtraSpaces <- function(string) {
  gsub(" {2, }", " ", string)
}
.grepVersionNumber <- " *\\(.*"

grepExtractPkgs <- ".*\\([ \n\t]*(<*>*=*)[ \n\t]*(.*)\\)"
grepExtractPkgsFilename <-
  "^[[:alpha:]].*_([0-9]+[.\\-][0-9]+[.\\-][0-9]+[.\\-]*[0-9]*)(_.*)(\\.zip|\\.tar.gz)"

.grepR <- "^ *R( |\\(|$)"
