#' Extract info from package character strings
#'
#' Cleans a character vector of non-package name related information (e.g., version)
#'
#' @param pkgs A character string vector of packages with or without GitHub path or versions
#' @return Just the package names without extraneous info.
#' @seealso [trimVersionNumber()]
#' @family version specifications
#' @export
#' @rdname extractPkgName
#' @examples
#' extractPkgName("Require (>=0.0.1)")
extractPkgName <- function(pkgs, filenames) {
  if (!missing(pkgs)) {
    hasNamesAny <- !is.null(names(pkgs))
    if (hasNamesAny) {
      hasNames <- (nchar(names(pkgs)) > 0) %in% TRUE
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
      out <- unlist(lapply(fnsSplit, function(xx3) xx3[[1]]))
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
    ## Strip pak's source prefixes (any::, cran::, github::, url::) before
    ## attempting version extraction; without this an "any::pkg@ver" ref
    ## doesn't match either form.
    pkgsBare <- sub("^[A-Za-z][A-Za-z0-9+.-]*::", "", pkgs)
    hasVersionNum <- grepl(grepExtractPkgs, pkgsBare, perl = FALSE)
    out <- rep(NA, length(pkgsBare))
    out[hasVersionNum] <- gsub(grepExtractPkgs, "\\2", pkgsBare[hasVersionNum], perl = FALSE)
    ## Also handle pak's "pkg@ver" form -- skip GitHub refs (owner/repo@sha)
    ## by requiring no "/" in the part before "@".
    if (isTRUE(getOption("Require.usePak", FALSE))) {
      atForm <- is.na(out) & grepl("@", pkgsBare, fixed = TRUE) &
        !grepl("/", sub("@.*$", "", pkgsBare), fixed = TRUE)
      out[atForm] <- sub("^[^@]+@", "", pkgsBare[atForm])
    }
  } else {
    if (!missing(filenames)) {
      fnsSplit <- strsplit(basename(filenames), "_")
      out <- unlist(lapply(fnsSplit, function(y) {
        if (length(y) >= 2)
          gsub(paste0("\\.zip|\\.tar\\.gz|", macBinaryFileExtGrep), "", y[[2]])
        else
          NA_character_
        }))
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

## The single definition of "looks like a GitHub `account/repo` spec".
##
## Both isGH() and extractPkgGitHub() use this. They used to carry separate
## definitions: isGH()'s was "^[[:alpha:]]+/.+", whose account class is
## alphabetic only, so an account carrying a hyphen or a digit -- r-lib,
## e-sensing, s-u, user123 -- was GitHub to one function and not to the other.
## The consequence was that
##   Install("r-lib/crancache (==0.0.0.9001)")
## asked GitHub for a *ref* named 0.0.0.9001: the GitHub-detecting path built
## a ref out of the trailing version, while the isGH()-gated code that reads
## (==ver) as a version constraint never ran.
##
## Account and repo do NOT share a character class, which is why they are
## spelled separately here:
##
##   account  alphanumerics and hyphens only. No "_", no "."; and no leading
##            or trailing hyphen. (GitHub also forbids consecutive hyphens and
##            caps the length at 39; neither is worth encoding.)
##   repo     alphanumerics plus "-", "_" and "." -- so "SpaDES.core" and
##            "fire_sense" are valid repos but not valid accounts.
##
## None of ".", "_" or "-" needs escaping inside a bracket expression: "." is
## literal there, "_" is never special, and "-" is literal when it is last.
## The pattern these replaced wrote "[[:alnum:]\\_\\.\\-]", which in a POSIX
## bracket expression is not an escape at all -- it silently admitted a literal
## backslash to the class.
##
## Not anchored at the end, so a trailing "@ref" or " (>=ver)" still matches.
## Non-GitHub specs fall out for free: a scheme prefix (any::, bioc::, url::,
## local::) and a Windows drive letter both hit ":" where "/" is required, and
## an absolute path has no account before its first "/".
.ghAccountRegex <- "[[:alnum:]]([[:alnum:]-]*[[:alnum:]])?"
.ghRepoRegex <- "[[:alnum:]._-]+"
.ghRefRegex <- paste0("^", .ghAccountRegex, "/", .ghRepoRegex)

#' @rdname extractPkgName
#' @export
#' @examples
#' extractPkgGitHub("PredictiveEcology/Require")
extractPkgGitHub <- function(pkgs) {
  isGH <- grepl(.ghRefRegex, pkgs, perl = FALSE)
  if (any(isGH)) {
    a <- trimVersionNumber(pkgs[isGH])
    hasRepo <- grepl("/", a)
    hasBranch <- grepl("@", a)
    a <- strsplit(a, split = "/|@")
    a <- Map(y2 = a, hasRep = hasRepo, function(y2, hasRep) y2[1 + hasRep])
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

#' Strip GitHub locators so packages resolve from `repos`
#'
#' Rewrites GitHub-style specs (`account/repo@branch`) to their bare package
#' name, preserving any version constraint. This lets `Require`/`Install`
#' satisfy them from `repos` (e.g., r-universe binaries) instead of cloning and
#' building from GitHub source -- avoiding the need for git authentication and
#' Rtools. Plain (non-GitHub) specs are returned unchanged. Used internally when
#' `getOption("Require.noRemotes")` is `TRUE`.
#'
#' @inheritParams extractPkgName
#' @param verbose Numeric. Controls reporting of which specs were rewritten.
#' @return A character vector the same length as `pkgs`, with GitHub locators
#'   removed.
#' @keywords internal
#' @examples
#' Require:::stripGitHubToRepos("PredictiveEcology/SpaDES.core@development (>= 3.0.3.9003)")
#' # -> "SpaDES.core (>= 3.0.3.9003)"
stripGitHubToRepos <- function(pkgs, verbose = getOption("Require.verbose", 1)) {
  if (!length(pkgs)) {
    return(pkgs)
  }
  isGH <- !is.na(extractPkgGitHub(trimVersionNumber(pkgs)))
  if (!any(isGH)) {
    return(pkgs)
  }
  nm <- extractPkgName(pkgs[isGH])
  ver <- extractVersionNumber(pkgs[isGH])
  hasVer <- !is.na(ver) & nzchar(ver)
  rebuilt <- nm
  if (any(hasVer)) {
    ineq <- extractInequality(pkgs[isGH][hasVer])
    rebuilt[hasVer] <- paste0(nm[hasVer], " (", ineq, " ", ver[hasVer], ")")
  }
  if (verbose >= 1) {
    messageVerbose(
      "Require.noRemotes: resolving from repos instead of GitHub:\n",
      paste0("  ", pkgs[isGH], " -> ", rebuilt, collapse = "\n"),
      verbose = verbose, verboseLevel = 1
    )
  }
  pkgs[isGH] <- rebuilt
  pkgs
}

#' Trim version number off a compound package name
#'
#' The resulting string(s) will have only name (including github.com repository if it exists).
#'
#' @inheritParams extractPkgName
#'
#' @rdname trimVersionNumber
#' @seealso [extractPkgName()]
#' @family version specifications
#' @export
#' @examples
#' trimVersionNumber("PredictiveEcology/Require (<=0.0.1)")
trimVersionNumber <- function(pkgs) {
  if (!is.null(pkgs)) {
    nas <- is.na(pkgs)
    if (any(!nas)) {
      ## Strip pak source prefixes first (any::, cran::, etc.) so the bare
      ## name matches downstream string ops (installed.packages() rownames,
      ## pkg_history lookups). Leave url:: alone -- those callers usually
      ## want the URL preserved.
      hasPrefix <- grepl("^[A-Za-z][A-Za-z0-9+.-]*::", pkgs[!nas]) &
        !startsWith(pkgs[!nas], "url::")
      pkgs[!nas][hasPrefix] <- sub("^[A-Za-z][A-Za-z0-9+.-]*::", "", pkgs[!nas][hasPrefix])
      ew <- endsWith(pkgs[!nas], ")")
      if (any(ew)) {
        pkgs[!nas][ew] <- gsub(paste0("\n|\t|", .grepVersionNumber), "", pkgs[!nas][ew])
      }
      ## pak "pkg@ver" form. Skip GitHub refs (owner/repo@sha) by requiring
      ## no "/" before the "@". Only active when usePak so non-pak callers
      ## keep their existing behavior.
      if (isTRUE(getOption("Require.usePak", FALSE))) {
        atForm <- grepl("@", pkgs[!nas], fixed = TRUE) &
          !grepl("/", sub("@.*$", "", pkgs[!nas]), fixed = TRUE)
        if (any(atForm)) {
          pkgs[!nas][atForm] <- sub("@.+$", "", pkgs[!nas][atForm])
        }
      }
    }
    pkgs
  }
}

rmExtraSpaces <- function(string) {
  gsub(" {2, }", " ", string)
}

# the @ is both in pak for CRAN and GitHub ... need to disentangle these for grep
.grepVersionNumber <- " *\\(.*"#| {0,5}@.+$"


grepExtractPkgs <- ".*\\([ \n\t]*(<*>*=*)[ \n\t]*(.*)\\)"
grepExtractPkgsFilename <-
  "^[[:alpha:]].*_([0-9]+[.\\-][0-9]+[.\\-][0-9]+[.\\-]*[0-9]*)(_.*)(\\.zip|\\.tar.gz)"

.grepR <- "^ *R( |\\(|$)"
