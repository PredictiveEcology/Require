#' A helper function to get or set CRAN repos
#'
#' This will get the current option in `getOption('repos')`, and if that is not
#' set to a url, then it will prompt the user to select a mirror, unless
#' `ind` is set, in which case, it will use that mirror (in
#' `chooseCRANmirror()`)
#' @importFrom utils chooseCRANmirror
#' @export
#' @param repos A CRAN-like repository
#' @param ind an integer of which mirror to use in `chooseCRANmirror()`
#' @keywords internal
getCRANrepos <- function(repos = NULL, ind) {
  if (isNonRepo(repos)) {
    repos <- getOption("repos")
    # names(repos) %in% "CRAN"
  }

  # still might be imprecise repository
  if (isNonRepo(repos)) {
    repos <- "@CRAN@"
  }

  # if @CRAN@, and non interactive session
  if (isTRUE("@CRAN@" %in% repos)) {
    cranRepo <- Sys.getenv("CRAN_REPO")
    repos <- if (nzchar(cranRepo)) {
      # Resolve the "@CRAN@" placeholder(s) to `cranRepo` IN PLACE, preserving
      # every other configured repo. The previous
      # `options("repos" = c("CRAN" = cranRepo))` REPLACED the whole repos vector
      # with CRAN-only, silently dropping an r-universe (e.g. RStudio's default
      # repos is `c(CRAN = "@CRAN@")` and RStudio sets `CRAN_REPO`, so an
      # `options(repos = unique(c(<r-universe>, getOption("repos"))))` set by the
      # user was wiped here). That broke `Require.noRemotes` installs, which
      # resolve PredictiveEcology packages from that r-universe. Mirrors the
      # `reposNow[!hasAts]` handling below (which already preserves other repos).
      reposNow <- getOption("repos")
      reposNow[reposNow %in% "@CRAN@"] <- cranRepo
      options(repos = reposNow)
      cranRepo
    } else {
      if (isInteractive() && missing(ind)) {
        chooseCRANmirror2() ## sets repo option
      } else if (missing(ind)) {
        stop("Please set a CRAN mirror")
      } else {
        chooseCRANmirror(ind = ind)
      }
      getOption("repos")["CRAN"]
    }
    if (isTRUE("" == repos) || isTRUE(is.na(repos))) {
      warning("Please choose a valid CRAN repo")
      repos <- getCRANrepos(repos, 1)
    }
  }
  # Clean up the global repos option: drop the "@CRAN@" placeholder (now
  # resolved above) and any duplicate repo URLs so downstream resolvers don't
  # query the same repo twice. Keep the first occurrence to preserve names.
  reposNow <- getOption("repos")
  keep <- !(reposNow %in% "@CRAN@") & !duplicated(unname(reposNow))
  if (!all(keep)) {
    options(repos = reposNow[keep])
  }

  return(repos)
}

#' Pass through function for `chooseCRANmirror`
#'
#' This is here to allow mocking during unit testing related to `chooseCRANmirror`.
#'
#' @importFrom utils chooseCRANmirror
#' @param ... Passed to `chooseCRANmirror`
#' @keywords internal
chooseCRANmirror2 <- function(...) {
  chooseCRANmirror(...)
}

isInteractive <- function() interactive()

isNonRepo <- function(repos) {
  (is.null(repos) || isTRUE("" == repos) || isTRUE(is.na(repos)) || is.logical(repos))
}
