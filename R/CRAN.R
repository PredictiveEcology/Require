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
      options("repos" = c("CRAN" = cranRepo))
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
