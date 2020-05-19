#' @importFrom utils chooseCRANmirror
#' @keywords internal
getCRANrepos <- function(repos = NULL) {
  if (is.null(repos)) {
    repos <- getOption("repos")["CRAN"]
  }

  # still might be imprecise repository, specifically ""
  if (isTRUE("" == repos)) {
    repos <- "@CRAN@"
  }

  # if @CRAN@, and non interactive session
  if (isTRUE("@CRAN@" %in% repos)) {
    cranRepo <- Sys.getenv("CRAN_REPO")
    repos <- if (nzchar(cranRepo)) {
      cranRepo
    } else {
      if (isInteractive()) {
        chooseCRANmirror2() ## sets repo option
        getOption("repos")["CRAN"]
      } else {
        "https://cloud.r-project.org"
      }
    }
  }

  return(repos)
}

#' Pass through function for \code{chooseCRANmirror}
#'
#' This is here to allow mocking during unit testing related to chooseCRANmirror.
#'
#' @importFrom utils chooseCRANmirror
#' @param ... Passed to \code{chooseCRANmirror}
#' @keywords internal
chooseCRANmirror2 <- function(...) {
  chooseCRANmirror(...)
}

isInteractive <- function() interactive()
