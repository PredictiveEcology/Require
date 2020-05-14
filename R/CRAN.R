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
      if (interactive()) {
        chooseCRANmirror2() ## sets repo option
        getOption("repos")["CRAN"]
      } else {
        "https://cloud.r-project.org"
      }
    }
  }

  return(repos)
}

#' @importFrom utils chooseCRANmirror
#' @keywords internal
chooseCRANmirror2 <- function() {
  chooseCRANmirror()
}
