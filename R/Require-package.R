#' Require package
#'
#' A single key function, \code{Require} that wraps \code{install.packages},
#' \code{remotes::install_github}, \code{versions::install.versions}, and \code{base::require}
#' that allows for reproducible workflows. As with other functions in a reproducible workflow,
#' this package emphasizes functions that return the  same result whether it is the first or
#' subsequent times running the function.
#'
#' @import methods
"_PACKAGE"
