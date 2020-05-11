#' Make a temporary sub-directory or file in that subdirectory
#'
#' Create a temporary subdirectory in \code{.RequireTempPath()}, or a
#' temporary file in that temporary subdirectory.
#'
#' @param sub Character string, length 1. Can be a result of
#'   \code{file.path("smth", "smth2")} for nested temporary sub
#'   directories.
#' @param tempdir Optional character string where the temporary dir should be placed.
#'   Defaults to \code{.RequireTempPath()}
#'
#' @rdname tempFilesAndFolders
#' @export
tempdir2 <- function(sub = "", tempdir = getOption("Require.tempPath", .RequireTempPath())) {
  checkPath(normPath(file.path(tempdir, sub)), create = TRUE)
}

#' @param ... passed to \code{tempfile}, e.g., \code{fileext}
#'
#' @rdname tempFilesAndFolders
#' @export
tempfile2 <- function(sub = "", ...) {
  normPath(file.path(tempdir2(sub = sub), basename(tempfile(...))))
}

.RequireTempPath <- function() normPath(file.path(tempdir(), "Require"))
