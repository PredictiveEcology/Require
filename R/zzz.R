.pkgEnv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  opts <- options()
  opts.Require <- RequireOptions()
  toset <- !(names(opts.Require) %in% names(opts))
  if (any(toset)) options(opts.Require[toset])
  if (!is.null(getOption("Require.RPackageCache")))
    checkPath(getOption("Require.RPackageCache"), create = TRUE)

  invisible()
}
