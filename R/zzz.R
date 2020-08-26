.pkgEnv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  opts <- options()
  opts.Require <- RequireOptions()
  toset <- !(names(opts.Require) %in% names(opts))
  if (any(toset)) options(opts.Require[toset])
  if (!is.null(getOption("Require.RPackageCache")))
    checkPath(getOption("Require.RPackageCache"), create = TRUE)

  if (getOption("Require.persistentPkgEnv")) {
    theFile <- file.path("~", "._Require_pkgEnv.rdata")
    if (file.exists(theFile)) {
      load(theFile, envir = .pkgEnv)
      list2env(.pkgEnv$pkgEnvLast, .pkgEnv)
      rm(pkgEnvLast, envir = .pkgEnv)
    }
  }
  invisible()
}

.onUnload <- function(libpath) {
  if (getOption("Require.persistentPkgEnv")) {
    pkgEnvLast <- as.list(Require:::.pkgEnv); 
    save(pkgEnvLast, file = file.path("~", "._Require_pkgEnv.rdata"))
  }
}