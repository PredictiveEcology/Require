utils::globalVariables(c(
  "pkgEnvLast"
))

.pkgEnv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  opts <- options()
  opts.Require <- RequireOptions()
  toset <- !(names(opts.Require) %in% names(opts))
  if (any(toset)) options(opts.Require[toset])
  if (!is.null(getOption("Require.RPackageCache")))
    checkPath(rpackageFolder(getOption("Require.RPackageCache")), create = TRUE)

  if (getOption("Require.persistentPkgEnv")) {
    if (file.exists(.thePersistentFile)) {
      pkgEnvLast <- readRDS(.thePersistentFile)
      list2env(pkgEnvLast, .pkgEnv)
    }
  }
  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (isInteractive()) {
    mess <- c("Require version: ", as.character(utils::packageVersion("Require")), ". ")
    mess <- c(mess, "See ?RequireOptions for additional settings.")
    packageStartupMessage(mess)
  }
}

.onUnload <- function(libpath) {
  if (getOption("Require.persistentPkgEnv")) {
    pkgEnvLast <- as.list(.pkgEnv); 
    saveRDS(pkgEnvLast, file = .thePersistentFile)
  }
}

.thePersistentFile <- file.path("~", "._Require_pkgEnv.rdata")