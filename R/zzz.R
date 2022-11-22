utils::globalVariables(c(
  "pkgEnvLast"
))

.pkgEnv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  opts <- options()
  opts.Require <- RequireOptions()
  toset <- !(names(opts.Require) %in% names(opts))
  if (any(toset)) options(opts.Require[toset])

  # if (getOption("Require.persistentPkgEnv")) {
  #   if (file.exists(.thePersistentFile())) {
  #     pkgEnvLast <- readRDS(.thePersistentFile())
  #     list2env(pkgEnvLast, .pkgEnv)
  #   }
  # }
  possCacheDir <- getOptionRPackageCache()
  # if (!is.null(possCacheDir)) {
  #   dir.create(possCacheDir, showWarnings = FALSE, recursive = TRUE)
  # }

  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (isInteractive()) {
    possCacheDir <- getOptionRPackageCache()
    mess <- c(
      "Require version: ", as.character(utils::packageVersion("Require")), "\n",
      if (!is.null(possCacheDir)) {
        paste0(
          "  Using cache directory: ", possCacheDir,
          "; clear with clearRequirePackageCache().\n"
        )
      },
      "  See ?RequireOptions for this and other settings."
    )

    packageStartupMessage(mess)
  }
}

.onUnload <- function(libpath) {

}

# .thePersistentFile <- function() {
#   file.path(RequireCacheDir(FALSE), "pkgEnv.Rdata")
# }
