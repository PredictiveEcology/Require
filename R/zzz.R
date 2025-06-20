utils::globalVariables(c(
  "pkgEnvLast"
))


#' @include envs.R
envPkgCreate()
# .pkgEnv <- newEmptyEnv() # new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  opts <- options()
  ## have to set this first for pak to work in vanilla session
  existing <- Sys.getenv("R_REQUIRE_CACHE")
  if (!nzchar(existing)) {
    Sys.unsetenv("R_REQUIRE_CACHE")
    ## will use `R_USER_CACHE_DIR` as base path for setting `R_REQUIRE_CACHE`;
    ## NOTE: do not modify `R_USER_CACHE_DIR` (see #124).
    defCacheDir <- normalizePath(tools::R_user_dir("Require", which = "cache"), mustWork = FALSE)
    Sys.setenv("R_REQUIRE_CACHE" = defCacheDir)
  }

  # if (FALSE) {
  if (isTRUE(getOption("Require.usePak"))) {
    if (requireNamespace("pak")) {
      existingCacheDir <- pak::cache_summary()$cachepath
    }
    # if (!is.character(existingCacheDir) && nzchar(existingCacheDir))
    #   Sys.setenv("R_REQUIRE_CACHE" = tempdir3())
    # }
  }

  opts.Require <- RequireOptions()
  toset <- !(names(opts.Require) %in% names(opts))
  if (any(toset)) options(opts.Require[toset])

  # if (getOption("Require.persistentPkgEnv")) {
  #   if (file.exists(.thePersistentFile())) {
  #     pkgEnvLast <- readRDS(.thePersistentFile())
  #     list2env(pkgEnvLast, .pkgEnv)
  #   }
  # }
  .RequireDependencies <<- RequireDependencies()
  if (!isTRUE("sys" %in% .RequireDependencies)) {
    .RequireDependencies <- c("Require", "data.table (>= 1.10.4)", "methods", "sys", "tools", "utils")
  }
  .RequireDependenciesNoBase <<- extractPkgName(setdiff(.RequireDependencies, .basePkgs))

  possCacheDir <- cacheGetOptionCachePkgDir()
  # if (!is.null(possCacheDir)) {
  #   dir.create(possCacheDir, showWarnings = FALSE, recursive = TRUE)
  # }

  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (isInteractive()) {
    possCacheDir <- cacheGetOptionCachePkgDir()
    mess <- c(
      "Require version: ", as.character(utils::packageVersion("Require")), "\n",
      if (!is.null(possCacheDir)) {
        paste0(
          "  Using cache directory: ", possCacheDir,
          "; clear with cacheClearPackages().\n"
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
#   file.path(cacheDir(FALSE), "pkgEnv.Rdata")
# }
