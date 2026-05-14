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
    defCacheDir <- tools::R_user_dir("Require", which = "cache") |>
      checkPath(create = TRUE)
    Sys.setenv("R_REQUIRE_CACHE" = defCacheDir)
  }

  # if (FALSE) {
  if (isTRUE(getOption("Require.usePak"))) {
    if (requireNamespace("pak", quietly = TRUE)) {
      # tryCatch: under R CMD check, pak::cache_summary() errors with
      # "R_USER_CACHE_DIR env var not set during package check" (pkgcache
      # CRAN policy). The probed value isn't used downstream — the call is
      # only here to warm pak — so swallow the error.
      tryCatch(pak::cache_summary(), error = function(e) NULL)
    }
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

  ## Ensure Require's bookkeeping dir exists at load time. (Note: this is
  ## Require's own SHA DB / mirrors / DESCRIPTION cache area, NOT pak's
  ## package tarball cache. See [.requirePkgInfoDir].)
  possCacheDir <- .requirePkgInfoDir(create = TRUE)

  ## Deprecation: the package-tarball cache location is now controlled by
  ## pak's standard env var R_USER_CACHE_DIR (consistent with the rest of
  ## the R ecosystem). The Require-specific knobs below remain functional
  ## for one release cycle but emit a one-time warning so users can
  ## migrate.
  if (isTRUE(getOption("Require.usePak", TRUE))) {
    optVal <- getOption("Require.cachePkgDir", "default")
    if (!identical(optVal, "default") && !identical(optVal, NULL)) {
      packageStartupMessage(
        "Require: options('Require.cachePkgDir') is deprecated and is ",
        "ignored under usePak = TRUE. To redirect pak's package cache, ",
        "set R_USER_CACHE_DIR in .Renviron instead."
      )
    }
    envVal <- Sys.getenv("R_REQUIRE_PKG_CACHE")
    if (nzchar(envVal)) {
      packageStartupMessage(
        "Require: R_REQUIRE_PKG_CACHE is deprecated and is ignored under ",
        "usePak = TRUE. To redirect pak's package cache, set ",
        "R_USER_CACHE_DIR in .Renviron instead (currently: '",
        envVal, "')."
      )
    }
  }

  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (isInteractive()) {
    possCacheDir <- cachePkgDir()
    mess <- c(
      "Require version: ", as.character(utils::packageVersion("Require")), "\n",
      if (!is.null(possCacheDir)) {
        paste0(
          "  Using cache directory: ", possCacheDir, ";\n   clear with cacheClearPackages().\n"
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
