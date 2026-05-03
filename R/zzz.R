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

  possCacheDir <- cacheGetOptionCachePkgDir() |> checkPath(create = TRUE)
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
