if (.isDevelVersion() && nchar(Sys.getenv("R_REQUIRE_RUN_ALL_TESTS")) == 0) {
  withr::local_envvar(R_REQUIRE_RUN_ALL_TESTS = "true", .local_envir = teardown_env())
}

## pak's pkgcache refuses to use the system cache during R CMD check (CRAN
## policy: pkgcache aborts with "R_USER_CACHE_DIR env var not set during
## package check"). Without this, every pak::pak() call inside the test suite
## errors out with `get_user_cache_dir()`, the install fails, identify-and-defer
## retries, and the suite stalls under R CMD check on CI for hours.
## Point pak at a per-session temp dir so it can write its cache. The dir
## must exist before pak::cache_summary() / loadNamespace("pak") runs, so
## create it eagerly here rather than relying on pak to mkdir.
##
## Gate this override on `!interactive()` so dev runs of
## testthat::test_local() use the user's real pkgcache rather than a
## throwaway tempdir per session. Require's snapshot installer
## populates pkgcache after each download (pkg_cache_add_file), so a
## persistent cache means a second test_local() invocation hits all
## 378 tarballs and skips the libcurl-multi download phase entirely.
## Under R CMD check / CI / batch usage, interactive() is FALSE and the
## tempdir override still applies.
if (!nzchar(Sys.getenv("R_USER_CACHE_DIR")) && !interactive()) {
  .userCacheDir <- tempfile("RequireUserCache_")
  dir.create(.userCacheDir, recursive = TRUE, showWarnings = FALSE)
  withr::local_envvar(
    R_USER_CACHE_DIR = .userCacheDir,
    .local_envir = teardown_env()
  )
  rm(.userCacheDir)
}
verboseForDev <- 2
Require.usePak <- TRUE#Sys.getenv("R_REQUIRE_USE_PAK", "false") == "true"
Require.installPackageSys <- 2L#2 * (isMacOS() %in% FALSE)
Require.offlineMode <- FALSE
usePkgCache <- tempdir2("RequireCacheForTests") # or NULL for using default

## pak namespace is loaded lazily by code paths that need it. Eagerly loading
## here had two issues:
## 1. pak::cache_summary() errored under R CMD check (pkgcache "R_USER_CACHE_DIR
##    env var not set during package check" policy).
## 2. requireNamespace("pak") in a fresh `R --vanilla` test subprocess
##    occasionally hung indefinitely on cold pak/pkgcache state — the same
##    hang we observed in the 6-hour CI matrix timeouts.

isDev <- Sys.getenv("R_REQUIRE_RUN_ALL_TESTS") == "true" &&
  Sys.getenv("R_REQUIRE_CHECK_AS_CRAN") != "true"
## Actually interactive
isDevAndInteractive <- interactive() && isDev && Sys.getenv("R_REQUIRE_TEST_AS_INTERACTIVE") != "false"

# try(rm(getFromCache1, getDeps1, getDepsFromCache1), silent = TRUE); i <- 0
withr::local_options(
  .new = list(
    Require.usePak = Require.usePak,
    Require.verbose = ifelse(isDev, verboseForDev, -2)
  ),
  .local_envir = teardown_env()
)

if (!isDevAndInteractive) { # i.e., CRAN
  withr::local_envvar(# R_REQUIRE_PKG_CACHE = "FALSE",
                      .local_envir = teardown_env())
}

## Always use temporary package cache for tests (#128):
## - we don't want to modify the user's cache;
## - user's cache may have package versions that are newer than those requested in the tests;
withr::local_envvar("R_REQUIRE_CACHE" = usePkgCache,
                    .local_envir = teardown_env())

suggests <- DESCRIPTIONFileDeps(system.file("DESCRIPTION", package = "Require"), which = "Suggests") |>
  extractPkgName()
suggests <- setdiff(suggests, c("testthat", "SpaDES", "SpaDES.core", "quickPlot")) # doesn't like being local_package'd
withr::local_options(Require.packagesLeaveAttached = suggests, .local_envir = teardown_env())
# for (pk in suggests) {
#   try(suppressWarnings(withr::local_package(pk, .local_envir = teardown_env(), quietly = TRUE, verbose = FALSE)), silent = TRUE)
# }

## can't use withr::local_package reliably because if a package gets unloaded in the tests,
##   then there is a warning on teardown that can't be silenced
for (pk in suggests) {
  try(suppressWarnings(
    requireNamespace(pk, # .local_envir = teardown_env(),
                     quietly = TRUE)), silent = TRUE)
}

# withr::defer({
#   aa <- rev(names(pkgDepTopoSort(suggests[suggests %in% loadedNamespaces()])))
#   bb <- lapply(aa, function(p) try(unloadNamespace(p), silent = TRUE))
# }, envir = teardown_env())

withr::local_options(
  .new = list(
    repos = getCRANrepos(ind = 1),
    Ncpus = 2,
    Require.isDev = isDev,
    Require.isDevAndInteractive = isDevAndInteractive,
    install.packages.check.source = "never",
    install.packages.compile.from.source = "never",
    Require.unloadNamespaces = TRUE,
    Require.offlineMode = Require.offlineMode,
    Require.Home = "~/GitHub/Require",
    ## Force cli's dynamic redraw during interactive dev test runs.
    ## testthat's evaluate::evaluate sink makes cli's auto-detection
    ## (isatty(stderr()) || ...) return FALSE inside tests and fall
    ## back to one-line-per-tick "static" output. Override here for
    ## any *direct* cli use in Require / our test code; the
    ## R_CLI_DYNAMIC env var (in local_envvar below) carries this
    ## into subprocesses.
    cli.dynamic = if (isDevAndInteractive) TRUE else NULL,
    ## pak vendors its own progress renderer that ignores cli.dynamic.
    ## Even with the option set, pak's pkgdepends progress bar emits
    ## its spinner ticks as separate lines under testthat's sink.
    ## Disable pak's progress entirely during tests — user still sees
    ## informational headers ("Will install N packages", "✔ Installed
    ## X") and the per-package install confirmations, just no spinner
    ## storm. Same logic as cli.dynamic: only during interactive dev.
    pkg.show_progress = if (isDevAndInteractive) FALSE else NULL
  ),
  .local_envir = teardown_env()
)

withr::local_envvar(
  .new = list(
    "R_TESTS" = "",
    "R_REMOTES_UPGRADE" = "never",
    "CRANCACHE_DISABLE" = TRUE,
    ## Companion to options(cli.dynamic) above. Options live only in
    ## the parent R session; pak runs in an r_session callr subprocess
    ## that doesn't inherit them, so cli's auto-detection there still
    ## falls back to static and we get the same one-line-per-tick spew.
    ## Env vars DO propagate to the subprocess, and cli's
    ## is_dynamic_tty() reads R_CLI_DYNAMIC after getOption("cli.dynamic")
    ## but before isatty(). Empty string (NA via setting NA) leaves it
    ## untouched in CI / R CMD check.
    "R_CLI_DYNAMIC" = if (isDevAndInteractive) "true" else NA
  ),
  .local_envir = teardown_env()
)

if (Sys.info()[["user"]] == "achubaty") {
  withr::local_options(.local_envir = teardown_env(),
                       Require.Home = "~/GitHub/PredictiveEcology/Require")
}

## This is for cases e.g., linux where there are >2 .libPaths().
##  The tests use `withr::local_libpaths`, which keeps all site paths. This means that
##  some of the tests fail because R will load a copy of a package e.g., rlang that is
##  in one of the site libraries. Essentially, this is fine for a user, but the tests
##  weren't written to accommodate this.
lp <- .libPaths()
lp2 <- c(head(lp, 1), tail(lp, 1))
orig <- setLibPaths(lp2, standAlone = TRUE)
withr::defer(.libPaths(lp), envir = teardown_env())

if (Sys.info()["user"] %in% "emcintir") {
  secretPath <- if (isWindows()) "c:/Eliot/.secret" else "/home/emcintir/.secret"
  repos <- getOption("repos")
  if (isUbuntuOrDebian()) {
    repos <- c(PPM = positBinaryRepos(), repos)
  }
  repos <- repos[!duplicated(repos)] # keep names
  withr::local_options(
    .local_envir = teardown_env(),
    Require.cloneFrom = Sys.getenv("R_LIBS_USER"),
    "Require.installPackagesSys" = Require.installPackageSys,
    Ncpus = 8,
    repos = repos,
    Require.origLibPathForTests = .libPaths()[1],
    gargle_oauth_email = "eliotmcintire@gmail.com",
    gargle_oauth_cache = secretPath) # , .local_envir = teardown_env())
  # googledrive::drive_auth()
  cat(paste0("EnvVar:\n  R_REQUIRE_CACHE: ", Sys.getenv("R_REQUIRE_CACHE"), "\n"))
  cat(paste0("Num Cached Pkgs: ",
             length(dir(file.path(Sys.getenv("R_REQUIRE_CACHE"), "packages/4.4"), recursive = FALSE)),
             "\n"))
  print(options()[c("Ncpus", "repos", "Require.installPackagesSys", "Require.verbose",
                    "Require.cloneFrom", "Require.usePak")])
  print(paste("Cache size:", length(dir(cachePkgDir())), "files"))
} else {
  ## clean up cache on GA and other
  withr::defer(unlink(cacheDir(), recursive = TRUE), envir = teardown_env())
}

runTests <- function(have, pkgs) {
  ## the is.character is for pak -- has a column but it is a path, not logical
  if (is.null(have$installed) || is.character(have$installed))
    have[, installed := installResult %in% "OK"]
  ## recall LandR.CS won't be installed, also, Version number is not in place for newly installed packages
  theTest <- all(!is.na(have[installed == TRUE &
                               !Package %in% extractPkgName(.RequireDependencies)]$Version))
  if  (identical(Sys.info()[["user"]], "emcintir") && interactive()) if (!isTRUE(theTest)) browser()
  testthat::expect_true(isTRUE(theTest))
  if ("installResult" %in% colnames(have)) {
    theTest <- NROW(have[is.na(installResult) | installResult %in% "OK" |
                           installResult %in% "Can't install Require dependency"]) == sum(have$installed)
    if  (identical(Sys.info()[["user"]], "emcintir") && interactive()) if (!isTRUE(theTest)) browser()
    testthat::expect_true(isTRUE(theTest))
  }
}

testWarnsInUsePleaseChange <- function(warns, please = TRUE, inUse = TRUE, couldNot = TRUE,
                                       restart = TRUE, installFailed = TRUE) {
  test <- TRUE
  if (length(warns)) {
    tst <- character()
    if (isTRUE(restart))
      tst <- .txtPleaseRestart
    if (isTRUE(please))
      tst <- c(tst, .txtPleaseChangeReqdVers)
    if (isTRUE(inUse))
      tst <- c(tst, .txtMsgIsInUse)
    if (isTRUE(couldNot))
      tst <- c(tst, .txtCouldNotBeInstalled)
    if (isTRUE(installFailed))
      tst <- c(tst, .txtInstallationNonZeroExit, .txtInstallationPkgFailed)
    tst <- paste(tst, collapse = "|")
    test <- all(grepl(tst, warns)) # "Please change" comes with verbose >= 1
  }
  test
}

testCouldNotBeInstalled <- function(warns) {
  test <- TRUE
  if (length(warns)) {
    test <- all(grepl(paste0(.txtCouldNotBeInstalled), warns))
  }
  test
}

rcmdDebug <- function(counterName = "a", envir = parent.frame(), envirAssign = .GlobalEnv,
                      path = "/home/emcintir/tmp/") {
  if (!exists(counterName, envir = envirAssign))
    assign(counterName, 0, envir = envirAssign) # m <<- 0
  m <- get(counterName, envir = envirAssign)
  m <- m + 1
  assign(counterName, m, envir = envirAssign)
  save(list = ls(envir), envir = envir, file = paste0(path, counterName, "_", interactive(), "_", m, ".rda"))
}

rcmdLoad <- function(interactive = TRUE, counterName = "a", num = "max", path = "/home/emcintir/tmp") {
  if (identical(num, "max")) {
    poss <- dir(path, pattern = paste0("^", counterName, "_", interactive))
    num <- as.numeric(max(sapply(strsplit(poss, "_|\\."), function(x) x[[3]])))
  }
  int <- new.env()
  load(dir(path, pattern = paste0(counterName, "_", interactive, "_", num),
           full.names = TRUE),
       envir = int)
  as.list(int)
}

PEUniverseRepo <- function() {
  unique(tolower(c("https://predictiveecology.r-universe.dev", getOption("repos"))))
}
