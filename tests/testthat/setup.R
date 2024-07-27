if (.isDevelVersion() && nchar(Sys.getenv("R_REQUIRE_RUN_ALL_TESTS")) == 0) {
  Sys.setenv("R_REQUIRE_RUN_ALL_TESTS" = "true")
}
verboseForDev <- -2
Require.usePak <- FALSE
Require.installPackageSys <- 2

if (isTRUE(Require.usePak))
  if (requireNamespace("pak"))
    existingCacheDir <- pak::cache_summary()$cachepath


isDev <- Sys.getenv("R_REQUIRE_RUN_ALL_TESTS") == "true" &&
  Sys.getenv("R_REQUIRE_CHECK_AS_CRAN") != "true"
# Actually interactive
isDevAndInteractive <- interactive() && isDev && Sys.getenv("R_REQUIRE_TEST_AS_INTERACTIVE") != "false"

# try(rm(getFromCache1, getDeps1, getDepsFromCache1), silent = TRUE); i <- 0
withr::local_options(.local_envir = teardown_env(),
                     Require.verbose = ifelse(isDev, verboseForDev, -2))
withr::local_options(.local_envir = teardown_env(),
                     Require.usePak = Require.usePak)

if (!isDevAndInteractive) { # i.e., CRAN
  Sys.setenv(R_REQUIRE_PKG_CACHE = "FALSE")
}

suggests <- DESCRIPTIONFileDeps(system.file("DESCRIPTION", package = "Require"), which = "Suggests") |>
  extractPkgName()
suggests <- setdiff(suggests, c("testthat", "SpaDES", "SpaDES.core", "quickPlot")) # dpesn't like being local_package'd
withr::local_options("Require.packagesLeaveAttached" = suggests, .local_envir = teardown_env())
# for (pk in suggests) {
#   try(suppressWarnings(withr::local_package(pk, .local_envir = teardown_env(), quietly = TRUE, verbose = FALSE)), silent = TRUE)
# }

# can't use withr::local_package reliably because if a package gets unloaded in the tests,
#   then there is a warning on teardown that can't be silenced
for (pk in suggests) {
  try(suppressWarnings(
    requireNamespace(pk, # .local_envir = teardown_env(),
                     quietly = TRUE)), silent = TRUE)
}

# withr::defer({
#   aa <- rev(names(pkgDepTopoSort(suggests[suggests %in% loadedNamespaces()])))
#   bb <- lapply(aa, function(p) try(unloadNamespace(p), silent = TRUE))
# }, envir = teardown_env())

withr::local_options(.local_envir = teardown_env(),
                     repos = getCRANrepos(ind = 1),
                     Ncpus = 2,
                     Require.isDev = isDev,
                     Require.isDevAndInteractive = isDevAndInteractive,
                     install.packages.check.source = "never",
                     install.packages.compile.from.source = "never",
                     Require.unloadNamespaces = TRUE,
                     Require.Home = "~/GitHub/Require")

withr::local_envvar(.local_envir = teardown_env(),
                    "R_TESTS" = "",
                    "R_REMOTES_UPGRADE" = "never",
                    "CRANCACHE_DISABLE" = TRUE
)

if (Sys.info()["user"] == "achubaty") {
  withr::local_options(.local_envir = teardown_env(),
                       "Require.Home" = "~/GitHub/PredictiveEcology/Require")
}

# This is for cases e.g., linux where there are >2 .libPaths().
#  The tests use `withr::local_libpaths`, which keeps all site paths. This means that
#  some of the tests fail because R will load a copy of a package e.g., rlang that is
#  in one of the site libraries. Essentially, this is fine for a user, but the tests
#  weren't written to accommodate this.
lp <- .libPaths()
lp2 <- c(head(lp, 1), tail(lp, 1))
orig <- setLibPaths(lp2, standAlone = TRUE)
withr::defer(.libPaths(lp), envir = teardown_env())

if (Sys.info()["user"] %in% "emcintir") {
  secretPath <- if (isWindows()) "c:/Eliot/.secret" else "/home/emcintir/.secret"
  repos <- getOption("repos")
  if (isLinux())
    repos <- c(PPM = positBinaryRepos(), repos)
  repos <- repos[!duplicated(repos)] # keep names
  withr::local_options(
    .local_envir = teardown_env(),
    Require.cloneFrom = Sys.getenv("R_LIBS_USER"),
    "Require.installPackagesSys" = Require.installPackageSys,
    Ncpus = 8,
    repos = repos,
    Require.origLibPathForTests = .libPaths()[1],
    gargle_oauth_email = "eliotmcintire@gmail.com",
    gargle_oauth_cache = secretPath)#, .local_envir = teardown_env())
  # googledrive::drive_auth()
  print(options()[c("Ncpus", "repos", "Require.installPackagesSys", "Require.verbose", "Require.cloneFrom", "Require.usePak")])
  print(paste("Cache size:", length(dir(cachePkgDir())), "files"))
} else {
  # clean up cache on GA and other
  withr::defer(unlink(cacheDir(), recursive = TRUE), envir = teardown_env())
}


runTests <- function(have, pkgs) {
  # the is.character is for pak -- has a column but it is a path, not logical
  if (is.null(have$installed) || is.character(have$installed))
    have[, installed := installResult %in% "OK"]
  # recall LandR.CS won't be installed, also, Version number is not in place for newly installed packages
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
                                       restart = TRUE) {
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
  save(list = ls(envir), envir = envir, file = paste0(path, counterName, "_", interactive(), "_", m,".rda"))
}

rcmdLoad <- function(interactive = TRUE, counterName = "a", num = "max", path = "/home/emcintir/tmp") {
  if (identical(num, "max")) {
    poss <- dir(path, pattern = paste0("^", counterName, "_", interactive))
    num <- as.numeric(max(sapply(strsplit(poss, "_|\\."), function(x) x[[3]])))
  }
  int <- new.env();
  load(dir(path, pattern = paste0(counterName, "_", interactive, "_", num),
           full.names = TRUE),
       envir = int)
  as.list(int)
}


PEUniverseRepo <- function()
  unique(c("https://predictiveecology.r-universe.dev", getOption("repos")))
