if (.isDevelVersion() && nchar(Sys.getenv("R_REQUIRE_RUN_ALL_TESTS")) == 0) {
  Sys.setenv("R_REQUIRE_RUN_ALL_TESTS" = "true")
}
verboseForDev <- -1
installPackageSys <- 2

isDev <- Sys.getenv("R_REQUIRE_RUN_ALL_TESTS") == "true" &&
  Sys.getenv("R_REQUIRE_CHECK_AS_CRAN") != "true"
# Actually interactive
isDevAndInteractive <- interactive() && isDev && Sys.getenv("R_REQUIRE_TEST_AS_INTERACTIVE") != "false"

# try(rm(getFromCache1, getDeps1, getDepsFromCache1), silent = TRUE); i <- 0
withr::local_options(.local_envir = teardown_env(),
                     Require.verbose = ifelse(isDev, verboseForDev, -2))

if (!isDevAndInteractive) { # i.e., CRAN
  Sys.setenv(R_REQUIRE_PKG_CACHE = "FALSE")
}

suggests <- DESCRIPTIONFileDeps(system.file("DESCRIPTION", package = "Require"), which = "Suggests") |>
  extractPkgName()
# pkgsToLoad <- c("curl", "gitcreds", "httr", "openssl", "googledrive", "rappdirs", "waldo", "rematch2", "diffobj")
for (pk in suggests)
  suppressWarnings(withr::local_package(pk, .local_envir = teardown_env(), quietly = TRUE))

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

if (Sys.info()["user"] %in% "emcintir") {
  secretPath <- if (isWindows()) "c:/Eliot/.secret" else "/home/emcintir/.secret"
  repos <- getOption("repos")
  if (isLinux())
    repos <- c(PPM = positBinaryRepos(), repos)
  repos <- repos[!duplicated(repos)] # keep names
  withr::local_options(
    .local_envir = teardown_env(),
    Require.cloneFrom = Sys.getenv("R_LIBS_USER"),
    Ncpus = 8,
    repos = repos,
    Require.origLibPathForTests = .libPaths()[1],
    Require.installPackagesSys = isDevAndInteractive * installPackageSys,
    gargle_oauth_email = "eliotmcintire@gmail.com",
    gargle_oauth_cache = secretPath)#, .local_envir = teardown_env())
  googledrive::drive_auth()
  print(options()[c("Ncpus", "repos", "Require.installPackagesSys", "Require.verbose", "Require.cloneFrom")])
}


runTests <- function(have, pkgs) {
  # recall LandR.CS won't be installed, also, Version number is not in place for newly installed packages
  theTest <- all(!is.na(have[installed == TRUE &
                               !Package %in% extractPkgName(.RequireDependencies)]$Version))
  if (!isTRUE(theTest)) browser()
  testthat::expect_true(isTRUE(theTest))
  if ("installResult" %in% colnames(have)) {
    theTest <- NROW(have[is.na(installResult) | installResult %in% "OK" |
                           installResult %in% "Can't install Require dependency"]) == sum(have$installed)
    if (!isTRUE(theTest)) browser()
    testthat::expect_true(isTRUE(theTest))
  }
}


testWarnsInUsePleaseChange <- function(warns) {
  test <- TRUE
  if (length(warns)) {
    test <- all(grepl(paste0(msgIsInUse, "|Please change required"), warns)) # "Please change" comes with verbose >= 1
  }
  test
}



rcmdDebug <- function(counterName = "a", envir = parent.frame(), envirAssign = .GlobalEnv,
                      path = "/home/emcintir/tmp/") {
  # objNam <- deparse(substitute(...))
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
  load(dir(path, pattern = paste0(counterName, "_", interactive, "_", num), full.names = T),
       envir = int)
  as.list(int)
}
