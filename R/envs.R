newEmptyEnv <- function() {
  new.env(parent = emptyenv())
}

#' 1st level --> create the .pkgEnv object in Require
#' @param parentEnv The parent environment in which to make the new environment.
#'   Defaults to `asNamespace("Require")`
envPkgCreate <- function(parentEnv = asNamespace("Require")) {
  assign(.envPkgName, newEmptyEnv(), envir = parentEnv)
}

pkgEnv <- function(envir = .GlobalEnv) {
  memPersist <- isTRUE(getOption("Require.cachePersist", NULL))
  if (!memPersist)
    envir <- asNamespace("Require")
  env <- get0(.envPkgName, envir = envir, inherits = FALSE)
  if (is.null(env))
    env <- envPkgCreate(parentEnv = envir)
  env
}

# 2nd level
pkgEnvStartTimeCreate <- function() {
  if (is.null(pkgEnv()))
    envPkgCreate()
  assign(.envPkgStartTimeName, Sys.time(), envir = pkgEnv())
}

pkgEnvStartTime <- function() {
  if (is.null(pkgEnv()))
    envPkgCreate()
  get0(.envPkgStartTimeName, envir = pkgEnv())
}


#' 2nd level
pkgDepEnv <- function() {
  if (is.null(pkgEnv()))
    envPkgCreate()
  get0(.envPkgDepName, envir = pkgEnv())
}

envPkgDepCreate <- function() {
  if (is.null(pkgEnv()))
    envPkgCreate()
  assign(.envPkgDepName, newEmptyEnv(), envir = pkgEnv())
}


#' 2nd level
pakEnv <- function() {
  env <- get0(.envPakName, envir = pkgEnv())
  if (is.null(env)) {
    envPakCreate()
    env <- get0(.envPakName, envir = pkgEnv())
  }
  env
}

envPakCreate <- function() {
  if (is.null(pkgEnv()))
    envPakCreate()
  assign(.envPakName, newEmptyEnv(), envir = pkgEnv())
}



# 2nd level
envPkgDepGitHubSHACreate <- function() {
  if (is.null(pkgEnv()))
    envPkgCreate()
  assign(.txtGetSHAfromGitHub, newEmptyEnv(), envir = pkgEnv())
}

envPkgDepGitHubSHA <- function() {
  if (is.null(pkgEnv()))
    envPkgCreate()
  get0(.txtGetSHAfromGitHub, envir = pkgEnv())
}


#' 3rd level  for deps #############################################
envPkgDepDepsCreate <- function() {
  if (is.null(pkgDepEnv()))
    envPkgDepCreate()
  assign(.envPkgDepDepsName, newEmptyEnv(), envir = pkgDepEnv())
}

envPkgDepDeps <- function() {
  if (is.null(pkgDepEnv()))
    envPkgDepCreate()
  get0(.envPkgDepDepsName, envir = pkgDepEnv())
}

#' 3rd level for DESCRIPTIONFile
envPkgDepDESCFileCreate <- function() {
  if (is.null(pkgDepEnv()))
    envPkgDepCreate()
  assign(.envPkgDepDESCFileName, newEmptyEnv(), envir = pkgDepEnv())
}

envPkgDepDESCFile <- function() {
  if (is.null(pkgDepEnv()))
    envPkgDepCreate()
  get0(.envPkgDepDESCFileName, envir = pkgDepEnv())
}

envPkgDepArchiveDetailsInnerCreate <- function() {
  if (is.null(pkgEnv()))
    envPkgCreate()
  assign(.txtGetArchiveDetailsInner, newEmptyEnv(), envir = pkgEnv())
}

envPkgDepArchiveDetailsInner <- function() {
  if (is.null(pkgEnv()))
    envPkgCreate()
  get0(.txtGetArchiveDetailsInner, envir = pkgEnv())
}

.envPkgDepDESCFileName <- "DESCRIPTIONFile"
.envPkgDepDepsName <- "deps"
.envPkgDepName <- "pkgDep"
.envPakName <- "pak"
.envPkgName <- ".Require.pkgEnv"
.envPkgStartTimeName <- "startTime"

.envftbsp <- "FailedToBuildSrcPkg"

.messInstPkgCounter <- 0

envPkgDepDepsFilename <- "pkgDepCache.rds"

archiveFile <- function(repo)
  sprintf("%s/src/contrib/Meta/archive.rds", repo)
