newEmptyEnv <- function() {
  new.env(parent = emptyenv())
}

#' 1st level --> create the .pkgEnv object in Require
envPkgCreate <- function() {
  assign(.envPkgName, newEmptyEnv(), envir = asNamespace("Require"))
}

pkgEnv <- function() {
  get(.envPkgName, envir = asNamespace("Require"))
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
.envPkgName <- ".pkgEnv"
.envPkgStartTimeName <- "startTime"

.txtGetArchiveDetailsInner <- "getArchiveDetailsInner"
.txtGetSHAfromGitHub <- "getSHAfromGitHub"
.txtPkgHasGHP <- "hasGHP"
.txtInternetExistsTime <- "internetExistsTime"
# .pkgEnv[[.txtGetSHAfromGitHub]] <- new.env()

.txtGitHub <- "GitHub"
