newEmptyEnv <- function() {
  new.env(parent = emptyenv())
}

#' 1st level --> create the .pkgEnv object in Require
pkgEnvCreate <- function() {
  assign(.pkgEnvName, newEmptyEnv(), envir = asNamespace("Require"))
}

pkgEnv <- function() {
  get(.pkgEnvName, envir = asNamespace("Require"))
}

# 2nd level
pkgEnvStartTimeCreate <- function() {
  if (is.null(pkgEnv()))
    pkgEnvCreate()
  assign(.pkgEnvStartTimeName, Sys.time(), envir = pkgEnv())
}

pkgEnvStartTime <- function() {
  if (is.null(pkgEnv()))
    pkgEnvCreate()
  get0(.pkgEnvStartTimeName, envir = pkgEnv())
}


#' 2nd level
pkgDepEnv <- function() {
  if (is.null(pkgEnv()))
    pkgEnvCreate()
  get0(.pkgDepEnvName, envir = pkgEnv())
}

pkgDepEnvCreate <- function() {
  if (is.null(pkgEnv()))
    pkgEnvCreate()

  assign(.pkgDepEnvName, newEmptyEnv(), envir = pkgEnv())
}



# 2nd level
pkgDepGitHubSHAEnvCreate <- function() {
  if (is.null(pkgEnv()))
    pkgEnvCreate()
  assign(getSHAfromGitHubObjName, newEmptyEnv(), envir = pkgEnv())
}

pkgDepGitHubSHAEnv <- function() {
  if (is.null(pkgEnv()))
    pkgEnvCreate()
  get0(getSHAfromGitHubObjName, envir = pkgEnv())
}


#' 3rd level  for deps #############################################
pkgDepDepsEnvCreate <- function() {
  if (is.null(pkgDepEnv()))
    pkgDepEnvCreate()
  assign(.pkgDepDepsEnvName, newEmptyEnv(), envir = pkgDepEnv())
}

pkgDepDepsEnv <- function() {
  if (is.null(pkgDepEnv()))
    pkgDepEnvCreate()
  get0(.pkgDepDepsEnvName, envir = pkgDepEnv())
}

#' 3rd level for DESCRIPTIONFile
pkgDepDESCFileEnvCreate <- function() {
  if (is.null(pkgDepEnv()))
    pkgDepEnvCreate()
  assign(.pkgDepDESCFileEnvName, newEmptyEnv(), envir = pkgDepEnv())
}

pkgDepDESCFileEnv <- function() {
  if (is.null(pkgDepEnv()))
    pkgDepEnvCreate()
  get0(.pkgDepDESCFileEnvName, envir = pkgDepEnv())
}

pkgDepArchiveDetailsInnerEnvCreate <- function() {
  if (is.null(pkgEnv()))
    pkgEnvCreate()
  assign(getArchiveDetailsInnerTxt, newEmptyEnv(), envir = pkgEnv())
}

pkgDepArchiveDetailsInnerEnv <- function() {
  if (is.null(pkgEnv()))
    pkgEnvCreate()
  get0(getArchiveDetailsInnerTxt, envir = pkgEnv())
}

.pkgDepDESCFileEnvName <- "DESCRIPTIONFile"
.pkgDepDepsEnvName <- "deps"
.pkgDepEnvName <- "pkgDep"
.pkgEnvName <- ".pkgEnv"
getArchiveDetailsInnerTxt <- "getArchiveDetailsInner"
.pkgEnvStartTimeName <- "startTime"
getSHAfromGitHubObjName <- "getSHAfromGitHub"
.pkgHasGHP <- "hasGHP"
.internetExistsTime <- "internetExistsTime"
# .pkgEnv[[getSHAfromGitHubObjName]] <- new.env()


