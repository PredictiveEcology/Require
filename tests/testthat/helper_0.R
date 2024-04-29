setupTest <- function(verbose = getOption("Require.verbose"),
                      needRequireInNewLib = FALSE, envir = parent.frame()) {
  newLib <- tempdir3()
  if (needRequireInNewLib)
    linkOrCopyPackageFiles("Require", fromLib = .libPaths()[1], newLib)
  withr::local_libpaths(newLib, .local_envir = envir)
  messageVerbose(blue(" getOption('Require.verbose'): ",
    getOption("Require.verbose")),
    verboseLevel = 0
  )
  messageVerbose(blue(" getOption('repos'): ",
    paste(getOption("repos"), collapse = comma)),
    verboseLevel = 0
  )
  return()
}

omitPkgsTemporarily <- function(pkgs) {
  if (getRversion() < "4.2") {
    pkgs <- grep("mumin", pkgs, invert = TRUE, value = TRUE) # MuMIn requires R >= 4.2
    pkgs <- grep("LandR", pkgs, invert = TRUE, value = TRUE) # LandR requires R >= 4.2
    pkgs <- grep("fireSenseUtils", pkgs, invert = TRUE, value = TRUE) # LandR requires R >= 4.2
  }
  # while not on CRAN
  pkgs <- grep("^SpaDES.core", pkgs, invert = TRUE, value = TRUE) # not on CRAN
#   pkgs <- grep("SpaDES.experiment", pkgs, invert = TRUE, value = TRUE) # file.move is not exported from reproducible
  pkgs
}

dontTryDetach <- c("devtools", "testthat", "googledrive")

dontDetach <- function() {
  extractPkgName(unlist(unname(pkgDep(dontTryDetach, recursive = T))))
}
