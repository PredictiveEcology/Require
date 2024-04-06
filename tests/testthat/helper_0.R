setupTest <- function(verbose = getOption("Require.verbose"), envir = parent.frame()) {
  # opts <- options()

  # getCRANrepos(ind = 1)

  # print(paste("googledrive is loaded: ", "googledrive" %in% loadedNamespaces()))
  # requireNamespace("waldo")

  # cannot open file 'startup.Rs': No such file or directory
  # suggested solution https://stackoverflow.com/a/27994299/3890027
  # withr::local_envvar(list("R_TESTS" = "",
  #                          "R_REMOTES_UPGRADE" = "never",
  #                          "CRANCACHE_DISABLE" = TRUE),
  #                     .local_envir = envir)
  # Sys.setenv("R_TESTS" = "")
  # Sys.setenv("R_REMOTES_UPGRADE" = "never")
  # Sys.setenv("CRANCACHE_DISABLE" = TRUE)
  # withr::local_options(.local_envir = envir,
  # # outOpts <- options(
  #   # "Require.persistentPkgEnv" = TRUE,
  #   install.packages.check.source = "never",
  #   install.packages.compile.from.source = "never",
  #   # Ncpus = 2L,
  #   Require.unloadNamespaces = TRUE
  # )

  # if (Sys.info()["user"] == "achubaty") {
  #   withr::local_options(.local_envir = envir,
  #                        "Require.Home" = "~/GitHub/PredictiveEcology/Require")
  #   # outOpts2 <- options("Require.Home" = "~/GitHub/PredictiveEcology/Require")
  # } else {
  #   withr::local_options(.local_envir = envir,
  #                        "Require.Home" = "~/GitHub/Require")
  #   # outOpts2 <- options("Require.Home" = "~/GitHub/Require")
  # }

  #   libPath <- .libPaths()
  withr::local_libpaths(tempdir2(.rndstr()), .local_envir = envir)
  # setLibPaths(tempdir2(.rndstr()))
  # origWd <- getwd()
  # thisFilename <- Require:::getInStack("r")
  # env <- Require:::whereInStack("ee")
  # startTime <- Sys.time()
  # Require:::messageVerbose(Require:::green(" --------------------------------- Starting ",
  #   thisFilename, "  at: ", format(startTime, digits = 2),
  #   "---------------------------"),
  #   verbose = verbose, verboseLevel = 0
  # )
  messageVerbose(blue(" getOption('Require.verbose'): ",
    getOption("Require.verbose")),
    verboseLevel = 0
  )
  messageVerbose(blue(" getOption('repos'): ",
    paste(getOption("repos"), collapse = comma)),
    verboseLevel = 0
  )
  return(#list(startTime = startTime, # thisFilename = thisFilename,
              # libPath = libPath,
          #    origWd = origWd,
           #   opts = opts)
    )
}

# endTest <- function(setupInitial, verbose = getOption("Require.verbose")) {
#   currOptions <- options()
#   changedOptions <- setdiff(currOptions, setupInitial$opts)
#   toRevert <- setupInitial$opts[names(changedOptions)]
#   names(toRevert) <- names(changedOptions)
#   if (any(grepl("datatable.alloccol", names(toRevert)))) browser()
#   options(toRevert)
#
#
#   # thisFilename <- setupInitial$thisFilename
#   endTime <- Sys.time()
#   # ee <- Require:::getInStack("ee")
#   # ee[[thisFilename]] <- format(endTime - setupInitial$startTime, digits = 2)
#   # Require:::messageVerbose("\033[32m ----------------------------------",
#   #   thisFilename, ": ", ee[[thisFilename]], " \033[39m",
#   #   verboseLevel = -1, verbose = verbose
#   # )
#   .libPaths(setupInitial$libPath)
#   setwd(setupInitial$origWd)
# }


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
