setupInitial <- setupTest()

# This error doesn't occur on Linux
if (isDev && Require:::isWindows()) {
  projectDir <- Require:::tempdir2(Require:::.rndstr(1))
  pkgDir <- file.path(projectDir, "R")
  setLibPaths(pkgDir, standAlone = TRUE)
  dir.create(pkgDir, showWarnings = FALSE, recursive = TRUE)

# Trying to install a package whose dependency is a loadedNamespace, but that is not available
  ####
  # out6 <- capture.output(type = "message",
  #                        out <- suppressWarnings(
                           Require("whisker",
                                                         require = TRUE,
                                                         standAlone = TRUE)
                           # )) # warnings would be if whisker is used by e.g., devtools
  # out3 <- capture.output(type = "message",
                         # out5 <- capture.output(out4 <- suppressWarnings(
                           remove.packages("whisker")
                           #), silent = TRUE))
  warns <- list()
  # mess <- capture.output(type = "message",
  #                        out2 <- withCallingHandlers(
                           Require("PredictiveEcology/SpaDES.project@development", require = FALSE, verbose = 2)
  #                          , warning = function(w) {
  #                            warns <<- Require:::appendToWarns(w$message, warns, Package = c("whisker", "SpaDES.project"))
  #                            invokeRestart("muffleWarning")
  #                          })
  # )
  testit::assert(any(grepl("is in use", warns)))
  testit::assert(any(grepl("is in use", mess)))



  ####
}

endTest(setupInitial)
