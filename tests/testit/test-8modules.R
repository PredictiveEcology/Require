setupInitial <- setupTest()

if (.isDevTestAndInteractive) {
  projectDir <- Require:::tempdir2(Require:::.rndstr(1))
  pkgDir <- file.path(projectDir, "R")
  setLibPaths(pkgDir, standAlone = TRUE)
  dir.create(pkgDir, showWarnings = FALSE, recursive = TRUE)
  origDir <- setwd(projectDir)
  modulePath <- file.path(pkgDir, "m")

  # Install 3 packages that are needed for subsequent module and package installations
  Require("PredictiveEcology/SpaDES.project@transition",
                   upgrade = FALSE, require = FALSE)
  # setLinuxBinaryRepo() # OK to run on any system

  # Install modules
  SpaDES.project::getModule(modulePath = modulePath,
                            c("PredictiveEcology/Biomass_speciesData@master",
                              "PredictiveEcology/Biomass_borealDataPrep@master",
                              "PredictiveEcology/Biomass_core@master",
                              "CeresBarros/Biomass_validationKNN@master",
                              "PredictiveEcology/Biomass_speciesParameters@development"))

  outs <- SpaDES.project::packagesInModules(modulePath = modulePath)

  Require::Require(c(unname(unlist(outs)),
                     "PredictiveEcology/SpaDES.experiment@development",
                     "devtools", "ggspatial", "ggpubr", "cowplot"),
                   require = FALSE, standAlone = TRUE)
}

endTest(setupInitial)
