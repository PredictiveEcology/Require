setupInitial <- setupTest()

if (isDevAndInteractive) {
  projectDir <- Require:::tempdir2(Require:::.rndstr(1))
  setLinuxBinaryRepo()
  pkgDir <- file.path(projectDir, "R")
  setLibPaths(pkgDir, standAlone = TRUE)
  dir.create(pkgDir, showWarnings = FALSE, recursive = TRUE)
  origDir <- setwd(projectDir)
  modulePath <- file.path(pkgDir, "m")

  # Install 3 packages that are needed for subsequent module and package installations
  Require("PredictiveEcology/SpaDES.project@transition",
    upgrade = FALSE, require = FALSE
  )

  # Install modules
  getFromNamespace("getModule", "SpaDES.project")(modulePath = modulePath,
    c(
      "PredictiveEcology/Biomass_speciesData@master",
      "PredictiveEcology/Biomass_borealDataPrep@master",
      "PredictiveEcology/Biomass_core@master",
      "CeresBarros/Biomass_validationKNN@master",
      "PredictiveEcology/Biomass_speciesParameters@development"
  ))

  outs <- getFromNamespace("packagesInModules", "SpaDES.project")(modulePath = modulePath)
  pkgs <- c(
    unname(unlist(outs)),
    "PredictiveEcology/SpaDES.experiment@development",
    "PredictiveEcology/SpaDES.project@transition",
    "devtools", "ggspatial", "ggpubr", "cowplot"
  )
  pkgsShort <- unique(sort(pkgs))
  deps <- pkgDep(pkgsShort, recursive = TRUE)

  # THE INSTALL
  pkgs <- omitPkgsTemporarily(pkgs)
  outFull <- Require::Require(pkgs, require = FALSE, standAlone = TRUE)

  # THE POST INSTALL COMPARISON
  ip <- data.table::as.data.table(installed.packages(lib.loc = .libPaths()[1], noCache = TRUE))

  allNeeded <- unique(extractPkgName(unname(unlist(deps))))
  allNeeded <- allNeeded[!allNeeded %in% .basePkgs]
  persLibPathOld <- ip$LibPath[which(ip$Package == "amc")]
  installedInFistLib <- ip[LibPath == persLibPathOld]
  # testit::assert(all(installed))
  ip <- ip[!Package %in% .basePkgs][, c("Package", "Version")]
  allInIPareInpkgDT <- all(ip$Package %in% allNeeded)
  installedNotInIP <- setdiff(allNeeded, ip$Package)
  installedPkgs <- setdiff(allNeeded, installedNotInIP)
  allInpkgDTareInIP <- all(installedPkgs %in% ip$Package)
  testit::assert(isTRUE(allInpkgDTareInIP))
  testit::assert(isTRUE(allInIPareInpkgDT))

  pkgDT <- toPkgDT(unique(sort(unname(unlist(deps)))))
  pkgDT[, versionSpec := extractVersionNumber(packageFullName)]
  pkgDT[!is.na(versionSpec), inequality := extractInequality(packageFullName)]

  pkgDT <- ip[pkgDT, on = "Package"]
  pkgDT[
    !is.na(inequality) & !is.na(Version),
    good := compareVersion2(package_version(Version), versionSpec, inequality)
  ]
  anyBad <- any(pkgDT$good %in% FALSE)
  testit::assert(isFALSE(anyBad))

  #########################################
  # FROM LandR_CBM
  pkgDir <- tempdir2("test-8_2nd")
  dir.create(pkgDir, recursive = TRUE, showWarnings = FALSE)
  .libPaths(pkgDir, include.site = FALSE)

  setLinuxBinaryRepo()


  modulePkgs <- c("archive", "assertthat", "compiler", "crayon", "data.table",
                  "DEoptim", "dplyr", "fastdigest", "fastDummies", "fasterize",
                  "fpCompare", "future", "gamlss", "gdalUtilities", "ggforce",
                  "ggplot2", "ggpubr", "ggspatial", "glmm", "grid", "gridExtra",
                  "ianmseddy/LandR.CS@development", "ianmseddy/LandR.CS@master (>= 0.0.2.0002)",
                  "ianmseddy/PSPclean@development", "ianmseddy/PSPclean@development (>= 0.1.2.9000)",
                  "kSamples", "logging", "magrittr", "MASS", "Matrix", "merTools",
                  "methods", "mgcv", "nlme", "numDeriv", "parallel", "parallelly",
                  "plyr", "PredictiveEcology/CBMutils", "PredictiveEcology/CBMutils (>= 0.0.6)",
                  "PredictiveEcology/climateData@development (>= 0.0.0.0.9002)",
                  "PredictiveEcology/fireSenseUtils@development", "PredictiveEcology/fireSenseUtils@development (>= 0.0.4.9014)",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.0.5.9013)",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.0.5.9026)",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.0.5.9028)",
                  "PredictiveEcology/fireSenseUtils@development (>=0.0.4.9080)",
                  "PredictiveEcology/LandR@development", "PredictiveEcology/LandR@development (>= 1.0.0.9001)",
                  "PredictiveEcology/LandR@development (>= 1.0.5)", "PredictiveEcology/LandR@development (>= 1.0.7.9023)",
                  "PredictiveEcology/LandR@development (>= 1.0.7.9025)", "PredictiveEcology/LandR@development (>= 1.0.7.9030)",
                  "PredictiveEcology/LandR@development (>= 1.0.9.9000)", "PredictiveEcology/pemisc@development",
                  "PredictiveEcology/pemisc@development (>= 0.0.3.9002)", "PredictiveEcology/reproducible@development",
                  "PredictiveEcology/reproducible@development (>= 1.2.10.9001)",
                  "PredictiveEcology/reproducible@development (>= 1.2.6.9008)",
                  "PredictiveEcology/reproducible@development (>= 1.2.6.9009)",
                  "PredictiveEcology/reproducible@development (>=1.2.7.9010)",
                  "PredictiveEcology/Require@development", "PredictiveEcology/SpaDES.core@development (>= 1.0.10.9005)",
                  "PredictiveEcology/SpaDES.core@development (>= 1.0.6.9016)",
                  "PredictiveEcology/SpaDES.core@development (>= 1.0.8.9000)",
                  "PredictiveEcology/SpaDES.core@development (>= 1.0.9.9004)",
                  "PredictiveEcology/SpaDES.core@development (>= 1.0.9.9008)",
                  "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9003)",
                  "PredictiveEcology/SpaDES.core@development (>=1.0.6.9019)", "PredictiveEcology/SpaDES.install (>= 0.0.5.9013)",
                  "PredictiveEcology/SpaDES.tools@development", "PredictiveEcology/SpaDES.tools@development (>= 0.3.7.9007)",
                  "pryr", "purrr", "quickPlot", "R.utils", "raster", "rasterVis",
                  "Rcpp", "reproducible (>= 1.2.6.9005)", "rgeos", "RhpcBLASctl",
                  "robustbase", "RSQLite", "scales", "sf", "snow", "sp", "SpaDES.core",
                  "SpaDES.tools", "spatialEco", "stats", "terra", "tidyr", "viridis"
  )

  otherPkgs <- c("archive", "details", "DBI", "s-u/fastshp", "logging", "RPostgres", "slackr")

  pkgs <- unique(c(modulePkgs, otherPkgs))

  pkgs <- omitPkgsTemporarily(pkgs)

  dirForInstall <- tempdir2(.rndstr(1))

  st1 <- system.time(out1 <- capture.output(type = "message",
                 Install(pkgs, standAlone = TRUE, upgrade = FALSE, libPaths = dirForInstall)
                 ))
  # Do a second time; should be empty (and fast)
  st2 <- system.time(out2 <- capture.output(type = "message",
                         Install(pkgs, standAlone = TRUE, upgrade = FALSE, libPaths = dirForInstall)
  ))
  # some sort of test about whether anything was installed; pick reproducible as a random pkg
  testit::assert(sum(grepl("reproducible", out1)) == 1)
  testit::assert(sum(grepl("reproducible", out2)) == 0)
  testit::assert(st1["elapsed"]/st2["elapsed"] > 5) # WAY faster -- though st1 is not that slow b/c local binaries

}

endTest(setupInitial)
