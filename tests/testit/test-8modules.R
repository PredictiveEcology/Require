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
                   upgrade = FALSE, require = FALSE)

  # Install modules
  SpaDES.project::getModule(modulePath = modulePath,
                            c("PredictiveEcology/Biomass_speciesData@master",
                              "PredictiveEcology/Biomass_borealDataPrep@master",
                              "PredictiveEcology/Biomass_core@master",
                              "CeresBarros/Biomass_validationKNN@master",
                              "PredictiveEcology/Biomass_speciesParameters@development"))

  outs <- SpaDES.project::packagesInModules(modulePath = modulePath)
  pkgs <- c(unname(unlist(outs)),
            "PredictiveEcology/SpaDES.experiment@development",
            "PredictiveEcology/SpaDES.project@transition",
            "devtools", "ggspatial", "ggpubr", "cowplot")
  pkgsShort <- unique(sort(pkgs))
  deps <- pkgDep(pkgsShort, recursive = TRUE)

  # THE INSTALL
  outFull <- Require::Require(pkgs, require = FALSE, standAlone = TRUE)

  # THE POST INSTALL COMPARISON
  ip <- data.table::as.data.table(installed.packages(lib.loc = .libPaths()[1], noCache = TRUE))

  allNeeded <- unique(extractPkgName(unname(unlist(deps))))
  allNeeded <- allNeeded[!allNeeded %in% .basePkgs]
  persLibPathOld <- ip$LibPath[which(ip$Package == "amc")]
  installedInFistLib <- ip[LibPath == persLibPathOld]
  # testit::assert(all(installed))
  ip <- ip[!Package %in% .basePkgs][, c("Package", "Version")]
  allInIPareInpkgDT <- all(ip$Package %in% allNeeded )
  installedNotInIP <- setdiff(allNeeded, ip$Package)
  installedPkgs <- setdiff(allNeeded, installedNotInIP)
  allInpkgDTareInIP <- all(installedPkgs %in% ip$Package  )
  testit::assert(isTRUE(allInpkgDTareInIP))
  testit::assert(isTRUE(allInIPareInpkgDT))

  pkgDT <- toPkgDT(unique(sort(unname(unlist(deps)))))
  pkgDT[, versionSpec := extractVersionNumber(packageFullName)]
  pkgDT[!is.na(versionSpec), inequality := extractInequality(packageFullName)]

  pkgDT <- ip[pkgDT, on = "Package"]
  pkgDT[!is.na(inequality) & !is.na(Version),
        good := compareVersion2(package_version(Version), versionSpec, inequality)]
  anyBad <- any(pkgDT$good %in% FALSE)
  testit::assert(isFALSE(anyBad))
}

endTest(setupInitial)
