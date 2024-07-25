test_that("test 09", {

  skip_if(getOption("Require.usePak"), message = "Takes too long on pak")
  setupInitial <- setupTest(needRequireInNewLib = TRUE)
  # on.exit(endTest(setupInitial))

  isDev <- getOption("Require.isDev")
  isDevAndInteractive <- getOption("Require.isDevAndInteractive")
  if (isDevAndInteractive && !isMacOSX()) { ## TODO: source installs failing on macOS
    # 4.3.0 doesn't have binaries, and historical versions of spatial packages won't compile
    pkgPath <- paste0(file.path(tempdir2(Require:::.rndstr(1))), "/")
    a <- checkPath(pkgPath, create = TRUE)
    snapshotFiles <- "../../inst/snapshot.txt"
    # if (getRversion() <= "4.2.3") {
    #
    #   snapshotFiles <- rev(
    #     c("https://raw.githubusercontent.com/PredictiveEcology/WBI_forecasts/development/packageVersions_clean.txt"          ,
    #       "https://raw.githubusercontent.com/PredictiveEcology/LandWeb/rework-config/packages_2022-03-22.txt"
    #     ))
    #   fn <- file.path(pkgPath, "pkgSnapshot.txt")
    #   download.file(snapshotFiles[2], destfile = fn)
    #
    # } else {
    #   withr::local_tempdir(tmpdir = pkgPath)
    #   # This file is missing `map` and `tiler` packages, which are dependencies of
    #   #   `PredictiveEcology/LandWebUtils@dcb26fe3308d0f572de5036d7f115d8eff5f9887`
    #   #   (`tiler` is actually a dep of `PredictiveEcology/map@development`, so it is recursive need
    #   #    based on March 12, 2024 version of PredictiveEcology/map@development)
    #   # This file is missing `SpaDES.project` package, which is a dependency of
    #   #   `PredictiveEcology/SpaDES.config@94e90b0537b103f83504c96f51be157449e32c9c`
    #
    fnMissing <- c("tiler", "map", "SpaDES.project")
    #   (snapshotFiles <- googledrive::drive_download(googledrive::as_id("1WaJq6DZJxy_2vs2lfzkLG5u3T1MKREa8"),
    #                                                overwrite = TRUE)) |> capture_messages() -> mess
    #   snapshotFiles <- snapshotFiles$local_path
    #
    # }
    ## Long pkgSnapshot -- issue 41
    for (snf in snapshotFiles) {
      # origLibPaths <- setLibPaths(pkgPath, standAlone = TRUE)
      pkgs <- data.table::fread(snf)

      if (FALSE) {
        # minor corrections
        pkgs[Package %in% "climateData", Version := "1.0.4"]
        # pkgs[grep("SpaDES.config", Package, invert = TRUE)]
        pkgs[Package %in% "rnaturalearthhires", Version := "1.0.0.9000"]
        # tmp <- pkgs[1:3, ]
        tmps2 <- neededBasedOnPackageFullNames[Package %in% c("modelr", "doBy", "Deriv")][, c("Package", "packageFullName")]

        pkgs <- rbindlist(list(pkgs,
                               ip[Package %in% c("Deriv", "doBy", "modelr"), mget(intersect(colnames(pkgs), colnames(ip)))]),
                          fill = TRUE)
        #
        # data.table::fwrite(pkgs, file = snf)
        # googledrive::drive_update(file = googledrive::as_id("1WaJq6DZJxy_2vs2lfzkLG5u3T1MKREa8"),
        #                           media = snf)

        pkgs <- pkgs[Package %in% "reproducible", Version := "2.0.9"]
        pkgs <- pkgs[Package %in% "SpaDES.core", Version := "2.0.2"]
        # These have NA for repository
        "NLMR"
        "visualTest"

      }
      # remove some specifics for tests that are not expected to work
      skips <- c("rJava", "Require", "SpaDES.install")

      # Can't compile on R 4.4
      windowsSkips <- c("XML", "sysfonts", "rgdal", "rgeos",
                        'RCurl', 'httpuv', 'rgdal', 'rgl', 'sf', 'terra', 'DT',
                        'SpaDES.core', 'SpaDES.tools', 'biomod2',
                        # 'climateData',
                        'lwgeom', 'raster', 'servr', 'stars',
                        'geodata', 'shiny', 'tidyterra', 'leaflet', 'prioritizr',
                        'rpostgis', 'satellite', 'amc', 'merTools', 'rasterVis',
                        'tmap'
                        # , 'LandR', 'LandR.CS', 'LandWebUtils'
                        ) # pkgDep may add these back, but maybe newer versions
                                      # that can be built
      pkgs <- pkgs[!(Package %in% skips)]
      if (isWindows()) {
        # keep the GitHub ones because they have SHA, which should work fine
        # pkgs <- pkgs[!(Package %in% windowsSkips) & (GithubSHA1 == "" | is.na(GithubSHA1))]
        # pkgs <- pkgs[!(Package %in% windowsSkips)]

      }

      # stringfish can't be installed in Eliot's system from binaries
      if (isWindows())
        if (Sys.info()["user"] == "emcintir")
          withr::local_options(Require.otherPkgs = union(getOption("Require.otherPkgs"), "stringfish"))
      # pkgs <- pkgs[!Package %in% c("RandomFields", "RandomFieldsUtils")] # the version 1.0-7 is corrupt on RSPM
      pkgs <- pkgs[!Package %in% c("usefulFuns")] # incorrectly imports Require from reproducible... while other packages need newer reproducible

      # ERROR: compilation failed for package 'sf'# on Windows R 4.3.2
      # pkgs[Package %in% "sf", Version := "1.0-9"] # the version 1.0-7 is corrupt on RSPM
      #pkgs[Package %in% "SpaDES.core", `:=`(Version = "1.1.1", GithubRepo = "SpaDES.core",
      #                                      GithubUsername = "PredictiveEcology", GithubRef = "development",
      #                                      GithubSHA1 = "535cd39d84aeb35de29f88b0245c9538d86a1223")]
      # pks <- c("ymlthis", "SpaDES.tools", "amc")
      # pkgs <- pkgs[Package %in% pks]
      # "PredictiveEcology/SpaDES.config@94e90b0537b103f83504c96f51be157449e32c9c (==0.0.2.9071)"
      #
      # pkgs <- pkgs[Package %in% extractPkgName(pkgDep("PredictiveEcology/SpaDES.config@94e90b0537b103f83504c96f51be157449e32c9c (==0.0.2.9071)")[[1]])]

      # debug(installAll)
      snfTmp <- tempfile2(fileext = ".txt")
      data.table::fwrite(pkgs, file = snfTmp) # have to get rid of skips in the snfTmp
      packageFullName <- ifelse(!nzchar(pkgs$GithubRepo) | is.na(pkgs$GithubRepo), paste0(pkgs$Package, " (==", pkgs$Version, ")"),
                                paste0(pkgs$GithubUsername, "/", pkgs$GithubRepo, "@", pkgs$GithubSHA1)
      )
      names(packageFullName) <- packageFullName
      # warnsReq <- capture_warnings(Require::Install("Require"))
      # aaaa <<- 1
      # on.exit(rm(aaaa, envir = .GlobalEnv))
      opts <- options(repos = PEUniverseRepo()); on.exit(options(opts), add = TRUE)
      warns <- capture_warnings(
        # mess <- capture_messages(
        out <- Require(packageVersionFile = snfTmp, require = FALSE, # purge = TRUE,
                       returnDetails = TRUE)
        # )
      )


      # NLMR specification is for a version that doesn't exist
      NLMRandVisualTestWarn <- grepl(.txtPleaseChangeReqdVers, warns)
      expect_true(sum(unique(NLMRandVisualTestWarn)) <= 1L)
      warns <- warns[-which(NLMRandVisualTestWarn)]

      # Why tmap and tmaptools and stars not installed in first pass?
      warns <- grep("tmap|tmaptools|stars|cannot open", warns, invert = TRUE, value = TRUE) #

      test <- testWarnsInUsePleaseChange(warns)
      expect_true(test)

      "Please change required version e.g., NLMR (<=1.1)"
      warns <- capture_warnings(
        out11 <- pkgDep(unname(packageFullName)[-1], recursive = TRUE, simplify = FALSE)
      )
      expect_true(sum(grepl("Please change required.*NLMR", warns)) <=1 )

      neededBasedOnPackageFullNames <- rbindlistRecursive(out11$deps)
      dups <- duplicated(neededBasedOnPackageFullNames$Package)
      neededBasedOnPackageFullNames <- neededBasedOnPackageFullNames[!dups]
      neededBasedOnPackageFullNames[grep("biosim", ignore.case = TRUE, Package), Package := "BioSIM"] |> invisible()
      packagesBasedOnPackageFullNames <- c(neededBasedOnPackageFullNames$Package, "Require")

      # tooManyInstalled not right
      tooManyInstalled <- setdiff(packagesBasedOnPackageFullNames, pkgs$Package)
      loaded <- c("Require", "testthat")
      tooManyInstalled <- setdiff(tooManyInstalled, c(fnMissing, loaded))
      # if (isWindows()) {
      #   tooManyInstalled <- setdiff(tooManyInstalled, windowsSkips)
      # }

      expect_identical(tooManyInstalled, character(0))

      ip <- data.table::as.data.table(installed.packages(lib.loc = .libPaths()[1], noCache = TRUE))
      ip <- ip[!Package %in% .basePkgs]
      allInIPareInPkgs <- all(ip$Package %in% packagesBasedOnPackageFullNames)
      expect_true(allInIPareInPkgs)

      # Check based on Version number
      joined <- ip[pkgs, on = "Package"]
      whDiff <- (joined$Version != joined$i.Version)
      versionProblems <- joined[which(whDiff)]
      testthatDeps <- extractPkgName(pkgDep("testthat", dependencies = TRUE, recursive = TRUE)$testthat)
      devtoolsDeps <- extractPkgName(pkgDep("devtools", dependencies = TRUE, recursive = TRUE)$devtools)
      versionProblems <- versionProblems[
        which(!(versionProblems$Package %in% testthatDeps |
        versionProblems$Package %in% devtoolsDeps))]

      # scales didn't install the "equals" version because a different package needs >= 1.3.0
      versionProblems <- versionProblems[!Package %in% "scales"]
      expect_true(NROW(versionProblems) == 0)

      # See if any packages are missing
      installedNotInIP <- setdiff(packagesBasedOnPackageFullNames, ip$Package)
      missingPackages <- pkgs[Package %in% installedNotInIP]
      vers <- strsplit(pkgs$Version, "\\.|\\-")
      has4 <- lengths(vers) > 3
      looksLikeGHPkgWithoutGitInfo <- pkgs[has4 & !nzchar(GithubRepo)]$Package
      missingPackages <- missingPackages[!Package %in% looksLikeGHPkgWithoutGitInfo]
      loded <- loadedNamespaces()
      missingPackages <- missingPackages[!Package %in% loded]

      knownFails <- c(extractPkgName(.RequireDependencies),
                      c("SpaDES.config", "NLMR", "visualTest")) # can't install because Require is installed, but too old
      if (isLinux())
        knownFails <- c(knownFails, c("sodium", "keyring"))


      # Known missing --
      # NLMR because the version number doesn't exist on CRAn archives
      # and visualTest which is missing GitHub info for some reason --

      expect_true(identical(setdiff(missingPackages$Package, knownFails), character(0)))
      warns <- capture_warnings(
        lala <- capture.output(type = "message", {
          out2 <- Require(
            packageVersionFile = snfTmp,
            require = FALSE, returnDetails = TRUE# , purge = TRUE
          )
        })
      )

      test <- testWarnsInUsePleaseChange(warns)
      expect_true(test)

      att <- attr(out2, "Require")
      att <- att[!duplicated(att$Package)]

      versionViolation <- att$Package[grep("violation", att$installResult)]
      noneAvailable <- att$Package[grep(.txtNoneAvailable, att$installResult)]
      didnt <- att[!is.na(att$installResult)]


      allDone <- setdiff(didnt$Package, c(versionViolation, testthatDeps, looksLikeGHPkgWithoutGitInfo,
                                          noneAvailable, c("Require", "data.table")))
      allDone <- setdiff(allDone, knownFails)
      expect_identical(allDone, character(0))


    }
    # setLibPaths(origLibPaths)
  }


})

