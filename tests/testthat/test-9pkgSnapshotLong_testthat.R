test_that("test 5", {

  withr::local_package("googledrive")
  setupInitial <- setupTest()
  on.exit(endTest(setupInitial))

  isDev <- getOption("Require.isDev")
  isDevAndInteractive <- getOption("Require.isDevAndInteractive")
  if (isDevAndInteractive && !isMacOSX()) { ## TODO: source installs failing on macOS
    # Require::Install("profvis")
    # 4.3.0 doesn't have binaries, and historical versions of spatial packages won't compile
    pkgPath <- paste0(file.path(tempdir2(Require:::.rndstr(1))), "/")
    a <- checkPath(pkgPath, create = TRUE)

    if (getRversion() <= "4.2.3") {

      snapshotFiles <- rev(
        c("https://raw.githubusercontent.com/PredictiveEcology/WBI_forecasts/development/packageVersions_clean.txt"          ,
          "https://raw.githubusercontent.com/PredictiveEcology/LandWeb/rework-config/packages_2022-03-22.txt"
        ))
      fn <- file.path(pkgPath, "pkgSnapshot.txt")
      download.file(snapshotFiles[2], destfile = fn)

    } else {
      withr::local_tempdir(tmpdir = pkgPath)
      # This file is missing `map` and `tiler` packages, which are dependencies of
      #   `PredictiveEcology/LandWebUtils@dcb26fe3308d0f572de5036d7f115d8eff5f9887`
      #   (`tiler` is actually a dep of `PredictiveEcology/map@development`, so it is recursive need
      #    based on March 12, 2024 version of PredictiveEcology/map@development)
      # This file is missing `SpaDES.project` package, which is a dependency of
      #   `PredictiveEcology/SpaDES.config@94e90b0537b103f83504c96f51be157449e32c9c`

      fnMissing <- c("tiler", "map", "SpaDES.project")
      snapshotFiles <- googledrive::drive_download(googledrive::as_id("1WaJq6DZJxy_2vs2lfzkLG5u3T1MKREa8"),
                                                   overwrite = TRUE)
      fn <- snapshotFiles$local_path

    }
    ## Long pkgSnapshot -- issue 41
    for (snf in snapshotFiles) {
      origLibPaths <- setLibPaths(pkgPath, standAlone = TRUE)
      pkgs <- data.table::fread(fn)

      if (FALSE) {
        # minor corrections
        pkgs[Package %in% "climateData", Version := "1.0.4"]
        pkgs[Package %in% "rnaturalearthhires", Version := "1.0.0.9000"]
        #
        data.table::fwrite(pkgs, file = fn)
        googledrive::drive_update(file = googledrive::as_id("1WaJq6DZJxy_2vs2lfzkLG5u3T1MKREa8"),
                                  media = fn)
      }


      # remove some specifics for tests that are not expected to work
      skips <- c("rJava", "Require", "testit", "SpaDES.install")
      # browser()
      pkgs <- pkgs[!(Package %in% skips)]

      # stringfish can't be installed in Eliot's system from binaries
      if (Sys.info()["user"] == "emcintir")
        options(Require.otherPkgs = union(getOption("Require.otherPkgs"), "stringfish"))
      # pkgs <- pkgs[!Package %in% c("RandomFields", "RandomFieldsUtils")] # the version 1.0-7 is corrupt on RSPM
      pkgs <- pkgs[!Package %in% c("usefulFuns")] # incorrectly imports Require from reproducible... while other packages need newer reproducible

      # ERROR: compilation failed for package 'sf'# on Windows R 4.3.2
      # pkgs[Package %in% "sf", Version := "1.0-9"] # the version 1.0-7 is corrupt on RSPM
      #pkgs[Package %in% "SpaDES.core", `:=`(Version = "1.1.1", GithubRepo = "SpaDES.core",
      #                                      GithubUsername = "PredictiveEcology", GithubRef = "development",
      #                                      GithubSHA1 = "535cd39d84aeb35de29f88b0245c9538d86a1223")]
      # pks <- c("ymlthis", "SpaDES.tools", "amc")
      # pkgs <- pkgs[Package %in% pks]
      data.table::fwrite(pkgs, file = fn) # have to get rid of skips in the fn
      packageFullName <- ifelse(!nzchar(pkgs$GithubRepo), paste0(pkgs$Package, " (==", pkgs$Version, ")"),
                                paste0(pkgs$GithubUsername, "/", pkgs$GithubRepo, "@", pkgs$GithubSHA1)
      )
      names(packageFullName) <- packageFullName

      # remove.packages(pks)
      # unlink(dir(RequirePkgCacheDir(), pattern = paste(pks, collapse = "|"), full.names = TRUE))
      warns <- capture_warnings( # "is in use and will not be installed"
        (out <- Require(packageVersionFile = fn, require = FALSE, dependencies = FALSE, verbose = 2)) |>
          capture_messages() -> mess
      )
      out11 <- pkgDep(packageFullName, recursive = TRUE, simplify = FALSE)

      expect_true(all(grepl("in use and will not be installed", warns)))

      neededBasedOnPackageFullNames <- rbindlistRecursive(out11$deps)
      dups <- duplicated(neededBasedOnPackageFullNames$Package)
      neededBasedOnPackageFullNames <- neededBasedOnPackageFullNames[!dups]
      neededBasedOnPackageFullNames[grep("biosim", ignore.case = TRUE, Package), Package := "BioSIM"] |> invisible()
      packagesBasedOnPackageFullNames <- neededBasedOnPackageFullNames$Package

      tooManyInstalled <- setdiff(packagesBasedOnPackageFullNames, pkgs$Package)
      loaded <- c("testthat", "Require")
      tooManyInstalled <- setdiff(tooManyInstalled, c(fnMissing, loaded))
      expect_true(length(tooManyInstalled) == 0)

      ip <- data.table::as.data.table(installed.packages(lib.loc = .libPaths()[1], noCache = TRUE))
      ip <- ip[!Package %in% .basePkgs]
      allInIPareInPkgs <- all(ip$Package %in% packagesBasedOnPackageFullNames)
      expect_true(allInIPareInPkgs)

      # Check based on Version number
      joined <- ip[pkgs, on = "Package"]
      whDiff <- (joined$Version != joined$i.Version)
      versionProblems <- joined[which(whDiff)]
      testthatDeps <- extractPkgName(pkgDep("testthat", dependencies = TRUE, recursive = TRUE)$testthat)
      versionProblems <- versionProblems[which(!versionProblems$Package %in% testthatDeps)]

      # scales didn't install the "equals" version because a different package needs >= 1.3.0
      versionProblems <- versionProblems[!Package %in% "scales"]
      expect_true(NROW(versionProblems) == 0)

      attrA <- attr(out, "Require")




      # See if any packages are missing
      installedNotInIP <- setdiff(packagesBasedOnPackageFullNames, ip$Package)
      missingPackages <- pkgs[Package %in% installedNotInIP]
      vers <- strsplit(pkgs$Version, "\\.|\\-")
      has4 <- lengths(vers) > 3
      looksLikeGHPkgWithoutGitInfo <- pkgs[has4 & !nzchar(GithubRepo)]$Package
      missingPackages <- missingPackages[!Package %in% looksLikeGHPkgWithoutGitInfo]
      loded <- loadedNamespaces()
      missingPackages <- missingPackages[!Package %in% loded]

      expect_true(NROW(missingPackages) == 0)

      # installedPkgs <- setdiff(packagesBasedOnPackageFullNames, installedNotInIP)
      # allInpkgDTareInIP <- all(installedPkgs %in% ip$Package)
      # if (!isTRUE(allInpkgDTareInIP)) browser()
      # if (!isTRUE(allInIPareInpkgDT)) browser()
      #
      # testthat::expect_true(isTRUE(allInIPareInpkgDT))
      # testthat::expect_true(isTRUE(allInpkgDTareInIP))
      # testthat::expect_true(all(installedNotInIP$installResult == "No available version"))

      # pkgsInOut <- allInpkgDTareInIP
      # theTest <- NROW(ip) >= NROW(pkgsInOut)
      # testthat::expect_true(isTRUE(theTest))
      # browser()


      # These are being installed this 2nd time: why? -- looks like because they were not installed
      #   the first time because they are dependencies of testthat
      # nos <- c("bslib", "desc", "diffobj", "htmlwidgets", "knitr", "pkgbuild", "pkgload",
      #   "rprojroot", "scales", "shiny", "testthat", "tinytex", "withr")
      # nosInd <- nos %in% extractPkgName(pkgDep("testthat", dependencies = TRUE, recursive = TRUE)$testthat)
      # nos[!nosInd] # scales is attempted to be installed, but isn't --> it is lost somewhere

      warns <- capture_warnings(
        lala <- capture.output(type = "message", {
          out2 <- Require(
            packageVersionFile = snf,
            require = FALSE, returnDetails = TRUE, purge = TRUE
          )
        })
      )
      expect_true(all(grepl("in use and will not be installed", warns)))

      att <- attr(out2, "Require")
      att <- att[!duplicated(att$Package)]

      didnt <- att[!is.na(installResult)]

      expect_true(all(didnt$Package %in% c(testthatDeps, looksLikeGHPkgWithoutGitInfo, "Require")))


    }
    setLibPaths(origLibPaths)
  }


})

