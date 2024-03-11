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
    checkPath(pkgPath, create = TRUE)

    if (getRversion() <= "4.2.3") {

      snapshotFiles <- rev(
        c("https://raw.githubusercontent.com/PredictiveEcology/WBI_forecasts/development/packageVersions_clean.txt"          ,
          "https://raw.githubusercontent.com/PredictiveEcology/LandWeb/rework-config/packages_2022-03-22.txt"
        ))
      fn <- file.path(pkgPath, "pkgSnapshot.txt")
      download.file(snapshotFiles[2], destfile = fn)

    } else {
      withr::local_tempdir(pkgPath)
      snapshotFiles <- googledrive::drive_download(googledrive::as_id("1WaJq6DZJxy_2vs2lfzkLG5u3T1MKREa8"),
                                                   overwrite = TRUE)
      fn <- snapshotFiles$local_path
    }
    ## Long pkgSnapshot -- issue 41
    for (snf in snapshotFiles) {
      origLibPaths <- setLibPaths(pkgPath, standAlone = TRUE)
      pkgs <- data.table::fread(fn)


      # remove some specifics for tests that are not expected to work
      skips <- c("rJava", "Require", "testit", "SpaDES.install")
      # browser()
      pkgs <- pkgs[!(Package %in% skips)]

      # stringfish can't be installed in Eliot's system from binaries
      if (Sys.info()["user"] == "emcintir")
        options(Require.otherPkgs = setdiff(getOption("Require.otherPkgs"), "stringfish"))
      pkgs <- pkgs[!Package %in% c("RandomFields", "RandomFieldsUtils")] # the version 1.0-7 is corrupt on RSPM
      pkgs <- pkgs[!Package %in% c("usefulFuns")] # incorrectly imports Require from reproducible... while other packages need newer reproducible
      pkgs[Package %in% "sf", Version := "1.0-9"] # the version 1.0-7 is corrupt on RSPM
      #pkgs[Package %in% "SpaDES.core", `:=`(Version = "1.1.1", GithubRepo = "SpaDES.core",
      #                                      GithubUsername = "PredictiveEcology", GithubRef = "development",
      #                                      GithubSHA1 = "535cd39d84aeb35de29f88b0245c9538d86a1223")]
      # pks <- c("ymlthis", "SpaDES.tools", "amc")
      # pkgs <- pkgs[Package %in% pks]
      data.table::fwrite(pkgs, file = fn) # have to get rid of skips in the fn
      packageFullName <- ifelse(is.na(pkgs$GithubRepo), paste0(pkgs$Package, " (==", pkgs$Version, ")"),
                                paste0(pkgs$GithubUsername, "/", pkgs$GithubRepo, "@", pkgs$GithubSHA1)
      )
      names(packageFullName) <- packageFullName

      # remove.packages(pks)
      # unlink(dir(RequirePkgCacheDir(), pattern = paste(pks, collapse = "|"), full.names = TRUE))
      out <- Require(packageVersionFile = fn, require = FALSE, dependencies = FALSE)
      out11 <- pkgDep(packageFullName, recursive = TRUE)
      allNeeded <- unique(extractPkgName(unname(c(names(out11), unlist(out11)))))
      allNeeded <- allNeeded[!allNeeded %in% .basePkgs]
      persLibPathOld <- pkgs$LibPath[which(pkgs$Package == "amc")]
      # pkgDT <- attr(out, "Require")
      # pkgsInOut <- extractPkgName(pkgDT$Package[pkgDT$installed])
      installedInFistLib <- pkgs[LibPath == persLibPathOld]
      # testthat::expect_true(all(installed))
      ip <- data.table::as.data.table(installed.packages(lib.loc = .libPaths()[1], noCache = TRUE))
      ip <- ip[!Package %in% .basePkgs]
      allInIPareInpkgDT <- all(ip$Package %in% allNeeded)
      installedNotInIP <- setdiff(allNeeded, ip$Package)

      installedPkgs <- setdiff(allNeeded, installedNotInIP)
      allInpkgDTareInIP <- all(installedPkgs %in% ip$Package)
      if (!isTRUE(allInpkgDTareInIP)) browser()
      if (!isTRUE(allInIPareInpkgDT)) browser()

      testthat::expect_true(isTRUE(allInIPareInpkgDT))
      testthat::expect_true(isTRUE(allInpkgDTareInIP))
      # testthat::expect_true(all(installedNotInIP$installResult == "No available version"))

      pkgsInOut <- allInpkgDTareInIP
      theTest <- NROW(ip) >= NROW(pkgsInOut)
      testthat::expect_true(isTRUE(theTest))
      browser()

      lala <- capture.output(type = "message", {
        out2 <- Require(
          packageVersionFile = fn,
          require = FALSE, verbose = 2, purge = TRUE
        )
      })
      # missings <- grep("The following shows packages", lala, value = TRUE)
      # missings <- gsub(".+: (.+); adding .+", "\\1", missings)
      # missings <- strsplit(missings, ", ")[[1]]
      #
      # if (any(grepl(Require:::messageFollowingPackagesIncorrect, lala))) {
      #   lastLineOfMessageDF <- tail(grep(":", lala), 1)
      #   NnotInstalled <- as.integer(strsplit(lala[lastLineOfMessageDF], split = ":")[[1]][1])
      # } else {
      #   NnotInstalled <- 0
      # }
      browser()
      allNeeded <- setdiff(allNeeded, skips)
      installedPkgs <- setdiff(installedPkgs, skips)

      theTest <- NROW(installedPkgs) == NROW(allNeeded)
      if (isDevAndInteractive) if (!isTRUE(theTest)) browser()
      testthat::expect_true(isTRUE(theTest))

      theTest2 <- NROW(ip[Package %in% allNeeded]) == NROW(allNeeded)
      testthat::expect_true(isTRUE(theTest2))

    }
    setLibPaths(origLibPaths)
  }


})
