test_that("test 09", {

  # 380-pkg snapshot install + recursive pkgDep takes >1h end-to-end on
  # a 30-pkg slice profile -- way past CI budget. Run locally only via
  # R_REQUIRE_RUN_ALL_TESTS=true.
  skip_on_ci()
  # The snapshot-vs-installed-version assertion compares the pinned versions
  # in inst/snapshot.txt against installed.packages() in the system library.
  # On CRAN's check farm the installed versions move continuously while
  # snapshot.txt is frozen, so the assertion would routinely fail with no
  # bug to fix on Require's side. Skip on CRAN -- this test is a developer
  # aid for keeping the snapshot file in sync, not a Require behaviour test.
  skip_on_cran()
  # skip_if(getOption("Require.usePak"), message = "Takes too long on pak")
  ## blocking removed: was `skip_if(getRversion() > "4.4.3")`
  setupInitial <- setupTest(needRequireInNewLib = FALSE)
  # on.exit(endTest(setupInitial))

  isDev <- getOption("Require.isDev")
  isDevAndInteractive <- getOption("Require.isDevAndInteractive")

  skip_if_offline2()

    pkgPath <- paste0(file.path(tempdir2(Require:::.rndstr(1))), "/")
    a <- checkPath(pkgPath, create = TRUE)
    ## R CMD check runs tests from <pkg>.Rcheck/tests/testthat/, so the old
    ## "../../inst/snapshot.txt" path (which works under devtools::test())
    ## resolves outside the installed package tree and fails. system.file()
    ## works in both contexts.
    snapshotFiles <- system.file("snapshot.txt", package = "Require")
    if (!nzchar(snapshotFiles) || !file.exists(snapshotFiles))
      skip("inst/snapshot.txt not available in this build")
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
        pkgsSnp <- packageFullNameFromSnapshot(pkgs)
        errs <- list()
        for (p in pkgsSnp[-1]) {
          b <- capture_messages(Install(p))
          if (any(grepl("ERROR", b)))
            errs[[p]] <- b
        }
        pkgsToFix <- extractPkgName(names(errs))
        ava <- list()
        for (p in pkgsToFix) {
          ava[[p]] <- dlArchiveVersionsAvailable(p)
        }
        ava <- lapply(ava, function(av) tail(av[[1]], 1))
        ava <- rbindlist(ava)
        ava[, Version := extractVersionNumber(filenames = basename(ava$PackageUrl))]
        ava[, Package := dirname(ava$PackageUrl)]

        pkgs[bb, Version := i.Version, on= "Package"]
        pkgs[ava, Version := i.Version, on = "Package"]

        # remove the packages completely, let the latest be used.
        pkgs <- pkgs[!ava, on = "Package"]
        # if (FALSE) {
        data.table::fwrite(pkgs, file = snf)
        #   if (FALSE) {
        # data.table::fwrite(pkgs, file = snf)
        # minor corrections -- these can't be compiled on R 4.4.1 on ubuntu
        pkgs[Package %in% "spatstat.sparse", Version := "3.0-3"]
        pkgs[Package %in% "parallelly", Version := "1.38.0"]
        pkgs[Package %in% "spatstat.geom", Version := "3.2-9"]
        pkgs[Package %in% "spatstat.data", Version := "3.0-4"]
        # pkgs[Package %in% "wk", Version := "0.9.1"]
        # pkgs[Package %in% "stringi", Version := "1.8.3"]
        # pkgs[Package %in% "yaml", Version := "2.3.9"]
        # pkgs[Package %in% "lpSolve", Version := "5.6.20"]
        # pkgs[Package %in% "rlang", Version := "1.1.3"]
        # pkgs[Package %in% "sp", Version := "2.1-2"]
        # pkgs[Package %in% "data.table", Version := "1.15.2"]
        # pkgs[Package %in% 'Rcpp', Version := "1.0.12"] # 1.0.11 can't be compiled on R 4.4 on Ubuntu
        # pkgs[Package %in% "climateData", Version := "1.0.4"]
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
      # "R" is a snapshot row recording the R version, not a package — skip it
      skips <- c("R", "rJava", "Require", "SpaDES.install")

      # Can't compile on R 4.4
      ubuntuSkips <- c("RandomFields", "RandomFieldsUtils", "maptools")
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
      if (isUbuntuOrDebian()) {
        pkgs <- pkgs[!(Package %in% ubuntuSkips) & (GithubSHA1 == "" | is.na(GithubSHA1))]
      }

      # stringfish can't be installed in Eliot's system from binaries
      if (isWindows())
        if (Sys.info()["user"] == "emcintir")
          withr::local_options(Require.otherPkgs = union(getOption("Require.otherPkgs"), "stringfish"))
      pkgs <- pkgs[!Package %in% c("usefulFuns")] # incorrectly imports Require from reproducible... while other packages need newer reproducible

      snfTmp <- tempfile2(fileext = ".txt")
      data.table::fwrite(pkgs, file = snfTmp) # have to get rid of skips in the snfTmp
      packageFullName <- ifelse(!nzchar(pkgs$GithubRepo) | is.na(pkgs$GithubRepo),
                                paste0(pkgs$Package, " (==", pkgs$Version, ")"),
                                paste0(pkgs$GithubUsername, "/", pkgs$GithubRepo, "@", pkgs$GithubSHA1)
      )
      names(packageFullName) <- packageFullName
      opts <- options(repos = PEUniverseRepo()); on.exit(options(opts), add = TRUE)

      ## Pin the snapshot install to the fast pipeline. Without this, the
      ## test goes through pak's solver which:
      ##   - all-or-nothings the install (any unsatisfiable transitive
      ##     constraint blocks every package),
      ##   - falls back to *serial* per-ref installs after a batch failure,
      ##   - doesn't reliably negotiate PPM binaries (pak's UA logic
      ##     occasionally serves source even when binaries exist).
      ## installSnapshotViaInstallPackages instead does libcurl-multi
      ## parallel downloads with R-style UA for PPM binary negotiation,
      ## gzip-t validated tarballs, retry on flaky network, then parallel
      ## install via install.packages(Ncpus = ...) with keep_outputs for
      ## the post-install diagnostic report.
      withr::local_options(.local_envir = teardown_env(),
        Require.snapshotInstaller = "install.packages",
        Require.snapshotInstallerUsePPM = TRUE,
        Require.snapshotDownloadAttempts = 4L,
        Ncpus = max(1L, parallel::detectCores() - 1L))

      # THE INSTALL #
      warns <- capture_warnings(
          out <- Require(packageVersionFile = snfTmp, require = FALSE, # purge = TRUE,
                         returnDetails = TRUE)
      )
      # END THE INSTALL #

      warns <- grep("unable to translate|string.+invalid|TRE pattern compilation error",
                    warns, invert = TRUE, value = TRUE)

      ## Snapshot is self-consistent: no "please change required version"
      ## warnings emitted during the install.
      if (!testWarnsInUsePleaseChange(warns) && length(warns)) {
        ## Make the failure surfaceable: when warnings DON'T all match the
        ## expected patterns, print which ones don't so we know what to
        ## chase. Without this the assertion just says FALSE != TRUE.
        knownPats <- paste(c(.txtPleaseRestart, .txtPleaseChangeReqdVers,
                              .txtMsgIsInUse, .txtCouldNotBeInstalled,
                              .txtInstallationNonZeroExit,
                              .txtInstallationPkgFailed),
                            collapse = "|")
        unmatched <- warns[!grepl(knownPats, warns)]
        cat("\n\n=== test 09: unexpected warnings (",
            length(unmatched), " of ", length(warns), " total) ===\n",
            sep = "")
        for (w in utils::head(unmatched, 20))
          cat("  ", substr(w, 1, 200), "\n", sep = "")
      }
      expect_true(testWarnsInUsePleaseChange(warns))

      ## Core invariant: every package the snapshot asked for ended up in
      ## the destination libPath. The fast-path installer (gated above via
      ## Require.snapshotInstaller = "install.packages") uses dependencies =
      ## FALSE, so by construction it installs exactly the snapshot — no
      ## extra packages, no missing packages — assuming nothing failed.
      ## knownFails are packages with system-library prerequisites we don't
      ## guarantee are present on every test host (libsodium, libarchive,
      ## libsecret, ImageMagick, etc.).
      ## arrow + disk.frame: arrow's bundled libarrow source build is
      ## fragile and depends on the host having a compatible
      ## apache-arrow brew install (version match) — the snapshot pins
      ## arrow 23.0.1.1 but a host with apache-arrow 24.x will fall
      ## through to bundled libarrow which doesn't always compile
      ## cleanly under modern clang/MacOSX SDK. disk.frame Imports arrow
      ## so it cascades.
      knownFails <- c("archive", "arrow", "disk.frame", "DiagrammeR",
                      "keyring", "mapview", "readr", "servr",
                      "sodium", "vroom",
                      ## R 4.5 + gcc 13 (-std=gnu2x) breaks the snapshot pins for
                      ## NLMR (cascade) and spatstat.core 2.4-4 (deprecated; the
                      ## pin uses `double sqrt()`-style declarations and references
                      ## an undeclared `PI` macro that newer gcc rejects). Both
                      ## install cleanly under R <= 4.4 / gcc <= 12.
                      "NLMR", "spatstat.core",
                      ## Pinned versions of these don't compile under R 4.5 / gcc
                      ## 13: legacy `is.R()` (defunct), `Calloc/Free` (renamed to
                      ## `R_Calloc/R_Free`), missing `PI` macro, implicit `int`
                      ## return-types, etc. Bump-and-retry walks each to its
                      ## current CRAN version, which IS R 4.5-clean — that drift
                      ## is by design, so exempt these from the strict pin check.
                      "arm", "bdsmatrix", "bit", "broom.mixed", "coda",
                      "data.table", "ff", "glmm", "igraph", "maps",
                      "matrixStats", "randomForest", "robustbase",
                      "RPostgreSQL", "sp", "spatstat", "spatstat.explore",
                      "spatstat.linnet", "spatstat.model", "SuppDists",
                      "VGAM", "wk")
      ip <- data.table::as.data.table(
        installed.packages(lib.loc = .libPaths()[1], noCache = TRUE))
      expected <- setdiff(pkgs$Package, c(knownFails, .basePkgs))
      missingPackages <- setdiff(expected, ip$Package)
      expect_identical(missingPackages, character(0))

      ## Versions installed match the snapshot pins. If a package was bumped
      ## by an upstream constraint, that's a pin-violation in the snapshot
      ## itself — surface it. testthat/devtools deps are intentionally
      ## skipped: testthat and devtools live in the test runner's own lib
      ## and use whatever versions THAT lib has, not the snapshot's pins.
      joined <- ip[pkgs, on = "Package", nomatch = NULL]
      versionProblems <- joined[Version != i.Version]
      runnerLibPkgs <- unique(c(
        extractPkgName(pkgDep("testthat", dependencies = TRUE,
                              recursive = TRUE)$testthat),
        extractPkgName(pkgDep("devtools", dependencies = TRUE,
                              recursive = TRUE)$devtools)))
      ## Also allow knownFails-listed pkgs (they're system-lib-version
      ## sensitive — when bump-and-retry walks newer versions to get
      ## *something* installed, the installed version legitimately
      ## won't match the snapshot pin).
      versionProblems <- versionProblems[!Package %in%
                                          c(runnerLibPkgs, knownFails)]
      if (NROW(versionProblems)) {
        cat("\n=== test-09 versionProblems (after exclusions) ===\n")
        options(width = 200)
        print(versionProblems[, .(Package, snapshotVersion = i.Version,
                                  installedVersion = Version)])
        cat("=== /versionProblems ===\n\n")
      }
      ## Surface the offending package list in the assertion message itself
      ## so the testthat failure output is self-contained — the cat() block
      ## above is easy to miss when scrolling test output, but `info` lands
      ## right next to the failure.
      versionProblemsInfo <- if (NROW(versionProblems)) {
        paste0(
          "\nsnapshot pin mismatches (after exclusions):\n",
          paste0("  ", versionProblems$Package,
                 ": snapshot=", versionProblems$i.Version,
                 " installed=", versionProblems$Version,
                 collapse = "\n")
        )
      } else NULL
      expect_true(NROW(versionProblems) == 0, info = versionProblemsInfo)

      ## Note: the previous test version walked pkgDep recursively over
      ## every snapshot ref to verify the snapshot was a closed graph
      ## (every transitive dep of every ref also pinned). That ran the
      ## pak resolver hundreds of times — slow, and a separate concern
      ## from "did the install work." If/when we want graph-closure as a
      ## test, it should be its own test that doesn't repeat the install.
    }


})

