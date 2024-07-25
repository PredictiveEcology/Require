test_that("test 5b", {

  # skip_if(getOption("Require.usePak"), message = "Not an option on usePak = TRUE")
  setupInitial <- setupTest()

  isDev <- getOption("Require.isDev")
  isDevAndInteractive <- getOption("Require.isDevAndInteractive")

  if (isDevAndInteractive) {
    # Use a mixture of different types of "off CRAN"
    if (!isMacOSX()) {
      pkgs <- c("knn", "ggplot2 (==3.4.3)", "silly1", "SpaDES.core")
      pkgsClean <- extractPkgName(pkgs)
      lala <- try(suppressWarnings(capture.output(suppressMessages(remove.packages(pkgsClean)))), silent = TRUE)

      # ERROR: dependency 'Require' is not available for package 'SpaDES.core' --> doesn't show up
      acceptableFails <- c("Require", "SpaDES.core")
      warns <- capture_warnings(# package 'Require' is in use and will not be installed
        out22 <- Require(pkgs, require = FALSE, returnDetails = TRUE)
      )
      ip <- installed.packages() ## silly1 won't be installed

      # depending on whether SpaDES.core gets installed... could be poss1 or poss2
      poss1 <- identical(setdiff(pkgsClean, ip[, "Package"]), pkgs[[3]])  # installs SpaDES.core anyway

      # This one fails to install SpaDES.core because already loaded
      poss2 <- sum(pkgsClean %in% ip[, "Package"]) ==
        length(pkgsClean) - length(acceptableFails) ## TODO: fails on macOS
      expect_true(poss1 || poss2)
    }

    ## Test Install and also (HEAD)
    messToSilence <- capture_messages(try(remove.packages("fpCompare"), silent = TRUE))
    capted1 <- capture_messages(
      out1 <- Install("PredictiveEcology/fpCompare@development (HEAD)", verbose = 5, returnDetails = TRUE) # will install
    )
    capted2 <- capture_messages(
      out2 <- Install("PredictiveEcology/fpCompare@development (HEAD)", verbose = 5, returnDetails = TRUE) # will install
    )
    theGrep1 <- .txtInstallingColon
    theGrep2 <- "SHA1 has not"
    if (!isTRUE(getOption("Require.usePak"))) {
      if (isWindows())
        testthat::expect_true(isTRUE(sum(grepl(theGrep1, capted1)) == 1))
      testthat::expect_true(isTRUE(sum(grepl(theGrep2, capted2)) == 1))
    }
    # two sources, where both are OK; use CRAN by preference
    if (!isMacOSX()) {
      lala <- suppressWarnings(capture.output(suppressMessages(
        remove.packages("SpaDES.core")))) ## TODO: fails on macOS
      suppressWarnings(
        out <- Require(c("PredictiveEcology/SpaDES.core@development (>=1.1.2)",
                         "SpaDES.core (>=1.0.0)"),
                       require = FALSE, returnDetails = TRUE
        )
      )
      out2 <- attr(out, "Require")
      # try(unlink(dir(cachePkgDir(), pattern = "SpaDES.core", full.names = TRUE)))

      if (!isTRUE(getOption("Require.usePak"))) {
        testthat::expect_true(out2[Package == "SpaDES.core"]$installFrom %in% c("CRAN", .txtLocal))
        # if (isWindows())
        testthat::expect_true(out2[Package == "SpaDES.core"]$installed)
      } else {
        testthat::expect_true(out2[Package == "SpaDES.core"]$installResult == "OK")
      }

    }
  }

})
