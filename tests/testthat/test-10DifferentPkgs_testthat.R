test_that("test 10", {
  setupInitial <- setupTest(needRequireInNewLib = TRUE)
  # on.exit(endTest(setupInitial))

  isDev <- getOption("Require.isDev")
  isDevAndInteractive <- getOption("Require.isDevAndInteractive")
  if (isDevAndInteractive && !isMacOSX()) { ## TODO: source installs failing on macOS
    # 4.3.0 doesn't have binaries, and historical versions of spatial packages won't compile
    pkgs <- c('reproducible',
              'SpaDES.core (>= 2.0.3)',
              ## other:
              'aws.s3',
              'bcgov/climr@devl (HEAD)',
              'bcgov/ccissr@main (HEAD)',
              'crayon',
              'data.table',
              'foreach',
              'gdalUtilities',
              'ggplot2',
              'terra',
              'themis',
              'tidymodels')
    if (isLinux()) {
      origRepos2 <- setLinuxBinaryRepo()
      on.exit(options(origRepos2))
    }

    # warnsReq <- capture_warnings(Require::Install("Require"))
    Install(pkgs) |>
      capture_warnings() -> warns

    if (getOption("Require.installPackagesSys") < 2)
      warns <- grep("installation of package.+cissr.+had non-zero exit status", invert = TRUE, warns)
    test <- testWarnsInUsePleaseChange(warns)
    expect_true(test)

    ins <- installed.packages(noCache = TRUE) |> as.data.table()
    notInstalled <- setdiff(extractPkgName(pkgs), ins$Package)
    notInstalled <- setdiff(notInstalled, loadedNamespaces())

    # Currently failing to install ccissr because of an incorrect exports
    #   in climr July 7 ## TODO
    notInstalled <- setdiff(notInstalled, "ccissr")
    expect_identical(notInstalled, character(0))
  }


})

