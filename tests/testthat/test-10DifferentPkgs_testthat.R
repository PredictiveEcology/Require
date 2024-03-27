test_that("test 5", {

  setupInitial <- setupTest()
  on.exit(endTest(setupInitial))

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
    Install(pkgs)
    ins <- installed.packages() |> as.data.table()
    expect_true(all(extractPkgName(pkgs) %in% ins$Package))
    # setLibPaths(origLibPaths)
  }


})

