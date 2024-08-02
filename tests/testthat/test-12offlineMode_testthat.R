test_that("test12 Require.offlineMode", {

  skip_on_ci() # These are still experimental
  skip_on_cran() # These are still experimental
  # skip_if(getOption("Require.usePak"), message = "Not an option on usePak = TRUE")
  setupInitial <- setupTest()

  isDev <- getOption("Require.isDev")
  isDevAndInteractive <- getOption("Require.isDevAndInteractive")
  fpcs <- c("fpCompare", "PredictiveEcology/fpCompare")
  cachePurge()
  for (fpc in fpcs) {
    withr::local_options(Require.offlineMode = FALSE)
    fpcPkgName <- extractPkgName(fpc)
    cacheClearPackages(fpcPkgName, ask = FALSE)
    Install(fpc)
    browser()
    mess <- capture_messages(remove.packages(fpcPkgName))
    withr::local_options(Require.offlineMode = TRUE)
    warns <- capture_warnings(Install(fpc))
    expect_true(base::require(fpcPkgName, quietly = TRUE, character.only = TRUE))
    detach(name = paste0("package:", fpcPkgName), unload = TRUE, character.only = TRUE)
    # expect_match(basename(find.package(fpcPkgName)), fpcPkgName)
    mess <- capture_messages(remove.packages(fpcPkgName))
    expect_false(base::require(fpcPkgName, quietly = TRUE, character.only = TRUE))
    cacheClearPackages(fpcPkgName, ask = FALSE)
    warns <- capture_warnings(Install(fpc))
    expect_false(base::require(fpcPkgName, quietly = TRUE, character.only = TRUE))
    expect_match(warns, .txtCouldNotBeInstalled)
    withr::local_options(Require.offlineMode = FALSE)
    # cacheClearPackages(extractPkgName(fpc), ask = FALSE)
    Install(fpc)
    expect_true(base::require(fpcPkgName, quietly = TRUE, character.only = TRUE))
    detach(name = paste0("package:", fpcPkgName), unload = TRUE, character.only = TRUE)
    mess <- capture_messages(remove.packages(fpcPkgName))
    Install(fpc)
    expect_true(base::require(fpcPkgName, quietly = TRUE, character.only = TRUE))
    detach(name = paste0("package:", fpcPkgName), unload = TRUE, character.only = TRUE)
    mess <- capture_messages(remove.packages(fpcPkgName))

  }

})
