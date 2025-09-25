testthat::test_that("Require does not interfere with R_USER_CACHE_DIR", {
  # setupInitial <- setupTest()

  ## Require should not be modifying R_USER_CACHE_DIR
  testthat::expect_true(basename(Sys.getenv("R_USER_CACHE_DIR")) != "Require")

  ## check for duplicate 'R/Require/R/Require'
  testthat::expect_true(basename(tools::R_user_dir("Require", "cache")) !=
                          basename(dirname(dirname(tools::R_user_dir("Require", "cache")))))
})
