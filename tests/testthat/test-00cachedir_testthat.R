testthat::test_that("Require does not interfere with R_USER_CACHE_DIR", {
  ## Require should not be modifying R_USER_CACHE_DIR
  testthat::expect_true(basename(Sys.getenv("R_USER_CACHE_DIR")) != "Require")

  ## check for duplicate 'R/Require/R/Require'
  testthat::expect_true(
    basename(tools::R_user_dir("Require", "cache")) !=
      basename(dirname(dirname(tools::R_user_dir("Require", "cache"))))
  )
})

testthat::test_that("setting 'R_REQUIRE_CACHE' and 'R_REQUIRE_PKG_CACHE' works", {
  testCacheDir <- file.path(tempdir(), "testCacheDir")
  testCachePkgDir <- file.path(testCacheDir, "packages")

  withr::with_envvar(list(R_REQUIRE_CACHE = testCacheDir, R_REQUIRE_PKG_CACHE = testCachePkgDir), {
    testthat::expect_identical(cacheDir(), testCacheDir)
    testthat::expect_identical(cacheGetOptionCachePkgDir(), Require:::rPkgDir(testCachePkgDir))
  })
})
