test_that("test 1", {

  setupInitial <- setupTest()
  # on.exit(endTest(setupInitial))

  isDev <- getOption("Require.isDev")

  a <- extractPkgName("Require (>=0.0.1)")
  testthat::expect_true({
    isTRUE(all.equal("Require", a))
  })
  a <- extractPkgName("PredictiveEcology/Require (>=0.0.1)")
  testthat::expect_true({
    isTRUE(all.equal("Require", a))
  })

  a <- extractVersionNumber("Require (<=0.0.1)")
  testthat::expect_true({
    isTRUE(all.equal("0.0.1", a))
  })
  a <- extractVersionNumber("PredictiveEcology/Require (>=0.0.1)")
  testthat::expect_true({
    isTRUE(all.equal("0.0.1", a))
  })

  a <- extractInequality("Require (<=0.0.1)")
  testthat::expect_true({
    isTRUE(all.equal("<=", a))
  })
  a <- extractInequality("Require (==0.0.1)")
  testthat::expect_true({
    isTRUE(all.equal("==", a))
  })
  a <- extractInequality("Require (>=0.0.1)")
  testthat::expect_true({
    isTRUE(all.equal(">=", a))
  })

  a <- extractPkgGitHub("PredictiveEcology/Require")
  testthat::expect_true({
    isTRUE(all.equal("Require", a))
  })
  a <- extractPkgGitHub("PredictiveEcology/Require (>=0.0.1)")
  testthat::expect_true({
    isTRUE(all.equal("Require", a))
  })
  a <- extractPkgGitHub("Require (>=0.0.1)")
  testthat::expect_true({
    identical(is.na(NA), is.na(a))
  }) # Seems to be different class under different conditions

  a <- trimVersionNumber("PredictiveEcology/Require (<=0.0.1)")
  testthat::expect_true({
    isTRUE(all.equal("PredictiveEcology/Require", a))
  })
  a <- trimVersionNumber("Require (<=0.0.1)")
  testthat::expect_true({
    isTRUE(all.equal("Require", a))
  })

  out <- parseGitHub("r-forge/mumin/pkg")
  testthat::expect_true({
    "hasSubFolder" %in% colnames(out)
  })

})
