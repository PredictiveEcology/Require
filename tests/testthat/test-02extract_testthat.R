test_that("test 1", {

  setupInitial <- setupTest()
  # on.exit(endTest(setupInitial))

  notOnCranOrCI <- getOption("Require.notOnCranOrCI")

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

test_that("stripGitHubToRepos (Require.noRemotes)", {
  # GitHub specs lose account + branch but keep the version constraint
  testthat::expect_identical(
    stripGitHubToRepos("PredictiveEcology/SpaDES.core@development (>= 3.0.3.9003)", verbose = -1),
    "SpaDES.core (>= 3.0.3.9003)"
  )
  testthat::expect_identical(
    stripGitHubToRepos("ianmseddy/LandR.CS@development", verbose = -1),
    "LandR.CS"
  )
  # Plain (non-GitHub) specs are untouched, with or without a version
  testthat::expect_identical(
    stripGitHubToRepos(c("RCurl", "reproducible (>= 2.1.0)"), verbose = -1),
    c("RCurl", "reproducible (>= 2.1.0)")
  )
  # Mixed vector: only the GitHub entries change
  inSpecs <- c("RCurl", "PredictiveEcology/reproducible@development",
               "SpaDES.tools (>= 1.0.0.9001)",
               "PredictiveEcology/SpaDES.project@development (>= 0.0.8.9026)")
  testthat::expect_identical(
    stripGitHubToRepos(inSpecs, verbose = -1),
    c("RCurl", "reproducible", "SpaDES.tools (>= 1.0.0.9001)",
      "SpaDES.project (>= 0.0.8.9026)")
  )
  # Empty input is a no-op
  testthat::expect_identical(stripGitHubToRepos(character(0), verbose = -1), character(0))
})
