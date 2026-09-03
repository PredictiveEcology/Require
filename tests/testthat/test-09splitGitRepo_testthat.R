## splitGitRepo() had no tests. The branch used to be read positionally -- the
## 3rd element of strsplit(gitRepo, "/|@") -- which is correct for
## `Acct/Repo@branch` and `Acct/Repo@branch/subFolder`, but for
## `Acct/Repo/subFolder@branch` it returned the subFolder as the branch and
## discarded the real one. Both spellings are in use for a package or SpaDES
## module living in a subdirectory of its repo.

testthat::test_that("splitGitRepo parses the documented account/repo@branch forms", {
  g <- Require:::splitGitRepo("PredictiveEcology/Require@development")
  testthat::expect_identical(g$acct[[1]], "PredictiveEcology")
  testthat::expect_identical(g$repo[[1]], "Require")
  testthat::expect_identical(g$br[[1]], "development")
  testthat::expect_true(is.na(g$subFolder[[1]]))

  ## no branch -> HEAD
  g <- Require:::splitGitRepo("PredictiveEcology/Require")
  testthat::expect_identical(g$br[[1]], "HEAD")

  ## bare repo -> default account
  g <- Require:::splitGitRepo("Require")
  testthat::expect_identical(g$acct[[1]], "PredictiveEcology")
  testthat::expect_identical(g$repo[[1]], "Require")

  g <- Require:::splitGitRepo("Require", default = "someoneElse")
  testthat::expect_identical(g$acct[[1]], "someoneElse")
})

testthat::test_that("splitGitRepo finds the branch regardless of where '@' falls", {
  ## subFolder AFTER the branch
  g <- Require:::splitGitRepo("Acct/Repo@main/subDir")
  testthat::expect_identical(g$br[[1]], "main")
  testthat::expect_identical(g$subFolder[[1]], "subDir")

  ## subFolder BEFORE the branch -- this used to yield br = "subDir"
  g <- Require:::splitGitRepo("Acct/Repo/subDir@main")
  testthat::expect_identical(g$br[[1]], "main")
  testthat::expect_identical(g$subFolder[[1]], "subDir")

  ## a nested subFolder path
  g <- Require:::splitGitRepo("Acct/Repo/a/b@main")
  testthat::expect_identical(g$br[[1]], "main")
  testthat::expect_identical(g$subFolder[[1]], "a/b")
})

testthat::test_that("splitGitRepo keeps the version spec separate", {
  g <- Require:::splitGitRepo("PredictiveEcology/Require@development (>= 1.0.0)")
  testthat::expect_identical(g$repo[[1]], "Require")
  testthat::expect_identical(g$br[[1]], "development")
  testthat::expect_identical(g$versionSpec[[1]], "(>= 1.0.0)")
})

testthat::test_that("splitGitRepo is vectorised and names elements by repo", {
  g <- Require:::splitGitRepo(c("a/b@c", "x/y/z@w"))
  testthat::expect_identical(unname(unlist(g$repo)), c("b", "y"))
  testthat::expect_identical(unname(unlist(g$br)), c("c", "w"))
  testthat::expect_identical(unname(unlist(g$subFolder)), c(NA_character_, "z"))
  testthat::expect_identical(names(g$repo), c("b", "y"))
})
