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

testthat::test_that("splitGitRepo decodes %2F in a branch name", {
  g <- Require:::splitGitRepo("Acct/Repo@feature%2Fx")
  testthat::expect_identical(g$br[[1]], "feature/x")
  testthat::expect_true(is.na(g$subFolder[[1]]))

  g <- Require:::splitGitRepo("Acct/Repo@feature%2fx/modules/mod")
  testthat::expect_identical(g$br[[1]], "feature/x")
  testthat::expect_identical(g$subFolder[[1]], "modules/mod")

  ## unescaped: the first segment is still the branch
  g <- Require:::splitGitRepo("Acct/Repo@feature/x")
  testthat::expect_identical(g$br[[1]], "feature")
  testthat::expect_identical(g$subFolder[[1]], "x")
})

## parseGitHub() (package refs) and splitGitRepo() (repo downloads) are two parsers of
## one spec. A non-pak install goes through both: parseGitHub() splits the ref, then
## downloadAndBuildToLocalFile() rebuilds `acct/repo@branch` for splitGitRepo(). A "/"
## in the branch broke that hand-off.
testthat::test_that("parseGitHub and splitGitRepo agree on account, repo, branch, subfolder", {
  specs <- c("A/R@dev", "A/R", "A/R/sub@dev", "A/R@fix%2Fpak-x", "A/R/sub@fix%2Fy",
             "A/R@dev (>= 1.0)")
  p <- Require:::parseGitHub(specs)
  g <- Require:::splitGitRepo(specs)
  sub <- if (is.null(p$GitSubFolder)) NA_character_ else p$GitSubFolder
  testthat::expect_identical(p$Account, unname(unlist(g$acct)))
  testthat::expect_identical(p$Repo, unname(unlist(g$repo)))
  testthat::expect_identical(p$Branch, unname(unlist(g$br)))
  testthat::expect_identical(sub, unname(unlist(g$subFolder)))
})

testthat::test_that("parseGitHub: a package ref's branch may contain '/'", {
  p <- Require:::parseGitHub(c("A/R@fix/pak-x", "A/R/sub@fix/sub-y"))
  testthat::expect_identical(p$Account, c("A", "A"))
  testthat::expect_identical(p$Repo, c("R", "R"))
  testthat::expect_identical(p$Branch, c("fix/pak-x", "fix/sub-y"))
  testthat::expect_identical(p$GitSubFolder, c(NA, "sub"))
})

testthat::test_that("joinGitRepo is undone by splitGitRepo", {
  j <- Require:::joinGitRepo(c("A", "A"), c("R", "R"), c("fix/pak-x", "dev"))
  testthat::expect_identical(j, c("A/R@fix%2Fpak-x", "A/R@dev"))
  testthat::expect_identical(unname(unlist(Require:::splitGitRepo(j)$br)), c("fix/pak-x", "dev"))
})

testthat::test_that("non-pak install: the downloaded branch is the whole branch name", {
  got <- NULL
  testthat::local_mocked_bindings(
    downloadRepo = function(gitRepo, subFolder, ...) got <<- list(gitRepo = gitRepo, subFolder = subFolder),
    build = function(...) NULL, .package = "Require")
  p <- Require:::parseGitHub("PredictiveEcology/quickPlot@ci/migrate-to-org-workflow")
  withr::local_dir(withr::local_tempdir())
  Require:::downloadAndBuildToLocalFile(p$Account, p$Repo, p$Branch, "quickPlot",
                                        GitSubFolder = p$GitSubFolder, verbose = -1,
                                        VersionOnRepos = NA)
  g <- Require:::splitGitRepo(got$gitRepo)
  testthat::expect_identical(g$br[[1]], "ci/migrate-to-org-workflow")
  testthat::expect_true(is.na(g$subFolder[[1]]))
  testthat::expect_true(is.null(got$subFolder) || is.na(got$subFolder))
})

## masterMainToHead() matched "@main"/"@master" as a prefix, so a branch that only
## starts with those words was rewritten: `@maintenance` became `@HEADtenance`.
testthat::test_that("masterMainToHead rewrites only a whole 'main' or 'master' branch", {
  testthat::expect_identical(
    Require:::masterMainToHead(c("A/R@main", "A/R@master (>= 1.0)", "A/R@main(>=1.0)", "A/R",
                                 "A/R@maintenance", "A/R@master-fix", "A/R@main/x", "A/R@dev")),
    c("A/R@HEAD", "A/R@HEAD (>= 1.0)", "A/R@HEAD(>=1.0)", "A/R@HEAD",
      "A/R@maintenance", "A/R@master-fix", "A/R@main/x", "A/R@dev"))
})
