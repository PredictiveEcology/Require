## getGitCredsToken(): gitcreds first, environment as fallback.
##
## gitcreds::gitcreds_get() reads the git credential store and ignores
## GITHUB_PAT/GITHUB_TOKEN, so a CI runner -- which sets the env var but
## configures no credential helper -- got no token at all and fell back to
## anonymous requests, capped at 60/hour per IP. That is the regression these
## guard: whichever job exhausted the shared quota failed with HTTP 403, so the
## failure appeared to wander between matrix legs.

test_that("envToken reads GITHUB_PAT", {
  withr::local_envvar(c(GITHUB_PAT = "ghp_fromPat", GITHUB_TOKEN = ""))

  expect_identical(Require:::envToken(), "token ghp_fromPat")
})

test_that("envToken falls back to GITHUB_TOKEN", {
  withr::local_envvar(c(GITHUB_PAT = "", GITHUB_TOKEN = "ghs_fromToken"))

  expect_identical(Require:::envToken(), "token ghs_fromToken")
})

test_that("envToken prefers GITHUB_PAT when both are set", {
  withr::local_envvar(c(GITHUB_PAT = "ghp_first", GITHUB_TOKEN = "ghs_second"))

  expect_identical(Require:::envToken(), "token ghp_first")
})

test_that("envToken is NULL when neither is set", {
  withr::local_envvar(c(GITHUB_PAT = "", GITHUB_TOKEN = ""))

  expect_null(Require:::envToken())
})

test_that("getGitCredsToken uses the environment when the credential store is empty", {
  ## the regression: this used to return NULL, leaving requests unauthenticated
  withr::local_envvar(c(GITHUB_PAT = "ghp_fromEnv", GITHUB_TOKEN = ""))

  testthat::with_mocked_bindings(
    gitcredsToken = function() NULL,
    .package = "Require",
    expect_identical(getGitCredsToken(), "token ghp_fromEnv")
  )
})

test_that("getGitCredsToken prefers the credential store over the environment", {
  ## the environment is only a fallback; a developer machine keeps its behaviour
  withr::local_envvar(c(GITHUB_PAT = "ghp_fromEnv", GITHUB_TOKEN = ""))

  testthat::with_mocked_bindings(
    gitcredsToken = function() "token gho_fromStore",
    .package = "Require",
    expect_identical(getGitCredsToken(), "token gho_fromStore")
  )
})

test_that("getGitCredsToken is NULL when neither source has anything", {
  withr::local_envvar(c(GITHUB_PAT = "", GITHUB_TOKEN = ""))

  testthat::with_mocked_bindings(
    gitcredsToken = function() NULL,
    .package = "Require",
    expect_null(getGitCredsToken())
  )
})

test_that("the token getGitCredsToken returns is the shape GETWauthThenNonAuth signs with", {
  ## GETWauthThenNonAuth() passes it straight to
  ## httr::add_headers(Authorization = token), so it has to carry the "token "
  ## prefix. A bare PAT would silently authenticate nothing.
  withr::local_envvar(c(GITHUB_PAT = "ghp_shapeCheck", GITHUB_TOKEN = ""))

  testthat::with_mocked_bindings(
    gitcredsToken = function() NULL,
    .package = "Require",
    expect_match(getGitCredsToken(), "^token ")
  )
})
