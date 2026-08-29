## A snapshot records the R version it was taken under, in a leading "R" row.
## Installing it under a different R routinely fails to build -- pins made
## under an older R can use headers or functions the current one has removed
## (Calloc / Free before STRICT_R_HEADERS, is.R() now defunct) -- and the
## symptom is hundreds of lines of compiler output that never say why.
##
## This was a messageVerbose(), so it was suppressed in quiet sessions and
## swallowed under testthat: the one piece of information that explained the
## failure was the piece most likely to be lost. These pin it as a warning.

snapshotFileWithRversion <- function(rver, dir = withr::local_tempdir(.local_envir = parent.frame())) {
  f <- file.path(dir, "pkgSnapshot.txt")
  data.table::fwrite(
    data.table::data.table(
      Package = c("R", "aaaNotARealPackage"),
      Version = c(rver, "1.0.0")
    ), file = f)
  f
}

test_that("a snapshot from a different R version warns", {
  otherR <- if (getRversion() >= "4.5") "4.4" else "4.6"
  f <- snapshotFileWithRversion(otherR)

  expect_warning(
    try(suppressMessages(
      Require::Require(packageVersionFile = f, require = FALSE, standAlone = TRUE)
    ), silent = TRUE),
    "snapshot was made using R"
  )
})

test_that("the warning names both R versions", {
  otherR <- if (getRversion() >= "4.5") "4.4" else "4.6"
  f <- snapshotFileWithRversion(otherR)

  w <- tryCatch({
    try(suppressMessages(
      Require::Require(packageVersionFile = f, require = FALSE, standAlone = TRUE)
    ), silent = TRUE)
    character()
  }, warning = function(x) conditionMessage(x))

  expect_match(w, otherR, fixed = TRUE)
  expect_match(w, as.character(getRversion()), fixed = TRUE)
})

test_that("the R-version warning survives a silent verbosity setting", {
  ## the regression: as a messageVerbose() this vanished at low verbose, which
  ## is the default in many sessions and always the case under testthat
  otherR <- if (getRversion() >= "4.5") "4.4" else "4.6"
  f <- snapshotFileWithRversion(otherR)

  withr::local_options(Require.verbose = -2)

  expect_warning(
    try(suppressMessages(
      Require::Require(packageVersionFile = f, require = FALSE, standAlone = TRUE)
    ), silent = TRUE),
    "snapshot was made using R"
  )
})

test_that("a snapshot from the running R version does not warn about R", {
  f <- snapshotFileWithRversion(Require:::versionMajorMinor())

  ws <- testthat::capture_warnings(
    try(suppressMessages(
      Require::Require(packageVersionFile = f, require = FALSE, standAlone = TRUE)
    ), silent = TRUE)
  )

  expect_false(any(grepl("snapshot was made using R", ws)))
})
