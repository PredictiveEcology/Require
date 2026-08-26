## lessThanToAt(): turning a `<` / `<=` version constraint into an exact
## `pkg@version` pin by consulting the package's version history.
##
## Regression guards for two defects that only appear when the history lookup
## comes back empty -- i.e. no released version satisfies the constraint. Both
## were reachable from the ordinary pak install path
## (pakInstallFiltered -> `pkgs[whUnpinned] <- lessThanToAt(pkgs[whUnpinned])`).
##
## pkgHistoryVersions() wraps the pak::pkg_history() call so these can drive
## the empty case without a network round trip.

fakeHistory <- function(versions) {
  function(pkgNoVersion) data.frame(Version = versions, stringsAsFactors = FALSE)
}

test_that("lessThanToAt pins to the highest version satisfying the constraint", {
  testthat::with_mocked_bindings(
    pkgHistoryVersions = fakeHistory(c("1.0.0", "1.5.0", "2.0.0", "2.5.0")),
    .package = "Require",
    {
      out <- Require:::lessThanToAt("somePkg (<2.0.0)")
      expect_identical(out, "somePkg@1.5.0")
    }
  )
})

test_that("lessThanToAt leaves refs without a `<` constraint alone", {
  expect_identical(Require:::lessThanToAt(c("aaa", "bbb (>= 1.0)")),
                   c("aaa", "bbb (>= 1.0)"))
  expect_identical(Require:::lessThanToAt(character(0)), character(0))
})

test_that("lessThanToAt returns the input unchanged when nothing satisfies the constraint", {
  ## THE CRASH: previously the `else` branch assigned an already-emptied pkgDT,
  ## so this returned character(0) and the caller's
  ##   pkgs[whUnpinned] <- lessThanToAt(pkgs[whUnpinned])
  ## died with "replacement has length zero".
  testthat::with_mocked_bindings(
    pkgHistoryVersions = fakeHistory(c("9.0.0", "9.5.0")),   # all too new
    .package = "Require",
    {
      out <- suppressWarnings(Require:::lessThanToAt("somePkg (<1.0.0)"))

      expect_length(out, 1L)
      expect_identical(out, "somePkg (<1.0.0)")
    }
  )
})

test_that("lessThanToAt keeps the vector's length when the only `<` ref is unresolvable", {
  ## the caller assigns back into a subset, so length must be preserved
  pkgs <- c("aaa", "somePkg (<1.0.0)", "bbb")

  testthat::with_mocked_bindings(
    pkgHistoryVersions = fakeHistory(c("9.0.0")),
    .package = "Require",
    {
      out <- suppressWarnings(Require:::lessThanToAt(pkgs))

      expect_length(out, length(pkgs))
      expect_identical(out, pkgs)
      ## and the assignment the caller performs must not error
      target <- c("x", "y", "z")
      expect_no_error(target[seq_along(pkgs)] <- out)
    }
  )
})

test_that("lessThanToAt writes the resolved version onto the right package", {
  ## THE MIS-INDEXING: `hasLT` indexes `pkgs`, `noneAvail` indexes the `hasLT`
  ## subset. The previous `hasLT <- hasLT[!noneAvail]` mismatched both length
  ## and position, so a resolved version could land on the wrong package --
  ## silently, with no error.
  pkgs <- c("leadingPkg", "somePkg (<2.0.0)", "trailingPkg")

  testthat::with_mocked_bindings(
    pkgHistoryVersions = fakeHistory(c("1.0.0", "1.5.0", "2.0.0")),
    .package = "Require",
    {
      out <- suppressWarnings(Require:::lessThanToAt(pkgs))

      expect_length(out, 3L)
      ## the neighbours are untouched ...
      expect_identical(out[1], "leadingPkg")
      expect_identical(out[3], "trailingPkg")
      ## ... and only the constrained one was rewritten
      expect_match(out[2], "^somePkg@")
    }
  )
})

test_that("lessThanToAt warns when no version satisfies the constraint", {
  testthat::with_mocked_bindings(
    pkgHistoryVersions = fakeHistory(c("9.0.0", "9.5.0")),
    .package = "Require",
    expect_warning(Require:::lessThanToAt("somePkg (<1.0.0)"))
  )
})
