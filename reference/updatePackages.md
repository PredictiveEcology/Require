# Update installed packages with latest available versions

Similar to `update.packages`, but works for archived, non-archived, and
Github packages.

## Usage

``` r
updatePackages(
  libPaths = .libPaths()[1],
  purge = FALSE,
  verbose = getOption("Require.verbose")
)
```

## Arguments

- libPaths:

  The library to update; defaults to `.libPaths()[1]`

- purge:

  Logical. Should the assessment of `installed.packages` purge the
  cached version. Default is `FALSE`

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

## Value

Run for its side effect, namely, updating installed packages to their
latest possible state, whether they are on CRAN currently, archived, or
on GitHub.
