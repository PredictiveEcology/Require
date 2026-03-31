# Clear Require Cache elements

Clear Require Cache elements

## Usage

``` r
cacheClearPackages(
  packages,
  ask = interactive(),
  Rversion = versionMajorMinor(),
  clearCranCache = FALSE,
  verbose = getOption("Require.verbose")
)

clearRequirePackageCache(
  packages,
  ask = interactive(),
  Rversion = versionMajorMinor(),
  clearCranCache = FALSE,
  verbose = getOption("Require.verbose")
)
```

## Arguments

- packages:

  Either missing or a character vector of package names (currently
  cannot specify version number) to remove from the local Require Cache.

- ask:

  Logical. If `TRUE`, then it will ask user to confirm

- Rversion:

  An R version (major dot minor, e.g., "4.2"). Defaults to current R
  version.

- clearCranCache:

  Logical. If `TRUE`, then this will also clear the local `crancache`
  cache, which is only relevant if
  `options(Require.useCranCache = TRUE)`, i.e., if `Require` is using
  the `crancache` cache also

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).
