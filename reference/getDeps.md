# The `packages` argument may have up to 4 pieces of information for GitHub packages: name, repository, branch, version. For CRAN-alikes, it will only be 2 pieces: name, version. There can also be an inequality or equality, if there is a version.

The `packages` argument may have up to 4 pieces of information for
GitHub packages: name, repository, branch, version. For CRAN-alikes, it
will only be 2 pieces: name, version. There can also be an inequality or
equality, if there is a version.

## Usage

``` r
getDeps(pkgDT, which, recursive, type = type, repos, libPaths, verbose)
```

## Arguments

- pkgDT:

  A `pkgDT` object e.g., from `toPkgDT`

- which:

  a character vector listing the types of dependencies, a subset of
  `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.
  Character string `"all"` is shorthand for that vector, character
  string `"most"` for the same vector without `"Enhances"`.

- recursive:

  Logical. Should dependencies of dependencies be searched, recursively.
  NOTE: Dependencies of suggests will not be recursive. Default `TRUE`.

- type:

  See
  [`utils::install.packages`](https://rdrr.io/r/utils/install.packages.html)

- repos:

  is used for `ap`.

- libPaths:

  A path to search for installed packages. Defaults to
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

## Value

A (named) vector of SaveNames, which is a concatenation of the 2 or 4
elements above, plus the `which` and the `recursive`.

## Details

If version is not supplied, it will take the local, installed version,
if it exists. Otherwise, it is assumed that the HEAD is desired. The
function will find it in the `ap` or on `github.com`. For github
packages, this is obviously a slow step, which can be accelerated if
user supplies a sha or a version e.g.,
getDeps("PredictiveEcology/LandR@development (==1.0.2)")
