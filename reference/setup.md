# Setup a project library, cache, options

`setup` and `setupOff` are currently deprecated. These may be re-created
in a future version. In its place, a user can simply put
`.libPaths(libs, include.site = FALSE)` in their `.Rprofile` file, where
`libs` is the directory where the packages should be installed and
should be a folder with the R version number, e.g., derived by using
`checkLibPaths(libs)`.

## Usage

``` r
setup(
  newLibPaths,
  RPackageFolders,
  RPackageCache = cacheGetOptionCachePkgDir(),
  standAlone = getOption("Require.standAlone", TRUE),
  verbose = getOption("Require.verbose")
)

setupOff(removePackages = FALSE, verbose = getOption("Require.verbose"))
```

## Arguments

- newLibPaths:

  Same as `RPackageFolders`. This is for more consistent naming with
  `Require(..., libPaths = ...)`.

- RPackageFolders:

  One or more folders where R packages are installed to and loaded from.
  In the case of more than one folder provided, installation will only
  happen in the first one.

- RPackageCache:

  See
  [`?RequireOptions`](https://Require.predictiveecology.org/reference/RequireOptions.md).

- standAlone:

  Logical. If `TRUE`, all packages will be installed to and loaded from
  the `libPaths` only. NOTE: If `TRUE`, THIS WILL CHANGE THE USER'S
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html), similar to
  e.g., the `checkpoint` package. If `FALSE`, then `libPath` will be
  prepended to [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)
  during the `Require` call, resulting in shared packages, i.e., it will
  include the user's default package folder(s). This can be create
  dramatically faster installs if the user has a substantial number of
  the packages already in their personal library. Default `FALSE` to
  minimize package installing.

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

- removePackages:

  Deprecated. Please remove packages manually from
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)
