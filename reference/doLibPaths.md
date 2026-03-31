# Deals with missing libPaths arg, and takes first

Deals with missing libPaths arg, and takes first

## Usage

``` r
doLibPaths(libPaths, standAlone = FALSE)
```

## Arguments

- libPaths:

  The library path (or libraries) where all packages should be
  installed, and looked for to load (i.e., call `library`). This can be
  used to create isolated, stand alone package installations, if used
  with `standAlone = TRUE`. Currently, the path supplied here will be
  prepended to [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)
  (temporarily during this call) to `Require` if `standAlone = FALSE` or
  will set (temporarily)
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) to
  `c(libPaths, tail(libPaths(), 1)` to keep base packages.

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
