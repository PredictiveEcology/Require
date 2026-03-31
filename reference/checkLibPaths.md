# Creates the directories, and adds version number

Creates the directories, and adds version number

## Usage

``` r
checkLibPaths(libPaths, ifMissing, exact = FALSE, ...)
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

- ifMissing:

  An alternative path if `libPaths` argument is missing.

- exact:

  Logical. If `FALSE`, the default, then `checkLibPaths` will append the
  R version number on the `libPaths` supplied. If `TRUE`,
  `checkLibPaths` will return exactly the `libPaths` supplied.

- ...:

  Not used, but allows other functions to pass through arguments.
