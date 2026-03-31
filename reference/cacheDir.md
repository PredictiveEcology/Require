# Path to (package) cache directory

Sets (if `create = TRUE`) or gets the cache directory associated with
the `Require` package.

## Usage

``` r
cacheDir(create, verbose = getOption("Require.verbose"))

cachePkgDir(create)
```

## Arguments

- create:

  A logical indicating whether the path should be created if it does not
  exist. Default is `FALSE`.

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

## Value

If `!is.null(cacheGetOptionCachePkgDir())`, i.e., a cache path exists,
the cache directory will be created, with a README placed in the folder.
Otherwise, this function will just return the path of what the cache
directory would be.

## Details

To set a different directory than the default, set the system variable:
`R_USER_CACHE_DIR = "somePath"` and/or
`R_REQUIRE_PKG_CACHE = "somePath"` e.g., in `.Renviron` file or
[`Sys.setenv()`](https://rdrr.io/r/base/Sys.setenv.html). See Note
below.

## Note

Currently, there are 2 different Cache directories used by Require:
`cacheDir` and `cachePkgDir`. The `cachePkgDir` is intended to be a
sub-directory of the `cacheDir`. If you set
`Sys.setenv("R_USER_CACHE_DIR" = "somedir")`, then both the package
cache and cache dirs will be set, with the package cache a
sub-directory. You can, however, set them independently, if you set
`"R_USER_CACHE_DIR"` and `"R_REQUIRE_PKG_CACHE"` environment variable.
The package cache can also be set with
`options("Require.cachePkgDir" = "somedir")`.
