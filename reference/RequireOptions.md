# `Require` options

These provide top-level, powerful settings for a comprehensive
reproducible workflow. See Details below.

## Usage

``` r
RequireOptions()

getRequireOptions()
```

## Details

- `RequireOptions()`:

  prints the default values of package options set at startup, which may
  have been changed (e.g., by the user) during the current session.

- `getRequireOptions()`:

  prints the current values of package options.

Below are options that can be set with
`options("Require.xxx" = newValue)`, where `xxx` is one of the values
below, and `newValue` is a new value to give the option. Sometimes these
options can be placed in the user's `.Rprofile` file so they persist
between sessions.

The following options are likely of interest to most users:

- `install`:

  Default: `TRUE`. This is the default argument to `Require`, but does
  not affect `Install`. If this is `FALSE`, then no installations will
  be attempted, and missing packages will result in an error.

- `RPackageCache`:

  Default:
  [`cacheGetOptionCachePkgDir()`](https://Require.predictiveecology.org/reference/cacheGetOptionCachePkgDir.md),
  which must be either a path or a logical. To turn off package caching,
  set this to `FALSE`. This can be set using an environment variable
  e.g., `Sys.setenv(R_REQUIRE_PKG_CACHE = "somePath")`, or
  `Sys.setenv(R_REQUIRE_PKG_CACHE = "TRUE")`; if that is not set, then
  an either a path or logical option
  (`options(Require.cachePkgDir = "somePath")` or
  `options(Require.cachePkgDir = TRUE)`). If `TRUE`, the default folder
  location
  [`cachePkgDir()`](https://Require.predictiveecology.org/reference/cacheDir.md)
  will be used. If this is `TRUE` or a path is provided, then binary and
  source packages will be cached here. Subsequent downloads of same
  package will use local copy. Default is to have packages not be cached
  locally so each install of the same version will be from the original
  source, e.g., CRAN, GitHub.

- `otherPkgs`:

  Default: A character vector of packages that are generally more
  successful if installed from Source on Unix-alikes. Since there are
  repositories that offer binary packages builds for Linux (e.g.,
  RStudio Package Manager), the vector of package names indicated here
  will default to a standard CRAN repository, forcing a source install.
  See also `spatialPkgs` option, which does the same for spatial
  packages.

- `purge`:

  Default: `FALSE`. If set to (almost) all internal caches used by
  `Require` will be deleted and rebuilt. This should not generally be
  necessary as it will automatically be deleted after (by default) 1
  hour (set via `R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE` environment
  variable in seconds).

- `spatialPkgs`:

  Default: A character vector of packages that are generally more
  successful if installed from Source on Unix-alikes. Since there are
  repositories that offer binary packages builds for Linux (e.g., Posit
  Package Manager), the vector of package names indicated here will
  default to a standard CRAN repository, forcing a source install. See
  also `otherPkgs` option, which does the same for non-spatial packages.

- `useCranCache`:

  Default: `FALSE`. A user can optionally use the locally cached
  packages that are available due to a user's use of the `crancache`
  package.

- `verbose`:

  Default: `1`. See ?Require.
