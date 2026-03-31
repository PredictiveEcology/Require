# Available and archived versions

These are wrappers around available.packages and also get the archived
versions available on CRAN.

## Usage

``` r
dlArchiveVersionsAvailable(
  package,
  repos = getOption("repos"),
  verbose = getOption("Require.verbose")
)

available.packagesCached(
  repos,
  purge,
  verbose = getOption("Require.verbose"),
  returnDataTable = TRUE,
  type
)
```

## Arguments

- package:

  A single package name (without version or github specifications)

- repos:

  The remote repository (e.g., a CRAN mirror), passed to either
  `install.packages`, `install_github` or `installVersions`.

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

- purge:

  Logical. Should all caches be purged? Default is
  `getOption("Require.purge", FALSE)`. There is a lot of internal
  caching of results throughout the `Require` package. These help with
  speed and reduce calls to internet sources. However, sometimes these
  caches must be purged. The cached values are renewed when found to be
  too old, with the age limit. This maximum age can be set in seconds
  with the environment variable
  `R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE`, or if unset, defaults to
  3600 (one hour – see
  [`utils::available.packages`](https://rdrr.io/r/utils/available.packages.html)).

  Internally, there are calls to `available.packages`.

- returnDataTable:

  Logical. If `TRUE`, the default, then the return is a data.table.
  Otherwise, it is a `matrix`, as per `available.packages`

- type:

  See
  [`utils::install.packages`](https://rdrr.io/r/utils/install.packages.html)

## Details

`dlArchiveVersionsAvailable` searches CRAN Archives for available
versions. It has been borrowed from a sub-set of the code in a
non-exported function: `remotes:::download_version_url`
