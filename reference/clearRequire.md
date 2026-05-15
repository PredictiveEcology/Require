# Clear cached package tarballs

Removes downloaded package archives from the cache that
[`cachePkgDir()`](https://Require.predictiveecology.org/reference/cacheDir.md)
returns. With `usePak = TRUE` (the default) this delegates to
[`pak::cache_clean()`](https://pak.r-lib.org/reference/cache.html) (no
`packages` arg) or `pak::cache_delete(package = ...)` (selective). With
`usePak = FALSE` it walks Require's legacy bookkeeping dir and unlinks
tarballs there.

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
  cannot specify version number) to remove from the cache.

- ask:

  Logical. If `TRUE`, asks the user to confirm before deleting.

- Rversion:

  An R version (major.minor, e.g., `"4.2"`). Defaults to the current R
  version. **Ignored under `usePak = TRUE`** – see "Migration note".

- clearCranCache:

  Logical. If `TRUE`, also clears the local `crancache` cache, which is
  only relevant if `options(Require.useCranCache = TRUE)`.

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

## Details

Require's own bookkeeping (SHA DB, mirrors.csv, available.packages
snapshots) is preserved by this function; use
[`cachePurge()`](https://Require.predictiveecology.org/reference/cachePurge.md)
to clear those as well.

`clearRequirePackageCache()` is a deprecated alias.

## Migration note

The `Rversion` parameter is honoured only on the legacy path – pak's
cache is not partitioned by R version the way Require's cache was, so
[`pak::cache_clean()`](https://pak.r-lib.org/reference/cache.html)/[`pak::cache_delete()`](https://pak.r-lib.org/reference/cache.html)
operate on the whole cache regardless of `Rversion`. The function emits
a verbose note when `Rversion != versionMajorMinor()` under
`usePak = TRUE`.
