# Get the option for `Require.cachePkgDir` (deprecated)

**Deprecated.** Use
[`cachePkgDir()`](https://Require.predictiveecology.org/reference/cacheDir.md)
instead – it is now the single getter for the package-tarball cache and
wraps `pak::cache_summary()$cachepath` under `usePak = TRUE` (the
default).

This function is kept for one release cycle as a functional shim. It
still resolves a user-supplied path from
`options("Require.cachePkgDir")` or the `R_REQUIRE_PKG_CACHE`
environment variable when set, but those two knobs are themselves
deprecated and ignored under `usePak = TRUE`. To redirect pak's package
cache to a shared location, set `R_USER_CACHE_DIR` in `.Renviron` (pak's
standard env var). See
[`cacheDir()`](https://Require.predictiveecology.org/reference/cacheDir.md)
for the full migration table.

Resolution order (legacy path):

1.  If `R_REQUIRE_PKG_CACHE` is set, return it.

2.  Else if `options("Require.cachePkgDir")` is character, return it.

3.  Else if the option is `TRUE`, return `cachePkgDir(FALSE)`.

4.  Else if the option is `FALSE`, return `NULL`.

5.  Otherwise, return `cachePkgDir(FALSE)`.

## Usage

``` r
cacheGetOptionCachePkgDir()
```

## Value

The package cache directory, or `NULL` when caching is disabled.
Deprecated in favour of
[`cachePkgDir()`](https://Require.predictiveecology.org/reference/cacheDir.md).
