# Path to (package) cache directory

`cacheDir()` returns Require's own scratch directory (SHA database,
available.packages snapshots, mirrors.csv, pkgDep cache);
`cachePkgDir()` returns the package binary tarball cache.

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

A path string. When `create = TRUE`, the directory is created (with a
`README` placed in `cacheDir()`'s root if absent); otherwise the
function just returns what the path would be.

## What goes where

|  |  |  |  |
|----|----|----|----|
| Function | What it holds | Default location | Knob |
| `cacheDir()` | Require-internal bookkeeping (SHA DB, mirrors.csv, pkgDep cache) | `tools::R_user_dir("Require", "cache")` | `R_REQUIRE_CACHE` |
| `cachePkgDir()` | Package binary tarballs | pak's `cache_summary()$cachepath` (pak mode) | `R_USER_CACHE_DIR` (via pak) |
| `cachePkgDir()` | Package binary tarballs | `<cacheDir>/packages/<Rver>` (legacy) | `R_REQUIRE_CACHE` |

Both defaults flow from
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html), so
setting `R_USER_CACHE_DIR=/some/path` in `.Renviron` redirects **both**
caches to sibling subdirectories of `/some/path/R/` – pak's cache lands
in `pkgcache/pkg/`, Require's in `Require/`. That's the one-knob way to
set up a shared cache across machines or R versions.

## How `cachePkgDir()` changes with `usePak`

- `getOption("Require.usePak", TRUE)` (default):

  Thin wrapper over `pak::cache_summary()$cachepath`. The directory is
  owned by pak/pkgcache; location is controlled by `R_USER_CACHE_DIR`
  (read at pak's subprocess spawn time). Default:
  `tools::R_user_dir("pkgcache", "cache")/pkg`.

- `usePak = FALSE` (legacy):

  Returns `<cacheDir>/packages/<Rver>`, controlled by `R_REQUIRE_CACHE`.

Require-internal bookkeeping files always live next to the legacy path
(`<cacheDir>/packages/<Rver>`) regardless of `usePak` – pak doesn't know
about them and would treat them as stray files.

## Deprecations

The following Require-specific knobs and helpers were folded into the
pair above. Each is still functional for one release cycle and emits a
deprecation warning when used.

|  |  |
|----|----|
| Deprecated | Use instead |
| [`cacheGetOptionCachePkgDir()`](https://Require.predictiveecology.org/reference/cacheGetOptionCachePkgDir.md) | `cachePkgDir()` |
| `rpackageFolder()` (internal) | (inlined into [`checkLibPaths()`](https://Require.predictiveecology.org/reference/checkLibPaths.md)) |
| [`purgeCache()`](https://Require.predictiveecology.org/reference/cachePurge.md) | [`cachePurge()`](https://Require.predictiveecology.org/reference/cachePurge.md) |
| [`clearRequirePackageCache()`](https://Require.predictiveecology.org/reference/clearRequire.md) | [`cacheClearPackages()`](https://Require.predictiveecology.org/reference/clearRequire.md) |
| `options("Require.cachePkgDir")` | `R_USER_CACHE_DIR` env var |
| `Sys.getenv("R_REQUIRE_PKG_CACHE")` | `R_USER_CACHE_DIR` env var |
