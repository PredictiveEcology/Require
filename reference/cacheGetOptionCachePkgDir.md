# Get the option for `Require.cachePkgDir`

First checks if an environment variable `Require.cachePkgDir` is set and
defines a path. If not set, checks whether the
`options("Require.cachePkgDir")` is set. If a character string, then it
returns that. If `TRUE`, then use
[`cachePkgDir()`](https://Require.predictiveecology.org/reference/cacheDir.md).
If `FALSE` then returns `NULL`.

## Usage

``` r
cacheGetOptionCachePkgDir()
```
