# Purge everything in the Require cache

Clears Require's own bookkeeping caches: the
[`available.packages()`](https://rdrr.io/r/utils/available.packages.html)
snapshot, the GitHub SHA database, the `DESCRIPTION` cache, and the
pkgDep memoisation database. These live under
[`cacheDir()`](https://Require.predictiveecology.org/reference/cacheDir.md)
and are distinct from the package binary tarball cache.

## Usage

``` r
cachePurge(packages = FALSE, repos = getOption("repos"))

purgeCache(packages = FALSE, repos = getOption("repos"))
```

## Arguments

- packages:

  Either a character vector of packages to install via
  `install.packages`, then load (i.e., with `library`), or, for
  convenience, a vector or list (using `c` or `list`) of unquoted
  package names to install and/or load (as in `require`, but
  vectorized). Passing vectors of names may not work in all cases, so
  user should confirm before relying on this behaviour in operational
  code. In the case of a GitHub package, it will be assumed that the
  name of the repository is the name of the package. If this is not the
  case, then pass a *named* character vector here, where the names are
  the package names that could be different than the GitHub repository
  name. As a convenience for copy-pasting long lists, any character
  element that contains newlines is split into one package per line,
  with whitespace trimmed and blank or `#`-prefixed lines dropped. For
  example,
  `Require("\n dplyr\n # ggplot2\n PredictiveEcology/LandR@development\n")`
  is equivalent to
  `Require(c("dplyr", "PredictiveEcology/LandR@development"))`.
  Alternatively, an unquoted `{...}` block is accepted, with each line
  treated as one package spec, e.g.
  `Require({ dplyr; lme4; PredictiveEcology/LandR@development })`. Note
  that R's parser strips comments inside `{...}` before this function
  runs, so to omit a package delete the line (or comment it out – both
  have the same effect). Version constraints such as `pkg (>= 1.0)` do
  not parse inside `{...}` and must use the quoted string form.

- repos:

  The remote repository (e.g., a CRAN mirror), passed to either
  `install.packages`, `install_github` or `installVersions`. **When
  `options(Require.usePak = TRUE)`:** `repos` is added to pak's
  repository list via `options(repos)`. However, pak always includes
  CRAN and Bioconductor as built-in defaults regardless of this setting
  – `repos` can only *add* sources, it cannot prevent pak from also
  searching CRAN. This differs from the default (`usePak = FALSE`)
  behaviour where `repos` strictly controls which repositories are used.
  Use [`pak::cache_clean()`](https://pak.r-lib.org/reference/cache.html)
  to clear pak's download cache if needed.

## Value

Run for its side effect, namely, all cached objects are removed.

## Details

With `packages = TRUE`, the package binary cache is also cleared by
calling
[`cacheClearPackages()`](https://Require.predictiveecology.org/reference/clearRequire.md)
– which under `usePak = TRUE` (the default) delegates to
[`pak::cache_clean()`](https://pak.r-lib.org/reference/cache.html), and
under the legacy path walks Require's bookkeeping dir directly.

`purgeCache()` is a deprecated alias.
