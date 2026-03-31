# Purge everything in the Require cache

Require uses caches for local Package saving, local caches of
`available.packages`, local caches of GitHub (e.g., `"DESCRIPTION"`)
files, and some function calls that are cached. This function clears all
of them.

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
  name.

- repos:

  The remote repository (e.g., a CRAN mirror), passed to either
  `install.packages`, `install_github` or `installVersions`.

## Value

Run for its side effect, namely, all cached objects are removed.
