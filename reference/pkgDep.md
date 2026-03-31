# Reverse package depends

This is a wrapper around
[`tools::dependsOnPkgs`](https://rdrr.io/r/tools/dependsOnPkgs.html),
but with the added option of `topoSort`, which will sort them such that
the packages at the top will have the least number of dependencies that
are in `pkgs`. This is essentially a topological sort, but it is done
heuristically. This can be used to e.g., `detach` or `unloadNamespace`
packages in order so that they each of their dependencies are detached
or unloaded first.

`pkgDep2` is a convenience wrapper of `pkgDep` that "goes one level in",
i.e., the first order dependencies, and runs the `pkgDep` on those.

This will first look in local filesystem (in
[`.libPaths()`](https://rdrr.io/r/base/libPaths.html)) and will use a
local package to find its dependencies. If the package does not exist
locally, including whether it is the correct version, then it will look
in (currently) `CRAN` and its archives (if the current `CRAN` version is
not the desired version to check). It will also look on `GitHub` if the
package description is of the form of a GitHub package with format
`account/repo@branch` or `account/repo@commit`. For this, it will
attempt to get package dependencies from the GitHub `DESCRIPTION` file.
This is intended to replace
[`tools::package_dependencies`](https://rdrr.io/r/tools/package_dependencies.html)
or `pkgDep` in the miniCRAN package, but with modifications to allow
multiple sources to be searched in the same function call.

## Usage

``` r
pkgDepTopoSort(
  pkgs,
  deps,
  reverse = FALSE,
  topoSort = TRUE,
  libPaths,
  useAllInSearch = FALSE,
  returnFull = TRUE,
  recursive = TRUE,
  purge = getOption("Require.purge", FALSE),
  which = c("Depends", "Imports", "LinkingTo"),
  type = getOption("pkgType"),
  verbose = getOption("Require.verbose"),
  ...
)

pkgDep2(...)

pkgDep(
  packages,
  libPaths,
  which = c("Depends", "Imports", "LinkingTo"),
  recursive = TRUE,
  depends,
  imports,
  suggests,
  linkingTo,
  repos = getOption("repos"),
  keepVersionNumber = TRUE,
  includeBase = FALSE,
  includeSelf = TRUE,
  sort = TRUE,
  simplify = TRUE,
  purge = getOption("Require.purge", FALSE),
  verbose = getOption("Require.verbose"),
  type = getOption("pkgType"),
  Additional_repositories = FALSE,
  ...
)
```

## Arguments

- pkgs:

  A vector of package names to evaluate their reverse depends (i.e., the
  packages that *use* each of these packages)

- deps:

  An optional named list of (reverse) dependencies. If not supplied,
  then `tools::dependsOnPkgs(..., recursive = TRUE)` will be used

- reverse:

  Logical. If `TRUE`, then this will use `tools::pkgDependsOn` to
  determine which packages depend on the `pkgs`

- topoSort:

  Logical. If `TRUE`, the default, then the returned list of packages
  will be in order with the least number of dependencies listed in
  `pkgs` at the top of the list.

- libPaths:

  A path to search for installed packages. Defaults to
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)

- useAllInSearch:

  Logical. If `TRUE`, then all non-core R packages in
  [`search()`](https://rdrr.io/r/base/search.html) will be appended to
  `pkgs` to allow those to also be identified

- returnFull:

  Logical. Primarily useful when `reverse = TRUE`. If `TRUE`, then then
  all installed packages will be searched. If `FALSE`, the default, only
  packages that are currently in the
  [`search()`](https://rdrr.io/r/base/search.html) path and passed in
  `pkgs` will be included in the possible reverse dependencies.

- recursive:

  Logical. Should dependencies of dependencies be searched, recursively.
  NOTE: Dependencies of suggests will not be recursive. Default `TRUE`.

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

- which:

  a character vector listing the types of dependencies, a subset of
  `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.
  Character string `"all"` is shorthand for that vector, character
  string `"most"` for the same vector without `"Enhances"`.

- type:

  See
  [`utils::install.packages`](https://rdrr.io/r/utils/install.packages.html)

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

- ...:

  Currently only `dependencies` as an alternative to `which`. If
  specified, then `which` will be ignored.

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

- depends:

  Logical. Include packages listed in "Depends". Default `TRUE`.

- imports:

  Logical. Include packages listed in "Imports". Default `TRUE`.

- suggests:

  Logical. Include packages listed in "Suggests". Default `FALSE`.

- linkingTo:

  Logical. Include packages listed in "LinkingTo". Default `TRUE`.

- repos:

  The remote repository (e.g., a CRAN mirror), passed to either
  `install.packages`, `install_github` or `installVersions`.

- keepVersionNumber:

  Logical. If `TRUE`, then the package dependencies returned will
  include version number. Default is `FALSE`

- includeBase:

  Logical. Should R base packages be included, specifically, those in
  `tail(.libPaths(), 1)`

- includeSelf:

  Logical. If `TRUE`, the default, then the dependencies will include
  the package itself in the returned list elements, otherwise, only the
  "dependencies"

- sort:

  Logical. If `TRUE`, the default, then the packages will be sorted
  alphabetically. If `FALSE`, the packages will not have a discernible
  order as they will be a concatenation of the possibly recursive
  package dependencies.

- simplify:

  Logical or numeric. If `TRUE` (or \> 0), the default, the return
  object is "just" a character vector of package names (with version
  requirements). If `FALSE` (or `0`), then a `data.table` will be
  returned with 4 columns, `Package`, `packageFullName`, `parentPackage`
  (the package name for which the given line entry is a dependency; will
  be "user" if it was user supplied) and `deps`, which is a list of
  `data.table`s of all dependencies. If a negative number, then it will
  return a similar `data.table` as with `FALSE`, however, duplications
  in the recursive package dependencies are left intact.

- Additional_repositories:

  Logical. If `TRUE`, then `pkgDep` will return a list of `data.table`
  objects (instead of character vectors) with a column `packageFullName`
  and possibly a second column `Additional_repositories`, which may have
  been specified in a `DESCRIPTION` file. NOTE: THIS ALTERS THE OUTPUT
  CLASS

## Value

A possibly ordered, named (with packages as names) list where list
elements are either full reverse depends.

## Note

[`tools::package_dependencies`](https://rdrr.io/r/tools/package_dependencies.html)
and `pkgDep` will differ under the following circumstances:

1.  GitHub packages are not detected using
    [`tools::package_dependencies`](https://rdrr.io/r/tools/package_dependencies.html);

2.  [`tools::package_dependencies`](https://rdrr.io/r/tools/package_dependencies.html)
    does not detect the dependencies of base packages among themselves,
    *e.g.*, `methods` depends on `stats` and `graphics`.

## Examples

``` r
if (FALSE) { # \dontrun{
if (Require:::.runLongExamples()) {
  opts <- Require:::.setupExample()

  pkgDepTopoSort(c("Require", "data.table"), reverse = TRUE)

  Require:::.cleanup(opts)
}
} # }

if (FALSE) { # \dontrun{
if (Require:::.runLongExamples()) {
  opts <- Require:::.setupExample()

  pkgDep2("reproducible")
  # much bigger one
  pkgDep2("tidyverse")

  Require:::.cleanup(opts)
}
} # }
if (FALSE) { # \dontrun{
if (Require:::.runLongExamples()) {
  opts <- Require:::.setupExample()

  pkgDep("tidyverse", recursive = TRUE)

  # GitHub, local, and CRAN packages
  pkgDep(c("PredictiveEcology/reproducible", "Require", "plyr"))

  Require:::.cleanup(opts)
}
} # }
```
