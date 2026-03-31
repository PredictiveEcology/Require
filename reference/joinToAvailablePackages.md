# Join a data.table with a `Package` column to `available.packages`

Will join
[`available.packages()`](https://rdrr.io/r/utils/available.packages.html)
with `pkgDT`, if `pkgDT` does not already have a column named `Depends`,
which would be an indicator that this had already happened.

## Usage

``` r
joinToAvailablePackages(pkgDT, repos, type, which, verbose)
```

## Arguments

- pkgDT:

  A `pkgDT` object e.g., from `toPkgDT`

- repos:

  is used for `ap`.

- type:

  See
  [`utils::install.packages`](https://rdrr.io/r/utils/install.packages.html)

- which:

  a character vector listing the types of dependencies, a subset of
  `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.
  Character string `"all"` is shorthand for that vector, character
  string `"most"` for the same vector without `"Enhances"`.

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

## Value

The returned `data.table` will have most of the columns from
`available.packages` appended to the `pkgDT`, including `Depends`,
`Imports`, `Suggests`. It will change the column name that is normally
returned from `available.packages` as `Version` to `VersionOnRepos`.
