# Package dependencies when one or more packages removed

This is primarily for package developers. It allows the testing of what
the recursive dependencies would be if a package was removed from the
immediate dependencies.

## Usage

``` r
pkgDepIfDepRemoved(
  pkg = character(),
  depsRemoved = character(),
  verbose = getOption()
)
```

## Arguments

- pkg:

  A package name to be testing the dependencies

- depsRemoved:

  A vector of package names who are to be "removed" from the `pkg`
  immediate dependencies

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

## Value

A list with 3 named lists `Direct`, `Recursive` and `IfRemoved`.
`Direct` will show the top level direct dependencies, either `Remaining`
or `Removed`. `Recursive` will show the full recursive dependencies,
either `Remaining` or `Removed`. `IfRemoved` returns all package
dependencies that are removed for each top level dependency. If a top
level dependency is not listed in this final list, then it means that it
is also a recursive dependency elsewhere, so its removal has no effect.

## Examples

``` r
# \donttest{
if (Require:::.runLongExamples()) {
  opts <- Require:::.setupExample()

  pkgDepIfDepRemoved("reproducible", "data.table")

  Require:::.cleanup(opts)
}
# }
```
