# Detach and unload all packages

This uses `pkgDepTopoSort` internally so that the package dependency
tree is determined, and then packages are unloaded in the reverse order.
Some packages don't unload successfully for a variety of reasons.
Several known packages that have this problem are identified internally
and *not* unloaded. Currently, these are `glue`, `rlang`, `ps`,
`ellipsis`, and, `processx`.

## Usage

``` r
detachAll(
  pkgs,
  dontTry = NULL,
  doSort = TRUE,
  verbose = getOption("Require.verbose")
)
```

## Arguments

- pkgs:

  A character vector of packages to detach. Will be topologically sorted
  unless `doSort` is `FALSE`.

- dontTry:

  A character vector of packages to not try. This can be used by a user
  if they find a package fails in attempts to unload it, e.g., "ps"

- doSort:

  If `TRUE` (the default), then the `pkgs` will be topologically sorted.
  If `FALSE`, then it won't. Useful if the `pkgs` are already sorted.

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

## Value

A numeric named vector, with names of the packages that were attempted.
`2` means the package was successfully unloaded, `1` it was tried, but
failed, `3` it was not loaded, so was not unloaded.
