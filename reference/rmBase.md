# Recursive function to remove `.basePkgs`

Recursive function to remove `.basePkgs`

## Usage

``` r
rmBase(includeBase = formals(pkgDep)[["includeBase"]], deps)
```

## Arguments

- includeBase:

  Logical. If `FALSE`, the default, then base packages will be removed.

- deps:

  Either a list of dependencies, a data.table of dependencies with a
  column `Package` or a vector of dependencies.
