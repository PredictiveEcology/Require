# A list of R packages that should likely be installed from Source, not Binary

The list of R packages that `Require` installs from source on Linux,
even if the `getOptions("repos")` is a binary repository. This list can
be updated by the user by modifying the options `Require.spatialPkgs` or
`Require.otherPkgs`. Default "force source only packages" are visible
with
[`RequireOptions()`](https://Require.predictiveecology.org/reference/RequireOptions.md).

## Usage

``` r
sourcePkgs(additional = NULL, spatialPkgs = NULL, otherPkgs = NULL)
```

## Arguments

- additional:

  Any other packages to be added to the other 2 argument vectors

- spatialPkgs:

  A character vector of package names that focus on spatial analyses.

- otherPkgs:

  A character vector of package names that often require system specific
  compilation.

## Value

A sorted concatenation of the 3 input parameters.
