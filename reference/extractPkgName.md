# Extract info from package character strings

Cleans a character vector of non-package name related information (e.g.,
version)

## Usage

``` r
extractPkgName(pkgs, filenames)

extractVersionNumber(pkgs, filenames)

extractInequality(pkgs)

extractPkgGitHub(pkgs)
```

## Arguments

- pkgs:

  A character string vector of packages with or without GitHub path or
  versions

- filenames:

  Can be supplied instead of `pkgs` if it is a filename e.g., a .tar.gz
  or .zip that was downloaded from CRAN.

## Value

Just the package names without extraneous info.

## See also

[`trimVersionNumber()`](https://Require.predictiveecology.org/reference/trimVersionNumber.md)

## Examples

``` r
extractPkgName("Require (>=0.0.1)")
#> [1] "Require"
extractVersionNumber(c(
  "Require (<=0.0.1)",
  "PredictiveEcology/Require@development (<=0.0.4)"
))
#> [1] "0.0.1" "0.0.4"
extractInequality("Require (<=0.0.1)")
#> [1] "<="
extractPkgGitHub("PredictiveEcology/Require")
#> [1] "Require"
```
