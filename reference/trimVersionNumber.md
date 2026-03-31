# Trim version number off a compound package name

The resulting string(s) will have only name (including github.com
repository if it exists).

## Usage

``` r
trimVersionNumber(pkgs)
```

## Arguments

- pkgs:

  A character string vector of packages with or without GitHub path or
  versions

## See also

[`extractPkgName()`](https://Require.predictiveecology.org/reference/extractPkgName.md)

## Examples

``` r
trimVersionNumber("PredictiveEcology/Require (<=0.0.1)")
#> [1] "PredictiveEcology/Require"
```
