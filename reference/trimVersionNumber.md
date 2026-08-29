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

## Value

A character vector the length of `pkgs`, each element with its version
specification (and any pak source prefix) removed.

## See also

[`extractPkgName()`](https://Require.predictiveecology.org/reference/extractPkgName.md)

Other version specifications:
[`compareVersion2()`](https://Require.predictiveecology.org/reference/compareVersion2.md),
[`extractPkgName()`](https://Require.predictiveecology.org/reference/extractPkgName.md),
[`parseGitHub()`](https://Require.predictiveecology.org/reference/GitHubTools.md),
[`trimRedundancies()`](https://Require.predictiveecology.org/reference/trimRedundancies.md)

## Examples

``` r
trimVersionNumber("PredictiveEcology/Require (<=0.0.1)")
#> [1] "PredictiveEcology/Require"
```
