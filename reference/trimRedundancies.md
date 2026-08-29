# Collapse redundant package specifications

Given package specifications that may name the same package more than
once – e.g. `"pkg"`, `"pkg (>= 1.0)"` and `"user/pkg@branch"` – reduce
them to one entry per package, keeping the most specific requirement.
This is the reduction
[`Require()`](https://Require.predictiveecology.org/reference/Require.md)
applies to a dependency set before installing, exposed because packages
that assemble their own `reqdPkgs`-style lists need the same rule to
agree with what
[`Require()`](https://Require.predictiveecology.org/reference/Require.md)
will actually do.

## Usage

``` r
trimRedundancies(
  pkgInstall,
  repos = NULL,
  purge = NULL,
  libPaths = NULL,
  verbose = getOption("Require.verbose"),
  type = getOption("pkgType")
)
```

## Arguments

- pkgInstall:

  Package specifications: a character vector, or a `data.table` as
  produced internally by `Require`.

- repos, purge, libPaths:

  Unused; retained so existing positional calls keep working.

- verbose:

  Numeric or logical, controlling messaging verbosity.

- type:

  Package type, as in
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html).

## Value

A `data.table` with one row per package, with redundant entries removed.
Callers wanting the specifications back as text take the
`packageFullName` column.

## See also

Other version specifications:
[`compareVersion2()`](https://Require.predictiveecology.org/reference/compareVersion2.md),
[`extractPkgName()`](https://Require.predictiveecology.org/reference/extractPkgName.md),
[`parseGitHub()`](https://Require.predictiveecology.org/reference/GitHubTools.md),
[`trimVersionNumber()`](https://Require.predictiveecology.org/reference/trimVersionNumber.md)

## Examples

``` r
trimRedundancies(c("data.table", "data.table (>= 1.14.0)"))
#>       Package        packageFullName versionSpec inequality hasInequality
#>        <char>                 <char>      <char>     <char>        <lgcl>
#> 1: data.table data.table (>= 1.14.0)      1.14.0         >=          TRUE
#>    keep44 isEquals oppositeInequals hasEqualsAndInequals hasVers
#>     <int>   <lgcl>           <lgcl>               <lgcl>  <lgcl>
#> 1:      1    FALSE            FALSE                FALSE    TRUE
#>    atLeastOneWithVersionSpec
#>                       <lgcl>
#> 1:                      TRUE
```
