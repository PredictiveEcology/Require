# Compare package versions

Alternative to
[`utils::compareVersion`](https://rdrr.io/r/utils/compareVersion.html)
that is vectorized on `version`, `versionSpec` and/or `inequality`. This
will also return an NA element in the returned vector if one of the
arguments has NA for that element.

## Usage

``` r
compareVersion2(version, versionSpec, inequality)
```

## Arguments

- version:

  One or more package versions. Can be `character` or `numeric_version`.

- versionSpec:

  One or more versions to compare to. Can be `character` or
  `numeric_version`.

- inequality:

  The inequality to use, i.e., `>=`.

## Value

a logical vector of the length of the longest of the 3 arguments.

## See also

Other version specifications:
[`extractPkgName()`](https://Require.predictiveecology.org/reference/extractPkgName.md),
[`parseGitHub()`](https://Require.predictiveecology.org/reference/GitHubTools.md),
[`trimRedundancies()`](https://Require.predictiveecology.org/reference/trimRedundancies.md),
[`trimVersionNumber()`](https://Require.predictiveecology.org/reference/trimVersionNumber.md)
