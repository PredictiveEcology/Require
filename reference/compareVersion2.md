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
