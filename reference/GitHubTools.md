# Parse a github package specification

This converts a specification like
`PredictiveEcology/Require@development` into separate columns,
"Account", "Repo", "Branch", "GitSubFolder" (if there is one)

## Usage

``` r
parseGitHub(pkgDT, verbose = getOption("Require.verbose"))
```

## Arguments

- pkgDT:

  A pkgDT data.table.

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

## Value

`parseGitHub` returns a `data.table` with added columns.

## Details

`parseGitHub` turns the single character string representation into 3 or
4: `Account`, `Repo`, `Branch`, `SubFolder`.
