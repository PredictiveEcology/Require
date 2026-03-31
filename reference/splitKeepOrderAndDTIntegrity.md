# `split` for a data.table that keeps integrity of a column of lists of data.table objects

[`data.table::split`](https://rdrr.io/pkg/data.table/man/split.html)
does 2 bad things:

1.  reorders if using `f`

2.  destroys the integrity of a column that is a list of data.tables,
    when using `by` So, to keep order, need `by`, but to keep integrity,
    need `f`. This function

## Usage

``` r
splitKeepOrderAndDTIntegrity(pkgDT, splitOn)
```

## Arguments

- pkgDT:

  A `pkgDT` object e.g., from `toPkgDT`

- splitOn:

  Character vector passed to `data.table::split(..., f = splitOn)`

## Value

A list of `data.table` objects of `length(unique(splitOn))`.
