# A helper function to get or set CRAN repos

This will get the current option in `getOption('repos')`, and if that is
not set to a url, then it will prompt the user to select a mirror,
unless `ind` is set, in which case, it will use that mirror (in
[`chooseCRANmirror()`](https://rdrr.io/r/utils/chooseCRANmirror.html))

## Usage

``` r
getCRANrepos(repos = NULL, ind)
```

## Arguments

- repos:

  A CRAN-like repository

- ind:

  an integer of which mirror to use in
  [`chooseCRANmirror()`](https://rdrr.io/r/utils/chooseCRANmirror.html)
