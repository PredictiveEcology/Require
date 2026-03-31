# Invert a 2-level list

This is a simple version of
[`purrr::transpose`](https://purrr.tidyverse.org/reference/transpose.html),
only for lists with 2 levels.

## Usage

``` r
invertList(l)
```

## Arguments

- l:

  A list with 2 levels. If some levels are absent, they will be `NULL`

## Value

A list with 2 levels deep, inverted from `l`

## Examples

``` r
# create a 2-deep, 2 levels in first, 3 levels in second
a <- list(a = list(d = 1, e = 2:3, f = 4:6), b = list(d = 5, e = 55))
invertList(a) # creates 2-deep, now 3 levels outer --> 2 levels inner
#> $d
#> $d$a
#> [1] 1
#> 
#> $d$b
#> [1] 5
#> 
#> 
#> $e
#> $e$a
#> [1] 2 3
#> 
#> $e$b
#> [1] 55
#> 
#> 
#> $f
#> $f$a
#> [1] 4 5 6
#> 
#> $f$b
#> NULL
#> 
#> 
```
