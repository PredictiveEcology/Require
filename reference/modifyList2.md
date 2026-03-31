# `modifyList` for multiple lists

This calls
[`utils::modifyList`](https://rdrr.io/r/utils/modifyList.html)
iteratively using [`base::Reduce`](https://rdrr.io/r/base/funprog.html),
so it can handle \>2 lists. The subsequent list elements that share a
name will override previous list elements with that same name. It also
will handle the case where any list is a `NULL`. Note: default
`keep.null = TRUE`, which is different than `modifyList`

## Usage

``` r
modifyList2(..., keep.null = FALSE)

modifyList3(..., keep.null = TRUE)
```

## Arguments

- ...:

  One or more named lists.

- keep.null:

  If `TRUE`, `NULL` elements in `val` become `NULL` elements in `x`.
  Otherwise, the corresponding element, if present, is deleted from `x`.

## Details

More or less a convenience around `Reduce(modifyList, list(...))`, with
some checks, and the addition of `keep.null = TRUE` by default.

## Note

`modifyList3` retains the original behaviour of `modifyList2` (prior to
Oct 2022); however, it cannot retain `NULL` values in lists.

## Examples

``` r
modifyList2(list(a = 1), list(a = 2, b = 2))
#> $a
#> [1] 2
#> 
#> $b
#> [1] 2
#> 
modifyList2(list(a = 1), NULL, list(a = 2, b = 2))
#> $a
#> [1] 2
#> 
#> $b
#> [1] 2
#> 
modifyList2(
  list(a = 1), list(x = NULL), list(a = 2, b = 2),
  list(a = 3, c = list(1:10))
)
#> $a
#> [1] 3
#> 
#> $b
#> [1] 2
#> 
#> $c
#> $c[[1]]
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> 
```
