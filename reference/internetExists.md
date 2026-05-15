# Internet Exists query

Simple test for internet availability.

## Usage

``` r
internetExists(verbose = getOption("Require.verbose"), force = FALSE)
```

## Arguments

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

- force:

  If `TRUE`, probe even when `options("Require.checkInternet")` is
  `FALSE`. Used at points where we're about to do real work (e.g.,
  install dispatch) and a 2-second probe is cheap relative to the cost
  of failing late. Defaults to `FALSE` to preserve the no-probe default
  behaviour for all other call sites.

## Value

Logical. `TRUE` if internet is available, `FALSE` if not.
