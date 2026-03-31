# Like `setdiff`, but takes into account names

This will identify the elements in `l1` that are not in `l2`. If
`missingFill` is provided, then elements that are in `l2`, but not in
`l1` will be returned, assigning `missingFill` to their values. This
might be `NULL` or `""`, i.e., some sort of empty value. This function
will work on named lists, named vectors and likely on other named
classes.

## Usage

``` r
setdiffNamed(l1, l2, missingFill)
```

## Arguments

- l1:

  A named list or named vector

- l2:

  A named list or named vector (must be same class as `l1`)

- missingFill:

  A value, such as `NULL` or `""` or `"missing"` that will be given to
  the elements returned, that are in `l2`, but not in `l1`

## Value

A vector or list of the elements in `l1` that are not in `l2`, and
optionally the elements of `l2` that are not in `l1`, with values set to
`missingFill`

## Details

There are 3 types of differences that might occur with named
elements: 1. a new named element, 2. an removed named element, and 3. a
modified named element. This function captures all of these. In the case
of unnamed elements, e.g., `setdiff`, the first two are not seen as
differences, if the values are not different.
