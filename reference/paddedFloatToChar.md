# Convert numeric to character with padding

This will pad floating point numbers, right or left. For integers,
either class integer or functionally integer (e.g., 1.0), it will not
pad right of the decimal. For more specific control or to get exact
padding right and left of decimal, try the `stringi` package. It will
also not do any rounding. See examples.

## Usage

``` r
paddedFloatToChar(x, padL = ceiling(log10(x + 1)), padR = 3, pad = "0")
```

## Arguments

- x:

  numeric. Number to be converted to character with padding

- padL:

  numeric. Desired number of digits on left side of decimal. If not
  enough, `pad` will be used to pad.

- padR:

  numeric. Desired number of digits on right side of decimal. If not
  enough, `pad` will be used to pad.

- pad:

  character to use as padding (`nchar(pad) == 1` must be `TRUE`).
  Currently, can be only `"0"` or `" "` (i.e., space).

## Value

Character string representing the filename.

## Author

Eliot McIntire and Alex Chubaty

## Examples

``` r
paddedFloatToChar(1.25)
#> [1] "1.250"
paddedFloatToChar(1.25, padL = 3, padR = 5)
#> [1] "001.25000"
paddedFloatToChar(1.25, padL = 3, padR = 1) # no rounding, so keeps 2 right of decimal
#> [1] "001.25"
```
