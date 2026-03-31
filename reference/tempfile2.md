# Make a temporary subfile in a temporary (sub-)directory

Make a temporary subfile in a temporary (sub-)directory

## Usage

``` r
tempfile2(
  sub = "",
  tempdir = getOption("Require.tempPath", .RequireTempPath()),
  ...
)
```

## Arguments

- sub:

  Character string, length 1. Can be a result of
  `file.path("smth", "smth2")` for nested temporary sub directories.

- tempdir:

  Optional character string where the temporary dir should be placed.
  Defaults to `.RequireTempPath()`

- ...:

  passed to `tempfile`, e.g., `fileext`

## See also

[`tempdir2()`](https://Require.predictiveecology.org/reference/tempdir2.md)
