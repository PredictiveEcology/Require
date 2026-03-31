# Make a temporary (sub-)directory

Create a temporary subdirectory in `.RequireTempPath()`, or a temporary
file in that temporary subdirectory.

## Usage

``` r
tempdir2(
  sub = "",
  tempdir = getOption("Require.tempPath", .RequireTempPath()),
  create = TRUE
)
```

## Arguments

- sub:

  Character string, length 1. Can be a result of
  `file.path("smth", "smth2")` for nested temporary sub directories.

- tempdir:

  Optional character string where the temporary dir should be placed.
  Defaults to `.RequireTempPath()`

- create:

  Logical. Should the directory be created. Default `TRUE`

## See also

[`tempfile2()`](https://Require.predictiveecology.org/reference/tempfile2.md)
