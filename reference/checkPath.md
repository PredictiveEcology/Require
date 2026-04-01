# Check directory path

Checks the specified path to a directory for formatting consistencies,
such as trailing slashes, etc.

## Usage

``` r
checkPath(path, create)

# S4 method for class 'character,logical'
checkPath(path, create)

# S4 method for class 'character,missing'
checkPath(path)

# S4 method for class 'NULL,ANY'
checkPath(path)

# S4 method for class 'missing,ANY'
checkPath()
```

## Arguments

- path:

  A character string corresponding to a directory path.

- create:

  A logical indicating whether the path should be created if it does not
  exist. Default is `FALSE`.

## Value

Character string denoting the cleaned up filepath.

## Note

This will not work for paths to files. To check for existence of files,
use [`file.exists()`](https://rdrr.io/r/base/files.html). To normalize a
path to a file, use
[`normPath()`](https://Require.predictiveecology.org/reference/normPath.md)
or [`normalizePath()`](https://rdrr.io/r/base/normalizePath.html).

## See also

[`file.exists()`](https://rdrr.io/r/base/files.html),
[`dir.create()`](https://rdrr.io/r/base/files2.html).

## Examples

``` r
## normalize file paths
paths <- list("./aaa/zzz",
              "./aaa/zzz/",
              ".//aaa//zzz",
              ".//aaa//zzz/",
              ".\\\\aaa\\\\zzz",
              ".\\\\aaa\\\\zzz\\\\",
              file.path(".", "aaa", "zzz"))

checked <- normPath(paths)
length(unique(checked)) ## 1; all of the above are equivalent
#> [1] 3

## check to see if a path exists
tmpdir <- file.path(tempdir(), "example_checkPath")

dir.exists(tmpdir) ## FALSE
#> [1] FALSE
tryCatch(checkPath(tmpdir, create = FALSE), error = function(e) FALSE) ## FALSE
#> [1] FALSE

checkPath(tmpdir, create = TRUE)
#> [1] "/tmp/Rtmpty21pO/example_checkPath"
dir.exists(tmpdir) ## TRUE
#> [1] TRUE

unlink(tmpdir, recursive = TRUE) # clean up
```
