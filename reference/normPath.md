# Normalize filepath

Checks the specified filepath for formatting consistencies:

1.  use slash instead of backslash;

2.  do tilde etc. expansion;

3.  remove trailing slash.

## Usage

``` r
normPath(path)

# S4 method for class 'character'
normPath(path)

# S4 method for class 'list'
normPath(path)

# S4 method for class 'NULL'
normPath(path)

# S4 method for class 'missing'
normPath()

# S4 method for class 'logical'
normPath(path)
```

## Arguments

- path:

  A character vector of filepaths.

## Value

Character vector of cleaned up filepaths.

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
