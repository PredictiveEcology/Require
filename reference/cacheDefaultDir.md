# The default cache directory for Require Cache

A wrapper around `tools::R_user_dir("Require", which = "cache")` that
creates the directory, if it does not exist.

## Usage

``` r
cacheDefaultDir()
```

## Value

The default cache directory
