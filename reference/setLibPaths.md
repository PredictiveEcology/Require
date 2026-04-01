# Set `.libPaths`

This will set the [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)
by either adding a new path to it if `standAlone = FALSE`, or will
concatenate `c(libPath, tail(.libPaths(), 1))` if `standAlone = TRUE`.
Currently, the default is to make this new
[`.libPaths()`](https://rdrr.io/r/base/libPaths.html) "sticky", meaning
it becomes associated with the current directory even through a restart
of R. It does this by adding and/updating the `.Rprofile` file in the
current directory. If this current directory is a project, then the
project will have the new
[`.libPaths()`](https://rdrr.io/r/base/libPaths.html) associated with
it, even through an R restart.

## Usage

``` r
setLibPaths(
  libPaths,
  standAlone = TRUE,
  updateRprofile = getOption("Require.updateRprofile", FALSE),
  exact = FALSE,
  verbose = getOption("Require.verbose")
)
```

## Arguments

- libPaths:

  A new path to append to, or replace all existing user components of
  `.libPath()`

- standAlone:

  Logical. If `TRUE`, all packages will be installed to and loaded from
  the `libPaths` only. NOTE: If `TRUE`, THIS WILL CHANGE THE USER'S
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html), similar to
  e.g., the `checkpoint` package. If `FALSE`, then `libPath` will be
  prepended to [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)
  during the `Require` call, resulting in shared packages, i.e., it will
  include the user's default package folder(s). This can be create
  dramatically faster installs if the user has a substantial number of
  the packages already in their personal library. Default `FALSE` to
  minimize package installing.

- updateRprofile:

  Logical or Character string. If `TRUE`, then this function will put
  several lines of code in the current directory's `.Rprofile` file
  setting up the package libraries for this and future sessions. If a
  character string, then this should be the path to an `.Rprofile` file.
  To reset back to normal, run `setLibPaths()` without a `libPath`.
  Default: `getOption("Require.updateRprofile", FALSE)`, meaning
  `FALSE`, but it can be set with an option or within a single call.

- exact:

  Logical. This function will automatically append the R version number
  to the `libPaths` to maintain separate R package libraries for each R
  version on the system. There are some cases where this behaviour is
  not desirable. Set `exact` to `TRUE` to override this automatic
  appending and use the exact, unaltered `libPaths`. Default is `FALSE`

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

## Value

The main point of this function is to set
[`.libPaths()`](https://rdrr.io/r/base/libPaths.html), which will be
changed as a side effect of this function. As when setting `options`,
this will return the previous state of
[`.libPaths()`](https://rdrr.io/r/base/libPaths.html) allowing the user
to reset easily.

## Details

This details of this code were modified from
<https://github.com/milesmcbain>. A different, likely non-approved by
CRAN approach that also works is here:
<https://stackoverflow.com/a/36873741/3890027>.

## Examples

``` r
# \donttest{
if (Require:::.runLongExamples()) {
  opts <- Require:::.setupExample()
  origDir <- setwd(tempdir())
  td <- tempdir()
  setLibPaths(td) # set a new R package library locally
  setLibPaths() # reset it to original
  setwd(origDir)
  # Using standAlone = FALSE means that newly installed packages
  #   will be installed
  #   in the new package library, but loading packages can come
  #   from any of the ones listed in .libPaths()

  # will have 2 or more paths
  otherLib <- file.path(td, "newProjectLib")
  setLibPaths(otherLib, standAlone = FALSE)
  # Can restart R, and changes will stay

  # remove the custom .libPaths()
  setLibPaths() # reset to previous; remove from .Rprofile
  # because libPath arg is empty

  Require:::.cleanup(opts)
  unlink(otherLib, recursive = TRUE)
}
# }
```
