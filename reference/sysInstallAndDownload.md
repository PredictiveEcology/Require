# download.files or install.packages in a separate process

This uses `sys` package so that messaging can be controlled. This also
provides the option to parallelize by spawning multiple `background`
process to allow parallel e.g., downloads. Noting that if `libcurl` is
installed (and detected using `capabilities("libcurl")`), then no
explicit parallelism will be allowed, instead `method = "libcurl"` will
be passed enabling parallel downloads.

## Usage

``` r
sysInstallAndDownload(
  args,
  splitOn = "pkgs",
  doLine = "outfiles <- do.call(download.packages, args)",
  returnOutfile = FALSE,
  doLineVectorized = TRUE,
  tmpdir,
  libPaths,
  verbose
)
```

## Arguments

- args:

  A list with all arguments for a do.call to either
  `download.file, `install.packages`or a custom other function e.g.,`downloadAndBuildToLocalFile\`.

- splitOn:

  A character vector of the names in `args` to parallelize over.
  Defaults to `pkgs`. All other named elements in `args` will be assumed
  to be length 1 and used for every parallel process.

- doLine:

  A character string with the `"outfiles <- do.call(..., args)"` line.

- returnOutfile:

  A logical. If `TRUE`, then the names of the `outfiles` will be
  returned.

- doLineVectorized:

  A logical. If `TRUE`, and parallism is being used, this indicates that
  the `doLine` is a function that allows for multiple elements in
  `args[[splitOn[[1]]]`. If `FALSE`, the function will make multiple
  sequential calls within each parallel process to the `doLine` call.

- tmpdir:

  A single path where all downloads will be put

- libPaths:

  The library path (or libraries) where all packages should be
  installed, and looked for to load (i.e., call `library`). This can be
  used to create isolated, stand alone package installations, if used
  with `standAlone = TRUE`. Currently, the path supplied here will be
  prepended to [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)
  (temporarily during this call) to `Require` if `standAlone = FALSE` or
  will set (temporarily)
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) to
  `c(libPaths, tail(libPaths(), 1)` to keep base packages.

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

## Value

Mostly for side effects, namely installed packages or downloaded
packages or files. However, in the case of `returnOutfile = TRUE`, then
a list of filenames will be returned with any outputs from the `doLine`.
