# GitHub package tools

A series of helpers to access and deal with GitHub packages

## Usage

``` r
DESCRIPTIONFileVersionV(file, purge = getOption("Require.purge", FALSE))

DESCRIPTIONFileOtherV(file, other = "RemoteSha")

dlGitHubDESCRIPTION(
  pkg,
  purge = getOption("Require.purge", FALSE),
  verbose = getOption("Require.verbose")
)
```

## Arguments

- file:

  A file path to a `DESCRIPTION` file

- purge:

  Logical. Should all caches be purged? Default is
  `getOption("Require.purge", FALSE)`. There is a lot of internal
  caching of results throughout the `Require` package. These help with
  speed and reduce calls to internet sources. However, sometimes these
  caches must be purged. The cached values are renewed when found to be
  too old, with the age limit. This maximum age can be set in seconds
  with the environment variable
  `R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE`, or if unset, defaults to
  3600 (one hour – see
  [`utils::available.packages`](https://rdrr.io/r/utils/available.packages.html)).

  Internally, there are calls to `available.packages`.

- other:

  Any other keyword in a `DESCRIPTION` file that precedes a ":". The
  rest of the line will be retrieved.

- pkg:

  A character string with a GitHub package specification (c.f. remotes)

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

## Details

`dlGitHubDESCRIPTION` retrieves the DESCRIPTION file from GitHub.com
