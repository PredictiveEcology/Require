# GITHUB_PAT-aware and `main`-`master`-aware download from GitHub

Equivalent to
[`utils::download.file`](https://rdrr.io/r/utils/download.file.html),
but taking the `GITHUB_PAT` environment variable and using it to access
the Github url.

## Usage

``` r
.downloadFileMasterMainAuth(
  url,
  destfile,
  need = "HEAD",
  verbose = getOption("Require.verbose"),
  verboseLevel = 2
)
```

## Arguments

- url:

  a [`character`](https://rdrr.io/r/base/character.html) string (or
  longer vector for the `"libcurl"` method) naming the URL of a resource
  to be downloaded.

- destfile:

  a character string (or vector, see the `url` argument) with the file
  path where the downloaded file is to be saved. Tilde-expansion is
  performed.

- need:

  If specified, user can suggest which `master` or `main` or `HEAD` to
  try first. If unspecified, `HEAD` is used.

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

- verboseLevel:

  A numeric indicating what verbose threshold (level) above which this
  message will show.

## Value

This is called for its side effect, namely, the same as
[`utils::download.file`](https://rdrr.io/r/utils/download.file.html),
but using a `GITHUB_PAT`, it if is in the environment, and trying both
`master` and `main` if the actual `url` specifies either `master` or
`main` and it does not exist.
