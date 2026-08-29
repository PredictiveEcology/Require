# GET a URL with a GitHub token, falling back to an unauthenticated request

Signs the request with `httr::add_headers(Authorization = token)` when a
token is supplied, and retries without credentials if GitHub rejects
them or returns a 404. `token` must carry the `"token "` prefix that
[`getGitCredsToken()`](https://Require.predictiveecology.org/reference/getGitCredsToken.md)
produces – a bare PAT authenticates nothing.

## Usage

``` r
GETWauthThenNonAuth(url, token, verbose = getOption("Require.verbose"))
```

## Arguments

- url:

  The URL to GET.

- token:

  A GitHub token of the form `"token <pat>"`, or `NULL` for an
  unauthenticated request.

- verbose:

  Numeric or logical, controlling messaging verbosity.

## Value

The `httr` response object.

## Details

Exported for the SpaDES packages, which query the GitHub API for module
repositories and need the same authentication behaviour as `Require`.
Not part of the advertised API.
