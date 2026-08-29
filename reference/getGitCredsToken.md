# Find a GitHub token, from the git credential store or the environment

[`gitcreds::gitcreds_get()`](https://gitcreds.r-lib.org/reference/gitcreds_get.html)
reads the *git credential store* only; it does not consult `GITHUB_PAT`
or `GITHUB_TOKEN`. CI runners routinely set the environment variable and
configure no credential helper, so relying on gitcreds alone left API
calls unauthenticated and subject to GitHub's 60-request/hour per-IP
limit – which surfaces as intermittent HTTP 403s spread across whichever
job happens to exhaust the quota.

## Usage

``` r
getGitCredsToken()
```

## Value

The `"token <pat>"` string
[`GETWauthThenNonAuth()`](https://Require.predictiveecology.org/reference/GETWauthThenNonAuth.md)
expects, or `NULL`.

## Details

The credential store still wins when it has something, so behaviour on a
developer machine is unchanged; the environment is only a fallback.

Exported for the SpaDES packages, which authenticate GitHub API calls
the same way. Not part of the advertised API.
