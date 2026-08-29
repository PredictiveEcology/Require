# Strip GitHub locators so packages resolve from `repos`

Rewrites GitHub-style specs (`account/repo@branch`) to their bare
package name, preserving any version constraint. This lets
`Require`/`Install` satisfy them from `repos` (e.g., r-universe
binaries) instead of cloning and building from GitHub source – avoiding
the need for git authentication and Rtools. Plain (non-GitHub) specs are
returned unchanged. Used internally when
`getOption("Require.noRemotes")` is `TRUE`.

## Usage

``` r
stripGitHubToRepos(pkgs, verbose = getOption("Require.verbose", 1))
```

## Arguments

- pkgs:

  A character string vector of packages with or without GitHub path or
  versions

- verbose:

  Numeric. Controls reporting of which specs were rewritten.

## Value

A character vector the same length as `pkgs`, with GitHub locators
removed.

## Examples

``` r
Require:::stripGitHubToRepos("PredictiveEcology/SpaDES.core@development (>= 3.0.3.9003)")
#> Require.noRemotes: resolving from repos instead of GitHub:
#>   PredictiveEcology/SpaDES.core@development (>= 3.0.3.9003) -> SpaDES.core (>= 3.0.3.9003)
#> [1] "SpaDES.core (>= 3.0.3.9003)"
# -> "SpaDES.core (>= 3.0.3.9003)"
```
