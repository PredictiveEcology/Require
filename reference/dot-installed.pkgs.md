# Partial alternative (faster) to `installed.packages`

This reads the DESCRIPTION files only, so can only access fields that
are available in the DESCRIPTION file. This is different than
`installed.packages` which has many other fields, like "Built",
"NeedsCompilation" etc. If those fields are needed, then this function
will return an empty column in the returned character matrix.

## Usage

``` r
.installed.pkgs(
  lib.loc = .libPaths(),
  which = c("Depends", "Imports", "LinkingTo"),
  other = NULL,
  purge = getOption("Require.purge", FALSE),
  packages = NULL,
  collapse = FALSE
)
```

## Arguments

- lib.loc:

  character vector describing the location of R library trees to search
  through, or `NULL` for all known trees (see
  [`.libPaths`](https://rdrr.io/r/base/libPaths.html)).

- which:

  a character vector listing the types of dependencies, a subset of
  `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.
  Character string `"all"` is shorthand for that vector, character
  string `"most"` for the same vector without `"Enhances"`.

- other:

  Can supply other fields; the only benefit here is that a user can
  specify `"github"` (lower case) and it will automatically add
  c("GithubRepo", "GithubUsername", "GithubRef", "GithubSHA1",
  "GithubSubFolder") fields

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

- packages:

  Character vector. If `NULL` (default), then all installed packages are
  searched for. If a character vector is supplied, then it will only
  return information about those packages (and is thus faster to
  execute).

- collapse:

  Logical. If `TRUE` then the dependency fields will be collapsed; if
  `FALSE` (default) then the `which` fields will be kept separate.
