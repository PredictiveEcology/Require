## Release information

Require 2.1.0.

This release adds the `Require.noRemotes` option, which resolves GitHub-style
package specifications from configured repositories rather than building them
from source -- removing the need for git authentication and a compiler
toolchain. It also exports `trimRedundancies()`, `GETWauthThenNonAuth()` and
`getGitCredsToken()`, and fixes a number of installation-correctness bugs: exact version pins now
reach the dependency solver, installs proceed one dependency level at a time,
and `getCRANrepos()` no longer discards non-CRAN repositories when resolving
the `"@CRAN@"` placeholder. See NEWS.md.

## Test environments

  ### local
  * Ubuntu 24.04 - R 4.6.1, R 4.5.3, R 4.4.3 (the full test suite, including
    the tests gated behind `NOT_CRAN`, passes on R 4.6.1: 822 tests)

  ### win-builder
  * Windows - R-devel
  * Windows - R release
  * Windows - R oldrelease

  ### GitHub Actions
  * Ubuntu - R-devel, R 4.6 (release), R 4.5 (oldrel-1), R 4.4 (oldrel-2),
    R 4.3 (oldrel-3)
  * Windows Server - R-devel, R 4.6 (release), R 4.5 (oldrel-1),
    R 4.4 (oldrel-2)
  * macOS - R 4.6 (release)

  ### R-hub
  * macOS - R release (x86_64 and arm64)

## R CMD check results

0 errors | 0 warnings | 0 notes

One win-builder run (R-oldrelease) reported a NOTE on a URL in
`man/setLibPaths.Rd`:

    URL: https://stackoverflow.com/a/36873741/3890027
      Status: 403 Forbidden

The link is correct. Stack Overflow returns 403 to automated requests, so the
URL checker cannot fetch it; the same link is fine in a browser. The other two
win-builder runs did not flag it.

## Downstream dependencies

There are no reverse dependencies on CRAN.
