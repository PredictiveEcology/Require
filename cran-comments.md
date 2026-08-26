## Release information

Require 2.1.0.

This release adds the `Require.noRemotes` option, which resolves GitHub-style
package specifications from configured repositories rather than building them
from source -- removing the need for git authentication and a compiler
toolchain. It also exports `trimRedundancies()`, `GETWauthThenNonAuth()` and
`getGitCredsToken()`, and fixes a number of installation-correctness bugs
(notably `getCRANrepos()` no longer discarding non-CRAN repositories when
resolving the `"@CRAN@"` placeholder). See NEWS.md.

## Test environments

  ### local
  * Ubuntu 24.04 - R 4.5.3

  ### GitHub Actions
  * Ubuntu - R-devel, R 4.5 (release), R 4.4 (oldrel-1), R 4.3 (oldrel-2),
    R 4.2 (oldrel-3)
  * Windows Server - R-devel, R 4.5 (release), R 4.4 (oldrel-1),
    R 4.3 (oldrel-2)
  * macOS - R 4.5 (release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

There are no reverse dependencies on CRAN.
