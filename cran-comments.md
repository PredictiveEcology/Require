## Release information

Require 2.1.1.

This is a targeted fix release for the check failures CRAN reported against
2.1.0. **It follows 2.1.0 by only a few days, and that is deliberate**: the
2.1.0 checks are currently failing on four macOS flavours and on the BLIS
additional-issues machine, and this release exists to clear them. There is no
new functionality.

The failures were entirely in the test suite, not in the package's behaviour:

* The suite assumed the packages it needs are reachable from the first and last
  entries of `.libPaths()`. Under `R CMD check` the first entry is the `.Rcheck`
  library, which holds only the package being checked, so every contributed
  package -- including Require's own `data.table` import -- sits in a *middle*
  entry. That is the layout on CRAN's macOS builders and on the BLIS machine,
  and it produced the 11 test failures.

* Tests that ran `Require()` or `Install()` under `R CMD check` left a `pak`
  symlink in the `.Rcheck` directory, which produced the "non-standard things in
  the check directory" NOTE.

The one user-visible change is a bug fix to `splitGitRepo()`, which took the
branch from a fixed position rather than from whatever follows `@`, so
`account/repo/subFolder@branch` reported the subfolder as the branch and
discarded the real one. See NEWS.md.

## Test environments

  ### local
  * Ubuntu 24.04 - R 4.6.1 (`R CMD check --as-cran` on the built tarball, with
    `NOT_CRAN` unset and `_R_CHECK_CRAN_INCOMING_=true`)

  ### win-builder
  * Windows - R-devel (2026-08-31 r90457 ucrt)
  * Windows - R 4.6.1 (release)
  * Windows - R 4.5.3 (oldrelease)

  ### macOS builder (mac.r-project.org)
  * macOS 26.6, Apple M1 - R 4.6.1 (release, arm64)

  ### R-hub
  * Windows - R-devel, R-release, R-oldrel (x86_64)
  * macOS - R-release (arm64 and x86_64)
  * Fedora - R-devel with ATLAS

  ### GitHub Actions
  * Ubuntu - R-devel, R 4.6 (release), R 4.5 (oldrel-1), R 4.4 (oldrel-2),
    R 4.3 (oldrel-3), plus an `_R_CHECK_DEPENDS_ONLY_` run on release
  * Ubuntu - R 4.6 with the dependencies installed into a library of their own,
    reproducing the split `.libPaths()` layout described above
  * Windows Server - R-devel, R 4.6 (release), R 4.5 (oldrel-1), R 4.4 (oldrel-2)
  * macOS - R 4.6 (release)

## R CMD check results

0 errors | 0 warnings | 1 note

The note is "checking CRAN incoming feasibility", on all three win-builder runs:

    Maintainer: 'Eliot J B McIntire <eliot.mcintire@canada.ca>'

    Days since last update: 5

That is the short interval since 2.1.0, explained above: this release exists to
fix the checks CRAN is currently reporting for 2.1.0.

The win-builder R-oldrelease (R 4.5.3) run additionally reported:

    Found the following (possibly) invalid URLs:
      URL: https://stackoverflow.com/a/36873741/3890027
        From: man/setLibPaths.Rd
        Status: 403
        Message: Forbidden

The URL is correct and opens normally in a browser. Stack Overflow returns 403
Forbidden to non-browser clients, so the URL checker cannot fetch it -- the
well-known false positive for stackoverflow.com links. The win-builder R-devel
and R-release runs did not report it, and neither did any other platform above.

## Downstream dependencies

Require has one reverse dependency on CRAN: SpaDES.core.

This changed since the 2.1.0 submission, which reported none. That was correct
at the time only because SpaDES.core had been archived on 2026-07-13; it is back
on CRAN at 3.2.1, so the dependency exists again and has been checked.

revdepcheck results:

    We checked 1 reverse dependencies, comparing R CMD check results across
    CRAN and dev versions of this package.

     * We saw 0 new problems
     * We failed to check 0 packages

SpaDES.core 3.2.1 checked clean against this version of Require: 0 errors,
0 warnings, 0 notes.
