## Release information

This is a major release (2.0.0). The headline change is that the package
dependency and installation engine now defaults to `pak`. The legacy
non-pak code path is retained for users who set
`options(Require.usePak = FALSE)`, but `pak` is the only actively
maintained installer going forward -- the version bump signals the
backbone switch. See `NEWS.md` for the full list of changes.

Bundled in this release are several `install = "force"` fixes (don't
gratuitously upgrade transitive CRAN deps; correctly mark user-requested
rows for install; restore `forceInstall = TRUE`), a pre-install
integrity check that aborts when a hard dep is unresolved rather than
producing a broken install, and Windows + RStudio SSL-warning
suppression in the `whIsOfficialCRANrepo()` retry loop.

A handful of cache-management helpers were also consolidated and the
legacy names deprecated (functional shims warn for one release cycle);
see the `## deprecations` section in `NEWS.md` and the rewritten
`?cachePkgDir` topic for the migration map.

## Test environments

Require is a pure-R package (no compiled code), so the GitHub Actions +
win-builder matrix covers the OS / R-version surface; rhub flavours
that target compiled-code or numeric-precision issues (ASAN/UBSAN,
valgrind, ATLAS, noLD) were skipped as they add no value here.

### GitHub Actions
* Ubuntu 24.04, R-devel, R-release (4.5.3), R-oldrel-1 (4.4.x), R-oldrel-2 (4.3.x), R-oldrel-3 (4.2.x)
* Windows,     R-devel, R-release (4.5.3), R-oldrel-1, R-oldrel-2, R-oldrel-3
* macOS,       R-release (4.5.3)

### Local
* Ubuntu 24.04, R 4.5.3 (`devtools::check(args = "--as-cran")`, 0E/0W/0N)

### win-builder
* Windows, R-oldrelease (4.4.x)
* Windows, R-release    (4.5.3)
* Windows, R-devel

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

We checked the 1 reverse dependency (`SpaDES.core`) from CRAN, comparing
R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

SpaDES.core has 1 pre-existing vignette error (an `ii-modules.Rmd`
sentinel about an interrupted `spades()` call) that occurs identically
on both the CRAN and dev versions of Require. It is unrelated to this
package and was present before the 2.0.0 changes.
