## Release information

This is a feature update. The major change is that the package dependency
and installation engine now defaults to `pak`. The legacy non-pak code
path is retained for users who set `options(Require.usePak = FALSE)`, but
going forward `pak` is the only actively maintained installer. See
`NEWS.md` for the full list of changes.

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
