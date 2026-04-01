## Release information

This is a minor feature release (1.1.0) with new functions, bugfixes, and housekeeping improvements.

Key changes:
* Package cache now uses per-repository subdirectories to prevent cross-repo cache contamination (#143).
* New helper `removeOldFlatCachePkgs()` to migrate users from the old flat cache layout.
* Default for `getOption("Require.usePak")` corrected from TRUE to FALSE.
* Several bugfixes (see `NEWS.md` for full list).
* All `\dontrun{}` examples converted to `\donttest{}`.

See `NEWS.md` for a full list of changes.

## Test environments

### Rhub / win-builder

Tested with `rhub::rhub_check()` and `devtools::check_win_devel()`.

### Previous R versions
* Ubuntu 24.04                 (GitHub), 4.3.3, 4.4.3
* Windows                      (GitHub), 4.3.3, 4.4.3

### Current R versions
* Ubuntu 24.04                  (local), R 4.5.2
* Ubuntu 24.04                 (GitHub), R 4.5.2
* Windows                      (GitHub), R 4.5.2
* Windows                 (win-builder), R 4.5.2

### Development R version
* Ubuntu 24.04                 (GitHub), R-devel
* Windows                      (GitHub), R-devel
* Windows                 (win-builder), R-devel

## R CMD check results

There are no errors or warnings in any of the above.

## Downstream dependencies

We checked all reverse dependencies from CRAN (1 package: SpaDES.core).

> revdepcheck::revdep_report_cran()
## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
