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

There is one NOTE on win-builder:

    Author field differs from that derived from Authors@R

This is a known cosmetic difference in how R renders ORCID identifiers from
`Authors@R`: the auto-derived `Author:` field includes `ORCID: ` as a label
before the URL, while the `Authors@R` field shows the bare URL. No action is
needed.

## Downstream dependencies

We checked 1 reverse dependency (SpaDES.core 3.0.4).

 * We saw 0 new problems
 * We failed to check 0 packages

SpaDES.core has 1 pre-existing error (vignette requires package `NLMR` from a
non-CRAN repository) that occurs identically on both the CRAN and dev versions
of Require. It is unrelated to this package.
