## Release information

This is minor update to address CRAN failures on Fedora and Mac, as requested by Prof. Brian Ripley. 

See `NEWS.md` for a full list of changes.

## Test environments

### Rhub

Tested with `rhub::rhub_check(branch = "master", platforms = rhub::rhub_platforms()$name[c(1:18, 20)])`,
which includes the same systems as were failing on CRAN machines.
  
### Previous R versions
* Ubuntu 20.04                 (GitHub), 4.2.3, 4.3.3
* Windows                      (GitHub), 4.2.3, 4.3.3
* Windows                 (win-builder), R 4.3.3

### Current R versions
* macOS 14.5                    (local), R 4.4.1
* Ubuntu 20.04                 (GitHub), R 4.4.1
* Ubuntu 20.04                  (local), R 4.4.1
* Windows                      (GitHub), R 4.4.1
* Windows                       (local), R 4.4.1
* Windows                 (win-builder), R 4.4.1

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel  (2024-08-05 r86925)
* Ubuntu 20.04                  (local), R-devel  (2024-08-05 r86925)
* Windows                      (GitHub), R-devel  (2024-08-05 r86925 ucrt)
* Windows                 (win-builder), R-devel  (2024-08-05 r86925 ucrt)

## R CMD check results

There are no errors, or warnings in any of the above.

## Downstream dependencies

We checked all reverse dependency from CRAN and found none.

> revdepcheck::revdep_report_cran() ## update cran-comments with this output
## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
