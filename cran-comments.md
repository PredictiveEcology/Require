## Updated release

This update deals with several enhancements (see NEWS.md), a removed package dependency (remotes), minor bugfixes, and changes to tests.

## Test environments

### GitHub Actions
- os: macOS-latest,   r: 'release'
- os: windows-latest, r: 'release'
- os: windows-latest, r: '4.2'
- os: ubuntu-20.04,   r: 'devel', 
- os: ubuntu-20.04,   r: 'release'
- os: ubuntu-20.04,   r: 'oldrel'

### Winbuilder
* Windows                 (win-builder), 4.0.5 (2021-03-31)
* Windows                 (win-builder), R 4.1.3 (2022-03-10)
* Windows                 (win-builder), 4.2.0 beta (2022-04-11 r82149 ucrt)

## R CMD check results

There were no ERRORs nor WARNINGs.

## Downstream dependencies

We checked all reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problem in a SpaDES.core example
 * We failed to check 0 packages
 
We will submit a new SpaDES.core when this package is accepted on CRAN.
