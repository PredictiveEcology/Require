## Updated release

This is an update to deal with CRAN policy violation (directory left over after testing).

## Test environments

### GitHub Actions
- os: macOS-latest,   r: 'release'
- os: windows-latest, r: 'release'
- os: windows-latest, r: '4.0'
- os: windows-latest, r: '3.6'
- os: ubuntu-20.04,   r: 'devel', 
- os: ubuntu-20.04,   r: 'release'
- os: ubuntu-20.04,   r: 'oldrel'
- os: ubuntu-20.04,   r: '3.6'

### Winbuilder
* Windows                 (win-builder), R 4.0.5
* Windows                 (win-builder), R 4.1.0
* Windows                 (win-builder), R-devel (2021-05-25 r80389)

## R CMD check results

There were no ERRORs nor WARNINGs.

## Downstream dependencies

We checked all reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
