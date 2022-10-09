## Updated release

This update deals with semi-major bugfix, thus requiring a rapid, and unprecedented, resubmission (see NEWS.md). There were minor other enhancements.

## Test environments

### GitHub Actions
- os: macOS-latest,   r: 'release'
- os: windows-latest, r: 'release'
- os: windows-latest, r: '4.2'
- os: windows-latest, r: '4.1'
- os: windows-latest, r: '4.0'
- os: ubuntu-20.04,   r: 'devel', 
- os: ubuntu-20.04,   r: 'release'
- os: ubuntu-20.04,   r: 'oldrel'
- os: ubuntu-20.04,   r: '4.0'

### Winbuilder
* Windows                 (win-builder), R Under development (unstable) (2022-09-20 r82883 ucrt)
* Windows                 (win-builder), 4.1.3 (2022-03-10)
* Windows                 (win-builder), 4.2.1 (2022-06-23 ucrt)

### R-hub
* Linux (Debian, Fedora, MacOS, Windows) 

## R CMD check results

There were no ERRORs nor WARNINGs. The one NOTE is about the package maintainer, which is unchanged.

## Downstream dependencies

We checked all reverse dependencies, currently, all reverse dependencies are packages we are also maintainers. 

We will submit new versions of reproducible & SpaDES.core soon after this package is accepted on CRAN.
