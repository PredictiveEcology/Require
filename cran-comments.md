## Updated release

This release fixes an error shown on the CRAN check page.
See `NEWS.md` for complete list of changes.

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
* Windows                 (win-builder), R-devel

## R CMD check results

There were no ERRORs, or WARNINGs.  The only NOTE concerns the "unable to verify current time".

There was one NOTE through about the Maintainer. The correct maintainer is <eliot.mcintire@canada.ca>, as indicated.

## Downstream dependencies

We have tested this version of the package with several downstream dependencies. There are no downstream problems.
