## Updated release

This is a minor bugfix update. 
See `NEWS.md` for complete list of changes.

## Test environments

### GitHub Actions
- os: macOS-latest,   r: 'release'
- os: windows-latest, r: 'release'
- os: windows-latest, r: '3.6'
- os: ubuntu-18.04,   r: 'devel', 
- os: ubuntu-18.04,   r: 'release'
- os: ubuntu-18.04,   r: 'oldrel'
- os: ubuntu-18.04,   r: '3.5'
          
### Winbuilder -- all passed Sept 8, 2020
* Windows                 (win-builder), R 3.6.3
* Windows                 (win-builder), R 4.0.2
* Windows                 (win-builder), R 4.1.0 (2020-11-30)

## R CMD check results

There were no ERRORs, or WARNINGs.  The only NOTE concerns the "unable to verify current time".

There was one NOTE through about the Maintainer. The correct maintainer is <eliot.mcintire@canada.ca>, as indicated.

## Downstream dependencies

We have tested this version of the package with several downstream dependencies. There are no downstream problems.
