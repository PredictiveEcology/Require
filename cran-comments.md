## Updated release

This update is a re-implementation of the underlying functions, with large speed and edge case improvements.

## Test environments

### GitHub Actions
- os: macOS-latest,   r: 'release'
- os: windows-latest, r: 'release'
- os: windows-latest, r: '4.2'
- os: windows-latest, r: '4.1'
- os: ubuntu-20.04,   r: 'devel', 
- os: ubuntu-20.04,   r: 'release'
- os: ubuntu-20.04,   r: 'oldrel'

### Winbuilder
* Windows                 (win-builder), R Under development (unstable) (2022-11-04)
* Windows                 (win-builder), 4.1.3 (2022-03-10)
* Windows                 (win-builder), 4.2.2 (2022-06-23)

### R-hub
* Linux (Debian, Fedora, MacOS, Windows) 

## R CMD check results

There were no ERRORs nor WARNINGs. The one NOTE is about the package maintainer, which is unchanged.

## Downstream dependencies

We checked all reverse dependencies, currently, all reverse dependencies are packages we are also maintainers. 

There were no issues.
