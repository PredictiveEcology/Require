## Updated release

This is a minor update submission that addresses an important bug, namely when there were multiple *user-defined* libraries in the .libPaths(), these were not respected; only the first was kept with the 2nd and subsequent user libraries silently dropped. Now all are kept.

## Test environments

### GitHub Actions
- os: macOS-latest,   r: 'release'
- os: windows-latest, r: 'devel'
- os: windows-latest, r: '4.2'
- os: windows-latest, r: '4.1'
- os: ubuntu-20.04,   r: 'devel', 
- os: ubuntu-20.04,   r: 'release'
- os: ubuntu-20.04,   r: 'oldrel'

### Winbuilder
* Windows                 (win-builder), R Under development (unstable) (2022-10-11 r83083 ucrt)
* Windows                 (win-builder), 4.2.2 (2022-10-31 ucrt)
* Windows                 (win-builder), 4.1.3 (2022-03-10)


### R-hub
* Linux (Debian, Fedora, MacOS, Windows) 

## R CMD check results

There were no ERRORs nor WARNINGs. The one NOTE is about the package maintainer, which is unchanged and that this is a new submission.

## Downstream dependencies

> revdepcheck::revdep_report_cran() ## update cran-comments with this output
## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
