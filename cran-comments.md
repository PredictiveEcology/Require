## Updated release

This is a maintenance release which fixes issues during CRAN tests. 
We have also removed the overriding of a user-selected CRAN repository.
We default to the user's choice of repo, only setting it when none is set ("", NULL, or @CRAN@) (e.g., in some tests) and it is non-interactive.
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
          
### Winbuilder
* Windows                 (win-builder), R 3.6.3
* Windows                 (win-builder), R 4.0.2
* Windows                 (win-builder), R 4.1.0 (2020-08-12 r78957)

### rhub
* Fedora Linux                      (clang, gfortran), R 4.1.0 (2020-08-12)
* Windows     (Windows Server 2008 R2 SP1, 32/64 bit), R 4.1.0 (2020-08-12 r78957)
* Ubuntu Linux                             (16.04 LTS, R-release, GCC)

## R CMD check results

There were no ERRORs nor WARNINGs.

There was one NOTE through about the Maintainer. The correct maintainer is eliot.mcintire@canada.ca, as indicated.

## Downstream dependencies

We have tested this verison of the package with several downstream dependencies, and have found no problems.
