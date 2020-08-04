## Updated release

This is a maintenance release which fixes issues during CRAN tests.
We now default to the user's choice of repo, only setting it when none is set (e.g., in some tests).
See `NEWS.md` for complete list of changes.

## Test environments

### Previous R versions
* macOS 10.13.3 High Sierra (travis-ci), R 3.6.3
* macOS 10.15.5 Catalina        (local), R 3.6.3
* Ubuntu 16.04              (travis-ci), R 3.6.3
* Windows                    (appveyor), R 3.6.3
* Windows                 (win-builder), R 3.6.3

### Current R versions
* macOS 10.13.3 High Sierra (travis-ci), R 4.0.2
* macOS 10.15.5 Catalina        (local), R 4.0.2
* Ubuntu 16.04              (travis-ci), R 4.0.2
* Ubuntu 20.04                  (local), R 4.0.2
* Windows                    (appveyor), R 4.0.2
* Windows                 (win-builder), R 4.0.2

### Development R version
* Ubuntu 16.04              (travis-ci), R 4.1.0 (2020-08-03 r78963)
* Windows                    (appveyor), R 4.1.0 (2020-08-02 r78957)
* Windows                 (win-builder), R 4.1.0 (2020-08-02 r78957)

## R CMD check results

There were no ERRORs nor WARNINGs.

There was one NOTE regarding possible spelling errors. Each of these is a false positive.

## Downstream dependencies

We have tested this verison of the package with several downstream dependencies, and have found no problems.
