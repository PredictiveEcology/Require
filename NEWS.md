Known issues: https://github.com/PredictiveEcology/Require/issues

version 0.0.8
==============

## New features
* GitHub SHA is now stored during `pkgSnapshot`, meaning that a new system can be built with exact versions and SHAs of GitHub packages.
* For GitHub packages, now uses both DESCRIPTION and NAMESPACE files to determine dependencies. GitHub packages are generally for packages in some state of development. This may include missing declarations in DESCRIPTION. NAMESPACE is what R uses to actually determine package dependencies upon installation.
* Now keeps the binary/source package locally if `options("Require.RPackageCache" = "someLocalDir")` is set to a local folder. Currently defaults to NULL, meaning no local cache.
* `Require` and `pkgSnapshot` can now understand and work with GitHub SHAs and thus packages installed from GitHub, e.g., `Require("PredictiveEcology/Require@development")` will install the development version. When using `pkgSnapshot`, the exact SHA will be used to restore that package at the exact version with `Require(packageVersionFile = "packageVersions.txt")`.
* If a package is already loaded prior to changing running `setLibPaths`, it is possible to create a version conflict. `base::require` will error if the version in the `.libPaths()` is older than the version whose namespace is already loaded. To accommodate this, there is a check for this error, and if the newer version (that is already loaded) does not violate the `Require('package (versionSpecification)')`, then it will install the newer version. If it does violate the version specification, it will error cleanly with a message describing the possible solutions.
* Much better messaging and reporting

## Bug fixes
* `pkgDepTopoSort` now appears to be correct for all types of package descriptions currently allowed by `Require`, namely, packages with no version specification, packages with version specification (including older versions), and GitHub packages.
* many minor edge cases


version 0.0.7
==============

## New features
* no longer sets CRAN repository to cloud.r-project.org even if non-interactive with no CRAN repository set. Now uses `chooseCRANmirror(ind = 1)`

## Bug fixes
* fixes CRAN check issues on Fedora.

version 0.0.6
==============

## New features
* none

## Bug fixes
* fixed CRAN check issues.
* default repo now uses option `repos` instead of specifying CRAN repo.

version 0.0.5
==============

## New features
* moved several functions that have to do with package loading and installing from `reproducible` to `Require`, including `pkgDep`, `pkgDepTopoSort`.

## Bug fixes
* recursive `pkgDep` did not correctly resolve multiple instances of the same package, each with different minimum version numbering. Now it reports minimum version required for all package dependencies.
* minor changes in non-exported functions
* handling of bugs in `base::available.packages` for old Mac machines and R versions

version 0.0.4
==============

## Bug fixes
* remove `installed.packages` from test code, as per CRAN request

version 0.0.3
==============

* Change title to Title Case in DESCRIPTION

version 0.0.2
==============

* Change backticks to single quotes in DESCRIPTION

version 0.0.1
==============

## New features
* This is a rewrite of the function, `Require` (and helpers) which will be removed from package `reproducible`
* This function is intended to be a tool for package management used within a "reproducible" workflow
* It differs from all other attempts at achieving this goal by having the trait that the first and subsequent times the function `Require` is run, the result will be the same
