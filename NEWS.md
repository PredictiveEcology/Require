Known issues: https://github.com/PredictiveEcology/Require/issues

version 0.0.12
==============

## Dependency changes
* with the release of R 4.1, we dropped support for R 3.5. R 3.6 (`oldrel`) and newer are supported.

## New features
* `setup`: new function for creating a new project. See `readme.md`
* `setLibPath` and package caching (via `options("RPackageCache")`) now automatically create and use a subfolder of user-provided path with the R major & minor version number (as with normal R behaviour) to allow multiple R versions to coexist on the same machine.
* `setLibPaths` gains a new argument, `updateRprofile`, which allows a user's changes to `.libPaths()` to persist through an R restart. Set to `getOption("Require.updateRprofile", FALSE)`, at start

## Bug fixes
* several edge cases with complex loading of many packages
* was incorrectly (not) loading base packages, e.g., `parallel`
* small minor bugfixes
* In cases where a DESCRIPTION file had both a package with a minimum version (e.g., in Imports) and a REMOTES: for that package (without a minimum version, but with a branch, say), `Require` would use the REMOTES: entry. But since that means there is no minimum package version, and `Require` does not automatically install a package that is not violating a minimum version number, it would not install anything. Now, it harmonizes the 2 entries for a given package, and uses both the minimum version number and the git branch as the potential source to find that version number.
* allow either `master` or `main` branches to be installed from GitHub, without needing to specify (#26)

version 0.0.10
==============

## Bug fixes
* CRAN error on one flavour of Linux
* erroneous `checkPath` error creating `Specified path xxxx doesn't exist` even though it does.

version 0.0.9
==============

## New features
* `modifyList2`, a generalization of `utils::modifyList` for >2 lists. Also, can handle NULL lists.
* slight improvements in speed for some internal functions
* `detachAll` now unloads reverse depends of the depends, if they are loaded

## Bug fixes
* deals with more cases of installing arbitrary packages from a `packageVersion.txt` file
* Does not mistakenly create a new, empty directory of packages to accommodate 2 `LibPaths` from `packageVersion.txt` file, *if the second (or more) `LibPath`* is full of base packages.
* Handles better false positives (packages did not install properly when they did) and some false negatives (no error collected at end when there was an error in installing)
* better suggestion of what to do in some edge cases of failed package installs
* captures and deals with a bug in `install.packages` (`argument "av2" is missing, with no default`) on R-devel for Windows (on Sept 09, 2020). May be transient.
* Was, by default, installing from `source` on Windows. Fixed.


version 0.0.8
==============

## New features
* GitHub SHA is now stored during `pkgSnapshot`, meaning that a new system can be built with exact versions and SHAs of GitHub packages.
* For GitHub packages, now uses both DESCRIPTION and NAMESPACE files to determine dependencies. GitHub packages are generally for packages in some state of development. This may include missing declarations in DESCRIPTION. NAMESPACE is what R uses to actually determine package dependencies upon installation.
* Now keeps the binary/source package locally if `options("Require.RPackageCache" = "someLocalDir")` is set to a local folder. Currently defaults to NULL, meaning no local cache.
* `Require` and `pkgSnapshot` can now understand and work with GitHub SHAs and thus packages installed from GitHub, e.g., `Require("PredictiveEcology/Require@development")` will install the development version. When using `pkgSnapshot`, the exact SHA will be used to restore that package at the exact version with `Require(packageVersionFile = "packageVersions.txt")`.
* If a package is already loaded prior to changing running `setLibPaths`, it is possible to create a version conflict. `base::require` will error if the version in the `.libPaths()` is older than the version whose namespace is already loaded. To accommodate this, there is a check for this error, and if the newer version (that is already loaded) does not violate the `Require('package (versionSpecification)')`, then it will install the newer version. If it does violate the version specification, it will error cleanly with a message describing the possible solutions.
* Much better messaging and reporting
* New function: `detachAll` that attempts to detach and unload packages and all their dependencies, in reverse topological order.
* Speed improvements, especially with `pkgDep` and `pkgDepTopoSort`
* New function `pkgDepAlt` which is an alternative to `pkgDep`, yet easier to maintain and still experimental. It is not yet the workhorse inside `Require`, but it may become that.
* Now correctly removes spaces and tab characters within a package version description -- this was creating an error such as `Error: invalid version specification ' 	3.3-13'`

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
