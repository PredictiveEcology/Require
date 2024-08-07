# Require

version 1.0.1
=============

## CRAN requested fixes
* Require was failing on secondary check systems (Fedora and one Mac system). This update fixes those.

## enhancements
* `offlineMode`, gained improved functionality; though it is still experimental. It can be set using `options(Require.offlineMode = TRUE)`, but it will be automatically set if internet is not available, has now been widely tested. If packages are available in the local caches, and all elements of package versioning (e.g., `available.packages()` and github packages) have been previously run, then installations should occur as if the internet were available.

## Other
* package testing on Linux Fedora and one MacOS machine on CRAN extra machines were addressed.

version 1.0.0
=============

## major changes
* Installation, package downloading, and package building from source now occur in an external process using `sys` package. This allows for more control over messaging during installations, and it also allows of installation of many packages that are already loaded (with a message that the session will need restarting). This can be turned off with This is turned on with `option(Require.installPackagesSys = FALSE)`.
* All internals for `pkgDep` have been changed. The new algorithms are faster and more reliable, with far fewer lines of code.
* All testing has been converted from using `testit` to using `testthat`. This change adds many dependencies to `Suggests`, but the benefits, e.g., using `withr` to control loading and unloading of options, packages etc., outweigh the drawbacks.

## enhancements
* `packages` argument for `Require` and `Install` can now be unquoted names length == 1 or if length > 1 using `c()` or `list()`, in addition to a character string, e.g., `Install(ggplot2)`;
* Now, if a `GitHub.com` package has a field `Additional_repositories` in the DESCRIPTION file, `Require` will search there for packages that it does not find in the `repos` argument. This does not affect `CRAN` packages, as this information is not contained within the `available.packages()` data base, which is what is used to identify dependencies, rather than reading each `DESCRIPTION` file individually;
* `verbose` now propagates better through all internal functions, so e.g., `verbose = -2` will make installing very silent;
* Better automatic cleaning of Cached packages that are corrupt;
* experimental use of `pak` as the backend installer of packages instead of `install.packages`. A user can attempt to use this backend with `options(Require.usePak = TRUE)`. There are a number of cases (specifically when needing exact versions) that do not work; but for "normal" package installations it is widely tested. `pak` backend tends to be similar speed for first installations, but much slower for subsequent calls to `Install`/`Require`;
* Better recovery from installation failures e.g., if the local cached copy is corrupt, it will be automatically cleaned;
* `Require.Rmd` vignette for "Getting Started" is new;
* many speed enhancements in cases where e.g., a download is not necessary;
* when downloads from `GitHub.com` are done, `Require` now uses `gitcreds` to get `git` credentials and `httr` to download the files with the token;

## Function name changes

* all functions related to `cache` now start with `cache`, e.g., `cacheClearPackages` replaces `clearRequirePackageCache`. Previous names are kept for backwards compatibility.

## bugfixes
* If a GitHub packages was attempted to be installed, but failed because the package was already loaded in the session, `Require` would incorrectly think it had successfully installed (#87);
* Warning occurred if a package was no longer on CRAN and user had supplied multiple `repos` or `getOption('repos')`. The result was unaffected by the warning, but warning is now removed;
* allow user-specified path in `pkgSnapshot()` (#93);
* a number of new cases have been added to `tests` that previously would have hit errors;
* many other small bugs fixed;
* Some issues specific to MacOS have been fixed.
* fixes or implemented other issues #91, #96, #97, #102, #105

version 0.3.1
=============

## enhancements
* minor modifications for when internet is not available
* deal with more edge cases for package snapshots that are not internally consistent, i.e., violate package versions, or skip missing branches on GitHub, if not needed (#81).

## bugfixes
* updates to tests that have begun to fail

version 0.3.0
=============

## enhancements
* Moved from MRAN archives for binaries to <https://packagemanager.posit.co/>
* because of the move from MRAN to posit package manager, attempts are made to use archived binary packages for Linux also.
* improved messaging in several places
* improved error catching in several places
* a number of cases that were annoying for users were identified and addressed.
* `setupOff` and `setLibPaths` enhanced to be fully functioning in a wide diversity of cases.
* When setting `install = "force"` in `Require`, now only the user-specified packages are forced to be installed; the rest are installed if required, mimicking `install.packages`
* small efficiency gains in many places
* `(HEAD)` is now more robust as a way to keep a package up to date.

## advanced changes
* several functions now exported, `.downloadFileMasterMainAuth`, `messageVerbose`, `messageDF` as they were deemed useful enough for other packages.

## bugfixes
* slow assessment of package dependencies on CRAN packages because of stale `available.packagesCached()` object. Now, catches this condition and refreshes `available.packages()`
* corrected support for multiple repos that each offer the same packages. Now works like `install.packages`, i.e., first one first.
* base packages can now be installed as previous issues about installing them were dealt with.

version 0.2.6
=============

## enhancements
* attempts to deal with more cases of failed installations
* `Install` did not have an `install` argument; this has now been introduced, allowing the (most likely) use case of `Install(pkg, install = "force")`
* examples now use `Install` more often than `Require(..., require = FALSE)` for simplicity.

## improved messaging
* If non-interactive and no CRAN mirror is set, user gets more informative error.

## bugfixes
* Cases of multiple user-specified `.libPaths()` were treated incorrectly; they are now all respected. 
* when git repo was not installed because it was identical to the SHA already installed, it would not be loaded, thinking it failed to install; fixed
* can now deal with case when `repos` has multiple, non-binary CRAN-like repositories, when there is also at least one binary repository supplied e.g., the rstudio package manager, i.e., there are at least 3 repositories supplied, 1 of which is binary.
* other minor

version 0.2.5
=============

## enhancements
* several modifications to enable CRAN-policy violations all addressed, notably keeping all temporary and (package and personal) cache directories clean after examples and tests
* This is a major overhaul of the inner workings of `Require`. It now downloads and builds `Archive` and `GitHub` packages prior to installation, then installs all packages (`CRAN`, `Archive`, `GitHub`, `MRAN` on Windows) with one `install.packages` call (Linux-alikes) or up to two `install.packages` calls (binary and source), allowing efficient parallel installs. This results in very fast installs for all combinations of packages.
new `options("Require.offlineMode")` can be set to `FALSE` to stop `Require` and `pkgDep` from checking the internet. This will fail, unless the cached packages are available locally (i.e., it was run once with all packages installed previously). If they are, then they will be installed without needing the internet. This option will also be set automatically on the first attempt to get a file from the internet, which fails, triggering a test of the internet. If that fails, then the option will be set to `FALSE` until next call to `Require` or `pkgDep` when it will be reset. This is experimental still.
* many more edge cases found and dealt with
* experimental use of `(HEAD)` to keep a package "up to date" with the HEAD of a GitHub branch. The behaviour still uses version numbering, so will not update based on SHA, but if the HEAD is ahead of the locally installed package and the `(HEAD)` is specified, then it will update. Specifically, use this instead of a version number, e.g., `"PredictiveEcology/Require@development (HEAD)"`
* `modifyList2` now follows `modifyList` by adding the `keep.null` argument.
* `setdiffNamed` will compare 2 named lists or vectors and keep on those elements that are in the first list (or vector), keeping in mind the name as well as the element.
* package messaging is not sorted alphabetically during installation
* all `message` calls now `messageVerbose`, so verbosity can be fully controlled with the argument `verbose` or `options("Require.verbose")`. See `?RequireOptions`.
* tests clean up more completely after themselves
* if `options(Require.cachePkgDir = FALSE)` (or environment variable `"R_REQUIRE_PKGCACHE"`), then no cache folder will be created; previously a nearly empty folder was created by default. See `?RequireOptions`
* Remove option `Require.persistentPkgEnv` as it was deemed superfluous.
* numerous enhancements for speed
* new function `Install`, which is `Require(..., require = FALSE)`
* `(HEAD)` has now been tested for CRAN repositories and works as expected.
* Updated README to show new functionality
* will attempt to use local cached packages from `crancache` if the user sets `options(Require.useCranCache = TRUE)`. This is experimental and is still being tested.
* A new function, `clearRequirePackageCache`, for clearing the package cache.
* The cache has been developed to be able to be shared across Operating Systems, if there is a shared file system.
* GitHub packages require the SHA to be assessed; now this is Cached to disk as well as RAM, so that it persists even if there is an R restart. 
* All non-package cache files (`available.packages`, `pkgDep`, `GitHubSHA`) will be refreshed (purged) every 1 hour.
* Much improved messaging, including identifying `MRAN` package installs explicitly (instead of just "Archive")

## bugfixes
* `pkgDep` was using local `DESCRIPTION` file to establish package dependencies for a package, if it was available. When the local package is ahead of CRAN (a developer's case), then this is desirable. But, when the local installed version is behind CRAN (a common user's case), then this is not desirable. `pkgDep` now uses CRAN's version (using `available.packages`) as developers can handle this situation on their own.
* several minor
* bugfix for `defaultCacheDir`, which would default to `runneradmin` under some conditions and did not allow installing packages due to permissions.

## deprecated
* `setup` and `setupOff` are now deprecated; messaging is supplied for what to do if these were being used
* several options are deprecated


version 0.1.6
=============

## enhancements
* `pkgSnapshot` examples brought up to present usage & simplified
* `pkgSnapshot` now uses a default filename that is an option `Require.packageVersionFile`.
* `Require` can now accept `packageVersionFile = TRUE`, meaning use the package version file that is set in the `Require.packageVersionFile` option.

## bugfix
* minor bugfix only detected on submission to CRAN


version 0.1.5
=============

## enhancements
* package caching for packages that need sources installs (i.e., identified with `sourcePkgs()`, which tend to occur when R packages require idiosyncratic system dependencies) cache the binary version and reuse that on the same system with subsequent re-installs.

## bugfix
* `pkgDep` was misidentifying the correct package dependencies. This would manifest when a user had a version of package "A" installed as well as all its dependencies, e.g., "B". When the user updated "A" to a new version that required a new version of "B", it would not correctly identify the new dependency requirement, and not update "B", causing "A" update to fail. This is fixed.

version 0.1.4
=============

* Make corrections for 2 failing architectures on CRAN
* MUCH less verbose during automated testing

## enhancement
* `verbose` argument is now widespread, with -1, 0, 1, 2 all valid and correctly inherited values. See argument description in e.g., `?Require`
* improved warning handling

## bugfixes
* more edge cases found and dealt with

version 0.1.2
==============
## dependencies
* drop support for R 3.6 (R >= 4.0 are supported)

## enhancements
* The `Require` argument, `require`, can now be a character string, indicating which packages should be attached via `require`
* Now can use `GITHUB_PAT` environment variable, if set, when it accesses GitHub.com repositories (files or entire repository)
* Attempt to capture and correct cases where GitHub.com branches are incorrectly labelled `master` instead of `main` (or vice versa)
* much quieter messaging by default (can increase with verbose = 1)
* `require` argument in `Require` can now be a character vector indicating which packages should be attached, not just installed. Note: by default, all packages that are passed to `packages` are attached if `require = TRUE`

* much faster installations:

  * When source packages, they are grouped and installed together using the internal parallelism of install.packages (setting Ncpus option to 4)
  * when binary, passes vectors to install.packages so much faster.
  * all packages are installed in install-safe groups for speed

* can use pak package under the hood when options("Require.usepak" = TRUE), though there are still many cases that pak cannot deal with. Users should try and determine if this option delivers as expected. pak installs tend to be slightly faster if they work correctly.
* binary package caching is turned in by default in a user-specific standard directory, making repeat installations (on same system, or shared drive systems) much faster.
* MRAN installs for Windows are now much more robust under many conditions.
* archived packages (ie no longer on CRAN) will now be found and installed (latest available version)
* more robust dependency identification even for archived or older packages or package versions (including their dependencies)
* MRAN binaries will be used in MacOSX.
* improved installation of older packages (e.g. when dependencies are removed from CRAN, or source versions can't be easily compiled)
* several other minor improvements in package dependency resolution and installation.

## bugfixes
* fix issue with 'dot directories' in `normPath()`.
* identified possible bug with `install.packages` when `options(Ncpus = XX)` where XX is a number > 1. Some packages are skipped. `Require` now captures this and attempts to install the ones that did not get correctly installed.
* multiple fixes for certain edge cases.

version 0.1.1
==============
## enhancements
* can now use `pak` if `options("Require.usepak" = TRUE)` and there are no version specifications (i.e., if a user specifies e.g., `Require("reproducible (<= 1.2.9))`, then the non-`pak` approach will be used)

## bugfixes
* fixed an error installing certain GitHub packages

version 0.1.0
==============
## enhancements
* install CRAN packages using vectorized `install.packages` --> much faster
* now uses internal `installGithubPackage` instead of `remotes::install_github`
* this previous means that all installations use `install.packages` directly
* remove dependency on `remotes`

## bugfixes
* `Require` would silently fail to install a GitHub package if there was a warning during the installation. These warnings are now correctly captured, without stopping the installation.
* bugfix where a package being installed from GitHub directly had a `Remotes` field for a package that was in `Suggests` (in its DESCRIPTION file). It would install this `Remotes` package even though it was only in `Suggests`
* bugfix when user supplies a non-CRAN `repos` argument to `Require`. It was not correctly using. Thanks to @CeresBarros for identifying issue #30
* bugfix "All packages appear to have installed correctly" was misreporting under some cases.
* `repos` argument not correctly passed into `doInstalls` from `Require`. This meant that installs would not respect a user supplied repos, but would use the `options("repos")` instead.
* `extractPkgNames` now allows GitHub packages that have the repository omitted, i.e., they only have `@`. This is useful if there is a default expectation for a github repository
* better handling of GitHub package install issues

version 0.0.13
==============
* fix CRAN policy violation -- dealt with extraneous folder created during testing

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
* fix use of options in `setup()`

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
* Now keeps the binary/source package locally if `options("Require.cachePkgDir" = "someLocalDir")` is set to a local folder. Currently defaults to NULL, meaning no local cache.
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
