# Require

<!-- badges start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/Require)](https://cran.r-project.org/package=Require)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/Require)](https://cran.r-project.org/package=Require)
[![R build status](https://github.com/PredictiveEcology/Require/workflows/R-CMD-check/badge.svg)](https://github.com/PredictiveEcology/Require/actions)
[![Join the chat at https://gitter.im/PredictiveEcology/Require](https://badges.gitter.im/PredictiveEcology/Require.svg)](https://gitter.im/PredictiveEcology/Require?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
<!-- badges: end -->

A simple package for reproducible package management in R.
This is different than other approaches to package management such as `pak`, `packrat`, `checkpoint`, and `renv`, by including all-in-one management for packages in R focused around a single function, `Require`. We outline differences with these packages below.

# Objectives

Some packages, including those in our PredictiveEcology repository, have _many_ package dependencies. 
Some of them are on CRAN, but some are still in development and so are hosted elsewhere. 
Mixing many package dependencies that are constantly evolving creates challenges with standard R package management.
For example, what is the best way to move analyses from one machine to another, or set up a series of High Performance Compute nodes? 
How should we use functions like `install.packages` in a reproducible workflow that are clearly intended to be used once or very few times?
How do we deal with many packages on GitHub that have many common dependencies?
How do we deal with packages that have dependencies that are no longer on CRAN ("they have been archived")?
How do we replicate an analysis 6 months from now when some packages have changed, and their dependencies have changed?
Finally, how do we do all this for many concurrent projects without installing hundreds of packages in a new directory for every project?

# `Require` & `Install`

The `Require` package provides two "rerun-tolerant" functions, `Require` and `Install` (a recent addition). "rerun-tolerant" means that the results from running this function (the output) will be identical each time, even when the conditions when run are different. This means that if one or more packages is not installed prior to running the function, then the function will determine which are not installed, install those. If no packages are missing, then it will not install anything. This function uses RAM caching, so the first time it is run in a new R session will be slower than subsequent times in which cached copies of e.g., the package dependency tree, can be used. "rerun-tolerant" is a requirement for a robust reproducible workflow; for every "manual" break in code (i.e., a user runs a bit of code, then skips a few lines, then runs more etc.) provides the potential for sections of code to become stale without the user being aware. 

`Install` and `Require` are identical except that `Require` will also call `require` (lower case `r`) on all the named packages with the default setting of `require = TRUE`. 

```r
# These lines
if (!require("data.table")) {install.packages("data.table"); require("data.table")}
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
if (!require("lme4")) {install.packages("lme4"); require("lme4")}

# become
Require(c("data.table", "dplyr", "lme4"))
```

# Other packages that also install packages

The below descriptions are necessarily simple; please go see each package for more details. Below, we highlight some key features that are relevant to this README. `Require` offers a different way to achieve the features from all 5 of these packages that are necessary to build a unified, organic, yet reproducible approach to package management in one or many projects.

## `pak`

`pak` focuses on fast installations of *current* versions of packages on CRAN-like packages and GitHub.com and other similar code-sharing pages. This works well if the objective is to keep current. It is fast.

```r
# These lines
pak::pkg_install(c("data.table", "dplyr", "lme4"))

# become
Require::Install(c("data.table", "dplyr", "lme4"))
```

## `renv`

`renv` is a tool to help with a more static package installation process. While it can handle packages that are updated manually by an individual, the key strength is around keeping track of the versions that exist in a project. `renv` is not intended to expose the code used to install packages. This makes the managing of packages separate from the script that is/are used in the project, i.e., the package script does not contain the necessary information to recreate the package library.

```
renv::snapshot()
# becomes
Require::pkgSnapshot()
```

## `packrat` 

This is mostly the predecessor to `renv`. `renv` can do everything `packrat` can do, but better.

## `checkpoint`

This approach takes a date as an input and will install all the packages a user wants, from that date. This uses the MRAN servers hosted by Microsoft.

```
checkpoint("2022-11-09")
# cannot be achieved directly with Require, but
Require("reproducible (==1.2.10)") # which was the version on that date
```

## `Require`

### Features

* reproducible workflows -- rerun-tolerant
* fast (see one example of timings below)
* packages can be on CRAN, CRAN-alikes, or GitHub.com
* uses `.libPaths()` like base-R
* true *stand alone*, not the folder-based approach used in `.libPaths()`. For example, if a user e.g., on Windows Home has packages installed in the system folder because admin privileges allows it, setting `standAlone = TRUE` will ignore those packages and only use the ones in `.libPaths()[1]`
* can use hierarchical library paths
* can take snapshots, keeping version information for reinstallation on another machine/system. See below and `?pkgSnapshot`
* can use both binary or source installs; yet overrides a user request where this is likely inappropriate, e.g., `Rcpp` often fails when installed on Linux from a binary package manager. Spatial packages are similar. 
* uses a local cache of packages (defaults outside the project, but inside the user's home) so multiple projects can install packages quickly without re-downloading from the cloud repositories (see `RequireOptions()$Require.RPackageCache`)
* puts all package installing into a compact form, so it can be placed within the project source code, enhancing reproducibility and transparency
* by default, runs `require` on every package, in order, as supplied to `packages` argument


# Installing

## News

See updates from latest [CRAN](https://cran.r-project.org/package=Require) and [development](https://github.com/PredictiveEcology/Require/blob/development/NEWS.md) versions. 

## Installation

### Current release

[![R build status](https://github.com/PredictiveEcology/Require/workflows/R-CMD-check/badge.svg?branch=master)](https://github.com/PredictiveEcology/Require/actions)
[![codecov](https://codecov.io/gh/PredictiveEcology/Require/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/Require)

**Install from CRAN:**

```r
install.packages("Require")
```

**Install from GitHub:**
    
```r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("PredictiveEcology/Require") 
```

### Development version

[![R build status](https://github.com/PredictiveEcology/Require/workflows/R-CMD-check/badge.svg?branch=development)](https://github.com/PredictiveEcology/Require/actions)
[![codecov](https://codecov.io/gh/PredictiveEcology/Require/branch/development/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/Require)

**Install from r-universe:**

This is a development version of the package:

```r
install.packages("Require", repose = "https://predictiveecology.r-universe.dev") 
```

**Install from GitHub:**

```r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("PredictiveEcology/Require@development") 
```


## Basic usage

`Require` is a wrapper around functions `utils::install packages` and one of the main function to load packages, `base::require`. Like `install.packages`, it is vectorized on package names.

```r
if (!require("Require")) {install.packages("Require")} # sadly, Require can't install itself
Require::Require("data.table")

# With version numbering, this will install data.table, and also check that the installed 
#    version is greater than 1.12.8; if not, then it will install a newer version
Require("data.table (>=1.12.8)")

# vectorized, mixed github and CRAN, mixed version number and not
Require(c("data.table (>=1.12.8)", "PredictiveEcology/quickPlot"))
```
## Timings

`Require` has been optimized for speed. While `pak` is fast, in many cases `Require` is faster. Below, in cases where all packages are already installed (i.e., a standard day-to-day work situation), `Require` is 3-40x faster.
```r
# First time run, before cache exists
> library(pak)
> system.time(pak::pkg_install(c("data.table", "dplyr", "lme4")))
✔ Loading metadata database ... done
                                                                            
ℹ No downloads are needed
✔ 3 pkgs + 24 deps: kept 21 [3.4s]
   user  system elapsed 
  0.949   0.036   3.532 
> library(Require)
> system.time(Require::Require(c("data.table", "dplyr", "lme4"), require = FALSE, purge = TRUE))
   user  system elapsed 
  1.250   0.019   1.494

# Second time run, using cache
> system.time(pak::pkg_install(c("data.table", "dplyr", "lme4")))
                                                                            
ℹ No downloads are needed
✔ 3 pkgs + 24 deps: kept 21 [966ms]
   user  system elapsed 
  0.028   0.001   0.982 
> system.time(Require::Require(c("data.table", "dplyr", "lme4"), require = FALSE))
   user  system elapsed 
  0.027   0.001   0.024
```
## Other features

### Keeping it all isolated

Require can make install to and use from a single directory, so a project can be fully isolated (unlike `.libPaths()`, which will always see packages in the R_HOME directory)

```r
library(Require)
projectPackages = "projectPackages"
dir.create(projectPackages)
Require("remotes (>=2.4.0)", standAlone = TRUE, libPaths = projectPackages)
```

Or we can use a hybrid of our main, "personal" library and a project specific one for "extra" packages:

```r
Require("fpCompare (>=0.2.0)", require = FALSE) # don't load it, just install
```
### Installing old package versions

`Require` has the functionality of `renv` and `versions` in that you can install previous versions. On Windows, it will search for the binary version on MRAN and CRAN Archives. In the same way as above, we can specify maximum or exact package versions.

```r
Require("fpCompare (<=0.1.0)") # we don't have to know where to get this
```

### Managing a project

Because it is vectorized, there can be a long list of packages at the top of a project file, with various sources and version specifications.

```r
Require(c("data.table (==1.12.8)", "dplyr", "reproducible", 
          "PredictiveEcology/SpaDES@development", "raster (>=3.1.5)"), 
        libPaths = tempdir(),
        standAlone = TRUE)
```

### Taking a snapshot

When a system is set up with the correct packages and versions, we can take a snapshot and give that file to another person or machine:

```r
library(Require)
pkgSnapshot("mySnapshot.txt", standAlone = TRUE) # to get only the project specific ones

## move to a new machine, say
Require(packageVersionFile = "mySnapshot.txt")
```

The argument `packageVersionFile` can also be `TRUE` if the default filename is accepted.

### Using local package cache

By default, `Require` stashes (source and the locally-built binary) packages in a cached folder. If the user needs to reinstall them for the same project, a different project, or a different machine on a network, the `Require` will use this local copy. This will skip the download, and will also be a binary on Linux-alike systems, meaning it will install very fast.  Setting `options("Require.RPackageCache" = "somePath")` will move it to that location; or setting  `options("Require.RPackageCache" = FALSE)` will turn caching off. 

### Keeping up to date

`Require` generally does not try to keep packages up to date; instead if defaults to keeping packages sufficiently up to date that they do not violate version requirements. However, `Require` has 2 mechanisms to keep packages up to date: either as a group using the `update = TRUE` (or `upgrade = TRUE`) argument in `Require` or using the `(HEAD)` specification for each individual package. Using `(HEAD)` allows a user to always have the latest version of individual packages, without wholesale updating.

```
Require("PredictiveEcology/reproducible@development (HEAD)") # will install reproducible if it has change on GitHub.
Require("reproducible", update = TRUE) # will update reproducible and all dependencies
```

# Conclusion

`Require` package offers a simple, lightweight (one non-base package dependency), package focused around a single function that is "rerun-tolerant", i.e., it will take sufficiently little time to execute that it can be left in your script so it is run every time, even for ongoing work.
Because it has one dependencies (`data.table`) and so can be used to install packages without interfering with itself.

## Contributions

Please see `CONTRIBUTING.md` for information on how to contribute to this project.
