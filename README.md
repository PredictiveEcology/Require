---
title: Readme
output: rmarkdown::html_vignette
author: "Eliot McIntire"
vignette: >
  %\VignetteIndexEntry{Readme}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
editor_options: 
  chunk_output_type: console
---


# Require

<!-- badges start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/Require)](https://cran.r-project.org/package=Require)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/Require)](https://cran.r-project.org/package=Require)
[![R build status](https://github.com/PredictiveEcology/Require/workflows/R-CMD-check/badge.svg)](https://github.com/PredictiveEcology/Require/actions)
<!-- badges: end -->

`Require` is a single package that combines features of `base::install.packages`, `base::library`, `base::require`, as well as `pak::pkg_install`, `remotes::install_github`, and `versions::install_version`, plus the snapshotting capabilities of `renv`. It takes its name from the idea that a user could simply have one line like this:

```r
Require(c("dplyr", "lmer", "PredictiveEcology/LandR@development"))
```
named after the `require` function, that would load packages. But with `Require`, it will also install the packages, if necessary. Set it and forget it. This makes if *very clear* what packages are being used in a project. `Require` also continues to work, even if packages are taken off CRAN. This means that even if there is a dependency that is removed from CRAN ("archived"), the line will still work. Because it can be done in one line, it becomes relatively easy to share, it is transparent, and facilitates reproducibility (especially when combined with version specifications). These in turn facilitate, for example, making reprexes for debugging. 

# Objectives

Some packages have _many_ package dependencies. 
Some of them are on CRAN; some are in development and may be hosted elsewhere. 
Mixing many package dependencies that are constantly evolving creates challenges with standard R package management.
For example, what is the best way to move analyses from one machine to another, or set up a series of High Performance Compute nodes? 
How should we use functions like `install.packages` in a reproducible workflow that are clearly intended to be used once or very few times?
How do we deal with many packages on GitHub that have many common dependencies, but that may point to different branches on a GitHub repository?
How do we deal with packages that have dependencies that are no longer on CRAN ("they have been archived")?
How do we replicate an analysis 6 months from now when some packages have changed, and their dependencies have changed?

# `Require` & `Install`

The `Require` package provides two "rerun-tolerant" functions, `Require` and `Install`. "Rerun-tolerant" means that the results from running this function (the output) will be identical each time, even when the conditions when run are different. This means that if one or more packages is not installed prior to running the function, then the function will determine which are not installed, install those and continue on. If no packages are missing, then it will not install anything. This function uses both RAM and disk caching, so the first time it is run in a new R session will be slower than subsequent times in which cached copies of e.g., the package dependency tree, can be used. "Rerun-tolerant" is a requirement for a robust reproducible workflow; for every "manual" break in code (i.e., a user runs a bit of code, then skips a few lines, then runs more etc.) provides the potential for sections of code to become stale without the user being aware. 

`Install` and `Require` are identical except that `Require` will also call `require` (lower case `r`) on all the named packages with the default setting of `require = TRUE`. 

# Converting to using `Require`

These lines:
```r
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
if (!require("lme4")) {install.packages("lme4"); require("lme4")}
if (!require("tidyverse")) {install.packages("tidyverse"); require("tidyverse")}
```

become:
```r
if (!require("Require")) {install.packages("Require"); require("Require")}
Require(c("dplyr", "lme4", "tidyverse"))
```
noting that Require is an extra package, so to be fully reproducible, it needs to be installed first.

## Other packages that also install packages

The below descriptions are necessarily simple; please go see each package for more details. Below, we highlight some key features that are relevant to this README. `Require` offers a different way to achieve the features from all 5 of these packages that are necessary to build a unified, organic, yet reproducible approach to package management in one or many projects.

### `pak`

`pak` focuses on fast installations of *current* versions of packages on CRAN-like packages and GitHub.com and other similar code-sharing pages. This works well if the objective is to keep current. It is fast.

```r
# These lines
pak::pkg_install(c("dplyr", "lme4"))

# become
Require::Install(c("dplyr", "lme4"))
```

### `renv`

`renv` is a tool to help with a more static package installation process. While it can handle packages that are updated manually by an individual, the key strength is around keeping track of the versions that exist in a project. `renv` is not intended to expose the code used to install packages. This makes the managing of packages separate from the script that is/are used in the project, i.e., the package script does not contain the necessary information to recreate the package library.

```
renv::snapshot()
# becomes
Require::pkgSnapshot()
```

### `packrat` 

This is mostly the predecessor to `renv`. `renv` can do everything `packrat` can do, but better.

### `checkpoint`

This approach takes a date as an input and will install all the packages a user wants, from that date. This uses the posit.packagemanager.co servers hosted by Posit.

```
checkpoint("2022-11-09")
# cannot be achieved directly with Require, but
Require("reproducible (==1.2.10)") # which was the version on that date
```


# Package features

* reproducible workflows -- rerun-tolerant
* fast (see one example of timings below)
* packages can be on CRAN, CRAN-alikes, or GitHub.com
* uses `.libPaths()` like base-R
* true *stand alone*, not the folder-based approach used in `.libPaths()`. For example, if a user e.g., on Windows Home has packages installed in the system folder because admin privileges allows it, setting `standAlone = TRUE` will ignore those packages and only use the ones in `.libPaths()[1]`
* can use hierarchical library paths
* can take snapshots, keeping version information for reinstallation on another machine/system. See below and `?pkgSnapshot`
* can use both binary or source installs; yet overrides a user request where this is likely inappropriate, e.g., `Rcpp` often fails when installed on Linux from a binary package manager. Spatial packages are similar. 
* uses a local cache of packages (defaults outside the project, but inside the user's home) so multiple projects can install packages quickly without re-downloading from the cloud repositories (see `RequireOptions()$Require.cachePkgDir`)
* puts all package installing into a compact form, so it can be placed within the project source code, enhancing reproducibility and transparency
* If using the function `Require::Require`, runs `require` on every package, in order, as supplied to `packages` argument


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

**Install development version:**
    
```r
# Installing the development binary (Windows/MacOS)
install.packages("Require", repos = "https://predictiveecology.r-universe.dev")

# Or installing from source
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
## It is fast

`Require` has been optimized for speed. While `pak` is fast, in many cases `Require` is faster. Below, in cases where all packages are already installed, `Require` is 10x-20x faster, so it can be left in code.  

```r
# First time run, 
> pkgs <- c("dplyr", "reproducible", 
+           "PredictiveEcology/SpaDES@development", "terra")
> system.time(pak::pkg_install(pkgs))
✔ Loading metadata database ... done
                                                                                         
ℹ No downloads are needed
✔ 4 pkgs + 39 deps: kept 40 [6.8s]
   user  system elapsed 
   0.79    0.28    6.86 
> system.time(Require::Install(pkgs))
  1 packages on GitHub
  3 packages on GitHub
No packages to install/update
   user  system elapsed 
   0.44    0.07    0.51 
> system.time(pak::pkg_install(pkgs))
                                                                                          
# Second time run within same session
ℹ No downloads are needed
✔ 4 pkgs + 39 deps: kept 40 [2.1s]
   user  system elapsed 
   0.06    0.04    2.12 
> system.time(Require::Install(pkgs))
No packages to install/update
   user  system elapsed 
   0.05    0.02    0.06 
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

### Installing old package versions

`Require` has the functionality of `renv` and `versions` in that you can install previous versions. On Windows, it will search for the binary version on MRAN and CRAN Archives. In the same way as above, we can specify maximum or exact package versions.

```r
Require("fpCompare (<=0.1.0)") # we don't have to know where to get this
```

### Managing a project

Because it is vectorized, there can be a long list of packages at the top of a project file, with various sources and version specifications.

```r
Install(c("dplyr (==1.1.4)", "reproducible", 
          "PredictiveEcology/SpaDES@development", "raster (>=3.1.5)"))
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

When installing on many machines on a network, having a local cache can speed up installations. By default, this is activated, with a message upon package load as to where the cache folder is. Setting `options("Require.cachePkgDir" = "somePath")` will move it to that location; or setting  `options("Require.cachePkgDir" = NULL)` will turn caching off. By default, binaries will be saved on Windows. Also by default, binaries will be *built* on the fly on *nix systems and this binary will be cached for even faster installs later.

### Keeping up to date

`Require` generally does not try to keep packages up to date; instead if defaults to keeping packages sufficiently up to date that they do not violate version requirements. However, `Require` has 2 mechanisms to keep packages up to date: either as a group using the `update = TRUE` (or `upgrade = TRUE`) argument in `Require` or using the `(HEAD)` specification for each individual package. Using `(HEAD)` allows a user to always have the latest version of individual packages, without wholesale updating.

```
Require("PredictiveEcology/reproducible@development (HEAD)") # will install reproducible if it has change on GitHub.
Require("reproducible", update = TRUE) # will update reproducible and all dependencies
```

# Conclusion

`Require` package offers a simple package focused around a single function that is "rerun-tolerant", i.e., it will take sufficiently little time to execute that it can be left in your script so it is run every time, even for ongoing work.
The package has two dependencies (`data.table` and `sys`) and so can be used to install packages without interfering with itself.

## Contributions

Please see `CONTRIBUTING.md` for information on how to contribute to this project.
