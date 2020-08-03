# Require

<!-- badges start -->
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/Require)](https://cran.r-project.org/package=Require)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/Require)](https://cran.r-project.org/package=Require)
[![R build status](https://github.com/PredictiveEcology/Require/workflows/R-CMD-check/badge.svg)](https://github.com/PredictiveEcology/Require/actions)
<!-- badges: end -->

A simple package for reproducible package management in R.
Built on top of `git2r` and `archivist`, this package aims at making high-level, robust, machine and OS independent tools for making deeply reproducible package management in R.
This extends beyond the package management utilities of `packrat` and `checkpoint` by including all in one management for packages in R focused around a single function, `Require`.

# Objectives

Some packages, including those in our PredictiveEcology repository, have _many_ package dependencies. 
Some of them are on CRAN, but some are still in development and so are hosted elsewhere. 
Mixing many package dependencies that are constantly evolving creates challenges with standard R package management.
For example, what is the best way to move analyses from one machine to another, or set up a series of High Performance Compute nodes? 
How should we use functions like `install.packages` in a reproducible workflow that are clearly intended to be used once or very few times?
How do we deal with many packages on GitHub that have many common dependencies?
Finally, how do we do all this for many concurrent projects without installing hundreds of packages in a new directory for every project?

The `Require` package attempts to address these issues and others. 
It is different than `packrat` in that it is much simpler and is closer to base R package management.
`Require` _can_ use hierarchical library paths, as in base R, with many paths in the `.libPaths()`, or can set a single library path to be `standAlone`.
This allows "system" packages to be used as well as "project-specific" packages to be used together, as in base R.
It is different than other packages like `renv` in that it is focused around a single function, `Require`, that can be used in a reproducible workflow. 
`renv` uses a notion of package versions, the "snapshot", _as installed_ at its foundation; any changes to package versions by a user then updates this snapshot.
`renv` does not keep this information in the source code of the project.
`Require` uses the notion of a "snapshot" as a decision to make by a user when it is time to _set_ the package versions. 
Package versions are primarily updated by the code developer by stating the minimum (or maximum) package version _in the source code_.
This means that by default, projects are somewhat more fluid, defined by no package version if none is required or minimum (or maximum) package versions if required until it is time to freeze it, say when publishing or needing to set up virtual machines with identical setup.
From this perspective, `renv` is more "top-down", and `Require` is more "bottom-up", though they can each emulate the other's behaviour.

We define a reproducible workflow as a workflow that can be run from the start to any point in the project, without having to "skip over" or "comment out" or "jump to" particular lines or chunks of code. 
`Require` does that. 

## Basic usage

`Require` is essentially a wrapper around functions that install packages, e.g., `install.packages`, `install_github` and one of the main function to load packages, `require`. 

```
# install.packages("Require") 
library(Require)
Require("data.table")
```

And with version numbering:

```
Require("data.table (>=1.12.8)")
```

It is vectorized on package names, and can include mixed github and CRAN, mixed version number and not:
```
Require(c("data.table (>=1.12.8)", "PredictiveEcology/quickPlot"))
```

## Advanced usage

### Keeping it all isolated

We can keep all packages in an isolated folder for a project, using `standAlone = TRUE`

```
setLibPaths("Project_A_Packages", standAlone = TRUE)
.libPaths() # this is just to check what happened in the previous line -- there are 2 folders only
Require("data.table (>=1.12.8)")
```

Or we can use a hybrid of our main, "personal" library and a project specific one for "extra" packages:

```
setLibPaths("Project_A_Packages", standAlone = FALSE)
.libPaths() # we have added a library to original ones on this system
Require("data.table (>=1.12.8)")
```
### Installing old package versions

In the same way as above, we can specify maximum or exact package versions. 
`Require` will retrieve these on CRAN archives.

```
Require("data.table (<=1.11.0)")
```

### Managing a project

Because it is vectorized, there can be a long list of packages at the top of a project file, with various sources and version specifications.

```
library(Require)
setLibPaths("ProjectA", standAlone = TRUE)
Require(c("data.table (==1.12.8)", "dplyr", "reproducible", 
          "PredictiveEcology/SpaDES@development", "raster (>=3.1.5)"))
```

### Taking a snapshot

When a system is set up with the correct packages and versions, we can take a snapshot and give that file to another person or machine:

```
pkgSnapshot("mySnapshot.txt", standAlone = TRUE) # to get only the project specific ones
```
Move to a new machine, say
```
Require::Require(packageVersionFile = "mySnapshot.txt")
```

# Installing

## News

See updates from latest [CRAN](https://cran.r-project.org/package=Require) and [development](https://github.com/PredictiveEcology/Require/blob/development/NEWS.md) versions. 

## Installation

### Current release

[![Build Status](https://travis-ci.org/PredictiveEcology/Require.svg?branch=master)](https://travis-ci.org/PredictiveEcology/Require)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/2fxqhgk6miv2fytd/branch/master?svg=true)](https://ci.appveyor.com/project/achubaty/Require/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/Require/badge.svg?branch=master)](https://coveralls.io/github/PredictiveEcology/Require?branch=master)

**Install from CRAN:**

```r
install.packages("Require")
```

**Install from GitHub:**
    
```
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/Require", dependencies = TRUE) 
```

### Development version

[![Build Status](https://travis-ci.org/PredictiveEcology/Require.svg?branch=development)](https://travis-ci.org/PredictiveEcology/Require)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/2fxqhgk6miv2fytd/branch/development?svg=true)](https://ci.appveyor.com/project/achubaty/Require/branch/development)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/Require/badge.svg?branch=development)](https://coveralls.io/github/PredictiveEcology/Require?branch=development)

**Install from GitHub:**

```
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/Require", ref = "development", dependencies = TRUE) 
```

# Conclusion

`Require` package offers a simple, lightweight, package focused around a single function.
The package has very few dependencies and so can be used to install packages without interfering with itself.

## Contributions

Please see `CONTRIBUTING.md` for information on how to contribute to this project.
