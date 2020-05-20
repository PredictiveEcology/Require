# Require

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/Require)](https://cran.r-project.org/package=Require)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/Require)](https://cran.r-project.org/package=Require)

A simple package for reproducible package management in R.
Built on top of `git2r` and `archivist`, this package aims at making high-level, robust, machine and OS independent tools for making deeply reproducible package management in R.
This extends beyond the package management utilities of `packrat` and `checkpoint` by including all in one management for packages in R focused around a single function, `Require`.

# Objectives

Some packages, including those in our PredictiveEcology repository, have _many_ package dependencies. 
Some of them are on CRAN, but some are still in development and so are hosted elsewhere. 
Mixing many package depdencies that are constantly evolving creates challenges with standard R package management.
For example, what is the best way to move analyses from one machine to another, or set up a series of High Performance Compute nodes? 
How should we use functions like `install.packages` in a reproducible workflow that are clearly intended to be used once or very few times?
How do we deal with many packages on GitHub that have many common dependencies?
Finally, how do we do all this for many concurrent projects without installing hundreds of packages in a new directory for every project?

The `Require` package attempts to address these issues and others. 
It is different than `packrat` in that it is much simpler and is closer to base R package management.
It is different than other packages like `renv` in that it is focused around a single function, `Require`, that can be used in a reproducible workflow.

We define a reproducible workflow as a workflow that can be run from the start to any point in the project, without having to "skip over" or "comment out" or "jump to" particular lines or chunks of code. 
`Require` does that. 

## Basic usage

`Require` is essentially a wrapper around functions that install packages, e.g., `install.packages`, `install_github` and one of the main function to load packages, `require`. 
It is vectorized on package names.

```{r Intro, eval=FALSE}
# install.packages("Require") # sadly, Require can't install itself, so must comment this line
library(Require)
Require("data.table")

# With version numbering
Require("data.table (>=1.12.8)")

# vectorized, mixed github and CRAN, mixed version number and not
Require(c("data.table (>=1.12.8)", "PredictiveEcology/quickPlot"))

```

## Advanced usage

### Keeping it all isolated

We can keep all packages in an isolated folder for a project, using `standAlone = TRUE`

```{r standALone}
setLibPaths("Project_A_Packages", standAlone = TRUE)
.libPaths() # this is just to check what happened in the previous line -- there are 2 folders only
Require("data.table (>=1.12.8)")

```

Or we can use a hybrid of our main, "personal" library and a project specific one for "extra" packages:
```{r standALone}
setLibPaths("Project_A_Packages", standAlone = FALSE)
.libPaths() # we have added a library to original ones on this system
Require("data.table (>=1.12.8)")

```
### Installing old package versions

In the same way as above, we can specify maximum or exact package versions. 
`Require` will retrieve these on CRAN archives.

```{r archiveVersions}
Require("data.table (<=1.11.0)")
```

### Managing a project

Because it is vectorized, there can be a long list of packages at the start of a project.
```{r LongPackageList}
Require(c("data.table", "dplyr", "reproducible", "SpaDES", "raster"))
```

### Taking a snapshot

When a system is set up with the correct packages and versions, we can take a snapshot and give that file to another person or machine:

```{r packageSnaptop}
pkgSnapshot("mySnapshot.txt", standAlone = TRUE) # to get only the project specific ones

## move to a new machine, say
Require::Require(packageVersionFile = "mySnapshot.txt")

```

# Installing

## News

See updates from latest [CRAN](https://cran.r-project.org/package=Require) and [development](https://github.com/PredictiveEcology/Require/blob/development/NEWS.md) versions. 

## Installation

### Current release (not yet on CRAN)

[![Build Status](https://travis-ci.org/PredictiveEcology/Require.svg?branch=master)](https://travis-ci.org/PredictiveEcology/Require)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/2fxqhgk6miv2fytd/branch/master?svg=true)](https://ci.appveyor.com/project/achubaty/Require/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/Require/badge.svg?branch=master)](https://coveralls.io/github/PredictiveEcology/Require?branch=master)

**Install from CRAN:**

Not yet on CRAN
```r
# install.packages("Require")
```

**Install from GitHub:**
    
```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/Require", dependencies = TRUE) 
```


### Development version

[![Build Status](https://travis-ci.org/PredictiveEcology/Require.svg?branch=development)](https://travis-ci.org/PredictiveEcology/Require)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/2fxqhgk6miv2fytd/branch/development?svg=true)](https://ci.appveyor.com/project/achubaty/Require/branch/development)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/Require/badge.svg?branch=development)](https://coveralls.io/github/PredictiveEcology/Require?branch=development)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/Require", ref = "development", dependencies = TRUE) 
```

# Conclusion

`Require` package offers a simple, lightweight, package focused around a single function.
The package has very few dependencies and so can be used to install packages without interfering with itself.

## Contributions

Please see `CONTRIBUTING.md` for information on how to contribute to this project.
