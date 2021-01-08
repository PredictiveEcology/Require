# Require

<!-- badges start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/Require)](https://cran.r-project.org/package=Require)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/Require)](https://cran.r-project.org/package=Require)
[![R build status](https://github.com/PredictiveEcology/Require/workflows/R-CMD-check/badge.svg)](https://github.com/PredictiveEcology/Require/actions)
[![Join the chat at https://gitter.im/PredictiveEcology/Require](https://badges.gitter.im/PredictiveEcology/Require.svg)](https://gitter.im/PredictiveEcology/Require?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
<!-- badges: end -->

A simple package for reproducible package management in R.
This is different than other approaches to package management such as `packrat`, `checkpoint`, and `renv`, by including all-in-one management for packages in R focused around a single function, `Require`.

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
`Require` does that -- you can leave all lines of code in a script and they will work efficiently from any starting point.

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

## Setup a project

To have a reproducible project, add `Require::setup()` at the start of a script. This will
set the default R package library to a folder called `"R"` within the (project) working directory.
It will also set a package cache directory in the `.cache` subfolder of
the user's home directory (so it can) be used by every project on that machine. This could
be set to a folder shared across machines in, say, a network. This will also set
several options. Rather than "run this once", the function is designed to be run
"every" time, ensuring reproducibility if the script is given to somebody else. 

By default, this will add or modify the `.Rprofile` file in the (project) working directory so that each time the project is loaded, it will be run with the R package folder set up previously.

```{r setup}
Require::setup()
```

The above will set up a completely isolated folder (other than base R packages). This 
means that all packages will have to be re-installed. This may be the right behaviour,
for a fully reproducible system. However, there other situations, namely, there are 
already packages installed in the user or system libraries that can be re-used and 
only a few additional packages are needed for the project and should be isolated from
the other packages on the machine. 

```{r setup-standAloneF}
Require::setup(standAlone = FALSE)
```

## Turn setup off

To reset your project to how it was, use `setupOff()`, then remove any cases of `Require::setup()` that are in your scripts.

```{r setupOff}
setupOff()
```
## Advanced

### Keeping it all isolated

Rather than using `Require::setup()`, we can use the internal functions directly: `setLibPaths`, and various `options`. 

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
setup("ProjectA", standAlone = TRUE)
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

### Using local package cache

When installing on many machines on a network, having a local cache can speed up installations. Setting `options("Require.RPackageCache" = someSharedDirectory)` will turn on local cache. By default, binaries will be saved on Windows. Also by default, binaries will be *built* on the fly on *nix systems and this binary will be cached for even faster installs later (turned off with `options("Require.RPackageCache" = NULL)`)

# Installing

## News

See updates from latest [CRAN](https://cran.r-project.org/package=Require) and [development](https://github.com/PredictiveEcology/Require/blob/development/NEWS.md) versions. 

## Installation

### Current release

[![R build status](https://github.com/PredictiveEcology/Require/workflows/R-CMD-check/badge.svg?branch=master)](https://github.com/PredictiveEcology/Require/actions)
[![codecov](https://codecov.io/gh/PredictiveEcology/Require/branch/master/graph/badge.svg)](https://codecov.io/gh/PredictiveEcology/Require)

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

[![R build status](https://github.com/PredictiveEcology/Require/workflows/R-CMD-check/badge.svg?branch=development)](https://github.com/PredictiveEcology/Require/actions)
[![codecov](https://codecov.io/gh/PredictiveEcology/Require/branch/development/graph/badge.svg)](https://codecov.io/gh/PredictiveEcology/Require)

**Install from GitHub:**

```
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/Require", ref = "development", dependencies = TRUE) 
```

# Conclusion

`Require` package offers a simple, lightweight, package focused around a single function that is resilient to being called multiple times (unlike `install.packages`).
The package has two dependencies (`data.table` and `remotes`) and so can be used to install packages without interfering with itself.

## Contributions

Please see `CONTRIBUTING.md` for information on how to contribute to this project.
