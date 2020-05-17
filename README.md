# Require

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/Require)](https://cran.r-project.org/package=Require)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/Require)](https://cran.r-project.org/package=Require)

A simple package for reproducible package management in R.
Built on top of `git2r` and `archivist`, this package aims at making high-level, robust, machine and OS independent tools for making deeply reproducible package management in R.
This extends beyond the package management utilities of `packrat` and `checkpoint` by including all in one management for packages in R focused around a single function, `Require`.

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

## Contributions

Please see `CONTRIBUTING.md` for information on how to contribute to this project.
