Known issues: https://github.com/PredictiveEcology/Require/issues

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
