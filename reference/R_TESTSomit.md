# This environment variable "R_TESTS" is set during testing, and it points to a file called `Startup.Rs` that is placed in the `.libPaths()`. If the `.libPaths()` is changed during the testing, then that file will not be found, and install.packages will fail to install a package with an error of source file not found. See: <https://github.com/HenrikBengtsson/startup/issues/19>.

The environment variable is set here:
`https://github.com/wch/r-source/blob/8b6429feb661b02e2b2b6df1757b31cf1250a33e/src/library/tools/R/testing.R#L472-Lundefined`

## Usage

``` r
R_TESTSomit()
```
