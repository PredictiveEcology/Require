# Setup for binary Linux repositories

Enable use of binary package builds for Linux from the RStudio Package
Manager repo. This will set the `repos` option, affecting the current R
session. It will put this `binaryLinux` in the first position. If the
`getOption("repos")` is `NULL`, it will put `backupCRAN` in second
position.

## Usage

``` r
setLinuxBinaryRepo(
  binaryLinux = urlForArchivedPkgs,
  backupCRAN = srcPackageURLOnCRAN
)
```

## Arguments

- binaryLinux:

  A CRAN repository serving binary Linux packages.

- backupCRAN:

  If there is no CRAN repository set
