# Take a snapshot of all the packages and version numbers

This can be used later by `Require` to install or re-install the correct
versions. See examples.

## Usage

``` r
pkgSnapshot(
  packageVersionFile = getOption("Require.packageVersionFile"),
  libPaths = .libPaths(),
  standAlone = FALSE,
  purge = getOption("Require.purge", FALSE),
  exact = TRUE,
  includeBase = FALSE,
  verbose = getOption("Require.verbose")
)

pkgSnapshot2(
  packageVersionFile = getOption("Require.packageVersionFile"),
  libPaths,
  standAlone = FALSE,
  purge = getOption("Require.purge", FALSE),
  exact = TRUE,
  includeBase = FALSE,
  verbose = getOption("Require.verbose")
)
```

## Arguments

- packageVersionFile:

  A filename to save the packages and their currently installed version
  numbers. Defaults to `"packageVersions.txt"`. If this is specified to
  be `NULL`, the function will return the exact `Require` call needed to
  install all the packages at their current versions. This can be useful
  to add to a script to allow for reproducibility of a script.

- libPaths:

  The path to the local library where packages are installed. Defaults
  to the `.libPaths()[1]`.

- standAlone:

  Logical. If `TRUE`, all packages will be installed to and loaded from
  the `libPaths` only. NOTE: If `TRUE`, THIS WILL CHANGE THE USER'S
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html), similar to
  e.g., the `checkpoint` package. If `FALSE`, then `libPath` will be
  prepended to [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)
  during the `Require` call, resulting in shared packages, i.e., it will
  include the user's default package folder(s). This can be create
  dramatically faster installs if the user has a substantial number of
  the packages already in their personal library. Default `FALSE` to
  minimize package installing.

- purge:

  Logical. Should all caches be purged? Default is
  `getOption("Require.purge", FALSE)`. There is a lot of internal
  caching of results throughout the `Require` package. These help with
  speed and reduce calls to internet sources. However, sometimes these
  caches must be purged. The cached values are renewed when found to be
  too old, with the age limit. This maximum age can be set in seconds
  with the environment variable
  `R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE`, or if unset, defaults to
  3600 (one hour – see
  [`utils::available.packages`](https://rdrr.io/r/utils/available.packages.html)).

  Internally, there are calls to `available.packages`.

- exact:

  Logical. If `TRUE`, the default, then for GitHub packages, it will
  install the exact SHA, rather than the head of the
  `account/repo@branch`. For CRAN packages, it will install the exact
  version. If `FALSE`, then GitHub packages will identify their branch
  if that had been specified upon installation, not a SHA. If the
  package had been installed with reference to a SHA, then it will
  return the SHA as it does not know what branch it came from.
  Similarly, CRAN packages will report their version and specify with a
  `>=`, allowing a subsequent user to install with a minimum version
  number, as opposed to an exact version number.

- includeBase:

  Logical. Should R base packages be included, specifically, those in
  `tail(.libPaths(), 1)`

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

## Value

Will both write a file, and (invisibly) return a vector of packages with
the version numbers. This vector can be used directly in `Require`,
though it should likely be used with `require = FALSE` to prevent
attaching all the packages.

## Details

A file is written with the package names and versions of all packages
within `libPaths`. This can later be passed to `Require`.

`pkgSnapshot2` returns a vector of package names and versions, with no
file output. See examples.

## Examples

``` r
# \donttest{
if (Require:::.runLongExamples()) {
  opts <- Require:::.setupExample()

  # install one archived version so that below does something interesting
  libForThisEx <- tempdir2("Example")
  Require("crayon (==1.5.1)", libPaths = libForThisEx, require = FALSE)
  # Normal use -- using the libForThisEx for example;
  #    normally libPaths would be omitted to get all
  #    packages in user or project library
  tf <- tempfile()

  # writes to getOption("Require.packageVersionFile")
  # within project; also returns a vector
  # of packages with version
  pkgs <- pkgSnapshot(
    packageVersionFile = tf,
    libPaths = libForThisEx, standAlone = TRUE # only this library
  )

  # Now move this file to another computer e.g. by committing in git,
  #   emailing, googledrive
  #   on next computer/project
  Require(packageVersionFile = tf, libPaths = libForThisEx)

  # Using pkgSnapshot2 to get the vector of packages and versions
  pkgs <- pkgSnapshot2(
    libPaths = libForThisEx, standAlone = TRUE
  )
  Install(pkgs) # will install packages from previous line

  Require:::.cleanup(opts)
  unlink(getOption("Require.packageVersionFile"))
}
# }
```
