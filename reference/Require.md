# Require: Installing and Loading R Packages for Reproducible Workflows

A single key function, 'Require' that makes rerun-tolerant versions of
'install.packages' and 'require' for CRAN packages, packages no longer
on CRAN (i.e., archived), specific versions of packages, and GitHub
packages. This approach is developed to create reproducible workflows
that are flexible and fast enough to use while in development stages,
while able to build snapshots once a stable package collection is found.
As with other functions in a reproducible workflow, this package
emphasizes functions that return the same result whether it is the first
or subsequent times running the function, with subsequent times being
sufficiently fast that they can be run every time without undue waiting
burden on the user or developer.

This is an "all in one" function that will run `install.packages` for
CRAN and [GitHub](https://github.com/) packages and will install
specific versions of each package if versions are specified either via
an (in)equality (e.g., `"glue (>=1.6.2)"` or `"glue (==1.6.2)"` for an
exact version) or with a `packageVersionFile`. If `require = TRUE`, the
default, the function will then run `require` on all named packages that
satisfy their version requirements. If packages are already installed
(`packages` supplied), and their optional version numbers are satisfied,
then the "install" component will be skipped.

## Usage

``` r
Require(
  packages,
  packageVersionFile,
  libPaths,
  install_githubArgs = list(),
  install.packagesArgs = list(INSTALL_opts = "--no-multiarch"),
  standAlone = getOption("Require.standAlone", FALSE),
  install = getOption("Require.install", TRUE),
  require = getOption("Require.require", TRUE),
  repos = getOption("repos"),
  purge = getOption("Require.purge", FALSE),
  verbose = getOption("Require.verbose", FALSE),
  type = getOption("pkgType"),
  upgrade = FALSE,
  returnDetails = FALSE,
  ...
)

Install(
  packages,
  packageVersionFile,
  libPaths,
  install_githubArgs = list(),
  install.packagesArgs = list(INSTALL_opts = "--no-multiarch"),
  standAlone = getOption("Require.standAlone", FALSE),
  install = TRUE,
  repos = getOption("repos"),
  purge = getOption("Require.purge", FALSE),
  verbose = getOption("Require.verbose", FALSE),
  type = getOption("pkgType"),
  upgrade = FALSE,
  ...
)
```

## Arguments

- packages:

  Either a character vector of packages to install via
  `install.packages`, then load (i.e., with `library`), or, for
  convenience, a vector or list (using `c` or `list`) of unquoted
  package names to install and/or load (as in `require`, but
  vectorized). Passing vectors of names may not work in all cases, so
  user should confirm before relying on this behaviour in operational
  code. In the case of a GitHub package, it will be assumed that the
  name of the repository is the name of the package. If this is not the
  case, then pass a *named* character vector here, where the names are
  the package names that could be different than the GitHub repository
  name.

- packageVersionFile:

  Character string of a file name or logical. If `TRUE`, then this
  function will load the default file,
  `getOption("Require.packageVersionFile")`. If this argument is
  provided, then this will override any packages passed to `packages`.
  By default, `Require` will attempt to resolve dependency violations
  (i.e., if this `packageVersionFile` specifies a version of a package
  that violates the dependency specification of another package). If a
  user wishes to attempt to install the `packageVersionFile` without
  assessing the dependencies, set `dependencies = FALSE`.

- libPaths:

  The library path (or libraries) where all packages should be
  installed, and looked for to load (i.e., call `library`). This can be
  used to create isolated, stand alone package installations, if used
  with `standAlone = TRUE`. Currently, the path supplied here will be
  prepended to [`.libPaths()`](https://rdrr.io/r/base/libPaths.html)
  (temporarily during this call) to `Require` if `standAlone = FALSE` or
  will set (temporarily)
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) to
  `c(libPaths, tail(libPaths(), 1)` to keep base packages.

- install_githubArgs:

  Deprecated. Values passed here are merged with `install.packagesArgs`,
  with the `install.packagesArgs` taking precedence if conflicting.

- install.packagesArgs:

  List of optional named arguments, passed to `install.packages`.
  Default is only `--no-multi-arch`, meaning that only the current
  architecture will be built and installed (e.g., 64 bit, not 32 bit, in
  many cases).

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

- install:

  Logical or "force". If `FALSE`, this will not try to install anything.
  If `"force"`, then it will force installation of requested packages,
  mimicking a call to e.g., `install.packages`. If `TRUE`, the default,
  then this function will try to install any missing packages or
  dependencies.

- require:

  Logical or character string. If `TRUE`, the default, then the function
  will attempt to call `require` on all requested `packages`, possibly
  after they are installed. If a character string, then it will only
  call `require` on those specific packages (i.e., it will install the
  ones listed in `packages`, but load the packages listed in `require`)

- repos:

  The remote repository (e.g., a CRAN mirror), passed to either
  `install.packages`, `install_github` or `installVersions`.

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

- verbose:

  Numeric or logical indicating how verbose should the function be. If
  -1 or -2, then as little verbosity as possible. If 0 or FALSE, then
  minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
  `Require` function, when `verbose >= 2`, also returns details as if
  `returnDetails = TRUE` (for backwards compatibility).

- type:

  See
  [`utils::install.packages`](https://rdrr.io/r/utils/install.packages.html)

- upgrade:

  When `FALSE`, the default, will only upgrade a package when the
  version on in the local library is not adequate for the version
  requirements of the `packages`. Note: for convenience, `update` can be
  used for this argument.

- returnDetails:

  Logical. If `TRUE` the return object will have an attribute:
  `attr(.., "Require")` which has lots of information about the
  processes of the installs.

- ...:

  Passed to `install.packages`. Good candidates are e.g., `type` or
  `dependencies`. This can be used with `install_githubArgs` or
  `install.packageArgs` which give individual options for those 2
  internal function calls.

## Value

`Require` is intended to replace
[`base::require`](https://rdrr.io/r/base/library.html), thus it returns
a logical, named vector indicating whether the named packages have been
loaded. Because `Require` also has the ability to install packages, a
return value of `FALSE` does not mean that it did not install correctly;
rather, it means it did not attach with `require`, which could be
because it did not install correctly, or also because e.g.,
`require = FALSE`.

`standAlone` will either put the `Require`d packages and their
dependencies *all* within the `libPaths` (if `TRUE`) or if `FALSE` will
only install packages and their dependencies that are otherwise not
installed in `.libPaths()[1]`, i.e., the current active R package
directory. Any packages or dependencies that are not yet installed will
be installed in `libPaths`.

## Details

`Install` is the same as `Require(..., require = FALSE)`, for
convenience.

## Note

For advanced use and diagnosis, the user can set `verbose = TRUE` or `1`
or `2` (or via `options("Require.verbose")`). This will attach an
attribute `attr(obj, "Require")` to the output of this function.

## GitHub Package

Follows `remotes::install_github` standard. As with
`remotes::install_github`, it is not possible to specify a past version
of a GitHub package unless that version is a tag or the user passes the
SHA that had that package version. Similarly, if a developer does a
local install e.g., via `pkgload::install`, of an active project, this
package will not be able know of the GitHub state, and thus
`pkgSnapshot` will not be able to recover this state as there is no SHA
associated with a local installation. Use `Require` (or
`remotes::install_github`) to create a record of the GitHub state.

## Package Snapshots

To build a snapshot of the desired packages and their versions, first
run `Require` with all packages, then `pkgSnapshot`. If a `libPaths` is
used, it must be used in both functions.

## Mutual Dependencies

This function works best if all required packages are called within one
`Require` call, as all dependencies can be identified together, and all
package versions will be addressed (if there are no conflicts), allowing
a call to
[`pkgSnapshot()`](https://Require.predictiveecology.org/reference/pkgSnapshot.md)
to take a snapshot or "record" of the current collection of packages and
versions.

## Local Cache of Packages

When installing new packages, `Require` will put all source and binary
files in an R-version specific subfolder of
`getOption("Require.cachePkgDir")` whose default is `RPackageCache()`,
meaning *cache packages locally in a project-independent location*, and
will reuse them if needed. To turn off this feature, set
`options("Require.cachePkgDir" = FALSE)`.

## See also

Useful links:

- <https://Require.predictiveecology.org>

- <https://github.com/PredictiveEcology/Require>

- Report bugs at <https://github.com/PredictiveEcology/Require/issues>

## Author

**Maintainer**: Eliot J B McIntire <eliot.mcintire@canada.ca>
([ORCID](https://orcid.org/0000-0002-6914-8316))

Other contributors:

- Alex M Chubaty <achubaty@for-cast.ca>
  ([ORCID](https://orcid.org/0000-0001-7146-8135)) \[contributor\]

- His Majesty the King in Right of Canada, as represented by the
  Minister of Natural Resources Canada \[copyright holder\]

## Examples

``` r
if (FALSE) { # \dontrun{
opts <- Require:::.setupExample()

library(Require)
getCRANrepos(ind = 1)
Require("utils") # analogous to require(stats), but it checks for
#   pkg dependencies, and installs them, if missing

# unquoted version
Require(c(tools, utils))

if (Require:::.runLongExamples()) {
  # Install in a new local library (libPaths)
  tempPkgFolder <- file.path(tempdir(), "Require/Packages")
  # use standAlone, means it will put it in libPaths, even if it already exists
  #   in another local library (e.g., personal library)
  Install("crayon", libPaths = tempPkgFolder, standAlone = TRUE)

  # Mutual dependencies, only installs once -- e.g., cli
  tempPkgFolder <- file.path(tempdir(), "Require/Packages")
  Install(c("cli", "R6"), libPaths = tempPkgFolder, standAlone = TRUE)

  # Mutual dependencies, only installs once -- e.g., rlang
  tempPkgFolder <- file.path(tempdir(), "Require/Packages")
  Install(c("rlang", "ellipsis"), libPaths = tempPkgFolder, standAlone = TRUE)

  #####################################################################################
  # Isolated projects -- Use a project folder and pass to libPaths or set .libPaths() #
  #####################################################################################
  # GitHub packages
  if (requireNamespace("gitcreds", quietly = TRUE)) {
    # if (is(try(gitcreds::gitcreds_get(), silent = TRUE), "gitcreds")) {
    ProjectPackageFolder <- file.path(tempdir(), "Require/ProjectA")
    if (requireNamespace("curl")) {
      Require("PredictiveEcology/fpCompare@development",
        libPaths = ProjectPackageFolder,
      )
    }

    # No install because it is there already
    Install("PredictiveEcology/fpCompare@development",
      libPaths = ProjectPackageFolder,
    ) # the latest version on GitHub

    ############################################################################
    # Mixing and matching GitHub, CRAN, with and without version numbering
    ############################################################################
    pkgs <- c(
      "remotes (<=2.4.1)", # old version
      "digest (>= 0.6.28)", # recent version
      "PredictiveEcology/fpCompare@a0260b8476b06628bba0ae73af3430cce9620ca0" # exact version
    )
    Require::Require(pkgs, libPaths = ProjectPackageFolder)
    # }
  }
  Require:::.cleanup(opts)
}
} # }
```
