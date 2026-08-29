# `Require` options

These provide top-level, powerful settings for a comprehensive
reproducible workflow. See Details below.

## Usage

``` r
RequireOptions()

getRequireOptions()
```

## Value

A named list of the package options and their default values.

## Details

- `RequireOptions()`:

  prints the default values of package options set at startup, which may
  have been changed (e.g., by the user) during the current session.

- `getRequireOptions()`:

  prints the current values of package options.

Below are options that can be set with
`options("Require.xxx" = newValue)`, where `xxx` is one of the values
below, and `newValue` is a new value to give the option. Sometimes these
options can be placed in the user's `.Rprofile` file so they persist
between sessions.

The following options are likely of interest to most users:

- `install`:

  Default: `TRUE`. This is the default argument to `Require`, but does
  not affect `Install`. If this is `FALSE`, then no installations will
  be attempted, and missing packages will result in an error.

- `cachePkgDir`:

  Deprecated, and ignored under `Require.usePak = TRUE` (the default):
  redirect pak's package cache with the `R_USER_CACHE_DIR` environment
  variable instead. `R_REQUIRE_PKG_CACHE` is deprecated for the same
  reason. Both are still honoured when `Require.usePak = FALSE`.
  Default:
  [`cacheGetOptionCachePkgDir()`](https://Require.predictiveecology.org/reference/cacheGetOptionCachePkgDir.md),
  which must be either a path or a logical. To turn off package caching,
  set this to `FALSE`. This can be set using an environment variable
  e.g., `Sys.setenv(R_REQUIRE_PKG_CACHE = "somePath")`, or
  `Sys.setenv(R_REQUIRE_PKG_CACHE = "TRUE")`; if that is not set, then
  an either a path or logical option
  (`options(Require.cachePkgDir = "somePath")` or
  `options(Require.cachePkgDir = TRUE)`). If `TRUE`, the default folder
  location
  [`cachePkgDir()`](https://Require.predictiveecology.org/reference/cacheDir.md)
  will be used. If this is `TRUE` or a path is provided, then binary and
  source packages will be cached here. Subsequent downloads of same
  package will use local copy. Default is to have packages not be cached
  locally so each install of the same version will be from the original
  source, e.g., CRAN, GitHub.

- `otherPkgs`:

  Default: A character vector of packages that are generally more
  successful if installed from Source on Unix-alikes. Since there are
  repositories that offer binary packages builds for Linux (e.g.,
  RStudio Package Manager), the vector of package names indicated here
  will default to a standard CRAN repository, forcing a source install.
  See also `spatialPkgs` option, which does the same for spatial
  packages.

- `noRemotes`:

  Default: `FALSE`. If `TRUE`, GitHub-style specs
  (`account/repo@branch`) passed to `Require`/`Install` are rewritten to
  their bare package name (the version constraint, if any, is
  preserved). The packages are then resolved from `repos` (e.g., a
  CRAN-like or r-universe repository serving prebuilt binaries) instead
  of being cloned and built from GitHub source. This avoids the need for
  git authentication and a source-build toolchain (e.g., Rtools on
  Windows), which is useful for workshops and other binary-only setups.
  Because version constraints are kept, `repos` must provide a version
  that satisfies them, otherwise resolution will fail (rather than
  silently falling back to GitHub). Ensure the relevant repository
  (e.g., `predictiveecology.r-universe.dev`) is in `getOption("repos")`.

- `purge`:

  Default: `FALSE`. If set to (almost) all internal caches used by
  `Require` will be deleted and rebuilt. This should not generally be
  necessary as it will automatically be deleted after (by default) 1
  hour (set via `R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE` environment
  variable in seconds).

- `cloneFrom`:

  Default: `NULL`. A path to an existing package library. When set, any
  package that is already installed there at the version being requested
  is copied or hard-linked into the destination library instead of being
  downloaded and installed, which is much faster when populating a new
  project library on a machine that already has the packages. Only
  packages that need no compilation and were built under a compatible R
  version are cloned; the rest go through the normal install machinery,
  as do `pak`, `callr`, `processx` and `cli`, whose native helper
  executables do not survive a file-by-file copy. Has no effect under
  `Require.usePak = TRUE` (the default): cloning lives in the legacy
  (non-pak) install path, so this option is consulted only when
  `Require.usePak = FALSE`.

- `downloadTimeout`:

  Default: `300L` (seconds). Used as the floor for `options("timeout")`
  during GitHub source-archive downloads in the legacy (non-pak) install
  path. R's stock 60-second timeout is too short for slow connections
  fetching multi-MB zips. Has no effect under `Require.usePak = TRUE`,
  which delegates downloads to pak's own libcurl client.

- `spatialPkgs`:

  Default: A character vector of packages that are generally more
  successful if installed from Source on Unix-alikes. Since there are
  repositories that offer binary packages builds for Linux (e.g., Posit
  Package Manager), the vector of package names indicated here will
  default to a standard CRAN repository, forcing a source install. See
  also `otherPkgs` option, which does the same for non-spatial packages.

- `useCranCache`:

  Default: `FALSE`. A user can optionally use the locally cached
  packages that are available due to a user's use of the `crancache`
  package.

- `checkInternet`:

  Default: `TRUE`. Whether `Require` may run a short internet probe
  before work that needs the network. Results are cached for
  `Require.internetExistsTimeout` seconds (30). Setting this `FALSE`
  skips the probe at most call sites, but not those that pass
  `force = TRUE`, where failing late costs more than a 2-second check.

- `forcePakReinstall`:

  Default: `FALSE`. Set `TRUE` to force one clean
  `install.packages("pak")` into the project library. This is the remedy
  for a pak whose bundled callr/processx helper executables no longer
  run – the state a file-by-file copy of a library leaves pak in.
  [`find.package()`](https://rdrr.io/r/base/find.package.html) cannot
  detect that (every file is present, they just do not run), so the
  reinstall has to be asked for explicitly.

- `installPackagesSys`:

  Default: `2L`. Legacy (non-pak) path only. `0` installs in-process
  with
  [`install.packages()`](https://rdrr.io/r/utils/install.packages.html);
  a non-zero value runs builds and installs through the `sys` package in
  a subprocess; `2` additionally performs the CRAN downloads itself
  rather than leaving them to
  [`install.packages()`](https://rdrr.io/r/utils/install.packages.html).
  Requires the `sys` package.

- `offlineMode`:

  Default: `FALSE`. When `TRUE`, `Require` attempts no network access
  and installs only from local caches. It is also set automatically –
  together with `Require.offlineModeSetAutomatically` – when an install
  fails and an internet probe finds no connection.

- `packageVersionFile`:

  Default: `"packageVersions.txt"`. The default file path used by
  [`pkgSnapshot()`](https://Require.predictiveecology.org/reference/pkgSnapshot.md)
  when writing a snapshot.

- `snapshotInstaller`:

  Default: `"pak"`. Which installer to use for a snapshot install.
  `"install.packages"` selects the pre-pak chain, which is retained but
  no longer developed. See
  [`?pkgSnapshot`](https://Require.predictiveecology.org/reference/pkgSnapshot.md)
  for the rest of the snapshot-installer options.

- `snapshotInstallerUsePPM`:

  Default: `TRUE`. Prepend a Posit Package Manager binary repository
  when installing a snapshot through the `install.packages` chain. PPM
  serves macOS binaries by content-negotiating the `R/<version>`
  User-Agent.

- `standAlone`:

  Default: `TRUE`. The default `standAlone` argument for `Require`,
  `Install` and
  [`setLibPaths()`](https://Require.predictiveecology.org/reference/setLibPaths.md).
  `TRUE` means the given library path is used on its own, without the
  user and site libraries appended; `FALSE` appends them. It controls
  whether those libraries are added, not how many library paths may be
  given – passing several is always allowed.

- `updateRprofile`:

  Default: `FALSE`. Whether
  [`setLibPaths()`](https://Require.predictiveecology.org/reference/setLibPaths.md)
  also writes the library path into the project `.Rprofile`, so it
  persists across sessions.

- `usePak`:

  Default: `TRUE`. Use pak to resolve and install packages. `FALSE`
  selects the pre-pak path. Several options apply only to that path and
  do nothing under the default: `cachePkgDir`, `cloneFrom`,
  `downloadTimeout` and `installPackagesSys`.

- `verbose`:

  Default: `1`. See ?Require.
