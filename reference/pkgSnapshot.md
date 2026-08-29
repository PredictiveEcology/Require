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

## Installing from a snapshot file

Pass the snapshot file to
[`Require()`](https://Require.predictiveecology.org/reference/Require.md)
via `packageVersionFile = "snapshot.txt"`. By default this routes
through a multi-stage installer (gated by
`options(Require.snapshotInstaller = "install.packages")`) that:

1.  **Skips already-installed-at-target-version refs.**

2.  **Cache pre-filter** via
    [`pkgcache::pkg_cache_list()`](https://r-lib.github.io/pkgcache/reference/pkg_cache_api.html).
    Source tarballs feed `pak::pkg_install(local::...)`; binaries are
    reserved for step (4). Rotten cache rows (missing fullpath,
    gzip-corrupt, DESCRIPTION mismatch) are auto-evicted via
    [`pkgcache::pkg_cache_delete_files()`](https://r-lib.github.io/pkgcache/reference/pkg_cache_api.html)
    so future runs don't keep tripping on them.

3.  **Parallel libcurl-multi download** for refs not in cache, chunked
    at 50 URLs per call (macOS file-descriptor limit). Walks priority
    URLs: row's `Repository` -\> PPM -\> CRAN -\> CRAN/Archive. Up to 4
    retries with exponential backoff
    (`Require.snapshotDownloadAttempts`).

4.  **Hybrid binary-first install** via
    `install.packages(type = "binary")` for any ref that has a cache
    binary matching this R session (`R.version$platform`,
    `<major>.<minor>`). Skips compilation, reduces pak's parallel-build
    workload. Disable via
    `options(Require.snapshotInstallerHybrid = FALSE)`.

5.  **`pak::pkg_install(local::...)` for the rest** with
    `dependencies = NA`, `upgrade = FALSE`. Refs
    already-installed-at-target-version are excluded upfront so pak
    doesn't reinstall-to-self.

6.  **`install.packages(repos = file://...)` fallback** if pak refuses
    (its solver is strict; `install.packages` is best-effort and
    tolerates per-package compile failures).

7.  **Bump-and-retry**: for any ref still missing, walks newer-than-pin
    versions from CRAN/PPM/Archive ascending and tries each until one
    installs (capped at 20 candidates). Disable via
    `options(Require.snapshotInstallerBumpOnFail = FALSE)` for strict
    reproducibility (no drift, fail loudly).

8.  **Diagnostic report** classifying each gap with a concrete `fix:`
    line.

Built binaries from this run are added back to `pkgcache` via
`cacheBuiltBinaries()` (registered on `on.exit`), so a subsequent run
hits step (4) instead of recompiling.

## Common snapshot pitfalls

- Version-coherence (the snapshot's own pins disagree):

  A ref's `DESCRIPTION` declares `Imports: X (>= V)` but the snapshot
  pins `X` at a version that doesn't satisfy it. `pak`'s strict solver
  refuses to install. The installer runs a coherence pre-check before
  handing off to pak and prints any unsatisfied constraint with a fix
  suggestion (e.g.,
  `servr 0.30 requires xfun (>= 0.42); snapshot pins xfun = 0.40 -> bump xfun`).

- `R` pseudo-package:

  `pkgSnapshot()` writes a row recording the running R version (e.g.,
  `R,4.4,...`). The installer skips this row alongside base packages.

- System library mismatches (compile failures):

  Source builds need the host's system libs to match what the package
  expects. The installer's `classifyCompileFailure()` recognises
  missing-header errors (jpeglib.h, gdal.h, geos_c.h, glpk.h,
  ft2build.h, sodium.h, ...) and prints the corresponding
  `brew install ...` (or apt) suggestion. R 4.5's removal of
  `Calloc/Free`, GDAL \>= 3.10's `const OGRSpatialReference*` ABI
  change, and Rcpp's `class_::constructor<>` template-arity limit are
  each pattern-matched and reported with a "bump `<pkg>`" suggestion.

- Mac toolchain – `~/.R/Makevars`:

  R's default compile flags only search `/opt/R/arm64/include`. To pick
  up Homebrew headers (libjpeg, glpk, freetype, etc.), add to
  `~/.R/Makevars`:


            CPPFLAGS += -I/opt/homebrew/include
            LDFLAGS  += -L/opt/homebrew/lib
          

- Stale `pkgcache` index:

  `pkgcache` shares state across R versions and architectures. Cache
  rows tagged with platform/rversion that don't match this session are
  filtered out (e.g., R-4.5 binaries when running R-4.4); validation
  also catches index rows whose file content disagrees with the index (a
  known historical bug-class). Both kinds get auto-evicted, so
  [`pak::cache_clean()`](https://pak.r-lib.org/reference/cache.html) is
  rarely needed.

- Pak's `local::` is source-only:

  Confirmed empirically that pak's `pkg_install("local::<file>")`
  rejects binary tarballs (`.tgz` / `.zip` content) with "Platform
  mismatch" – even when the binary is for the current platform. The
  hybrid stage installs binaries via `install.packages(type = "binary")`
  BEFORE pak runs, so pak only sees source refs.

- Pak strict-aborts on first build failure:

  `install.packages` continues past per-package compile failures;
  [`pak::pkg_install`](https://pak.r-lib.org/reference/pkg_install.html)
  does not. The fallback to `install.packages` and the bump-and-retry
  stage together provide a best-effort completion guarantee even when
  individual refs are environment-fragile.

## Snapshot-installer options

- `Require.snapshotInstaller`:

  `"install.packages"` to use the pipeline above; `"pak"` for the legacy
  direct-pak path.

- `Require.snapshotInstallerUsePPM`:

  TRUE (default) to prepend a PPM binary repo. PPM serves Mac binaries
  by content-negotiating the `R/<version>` User-Agent.

- `Require.snapshotInstallerHybrid`:

  TRUE (default) – pre-install cache binaries via
  `install.packages(type = "binary")` before pak.

- `Require.snapshotInstallerBumpOnFail`:

  TRUE (default) – walk newer versions for refs that fail at the pin.
  FALSE for strict reproducibility.

- `Require.snapshotInstallerKnownFails`:

  character vector of pkg names to skip in bump-retry (e.g.
  environment-dependent refs whose newer versions also won't help).

- `Require.snapshotInstallerPakSilent`:

  FALSE (default) – pak's resolver output reaches the user.

- `Require.snapshotDownloadAttempts`:

  Retry count for libcurl-multi downloads. Default 4.

- `Require.snapshotDownloadChunk`:

  URLs per
  [`download.file()`](https://rdrr.io/r/utils/download.file.html) call.
  Default 50 (stays under macOS's ~256 file-descriptor ulimit).

## Examples

``` r
if (FALSE) { # Require:::.runLongExamples()
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
```
