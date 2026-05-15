# Require 2.0.0

## breaking changes

* The package dependency and package installation engine now defaults to
  `pak`. `options(Require.usePak = FALSE)` still falls back to the legacy
  non-pak code path, but `pak` is the only actively maintained installer
  going forward. This is the headline change motivating the major version
  bump. See vignette for the migration map and behavioural differences
  Require still papers over relative to a raw `pak::pak()` call.

* `pak`'s automatic system-requirements installation is now disabled by
  default. `pak` can otherwise probe for `sudo` and `apt-get install`
  missing system libraries; installing OS libraries is the user's or
  administrator's responsibility, not Require's, and the silent
  privilege escalation is unwanted in containers, CI, shared/HPC nodes
  and on CRAN. Set `PKG_SYSREQS=true` (env var) or
  `options(pkg.sysreqs = TRUE)` *before* loading Require to opt back in;
  an explicit opt-in is honoured everywhere. A regression test guards
  this behaviour.

## bug fixes

* Windows + RStudio: the SSL warning emitted by `.rs.downloadFile`
  intercepting `download.file("https://cran.r-project.org/CRAN_mirrors.csv")`
  no longer surfaces. The retry loop in `whIsOfficialCRANrepo()` already
  handled the failure gracefully; the warning was pure noise. The call
  is now wrapped in `suppressWarnings(try(..., silent = TRUE))`.

* `install = "force"` no longer (1) gratuitously upgrades transitive CRAN
  deps (e.g. broom, mgcv, sf, survival) when only top-level packages were
  requested, and (2) silently no-ops when called via `Install()` (which
  passes `require = FALSE`). Three coordinated fixes:
    - `pakDepsToPkgDT()` now pins installed user packages to their installed
      version under `install = "force"` (previously skipped). Pak resolves
      the dep tree against the constraints the user is already running
      against, so installed deps that satisfy stay put.
    - The "Deal with force installs" block in `Require()` now sets
      `needInstall = .txtInstall` directly on user-requested rows, so
      `pakInstallFiltered`'s `toInstall` filter no longer comes up empty
      when `recordLoadOrder` was skipped (the previous indirection went
      through `loadOrder`, which `Install()` doesn't populate).
    - `forceInstall = TRUE` is restored for user-requested rows under
      `install = "force"` (regressed to `FALSE` in the pak-integration
      commit `c2830a6b`). Only the non-pak `doInstalls` HEAD-ref handling
      reads this flag; the pak path is unaffected.

* `pakInstallFiltered()` now runs a pre-install integrity check: if any
  user-requested package's installed `DESCRIPTION` names a hard dep
  (Depends/Imports/LinkingTo) that is neither already installed nor in
  pak's plan, the install is skipped entirely and a warning is emitted.
  This catches the common failure mode where pak's CRAN-style resolver
  ignores `Remotes:` from a non-CRAN parent (e.g. r-universe), the
  Remote-only dep is missing, and pak would otherwise succeed from a
  cached binary -- producing a broken install whose `library()` call
  later fails with "there is no package called X".

## enhancements (originally drafted for 1.1.1)

* Major change. The package dependency and package installation engine now
  uses `pak` by default. Going forward, this will be the only maintained code.
  Require handles many cases that `pak` does not handle. See vignette.
  
* `Require()` now skips pak's online resolver entirely when every
  package it would install is already in pak's download cache at a
  version that satisfies the user's constraint. Avoids pak's metadata
  refresh (which can stall on TCP timeouts when network is slow or
  unreachable -- previously 47 s or indefinite wait when an internet-
  less Ubuntu had everything cached). Consistent with Require's
  philosophy: don't reach out for updates we never asked for.

  Implementation: a new `allInPakCache(pkgDT)` gate; when it returns
  `TRUE`, dispatch goes to `pakOfflineInstall()` instead of
  `pakInstallFiltered()` -- even when `Require.offlineMode = FALSE`.
  The shortcut is skipped when the user signals "ignore the cache":
  `install = "force"` or `purge = TRUE`.

* `pakCachedTarball()` now respects version constraints. New optional
  `versionSpec` / `inequality` arguments; cache rows whose `version`
  doesn't satisfy the inequality are filtered out, so e.g.
  `Require("dplyr (>= 2.0.0)")` no longer mistakenly uses a cached
  `dplyr 1.2.1`. `allInPakCache()` and `pakOfflineInstall()` both
  thread `pkgDT$versionSpec` / `inequality` into the lookup.

* `Require.offlineMode = TRUE` no longer fails when pak's subprocess
  probes the network at startup. pkgcache fetches
  `https://bioconductor.org/config.yaml` via `download.file()` even when
  installing `local::` source refs with `dependencies = FALSE`, which
  aborts the install when offline. Suppressed by setting
  `R_BIOC_VERSION` and `R_BIOC_CONFIG_URL` (pointing at pkgcache's
  bundled `bioc-config.yaml` fixture, located inside pak's private
  library on most systems) for the duration of the pak call, restored
  on exit. pak's startup errors are now treated as advisory: the
  ground-truth `installed.packages()` check decides whether the install
  actually landed.

* New auto-recovery when an online pak install fails because the network
  is unreachable. If `pakInstallFiltered()` leaves any row flagged
  "could not be installed", `Require()` now probes the network once (2
  seconds) and, if missing, flips `Require.offlineMode = TRUE` and
  retries the still-missing packages via `pakOfflineInstall()` against
  the local pak download cache. The happy path is unchanged -- the
  probe is paid only on the sad path. The auto-set state is cleared on
  `Require()`'s on.exit so the user's explicit setting is preserved.

* `internetExists()` and `setOfflineModeTRUE()` gained a `force` parameter
  that probes regardless of `options("Require.checkInternet")`. Default
  remains `FALSE`, so non-install code paths still respect the user's
  opt-in.

* New `Require.downloadTimeout` option (default `300L` seconds). Raises
  `options("timeout")` for the duration of GitHub source-archive downloads
  in the legacy (non-pak) install path, where R's stock 60s default can
  abort multi-MB fetches on slow connections (issue #140). Has no effect
  under `Require.usePak = TRUE`, which uses pak's own libcurl downloader
  with its own retry/timeout.

* `Require()` now accepts a multi-line string of packages -- newlines split
  into one package per line, whitespace is trimmed, and blank or
  `#`-prefixed lines are dropped (issue #147). An unquoted `{...}` block
  form is also accepted, e.g.
  `Require({ dplyr; lme4; PredictiveEcology/LandR@development })`;
  comments inside `{...}` are stripped by R's parser before this function
  runs, and version constraints like `pkg (>= 1.0)` don't parse in that
  form -- use the quoted/multi-line-string form for those.

* `pkgDepTopoSort()` first argument renamed from `pkgs` to `packages` for
  consistency with `Require()`, `Install()`, and `pkgDep()`.
  
## Function consolidation for cached packages

* `cachePkgDir()` now points at the cache that actually holds package
  tarballs. Under `Require.usePak = TRUE` it returns
  `pak::cache_summary()$cachepath` (pak owns the binary/source cache
  via pkgcache, redirectable through the standard `R_USER_CACHE_DIR`
  env var); under `usePak = FALSE` it returns the legacy
  `<cacheDir>/packages/<Rver>`. Previously it always returned the
  legacy path regardless of installer, so users (and Require itself)
  were inspecting the wrong directory in pak mode. Closes the spirit
  of issue #91 -- env-var-driven shared cache works, and the R-version
  subdirectory is appended automatically by pak / `tools::R_user_dir()`.

* `cacheClearPackages()` and `cachePurge(packages = TRUE)` now route
  through pak's cache management in pak mode --
  `pak::cache_clean()` when no `packages` argument is given,
  `pak::cache_delete(package = <names>)` when it is. Under
  `usePak = FALSE` the legacy walk-and-unlink behaviour is unchanged.
  Previously these helpers only cleared Require's bookkeeping
  directory while pak's tarball cache silently kept growing. Note:
  the `Rversion` argument is a no-op under pak (pak's cache isn't
  partitioned by R version the way Require's was).

* New internal `.requirePkgInfoDir()` for Require's own bookkeeping
  (SHA DB, mirrors, `pkgDepDB`, `available.packages` cache,
  `DESCRIPTION` snapshots). Stays at the legacy
  `<cacheDir>/packages/<Rver>` path regardless of pak mode -- those
  files belong to Require and pak doesn't index them, so keeping
  them out of pak's tarball cache prevents accidental deletion by
  `pak::cache_clean()` and silent file invisibility. All internal
  callers that previously used `cachePkgDir()` for bookkeeping were
  migrated.

## deprecations

* The following names emit a `.Deprecated()` warning and forward to
  their canonical replacement for one release cycle:
  - `cacheGetOptionCachePkgDir()` -> `cachePkgDir()`
  - `purgeCache()`                -> `cachePurge()`
  - `clearRequirePackageCache()`  -> `cacheClearPackages()`

* `options("Require.cachePkgDir")` and the `R_REQUIRE_PKG_CACHE`
  environment variable are deprecated. Under `usePak = TRUE` they no
  longer influence the cache location (pak owns it). At package load
  a one-time `packageStartupMessage()` points users at the standard
  replacement: set `R_USER_CACHE_DIR` in `.Renviron` to share or
  relocate the cache. `tools::R_user_dir()` automatically routes both
  pak's cache (`~/.cache/R/pkgcache/pkg`) and Require's scratch dir
  (`~/.cache/R/Require`) through that single env var, so a shared-
  cache setup needs just one line.

* `?cachePkgDir` (and the related `?cachePurge`, `?cacheClearPackages`,
  `?cacheGetOptionCachePkgDir` topics) was rewritten to lead with a
  "What goes where" table covering both `usePak` modes, an explicit
  deprecation/migration table for every name above, and a description
  of how `cachePkgDir()`'s return value changes with `usePak`.

## bug fixes

* Snapshot installs no longer install the wrong (latest-CRAN) version of
  a pinned package. With `Require::Require(packageVersionFile = ...)`
  the snapshot rows carry `(==X)` exact pins, but the cache-shortcut
  path (.9048) was constructing the pak ref via
  `trimVersionNumber()`, which strips both parenthetical specs AND
  pak's `pkg@ver` form when `Require.usePak = TRUE`. So
  `fpCompare (==0.2.2)` became a bare `fpCompare` ref and pak
  installed the latest CRAN version (0.2.4) instead of the snapshot
  pin. `pakCachedTarball()` now returns the cached row's `version`,
  and `pakOfflineInstall()` constructs `pkg@<cachedVersion>` for
  source-tarball refs (`.tar.gz`) so pak resolves to exactly the
  cached version. Binary `.zip` / `.tgz` refs unchanged (still
  `local::<file>`), and GitHub `account/repo@SHA` refs unchanged.

* `allInPakCache()` now refuses the cache shortcut when any requested
  ref carries the `(HEAD)` pin (`account/repo@branch (HEAD)`). `(HEAD)`
  means "the current tip of the branch", which can only be resolved
  online -- a cached tarball provides no information about whether it
  represents the current tip. With this guard, `Require()` correctly
  forwards HEAD-pinned refs to pak's online resolver instead of
  short-circuiting to a stale cached build.

* Removed the `pakResetSubprocess()` call from the top of
  `pakOfflineInstall()`. On Windows the kill-and-wait race against
  pak's auto-respawn broke the very next `pak::cache_list()` call, so
  immediately after the cache shortcut reported "all requested packages
  are in the pak download cache", `pakCachedTarball()` returned NULL
  for every package with "no rows in pak::cache_list()". Visible thanks
  to the diagnostic logging added in .9047. The reset was intended for
  the recovery-after-failure path but is unnecessary on the cache-
  shortcut path (no wedged subprocess to recover from), and on Windows
  it was actively destructive. Leaving the subprocess alone in both
  cases.

* Recovery from a failed online install (no internet) now works on
  Windows. `Require::Install("dplyr", ...)` with `offlineMode = FALSE`
  and no internet fails inside `pakInstallFiltered`, the recovery hook
  flips `offlineMode = TRUE`, and `pakOfflineInstall` retries from the
  pak cache. Previously the retry reported "not in pak cache" for
  every package even though `pak::cache_list()` showed them present.
  Root cause: `pak::cache_list()` is executed in pak's persistent
  background subprocess, which can be in a wedged state from the
  preceding failed install plan, returning stale or empty rows.
  `pakOfflineInstall()` now calls `pakResetSubprocess()` at the top
  so the cache lookup runs against a fresh subprocess.

* `pakOfflineInstall()` now logs why a package is reported as "not in
  pak cache" at default verbose: how many `cache_list()` rows it found
  for that package and whether their on-disk files exist. Surfaces
  the underlying state instead of just "not in cache".

* Offline install on Windows no longer re-downloads cached binaries.
  The earlier bare-ref pak path
  (`pak::pak("dplyr", ...)` + `PKG_METADATA_UPDATE_AFTER`) worked on
  Linux but on Windows pak's resolver picks CRAN's multi-arch URL
  (`i386+x86_64-w64-mingw32`) as canonical, missed the PPM single-arch
  cached binary (`x86_64-w64-mingw32`), and went online. The fix is
  per-extension routing in `pakOfflineInstall()`:

  - `.zip` (Windows binary) and `.tgz` (Mac binary) are passed to pak
    as `local::<file>` refs. pak treats these as direct binary
    installs -- no resolver, no cache-key matching, no download.
  - `.tar.gz` keeps the bare-ref pak path (where `local::` would
    trigger `R CMD build` and rebuild vignettes -- the original
    reason `local::` couldn't be used for source-format files
    offline). With the env-var hooks (`PKG_METADATA_UPDATE_AFTER`,
    `R_BIOC_VERSION`, `R_BIOC_CONFIG_URL`), the bare-ref path uses
    pak's own cache without going online on Linux.

  pak stays in charge throughout -- its resolver, dep-ordering,
  sysreqs, build, and progress UI all apply. Only the ref form
  changes based on file extension.

* Reverted to using `pak::pak()` for the offline install (vs. the
  `install.packages(<file>, repos = NULL)` approach in .9044). The
  install.packages route installed each tarball standalone with no dep
  ordering, so a multi-package install where A depends on B in the
  same batch failed with "dependency 'B' is not available for package
  'A'". Trying to topologically sort and install one-at-a-time
  reproduces logic that pak already does; per user direction, keep pak
  in charge. The env-var hooks
  (`PKG_METADATA_UPDATE_AFTER`, `R_BIOC_VERSION`, `R_BIOC_CONFIG_URL`)
  and `pak.no_extra_messages` are back. The `trimVersionNumber()`
  fix that strips Require-internal `pkg (>= X)` constraints before
  pak sees them is preserved.

* Offline install no longer fails silently with "tarball was in pak
  cache but offline install failed" for every package when the dep
  tree contains version constraints. `pakOfflineInstall()` was passing
  `packageFullName` (e.g. `glue (>= 1.3.2)`) verbatim to `pak::pak()`,
  which rejects parenthetical inequality constraints with
  `Cannot parse package: glue (>= 1.3.2)`. The error was buried at
  `verboseLevel = 2`, so at default verbose the user only saw the
  generic "offline install failed" warning with no diagnostic. Now
  `trimVersionNumber()` strips the parenthetical before pak sees the
  ref (preserving GitHub `account/repo@ref` forms), and any pak error
  is surfaced at default verbose so failures are debuggable.

* `useLoadedIfSufficient()` now verifies the package's `DESCRIPTION`
  exists on disk in an effective lib path before marking the row as
  satisfied. In a single R session, `remove.packages()` deletes files
  from disk but leaves the namespace in `loadedNamespaces()`, and
  `system.file(package = ...)` continues to return the recorded
  (now-nonexistent) path. The previous logic (loaded + libPath-in-
  effective) treated such packages as already installed; offline
  `Require(pkg)` then skipped reinstall and downstream
  `installed.packages()` walks emitted `cannot open compressed file
  '.../DESCRIPTION'` warnings. Adding the disk-presence check makes
  Require correctly route those rows back through the install pipeline
  so the package ends up on disk again, consistent with Require's
  "after this call, the packages are installed" contract.

* Online pak install no longer spins forever when a package fails to
  build because of missing system packages. The identify-and-defer
  loop's dep resolver re-includes the failing package in every retry
  plan (since dependents still reference it), so the loop ping-pongs on
  the same culprit indefinitely. New `extractMissingSysreqs()` parses
  pak's "Missing N system packages" block and the `+ <sysreq> - <pkg>`
  mapping. When detected, the loop bails out with an actionable warning
  naming each affected package and its missing system dependencies
  (e.g. "fs needs: cmake, libuv1-dev"). Packages that already installed
  before the failure are preserved.

* Offline install under `Require.usePak = TRUE + Require.offlineMode = TRUE`
  now uses pak's normal install flow against its existing cache instead
  of forcing a `local::<tarball>` source install. The previous design
  fed pak `local::` refs which forced its source-install pipeline
  (`R CMD build` rebuilds vignettes -- needs network -- and failed offline
  with "Failed to build dplyr 1.2.1 (300ms)"). The new flow passes
  normal CRAN/GitHub refs to `pak::pak()` and uses three env-var hooks
  to keep pak's subprocess fully offline:
  - `PKG_METADATA_UPDATE_AFTER=365d` -- treat pak's cached metadata as
    fresh so pak doesn't refresh from CRAN/PPM.
  - `R_BIOC_VERSION` -- short-circuit pkgcache's Bioconductor version
    probe.
  - `R_BIOC_CONFIG_URL=file://...` -- redirect any residual yaml fetch
    to pkgcache's bundled fixture.
  Plus `options(pak.no_extra_messages = TRUE)` to silence the pillar
  hint. All four are saved + restored on exit.

  With the cached metadata and a cached binary (or source) for the
  package, pak installs without recompilation -- e.g. dplyr in ~64ms
  on a warm cache instead of failing.

* Offline install on Linux no longer fails to recognise PPM (binary)
  tarballs in pak's cache. PPM binaries share the bare `pkg_ver.tar.gz`
  filename with their source counterparts on Linux; only
  `pak::cache_list()`'s `platform` column distinguishes them. The old
  filename-only classification misrouted PPM binaries into pak's
  source-install branch, which then tried to `R CMD build` them
  (rebuilding vignettes that need network) and aborted with
  "Failed to build dplyr 1.2.1 (300ms)" or similar. `pakCachedTarball()`
  now returns an `is_binary` flag derived from the `platform` column,
  and `pakOfflineInstall()` routes accordingly.

* The Linux binary path now picks `type` from `.Platform$pkgType` so
  `install.packages(..., type = "binary")` -- which errors on Linux --
  is not used. Linux gets `type = "source"`; Mac/Windows still get
  `type = "binary"`.

* `pakOfflineInstall()` now emits two distinct warnings when something
  is missing on disk after the install: "not in pak cache" for packages
  whose tarball wasn't cached, and "tarball was in pak cache but
  offline install failed" for packages that had a tarball but failed to
  install. The old single hard-coded "not in pak cache" message was
  actively misleading when the latter happened.

* `Require::Install()` with `==` / `<=` version pins now actually installs
  the requested version. Five interacting bugs in the pak install path
  caused `Install(c("stringfish (<= 0.15.8)", "qs (== 0.27.3)"))` to
  silently install stringfish 0.19.0 (ignoring the upper bound) and
  report qs as `[still-missing]` in the install summary even after the
  archive-fallback pass had successfully installed it. Fixes:

  * **@-version ref normalization.** New `pakRefToBareName()` helper
    (`R/pak.R`) reduces any pak ref to the bare package name that
    `installed.packages()` returns. `extractPkgName()` only strips
    parenthetical `(>=X)` version specs — it does NOT strip pak's
    `pkg@X` exact-pin form that `equalsToAt()` / `lessThanToAt()`
    introduce. Consequence pre-fix: `qs@0.27.3` survived through
    `pkgNamesAll` and `passNames`, never matched
    `installed.packages()`'s bare `"qs"`, and every version-pinned
    install looked "still missing" to the iter-loop / archive-fallback /
    install-summary checks — even right after a successful install.

  * **Cache key now respects user-supplied version constraints.**
    `pakDepsCacheKey()` previously hashed only the version-stripped
    `pkgsForPak`, so two calls differing only in constraints shared a
    cache entry. The cached `pak_result` was reused by downstream
    `pakDepsToPkgDT` processing whose behavior DOES branch on the
    user-supplied constraints (`trimRedundancies` + `lessThanToAt`
    rely on constraint rows actually being present in `pkgDT`); a stale
    entry from a different constraint set silently corrupted the next
    install plan. Fix: thread a `userPkgs` parameter through
    `pakDepsCacheKey` / `pakDepsResolve` / `pakDepsCacheInvalidate`,
    pass `resolvedPkgs` (constraint-bearing form) at the call site.

  * **`pakInstallFiltered` dedup keeps the strictest constraint row.**
    When pkgDT had two rows for the same Package (e.g. user's
    `(<= 0.15.8)` upper bound and a transitive dep's `(>= 0.15.1)`
    lower bound, both correctly kept by `trimRedundancies` because
    they're complementary, not redundant), `unique(by = "Package")`
    arbitrarily kept whichever sorted first — typically the `>=` row
    from the dep tree. The user's `<=` pin was then dropped, the
    downstream `gsub("\\(>=...\\)")` stripped to bare name, the `any::`
    prefix made it `any::stringfish`, and pak silently installed the
    latest. Fix: sort by inequality priority
    (`==` > `<=` > `<` > `>=` > `>` > none) before unique-by-Package
    so the strictest row wins. `equalsToAt()` / `lessThanToAt()` then
    translate the surviving `==` / `<=` / `<` row into pak's exact
    `@version` pin form.

  * **No more empty `Warning message: could not be installed:`.**
    `pakGetArchive()` was being called by `pakErrorHandling` with an
    empty `pkgNoVersion` when pak emitted an internal error that
    didn't match any known parse pattern (e.g.
    `if (!version_satisfies(...))`). The downstream warning then fired
    with no package name and no reason. Fix: early-return at
    `pakGetArchive` entry when `pkg2` is empty; `nzchar()` guard at
    the warn site as belt-and-braces.

  * **Mid-pipeline retry warnings demoted to debug messages.**
    `pakRetryLoop` and `pakSerialInstall` were emitting
    `warning(... immediate. = TRUE)` for every transient install
    failure — but those layers are early stages of a multi-layer retry
    pipeline (parallel batch → identify-and-defer → serial →
    CRAN-archive fallback) and the failure is routinely repaired by a
    downstream layer. Users were told inline
    `Warning: could not be installed: qs@0.27.3` then watched qs
    install successfully via the archive pass two seconds later.
    Those emissions are now `messageVerbose(... verboseLevel = 2)`
    prefixed with the source layer (`pakRetryLoop:` /
    `pakSerialInstall:`) for diagnostics. The post-install
    `silentlyFailed` warning remains the authoritative end-state
    report — it inspects the actual lib state and only fires for
    packages that did NOT make it in by the end.

* Install summary's canonical `installFailures` parse now runs AFTER
  the archive-fallback pass so per-package `Failed to build X` lines
  emitted during the archive pass are picked up rather than falling
  through to the catch-all `still-missing` branch. Rows are also
  filtered by `finalMissing`, so packages that failed in iter 1 but
  succeeded in a deferred-culprit serial pass don't leak into the
  summary as build-errors when in fact they ended up installed.

# Require 1.1.0.9029 (development version)

## bug fixes

* identify-and-defer iter check now strips pak's `any::` CRAN prefix
  (and `owner/` GitHub prefix) from `passNames` before comparing with
  `installed.packages()`. Without this, `extractPkgName("any::cli")`
  returns `"any::cli"` while `installed.packages()` returns `"cli"`, so
  every successfully-installed CRAN ref in the iter pass-list looked
  "still missing" — sending the loop into the no-parseable-culprits
  serial-install fallback every single time, even on a clean
  `Require::Install(devtools)` with all CRAN deps. Symptom: a 3-minute
  parallel install followed by another 3 minutes of pointless serial
  pak calls that all report "kept N". Same transformation as the
  `pkgNamesAll` computation in the final-missing check above; the iter
  check just forgot to apply it. The 1.1.0.9027 `noCache = TRUE` fix
  was real but secondary — the cache wasn't the problem; the prefix
  mismatch was.

# Require 1.1.0.9028 (development version)

## bug fixes

* `pakBuildFailReason()` now actually surfaces pak's real failure cause.
  Two issues in 1.1.0.9025: (a) the filter did not strip pak's own
  wrapper line `Error : ! error in pak subprocess` or the `Caused by
  error:` chain delimiter, so when pak's `try()`-string already chained
  to the real reason, the fallback returned the wrapper line and the
  cause was never seen; (b) the diagnostic regex did not include
  `Could not solve package dependencies` or `Can't find package
  called`, two of pak's most common cause-line patterns. Both fixed.
  The bullet `! ` prefix that pak adds is now stripped from the
  fallback line so the warning reads cleanly.

* `pakRetryLoop()` no longer fires the duplicate "could not be installed:"
  warning. The `alreadyWarned <<- TRUE` super-assignment in
  `pakRetryLoop`'s own body walked past the local declaration to
  `pakInstallFiltered`'s enclosing scope (where no such variable
  exists), leaving the local `FALSE` and triggering the post-loop
  fallback warning every time. Changed to `alreadyWarned <-` so the
  local actually gets set. (`warnedDropped` legitimately uses `<<-`
  because it really is in the enclosing scope — only `alreadyWarned`
  was wrong.) This was a pre-existing bug that 1.1.0.9025 reproduced
  in the new `identical(packages, pkgsIn)` branch.

# Require 1.1.0.9027 (development version)

## bug fixes

* Post-install `installed.packages()` checks now pass `noCache = TRUE`.
  pak runs each install in a subprocess; the parent R session's
  `installed.packages()` cache is not invalidated when the subprocess
  writes to the lib. Without this, freshly-installed packages looked
  "still missing" to the strategy loop in `pakInstallFiltered`, falling
  into the "no parseable culprits; falling back to serial install"
  branch and re-running pak unnecessarily — visible as e.g. a simple
  `Require::Install(pkgload)` taking ~12s instead of ~3s, with bogus
  "still missing after iter 1" messages.

# Require 1.1.0.9026 (development version)

## new features

* `Require()` now skips reinstall when a package is already loaded in the
  current R session with a version that satisfies the requested
  constraint. Previously, even when the loaded version was sufficient,
  Require would still ask pak (or `install.packages()`) to install/upgrade
  the package — which fails when the loaded namespace is imported by
  another loaded package (e.g. `reproducible` <- `climateData`),
  surfacing as the generic "Error : ! error in pak subprocess".  The new
  `useLoadedIfSufficient()` helper runs after `whichToInstall()` and, for
  any candidate flagged for install, checks `getNamespaceVersion()` and
  `compareVersion2()` against the row's `versionSpec`/`inequality`. When
  the loaded version satisfies, the row is marked `installed = TRUE`,
  `installedVersionOK = TRUE`, `needInstall = .txtDontInstall`, plus a
  new `loadedSufficient = TRUE` flag. `doLoads()` consults the flag and
  attaches via `require(x, character.only = TRUE)` (no `lib.loc`) to
  avoid R's "cannot be unloaded because <X> is imported by <Y>" error
  path. Honoured for HEAD-checked GitHub refs too — version pin trumps
  HEAD when the user's spec is a `(>= ...)` constraint. Skipped when
  `install = "force"`, since that explicitly asks for reinstall.

# Require 1.1.0.9025 (development version)

## bug fixes

* pak install warnings now surface the actual subprocess failure reason
  instead of the generic "Error : ! error in pak subprocess" wrapper.
  `pakBuildFailReason()` now also accepts the captured pak-subprocess
  message stream and `pakRetryLoop()` / `pakSerialInstall()` slice and
  pass it through, so warnings include the real cause — e.g. "namespace
  'reproducible' is imported by 'climateData' so cannot be unloaded".
  The reason-extractor's diagnostic regex was extended to recognise
  unload-blocked-by-import and locked-package patterns. Also fixed a
  duplicate-warning bug: the `identical(packages, pkgsIn)` branch in
  `pakRetryLoop` warned without setting `alreadyWarned`, so the
  post-loop `!alreadyWarned` block fired a second, less-informative
  warning with no package names.

# Require 1.1.0.9024 (development version)

## bug fixes

* `Require()` now recovers from R's "cannot be unloaded because <pkg> is
  imported by <others>" failure. Previously, when `require(x, lib.loc =
  libPaths)` failed for this reason — typical when a package (e.g.
  `reproducible`, `Rcpp`, `dplyr`) is already loaded from a different lib
  and its dependents (`SpaDES.core`, `LandR`, `terra`, ...) have imported
  it — Require warned "package will not be attached" and left `x` off the
  search path. Modules calling unqualified functions from `x` (e.g.
  `prepInputs(...)` inside a SpaDES `init` event) then failed with
  "object 'prepInputs' not found". The recovery detects the situation via
  `loadedNamespaces()` (the failed-unload kept the namespace loaded) and
  retries `require(x, character.only = TRUE)` *without* `lib.loc`, which
  attaches the already-loaded namespace to `search()`. R prints the
  "Failed with error: ... cannot be unloaded" text directly to stderr
  rather than as a condition, so a `withCallingHandlers(warning=...)`
  capture would not have seen it.

# Require 1.1.0.9023 (development version)

## bug fixes

* `pakGetArchive()` now returns the input `packages` unchanged when
  `options(repos)` has no concrete CRAN URL (e.g. only an r-universe is
  configured, or only `@CRAN@` placeholder). Previously,
  `paste0("url::", character(0))` collapsed to a length-1 `"url::"`
  string; downstream `pak::pak("url::")` then aborted the whole archive
  batch with an opaque "All URLs failed". The archive-fallback call
  site additionally rejects any ref that is not a fully-formed
  `url::https?://...` URL.

# Require 1.1.0.9022 (development version)

## bug fixes

* Archive fallback now passes all archive URLs to pak in a single batch
  call so cross-archive dependencies resolve correctly. Previously, the
  fallback installed each archive ref serially: this worked for
  archived packages whose deps were on current CRAN, but failed for
  cross-archive cases like `disk.frame` (which depends on `pryr`,
  itself archived) — pak would emit "Can't find package called pryr"
  because the pryr archive URL wasn't in the same install plan.
  Verified end-to-end on the (disk.frame, pryr) pair: 2 pkgs + 54
  transitive deps install in a single ~30s pak call. If the batch call
  fails for any reason, falls back to per-ref serial install (which
  recovers archives without cross-archive deps).

# Require 1.1.0.9021 (development version)

## new features

* `pakInstallFiltered()` now runs an *archive fallback* pass at the end of
  install. For any still-missing packages whose failure pak did not
  attribute (i.e. no per-package `Failed to build` line — typical of
  archived-from-CRAN refs that the current CRAN mirror can't resolve),
  Require constructs a `url::https://.../Archive/<pkg>/<pkg>_<ver>.tar.gz`
  ref via the existing `pakGetArchive()` helper and attempts a serial
  install of each. Confirmed working for archived CRAN packages such as
  `pryr` that pak wouldn't resolve via `any::pryr`. Packages that still
  fail (e.g. genuine source-build issues, transitive deps no longer
  available) remain in the install-failure summary.

# Require 1.1.0.9020 (development version)

## new features

* `pakInstallFiltered()` now emits an end-of-install summary listing each
  package that did not end up in the project library, with a parsed
  reason where pak's output was specific enough to attribute one. The
  reason is one of:
    - `missing-build-deps` — R CMD INSTALL pre-flight check refused to
      build the package because some `Imports` were not yet in the
      library at build time (typical cascade culprit). Brief includes
      the dep names parsed from pak's `ERROR: dependencies '...' are
      not available for package '...'` line.
    - `compile-error` — gcc/Fortran error during source build.
    - `version-conflict` — pak refused with an unsatisfiable
      version pin in the dep tree.
    - `build-error` — generic "Failed to build" with no parseable
      ERROR: line.
    - `still-missing` — package wasn't in `.libPaths()` at the end of
      all install passes, but pak emitted no specific failure for it
      (typical cascade casualty when pak's subprocess crashed during
      dep resolution).
  The full structured table is also stored in
  `pakEnv()$.lastInstallFailures` for programmatic access.
* New helpers `extractInstallFailures()` and `reportInstallFailures()`
  expose the parser and reporter independently of the install loop.

## bug fixes

* `pakInstallFiltered()` post-install loop: the lazy initialisation of
  `nowInstalledAll` used `<<-` rather than `<-`, so the assignment leaked
  into the global environment instead of updating the local variable
  declared earlier in the function. Subsequent `nowInstalledAll[Package
  == pkg]` then errored with "object 'Package' not found" when the
  package wasn't in `libPaths[1]` (the common case after a partial
  install with cascade casualties). Fixed by switching to `<-`.

## new features

* `pakInstallFiltered()` gains a fallback **serial install** path: when
  the iterative identify-and-defer loop has packages still missing but
  no further build-failure culprits are parseable from pak's output —
  typically because pak's subprocess crashed during dep resolution on a
  large casualty batch — Require now invokes `pakSerialInstall()` on the
  remaining missing refs. Each per-ref pak call has a tiny dep graph
  pak resolves cleanly, and a single ref's failure no longer aborts the
  rest. Slow but reliable; usually the only step that gets full LandR-
  scale workflows installable end-to-end.
* New helper `pakResetSubprocess()` force-restarts pak's persistent
  callr `r_session` (the one held in `pak:::pkg_data$remote`). Called
  between identify-and-defer iterations and before the deferred-culprit
  serial install, so each phase starts with a clean pak subprocess.
  Necessary because pak can wedge after a large failed install plan in
  a way that makes every subsequent call emit "Error : ! error in pak
  subprocess" without naming a build culprit.

# Require 1.1.0.9018 (development version)

## new features

* `pakInstallFiltered()` gains an iterative *identify-and-defer* install
  strategy (now the default) that handles pak's cascade-abort behaviour on
  large transitive dep graphs. When pak emits per-package `Failed to build
  <pkg>` lines, those packages are treated as the authoritative culprits;
  the rest of the unbuilt packages — cascade casualties from pak aborting
  the install plan — get a clean parallel retry without the culprits in
  the batch. Culprits are then installed one-by-one at the end via the
  new `pakSerialInstall()`, when their build-time deps are present in the
  project lib so R CMD INSTALL's pre-flight check passes.
* New helper `extractBuildFailures(output)` parses pak's stderr/messages
  for `Failed to build <pkg>` lines.
* New helper `pakSerialInstall(pkgs, lib, repos, verbose)` installs refs
  one at a time; used by the deferred phase of identify-and-defer.
* Strategy is selectable via `options(Require.pakInstallStrategy = ...)`:
  - `"identify-and-defer"` (default)
  - `"original"` (legacy single-pass behaviour)
* Per-call install timing is recorded in `pakEnv()$.lastPakInstallTimings`.

## bug fixes

* `pakInstallFiltered()` post-install loop: `nowInstalledAll` now gets the
  same empty-matrix guard as `nowInstalled` (it could previously error
  "object 'Package' not found" when `installed.packages(.libPaths())`
  returned a matrix without expected columns, masking the upstream
  install failure).

# Require 1.1.0.9017 (development version)

## bug fixes

* `pakErrorHandling()` no longer crashes when `pak`'s error output contains
  characters that, when spliced into a regex, form an invalid pattern (e.g.
  TRE "Unknown collating element" from stray brackets, or dots in package
  names like `paws.application.integration`). Symptoms were a misleading
  warning `could not be installed: invalid regular expression '...'`,
  followed by `Error: object 'Package' not found` from
  `pakInstallFiltered()`, with the real `pak` build-failure reason
  silently swallowed. Three fixes:
  * New `regexEscape()` helper escapes regex metacharacters in
    `pkgNoVersion` / `vers` before splicing them into a `paste0()` pattern;
    the surrounding `grep` is also wrapped in `tryCatch` so a still-malformed
    pattern returns `integer(0)` rather than aborting.
  * When `pakErrorHandling()` itself errors, the surrounding `tryCatch` in
    `pakRetryLoop()` now also reports `pakBuildFailReason()` of the original
    `pak` error and `message()`s the full raw `pak` error (truncated at
    8 kB) so the underlying build-failure cause is no longer hidden.
  * `pakInstallFiltered()`'s post-install loop guards against
    `installed.packages()` returning an empty matrix without the expected
    columns, which previously surfaced as `object 'Package' not found` and
    masked the real build failure.

  These fixes were already merged in the `dependencies=NA` commit
  (1.1.0.9016) but were not separately documented; this entry records
  them retroactively.

# Require 1.1.0.9016 (development version)

## bug fixes

* CRAN-like packages installed via `pakInstall()` now use
  `dependencies = NA` (was `FALSE`). With `dependencies = FALSE`, pak
  parallelises source builds without waiting for build-time hard deps
  to finish — e.g. `htmlwidgets` would start building while `htmltools`
  was still mid-install and fail with "dependencies are not available".
  `dependencies = NA` lets pak topologically order builds by the
  hard-dep graph. Combined with `upgrade = FALSE`, this still avoids
  upgrading already-installed packages beyond what Require requested.

# Require 1.1.0.9015 (development version)

## dependencies

* `pak` is now an `Imports` (was `Suggests`). The `usePak` branch requires `pak`
  for all GitHub/url-style installs, and isolated project libraries (e.g., those
  created by `SpaDES.project::setupProject()`) do not always inherit the user's
  default library where `pak` might be installed. Declaring `pak` as a hard
  dependency ensures it is present wherever Require is.

# Require 1.1.0.9013 (development version)

## bug fixes

* GitHub and `url::` packages are now installed with `upgrade = TRUE`,
  `dependencies = FALSE` so pak always fetches the latest commit from the
  requested branch without upgrading transitive CRAN dependencies. Previously,
  `upgrade = FALSE` caused pak to "keep" any already-installed version of a
  GitHub package even when a newer version was required, because pak treats a
  bare `owner/repo@branch` ref as satisfied by whatever version is already in
  the library. CRAN-like packages are still installed with `upgrade = FALSE`,
  `dependencies = FALSE` to avoid unnecessary upgrades of already-satisfied
  dependencies.

# Require 1.1.0.9011 (development version)

## bug fixes

* `pak::pak()` is now called with `dependencies = NA` (pak's default) instead of
  `dependencies = FALSE`. Previously, `dependencies = FALSE` caused installation
  failures for GitHub dev packages whose latest DESCRIPTION had new or updated dep
  requirements that were not captured in Require's earlier dep-tree snapshot. Using
  `dependencies = NA` lets pak satisfy any such requirements automatically, matching
  the behaviour of a direct `pak::pak()` call.
* "Please change required version" is no longer emitted spuriously when pak fails to
  install a package that was not previously present in the library (first-time install
  failure). Previously, a `NA` pre-install version was compared with the post-attempt
  installed version, incorrectly signalling that pak had installed a different version.

# Require 1.1.0.9010 (development version)

## bug fixes

* When pak fails to install a package with an error that Require does not
  recognise as retryable (e.g. a subprocess crash, network timeout, or GitHub
  API error), the install attempt now stops immediately and the actual pak error
  reason is included in the `"could not be installed"` warning.  Previously the
  retry loop would silently repeat the same failed call 15 times and then emit
  a bare `"could not be installed: <pkg>"` with no explanation.

# Require 1.1.0.9009 (development version)

## bug fixes

* When pak fails to install a newer version of a package but an older version is already
  installed, Require now loads the installed version as a fallback (with a warning) instead
  of refusing to load at all. Previously this produced confusing downstream errors (e.g.
  "object 'sppEquivalencies_CA' not found") because the package was silently not attached,
  even though a usable version was present in the library.

# Require 1.1.0.9008 (development version)

## bug fixes

* `require()` failures are now always visible regardless of `Require.verbose` setting.
  Previously, when `Require.verbose <= 0`, a package that failed to attach (e.g. a
  missing dependency, wrong library path) was silently ignored, producing confusing
  downstream errors like "object 'sppEquivalencies_CA' not found". Now a `warning()`
  with `immediate. = TRUE` is always emitted when `require()` returns FALSE, including
  the underlying R message and the library paths that were searched.

# Require 1.1.0.9007 (development version)

## Enhancements
* `pak` is now the default dependency-resolver and install backend
  (`options(Require.usePak = TRUE)` is set by default). `pak::pkg_deps()` replaces
  Require's internal `pkgDep()` pipeline for full transitive dependency resolution,
  while Require's version-priority logic (`whichToInstall`, `trimRedundancies`,
  `confirmEqualsDontViolateInequalitiesThenTrim`) still governs which packages
  actually get installed. Archived CRAN packages, GitHub references, and
  CRAN/GitHub conflicts are all handled via retry loops in `pakDepsToPkgDT()` and
  `pakInstallFiltered()`.
* When pak fails to build or install a package, the warning now includes the
  actual reason (e.g., namespace version mismatch, file locked on Windows,
  compilation failure) rather than a bare "could not be installed" message.
* Misleading "Please change required version" warnings are now suppressed when a
  package build fails and the installed version is unchanged; the warning is only
  shown when pak successfully installed a different (but still insufficient) version.
* When pak detects a CRAN/GitHub conflict caused by a `Remotes:` entry in another
  package's `DESCRIPTION` (e.g., `sp` vs `sp` via `SpaDES.core` Remotes), the
  conflict table now clearly shows both sides:
  `sp (CRAN)  vs  sp (via PredictiveEcology/SpaDES.core@development Remotes)`.
  Previously this displayed the misleading `sp  vs  PredictiveEcology/SpaDES.core@development`.
* The pak dependency-tree cache (in-memory and disk) now reports cache hits at the
  default `verbose = 1` level, making it visible that subsequent `Require()` calls
  are served from cache rather than querying pak/CRAN again.
* When a non-pak install log contains a namespace version error
  (`namespace 'X' Y is being loaded, but >= Z is required`), Require now
  automatically installs the required version of `X` and retries, rather than
  failing silently.

## Bugfixes
* When a GitHub package fails to build (e.g. `map` compilation error) and is
  permanently removed from the pak retry list, Require now emits a warning
  naming the package and, where extractable, the reason (namespace mismatch,
  compilation failure, etc.). Previously the failure was silent when other
  packages succeeded. Cascade failures — packages that depend on the failed
  package and therefore also fail to install — are similarly reported after
  the update loop.
* Fixed `require()` not being called for packages (e.g. `LandR`) when using
  `Require.usePak = TRUE`. The root cause: `pakDepsToPkgDT()` step-3b compared
  pak's CRAN-resolved version against the user's version constraint. When the
  user had a dev version installed (satisfying the constraint) but pak's CRAN
  resolution returned an older version, the package was incorrectly removed from
  `pkgDT`. Because `recordLoadOrder()` could not find the package in `pkgDT`,
  `base::require()` was never called. The fix checks the actually-installed version
  before classifying a package as unsatisfiable.
* Fixed a second `require()` failure mode: a user-requested package (e.g.
  `LandR`) could end up completely absent from `pkgDT` if step-3b removed it
  from the local package list AND it was not a transitive dependency of any
  other requested package. In this case `recordLoadOrder()` had no row to
  match, so `loadOrder` was never set and `base::require()` was never called.
  The fix adds a recovery pass after the main pipeline: any user-requested
  package that is absent from `pkgDT` but installed at a satisfying version
  is rbind-ed back with `loadOrder` set and `installedVersionOK = TRUE`.
  Also adds verbose ≥ 1 diagnostics in `doLoads()` to report when packages
  with `loadOrder` set are skipped (and why) or when `base::require()` itself
  returns `FALSE`.
* Fixed `file:////` URL error when downloading archived packages that were
  previously cached locally; `basename()` is now used for `file://` repository
  URLs to match the flat cache layout.

## Enhancements
* When packages are not found on CRAN, the message now shows the full recursive
  dependency chain explaining why they are needed, e.g.:
  `fastdigest (required by: digest -> reproducible) not on CRAN; checking CRAN archives`

# Require 1.1.0

## Breaking changes
* Package cache now uses per-repository subdirectories (e.g., `cloudr-projectorg/`) instead of a flat directory. This prevents cross-repository cache contamination (e.g., an r-universe package being used when only CRAN is specified). Old flat-cache files will be ignored and packages re-downloaded as needed. A `removeOldFlatCachePkgs()` function is provided to clean up legacy flat-cache files (#143).

## New functions
* `removeOldFlatCachePkgs()`: migrates users from the pre-#143 flat package cache by removing old `.tar.gz` files from the top-level cache directory.
* `cachePkgDirForRepo()`: returns (and optionally creates) the per-repository cache subdirectory for a given repository URL.

## Enhancements
* Default for `getOption("Require.usePak")` changed from `TRUE` to `FALSE` for consistency with the documented default.
* `CODECOV_TOKEN` added to the test-coverage GitHub Actions workflow to avoid rate limiting on Codecov.
* Expanded test suite with targeted unit tests for many previously uncovered internal functions, including message helpers, cache helpers, environment accessors, and `pkgDepTopoSort`.

## Bugfixes
* fixes on MacOS that were preventing many types of packages from installing.
* several minor.
* better fails when status is 403 for package dependency checking.
* `updatePackages` had 2 minor bugs that prevented some mixtures of necessary updates from being correctly identified.
* resolved failure to install when using `(HEAD)` in some cases for packages in custom repositories
* use `R_REQUIRE_CACHE` environment variable for setting the cache directory instead of modifying `R_USER_CACHE_DIR` (#124).
* `extractVersionNumber()` no longer returns `character(0)` for empty filename inputs.
* Fixed `data.table` recycling warning in `sysInstallAndDownload`.
* `fileRenameOrMove()` now catches errors from `dirname(to)` on Windows when paths exceed MAX_PATH limits.
* Fixed `rbindlist(fill=TRUE)` column-mismatch errors on R-devel for Windows in `available.packagesCached()`.
* Broadened download failure warning pattern to handle more cases on older Windows R versions.

# Require 1.0.1

## enhancements
* `offlineMode`, gained improved functionality; though it is still experimental. It can be set using `options(Require.offlineMode = TRUE)`, but it will be automatically set if internet is not available, has now been widely tested. If packages are available in the local caches, and all elements of package versioning (e.g., `available.packages()` and github packages) have been previously run, then installations should occur as if the internet were available.

## Other
* package testing on Linux Fedora and one macOS machine on CRAN extra machines were addressed.

# Require 1.0.0

## major changes
* Installation, package downloading, and package building from source now occur in an external process using `sys` package. This allows for more control over messaging during installations, and it also allows of installation of many packages that are already loaded (with a message that the session will need restarting). This can be turned off with This is turned on with `option(Require.installPackagesSys = FALSE)`.
* All internals for `pkgDep` have been changed. The new algorithms are faster and more reliable, with far fewer lines of code.
* All testing has been converted from using `testit` to using `testthat`. This change adds many dependencies to `Suggests`, but the benefits, e.g., using `withr` to control loading and unloading of options, packages etc., outweigh the drawbacks.

## enhancements
* `packages` argument for `Require` and `Install` can now be unquoted names length == 1 or if length > 1 using `c()` or `list()`, in addition to a character string, e.g., `Install(ggplot2)`;
* Now, if a `GitHub.com` package has a field `Additional_repositories` in the DESCRIPTION file, `Require` will search there for packages that it does not find in the `repos` argument. This does not affect `CRAN` packages, as this information is not contained within the `available.packages()` data base, which is what is used to identify dependencies, rather than reading each `DESCRIPTION` file individually;
* `verbose` now propagates better through all internal functions, so e.g., `verbose = -2` will make installing very silent;
* Better automatic cleaning of Cached packages that are corrupt;
* experimental use of `pak` as the backend installer of packages instead of `install.packages`. A user can attempt to use this backend with `options(Require.usePak = TRUE)`. There are a number of cases (specifically when needing exact versions) that do not work; but for "normal" package installations it is widely tested. `pak` backend tends to be similar speed for first installations, but much slower for subsequent calls to `Install`/`Require`;
* Better recovery from installation failures e.g., if the local cached copy is corrupt, it will be automatically cleaned;
* `Require.Rmd` vignette for "Getting Started" is new;
* many speed enhancements in cases where e.g., a download is not necessary;
* when downloads from `GitHub.com` are done, `Require` now uses `gitcreds` to get `git` credentials and `httr` to download the files with the token;

## Function name changes

* all functions related to `cache` now start with `cache`, e.g., `cacheClearPackages` replaces `clearRequirePackageCache`. Previous names are kept for backwards compatibility.

## bugfixes
* If a GitHub packages was attempted to be installed, but failed because the package was already loaded in the session, `Require` would incorrectly think it had successfully installed (#87);
* Warning occurred if a package was no longer on CRAN and user had supplied multiple `repos` or `getOption('repos')`. The result was unaffected by the warning, but warning is now removed;
* allow user-specified path in `pkgSnapshot()` (#93);
* a number of new cases have been added to `tests` that previously would have hit errors;
* many other small bugs fixed;
* Some issues specific to macOS have been fixed.
* fixes or implemented other issues #91, #96, #97, #102, #105

# Require 0.3.1

## enhancements
* minor modifications for when internet is not available
* deal with more edge cases for package snapshots that are not internally consistent, i.e., violate package versions, or skip missing branches on GitHub, if not needed (#81).

## bugfixes
* updates to tests that have begun to fail

# Require 0.3.0

## enhancements
* Moved from MRAN archives for binaries to <https://packagemanager.posit.co/>
* because of the move from MRAN to posit package manager, attempts are made to use archived binary packages for Linux also.
* improved messaging in several places
* improved error catching in several places
* a number of cases that were annoying for users were identified and addressed.
* `setupOff` and `setLibPaths` enhanced to be fully functioning in a wide diversity of cases.
* When setting `install = "force"` in `Require`, now only the user-specified packages are forced to be installed; the rest are installed if required, mimicking `install.packages`
* small efficiency gains in many places
* `(HEAD)` is now more robust as a way to keep a package up to date.

## advanced changes
* several functions now exported, `.downloadFileMasterMainAuth`, `messageVerbose`, `messageDF` as they were deemed useful enough for other packages.

## bugfixes
* slow assessment of package dependencies on CRAN packages because of stale `available.packagesCached()` object. Now, catches this condition and refreshes `available.packages()`
* corrected support for multiple repos that each offer the same packages. Now works like `install.packages`, i.e., first one first.
* base packages can now be installed as previous issues about installing them were dealt with.

# Require 0.2.6

## enhancements
* attempts to deal with more cases of failed installations
* `Install` did not have an `install` argument; this has now been introduced, allowing the (most likely) use case of `Install(pkg, install = "force")`
* examples now use `Install` more often than `Require(..., require = FALSE)` for simplicity.

## improved messaging
* If non-interactive and no CRAN mirror is set, user gets more informative error.

## bugfixes
* Cases of multiple user-specified `.libPaths()` were treated incorrectly; they are now all respected. 
* when git repo was not installed because it was identical to the SHA already installed, it would not be loaded, thinking it failed to install; fixed
* can now deal with case when `repos` has multiple, non-binary CRAN-like repositories, when there is also at least one binary repository supplied e.g., the rstudio package manager, i.e., there are at least 3 repositories supplied, 1 of which is binary.
* other minor

# Require 0.2.5

## enhancements
* several modifications to enable CRAN-policy violations all addressed, notably keeping all temporary and (package and personal) cache directories clean after examples and tests
* This is a major overhaul of the inner workings of `Require`. It now downloads and builds `Archive` and `GitHub` packages prior to installation, then installs all packages (`CRAN`, `Archive`, `GitHub`, `MRAN` on Windows) with one `install.packages` call (Linux-alikes) or up to two `install.packages` calls (binary and source), allowing efficient parallel installs. This results in very fast installs for all combinations of packages.
new `options("Require.offlineMode")` can be set to `FALSE` to stop `Require` and `pkgDep` from checking the internet. This will fail, unless the cached packages are available locally (i.e., it was run once with all packages installed previously). If they are, then they will be installed without needing the internet. This option will also be set automatically on the first attempt to get a file from the internet, which fails, triggering a test of the internet. If that fails, then the option will be set to `FALSE` until next call to `Require` or `pkgDep` when it will be reset. This is experimental still.
* many more edge cases found and dealt with
* experimental use of `(HEAD)` to keep a package "up to date" with the HEAD of a GitHub branch. The behaviour still uses version numbering, so will not update based on SHA, but if the HEAD is ahead of the locally installed package and the `(HEAD)` is specified, then it will update. Specifically, use this instead of a version number, e.g., `"PredictiveEcology/Require@development (HEAD)"`
* `modifyList2` now follows `modifyList` by adding the `keep.null` argument.
* `setdiffNamed` will compare 2 named lists or vectors and keep on those elements that are in the first list (or vector), keeping in mind the name as well as the element.
* package messaging is not sorted alphabetically during installation
* all `message` calls now `messageVerbose`, so verbosity can be fully controlled with the argument `verbose` or `options("Require.verbose")`. See `?RequireOptions`.
* tests clean up more completely after themselves
* if `options(Require.cachePkgDir = FALSE)` (or environment variable `"R_REQUIRE_PKGCACHE"`), then no cache folder will be created; previously a nearly empty folder was created by default. See `?RequireOptions`
* Remove option `Require.persistentPkgEnv` as it was deemed superfluous.
* numerous enhancements for speed
* new function `Install`, which is `Require(..., require = FALSE)`
* `(HEAD)` has now been tested for CRAN repositories and works as expected.
* Updated README to show new functionality
* will attempt to use local cached packages from `crancache` if the user sets `options(Require.useCranCache = TRUE)`. This is experimental and is still being tested.
* A new function, `clearRequirePackageCache`, for clearing the package cache.
* The cache has been developed to be able to be shared across Operating Systems, if there is a shared file system.
* GitHub packages require the SHA to be assessed; now this is Cached to disk as well as RAM, so that it persists even if there is an R restart. 
* All non-package cache files (`available.packages`, `pkgDep`, `GitHubSHA`) will be refreshed (purged) every 1 hour.
* Much improved messaging, including identifying `MRAN` package installs explicitly (instead of just "Archive")

## bugfixes
* `pkgDep` was using local `DESCRIPTION` file to establish package dependencies for a package, if it was available. When the local package is ahead of CRAN (a developer's case), then this is desirable. But, when the local installed version is behind CRAN (a common user's case), then this is not desirable. `pkgDep` now uses CRAN's version (using `available.packages`) as developers can handle this situation on their own.
* several minor
* bugfix for `defaultCacheDir`, which would default to `runneradmin` under some conditions and did not allow installing packages due to permissions.

## deprecated
* `setup` and `setupOff` are now deprecated; messaging is supplied for what to do if these were being used
* several options are deprecated

# Require 0.1.6

## enhancements
* `pkgSnapshot` examples brought up to present usage & simplified
* `pkgSnapshot` now uses a default filename that is an option `Require.packageVersionFile`.
* `Require` can now accept `packageVersionFile = TRUE`, meaning use the package version file that is set in the `Require.packageVersionFile` option.

## bugfix
* minor bugfix only detected on submission to CRAN

# Require 0.1.5

## enhancements
* package caching for packages that need sources installs (i.e., identified with `sourcePkgs()`, which tend to occur when R packages require idiosyncratic system dependencies) cache the binary version and reuse that on the same system with subsequent re-installs.

## bugfix
* `pkgDep` was misidentifying the correct package dependencies. This would manifest when a user had a version of package "A" installed as well as all its dependencies, e.g., "B". When the user updated "A" to a new version that required a new version of "B", it would not correctly identify the new dependency requirement, and not update "B", causing "A" update to fail. This is fixed.

# Require 0.1.4

* Make corrections for 2 failing architectures on CRAN
* MUCH less verbose during automated testing

## enhancement
* `verbose` argument is now widespread, with -1, 0, 1, 2 all valid and correctly inherited values. See argument description in e.g., `?Require`
* improved warning handling

## bugfixes
* more edge cases found and dealt with

# Require 0.1.2

## dependencies
* drop support for R 3.6 (R >= 4.0 are supported)

## enhancements
* The `Require` argument, `require`, can now be a character string, indicating which packages should be attached via `require`
* Now can use `GITHUB_PAT` environment variable, if set, when it accesses GitHub.com repositories (files or entire repository)
* Attempt to capture and correct cases where GitHub.com branches are incorrectly labelled `master` instead of `main` (or vice versa)
* much quieter messaging by default (can increase with verbose = 1)
* `require` argument in `Require` can now be a character vector indicating which packages should be attached, not just installed. Note: by default, all packages that are passed to `packages` are attached if `require = TRUE`

* much faster installations:

  * When source packages, they are grouped and installed together using the internal parallelism of `install.packages` (setting `Ncpus` option to 4)
  * when binary, passes vectors to install.packages so much faster.
  * all packages are installed in install-safe groups for speed

* can use pak package under the hood when `options("Require.usepak" = TRUE)`, though there are still many cases that pak cannot deal with. Users should try and determine if this option delivers as expected. pak installs tend to be slightly faster if they work correctly.
* binary package caching is turned in by default in a user-specific standard directory, making repeat installations (on same system, or shared drive systems) much faster.
* MRAN installs for Windows are now much more robust under many conditions.
* archived packages (i.e., no longer on CRAN) will now be found and installed (latest available version)
* more robust dependency identification even for archived or older packages or package versions (including their dependencies)
* MRAN binaries will be used in macOS.
* improved installation of older packages (e.g. when dependencies are removed from CRAN, or source versions can't be easily compiled)
* several other minor improvements in package dependency resolution and installation.

## bugfixes
* fix issue with 'dot directories' in `normPath()`.
* identified possible bug with `install.packages` when `options(Ncpus = XX)` where XX is a number > 1. Some packages are skipped. `Require` now captures this and attempts to install the ones that did not get correctly installed.
* multiple fixes for certain edge cases.

# Require 0.1.1

## enhancements
* can now use `pak` if `options("Require.usepak" = TRUE)` and there are no version specifications (i.e., if a user specifies e.g., `Require("reproducible (<= 1.2.9))`, then the non-`pak` approach will be used)

## bugfixes
* fixed an error installing certain GitHub packages

# Require 0.1.0

## enhancements
* install CRAN packages using vectorized `install.packages` --> much faster
* now uses internal `installGithubPackage` instead of `remotes::install_github`
* this previous means that all installations use `install.packages` directly
* remove dependency on `remotes`

## bugfixes
* `Require` would silently fail to install a GitHub package if there was a warning during the installation. These warnings are now correctly captured, without stopping the installation.
* bugfix where a package being installed from GitHub directly had a `Remotes` field for a package that was in `Suggests` (in its DESCRIPTION file). It would install this `Remotes` package even though it was only in `Suggests`
* bugfix when user supplies a non-CRAN `repos` argument to `Require`. It was not correctly using. Thanks to @CeresBarros for identifying issue #30
* bugfix "All packages appear to have installed correctly" was misreporting under some cases.
* `repos` argument not correctly passed into `doInstalls` from `Require`. This meant that installs would not respect a user supplied repos, but would use the `options("repos")` instead.
* `extractPkgNames` now allows GitHub packages that have the repository omitted, i.e., they only have `@`. This is useful if there is a default expectation for a github repository
* better handling of GitHub package install issues

# Require 0.0.13

* fix CRAN policy violation -- dealt with extraneous folder created during testing

# Require 0.0.12

## Dependency changes
* with the release of R 4.1, we dropped support for R 3.5. R 3.6 (`oldrel`) and newer are supported.

## New features
* `setup`: new function for creating a new project. See `readme.md`
* `setLibPath` and package caching (via `options("RPackageCache")`) now automatically create and use a subfolder of user-provided path with the R major & minor version number (as with normal R behaviour) to allow multiple R versions to coexist on the same machine.
* `setLibPaths` gains a new argument, `updateRprofile`, which allows a user's changes to `.libPaths()` to persist through an R restart. Set to `getOption("Require.updateRprofile", FALSE)`, at start

## Bug fixes
* several edge cases with complex loading of many packages
* was incorrectly (not) loading base packages, e.g., `parallel`
* small minor bugfixes
* In cases where a DESCRIPTION file had both a package with a minimum version (e.g., in Imports) and a REMOTES: for that package (without a minimum version, but with a branch, say), `Require` would use the REMOTES: entry. But since that means there is no minimum package version, and `Require` does not automatically install a package that is not violating a minimum version number, it would not install anything. Now, it harmonizes the 2 entries for a given package, and uses both the minimum version number and the git branch as the potential source to find that version number.
* allow either `master` or `main` branches to be installed from GitHub, without needing to specify (#26)
* fix use of options in `setup()`

# Require 0.0.10

## Bug fixes
* CRAN error on one flavour of Linux
* erroneous `checkPath` error creating `Specified path xxxx doesn't exist` even though it does.

# Require 0.0.9

## New features
* `modifyList2`, a generalization of `utils::modifyList` for >2 lists. Also, can handle NULL lists.
* slight improvements in speed for some internal functions
* `detachAll` now unloads reverse depends of the depends, if they are loaded

## Bug fixes
* deals with more cases of installing arbitrary packages from a `packageVersion.txt` file
* Does not mistakenly create a new, empty directory of packages to accommodate 2 `LibPaths` from `packageVersion.txt` file, *if the second (or more) `LibPath`* is full of base packages.
* Handles better false positives (packages did not install properly when they did) and some false negatives (no error collected at end when there was an error in installing)
* better suggestion of what to do in some edge cases of failed package installs
* captures and deals with a bug in `install.packages` (`argument "av2" is missing, with no default`) on R-devel for Windows (on Sept 09, 2020). May be transient.
* Was, by default, installing from `source` on Windows. Fixed.

# Require 0.0.8

## New features
* GitHub SHA is now stored during `pkgSnapshot`, meaning that a new system can be built with exact versions and SHAs of GitHub packages.
* For GitHub packages, now uses both DESCRIPTION and NAMESPACE files to determine dependencies. GitHub packages are generally for packages in some state of development. This may include missing declarations in DESCRIPTION. NAMESPACE is what R uses to actually determine package dependencies upon installation.
* Now keeps the binary/source package locally if `options("Require.cachePkgDir" = "someLocalDir")` is set to a local folder. Currently defaults to NULL, meaning no local cache.
* `Require` and `pkgSnapshot` can now understand and work with GitHub SHAs and thus packages installed from GitHub, e.g., `Require("PredictiveEcology/Require@development")` will install the development version. When using `pkgSnapshot`, the exact SHA will be used to restore that package at the exact version with `Require(packageVersionFile = "packageVersions.txt")`.
* If a package is already loaded prior to changing running `setLibPaths`, it is possible to create a version conflict. `base::require` will error if the version in the `.libPaths()` is older than the version whose namespace is already loaded. To accommodate this, there is a check for this error, and if the newer version (that is already loaded) does not violate the `Require('package (versionSpecification)')`, then it will install the newer version. If it does violate the version specification, it will error cleanly with a message describing the possible solutions.
* Much better messaging and reporting
* New function: `detachAll` that attempts to detach and unload packages and all their dependencies, in reverse topological order.
* Speed improvements, especially with `pkgDep` and `pkgDepTopoSort`
* New function `pkgDepAlt` which is an alternative to `pkgDep`, yet easier to maintain and still experimental. It is not yet the workhorse inside `Require`, but it may become that.
* Now correctly removes spaces and tab characters within a package version description -- this was creating an error such as `Error: invalid version specification ' 	3.3-13'`

## Bug fixes
* `pkgDepTopoSort` now appears to be correct for all types of package descriptions currently allowed by `Require`, namely, packages with no version specification, packages with version specification (including older versions), and GitHub packages.
* many minor edge cases

# Require 0.0.7

## New features
* no longer sets CRAN repository to cloud.r-project.org even if non-interactive with no CRAN repository set. Now uses `chooseCRANmirror(ind = 1)`

## Bug fixes
* fixes CRAN check issues on Fedora.

# Require 0.0.6

## New features
* none

## Bug fixes
* fixed CRAN check issues.
* default repo now uses option `repos` instead of specifying CRAN repo.

# Require 0.0.5

## New features
* moved several functions that have to do with package loading and installing from `reproducible` to `Require`, including `pkgDep`, `pkgDepTopoSort`.

## Bug fixes
* recursive `pkgDep` did not correctly resolve multiple instances of the same package, each with different minimum version numbering. Now it reports minimum version required for all package dependencies.
* minor changes in non-exported functions
* handling of bugs in `base::available.packages` for old Mac machines and R versions

# Require 0.0.4

## Bug fixes
* remove `installed.packages` from test code, as per CRAN request

# Require 0.0.3

* Change title to Title Case in DESCRIPTION

# Require 0.0.2

* Change backticks to single quotes in DESCRIPTION

# Require 0.0.1

## New features
* This is a rewrite of the function, `Require` (and helpers) which will be removed from package `reproducible`
* This function is intended to be a tool for package management used within a "reproducible" workflow
* It differs from all other attempts at achieving this goal by having the trait that the first and subsequent times the function `Require` is run, the result will be the same
