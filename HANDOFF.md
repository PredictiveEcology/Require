# HANDOFF — usePak branch, snapshot installer rebuild

## Where we are
Branch: `usePak` (HEAD `f33a16f0`).
Local-only changes pending commit:
- `R/pkgSnapshot.R` — major changes (cache self-heal, source-only filter, coherence pre-check, diagnostic improvements).
- `inst/snapshot.txt` — bumps: `xfun 0.40 → 0.52`, `renv 1.0.3 → 1.2.2`.

## What's been built (the snapshot install pipeline in `R/pkgSnapshot.R`)
`installSnapshotViaInstallPackages()` is now a multi-stage pipeline:

1. **Skip already-installed** (matching version pin in destLib).
2. **Pre-filter via pkgcache** — query `pkgcache::pkg_cache_list()`, match by package + version (or GH SHA in URL).
   - **Source-only**: pak's `local::<file>` ref handling rejects binary tarballs with "Platform mismatch" (verified empirically). The pre-filter drops binaries entirely. Source tarballs only feed the local:: pipeline.
   - **Self-heal**: walks every matching cache row in priority order, validates each (file exists / `gzip -t` passes / DESCRIPTION's `Package:` matches expected). The first valid hit wins. Any rejected hits get queued and **evicted** via `pkgcache::pkg_cache_delete_files(url=...)` or `(fullpath=...)` — corrupt entries don't keep blocking future runs.
3. **Download the rest** via libcurl-multi, chunked at 50 URLs/batch (macOS ulimit ~256). Priority per ref: row's Repository → PPM → CRAN. 4 retry attempts with exponential backoff. Reports elapsed seconds per pkg.
4. **Find nearest archived version** for any unresolvable ref (one-by-one, via `findNearestArchivedVersion`).
5. **Repackage** any tarball without `<pkg>/DESCRIPTION` at top level via `R CMD build`. Universal content-based check.
6. **Populate pkgcache** post-download:
   - **destPath naming**: uses `pkgs$Version` (not `substr(SHA, 1, 7)`) for GH refs when Version is populated. pak validates filename version against DESCRIPTION's Version, so `visualTest_9b835a7.tar.gz` would always fail with "Line starting 'visualTest/DESCRIPTI ...' is malformed!" (the actual cause is filename-version mismatch).
   - **`pkg_cache_add_file(relpath = ...)` bug fix**: the previous code passed `relpath = "Require/snapshot"` (no filename), but `relpath` is the FULL relative path including filename. Result: every add overwrote the same single file at `<cache>/Require/snapshot`, producing silent corruption (e.g., 4 separate index entries — fastdigest, knn, spatstat.core, visualTest — all pointing to a file whose actual content was `pscl 1.5.9`, the last writer). Now uses `file.path("Require","snapshot",basename(destPaths[i]))`.
7. **Snapshot-coherence pre-check** — reads each ref's DESCRIPTION (`Depends`/`Imports`/`LinkingTo`) and verifies every version-constrained dep is satisfied by the snapshot's other pins. Reports unsatisfied constraints upfront so the user knows what to bump *before* the slow install starts. Reduces "pak refused — couldn't solve" mysteries to a one-line list.
8. **Try `pak::pkg_install(local::..., dependencies = NA)`** — primary path. On failure, walks the caught condition's `$parent` chain (rlang-style chained errors) to surface the inner cause; pak 0.9.2 doesn't export `last_error`, the chain on the caught error has all of it.
9. **`pak::pkg_deps` resolver-only probe** — runs after pak fails to differentiate "resolve failure" (constraint conflict) from "install failure" (compile error). Says explicitly "failure is at install stage, not resolve" when applicable.
10. **Fall back to `install.packages(repos = file://, dependencies = NA, Ncpus = N)`** — best-effort closed-snapshot install. Tolerates per-package compile failures.
11. **Auto-fill missing transitive deps** from CRAN/PPM (snapshots aren't always closed graphs).
12. **Cache compiled binaries** via `cacheBuiltBinaries()` registered on `on.exit` — survives Ctrl-C, error, pak crash.
13. **Diagnostic report** classifies any missing pkg by status with concrete `fix:` lines.

## Bugs found and fixed (this session)
- **Cache add overwriting same file** — `pkg_cache_add_file(relpath="Require/snapshot")` was missing the filename → all writes overwrote the same single file at `<cache>/Require/snapshot`. Fix: include `basename(destPaths[i])` in relpath. **This is the root cause of every "corrupt pkgcache entry" we'd been seeing in earlier sessions** — rotten cache entries were not "pkgcache lying", they were our own buggy adds.
- **Cache pre-filter only tried first hit** — a single rotten top-priority entry blocked refs that had other valid hits. Now walks all hits in priority order until one validates.
- **Stale cache entries persisted** across runs because nothing evicted them — same 6 rotten entries forced 6 fresh downloads on every run. Now evicted automatically when validation fails.
- **`local::*.tgz` and `local::*.tar.gz`-with-binary-content** trigger pak "Platform mismatch" — pak rejects binary tarballs as local:: refs unconditionally. Fix: pre-filter is source-only. Verified empirically with `/tmp/test-pak-local-tgz.R`.
- **GH ref destPath used `<pkg>_<sha7>.tar.gz`**, but pak validates filename version against DESCRIPTION's Version → always mismatches → cryptic error "Line starting 'visualTest/DESCRIPTI ...' is malformed!". Fix: destPath uses `pkgs$Version` when populated.
- **Empty pak diagnostics** because `pak::last_error()` isn't exported in pak 0.9.2. Fix: walk the caught condition's `$parent` chain instead — that chain is on the error regardless of pak version.

## Snapshot coherence violations found
The pre-check (step 7) reported exactly **one** unsatisfied constraint:
- `servr 0.30 requires xfun >= 0.42; snapshot pins xfun = 0.40` — fixed by bumping `xfun → 0.52`.

After that bump, pak's resolver succeeds (`pkg_deps probe (resolver-only) succeeded with 391 refs — failure is at install stage, not resolve.`).

## What still blocks pak success — and why install.packages survives
pak's parallel build aborts on the first per-package build failure. install.packages tolerates per-package failures and continues. So any single compile failure on the user's system kills pak's whole install but only kills one package for install.packages.

System-library compile failures observed on the dev Mac:
- **`jpeg 0.1-10`**: `'jpeglib.h' file not found`. Cause: R's compile uses `-I/opt/R/arm64/include` only; libjpeg headers are at `/opt/homebrew/include`. Fix: `~/.R/Makevars` with:
  ```
  CPPFLAGS += -I/opt/homebrew/include
  LDFLAGS  += -L/opt/homebrew/lib
  ```
- **`renv 1.0.3`**: lazy-load fails inside pak's parallel build with `target_mtime > source_mtime` NA error. Fix: bump to `renv 1.2.2`.
- **`terra 1.7-78`**: configure fails because `gdal_proj` test binary can't load `libabsl_log_internal_check_op.2508.0.0.dylib` — re2 is linked against an old abseil version that's no longer installed. Fix on the dev machine: `brew reinstall re2` or `brew upgrade abseil re2`. install.packages's per-package compile sandbox hits the same error but continues; pak aborts.

These are all environment / system-state issues, not Require code issues. Once the user's system is clean (Makevars + brew state), pak should fully succeed.

## Strategy notes that turned out wrong
- **"Use cache binaries for our platform"** — pak's local:: refs reject binaries entirely. Cache binaries are still useful for pak's *non-local* install path (which we don't drive here). Pre-filter is source-only.
- **"Topological-batched pak with smaller chunks"** — pak's resolver needs the *full* ref set to use it as a closed graph. Batching breaks the closed-graph guarantee. (Per user.)

## Test infrastructure
- `tests/testthat/test-09pkgSnapshotLong_testthat.R` — 3 core assertions (no rogue please-change warnings, all expected installed, version pins honored). Hard-codes fast-path options via `withr::local_options`.
- `tests/testthat/setup.R` — `cli.dynamic = TRUE`, `R_CLI_DYNAMIC = "true"` env, `pkg.show_progress = FALSE` for interactive dev. Override of `R_USER_CACHE_DIR` to tempdir gated on `!interactive()` so dev runs reuse the user's real pkgcache.

## Files most recently touched (uncommitted)
- `R/pkgSnapshot.R` — cache self-heal + source-only + relpath fix + GH destPath naming + coherence pre-check + pak error chain + pkg_deps probe + timing
- `inst/snapshot.txt` — `xfun 0.40 → 0.52`, `renv 1.0.3 → 1.2.2`

## Helper scripts (machine-local)
- `/tmp/run-test09.R` — driver with `R_USER_CACHE_DIR` set to user's real pkgcache, runs `testthat::test_local(filter = "09")`, dumps `pak::last_error()` (when exported).
- `/tmp/find-conflicts.R` — replicates the snapshot-coherence pre-check standalone over the cache's source tarballs. Useful when iterating on snapshot bumps without running the full installer.
- `/tmp/test-pak-local-tgz.R` — minimal repro that pak's `local::*.tgz` rejects binary tarballs.

## Quick resume on Mac
```bash
cd ~/GitHub/Require
git pull origin usePak

# One-time system fixes:
cat > ~/.R/Makevars <<'EOF'
CPPFLAGS += -I/opt/homebrew/include
LDFLAGS  += -L/opt/homebrew/lib
EOF
brew reinstall re2     # if abseil/re2 are out of sync
```
Then in R:
```r
testthat::test_local(filter = "09")
```
First-run output should include the snapshot-coherence pre-check (0 conflicts now), pkgcache pre-filter (377+/378 hits typical), and `pak::pkg_install` succeeding through to `installed via pak (binary cache)`.

## How to dig deeper
- The diagnostic helper `diagnoseSnapshotInstallFailures()` parses per-package R CMD INSTALL logs from `keep_outputs/`.
- pak's chained error: just walk `err$parent` recursively on the caught condition; that's where the resolver/build failure reason lives in pak 0.9.2 (no `last_error()` export).
- For tarball-structure issues, `tar tzf <file>` should show `<pkg>/DESCRIPTION` as one of the first entries — anything else means it needs repackaging.
- For pkgcache index sanity, `pkgcache::pkg_cache_list()` then look for entries with `path = "Require/snapshot"` (no filename) — those are the legacy buggy adds and will be evicted by the next run that hits them.
