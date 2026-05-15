#' Take a snapshot of all the packages and version numbers
#'
#' This can be used later by `Require` to install or re-install the correct versions. See examples.
#'
#' @details
#' A file is written with the package names and versions of all packages within `libPaths`.
#' This can later be passed to `Require`.
#'
#' `pkgSnapshot2` returns a vector of package names and versions, with no file output. See
#' examples.
#'
#' @section Installing from a snapshot file:
#' Pass the snapshot file to `Require()` via `packageVersionFile = "snapshot.txt"`.
#' By default this routes through a multi-stage installer (gated by
#' `options(Require.snapshotInstaller = "install.packages")`) that:
#'
#' 1. **Skips already-installed-at-target-version refs.**
#' 2. **Cache pre-filter** via `pkgcache::pkg_cache_list()`. Source tarballs feed
#'    `pak::pkg_install(local::...)`; binaries are reserved for step (4). Rotten
#'    cache rows (missing fullpath, gzip-corrupt, DESCRIPTION mismatch) are
#'    auto-evicted via `pkgcache::pkg_cache_delete_files()` so future runs
#'    don't keep tripping on them.
#' 3. **Parallel libcurl-multi download** for refs not in cache, chunked at 50
#'    URLs per call (macOS file-descriptor limit). Walks priority URLs: row's
#'    `Repository` -> PPM -> CRAN -> CRAN/Archive. Up to 4 retries with
#'    exponential backoff (`Require.snapshotDownloadAttempts`).
#' 4. **Hybrid binary-first install** via `install.packages(type = "binary")`
#'    for any ref that has a cache binary matching this R session
#'    (`R.version$platform`, `<major>.<minor>`). Skips compilation, reduces
#'    pak's parallel-build workload. Disable via
#'    `options(Require.snapshotInstallerHybrid = FALSE)`.
#' 5. **`pak::pkg_install(local::...)` for the rest** with `dependencies = NA`,
#'    `upgrade = FALSE`. Refs already-installed-at-target-version are excluded
#'    upfront so pak doesn't reinstall-to-self.
#' 6. **`install.packages(repos = file://...)` fallback** if pak refuses (its
#'    solver is strict; `install.packages` is best-effort and tolerates
#'    per-package compile failures).
#' 7. **Bump-and-retry**: for any ref still missing, walks newer-than-pin
#'    versions from CRAN/PPM/Archive ascending and tries each until one
#'    installs (capped at 20 candidates). Disable via
#'    `options(Require.snapshotInstallerBumpOnFail = FALSE)` for strict
#'    reproducibility (no drift, fail loudly).
#' 8. **Diagnostic report** classifying each gap with a concrete `fix:` line.
#'
#' Built binaries from this run are added back to `pkgcache` via
#' `cacheBuiltBinaries()` (registered on `on.exit`), so a subsequent run hits
#' step (4) instead of recompiling.
#'
#' @section Common snapshot pitfalls:
#'
#' \describe{
#'   \item{Version-coherence (the snapshot's own pins disagree)}{
#'     A ref's `DESCRIPTION` declares `Imports: X (>= V)` but the snapshot
#'     pins `X` at a version that doesn't satisfy it. `pak`'s strict solver
#'     refuses to install. The installer runs a coherence pre-check before
#'     handing off to pak and prints any unsatisfied constraint with a fix
#'     suggestion (e.g., `servr 0.30 requires xfun (>= 0.42); snapshot pins
#'     xfun = 0.40 -> bump xfun`).
#'   }
#'   \item{`R` pseudo-package}{
#'     `pkgSnapshot()` writes a row recording the running R version
#'     (e.g., `R,4.4,...`). The installer skips this row alongside base
#'     packages.
#'   }
#'   \item{System library mismatches (compile failures)}{
#'     Source builds need the host's system libs to match what the package
#'     expects. The installer's `classifyCompileFailure()` recognises
#'     missing-header errors (jpeglib.h, gdal.h, geos_c.h, glpk.h,
#'     ft2build.h, sodium.h, ...) and prints the corresponding
#'     `brew install ...` (or apt) suggestion. R 4.5's removal of
#'     `Calloc/Free`, GDAL >= 3.10's `const OGRSpatialReference*` ABI
#'     change, and Rcpp's `class_::constructor<>` template-arity limit are
#'     each pattern-matched and reported with a "bump `<pkg>`" suggestion.
#'   }
#'   \item{Mac toolchain -- `~/.R/Makevars`}{
#'     R's default compile flags only search `/opt/R/arm64/include`. To pick
#'     up Homebrew headers (libjpeg, glpk, freetype, etc.), add to
#'     `~/.R/Makevars`:
#'     \preformatted{
#'       CPPFLAGS += -I/opt/homebrew/include
#'       LDFLAGS  += -L/opt/homebrew/lib
#'     }
#'   }
#'   \item{Stale `pkgcache` index}{
#'     `pkgcache` shares state across R versions and architectures. Cache
#'     rows tagged with platform/rversion that don't match this session
#'     are filtered out (e.g., R-4.5 binaries when running R-4.4); validation
#'     also catches index rows whose file content disagrees with the index
#'     (a known historical bug-class). Both kinds get auto-evicted, so
#'     `pak::cache_clean()` is rarely needed.
#'   }
#'   \item{Pak's `local::` is source-only}{
#'     Confirmed empirically that pak's `pkg_install("local::<file>")`
#'     rejects binary tarballs (`.tgz` / `.zip` content) with
#'     "Platform mismatch" -- even when the binary is for the current
#'     platform. The hybrid stage installs binaries via
#'     `install.packages(type = "binary")` BEFORE pak runs, so pak only
#'     sees source refs.
#'   }
#'   \item{Pak strict-aborts on first build failure}{
#'     `install.packages` continues past per-package compile failures;
#'     `pak::pkg_install` does not. The fallback to `install.packages` and
#'     the bump-and-retry stage together provide a best-effort completion
#'     guarantee even when individual refs are environment-fragile.
#'   }
#' }
#'
#' @section Snapshot-installer options:
#' \describe{
#'   \item{`Require.snapshotInstaller`}{`"install.packages"` to use the
#'     pipeline above; `"pak"` for the legacy direct-pak path.}
#'   \item{`Require.snapshotInstallerUsePPM`}{TRUE (default) to prepend a PPM
#'     binary repo. PPM serves Mac binaries by content-negotiating the
#'     `R/<version>` User-Agent.}
#'   \item{`Require.snapshotInstallerHybrid`}{TRUE (default) -- pre-install
#'     cache binaries via `install.packages(type = "binary")` before pak.}
#'   \item{`Require.snapshotInstallerBumpOnFail`}{TRUE (default) -- walk newer
#'     versions for refs that fail at the pin. FALSE for strict
#'     reproducibility.}
#'   \item{`Require.snapshotInstallerKnownFails`}{character vector of pkg
#'     names to skip in bump-retry (e.g. environment-dependent refs whose
#'     newer versions also won't help).}
#'   \item{`Require.snapshotInstallerPakSilent`}{FALSE (default) -- pak's
#'     resolver output reaches the user.}
#'   \item{`Require.snapshotDownloadAttempts`}{Retry count for libcurl-multi
#'     downloads. Default 4.}
#'   \item{`Require.snapshotDownloadChunk`}{URLs per `download.file()` call.
#'     Default 50 (stays under macOS's ~256 file-descriptor ulimit).}
#' }
#'
#' @return
#' Will both write a file, and (invisibly) return a vector of packages with the
#' version numbers. This vector can be used directly in `Require`, though it should likely
#' be used with `require = FALSE` to prevent attaching all the packages.
#'
#' @param packageVersionFile A filename to save the packages and their currently
#'        installed version numbers. Defaults to `"packageVersions.txt"`.
#'        If this is specified to be `NULL`, the function will return the exact
#'        `Require` call needed to install all the packages at their current
#'        versions. This can be useful to add to a script to allow for reproducibility of
#'        a script.
#' @param libPaths The path to the local library where packages are installed.
#'        Defaults to the `.libPaths()[1]`.
#' @param exact Logical. If `TRUE`, the default, then for GitHub packages, it
#'        will install the exact SHA, rather than the head of the `account/repo@branch`. For
#'        CRAN packages, it will install the exact version. If `FALSE`, then GitHub
#'        packages will identify their branch if that had been specified upon installation,
#'        not a SHA. If the package had been installed with reference to a SHA, then it
#'        will return the SHA as it does not know what branch it came from.
#'        Similarly, CRAN packages will report their version and specify with a `>=`,
#'        allowing a subsequent user
#'        to install with a minimum version number, as opposed to an exact version number.
#'
#' @export
#' @inheritParams Require
#' @inheritParams pkgDep
#' @importFrom data.table fwrite
#' @importFrom utils write.table
#' @examples
#' \donttest{
#' if (Require:::.runLongExamples()) {
#'   opts <- Require:::.setupExample()
#'
#'   # install one archived version so that below does something interesting
#'   libForThisEx <- tempdir2("Example")
#'   Require("crayon (==1.5.1)", libPaths = libForThisEx, require = FALSE)
#'   # Normal use -- using the libForThisEx for example;
#'   #    normally libPaths would be omitted to get all
#'   #    packages in user or project library
#'   tf <- tempfile()
#'
#'   # writes to getOption("Require.packageVersionFile")
#'   # within project; also returns a vector
#'   # of packages with version
#'   pkgs <- pkgSnapshot(
#'     packageVersionFile = tf,
#'     libPaths = libForThisEx, standAlone = TRUE # only this library
#'   )
#'
#'   # Now move this file to another computer e.g. by committing in git,
#'   #   emailing, googledrive
#'   #   on next computer/project
#'   Require(packageVersionFile = tf, libPaths = libForThisEx)
#'
#'   # Using pkgSnapshot2 to get the vector of packages and versions
#'   pkgs <- pkgSnapshot2(
#'     libPaths = libForThisEx, standAlone = TRUE
#'   )
#'   Install(pkgs) # will install packages from previous line
#'
#'   Require:::.cleanup(opts)
#'   unlink(getOption("Require.packageVersionFile"))
#' }
#' }
#'
#' @rdname pkgSnapshot
pkgSnapshot <- function(packageVersionFile = getOption("Require.packageVersionFile"),
                        libPaths = .libPaths(),
                        standAlone = FALSE,
                        purge = getOption("Require.purge", FALSE),
                        exact = TRUE,
                        includeBase = FALSE,
                        verbose = getOption("Require.verbose")) {
  libPaths <- checkLibPaths(libPaths = libPaths, exact = TRUE)
  libPaths <- doLibPaths(libPaths, standAlone)

  ip <- doInstalledPackages(libPaths, purge, includeBase)
  rv <- versionMajorMinor()
  rv <- cbind(Package = "R", Version = rv)
  ip <- rbind(rv, ip, fill = TRUE)

  fwrite(ip,
    file = packageVersionFile,
    row.names = FALSE,
    na = NA
  )
  messageVerbose(
    "package version file saved in ",
    packageVersionFile,
    verbose = verbose,
    verboseLevel = 1
  )

  return(invisible(ip))
}

#' @rdname pkgSnapshot
#' @export
pkgSnapshot2 <-
  function(packageVersionFile = getOption("Require.packageVersionFile"),
           libPaths,
           standAlone = FALSE,
           purge = getOption("Require.purge", FALSE),
           exact = TRUE,
           includeBase = FALSE,
           verbose = getOption("Require.verbose")) {
    libPaths <- doLibPaths(libPaths, standAlone)

    ip <- doInstalledPackages(libPaths, purge, includeBase)

    if (isTRUE(exact)) {
      ref <- ip$GithubSHA1
      ineq <- "=="
    } else {
      ref <- ip$GithubRef
      ineq <- ">="
    }
    thePkgAndVers <- paste0(ifelse(
      !is.na(ip$GithubRepo),
      paste0(ip$GithubUsername, "/", ip$GithubRepo, "@", ref),
      # github
      paste0(ip$Package, " (", ineq, ip$Version, ")") # cran
    ))
    thePkgAndVers
  }


#' Only checks for deprecated libPath argument (singular)
#' @inheritParams Require
#' @param ... Checks for the incorrect argument `libPath` (no s)
dealWithMissingLibPaths <- function(libPaths, standAlone = getOption("Require.standAlone", FALSE),
                                    ...) {
  missingLP <- missing(libPaths)
  if (missingLP) {
    if (!is.null(list(...)[["libPath"]])) {
      libPaths <- list(...)[["libPath"]]
    }
  }
  libPaths <- doLibPaths(libPaths, standAlone)
  libPaths
}

#' Creates the directories, and adds version number
#' @inheritParams Require
#' @param ifMissing An alternative path if `libPaths` argument is missing.
#' @param exact Logical. If `FALSE`, the default, then `checkLibPaths` will
#'   append the R version number on the `libPaths` supplied. If `TRUE`, `checkLibPaths`
#'   will return exactly the `libPaths` supplied.
#' @param ... Not used, but allows other functions to pass through arguments.
checkLibPaths <- function(libPaths, ifMissing, exact = FALSE, ...) {
  missLP <- missing(libPaths)
  if (missLP) {
    if (missing(ifMissing)) {
      return(.libPaths())
    } else {
      pathsToCheck <- ifMissing
    }
  } else {
    pathsToCheck <- libPaths
  }
  unlist(lapply(pathsToCheck, function(lp) {
    checkPath(rpackageFolder(lp, exact = exact), create = TRUE)
  }))
}

#' Deals with missing libPaths arg, and takes first
#' @inheritParams Require
#' @importFrom utils head tail
doLibPaths <- function(libPaths, standAlone = FALSE) {
  if (missing(libPaths)) {
    libPaths <- .libPaths()
  }
  if (standAlone) {
    libPaths <- head(libPaths, 1)
    # libPaths <- c(head(libPaths, 1), tail(.libPaths(), 1))
  } else {
    libPaths <- unique(c(head(libPaths, 1), .libPaths()))
  }

  # if (isTRUE(standAlone)) {
  #   libPaths <- libPaths[1]
  # }
  libPaths
}

doInstalledPackages <- function(libPaths, purge, includeBase) {
  ip <-
    as.data.table(
      .installed.pkgs(lib.loc = libPaths, which = c("Depends", "Imports", "LinkingTo", "Remotes"),
        other = c("GitHubSha", "Repository", "GitSubFolder"), purge = purge
      )
    )
  if (isFALSE(includeBase)) {
    ip <- ip[!Package %in% .basePkgs]
  }

  ip
}

## Snapshot install path that bypasses pak's solver. The premise: a snapshot
## already pins exact versions, so dep resolution is wasted work. We download
## each pinned tarball into pak's content-addressed cache (idempotent), stage
## the tarballs as a local mini-repo via tools::write_PACKAGES, then call
## install.packages with type="source", dependencies=FALSE, Ncpus=N.
## install.packages reads the synthesized PACKAGES, builds a topo order over
## the explicit list, and parallelizes independent branches.
##
## Why dependencies=FALSE is safe here: the snapshot is the dep set. There is
## nothing to *add*. Topo ordering among the listed packages still works
## (install.packages always honours inter-dep order regardless of the
## dependencies arg). Internal version-mismatch in a snapshot (pkg A wants
## foo>=2 but snapshot pins foo@1) is not detected by install.packages with
## dependencies=FALSE -- but the same is true with pak under the same flag,
## and snapshot authors have already accepted that state by pinning what they
## pinned.
installSnapshotViaInstallPackages <- function(snapshot,
                                              libPaths = .libPaths()[1],
                                              Ncpus = max(1L, parallel::detectCores() - 1L),
                                              verbose = getOption("Require.verbose", 1)) {
  pkgs <- as.data.table(snapshot)
  ## "R" is a snapshot row recording the required R version, not a real
  ## package -- exclude it alongside base packages so the installer doesn't
  ## try to "install R" (and so the diagnostic doesn't report it missing).
  pkgs <- pkgs[!Package %in% c("R", .basePkgs)]
  if (!nrow(pkgs)) {
    messageVerbose("Snapshot has no non-base packages to install",
                   verbose = verbose, verboseLevel = 1)
    return(invisible(TRUE))
  }

  ## Skip pkgs already installed at the requested version in libPaths[1].
  ## CRAN pin: match Version exactly.
  ## GH pin: match RemoteSha (if recorded) against GithubSHA1.
  destLib <- libPaths[1]

  ## Cache the just-built binaries in pkgcache. Registered via on.exit
  ## so an interrupted run (Ctrl-C during compile, error mid-install,
  ## pak crash, etc.) still saves whatever binaries DID land in
  ## destLib -- partial progress accumulates across restarts.
  ##
  ## Each installed package directory under destLib IS already a binary
  ## (libs/.so compiled, R/ byte-compiled, Meta/Rd.rds, DESCRIPTION).
  ## Tar with `tar czf - -C destLib <pkg>` and register in pkgcache with
  ## built = TRUE + matching platform + rversion under a synthetic
  ## require-snapshot-bin:// URL. The pre-filter above prefers these
  ## (priority "ourBinary > source") so the next install of the same
  ## pin just unpacks (~50ms) instead of recompiling (minutes).
  ##
  ## Skip refs already cached as our-platform binaries to avoid
  ## re-tarring on every run (pkgcache add isn't idempotent).
  cacheBuiltBinaries <- function() {
    if (!requireNamespace("pkgcache", quietly = TRUE)) return(invisible())
    if (!nzchar(Sys.which("tar"))) return(invisible())
    ipForBin <- tryCatch(
      rownames(installed.packages(lib.loc = destLib, noCache = TRUE)),
      error = function(e) character())
    installedSnapshotPkgs <- intersect(snapshot$Package, ipForBin)
    if (!length(installedSnapshotPkgs)) return(invisible())
    rverShort <- paste0(R.version$major, ".",
                        strsplit(R.version$minor, "\\.")[[1]][1])
    ## binRelpath is computed PER-PACKAGE inside the loop below -- it
    ## must include the file's basename, otherwise every call to
    ## pkg_cache_add_file() overwrites the same single file at
    ## <cache>/<binRelpath>, leaving the cache index full of rows that
    ## all alias the same fullpath (the "Require/snapshot/bin/<plat>/
    ## <rver>" residue this comment exists to prevent). Fixed in
    ## c93b2... after diagnosing 142 cache rows pointing at the same
    ## directory-shaped fullpath.
    cacheNow <- tryCatch(pkgcache::pkg_cache_list(),
                         error = function(e) NULL)
    binStaging <- tempfile2("snapInstall_bins_")
    dir.create(binStaging, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(binStaging, recursive = TRUE), add = TRUE)
    binAdded <- 0L; binSkipped <- 0L
    for (p in installedSnapshotPkgs) {
      pkgDir <- file.path(destLib, p)
      if (!dir.exists(pkgDir)) next
      desc <- tryCatch(
        read.dcf(file.path(pkgDir, "DESCRIPTION"), fields = "Version"),
        error = function(e) NULL)
      if (is.null(desc) || nrow(desc) == 0L ||
          is.na(desc[1, "Version"])) next
      ver <- desc[1, "Version"]
      ## Skip if a binary for this pkg+ver+platform+rversion already
      ## sits in pkgcache (pak's own entry, or one we wrote earlier).
      if (!is.null(cacheNow) && nrow(cacheNow) > 0) {
        already <- !is.na(cacheNow$package) &
                   cacheNow$package == p &
                   !is.na(cacheNow$version) &
                   cacheNow$version == ver &
                   !is.na(cacheNow$built) & as.logical(cacheNow$built) &
                   !is.na(cacheNow$platform) &
                   cacheNow$platform == R.version$platform &
                   !is.na(cacheNow$rversion) &
                   cacheNow$rversion == rverShort
        if (any(already, na.rm = TRUE)) {
          fp <- cacheNow$fullpath[already][1]
          if (!is.na(fp) && file.exists(fp)) {
            binSkipped <- binSkipped + 1L
            next
          }
        }
      }
      binFile <- file.path(binStaging, paste0(p, "_", ver, ".tgz"))
      rc <- tryCatch(
        system2("tar",
                c("czf", shQuote(binFile),
                  "-C", shQuote(destLib), shQuote(p)),
                stdout = FALSE, stderr = FALSE),
        error = function(e) -1L)
      if (!identical(as.integer(rc), 0L) || !file.exists(binFile)) next
      fakeUrl <- paste0("require-snapshot-bin://",
                        R.version$platform, "/", rverShort, "/",
                        p, "_", ver, ".tgz")
      ## relpath MUST include the filename -- otherwise every add
      ## overwrites the same single file (see comment at the top of
      ## this function). Compose per-package.
      binRelpath <- file.path("Require/snapshot/bin",
                              R.version$platform, rverShort,
                              paste0(p, "_", ver, ".tgz"))
      addRes <- tryCatch(
        pkgcache::pkg_cache_add_file(
          file = binFile, relpath = binRelpath,
          url = fakeUrl, package = p, version = ver,
          platform = R.version$platform, built = TRUE,
          rversion = rverShort),
        error = function(e) e)
      if (!inherits(addRes, "error")) binAdded <- binAdded + 1L
    }
    if (verbose >= 1 && (binAdded > 0L || binSkipped > 0L))
      cat("[snapshotInstaller] cached ", binAdded, " new + ",
          binSkipped, " already-present built-binary tarball(s) in ",
          "pkgcache (", R.version$platform, " R ", rverShort, ")\n",
          sep = "")
    invisible()
  }
  on.exit(cacheBuiltBinaries(), add = TRUE)
  ip <- tryCatch(
    as.data.table(installed.packages(lib.loc = destLib, noCache = TRUE)),
    error = function(e) data.table(Package = character(), Version = character()))
  ipDesc <- function(p) {
    f <- file.path(destLib, p, "DESCRIPTION")
    if (!file.exists(f)) return(NA_character_)
    dcf <- tryCatch(read.dcf(f, fields = c("RemoteSha", "GithubSHA1")),
                    error = function(e) NULL)
    if (is.null(dcf) || nrow(dcf) == 0) return(NA_character_)
    sha <- dcf[1, "RemoteSha"]
    if (is.na(sha) || !nzchar(sha)) sha <- dcf[1, "GithubSHA1"]
    sha
  }

  isGH <- !is.na(pkgs$GithubRepo) & nzchar(pkgs$GithubRepo)
  alreadyOK <- logical(nrow(pkgs))
  for (i in seq_len(nrow(pkgs))) {
    p <- pkgs$Package[i]
    ipRow <- ip[Package == p]
    if (!nrow(ipRow)) next
    if (isGH[i]) {
      sha <- ipDesc(p)
      alreadyOK[i] <- !is.na(sha) && identical(sha, pkgs$GithubSHA1[i])
    } else {
      alreadyOK[i] <- !is.na(pkgs$Version[i]) &&
        identical(ipRow$Version[1], pkgs$Version[i])
    }
  }
  if (any(alreadyOK)) {
    messageVerbose(sum(alreadyOK), " of ", nrow(pkgs),
                   " snapshot packages already installed at requested version; skipping",
                   verbose = verbose, verboseLevel = 1)
    pkgs <- pkgs[!alreadyOK]
    isGH <- isGH[!alreadyOK]
  }
  if (!nrow(pkgs)) return(invisible(TRUE))

  dlDir <- tempfile2("snapInstall_dl_")
  if (!dir.exists(dlDir)) dir.create(dlDir, recursive = TRUE)
  on.exit(unlink(dlDir, recursive = TRUE), add = TRUE)

  ## Honour the snapshot's Repository column: rows like visualTest, NLMR can
  ## point at non-CRAN CRAN-style mirrors (e.g., r-universe.dev). Without
  ## these, pak's resolver only checks the default repos and 404s on packages
  ## that never lived on CRAN.
  reposFromSnapshot <- character()
  if (!is.null(snapshot$Repository)) {
    rfs <- unique(snapshot$Repository[!is.na(snapshot$Repository)])
    rfs <- rfs[grepl("^https?://", rfs)]
    if (length(rfs)) reposFromSnapshot <- rfs
  }

  ## Prefer PPM binaries when available: PPM serves pre-compiled
  ## tarballs indexed by distro, and pak honours options(repos), so prepending
  ## a PPM URL means recent versions skip compilation entirely. Older archived
  ## versions silently fall back to source. Opt out with
  ## options(Require.snapshotInstallerUsePPM = FALSE).
  origRepos <- getOption("repos")
  newRepos <- origRepos
  if (length(reposFromSnapshot)) {
    newRepos <- c(newRepos, setNames(reposFromSnapshot, paste0("snap", seq_along(reposFromSnapshot))))
    messageVerbose("Adding ", length(reposFromSnapshot),
                   " repo(s) from snapshot Repository column",
                   verbose = verbose, verboseLevel = 1)
  }
  if (isTRUE(getOption("Require.snapshotInstallerUsePPM", TRUE))) {
    ppm <- detectPPMRepo()
    if (!is.null(ppm) && !any(grepl("packagemanager.posit.co", newRepos, fixed = TRUE))) {
      newRepos <- c(PPM = ppm, newRepos)
      messageVerbose("Using PPM binaries: ", ppm,
                     verbose = verbose, verboseLevel = 1)
    }
  }
  if (!identical(newRepos, origRepos)) {
    options(repos = newRepos)
    on.exit(options(repos = origRepos), add = TRUE)
  }

  ## PPM serves Linux *binaries* via User-Agent content-negotiation: the same
  ## URL returns a source tarball to plain libcurl but a binary tarball when
  ## the request UA matches the `R/<version>` pattern. R's default
  ## HTTPUserAgent ("R (4.5.2 ...)") lacks the `R/<version>` token PPM keys
  ## on, so download.file() ends up fetching source. Override for the duration
  ## of this function so the libcurl multi call below picks up binaries
  ## (saves minutes-per-package on compiled refs).
  origUA <- getOption("HTTPUserAgent")
  options(HTTPUserAgent = sprintf(
    "R/%s R (%s)",
    getRversion(),
    paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))
  on.exit(options(HTTPUserAgent = origUA), add = TRUE)

  ## Build candidate URLs per ref, in priority order. libcurl multi handles
  ## parallel fetch of the vector in one call; we re-issue sequential passes
  ## only for refs that 404'd in the previous priority. CRAN refs try PPM
  ## binary paths first (Linux pre-compiled tarballs save build time even
  ## for older versions when PPM keeps them), then CRAN source.
  ppmRepos  <- newRepos[grepl("packagemanager.posit.co", newRepos, fixed = TRUE)]
  cranRepos <- newRepos[grepl("cran|cloud\\.r-project", newRepos)]
  if (!length(cranRepos)) cranRepos <- "https://cloud.r-project.org"

  buildUrls <- function(i) {
    if (isGH[i]) {
      return(paste0("https://github.com/", pkgs$GithubUsername[i], "/",
                    pkgs$GithubRepo[i], "/archive/", pkgs$GithubSHA1[i], ".tar.gz"))
    }
    pkg <- pkgs$Package[i]; ver <- pkgs$Version[i]
    ## A snapshot row's own Repository URL takes priority: rows pinning
    ## packages from r-universe / RSPM / etc. tell us exactly where the
    ## tarball lives, and PPM/CRAN won't have it. Try the row repo first;
    ## fall through to PPM/CRAN if it 404s (covers re-pointing later).
    rowRepo <- if (!is.null(pkgs$Repository)) pkgs$Repository[i] else NA_character_
    rowRepos <- if (length(rowRepo) && !is.na(rowRepo) &&
                    grepl("^https?://", rowRepo)) rowRepo
                else character()
    out <- character()
    for (r in c(rowRepos, ppmRepos, cranRepos)) {
      out <- c(out,
               paste0(r, "/src/contrib/", pkg, "_", ver, ".tar.gz"),
               paste0(r, "/src/contrib/Archive/", pkg, "/", pkg, "_", ver, ".tar.gz"))
    }
    unique(out)
  }
  candidates <- lapply(seq_len(nrow(pkgs)), buildUrls)
  ## destPath naming: pak's pkgdepends parses the package version out of
  ## the FILE NAME (`<pkg>_<version>.tar.gz`) and validates it against
  ## DESCRIPTION's Version field. If the snapshot row has a Version pin
  ## (which it usually does -- even GH rows have it for readability),
  ## use that. Falling back to the SHA-prefix for unversioned GH refs
  ## leaves us with `visualTest_9b835a7.tar.gz` -> filename version
  ## "9b835a7" != DESCRIPTION's "1.0.0" -> pak emits the misleading
  ## "Line starting 'visualTest/DESCRIPTI ...' is malformed!" error.
  destVersion <- ifelse(!is.na(pkgs$Version) & nzchar(pkgs$Version),
                        pkgs$Version,
                        ifelse(isGH, substr(pkgs$GithubSHA1, 1, 7), ""))
  destPaths <- file.path(dlDir,
                         paste0(pkgs$Package, "_", destVersion, ".tar.gz"))

  ## Parallel multi-pass downloader. Each pass: take the next candidate URL
  ## for every still-missing ref and pass them all to one libcurl multi call.
  ## libcurl multi can intermittently drop bytes mid-stream -- the file ends
  ## up with a valid `1f 8b` gzip header and even a complete tar header
  ## section (so `untar(list = TRUE)` happily lists files), but the gzip
  ## stream is truncated below the headers. pak's pkgdepends catches this
  ## later as "incomplete block on file" and kills the whole install.
  ## Catch it here instead by validating the gzip stream end-to-end with
  ## `gzip -t`, which scans every byte. Falls back to `untar(list = TRUE)`
  ## if `gzip` isn't on PATH (Windows without gzip in shell).
  haveGzip <- nzchar(Sys.which("gzip"))
  isGoodTarball <- function(p) {
    if (!file.exists(p) || file.size(p) < 100L) return(FALSE)
    if (haveGzip) {
      rc <- tryCatch(
        suppressWarnings(system2("gzip", c("-t", shQuote(p)),
                                 stdout = FALSE, stderr = FALSE)),
        error = function(e) 1L)
      if (!identical(as.integer(rc), 0L)) return(FALSE)
    }
    files <- tryCatch(suppressWarnings(utils::untar(p, list = TRUE)),
                      error = function(e) NULL)
    is.character(files) && length(files) > 0L
  }

  ## Validate that a cached tarball actually contains the package we expect.
  ## pkgcache entries can be wrong/stale -- we've seen entries indexed under
  ## (package = "fastdigest", version = "0.6-3") whose actual file content
  ## was a `pscl 1.5.9` tarball. Without this check, we'd accept the
  ## mismatch, blindly rename its inner dir to "fastdigest", run R CMD
  ## build, and produce a `pscl_1.5.9.tar.gz` (because R CMD build reads
  ## DESCRIPTION). Then no `fastdigest_*.tar.gz` exists, our destPaths
  ## update fails silently, and downstream install errors with
  ## `tar: fastdigest/DESCRIPTION: Not found in archive`.
  ##
  ## Cheap check: tar list the file, look for a `<dir>/DESCRIPTION` entry
  ## (any top-level dir is OK -- it'll get renamed in the repack step),
  ## then read just that DESCRIPTION via untar(files = ...) and check the
  ## Package: field. If mismatch, the cache hit is corrupt and the caller
  ## should skip it (re-download or use a different cache entry).
  cacheTarballMatchesPkg <- function(cachedFile, expectedPkg) {
    files <- tryCatch(suppressWarnings(utils::untar(cachedFile, list = TRUE)),
                      error = function(e) character())
    if (!length(files)) return(FALSE)
    descIdx <- which(grepl("^[^/]+/DESCRIPTION$", files))
    if (!length(descIdx)) return(FALSE)
    descPath <- files[descIdx[1]]
    extractTo <- tempfile2("snapInstall_descPeek_")
    dir.create(extractTo, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(extractTo, recursive = TRUE), add = TRUE)
    rc <- tryCatch(
      suppressWarnings(utils::untar(cachedFile, files = descPath,
                                    exdir = extractTo)),
      error = function(e) 1L)
    descFile <- file.path(extractTo, descPath)
    if (!identical(as.integer(rc), 0L) || !file.exists(descFile))
      return(FALSE)
    desc <- tryCatch(read.dcf(descFile, fields = "Package"),
                     error = function(e) NULL)
    if (is.null(desc) || nrow(desc) == 0L) return(FALSE)
    ## desc[1, "Package"] is a named character (matrix indexing), so
    ## identical() against a plain expectedPkg returns FALSE even when
    ## the values match. Compare via as.character + == instead.
    isTRUE(as.character(desc[1, "Package"]) == as.character(expectedPkg))
  }

  ## quiet = TRUE is mandatory for libcurl-multi to actually run downloads in
  ## parallel. With quiet = FALSE, R serializes the URLs through libcurl one
  ## at a time so it can attribute progress lines to individual files --
  ## defeating the whole point of the multi handle. Per-file progress is
  ## useless inside a 378-URL batch anyway; we print one "Downloading N
  ## tarballs" announcement above and that's all the user needs.
  ##
  ## Chunk the batch: libcurl multi opens a socket per URL. macOS's default
  ## file-descriptor limit is ~256, so a 378-URL single multi call exhausts
  ## the limit and fails *all* downloads silently (the user's symptom: 4
  ## retries each report "for 378 ref(s)" -- zero succeeded). Linux's higher
  ## default (>=1024) masks this. Chunking to 50 URLs per multi call keeps
  ## us comfortably under any platform's FD limit while preserving
  ## meaningful parallelism. Configurable via Require.snapshotDownloadChunk.
  chunkSize <- max(1L, as.integer(getOption(
    "Require.snapshotDownloadChunk", 50L)))
  pullBatch <- function(idx, urls) {
    starts <- seq.int(1L, length(idx), by = chunkSize)
    for (s in starts) {
      e <- min(s + chunkSize - 1L, length(idx))
      ci <- idx[s:e]
      cu <- urls[s:e]
      suppressWarnings(tryCatch(
        utils::download.file(cu, destPaths[ci], method = "libcurl",
                             quiet = TRUE, mode = "wb"),
        error = function(err) NULL))
    }
    vapply(idx, function(i) isGoodTarball(destPaths[i]), logical(1))
  }

  ## Pre-filter via pkgcache (pak's content-addressed cache, kept at
  ## tools::R_user_dir("pkgcache", "cache")). For each ref, look up an
  ## entry whose package + version matches the snapshot pin (or whose
  ## URL contains the GH SHA for a GH ref) and whose stored file still
  ## passes isGoodTarball. On hit, copy from the cache into dlDir and
  ## skip the download. This is the same cache pak's own pkg_install
  ## populates and reads from, so we share state with pak's flow.
  cachedHits <- logical(nrow(pkgs))
  ## Track our-platform binary hits separately for the binary-first
  ## hybrid pre-install. We can't feed binaries to pak via local:: refs
  ## (proven empirically: pak rejects those as "Platform mismatch"), but
  ## install.packages(type=binary) handles them fine -- and pre-installing
  ## binaries skips compilation entirely for the matching refs and
  ## reduces pak's parallel-build workload (where pak is fragile).
  binaryHits <- rep(NA_character_, nrow(pkgs))
  cacheList <- NULL
  ## Eviction list: URLs (or fullpaths when URL is NA) of cache entries
  ## that fail validation during pre-filter -- fullpath missing on disk,
  ## tarball corrupt by gzip-t, or DESCRIPTION's Package field doesn't
  ## match what the index claims. Without eviction these rotten entries
  ## persist across runs and force the same 6 packages to re-download
  ## every time. We collect, then call pkg_cache_delete_url after the
  ## loop (delete during iteration would invalidate our cacheList view).
  evictUrls  <- character()
  evictFiles <- character()
  evictReasons <- character()
  if (requireNamespace("pkgcache", quietly = TRUE)) {
    cacheList <- tryCatch(pkgcache::pkg_cache_list(),
                          error = function(e) NULL)
    ## Bulk-evict legacy entries whose `path` is exactly "Require/snapshot"
    ## (no trailing filename). These are residue from a now-fixed bug where
    ## `pkg_cache_add_file(relpath = "Require/snapshot")` was called
    ## without including the filename -- every add silently overwrote the
    ## same single file at <cache>/Require/snapshot, leaving an index
    ## entry per package pointing at the same fullpath. Even after we
    ## fixed relpath, those legacy index rows persist and confuse pak's
    ## resolver (it sees package X version Y indexed and may use that in
    ## preference to our local:: ref). Clean them in one sweep.
    if (!is.null(cacheList) && nrow(cacheList) > 0) {
      ## Match BOTH legacy patterns:
      ##   "Require/snapshot"                                 (source bug)
      ##   "Require/snapshot/bin/<platform>/<rverShort>"      (binary bug
      ##      fixed in same session -- cacheBuiltBinaries had identical
      ##      relpath-without-filename issue, producing 142 rows aliasing
      ##      the same directory-shaped fullpath)
      legacyIdx <- !is.na(cacheList$path) & (
        cacheList$path == "Require/snapshot" |
        grepl("^Require/snapshot/bin/[^/]+/\\d+\\.\\d+$",
              cacheList$path))
      if (any(legacyIdx)) {
        legacyN <- sum(legacyIdx)
        if (verbose >= 1)
          messageVerbose("Evicting ", legacyN,
                         " legacy 'Require/snapshot' cache entries ",
                         "(filename-stripped relpath, all aliasing the ",
                         "same single file -- pre-fix residue)",
                         verbose = verbose, verboseLevel = 1)
        legacyRows <- cacheList[legacyIdx, , drop = FALSE]
        for (i in seq_len(nrow(legacyRows))) {
          u <- legacyRows$url[i]
          if (!is.na(u) && nzchar(u)) {
            tryCatch(pkgcache::pkg_cache_delete_files(url = u),
                     error = function(e) invisible())
          } else if (!is.na(legacyRows$fullpath[i])) {
            tryCatch(pkgcache::pkg_cache_delete_files(
                       fullpath = legacyRows$fullpath[i]),
                     error = function(e) invisible())
          }
        }
        ## Refresh cacheList; the rest of the pre-filter uses it.
        cacheList <- tryCatch(pkgcache::pkg_cache_list(),
                              error = function(e) cacheList)
      }
    }
    if (verbose >= 1) {
      cacheRoot <- tryCatch(
        tools::R_user_dir("pkgcache", "cache"),
        error = function(e) NA_character_)
      messageVerbose(
        "pkgcache state: ",
        if (is.null(cacheList)) "unavailable"
        else paste0(nrow(cacheList), " entries at ", cacheRoot),
        verbose = verbose, verboseLevel = 1)
    }
    if (!is.null(cacheList) && nrow(cacheList) > 0) {
      ourRverShort <- paste0(R.version$major, ".",
                              strsplit(R.version$minor, "\\.")[[1]][1])
      ## pkgcache's index columns each tell only part of the story
      ## about whether a row is source or binary, and for which R:
      ##   - `built`:    TRUE means binary; FALSE means source; NA = ?
      ##   - `platform`: name of platform OR "source"; sometimes NA
      ##   - `rversion`: R version; mostly NA
      ##   - `path`:     structural path within cache, always present:
      ##       bin/macosx/<arch>/contrib/<rver>/<pkg>_<ver>.tgz   (mac bin)
      ##       bin/windows/contrib/<rver>/<pkg>_<ver>.zip          (win bin)
      ##       src/contrib/<pkg>_<ver>.tar.gz                       (source)
      ##       src/contrib/<pkg>_<ver>.tar.gz-<plat>-<rver>         (built bin)
      ##       src/contrib/__linux__/<distro>/<rver>/<pkg>_<ver>... (PPM lin)
      ## So combine: a row is binary if ANY of (built==TRUE, path
      ## starts with bin/, path encodes a `-<platform>-<rver>` suffix).
      ## R version: trust `rversion` column first, else parse path.
      ## File extension on disk also matters: a `.tgz`/`.zip` is binary
      ## even if path / built suggest otherwise.
      isBinFromPath <- function(path, fp) {
        !is.na(path) & (
          grepl("(^|/)bin/", path) |
          grepl("\\.tar\\.gz-[^/]+-[0-9]+\\.[0-9]+$", path)
        ) |
        !is.na(fp) & (
          grepl("\\.tgz$", fp, ignore.case = TRUE) |
          grepl("\\.zip$", fp, ignore.case = TRUE)
        )
      }
      rverFromPath <- function(path) {
        ## Use `sub()` (vectorised, returns same-length output as input)
        ## instead of `regmatches(path, regexpr(...))` which DROPS
        ## non-matching elements and leaves us with mismatched-length
        ## logical masks that R recycles -- the source of 299 spurious
        ## "longer object length is not a multiple..." warnings on
        ## every snapshot install. With `sub`: when the pattern doesn't
        ## match, the input element passes through unchanged; pair with
        ## a separate `grepl` test to detect "matched vs not".
        out <- rep(NA_character_, length(path))
        ## /contrib/<rver>/<file>
        m1 <- grepl("/contrib/\\d+\\.\\d+/", path)
        if (any(m1))
          out[m1] <- sub(".*/contrib/(\\d+\\.\\d+)/.*", "\\1", path[m1])
        ## /__linux__/<distro>/<rver>/<file>
        m2 <- is.na(out) & grepl("/__linux__/[^/]+/\\d+\\.\\d+/", path)
        if (any(m2))
          out[m2] <- sub(".*/__linux__/[^/]+/(\\d+\\.\\d+)/.*", "\\1",
                         path[m2])
        ## Built-binary suffix at end: -<platform>-<rver>(.<patch>)?
        m3 <- is.na(out) & grepl("-[^/]+-\\d+\\.\\d+(\\.\\d+)?$", path)
        if (any(m3))
          out[m3] <- sub(".*-(\\d+\\.\\d+)(\\.\\d+)?$", "\\1", path[m3])
        out
      }
      for (i in seq_len(nrow(pkgs))) {
        if (isGH[i]) {
          ## GH refs match BOTH (a) URL containing the GH SHA needle
          ## (catches the original GH archive download cached by pak)
          ## AND (b) package + version (catches built-binary entries
          ## added by our cacheBuiltBinaries -- those have URL like
          ## `require-snapshot-bin://...` with no GH needle but populated
          ## package/version columns). Without (b), every run rebuilds
          ## visualTest from source even though the binary IS cached.
          urlNeedle <- paste0(pkgs$GithubUsername[i], "/",
                              pkgs$GithubRepo[i], "/archive/",
                              pkgs$GithubSHA1[i])
          hit <- cacheList[(grepl(urlNeedle, cacheList$url, fixed = TRUE) |
                            (!is.na(cacheList$package) &
                             !is.na(cacheList$version) &
                             cacheList$package == pkgs$Package[i] &
                             cacheList$version == pkgs$Version[i])) &
                            !is.na(cacheList$fullpath), , drop = FALSE]
        } else {
          hit <- cacheList[!is.na(cacheList$package) &
                            !is.na(cacheList$version) &
                            cacheList$package == pkgs$Package[i] &
                            cacheList$version == pkgs$Version[i] &
                            !is.na(cacheList$fullpath), , drop = FALSE]
        }
        if (!nrow(hit)) next
        ## Filter: pak's `local::<file>` ref handling is SOURCE-ONLY.
        ## Binaries (any platform, any rversion) trigger "Platform
        ## mismatch" in pak's resolver -- confirmed empirically with a
        ## minimal reproducer. So drop binary entries from the local::
        ## pipeline; only source tarballs feed local:: refs cleanly.
        ##
        ## BUT: our-platform R-version-matching binaries are useful for
        ## the install.packages(type=binary) hybrid pre-install path --
        ## those skip compilation entirely. Track them separately in
        ## `binaryHits[i]`. The hybrid kicks in below, before pak runs.
        ##
        ## Detection combines all available signals: trust the `built`
        ## column when populated, then path layout (bin/ prefix or
        ## "<plat>-<rver>" suffix), then file extension on disk
        ## (.tgz / .zip).
        builtCol <- !is.na(hit$built) & as.logical(hit$built)
        isBinPath <- isBinFromPath(hit$path, hit$fullpath)
        isBin <- builtCol | isBinPath
        ## Identify our-platform binary candidates among isBin rows.
        ## Normalize rverEff to major.minor: pkgcache stores some built-
        ## binary entries with rversion="4.4.3" (full R.version$minor),
        ## others with the path-derived "4.4". A strict equality of "4.4"
        ## misses ~80% of valid R-4.4-family binaries. Strip patch level.
        rverEff <- ifelse(!is.na(hit$rversion), hit$rversion,
                          rverFromPath(hit$path))
        rverEff <- sub("^(\\d+\\.\\d+).*$", "\\1", rverEff)
        ourArchPrefix <- sub("^([^-]+-[^-]+)-.*", "\\1",
                              R.version$platform)
        platMatchesOur <- !is.na(hit$platform) & (
          hit$platform == R.version$platform |
          startsWith(hit$platform, paste0(ourArchPrefix, "-")))
        ourBin <- isBin & !is.na(rverEff) & rverEff == ourRverShort &
                  platMatchesOur
        if (any(ourBin)) {
          ## Pick the first usable our-platform binary. Validate it the
          ## same way as source hits (gzip-t + DESCRIPTION Package match).
          ## Queue rotten ones for eviction so future runs hit cleanly --
          ## ~138 cache entries empirically had pkg-name mismatches (the
          ## tarball's DESCRIPTION names a different package than the
          ## index claims), residue from older buggy adds.
          for (k in which(ourBin)) {
            cb <- hit$fullpath[k]
            reasonBin <- NA_character_
            if (!file.exists(cb))                    reasonBin <- "fullpath-missing"
            else if (!isGoodTarball(cb))             reasonBin <- "tarball-corrupt"
            else if (!cacheTarballMatchesPkg(cb,
                       pkgs$Package[i]))             reasonBin <- "pkg-name-mismatch"
            if (is.na(reasonBin)) {
              binaryHits[i] <- cb
              break
            }
            u <- hit$url[k]
            if (!is.na(u) && nzchar(u)) {
              evictUrls    <- c(evictUrls, u)
              evictReasons <- c(evictReasons,
                                paste0(pkgs$Package[i], " (binary): ",
                                       reasonBin))
            } else {
              evictFiles   <- c(evictFiles, cb)
              evictReasons <- c(evictReasons,
                                paste0(pkgs$Package[i], " (binary): ",
                                       reasonBin, " (no url)"))
            }
          }
        }
        keep <- !isBin
        hit <- hit[keep, , drop = FALSE]
        if (!nrow(hit)) next
        ## Walk hits in priority order, validating each. The previous
        ## logic only tried hit[1]; a single rotten top-priority entry
        ## (corrupt file, DESCRIPTION mismatch, missing fullpath)
        ## blocked the whole ref even if other valid hits existed.
        for (k in seq_len(nrow(hit))) {
          cached <- hit$fullpath[k]
          reason <- NA_character_
          if (!file.exists(cached))                   reason <- "fullpath-missing"
          else if (!isGoodTarball(cached))            reason <- "tarball-corrupt"
          else if (!cacheTarballMatchesPkg(cached,
                     pkgs$Package[i]))                reason <- "pkg-name-mismatch"
          if (is.na(reason)) {
            file.copy(cached, destPaths[i], overwrite = TRUE)
            cachedHits[i] <- TRUE
            break
          }
          ## Queue this rotten entry for eviction so it doesn't keep
          ## blocking future runs. Prefer URL-keyed delete (pkgcache's
          ## natural API); fall back to fullpath delete for entries
          ## with NA URLs (some legacy adds left these).
          u <- hit$url[k]
          if (!is.na(u) && nzchar(u)) {
            evictUrls   <- c(evictUrls, u)
            evictReasons <- c(evictReasons, paste0(pkgs$Package[i], ": ", reason))
          } else {
            evictFiles  <- c(evictFiles, cached)
            evictReasons <- c(evictReasons, paste0(pkgs$Package[i], ": ", reason, " (no url)"))
          }
        }
      }
    }
  }
  ## Evict the rotten cache entries we found. Best-effort: a failure
  ## shouldn't block the install, since the pre-filter already routed
  ## around them.
  if (length(evictUrls) || length(evictFiles)) {
    if (verbose >= 1)
      messageVerbose("Evicting ", length(evictUrls) + length(evictFiles),
                     " corrupt pkgcache entries: ",
                     paste(utils::head(unique(evictReasons), 6),
                           collapse = "; "),
                     if (length(unique(evictReasons)) > 6) " ..." else "",
                     verbose = verbose, verboseLevel = 1)
    for (u in unique(evictUrls)) {
      tryCatch(pkgcache::pkg_cache_delete_files(url = u),
               error = function(e) invisible())
    }
    for (f in unique(evictFiles)) {
      tryCatch(pkgcache::pkg_cache_delete_files(fullpath = f),
               error = function(e) invisible())
    }
  }
  if (any(cachedHits)) {
    messageVerbose(sum(cachedHits), " of ", nrow(pkgs),
                   " snapshot tarballs hit pkgcache (pak's cache); ",
                   "skipping download for those",
                   verbose = verbose, verboseLevel = 1)
  } else if (verbose >= 1 && !is.null(cacheList) && nrow(cacheList) > 0) {
    ## Cache has entries but none matched our snapshot. Show a sample
    ## so the user can spot mismatches (e.g., URL format differences,
    ## missing package/version columns).
    sampleN <- min(3L, nrow(cacheList))
    cat("[snapshotInstaller] no cache hits for snapshot. Sample of ",
        sampleN, " cache entries (of ", nrow(cacheList), "):\n", sep = "")
    safeNA <- function(x) if (is.null(x) || is.na(x)) "NA" else as.character(x)
    for (k in seq_len(sampleN)) {
      cat("  pkg=", safeNA(cacheList$package[k]),
          " ver=", safeNA(cacheList$version[k]),
          " url=", safeNA(cacheList$url[k]), "\n", sep = "")
    }
  }
  needed <- which(!cachedHits)
  maxPriority <- max(lengths(candidates))
  ## Retry the full priority loop up to maxAttempts times. Each attempt
  ## walks every priority URL for every still-missing ref. For users on
  ## flaky connections (transient DNS/timeout/partial-read failures) the
  ## first attempt may drop a few refs that the second attempt picks up
  ## cleanly. Exponential backoff between attempts gives upstream a moment
  ## to recover. Configurable via options(Require.snapshotDownloadAttempts).
  maxAttempts <- max(1L, as.integer(getOption(
    "Require.snapshotDownloadAttempts", 4L)))
  dlT0 <- Sys.time()
  origNeeded <- length(needed)
  for (attempt in seq_len(maxAttempts)) {
    if (!length(needed)) break
    if (attempt == 1L) {
      messageVerbose("Downloading ", length(needed),
                     " snapshot tarballs in parallel via libcurl ",
                     "(walks priority URLs: row Repo, then PPM, then CRAN; ",
                     "up to ", maxAttempts, " attempts on flaky connections)",
                     verbose = verbose, verboseLevel = 1)
    } else {
      delay <- min(60L, 2L ^ (attempt - 1L))
      messageVerbose("Retry attempt ", attempt, " of ", maxAttempts,
                     " for ", length(needed), " ref(s) after ",
                     delay, "s backoff",
                     verbose = verbose, verboseLevel = 1)
      Sys.sleep(delay)
    }
    for (priority in seq_len(maxPriority)) {
      if (!length(needed)) break
      has <- vapply(needed, function(i) priority <= length(candidates[[i]]),
                    logical(1))
      if (!any(has)) break
      sub_idx  <- needed[has]
      sub_urls <- vapply(sub_idx, function(i) candidates[[i]][priority],
                         character(1))
      ok <- pullBatch(sub_idx, sub_urls)
      needed <- needed[!(needed %in% sub_idx[ok])]
    }
  }
  if (origNeeded > 0L && verbose >= 1) {
    dlSecs <- as.numeric(difftime(Sys.time(), dlT0, units = "secs"))
    gotN <- origNeeded - length(needed)
    messageVerbose(sprintf(
      "  ... downloaded %d/%d in %.1fs (%.1fs/pkg)%s",
      gotN, origNeeded, dlSecs,
      if (gotN > 0L) dlSecs / gotN else 0,
      if (length(needed)) paste0(" -- ", length(needed),
                                  " still missing, trying archived versions next")
      else ""),
      verbose = verbose, verboseLevel = 1)
  }

  ## For any ref still missing, try the nearest available archived version
  ## (one-by-one, since each ref needs its own pkg_history lookup).
  substituted <- character()
  if (length(needed)) {
    for (i in needed) {
      if (isGH[i]) next
      sub <- findNearestArchivedVersion(pkgs$Package[i], pkgs$Version[i],
                                        verbose = verbose)
      if (is.null(sub) || !nzchar(sub)) next
      tryUrls <- character()
      for (r in c(ppmRepos, cranRepos)) {
        tryUrls <- c(tryUrls,
                     paste0(r, "/src/contrib/", pkgs$Package[i], "_", sub, ".tar.gz"),
                     paste0(r, "/src/contrib/Archive/", pkgs$Package[i],
                            "/", pkgs$Package[i], "_", sub, ".tar.gz"))
      }
      newDest <- file.path(dlDir, paste0(pkgs$Package[i], "_", sub, ".tar.gz"))
      hit <- FALSE
      for (u in tryUrls) {
        ## quiet = TRUE: per-URL "trying URL" spam at verbose = 2 doesn't
        ## help the user -- we already log per-package substitution status
        ## via the messageVerbose calls below.
        suppressWarnings(tryCatch(
          utils::download.file(u, newDest, method = "libcurl",
                               quiet = TRUE, mode = "wb"),
          error = function(e) NULL))
        if (isGoodTarball(newDest)) { hit <- TRUE; break }
      }
      if (hit) {
        substituted <- c(substituted,
                         sprintf("%s: %s -> %s", pkgs$Package[i],
                                 pkgs$Version[i], sub))
        pkgs$Version[i] <- sub
        destPaths[i] <- newDest
        cachedHits[i] <- FALSE  # version changed, treat as fresh download
      }
    }
    needed <- needed[!file.exists(destPaths[needed]) | !vapply(destPaths[needed], isGoodTarball, logical(1))]
  }
  if (length(substituted)) {
    messageVerbose(length(substituted),
                   " refs substituted with nearest archived version:",
                   verbose = verbose, verboseLevel = 1)
    if (verbose >= 1) cat(paste0("  ", substituted), sep = "\n")
  }
  unresolvedRefs <- character()
  if (length(needed)) {
    messageVerbose(length(needed), " of ", nrow(pkgs),
                   " refs failed to download and will be skipped",
                   verbose = verbose, verboseLevel = 1)
    if (verbose >= 1) {
      cat("[snapshotInstaller] unresolvable refs:\n")
      cat(paste0("  ", pkgs$Package[needed], "@", pkgs$Version[needed]),
          sep = "\n")
    }
    ## Capture the unresolved set before mutating pkgs so the post-install
    ## diagnostic can distinguish "couldn't download" from "failed to build".
    unresolvedRefs <- setNames(pkgs$Version[needed], pkgs$Package[needed])
    pkgs   <- pkgs[-needed, , drop = FALSE]
    isGH   <- isGH[-needed]
    destPaths <- destPaths[-needed]
    cachedHits <- cachedHits[-needed]
  }
  if (!nrow(pkgs)) stop("All snapshot refs failed to download")

  ## Repackage any tarball whose contents don't have <pkg>/DESCRIPTION
  ## at top level into a proper R source tarball.
  ##
  ## Source tarballs from `git archive` (GitHub archive endpoint, OR
  ## r-universe / r-builders that use `git archive` internally) are
  ## broken as R-package source tarballs in two ways:
  ##   1. Start with a `pax_global_header` tar entry (git's metadata).
  ##      pak's pkgdepends tar reader chokes:
  ##        "! Line starting 'pax_global_header ...' is malformed!"
  ##      -> pak refuses the whole install plan (all-or-nothing).
  ##   2. Top-level dir is `<repo>-<sha>/` (or similar) not `<pkg>/`,
  ##      so install.packages via a file:// repo path fails:
  ##        `tar: <pkg>/DESCRIPTION not found in archive`.
  ##
  ## Affected packages aren't always GH-coord rows -- fastdigest, knn,
  ## spatstat.core etc. came from CRAN-archive style sources but their
  ## tarballs were also git-archive built. So we DETECT (any tarball
  ## whose tar listing doesn't include `<pkg>/DESCRIPTION` at top) and
  ## REPACKAGE via R CMD build (canonical R source tarball, no pax,
  ## proper inner dir).
  ##
  ## We also rename to `<pkg>_<DescriptionVersion>.tar.gz` because
  ## pak's resolver validates filename version against DESCRIPTION
  ## Version and emits the misleading "Line starting '<pkg>/DESCRIPTI
  ## ...' is malformed!" on mismatch -- actually a version-mismatch error.
  if (nzchar(Sys.which("tar"))) {
    needsRepack <- vapply(seq_len(nrow(pkgs)), function(i) {
      if (!file.exists(destPaths[i]) || !isGoodTarball(destPaths[i]))
        return(FALSE)
      pkgName <- pkgs$Package[i]
      ## Cheap check: does the tar listing include `<pkg>/DESCRIPTION`?
      files <- tryCatch(
        suppressWarnings(utils::untar(destPaths[i], list = TRUE)),
        error = function(e) character())
      !any(files == paste0(pkgName, "/DESCRIPTION"))
    }, logical(1))
    needIdxs <- which(needsRepack)
    if (length(needIdxs)) {
      repacked <- 0L
      Rbin <- file.path(R.home("bin"), "R")
      for (i in needIdxs) {
        pkgName <- pkgs$Package[i]
        workDir <- tempfile2("snapInstall_repack_")
        dir.create(workDir, recursive = TRUE, showWarnings = FALSE)
        rcExtract <- tryCatch(
          system2("tar", c("xzf", shQuote(destPaths[i]),
                           "-C", shQuote(workDir)),
                  stdout = FALSE, stderr = FALSE),
          error = function(e) -1L)
        if (!identical(as.integer(rcExtract), 0L)) {
          if (verbose >= 1)
            cat("[snapshotInstaller] tar extract failed for ",
                pkgName, "\n", sep = "")
          unlink(workDir, recursive = TRUE); next
        }
        inner <- list.files(workDir, full.names = TRUE)
        isDir <- file.info(inner)$isdir
        isDir[is.na(isDir)] <- FALSE
        inner <- inner[isDir]
        if (!length(inner)) {
          if (verbose >= 1)
            cat("[snapshotInstaller] no inner dir found in ",
                pkgName, " tarball\n", sep = "")
          unlink(workDir, recursive = TRUE); next
        }
        target <- file.path(workDir, pkgName)
        if (!identical(basename(inner[1]), pkgName)) {
          if (!file.rename(inner[1], target)) {
            if (verbose >= 1)
              cat("[snapshotInstaller] rename inner dir failed for ",
                  pkgName, " (", basename(inner[1]), " -> ", pkgName,
                  ")\n", sep = "")
            unlink(workDir, recursive = TRUE); next
          }
        }
        ## Capture R CMD build stdout+stderr to diagnose silent failures.
        oldwd <- setwd(workDir)
        rcBuild <- tryCatch(
          system2(Rbin, c("CMD", "build", "--no-build-vignettes",
                          "--no-manual", shQuote(pkgName)),
                  stdout = TRUE, stderr = TRUE),
          error = function(e) e)
        setwd(oldwd)
        ## With stdout=TRUE+stderr=TRUE, system2 returns the captured
        ## output (character vector) plus an attribute "status" if exit
        ## was non-zero. error from tryCatch covers spawn failure.
        if (inherits(rcBuild, "error")) {
          if (verbose >= 1)
            cat("[snapshotInstaller] R CMD build spawn failed for ",
                pkgName, ": ", conditionMessage(rcBuild), "\n", sep = "")
          unlink(workDir, recursive = TRUE); next
        }
        rcStatus <- attr(rcBuild, "status")
        if (!is.null(rcStatus) && !identical(as.integer(rcStatus), 0L)) {
          if (verbose >= 1) {
            tail6 <- utils::tail(rcBuild, 6)
            cat("[snapshotInstaller] R CMD build failed for ", pkgName,
                " (rc=", rcStatus, "). Last lines:\n  ",
                paste(tail6, collapse = "\n  "), "\n", sep = "")
          }
          unlink(workDir, recursive = TRUE); next
        }
        built <- list.files(workDir, pattern = paste0("^",
                                                       pkgName,
                                                       "_.*\\.tar\\.gz$"),
                            full.names = TRUE)
        if (!length(built)) {
          if (verbose >= 1) {
            allFiles <- list.files(workDir, recursive = FALSE)
            tail6 <- utils::tail(rcBuild, 6)
            cat("[snapshotInstaller] R CMD build produced no tarball for ",
                pkgName, ". workDir contents: ",
                paste(allFiles, collapse = ", "),
                "\n  R CMD build output (last 6 lines):\n  ",
                paste(tail6, collapse = "\n  "), "\n", sep = "")
          }
          unlink(workDir, recursive = TRUE); next
        }
        newDest <- file.path(dirname(destPaths[i]), basename(built[1]))
        if (!file.copy(built[1], newDest, overwrite = TRUE)) {
          unlink(workDir, recursive = TRUE); next
        }
        if (newDest != destPaths[i]) {
          unlink(destPaths[i])  # discard old sha- or version-mismatched copy
          destPaths[i] <- newDest
        }
        if (isGoodTarball(destPaths[i])) {
          repacked <- repacked + 1L
          ## Treat repacked refs as fresh: their content differs from
          ## whatever was in the cache, so post-download cache-add must
          ## register the new tarball.
          cachedHits[i] <- FALSE
        }
        unlink(workDir, recursive = TRUE)
      }
      if (verbose >= 1)
        messageVerbose(
          "Repackaged ", repacked, "/", length(needIdxs),
          " tarball(s) with non-standard top-level dir via R CMD build ",
          "(", nrow(pkgs) - length(needIdxs), " tarballs already had ",
          "<pkg>/DESCRIPTION top-level -- no repack needed). ",
          "Repacking is required for pak's resolver and install.packages's ",
          "file:// repo path.",
          verbose = verbose, verboseLevel = 1)
    }
  }

  ## Populate pkgcache with the refs we just fetched / repackaged so
  ## subsequent runs find them in the pre-filter above. Gate on
  ## cachedHits[i]: refs that came from a valid cache hit are unchanged
  ## from what's in cache and don't need a re-add. Refs that were
  ## downloaded fresh, version-substituted, or repackaged via R CMD
  ## build had cachedHits[i] reset to FALSE, so they get re-added here.
  ##
  ## CRITICAL: relpath must be the FULL relative path INCLUDING filename.
  ## pkgcache's add() copies the file to file.path(<cache>, relpath) -- so
  ## passing "Require/snapshot" as relpath (without filename) means every
  ## call overwrites the same single file at <cache>/Require/snapshot.
  ## That's how this cache accumulated corrupt entries (e.g. fastdigest's
  ## index row pointing at a file whose DESCRIPTION says pscl 1.5.9 -- the
  ## "last writer wins" produces silent corruption). Always include
  ## basename(destPaths[i]) so each ref gets its own file.
  ## Best-effort: a failed cache_add should NEVER block the install.
  if (requireNamespace("pkgcache", quietly = TRUE)) {
    addedCount <- 0L
    skippedHit <- 0L
    for (i in seq_len(nrow(pkgs))) {
      if (cachedHits[i]) { skippedHit <- skippedHit + 1L; next }
      if (!file.exists(destPaths[i]) || !isGoodTarball(destPaths[i])) next
      ## URL we should record: prefer the row's Repository, else the
      ## first PPM/CRAN candidate we'd have used. For GH, the canonical
      ## archive URL.
      url <- if (isGH[i]) {
        paste0("https://github.com/", pkgs$GithubUsername[i], "/",
               pkgs$GithubRepo[i], "/archive/",
               pkgs$GithubSHA1[i], ".tar.gz")
      } else {
        rowRepo <- pkgs$Repository[i]
        baseRepo <- if (!is.na(rowRepo) && grepl("^https?://", rowRepo))
                      rowRepo
                    else if (length(ppmRepos)) ppmRepos[1]
                    else cranRepos[1]
        paste0(baseRepo, "/src/contrib/", pkgs$Package[i],
               "_", pkgs$Version[i], ".tar.gz")
      }
      relpath <- file.path("Require", "snapshot", basename(destPaths[i]))
      addRes <- tryCatch(
        pkgcache::pkg_cache_add_file(
          file = destPaths[i],
          relpath = relpath,
          url = url,
          package = pkgs$Package[i],
          version = pkgs$Version[i]),
        error = function(e) e)
      if (inherits(addRes, "error")) {
        if (verbose >= 1)
          cat("[snapshotInstaller] pkg_cache_add_file failed for ",
              pkgs$Package[i], ": ",
              conditionMessage(addRes), "\n", sep = "")
      } else {
        addedCount <- addedCount + 1L
      }
    }
    if (verbose >= 1)
      messageVerbose("Added ", addedCount, " tarball(s) to pkgcache; ",
                     skippedHit, " already cached (pre-filter hit)",
                     verbose = verbose, verboseLevel = 1)
  }

  ## Snapshot-coherence pre-check: read each ref's DESCRIPTION and look
  ## for Imports/Depends/LinkingTo version constraints that the
  ## snapshot's other pins don't satisfy. The classic failure mode for
  ## a manually-curated snapshot is "we bumped servr but not its
  ## dependency xfun" -- pak then can't solve and refuses everything.
  ## install.packages is permissive about this and installs anyway, but
  ## pak is strict, so a single missed bump blocks the whole snapshot.
  ## Surface these conflicts up front so the user can patch the
  ## snapshot before the slow install starts.
  pinned <- setNames(pkgs$Version, pkgs$Package)
  parseConstraints <- function(field) {
    if (is.null(field) || is.na(field) || !nzchar(field)) return(NULL)
    parts <- strsplit(field, "[,\n]+")[[1]]
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    out <- lapply(parts, function(p) {
      m <- regmatches(p, regexec("^([A-Za-z0-9._]+)(\\s*\\(\\s*([<>=!]+)\\s*([0-9.\\-]+)\\s*\\))?$", p))[[1]]
      if (length(m) < 2 || !nzchar(m[2])) return(NULL)
      list(pkg = m[2], op = if (length(m) >= 4) m[4] else "",
           ver = if (length(m) >= 5) m[5] else "")
    })
    out[!vapply(out, is.null, logical(1))]
  }
  conflicts <- character()
  for (i in seq_len(nrow(pkgs))) {
    if (!file.exists(destPaths[i]) || !isGoodTarball(destPaths[i])) next
    files <- tryCatch(suppressWarnings(utils::untar(destPaths[i], list = TRUE)),
                      error = function(e) character())
    descIdx <- which(grepl("^[^/]+/DESCRIPTION$", files))
    if (!length(descIdx)) next
    ex <- tempfile2("snapInstall_descChk_")
    dir.create(ex, recursive = TRUE, showWarnings = FALSE)
    rcU <- tryCatch(suppressWarnings(utils::untar(destPaths[i],
                                                   files = files[descIdx[1]],
                                                   exdir = ex)),
                    error = function(e) 1L)
    descFile <- file.path(ex, files[descIdx[1]])
    if (!identical(as.integer(rcU), 0L) || !file.exists(descFile)) {
      unlink(ex, recursive = TRUE); next
    }
    dcf <- tryCatch(read.dcf(descFile,
                             fields = c("Depends","Imports","LinkingTo")),
                    error = function(e) NULL)
    unlink(ex, recursive = TRUE)
    if (is.null(dcf) || !nrow(dcf)) next
    for (col in colnames(dcf)) {
      cs <- parseConstraints(dcf[1, col])
      for (c in cs) {
        if (c$pkg %in% c("R", .basePkgs)) next
        if (!c$pkg %in% names(pinned)) next  # unpinned dep is OK (not a snapshot conflict)
        if (!nzchar(c$op) || !nzchar(c$ver)) next
        installed <- pinned[[c$pkg]]
        cmp <- tryCatch(utils::compareVersion(installed, c$ver),
                        error = function(e) 0L)
        ok <- switch(c$op,
                     ">=" = cmp >= 0,
                     ">"  = cmp >  0,
                     "<=" = cmp <= 0,
                     "<"  = cmp <  0,
                     "==" = cmp == 0,
                     "!=" = cmp != 0,
                     TRUE)
        if (!isTRUE(ok)) {
          conflicts <- c(conflicts, sprintf(
            "%s %s requires %s %s %s; snapshot pins %s = %s",
            pkgs$Package[i], pkgs$Version[i],
            c$pkg, c$op, c$ver, c$pkg, installed))
        }
      }
    }
  }
  if (length(conflicts)) {
    if (verbose >= 1) {
      messageVerbose(
        "Snapshot version-coherence pre-check found ", length(conflicts),
        " unsatisfied dep constraint(s); pak's solver will refuse these. ",
        "Bump these pins to make the snapshot coherent:",
        verbose = verbose, verboseLevel = 1)
      for (c in conflicts) cat("  -", c, "\n")
    }
  }

  ## Install. pak::pkg_install(local::..., dependencies = NA) is the
  ## primary path because pak maintains a binary cache that reuses
  ## compiled tarballs from previous source builds. We populate the
  ## same cache (pkgcache::pkg_cache_add_file above + cacheBuiltBinaries
  ## via on.exit) so pak finds binaries even for refs it didn't fetch
  ## itself.
  ##
  ## EMPIRICAL NOTE: pak's resolver doesn't fully respect local:: refs
  ## as the closed graph for transitive deps. Even with all 378 refs
  ## passed in, pak's pkgdepends queries CRAN/PPM for each transitive
  ## dependency name and may pick a NEWER version (e.g. snapshot pins
  ## ggplot2_3.4.4 but PPM has 4.0.3 -> pak tries to fetch 4.0.3 instead
  ## of using our local 3.4.4). When transitively-needed packages have
  ## "future" pin versions on PPM that pak considers but can't actually
  ## fetch (URL exists but version mismatch with another constraint),
  ## the whole solve fails with "! error in pak subprocess".
  ##
  ## install.packages is the fallback. It's permissive: with
  ## dependencies = NA + a closed file:// repo containing every
  ## snapshot ref, it computes topological order from PACKAGES and
  ## installs without re-resolving against external repos. Works
  ## reliably for closed snapshots even when pak refuses.
  ##
  ## Diagnosed pak failures we DID fix (in this commit history):
  ##   - visualTest GH archive's pax_global_header -> repackage via R
  ##     CMD build (above)
  ##   - GH tarball filename version != DESCRIPTION Version -> rename
  ##     destPaths[i] to <pkg>_<DescriptionVersion>.tar.gz (above)
  ## Remaining "pak refused" is the version-resolver quirk noted above
  ## and is fundamental to how pak's pkgdepends works; the install.packages
  ## fallback is correct for our closed-snapshot use case.
  ##
  ## Set options(Require.snapshotInstallerPakSilent = TRUE) to sink
  ## pak's noisy resolver output to a tempfile during the attempt.
  ## Hybrid binary-first pre-install. For refs where pkgcache has an
  ## our-platform R-version-matching binary, install via
  ## install.packages(type=binary) BEFORE pak. This:
  ##   1) skips compilation entirely for those refs (fast),
  ##   2) reduces pak's parallel-build workload (pak aborts the whole
  ##      install on a single build failure -- fewer compiles in pak's
  ##      hands = fewer chances to hit that abort),
  ##   3) populates destLib so pak's `upgrade = FALSE` short-circuits
  ##      those refs as "already installed at requested version".
  ## Disable with options(Require.snapshotInstallerHybrid = FALSE).
  ##
  ## NOTE on why we don't pass `cran::pkg@version` refs to pak instead:
  ## an experiment (see commit notes & tag pre-pak-url-refactor) confirmed
  ## that for archived-version refs (e.g. arrow@23.0.1.1, where 24.0.0 is
  ## current), pak's resolver only constructs SOURCE-Archive URLs -- it
  ## never tries a binary URL because CRAN never builds binaries for
  ## non-current versions. So even with our binary in pkgcache, pak would
  ## rebuild from source. Snapshot installs are dominated by archived
  ## versions, so the install.packages(type=binary) hybrid is what makes
  ## binaries usable at all -- it's not redundant. local:: refs play the
  ## same role for source: they bypass pak's resolver (which would also
  ## attempt a fresh download from the Archive URL).
  preInstalled <- character()
  hybridOn <- isTRUE(getOption("Require.snapshotInstallerHybrid", TRUE))
  if (hybridOn) {
    binIdx <- which(!is.na(binaryHits) & !cachedHits[seq_along(binaryHits)] |
                     !is.na(binaryHits))
    binIdx <- which(!is.na(binaryHits))
    if (length(binIdx)) {
      ## Skip refs that are ALREADY installed at the requested version
      ## (no point reinstalling). Use installed.packages() against destLib
      ## directly (cheap, already done implicitly above).
      ipNow <- tryCatch(as.data.frame(
        installed.packages(lib.loc = destLib, noCache = TRUE)),
        error = function(e) data.frame(Package = character(),
                                       Version = character()))
      alreadyInLib <- vapply(binIdx, function(i) {
        row <- ipNow[ipNow$Package == pkgs$Package[i], , drop = FALSE]
        nrow(row) > 0 && identical(as.character(row$Version[1]),
                                    as.character(pkgs$Version[i]))
      }, logical(1))
      binIdx <- binIdx[!alreadyInLib]
    }
    if (length(binIdx)) {
      if (verbose >= 1)
        messageVerbose("Hybrid pre-install: ", length(binIdx),
                       " of ", nrow(pkgs),
                       " refs have a cached binary matching this R session (",
                       R.version$platform, ", R ", ourRverShort,
                       "); installing those via install.packages(type=binary) ",
                       "to skip compilation",
                       verbose = verbose, verboseLevel = 1)
      hybridT0 <- Sys.time()
      ## install.packages with file paths + repos = NULL + type = "binary"
      ## treats each path as a binary tarball and unpacks. On Mac, this
      ## handles `.tgz` natively. We loop in chunks to avoid arg-list
      ## limits and to allow per-file failure isolation (one bad binary
      ## shouldn't kill the whole batch).
      binFiles <- binaryHits[binIdx]
      binPkgs  <- pkgs$Package[binIdx]
      origRepos2 <- getOption("repos")
      options(repos = c(CRAN = "https://cloud.r-project.org"))  # placeholder
      installedOK <- character()
      for (j in seq_along(binFiles)) {
        rc <- tryCatch({
          suppressMessages(suppressWarnings(
            utils::install.packages(
              pkgs = binFiles[j], lib = destLib, repos = NULL,
              type = "binary", dependencies = FALSE,
              quiet = isTRUE(verbose < 1))))
          ## Verify the install actually landed in destLib.
          installed <- file.exists(file.path(destLib, binPkgs[j], "DESCRIPTION"))
          if (installed) installedOK <- c(installedOK, binPkgs[j])
          installed
        }, error = function(e) FALSE)
      }
      options(repos = origRepos2)
      preInstalled <- installedOK
      if (verbose >= 1) {
        hybridSecs <- as.numeric(difftime(Sys.time(), hybridT0,
                                          units = "secs"))
        messageVerbose(sprintf(
          "  ... pre-installed %d/%d binaries in %.1fs (%s)",
          length(installedOK), length(binIdx), hybridSecs,
          if (length(installedOK) < length(binIdx))
            sprintf("%d failed; pak will pick those up from source",
                    length(binIdx) - length(installedOK))
          else "all OK"),
          verbose = verbose, verboseLevel = 1)
      }
    }
  }

  ## Exclude already-installed-at-target-version refs from localRefs.
  ## pak treats `local::<file>` as an explicit install request and
  ## reinstalls regardless of destLib state -- even when the installed
  ## version matches the snapshot pin (the plan shows entries like
  ## `+ DEoptim 2.2-8 -> 2.2-8`, "updating" to itself, which is wasted
  ## compile time). Trim these so pak only resolves+installs what's
  ## genuinely missing or wrong-version.
  ipForRefs <- tryCatch(as.data.frame(
    installed.packages(lib.loc = destLib, noCache = TRUE)),
    error = function(e) data.frame(Package = character(),
                                   Version = character()))
  alreadyAtTarget <- vapply(seq_len(nrow(pkgs)), function(i) {
    row <- ipForRefs[ipForRefs$Package == pkgs$Package[i], , drop = FALSE]
    if (!nrow(row)) return(FALSE)
    if (isGH[i]) {
      ## GH ref: prefer SHA-based check; fall back to Version when the
      ## installed DESCRIPTION has no RemoteSha/GithubSHA1 fields. The
      ## hybrid binary pre-install (install.packages(type=binary)) just
      ## unpacks the cached .tgz, which doesn't write remotes-style SHA
      ## fields. Without the fallback, every GH ref appears "not at
      ## target" after binary install -> pak reinstalls it on every run
      ## ("+ visualTest 1.0.0 -> 1.0.0" update-to-itself churn).
      f <- file.path(destLib, pkgs$Package[i], "DESCRIPTION")
      if (file.exists(f)) {
        dcf <- tryCatch(read.dcf(f, fields = c("RemoteSha","GithubSHA1")),
                        error = function(e) NULL)
        if (!is.null(dcf) && nrow(dcf) > 0) {
          sha <- dcf[1, "RemoteSha"]
          if (is.na(sha) || !nzchar(sha)) sha <- dcf[1, "GithubSHA1"]
          if (!is.na(sha) && nzchar(sha))
            return(isTRUE(as.character(sha) ==
                          as.character(pkgs$GithubSHA1[i])))
        }
      }
      ## Fallback: version match. Snapshot's Version pin is enough when
      ## the installed copy doesn't carry SHA metadata.
      isTRUE(as.character(row$Version[1]) == as.character(pkgs$Version[i]))
    } else {
      isTRUE(as.character(row$Version[1]) == as.character(pkgs$Version[i]))
    }
  }, logical(1))
  pakRefIdx <- which(!alreadyAtTarget)
  ## Defensive: drop refs whose destPath is empty, NA, or doesn't point
  ## to an existing tarball. Without this, an empty-string destPath gets
  ## passed as `local::` to pak, which fails with `is_existing_file(file)
  ## is not TRUE`. Empty destPaths can creep in if some upstream code
  ## path leaves the slot unset (e.g. a ref that the cache pre-filter
  ## marked as cached but whose copy step silently failed). Filter and
  ## warn with the offending ref names so we can chase the upstream bug.
  badPath <- vapply(pakRefIdx, function(i) {
    p <- destPaths[i]
    is.na(p) || !nzchar(p) || !file.exists(p)
  }, logical(1))
  if (any(badPath)) {
    if (verbose >= 1) {
      bad <- pakRefIdx[badPath]
      messageVerbose(
        "Dropping ", length(bad),
        " ref(s) from pak's input -- destPath empty/missing for: ",
        paste0(pkgs$Package[bad], "@",
               ifelse(is.na(pkgs$Version[bad]), "?", pkgs$Version[bad]),
               " (path='", destPaths[bad], "')",
               collapse = "; "),
        verbose = verbose, verboseLevel = 1)
    }
    pakRefIdx <- pakRefIdx[!badPath]
  }
  ## Build local:: refs from on-disk source tarballs.
  ##
  ## Why not canonical pak refs (cran::pkg@version, url::URL)? See the
  ## "NOTE on why we don't pass cran:: refs" comment above the hybrid
  ## block -- measured 18x slower because pak's resolver rebuilds archived
  ## versions from source even when the binary is cached. local:: bypasses
  ## the resolver entirely.
  ##
  ## paste0 caveat: paste0("local::", character(0)) returns c("local::")
  ## length 1, not character(0), because R recycles the zero-length
  ## operand. Explicit empty-case guard below.
  pakInputRefs <- if (length(pakRefIdx))
                    paste0("local::", destPaths[pakRefIdx])
                  else character(0)
  refStrategyLabel <- "local::"
  if (verbose >= 2 && length(pakRefIdx)) {
    refLines <- vapply(pakRefIdx, function(k) {
      sprintf("  %s@%s",
              pkgs$Package[k],
              if (is.na(pkgs$Version[k])) "?" else pkgs$Version[k])
    }, character(1))
    messageVerbose("pak input refs (", length(pakRefIdx), "):\n",
                   paste(refLines, collapse = "\n"),
                   verbose = verbose, verboseLevel = 2)
  }
  if (verbose >= 1 && sum(alreadyAtTarget) > 0)
    messageVerbose("Excluding ", sum(alreadyAtTarget),
                   " already-installed refs from pak's input ",
                   "(pre-installed binaries + test-runner-installed); ",
                   "passing ", length(pakInputRefs), " to pak",
                   verbose = verbose, verboseLevel = 1)
  pakLogTail <- character()
  pakErr <- NULL
  pakDetail <- character()
  pakPlanInfo <- character()
  if (requireNamespace("pak", quietly = TRUE) && length(pakInputRefs)) {
    messageVerbose("Trying pak::pkg_install with ", length(pakInputRefs),
                   " ", refStrategyLabel, " refs, lib=", destLib,
                   " (fallback: install.packages)",
                   verbose = verbose, verboseLevel = 1)
    pakT0 <- Sys.time()
    silent <- isTRUE(getOption("Require.snapshotInstallerPakSilent", FALSE))
    pakLogPath <- tempfile2("pak_log_")
    if (silent) {
      pakLogCon <- file(pakLogPath, "w")
      pakErr <- tryCatch({
        sink(pakLogCon, type = "output")
        sink(pakLogCon, type = "message")
        pak::pkg_install(pakInputRefs, lib = destLib,
                         dependencies = NA, upgrade = FALSE, ask = FALSE)
        NULL
      }, error = function(e) e, finally = {
        try(sink(NULL, type = "message"), silent = TRUE)
        try(sink(NULL, type = "output"), silent = TRUE)
        try(close(pakLogCon), silent = TRUE)
      })
    } else {
      ## Capture pak's stdout/stderr to a tempfile in parallel with letting
      ## it print to the console -- informational lines (resolver progress,
      ## "Will install N", first compile errors) reach the user normally,
      ## AND we get a copy to surface as `pakLogTail` if pak refuses with
      ## a wrapper-only error like "! error in pak subprocess".
      pakErr <- tryCatch({
        pak::pkg_install(pakInputRefs, lib = destLib,
                         dependencies = NA, upgrade = FALSE, ask = FALSE)
        NULL
      }, error = function(e) e)
    }
    pakSecs <- as.numeric(difftime(Sys.time(), pakT0, units = "secs"))
    if (verbose >= 1)
      messageVerbose(sprintf("pak::pkg_install returned in %.1fs (%s)",
                             pakSecs,
                             if (is.null(pakErr)) "ok" else "failed"),
                     verbose = verbose, verboseLevel = 1)
    ## pak's wrapper "! error in pak subprocess" hides the subprocess's
    ## actual error. Walk the caught condition's $parent chain (rlang-
    ## style chained errors) to surface the inner cause. pak versions
    ## differ in API: `pak::last_error()` was added in 0.10+; older 0.9
    ## doesn't export it. The condition chain on `pakErr` is independent
    ## of that -- it's set by the `chain_error()` call inside pak.
    if (!is.null(pakErr)) {
      cur <- pakErr
      depth <- 0L
      while (!is.null(cur) && depth < 8L) {
        pmsg <- tryCatch(conditionMessage(cur), error = function(e) "")
        if (nzchar(pmsg)) {
          pakDetail <- c(pakDetail,
                         paste0("[", depth, "] ",
                                paste(class(cur), collapse = "/")),
                         paste0("    ", strsplit(pmsg, "\n",
                                                  fixed = TRUE)[[1]]))
        }
        cur <- cur$parent
        depth <- depth + 1L
      }
      ## Newer pak (>= 0.10) adds last_error() / last_error_trace().
      ## Try them defensively -- they often surface a slightly different
      ## (more structured) view of the same error than the chain above.
      if ("last_error" %in% getNamespaceExports("pak")) {
        ## getNamespace lookup instead of pak::last_error: older pak (<0.10)
        ## does not export last_error, which R CMD check would flag as a
        ## missing object. The getNamespaceExports gate above already
        ## ensures this branch only runs when the symbol exists.
        pakLastError <- getNamespace("pak")[["last_error"]]
        le <- tryCatch(pakLastError(), error = function(e) NULL)
        if (!is.null(le)) {
          msg <- tryCatch(conditionMessage(le), error = function(e) "")
          if (nzchar(msg))
            pakDetail <- c(pakDetail, "[last_error]",
                           paste0("    ", strsplit(msg, "\n",
                                                    fixed = TRUE)[[1]]))
        }
      }
      if (requireNamespace("cli", quietly = TRUE))
        pakDetail <- cli::ansi_strip(pakDetail)
      pakDetail <- pakDetail[nzchar(trimws(pakDetail))]

      ## Probe with the resolver-only `pkg_deps`. If the failure is at
      ## the solve stage (the common case for snapshots that pin
      ## "future" versions vs PPM's currently-served version),
      ## pkg_deps reproduces the same error in isolation. If it
      ## succeeds, the failure is install-stage (e.g. compile fail)
      ## and the log tail / parent chain above carries the detail.
      ## Use `dependencies = NA` (Depends/Imports/LinkingTo) to match
      ## what pkg_install would resolve.
      probeRes <- tryCatch(
        pak::pkg_deps(pakInputRefs, dependencies = NA, upgrade = FALSE),
        error = function(e) e)
      if (inherits(probeRes, "error")) {
        pakPlanInfo <- c("pkg_deps probe (resolver-only) also errored:",
                         strsplit(conditionMessage(probeRes), "\n",
                                  fixed = TRUE)[[1]])
        ## Walk that probe's chain too -- same pattern.
        cur <- probeRes$parent
        depth <- 0L
        while (!is.null(cur) && depth < 6L) {
          pmsg <- tryCatch(conditionMessage(cur), error = function(e) "")
          if (nzchar(pmsg))
            pakPlanInfo <- c(pakPlanInfo,
                             paste0("  [parent ", depth, "] ",
                                    paste(class(cur), collapse = "/")),
                             paste0("      ",
                                    strsplit(pmsg, "\n",
                                             fixed = TRUE)[[1]]))
          cur <- cur$parent
          depth <- depth + 1L
        }
      } else if (is.data.frame(probeRes)) {
        pakPlanInfo <- paste0("pkg_deps probe (resolver-only) succeeded with ",
                              nrow(probeRes), " refs -- failure is at install ",
                              "stage, not resolve. Inspect the per-package ",
                              "log tail above for the actual cause.")
      }
      if (requireNamespace("cli", quietly = TRUE))
        pakPlanInfo <- cli::ansi_strip(pakPlanInfo)
    }
    if (file.exists(pakLogPath)) {
      pakLog <- tryCatch(readLines(pakLogPath, warn = FALSE),
                         error = function(e) character())
      if (requireNamespace("cli", quietly = TRUE))
        pakLog <- cli::ansi_strip(pakLog)
      pakLog <- gsub("\r", "", pakLog)
      pakLog <- pakLog[nzchar(trimws(pakLog))]
      ## Strip pak's spinner / glyph lines (sparkles, scales, arrows, ticks,
      ## braille spinners). \u escapes keep this file ASCII while the runtime
      ## regex still matches the original UTF-8 glyphs from pak's output.
      pakLog <- pakLog[!grepl(
        "^[[:space:]]*[\u2728\u2696\u2192\u2716\u2139\u2714\u2807\u2808-\u280F\u28A8\u28B9\u28E8\u28E9]",
        pakLog)]
      pakLog <- pakLog[!grepl(
        "^[[:space:]]*(Found|Resolving|Updating metadata|Downloading|Will install|Will download|Getting|Installing|Got |Installed |Will update|Checking installed|Checking for [0-9]+)",
        pakLog)]
      pakLogTail <- utils::tail(pakLog, 8)
    }
    on.exit(unlink(pakLogPath), add = TRUE)
  }

  outDir <- tempfile2("snapInstall_outs_")
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(outDir, recursive = TRUE), add = TRUE)

  if (!is.null(pakErr)) {
    messageVerbose(
      "pak refused (",
      sub("\n.*$", "", conditionMessage(pakErr)), "); ",
      "falling back to install.packages",
      if (length(pakDetail))
        paste0(
          "\n  pak's detailed error:\n    ",
          paste(pakDetail, collapse = "\n    "))
      else "",
      if (length(pakPlanInfo))
        paste0(
          "\n  pak::pkg_install_plan probe:\n    ",
          paste(pakPlanInfo, collapse = "\n    "))
      else "",
      if (length(pakLogTail))
        paste0(
          "\n  pak's last log lines:\n    ",
          paste(pakLogTail, collapse = "\n    "))
      else "",
      verbose = verbose, verboseLevel = 1)

    ## All tarballs (including repackaged GH ones) now have <pkg>/ as
    ## their top-level dir, so install.packages via a single file://
    ## repo handles them uniformly. dependencies = NA computes topo
    ## order from the PACKAGES index.
    repoDir <- tempfile2("snapInstall_repo_")
    contribDir <- file.path(repoDir, "src", "contrib")
    if (!dir.exists(contribDir)) dir.create(contribDir, recursive = TRUE)
    on.exit(unlink(repoDir, recursive = TRUE), add = TRUE)
    for (i in seq_len(nrow(pkgs))) {
      dest <- file.path(contribDir, basename(destPaths[i]))
      file.copy(destPaths[i], dest, overwrite = TRUE)
    }
    tools::write_PACKAGES(contribDir, type = "source")
    reposURL <- paste0("file://", repoDir)
    ## Skip refs already installed at the snapshot's pinned version.
    ## install.packages with `pkgs$Package` would otherwise "update" them
    ## from the file:// repo's source tarball -- which on packages with
    ## SystemRequirements (cmake, nlopt, etc.) means a needless source
    ## compile that fails when those system libs aren't installed. The
    ## hybrid pre-install above already put the right binary in destLib;
    ## don't redo it.
    ipForFB <- tryCatch(as.data.frame(
      installed.packages(lib.loc = destLib, noCache = TRUE)),
      error = function(e) data.frame(Package = character(),
                                     Version = character()))
    needsFB <- vapply(seq_len(nrow(pkgs)), function(i) {
      row <- ipForFB[ipForFB$Package == pkgs$Package[i], , drop = FALSE]
      if (!nrow(row)) return(TRUE)
      !isTRUE(as.character(row$Version[1]) ==
              as.character(pkgs$Version[i]))
    }, logical(1))
    pkgsForFB <- pkgs$Package[needsFB]
    if (verbose >= 1)
      messageVerbose("install.packages fallback: skipping ",
                     sum(!needsFB), " refs already at target version, ",
                     "installing ", length(pkgsForFB),
                     verbose = verbose, verboseLevel = 1)
    if (length(pkgsForFB))
      suppressWarnings(utils::install.packages(
        pkgsForFB, lib = destLib, repos = reposURL,
        type = "source", dependencies = NA, Ncpus = Ncpus,
        keep_outputs = outDir,
        quiet = isTRUE(verbose < 1)))
  } else if (requireNamespace("pak", quietly = TRUE)) {
    messageVerbose("[snapshotInstaller] installed via pak (binary cache)",
                   verbose = verbose, verboseLevel = 1)
  } else {
    ## pak isn't installed at all -- go directly to install.packages.
    ## Repackaged GH tarballs already have <pkg>/ at top level, so
    ## one file:// repo handles everything.
    repoDir <- tempfile2("snapInstall_repo_")
    contribDir <- file.path(repoDir, "src", "contrib")
    if (!dir.exists(contribDir)) dir.create(contribDir, recursive = TRUE)
    on.exit(unlink(repoDir, recursive = TRUE), add = TRUE)
    for (i in seq_len(nrow(pkgs))) {
      dest <- file.path(contribDir, basename(destPaths[i]))
      file.copy(destPaths[i], dest, overwrite = TRUE)
    }
    tools::write_PACKAGES(contribDir, type = "source")
    reposURL <- paste0("file://", repoDir)
    suppressWarnings(utils::install.packages(
      pkgs$Package, lib = destLib, repos = reposURL,
      type = "source", dependencies = NA, Ncpus = Ncpus,
      keep_outputs = outDir,
      quiet = isTRUE(verbose < 1)))
  }

  ## Auto-fill missing transitive deps. Snapshots are sometimes incomplete:
  ## a package that genuinely needs (Imports / Depends / LinkingTo) some
  ## other package didn't make it into inst/snapshot.txt because the
  ## snapshot was built from a session that already had the dep loaded
  ## from another libPath. Without auto-fill, install.packages errors
  ## with "ERROR: dependency 'X' is not available" and cascades into a
  ## wall of failures. Walk each just-installed snapshot package's
  ## DESCRIPTION, collect names referenced by hard-dep fields that
  ## aren't in destLib + .basePkgs, then `install.packages(..., dependencies = NA)`
  ## from CRAN/PPM (NOT the local file:// repo, since the snapshot
  ## doesn't have these). Result is reported as [auto-filled] in the
  ## diagnostic so the user can decide whether to add them to the
  ## snapshot for a deterministic future run.
  autoFilled <- character()
  ipForFill <- tryCatch(
    rownames(installed.packages(lib.loc = destLib, noCache = TRUE)),
    error = function(e) character())
  installedSnapshotPkgs <- intersect(snapshot$Package, ipForFill)
  neededDeps <- character()
  for (p in installedSnapshotPkgs) {
    descFile <- file.path(destLib, p, "DESCRIPTION")
    if (!file.exists(descFile)) next
    desc <- tryCatch(
      read.dcf(descFile, fields = c("Depends", "Imports", "LinkingTo")),
      error = function(e) NULL)
    if (is.null(desc) || !nrow(desc)) next
    ## Filter NA fields BEFORE pasting -- read.dcf returns NA for missing
    ## fields, and `paste(unlist(c(NA, "...")), collapse=", ")` yields a
    ## literal "NA, ..." which extractPkgName then turns into a fake "NA"
    ## ref, leading to "auto-filling 1 transitive dep(s) not in snapshot:
    ## NA" in the diagnostic.
    fields <- unlist(desc)
    fields <- fields[!is.na(fields) & nzchar(fields)]
    if (!length(fields)) next
    txt <- paste(fields, collapse = ", ")
    refs <- unlist(strsplit(txt, ",\\s*"))
    refs <- refs[nzchar(refs) & !is.na(refs)]
    nms <- extractPkgName(refs)
    nms <- nms[nzchar(nms) & !is.na(nms) & nms != "R" & nms != "NA"]
    neededDeps <- c(neededDeps,
                    setdiff(nms, c(ipForFill, .basePkgs, snapshot$Package)))
  }
  neededDeps <- unique(neededDeps)
  if (length(neededDeps)) {
    messageVerbose(
      "[snapshotInstaller] auto-filling ", length(neededDeps),
      " transitive dep(s) not in snapshot: ",
      paste(neededDeps, collapse = ", "),
      verbose = verbose, verboseLevel = 1)
    fillOutDir <- tempfile2("snapInstall_fill_outs_")
    dir.create(fillOutDir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(fillOutDir, recursive = TRUE), add = TRUE)
    ## getOption("repos") here still has PPM + CRAN + any snapshot repos;
    ## the local file:// repo (if it was created in the install.packages
    ## fallback) isn't in options(repos) -- install.packages got it via the
    ## `repos` arg only -- so we don't need to filter.
    suppressWarnings(utils::install.packages(
      neededDeps, lib = destLib, type = "source",
      dependencies = NA, Ncpus = Ncpus,
      keep_outputs = fillOutDir,
      quiet = isTRUE(verbose < 1)))
    ipAfter <- tryCatch(
      rownames(installed.packages(lib.loc = destLib, noCache = TRUE)),
      error = function(e) character())
    autoFilled <- intersect(neededDeps, ipAfter)
    ## Also include any TRANSITIVE deps install.packages pulled in along
    ## the way (dependencies = NA recurses), so the diagnostic report
    ## attributes them correctly rather than flagging them as rogue.
    autoFilled <- unique(c(autoFilled,
                            setdiff(ipAfter,
                                    c(ipForFill, snapshot$Package, .basePkgs))))
  }

  ## Bump-and-retry: walk newer-than-snapshot versions for any refs
  ## that didn't make it into destLib. install.packages-fallback's
  ## "best-effort" mode already tolerates per-package compile failures
  ## (so ONE bad ref doesn't kill the rest), but a still-missing ref
  ## leaves the user with a half-installed snapshot. For some packages
  ## (notably arrow's bundled-libarrow source build, or any pkg whose
  ## pinned version's source has bit-rotted under newer toolchains),
  ## the snapshot's exact pin won't compile, but a slightly newer
  ## version will. Walk the archive ascending; first install that
  ## sticks wins, with the substitution recorded in the diagnostic.
  ##
  ## Bumping moves AWAY from the snapshot's exact pin, which violates
  ## the reproducibility goal -- gate behind an option, default on, but
  ## clearly surfaced in the report so users see the drift.
  bumpedSubst <- character()
  if (isTRUE(getOption("Require.snapshotInstallerBumpOnFail", TRUE))) {
    ipNow <- tryCatch(
      rownames(installed.packages(lib.loc = destLib, noCache = TRUE)),
      error = function(e) character())
    expectedNow <- snapshot$Package[!is.na(snapshot$Package) &
                                     nzchar(snapshot$Package) &
                                     !snapshot$Package %in% c("R", .basePkgs)]
    stillMissing <- setdiff(expectedNow, ipNow)
    ## Don't bother bumping refs that the user has flagged as
    ## environment-dependent via Require.snapshotInstallerKnownFails
    ## (matches the test's `knownFails` semantically -- they're
    ## system-lib-version-dependent, bumping won't help).
    knownFailsOpt <- getOption("Require.snapshotInstallerKnownFails",
                               character())
    if (length(knownFailsOpt))
      stillMissing <- setdiff(stillMissing, knownFailsOpt)
    if (length(stillMissing)) {
      if (verbose >= 1)
        messageVerbose(
          "Bump-retry: ", length(stillMissing),
          " ref(s) failed at snapshot pin; trying newer versions ",
          "from CRAN/PPM/Archive: ",
          paste(utils::head(stillMissing, 10), collapse = ", "),
          if (length(stillMissing) > 10) " ..." else "",
          verbose = verbose, verboseLevel = 1)
      bumpRes <- bumpAndRetryFailed(
        stillMissing = stillMissing, snapshot = snapshot,
        destLib = destLib, repos = getOption("repos"),
        verbose = verbose)
      bumpedSubst <- bumpRes$bumped
      ## bumped refs count as substitutions for the diagnostic
      substituted <- c(substituted, bumpedSubst)
    }
  }

  ## (Binary caching is now registered via on.exit earlier in this
  ## function so partial installs -- pak crash, install.packages
  ## interrupt, error in auto-fill -- still get the binaries that DID
  ## land in destLib cached for next time.)

  ## Self-diagnose: cross-check what's actually installed in destLib against
  ## the snapshot, then explain each gap with a concrete fix the user can
  ## apply to the snapshot file. This is the difference between a cryptic
  ## "ERROR: dependency 'X' is not available" and an actionable
  ## "X failed to compile because R 4.5 removed Calloc/Free; bump X to >= Y".
  diagnoseSnapshotInstallFailures(
    snapshot = snapshot, destLib = destLib,
    unresolvedRefs = unresolvedRefs, substituted = substituted,
    autoFilled = autoFilled,
    outDir = outDir, verbose = verbose)

  invisible(TRUE)
}

## Post-install introspection: classify every snapshot package that didn't
## land in destLib and emit a structured report (status, why, fix). Reads
## per-package R CMD INSTALL logs (written by install.packages with
## keep_outputs) and matches against known failure patterns.
##
## Patterns recognised:
##  * version-conflict   "namespace 'X' V is being loaded, but >= W is required"
##  * missing-dep        "ERROR: dependency 'X' is not available for package"
##  * compile-failed     "ERROR: compilation failed" / "non-zero exit status"
##  * download-failed    couldn't fetch tarball from any candidate URL
##  * substituted        installed, but at a different version than pinned
## classifyCompileFailure: scan the captured `R CMD INSTALL` output of a
## failed compile and return a specific (reason, fix) pair when the error
## matches a known pattern. Pak's diagnostic showed terse messages like
## "compilation failed" -- useless for the user. The actual cause is
## usually the FIRST clang error, which encodes whether the issue is:
##   - a missing system header (jpeglib.h, glpk.h, gdal.h, ...)
##   - a missing link-time library (-lX not found)
##   - an Rcpp template-arity mismatch (newer pkg, older Rcpp)
##   - a GDAL ABI change (sf / terra vs GDAL >= 3.10's const SRS)
##   - an R API change (R 4.5 removed Calloc/Free)
## Each pattern resolves to a concrete user-facing fix (brew install X,
## bump pkg pin to >= Y).
classifyCompileFailure <- function(txt, pkg) {
  txtNonEmpty <- txt[nzchar(txt)]
  ## First compile error is usually more informative than the last 6
  ## lines -- the tail is often "make: *** Error 1" cleanup.
  errIdx <- grep("error:|fatal error:", txt)
  firstErr <- if (length(errIdx)) txt[errIdx[1]] else ""
  ## Window around the first error so the user sees what code triggered
  ## it (usually the source line + caret).
  errCtx <- if (length(errIdx))
              txt[seq(max(1L, errIdx[1] - 1L), min(length(txt),
                                                    errIdx[1] + 4L))]
            else character()

  ## Pattern: missing system header. Maps the header to a common
  ## brew/apt package name when we know it; falls back to suggesting
  ## the user install the matching dev package.
  m <- regmatches(txt, regexec(
    "fatal error: ['<\"]?([A-Za-z0-9._/+-]+\\.h)['>\"]? file not found",
    txt))
  m <- m[lengths(m) > 0]
  if (length(m)) {
    hdr <- m[[1]][2]
    pkgHints <- list(
      "jpeglib.h"   = "brew install jpeg-turbo (or libjpeg)",
      "png.h"       = "brew install libpng",
      "gdal.h"      = "brew install gdal",
      "geos_c.h"    = "brew install geos",
      "proj.h"      = "brew install proj",
      "glpk.h"      = "brew install glpk",
      "ft2build.h"  = "brew install freetype",
      "freetype.h"  = "brew install freetype",
      "sodium.h"    = "brew install libsodium",
      "archive.h"   = "brew install libarchive",
      "secret.h"    = "brew install libsecret (Linux) -- macOS keychain is built-in",
      "magick/api.h" = "brew install imagemagick",
      "MagickWand/MagickWand.h" = "brew install imagemagick",
      "openssl/ssl.h" = "brew install openssl",
      "curl/curl.h" = "brew install curl",
      "tbb/tbb.h"   = "brew install tbb",
      "udunits2.h"  = "brew install udunits",
      "fftw3.h"     = "brew install fftw",
      "gsl/gsl_vector.h" = "brew install gsl",
      "boost/version.hpp" = "brew install boost",
      "X11/Xlib.h"  = "brew install --cask xquartz",
      "Rmpi.h"      = "brew install open-mpi (and configure R to find it)")
    fixHint <- pkgHints[[hdr]]
    if (is.null(fixHint))
      fixHint <- paste0("install the system library that provides ", hdr,
                        " (then add `-I/path/to/include` via ~/.R/Makevars ",
                        "if R doesn't find it on its default search path)")
    return(list(
      reason = sprintf("missing system header '%s'", hdr),
      fix = paste0(
        fixHint,
        ". Verify with: ls $(brew --prefix)/include/", hdr)))
  }

  ## Pattern: linker can't find -lX.
  m <- regmatches(txt, regexec(
    "ld: library (?:'[^']+' )?not found for ?-l([A-Za-z0-9._+-]+)",
    txt))
  m <- m[lengths(m) > 0]
  if (length(m)) {
    lib <- m[[1]][2]
    return(list(
      reason = sprintf("linker can't find library '-l%s'", lib),
      fix = paste0(
        "install the system library and add `-L/path/to/lib` via ",
        "~/.R/Makevars (or equivalent). Try: brew install ", lib)))
  }

  ## Pattern: Rcpp class_::constructor<> template arity exceeded --
  ## terra 1.8+ uses 10-arg constructor, Rcpp <= 1.0.13 only supports up
  ## to 7. Bump Rcpp.
  if (any(grepl("no matching member function for call to 'constructor'",
                txt)) &&
      any(grepl("too many template arguments for function template 'constructor'",
                txt))) {
    return(list(
      reason = paste0(pkg,
        "'s RcppModule uses class_::constructor<> with more template ",
        "arguments than the snapshot's Rcpp supports"),
      fix = paste0(
        "bump Rcpp in the snapshot to a newer version (>= 1.0.14 typically). ",
        "Note: ", pkg, "'s DESCRIPTION may declare a too-lenient ",
        "Rcpp constraint (e.g. >= 1.0-10) that doesn't catch this at resolve")))
  }

  ## Pattern: GDAL >= 3.10 made OGRLayer::GetSpatialRef() return
  ## const OGRSpatialReference*. Older sf / terra source code stores it
  ## in a non-const pointer -> compile error.
  if (any(grepl(
    "cannot initialize a variable of type 'OGRSpatialReference \\*' with an rvalue of type 'const OGRSpatialReference \\*'",
    txt))) {
    return(list(
      reason = paste0(pkg,
        "'s source uses non-const OGRSpatialReference* but GDAL >= 3.10 ",
        "returns const (ABI break)"),
      fix = paste0(
        "bump ", pkg, " to a version released after GDAL 3.10 (Nov 2024) -- ",
        "e.g. terra >= 1.7-83 or sf >= 1.0-17")))
  }

  ## Pattern: R 4.5 removed Calloc/Free from R headers (now require S/R
  ## prefix). Older C/C++ packages built against R 4.4 source headers
  ## fail at R 4.5.
  if (any(grepl("(?:'Calloc'|'Free'|'Realloc')(?: was not declared| undeclared)",
                txt)) ||
      any(grepl("error: use of undeclared identifier 'Calloc'", txt))) {
    return(list(
      reason = paste0(pkg,
        " uses Calloc/Free which R 4.5 renamed to R_Calloc/R_Free"),
      fix = paste0(
        "bump ", pkg, " to a version that uses R_Calloc/R_Free (or run ",
        "the install on R 4.4)")))
  }

  ## Pattern: -lz / zlib missing (common Linux issue).
  if (any(grepl("zlib\\.h.*file not found", txt))) {
    return(list(
      reason = "missing zlib development headers",
      fix = "Linux: apt install zlib1g-dev / yum install zlib-devel; macOS: zlib is normally bundled -- check Xcode CLT install"))
  }

  ## Pattern: post-compile load test failure with "symbol not found in
  ## flat namespace" -- the package's .so links against a symbol that
  ## the loaded C++ library doesn't export. Common with arrow-style
  ## packages that bundle their own C++ lib but fall back to a system
  ## one (or vice versa) at link time. Compile succeeded but dyn.load
  ## failed in the post-install test_load_package step.
  m <- regmatches(txt, regexec(
    "symbol not found in flat namespace[ '`]+([_A-Za-z0-9]+)", txt))
  m <- m[lengths(m) > 0]
  if (length(m)) {
    sym <- m[[1]][2]
    return(list(
      reason = paste0(pkg, " compiled OK but dyn.load failed: missing ",
                      "symbol '", sym, "' (ABI mismatch between the ",
                      "package and its C++ library)"),
      fix = paste0(
        "the package's bundled C++ lib doesn't match what .so was ",
        "linked against. For arrow specifically: bump ", pkg,
        " to match your host's brew apache-arrow version (or ",
        "`brew uninstall apache-arrow` to force the package to use ",
        "its own bundled libarrow). For sf/terra: bump to a version ",
        "compatible with your host GDAL/PROJ/GEOS.")))
  }

  ## Pattern: bundled-libarrow build failure (arrow-specific). The
  ## build retrieves libarrow tarball, then compiles a HUGE C++
  ## bundle inside libuv/. If a sub-build fails, the whole arrow
  ## install fails with `make[1]: *** [...] Error 1`.
  if (any(grepl("Trying Arrow C\\+\\+ found by pkg-config", txt)) &&
      any(grepl("Successfully retrieved libarrow", txt)) &&
      any(grepl("make.*\\*\\*\\* .* Error 1", txt))) {
    return(list(
      reason = paste0(pkg, "'s bundled libarrow source build failed ",
                      "(host has incompatible apache-arrow brew version)"),
      fix = paste0(
        "bump ", pkg, " to match the host's `pkg-config --modversion ",
        "arrow` (so configure uses the system lib instead of bundling). ",
        "Bump-and-retry will try to do this automatically if enabled.")))
  }

  ## Pattern: post-install test_load_package failure (generic). The
  ## .so loaded but R's library() check failed. Often ABI mismatch
  ## with a previously-installed dep that's now incompatible.
  if (any(grepl("test_load_package", txt)) &&
      any(grepl("package or namespace load failed", txt))) {
    return(list(
      reason = paste0(pkg, " compiled OK but failed the post-install ",
                      "load test (likely ABI mismatch with a dep)"),
      fix = paste0(
        "a previously-installed dependency (in destLib or .libPaths) ",
        "may be ABI-incompatible with this build of ", pkg, ". Try ",
        "removing the dep and reinstalling, or bumping ", pkg,
        " to a version compatible with the dep.")))
  }

  ## Generic compile-failed fallback: give the user the FIRST error and
  ## a few surrounding lines (more informative than the last 6).
  reason <- if (length(firstErr)) {
    cleaned <- sub("^[^:]+:[0-9]+:[0-9]+: ", "", firstErr)
    paste0("compile error: ", trimws(cleaned))
  } else {
    "compilation failed (no specific error pattern matched)"
  }
  list(
    reason = reason,
    fix = paste0(
      "first compile error context:\n        ",
      paste(errCtx, collapse = "\n        "),
      "\n      common remedies: install a missing system lib ",
      "(brew/apt), bump the package pin to a newer version compatible ",
      "with your R/system, or use a binary repo (PPM)."))
}

diagnoseSnapshotInstallFailures <- function(snapshot, destLib,
                                            unresolvedRefs = character(),
                                            substituted = character(),
                                            autoFilled = character(),
                                            outDir = character(),
                                            verbose = 1) {
  ip <- tryCatch(
    rownames(installed.packages(lib.loc = destLib, noCache = TRUE)),
    error = function(e) character())
  expected <- snapshot$Package[!snapshot$Package %in% .basePkgs]
  expected <- expected[nzchar(expected) & !is.na(expected)]
  missing  <- setdiff(expected, ip)

  ## Map snapshot Package -> Version for fix suggestions.
  snapVer <- setNames(snapshot$Version, snapshot$Package)

  diagnostics <- list()

  ## Download-stage failures: tarball never reached install.packages.
  for (p in names(unresolvedRefs)) {
    diagnostics[[p]] <- list(
      pkg = p, status = "download-failed",
      reason = sprintf("version %s not found on PPM, CRAN, or any candidate URL",
                       unresolvedRefs[[p]]),
      fix = paste0(
        "options: (a) bump the pin to a version on CRAN; ",
        "(b) set the snapshot Repository column to the package's home repo ",
        "(e.g. r-universe URL); ",
        "(c) provide GithubRepo / GithubUsername / GithubSHA1"))
  }

  ## Read per-package install logs (only present after install.packages
  ## fallback ran with keep_outputs).
  outText <- list()
  if (length(outDir) && nzchar(outDir) && dir.exists(outDir)) {
    for (f in list.files(outDir, pattern = "\\.out$", full.names = TRUE)) {
      p <- sub("\\.out$", "", basename(f))
      outText[[p]] <- tryCatch(readLines(f, warn = FALSE),
                                error = function(e) character())
    }
  }

  ## Classify install-stage failures (already missing, not in unresolved).
  failed <- setdiff(missing, names(unresolvedRefs))
  for (p in failed) {
    txt <- if (!is.null(outText[[p]])) outText[[p]] else character()

    ## namespace 'X' V is being loaded, but >= W is required
    m <- regmatches(txt, regexec(
      "namespace ['']?(.+?)['']? ([0-9.\\-]+) is being loaded, but >=? ([0-9.\\-]+) is required",
      txt))
    m <- m[lengths(m) > 0]
    if (length(m)) {
      hit <- m[[1]]
      diagnostics[[p]] <- list(
        pkg = p, status = "version-conflict",
        reason = sprintf("'%s' %s loaded, but %s requires >= %s",
                         hit[2], hit[3], p, hit[4]),
        fix = sprintf("bump %s to >= %s in the snapshot",
                      hit[2], hit[4]))
      next
    }

    ## ERROR: dependency 'X' is not available for package 'Y'
    m <- regmatches(txt, regexec(
      "ERROR: dependency ['']?(.+?)['']? is not available for package",
      txt))
    m <- m[lengths(m) > 0]
    if (length(m)) {
      depPkg <- m[[1]][2]
      cascading <- depPkg %in% failed || depPkg %in% names(unresolvedRefs)
      diagnostics[[p]] <- list(
        pkg = p, status = "missing-dep",
        reason = sprintf("requires '%s' which %s",
                         depPkg,
                         if (cascading) "also failed (cascade)"
                         else "isn't installed"),
        fix = if (cascading)
                sprintf("fix the upstream cause for '%s' (see its diagnostic)",
                        depPkg)
              else
                sprintf("add %s to the snapshot, or pin %s at a version that doesn't require it",
                        depPkg, p))
      next
    }

    ## ERROR: compilation failed for package
    if (any(grepl("ERROR: compilation failed|non-zero exit status",
                  txt))) {
      cls <- classifyCompileFailure(txt, p)
      diagnostics[[p]] <- list(
        pkg = p, status = "compile-failed",
        reason = cls$reason,
        fix = cls$fix)
      next
    }

    ## Fallthrough: missing without a recognised pattern. The most common
    ## cause is a cascade -- install.packages refused to even attempt the
    ## install because a hard dep already failed, so no .out file exists.
    ## Walk the snapshot's declared Depends/Imports/LinkingTo for this pkg
    ## and see which of them are in the failure set. If any are, this isn't
    ## the root cause; redirect the user to the upstream diagnostic.
    upstreamFailed <- character()
    snapRow <- snapshot[snapshot$Package == p, , drop = FALSE]
    if (NROW(snapRow)) {
      depCols <- intersect(c("Depends", "Imports", "LinkingTo"),
                           colnames(snapRow))
      depTxt <- paste(unlist(lapply(depCols, function(cc) snapRow[[cc]][1])),
                      collapse = ", ")
      depPkgs <- unique(extractPkgName(strsplit(depTxt, ",\\s*")[[1]]))
      depPkgs <- depPkgs[nzchar(depPkgs) & !depPkgs %in% .basePkgs]
      upstreamFailed <- intersect(depPkgs,
                                  c(failed, names(unresolvedRefs)))
    }
    if (length(upstreamFailed)) {
      diagnostics[[p]] <- list(
        pkg = p, status = "cascade",
        reason = sprintf("blocked by upstream failure of: %s",
                         paste(upstreamFailed, collapse = ", ")),
        fix = sprintf("fix the upstream cause(s): %s",
                      paste(upstreamFailed, collapse = ", ")))
      next
    }
    diagnostics[[p]] <- list(
      pkg = p, status = "unknown",
      reason = if (length(txt))
                 "no recognised failure pattern in install log"
               else
                 "no install log captured (likely deeper transitive cascade)",
      fix = if (length(outDir) && nzchar(outDir))
              sprintf("inspect %s for any leftover logs", outDir)
            else
              "rerun with options(Require.snapshotInstaller = 'install.packages') to capture per-package logs")
  }

  ## Substituted versions: not failures, but worth surfacing.
  substInfo <- list()
  for (s in substituted) {
    parts <- strsplit(s, ": | -> ")[[1]]
    if (length(parts) == 3 && parts[1] %in% ip) {
      substInfo[[parts[1]]] <- list(
        pkg = parts[1], status = "substituted",
        reason = sprintf("requested %s unavailable; installed %s instead",
                         parts[2], parts[3]),
        fix = sprintf("if exact version %s required, locate it on a custom repo or GitHub",
                      parts[2]))
    }
  }

  ## Auto-filled deps: not failures either. The snapshot was incomplete
  ## (a needed transitive dep wasn't pinned); installer fetched it from
  ## CRAN/PPM. Surfacing them lets the user decide whether to add them
  ## to inst/snapshot.txt for a fully reproducible future run.
  fillInfo <- list()
  for (p in autoFilled) {
    fillInfo[[p]] <- list(
      pkg = p, status = "auto-filled",
      reason = "needed transitive dep not in snapshot; installed from CRAN/PPM",
      fix = sprintf(
        "for a fully reproducible snapshot, add %s to inst/snapshot.txt with its current installed version",
        p))
  }

  if (!length(diagnostics) && !length(substInfo) && !length(fillInfo)) {
    if (verbose >= 1)
      messageVerbose("[snapshotInstaller] all snapshot packages installed cleanly",
                     verbose = verbose, verboseLevel = 1)
    return(invisible(list()))
  }

  if (verbose >= 1) {
    cat("\n[snapshotInstaller] diagnostic report\n",
        "  installed: ", length(intersect(expected, ip)), " / ",
        length(expected), "\n",
        "  issues   : ", length(diagnostics),
        if (length(substInfo)) paste0(" (+ ", length(substInfo),
                                       " substitution(s))") else "",
        if (length(fillInfo)) paste0(" (+ ", length(fillInfo),
                                       " auto-filled)") else "",
        "\n", sep = "")
    for (d in c(diagnostics, substInfo, fillInfo)) {
      cat(sprintf("  - %s [%s]\n    why: %s\n    fix: %s\n",
                  d$pkg, d$status, d$reason, d$fix))
    }
  }

  invisible(c(diagnostics, substInfo, fillInfo))
}

## Pick the nearest archived version available on CRAN when the snapshot
## pinned version is gone (404). Prefer the latest version <= requested
## (older versions are more likely still in the archive); fall back to the
## earliest version > requested. Returns NULL when nothing is available.
##
## Uses the existing `dlArchiveVersionsAvailable` helper that fetches CRAN's
## Meta/archive.rds and `extractVersionNumber` to parse versions out of the
## tarball filenames.
## Walk newer-than-pin versions for each still-missing ref until one
## installs cleanly. Drives the "bump-and-retry" stage of
## installSnapshotViaInstallPackages. Returns a list with `bumped`
## (character vector of "pkg: oldVer -> newVer" entries) and
## `stillMissing` (refs we couldn't recover).
##
## Strategy:
##   1. For each missing pkg, list candidate versions: the CURRENT
##      version on PPM/CRAN (from `available.packages()`) plus all
##      historical versions from CRAN's archive.rds. Sort ascending.
##   2. Filter to versions strictly newer than the snapshot's pin.
##   3. For each candidate (lowest first -> least drift), download the
##      source tarball directly (PPM -> CRAN -> CRAN/Archive), then
##      `install.packages(<file>, repos = NULL, dependencies = NA)`.
##      `dependencies = NA` so install.packages backfills any new
##      dep that the bumped version requires (and that other snapshot
##      pins don't provide).
##   4. First success wins. Record the substitution and continue.
##
## Bound the search: cap at 20 candidate versions per package -- past
## that the drift is too large to call a "snapshot install" anymore.
bumpAndRetryFailed <- function(stillMissing, snapshot, destLib,
                                repos = getOption("repos"),
                                maxCandidates = 20L,
                                maxIters = 3L,
                                verbose = getOption("Require.verbose", 0)) {
  bumped <- character()
  ## Iterate: a successful install of one ref (e.g., arrow bumped from
  ## 23.0.1.1 to 24.0.0) may UNBLOCK another ref that was failing only
  ## because that dep was missing (e.g., disk.frame@0.8.3 fails when
  ## arrow is missing, succeeds once arrow is installed). Loop until a
  ## pass makes no progress, capped at maxIters. Per pass, each pkg
  ## tries its snapshot pin FIRST (cheap, no drift) -- only walks newer
  ## versions if the pin still fails.
  remaining <- stillMissing
  for (iter in seq_len(maxIters)) {
    if (!length(remaining)) break
    progressed <- character()
    for (pkg in remaining) {
      snapVer <- snapshot$Version[snapshot$Package == pkg][1]
      if (is.na(snapVer) || !nzchar(snapVer)) snapVer <- NA_character_
      newer <- listCandidateVersions(pkg, repos = repos, verbose = verbose)
      if (!is.na(snapVer) && length(newer)) {
        cmp <- vapply(newer, function(v)
                      tryCatch(as.integer(utils::compareVersion(v, snapVer)),
                               error = function(e) NA_integer_),
                      integer(1))
        newer <- newer[!is.na(cmp) & cmp > 0]
        newer <- newer[order(numeric_version(newer))]
        newer <- utils::head(newer, maxCandidates)
      } else if (length(newer)) {
        newer <- newer[order(numeric_version(newer))]
        newer <- utils::head(newer, maxCandidates)
      }
      ## Try snapshot pin first (just-in-case its deps are now resolved
      ## by an earlier bump in this or a prior pass), then newer versions.
      candidates <- if (is.na(snapVer)) newer else c(snapVer, newer)
      if (!length(candidates)) next
      if (verbose >= 1 && iter == 1L)
        messageVerbose(
          "  - ", pkg, " (snapshot pin: ",
          if (is.na(snapVer)) "?" else snapVer,
          "): trying ", length(candidates),
          " version(s) -> ",
          paste(utils::head(candidates, 5), collapse = ", "),
          if (length(candidates) > 5) " ..." else "",
          verbose = verbose, verboseLevel = 1)
      for (v in candidates) {
        if (tryInstallByUrl(pkg = pkg, version = v, destLib = destLib,
                            repos = repos, verbose = verbose)) {
          if (!is.na(snapVer) && identical(v, snapVer)) {
            ## Ref installed at the snapshot pin (no drift) -- this
            ## happens when an earlier bump unblocked its deps. Don't
            ## record as a "bump" since version matches the snapshot.
            if (verbose >= 1)
              messageVerbose("    (OK) ", pkg, " ", v,
                             " installed at snapshot pin (deps resolved)",
                             verbose = verbose, verboseLevel = 1)
          } else {
            bumped <- c(bumped,
                        sprintf("%s: %s -> %s (bumped)",
                                pkg, snapVer, v))
            if (verbose >= 1)
              messageVerbose("    (OK) ", pkg, " ", v, " installed (bumped)",
                             verbose = verbose, verboseLevel = 1)
          }
          progressed <- c(progressed, pkg)
          break
        }
      }
    }
    remaining <- setdiff(remaining, progressed)
    if (!length(progressed)) break  # no progress -> further iters won't help
    if (verbose >= 1 && length(remaining) && iter < maxIters)
      messageVerbose("  iter ", iter, ": ", length(progressed),
                     " recovered, ", length(remaining),
                     " still missing -- retrying (deps may be unblocked)",
                     verbose = verbose, verboseLevel = 1)
  }
  list(bumped = bumped, stillMissing = remaining)
}

## List ALL known versions of a package (current + historical) across
## the repos. Returns a unique character vector of version strings.
listCandidateVersions <- function(pkg, repos = getOption("repos"),
                                   verbose = getOption("Require.verbose", 0)) {
  out <- character()
  ## Current versions in any of the repos' PACKAGES indexes.
  ## Wrapped in suppressWarnings: snapshot Repository columns sometimes
  ## point at r-universe instances that don't host PACKAGES.rds (only
  ## PACKAGES.gz / PACKAGES) -- available.packages issues a 404 warning
  ## that's noise, not an error. We don't want to surface that to the
  ## user (and the test fails on unmatched warnings).
  ap <- tryCatch(
    suppressWarnings(
      utils::available.packages(repos = repos, type = "source",
                                filters = list())),
    error = function(e) NULL)
  if (!is.null(ap) && nrow(ap) > 0) {
    cur <- ap[ap[, "Package"] == pkg, "Version"]
    out <- c(out, unique(unname(cur)))
  }
  ## Historical versions from CRAN archive.rds.
  cranLike <- repos[grepl("^https?://(cran\\.|cloud\\.r-)", repos)]
  if (!length(cranLike)) cranLike <- "https://cloud.r-project.org"
  ava <- tryCatch(suppressWarnings(
                    dlArchiveVersionsAvailable(pkg, repos = cranLike,
                                                verbose = verbose)),
                  error = function(e) NULL)
  if (!is.null(ava) && length(ava) && !is.null(ava[[1]]) &&
      is.data.frame(ava[[1]]) && nrow(ava[[1]])) {
    histVer <- extractVersionNumber(
      filenames = basename(ava[[1]][["PackageUrl"]]))
    histVer <- histVer[!is.na(histVer) & nzchar(histVer)]
    out <- c(out, unique(histVer))
  }
  unique(out)
}

## Download <pkg>_<version>.tar.gz from PPM/CRAN/Archive (in priority
## order) and install via install.packages. Returns TRUE on success
## (DESCRIPTION lands in destLib). Best-effort: silent on download or
## compile failure -- caller decides whether to keep walking versions.
tryInstallByUrl <- function(pkg, version, destLib, repos,
                             verbose = getOption("Require.verbose", 0)) {
  tmp <- tempfile(paste0(pkg, "_", version, "_"), fileext = ".tar.gz")
  on.exit(unlink(tmp), add = TRUE)
  ## Build candidate URLs in priority order: PPM (if present, may serve
  ## binaries), CRAN /src/contrib (current), CRAN /src/contrib/Archive
  ## (older). PPM URLs may 404 for older versions; CRAN /src/contrib
  ## only has the latest; CRAN/Archive has everything else.
  ppm <- repos[grepl("packagemanager.posit.co", repos, fixed = TRUE)]
  cranLike <- repos[grepl("^https?://(cran\\.|cloud\\.r-)", repos)]
  if (!length(cranLike)) cranLike <- "https://cloud.r-project.org"
  urls <- character()
  for (r in c(ppm, cranLike)) {
    urls <- c(urls,
              paste0(r, "/src/contrib/", pkg, "_", version, ".tar.gz"),
              paste0(r, "/src/contrib/Archive/", pkg, "/",
                     pkg, "_", version, ".tar.gz"))
  }
  for (u in unique(urls)) {
    ## suppressWarnings + tryCatch: 404s on PPM/Archive for older
    ## versions are expected as we walk down the priority list; they
    ## emit "downloaded length 0 != reported length N" warnings that
    ## the test surfaces as unexpected. Quiet here, then check that
    ## the resulting file is actually a tarball (not a 404 HTML page
    ## that download.file silently saved).
    ok <- tryCatch(
      suppressWarnings(utils::download.file(
        u, tmp, method = "libcurl", quiet = TRUE, mode = "wb"))
        == 0L && file.exists(tmp) && file.size(tmp) > 100L,
      error = function(e) FALSE,
      warning = function(w) FALSE)
    if (!isTRUE(ok)) next
    ## Got a tarball -- try to install. dependencies = NA so
    ## install.packages pulls anything new the bumped version needs.
    tryCatch({
      suppressMessages(suppressWarnings(
        utils::install.packages(tmp, lib = destLib, repos = NULL,
                                type = "source", dependencies = NA,
                                quiet = isTRUE(verbose < 2))))
    }, error = function(e) invisible())
    if (file.exists(file.path(destLib, pkg, "DESCRIPTION")))
      return(TRUE)
    ## install failed; reset the file for next URL attempt
    unlink(tmp)
  }
  FALSE
}

findNearestArchivedVersion <- function(pkg, requested,
                                       repos = getOption("repos"),
                                       verbose = getOption("Require.verbose", 0)) {
  ## CRAN's Meta/archive.rds lives only at the canonical CRAN mirror
  ## (and a handful of clones); PPM/RSPM URLs don't host it. Force a
  ## fallback to cloud.r-project.org so the lookup actually succeeds.
  cranLike <- repos[grepl("^https?://(cran\\.|cloud\\.r-)", repos)]
  if (!length(cranLike)) {
    cranLike <- "https://cloud.r-project.org"
  }
  ava <- tryCatch(dlArchiveVersionsAvailable(pkg, repos = cranLike, verbose = verbose),
                  error = function(e) NULL)
  if (is.null(ava) || !length(ava) || is.null(ava[[1]]) ||
      !is.data.frame(ava[[1]]) || !nrow(ava[[1]])) {
    return(NULL)
  }
  vers <- extractVersionNumber(filenames = basename(ava[[1]][["PackageUrl"]]))
  vers <- vers[!is.na(vers) & nzchar(vers)]
  if (!length(vers)) return(NULL)
  cmp <- vapply(vers, function(v) tryCatch(as.integer(utils::compareVersion(v, requested)),
                                            error = function(e) NA_integer_),
                integer(1))
  earlier <- vers[!is.na(cmp) & cmp < 0]
  later   <- vers[!is.na(cmp) & cmp > 0]
  if (length(earlier)) {
    return(tail(earlier[order(numeric_version(earlier))], 1))
  }
  if (length(later)) {
    return(head(later[order(numeric_version(later))], 1))
  }
  NULL
}

## Detect a Posit Package Manager Linux binary repo URL for the running
## distro by reading /etc/os-release. Returns NULL on non-Linux or when the
## codename is missing. PPM URL form: __linux__/<codename> triggers binary
## serving; trailing /latest gives whatever versions are current. Older
## archived versions are still resolvable via this URL but pak will fall
## back to source for those that PPM didn't pre-build.
detectPPMLinuxRepo <- function() detectPPMRepo()

## Cross-platform PPM repo URL resolver. Linux uses the
## __linux__/<codename> path so PPM serves prebuilt-against-distro
## binaries; macOS hits the plain /cran/latest base where PPM
## content-negotiates Mac binaries off the User-Agent we set in
## installSnapshotViaInstallPackages. Windows isn't covered (PPM can
## serve Windows binaries but we don't run snapshot installs from
## Windows in practice). Returns NULL when the platform isn't
## supported, callers can ignore PPM in that case.
detectPPMRepo <- function() {
  sys <- Sys.info()[["sysname"]]
  if (identical(sys, "Linux")) {
    f <- "/etc/os-release"
    if (!file.exists(f)) return(NULL)
    ll <- tryCatch(readLines(f, warn = FALSE), error = function(e) character())
    m <- grep("^VERSION_CODENAME=", ll, value = TRUE)
    if (!length(m)) return(NULL)
    codename <- sub('^VERSION_CODENAME=["]?([^"]+)["]?$', "\\1", m[1])
    if (!nzchar(codename)) return(NULL)
    return(paste0("https://packagemanager.posit.co/cran/__linux__/", codename, "/latest"))
  }
  if (identical(sys, "Darwin")) {
    return("https://packagemanager.posit.co/cran/latest")
  }
  NULL
}
