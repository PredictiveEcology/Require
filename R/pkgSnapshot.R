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
#' \dontrun{
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
  pkgs <- pkgs[!Package %in% .basePkgs]
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
  ## destLib — partial progress accumulates across restarts.
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
    binRelpath <- file.path("Require/snapshot/bin",
                             R.version$platform, rverShort)
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
    rowRepo <- pkgs$Repository[i]
    rowRepos <- if (!is.na(rowRepo) && grepl("^https?://", rowRepo)) rowRepo
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
  destPaths <- file.path(dlDir,
                         paste0(pkgs$Package, "_",
                                ifelse(isGH, substr(pkgs$GithubSHA1, 1, 7),
                                       pkgs$Version), ".tar.gz"))

  ## Parallel multi-pass downloader. Each pass: take the next candidate URL
  ## for every still-missing ref and pass them all to one libcurl multi call.
  ## libcurl multi can intermittently drop bytes mid-stream — the file ends
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

  ## quiet = TRUE is mandatory for libcurl-multi to actually run downloads in
  ## parallel. With quiet = FALSE, R serializes the URLs through libcurl one
  ## at a time so it can attribute progress lines to individual files —
  ## defeating the whole point of the multi handle. Per-file progress is
  ## useless inside a 378-URL batch anyway; we print one "Downloading N
  ## tarballs" announcement above and that's all the user needs.
  ##
  ## Chunk the batch: libcurl multi opens a socket per URL. macOS's default
  ## file-descriptor limit is ~256, so a 378-URL single multi call exhausts
  ## the limit and fails *all* downloads silently (the user's symptom: 4
  ## retries each report "for 378 ref(s)" — zero succeeded). Linux's higher
  ## default (≥1024) masks this. Chunking to 50 URLs per multi call keeps
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
  cacheList <- NULL
  if (requireNamespace("pkgcache", quietly = TRUE)) {
    cacheList <- tryCatch(pkgcache::pkg_cache_list(),
                          error = function(e) NULL)
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
      for (i in seq_len(nrow(pkgs))) {
        if (isGH[i]) {
          urlNeedle <- paste0(pkgs$GithubUsername[i], "/",
                              pkgs$GithubRepo[i], "/archive/",
                              pkgs$GithubSHA1[i])
          hit <- cacheList[grepl(urlNeedle, cacheList$url, fixed = TRUE) &
                            !is.na(cacheList$fullpath), , drop = FALSE]
        } else {
          hit <- cacheList[!is.na(cacheList$package) &
                            !is.na(cacheList$version) &
                            cacheList$package == pkgs$Package[i] &
                            cacheList$version == pkgs$Version[i] &
                            !is.na(cacheList$fullpath), , drop = FALSE]
        }
        if (nrow(hit)) {
          ## Prefer a built binary that matches OUR platform + R version
          ## (skips compile on install) over a source tarball (would
          ## recompile). Order: ourPlatformBinary > anyBinary > source.
          isBin <- !is.na(hit$built) & as.logical(hit$built)
          ## Strict platform + rversion match for binaries: a binary
          ## built for a different R version or arch is ABI-incompatible
          ## and would crash on load. Require explicit values, not NA.
          isOurBin <- isBin &
                       !is.na(hit$platform) &
                       hit$platform == R.version$platform &
                       !is.na(hit$rversion) &
                       hit$rversion == ourRverShort
          ## Drop binaries for OTHER platforms entirely — using them
          ## would break at load time. Keep our-platform binaries and
          ## all source tarballs (pkgcache stores PPM-served Linux
          ## tarballs with built=NA but they have prebuilt libs/ —
          ## R CMD INSTALL handles either).
          keep <- isOurBin | !isBin
          hit <- hit[keep, , drop = FALSE]
          isOurBin <- isOurBin[keep]
          ## Prefer our-platform binary over source: skips compile.
          hit <- hit[order(-as.integer(isOurBin)), , drop = FALSE]
          cached <- hit$fullpath[1]
          if (file.exists(cached) && isGoodTarball(cached)) {
            file.copy(cached, destPaths[i], overwrite = TRUE)
            cachedHits[i] <- TRUE
          }
        }
      }
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
  for (attempt in seq_len(maxAttempts)) {
    if (!length(needed)) break
    if (attempt == 1L) {
      messageVerbose("Downloading ", length(needed),
                     " snapshot tarballs in parallel via libcurl",
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
        ## help the user — we already log per-package substitution status
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
  }
  if (!nrow(pkgs)) stop("All snapshot refs failed to download")

  ## Repackage GitHub-archive tarballs into proper R source tarballs.
  ## GitHub's archive/<sha>.tar.gz (what `git archive` wraps) is broken
  ## as an R-package source tarball in two ways:
  ##   1. Starts with a `pax_global_header` tar entry (git's per-archive
  ##      metadata). pak's internal pkgdepends tar reader chokes:
  ##        "! Line starting 'pax_global_header ...' is malformed!"
  ##      → pak refuses the whole install plan (it's all-or-nothing).
  ##   2. Top-level dir is `<repo>-<sha>/` not `<pkg>/`, so install.packages
  ##      via a file:// repo path fails: `tar: <pkg>/DESCRIPTION not found`.
  ##
  ## We tried `tar -czf` to repackage but pak still rejects with
  ## "Line starting 'visualTest/DESCRIPTI ...' is malformed" — pak's
  ## tar reader is fussier than ustar requires. Use `R CMD build`
  ## instead: it produces a canonical R source tarball that pak
  ## accepts (also strips .Rbuildignore'd files, computes MD5, etc.
  ## — all the things make a proper CRAN-style package).
  ghIdxs <- which(isGH)
  if (length(ghIdxs) && nzchar(Sys.which("tar"))) {
    repacked <- 0L
    Rbin <- file.path(R.home("bin"), "R")
    for (i in ghIdxs) {
      if (!file.exists(destPaths[i]) || !isGoodTarball(destPaths[i])) next
      pkgName <- pkgs$Package[i]
      workDir <- tempfile2("snapInstall_repack_")
      dir.create(workDir, recursive = TRUE, showWarnings = FALSE)
      rcExtract <- tryCatch(
        system2("tar", c("xzf", shQuote(destPaths[i]),
                         "-C", shQuote(workDir)),
                stdout = FALSE, stderr = FALSE),
        error = function(e) -1L)
      if (!identical(as.integer(rcExtract), 0L)) {
        unlink(workDir, recursive = TRUE); next
      }
      inner <- list.files(workDir, full.names = TRUE)
      isDir <- file.info(inner)$isdir
      isDir[is.na(isDir)] <- FALSE
      inner <- inner[isDir]
      if (!length(inner)) {
        unlink(workDir, recursive = TRUE); next
      }
      target <- file.path(workDir, pkgName)
      if (!identical(basename(inner[1]), pkgName)) {
        if (!file.rename(inner[1], target)) {
          unlink(workDir, recursive = TRUE); next
        }
      }
      ## R CMD build writes <pkg>_<ver>.tar.gz to its current working
      ## directory. Run with cwd = workDir; collect the output tarball
      ## from there. CRITICAL: pak's local:: resolver validates that
      ## the tarball's filename version matches DESCRIPTION's Version
      ## field, otherwise it errors with the misleading "Line starting
      ## '<pkg>/DESCRIPTI ...' is malformed!" — actually a version
      ## mismatch error. Our destPaths uses <pkg>_<sha7>.tar.gz for GH
      ## refs (sha != version), so we must REPLACE destPaths[i] with
      ## the version-named output, not just copy contents.
      oldwd <- setwd(workDir)
      rcBuild <- tryCatch(
        system2(Rbin, c("CMD", "build", "--no-build-vignettes",
                        "--no-manual", shQuote(pkgName)),
                stdout = FALSE, stderr = FALSE),
        error = function(e) -1L)
      setwd(oldwd)
      if (identical(as.integer(rcBuild), 0L)) {
        built <- list.files(workDir, pattern = paste0("^",
                                                       pkgName,
                                                       "_.*\\.tar\\.gz$"),
                            full.names = TRUE)
        if (length(built)) {
          ## Move (not copy) into dlDir under R-build's filename.
          ## destPaths[i] = "<dlDir>/<pkg>_<sha7>.tar.gz" → discard;
          ## new destPaths[i] = "<dlDir>/<pkg>_<DescriptionVersion>.tar.gz".
          newDest <- file.path(dirname(destPaths[i]), basename(built[1]))
          if (file.copy(built[1], newDest, overwrite = TRUE)) {
            unlink(destPaths[i])  # remove original sha-named copy
            destPaths[i] <- newDest
            if (isGoodTarball(destPaths[i])) repacked <- repacked + 1L
          }
        }
      }
      unlink(workDir, recursive = TRUE)
    }
    if (verbose >= 1)
      messageVerbose("Repackaged ", repacked, "/", length(ghIdxs),
                     " GitHub-archive tarball(s) via R CMD build ",
                     "so pak's resolver accepts them",
                     verbose = verbose, verboseLevel = 1)
  }

  ## Populate pkgcache (pak's cache) with anything we just downloaded so
  ## subsequent runs hit the cache pre-filter above. Skip refs that
  ## already had a cache hit (cachedHits — they're already there) and
  ## skip if pkgcache is unavailable. Best-effort: a failed cache_add
  ## should NEVER block the install.
  if (requireNamespace("pkgcache", quietly = TRUE)) {
    ## cachedHits was indexed against the original pre-filter pkgs;
    ## by here pkgs has been trimmed for unresolved refs. Recompute
    ## new-vs-existing per current row by checking the cache list once.
    nowCacheList <- tryCatch(pkgcache::pkg_cache_list(),
                             error = function(e) NULL)
    addedCount <- 0L
    for (i in seq_len(nrow(pkgs))) {
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
      ## Skip if a hit for this URL is already in the cache (this run's
      ## download supplied it, e.g.) — pkg_cache_add_file would dedupe
      ## anyway but checking saves a copy.
      already <- !is.null(nowCacheList) && nrow(nowCacheList) &&
        any(nowCacheList$url == url, na.rm = TRUE) &&
        any(file.exists(nowCacheList$fullpath[
              !is.na(nowCacheList$url) & nowCacheList$url == url]), na.rm = TRUE)
      if (already) next
      ## Pass relpath = "Require/snapshot" so the cached copies live at
      ## a stable predictable path (<cache>/Require/snapshot/<filename>)
      ## rather than under whatever tempdir destPaths happens to come
      ## from. Without an explicit relpath, pkgcache defaults to
      ## dirname(file) which is our scratch tempdir — fine functionally
      ## (file_copy still works) but the recorded fullpath is brittle.
      addRes <- tryCatch(
        pkgcache::pkg_cache_add_file(
          file = destPaths[i],
          relpath = "Require/snapshot",
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
      messageVerbose("Added ", addedCount, "/", nrow(pkgs),
                     " tarball(s) to pkgcache for future reuse",
                     verbose = verbose, verboseLevel = 1)
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
  ## ggplot2_3.4.4 but PPM has 4.0.3 → pak tries to fetch 4.0.3 instead
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
  ##   - visualTest GH archive's pax_global_header → repackage via R
  ##     CMD build (above)
  ##   - GH tarball filename version != DESCRIPTION Version → rename
  ##     destPaths[i] to <pkg>_<DescriptionVersion>.tar.gz (above)
  ## Remaining "pak refused" is the version-resolver quirk noted above
  ## and is fundamental to how pak's pkgdepends works; the install.packages
  ## fallback is correct for our closed-snapshot use case.
  ##
  ## Set options(Require.snapshotInstallerPakSilent = TRUE) to sink
  ## pak's noisy resolver output to a tempfile during the attempt.
  localRefs <- paste0("local::", destPaths)
  pakLogTail <- character()
  pakErr <- NULL
  if (requireNamespace("pak", quietly = TRUE)) {
    messageVerbose("Trying pak::pkg_install (binary cache; ",
                   "fallback: install.packages)",
                   verbose = verbose, verboseLevel = 1)
    silent <- isTRUE(getOption("Require.snapshotInstallerPakSilent", FALSE))
    if (silent) {
      pakLogPath <- tempfile2("pak_log_")
      pakLogCon <- file(pakLogPath, "w")
      pakErr <- tryCatch({
        sink(pakLogCon, type = "output")
        sink(pakLogCon, type = "message")
        pak::pkg_install(localRefs, lib = destLib,
                         dependencies = NA, upgrade = FALSE, ask = FALSE)
        NULL
      }, error = function(e) e, finally = {
        try(sink(NULL, type = "message"), silent = TRUE)
        try(sink(NULL, type = "output"), silent = TRUE)
        try(close(pakLogCon), silent = TRUE)
      })
      if (file.exists(pakLogPath)) {
        pakLog <- tryCatch(readLines(pakLogPath, warn = FALSE),
                           error = function(e) character())
        if (requireNamespace("cli", quietly = TRUE))
          pakLog <- cli::ansi_strip(pakLog)
        pakLog <- gsub("\r", "", pakLog)
        pakLog <- pakLog[nzchar(trimws(pakLog))]
        pakLog <- pakLog[!grepl("^[[:space:]]*[✨⚖→✖ℹ✔⠇⠈-⠏⢨⢹⣨⣩]", pakLog)]
        pakLog <- pakLog[!grepl(
          "^[[:space:]]*(Found|Resolving|Updating metadata|Downloading|Will install|Will download|Getting|Installing|Got |Installed |Will update|Checking installed|Checking for [0-9]+)",
          pakLog)]
        pakLogTail <- utils::tail(pakLog, 8)
      }
    } else {
      pakErr <- tryCatch({
        pak::pkg_install(localRefs, lib = destLib,
                         dependencies = NA, upgrade = FALSE, ask = FALSE)
        NULL
      }, error = function(e) e)
    }
    ## Pull pak's structured detailed error (the wrapper exception
    ## "! error in pak subprocess" hides it). pak::last_error() returns
    ## the most recent error with the actual subprocess message and call
    ## stack — far more actionable than the wrapper.
    pakDetail <- character()
    if (requireNamespace("pak", quietly = TRUE)) {
      le <- tryCatch(pak::last_error(), error = function(e) NULL)
      if (!is.null(le)) {
        msg <- tryCatch(conditionMessage(le), error = function(e) "")
        if (nzchar(msg)) pakDetail <- strsplit(msg, "\n", fixed = TRUE)[[1]]
        ## Also include the chained parent's message, where pak typically
        ## stashes the actual subprocess error.
        parentMsg <- tryCatch(conditionMessage(le$parent),
                              error = function(e) "")
        if (nzchar(parentMsg))
          pakDetail <- c(pakDetail,
                         strsplit(parentMsg, "\n", fixed = TRUE)[[1]])
        if (requireNamespace("cli", quietly = TRUE))
          pakDetail <- cli::ansi_strip(pakDetail)
        pakDetail <- pakDetail[nzchar(trimws(pakDetail))]
      }
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
    suppressWarnings(utils::install.packages(
      pkgs$Package, lib = destLib, repos = reposURL,
      type = "source", dependencies = NA, Ncpus = Ncpus,
      keep_outputs = outDir,
      quiet = isTRUE(verbose < 1)))
  } else if (requireNamespace("pak", quietly = TRUE)) {
    messageVerbose("[snapshotInstaller] installed via pak (binary cache)",
                   verbose = verbose, verboseLevel = 1)
  } else {
    ## pak isn't installed at all — go directly to install.packages.
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
    txt <- paste(unlist(desc), collapse = ", ")
    if (!nzchar(txt)) next
    refs <- unlist(strsplit(txt, ",\\s*"))
    refs <- refs[nzchar(refs) & !is.na(refs)]
    nms <- extractPkgName(refs)
    nms <- nms[nzchar(nms) & !is.na(nms) & nms != "R"]
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
    ## fallback) isn't in options(repos) — install.packages got it via the
    ## `repos` arg only — so we don't need to filter.
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

  ## (Binary caching is now registered via on.exit earlier in this
  ## function so partial installs — pak crash, install.packages
  ## interrupt, error in auto-fill — still get the binaries that DID
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
      "namespace [‘']?(.+?)[’']? ([0-9.\\-]+) is being loaded, but >=? ([0-9.\\-]+) is required",
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
      "ERROR: dependency [‘']?(.+?)[’']? is not available for package",
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
      lastLines <- utils::tail(txt[nzchar(txt)], 6)
      diagnostics[[p]] <- list(
        pkg = p, status = "compile-failed",
        reason = "compilation failed (system lib mismatch or R API change)",
        fix = paste0(
          "common causes: missing system library (apt/brew install ...), ",
          "R API change (e.g. R 4.5 removed Calloc/Free), or wrong toolchain. ",
          "Try a newer pin, a binary repo (PPM), or install the system lib. ",
          "Last log lines:\n        ",
          paste(lastLines, collapse = "\n        ")))
      next
    }

    ## Fallthrough: missing without a recognised pattern. The most common
    ## cause is a cascade — install.packages refused to even attempt the
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
