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

  ## Prefer PPM Linux binaries when available: PPM serves pre-compiled
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
    ppm <- detectPPMLinuxRepo()
    if (!is.null(ppm) && !any(grepl("packagemanager.posit.co", newRepos, fixed = TRUE))) {
      newRepos <- c(PPM = ppm, newRepos)
      messageVerbose("Using PPM Linux binaries: ", ppm,
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
    out <- character()
    for (r in c(ppmRepos, cranRepos)) {
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

  pullBatch <- function(idx, urls) {
    suppressWarnings(tryCatch(
      utils::download.file(urls, destPaths[idx], method = "libcurl",
                           quiet = verbose < 2, mode = "wb"),
      error = function(e) NULL))
    vapply(idx, function(i) isGoodTarball(destPaths[i]), logical(1))
  }

  needed <- seq_len(nrow(pkgs))
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
        suppressWarnings(tryCatch(
          utils::download.file(u, newDest, method = "libcurl",
                               quiet = verbose < 2, mode = "wb"),
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
  if (length(needed)) {
    messageVerbose(length(needed), " of ", nrow(pkgs),
                   " refs failed to download and will be skipped",
                   verbose = verbose, verboseLevel = 1)
    if (verbose >= 1) {
      cat("[snapshotInstaller] unresolvable refs:\n")
      cat(paste0("  ", pkgs$Package[needed], "@", pkgs$Version[needed]),
          sep = "\n")
    }
    pkgs   <- pkgs[-needed, , drop = FALSE]
    isGH   <- isGH[-needed]
    destPaths <- destPaths[-needed]
  }
  if (!nrow(pkgs)) stop("All snapshot refs failed to download")

  ## Hand the local tarballs to pak via `local::<path>` refs. pak reads each
  ## tarball's DESCRIPTION to compute build-time topological order, runs
  ## parallel installs with its CLI progress, and reuses its on-disk binary
  ## cache where applicable.
  ##
  ## dependencies = NA (hard deps only) lets pak see each ref's Depends /
  ## Imports / LinkingTo and order builds accordingly. With dependencies =
  ## FALSE, pak treats every ref as standalone, so e.g. `arm` can start
  ## building before `coda` finishes and dies with "dependency 'coda' is
  ## not available for package 'arm'". A closed snapshot already contains
  ## every hard dep as another local:: ref, so pak resolves them within
  ## the input set and doesn't reach out to CRAN/PPM — no cascade-casualty
  ## risk. Soft deps (Suggests/Enhances) are still skipped since the
  ## snapshot is not guaranteed to include them.
  if (!requireNamespace("pak", quietly = TRUE))
    stop("pak is required for installSnapshotViaInstallPackages")
  localRefs <- paste0("local::", destPaths)
  messageVerbose("Installing ", length(localRefs),
                 " packages via pak::pkg_install(local::..., dependencies = NA)",
                 verbose = verbose, verboseLevel = 1)
  pakErr <- tryCatch({
    pak::pkg_install(localRefs, lib = destLib,
                     dependencies = NA, upgrade = FALSE, ask = FALSE)
    NULL
  }, error = function(e) e)

  if (!is.null(pakErr)) {
    ## pak's solver is all-or-nothing: any unsolvable constraint in the
    ## snapshot (e.g., a version pin that doesn't satisfy a transitive
    ## dependent's `(>= X)` requirement, or a missing archived dep) blocks
    ## every package. Fall back to install.packages, which is permissive:
    ## it installs what it can and fails per-package on broken deps. The
    ## pak diagnostic is preserved so the user can see what to fix in the
    ## snapshot for a clean future run.
    messageVerbose(
      "pak refused: ", sub("\n.*$", "", conditionMessage(pakErr)),
      "\n  falling back to install.packages for partial install",
      verbose = verbose, verboseLevel = 1)
    repoDir <- tempfile2("snapInstall_repo_")
    contribDir <- file.path(repoDir, "src", "contrib")
    if (!dir.exists(contribDir)) dir.create(contribDir, recursive = TRUE)
    on.exit(unlink(repoDir, recursive = TRUE), add = TRUE)
    for (i in seq_along(destPaths)) {
      dest <- file.path(contribDir, basename(destPaths[i]))
      file.copy(destPaths[i], dest, overwrite = TRUE)
    }
    tools::write_PACKAGES(contribDir, type = "source")
    reposURL <- paste0("file://", repoDir)
    suppressWarnings(utils::install.packages(
      pkgs$Package, lib = destLib, repos = reposURL,
      type = "source", dependencies = FALSE, Ncpus = Ncpus,
      quiet = isTRUE(verbose < 1)))
  }

  invisible(TRUE)
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
detectPPMLinuxRepo <- function() {
  if (!identical(Sys.info()[["sysname"]], "Linux")) return(NULL)
  f <- "/etc/os-release"
  if (!file.exists(f)) return(NULL)
  ll <- tryCatch(readLines(f, warn = FALSE), error = function(e) character())
  m <- grep("^VERSION_CODENAME=", ll, value = TRUE)
  if (!length(m)) return(NULL)
  codename <- sub('^VERSION_CODENAME=["]?([^"]+)["]?$', "\\1", m[1])
  if (!nzchar(codename)) return(NULL)
  paste0("https://packagemanager.posit.co/cran/__linux__/", codename, "/latest")
}
