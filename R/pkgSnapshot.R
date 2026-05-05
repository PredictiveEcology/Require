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

  refs <- ifelse(isGH,
                 paste0(pkgs$GithubUsername, "/", pkgs$GithubRepo, "@", pkgs$GithubSHA1),
                 paste0(pkgs$Package, "@", pkgs$Version))

  ## pak may live outside destLib (especially under standAlone); make sure
  ## it's on the search path long enough to call pkg_download. find.package
  ## only searches .libPaths(); under standAlone it won't see the user lib,
  ## so fall back to R_LIBS_USER.
  pakLib <- tryCatch(dirname(find.package("pak")), error = function(e) NULL)
  if (is.null(pakLib)) {
    for (lp in strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep,
                        fixed = TRUE)[[1]]) {
      if (nzchar(lp) && file.exists(file.path(path.expand(lp), "pak", "DESCRIPTION"))) {
        pakLib <- path.expand(lp); break
      }
    }
  }
  origPaths <- .libPaths()
  if (!is.null(pakLib) && !pakLib %in% origPaths) {
    .libPaths(c(origPaths, pakLib))
    on.exit(.libPaths(origPaths), add = TRUE)
  }

  dlDir <- tempfile2("snapInstall_dl_")
  if (!dir.exists(dlDir)) dir.create(dlDir, recursive = TRUE)
  on.exit(unlink(dlDir, recursive = TRUE), add = TRUE)

  ## Prefer PPM Linux binaries when available: PPM serves pre-compiled
  ## tarballs indexed by distro, and pak honours options(repos), so prepending
  ## a PPM URL means recent versions skip compilation entirely. Older archived
  ## versions silently fall back to source. Opt out with
  ## options(Require.snapshotInstallerUsePPM = FALSE).
  if (isTRUE(getOption("Require.snapshotInstallerUsePPM", TRUE))) {
    ppm <- detectPPMLinuxRepo()
    if (!is.null(ppm)) {
      origRepos <- getOption("repos")
      hasPPM <- any(grepl("packagemanager.posit.co", origRepos, fixed = TRUE))
      if (!hasPPM) {
        options(repos = c(PPM = ppm, origRepos))
        on.exit(options(repos = origRepos), add = TRUE)
        messageVerbose("Using PPM Linux binaries: ", ppm,
                       verbose = verbose, verboseLevel = 1)
      }
    }
  }

  messageVerbose("Downloading ", length(refs),
                 " snapshot tarballs (pak cache reused if present)",
                 verbose = verbose, verboseLevel = 1)
  ## pak::pkg_download is all-or-nothing on the batch: if any single ref
  ## fails to resolve (CRAN-archive 404, deleted version, etc.) the whole
  ## call errors. Try batch first for speed; on failure, fall back to
  ## per-ref so we still install whatever IS resolvable, and report the
  ## rest. This is consistent with the "install closest, runnable" stance.
  dl <- tryCatch(pak::pkg_download(refs, dest_dir = dlDir),
                 error = function(e) e)
  if (inherits(dl, "error")) {
    messageVerbose("Batch resolution failed (",
                   sub("\n.*$", "", conditionMessage(dl)),
                   "); falling back to per-ref download",
                   verbose = verbose, verboseLevel = 1)
    rows <- vector("list", length(refs))
    failed <- character()
    for (i in seq_along(refs)) {
      r <- tryCatch(pak::pkg_download(refs[i], dest_dir = dlDir),
                    error = function(e) e)
      if (inherits(r, "error")) {
        failed <- c(failed, refs[i])
        next
      }
      rows[[i]] <- r
    }
    rows <- rows[lengths(rows) > 0]
    if (!length(rows)) stop("All snapshot refs failed to resolve via pak")
    dl <- do.call(rbind, rows)
    if (length(failed)) {
      messageVerbose(length(failed), " of ", length(refs),
                     " refs failed to resolve and will be skipped",
                     verbose = verbose, verboseLevel = 1)
      if (verbose >= 1) {
        cat("[snapshotInstaller] unresolvable refs:\n")
        cat(paste0("  ", failed), sep = "\n")
      }
    }
  }
  if (!is.data.frame(dl) || !"fulltarget" %in% names(dl)) {
    stop("pak::pkg_download returned an unexpected structure")
  }

  ## pak::pkg_download returns extra rows beyond the requested ref (e.g., the
  ## *current* CRAN version in addition to an archived pin). If we stage all
  ## of them, write_PACKAGES picks the newest and install.packages installs
  ## the wrong version. Filter to only the rows matching what we asked for:
  ## for CRAN pins that is (package, version); for GH SHA pins that is the
  ## row of type "github".
  pkgCol <- if ("package" %in% names(dl)) "package" else "Package"
  verCol <- if ("version" %in% names(dl)) "version" else "Version"
  typeCol <- if ("type" %in% names(dl)) "type" else NA_character_
  keep <- logical(nrow(dl))
  for (i in seq_len(nrow(pkgs))) {
    if (isGH[i]) {
      hit <- dl[[pkgCol]] == pkgs$Package[i] &
        (if (!is.na(typeCol)) dl[[typeCol]] == "github" else TRUE)
    } else {
      hit <- dl[[pkgCol]] == pkgs$Package[i] & dl[[verCol]] == pkgs$Version[i]
    }
    keep <- keep | hit
  }
  if (!any(keep)) {
    stop("Could not match any pak::pkg_download rows back to the snapshot refs")
  }
  dl <- dl[keep, , drop = FALSE]

  ## Stage filtered tarballs as a local source repo. write_PACKAGES then
  ## synthesizes the PACKAGES index from each tarball's DESCRIPTION.
  repoDir <- tempfile2("snapInstall_repo_")
  contribDir <- file.path(repoDir, "src", "contrib")
  if (!dir.exists(contribDir)) dir.create(contribDir, recursive = TRUE)
  on.exit(unlink(repoDir, recursive = TRUE), add = TRUE)

  ## On cache hits, pak does not materialise the file at `fulltarget`; the
  ## actual tarball lives in pak's cache at <cachepath>/src/contrib/<basename>.
  ## Fall back to that location when fulltarget is missing.
  pakCacheRoot <- tryCatch(pak::cache_summary()$cachepath,
                           error = function(e) NULL)
  pakCacheContrib <- if (!is.null(pakCacheRoot))
    file.path(pakCacheRoot, "src", "contrib") else NA_character_

  for (i in seq_len(nrow(dl))) {
    src <- dl$fulltarget[i]
    if (!file.exists(src) && !is.na(pakCacheContrib)) {
      alt <- file.path(pakCacheContrib, basename(src))
      if (file.exists(alt)) src <- alt
    }
    if (!file.exists(src)) next
    dest <- file.path(contribDir,
                      paste0(dl[[pkgCol]][i], "_", dl[[verCol]][i], ".tar.gz"))
    file.copy(src, dest, overwrite = TRUE)
  }
  tools::write_PACKAGES(contribDir, type = "source")

  reposURL <- paste0("file://", repoDir)
  messageVerbose("Installing ", nrow(pkgs),
                 " packages via install.packages(Ncpus=", Ncpus,
                 ", dependencies=FALSE)",
                 verbose = verbose, verboseLevel = 1)

  install.packages(pkgs$Package, lib = destLib, repos = reposURL,
                   type = "source", dependencies = FALSE, Ncpus = Ncpus,
                   quiet = isTRUE(verbose < 1))

  invisible(TRUE)
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
