utils::globalVariables(c(
  "..keepCols", "op", "package", "ref",
  ## data.table NSE in pakInstallFiltered's deduplication block:
  "isNonCRAN", "hasNonCRAN", ".versionSpecPrio",
  ## data.table NSE in pakDepsToPkgDT:
  "src_type"
))

.txtFailedToBuildSrcPkg <- "Failed to build source package"
.txtCantFindPackage <- "Can't find package called "

# Escape regex metacharacters so an arbitrary string can be safely interpolated
# into a regex pattern. Used when pak's error output (which can contain dots,
# brackets, parentheses, or stray non-printable bytes) is spliced into grep
# patterns inside pakErrorHandling.
regexEscape <- function(x) {
  if (!length(x)) return(x)
  gsub("([][\\\\.|()*+?{}^$/-])", "\\\\\\1", x, perl = TRUE)
}

# Wrap a pak call to honour Require's verbose level.
# pak produces two kinds of output:
#   (1) Progress/spinner -- controlled by options(pkg.show_progress).
#       pak's remote() passes pkg.show_progress = is_verbose() to its subprocess,
#       where is_verbose() reads options(pkg.show_progress) (falling back to
#       interactive()). Setting this option before calling pak is sufficient.
#   (2) cli messages forwarded from the subprocess as message() conditions
#       (class "callr_message"). suppressMessages() catches these.
#
# Three levels:
#   verbose >= 1  : full output  -- progress bars + messages (pak defaults)
#   verbose == 0  : messages only -- no progress spinner, cli messages still shown
#   verbose <= -1 : silent       -- no progress, no messages
#
# Two suppression mechanisms are needed for verbose <= -1:
#   (1) options(pkg.show_progress = FALSE) -- tells pak's subprocess not to render
#       the animated progress spinner.
#   (2) suppressMessages() -- catches cli_message conditions forwarded from the
#       subprocess as message() conditions (e.g. "Installing X packages...").
#   (3) capture.output(type = "output") -- catches anything written directly to
#       stdout via cat()/writeLines() by pak's cli_server_default renderer, such
#       as "i No downloads are needed, 1 pkg is cached".
# TRUE iff the user has explicitly opted IN to pak's automatic
# system-requirements installation, via either the `PKG_SYSREQS` env var
# set to a truthy value or `options(pkg.sysreqs = TRUE)`. Used by
# .onLoad (to decide whether to force the subsystem off) and pakCall (to
# decide whether to re-assert off per call). Default is opt-OUT: if the
# user has said nothing, Require keeps pak's sudo-capable sysreqs path
# disabled. An explicit opt-in is honoured everywhere -- the user's
# machine, the user's sudo, the user's choice.
.sysreqsTruthy <- function(x) {
  isTRUE(x) ||
    (is.character(x) && length(x) == 1L &&
       tolower(trimws(x)) %in% c("true", "yes", "on", "1"))
}
.sysreqsUserOptedIn <- function() {
  envv <- Sys.getenv("PKG_SYSREQS", unset = "")
  if (nzchar(envv) && .sysreqsTruthy(envv)) return(TRUE)
  opt <- getOption("pkg.sysreqs", default = NULL)
  if (!is.null(opt) && .sysreqsTruthy(opt)) return(TRUE)
  FALSE
}

pakCall <- function(expr, verbose = getOption("Require.verbose")) {
  ## Inline null-coalesce: `%||%` is base in R 4.4+ but not 4.3, and Require
  ## doesn't import it from rlang. Without this, pakCall errors on R 4.3
  ## (silently, since try() in callers swallows it), turning every pak
  ## install attempt into a "could not be installed" no-op.
  if (is.null(verbose)) verbose <- 0L
  ## Defense-in-depth for the CRAN sudo-probe issue (see zzz.R .onLoad):
  ## re-assert that pak's sysreqs subsystem is OFF on every pak call, so
  ## nothing that toggles env/options mid-session can let pak run
  ## `sudo sh -c id` / apt-get. NOT applied when the user explicitly
  ## opted in at load time (.sysreqsUserOptIn captured in zzz.R) -- an
  ## informed user who wants pak to auto-install system libraries keeps
  ## that choice; it is their machine and their sudo. CRAN-safe because
  ## CRAN's check env never sets the opt-in, so the force-off path
  ## always applies there. Default (flag absent) = force off (safe).
  if (!isTRUE(get0(".sysreqsUserOptIn", envir = pkgEnv(), inherits = FALSE))) {
    Sys.setenv(PKG_SYSREQS = "false", PKG_SYSREQS_SUDO = "false")
    options(pkg.sysreqs = FALSE, pkg.sysreqs_sudo = FALSE)
  }
  if (verbose <= -1L) {
    old <- options(pkg.show_progress = FALSE)
    on.exit(options(old), add = TRUE)
    .res <- NULL
    utils::capture.output(.res <- suppressMessages(force(expr)), type = "output")
    .res
  } else if (verbose == 0L) {
    old <- options(pkg.show_progress = FALSE)
    on.exit(options(old), add = TRUE)
    force(expr)
  } else {
    force(expr)
  }
}

pakErrorHandling <- function(err, pkg, packages, verbose = getOption("Require.verbose")) {
  grp <- c(
    .txtCntInstllDep, .txtFailedToBuildSrcPkg, .txtConflictsWith,
    .txtCantFindPackage, .txtMissingValueWhereTFNeeded, .txtCldNotSlvPkgDeps,
    .txtFailedToDLFrom, .txtPakNoPkgCalledPak, .txtUnknownArchiveType
  )
  ## All grp entries are plain literals except .txtFailedToDLFrom (index 7),
  ## which is a regex containing ".+". fixed=TRUE is several times faster
  ## than full regex matching, and these greps fire on every pak error.
  grpFixed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
  spl <- c(" |\\)", "\033\\[..{0,1}m", "\033\\[..{0,1}m| |@", " |\\. ", "NULL", "NULL", "NULL", "NULL", "NULL")
  pat <- c("dependency", grp[2], "with", "called", "NULL", "NULL", "NULL", "NULL", "NULL")
  splitStr <- strsplit(err, split = "\n")[[1]]
  ## Pre-screen: pakErrorHandling is called once per failed pak::pkg_deps,
  ## which can be hundreds of times during a snapshot install. Skip the
  ## entire 9-pattern loop when none match (very common for benign errors).
  errStr <- paste(splitStr, collapse = "\n")
  hasAny <- vapply(seq_along(grp), function(j) {
    grepl(grp[j], errStr, fixed = grpFixed[j])
  }, logical(1))
  if (!any(hasAny)) return(packages)
  for (i in which(hasAny)) {
    a <- grep(grp[i], splitStr, value = TRUE, fixed = grpFixed[i])
    if (length(a)) {
      a1 <- gsub("\\.$", "", a)

      b <- strsplit(a1, split = spl[i])
      whDeps <- sapply(b, grep, pattern = pat[i])

      pkg2 <- gsub("@.+$", "", pkg)

      if (grp[i] == .txtUnknownArchiveType) {
        # redo
        splitStr
        whDeps <- grep(grp[i], splitStr)
        pkgLong <- splitStr[whDeps - 1]
        pkgLong <- strsplit(pkgLong, spl[2])[[1]]
        pkgLong <- basename(pkgLong)
        filename <- pkgLong[nchar(pkgLong) > 2]
        pkg2 <- extractPkgName(filenames = basename(filename))
        whRm <- grep(pkg2, packages)
        packages2 <- pakCacheDeleteTryAgain(pkg2 = pkg2, packages = packages, whRm = whRm)
        if (!identical(length(packages2), length(packages)))
          packages <- packages[-whRm]
      }
      # For "Failed to build source package X" errors, the ANSI-based splitting
      # (spl[2] = ANSI escape pattern) fails when as.character(err) has no ANSI codes
      # (which is the case when try(..., silent=TRUE) captures the plain message).
      # Directly extract the package name from the error text as a reliable fallback.
      if (grp[i] == .txtFailedToBuildSrcPkg && length(pkg2) > 1) {
        directName <- sub(".*Failed to build source package ([^. \\t\\n\\033]+).*",
                          "\\1", paste(splitStr, collapse = " "))
        directName <- gsub("\\033\\[[0-9;]*m", "", directName)  # strip residual ANSI
        directName <- trimws(directName)
        if (nzchar(directName) && directName != paste(splitStr, collapse = " ")) {
          pkg2 <- directName
        }
      }

      if (length(pkg2) > 1) {
        d <- Map(x = b, whDep = whDeps, function(x, whDep) {
          idx <- whDep + 1L
          if (length(idx) == 0L || idx > length(x)) return(character())
          x[[idx]]
        })
        pkg2 <- gsub("@.+$", "", unlist(d))
      }
      pkgNoVersion <- trimVersionNumber(pkg2)

      vers <- tryCatch(Map(x = b, whDep = whDeps, function(x, whDep) {
        idx <- whDep + 3L
        if (length(idx) == 0L || idx > length(x)) return("")
        x[[idx]]
      }), error = function(x) "")
      # Defensive: pkgNoVersion / vers come from parsing pak's error output and may
      # contain regex metacharacters or even non-printable bytes that, when spliced
      # into a regex, produce an invalid pattern (e.g. TRE "Unknown collating
      # element" from stray brackets).  Escape them so a malformed pak error
      # message can never crash the parser.
      pkgNoVersionEsc <- regexEscape(as.character(pkgNoVersion))
      versEsc         <- regexEscape(as.character(unlist(vers)))
      patVec <- paste0("^", pkgNoVersionEsc, ".*", versEsc, "|/",
                              pkgNoVersionEsc, ".*", versEsc)
      whRm <- unlist(unname(lapply(patVec, function(p) {
        tryCatch(grep(p, pkg), error = function(e) integer(0))
      })))

      if (grp[i] == .txtMissingValueWhereTFNeeded) {
        packages <- pakGetArchive(pkgNoVersion, packages = packages, whRm = whRm)
        break
      }
      if (grp[i] == .txtFailedToDLFrom) {
        #
      }

      if (grp[i] == .txtCntInstllDep) {
        whRmAll <- integer()
        for (j in seq_along(pkgNoVersion)) {
          if (isGH(pkgNoVersion[j])) { # "PredictiveEcology/fpCompare (>=2.0.0)"
            if (is.na(pkg[whRm[j]]) || !length(whRm[j])) next
            isOK <- pakCheckGHversionOK(pkg[whRm[j]])
            # pkgDT <- toPkgDTFull(pkg)
            # dl <- pak::pkg_download(trimVersionNumber(pkg), dest_dir = tempdir2())
            # vers <- extractVersionNumber(filenames = basename(dl$fulltarget))
            # isOK <- compareVersion2(vers, versionSpec = pkgDT$versionSpec, inequality = pkgDT$inequality)
            if (isOK %in% FALSE)
              whRmAll <- c(whRmAll, whRm[j])
              # packages <- packages[-whRm[j]]
            next
          }
          packages2 <- pakGetArchive(pkgNoVersion[j], packages = packages, whRm = whRm[j])
          if (!identical(length(packages2), length(packages)))
            whRmAll <- c(whRmAll, whRm[j])

        }
        packages <- packages[-whRmAll]
        break
      }

      dups <- Map(x = b, function(x) duplicated(x))


      if (sum(unlist(dups)) >= 2 || grp[i] == .txtCldNotSlvPkgDeps) {
        # likely a repository that has a 4th version number element,
        #  e.g., NetLogoR 1.0.5.9001 on e.g., predictiveecology.r-universe.dev
        repoToUse <- unlist(whIsOfficialCRANrepo(currentRepos = getOption("repos")))
        packages <- pakGetArchive(pkgNoVersion, packages = packages, whRm = whRm)
        # options(repos = repoToUse)
        break
      }
      pkgPossOther <- extractPkgName(filenames = basename(pkg))
      if (identical(pkg2, pkgPossOther)) {
        # vers <- tryCatch(Map(x = b, whDep = whDeps, function(x, whDep) x[[whDep + 3]]),
        #                  error = function(x) "")
        # whRm <- unlist(unname(lapply(paste0("^", pkg2, ".*", vers), grep, x = pkg)))
        if (length(whRm) > 0) {
          if (grp[i] == .txtCantFindPackage) {
            # This is the case when a package is archived
            packages2 <- pakGetArchive(pkg2, packages, whRm)
            if (identical(packages2, packages)) { # doesn't exist
              packages <- packages[-whRm]
              warning(err)
              break
            }
            messageVerbose(packages2, " may be archived from CRAN; checking archives... ",
                           verbose = verbose)
            packages <- packages2
            break
          } else {
            if (grp[i] == .txtFailedToBuildSrcPkg) {
              packages <- pakCacheDeleteTryAgain(pkg2, packages, whRm)
              break
            }
            if (grp[i] == .txtPakNoPkgCalledPak) {
              stop("\nTry running: \npak::meta_clean()")
              # stop(err)
            }
            packages <- packages[-whRm]
            break
          }
        } else if (grp[i] == .txtCantFindPackage) {
          # Transitive dep: pkg2 is not directly in packages (whRm is empty).
          # Append the archive url:: ref so the caller can include it in the retry.
          packages2 <- pakGetArchive(pkg2, packages, whRm = integer(0))
          if (!identical(length(packages2), length(packages))) {
            packages <- packages2
          }
          break
        } else {
          stop(err)
        }
      } else {
        packages <- pakCacheDeleteTryAgain(pkg2, packages, whRm)
        break
      }
    }
  }
  packages
}

pakPkgSetup <- function(pkgs, doDeps, verbose = getOption("Require.verbose")) {

  # rm spaces
  pkgs <- gsub(" {0,3}(\\()(..{0,1}) {0,4}(.+)(\\))", " \\1\\2\\3\\4", pkgs)

  if (TRUE) {
    deps <- list()
    deps <- pkgDep(pkgs, which = doDeps) # |> Cache()

    depsFlat <- unlist(unname(deps))
    depsFlat <- unique(depsFlat)
    pkgDT <- toPkgDTFull(depsFlat)
    if (FALSE) {
      setorderv(pkgDT, "Package")
      a <- pkgDT[, any(any(grep("==", inequality)) & any(grep(">=", inequality))), by = "Package"][V1 %in% TRUE]
      a <- pkgDT[a, on = "Package"]
      a <- a[!is.na(inequality)]
    }
    pkgDT <- trimRedundancies(pkgDT)
    pkgs <- pkgDT$packageFullName

  }
  # Convert equals to @
  whLT <- grep("<", pkgs)
  whEquals <- whEquals(pkgs) # grep("==", pkgs)
  isGH <- isGH(pkgs) # grepl("^[[:alpha:]]+/.+", pkgs)

  if (length(whLT)) {
    vers <- Map(pkg = pkgs[whLT], isGH = isGH[whLT], function(pkg, isGH) {
      pkgDT <- toPkgDTFull(pkg)
      if (isGH) {
        his <- pakCall(pak::pkg_deps(trimVersionNumber(pkg)), verbose)
        his <- his[his$package %in% extractPkgName(pkg), ]
        setnames(his, old = "version", new = "Version")
      } else {
        his <- pakCall(pak::pkg_history(trimVersionNumber(pkg)), verbose)
      }
      versOK <- compareVersion2(his$Version, pkgDT$versionSpec, pkgDT$inequality)
      if (all(versOK %in% FALSE)) {
        warning(msgPleaseChangeRqdVersion(trimVersionNumber(pkg), ">=", names(versOK)))
        NA
      } else {
        keep <- max(which(versOK))
        his[keep, ]$Version
      }

    })
    NAvers <- is.na(vers)
    if (any(NAvers)) {# means none available
      pkgs <- pkgs[-whLT[NAvers]]
      whLT <- whLT[!NAvers]
      # need redo these
      whEquals <- whEquals(pkgs) # grep("==", pkgs)
      isGH <- isGH(pkgs) # grepl("^[[:alpha:]]+/.+", pkgs)
    } else {
      pkgs[whLT] <- gsub("\\(<={0,1}(.+)\\)", paste0("(==", vers, ")"), pkgs[whLT])
    }
  }

  ind <- seq_along(pkgs)

  whEquals <- sort(c(whEquals, whLT))

  whGH <- which(isGH)

  isGT <- isGT(pkgs) # grepl(">", pkgs)
  whGT <- which(isGT)

  whHEAD <- grep("\\(HEAD\\)", pkgs)

  whAlreadyColoned <- grep("::", pkgs)

  whNormal <- ind[-sort(c(whEquals, whGT, whLT, whHEAD, whGH, whAlreadyColoned))]

  if (length(whEquals))
    pkgs[whEquals] <- equalsToAt(pkgs[whEquals])
    # pkgs[whEquals] <- gsub(" {0,3}\\(== {0,4}(.+)\\)", "@\\1", pkgs[whEquals])
  if (length(whLT))
    pkgs[whLT] <- lessThanToAt(pkgs[whLT])
    # pkgs[whLT] <- gsub(" {0,3}\\(<= {0,4}(.+)\\)", "@\\1", pkgs[whLT])
  if (length(whHEAD))
    pkgs[whHEAD] <- HEADtoNone(pkgs[whHEAD])
    # pkgs[whHEAD] <- gsub(" {0,3}\\(HEAD\\)", "", pkgs[whHEAD])

  if (length(whNormal))
    pkgs[whNormal] <- paste0("any::", pkgs[whNormal])

  pkgsForDESCRIPTIONFile <- if (length(whGT)) pkgs[whGT] else character()
  pkgsForDirect <- if (length(whGT)) pkgs[-whGT] else pkgs

  list(DESC = pkgsForDESCRIPTIONFile, direct = pkgsForDirect)
}


pakRequire <- function(packages, libPaths, doDeps, upgrade, verbose, packagesOrig) {
  if (!requireNamespace("pak")) stop("Please install pak")

  packages <- unique(packages)
  packages <- packages[!extractPkgName(packages) %in% .basePkgs]

  pkgs <- list()
  for (i in 1:15) {
  # while(!identical(packages, pkgs) ) {
    pkgs <- packages
    if (length(pkgs)) {
      pkgsList <- pakPkgSetup(pkgs, doDeps = doDeps)
      td3 <- tempdir3()
      on.exit({unlink(dirname(td3))}, add = TRUE)
      dfile <- DESCRIPTIONfileFromModule(verbose = -2,
                                         packageFolderName = td3,
                                         .txtDummyPackage,
                                         md = list(name = .txtDummyPackage, description = .txtDummyPackage,
                                                   version = list(a = 1, dummy = "0.0.1"),
                                                   authors =
                                                     'person(given = "Eliot",
                             family = "McIntire",
                             role = c("aut", "cre"),
                             email = "eliot.mcintire@canada.ca",
                             comment = c(ORCID = "0000-0002-6914-8316"))'
                                         ),
                                         deps = pkgsList$DESC,
                                         hasNamespaceFile = FALSE)
      err <- try(outs <- pakCall(pak::pak(c(
        paste0("deps::", td3),
        pkgsList$direct
      ), lib = libPaths[1], ask = FALSE,

      # already done in pakPkgSetup # doDeps,
      # FALSE doesn't work when `deps::` is used
      dependencies = doDeps,
      upgrade = upgrade), verbose),
      silent = TRUE)

      if (!is(err, "try-error"))
        break

      # deal with errors
      packages <- pakErrorHandling(err, pkgs, packages, verbose = verbose)
      if (length(packages) == 0)
        stop(err)

    } else {
      outs <- list()
      break
    }
  }

  pkgDT <- try(as.data.table(outs))
  pkgDT <- pkgDT[package != paste0(.txtDummyPackage, "-deps")]
  if (is(pkgDT, "try-error")) browser()
  setnames(pkgDT, old = c("package", "status"), new = c("Package", "installResult"))
  loadSequence <- match(extractPkgName(packagesOrig), pkgDT$Package)
  loadSequence <- na.omit(loadSequence)
  pkgDT[loadSequence, loadOrder := seq_along(loadSequence)]
  # if it didn't fail, then it is OK
  pkgDT[is.na(pkgDT$installed), needInstall := .txtInstall]
  set(pkgDT, NULL, c("installedVersionOK", "availableVersionOK"), TRUE)
  pkgDT[, packageFullName := Package]
}

whEquals <- function(pkgs) {
  grep("==", pkgs)
}

isGH <- function(pkgs) {
  grepl("^[[:alpha:]]+/.+", pkgs)
}

# For each plain CRAN ref in `pkgsForPak` that names a package currently
# installed in `libPaths`, rewrite it to `pkg@<installedVersion>`. Leaves
# GitHub refs, pre-pinned `pkg@X` refs, packages with an explicit user
# version constraint (carried in `resolvedPkgs`), and uninstalled
# packages alone. Returns the rewritten character vector.
pinInstalledForPak <- function(pkgsForPak, libPaths, resolvedPkgs = NULL) {
  if (!length(pkgsForPak)) return(pkgsForPak)
  ip <- tryCatch(installed.packages(lib.loc = libPaths),
                 error = function(e) NULL)
  if (is.null(ip) || !NROW(ip)) return(pkgsForPak)
  installedVer <- setNames(unname(ip[, "Version"]), unname(ip[, "Package"]))
  # Names of packages the user version-pinned (parenthetical specs in the
  # original refs). pinning these would force pak to install the installed
  # version, masking the user's upgrade/downgrade request.
  userPinned <- character(0)
  if (length(resolvedPkgs)) {
    hasUserSpec <- grepl("\\([^)]+\\)", resolvedPkgs)
    if (any(hasUserSpec))
      userPinned <- extractPkgName(resolvedPkgs[hasUserSpec])
  }
  out <- pkgsForPak
  for (i in seq_along(pkgsForPak)) {
    p <- pkgsForPak[i]
    if (isGH(p)) next                # GitHub refs: leave as-is
    if (grepl("@", p, fixed = TRUE)) next  # already pinned
    nm <- extractPkgName(p)
    if (!nzchar(nm)) next
    if (nm %in% userPinned) next      # user gave an explicit constraint
    v <- installedVer[nm]
    if (is.na(v) || !nzchar(v)) next
    out[i] <- paste0(nm, "@", unname(v))
  }
  out
}

pakPkgDep <- function(packages, which, simplify, includeSelf, includeBase,
                      keepVersionNumber, verbose = getOption("Require.verbose")) {
  if (!requireNamespace("pak")) stop("Please install pak")

  deps <- list()

  packagesOrig <- packages

  useCache <- FALSE
  if (useCache) {
    depsList <- get0("depsList", envir = pakEnv())
    haveAlready <- NULL
    if (!is.null(depsList)) {
      haveAlready <- match(packages, names(depsList)) |> na.omit()
      packages <- packages[-haveAlready]
    }

  }

  deps <- Map(pkg1 = packages, function(pkg1) {
    reposOrig <- getOption("repos")
    on.exit({
      options(repos = reposOrig)
    }, add = TRUE)
    pkgOrig <- pkg1
    pkg <- pkg1
    valExtra <- list()
    wh <- ifelse(any(grepl("suggests", tolower(unlist(which)))), TRUE,
                 ifelse(length(which), NA, FALSE))

    pkgDone <- character()
    supplement <- character(0)  # archive url:: refs for transitive deps discovered during retry
    i <- 0
    while (length(pkg1) > 0) {
      i <- i + 1 # counter
      pkg <- pkg1[1]
      # for (pkg in pkg1) {
      #for (i in 1:2) {
      # for (pkg in pkg1) { # will only be longer than 1 if added with pakErrorHandling below
      isGH <- isGH(pkg) # grepl("^[[:alpha:]]+/.+", pkgs)
      notGH <- isGH %in% FALSE
      if (any(notGH)) {
        pkg[notGH] <- equalsToAt(pkg[notGH])
        pkg2 <- lessThanToAt(pkg[notGH]) # can remove a pkg if not an option
      } else {
        pkg <- equalsToAt(pkg)
        pkg2 <- lessThanToAt(pkg) # can remove a pkg if not an option
      }
      if (length(pkg2) == 0) {
        pkg1 <- pkg2
        val <- character()
        break
      }
      if (any(notGH)) pkg[notGH] <- pkg2

      pkg <- HEADtoNone(pkg)
      isGT <- isGT(pkg) # grep(">", pkgs)
      needRm <- isGH | isGT
      if (any(needRm))
        pkg[needRm] <- trimVersionNumber(pkg[needRm])

      # give up for archives of archives
      if (i > 1 && pkg %in% pkgDone) wh <- FALSE

      ## Memory-only cache so repeated lookups within a session avoid the
      ## per-call ~5-15s callr subprocess cost. pakDepsCacheKey() uses
      ## tempfile/saveRDS/md5sum for collision-proof hashing of large
      ## batch inputs -- too heavy when called per-package in this hot
      ## loop. Plain paste suffices since the inputs are short.
      ppMemKey <- paste0("pakPkgDep_",
                         paste(c(pkg, supplement), collapse = "\x01"),
                         "\x02",
                         paste(unlist(wh), collapse = ","))
      val <- get0(ppMemKey, envir = pakEnv(), inherits = FALSE)
      if (is.null(val)) {
        val <- try(pakCall(pak::pkg_deps(c(pkg, supplement), dependencies = wh), verbose), silent = TRUE)
        if (!is(val, "try-error")) {
          assign(ppMemKey, val, envir = pakEnv())
        }
      }
      if (is(val, "try-error")) {
        pkgDone <- unique(c(pkg, pkgDone))
        pkgOrig2 <- pkg
        pkg <- pakErrorHandling(val, pkg, pkg, verbose = verbose)
        if (length(pkg)) {
          if (length(pkg) > length(pkgOrig2)) {
            # New archive url:: refs added for transitive deps.
            # Add to supplement so they're passed in the next pak::pkg_deps call,
            # but don't update pkg1 (the main package hasn't changed).
            newRefs <- setdiff(pkg, pkgOrig2)
            supplement <- unique(c(supplement, newRefs))
            pkgDone <- pkgDone[pkgDone != pkgOrig2]  # allow retry of main pkg with wh=NA
          } else {
            pkg1[1] <- pkg
          }
        } else { # fail because of various reasons
          pkg1 <- pkg
          val <- character()
        }
      } else {
        if (length(pkg1) > 1) {
          valExtra <- append(list(val), valExtra)
        }
        pkg1 <- pkg1[-1]
        break
      }
    }
    if (length(valExtra)) {
      if (!requireNamespace("tibble")) stop("Please install tibble")
      valExtra <- do.call(rbind, valExtra)
      newDeps <- append(val$deps, valExtra$deps)
      newDeps <- do.call(rbind, newDeps)
      data.table::setDT(newDeps)
      newDeps <- newDeps[newDeps[, .I[max(version) == version], by = "ref"]$V1,]
      whOverride <- which(newDeps$ref %in% valExtra$package)
      newDeps <- newDeps[whOverride, ref := valExtra$ref]
      newDeps <- tibble::as_tibble(newDeps)
      val$deps <- list(newDeps)
    }

    val

  }
  )

  if (useCache) {

    if (!is.null(haveAlready)) {
      newList <- Map(x = packagesOrig, function(x) list())
      newList[names(depsList)] <- depsList
      if (NROW(deps))
        newList[-haveAlready] <- deps
      deps <- newList
    }
    assign("depsList", deps, envir = pakEnv())
  }

  hasDeps <- lengths(deps) > 0
  if (simplify %in% TRUE && any(hasDeps)) {
    deps[hasDeps] <- Map(dep = deps[hasDeps], nam = names(deps)[hasDeps], function(dep, nam) {
      dd <- try(dep$deps)
      if (is(dd, "try-error")) browser()
      dep$deps <- Map(innerDep = dep$deps, outerPkg = dep$package, function(innerDep, outerPkg) {
        if (!nam %in% outerPkg || !any(tolower(unlist(which)) == "suggests")) {
          innerDep[tolower(innerDep$type) %in%
                     setdiff(tolower(unlist(which)), "suggests"),]
        } else {
          innerDep
        }})
      dep
    })
    deps[hasDeps] <- Map(dep = deps[hasDeps], packFullName = packagesOrig[hasDeps], function(dep, packFullName) {
      rr <- rbindlist(dep$deps) # only gets 1st order dependencies; still need self
      rr <- unique(rr)
      # rr <- rr[tolower(type) %in%  setdiff(tolower(unlist(which)), "suggests")]
      rr <- packageFullNameFromPkgVers(rr)
      hasWeirdSource <- grep("^.+::", rr$packageFullName)
      if (any(hasWeirdSource)) {
        # For url:: refs (archived CRAN packages), replace the full URL with the
        # plain package name so that extractPkgName() and allNeeded checks work.
        # The version constraint (op + version) is preserved if present.
        rr[hasWeirdSource, packageFullName := {
          vs <- if (!is.na(version) && nzchar(version)) paste0(" (", op, " ", version, ")") else ""
          paste0(package, vs)
        }]
      }
      # rr[, packageFullName := paste0(ref, ifelse(nzchar(version), paste0(" (", op, " ", version, ")"), ""))]

      if (includeSelf) {
        selfPkgs <- toPkgDTFull(packFullName)
        setnames(selfPkgs, old = c("Package", "versionSpec", "inequality"),
                 new = c("package", "version", "op"))
        selfPkgs[is.na(version), `:=`(version = "", op = "")]
        selfPkgs[, type := "Depends"]
        selfPkgs[, ref := trimVersionNumber(packageFullName)]
        keepCols <- colnames(rr)
        rr <- rbindlist(list(rr, selfPkgs[, ..keepCols]))
        pkg <- extractPkgName(packFullName)
        if (!identical(dep$ref, pkg)) {
          # Always use the user-supplied packFullName (plain name or GitHub ref).
          # Using dep$ref here would set url:: archive refs as packageFullName,
          # which extractPkgName() cannot parse back to the plain package name.
          rr[package %in% pkg, packageFullName := packFullName]
        }

      }

      setorderv(rr, c("package", "op"), order = c(1L, -1L))
      rr <- unique(rr, by = "package")

      lcPackage <- "package"
      ucPackage <- "Package"
      setnames(rr, old = lcPackage, new = ucPackage)
      rr <- rmRifInPackageCol(rr)
      setnames(rr, old = ucPackage, new = lcPackage)
      if (!includeBase) {
        rr <- rr[!package %in% .basePkgs]
      }

      deps <- rr$packageFullName

      if (keepVersionNumber %in% FALSE)
        deps <- trimVersionNumber(deps)
      deps
    })

  }
  deps
}

packageFullNameFromPkgVers <- function(rr) {
  rr[, packageFullName := paste0(ref, ifelse(nzchar(version), paste0(" (", op, " ", version, ")"), ""))]
}


DESCRIPTIONfileFromModule <- function(module, md, deps, hasNamespaceFile, NAMESPACEFile,
                                      filePathImportSpadesCore = file.path(".", fileext = ".R"),
                                      packageFolderName = module, verbose = getOption("Require.verbose")) {
  d <- list()
  # d$Package <- .moduleNameNoUnderscore(module)
  d$Package <- module
  d$Type <- "Package"

  d$Title <- md$name
  d$Description <- md$description
  d$Version <- as.character(eval(md$version[[2]]))
  d$Date <- Sys.Date()
  d$Authors <- md$authors
  d$Authors <- c(paste0("  ", format(d$Authors)[1]), format(d$Authors)[-1])


  # hasSC <- grepl("SpaDES.core", deps)
  # if (all(!hasSC))
  #   deps <- c("SpaDES.core", deps)

  d$Imports <- Require::extractPkgName(deps)
  versionNumb <- Require::extractVersionNumber(deps)
  needRemotes <- which(!is.na(Require::extractPkgGitHub(deps)))
  d$Remotes <- Require::trimVersionNumber(deps[needRemotes])

  hasVersionNumb <- !is.na(versionNumb)
  inequality <- paste0("(", gsub("(.+)\\((.+)\\)", "\\2", deps[hasVersionNumb]), ")")
  missingSpace <- !grepl("[[:space:]]", inequality)
  if (any(missingSpace))
    inequality[missingSpace] <- gsub("([=><]+)", "\\1 ", inequality[missingSpace])

  namespaceImports <- d$Imports
  # Create "import all" for each of the packages, unless it is already in an @importFrom
  if (hasNamespaceFile) {
    nsTxt <- readLines(NAMESPACEFile)
    hasImportFrom <- grepl("importFrom", nsTxt)
    if (any(hasImportFrom)) {
      pkgsNotNeeded <- unique(gsub(".+\\((.+)\\,.+\\)", "\\1", nsTxt[hasImportFrom]))
      namespaceImports <- grep(paste(pkgsNotNeeded, collapse = "|"),
                               namespaceImports, invert = TRUE, value = TRUE)
    }
  }

  # cat(paste0("#' @import ", namespaceImports, "\nNULL\n"), sep = "\n",
  #     file = filePathImportSpadesCore, fill = TRUE)

  d$Imports[hasVersionNumb] <- paste(d$Imports[hasVersionNumb], inequality)

  dFile <- filenameFromFunction(packageFolderName, "DESCRIPTION", fileExt = "")
  if (!dir.exists(packageFolderName))
    dir.create(packageFolderName, recursive = TRUE, showWarnings = FALSE)
  origDESCtxt <- if (file.exists(dFile)) read.dcf(dFile) else character()

  cat(paste("Package:", d$Package), file = dFile, sep = "\n")
  cat(paste("Type:", d$Type), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Title:", d$Title), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Version:", d$Version), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Description:", paste(d$Description, collapse = " ")), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Date:", d$Date), file = dFile, sep = "\n", append = TRUE)
  cat(c("Authors@R:  ", format(d$Authors)), file = dFile, sep = "\n", append = TRUE)

  if (length(d$Imports) || length(origDESCtxt))
    mergeField(origDESCtxt = origDESCtxt, field = d$Imports, fieldName = "Imports", dFile)

  # suggs <- c('knitr', 'rmarkdown', 'testthat', 'withr', 'roxygen2')
  # if (length(suggs) || length(origDESCtxt))
  #   mergeField(origDESCtxt = origDESCtxt, field = suggs, fieldName = "Suggests", dFile)

  if (length(d$Remotes) || length(origDESCtxt))
    mergeField(origDESCtxt = origDESCtxt, field = d$Remotes, fieldName = "Remotes", dFile)

  cat("Encoding: UTF-8", sep = "\n", file = dFile, append = TRUE)
  cat("License: GPL-3", sep = "\n", file = dFile, append = TRUE)
  cat("VignetteBuilder: knitr, rmarkdown", sep = "\n", file = dFile, append = TRUE)
  cat("ByteCompile: yes", sep = "\n", file = dFile, append = TRUE)
  cat("Roxygen: list(markdown = TRUE)", sep = "\n", file = dFile, append = TRUE)
  # cat(paste0("RoxygenNote: ", as.character(packageVersion("roxygen2"))), sep = "\n",
  #     file = dFile, append = TRUE)


  messageVerbose("New/updated DESCRIPTION file is: ", dFile, verbose = verbose)
  return(dFile)
}


filenameFromFunction <- function(packageFolderName, fn = "", subFolder = "", fileExt = ".R") {
  normPath(file.path(packageFolderName, subFolder, paste0(gsub("\\.", "", fn), fileExt)))
}

mergeField <- function(origDESCtxt, field, dFile, fieldName = "Imports") {
  fieldVals <- character()
  if (fieldName %in% colnames(origDESCtxt))
    fieldVals <- strsplit(origDESCtxt[, fieldName], split = ",+\n")[[1]]
  if (length(field)) {
    field <- trimRedundancies(unique(c(field, fieldVals)))
  }
  cat(c(paste0(fieldName, ":"), paste("   ", sort(field$packageFullName), collapse = ",\n")),
      sep = "\n", file = dFile, append = TRUE)
}



equalsToAt <- function(pkgs) {
  gsub(" {0,3}\\(== {0,4}(.+)\\)", "@\\1", pkgs)
}

# Reduce a vector of pak refs to the bare package names that line up with
# rownames(installed.packages()). Three things to strip:
#   * "any::"  prefix on plain CRAN refs   (any::cli           -> cli)
#   * "owner/" prefix on GitHub refs       (tidyverse/ggplot2  -> ggplot2)
#   * "@version" suffix on exact-pin refs  (qs@0.27.3          -> qs)
# extractPkgName() handles owner/repo and (>=X) parenthetical version specs,
# but does NOT strip pak's "@version" exact-pin form (introduced upstream by
# equalsToAt() / lessThanToAt() to translate "pkg (== X)" / "pkg (<= X)"
# into pak's `pkg@X` syntax). Without the @-strip every version-pinned ref
# survives as "pkg@X" and the install-summary / iter-loop / archive-fallback
# checks all misclassify it as still-missing -- even right after a successful
# install -- because installed.packages() returns "pkg".
pakRefToBareName <- function(refs) {
  sub("@.*$", "", sub("^any::", "", sub("^[^/]+/", "", extractPkgName(refs))))
}

# Look up `pkg` (bare name) in pak's local download cache and return the path
# to the most-recent matching tarball, or NA_character_ if not present.
# Prefers a binary file matching the current platform when available, else the
# newest source tarball.  Used by the offline-mode install path so that
# `Install("fpCompare")` can succeed without any network access as long as
# pak previously downloaded the package.
## Returns TRUE iff every package flagged `needInstall == .txtInstall` in
## `pkgDT` has a usable tarball in pak's download cache (per
## `pakCachedTarball()`). Used as the gate for the "skip pak's online
## resolver entirely" shortcut in `Require()`: when every package we'd
## install is already cached, we route through `pakOfflineInstall()` even
## in non-offline mode, avoiding pak's metadata refresh (which can stall
## on TCP timeouts when network is down).
allInPakCache <- function(pkgDT) {
  if (!"needInstall" %in% names(pkgDT)) return(FALSE)
  toInstall <- pkgDT[needInstall == .txtInstall]
  if (!NROW(toInstall)) return(TRUE)
  ## (HEAD) pins -- `account/repo@branch (HEAD)` -- always require the
  ## network: HEAD means "current tip of the branch", which we can't
  ## determine from a local cache. If any row is HEAD-pinned, refuse the
  ## shortcut so pak can resolve the tip online.
  toInstall <- checkHEAD(toInstall)
  if (any(toInstall[[hasHEADtxt]] %in% TRUE)) return(FALSE)
  hasVS <- "versionSpec" %in% names(toInstall)
  hasIN <- "inequality"  %in% names(toInstall)
  for (i in seq_len(NROW(toInstall))) {
    pkg   <- toInstall$Package[i]
    vSpec <- if (hasVS) toInstall$versionSpec[i] else NA_character_
    ineq  <- if (hasIN) toInstall$inequality[i]  else NA_character_
    # pakCachedTarball returns NULL if no cached row satisfies the
    # version constraint (or if nothing is cached at all). Either way:
    # we must go online to fetch a satisfying build.
    if (is.null(pakCachedTarball(pkg, versionSpec = vSpec, inequality = ineq)))
      return(FALSE)
  }
  TRUE
}

## Returns `NULL` if no usable tarball is cached, or a list
##   list(path = <fullpath>, is_binary = <logical>)
## We need `is_binary` from `pak::cache_list()`'s `platform` column rather
## than from filename heuristics because on Linux, PPM binaries share the
## bare `pkg_ver.tar.gz` filename with their source counterparts -- only
## the `platform` column ("source" vs e.g. "x86_64-pc-linux-gnu-ubuntu-24.04")
## distinguishes them. Filename-only classification routed PPM binaries to
## the source-install branch, which made pak try to R-CMD-BUILD them
## offline (and fail, since vignettes etc. need network).
pakCachedTarball <- function(pkg, versionSpec = NA_character_,
                             inequality = NA_character_) {
  if (!requireNamespace("pak", quietly = TRUE)) return(NULL)
  cl <- tryCatch(pak::cache_list(), error = function(e) NULL)
  if (is.null(cl) || NROW(cl) == 0L || !"package" %in% names(cl))
    return(NULL)
  rows <- cl[!is.na(cl$package) & cl$package == pkg, , drop = FALSE]
  if (NROW(rows) == 0L) return(NULL)
  # Reject pak intermediate files: extracted directories, platform-suffixed
  # build artifacts (e.g. `_X.tar.gz-aarch64-apple-darwin20-4.5.2`), and
  # `.tar.gz-t` partial-download stubs. Only accept paths ending in a real
  # installable archive extension.
  isInstallable <- grepl("\\.(tar\\.gz|tgz|zip)$", rows$fullpath)
  rows <- rows[isInstallable, , drop = FALSE]
  if (NROW(rows) == 0L) return(NULL)

  ## Version-constraint filter: drop cached rows whose `version` does NOT
  ## satisfy the user's inequality (e.g. `dplyr (>= 2.0.0)` rules out a
  ## cached 1.2.1). Caller passes the constraint from pkgDT's
  ## `versionSpec` / `inequality` columns; with neither supplied we keep
  ## all rows (the original unconstrained behaviour).
  hasConstraint <- !is.na(versionSpec) && nzchar(versionSpec) &&
                   !is.na(inequality)  && nzchar(inequality)
  if (hasConstraint && "version" %in% names(rows)) {
    okVer <- vapply(rows$version, function(v) {
      if (is.na(v) || !nzchar(v)) return(FALSE)
      isTRUE(compareVersion2(v, versionSpec, inequality))
    }, logical(1))
    rows <- rows[okVer, , drop = FALSE]
    if (NROW(rows) == 0L) return(NULL)
  }

  # Authoritative binary vs source: pak::cache_list()'s `platform` column.
  # "source" or NA -> source; matching-arch string -> binary.
  isBinary <- !is.na(rows$platform) & rows$platform != "source" &
              grepl(R.version$arch, rows$platform, fixed = TRUE)

  # Prefer platform-matching binary over source.
  if (any(isBinary)) {
    rows <- rows[isBinary, , drop = FALSE]
    chosenBinary <- TRUE
  } else {
    isSrc <- is.na(rows$platform) | rows$platform == "source"
    rowsSrc <- rows[isSrc, , drop = FALSE]
    if (NROW(rowsSrc) > 0L) {
      rows <- rowsSrc
      chosenBinary <- FALSE
    } else {
      # Unrecognised platform value: treat as source-ish to be safe.
      chosenBinary <- FALSE
    }
  }

  rows <- rows[file.exists(rows$fullpath), , drop = FALSE]
  if (NROW(rows) == 0L) return(NULL)
  i <- which.max(file.mtime(rows$fullpath))
  ## Return the cached row's `version` too. Callers need it to construct
  ## an exact-pin pak ref (`pkg@version`) so pak installs the cached
  ## version rather than the latest available on CRAN -- otherwise a
  ## snapshot install of `fpCompare (==0.2.2)` was getting fpCompare
  ## 0.2.4 because the parenthetical `(==X)` form gets stripped to a
  ## bare `pkg` ref before pak ever sees the constraint.
  cachedVer <- if ("version" %in% names(rows)) rows$version[i] else NA_character_
  list(path = rows$fullpath[i], is_binary = chosenBinary,
       version = cachedVer)
}

# Offline install via pak: resolve each user package to a local tarball in
# pak's cache and install via `local::path` refs (which require no network).
# Returns the (possibly-modified) pkgDT with `installed`, `Version`,
# `LibPath`, and `installResult` updated for each row.  Packages absent from
# pak's cache are flagged as `.txtCouldNotBeInstalled` (just like the online
# path's `silentlyFailed` warning).
pakOfflineInstall <- function(pkgDT, libPaths, verbose = getOption("Require.verbose")) {
  if (!requireNamespace("pak", quietly = TRUE)) stop("Please install pak")
  toInstall <- pkgDT[needInstall == .txtInstall]
  if (!NROW(toInstall)) return(pkgDT)

  ## We used to call `pakResetSubprocess()` here, hoping that a wedged
  ## subprocess after a failed `pakInstallFiltered` plan would otherwise
  ## return stale rows from `pak::cache_list()`. On Windows the reset
  ## itself broke the next `pak::cache_list()` call (interrupt+kill
  ## raced against pak's auto-respawn, so the subsequent query returned
  ## "no rows" even when the cache was fully populated -- visible in the
  ## diagnostic log as `pakCachedTarball(<pkg>) returned NULL; no rows
  ## in pak::cache_list()` immediately after the cache shortcut had
  ## just confirmed the same rows were present). Leaving the subprocess
  ## alone is safer; the cache-shortcut path enters here BEFORE any
  ## failed install so there's no wedge to recover from.

  ## Strategy: keep pak as the installer (so its resolver, dep-ordering,
  ## sysreqs, build, and progress UI all apply). Pass it bare/cleaned-up
  ## refs and set env vars + an option so its startup network probes are
  ## skipped:
  ##   - `PKG_METADATA_UPDATE_AFTER=365d` -- treat cached repo metadata as
  ##     fresh, so pak doesn't refresh from CRAN/PPM at startup.
  ##   - `R_BIOC_VERSION` -- short-circuits pkgcache's Bioconductor
  ##     version detection (it fetches `bioconductor.org/config.yaml`
  ##     otherwise).
  ##   - `R_BIOC_CONFIG_URL=file://...` -- redirects any residual yaml
  ##     fetch to pkgcache's bundled fixture inside pak's private library.
  ##   - `options(pak.no_extra_messages = TRUE)` -- suppresses the
  ##     "Optional package `pillar` is not available" startup hint.
  ## All four are saved + restored on exit.

  notInCache <- character(0)
  refsToInstall <- character(0)
  pkgsToInstall <- character(0)
  hasVS <- "versionSpec" %in% names(toInstall)
  hasIN <- "inequality"  %in% names(toInstall)
  for (i in seq_len(NROW(toInstall))) {
    pkg   <- toInstall$Package[i]
    vSpec <- if (hasVS) toInstall$versionSpec[i] else NA_character_
    ineq  <- if (hasIN) toInstall$inequality[i]  else NA_character_
    cached <- pakCachedTarball(pkg, versionSpec = vSpec, inequality = ineq)
    if (is.null(cached)) {
      notInCache <- c(notInCache, pkg)
      ## Diagnostic: log raw cache rows for this pkg so users can see why
      ## the lookup missed (no rows / no matching binary / missing file
      ## on disk). Verbose-gated to keep the happy path quiet.
      if (verbose >= 1) {
        raw <- tryCatch({
          cl <- pak::cache_list()
          cl[!is.na(cl$package) & cl$package == pkg,
             c("package", "version", "platform", "fullpath"), drop = FALSE]
        }, error = function(e) NULL)
        if (!is.null(raw) && NROW(raw) > 0L) {
          messageVerbose("pakCachedTarball(", pkg, ") returned NULL despite ",
                         NROW(raw), " cache row(s); fullpath exists? ",
                         paste(file.exists(raw$fullpath), collapse = ","),
                         verbose = verbose, verboseLevel = 1)
        } else {
          messageVerbose("pakCachedTarball(", pkg,
                         ") returned NULL; no rows in pak::cache_list()",
                         verbose = verbose, verboseLevel = 1)
        }
      }
    } else {
      ## Choose the ref form per file extension so pak skips the network
      ## without rebuilding anything:
      ##
      ##  - `.zip` (Windows binary) / `.tgz` (Mac binary):  use
      ##    `local::<file>` -- pak installs the binary directly, no
      ##    rebuild, no resolver, no CRAN-vs-PPM cache-key mismatch.
      ##  - `.tar.gz`:  pin pak's resolver to the EXACT version we found
      ##    in the cache. `local::<tar.gz>` would trigger `R CMD build`
      ##    (rebuilds vignettes -- needs network). A bare `pkg` ref
      ##    leaves pak free to pick the latest CRAN version instead of
      ##    the cached one (the bug snapshot installs hit:
      ##    `fpCompare (==0.2.2)` got stripped to `fpCompare` and pak
      ##    installed 0.2.4). `pkg@<cachedVersion>` keeps pak in charge
      ##    of dep ordering / sysreqs / build but tells it which version
      ##    we want, and the cache filter has already verified that
      ##    version satisfies the user's constraint.
      ##
      ## GitHub refs (`account/repo@SHA`) are preserved via
      ## `trimVersionNumber()`, which keeps the `@SHA` for
      ## owner/repo-style refs while stripping parenthetical specs.
      isBinaryArchive <- grepl("\\.(zip|tgz)$", cached$path,
                               ignore.case = TRUE)
      fullName <- if ("packageFullName" %in% names(toInstall))
        toInstall$packageFullName[i] else NA_character_
      isGitHubRef <- !is.na(fullName) && nzchar(fullName) &&
        grepl("/", sub("@.*$", "", fullName), fixed = TRUE)
      ref <- if (isBinaryArchive) {
        paste0("local::", cached$path)
      } else if (isGitHubRef) {
        ## GitHub: preserve `account/repo@SHA` (trimVersionNumber's
        ## `@version` strip is gated on no-slash-before-@, so this
        ## is safe).
        trimVersionNumber(fullName)
      } else if (!is.null(cached$version) &&
                 !is.na(cached$version) && nzchar(cached$version)) {
        paste0(pkg, "@", cached$version)
      } else {
        pkg
      }
      refsToInstall <- c(refsToInstall, ref)
      pkgsToInstall <- c(pkgsToInstall, pkg)
    }
  }

  installFailedPkgs <- character(0)
  triedPkgs <- pkgsToInstall

  if (length(refsToInstall)) {
    messageVerbose("offline mode: installing ", length(refsToInstall),
                   " package(s) from pak cache: ",
                   paste(pkgsToInstall, collapse = ", "),
                   verbose = verbose, verboseLevel = 1)

    ## Locate pkgcache's bioc-config.yaml fixture. pkgcache lives inside
    ## pak's private library on most installs, so the top-level
    ## system.file() lookup returns "" -- fall back to pak's library/.
    biocFixture <- tryCatch(
      system.file("fixtures", "bioc-config.yaml", package = "pkgcache"),
      error = function(e) "")
    if (!nzchar(biocFixture)) {
      pakDir <- tryCatch(find.package("pak"), error = function(e) "")
      if (length(pakDir) && nzchar(pakDir)) {
        cand <- file.path(pakDir, "library", "pkgcache", "fixtures",
                          "bioc-config.yaml")
        if (file.exists(cand)) biocFixture <- cand
      }
    }
    biocVer <- if (nzchar(biocFixture) && file.exists(biocFixture)) {
      relLine <- grep("^release_version:", readLines(biocFixture, warn = FALSE),
                      value = TRUE)[1L]
      v <- sub('^release_version:\\s*"?([^"\\s]+)"?\\s*$', "\\1", relLine, perl = TRUE)
      if (length(v) && nzchar(v) && !is.na(v)) v else "3.22"
    } else "3.22"
    envNms <- c("R_BIOC_VERSION", "R_BIOC_CONFIG_URL",
                "PKG_METADATA_UPDATE_AFTER")
    oldEnv <- Sys.getenv(envNms, names = TRUE, unset = NA)
    if (is.na(oldEnv[["R_BIOC_VERSION"]]))
      Sys.setenv(R_BIOC_VERSION = biocVer)
    if (is.na(oldEnv[["R_BIOC_CONFIG_URL"]]) && nzchar(biocFixture)) {
      Sys.setenv(R_BIOC_CONFIG_URL =
                   paste0("file://", normalizePath(biocFixture, winslash = "/")))
    }
    if (is.na(oldEnv[["PKG_METADATA_UPDATE_AFTER"]]))
      Sys.setenv(PKG_METADATA_UPDATE_AFTER = "365d")
    oldExtraOpt <- getOption("pak.no_extra_messages")
    options(pak.no_extra_messages = TRUE)
    on.exit({
      for (nm in envNms) {
        v <- oldEnv[[nm]]
        if (is.na(v)) Sys.unsetenv(nm) else do.call(Sys.setenv, setNames(list(v), nm))
      }
      options(pak.no_extra_messages = oldExtraOpt)
    }, add = TRUE)

    err <- try(pakCall(
      pak::pak(refsToInstall, lib = libPaths[1], ask = FALSE,
               dependencies = FALSE, upgrade = FALSE),
      verbose), silent = TRUE)
    if (is(err, "try-error")) {
      ## Surface the underlying pak error at default verbose so silent-pak
      ## failures are debuggable. The generic "offline install failed"
      ## warning below loses the diagnostic.
      messageVerbose("pak install reported error; deferring to ",
                     "installed.packages() ground-truth check: ",
                     as.character(err),
                     verbose = verbose, verboseLevel = 1)
    }
  }

  ## Verify final state on disk and update pkgDT accordingly. This is the
  ## ground truth -- a successful install.packages call doesn't guarantee
  ## the package landed in libPaths.
  if (length(triedPkgs)) {
    ipNow <- tryCatch(installed.packages(lib.loc = libPaths[1L], noCache = TRUE),
                      error = function(e) NULL)
    for (pkg in triedPkgs) {
      wh <- which(pkgDT$Package == pkg)
      if (!length(wh)) next
      if (!is.null(ipNow) && pkg %in% rownames(ipNow)) {
        set(pkgDT, wh, "installed",          TRUE)
        set(pkgDT, wh, "installedVersionOK", TRUE)
        set(pkgDT, wh, "Version",            unname(ipNow[pkg, "Version"]))
        set(pkgDT, wh, "LibPath",            unname(ipNow[pkg, "LibPath"]))
        set(pkgDT, wh, "installResult",      "OK")
      } else {
        installFailedPkgs <- c(installFailedPkgs, pkg)
      }
    }
  }

  ## Distinct warnings: "tarball not in cache" vs "tarball was in cache but
  ## install failed". The former is genuinely unrecoverable until the user
  ## gets internet back; the latter usually points at a build-tool or
  ## sysreq issue and is worth flagging separately for debugging.
  if (length(notInCache)) {
    for (pkg in notInCache) {
      wh <- which(pkgDT$Package == pkg)
      if (length(wh)) set(pkgDT, wh, "installResult", .txtCouldNotBeInstalled)
    }
    warning(.txtCouldNotBeInstalled, ": ",
            paste(notInCache, collapse = ", "),
            "; offline mode and not in pak cache",
            call. = FALSE)
  }
  if (length(installFailedPkgs)) {
    installFailedPkgs <- unique(installFailedPkgs)
    for (pkg in installFailedPkgs) {
      wh <- which(pkgDT$Package == pkg)
      if (length(wh)) set(pkgDT, wh, "installResult", .txtCouldNotBeInstalled)
    }
    warning(.txtCouldNotBeInstalled, ": ",
            paste(installFailedPkgs, collapse = ", "),
            "; tarball was in pak cache but offline install failed ",
            "(check build tools / sysreqs / R version compatibility)",
            call. = FALSE)
  }
  pkgDT
}

lessThanToAt <- function(pkgs) {
  hasLT <- grepl("<", pkgs) # only < not <=
  if (any(hasLT %in% TRUE)) {
    #trulyLT <- grepl("<[^=]", pkgs) # only < not <=
    #whTrulyLT <- which(trulyLT)
    #val <- character(length(pkgs))
    # if (any(trulyLT)) {
    pkgDT <- toPkgDTFull(pkgs[hasLT])#[whTrulyLT])
    vers <- Map(pkg = pkgDT$packageFullName, function(pkg) {

      isGH <- isGH(pkg)
      if (any(isGH)) {
        isOK <- pakCheckGHversionOK(pkg)
        notOK <- isOK %in% FALSE
        if (any(notOK)) {
          pkg2 <- pkg[!notOK]
          if (length(pkg2) == 0)
            return(character())
          pkg[!notOK] <- pkg2
        }
      }

      # vers <- Map(pkg = pkgs[whTrulyLT], function(pkg) {
      pkgNoVersion <- trimVersionNumber(pkg)
      his <- try(pak::pkg_history(pkgNoVersion))
      if (is(his, "try-error")) return(character())
      whOK <- compareVersion2(his$Version, pkgDT$versionSpec, pkgDT$inequality)
      if (all(whOK %in% FALSE)) {
        warning(msgPleaseChangeRqdVersion(pkgNoVersion, ineq = ">=", newVersion = tail(his$Version, 1)))
      }
      vers <- tail(his$Version[whOK], 1)
    })
    noneAvail <- lengths(vers) == 0
    if (any(noneAvail)) {
      pkgDT <- pkgDT[!noneAvail]
      vers <- vers[!noneAvail]
      hasLT <- hasLT[!noneAvail]
    }
    if (any(noneAvail %in% FALSE)) {
      set(pkgDT, NULL, "Version", vers)
      # set(pkgDT, whTrulyLT, "Version", vers)
      set(pkgDT, NULL, "packageFullName", paste0(pkgDT$Package, "@", pkgDT$Version))
      pkgs[hasLT] <- pkgDT$packageFullName
    } else {
      pkgs <- pkgDT$packageFullName
    }
    # val[trulyLT] <- pkgDT$packageFullName
    # }
    # LTorET <- trulyLT %in% FALSE
    # if (any(LTorET)) {
    #   val[LTorET] <- gsub(" {0,3}\\(<= {0,4}(.+)\\)", "@\\1", pkgs[LTorET])
    # }
  }
  pkgs
}

HEADtoNone <- function(pkgs) {
  gsub(" {0,3}\\(HEAD\\)", "", pkgs)
}

isGT <- function(pkgs) grepl(">", pkgs)

pakGetArchive <- function(pkg2, packages = pkg2, whRm = seq_along(packages)) {
  # Guard against being called with no package to look up. pakErrorHandling
  # parses pak's error output and can pass through an empty `pkgNoVersion`
  # when the parse yields no packages (e.g. a pak-internal error like
  # `if (!version_satisfies(...))` that doesn't match any known pattern).
  # Without this guard, pkgNoVer below also becomes character(0), and the
  # downstream `warning(.txtCouldNotBeInstalled, ": ", pkgNoVer)` fires with
  # an empty body -- surfacing as the noise warning
  # `Warning message: could not be installed:` (no package name, no reason).
  if (!length(pkg2) || all(!nzchar(pkg2))) return(packages)
  pkg2Orig <- pkg2
  ## trimVersionNumber (with usePak) now handles both pak prefixes
  ## (any::, cran::) and the "pkg@ver" form, so a snapshot ref like
  ## "BH@1.81.0-1" reduces cleanly to "BH" for pak::pkg_history below.
  pkgNoVer <- trimVersionNumber(pkg2)
  hasVer <- pkgNoVer != packages[whRm]

  isCRAN <- unlist(whIsOfficialCRANrepo(getOption("repos"), srcPackageURLOnCRAN))
  hisAll <- try(pak::pkg_history(pkgNoVer), silent = TRUE)
  ## Was previously `tail(..., 1)` (the LATEST archive entry). That broke
  ## snapshot installs that pin a specific older version: a snapshot ref
  ## like "BH@1.81.0-1" produced an Archive URL for the latest BH version
  ## instead of 1.81.0-1, and pak then failed to install the wrong file.
  ## Extract the requested version from the input ref and pick the matching
  ## row from pkg_history; only fall through to "latest" when no version
  ## is pinned.
  reqVer <- extractVersionNumber(packages[whRm])
  hisHasVersion <- inherits(hisAll, "data.frame") && !is.null(hisAll$Version)
  his <- if (hisHasVersion) {
    if (length(reqVer) == 1L && !is.na(reqVer) && reqVer %in% hisAll$Version) {
      hisAll[hisAll$Version == reqVer, , drop = FALSE]
    } else {
      utils::tail(hisAll, 1)
    }
  } else {
    hisAll
  }
  if (hisHasVersion && any(pkgNoVer != packages[whRm])) {
    vers <- extractVersionNumber(packages[whRm][hasVer])
    ineq <- "=="
    hasOKVersion <- compareVersion2(his$Version, versionSpec = vers, ineq)
    if (hasOKVersion %in% FALSE) {
      warning(msgPleaseChangeRqdVersion(trimVersionNumber(pkgNoVer), ">=", his$Version))
      packages <- packages[-whRm]
      return(packages)
    }
  }
  if (!is(his, "try-error") || length(isCRAN) > 0) {
    if (is(his, "try-error")) {
      # Package not found in archive either -- remove it from `packages`
      # and let pakInstallFiltered's downstream silentlyFailed warning be
      # the canonical user-facing failure surface. Demoting the warning
      # here (previously a top-level warning()) avoids two separate
      # numbered warnings for the same root cause -- e.g. one for clusters
      # (not on CRAN, not in a Remote pak knows about) and one for its
      # parent fireSenseUtils -- when the user really just needs one
      # coherent diagnostic at the end of the call.
      packages <- packages[-whRm]
      if (any(nzchar(pkgNoVer))) {
        nz <- pkgNoVer[nzchar(pkgNoVer)]
        ghFailed <- grepl("/", nz, fixed = TRUE)
        suffix <- if (any(ghFailed)) paste0("\n", .txtDidYouSpell) else ""
        messageVerbose("pakGetArchive: ", .txtCouldNotBeInstalled, ": ",
                       paste(nz, collapse = ", "), suffix,
                       verbose = getOption("Require.verbose"),
                       verboseLevel = 2)
      }
      return(packages)
    }
    # pakGetArchive is the FALLBACK path: pak's primary resolution already
    # failed for `pkg2`. Always return the source Archive URL (not the current
    # binary URL) -- the binary URL is the one pak just tried and failed on
    # (typically because available.packages(type="binary") still indexes the
    # package even after CRAN removed the binary file, e.g. archived-from-source
    # packages whose Mac/Windows binaries were also pruned). The source Archive
    # URL is the authoritative location for any version pak::pkg_history() lists,
    # so it works for both truly-archived packages and transient binary-fetch
    # failures.
    type <- "source"
    pth <- file.path("Archive", his$Package, paste0(his$Package, "_", his$Version, ".tar.gz"))
    if (isTRUE(!startsWith(isCRAN, "https"))) isCRAN <- paste0("https://", isCRAN)
    pth <- paste0("url::",file.path(contrib.url(isCRAN, type = type), pth))
    # Guard against malformed refs: when isCRAN is empty (e.g. repos has no
    # concrete CRAN URL, only @CRAN@ placeholder or only r-universe), paste0
    # collapses to a bare "url::" string. Returning that downstream causes
    # pak to abort the whole batch with "All URLs failed". Return packages
    # unchanged so the caller can skip the malformed entry.
    if (!length(pth) || any(!grepl("^url::https?://.+", pth))) {
      return(packages)
    }
    if (length(whRm) > 0L) {
      packages[whRm] <- pth
    } else {
      # whRm is empty when the archived package is a transitive dep not directly in
      # the packages list (e.g. called from pakPkgDep with the direct package as pkg).
      # Append the archive ref so the retry includes it explicitly.
      packages <- c(packages, pth)
    }
  }

  # his <- try(tail(pak::pkg_history(pkgNoVer), 1), silent = TRUE)
  # if (!is(his, "try-error")) {
  #   pth <- file.path("Archive", his$Package, paste0(his$Package, "_", his$Version, ".tar.gz"))
  #   if (isTRUE(!startsWith(isCRAN, "https"))) isCRAN <- paste0("https://", isCRAN)
  #   pth <- paste0("url::",file.path(contrib.url(isCRAN), pth))
  #   packages[whRm] <- pth
  # } else {
  #   messageCantInstallNoVersion(pkg2)
  # }
  packages
}

.txtDummyPackage <- "dummy"

pakCheckGHversionOK <- function(pkg) {
  pkgDT <- toPkgDTFull(pkg)
  dl <- try(pak::pkg_download(trimVersionNumber(pkg), dest_dir = tempdir2()))
  if (is(dl, "try-error")) return(FALSE)
  vers <- extractVersionNumber(filenames = basename(dl$fulltarget))
  isOK <- compareVersion2(vers, versionSpec = pkgDT$versionSpec, inequality = pkgDT$inequality)
  isOK
}

# Build the conflict-table row for a "dependency conflict" case.
# dcp  = plain CRAN package name (e.g. "sp")
# cand = the candidate GitHub ref found in the "Conflicts with" error line
#        (may be the same package, e.g. "r-spatial/sp@main",
#         or a different package whose Remotes pulled in the clash,
#         e.g. "PredictiveEcology/SpaDES.core@development")
# Returns a named list suitable for rbindlist(), or NULL when no row should be added.
pakDepConflictRow <- function(dcp, cand) {
  if (!length(cand) || !nzchar(cand)) return(NULL)
  if (extractPkgName(cand) == dcp) {
    list(Package    = dcp,
         Conflict   = paste0(dcp, "  vs  ", cand),
         Resolution = "drop CRAN ref; resolve via GitHub Remotes")
  } else {
    list(Package    = dcp,
         Conflict   = paste0(dcp, " (CRAN)  vs  ", dcp, " (via ", cand, " Remotes)"),
         Resolution = "drop CRAN ref; resolve via GitHub Remotes")
  }
}

# Extract the most informative line(s) from a pak try-error string.
# Strips ANSI codes, removes generic framing lines, and returns up to two
# lines that explain WHY the build/install failed.
pakBuildFailReason <- function(errStr, capturedMsgs = character(0)) {
  # Combine the try() exception text (which is usually the generic
  # "Error : ! error in pak subprocess" optionally chained with
  # "Caused by error: ! <real reason>") with anything pak's subprocess
  # streamed via message() during the failed call. The real cause is
  # often inside the chain or buried in the captured stream -- the
  # outer wrapper exception line on its own says nothing useful.
  rawText <- paste(c(as.character(errStr), as.character(capturedMsgs)),
                   collapse = "\n")
  lines <- strsplit(rawText, "\n")[[1]]
  lines <- gsub("\033\\[[0-9;]*m", "", lines)   # strip ANSI escape sequences
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  # Remove generic R/pak framing lines that don't explain the root cause.
  # Crucially, this includes pak's own wrapper "Error : ! error in pak
  # subprocess" and the "Caused by error:" chain delimiter -- keeping those
  # would cause the fallback below to return them and hide the actual cause.
  lines <- grep(paste(
    "^Error in pak::",
    "pakRetryLoop",
    "^\\s*$",
    "^Error$",
    "^Error : ! error in pak subprocess$",
    "^Caused by error:?$",
    sep = "|"), lines, value = TRUE, invert = TRUE)
  # Prioritise lines that contain diagnostic keywords
  diag <- grep(paste(
    "namespace '[^']+' .+ is being loaded",
    "namespace '[^']+' is imported by",
    "cannot be unloaded",
    "is locked by package",
    "package .+ is already loaded",
    "Could not solve package dependencies",
    "Can't find package called",
    # `Can't find reference @<branch> in GitHub repo <owner>/<repo>` is the
    # specific dep-resolution error pak emits when a user pins a GitHub
    # ref to a branch that doesn't exist on the remote (typo, or branch
    # never pushed). More informative than the generic "Could not solve
    # package dependencies" wrapper above.
    "Can't find reference @[^ ]+ in GitHub repo",
    "invalid.*expression", "ERROR:", "permission denied",
    "unable to move", "cannot remove", "compilation failed",
    "lazy loading failed", "Execution halted",
    sep = "|"), lines, value = TRUE, ignore.case = FALSE)
  if (length(diag)) return(paste(head(unique(diag), 2L), collapse = "; "))
  # Fallback: first non-"Error in" line; strip pak's "! " bullet prefix so
  # the warning reads cleanly (e.g. "! Could not foo" -> "Could not foo").
  fb <- head(lines[!startsWith(lines, "Error in")], 1L)
  if (length(fb) && nzchar(fb)) sub("^!\\s*", "", fb) else ""
}

# ---------------------------------------------------------------------------
# pakConditionLog(): recover structured failure data from a pak error.
#
# When pak::pak() throws on a build failure, the condition chain attaches the
# real R CMD INSTALL output to a `package_build_error` parent (fields
# `package`, `version`, `message`, `stdout`). pak's console stream only
# emits a one-line "X Failed to build <pkg> <ver>" summary and discards the
# rest, so Require's text parsers (extractInstallFailures /
# extractBuildFailures) never see the actual root cause -- which leads to
# the "no parseable culprits" / "still-missing -- cascade casualty"
# diagnostic dead-end even though pak DOES know what went wrong.
#
# Given a try-error or condition, walks the condition chain (capped depth)
# looking for any `package_build_error` node, and returns character lines
# suitable to splice into allCapturedMsgs so the existing parsers can
# extract a real reason. The synthetic "X Failed to build <pkg> <ver>" line
# ensures extractBuildFailures (regex `Failed to build\s+([A-Za-z0-9._]+)`)
# can attribute the failure to the right ref.
# ---------------------------------------------------------------------------
pakConditionLog <- function(err) {
  cond <- if (inherits(err, "try-error")) attr(err, "condition") else err
  if (!inherits(cond, "condition")) return(character(0))
  out <- character(0)
  # Walk the condition chain twice: first looking for package_build_error
  # (build-time failures); if none found, fall back to dumping the full
  # condition message (covers dep-resolution failures like "Can't find
  # reference @<branch> in GitHub repo <owner>/<repo>" which happen before
  # any per-package build is attempted and therefore don't have a
  # package_build_error attached).
  cur <- cond
  for (i in seq_len(10L)) {
    if (inherits(cur, "package_build_error")) {
      pkg <- cur$package; if (is.null(pkg)) pkg <- ""
      ver <- cur$version; if (is.null(ver)) ver <- ""
      msg <- cur$message; if (is.null(msg)) msg <- ""
      stdo <- cur$stdout; if (is.null(stdo)) stdo <- character(0)
      if (nzchar(pkg)) {
        out <- c(out, sprintf("X Failed to build %s %s", pkg, ver))
      }
      # Drop pak's own "Failed to build source package <pkg>." preamble:
      # the regex `Failed to build\s+([A-Za-z0-9._]+)` would capture
      # "source" as a phantom package name, polluting downstream parsing.
      # We've already synthesized a properly-formatted line above.
      msgLines <- strsplit(msg, "\n", fixed = TRUE)[[1]]
      msgLines <- msgLines[!grepl("^Failed to build source package",
                                  msgLines, perl = TRUE)]
      if (length(msgLines)) out <- c(out, msgLines)
      if (length(stdo) && any(nzchar(stdo))) {
        out <- c(out,
                 strsplit(paste(stdo, collapse = "\n"),
                          "\n", fixed = TRUE)[[1]])
      }
      return(out)
    }
    cur <- cur$parent
    if (is.null(cur)) break
  }
  # No build-error parent. Walk the chain again concatenating messages so the
  # outer parsers (extractInstallFailures, pakBuildFailReason) see the
  # underlying cause. For a "Can't find reference @X in GitHub repo Y/Z"
  # failure, also synthesize a "X Failed to build <Z>" anchor so
  # extractInstallFailures' per-package iteration attributes the row to the
  # right package name.
  cur <- cond
  msgs <- character(0)
  for (i in seq_len(10L)) {
    m <- cur$message
    if (!is.null(m) && nzchar(m))
      msgs <- c(msgs, strsplit(m, "\n", fixed = TRUE)[[1]])
    cur <- cur$parent
    if (is.null(cur)) break
  }
  if (length(msgs)) {
    # Trailing punctuation (most often a period: "in GitHub repo a/b.")
    # gets greedily captured by `[A-Za-z0-9._-]+`; strip it.
    refRx <- "Can't find reference @[^ ]+ in GitHub repo ([A-Za-z0-9._-]+/[A-Za-z0-9._-]+)"
    refHit <- regmatches(msgs, regexec(refRx, msgs, perl = TRUE))
    for (m in refHit) {
      if (length(m) >= 2L && nzchar(m[2])) {
        ownerRepo <- sub("\\.+$", "", m[2])
        pkg <- sub("\\.+$", "", sub(".*/", "", ownerRepo))
        out <- c(out, sprintf("X Failed to build %s", pkg))
        break
      }
    }
    out <- c(out, msgs)
  }
  out
}

pakCacheDeleteTryAgain <- function(pkg2, packages, whRm) {
  prevFail <- get0("failedPkgs", envir = pakEnv())
  pkg3 <- extractPkgName(pkg2)
  if (any(pkg3 %in% prevFail)) {
    # Already tried clearing cache; give up on this package.
    # Do NOT modify failedPkgs (setdiff would clear it when pkg3 is already present).
    packages <- packages[-whRm]
  } else {
    try(pak::cache_delete(package = pkg3[1]), silent = TRUE)
    nowFails <- c(prevFail, pkg3)
    assign("failedPkgs", nowFails, envir = pakEnv())
  }
  packages
}

# ---------------------------------------------------------------------------
# pakWhoNeeds() -- diagnostic: given a pak_result (from pak::pkg_deps()), show
# which packages list `pkg` as a direct dependency (of any type), and flag any
# that list it under a "remotes"-style ref.
#
# Usage:
#   # After any Require call with usePak = TRUE (uses in-memory cache):
#   Require:::pakWhoNeeds("BH")
#
#   # Or supply pak_result directly:
#   pak_result <- pak::pkg_deps(c("SpaDES.core", "data.table"), dependencies = NA)
#   Require:::pakWhoNeeds("BH", pak_result)
# ---------------------------------------------------------------------------
pakWhoNeeds <- function(pkg, pak_result = NULL) {
  if (is.null(pak_result)) {
    # Try to pull the most-recently stored result from the in-memory cache.
    envKeys <- ls(envir = pakEnv(), pattern = "^pakDeps_")
    if (!length(envKeys)) {
      message("No cached pak_result found. Run Require::Install(...) with ",
              "options(Require.usePak = TRUE) first, or supply pak_result directly.")
      return(invisible(NULL))
    }
    # Use the most recently assigned key (last element of ls() is arbitrary, but
    # for a single active session there is usually only one).
    pak_result <- get(envKeys[length(envKeys)], envir = pakEnv(), inherits = FALSE)
  }
  if (is.null(pak_result) || !NROW(pak_result)) {
    message("pak_result is NULL or empty.")
    return(invisible(NULL))
  }
  hits <- lapply(seq_len(NROW(pak_result)), function(i) {
    dep_tbl <- tryCatch(as.data.table(pak_result$deps[[i]]), error = function(e) NULL)
    if (is.null(dep_tbl) || !NROW(dep_tbl)) return(NULL)
    matched <- dep_tbl[package == pkg]
    if (!NROW(matched)) return(NULL)
    cbind(data.table(parent = pak_result$package[i],
                     parent_ref = pak_result$ref[i]),
          matched[, .(dep_type = type, dep_ref = ref, op, version)])
  })
  hits <- rbindlist(Filter(Negate(is.null), hits), fill = TRUE, use.names = TRUE)
  if (!NROW(hits)) {
    message(pkg, " is not listed as a direct dependency of any package in pak_result.")
    return(invisible(hits))
  }
  hits[]
}

# ---------------------------------------------------------------------------
# pakDepsResolve() -- cached wrapper around pak::pkg_deps() retry loop
#
# Runs the full retry-and-fallback resolution and caches the resulting
# pak_result data.table in two tiers:
#
#   1. In-memory  : pakEnv() keyed by MD5 hash of inputs.  Free on purge or
#                   when R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE elapses.
#   2. Disk       : cacheDir()/pak/pkg_deps/<hash>.rds -- survives R restarts,
#                   giving cross-session speed-up for repeat calls.
#
# TTL defaults to 24 h (longer than the 1-h available.packages TTL because
# the dep tree changes far less often than package availability metadata).
# Override with options(Require.pak.depCacheTTL = <seconds>).
# ---------------------------------------------------------------------------
.pakDepsCacheTTL <- 24 * 3600   # 24 hours default

pakDepsCacheKey <- function(pkgsForPak, wh, repos, userPkgs = NULL) {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  # coerce to character vectors: options(repos = list(...)) is a supported
  # pattern, and sort() errors on list input with 'x must be atomic'
  payload <- list(pkgs  = sort(as.character(unlist(pkgsForPak, use.names = FALSE))),
                  wh    = sort(as.character(unlist(wh))),
                  repos = sort(as.character(unlist(repos, use.names = FALSE))))
  # `userPkgs` (when supplied) carries the user's original version-bearing
  # refs, e.g. c("stringfish (<= 0.15.8)", "qs (== 0.27.3)"). pak::pkg_deps()
  # only sees `pkgsForPak` -- the version-stripped form -- so without folding
  # the constraints into the cache key, two calls with the same package
  # *names* but different constraints (e.g. `... (<= 0.15.8)` vs no spec at
  # all) would share a cache entry. The cached pak_result is then reused by
  # downstream pakDepsToPkgDT processing whose behavior DOES branch on the
  # user-supplied constraints (e.g. trimRedundancies + lessThanToAt rely on
  # constraint rows actually being present in pkgDT) -- so a stale cached
  # entry from a different constraint set silently corrupts the next install
  # plan. Symptom: a second call after `remove.packages(pkg)` would see pak
  # asked for `any::pkg` instead of the user's pinned `pkg@ver` ref and
  # quietly install the wrong (latest) version.
  if (!is.null(userPkgs))
    payload$userPkgs <- sort(as.character(unlist(userPkgs, use.names = FALSE)))
  saveRDS(payload, tmp, compress = FALSE)
  unname(tools::md5sum(tmp))
}

pakDepsCacheDir <- function() {
  file.path(cacheDir(), "pak", "pkg_deps")
}

pakDepsResolve <- function(pkgsForPak, wh, repos, verbose, purge, userPkgs = NULL) {

  # --- 1. Compute cache key ---
  key      <- pakDepsCacheKey(pkgsForPak, wh, repos, userPkgs = userPkgs)
  envKey   <- paste0("pakDeps_", key)
  cacheDir <- pakDepsCacheDir()
  cacheFile <- file.path(cacheDir, paste0(key, ".rds"))
  ttl      <- getOption("Require.pak.depCacheTTL", .pakDepsCacheTTL)
  offline  <- isTRUE(getOption("Require.offlineMode"))

  # --- 2. In-memory cache hit ---
  if (!isTRUE(purge)) {
    cached <- get0(envKey, envir = pakEnv(), inherits = FALSE)
    if (!is.null(cached)) {
      messageVerbose("Require/pak skipping new package dependency identification: using memory cache (",
                     length(unique(cached$package)), " packages)",
                     verbose = verbose, verboseLevel = 1)
      return(cached)
    }
  }

  # --- 3. Disk cache hit ---
  if (!isTRUE(purge) && file.exists(cacheFile)) {
    age <- as.numeric(difftime(Sys.time(), file.mtime(cacheFile), units = "secs"))
    if (offline || age < ttl) {
      cached <- tryCatch(readRDS(cacheFile), error = function(e) NULL)
      if (!is.null(cached)) {
        assign(envKey, cached, envir = pakEnv())
        messageVerbose("Require/pak skipping new package dependency identification: using cache (",
                       length(unique(cached$package)), " packages, ",
                       round(age / 3600, 1), "h old)",
                       verbose = verbose, verboseLevel = 1)
        return(cached)
      }
    }
  }

  # --- 4. Cache miss: run the full retry + fallback resolution ---
  pak_result <- NULL

  for (.pakDepsAttempt in 1:5) {
    pak_result_or_err <- tryCatch(
      list(result = pakCall(pak::pkg_deps(pkgsForPak, dependencies = wh), verbose), err = NULL),
      error = function(e) list(result = NULL, err = conditionMessage(e))
    )
    pak_result <- pak_result_or_err$result
    if (!is.null(pak_result)) break

    errMsg <- pak_result_or_err$err

    # pak error messages often contain ANSI escape codes; strip them so that
    # nchar() gives the visible width and extracted refs are clean for matching.
    stripAnsi <- function(x) gsub("\033\\[[0-9;]*m", "", x)
    errLines <- stripAnsi(strsplit(errMsg, "\n")[[1]])
    changed  <- FALSE

    # --- Handle "X: Conflicts with Y" / "X conflicts with Y, to be installed" ---
    # pak reports this (case-insensitive) when two different refs resolve to the same
    # package. Two formats are seen in practice:
    #   "* owner/pkg@branch: Conflicts with pkg"            (format A)
    #   "* owner/pkg@branch: owner/pkg@branch conflicts with pkg, to be installed" (format B)
    # Strategy: keep the GitHub ref and remove the plain CRAN name from pkgsForPak.
    conflictRows    <- list()  # accumulate rows for the summary table
    conflictLines <- grep("(?i)conflicts with", errLines, value = TRUE, perl = TRUE)
    if (length(conflictLines)) {
      for (cl in conflictLines) {
        cl  <- trimws(sub("^\\*\\s*", "", cl))           # strip leading "* "
        lhs <- trimws(sub(":.*",  "", cl))               # before first ":"
        # Extract the RHS (what it conflicts with), case-insensitive, strip trailing noise
        rhs <- trimws(sub("(?i).*conflicts with\\s*", "", cl, perl = TRUE))
        rhs <- trimws(sub(",.*$", "", rhs))              # strip ", to be installed" etc.
        # Remove whichever is a plain CRAN ref (no @branch, no owner/);
        # if both are GitHub, remove the one without a @branch spec
        lhsGH <- isGH(lhs) || grepl("@", lhs)
        rhsGH <- isGH(rhs) || grepl("@", rhs)
        toRm  <- if (!rhsGH) rhs else if (!lhsGH) lhs else rhs
        pkgNmToRm <- extractPkgName(toRm)
        keep <- if (!rhsGH) lhs else rhs
        # Remove every pkgsForPak entry for this package name that is NOT the winner.
        # Only mark changed if something was actually removed -- otherwise the same
        # conflict will appear in the next attempt and we'll loop until attempt limit.
        before <- length(pkgsForPak)
        pkgsForPak <- pkgsForPak[
          !(extractPkgName(pkgsForPak) == pkgNmToRm &
              trimVersionNumber(pkgsForPak) != trimVersionNumber(keep))
        ]
        if (length(pkgsForPak) < before) {
          changed <- TRUE
          conflictRows[[length(conflictRows) + 1L]] <-
            list(Package = pkgNmToRm,
                 Conflict   = paste0(toRm, "  vs  ", keep),
                 Resolution = paste0("keep ", keep))
        }
      }
    }

    # --- Handle "Can't find package called X" (archived packages) ---
    cantLines <- grep(.txtCantFindPackage, errLines, value = TRUE)
    cantPkgs  <- trimws(sub(paste0(".*", .txtCantFindPackage), "", cantLines))
    cantPkgs  <- sub("\\.$", "", cantPkgs)
    cantPkgs  <- cantPkgs[nzchar(cantPkgs) & !grepl("::", cantPkgs)]
    if (length(cantPkgs)) {
      newRefs <- character(0)
      for (cp in cantPkgs) {
        urlRef <- tryCatch(
          pakGetArchive(cp, packages = cp, whRm = 1L),
          error = function(e) cp
        )
        urlRef <- grep("^url::", urlRef, value = TRUE)
        if (length(urlRef)) {
          newRefs <- c(newRefs, urlRef[1L])
          conflictRows[[length(conflictRows) + 1L]] <-
            list(Package = cp,
                 Conflict   = paste0(cp, " (not on CRAN)"),
                 Resolution = paste0("use ", urlRef[1L]))
        }
      }
      if (length(newRefs)) {
        pkgsForPak <- pkgsForPak[!extractPkgName(pkgsForPak) %in% cantPkgs]
        pkgsForPak <- c(pkgsForPak, newRefs)
        changed <- TRUE
      }
    }

    # --- Handle "X: dependency conflict" (Remotes-based CRAN/GitHub collision) ---
    # pak reports "X: dependency conflict" when X is listed as a plain CRAN ref in
    # pkgsForPak AND some GitHub package in the dep tree has "Remotes: owner/X" in its
    # DESCRIPTION, causing pak to see two different refs for the same package.
    # Unlike "Conflicts with" (where both refs are explicit), here only the CRAN ref
    # is in pkgsForPak; the GitHub ref was added implicitly via Remotes following.
    # Strategy: remove the plain CRAN ref from pkgsForPak so pak can resolve consistently
    # through the Remotes path. Step 2b normalization then restores CRAN for any package
    # the user originally requested from CRAN.
    # Pattern: "* ggplot2: dependency conflict" -- the leading "* " is NOT whitespace,
    # so we must NOT anchor with [[:space:]]* at the start.
    depConflictLines <- grep(":[[:space:]]*dependency conflict$", errLines, value = TRUE)
    if (length(depConflictLines)) {
      depConflictPkgs <- trimws(sub("^[[:space:]]*\\*[[:space:]]*", "", depConflictLines))
      depConflictPkgs <- trimws(sub("[[:space:]]*:[[:space:]]*dependency conflict$", "", depConflictPkgs))
      depConflictPkgs <- depConflictPkgs[nzchar(depConflictPkgs) & !grepl("[/:]", depConflictPkgs)]
      for (dcp in depConflictPkgs) {
        # Only remove plain CRAN-style refs (no /, no @, no ::)
        crankIdx <- which(extractPkgName(pkgsForPak) == dcp &
                          !isGH(pkgsForPak) & !grepl("::", pkgsForPak))
        if (length(crankIdx)) {
          pkgsForPak <- pkgsForPak[-crankIdx]
          changed <- TRUE
          # Try to find the GitHub ref pak saw via Remotes-following (may appear in
          # the error lines as a "conflicts with" entry for the same package).
          cand <- character(0)
          conflictForDcp <- grep(paste0("(?i)", dcp, ".*conflicts with|conflicts with.*", dcp),
                                 errLines, value = TRUE, perl = TRUE)
          if (length(conflictForDcp)) {
            cl2  <- trimws(sub("^\\*\\s*", "", conflictForDcp[1L]))
            lhs2 <- trimws(sub(":.*", "", cl2))
            rhs2 <- trimws(sub("(?i).*conflicts with\\s*", "", cl2, perl = TRUE))
            rhs2 <- trimws(sub(",.*$", "", rhs2))
            cand <- if (isGH(lhs2) || grepl("@", lhs2)) lhs2 else rhs2
          }
          # pakDepConflictRow() returns NULL (no context), or a list with the
          # appropriate Conflict string -- either "dcp vs owner/dcp@branch" (same
          # package) or "dcp (CRAN) vs dcp (via owner/other@branch Remotes)".
          row <- pakDepConflictRow(dcp, cand)
          if (!is.null(row)) conflictRows[[length(conflictRows) + 1L]] <- row
        }
      }
    }

    # Print a summary table of what was found and how it will be resolved.
    # Full error detail is available at verboseLevel >= 3 for debugging.
    if (changed && length(conflictRows)) {
      tbl <- rbindlist(conflictRows, fill = TRUE, use.names = TRUE)
      w1 <- max(nchar(c("Package",    tbl$Package)))
      w2 <- max(nchar(c("Conflict",   tbl$Conflict)))
      w3 <- max(nchar(c("Resolution", tbl$Resolution)))
      hdr  <- sprintf("  %-*s  %-*s  %-*s", w1, "Package", w2, "Conflict", w3, "Resolution")
      sep  <- paste0("  ", strrep("-", w1), "  ", strrep("-", w2), "  ", strrep("-", w3))
      rows <- sprintf("  %-*s  %-*s  %-*s",
                      w1, tbl$Package, w2, tbl$Conflict, w3, tbl$Resolution)
      messageVerbose(
        "Note: pak detected conflicts/archived packages (attempt ", .pakDepsAttempt,
        "); adjusting and retrying:\n",
        paste(c(hdr, sep, rows), collapse = "\n"),
        verbose = verbose, verboseLevel = 2)
    }
    messageVerbose("pak::pkg_deps full error (attempt ", .pakDepsAttempt, "):\n", errMsg,
                   verbose = verbose, verboseLevel = 3)

    if (!changed) break  # error is not one we know how to fix; give up
  }

  if (is.null(pak_result)) {
    # Final fallback: resolve each package individually so pak never sees cross-package
    # conflicts. Package A may list "SpaDES.tools" (CRAN) and package B may list
    # "PredictiveEcology/SpaDES.tools@development" -- resolving them separately avoids
    # the conflict. We then merge all dep tables and let Require's conflict resolution
    # (confirmEqualsDontViolateInequalitiesThenTrim + trimRedundancies) pick the winner.
    # Also pass any accumulated url:: archive refs to each call, so packages with
    # archived transitive deps (e.g. pryr) can still be resolved.
    #
    messageVerbose("Require Note: pak's batch dependency resolution failed; ",
                   "switching to per-package resolution.",
                   verbose = verbose, verboseLevel = 1)
    archiveRefs <- grep("^url::", pkgsForPak, value = TRUE)
    nonArchivePkgs <- pkgsForPak[!grepl("^url::", pkgsForPak)]
    per_pkg_results <- lapply(nonArchivePkgs, function(pkg) {
      # First try with archive refs (for packages with archived transitive deps).
      # If that fails (e.g., archive refs introduce new CRAN/GitHub conflicts), retry
      # without archive refs -- it's better to get a partial dep tree than nothing.
      query <- if (length(archiveRefs)) unique(c(pkg, archiveRefs)) else pkg
      result <- tryCatch(pakCall(pak::pkg_deps(query, dependencies = wh), verbose), error = function(e) NULL)
      if (is.null(result) && length(archiveRefs))
        result <- tryCatch(pakCall(pak::pkg_deps(pkg, dependencies = wh), verbose), error = function(e) NULL)
      result
    })
    per_pkg_results <- per_pkg_results[!sapply(per_pkg_results, is.null)]
    if (length(per_pkg_results)) {
      pak_result <- tryCatch(
        rbindlist(per_pkg_results, fill = TRUE, use.names = TRUE),
        error = function(e) NULL
      )
    }
  }

  # --- 5. Store successful result in both cache tiers ---
  if (!is.null(pak_result)) {
    assign(envKey, pak_result, envir = pakEnv())
    tryCatch({
      dir.create(cacheDir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(pak_result, cacheFile)
    }, error = function(e) NULL)   # non-fatal if disk write fails
  }

  pak_result
}

# ---------------------------------------------------------------------------
# Invalidate the pak dep-tree disk cache for a given set of inputs.
# Called after successful installation so the next call re-resolves freshly
# (installed state changed; cache key stays the same but should be revalidated
# sooner than the normal TTL would allow).
# ---------------------------------------------------------------------------
pakDepsCacheInvalidate <- function(pkgsForPak, wh, repos, userPkgs = NULL) {
  key      <- tryCatch(pakDepsCacheKey(pkgsForPak, wh, repos, userPkgs = userPkgs),
                       error = function(e) NULL)
  if (is.null(key)) return(invisible(NULL))
  envKey   <- paste0("pakDeps_", key)
  cacheFile <- file.path(pakDepsCacheDir(), paste0(key, ".rds"))
  rm(list = intersect(envKey, ls(envir = pakEnv())), envir = pakEnv())
  if (file.exists(cacheFile)) unlink(cacheFile)
  invisible(NULL)
}

# Resolve package dependencies using pak, returning a Require-format pkgDT.
# This replaces the pkgDep() + parsePackageFullname() + ... pipeline when usePak = TRUE.
pakDepsToPkgDT <- function(packages, which, libPaths, standAlone, verbose,
                          purge = getOption("Require.purge", FALSE),
                          install = TRUE) {
  pakLoad <- tryCatch(loadNamespace("pak"),
                      error = function(e) e)
  if (inherits(pakLoad, "error")) {
    stop("Please install pak (loadNamespace('pak') failed: ",
         conditionMessage(pakLoad), ")", call. = FALSE)
  }

  # pak spawns a subprocess that inherits .libPaths(). Set .libPaths() to match
  # Require's standAlone semantics before calling pak, then restore on exit.
  #
  # standAlone = TRUE  -> c(libPaths[1], base_pkg_lib)   (isolated project library)
  # standAlone = FALSE -> c(libPaths[1], existing .libPaths())  (shared)
  #
  # In both cases, pak's own library must be present so the subprocess can load pak.
  pakLib    <- tryCatch(dirname(find.package("pak")), error = function(e) NULL)
  basePkgLib <- tail(.libPaths(), 1L)   # always the base R packages path
  origPaths  <- .libPaths()
  if (isTRUE(standAlone)) {
    newPaths <- unique(c(libPaths[1L], basePkgLib))
  } else {
    newPaths <- unique(c(libPaths[1L], origPaths))
  }
  if (!is.null(pakLib) && !pakLib %in% newPaths)
    newPaths <- c(newPaths, pakLib)
  .libPaths(newPaths)
  on.exit(.libPaths(origPaths), add = TRUE)

  # pak uses logical: TRUE = include Suggests, NA = standard (Imports/Depends/LinkingTo)
  wh <- if (any(grepl("suggests", tolower(unlist(which))))) TRUE else NA

  # Track which packages the user originally requested as plain CRAN refs (no GitHub, no url::).
  # Used in step 2b to normalize Remotes-based GitHub refs back to plain CRAN names so that
  # pakInstallFiltered installs from CRAN rather than from a fork.
  userCRANpkgs <- extractPkgName(packages[!isGH(packages) & !grepl("::", packages)])

  # Pre-resolve conflicts in the package list using Require's own deduplication logic
  # before handing anything to pak. This handles:
  #   (a) Same package as both CRAN ref and GitHub ref -> trimRedundantVersionAndNoVersion
  #       removes the no-version entry, keeping whichever has a version constraint.
  #       If neither has a version spec, the GitHub ref (higher repoLocation priority)
  #       is kept by the subsequent name-based dedup below.
  #   (b) Multiple GitHub branches for same package (e.g. @master vs @development) ->
  #       the branch with the highest version constraint wins.
  resolvedPkgs <- tryCatch(
    trimRedundancies(packages[!extractPkgName(packages) %in% .basePkgs])$packageFullName,
    error = function(e) packages
  )

  # Strip version specs and HEAD flags for the pak query; pak resolves from the ref alone
  pkgsForPak <- resolvedPkgs
  pkgsForPak <- HEADtoNone(pkgsForPak)
  pkgsForPak <- trimVersionNumber(pkgsForPak)
  pkgsForPak <- pkgsForPak[!pkgsForPak %in% .basePkgs]
  # For any remaining duplicated package names (both have no version spec), prefer GH ref
  pkgNms <- extractPkgName(pkgsForPak)
  dupNms <- unique(pkgNms[duplicated(pkgNms)])
  if (length(dupNms)) {
    toRemove <- integer(0)
    for (pn in dupNms) {
      idx <- which(pkgNms == pn)
      ghIdx <- idx[isGH(pkgsForPak[idx])]
      if (length(ghIdx) > 0) toRemove <- c(toRemove, setdiff(idx, ghIdx[1L]))
      else                    toRemove <- c(toRemove, idx[-1L])
    }
    if (length(toRemove)) pkgsForPak <- pkgsForPak[-toRemove]
  }
  pkgsForPak <- unique(pkgsForPak)
  # Convert == version specs to pak @version format for the dep query
  pkgsForPak <- equalsToAt(pkgsForPak)

  # Pin already-installed user packages to their installed version before
  # pak resolves transitive deps. Without this, pak picks the LATEST CRAN
  # release of each parent (e.g. processx 3.9.0) and returns its Imports
  # constraints -- forcing transitive deps to upgrade even when the
  # parent itself stays installed at an older version (e.g. processx
  # 3.8.6 Imports `ps (>= 1.2.0)`, but 3.9.0 Imports `ps (>= 1.9.3)`,
  # so an unpinned query made `Require("processx")` spuriously upgrade
  # ps from 1.9.2 to 1.9.3).
  #
  # Pin even when install = "force". `install = "force"` is documented to
  # force the user-requested packages, not their deps. Resolving the dep
  # tree against the LATEST user-package versions sweeps any transitive
  # constraint upgrades (e.g. "latest reproducible Imports broom >=
  # 1.0.13") into the plan, gratuitously bumping broom even though the
  # installed user package version is still satisfied by the installed
  # dep. Pinning to installed versions resolves the dep tree from the
  # constraints the user is already running against, so deps stay where
  # they are. Users wanting deps refreshed should use update.packages()
  # or the pak equivalent.
  pkgsForPak <- pinInstalledForPak(pkgsForPak, libPaths = libPaths,
                                   resolvedPkgs = resolvedPkgs)

  if (!length(pkgsForPak)) return(toPkgDTFull(character()))

  # 1. Resolve the full dep tree via pak, with two-tier caching (in-memory + disk).
  #    pakDepsResolve() handles the retry loop, conflict resolution, per-package
  #    fallback, and cache read/write. Returns NULL only if all strategies fail.
  #    `userPkgs = resolvedPkgs` keys the cache on the user's version-bearing
  #    refs (e.g. "stringfish (<= 0.15.8)") in addition to the version-stripped
  #    `pkgsForPak`. Without this, calls that differ only in constraints share
  #    the same entry and downstream pkgDT construction misuses the cached
  #    dep tree -- see pakDepsCacheKey() for the failure mode this prevents.
  pak_result <- pakDepsResolve(pkgsForPak, wh,
                               repos    = getOption("repos"),
                               verbose  = verbose,
                               purge    = purge,
                               userPkgs = resolvedPkgs)

  if (is.null(pak_result)) {
    messageVerbose("pak::pkg_deps: all strategies failed; using direct package list only.",
                   verbose = verbose, verboseLevel = 2)
    return(toPkgDTFull(packages))
  }

  # 2. Flatten all deps sub-tables to get the raw version requirements.
  # pak$deps[[i]] has columns: ref, type, package, op, version
  # 'type' is lowercase ("imports", "depends", "linkingto", "suggests")
  # 'op' is ">=" or "" (empty string means no version constraint)
  # 'version' is the minimum required version from the DESCRIPTION file
  validTypes <- tolower(unlist(which))
  all_reqs_list <- lapply(pak_result$deps, function(dep_tbl) {
    if (is.null(dep_tbl) || !NROW(dep_tbl)) return(NULL)
    dep_tbl <- as.data.table(dep_tbl)
    dep_tbl <- dep_tbl[tolower(type) %in% validTypes]
    dep_tbl <- dep_tbl[!package %in% c(.basePkgs, "R")]
    dep_tbl
  })
  all_reqs <- rbindlist(all_reqs_list, fill = TRUE, use.names = TRUE)

  # Filter out "Require" itself from transitive deps: Require is always running
  # (we are inside it), so it is never absent. Including it as a transitive dep
  # causes needToRestartR() to fire NeedRestart=TRUE, which incorrectly marks
  # data.table and sys as "Need to restart R" when an impossible version constraint
  # like data.table (>=100.0) is in the user's package list.
  if (NROW(all_reqs)) {
    all_reqs <- all_reqs[package != "Require"]
  }

  # 2b. Normalize refs in all_reqs to prevent CRAN/GitHub conflicts during install.
  # The dep sub-tables carry the raw dep ref (e.g. "tidyverse/ggplot2" from a Remotes
  # field) which can conflict with plain CRAN entries in pakInstallFiltered.  Normalize:
  # (1) Packages the user originally requested as plain CRAN -> always use plain name.
  # (2) Packages pak resolved as type "cran"/"standard" -> also use plain name.
  # This ensures pakInstallFiltered passes "any::ggplot2" (not "tidyverse/ggplot2")
  # to pak::pak(), avoiding spurious CRAN/GitHub conflicts during the install step.
  if (NROW(all_reqs)) {
    # User-requested CRAN packages: unconditionally normalize ref to plain name
    all_reqs[package %in% userCRANpkgs, ref := package]
    # pak-resolved CRAN packages: also normalize (covers transitive CRAN deps)
    if (!is.null(pak_result)) {
      pakResDT <- tryCatch(as.data.table(pak_result), error = function(e) NULL)
      if (!is.null(pakResDT) && all(c("package", "type") %in% names(pakResDT))) {
        refNorm <- unique(pakResDT[, .(package, src_type = type)])
        refNorm <- refNorm[order(!(src_type %in% c("cran", "standard")))]
        refNorm <- refNorm[!duplicated(package)]
        cran_pkgs <- refNorm[src_type %in% c("cran", "standard"), package]
        all_reqs[package %in% cran_pkgs, ref := package]
      }
    }
  }

  # 3. Build packageFullName from pak's ref + op + version
  if (NROW(all_reqs)) {
    all_reqs[, packageFullName := paste0(
      ref,
      ifelse(nzchar(op) & nzchar(version),
             paste0(" (", op, " ", version, ")"),
             "")
    )]
  }

  # 3b. Check that pak's resolved versions can actually satisfy any >= / > constraints
  # the user specified. pak silently installs the latest available version even when
  # it doesn't satisfy the constraint (e.g., fpCompare 0.2.4 installed despite >=2.0.0).
  # Catch these now: warn and remove the package so it is never passed to pakInstallFiltered.
  #
  # Only applies to CRAN-like packages. GitHub (owner/repo@branch) and url:: refs are
  # excluded: for GitHub refs pak installs exactly from the specified branch/commit, so
  # if the branch has the required version pak will install it; if not, pak errors during
  # install (not silently installs wrong version). Applying this check to GitHub refs
  # causes false positives when pak resolved an older cached/CRAN version for the same
  # package name while the user's GitHub ref is the one that actually satisfies the constraint.
  if (NROW(pak_result)) {
    pakVerMap <- setNames(pak_result$version, pak_result$package)
    origCheck <- toPkgDTFull(packages[!extractPkgName(packages) %in% .basePkgs])
    # Exclude GitHub and url:: refs from the version check -- only check CRAN-like packages.
    isCRANcheck <- !isGH(origCheck$packageFullName) &
                   !startsWith(origCheck$packageFullName, "url::")
    needCheck  <- origCheck[isCRANcheck &
                            !is.na(inequality) & inequality %in% c(">=", ">") &
                            !is.na(versionSpec) & nzchar(versionSpec) &
                            Package %in% names(pakVerMap) &
                            # Skip packages where pak returned NA/empty version (e.g. some GitHub
                            # deps resolved without metadata). compareVersion2("", ...) returns FALSE,
                            # which would incorrectly flag them as unsatisfiable.
                            nzchar(pakVerMap[Package]) & !is.na(pakVerMap[Package])]
    if (NROW(needCheck)) {
      canSatisfy <- compareVersion2(pakVerMap[needCheck$Package],
                                    needCheck$versionSpec, needCheck$inequality)
      badPkgs <- needCheck$Package[canSatisfy %in% FALSE]
      if (length(badPkgs)) {
        # Before flagging a package as unsatisfiable, check if the currently
        # installed version already satisfies the constraint.  This is important
        # for dev-version packages (e.g. LandR >= 1.1.5.9064) where the user has
        # the dev version installed but pak's CRAN resolution returns an older
        # version.  Removing such packages from `user_pkgFN` would prevent them
        # from appearing in pkgDT, so recordLoadOrder() could not find them and
        # require() would never be called -- the package would not be attached
        # even though it is correctly installed.
        badCandidates <- needCheck[Package %in% badPkgs]
        # Use the same libPaths that doLoads() / installedVers() will use, so that
        # the "is it already installed?" check is consistent with the later loading
        # step.  .libPaths() at this point has been changed to newPaths by the
        # standAlone guard; using the `libPaths` argument avoids that discrepancy.
        instPkgVers <- tryCatch({
          ipAll <- installed.packages(lib.loc = libPaths)
          setNames(ipAll[, "Version"], ipAll[, "Package"])
        }, error = function(e) character(0))
        trulyBad <- vapply(badCandidates$Package, function(pkg) {
          instVer <- instPkgVers[pkg]
          if (is.na(instVer) || !nzchar(instVer)) return(TRUE)  # not installed -> bad
          row <- badCandidates[Package == pkg][1L]
          !isTRUE(compareVersion2(instVer, row$versionSpec, row$inequality))
        }, logical(1))
        badPkgs <- badCandidates$Package[trulyBad]
        if (length(badPkgs)) {
          badFullNames <- badCandidates$packageFullName[trulyBad]
          warning(messageCantInstallNoVersion(badFullNames), call. = FALSE)
          packages <- packages[!extractPkgName(packages) %in% badPkgs]
        }
      }
    }
  }

  # 4. Include the user's originally stated packages (with their version specs).
  # These may have stricter requirements than what DESCRIPTION files state.
  user_pkgFN <- packages[!extractPkgName(packages) %in% .basePkgs]

  # 4a. Sync url:: archive refs from pkgsForPak back into user_pkgFN.
  # The retry loop may have replaced plain package names (e.g. "fastdigest") with
  # url:: archive refs (e.g. "url::https://.../fastdigest_0.6-4.tar.gz") in
  # pkgsForPak. Without this sync, user_pkgFN still has the plain name, so
  # pakInstallFiltered would try "any::fastdigest" instead of the url:: ref.
  archiveRefsInPkgsForPak <- grep("^url::", pkgsForPak, value = TRUE)
  if (length(archiveRefsInPkgsForPak)) {
    archivePkgNamesFromPak <- extractPkgName(
      filenames = basename(sub("^url::", "", archiveRefsInPkgsForPak))
    )
    for (.i in seq_along(archivePkgNamesFromPak)) {
      matchIdx <- which(extractPkgName(user_pkgFN) == archivePkgNamesFromPak[.i])
      if (length(matchIdx))
        user_pkgFN[matchIdx] <- archiveRefsInPkgsForPak[.i]
    }
  }

  # 5. Combine all packageFullName strings and parse through Require's existing pipeline
  all_pkgFN <- unique(c(
    user_pkgFN,
    if (NROW(all_reqs)) all_reqs$packageFullName else character()
  ))
  all_pkgFN <- all_pkgFN[nzchar(all_pkgFN)]

  pkgDT <- toPkgDTFull(all_pkgFN)

  # Fix Package column for url:: refs (archived packages).
  # extractPkgName() cannot parse "url::https://...pkg_ver.tar.gz" correctly --
  # it returns the full URL string instead of the package name.  Extract the
  # package name from the filename component of the URL so deduplication and
  # version checking work correctly.
  urlPkgRows <- which(startsWith(pkgDT$Package, "url::"))
  if (length(urlPkgRows)) {
    urlPkgNames <- extractPkgName(
      filenames = basename(sub("^url::", "", pkgDT$Package[urlPkgRows]))
    )
    # Break any SEXP aliasing between Package and packageFullName before any := .
    # toPkgDTFull() calls toDT(Package = extractPkgName(x), packageFullName = x).
    # For url:: refs extractPkgName() returns its input unchanged (same R SEXP),
    # so both columns end up pointing to the SAME character vector.  A := on either
    # column would then silently modify the other column too -- sequential := calls
    # would interfere.  Forcing as.character() allocates a new vector, breaking the
    # aliasing so the two columns become fully independent.
    set(pkgDT, NULL, "packageFullName", as.character(pkgDT$packageFullName))
    pkgDT[urlPkgRows, Package := urlPkgNames]
    # packageFullName still holds the original "url::..." strings for those rows.
    # Remove plain-name rows for packages that have a url:: ref -- the url:: version
    # carries the correct install path and must be used for the actual installation.
    archivePkgs <- pkgDT[startsWith(packageFullName, "url::")]$Package
    pkgDT <- pkgDT[!(Package %in% archivePkgs & !startsWith(packageFullName, "url::"))]
  }

  pkgDT <- confirmEqualsDontViolateInequalitiesThenTrim(pkgDT)
  pkgDT <- trimRedundancies(pkgDT)

  # Store pak's globally-resolved version map in pakEnv() so pakInstallFiltered
  # can use it as the authoritative constraint.  The pkgDT column approach is
  # unreliable because Require2.R re-runs confirmEqualsDontViolateInequalitiesThenTrim
  # and trimRedundancies on the returned pkgDT, which drops any extra columns.
  if (!is.null(pak_result) && !is.null(pak_result$version) && !is.null(pak_result$package)) {
    assign("pakResolvedVersionMap",
           setNames(as.character(pak_result$version), pak_result$package),
           envir = pakEnv())
  }

  pkgDT
}

# ---------------------------------------------------------------------------
# Extract package names from pak output that report a per-package build
# failure. pak prints a line of the form
#
#   X Failed to build <pkg> <version> (<elapsed>)
#
# (with a Unicode cross and possibly ANSI color codes) for each ref whose
# R CMD INSTALL returned non-zero. The other broken refs in the same batch
# are typically *cascade casualties* -- they would have built fine on their
# own, but pak aborted the rest of the install plan when one ref failed.
# Identifying just the true culprits lets us retry the cascade casualties
# successfully, then attempt the culprits at the end (when their build-time
# deps are present in the project lib).
# ---------------------------------------------------------------------------
extractBuildFailures <- function(output) {
  if (!length(output) || !any(nzchar(output))) return(character(0))
  # Strip ANSI color codes so the regex doesn't have to consume them.
  clean <- gsub("\033\\[[0-9;]*m", "", paste(output, collapse = "\n"))
  m <- regmatches(clean,
                  gregexpr("Failed to build\\s+([A-Za-z0-9._]+)",
                           clean, perl = TRUE))[[1]]
  if (!length(m)) return(character(0))
  unique(sub("Failed to build\\s+", "", m, perl = TRUE))
}

# ---------------------------------------------------------------------------
# Parse pak's "Missing N system packages" block. Returns a named character
# vector: names = pkg the system dep is needed BY (e.g. "fs"), values = the
# system package name (e.g. "cmake"). A single pkg with two missing sysreqs
# produces two entries with the same name. Empty character() if no block.
#
# pak emits this block during the resolve phase, BEFORE attempting the
# build, and it's a deterministic signal that the build will fail until the
# user installs the system packages. Retrying without fixing the sysreqs is
# pointless and produces the symptom in issue [INFINITE-RETRY]:
#     identify-and-defer iter 1: 1 culprit(s) deferred (fs); 74 cascade
#     casualties queued for next pass
#   followed by infinite repeats of the same iter on the same passList,
#   because pak's dep resolver re-includes fs in every plan that contains
#   any of its 74 dependents.
#
# Format (color codes already stripped):
#   X Missing 2 system packages. You'll probably need to install them manually:
#   + cmake       - fs
#   + libuv1-dev  - fs
# Pkg lists can be comma-separated when a single sysreq is needed by many.
# ---------------------------------------------------------------------------
extractMissingSysreqs <- function(output) {
  if (!length(output) || !any(nzchar(output))) return(character(0))
  clean <- gsub("\033\\[[0-9;]*m", "", paste(output, collapse = "\n"))
  lines <- strsplit(clean, "\n")[[1]]
  hdrs  <- grep("Missing\\s+\\d+\\s+system packages?", lines, perl = TRUE)
  if (!length(hdrs)) return(character(0))
  out <- character(0)
  for (h in hdrs) {
    j <- h + 1L
    while (j <= length(lines)) {
      ln <- lines[j]
      m  <- regmatches(ln, regexec(
        "^\\s*[+]\\s+(\\S+)\\s+-\\s+(.+?)\\s*$", ln, perl = TRUE))[[1]]
      if (length(m) < 3L) break
      sysreq <- m[2]
      pkgs   <- trimws(strsplit(m[3], "\\s*,\\s*")[[1]])
      pkgs   <- pkgs[nzchar(pkgs)]
      if (length(pkgs)) {
        v <- rep(sysreq, length(pkgs))
        names(v) <- pkgs
        out <- c(out, v)
      }
      j <- j + 1L
    }
  }
  out
}

# ---------------------------------------------------------------------------
# Parse pak's captured stderr/messages for per-package install failure
# diagnostics. Returns a data.table:
#
#   package       (chr)  ref / package name as pak referred to it
#   reason_type   (chr)  one of:
#                          "missing-build-deps"   build-time deps absent at
#                                                 R CMD INSTALL pre-flight
#                                                 check (typical cascade
#                                                 culprit: e.g. PSPclean
#                                                 needing sf/terra)
#                          "compile-error"        gcc / Fortran error during
#                                                 source build
#                          "version-conflict"     pak refused: dep tree has
#                                                 unsatisfiable version pin
#                          "build-error"          generic "Failed to build"
#                                                 with no ERROR: line we
#                                                 could parse
#                          "still-missing"        package wasn't in
#                                                 project lib at the end
#                                                 of all install passes,
#                                                 but pak emitted no
#                                                 specific failure for it
#                                                 (e.g. cascade casualty
#                                                 from a wedged subprocess)
#   reason_brief  (chr)  one-line summary suitable for a status bar
#   reason_detail (chr)  the actual pak error line(s) for context
# ---------------------------------------------------------------------------
extractInstallFailures <- function(output) {
  empty <- data.table(package = character(0),
                      reason_type = character(0),
                      reason_brief = character(0),
                      reason_detail = character(0))
  if (!length(output) || !any(nzchar(output))) return(empty)
  clean <- gsub("\033\\[[0-9;]*m", "", paste(output, collapse = "\n"))
  lines <- strsplit(clean, "\n")[[1]]

  results <- list()

  # X Failed to build PKG VER (TIME)  ->  per-package culprit
  buildFailIdx <- grep("Failed to build\\s+[A-Za-z0-9._]+", lines)
  for (i in buildFailIdx) {
    pkg <- sub(".*Failed to build\\s+([A-Za-z0-9._]+).*", "\\1", lines[i])
    # Look up to 25 lines ahead for the line that explains WHY the build
    # failed. We scan the window in priority order (most specific first)
    # rather than picking the first match of a single combined regex,
    # because a generic line like "* installing *source* package 'X'..."
    # almost always appears BEFORE the actual ERROR: / vignette-builder /
    # lazy-loading line in pak's stream -- so first-match-wins drops the
    # informative line in favor of the noise.
    window <- lines[i:min(i + 25L, length(lines))]
    # Each entry: c(<key>, <perl regex>, <ignore.case "T"/"F">). Order = priority.
    # ['\u2018] / [\u2019'] character classes match BOTH a straight ASCII
    # quote AND the unicode left/right single quotation marks pak's output
    # uses. \u escapes keep this file ASCII (R CMD check requires) while
    # the runtime regex still matches both forms.
    patternList <- list(
      c("missing-github-branch",
        "Can't find reference @[^ ]+ in GitHub repo [A-Za-z0-9._-]+/[A-Za-z0-9._-]+", "F"),
      c("vignette-builder",
        "vignette builder ['\u2018][^'\u2019]+['\u2019] not found", "F"),
      c("no-package-called",
        "there is no package called ['\u2018][^'\u2019]+['\u2019]", "F"),
      c("deps-not-available",
        "dependencies\\s+.+\\s+are not available for package",        "F"),
      c("lazy-load-failed",
        "lazy loading failed for package",                              "F"),
      c("compile-error",
        "compilation failed|fatal error",                               "T"),
      c("cannot-remove",
        "cannot remove",                                                "F"),
      # Generic ERROR: line -- only used when none of the specific patterns
      # above matched anywhere in the window.
      c("generic-error",  "^\\s*ERROR:",                                "F")
    )
    errLine <- NA_character_
    errKind <- NA_character_
    for (pat in patternList) {
      hit <- grep(pat[2], window, value = TRUE, perl = TRUE,
                  ignore.case = identical(pat[3], "T"))
      if (length(hit)) { errLine <- hit[1]; errKind <- pat[1]; break }
    }

    if (is.na(errLine)) {
      reasonType <- "build-error"
      reasonBrief <- "build failed (no specific reason parsed)"
      reasonDetail <- lines[i]
    } else if (identical(errKind, "missing-github-branch")) {
      # Pull out branch and owner/repo from
      #   Can't find reference @<branch> in GitHub repo <owner>/<repo>
      # Trailing punctuation (most commonly a period in pak's emitted
      # message) gets greedily captured -- strip it.
      branch <- sub(".*Can't find reference @([^ ]+) in GitHub repo .*",
                    "\\1", errLine, perl = TRUE)
      ownerRepo <- sub(".*in GitHub repo ([A-Za-z0-9._-]+/[A-Za-z0-9._-]+).*",
                       "\\1", errLine, perl = TRUE)
      ownerRepo <- sub("\\.+$", "", ownerRepo)
      reasonType <- "missing-github-branch"
      reasonBrief <- paste0("GitHub branch '", branch, "' not found in ",
                            ownerRepo, " (did you push it?)")
      reasonDetail <- errLine
    } else if (identical(errKind, "vignette-builder")) {
      vb <- sub(".*vignette builder ['\u2018]([^'\u2019]+)['\u2019] not found.*",
                "\\1", errLine, perl = TRUE)
      reasonType <- "missing-build-deps"
      reasonBrief <- paste0("missing VignetteBuilder package: ", vb)
      reasonDetail <- errLine
    } else if (identical(errKind, "no-package-called")) {
      pk <- sub(".*there is no package called ['\u2018]([^'\u2019]+)['\u2019].*",
                "\\1", errLine, perl = TRUE)
      reasonType <- "missing-build-deps"
      reasonBrief <- paste0("missing build-time package: ", pk)
      reasonDetail <- errLine
    } else if (identical(errKind, "deps-not-available")) {
      missing <- sub(".*dependencies\\s+(.+?)\\s+are not available for package.*",
                     "\\1", errLine)
      reasonType <- "missing-build-deps"
      reasonBrief <- paste0("build-time deps not yet in lib: ", missing)
      reasonDetail <- errLine
    } else if (identical(errKind, "lazy-load-failed")) {
      # "lazy loading failed for package 'X'" is itself a downstream symptom
      # -- the actual cause was emitted earlier in the build (e.g. an error
      # in .onLoad, an evaluation error in package R code, a missing
      # dependency referenced at top level). Try to surface the preceding
      # "Error:" / "Error in" line; otherwise fall back to the symptom.
      windowIdx <- which(window == errLine)[1]
      preceding <- if (!is.na(windowIdx) && windowIdx > 1L)
        rev(window[seq_len(windowIdx - 1L)]) else character(0)
      cause <- grep("^\\s*(Error[: ]|in method for)",
                    preceding, value = TRUE, perl = TRUE)
      cause <- if (length(cause)) trimws(cause[1]) else ""
      reasonType <- "build-error"
      reasonBrief <- if (nzchar(cause))
        paste0("lazy loading failed: ", cause)
      else
        sub("^\\s*ERROR:\\s*", "", errLine)
      reasonDetail <- if (nzchar(cause)) paste(cause, errLine, sep = " | ") else errLine
    } else if (identical(errKind, "compile-error")) {
      reasonType <- "compile-error"
      reasonBrief <- sub("^\\s*", "", errLine)
      reasonDetail <- errLine
    } else {
      reasonType <- "build-error"
      reasonBrief <- sub("^\\s*ERROR:\\s*", "", errLine)
      reasonDetail <- errLine
    }
    results[[length(results) + 1]] <- list(
      package = pkg, reason_type = reasonType,
      reason_brief = reasonBrief, reason_detail = reasonDetail)
  }

  # Conflicts: PKG depends on DEP == X but PKG2 depends on DEP == Y
  conflictIdx <- grep("Conflicts:|Cannot install packages.*Conflicts", lines)
  for (i in conflictIdx) {
    detail <- lines[i]
    m <- regmatches(detail, regexec("([A-Za-z0-9._]+)\\s+depends on", detail))[[1]]
    pkg <- if (length(m) > 1) m[2] else NA_character_
    if (is.na(pkg)) next
    results[[length(results) + 1]] <- list(
      package = pkg, reason_type = "version-conflict",
      reason_brief = "version conflict in dep tree",
      reason_detail = detail)
  }

  if (!length(results)) return(empty)
  out <- rbindlist(results)
  unique(out, by = c("package", "reason_type"))
}

# ---------------------------------------------------------------------------
# Print a structured install summary: each package that didn't end up in the
# project lib, with the reason (parsed from captured pak output) and a short
# hint about what to do next. Returns the failure table invisibly so callers
# can act on it programmatically.
# ---------------------------------------------------------------------------
reportInstallFailures <- function(failures, missingPkgNames = character(0),
                                   verbose = getOption("Require.verbose", 1)) {
  if (!is.data.table(failures))
    failures <- as.data.table(failures)

  reasoned <- failures$package
  unexplained <- setdiff(missingPkgNames, reasoned)
  if (length(unexplained)) {
    failures <- rbind(failures, data.table(
      package      = unexplained,
      reason_type  = "still-missing",
      reason_brief = "absent from project lib; pak did not emit a per-package error",
      reason_detail = ""), fill = TRUE)
  }
  if (NROW(failures) == 0L) return(invisible(failures))

  if (verbose >= 0) {
    n <- NROW(failures)
    cat(sprintf("\n=== Install summary: %d package(s) not installed ===\n", n))
    nameW <- max(nchar(failures$package), 8L)
    typeW <- max(nchar(failures$reason_type), 12L)
    for (i in seq_len(n)) {
      cat(sprintf("  %-*s  [%-*s]  %s\n",
                  nameW, failures$package[i],
                  typeW, failures$reason_type[i],
                  failures$reason_brief[i]))
    }
    cat("\n")
  }
  invisible(failures)
}

# ---------------------------------------------------------------------------
# pakResetSubprocess: force pak to spawn a fresh background R session on the
# next pak::pak() call. pak holds a persistent callr r_session in
# pak:::pkg_data$remote and reuses it across calls; if the previous call
# pushed pak's subprocess into a wedged state (e.g. after a large failed
# install plan, where pak emits "Error : ! error in pak subprocess" without
# naming a build culprit), every subsequent call inherits the failure even
# if the inputs change. Killing the r_session forces pak's
# restart_remote_if_needed() to allocate a fresh one. Safe no-op if pak
# isn't loaded or the remote isn't an r_session.
# ---------------------------------------------------------------------------
pakResetSubprocess <- function() {
  if (!requireNamespace("pak", quietly = TRUE)) return(invisible())
  rs <- tryCatch(
    get("pkg_data", envir = asNamespace("pak"))$remote,
    error = function(e) NULL)
  if (inherits(rs, "r_session")) {
    try(rs$interrupt(), silent = TRUE)
    try(rs$wait(100), silent = TRUE)
    try(rs$kill(), silent = TRUE)
  }
  invisible()
}

# ---------------------------------------------------------------------------
# pakSerialInstall: install pak refs one at a time. Used by the "deferred"
# pass of identify-and-defer for refs whose first parallel attempt failed.
# Each call only sees a single ref's transitive subgraph, and the build-time
# deps are now in the project lib (installed during the cascade-casualty
# retry pass), so the R CMD INSTALL pre-flight check passes.
#
# Each call uses dependencies = NA (CRAN-style) or FALSE (GitHub/url::), and
# upgrade = FALSE for CRAN, TRUE for GitHub -- same per-ref policy as the
# parallel version. Failures are warned but don't abort the loop.
# ---------------------------------------------------------------------------
pakSerialInstall <- function(pkgs, lib, repos, verbose) {
  if (!length(pkgs)) return(invisible(NULL))
  opts <- options(repos = repos)
  on.exit(options(opts), add = TRUE)
  failed <- character(0)
  for (i in seq_along(pkgs)) {
    pkg <- pkgs[[i]]
    isGH_  <- isGH(pkg)
    isUrl_ <- startsWith(pkg, "url::")
    # Per-ref dependency policy for this serial pass:
    #   GitHub refs    : deps = FALSE, upgrade = TRUE  (transitive CRAN deps
    #                    are handled in the parallel CRAN batch -- see
    #                    pakRetryLoop's main call. upgrade = TRUE ensures
    #                    pak fetches the requested branch HEAD.)
    #   url:: refs     : deps = NA,    upgrade = FALSE (typical case is the
    #                    CRAN-archive fallback for an archived-from-CRAN
    #                    package; its hard deps must be installed first or
    #                    the source build's pre-flight check fails).
    #   plain CRAN     : deps = NA,    upgrade = FALSE (some hard deps may
    #                    not yet be in lib, e.g. when the cascade-casualty
    #                    fallback installs refs whose deps were also
    #                    casualties.)
    deps <- if (isGH_) FALSE else NA
    up   <- isGH_
    # Capture pak's subprocess messages for this single ref so the warning
    # below can surface the actual root cause (e.g. "namespace 'X' is
    # imported by 'Y' so cannot be unloaded") instead of pak's generic
    # wrapper exception "Error : ! error in pak subprocess".
    pkgMsgs <- character(0)
    ## Only install a calling handler when we're suppressing output anyway
    ## (verbose < 1). At verbose >= 1, the handler's mere presence in cli's
    ## condition chain breaks cli's dynamic-vs-static redraw heuristic and
    ## every progress tick spews as a fresh line. Trade-off: at verbose >= 1
    ## we lose the per-package message capture, so pakBuildFailReason has
    ## only the err string to work with -- fine, because the user already saw
    ## the messages live on console at verbose >= 1.
    if (verbose < 1) {
      err <- try(withCallingHandlers(
        pakCall(
          pak::pak(pkg, lib = lib, ask = FALSE,
                   dependencies = deps, upgrade = up),
          verbose),
        message = function(m) {
          pkgMsgs <<- c(pkgMsgs, conditionMessage(m))
        }), silent = TRUE)
    } else {
      err <- try(pakCall(
        pak::pak(pkg, lib = lib, ask = FALSE,
                 dependencies = deps, upgrade = up),
        verbose), silent = TRUE)
    }
    if (is(err, "try-error")) {
      failed <- c(failed, pkg)
      reason <- pakBuildFailReason(as.character(err), pkgMsgs)
      # NOT a warning: pakSerialInstall is one of several retry layers
      # (parallel batch -> identify-and-defer iter -> serial fallback ->
      # CRAN-archive fallback). A failure here may still be resolved by
      # the archive-fallback pass downstream -- for example, an exact-pin
      # ref like `qs@0.27.3` that pak can't resolve via its current CRAN
      # mirror typically succeeds when pakInstallFiltered's archive pass
      # retries it as `url::https://.../Archive/qs/qs_0.27.3.tar.gz`.
      # Emitting an immediate warning here would scare the user mid-install
      # about a failure that's about to be repaired. Truly final failures
      # are surfaced by the post-install `silentlyFailed` warning at the
      # end of pakInstallFiltered, which checks the actual lib state and
      # only fires for packages that did NOT make it in by the end.
      messageVerbose("pakSerialInstall: ", .txtCouldNotBeInstalled, ": ", pkg,
                     if (nzchar(reason)) paste0("; ", reason) else "",
                     verbose = verbose, verboseLevel = 2)
      ## A failed pak::pak() can leave pak's persistent r_session in a
      ## wedged state where every subsequent call returns instantly with
      ## an error -- without this reset, a single early failure cascades
      ## into "could not be installed" for every remaining ref in the
      ## loop (observed on 250+ ref archive-fallback runs where 0 actual
      ## install attempts happened after the first failure).
      pakResetSubprocess()
    }
  }
  invisible(failed)
}

# Install only the packages Require has determined need installing (needInstall == .txtInstall).
# pak is called with exact version pins or any:: to avoid re-resolving deps.
pakInstallFiltered <- function(pkgDT, libPaths, repos, standAlone, verbose,
                                forceUpgrade = FALSE) {
  if (!requireNamespace("pak", quietly = TRUE)) stop("Please install pak")

  # Mirror the same .libPaths() logic as pakDepsToPkgDT so the install subprocess
  # sees the same library set that was used for dependency resolution.
  pakLib    <- tryCatch(dirname(find.package("pak")), error = function(e) NULL)
  basePkgLib <- tail(.libPaths(), 1L)
  origPaths  <- .libPaths()
  if (isTRUE(standAlone)) {
    newPaths <- unique(c(libPaths[1L], basePkgLib))
  } else {
    newPaths <- unique(c(libPaths[1L], origPaths))
  }
  if (!is.null(pakLib) && !pakLib %in% newPaths)
    newPaths <- c(newPaths, pakLib)
  .libPaths(newPaths)
  on.exit(.libPaths(origPaths), add = TRUE)

  toInstall <- pkgDT[needInstall == .txtInstall]
  if (!NROW(toInstall)) return(pkgDT)

  # Deduplicate: if the same Package appears as both a CRAN ref and a GitHub/url:: ref,
  # keep only the non-CRAN ref. pak::pak() would reject the list with a "Conflicts with"
  # error if both "any::SpaDES.tools" (CRAN) and "owner/SpaDES.tools@branch" (GitHub)
  # appear together, because dependencies = FALSE still does conflict detection.
  if (anyDuplicated(toInstall$Package)) {
    toInstall[, isNonCRAN := isGH(packageFullName) | startsWith(packageFullName, "url::")]
    toInstall[, hasNonCRAN := any(isNonCRAN), by = Package]
    # Remove plain CRAN rows when a non-CRAN ref exists for the same package
    toInstall <- toInstall[!(hasNonCRAN == TRUE & isNonCRAN == FALSE)]
    # Among multiple plain-CRAN rows for the same Package (e.g. one row carries
    # the user's "(<= 0.15.8)" upper-bound and a separate row carries a
    # transitive dep's "(>= 0.15.1)" lower-bound -- trimRedundancies keeps both
    # because they are complementary, not redundant), pick the row with the
    # strictest constraint before unique(by = "Package") collapses them.
    # Without this sort, unique() arbitrarily keeps whichever row sorted first
    # in pkgDT -- typically the transitive ">=" row, since dep tree rows are
    # appended after user rows. The user's "<=" pin is then dropped, the
    # downstream gsub("\\(>=...\\)", "") strips the row to a bare name, the
    # any:: prefix turns it into "any::stringfish", and pak silently installs
    # the latest (constraint-violating) version -- symptom seen in the field
    # as `Install("stringfish (<= 0.15.8)")` producing stringfish 0.19.0.
    # Strictness order:  ==  >  <=  >  <  >  >=  >  >  >  none.
    # equalsToAt() and lessThanToAt() (called below) translate ==/<=/< into
    # exact "@version" pins; >= and > get stripped to bare names so any::pkg
    # ends up resolving to latest.  Keeping the strictest row therefore
    # ensures the install is correctly pinned where the user asked for one.
    toInstall[, .versionSpecPrio := match(
      inequality, c("==", "<=", "<", ">=", ">"), nomatch = 6L)]
    setorderv(toInstall, c("Package", ".versionSpecPrio"))
    # If duplicates still remain (e.g., two GitHub branches), keep first
    toInstall <- unique(toInstall, by = "Package")
    toInstall[, c("isNonCRAN", "hasNonCRAN", ".versionSpecPrio") := NULL]
  }

  # Pre-install integrity check: abort the install for any toInstall package
  # whose installed DESCRIPTION names a hard dep that is neither currently
  # installed nor planned in pkgDT. Letting pak proceed in that state can
  # succeed from a cached binary and leave the user with a broken install
  # whose library() call later fails -- worse than a clean abort.
  unresolvedDeps <- list()
  if (NROW(toInstall)) {
    installedNamesPre <- tryCatch(
      rownames(installed.packages(lib.loc = origPaths, noCache = TRUE)),
      error = function(e) character(0))
    plannedNames <- unique(pkgDT$Package)
    for (pkg in toInstall$Package) {
      descPath <- tryCatch(
        system.file("DESCRIPTION", package = pkg, lib.loc = origPaths),
        error = function(e) "")
      if (!nzchar(descPath)) next  # not installed locally; can't pre-check
      hardDeps <- tryCatch(
        unique(extractPkgName(DESCRIPTIONFileDeps(
          descPath, which = c("Depends", "Imports", "LinkingTo")))),
        error = function(e) character(0))
      hardDeps <- setdiff(hardDeps, c(.basePkgs, "R", ""))
      missingDeps <- hardDeps[!(hardDeps %in% installedNamesPre |
                                 hardDeps %in% plannedNames)]
      if (length(missingDeps)) {
        unresolvedDeps[[pkg]] <- missingDeps
      }
    }
  }
  if (length(unresolvedDeps)) {
    affected <- names(unresolvedDeps)
    perPkgLines <- vapply(affected, function(p) {
      paste0("  - ", p, " depends on (not installed, not in plan): ",
             paste(unresolvedDeps[[p]], collapse = ", "))
    }, character(1))
    warning(
      "skipping install: hard dependencies are unresolved:\n",
      paste(perPkgLines, collapse = "\n"),
      call. = FALSE, immediate. = TRUE)
    for (p in affected) {
      wh <- which(pkgDT$Package == p)
      if (length(wh)) {
        set(pkgDT, wh, "installed",         FALSE)
        set(pkgDT, wh, "installedVersionOK", FALSE)
        set(pkgDT, wh, "installResult",     .txtCouldNotBeInstalled)
      }
    }
    toInstall <- toInstall[!Package %in% affected]
    if (!NROW(toInstall)) return(pkgDT)
  }

  # Convert Require's package specs to pak format
  pkgs <- toInstall$packageFullName

  # Strip HEAD flags (Require already decided to install HEAD packages)
  pkgs <- HEADtoNone(pkgs)

  # == version -> @version (exact pin for pak)
  pkgs <- equalsToAt(pkgs)

  # <= version -> find highest satisfying version via pak::pkg_history() -> @version
  pkgs <- lessThanToAt(pkgs)

  # >= version: strip the constraint. Since Require already checked that the installed
  # version does NOT satisfy >=, installing the latest will always satisfy it.
  pkgs <- gsub("[[:space:]]*\\(>=[[:space:]]*[^)]+\\)", "", pkgs)

  # > version: same logic as >=
  pkgs <- gsub("[[:space:]]*\\(>[[:space:]]*[^)]+\\)", "", pkgs)

  # For plain CRAN packages without any version pin or :: prefix, add "any::" so pak
  # resolves installation order from CRAN metadata. Archived packages not on CRAN will
  # fail with "Can't find package called any::pkg", which pakErrorHandling handles by
  # converting to a url:: archive reference on the next retry.
  # Note: isGH() requires all-alpha owner names; also exclude owner/repo refs with
  # hyphens (e.g. "s-u/fastshp") by checking for "/" directly.
  isCRANlike <- !isGH(pkgs) & !grepl("[@:/]", pkgs) & nzchar(pkgs)
  pkgs[isCRANlike] <- paste0("any::", pkgs[isCRANlike])

  # GitHub packages: strip any remaining version spec (already decided to install)
  whGH <- isGH(pkgs)
  if (any(whGH))
    pkgs[whGH] <- trimVersionNumber(pkgs[whGH])

  # Remove empty strings (e.g., if lessThanToAt() removed a package with no valid version)
  hasRemoved <- !nzchar(pkgs)
  if (any(hasRemoved)) {
    toInstall <- toInstall[!hasRemoved]
    pkgs <- pkgs[!hasRemoved]
    pkgDT[toInstall$Package, needInstall := .txtDontInstall, on = "Package"]
  }

  if (!length(pkgs)) return(pkgDT)

  # Install all packages in one call.
  #
  # Require's philosophy: only install/update what the version specs require.
  # upgrade = FALSE ensures pak does NOT upgrade already-installed packages
  # beyond what Require determined is necessary (e.g. tibble 3.2.1 -> 3.3.1
  # when no constraint requires it).
  #
  # CRAN-like refs use dependencies = NA (hard deps only). Earlier this was
  # `dependencies = FALSE`, on the theory that pakDepsToPkgDT had already put
  # the full transitive dep tree into toInstall and pak would topologically
  # order the install. In practice, pak parallelises source builds and with
  # `dependencies = FALSE` does NOT wait for one build's hard deps to finish
  # before starting another's: htmlwidgets would attempt to build while
  # htmltools was still mid-install and fail with "dependencies are not
  # available". `dependencies = NA` lets pak compute the build-time hard-dep
  # graph and order builds correctly. Combined with `upgrade = FALSE`, this
  # still prevents unwanted upgrades of already-installed packages.
  # GitHub/url:: refs use `dependencies = FALSE` so transitive CRAN deps
  # are NOT re-resolved/upgraded -- those go through the CRAN batch.
  # Collect names of packages that pakRetryLoop explicitly warned about so
  # that the post-install update loop can skip them (avoid double-warning).
  warnedDropped <- character(0)
  lastPakErr    <- ""   # last raw pak error string; used by silentlyFailed warning below

  pakRetryLoop <- function(packages, repos, verbose) {
    for (i in seq_len(15)) {
      pkgsIn <- packages
      # Snapshot the captured-messages buffer so we can slice out exactly the
      # lines pak's subprocess emitted during *this* attempt. The outer
      # capturePak(pakRetryLoop(...)) wraps the whole call in a calling
      # handler that pushes pak's message() output into allCapturedMsgs;
      # withCallingHandlers propagates through nested frames, so the
      # subprocess messages land in the same buffer and we can recover them.
      attemptStart <- length(allCapturedMsgs)
      opts <- options(repos = repos)
      # GitHub / url:: refs: must use upgrade=TRUE so pak always fetches the
      # latest commit from the branch rather than "keeping" the currently installed
      # version.  With upgrade=FALSE, pak considers a bare "owner/repo@branch" ref
      # satisfied by whatever version is already in the library -- even if we need
      # a newer one.  Use dependencies=FALSE for GitHub packages: Require's dep
      # resolution already placed all necessary dep updates in the CRAN batch.
      # CRAN-like refs: dependencies=NA so pak orders parallel source builds by
      # the build-time hard-dep graph (see comment block above).
      ghOrUrl <- isGH(packages) | startsWith(packages, "url::")
      # CRAN-batch upgrade: TRUE when caller passed install = "force".
      #
      # Earlier we hard-coded FALSE here to avoid pak's global `upgrade`
      # flag sweeping transitive CRAN deps into the upgrade plan, but
      # that broke a legitimate force scenario: with `pak::pak("any::pkg",
      # upgrade=FALSE)` pak treats an installed older version as
      # satisfying "any::" and skips, so e.g.
      #   Install("fpCompare (>= 0.2.4)", install = "force")
      # against installed fpCompare 0.2.3 became a no-op (caught by
      # test-04other on Windows). Restored to upgrade=TRUE for force.
      # Safe now because pakDepsToPkgDT unconditionally calls
      # pinInstalledForPak() (even under force), so installed transitive
      # deps enter pak's dep tree as exact `pkg@<installedVersion>` pins.
      # pak treats an exact `pkg@X` ref as already-satisfied when X is
      # what's installed -- `upgrade=TRUE` therefore upgrades only the
      # unpinned user packages (those with explicit constraints or no
      # spec); pinned deps stay put.
      cranUp <- isTRUE(forceUpgrade)
      err <- if (any(ghOrUrl) && any(!ghOrUrl)) {
        # Two separate calls when both types are present
        e1 <- try(pakCall(
          pak::pak(packages[ghOrUrl],  lib = libPaths[1], ask = FALSE,
                   dependencies = FALSE, upgrade = TRUE),
          verbose), silent = TRUE)
        e2 <- try(pakCall(
          pak::pak(packages[!ghOrUrl], lib = libPaths[1], ask = FALSE,
                   dependencies = NA, upgrade = cranUp),
          verbose), silent = TRUE)
        # Combine errors: prefer the first error if both fail; if only one
        # fails return that one; if neither fails return non-try-error.
        if (is(e1, "try-error")) e1 else if (is(e2, "try-error")) e2 else e2
      } else {
        up <- any(ghOrUrl) || cranUp  # TRUE -> upgrade=TRUE
        deps <- if (any(ghOrUrl)) FALSE else NA  # GH-only: FALSE; CRAN-only: NA
        try(pakCall(
          pak::pak(packages, lib = libPaths[1], ask = FALSE,
                   dependencies = deps, upgrade = up),
          verbose), silent = TRUE)
      }
      options(opts)
      options(opts)
      if (!is(err, "try-error")) break
      lastPakErr <<- as.character(err)
      # Splice pak's structured failure log (from the error condition's
      # `package_build_error` child) into allCapturedMsgs BEFORE we slice
      # attemptMsgs, so both per-attempt reasoning (pakBuildFailReason) and
      # the global parsers (extractBuildFailures / extractInstallFailures)
      # see the actual R CMD INSTALL output. Without this, pak's one-line
      # "Failed to build X" console summary is all the regex-based parsers
      # ever see -- the underlying ERROR text (e.g. "vignette builder 'knitr'
      # not found", "dependencies '...' are not available", etc.) lives only
      # on the condition object and is otherwise discarded.
      condLog <- pakConditionLog(err)
      if (length(condLog)) allCapturedMsgs <<- c(allCapturedMsgs, condLog)
      # Slice this attempt's captured pak-subprocess messages so error
      # reporters can mine them for the actual root cause (the try() exception
      # is just the generic wrapper "Error : ! error in pak subprocess").
      attemptMsgs <- if (length(allCapturedMsgs) > attemptStart)
        allCapturedMsgs[(attemptStart + 1L):length(allCapturedMsgs)]
      else
        character(0)
      alreadyWarned <- FALSE
      packages <- tryCatch(
        pakErrorHandling(as.character(err), pkgsIn, packages, verbose = verbose),
        error = function(e) {
          # pakErrorHandling crashed while trying to parse pak's error output
          # (typically a regex compilation failure on garbled input).  Surface
          # BOTH the parser error AND the underlying pak failure reason -- the
          # latter is what the user actually needs to debug the build, and
          # without this it gets silently swallowed.
          rawReason <- pakBuildFailReason(as.character(err), attemptMsgs)
          msg <- paste0(.txtCouldNotBeInstalled, "; parser error: ",
                        conditionMessage(e),
                        if (nzchar(rawReason)) paste0("; pak reason: ", rawReason) else "")
          warning(msg, call. = FALSE, immediate. = TRUE)
          # Also dump the full raw pak error to stderr so nothing is lost -- the
          # condensed "reason" lines may miss the line that actually identifies
          # the cause.  Truncate extremely long outputs to keep terminals sane.
          rawFull <- as.character(err)
          if (nchar(rawFull) > 8000L) rawFull <- paste0(substr(rawFull, 1L, 8000L), "\n...[truncated]")
          message("--- pak raw error (full) ---\n", rawFull, "\n--- end pak raw error ---")
          alreadyWarned <<- TRUE
          character(0)
        }
      )
      # NOT a warning here -- emit at verboseLevel = 2 only.
      # pakRetryLoop is one layer in a multi-layer retry pipeline: a failure
      # this iteration may still be repaired by a subsequent iter (different
      # subprocess state, different ref form), by the identify-and-defer
      # serial fallback in pakInstallFiltered, or by the CRAN-archive
      # fallback. Emitting an inline `Warning: could not be installed: ...`
      # mid-retry routinely scares the user about a failure that is then
      # repaired silently -- most visibly when an exact-pin ref triggers
      # pak's `if (!version_satisfies(...))` resolver bug on the first
      # attempt but installs cleanly on the deferred retry. The truly final
      # outcome is reported by pakInstallFiltered's `silentlyFailed`
      # warning at the end (which inspects the actual lib state) and by
      # the install summary table -- both of which only fire for packages
      # that did NOT make it in by the end of all retries.
      # We update `alreadyWarned` (a local) so the post-loop fallback at
      # line ~2095 doesn't fire a duplicate debug message for this same
      # iteration. We do NOT update `warnedDropped` -- that suppresses the
      # post-install `silentlyFailed` warning, which is the user-visible
      # end-state report. Pre-fix, in-loop warnings updated warnedDropped
      # to dedupe with silentlyFailed; now that the in-loop emission is a
      # debug-only message (not a warning), we want silentlyFailed to be
      # the authoritative source of user-visible failure warnings, even
      # for packages that pakErrorHandling dropped earlier.
      if (!alreadyWarned) {
        droppedPkgNames <- setdiff(extractPkgName(pkgsIn), extractPkgName(packages))
        if (length(droppedPkgNames)) {
          reason <- pakBuildFailReason(as.character(err), attemptMsgs)
          msg <- paste0("pakRetryLoop: ", .txtCouldNotBeInstalled, ": ",
                        paste(droppedPkgNames, collapse = ", "),
                        if (nzchar(reason)) paste0("; ", reason) else "")
          messageVerbose(msg, verbose = verbose, verboseLevel = 2)
          alreadyWarned <- TRUE
        } else if (identical(packages, pkgsIn)) {
          # pakErrorHandling did not recognise the error pattern and left the
          # package list unchanged -- there is no point retrying with the same
          # packages. Mark all remaining packages as failed for this loop;
          # the outer iter will fall through to serial / archive fallback.
          reason <- pakBuildFailReason(as.character(err), attemptMsgs)
          failedNames <- extractPkgName(packages)
          msg <- paste0("pakRetryLoop: ", .txtCouldNotBeInstalled, ": ",
                        paste(failedNames, collapse = ", "),
                        if (nzchar(reason)) paste0("; ", reason) else "")
          messageVerbose(msg, verbose = verbose, verboseLevel = 2)
          alreadyWarned <- TRUE
          packages <- character(0)
        }
      }
      if (!length(packages)) {
        if (!alreadyWarned) {
          # Include the actual build/install failure reason for diagnostics.
          # Same rationale as the per-iter messageVerbose calls above:
          # pakRetryLoop is mid-pipeline, so a failure here may still be
          # repaired by serial / archive fallbacks. The post-install
          # `silentlyFailed` warning is the authoritative end-state report.
          reason <- pakBuildFailReason(as.character(err), attemptMsgs)
          if (nzchar(reason)) {
            messageVerbose("pakRetryLoop: ", .txtCouldNotBeInstalled, ": ", reason,
                           verbose = verbose, verboseLevel = 2)
          } else {
            messageVerbose("pakRetryLoop: ", .txtCouldNotBeInstalled,
                           verbose = verbose, verboseLevel = 2)
          }
        }
        break
      }
    }
    invisible(NULL)
  }

  # Snapshot pre-install versions IN libPaths[1] (the install target) before pak
  # runs so we can detect build failures: if a package's version in libPaths[1]
  # is unchanged after the install attempt it means the install failed (build
  # error, cancelled batch, etc.) rather than pak choosing an older version
  # that doesn't satisfy the constraint. pkgDT$Version reflects whatever was
  # found across .libPaths(), which can be a different copy in another library --
  # using that as preVer would suppress the version-mismatch warning when
  # libPaths[1] was empty pre-call but a different libPath had a copy.
  preInstallVers <- {
    ipPre <- tryCatch(
      as.data.frame(installed.packages(lib.loc = libPaths[1L], noCache = TRUE),
                    stringsAsFactors = FALSE),
      error = function(e) data.frame(Package = character(0), Version = character(0)))
    pv <- setNames(rep(NA_character_, length(toInstall$Package)), toInstall$Package)
    if (NROW(ipPre)) {
      have <- intersect(toInstall$Package, ipPre$Package)
      for (.p in have) pv[.p] <- ipPre$Version[ipPre$Package == .p][1L]
    }
    pv
  }

  # ---------------------------------------------------------------------------
  # Install: iterative identify-and-defer
  #
  # Iterates a parallel pakRetryLoop pass while peeling off "culprit" packages
  # -- those that pak's per-package "Failed to build <pkg>" lines named. Each
  # iteration:
  #   1. Run pakRetryLoop on the current pass-list (parallel install) while
  #      capturing pak's messages.
  #   2. Check what's still missing in the project lib. If empty -> done.
  #   3. Parse captured output for "Failed to build X" -> culprits.
  #   4. Add culprits to a pending list, drop them from the pass-list, loop.
  #
  # Each iteration's pass-list is strictly smaller (or terminates) and contains
  # only the previously-missing cascade casualties of the prior iteration. This
  # handles nested cascades -- when pass 2 itself has a different culprit than
  # pass 1, that culprit is identified and deferred too.
  #
  # Final phase: install accumulated culprits one-by-one via pakSerialInstall.
  # By this point all their CRAN/build-time deps have been installed by the
  # iterations above, so R CMD INSTALL's pre-flight check passes.
  #
  # Behavior is selectable via options(Require.pakInstallStrategy):
  #   "identify-and-defer" (default)
  #   "original"           -- single parallel pass, legacy behavior
  # ---------------------------------------------------------------------------
  strategy <- getOption("Require.pakInstallStrategy", "identify-and-defer")
  if (!strategy %in% c("identify-and-defer", "original")) {
    warning("Unknown Require.pakInstallStrategy '", strategy,
            "'; falling back to 'identify-and-defer'", call. = FALSE)
    strategy <- "identify-and-defer"
  }
  installTimings <- list(strategy = strategy, start = Sys.time())
  # Accumulate pak's messages across every install pass so the final install
  # report can attribute reasons to specific packages (e.g. "PSPclean --
  # missing build-time deps: bit64, dplyr, ..."). Filled by withCallingHandlers
  # wrappers around each pakRetryLoop / pakSerialInstall call below.
  allCapturedMsgs <- character(0)
  ## Only capture when we're suppressing console output (verbose < 1). At
  ## verbose >= 1 a no-op calling handler in cli's condition chain breaks
  ## cli's dynamic redraw -- every progress tick spews as a fresh line. The
  ## downstream parser (extractBuildFailures, pakBuildFailReason) gets less
  ## detail at verbose >= 1, but the user already saw failures on console.
  capturePak <- function(expr) {
    if (verbose < 1) {
      withCallingHandlers(
        expr,
        message = function(m) {
          allCapturedMsgs <<- c(allCapturedMsgs, conditionMessage(m))
        })
    } else {
      expr
    }
  }

  # See pakRefToBareName() -- strips "any::" / "owner/" / "@version" so the
  # resulting names line up with rownames(installed.packages()). The post-loop
  # install-summary check, archive-fallback decision, and iter-loop's
  # "still-missing" comparison all depend on this normalization; without it
  # every version-pinned ref ("qs@0.27.3") is misclassified as missing
  # because installed.packages() returns the bare name ("qs").
  pkgNamesAll <- pakRefToBareName(pkgs)
  if (identical(strategy, "original")) {
    capturePak(pakRetryLoop(pkgs, repos, verbose))
  } else {
    # Iterative identify-and-defer.
    passList <- pkgs
    deferred <- character(0)  # culprit refs (named with their full pak ref)
    maxIter  <- 8L
    for (iter in seq_len(maxIter)) {
      # Force a fresh pak subprocess for every iteration after the first.
      # pak holds a persistent r_session that, after a large failed install
      # plan, can wedge into a state where every subsequent call emits
      # "Error : ! error in pak subprocess" without naming a build culprit
      # (so identify-and-defer has nothing parseable to defer and stalls).
      # Restarting the subprocess gives the next iteration clean state.
      if (iter > 1L) pakResetSubprocess()
      iterMsgsStart <- length(allCapturedMsgs) + 1L
      capturePak(pakRetryLoop(passList, repos, verbose))
      capturedMsgs <- allCapturedMsgs[iterMsgsStart:length(allCapturedMsgs)]

      ## Terminate early if pak reports missing system packages. Retrying
      ## won't help: pak's dep resolver re-includes the failing pkg in
      ## every plan that contains any of its dependents, so the loop just
      ## ping-pongs on the same culprit. Emit a clear actionable error
      ## (with sysreq -> pkg mapping) instead of spinning forever.
      sysreqMissing <- extractMissingSysreqs(capturedMsgs)
      if (length(sysreqMissing)) {
        affectedPkgs <- unique(names(sysreqMissing))
        # Group sysreqs by package for a readable summary
        byPkg <- split(unname(sysreqMissing), names(sysreqMissing))
        summary <- vapply(names(byPkg), function(p) {
          paste0(p, " needs: ", paste(unique(byPkg[[p]]), collapse = ", "))
        }, character(1))
        warning(.txtCouldNotBeInstalled, ": ",
                paste(affectedPkgs, collapse = ", "),
                "; missing system packages -- install them and re-run.\n  ",
                paste(summary, collapse = "\n  "),
                call. = FALSE)
        break
      }

      # noCache = TRUE: pak just installed these packages in a subprocess; the
      # parent R session's installed.packages() cache is still pre-install.
      # Without this, even successfully-installed packages look "still missing"
      # and the loop falls into the no-parseable-culprits serial fallback for
      # no reason, doubling install time.
      instNow <- tryCatch(rownames(installed.packages(lib.loc = libPaths[1], noCache = TRUE)),
                          error = function(e) character(0))
      # Same bare-name reduction as pkgNamesAll above. Without stripping
      # "any::" / "owner/" / "@version", instNow's bare names ("cli", "qs")
      # never match passNames' decorated form ("any::cli", "qs@0.27.3") and
      # every iteration's "still missing" check returns the full pass-list --
      # which then falls into the no-parseable-culprits serial fallback,
      # doubling install time and emitting bogus "still missing after iter 1"
      # messages for packages that pak in fact already installed.
      passNames <- pakRefToBareName(passList)
      missingNamesIter <- passNames[!passNames %in% instNow]
      if (!length(missingNamesIter)) {
        if (iter > 1L) {
          messageVerbose(
            "identify-and-defer: cascade casualties resolved after ",
            iter - 1L, " deferral pass(es); ", length(deferred),
            " culprit(s) pending serial install",
            verbose = verbose, verboseLevel = 1)
        }
        break
      }

      culpritsIter <- intersect(extractBuildFailures(capturedMsgs),
                                missingNamesIter)
      if (!length(culpritsIter)) {
        # No new culprits parseable from pak output. Common cause: pak's
        # subprocess crashes during dep resolution on large cascade-casualty
        # batches (no per-package "Failed to build X" line, just a generic
        # "Error : ! error in pak subprocess"). Fall back to serial install:
        # each pak::pak(single_ref) call has a tiny dep graph that resolves
        # fine, and a failure on one ref no longer abort the rest.
        pkgsMissingFallback <- passList[match(missingNamesIter, passNames)]
        pkgsMissingFallback <- pkgsMissingFallback[!is.na(pkgsMissingFallback)]
        messageVerbose(
          "identify-and-defer: ", length(missingNamesIter),
          " ref(s) still missing after iter ", iter,
          ", no parseable culprits; falling back to serial install",
          verbose = verbose, verboseLevel = 1)
        pakResetSubprocess()
        capturePak(pakSerialInstall(pkgsMissingFallback, libPaths[1], repos, verbose))
        break
      }

      pkgsCulpritIter <- passList[match(culpritsIter, passNames)]
      pkgsCulpritIter <- pkgsCulpritIter[!is.na(pkgsCulpritIter)]
      deferred <- c(deferred, pkgsCulpritIter)

      # Next iteration: previously-missing minus the culprits.
      pkgsMissingIter <- passList[match(missingNamesIter, passNames)]
      pkgsMissingIter <- pkgsMissingIter[!is.na(pkgsMissingIter)]
      newPassList <- pkgsMissingIter[!extractPkgName(pkgsMissingIter) %in% culpritsIter]

      messageVerbose(
        "identify-and-defer iter ", iter, ": ", length(culpritsIter),
        " culprit(s) deferred (",
        paste(utils::head(culpritsIter, 5L), collapse = ", "),
        if (length(culpritsIter) > 5L) ", ..." else "",
        "); ", length(newPassList),
        " cascade casualt", if (length(newPassList) == 1L) "y" else "ies",
        " queued for next pass",
        verbose = verbose, verboseLevel = 1)

      if (!length(newPassList) || identical(sort(newPassList), sort(passList))) {
        # No-progress guard.
        break
      }
      passList <- newPassList
    }

    # Final phase: install the accumulated culprits serially. Reset pak's
    # subprocess first -- the iteration loop may have left it in a wedged
    # state from the failed plan(s), and each serial install benefits from
    # a clean subprocess (see pakResetSubprocess() comment).
    if (length(deferred)) {
      messageVerbose(
        "identify-and-defer: installing ", length(deferred),
        " deferred culprit(s) one at a time",
        verbose = verbose, verboseLevel = 1)
      pakResetSubprocess()
      capturePak(pakSerialInstall(deferred, libPaths[1], repos, verbose))
    }
  }

  installTimings$end     <- Sys.time()
  installTimings$elapsed <- as.numeric(difftime(installTimings$end,
                                                installTimings$start,
                                                units = "secs"))
  if (verbose >= 1) {
    messageVerbose("pak install strategy '", strategy, "' took ",
                   round(installTimings$elapsed, 1L), "s for ",
                   length(pkgs), " requested ref(s)",
                   verbose = verbose, verboseLevel = 1)
  }
  assign(".lastPakInstallTimings", installTimings, envir = pakEnv())

  # ---------------------------------------------------------------------------
  # End-of-install summary: which packages actually didn't make it into the
  # project lib, and (where parseable from pak's captured output) why.
  # Stored in pakEnv() as `.lastInstallFailures` for programmatic access; a
  # human-readable line-per-package report is printed when verbose >= 0.
  #
  # The canonical `installFailures` parse happens AFTER the archive fallback
  # below, so that any per-package "Failed to build X" line emitted during the
  # archive pass (e.g. an archived CRAN package whose source build fails to
  # compile) is included rather than fall through to the catch-all
  # "still-missing" branch in reportInstallFailures.
  #
  # We do an early lightweight parse purely to identify which still-missing
  # refs have NO parseable reason yet -- those are the only ones worth retrying
  # via the CRAN archive (refs that pak already named as build failures won't
  # build any better from an archive URL).
  # ---------------------------------------------------------------------------
  emptyFailuresDT <- data.table(package = character(0),
                                reason_type = character(0),
                                reason_brief = character(0),
                                reason_detail = character(0))
  preArchiveFailures <- tryCatch(
    extractInstallFailures(allCapturedMsgs),
    error = function(e) emptyFailuresDT)
  # standAlone = TRUE: only libPaths[1] counts as "installed" -- the user
  # explicitly asked for an isolated lib, so finding a copy elsewhere on
  # .libPaths() does NOT satisfy the request. Without this guard, archive
  # fallback was skipped for archived-from-CRAN packages whose only on-disk
  # copy lived in the user library (e.g. pryr 0.1.6 after CRAN archive on
  # 2026-01-30 -- pak's binary URL 404s and finalInstalled saw the user-lib
  # copy, so finalMissing was empty and archive fallback never fired).
  # standAlone = FALSE: with upgrade = FALSE pak legitimately skips packages
  # visible elsewhere on .libPaths(), so we honor that as installed.
  checkLibs <- if (isTRUE(standAlone)) libPaths[1L] else .libPaths()
  finalInstalled <- tryCatch(rownames(installed.packages(lib.loc = checkLibs, noCache = TRUE)),
                             error = function(e) character(0))
  finalMissing   <- pkgNamesAll[!pkgNamesAll %in% finalInstalled]

  # ---------------------------------------------------------------------------
  # Archive fallback: for packages that ended up still-missing AND have no
  # parseable build-failure reason, try installing them from the CRAN archive.
  # The typical case is packages that were archived from CRAN (e.g.
  # disk.frame, pryr) where pak's "any::pkg" ref can't be resolved by the
  # current CRAN mirror, and pak emits a generic subprocess error rather
  # than a per-package "Failed to build" line. pakGetArchive() turns the
  # bare package name into a `url::https://.../Archive/<pkg>/<pkg>_<ver>.tar.gz`
  # ref that pak can install directly.
  #
  # All archive refs are passed to pak together (single batch call) so that
  # pak's resolver can satisfy cross-archive deps. e.g., disk.frame depends
  # on pryr (>= 0.1.4); since pryr is itself archived, pak couldn't find it
  # via "any::pryr" -- it has to see pryr's archive URL in the same plan.
  # If the batch call fails, we fall back to per-ref serial install (which
  # at least installs the archives that don't have such cross-deps).
  # ---------------------------------------------------------------------------
  if (length(finalMissing)) {
    explained <- preArchiveFailures$package
    archiveCandidates <- setdiff(finalMissing, explained)
    if (length(archiveCandidates)) {
      messageVerbose(
        "archive fallback: trying CRAN archive for ", length(archiveCandidates),
        " still-missing ref(s): ",
        paste(utils::head(archiveCandidates, 5L), collapse = ", "),
        if (length(archiveCandidates) > 5L) ", ..." else "",
        verbose = verbose, verboseLevel = 1)
      pakResetSubprocess()
      # Map bare names back to their version-pinned refs in the install set
      # so pakGetArchive can build an Archive URL for the EXACT requested
      # version (snapshot installs pin specific older versions; without this
      # mapping pakGetArchive would fall back to the latest archive entry).
      verRefMap <- setNames(pkgs, pakRefToBareName(pkgs))
      # Collect archive URLs for every candidate first, then attempt a
      # single batch install so pak's resolver can satisfy cross-archive
      # deps (e.g. disk.frame -> pryr where both are archived).
      archiveRefs <- character(0)
      for (pkg in archiveCandidates) {
        origRef <- verRefMap[[pkg]]
        if (is.null(origRef) || !nzchar(origRef)) origRef <- pkg
        ref <- tryCatch(pakGetArchive(origRef, packages = origRef, whRm = 1L),
                        error = function(e) character(0),
                        warning = function(w) character(0))
        # Only accept fully-formed CRAN-archive URL refs. Anything else
        # (unchanged pkg name, bare "url::", non-http path) would derail
        # the pak::pak() batch with an opaque "All URLs failed" error.
        if (length(ref) && !identical(ref, pkg) &&
            all(grepl("^url::https?://.+", ref))) {
          archiveRefs <- c(archiveRefs, ref)
        }
      }
      if (length(archiveRefs)) {
        opts <- options(repos = repos)
        on.exit(options(opts), add = TRUE)
        # Single batch call: archive URLs only. dependencies = NA so pak
        # resolves transitive CRAN deps; upgrade = FALSE so it doesn't
        # re-install pkgs already in lib.
        batchErr <- try(capturePak(pakCall(
          pak::pak(archiveRefs, lib = libPaths[1], ask = FALSE,
                   dependencies = NA, upgrade = FALSE),
          verbose)), silent = TRUE)
        options(opts)
        # If the batch failed, try per-ref serial as a final fallback --
        # archives without cross-archive deps will still install.
        if (is(batchErr, "try-error")) {
          messageVerbose(
            "archive fallback: batch call failed; retrying serially",
            verbose = verbose, verboseLevel = 1)
          pakResetSubprocess()
          capturePak(pakSerialInstall(archiveRefs, libPaths[1], repos, verbose))
        }
      }
      # Recompute final-missing after the archive pass.
      finalInstalled <- tryCatch(
        rownames(installed.packages(lib.loc = checkLibs, noCache = TRUE)),
        error = function(e) character(0))
      finalMissing <- pkgNamesAll[!pkgNamesAll %in% finalInstalled]
    }
  }

  # Canonical failure parse: re-read allCapturedMsgs *after* every install
  # pass (iterative + serial-deferred + archive fallback) so per-package
  # "Failed to build X" lines emitted by the archive pass are captured.
  # Then drop any rows for packages that did end up installed -- a package
  # that failed in iter 1 but built successfully in the deferred-culprit
  # serial pass (e.g. reproducible@HEAD whose build-time deps weren't yet
  # in lib during iter 1) would otherwise be reported as a build-error in
  # the install summary even though it's present in the lib.
  installFailures <- tryCatch(
    extractInstallFailures(allCapturedMsgs),
    error = function(e) emptyFailuresDT)
  if (NROW(installFailures))
    installFailures <- installFailures[package %in% finalMissing]

  installFailures <- reportInstallFailures(installFailures, finalMissing,
                                           verbose = verbose)
  assign(".lastInstallFailures", installFailures, envir = pakEnv())

  # Update pkgDT with installation results.
  # Use wh[1L] for scalar reads (versionSpec/inequality) but the full wh vector
  # for set() calls so that any duplicate Package rows are all updated consistently.
  nowInstalled    <- as.data.table(as.data.frame(installed.packages(lib.loc = libPaths[1], noCache = TRUE),
                                               stringsAsFactors = FALSE))
  # If installed.packages() returned an empty matrix without the expected
  # columns (can happen when libPaths[1] doesn't exist yet or the install
  # attempt failed before writing anything), the data.table[i, j] expressions
  # below would error with "object 'Package' not found", masking the actual
  # build failure.  Coerce to a known-empty schema so the loop falls through
  # cleanly and the upstream pak error remains the visible cause.
  if (!"Package" %in% names(nowInstalled)) {
    nowInstalled <- data.table(Package = character(0), Version = character(0),
                               LibPath = character(0))
  }
  nowInstalledAll <- NULL  # computed lazily in the else-branch below

  for (pkg in toInstall$Package) {
    wh <- which(pkgDT$Package == pkg)
    if (!length(wh)) next
    nowRow <- nowInstalled[Package == pkg]
    if (NROW(nowRow)) {
      installedVer <- nowRow$Version[1]
      # Check if installed version actually satisfies the original requirement.
      vSpec <- pkgDT$versionSpec[wh[1L]]
      ineq  <- pkgDT$inequality[wh[1L]]
      if (!is.na(vSpec) && nzchar(vSpec) && !is.na(ineq) && nzchar(ineq)) {
        satisfies <- compareVersion2(installedVer, versionSpec = vSpec, inequality = ineq)
        # If the raw DESCRIPTION constraint isn't met, check whether pak's own
        # globally-resolved version IS met.  pak's resolution is authoritative: if pak
        # decided that installing version X satisfies the full dep tree, and X is what
        # was installed, the constraint is effectively satisfied even if some intermediate
        # package's raw DESCRIPTION says something stricter.
        # The version map is stored in pakEnv() by pakDepsToPkgDT; a pkgDT column
        # would be dropped by the transforms Require2.R runs after pakDepsToPkgDT returns.
        pakRes <- NA_character_
        if (!isTRUE(satisfies)) {
          pakVerMap <- get0("pakResolvedVersionMap", envir = pakEnv(), inherits = FALSE)
          if (!is.null(pakVerMap)) {
            cand <- pakVerMap[pkg]
            if (!is.na(cand) && nzchar(cand)) {
              pakRes <- unname(cand)
              satisfies <- isTRUE(compareVersion2(pakRes, versionSpec = vSpec, inequality = ineq))
            }
          }
        }
        if (!isTRUE(satisfies)) {
          # We are inside `if (NROW(nowRow))`, i.e. pak HAS something installed
          # for `pkg` post-call -- but `installedVer` doesn't satisfy the user's
          # constraint. Three scenarios warrant the "Please change required
          # version" warning; only "build failure leaving the pre-existing
          # version untouched" suppresses it.
          preVer <- preInstallVers[pkg]
          versionChanged <- !is.na(preVer) && !isTRUE(identical(preVer, installedVer)) &&
                            !isTRUE(compareVersion(preVer, installedVer) == 0L)
          firstTimeInsufficient <- is.na(preVer)
          # pak intentionally chose installedVer (its resolved version matches
          # what's on disk): the install was a success, the version just doesn't
          # meet the user's constraint. This is distinct from a build failure
          # (where pakRes would be a different/newer version pak failed to put
          # on disk) and warrants the "please change required version" guidance
          # even when preVer == installedVer (e.g. on a re-Require() call).
          pakChoseInstalled <- !is.na(pakRes) && identical(pakRes, installedVer)
          if (versionChanged || firstTimeInsufficient || pakChoseInstalled)
            warning(msgPleaseChangeRqdVersion(pkg, ineq = ">=", newVersion = installedVer), call. = FALSE)
          # Always add to warnedDropped: either we already warned above (versionChanged),
          # or pak ran and chose not to update this package, meaning Require's over-strict
          # transitive constraint is the discrepancy -- not a real install failure.
          warnedDropped <- c(warnedDropped, pkg)
          set(pkgDT, wh, "installed",     FALSE)
          set(pkgDT, wh, "Version",       installedVer)
          set(pkgDT, wh, "LibPath",       nowRow$LibPath[1])
          set(pkgDT, wh, "installResult", .txtCouldNotBeInstalled)
          next
        }
      }
      set(pkgDT, wh, "installed",         TRUE)
      set(pkgDT, wh, "installedVersionOK", TRUE)
      set(pkgDT, wh, "Version",           installedVer)
      set(pkgDT, wh, "LibPath",           nowRow$LibPath[1])
      set(pkgDT, wh, "installResult",     "OK")
    } else {
      # Package not in libPaths[1] -- may already be installed (and satisfying)
      # in another lib path (pak skips packages that are already up-to-date).
      if (is.null(nowInstalledAll)) {
        # NB: must be `<-`, not `<<-`. This block runs in pakInstallFiltered's
        # own frame (not a nested function), so `<<-` would assign to global
        # rather than updating the local `nowInstalledAll` declared above --
        # leaving the local NULL and producing "object 'Package' not found"
        # when the next line indexes it.
        nowInstalledAll <- as.data.table(as.data.frame(installed.packages(lib.loc = .libPaths(), noCache = TRUE),
                                                       stringsAsFactors = FALSE))
        # Same guard as nowInstalled above: when installed.packages() returns
        # an empty matrix the data.table[Package == pkg] expression errors with
        # "object 'Package' not found".
        if (!"Package" %in% names(nowInstalledAll)) {
          nowInstalledAll <- data.table(Package = character(0),
                                        Version = character(0),
                                        LibPath = character(0))
        }
      }
      elseRow <- nowInstalledAll[Package == pkg]
      if (NROW(elseRow)) {
        elseVer <- elseRow$Version[1]
        vSpec   <- pkgDT$versionSpec[wh[1L]]
        ineq    <- pkgDT$inequality[wh[1L]]
        elseOK  <- if (!is.na(vSpec) && nzchar(vSpec) && !is.na(ineq) && nzchar(ineq))
                     isTRUE(compareVersion2(elseVer, versionSpec = vSpec, inequality = ineq))
                   else TRUE
        if (elseOK) {
          set(pkgDT, wh, "installed",          TRUE)
          set(pkgDT, wh, "installedVersionOK", TRUE)
          set(pkgDT, wh, "Version",            elseVer)
          set(pkgDT, wh, "LibPath",            elseRow$LibPath[1])
          set(pkgDT, wh, "installResult",      "OK")
          next
        }
      }
      set(pkgDT, wh, "installed",      FALSE)
      set(pkgDT, wh, "installResult",  .txtCouldNotBeInstalled)
    }
  }

  # Warn about packages that were in toInstall but still not installed after all
  # retries -- and that pakRetryLoop did not already warn about.  The typical case
  # is a cascade failure: package X fails to build -> package Y (which Imports X)
  # also fails to install because X isn't present when pak tries to package Y.
  # Without this warning the user sees no output from Require at all, just a
  # mysterious runtime error when they later try to use Y.
  silentlyFailed <- toInstall$Package[
    !toInstall$Package %in% warnedDropped &
    vapply(toInstall$Package, function(pkg) {
      wh <- which(pkgDT$Package == pkg)
      length(wh) > 0 &&
        isTRUE(pkgDT$installResult[wh[1L]] == .txtCouldNotBeInstalled)
    }, logical(1))
  ]
  if (length(silentlyFailed)) {
    reason <- pakBuildFailReason(lastPakErr)
    failedFullPaths <- toInstall$packageFullName[toInstall$Package %in% silentlyFailed]
    ghHint <- if (any(grepl("/", failedFullPaths, fixed = TRUE)))
      paste0("\n", .txtDidYouSpell) else ""
    warning(.txtCouldNotBeInstalled, ": ",
            paste(silentlyFailed, collapse = ", "),
            if (nzchar(reason)) paste0("; ", reason) else "",
            ghHint,
            call. = FALSE, immediate. = TRUE)
  }

  pkgDT
}
