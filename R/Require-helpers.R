utils::globalVariables(c(
  "..colsKeep", "..colsToNAfill", "..removeCols", ".I", ".N",
  "Archs", "AvailableVersion", "compareVersionAvail", "correctVersion",
  "dayAfterPutOnCRAN", "DepVersion", "destFile", "dup", "filepath",
  "fullGit", "github", "groupCRANtogether", "groupCRANtogetherChange",
  "groupCRANtogetherDif", "hasHEAD", "hasVersionSpec", "i.neededFiles",
  "inequality", "installFrom", "installFromFac", "installOrder",
  "installResult", "isGitPkg", "keep", "keep2", "lastRow", "localFileName",
  "localType", "maxVers", "mtime", "N", "Names", "neededFiles",
  "needLaterDate", "nextRow", "Package", "packageFullName", "repoLocation",
  "RepoWBranch", "tmpOrder", "type", "version", "VersionFromPV", "violations"
))

#' Parse a github package specification
#'
#' This converts a specification like `PredictiveEcology/Require@development`
#' into separate columns, "Account", "Repo", "Branch", "GitSubFolder" (if there is one)
#'
#' @details
#' `parseGitHub` turns the single character string representation into 3 or 4:
#' `Account`, `Repo`, `Branch`, `SubFolder`.
#'
#' @return
#' `parseGitHub` returns a `data.table` with added columns.
#'
#' @export
#' @rdname GitHubTools
#' @param pkgDT A pkgDT data.table.
#' @inheritParams Require
parseGitHub <- function(pkgDT, verbose = getOption("Require.verbose")) {
  pkgDT <- toPkgDT(pkgDT)

  if (is.null(pkgDT$githubPkgName)) {
    set(pkgDT, NULL, "githubPkgName", extractPkgGitHub(pkgDT$packageFullName))
    isGH <- !is.na(pkgDT$githubPkgName)
    if (is.null(pkgDT$repoLocation)) {
      set(pkgDT, which(isGH), "repoLocation", .txtGitHub)
      set(pkgDT, which(!isGH), "repoLocation", "CRAN")
    }

    if (any(pkgDT$repoLocation == .txtGitHub)) {
      isGH <- pkgDT$repoLocation == .txtGitHub
      isGitHub <- which(isGH)
      set(pkgDT, isGitHub, "fullGit", trimVersionNumber(pkgDT$packageFullName[isGitHub]))
      set(pkgDT, isGitHub, "fullGit", masterMainToHead(pkgDT$fullGit[isGitHub]))
      set(pkgDT, isGitHub, "Account", gsub("^(.*)/.*$", "\\1", pkgDT$fullGit[isGitHub]))
      set(pkgDT, isGitHub, "RepoWBranch", gsub("^(.*)/(.*)@*.*$", "\\2", pkgDT$fullGit[isGitHub]))
      set(pkgDT, isGitHub, "hasSubFolder", grepl("/", pkgDT$Account[isGitHub]))
      if (any(pkgDT$hasSubFolder, na.rm = TRUE)) { # fix both Account and RepoWBranch
        hasSubFold <- which(pkgDT$hasSubFolder)
        subFoldIndices <- seq_len(NROW(pkgDT[hasSubFold]))
        set(pkgDT, hasSubFold, "Account", gsub("^(.*)/(.*)$", "\\1", pkgDT$Account[hasSubFold]))
        pkgDT[hasSubFolder %in% TRUE,
              RepoWBranch := gsub(paste0("^", Account, "/"), "", fullGit),
              by = seq(sum(hasSubFolder, na.rm = TRUE))
        ]
        pkgDT[hasSubFolder %in% TRUE,
              GitSubFolder := strsplit(RepoWBranch, split = "/|@")[[1]][2],
              by = seq(sum(hasSubFolder, na.rm = TRUE))
        ]
        pkgDT[hasSubFolder %in% TRUE,
              RepoWBranch := gsub(paste0("/", GitSubFolder), "", RepoWBranch),
              by = seq(sum(hasSubFolder, na.rm = TRUE))
        ]
      }
      set(pkgDT, isGitHub, "Repo", gsub("^(.*)@(.*)$", "\\1", pkgDT$RepoWBranch[isGitHub]))
      # set(pkgDT, isGitHub, "Branch", "HEAD")
      set(pkgDT, isGitHub, "Branch", "main")
      wh1 <- which(isGH & grepl("@", pkgDT$RepoWBranch))
      set(pkgDT, wh1, "Branch", gsub("^.*@(.*)$", "\\1", pkgDT$RepoWBranch[wh1]))
      set(pkgDT, NULL, c("RepoWBranch", "fullGit"), NULL)
    }
  }
  pkgDT[]
}

#' @rdname DESCRIPTION-helpers
#' @param file A file path to a DESCRIPTION file
DESCRIPTIONFileVersionV <- function(file, purge = getOption("Require.purge", FALSE)) {
  if (is.null(envPkgDepDESCFile())) purge <- dealWithCache(purge, checkAge = FALSE)
  out <- lapply(file, function(f) {
    out <- if (!is.null(envPkgDepDESCFile())) {
      if (purge && length(f) == 1) suppressWarnings(rm(f, envir = envPkgDepDESCFile()))
      if (length(f) == 1) {
        get0(f, envir = envPkgDepDESCFile())
      } else {
        f
      }
    } else {
      NULL
    }
    if (length(f) == 1) {
      lines <- try(readLines(f), silent = TRUE)
      if (is(lines, "try-error")) {
        warning(lines)
        lines <- character()
      }
    } else {
      lines <- f
    }
    suppressWarnings({
      vers_line <- lines[grep("^Version: *", lines)]
    })
    out <- gsub("Version: ", "", vers_line)
    if (length(out) == 0) out <- NA
    if (length(f) == 1) {
      assign(f, out, envir = envPkgDepDESCFile())
    }
    out
  })
  unlist(out)
}

#' @rdname DESCRIPTION-helpers
#' @param file A file path to a `DESCRIPTION` file
#' @param other Any other keyword in a `DESCRIPTION` file that precedes a ":".
#'   The rest of the line will be retrieved.
DESCRIPTIONFileOtherV <- function(file, other = "RemoteSha") {
  out <- lapply(file, function(f) {
    if (length(f) == 1) {
      lines <- try(readLines(f), silent = TRUE)
      if (is(lines, "try-error")) {
        warning(lines)
        lines <- character()
      }
    } else {
      lines <- f
    }
    suppressWarnings({
      vers_line <- lines[grep(paste0("^", other, ": *"), lines)]
    })
    out <- gsub(paste0(other, ": "), "", vers_line)
    if (length(out) == 0) out <- NA
    if (length(out) > 1) out <- tail(out, 1)
    out
  })
  unlist(out)
}

.compareVersionV <- Vectorize(compareVersion)
.evalV <- Vectorize(eval, vectorize.args = "expr")
.parseV <- Vectorize(parse, vectorize.args = "text")

#' GitHub package tools
#'
#' A series of helpers to access and deal with GitHub packages
#'
#' @details
#' `dlGitHubDESCRIPTION` retrieves the DESCRIPTION file from GitHub.com
#'
#' @rdname DESCRIPTION-helpers
#' @export
#' @param pkg A character string with a GitHub package specification (c.f. remotes)
#' @inheritParams pkgDep
#' @inheritParams Require
dlGitHubDESCRIPTION <- function(pkg, purge = getOption("Require.purge", FALSE),
                                verbose = getOption("Require.verbose")) {
  dlGitHubFile(pkg, "DESCRIPTION", purge = purge, verbose = verbose)
}

#' @inheritParams Require
dlGitHubNamespace <- function(pkg, purge = getOption("Require.purge", FALSE),
                              verbose = getOption("Require.verbose")) {
  dlGitHubFile(pkg, "NAMESPACE", purge = purge, verbose = verbose)
}

pkgDTtoPackageFullName <- function(pkg) {
  if (is.data.table(pkg)) {
    if (!all(c("Account", "Repo", "Branch") %in% colnames(pkg))) {
      if (any(c("packageFullName") %in% colnames(pkg))) {
        pkg <- pkg$packageFullName
      }
    }
  }
  pkg
}

#' @inheritParams Require
dlGitHubFile <- function(pkg, filename = "DESCRIPTION",
                         purge = getOption("Require.purge", FALSE),
                         verbose = getOption("Require.verbose")) {
  ret <- if (NROW(pkg) > 0) {
    needsParse <- TRUE
    cn <- colnames(pkg)
    if (!is.null(cn)) {
      needsParse <- !all(c("hasSubFolder", "Repo", "Branch", "Account") %in% cn)
    }
    if (needsParse) {
      pkg <- pkgDTtoPackageFullName(pkg)
      pkgDT <- parseGitHub(pkg, verbose = verbose)
    } else {
      pkgDT <- pkg
    }
    if (!is.null(pkgDT[["shas"]])) {
      pkgDT[nchar(pkgDT[["shas"]]) > 0, Branch := shas]
    }
    if (is.null(pkgDT[["hasSubFolder"]])) set(pkgDT, NULL, "hasSubFolder", FALSE)
    pkgDT[repoLocation == .txtGitHub,
          url := {
            gitHubFileUrl(
              hasSubFolder = hasSubFolder, Branch = Branch, GitSubFolder = GitSubFolder,
              Account = Account, Repo = Repo, filename = filename
            )
          },
          by = "Package"
    ]
    destFile <- RequireGitHubCacheFile(pkgDT, filename = filename)

    feDF <- file.exists(destFile)
    if (isTRUE(any(feDF))) {
      destFile2 <- destFile[feDF]
      versionLocal <- DESCRIPTIONFileVersionV(destFile2)
      versionLocalOK <- rep(TRUE, length(versionLocal)) # no versionSpec will give NA next; NA is "keep"
      anyHEAD <- (pkgDT$versionSpec[pkgDT$repoLocation == .txtGitHub][feDF] == "HEAD")
      if (isTRUE(any(anyHEAD %in% TRUE)))
        versionLocalOK[anyHEAD] <- FALSE
      hasNonHead <- anyHEAD %in% FALSE
      if (isTRUE(any(hasNonHead)))
        versionLocalOK <- compareVersion2(versionLocal[hasNonHead],
                                              pkgDT$versionSpec[pkgDT$repoLocation == .txtGitHub][feDF][hasNonHead],
                                              inequality = pkgDT$inequality[feDF][hasNonHead])
      versionLocalNotOK <- versionLocalOK %in% FALSE
      if (isTRUE(any(versionLocalNotOK))) {
        oo <- file.remove(unique(destFile2[versionLocalNotOK]))
      }
    }

    set(pkgDT, NULL, "destFile", destFile)

    if (!isTRUE(getOption("Require.offlineMode"))) {
      alreadyExists <- rmEmptyFiles(pkgDT$destFile)
      if (any(alreadyExists)) {
        fs <- file.size(pkgDT$destFile)
        tooSmall <- fs < 100
        if (any(tooSmall %in% TRUE)) {
          unlink(pkgDT$destFile[which(tooSmall)])
          alreadyExists <- tooSmall %in% FALSE
        }
      }
      if (any(!alreadyExists)) {
        pkgDT[repoLocation == .txtGitHub & alreadyExists %in% FALSE,
              filepath := {
                messageVerbose(Package, "@", Branch, " downloading ", filename, verbose = verbose - 1)
                ret <- NA
                dl <- .downloadFileMasterMainAuth(unique(url)[1], unique(destFile)[1],
                                                  need = "master",
                                                  verbose = verbose - 1
                )
                ret <- if (!is(dl, "try-error")) {
                  destFile
                } else {
                  NA
                }

                ret
              },
              by = c("Package", "Branch")
        ]
      }
      old <- grep("filepath|destFile", colnames(pkgDT), value = TRUE)[1]
      wh <- which(pkgDT$repoLocation == .txtGitHub)
      if (identical("DESCRIPTION", filename)) {
        set(pkgDT, wh, "DESCFile", pkgDT[[old]][wh])
      } else {
        set(pkgDT, wh, "filename", pkgDT[[old]][wh])
      }

    }
    pkgDT[]
  } else {
    pkg
  }
  ret
}

#' Available and archived versions
#'
#' These are wrappers around available.packages and also get the archived versions
#' available on CRAN.
#'
#' @rdname availableVersions
#' @export
#' @param package A single package name (without version or github specifications)
#' @details
#' `dlArchiveVersionsAvailable` searches CRAN Archives for available versions.
#' It has been borrowed from a sub-set of the code in a non-exported function:
#' `remotes:::download_version_url`
dlArchiveVersionsAvailable <- function(package, repos = getOption("repos"), verbose = getOption("Require.verbose")) {
  info <- list()
  for (repo in repos) {
    archiveFile <- archiveFile(repo) # sprintf("%s/src/contrib/Meta/archive.rds", repo)
    if (!exists(archiveFile, envir = pkgDepEnv(), inherits = FALSE)) {
      archive <- tryCatch(
        {
          con <- gzcon(url(archiveFile, "rb"))
          on.exit(close(con))
          readRDS(con)
        },
        warning = function(e) {
          setOfflineModeTRUE(verbose = verbose)
          list()
        },
        error = function(e) list()
      )
      assign(archiveFile, archive, envir = pkgDepEnv())
    } else {
      archive <- get(archiveFile, envir = pkgDepEnv())
    }
    if (length(archive) == 0) {
      archive <- Map(pack = package, function(pack) NULL)
    }
    info[[repo]] <- archive[package]
    naNames <- is.na(names(info[[repo]]))
    if (any(naNames)) {
      names(info[[repo]])[naNames] <- package[naNames]
    }
    if (!is.null(info[[repo]][[1]])) {
      info[[repo]] <- lapply(info[[repo]], function(x) {
        x$repo <- repo
        x
      })
    }
  }
  info <- invertList(info)
  # info <- lapply(info, unname)
  info <- lapply(info, function(dd) lapply(dd, function(d) as.data.table(d, keep.rownames = "PackageUrl")))
  info <- lapply(info, rbindlist, idcol = "repo")
  # info <- lapply(info, rbindlist)
  info <- lapply(info, function(d) {
    if (!is.null(d[["mtime"]])) setorderv(d, "mtime")
  })



  return(info)
}


#' @importFrom utils packageVersion installed.packages
installedVers <- function(pkgDT, libPaths) {

  pkgDT <- toPkgDT(pkgDT)
  # pp <- data.table::copy(pkgDT)
  if (NROW(pkgDT)) {
    # ip2 <- as.data.table(installed.packages(lib.loc = libPaths, fields = c("Package", "LibPath", "Version")))
    ip <- as.data.table(.installed.pkgs(lib.loc = libPaths, other = "LibPath", which = NULL, packages = pkgDT$Package))#, other = c("Package", "Version"))) # these 2 are defaults
    ip <- ip[ip$Package %in% pkgDT$Package]
    if (NROW(ip)) {
      pkgs <- pkgDT$Package
      names(pkgs) <- pkgDT$packageFullName
      ln <- loadedNamespaces()
      ln <- ln[!ln %in% .basePkgs]
      # Need both the next lines
      pkgs <- pkgs[pkgs %in% ln]
      pkgs <- pkgs[pkgs %in% ip$Package] # can be loadedNamespace, but not installed, if it had been removed in this session
      if (NROW(pkgs)) {
        pkgs <- pkgs[!duplicated(pkgs)]

        installedPkgsCurrent <- data.table(Package = pkgs, packageFullName = names(pkgs))
        installedPkgsCurrent[, VersionFromPV := tryCatch({
          lp <- ip$LibPath[ip$Package %in% Package][1]
          as.character(packageVersion(Package, lp))
        }, error = function(e) NA_character_), by = "Package"]
        ip <- try(installedPkgsCurrent[ip, on = "Package"])
        if (is(ip, "try-error")) {
          browserDeveloper("Error 234")
        }
        ip[!is.na(VersionFromPV), Version := VersionFromPV]
      }
    }
    ip <- ip[, c("Package", "LibPath", "Version")]
    ip <- unique(ip, by = c("Package")) # , "LibPath" # basically, only take the first one if 2 installed in LibPath
    pkgDT <- try(ip[pkgDT, on = "Package"], silent = TRUE)
    if (is(pkgDT, "try-error")) {
      browserDeveloper("Error 123")
    }

  } else {
    pkgDT <- cbind(pkgDT, LibPath = NA_character_, "Version" = NA_character_)
  }

  installed <- !is.na(pkgDT$Version)
  if (any(installed)) {
    set(pkgDT, NULL, "installed", installed)
  }
  pkgDT
}

#' @importFrom utils available.packages
#' @rdname availableVersions
#' @param returnDataTable Logical. If `TRUE`, the default, then the return
#'   is a data.table.
#'   Otherwise, it is a `matrix`, as per `available.packages`
#' @inheritParams Require
#' @inheritParams utils::install.packages
available.packagesCached <- function(repos, purge, verbose = getOption("Require.verbose"),
                                     returnDataTable = TRUE, type) {
  fillDefaults(pkgDep)
  if (!isTRUE(getOption("Require.offlineMode"))) {
    repos <- getCRANrepos(repos, ind = 1)
    purge <- purgeAvailablePackages(repos, purge = purge)
    # purge <- dealWithCache(purge = purge)
  } else {
    purge <- FALSE
  }
  cap <- list()
  isMac <- tolower(SysInfo["sysname"]) == "darwin"
  isOldMac <- isMac && compareVersion(as.character(getRversion()), "4.0.0") < 0
  isWindows <- isWindows()

  if (identical(type, "both")) {
    types <- if (isOldMac) {
      c("mac.binary.el-capitan", "source")
    } else if (!isWindows && !isMac) {
      c("source")
    } else {
      c("binary", "source")
    }
  } else {
    types <- type
  }

  missingHttp <- !startsWith(unlist(repos), "http")
  if (any(missingHttp)) {
    repos[missingHttp] <- lapply(repos[missingHttp], function(r) {
      paste0("https://", r)
    })
  }
  if (is.list(repos)) {
    nams <- names(repos)
    repos <- unlist(repos)
    names(repos) <- nams
  }

  reposShort <- paste(substr(unlist(lapply(strsplit(repos, "//"), function(x) x[[2]])), 1, 20), collapse = "_")
  typesShort <- paste(unlist(lapply(strsplit(types, "//"), function(x) x[[1]])), collapse = "_")
  objNam <- paste0("availablePackages", "_", reposShort, "_", typesShort)
  if (!exists(objNam, envir = pkgDepEnv()) || isTRUE(purge)) {
    for (type in types) {
      fn <- availablePackagesCachedPath(repos, type)
      purgeTime <- purgeBasedOnTimeSinceCached(file.info(fn)[, "mtime"])
      purge <- purge || purgeTime
      if (isTRUE(purge)) {
        unlink(fn)
      }
      rmEmptyFiles(fn, 200)
      needNewFile <- TRUE
      if (file.exists(fn)) {
        # can be interupted and be corrupted
        cap[[type]] <- try(readRDS(fn), silent = TRUE)
        if (!is(cap[[type]], "try-error")) needNewFile <- FALSE
      }
      if (isTRUE(needNewFile)) {
        caps <- lapply(repos, function(repo) {
          available.packagesWithCallingHandlers(repo, type)
        })
        caps <- lapply(caps, as.data.table)
        caps <- unique(rbindlist(caps), by = c("Package", "Version", "Repository"))
        cap[[type]] <- caps

        if (!is.null(cacheGetOptionCachePkgDir())) {
          checkPath(dirname(fn), create = TRUE)
          saveRDS(cap[[type]], file = fn)
        }
      }
    }
    cap <- do.call(rbind, cap)
    assign(objNam, cap, envir = pkgDepEnv())
    out <- cap
  } else {
    out <- get(objNam, envir = pkgDepEnv(), inherits = FALSE)
  }

  if (isFALSE(returnDataTable)) {
    # as.matrix is not rich enough ... do it manually
    bb <- as.matrix(out)
    rownames(bb) <- out[["Package"]]
    dimnames(bb)[[1]] <- unname(bb[, "Package"])
    out <- bb
  }



  return(out)
}

isBinary <- function(fn, needRepoCheck = TRUE, repos = getOption("repos")) {
  theTest <- (endsWith(fn, "zip") & isWindows() ) |
    (grepl("R_x86", fn) & !isWindows() & !isMacOSX()) |
    (endsWith(fn, "tgz") & isMacOSX() )
  if (isTRUE(needRepoCheck)) {
    if (isWindows() || isMacOSX()) {
      binRepo <- isBinaryCRANRepo(curCRANRepo = repos)
    } else {
      binRepo <- isBinaryCRANRepo()
    }
    theTest <- theTest | binRepo
  }
  theTest
}

isBinaryCRANRepo <- function(curCRANRepo = getOption("repos")[["CRAN"]],
                             repoToTest = formals(setLinuxBinaryRepo)[["binaryLinux"]]) {
  if (isWindows() || isMacOSX()) {
    isBin <- grepl("[\\|/])|/bin[\\|/]", curCRANRepo)
  } else {
    if (is.name(repoToTest))
      repoToTest <- eval(repoToTest)
    isBin <- tryCatch(startsWith(prefix = repoToTest, curCRANRepo),
                     silent = TRUE, error = function(x) FALSE)
  }
  isBin
}

toPkgDT <- function(pkgDT, deepCopy = FALSE) {
  if (!is.data.table(pkgDT)) {
    pkgDT <- rmExtraSpaces(pkgDT)
    pkgDT <- if (deepCopy) {
      data.table(Package = extractPkgName(pkgDT), packageFullName = pkgDT)
    } else {
      toDT(Package = extractPkgName(pkgDT), packageFullName = pkgDT)
    }
  }

  pkgDT
}

toDT <- function(...) {
  setDT(list(...))
}


#' Detach and unload all packages
#'
#' This uses `pkgDepTopoSort` internally so that the package
#' dependency tree is determined, and then packages are unloaded
#' in the reverse order. Some packages don't unload successfully for
#' a variety of reasons. Several known packages that have this problem
#' are identified internally and *not* unloaded. Currently, these are
#' `glue`, `rlang`, `ps`, `ellipsis`, and, `processx`.
#'
#' @return
#' A numeric named vector, with names of the packages that were attempted.
#' `2` means the package was successfully unloaded, `1` it was
#' tried, but failed, `3` it was not loaded, so was not unloaded.
#' @export
#' @param pkgs A character vector of packages to detach. Will be topologically sorted
#'   unless `doSort` is `FALSE`.
#' @param dontTry A character vector of packages to not try. This can be used
#'   by a user if they find a package fails in attempts to unload it, e.g., "ps"
#' @param doSort If `TRUE` (the default), then the `pkgs` will be
#'   topologically sorted. If `FALSE`, then it won't. Useful if the
#'   `pkgs` are already sorted.
#' @inheritParams Require
#' @importFrom utils sessionInfo
#'
#'
detachAll <- function(pkgs, dontTry = NULL, doSort = TRUE, verbose = getOption("Require.verbose")) {
  messageVerbose("Detaching is fraught with many potential problems; you may have to",
                 "restart your session if things aren't working",
                 verbose = verbose, verboseLevel = 2
  )
  srch <- search()
  pkgsOrig <- pkgs
  origDeps <- pkgDep(pkgs, recursive = TRUE)
  depsToUnload <- c(pkgs, unname(unlist(origDeps)))
  allLoaded <- loadedNamespaces()
  # si <- sessionInfo() # sessionInfo can't handle Require when loaded with load_all, under some conditions
  # allLoaded <- c(names(si$otherPkgs), names(si$loadedOnly))
  others <- pkgDepTopoSortMemoise(pkgs = pkgs, deps = allLoaded, reverse = TRUE, verbose = verbose, purge = FALSE)
  names(others) <- others
  depsToUnload <- c(others, depsToUnload)
  depsToUnload <- depsToUnload[!duplicated(depsToUnload)]
  depsToUnload <- setdiff(depsToUnload, dontTry)

  if (length(depsToUnload) > 0) {
    out <- if (isTRUE(doSort)) pkgDepTopoSortMemoise(pkgs = depsToUnload, purge = FALSE) else NULL
    pkgs <- rev(c(names(out), pkgs))
  }
  pkgs <- extractPkgName(pkgs)
  pkgs <- unique(pkgs)
  names(pkgs) <- pkgs

  dontTryExtra <- intersect(
    c("glue", "rlang", "ps", "ellipsis", "processx", "vctrs", "RCurl", "bitops"),
    pkgs
  )

  if (length(dontTryExtra)) {
    messageVerbose("some packages don't seem to unload their dlls correctly. ",
                   "These will not be unloaded: ", paste(dontTryExtra, collapse = comma),
                   verbose = verbose, verboseLevel = 2
    )
    dontTry <- c(dontTry, dontTryExtra)
  }

  dontTry <- unique(c(.RequireDependenciesNoBase, "covr", dontTry))
  didntDetach <- intersect(dontTry, pkgs)
  pkgs <- setdiff(pkgs, dontTry)
  dontNeedToUnload <- logical()
  detached <- c()
  if (length(pkgs)) {
    pkgs <- unique(pkgs)
    names(pkgs) <- pkgs
    isLoaded <- unlist(lapply(pkgs, isNamespaceLoaded))
    dontNeedToUnload <- rep(NA, sum(!isLoaded))
    names(dontNeedToUnload) <- pkgs[!isLoaded]
    pkgs <- pkgs[isLoaded]
    detached1 <- logical()
    for (pkg in pkgs) {
      det <- try(unloadNamespace(pkg), silent = TRUE)
      detached1[[pkg]] <- if (!is(det, "try-error")) TRUE else FALSE
    }
    detached <- detached1

    # detached1 <- try(sapply(pkgs, unloadNamespace))
    # if (!is(detached1, "try-error")) {
    #   detached1 <- try(sapply(detached1, is.null))
    #   if (!is.list(detached1))
    #     detached <- detached1
    # }
  }
  if (length(didntDetach)) {
    notDetached <- rep(FALSE, length(didntDetach))
    names(notDetached) <- didntDetach
    detached <- c(detached, notDetached)
  }
  detached <- c(dontNeedToUnload, detached)
  inSearchPath <- unlist(lapply(rev(pkgsOrig), function(p) {
    pkgGrp <- "package:"
    pkgString <- paste0(pkgGrp, p)
    pkgString <- grep(pkgString, srch, value = TRUE)
    pkgString <- gsub(pkgGrp, "", pkgString)
  }))
  detached[unlist(detached) %in% TRUE] <- 2
  detached[unlist(detached) %in% FALSE] <- 1
  detached[is.na(unlist(detached))] <- 3
  detached
}

isWindows <- function() {
  tolower(SysInfo["sysname"]) == "windows"
}

isMacOSX <- function() {
  isMac <- tolower(SysInfo["sysname"]) == "darwin"
}

isLinux <- function() {
  isMac <- tolower(SysInfo["sysname"]) == "linux"
}

warningCantInstall <- function(pkgs, libPaths = .libPaths()) {
  warning(
    "Can't install ", pkgs, "; you will likely need to restart R and run:\n",
    "-----\n",
    "install.packages(c('", paste(pkgs, collapse = comma), "'), lib = '", libPaths[1], "')",
    "\n-----\n...before any other packages get loaded"
  )
}


rpackageFolder <- function(path = cacheGetOptionCachePkgDir(), exact = FALSE) {
  if (!is.null(path)) {
    if (isTRUE(exact)) {
      return(path)
    }
    if (isFALSE(path)) {
      return(NULL)
    }

    path <- path[1]
    if (normPathMemoise(path) %in% normPathMemoise(strsplit(Sys.getenv("R_LIBS_SITE"), split = ":")[[1]])) {
      path
    } else {
      if (interactive() && !endsWith(path, versionMajorMinor())) {
        ## R CMD check on R >= 4.2 sets libpaths to use a random tmp dir
        ## need to know if it's a user, who *should* keep R-version-specific dirs
        file.path(path, versionMajorMinor())
      } else {
        path
      }
    }
  } else {
    NULL
  }
}


preparePkgNameToReport <- function(Package, packageFullName) {
  pkgsCleaned <- gsub(.grepTooManySpaces, " ", packageFullName)
  pkgsCleaned <- gsub(.grepTabCR, "", pkgsCleaned)
  pkgNameInPkgFullName <- unlist(Map(
    pkg = Package, pfn = packageFullName,
    function(pkg, pfn) grepl(pkg, pfn)
  ))
  Package[!pkgNameInPkgFullName] <- paste0(
    Package[!pkgNameInPkgFullName], " (",
    packageFullName[!pkgNameInPkgFullName], ")"
  )
  Package
}

splitGitRepo <- function(gitRepo, default = "PredictiveEcology", masterOrMain = NULL) {

  gitRepoOrig <- gitRepo
  # Can have version number --> most cases (other than SpaDES modules) just strip off
  gitRepo <- trimVersionNumber(gitRepo)
  hasVersionSpec <- gitRepo != gitRepoOrig

  grSplit <- strsplit(gitRepo, "/|@")

  repo <- lapply(grSplit, function(grsplit) grsplit[[2]])
  names(grSplit) <- repo
  names(repo) <- repo
  grAcct <- strsplit(gitRepo, "/") # only account and repo
  lenGT1 <- lengths(grAcct) == 1
  if (any(lenGT1)) {
    acct <- default
    grSplit[lenGT1] <- lapply(grSplit[lenGT1], function(grsplit) append(list(acct), grsplit))
  } else {
    acct <- lapply(grSplit, function(grsplit) grsplit[[1]])
  }
  lenGT2 <- lengths(grSplit) > 2
  br <- lapply(grSplit, function(x) list())
  vs <- br

  if (any(lenGT2)) {
    br[lenGT2] <- lapply(grSplit[lenGT2], function(grsplit) grsplit[[3]])
  }

  br[!lenGT2] <- "HEAD"

  if (any(hasVersionSpec)) {
    versionSpecs <- extractVersionNumber(gitRepoOrig[hasVersionSpec])
    inequs <- extractInequality(gitRepoOrig[hasVersionSpec])
    vs[hasVersionSpec] <- paste0("(", inequs, " ", versionSpecs, ")")
  }

  list(acct = acct, repo = repo, br = br, versionSpec = vs)
}

postInstallDESCRIPTIONMods <- function(pkgInstall, libPaths) {
  whGitHub <- which(pkgInstall$repoLocation %in% .txtGitHub)
  if (length(whGitHub)) {
    pkgGitHub <- pkgInstall[whGitHub]
    for (pk in pkgGitHub[installed %in% TRUE & grepl("OK|restart", installResult)]$Package) {
      if (is.null(pkgGitHub[["GitSubFolder"]]))
        set(pkgGitHub, NULL, "GitSubFolder", "")
      pkgGitHub[Package %in% pk, {
        file <- file.path(libPaths[1], Package, "DESCRIPTION")
        txt <- readLines(file)
        alreadyHasSHA <- grepl("Github|Remote", txt)
        leaveAlone <- FALSE
        if (any(alreadyHasSHA)) {
          if (any(grepl(SHAonGH, txt))) {
            leaveAlone <- TRUE
          }
        }

        if (isFALSE(leaveAlone)) {
          dups <- duplicated(vapply(strsplit(txt, split = "\\:"),
                                    function(x) x[[1]], FUN.VALUE = character(1)))
          if (any(dups)) {
            # Delete the first version of any duplicated entry -- i.e., take the more recent
            #   version
            dupsRev <- duplicated(vapply(strsplit(rev(txt), split = "\\:"),
                                      function(x) x[[1]], FUN.VALUE = character(1)))
            txtOut <- rev(rev(txt)[!dupsRev])
          }
          if (!exists("txtOut", inherits = FALSE)) {

            beforeTheseLines <- grep("NeedsCompilation:|Packaged:|Author:", txt)
            insertHere <- min(beforeTheseLines)
            sha <- SHAonGH
            newTxt <-
              paste0("RemoteType: github
RemoteHost: api.github.com
RemoteRepo: ", Repo, "
RemoteUsername: ", Account, "
RemoteRef: ", Branch, "
RemoteSha: ", sha, "
GithubRepo: ", Repo, "
GithubSubFolder: ", GitSubFolder, "
GithubUsername: ", Account, "
GithubRef: ", Branch, "
GithubSHA1: ", sha, "")
            newTxt <- strsplit(newTxt, split = "\n")[[1]]
            newTxt <- gsub("^ +", "", newTxt)
            txtOut <- c(txt[seq(insertHere - 1)], newTxt, txt[insertHere:length(txt)])
          }
          cat(txtOut, file = file, sep = "\n")
        }
      }]

    }
  }

  return(invisible())
}

#' @importFrom utils unzip
#' @inheritParams Require
downloadRepo <- function(gitRepo, subFolder, overwrite = FALSE, destDir = ".",
                         verbose = getOption("Require.verbose")) {
  dir.create(destDir, recursive = TRUE, showWarnings = FALSE)
  gr <- splitGitRepo(gitRepo)
  ar <- file.path(gr$acct, gr$repo)

  pkgName <- if (is.null(names(gitRepo))) gr$repo else names(gitRepo)

  repoFull <- file.path(destDir, pkgName)
  zipFileName <- normalizePath(paste0(repoFull, ".zip"), winslash = "/", mustWork = FALSE)
  masterMain <- c("main", "master")
  br <- if (any(gr$br %in% masterMain)) {
    # possibly change order -- i.e., put user choice first
    masterMain[rev(masterMain %in% gr$br + 1)]
  } else {
    gr$br
  }

  url <- paste0("https://github.com/", ar, "/archive/", br, ".zip")
  out <- suppressWarnings(
    try(.downloadFileMasterMainAuth(url, destfile = zipFileName, need = "master"), silent = TRUE)
  )
  if (is(out, "try-error")) {
    return(out)
  }

  out <- lapply(zipFileName, function(zfn) unzip(zfn, exdir = destDir)) # unzip it

  de <- dir.exists(repoFull)
  if (any(de)) {
    if (isTRUE(overwrite)) {
      unlink(repoFull[de], recursive = TRUE)
    } else {
      stop(repoFull, " directory already exists. Use overwrite = TRUE if you want to overwrite it")
    }
  }
  # Finds the common component i.e., the base directory. This will have the SHA as part fo the filename; needs remving

  badDirname <- try(lapply(out, function(d) {
    unlist(lapply(out, function(x) {
      for (n in seq(nchar(x[1]))) {
        has <- all(startsWith(x, substr(x[1], 1, n)))
        if (any(isFALSE(has))) {
          break
        }
      }
      normPath(substr(x[1], 1, n - 1))
    }))
  }))
  if (is(badDirname, "try-error")) stop("Error 654; something went wrong with downloading & building the package")
  badDirname <- unlist(badDirname)
  if (!missing(subFolder))
    if (isTRUE(is.na(subFolder)) || isTRUE(is.null(subFolder))) {
      subFolder <- FALSE
    }

  newName <- unlist(Map(
    bad = badDirname, subFolder = subFolder, pkgName = pkgName,
    function(bad, subFolder, pkgName) {
      actualFolderName <- basename(gsub(subFolder, "", bad))
      if (!identical(actualFolderName, pkgName)) { # means the folder is not the pkgName e.g., mumin != MuMIn
        origOut <- normPath(out)
        outNP <- origOut
        newFolder <- dirname(bad)
        newFolder <- file.path(newFolder, pkgName)
        if (!isFALSE(subFolder)) { # get rid of subfolder for all files
          subFolderNP <- normPath(file.path(bad, subFolder))
          origOut <- grep(subFolderNP, origOut, value = TRUE)
          outNP <- grep(subFolderNP, origOut, value = TRUE)
          outNP <- gsub(subFolderNP, newFolder, outNP )
        } else {
          outNP <- gsub(bad, newFolder, outNP)
        }
        fileRenameOrMove(origOut, outNP) # do the rename
        newFolder
      }
    }
  ))
  unlink(zipFileName)
  messageVerbose(paste0(gitRepo, " downloaded and placed in ",
                        normalizePath(repoFull, winslash = "/"), collapse = "\n"),
                 verbose = verbose, verboseLevel = 2
  )
  return(normalizePath(repoFull))
}

getSHAfromGitHub <- function(acct, repo, br, verbose = getOption("Require.verbose")) {
  if (nchar(br) == 40) {
    return(br)
  }

  gitRefsURL <- file.path("https://api.github.com/repos", acct, repo, "git", "refs")
  if (missing(br)) {
    br <- "main"
  }
  if (identical(br, "HEAD")) {
    br <- "main"
  }
  masterMain <- c("main", "master")
  if (any(br %in% c(masterMain))) {
    # possibly change order -- i.e., put user choice first
    br <- masterMain[rev(masterMain %in% br + 1)]
  }
  # tf <- tempfile()
  for (ii in 1:2) {
    tf <- file.path(RequireGitHubCacheDir(), paste0("listOfRepos_",acct, "@", repo))
    downloadNow <- TRUE
    if (file.exists(tf)) {
      if ((difftime(Sys.time(), file.info(tf)$mtime, units = "sec")) < 60) {
        downloadNow <- FALSE
      }
    }
    if (downloadNow)
      .downloadFileMasterMainAuth(gitRefsURL, destfile = tf, need = "master")
    gitRefs <- try(suppressWarnings(readLines(tf)), silent = TRUE)
    if (any(grepl("Bad credentials", gitRefs))) {#} || notFound) {
      # if (notFound)
       #  stop("Did you spell the GitHub.com repository, package and or branch/gitRefs correctly?")
      stop(gitRefs)
    }

    if (is(gitRefs, "try-error")) {
      return(gitRefs)
    }
    if (length(gitRefs) > 1) {
      # Seems to sometimes come out as individual lines; sometimes as one long concatenates string
      #   Was easier to collapse the individual lines, then re-split
      gitRefs <- paste(gitRefs, collapse = "")
    }
    gitRefsSplit <- strsplit(gitRefs, "},")[[1]] # this splits onto separate lines

    gitRefsSplit2 <- strsplit(gitRefsSplit, ":")

    if (any(grepl("master|main|HEAD", unlist(br)))) {
      br <- masterOrMainFromGitRefs(gitRefsSplit2)
      # br2 <- grep(unlist(gitRefsSplit2), pattern = "api.+heads/(master|main)", value = TRUE)
      # br <- gsub(br2, pattern = ".+api.+heads.+(master|main).+", replacement = "\\1")
    }
    for (branch in br) { # will be length 1 in most cases except master/main
      whHasBr <- which(vapply(gitRefsSplit2, function(xx) {
        any(grepl(paste0(".+refs/.+/+", branch, "\""), xx))
      }, FUN.VALUE = logical(1)))
      if (length(whHasBr) > 0) {
        break
      }
    }
    # This will catch cases where the RequireGitHubCacheDir() doesn't have it,
    #    but it is there (e.g., a new branch or new gitRefs)... this will deleted
    if (length(whHasBr) == 0) {
      if (ii %in% 1) {
        unlink(tf)
        next
      } else {
        stop(messageCantFind(br, acct, repo))
      }
    }
    break
  }

  gitRefsFinal <- gitRefsSplit2[[whHasBr]]
  shaLine <- grep("sha", gitRefsFinal) + 1
  shaLine <- strsplit(gitRefsFinal[shaLine], ",")[[1]][1]
  sha <- gsub(" *[[:punct:]]+(.+)[[:punct:]] *", "\\1", shaLine)
  sha
}

getSHAfromGitHubMemoise <- function(...) {
  pe <- pkgEnv()
  if (getOption("Require.useMemoise", TRUE)) {
    dots <- list(...)
    if (!exists(.txtGetSHAfromGitHub, envir = pe, inherits = FALSE)) {
      loadGitHubSHAsFromDisk(verbose = dots$verbose) # puts it into the Memoise-expected location
      if (!exists(.txtGetSHAfromGitHub, envir = pe, inherits = FALSE))
        pe[[.txtGetSHAfromGitHub]] <- new.env()
    }
    ret <- NULL
    ss <- match.call(definition = getSHAfromGitHub)
    uniqueID <- paste(lapply(ss[-1], eval, envir = parent.frame()), collapse = "_")
    if (!exists(uniqueID, envir = pe[[.txtGetSHAfromGitHub]], inherits = FALSE)) {
      pe[[.txtGetSHAfromGitHub]][[uniqueID]] <- list()
    } else {
      whIdent <- unlist(lapply(pe[[.txtGetSHAfromGitHub]][[uniqueID]], function(x) identical(x$input, dots)))
      if (any(whIdent)) {
        ret <- pe[[.txtGetSHAfromGitHub]][[uniqueID]][[which(whIdent)]]$output
      }
    }
    if (is.null(ret)) { # Case where it doesn't exist in pe
      inputs <- data.table::copy(dots)
      ret <- getSHAfromGitHub(...)
      # Add it to the pe
      newObj <- list(pe[[.txtGetSHAfromGitHub]][[uniqueID]], list(input = inputs, output = ret))
      pe[[.txtGetSHAfromGitHub]][[uniqueID]] <- newObj

    }
  } else {
    ret <- getSHAfromGitHub(...)
  }

  return(ret)
}

loadGitHubSHAsFromDisk <- function(verbose = getOption("Require.verbose")) {
  ret <- list()
  pe <- pkgEnv()
  if (!exists(.txtGetSHAfromGitHub, envir = pe, inherits = FALSE)) {
    fn <- getSHAFromGitHubDBFilename()
    if (isTRUE(file.exists(fn))) {
      out <- readRDS(fn)
      removeFile <- purgeBasedOnTimeSinceCached(out[[GitHubSHAonDiskCacheTime]])
      if (!removeFile) { # remove if 24 hours old
        # if (!exists(.txtGetSHAfromGitHub, envir = pe, inherits = FALSE))
        pe[[.txtGetSHAfromGitHub]] <- new.env()
        list2env(out, envir = pe[[.txtGetSHAfromGitHub]])
      } else {
        messageVerbose("Purging disk-backed pkgDep cache for GitHub SHA values; it has ",
                       "been more than ", defaultCacheAgeForPurge, ". Change this by setting ",
                       "Sys.getenv('R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE')",
                       verbose = verbose, verboseLevel = 2)
        unlink(fn)
      }
    }
  }

  ret <- getSHAFromPkgEnv()
  ret <- as.list(lapply(ret, function(x) x[[2]]$output))
  invisible(ret)
}

GitHubSHAonDiskCacheTime <- ".GitHubSHAonDiskCacheTime"

saveGitHubSHAsToDisk <- function(preShas) {
  pe <- pkgEnv()
  if (exists(.txtGetSHAfromGitHub, envir = pe, inherits = FALSE)) {
    obj <- getSHAFromPkgEnv()
    needSave <- if (missing(preShas)) { TRUE } else {
      length(setdiffNamed(as.list(lapply(obj, function(x) x[[2]]$output)), preShas)) > 0
    }
    fn <- getSHAFromGitHubDBFilename() # can return character() if RPackageCache is NULL; but here that is not possible
    if  (needSave && isTRUE(file.exists(fn))) {
      dd <- dirname(fn)
      if (!dir.exists(dd)) dir.create(dd, recursive = TRUE)
      obj[[GitHubSHAonDiskCacheTime]] <- format(Sys.time())
      out <- saveRDS(obj, fn)
    }
  }
}

getSHAFromPkgEnv <- function() {
  pe <- pkgEnv()
  as.list(pe[[.txtGetSHAfromGitHub]])
}


getSHAFromGitHubDBFilename <- function() {
  go <- cacheGetOptionCachePkgDir()
  if (!is.null(go))
    out <- file.path(go, paste0(.txtGetSHAfromGitHub, ".rds")) # returns NULL if no Cache used
  else
    out <- character()
  out
}



# .earliestRSPMDate <- "2015-06-06" # THIS WAS MRAN's DATE
.earliestRSPMDate <- "2017-10-10"
.latestRSPMDate <- Sys.Date() - 5

#' R versions
#'
#' Reference table of R versions and their release dates (2018 and later).
#'
#' Update this as needed using `rversions::r_versions()`:
#'
#' \verb{
#' # install.packages("rversions")
#' v = rversions::r_versions()
#' keep = which(as.Date(v$date, format = "%Y-%m-%d") >=
#'              as.Date("2018-01-01", format = "%Y-%m-%d"))
#' dput(v[keep, c("version", "date")])
#' }
rversions <- structure(
  list(
    version = c(
      "3.4.4", "3.5.0", "3.5.1", "3.5.2",
      "3.5.3", "3.6.0", "3.6.1", "3.6.2", "3.6.3", "4.0.0", "4.0.1",
      "4.0.2", "4.0.3", "4.0.4", "4.0.5", "4.1.0", "4.1.1", "4.1.2",
      "4.1.3", "4.2.0", "4.2.1"
    ),
    date = structure(c(
      1521101067, 1524467078,
      1530515071, 1545293080, 1552291489, 1556262303, 1562310303, 1576137903,
      1582963516, 1587711934, 1591427116, 1592809519, 1602313524, 1613376313,
      1617174315, 1621321522, 1628579106, 1635753912, 1646899538, 1650611141,
      1655967933
    ), class = c("POSIXct", "POSIXt"), tzone = "UTC")
  ),
  row.names = 108:128, class = "data.frame"
)

versionMajorMinor <- function(version = base::version) {
  if (!is(version, "simple.list")) {
    version <- strsplit(version, "[.]")[[1]]
    nams <- c("major", "minor", "revision")
    names(version) <- nams[seq_along(version)]
  }
  paste0(version[["major"]], ".", strsplit(version[["minor"]], "[.]")[[1]][1])
}


# Used inside internetExists
urlExists <- function(url) {
  con <- url(url)
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  for (i in 1:5) {
    a <- try(suppressWarnings(readLines(con, n = 1)), silent = TRUE)
    try(close(con), silent = TRUE)
    ret <- if (is(a, "try-error")) FALSE else TRUE
    if (isTRUE(ret)) {
      break
    } else {
      Sys.sleep(0.1)
    }
  }
  ret
}

#' @inheritParams Require
internetExists <- function(mess = "", verbose = getOption("Require.verbose")) {
  if (!isTRUE(getOption("Require.offlineMode"))) {
    if (getOption("Require.checkInternet", FALSE)) {
      internetMightExist <- TRUE
      iet <- get0(.txtInternetExistsTime, envir = pkgEnv())
      if (!is.null(iet)) {
        if ((Sys.time() - getOption("Require.internetExistsTimeout", 30)) < iet) {
          internetMightExist <- FALSE
        }
      }
      if (internetMightExist) {
        opts2 <- options(timeout = 2)
        on.exit(options(opts2))
        pe <- pkgEnv()
        ue <- pe$internetExists <- urlExists("https://www.google.com")
        if (isFALSE(ue)) {
          internetMightExist <- FALSE

          messageVerbose("\033[32mInternet does not appear to exist; proceeding anyway\033[39m",
                         verbose = verbose, verboseLevel = 2
          )
        }
        assign(.txtInternetExistsTime, Sys.time(), envir = pkgEnv())
      }
      out <- internetMightExist
    } else {
      out <- TRUE
    }
  } else {
    out <- FALSE
  }
  out
}


#' A list of R packages that should likely be installed from Source, not Binary
#'
#' The list of R packages that `Require` installs from source on Linux, even if
#' the `getOptions("repos")` is a binary repository. This list can be updated by
#' the user by modifying the options `Require.spatialPkgs` or
#' `Require.otherPkgs`. Default "force source only packages" are visible with
#' `RequireOptions()`.
#' @param spatialPkgs A character vector of package names that focus on spatial analyses.
#' @param otherPkgs A character vector of package names that often
#'   require system specific compilation.
#' @param additional Any other packages to be added to the other 2 argument vectors
#' @export
#' @return
#' A sorted concatenation of the 3 input parameters.
sourcePkgs <- function(additional = NULL,
                       spatialPkgs = NULL,
                       otherPkgs = NULL) {
  .spatialPkgs <- getOption("Require.spatialPkgs")
  if (is.null(spatialPkgs)) {
    spatialPkgs <- .spatialPkgs
  }
  .otherPkgs <- getOption("Require.otherPkgs")
  if (is.null(otherPkgs)) {
    otherPkgs <- .otherPkgs
  }
  unique(sort(c(spatialPkgs, otherPkgs, additional)))
}

srcPackageURLOnCRAN <- "https://cloud.r-project.org/"

stripHTTPAddress <- function(addr) {
  addr <- gsub("https://(.+)", "\\1", unname(addr))
  addr <- gsub("/$", "", unname(addr))
  addr
}





masterMainHEAD <- function(url, need) {
  hasMasterMain <- grepl(masterMainGrep, url)
  hasMaster <- grepl(masterGrep, url)
  hasMain <- grepl(mainGrep, url)
  if (any(hasMasterMain) && need %in% masterMain) {
    # Good -- try both master and main
    br <- need
  } else if (any(hasMasterMain) && need %in% "HEAD") {
    # need change
    br <- "HEAD"
    url <- gsub(masterMainGrep, paste0("/", br, "\\1"), url)
  }
  HEADgrep <- paste0("/", paste("HEAD", collapse = "|"), "(/|\\.)")
  hasHEAD <- grepl(HEADgrep, url)
  if (any(hasHEAD) && need %in% masterMain) {
    br <- need
    url <- gsub(HEADgrep, paste0("/", br, "\\1"), url)
  }
  if (any(hasHEAD) && need %in% "HEAD") {
    br <- "HEAD"
  }
  if (any(hasMasterMain) && length(url) == 1) {
    newBr <- masterMain[hasMain + 1]
    url[[2]] <- gsub(masterMainGrep, paste0("/", newBr, "\\1"), url)
  }
  url
}

#' GITHUB_PAT-aware and `main`-`master`-aware download from GitHub
#'
#' Equivalent to `utils::download.file`, but taking the `GITHUB_PAT` environment
#' variable and using it to access the Github url.
#'
#' @inheritParams utils::download.file
#' @param need If specified, user can suggest which `master` or `main` or `HEAD` to
#'   try first. If unspecified, `HEAD` is used.
#' @inheritParams Require
#' @inheritParams messageVerbose
#' @importFrom utils download.file assignInMyNamespace
#' @return
#' This is called for its side effect, namely, the same as `utils::download.file`, but
#' using a `GITHUB_PAT`, it if is in the environment, and trying both `master` and
#' `main` if the actual `url` specifies either `master` or `main` and it does not exist.
#' @export
.downloadFileMasterMainAuth <- function(url, destfile, need = "HEAD",
                                        verbose = getOption("Require.verbose"), verboseLevel = 2) {
  if (!dir.exists(dirname(destfile)))
    silent <- checkPath(dirname(destfile), create = TRUE)
  hasMasterMain <- grepl(masterMainGrep, url)
  if (!all(hasMasterMain)) {
    if (length(url) > 1) stop("This function is not vectorized")
    if (length(url) != length(destfile))
      stop("destfile must be same length as url")
  }
  url <- masterMainHEAD(url, need) # makes 2

  # Authentication
  token <- NULL
  usesGitCreds <- requireNamespace("gitcreds", quietly = TRUE) &&
    requireNamespace("httr", quietly = TRUE)
  if (usesGitCreds) {
    token <- tryCatch(
      gitcreds::gitcreds_get(use_cache = FALSE),
      error = function(e) NULL
    )
    if (!is.null(token)) {
      token <- paste0("token ", token$password)
    }
  }
  if (is.null(token)) {
    ghp <- Sys.getenv("GITHUB_PAT")
    messageGithubPAT(ghp, verbose = verbose, verboseLevel = 0)
    if (nzchar(ghp)) {
      messageVerbose("For better security, user should use the newer way to store git credentials.",
                     "\nUsing a GITHUB_PAT environment variable will continue to work, but see: ",
                     "https://usethis.r-lib.org/articles/git-credentials.html", verbose = verbose + GitHubMessage)
      if (GitHubMessage >= 0)
        assignInMyNamespace("GitHubMessage", -10)
      url <- sprintf(paste0("https://%s:@", gsub("https*://", "", url)), ghp)
    }
  }

  urls <- url
  urls <- split(urls, hasMasterMain)
  outNotMasterMain <- outMasterMain <- character()

  ret <- withCallingHandlers({

    for (i in 1:2) {
      if (!is.null(urls[["FALSE"]])) {
        outNotMasterMain <-
          Map(URL = urls[["FALSE"]], MoreArgs = list(df = destfile), function(URL, df) {
            if (!isTRUE(getOption("Require.offlineMode"))) {

              for (tryNum in 1:2) {
                if (is.null(token)) {
                  tryCatch(download.file(URL, destfile = df, quiet = TRUE),# need TRUE to hide ghp
                           error = function(e) {
                             if (is.null(token))
                               e$message <- stripGHP(ghp, e$message)
                             if (tryNum > 1)
                               messageVerbose(e$message, verbose = verbose)
                           })
                } else {
                  a <- GETWauthThenNonAuth(url, token, verbose = verbose)
                  # a <- httr::GET(url, httr::add_headers(Authorization = token))
                  # if (grepl("Bad credentials", a) || grepl("404", a$status_code))
                  #   a <- httr::GET(url, httr::add_headers())
                  data <- httr::content(a, "raw")
                  writeBin(data, df)
                }
                if (file.exists(df))
                  break
                if (is.null(token))
                  URL <- stripGHP(ghp, URL) # this seems to be one of the causes of failures -- the GHP sometimes fails
              }
            }
          })
      }
      if (!is.null(urls[["TRUE"]])) { # should be sequential because they are master OR main
        for (wh in seq(urls[["TRUE"]])) {
          if (!isTRUE(getOption("Require.offlineMode"))) {
            if (is.null(token)) {
              outMasterMain <- try(download.file(urls[["TRUE"]][wh], destfile = destfile, quiet = TRUE), silent = TRUE)
            } else {
              outMasterMain <- try(silent = TRUE, {
                a <- GETWauthThenNonAuth(urls[["TRUE"]][wh], token, verbose = verbose)
                # a <- httr::GET(urls[["TRUE"]][wh], httr::add_headers(Authorization = token))
                if (grepl("404", httr::http_status(a)$message))
                  stop()
                data <- httr::content(a, "raw")
                writeBin(data, destfile)
              })
              if (is.null(outMasterMain)) outMasterMain <- 0
            }
          }

          if (!is(outMasterMain, "try-error")) {
            namForOut <- if (is.null(token)) {
              stripGHP(ghp, urls[["TRUE"]][wh])
            } else {
              urls[["TRUE"]][wh]
            }
            names(outMasterMain) <- namForOut
            break
          }
        }
      }
      ret <- c(outNotMasterMain, outMasterMain)
      if (!any(unlist(lapply(ret, is, "try-error")))) {
        break
      }
      Sys.sleep(0.5)
    }
    ret
  },
  warning = function(w) {
    setOfflineModeTRUE(verbose = verbose)
    # strip the ghp from the warning message
    if (is.null(token))
      w$message <- stripGHP(ghp, w$message)
    invokeRestart("muffleWarning")
  },
  error = function(e) {
    # strip the ghp from the message
    if (is.null(token))
      e$message <- stripGHP(ghp, e$message)
    stop(e)
  })

  ret
}

stripGHP <- function(ghp, mess) {
  if (!missing(ghp))
    mess <- gsub(paste0(ghp, ".*@"), "", mess)
  mess
}

messageGithubPAT <- function(ghp, verbose = verbose, verboseLevel = 0) {
  if (nzchar(ghp)) {
    if (is.null(get0(.txtPkgHasGHP, envir = pkgEnv()))) {
      assign(.txtPkgHasGHP, TRUE, envir = pkgEnv())
      messageVerbose("Using GITHUB_PAT to access files on GitHub",
                     verboseLevel = 0, verbose = verbose
      )
    }
  }
}

notInArchives <- "Not in Archives"


masterMain <- c("main", "master")
masterMainGrep <- paste0("/", paste(masterMain, collapse = "|"), "(/|\\.)")
masterGrep <- paste0("/", "master", "(/|\\.)")
mainGrep <- paste0("/", "main", "(/|\\.)")

extractPkgNameFromWarning <- function(x) {

  if (any(grepl(.txtMsgIsInUse, x)) || # "in use"
      any(grepl(.txtInstallationPkgFailed, x))) { # "installation of 2 packages failed:"
    out <- NULL
    if (isWindows()) {
      aa <- strsplit(x, "\\'")
      out <- lapply(aa, function(y) {
        wh <- grep(", ", y)
        wh <- c(1, wh)
        y[c(wh + 1)]
      })
    }
    if (is.na(out) || !isWindows()) {
      aa <- strsplit(x, "\u2019|\u2018")[[1]]
      aa <- grep(.txtInstallationPkgFailed, aa, invert = TRUE, value = TRUE)
      aa <- grep("package|is in use|failed", aa, invert = TRUE, value = TRUE)
      out <- grep(", ", aa, value = TRUE, invert = TRUE)
    }

    out <- unlist(out)
  } else {
    out <- gsub(".+\u2018(.+)_.+\u2019.*", "\\1", x) # those two escape characters are the inverted commas
    out <- gsub(".+\u2018(.+)\u2019.*", "\\1", out)
    out <- gsub("^.+\\'(.+)\\'.+$", "\\1", out)
    out <- gsub(".+\u2018(.+)\u2019.+", "\\1", out) # package XXX is in use and will not be installed
  }

  if (isTRUE(any(grepl(.txtCannotOpenFile, x)))) {
    outs <- strsplit(out, split = "/|\\\\")
    out <- sapply(outs, function(x) x[length(x) - 1])
  }
  out
}

availablePackagesCachedPath <- function(repos, type) {
  file.path(cachePkgDir(),
            paste0(gsub("https|[:/]", "", repos), collapse = "/"),
            type, "availablePackages.rds")
}

installPackagesWithQuiet <- function(ipa, verbose) {
  if (isWindows())
    messageVerbose("  -- ", .txtInstallingColon,"\n", verbose = verbose, appendLF = FALSE)
  if (isWindows() && identical(ipa$type, "source") &&
      getOption("Require.installPackagesSys") == 0) {
    op <- options(Ncpus = 1)
    on.exit(options(op), add = TRUE)
  }

  if (isTRUE(length(ipa$type) > 1))
    ipa$type <- ipa$type[2]

  if (getOption("Require.installPackagesSys") &&
      requireNamespace("sys", quietly = TRUE)){
    for (i in 1:1) {
      anyFailed <- NULL
      out <- #try(
        sysInstallAndDownload(ipa, splitOn = "pkgs", tmpdir = ipa$destdir,
                                   doLine = "outfiles <- do.call(install.packages, args)",
                                   verbose = verbose)
        #)
      if (file.exists(out)) {
        txt <- readLines(out)
        anyFailed <- grep(.txtInstallationPkgFailed, txt)
      }

      if (length(anyFailed) == 0)
        break

      pkgName <- extractPkgNameFromWarning(txt[anyFailed])
      if (any(is.na(pkgName)))
        pkgName <- extractPkgNameFromWarning(paste(txt[anyFailed:(anyFailed + 1)], collapse = ""))
      messageVerbose("Failed installation for: ", paste(pkgName, collapse = ", "),
                     "\nTrying again ... ", verbose = verbose)
      ipa$pkgs <- pkgName
      ipa$available <- ipa$available[ipa$available[, "Package"] %in% pkgName, , drop = FALSE]
    }
  } else {

    if (isMacOSX() && "covr" %in% ipa$pkgs)
      print(ipa)
    # if (ipa$quiet && ipa$type %in% "source" && isWindows())
    #   ipa$quiet <- FALSE
    if (isTRUE(ipa$quiet)) {
      messSupp2 <- capture.output({
        messSupp <- capture.output(type = "message", {
          out <- do.call(install.packages, ipa)
        })
      })
    } else {
      out <- do.call(install.packages, ipa)
    }
  }
  return(out)
}

#' @importFrom utils remove.packages
checkHEAD <- function(pkgDT) {
  HEADgrep <- " *\\(HEAD\\)"
  set(pkgDT, NULL, "hasHEAD", grepl(HEADgrep, pkgDT$packageFullName))
  pkgDT
}

packageFullName <- function(pkgDT) {
  inequality <- if (is.null(pkgDT[["inequality"]])) "==" else pkgDT[["inequality"]]
  if (all(colnames(pkgDT) %in% c("GithubRepo", "GithubUsername"))) {
    ifelse(!is.na(pkgDT$GithubRepo) & nzchar(pkgDT$GithubRepo),
           paste0(pkgDT$GithubUsername, "/", pkgDT$Package, "@", pkgDT$GithubSHA1),
           paste0(pkgDT$Package, ifelse(is.na(pkgDT$Version) | is.na(pkgDT$inequality),
                                        "", paste0(" (", inequality, pkgDT$Version, ")")
           ))
    )
  } else {
    ifelse(!is.na(pkgDT$Repo) & nzchar(pkgDT$Repo),
           paste0(pkgDT$Account, "/", pkgDT$Package, "@", pkgDT$SHAonGH),
           paste0(pkgDT$Package, ifelse(is.na(pkgDT$Version) | is.na(pkgDT$inequality),
                                        "", paste0(" (", inequality, pkgDT$Version, ")")
           ))
    )
  }
}

gitHubFileUrl <- function(hasSubFolder, Branch, GitSubFolder, Account, Repo, filename) {
  if (any(hasSubFolder, na.rm = TRUE)) {
    Branch <- paste0(Branch, "/", GitSubFolder)
  }
  file.path("https://raw.githubusercontent.com", Account, Repo, Branch, filename, fsep = "/")
}


setOfflineModeTRUE <- function(verbose = getOption("Require.verbose")) {
  if (!isTRUE(getOption("Require.offlineMode"))) {
    if (!internetExists()) {
      options(
        "Require.offlineMode" = TRUE,
        "Require.offlineModeSetAutomatically" = TRUE
      )
      messageVerbose("Internet appears to be unavailable; setting options('Require.offlineMode' = TRUE)",
                     verbose = verbose)
    }
  }
}

checkAutomaticOfflineMode <- function() {
  if (getOption("Require.offlineModeSetAutomatically", FALSE)) {
    options(
      "Require.offlineModeSetAutomatically" = NULL,
      Require.offlineMode = FALSE
    )
  }
}

isRstudioServer <- function () {
  isRstudioServer <- FALSE
  if (isRstudio()) {
    rsAPIFn <- get(".rs.api.versionInfo", as.environment("tools:rstudio"))
    versionInfo <- rsAPIFn()
    if (!is.null(versionInfo)) {
      isRstudioServer <- identical("server", versionInfo$mode)
    }
  }
  isRstudioServer
}

isRstudio <- function() {
  isTRUE("tools:rstudio" %in% search())
}

installPackageVerbose <- function(verbose, verboseLevel = 1) {
  verbose >= verboseLevel && verbose < 5
}

RequireGitHubCacheFile <- function(pkgDT, filename) {
  theDir <- RequireGitHubCacheDir(create = TRUE)
  # checkPath(theDir, create = TRUE)
  destFile <- if(is.null(pkgDT$shas)) pkgDT$Branch else pkgDT$shas
  destFile <- paste0(pkgDT$Account, "_", pkgDT$Package, "_", destFile)
  destFile <- file.path(theDir, paste0(destFile, "_", filename))
}


rmEmptyFiles <- function(files, minSize = 100) {
  alreadyExists <- file.exists(files)
  if (any(alreadyExists)) {
    fs <- file.size(files[alreadyExists])
    tooSmall <- fs < minSize
    if (any(tooSmall %in% TRUE)) {
      unlink(files[alreadyExists[which(tooSmall)]])
      alreadyExists[alreadyExists] <- tooSmall %in% FALSE
    }
  }
  alreadyExists
}


GETWauthThenNonAuth <- function(url, token, verbose = getOption("Require.verbose")) {
  a <- httr::GET(url, httr::add_headers(Authorization = token))
  if (grepl("Bad credentials", a) || grepl("404", httr::http_status(a)$message)) {
    if (grepl("Bad credentials", a)) messageVerbose(red("Git credentials do not work for this url: ", url,
                                             "\nAre they expired?"), verbose = verbose)
    a <- httr::GET(url, httr::add_headers())
  }
  a
}



available.packagesWithCallingHandlers <- function(repo, type) {
  ignore_repo_cache <- FALSE
  for (attmpt in 1:2) {
    warns <- character()
    withCallingHandlers(
      out <- try(available.packages(repos = repo, type = type,
                                    ignore_repo_cache = ignore_repo_cache)),
      warning = function(w) {
        warns <<- w$message
        invokeRestart("muffleWarning")
      })
    SSLwarns <- grepl(.txtUnableToAccessIndex, warns)
    otherwarns <- grep(.txtUnableToAccessIndex, warns, invert = TRUE, value = TRUE)
    if (is(out, "try-error") || any(SSLwarns)) {
      # https://stackoverflow.com/a/76684292/3890027
      prevCurlVal <- Sys.getenv("R_LIBCURL_SSL_REVOKE_BEST_EFFORT")
      Sys.setenv(R_LIBCURL_SSL_REVOKE_BEST_EFFORT=TRUE)
      ignore_repo_cache <- TRUE
      on.exit({
        if (nzchar(prevCurlVal))
          Sys.setenv(R_LIBCURL_SSL_REVOKE_BEST_EFFORT = prevCurlVal)
        else
          Sys.unsetenv("R_LIBCURL_SSL_REVOKE_BEST_EFFORT")
      }, add = TRUE)
    } else {
      if (any(grepl("cannot open URL", warns)) && attmpt == 1) # seems to be transient esp with predictiveecology.r-universe.dev
        next
      if (length(otherwarns)) {
        warning(warns)
      }
      break
    }

  }
  out
}



masterOrMainFromGitRefs <- function(gitRefsSplit2) {
  br2 <- grep(unlist(gitRefsSplit2), pattern = "api.+heads/(master|main)", value = TRUE)
  br <- gsub(br2, pattern = ".+api.+heads.+(master|main).+", replacement = "\\1")
  br
}
