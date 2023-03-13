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
  set(pkgDT, NULL, "githubPkgName", extractPkgGitHub(pkgDT$packageFullName))
  isGH <- !is.na(pkgDT$githubPkgName)
  if (is.null(pkgDT$repoLocation)) {
    set(pkgDT, which(isGH), "repoLocation", "GitHub")
    set(pkgDT, which(!isGH), "repoLocation", "CRAN")
  }

  if (any(pkgDT$repoLocation == "GitHub")) {
    isGH <- pkgDT$repoLocation == "GitHub"
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
      # withCallingHandlers(set(pkgDT, hasSubFold, "RepoWBranch",
      #     gsub(paste0("^",pkgDT$Account[hasSubFold],"/"), "", pkgDT$fullGit[hasSubFold])),
      #     warning = function(w) browser())
      pkgDT[hasSubFolder %in% TRUE,
        GitSubFolder := strsplit(RepoWBranch, split = "/|@")[[1]][2],
        by = seq(sum(hasSubFolder, na.rm = TRUE))
      ]
      pkgDT[hasSubFolder %in% TRUE,
        RepoWBranch := gsub(paste0("/", GitSubFolder), "", RepoWBranch),
        by = seq(sum(hasSubFolder, na.rm = TRUE))
      ]

      # set(pkgDT, hasSubFold, "GitSubFolder",
      #     strsplit(pkgDT$RepoWBranch[hasSubFold], split = "/|@")[[1]][2])
      # set(pkgDT, hasSubFold, "RepoWBranch",
      #     gsub(paste0("/",pkgDT$GitSubFolder[hasSubFold]), "", pkgDT$RepoWBranch[hasSubFold]))
    }
    set(pkgDT, isGitHub, "Repo", gsub("^(.*)@(.*)$", "\\1", pkgDT$RepoWBranch[isGitHub]))
    set(pkgDT, isGitHub, "Branch", "HEAD")
    wh1 <- which(isGH & grepl("@", pkgDT$RepoWBranch))
    set(pkgDT, wh1, "Branch", gsub("^.*@(.*)$", "\\1", pkgDT$RepoWBranch[wh1]))
    set(pkgDT, NULL, c("RepoWBranch", "fullGit"), NULL)
  }
  pkgDT[]
}

#' @rdname DESCRIPTION-helpers
#' @param file A file path to a DESCRIPTION file
DESCRIPTIONFileVersionV <- function(file, purge = getOption("Require.purge", FALSE)) {
  if (is.null(.pkgEnv[["pkgDep"]][["DESCRIPTIONFile"]])) purge <- dealWithCache(purge, checkAge = FALSE)
  out <- lapply(file, function(f) {
    out <- if (!is.null(.pkgEnv[["pkgDep"]][["DESCRIPTIONFile"]])) {
      if (purge && length(f) == 1) suppressWarnings(rm(f, envir = .pkgEnv[["pkgDep"]][["DESCRIPTIONFile"]]))
      if (length(f) == 1) {
        get0(f, envir = .pkgEnv[["pkgDep"]][["DESCRIPTIONFile"]])
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
      assign(f, out, envir = .pkgEnv[["pkgDep"]][["DESCRIPTIONFile"]])
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
      lines <- readLines(f)
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
#' `getGitHubDESCRIPTION` retrieves the DESCRIPTION file from GitHub.com
#'
#' @rdname DESCRIPTION-helpers
#' @export
#' @param pkg A character string with a GitHub package specification (c.f. remotes)
#' @inheritParams pkgDep
#' @inheritParams Require
getGitHubDESCRIPTION <- function(pkg, purge = getOption("Require.purge", FALSE),
                                 verbose = getOption("Require.verbose")) {
  getGitHubFile(pkg, "DESCRIPTION", purge = purge, verbose = verbose)
}

#' @inheritParams Require
getGitHubNamespace <- function(pkg, purge = getOption("Require.purge", FALSE),
                               verbose = getOption("Require.verbose")) {
  getGitHubFile(pkg, "NAMESPACE", purge = purge, verbose = verbose)
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
getGitHubFile <- function(pkg, filename = "DESCRIPTION",
                          purge = getOption("Require.purge", FALSE),
                          verbose = getOption("Require.verbose")) {
  ret <- if (length(pkg) > 0) {
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
    pkgDT[repoLocation == "GitHub",
      url := {
        gitHubFileUrl(
          hasSubFolder = hasSubFolder, Branch = Branch, GitSubFolder = GitSubFolder,
          Account = Account, Repo = Repo, filename = filename
        )
      },
      by = "Package"
    ]

    checkPath(dirname(tempfile()), create = TRUE)
    set(
      pkgDT, NULL, "destFile",
      file.path(tempdir(), paste0(pkgDT$Package, "_", pkgDT$Branch, "_", filename))
    )
    if (isFALSE(getOption("Require.offlineMode", FALSE))) {
      pkgDT[repoLocation == "GitHub",
        filepath := {
          ret <- NA
          dl <- .downloadFileMasterMainAuth(unique(url)[1], unique(destFile)[1],
            need = "master",
            verbose = verbose, verboseLevel = 2
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
      if (identical("DESCRIPTION", filename)) {
        setnames(pkgDT, old = "filepath", new = "DESCFile")
      } else {
        setnames(pkgDT, old = "filepath", new = filename)
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
#' `archiveVersionsAvailable` searches CRAN Archives for available versions.
#' It has been borrowed from a sub-set of the code in a non-exported function:
#' `remotes:::download_version_url`
archiveVersionsAvailable <- function(package, repos) {
  info <- NULL
  for (repo in repos) {
    archiveFile <- sprintf("%s/src/contrib/Meta/archive.rds", repo)
    if (!exists(archiveFile, envir = .pkgEnv[["pkgDep"]], inherits = FALSE)) {
      archive <- tryCatch(
        {
          con <- gzcon(url(archiveFile, "rb"))
          on.exit(close(con))
          readRDS(con)
        },
        warning = function(e) {
          setOfflineModeTRUE()
          list()
        },
        error = function(e) list()
      )
      .pkgEnv[["pkgDep"]][[archiveFile]] <- archive
    } else {
      archive <- get(archiveFile, envir = .pkgEnv[["pkgDep"]])
    }
    info <- archive[package]
    naNames <- is.na(names(info))
    if (any(naNames)) {
      names(info)[naNames] <- package[naNames]
    }
    if (!is.null(info)) {
      info <- lapply(info, function(x) {
        x$repo <- repo
        x
      })
    }
  }
  return(info)
}

getPkgDeps <- function(packages, which, purge = getOption("Require.purge", FALSE)) {
  pkgs <- trimVersionNumber(packages)
  out1 <- pkgDep(packages,
    recursive = TRUE, which = which, purge = purge,
    includeSelf = FALSE
  )
  out1 <- unique(unname(unlist(out1)))
  out2 <- c(out1, pkgs)
  out3 <- c(out1, packages)
  dt <- data.table(
    github = extractPkgGitHub(out2), Package = extractPkgName(out2),
    depOrOrig = c(rep("dep", length(out1)), rep("orig", length(packages))),
    packageFullName = out3
  )
  set(dt, NULL, "origOrder", seq_along(dt$github))
  dt[, bothDepAndOrig := length(depOrOrig) > 1, by = "Package"]
  dt[bothDepAndOrig == TRUE, depOrOrig := "both"]


  if ("github" %in% colnames(dt)) {
    setorderv(dt, na.last = TRUE, "github")
  } # keep github packages up at top -- they take precedence
  setorderv(dt, "origOrder")
  ret <- dt$packageFullName
  if (!is.null(names(packages))) {
    dt[depOrOrig == "orig", Names := names(packages)[match(packageFullName, packages)]]
    dt[is.na(Names), Names := ""]
    names(ret) <- dt$Names
  }
  ret
}



#' @importFrom utils packageVersion installed.packages
installedVers <- function(pkgDT) {
  pkgDT <- toPkgDT(pkgDT)
  if (NROW(pkgDT)) {
    ip <- as.data.table(installed.packages(fields = c("Package", "LibPath", "Version")))
    # ip <- ip[, c("Package", "LibPath", "Version")]
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
          if (identical(SysInfo[["user"]], "emcintir")) browser() else stop("Error number 234; please contact developers")
        }
        ip[!is.na(VersionFromPV), Version := VersionFromPV]
      }
    }
    ip <- ip[, c("Package", "LibPath", "Version")]
    ip <- unique(ip, by = c("Package")) # , "LibPath" # basically, only take the first one if 2 installed in LibPath
    pkgDT <- try(ip[pkgDT, on = "Package"], silent = TRUE)
    if (is(pkgDT, "try-error")) {
      if (identical(SysInfo[["user"]], "emcintir")) browser() else stop("Error number 123; please contact developers")
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
  if (isFALSE(getOption("Require.offlineMode", FALSE))) {
    repos <- getCRANrepos(repos)
    purge <- dealWithCache(purge = purge)
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

  reposShort <- paste(substr(unlist(lapply(strsplit(repos, "//"), function(x) x[[2]])), 1, 20), collapse = "_")
  typesShort <- paste(unlist(lapply(strsplit(types, "//"), function(x) x[[1]])), collapse = "_")
  objNam <- paste0("availablePackages", "_", reposShort, "_", typesShort)
  if (!exists(objNam, envir = .pkgEnv[["pkgDep"]]) || isTRUE(purge)) {
    for (type in types) {
      fn <- availablePackagesCachedPath(repos, type)
      if (isTRUE(purge)) {
        unlink(fn)
      }
      if (file.exists(fn)) {
        cap[[type]] <- readRDS(fn)
      } else {
        caps <- lapply(repos, function(repo) {
          tryCatch(available.packages(repos = repo, type = type),
            error = function(x) {
              browserDeveloper("Error 7531; please see developer")
              available.packages(ignore_repo_cache = TRUE, repos = repo, type = type)
            }
          )
        })
        caps <- lapply(caps, as.data.table)
        caps <- unique(rbindlist(caps), by = c("Package", "Version"))
        cap[[type]] <- caps

        if (!is.null(getOptionRPackageCache())) {
          checkPath(dirname(fn), create = TRUE)
          saveRDS(cap[[type]], file = fn)
        }
      }
    }
    cap <- do.call(rbind, cap)
    # cap <- cap[!duplicated(cap, by = "Package")] # This will keep only one copy if type = "both"

    assign(objNam, cap, envir = .pkgEnv[["pkgDep"]])
    out <- cap
  } else {
    out <- get(objNam, envir = .pkgEnv[["pkgDep"]], inherits = FALSE)
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
    isBin <- startsWith(prefix = repoToTest, curCRANRepo)
  }
  isBin
}


installRequire <- function(requireHome = getOption("Require.Home"),
                           verbose = getOption("Require.verbose")) {
  Rpath <- Sys.which("R")
  dFileAtInstalledRequire <- file.path(.libPaths()[1], "Require", "DESCRIPTION")
  haveIt <- file.exists(dFileAtInstalledRequire)
  installedRequireV <- if (haveIt) DESCRIPTIONFileVersionV(dFileAtInstalledRequire) else NULL
  isGitHub <- if (haveIt) DESCRIPTIONFileOtherV(dFileAtInstalledRequire, other = "github") else NA
  done <- FALSE
  if (is.na(isGitHub)) {
    if (!is.null(requireHome)) {
      dFile <- dir(requireHome, pattern = "DESCRIPTION", full.names = TRUE)
      pkgNameAtRequireHome <- DESCRIPTIONFileOtherV(dFile, "Package")
      pkgVersionAtRequireHome <- DESCRIPTIONFileVersionV(dFile)
      if (!is.null(pkgNameAtRequireHome)) {
        if (identical(pkgNameAtRequireHome, "Require")) {
          if (!identical(installedRequireV, pkgVersionAtRequireHome)) {
            origDir <- setwd(dirname(dirname(dFile)))
            on.exit(setwd(origDir), add = TRUE)
            messageVerbose("Installing Require ver: ", pkgVersionAtRequireHome, " from source at ", requireHome,
              verbose = verbose, verboseLevel = 2
            )
            out <- system(paste0(Rpath, " CMD INSTALL --no-multiarch --library=", .libPaths()[1], " Require"),
              wait = TRUE, ignore.stdout = TRUE, intern = TRUE, ignore.stderr = TRUE
            )
          }
        }
        done <- TRUE
      } else {
        if (!is.null(requireHome)) {
          messageVerbose(pkgNameAtRequireHome, " did not contain Require source code",
            verbose = verbose, verboseLevel = 2
          )
        }
      }
    }

    if (isFALSE(done)) {
      Rpath <- Sys.which("Rscript")
      system(paste0(
        Rpath, " -e \"install.packages(c('Require'), lib ='", .libPaths()[1],
        "', quiet = TRUE, repos = '", getOption("repos")[["CRAN"]], "')\""
      ), wait = TRUE)
      done <- TRUE
    }
  } else {
    stop("Require will need to be installed manually in", .libPaths()[1])
  }
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
#' tried, but failed, `3` it was in the search path and was detached
#' and unloaded.
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
  si <- sessionInfo()
  allLoaded <- c(names(si$otherPkgs), names(si$loadedOnly))
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
      "These will not be unloaded: ", paste(dontTryExtra, collapse = ", "),
      verbose = verbose, verboseLevel = 2
    )
    dontTry <- c(dontTry, dontTryExtra)
  }

  dontTry <- unique(c(c("Require", "data.table", "covr"), dontTry))
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
    detached <- sapply(pkgs, unloadNamespace)
    detached <- sapply(detached, is.null)
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
  detached[detached] <- 2
  detached[!detached] <- 1
  detached[is.na(detached)] <- 3
  detached
}

isWindows <- function() {
  tolower(SysInfo["sysname"]) == "windows"
}

isMacOSX <- function() {
  isMac <- tolower(SysInfo["sysname"]) == "darwin"
}

warningCantInstall <- function(pkgs) {
  warning(
    "Can't install ", pkgs, "; you will likely need to restart R and run:\n",
    "-----\n",
    "install.packages(c('", paste(pkgs, collapse = ", "), "'), lib = '", .libPaths()[1], "')",
    "\n-----\n...before any other packages get loaded"
  )
}

rpackageFolder <- function(path = getOptionRPackageCache(), exact = FALSE) {
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
      if (interactive() && !endsWith(path, rversion())) {
        ## R CMD check on R >= 4.2 sets libpaths to use a random tmp dir
        ## need to know if it's a user, who *should* keep R-version-specific dirs
        file.path(path, rversion())
      } else {
        path
      }
    }
  } else {
    NULL
  }
}

checkLibPaths <- function(libPaths, ifMissing, exact = FALSE) {
  if (missing(libPaths)) {
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
  whGitHub <- which(pkgInstall$repoLocation %in% "GitHub")
  if (length(whGitHub)) {
    pkgGitHub <- pkgInstall[whGitHub]
    pkgGitHub[,
      {
        file <- file.path(libPaths[1], Package, "DESCRIPTION")
        txt <- readLines(file)

        dups <- duplicated(vapply(strsplit(txt, split = "\\:"),
                          function(x) x[[1]], FUN.VALUE = character(1)))
        if (any(dups)) {
          if (all(grepl("Github|Remote", txt[dups])))
            browserDeveloper(paste0("Error 7456; Mostly likely this indicates that ",
                             "the DESCRIPTION file at ", file,
                             " has duplicated RemoteHost to GithubSHA1. Try to remove ",
                             "the first set"))
        }


        beforeTheseLines <- grep("NeedsCompilation:|Packaged:|Author:", txt)
        insertHere <- min(beforeTheseLines)
        sha <- SHAonGH
        newTxt <-
          paste0("RemoteType: github
RemoteHost: api.github.com
RemoteRepo: ", Package, "
RemoteUsername: ", Account, "
RemoteRef: ", Branch, "
RemoteSha: ", sha, "
GithubRepo: ", Package, "
GithubUsername: ", Account, "
GithubRef: ", Branch, "
GithubSHA1: ", sha, "")
        newTxt <- strsplit(newTxt, split = "\n")[[1]]
        newTxt <- gsub("^ +", "", newTxt)
        txtOut <- c(txt[seq(insertHere - 1)], newTxt, txt[insertHere:length(txt)])
        cat(txtOut, file = file, sep = "\n")
      },
      by = seq(NROW(pkgGitHub))
    ]
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
    })) # unique(dirname(d))[1])
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
      } # else {
      #   badToChange <- if (!isFALSE(subFolder)) {
      #     file.path(actualFolderName, subFolder)
      #   } else {
      #     basename(bad)
      #   }
      #   newName <- gsub(badToChange, pkgName, bad)
      #   fileRenameOrMove(bad, newName) # it was downloaded with a branch suffix
      #   newName
      # }

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

  shaPath <- file.path("https://api.github.com/repos", acct, repo, "git", "refs")
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
  tf <- tempfile()
  .downloadFileMasterMainAuth(shaPath, destfile = tf, need = "master")
  sha <- try(suppressWarnings(readLines(tf)), silent = TRUE)
  if (is(sha, "try-error")) {
    return(sha)
  }
  if (length(sha) > 1) {
    # Seems to sometimes come out as individual lines; sometimes as one long concatenates string
    #   Was easier to collapse the individual lines, then re-split
    sha <- paste(sha, collapse = "")
  }
  sha1 <- strsplit(sha, "},")[[1]] # this splits onto separate lines

  sha2 <- strsplit(sha1, ":")

  if (any(grepl("master|main|HEAD", unlist(br)))) {
    br2 <- grep(unlist(sha2), pattern = "api.+heads/(master|main)", value = TRUE)
    br <- gsub(br2, pattern = ".+api.+heads.+(master|main).+", replacement = "\\1")
  }
  for (branch in br) { # will be length 1 in most cases except master/main
    whHasBr <- which(vapply(sha2, function(xx) {
      any(grepl(paste0(".+refs/.+/+", branch, "\""), xx))
    }, FUN.VALUE = logical(1)))
    if (length(whHasBr) > 0) {
      break
    }
  }
  if (length(whHasBr) == 0) {
    stop("Can't find ", br, " on GitHub repo ", paste0(acct, "/", repo), "; \n -- does it exist? --")
  }

  sha3 <- sha2[[whHasBr]]
  shaLine <- grep("sha", sha3) + 1
  shaLine <- strsplit(sha3[shaLine], ",")[[1]][1]
  sha <- gsub(" *[[:punct:]]+(.+)[[:punct:]] *", "\\1", shaLine)
  sha
}

getSHAfromGitHubMemoise <- function(...) {
  if (getOption("Require.useMemoise", TRUE)) {
    dots <- list(...)
    if (!exists(getSHAfromGitHubObjName, envir = .pkgEnv, inherits = FALSE)) {
      loadGitHubSHAsFromDisk(verbose = dots$verbose) # puts it into the Memoise-expected location
    if (!exists(getSHAfromGitHubObjName, envir = .pkgEnv, inherits = FALSE))
        .pkgEnv[[getSHAfromGitHubObjName]] <- new.env()
    }
    ret <- NULL
    ss <- match.call(definition = getSHAfromGitHub)
    uniqueID <- paste(lapply(ss[-1], eval, envir = parent.frame()), collapse = "_")
    if (!exists(uniqueID, envir = .pkgEnv[[getSHAfromGitHubObjName]], inherits = FALSE)) {
      .pkgEnv[[getSHAfromGitHubObjName]][[uniqueID]] <- list()
    } else {
      whIdent <- unlist(lapply(.pkgEnv[[getSHAfromGitHubObjName]][[uniqueID]], function(x) identical(x$input, dots)))
      if (any(whIdent)) {
        ret <- .pkgEnv[[getSHAfromGitHubObjName]][[uniqueID]][[which(whIdent)]]$output
      }
    }
    if (is.null(ret)) { # Case where it doesn't exist in .pkgEnv
      inputs <- data.table::copy(dots)
      ret <- getSHAfromGitHub(...)
      # Add it to the .pkgEnv
      newObj <- list(.pkgEnv[[getSHAfromGitHubObjName]][[uniqueID]], list(input = inputs, output = ret))
      .pkgEnv[[getSHAfromGitHubObjName]][[uniqueID]] <- newObj

    }
  } else {
    ret <- getSHAfromGitHub(...)
  }

  return(ret)
}

loadGitHubSHAsFromDisk <- function(verbose = getOption("Require.verbose")) {
  ret <- list()
  if (!exists(getSHAfromGitHubObjName, envir = .pkgEnv, inherits = FALSE)) {
    fn <- getSHAFromGitHubDBFilename()
    if (isTRUE(file.exists(fn))) {
      out <- readRDS(fn)
      removeFile <- purgeBasedOnTimeSinceCached(out[[GitHubSHAonDiskCacheTime]])
      if (!removeFile) { # remove if 24 hours old
        # if (!exists(getSHAfromGitHubObjName, envir = .pkgEnv, inherits = FALSE))
        .pkgEnv[[getSHAfromGitHubObjName]] <- new.env()
        list2env(out, envir = .pkgEnv[[getSHAfromGitHubObjName]])
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
  if (exists(getSHAfromGitHubObjName, envir = .pkgEnv, inherits = FALSE)) {
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
  as.list(.pkgEnv[[getSHAfromGitHubObjName]])
}

getSHAfromGitHubObjName <- "getSHAfromGitHub"

getSHAFromGitHubDBFilename <- function() {
  go <- getOptionRPackageCache()
  if (!is.null(go))
    out <- file.path(go, paste0(getSHAfromGitHubObjName, ".rds")) # returns NULL if no Cache used
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

rversion <- function() {
  paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1])
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
  if (isFALSE(getOption("Require.offlineMode", FALSE))) {
    if (getOption("Require.checkInternet", FALSE)) {
      internetMightExist <- TRUE
      if (!is.null(.pkgEnv$internetExistsTime)) {
        if ((Sys.time() - getOption("Require.internetExistsTimeout", 30)) < .pkgEnv$internetExistsTime) {
          internetMightExist <- FALSE
        }
      }
      if (internetMightExist) {
        opts2 <- options(timeout = 2)
        on.exit(options(opts2))
        ue <- .pkgEnv$internetExists <- urlExists("https://www.google.com")
        if (isFALSE(ue)) {
          internetMightExist <- FALSE

          messageVerbose("\033[32mInternet does not appear to exist; proceeding anyway\033[39m",
            verbose = verbose, verboseLevel = 2
          )
        }
        .pkgEnv$internetExistsTime <- Sys.time()
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

# tryInstallAgainWithoutAPCache <- function(installPackagesQuoted, envir = parent.frame()) {
#   nameOfEnvVari <- "R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE"
#   prevCacheExpiry <- Sys.getenv(nameOfEnvVari)
#   val <- 0
#   val <- setNames(list(val), nm = nameOfEnvVari)
#   do.call(Sys.setenv, val)
#   prevCacheExpiry <- setNames(list(prevCacheExpiry), nm = nameOfEnvVari)
#   on.exit(do.call(Sys.setenv, prevCacheExpiry), add = TRUE)
#   out <- eval(installPackagesQuoted, envir = envir)
# }


installPackagesSystem <- function(pkg, args, libPath) {
  opts2 <- append(args, list(lib = normalizePath(libPath, winslash = "/")))
  opts2 <- modifyList2(list(Ncpus = getOption("Ncpus")), opts2, keep.null = TRUE)
  opts2 <- append(list(pkg), opts2)
  opts2 <- append(opts2, list(repos = NULL))
  theCharacters <- unlist(lapply(opts2, is.character))
  theCharactersAsVector <- lengths(opts2[theCharacters]) > 1
  inner <- unlist(lapply(opts2[theCharacters][theCharactersAsVector], function(x) paste(x, collapse = "', '")))
  aa <- paste0("c('", inner, "')")
  opts2[theCharacters][!theCharactersAsVector] <- paste0("'", opts2[theCharacters][!theCharactersAsVector], "'")
  opts2[theCharacters][theCharactersAsVector] <- aa
  hasName <- names(opts2) != ""
  Rpath <- Sys.which("Rscript")
  out2 <- paste(
    Rpath, "-e \"do.call(install.packages, list(",
    paste(
      opts2[!hasName], ", ",
      paste(names(opts2)[hasName],
        sep = " = ", opts2[hasName],
        collapse = ", "
      ), "))\""
    )
  )
  out <- system(out2, intern = TRUE)
  return(out)
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
#' @return
#' This is called for its side effect, namely, the same as `utils::download.file`, but
#' using a `GITHUB_PAT`, it if is in the environment, and trying both `master` and
#' `main` if the actual `url` specifies either `master` or `main` and it does not exist.
#' @export
.downloadFileMasterMainAuth <- function(url, destfile, need = "HEAD",
                                       verbose = getOption("Require.verbose"), verboseLevel = 2) {
  hasMasterMain <- grepl(masterMainGrep, url)
  url <- masterMainHEAD(url, need)

  # Authentication
  ghp <- Sys.getenv("GITHUB_PAT")
  messageGithubPAT(ghp, verbose = verbose, verboseLevel = 0)
  if (nzchar(ghp)) {
    url <- sprintf(paste0("https://%s:@", gsub("https://", "", url)), ghp)
  }

  urls <- url
  urls <- split(urls, hasMasterMain)
  outNotMasterMain <- outMasterMain <- character()

  for (i in 1:5) {
    if (!is.null(urls[["FALSE"]])) {
      outNotMasterMain <-
        withCallingHandlers(
          Map(URL = urls[["FALSE"]], df = destfile, function(URL, df) {
            if (isFALSE(getOption("Require.offlineMode", FALSE))) {
              try(download.file(URL, destfile = destfile, quiet = TRUE), silent = TRUE)
            }
          }),
          warning = function(w) {
            setOfflineModeTRUE()
            # strip the ghp from the warning message
            w$message <- gsub(paste0(ghp, ".*@"), "", w$message)
            invokeRestart("muffleWarning")
          }
        )
    }
    if (!is.null(urls[["TRUE"]])) { # should be sequential because they are master OR main
      for (wh in seq(urls[["TRUE"]])) {
        if (isFALSE(getOption("Require.offlineMode", FALSE))) {
          outMasterMain <-
            withCallingHandlers(
              {
                try(download.file(urls[["TRUE"]][wh], destfile = destfile[wh], quiet = TRUE))
              },
              warning = function(w) {
                setOfflineModeTRUE()
                # strip the ghp from the warning message
                w$message <- gsub(paste0(ghp, ".*@"), "", w$message)
                invokeRestart("muffleWarning")
              }
            )
        }

        if (!is(outMasterMain, "try-error")) {
          names(outMasterMain) <- urls[["TRUE"]][wh]
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
}

messageGithubPAT <- function(ghp, verbose = verbose, verboseLevel = 0) {
  if (nzchar(ghp)) {
    if (is.null(.pkgEnv$hasGHP)) {
      .pkgEnv$hasGHP <- TRUE
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
  out <- gsub(".+\u2018(.+)_.+\u2019.+", "\\1", x) # those two escape characters are the inverted commas
  gsub(".+\u2018(.+)\u2019.+", "\\1", out) # package XXX is in use and will not be installed
}

availablePackagesCachedPath <- function(repos, type) {
  file.path(RequirePkgCacheDir(), gsub("https|[:/]", "", repos[1]), type, "availablePackages.rds")
}

installPackagesWithQuiet <- function(ipa) {
  if (isTRUE(ipa$quiet)) {
    messSupp2 <- capture.output({
      messSupp <- capture.output(type = "message", {
        do.call(install.packages, ipa)
      })
    })
  } else {
    do.call(install.packages, ipa)
  }
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
  if (any(hasSubFolder)) {
    Branch <- paste0(Branch, "/", GitSubFolder)
  }
  file.path("https://raw.githubusercontent.com", Account, Repo, Branch, filename, fsep = "/")
}


setOfflineModeTRUE <- function() {
  if (isFALSE(getOption("Require.offlineMode", FALSE))) {
    if (!internetExists()) {
      options(
        "Require.offlineMode" = TRUE,
        "Require.offlineModeSetAutomatically" = TRUE
      )
      message("Internet appears to be unavailable; setting options('Require.offlineMode' = TRUE)")
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

messageCantInstallNoVersion <- function(packagesFullName) {
  turquoise(
    paste(unique(packagesFullName), collapse = ", "),
    " could not be installed; the version specification cannot be met"
  )
}
