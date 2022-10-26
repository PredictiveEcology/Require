utils::globalVariables(c(
  c("..colsKeep", "..colsToNAfill", ".I", ".N", "Archs", "AvailableVersion",
    "correctVersion", "dayAfterPutOnCRAN", "DepVersion", "destFile", "dup",
    "filepath", "github", "groupCRANtogether", "groupCRANtogetherChange",
    "groupCRANtogetherDif", "hasHEAD", "hasVersionSpec", "i.neededFiles", "inequality",
    "installFromFac", "installOrder", "installResult", "isGitPkg",
    "keep", "keep2", "lastRow", "localFileName", "localType", "maxVers",
    "mtime", "N", "Names", "neededFiles", "needLaterDate", "nextRow",
    "Package", "packageFullName", "repoLocation", "tmpOrder", "type",
    "version", "violations", "VersionFromPV", "..removeCols", "compareVersionAvail")
))

#' @details
#' `parseGitHub` turns the single character string representation into 3 or 4:
#' `Account`, `Repo`, `Branch`, `SubFolder`.
#'
#' @return
#' `parseGitHub` returns a `data.table` with added columns.
#'
#' @export
#' @rdname GitHubTools
#' @inheritParams Require
parseGitHub <- function(pkgDT, verbose = getOption("Require.verbose")) {
  ghp <- Sys.getenv("GITHUB_PAT")
  pkgDT <- toPkgDT(pkgDT)
  set(pkgDT, NULL, "githubPkgName", extractPkgGitHub(pkgDT$packageFullName))
  # pkgDT[, githubPkgName := extractPkgGitHub(packageFullName)]
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
      set(pkgDT, hasSubFold, "RepoWBranch",
          gsub(paste0("^",pkgDT$Account[hasSubFold],"/"), "", pkgDT$fullGit[hasSubFold]))
      set(pkgDT, hasSubFold, "GitSubFolder",
          strsplit(pkgDT$RepoWBranch[hasSubFold], split = "/|@")[[1]][2])
      set(pkgDT, hasSubFold, "RepoWBranch",
          gsub(paste0("/",pkgDT$GitSubFolder[hasSubFold]), "", pkgDT$RepoWBranch[hasSubFold]))

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
    if (length(f) == 1)
      assign(f, out, envir = .pkgEnv[["pkgDep"]][["DESCRIPTIONFile"]])
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
      lines <- readLines(f);
    } else {
      lines <- f
    }
    suppressWarnings({
      vers_line <- lines[grep(paste0("^",other,": *"), lines)]
    })
    out <- gsub(paste0(other, ": "), "", vers_line)
    if (length(out) == 0) out <- NA
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
    if (!is.null(cn))
      needsParse <- !all(c("hasSubFolder", "Repo", "Branch", "Account") %in% cn)
    if (needsParse) {
      pkg <- pkgDTtoPackageFullName(pkg)
      pkgDT <- parseGitHub(pkg, verbose = verbose)
    } else {
      pkgDT <- pkg
    }
    pkgDT[repoLocation == "GitHub",
          url := {
            gitHubFileUrl(hasSubFolder = hasSubFolder, Branch = Branch, GitSubFolder = GitSubFolder,
                          Account = Account, Repo = Repo, filename = filename)
            # if (any(hasSubFolder)) {
            #   Branch <- paste0(Branch, "/", GitSubFolder)
            # }
            # file.path("https://raw.githubusercontent.com", Account, Repo, Branch, filename, fsep = "/")
          },
          by = "Package"]

    checkPath(dirname(tempfile()), create = TRUE)
    set(pkgDT, NULL, "destFile",
        file.path(tempdir(), paste0(pkgDT$Package, "_", pkgDT$Branch, "_", filename)))
    if (internetExists("cannot download GitHub package", verbose = verbose)) {
      pkgDT[repoLocation == "GitHub",
            filepath := {
              ret <- NA
              dl <- downloadFileMasterMainAuth(unique(url)[1], unique(destFile)[1], need = "master",
                                               verbose = verbose, verboseLevel = 2)
              ret <- if (!is(dl, "try-error")) {
                destFile
              } else {
                NA
              }

              ret
            }, by = c("Package", "Branch")]
      if (identical("DESCRIPTION", filename))
        setnames(pkgDT, old = "filepath", new = "DESCFile")
      else
        setnames(pkgDT, old = "filepath", new = filename)
    }
    pkgDT[]
  } else {
    pkg
  }
  ret
}



#' @details
#' `doLoading` is a wrapper around `require`.
#'
#' @export
#' @importFrom utils capture.output
#' @inheritParams Require
#' @rdname Require-internals
doLoading <- function(pkgDT, require = TRUE, verbose = getOption("Require.verbose"), ...) {
  pkgDTForLoad <- pkgDT[loadOrder > 0]
  packages <- pkgDTForLoad$Package
  packageOrder <- pkgDTForLoad$loadOrder
  if (NROW(pkgDTForLoad) && !isFALSE(require)) {
    names(packages) <- packages
    packages <- packages[order(packageOrder)]
    requireOut <- lapply(packages, require, character.only = TRUE)

    pkgDT[, loaded := (pkgDT$Package %in% names(requireOut)[unlist(requireOut)] & loadOrder > 0)]
  }
  pkgDT[]
}

#' @rdname Require-internals
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
      archive <- tryCatch({
        con <- gzcon(url(archiveFile, "rb"))
        on.exit(close(con))
        readRDS(con)
      }, warning = function(e) list(), error = function(e) list())
      .pkgEnv[["pkgDep"]][[archiveFile]] <- archive
    } else {
      archive <- get(archiveFile, envir = .pkgEnv[["pkgDep"]])
    }
    info <- archive[package]
    naNames <- is.na(names(info))
    if (any(naNames))
      names(info)[naNames] <- package[naNames]
    if (!is.null(info)) {
      info <- lapply(info, function(x) {
        x$repo <- repo
        x
      })
    }
  }
  return(info)
}

#' GitHub specific helpers
#'
#' `installGitHub` is a vectorized `installGithubPackages`.
#' This will attempt to identify all dependencies of all supplied packages first,
#' then load the packages in the correct order so that each of their dependencies
#' are met before each is installed.
#'
#' @param pkgDT A character string with full package names or a `data.table`
#'   with at least 2 columns `"Package"` and `"packageFullName"`.
#' @param toInstall DESCRIPTION NEEDED
#' @param install_githubArgs Any arguments passed to `install_github`
#' @param dots A list of ..., e.g., list(...). Only for internal use.
#'
#' @return
#' `installGitHub` returns a named character vector indicating packages
#'   successfully installed, unless the word "Failed" is returned, indicating
#'   installation failure. The names will be the full GitHub package name,
#'   as provided to `gitPkgNames` in the function call.
#'
#' @export
#' @rdname GitHubTools
#'
#' @examples
#' \dontrun{
#'   installGitHub(c("PredictiveEcology/Require", "PredictiveEcology/quickPlot"))
#' }
#'
installGitHub <- function(pkgDT, toInstall, install_githubArgs = list(), dots = dots,
                          verbose = getOption("Require.verbose")) {

  pkgDT <- toPkgDT(pkgDT)
  toInstall <- toInstall[installFrom == "GitHub"]

  # Require doesn't actually install a previous version of a Git package at this point,
  #    it just takes the HEAD
  if (is.null(dots$dependencies) && is.null(install_githubArgs$dependencies))
    dots$dependencies <- NA # This is NA, which under normal circumstances should be irrelevant
  #  but there are weird cases where the internals of Require don't get correct
  #  version of dependencies e.g., achubaty/amc@development says "reproducible" on CRAN
  #  which has R.oo
  installPkgNames <- toInstall$packageFullName
  names(installPkgNames) <- toInstall$Package
  ord <- match(extractPkgName(installPkgNames), toInstall$Package)
  toInstall <- toInstall[ord]
  installPkgNames <- installPkgNames[ord]

  gitPkgs <- trimVersionNumber(toInstall$packageFullName)
  names(gitPkgs) <- toInstall$Package
  isTryError <- unlist(lapply(gitPkgs, is, "try-error"))
  attempts <- rep(0, length(gitPkgs))
  names(attempts) <- gitPkgs
  if (length(gitPkgs)) {
    gitPkgsToInstall <- gitPkgs[unlist(lapply(seq_along(gitPkgs), function(ind) {
      all(!extractPkgName(names(gitPkgs))[-ind] %in% extractPkgName(gitPkgs[[ind]]))
    }))]
    gitPkgDepOrig <- gitPkgsToInstall
    gitPkgNamesSimple <- extractPkgName(gitPkgsToInstall)
    ipa <- modifyList2(install_githubArgs, dots, keep.null = TRUE)
    ipa <- modifyList2(ipa, list(verbose = verbose), keep.null = TRUE)
    warns <- messes <- errors <- list()

    if (NROW(gitPkgsToInstall)) {
      ipa <- append(list(gitPkgsToInstall), ipa)
      do.call(installGithubPackage, ipa)
    }

    pkgDT
  }
  pkgDT
}

getPkgDeps <- function(packages, which, purge = getOption("Require.purge", FALSE)) {
  pkgs <- trimVersionNumber(packages)
  out1 <- pkgDep(packages, recursive = TRUE, which = which, purge = purge,
                 includeSelf = FALSE)
  out1 <- unique(unname(unlist(out1)))
  out2 <- c(out1, pkgs)
  out3 <- c(out1, packages)
  dt <- data.table(github = extractPkgGitHub(out2), Package = extractPkgName(out2),
                   depOrOrig = c(rep("dep", length(out1)), rep("orig", length(packages))),
                   packageFullName = out3)
  set(dt, NULL, "origOrder", seq_along(dt$github))
  dt[, bothDepAndOrig := length(depOrOrig) > 1, by = "Package"]
  dt[bothDepAndOrig == TRUE, depOrOrig := "both"]


  if ("github" %in% colnames(dt))
    setorderv(dt, na.last = TRUE, "github") # keep github packages up at top -- they take precedence
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

    ip <- as.data.table(installed.packages())[]
    ip <- ip[, c("Package", "LibPath", "Version")]
    ip <- ip[Package %in% pkgDT$Package]

    if (NROW(ip)) {
      pkgs <- pkgDT$Package
      names(pkgs) <- pkgDT$packageFullName
      ln <- loadedNamespaces()
      ln <- ln[!ln %in% .basePkgs]
      # Need both the next lines
      pkgs <- pkgs[pkgs %in% ln]
      pkgs <- pkgs[pkgs %in% ip$Package] # can be loadedNamespace, but not installed, if it had been removed in this session
      if (NROW(pkgs)) {
        installedPkgsCurrent <- lapply(pkgs, function(x) {
          pv <- try(as.character(numeric_version(packageVersion(x))), silent = TRUE)
          if (is(pv, "try-error")) {
            pv <- NA_character_
          }
          data.table(VersionFromPV = pv)
        } )
        installedPkgsCurrent <- rbindlist(lapply(installedPkgsCurrent, as.data.table), idcol = "packageFullName")
        set(installedPkgsCurrent, NULL, "Package", extractPkgName(installedPkgsCurrent$packageFullName))
        installedPkgsCurrent <- unique(installedPkgsCurrent, by = c("Package", "VersionFromPV"))
        ip <- try(installedPkgsCurrent[ip, on = "Package"])
        if (is(ip, "try-error")) stop("Error number 234; please contact developers")
        ip[!is.na(VersionFromPV), Version := VersionFromPV]
      }
    }
    ip <- ip[, c("Package", "LibPath", "Version")]
    ip <- unique(ip, on = c("Package", "LibPath"))
    pkgDT <- try(ip[pkgDT, on = "Package"], silent = TRUE)
    if (is(pkgDT, "try-error")) stop("Error number 123; please contact developers")

  } else {
    pkgDT <- cbind(pkgDT, LibPath = NA_character_, "Version" = NA_character_)
  }
  installed <- !is.na(pkgDT$Version)
  if (any(installed))
    set(pkgDT, NULL, "installed", installed)
  pkgDT[]
}

#' @importFrom utils available.packages
#' @inheritParams Require
available.packagesCached <- function(repos, purge, verbose = getOption("Require.verbose"),
                                     returnDataTable = TRUE) {
  if (internetExists("cannot get available packages", verbose = verbose)) {
    repos <- getCRANrepos(repos)
    purge <- dealWithCache(purge = purge)

    cap <- list()
    isMac <- tolower(Sys.info()["sysname"]) == "darwin"
    isOldMac <- isMac && compareVersion(as.character(getRversion()), "4.0.0") < 0
    isWindows <- isWindows()

    types <- if (isOldMac) {
      c("mac.binary.el-capitan", "source")
    } else if (!isWindows && !isMac) {
      c("source")
    } else {
      c("binary", "source")
    }

    reposShort <- paste(substr(unlist(lapply(strsplit(repos, "//"), function(x) x[[2]])), 1, 20), collapse = "_")
    typesShort <- paste(unlist(lapply(strsplit(types, "//"), function(x) x[[1]])), collapse = "_")
    objNam <- paste0("availablePackages", "_", reposShort, "_", typesShort)
    if (!exists(objNam, envir = .pkgEnv[["pkgDep"]]) || isTRUE(purge)) {
      for (type in types) {
        fn <- availablePackagesCachedPath(repos, type)
        if (isTRUE(purge))
          unlink(fn)
        if (file.exists(fn)) {
          cap[[type]] <- readRDS(fn)
        } else {
          cap[[type]] <- tryCatch(available.packages(repos = repos, type = type),
                                  error = function(x)
                                    available.packages(ignore_repo_cache = TRUE, repos = repos, type = type))
          # cap[[type]] <-  cap[[type]]

          cap[[type]] <- as.data.table(cap[[type]])

          if (!is.null(getOptionRPackageCache())) {
            checkPath(dirname(fn), create = TRUE)
            saveRDS(cap[[type]], file = fn)
          }
        }


      }
      cap <- do.call(rbind, cap)

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


  } else {
    out <- NULL
  }
  return(out)
}



#' @inheritParams Require
currentCRANPkgDates <- function(pkgs, verbose = getOption("Require.verbose")) {
  if (!exists("currentCranDates", envir = .pkgEnv[["pkgDep"]])) {
    messageVerbose("Getting dates of current CRAN packages",
                   verbose = verbose, verboseLevel = 1)
    tf <- tempfile();
    cranRepoHttp <- getOption("repos")["CRAN"]
    for (i in 1:2) {
      out <- suppressWarnings(try(download.file(file.path(cranRepoHttp, "src/contrib/"), tf, quiet = TRUE), silent = TRUE))
      if (is(out, "try-error"))
        if (i == 1) {
          cranRepoHttp <- paste0("https", "://", paste(c("cloud", "r-project", "org"), collapse = "."), "/")
        } else {
          stop(out, "Download from CRAN failed momentarily. Please try again shortly")
        }
    }

    currentCranDates <- readLines(tf)
    assign("currentCranDates", currentCranDates, envir = .pkgEnv[["pkgDep"]])
  } else {
    currentCranDates <- get("currentCranDates", envir = .pkgEnv[["pkgDep"]])
  }
  if (is.null(names(pkgs))) names(pkgs) <- pkgs

  aa <- gsub(" *<a href=\"", "", currentCranDates)
  bb <- unlist(lapply(paste0(pkgs, "_"), function(p) which(startsWith(aa, p))))
  currentCranDates2 <- aa[bb]

  # There are at least 2 formats that come from CRAN; this covers 2 of them
  dd2 <- gsub(paste0(".+([0-3][0-9]-[[:alpha:]]{3,3}-20[0-2][0-9]).+"), "\\1", currentCranDates2)
  dd <- as.POSIXct(dd2, format = "%d-%b-%y")
  if (any(is.na(dd))) {
    dd2 <- gsub(paste0(".*(20[0-2][0-9]-[0-1][0-9]-[0-3][0-9]).*"), "\\1", currentCranDates2)
    dd <- as.POSIXct(dd2)
  }
  ee <- gsub(paste0("^.+>[[:alnum:]\\.]+\\_(.*)\\.tar\\.gz<.*"), "\\1", currentCranDates2)
  ff <- gsub(paste0("^.+>([[:alnum:]\\.]+)\\_.*\\.tar\\.gz<.*"), "\\1", currentCranDates2)
  pkgsDateAvail <- data.table(Package = ff, date = dd, CRANVersion = ee)

  currentCranDates <- pkgsDateAvail[, mtime := date]
  set(currentCranDates, NULL, "date", NULL)

  currentCranDates
}

#' @inheritParams Require
installLocal <- function(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs,
                         verbose = getOption("Require.verbose")) {
  installFromCur <- "Local"
  installPackage <- toInstall[installFrom == installFromCur]$Package
  names(installPackage) <- installPackage
  installPkgNames <- installPackage

  names(installPkgNames) <- installPkgNames

  ord <- match(installPkgNames, toInstall[installFrom == installFromCur]$Package)
  toIn <- toInstall[installFrom == installFromCur][ord]

  if (is.null(dots$dependencies) & is.null(install.packagesArgs$dependencies))
    dots$dependencies <- NA # This was NA; which means let install.packages do it. But, failed in some cases:

  messageVerbose("Using local cache of ", paste(toIn$localFileName, collapse = ", "),
                 verbose = verbose, verboseLevel = 0)
  installPkgNames <- normPath(file.path(rpackageFolder(getOptionRPackageCache()), toIn$localFileName))
  names(installPkgNames) <- installPkgNames

  installPkgNamesBoth <- split(installPkgNames, endsWith(installPkgNames, "zip"))
  installPackageBoth <- split(installPackage, endsWith(installPkgNames, "zip"))

  warngs <- warnings1 <- list()
  warn <- Map(installPkgNamesInner = installPkgNamesBoth, Package = installPackageBoth,
              function(installPkgNamesInner, Package) {
                # # Deal with "binary" mumbo jumbo
                isBin <- isBinary(installPkgNamesInner, needRepoCheck = FALSE)
                isBinNotLinux <- all(isBin) && (isWindows() || isMacOSX())
                type <- c("source", "binary")[isBinNotLinux + 1]
                ipa <- modifyList2(list(type = type),
                                   install.packagesArgs, dots, keep.null = TRUE)
                ipa <- modifyList2(list(quiet = !(verbose >= 1)), ipa, keep.null = TRUE)
                ipa <- append(ipa, list(repos = NULL))
                curPkgs <- toIn$Package
                ipa <- append(list(pkgs = installPkgNamesInner), ipa)
                installPackagesWithQuiet(ipa)
                # do.call(install.packages, ipa)
              })

  pkgDT
}

#' @importFrom stats setNames
#' @inheritParams Require
installCRAN <- function(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs,
                        repos = getOption("repos"), verbose = getOption("Require.verbose")) {
  installPkgNames <- toInstall[installFrom == "CRAN"]$Package

  names(installPkgNames) <- installPkgNames

  toIn <- toInstall[installFrom == "CRAN"]
  if (length(installPkgNames) > 1) {
    ord <- match(installPkgNames, toInstall[installFrom == "CRAN"]$Package)
    toIn <- toIn[ord]
  }

  if (is.null(dots$dependencies) & is.null(install.packagesArgs$dependencies))
    dots$dependencies <- NA # This was NA; which means let install.packages do it. But, failed in some cases:
  #  Failed when newer package already loaded, but not in .libPaths() -- occurs when `setLibPaths` is run after packages are loaded

  if (is.null(dots$type) && is.null(install.packagesArgs$type) && "type" %in% colnames(toInstall)) {
    if (!is.na(toInstall$type))
      dots$type <- toInstall$type
  }

  ipa <- modifyList2(install.packagesArgs, dots, keep.null = TRUE)

  # manually override "type = 'both'" because it gets it wrong some of the time
  ap <- as.data.table(.pkgEnv[["pkgDep"]][["cAP"]])
  if (NROW(ap) > 1 && isWindows()) {
    ap <- ap[Package %in% installPkgNames]
    if (NROW(ap)) {
      onVec <- c("Package")
      if (!is.null(toInstall$versionSpec))
        if (all(!is.na(toInstall$versionSpec)) &&
            !isTRUE(all(toInstall$correctVersionAvail)))
          onVec <- c("Package", "Version" = "versionSpec")

      type <- c("source", "binary")[any(grepl("bin", ap[toInstall, on = onVec]$Repository)) + 1]
      install.packagesArgs["type"] <- type
      ipa <- modifyList2(list(type = type), ipa, keep.null = TRUE)
    }
  }

  needSomeSrc <- if (isWindows()) {
    rep(FALSE, NROW(pkgDT))
  } else {
    installPkgNames %in% sourcePkgs()
  }
  installPkgNamesList <- list()
  reposList <- list()

  if (any(needSomeSrc) && !identical(stripHTTPAddress(repos), stripHTTPAddress(srcPackageURLOnCRAN))) {
    installPkgNamesList$Src <- installPkgNames[needSomeSrc]
    installPkgNamesList$Reg <- installPkgNames[!needSomeSrc]
    if (any(isBinaryCRANRepo(repos)))
      messageVerbose(green("The following package(s) are named in either ",
                     "options('Require.spatialPkgs') or options('Require.otherPkgs') ",
                     "and will be installed from source: \n",
                     paste(installPkgNamesList$Src, collapse = ", ")),
                     verbose = verbose, verboseLevel = 1)
    reposList$Src <- c(CRAN = srcPackageURLOnCRAN)
    reposList$Reg <- repos
  } else {
    installPkgNamesList$Reg <- installPkgNames
    reposList$Reg <- repos
  }
  if (internetExists("cannot install packages from CRAN because internet appears unavailable",
                     verbose = verbose)) {
    td <- tempdir2(paste(collapse = "", sample(LETTERS, 8)))
    if (!is.null(getOption("Ncpus"))) tries <- 5
    for (attempt in seq(tries)) {
      warns <- list()
      Map(installPkgNames = installPkgNamesList, repos = reposList,
          # build = buildList,
          function(installPkgNames, repos) {

            ipa <- modifyList2(list(quiet = !(verbose >= 1)), ipa, keep.null = TRUE)
            ipaFull <- append(list(pkgs = installPkgNames, repos = repos), ipa)
            out <- installPackagesWithQuiet(ipaFull)
            # out <- do.call(install.packages, ipaFull)
          })
      # Sanity check -- try again for the ones that failed
      ip <- as.data.table(installed.packages())
      successReg <- c(installPkgNamesList$Reg) %in% ip$Package
      successSrc <- c(installPkgNamesList$Src) %in% ip$Package
      done <- c(successReg, successSrc)
      if (length(warns)) {
        failedReg <- installPkgNamesList$Reg %in% names(warns)
        failedSrc <- installPkgNamesList$Src %in% names(warns)
        done <- done | c(failedReg, failedSrc)
      }
      if (all(done) ) {
        break
      }
      installPkgNamesList$Reg <- installPkgNamesList$Reg[!successReg]
      installPkgNamesList$Src <- installPkgNamesList$Src[!successSrc]
    }

    # if (any(grepl("--build", c(dots, install.packagesArgs))))#, unlist(buildList)))))
    #   copyTarballsToCache(installPkgNames, TRUE)

    # Don't need to copy to cache as cache was the destdir
    permDen <- grepl("Permission denied", names(warns))
    packagesDen <- gsub("^.*[\\/](.*).dll.*$", "\\1", names(warns))
    if (any(permDen)) {
      stopMess <- character()
      if (any(pkgDT[Package %in% packagesDen]$installFrom == "CRAN"))
        stopMess <- c(
          stopMess,
          paste0("Due to permission denied, you will have to restart R, and reinstall:\n",
                 "------\n",
                 "install.packages(c('", paste(packagesDen, collapse = "', '"), "'), lib = '",
                 libPaths[1],"')")
        )
      if (any(pkgDT[Package %in% packagesDen]$installFrom == "GitHub"))
        stopMess <- c(
          stopMess,
          paste0("Due to permission denied, you will have to restart R, and reinstall:\n",
                 "------\n", "remotes::install_github(c('",
                 paste0(trimVersionNumber(pkgDT[Package %in% packagesDen]$packageFullName),
                        collapse = "', '"), "'), lib = '",libPaths[1],"')")
        )
      stop(stopMess)
    }
  }
  pkgDT
}

#' @inheritParams Require
installArchive <- function(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs,
                           repos = getOption("repos"), verbose = getOption("Require.verbose")) {
  Archive <- "Archive"
  messageVerbose("installing older versions is still experimental and may cause package version conflicts",
                 verbose = verbose, verboseLevel = 1)
  installPkgNames <- toInstall[installFrom == Archive]$Package

  names(installPkgNames) <- installPkgNames

  ord <- match(installPkgNames, toInstall[installFrom == Archive]$Package)
  toIn <- toInstall[installFrom == Archive][ord]

  installVersions <- toIn$OlderVersionsAvailable

  earliestDateOnMRAN <- as.Date(gsub(" .*", "", toIn$dayAfterPutOnCRAN))
  latestDateOnMRAN <- pmin(.latestMRANDate, as.Date(gsub(" .*", "", toIn$desiredDateEnd)))
  onMRANvec <- earliestDateOnMRAN > .earliestMRANDate
  earliestDateOnMRAN[!onMRANvec] <- as.Date(.earliestMRANDate) + 10
  onMRAN <- earliestDateOnMRAN > .earliestMRANDate & unname( isWindows() | isMacOSX() )
  # prevWD <- setwd(tempdir2(.rndstr(1)))
  # on.exit(setwd(prevWD), add = TRUE)

  if (any(onMRAN)) {
    origIgnoreRepoCache <- install.packagesArgs[["ignore_repo_cache"]]
    install.packagesArgs["ignore_repo_cache"] <- TRUE
    installedPkgs <- file.path(.libPaths()[1], unname(installPkgNames)[onMRAN])
    dirsAlreadyExist <- dir.exists(installedPkgs)
    if (any(dirsAlreadyExist)) {
      try(unlink(installedPkgs[dirsAlreadyExist], recursive = TRUE))
    }
    warnings1 <- list()

    urlsOuter <- c()
    extension <- if (isWindows()) ".zip" else ".tgz"
    osNameOnMRAN <- if (isWindows()) "windows" else "macosx"
    messageVerbose("-- Determining dates on MRAN to get correct versions ... ",
                   verbose = verbose, verboseLevel = 1)
    total <- length(unname(installPkgNames)[onMRAN])
    out <- Map(p = unname(installPkgNames)[onMRAN], earliestDateMRAN = earliestDateOnMRAN[onMRAN],
               lastestDateMRAN = latestDateOnMRAN[onMRAN], tot = total, counter = seq(total),
               v = installVersions[onMRAN], function(p, earliestDateMRAN, lastestDateMRAN, v, tot, counter, ...) {
                 if (tot > 1)
                   messageVerboseCounter(total = tot, verbose = verbose, verboseLevel = 1, counter = counter)

                 for (attempt in 0:15 ) { # Try up to 15 days from known earliestDateMRAN or latestDateMRAN of the package being available on CRAN
                   rver <- rversion()
                   evenOrOdd <- attempt %% 2 == 0
                   date <- if (evenOrOdd) earliestDateMRAN else lastestDateMRAN
                   dif <- floor(attempt/2)
                   date <- if (evenOrOdd) date + dif else date - dif

                   urls <- file.path("https://MRAN.revolutionanalytics.com/snapshot", date, "bin", osNameOnMRAN,
                                     "contrib", rver,
                                     paste0(p, "_", v, extension))
                   con <- url(urls)
                   on.exit(try(close(con), silent = TRUE), add = TRUE)
                   a  <- try(suppressWarnings(readLines(con, n = 1)), silent = TRUE)
                   close(con)
                   if (is(a, "try-error")) {
                     earliestDateOnMRAN <- earliestDateOnMRAN + 1
                     urls <- "Fail"
                   } else
                     break

                 }
                 names(urls) <- p
                 urlsOuter <<- c(urlsOuter, urls)
               })

    ipa <- modifyList2(install.packagesArgs, dots, keep.null = TRUE)
    ipa <- append(ipa, list(repos = NULL, type = "bin"))


    urlsSuccess <- urlsOuter[urlsOuter != "Fail"]
    urlsFail <- urlsOuter[urlsOuter == "Fail"]
    ipa <- modifyList2(list(quiet = !(verbose >= 1)), ipa, keep.null = TRUE)
    ipa <- append(list(pkgs = unname(urlsSuccess)), ipa)

    # The install
    installPackagesWithQuiet(ipa)
    # do.call(install.packages, ipa)

    installedVers <- suppressWarnings(try(DESCRIPTIONFileVersionV(
      file.path(.libPaths()[1], names(urlsSuccess), "DESCRIPTION"), purge = TRUE),
      silent = TRUE))

    onMRANAfter <- urlsOuter != "Fail"
    out <- installedVers == installVersions[onMRANAfter]
    names(out) <- names(urlsSuccess)
    if (!all(onMRANAfter)) {
      messageVerbose("-- incorrect version installed from MRAN for ",
                     paste(names(urlsFail), collapse = ", "),"; trying CRAN Archive (as source). ",
                     "Alternatively, try to manually increment the required version number to next version?",
                     verbose = verbose, verboseLevel = 2)
    } else {
      if (any(grepl("cannot open URL.*bin.*Not Found", unlist(warnings1))))
        messageVerbose("MRAN had the necessary version, but not the binary for this R version",
                       verbose = verbose, verboseLevel = 1)
    }

    install.packagesArgs["ignore_repo_cache"] <- origIgnoreRepoCache

    out <- unlist(out)
    thoseThatSucceeded <- out %in% TRUE # there are NAs if not installed; FALSE if version doesn't match
    names(thoseThatSucceeded) <- names(out)
    if (sum(!thoseThatSucceeded, na.rm = TRUE)) {
      wh <- match(names(out), pkgDT$Package)
      if (length(warnings1)) {
        whWarnings <- match(names(warnings1), pkgDT$Package)
        pkgDT[whWarnings, installResult := unlist(lapply(warnings1, function(x) x$message))]
      }
      pkgDT[wh, installResult := c("not installed", "installed")[unlist(out) + 1]]
      messageVerbose("Failed to install binaries of ",
                     paste(names(thoseThatSucceeded)[!thoseThatSucceeded], collapse = ", "),
                     "; trying src versions",
                     verbose = verbose, verboseLevel = 2)
    }
    # pkgDT <- updateInstalled(pkgDT, names(thoseThatSucceeded)[thoseThatSucceeded], out)
    onMRAN <- urlsOuter != "Fail" # thoseThatSucceeded

  }

  if (any(!onMRAN) ) {
    install.packagesArgs <- modifyList2(install.packagesArgs, list(type = "source"), keep.null = TRUE)
    installPkgNamesArchiveOnly <- toIn$Package[!onMRAN]
    for (repo in repos) {
      cranArchivePath <- file.path(repo, "src/contrib/Archive/")
      errorMess <- list()
      warn <- list()
      p <- toIn$PackageUrl[!onMRAN]
      p <- file.path(cranArchivePath, p)
      dots$type <- "source" # must be a source
      ipa <- modifyList2(install.packagesArgs, dots, keep.null = TRUE)
      ipa <- modifyList2(list(quiet = !(verbose >= 1)), ipa, keep.null = TRUE)
      ipa <- append(ipa, list(repos = NULL))
      ipa <- append(list(pkgs = unname(p)), ipa)

      installPackagesWithQuiet(ipa)

      # The install

      # check installed
      pkgsInstalled <- dir(.libPaths()[1])
      p <- p[!installPkgNamesArchiveOnly %in% pkgsInstalled]
      if (length(p) == 0)
        break
    }
    warn <- unlist(warn)
    if (length(warn)) {
      warning(warn)
      pkgDT[Package %in% toInstall$Package, installResult := unlist(warn)]
    }
  }
  # if (any(grepl("--build", c(dots, install.packagesArgs))))
  #   copyTarballsToCache(installPkgNames, TRUE)

  pkgDT
}


isBinary <- function(fn, needRepoCheck = TRUE, repos = getOption("repos")) {
  theTest <- endsWith(fn, "zip") | grepl("R_x86", fn)
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

copyTarballsToCache <- function(pkg, builtBinary, unlink = FALSE,
                                verbose = getOption("Require.verbose")) {
  # if (builtBinary) {
    theDir <- dir(full.names = TRUE)
    origFiles <- lapply(pkg, function(pat) grep(pattern = paste0("[/\\._]", pat, "_"), x = theDir, value = TRUE))
    if (length(unlist(origFiles))) {
      origFiles <- unlist(origFiles)
      newNames <- file.path(rpackageFolder(getOptionRPackageCache()), unique(basename(origFiles)))
      filesAlreadyExist <- file.exists(newNames)
      if (any(!filesAlreadyExist)) {
        messageVerbose(verbose = verbose, verboseLevel = 1,
                       green("Putting packages into RequirePkgCacheDir()"))
        try(linkOrCopy(origFiles[!filesAlreadyExist], newNames[!filesAlreadyExist]))
      }
      if (isTRUE(unlink))
        unlink(origFiles)

      fs <- file.size(newNames)
      fileSizeEq0 <- is.na(fs) | fs == 0
      if (any(fileSizeEq0)) {
        unlink(newNames[fileSizeEq0])
        warning("Copying file(s) to RequirePkgCacheDir() failed; please inspect or rerun")
      }

      return(invisible(newNames))
    }
  #}
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
            messageVerbose("Installing Require ver: ", pkgVersionAtRequireHome," from source at ", requireHome,
                           verbose = verbose, verboseLevel = 2)
            out <- system(paste0(Rpath, " CMD INSTALL --no-multiarch --library=", .libPaths()[1], " Require"),
                          wait = TRUE, ignore.stdout = TRUE, intern = TRUE, ignore.stderr = TRUE)
          }
        }
        done <- TRUE
      } else {
        if (!is.null(requireHome))
          messageVerbose(pkgNameAtRequireHome, " did not contain Require source code",
                         verbose = verbose, verboseLevel = 2)
      }
    }

    if (isFALSE(done)) {
      Rpath <- Sys.which("Rscript")
      system(paste0(Rpath, " -e \"install.packages(c('Require'), lib ='", .libPaths()[1],
                    "', quiet = TRUE, repos = '", getOption('repos')[["CRAN"]],"')\""), wait = TRUE)
      done <- TRUE
    }
  } else {
    stop("Require will need to be installed manually in", .libPaths()[1])
  }
}

toPkgDT <- function(pkgDT, deepCopy = FALSE) {
  if (!is.data.table(pkgDT)) {
    pkgDT <- rmExtraSpaces(pkgDT)
    pkgDT <- if (deepCopy)
      data.table(Package = extractPkgName(pkgDT), packageFullName = pkgDT)
    else
      toDT(Package = extractPkgName(pkgDT), packageFullName = pkgDT)
  }

  pkgDT
}

toDT <- function(...) {
  setDT(list(...))
}

rmDuplicatePkgs <- function(pkgDT, verbose = getOption("Require.verbose", 1)) {
  pkgDT <- try(unique(pkgDT))
  if (is(pkgDT, "try-error")) stop("Error 856; please contact developer")
  dups <- pkgDT[installed %in% FALSE, .N, by = "Package"][N > 1]
  if (NROW(dups)) {
    messageVerbose("Some packages are needed; multiple minimum version requirements; using most stringent",
                   verbose = verbose, verboseLevel = 2)
    pkgDT <- pkgDT[dups, dup := TRUE, on = "Package"]
    pkgDT <- pkgDT[is.na(dup) | (dup == TRUE & installFrom != "Fail"), keep := TRUE]

    pkgDT[installed == FALSE & keep == TRUE, keep2 := {
      out <- .I[1]
      if (!is.null(pkgDT$versionSpec)) {
        if (.N > 1) {
          if (all(!is.na(versionSpec))) {
            out <- .I[which(versionSpec == max(as.package_version(versionSpec)))[1]]
            if (length(out) > 1) {
              out <- out[1]
            }
          }
        }
      }
      rep(out, times = length(.I))
    }, by = "Package"]
    # Take the first one, these have been sorted on version
    pkgDT[installed == FALSE & keep == TRUE & seq(NROW(pkgDT)) != keep2, keep := NA]
    set(pkgDT, NULL, "duplicate", FALSE)
    pkgDT[is.na(keep), `:=`(keep = FALSE, installFrom = "Duplicate", duplicate = TRUE)] # Was "Fail" ...
    if (!all(pkgDT$keep)) {
      colsKeep <- intersect(c("Package", "packageFullName", "keep", "installResult"),
                            colnames(pkgDT))
      summaryOfDups <- pkgDT[dup == TRUE, ..colsKeep]
      setorderv(summaryOfDups, c("Package", "keep"), order = c(1,-1))
      messageDF(summaryOfDups, verbose = verbose, verboseLevel = 1)
    }
    pkgDT[, `:=`(keep2 = NULL, keep = NULL, dup = NULL)]
  }
  pkgDT
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
#'
#'
detachAll <- function(pkgs, dontTry = NULL, doSort = TRUE, verbose = getOption("Require.verbose")) {
  messageVerbose("Detaching is fraught with many potential problems; you may have to",
                 "restart your session if things aren't working",
                 verbose = verbose, verboseLevel = 2)
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

  dontTryExtra <- intersect(c("glue", "rlang", "ps", "ellipsis", "processx", "vctrs", "RCurl", "bitops"),
                            pkgs)

  if (length(dontTryExtra)) {
    messageVerbose("some packages don't seem to unload their dlls correctly. ",
                   "These will not be unloaded: ", paste(dontTryExtra, collapse = ", "),
                   verbose = verbose, verboseLevel = 2)
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
  tolower(Sys.info()["sysname"]) == "windows"
}

isMacOSX <- function()
  isMac <- tolower(Sys.info()["sysname"]) == "darwin"

warningCantInstall <- function(pkgs) {
  warning("Can't install ", pkgs, "; you will likely need to restart R and run:\n",
          "-----\n",
          "install.packages(c('",paste(pkgs, collapse = ", "),"'), lib = '",.libPaths()[1],"')",
          "\n-----\n...before any other packages get loaded")

}

rpackageFolder <- function(path = getOptionRPackageCache(), exact = FALSE)  {
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
    if (missing(ifMissing))
      return(.libPaths())
    else {
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
  pkgNameInPkgFullName <- unlist(Map(pkg = Package, pfn = packageFullName,
                                     function(pkg, pfn) grepl(pkg, pfn)))
  Package[!pkgNameInPkgFullName] <- paste0(Package[!pkgNameInPkgFullName], " (",
                                           packageFullName[!pkgNameInPkgFullName], ")")
  Package
}



splitGitRepo <- function(gitRepo, default = "PredictiveEcology", masterOrMain = NULL) {
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
  if (any(lenGT2)) {
    br[lenGT2] <- lapply(grSplit[lenGT2], function(grsplit) grsplit[[3]])
  }
  br[!lenGT2] <- "HEAD"

  list(acct = acct, repo = repo, br = br)
}


#' Install R Package from GitHub source code
#'
#' A lightweight alternative to `devtools::install_github`. All dependencies
#' must have been installed already for this to work.
#'
#' @param gitRepo A repository in the form: Account/Repository@Branch or Account/Repository@SHA
#' @param libPath The folder where you would like the package installed. Defaults
#'   to `.libPaths()[1]`
#' @inheritParams Require
#' @param ... Passed to R CMD INSTALL
#' @export
installGithubPackage <- function(gitRepo, libPath = .libPaths()[1], verbose = getOption("Require.verbose"),
                                 ...) {
  # Can be vectorized on gitRepo
  dir.create(libPath, showWarnings = FALSE, recursive = TRUE)
  masterMain <- c("main", "master")
  if (is.null(names(gitRepo))) {
    names(gitRepo) <- extractPkgName(gitRepo)
  }
  gr <- splitGitRepo(gitRepo)
  dots <- list(...)
  quiet <- dots$quiet
  if (is.null(quiet)) quiet <- !(verbose >= 0)
  alreadyExistingDESCRIPTIONFile <- lapply(gr$repo, function(repo) file.path(libPath, repo, "DESCRIPTION"))
  useRemotes <- FALSE
  filesExist <- file.exists(unlist(alreadyExistingDESCRIPTIONFile))
  skipDLandBuild <- FALSE

  if (!is.null(getOptionRPackageCache()) || any(filesExist)) {
    shaOnGitHub <-
      unlist(Map(repoInner = gr$repo, acctInner = gr$acct, brInner = gr$br,
                 function(repoInner, acctInner, brInner)
                   getSHAfromGitHub(repo = repoInner, acct = acctInner, br = brInner)))

    if (any(filesExist)) { # means already installed here; no new install
      if (is(shaOnGitHub, "try-error")) {
        useRemotes <- TRUE
      } else {
        shaLocal <- DESCRIPTIONFileOtherV(alreadyExistingDESCRIPTIONFile, other = "GithubSHA1")
        if (identical(unname(shaLocal), unname(shaOnGitHub))) {
          messageVerbose("Skipping install of ", gitRepo, ", the SHA1 has not changed from last install",
                         verbose = verbose, verboseLevel = 1)
          return(invisible())
        }
      }
    } else { # this means, not installed, but it may be in Cache
      dd <- dir(getOptionRPackageCache(), full.names = TRUE)
      cachedFiles <- lapply(shaOnGitHub, function(sha) grep(sha, dd, value = TRUE))
      if (any(lengths(cachedFiles) > 0)) {
        cachedFilesHere <- Map(cf = cachedFiles, sha = shaOnGitHub, function(cf, sha)
          file.copy(cf, gsub(paste0("(", sha, ")."), "", basename(cf))))
        skipDLandBuild <- lengths(cachedFilesHere) > 0
        if (any(skipDLandBuild)) {
          if (all(vapply(cachedFiles, function(cf) any(isBinary(cf, needRepoCheck = FALSE)),
                         FUN.VALUE = logical(1)))) {
            dots$INSTALL_opts <- setdiff(dots$INSTALL_opts, "--build")
          }
          messageVerbose("Identical SHA for '", paste(names(skipDLandBuild)[skipDLandBuild], collapse = "', '"),
                         "' found in ", RequirePkgCacheDir(), "; using it",
                         verbose = verbose, verboseLevel = 0)
        }


      }
    }
  }

  stillNeedDLandBuild <- !skipDLandBuild
  if (any(stillNeedDLandBuild)) {

    if (!useRemotes) {
      out <- downloadRepo(gitRepo[stillNeedDLandBuild], overwrite = TRUE, destDir = ".", #tmpPath,
                          verbose = verbose)
      if (is(out, "try-error"))
        useRemotes <- TRUE
    }
    # This is likely due to a wrong path for the git repo, or more likely an error b/c no GITHUB_PAT
    if (useRemotes) {
      if (!requireNamespace("remotes")) {
        install.packages("remotes")
      }
      out <- remotes::install_github(gitRepo, dependencies = NA)
      if (identical(out, extractPkgName(gitRepo)))
        return(invisible())
    }
  }
  if (any(stillNeedDLandBuild)) {
    if (nchar(Sys.which("R")) > 0) {
      messageVerbose("building package (R CMD build)",
                     verbose = verbose, verboseLevel = 1)
      internal <- !interactive()
      extras <- c("--no-resave-data", "--no-manual",
                  "--no-build-vignettes")
      Rpath1 <- Sys.getenv("R_HOME")
      Rpath <- file.path(Rpath1, "bin/R") # need to use Path https://stat.ethz.ch/pipermail/r-devel/2018-February/075507.html
      # origPath <- setwd(tmpPath)
      # on.exit(setwd(origPath), add = TRUE)
      out1 <- lapply(gr$repo[stillNeedDLandBuild], function(repo) {
        system(paste(Rpath, "CMD build ", repo, paste(extras, collapse = " ")),
               intern = internal, ignore.stdout = quiet, ignore.stderr = quiet)
      })
      if (any(unlist(out1) == 1L)) stop("Error 456; contact developer")
      theDESCRIPTIONfile <- dir(out, pattern = "DESCRIPTION", full.names = TRUE)
      packageName <- DESCRIPTIONFileOtherV(theDESCRIPTIONfile, other = "Package")
      messageVerbose("  ... Built!",
                     verbose = verbose, verboseLevel = 1)
      # shaOnGitHub2 <- shaOnGitHub[packageName]
      # Map(pack = packageName, sha = shaOnGitHub2, function(pack, sha) {
      #   fns <- copyTarballsToCache(pack, builtBinary = TRUE)
      #   newFP <- file.path(dirname(fns), paste0(sha, ".", basename(fns)))
      #   out <- file.rename(fns, newFP)
      #   return(out)
      # })


    } else {
      stop("Can't install packages this way because R is not on the search path")
    }
  }
  localDir <- dir()
  packageFNtoInstall <- lapply(gr$repo, function(pak) {
    packageFNtoInstall <- grep(pattern = paste0("^", pak, "_[[:digit:]].+(tar.gz|zip)"), localDir, value = TRUE)
    isBin <- isBinary(packageFNtoInstall, needRepoCheck = FALSE)
    if (any(isBin)) {
      packageFNtoInstall <- packageFNtoInstall[isBin]
    }
    packageFNtoInstall
  })

  # if (!isTRUE(grepl("--build", dots$INSTALL_opts)) && !is.null(getOptionRPackageCache())) {
  #   if (!all(isBinary(unlist(packageFNtoInstall), needRepoCheck = FALSE)))
  #     dots$INSTALL_opts <- paste(dots$INSTALL_opts, "--build")
  # }

  pkgsSimple <- names(gitRepo)
  names(pkgsSimple) <- pkgsSimple
  packageFNtoInstallList <- lapply(pkgsSimple, function(pack)
    unname(grep(pack, unlist(packageFNtoInstall), value = TRUE)))
  packageFNtoInstall <- unlist(packageFNtoInstallList)
  # packageFNtoInstall <- unlist(packageFNtoInstall[pmatch(names(gitRepo), unlist(packageFNtoInstall))])
  packageName <- names(packageFNtoInstall)
  names(packageName) <- packageName

  opts2 <- append(dots,
                  list(pkgs = packageFNtoInstall,
                       repos = NULL,
                       lib = normalizePath(libPath, winslash = "/")))
  opts2 <- append(opts2, list(type = "source")) # it may have "binary", which is incorrect
  opts2 <- modifyList2(list(quiet = !(verbose >= 1)), opts2, keep.null = TRUE)
  if (is.null(opts2$destdir)) {
    cachePath <- getOptionRPackageCache()
    opts2$destdir <- if (is.null(cachePath)) "." else cachePath
  }
  warns <- list()
  # Need this internal wCH because need to know which one failed
  withCallingHandlers(
    installPackagesWithQuiet(opts2),
    # do.call(install.packages, opts2),
    warning = function(w) {
      warns <<- appendToWarns(w = w$message, warns = warns, Packages = packageName)
    }
  )

  installStatus <- vapply(packageName, function(x) TRUE, logical(1))
  pkgsToModify <- packageName
  if (length(unlist(warns))) {
    problems <- unlist(lapply(packageName, function(pn) grepl(pn, warns)))
    installStatus[problems] <- FALSE
    pkgsToModify <- packageName[problems %in% FALSE]
  }

  lapply(pkgsToModify, function(pack) {
    postInstallDESCRIPTIONMods(pkg = pack, repo = gr$repo[[pack]],
                               acct = gr$acct[[pack]], br = gr$br[[pack]],
                               lib = normalizePath(libPath, winslash = "/", mustWork = FALSE))
    if (!is.null(getOptionRPackageCache())) {
      fns <- dir(pattern = pack)
      needUnlink <- fns == pack # only the exact match, i.e., source folder; not the tars
      if (any(needUnlink)) {
        unlink(fns[needUnlink], recursive = TRUE)
        fns <- fns[!needUnlink]
      }
      theDESCRIPTIONfile <- file.path(.libPaths()[1], pack, "DESCRIPTION")
      # system.file("DESCRIPTION", package = "peutils", lib.loc = .libPaths()[1])
      sha <- DESCRIPTIONFileOtherV(theDESCRIPTIONfile, other = "RemoteSha")
      file.rename(fns, paste0(sha, ".", fns))
    }
  })

  return(installStatus)

}

postInstallDESCRIPTIONMods <- function(pkg, repo, acct, br, lib) {
  file <- file.path(lib, pkg, "DESCRIPTION")
  txt <- readLines(file)
  beforeTheseLines <- grep("NeedsCompilation:|Packaged:|Author:", txt)
  insertHere <- min(beforeTheseLines)
  sha <- if (grepl("[[:alnum:]]{40,40}", br)) { # it is already a sha -- no need to find from head
    br
  } else {
    getSHAfromGitHub(acct, repo, br)
  }
  newTxt <-
    paste0("RemoteType: github
    RemoteHost: api.github.com
    RemoteRepo: ", pkg, "
    RemoteUsername: ", acct,"
    RemoteRef: ", br, "
    RemoteSha: ", sha, "
    GithubRepo: ", pkg, "
    GithubUsername: ", acct, "
    GithubRef: ", br, "
    GithubSHA1: ", sha, "")
  newTxt <- strsplit(newTxt, split = "\n")[[1]]
  newTxt <- gsub("^ +", "", newTxt)
  txtOut <- c(txt[seq(insertHere - 1)], newTxt, txt[insertHere:length(txt)])
  cat(txtOut, file = file, sep = "\n")
  return(invisible())
}

#' @rdname installGithubPackage
#' @export
installGitHubPackage <- installGithubPackage

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
    try(downloadFileMasterMainAuth(url, destfile = zipFileName, need = "master"), silent = TRUE))
  if (is(out, "try-error")){
    return(out)
  }

  out <- lapply(zipFileName, function(zfn) unzip(zfn, exdir = destDir)) # unzip it

  de <- dir.exists(repoFull)
  if (any(de))
    if (isTRUE(overwrite)) {
      unlink(repoFull[de], recursive = TRUE)
    } else {
      stop(repoFull, " directory already exists. Use overwrite = TRUE if you want to overwrite it")
    }
  badDirname <- try(lapply(out, function(d) unique(dirname(d))[1]))
  if (is(badDirname, "try-error")) stop("Error 654; something went wrong with downloading & building the package")
  badDirname <- unlist(badDirname)
  if (isTRUE(is.na(subFolder)) || isTRUE(is.null(subFolder))) {
    subFolder <- FALSE
  }

  newName <- unlist(Map(bad = badDirname, subFolder = subFolder, pkgName = pkgName,
      function(bad, subFolder, pkgName) {
    badToChange <- if (!isFALSE(subFolder)) {
      file.path(basename(gsub(subFolder, "", bad)), subFolder)
    } else {
      basename(bad)
    }
    newName <- gsub(badToChange, pkgName, bad)
    file.rename(bad, newName) # it was downloaded with a branch suffix
    newName
  })
  )
  unlink(zipFileName)
  messageVerbose(paste0(gitRepo, " downloaded and placed in ", normalizePath(repoFull, winslash = "/"), collapse = "\n"),
                 verbose = verbose, verboseLevel = 2)
  return(normalizePath(repoFull))
}

getSHAfromGitHub <- function(acct, repo, br) {
  if (nchar(br) == 40) return(br)

  shaPath <- file.path("https://api.github.com/repos", acct, repo, "git", "refs")
  if (missing(br))
    br <- "main"
  if (identical(br, "HEAD"))
    br <- "main"
  masterMain <- c("main", "master")
  if (any(br %in% c(masterMain))) {
    # possibly change order -- i.e., put user choice first
    br <- masterMain[rev(masterMain %in% br + 1)]
  }
  tf <- tempfile();
  downloadFileMasterMainAuth(shaPath, destfile = tf, need = "master")
  sha <- try(suppressWarnings(readLines(tf)), silent = TRUE)
  if (is(sha, "try-error")) return(sha)
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
    whHasBr <- which(vapply(sha2, function(xx)
      any(grepl(paste0(".+refs/.+/+", branch, "\""), xx)), FUN.VALUE = logical(1)))
    if (length(whHasBr) > 0) {
      break
    }
  }

  sha3 <- sha2[[whHasBr]]
  shaLine <- grep("sha", sha3) + 1
  shaLine <- strsplit(sha3[shaLine], ",")[[1]][1]
  sha <- gsub(" *[[:punct:]]+(.+)[[:punct:]] *", "\\1", shaLine)
  sha
}

.earliestMRANDate <- "2015-06-06"
.latestMRANDate <- Sys.Date() - 45

#' R versions
#'
#' Reference table of R versions and their release dates (2018 and later).
#'
#' Update this as needed using `rversions::r_versions()`:
#'
#' \verb{
#' # install.packages("rversions")
#' v = rversions::r_versions()
#' keep = which(as.Date(v$date, format = "%Y-%m-%d") >= as.Date("2018-01-01", format = "%Y-%m-%d"))
#' dput(v[keep, c("version", "date")])
#' }
rversions <- structure(list(
  version = c("3.4.4", "3.5.0", "3.5.1", "3.5.2",
              "3.5.3", "3.6.0", "3.6.1", "3.6.2", "3.6.3", "4.0.0", "4.0.1",
              "4.0.2", "4.0.3", "4.0.4", "4.0.5", "4.1.0", "4.1.1", "4.1.2",
              "4.1.3", "4.2.0", "4.2.1"),
  date = structure(c(1521101067, 1524467078,
                     1530515071, 1545293080, 1552291489, 1556262303, 1562310303, 1576137903,
                     1582963516, 1587711934, 1591427116, 1592809519, 1602313524, 1613376313,
                     1617174315, 1621321522, 1628579106, 1635753912, 1646899538, 1650611141,
                     1655967933), class = c("POSIXct", "POSIXt"), tzone = "UTC")),
  row.names = 108:128, class = "data.frame")

rversion <- function() {
  paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1])
}

#' Get or compare current R version to a known version
#'
#' Compares R version to a known version
#' @param testVers A character string using format "== 4.1"
#'   or ">= 4.1"
#' @return
#' If no `testVers` is supplied, then it will just return the current R version.
#' If `testVers` is supplied, then it will return a `TRUE` or `FALSE`.
#'
#' @export
#' @examples
#' rCurrentVersion(">= 4.1")
rCurrentVersion <- function(testVers) {
  curVer <- rversion()
  if (!missing(testVers)) {
    curVerNum <- as.character(numeric_version(curVer))
    testVers <- gsub("\\(|\\)", "", testVers) # remove parentheses, if any
    testVers <- paste0("(", testVers, ")")    # put them back
    testVersNum <- as.character(numeric_version(extractVersionNumber(testVers)))
    inequ <- extractInequality(testVers)
    comp <- compareVersion(curVerNum, testVersNum)
    out <- eval(parse(text = paste(comp, inequ, "0")))
  } else {
    out <- curVer
  }
  out
}

urlExists <- function(url) {
  con <- url(url)
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  for (i in 1:5) {
    a  <- try(suppressWarnings(readLines(con, n = 1)), silent = TRUE)
    try(close(con), silent = TRUE)
    ret <- if (is(a, "try-error")) FALSE else TRUE
    if (isTRUE(ret))
      break
    else
      Sys.sleep(0.1)
  }
  ret
}

#' @inheritParams Require
internetExists <- function(mess = "", verbose = getOption("Require.verbose")) {
  if (getOption("Require.checkInternet", FALSE)) {
    internetMightExist <- TRUE
    if (!is.null(.pkgEnv$internetExistsTime)) {
      if ((Sys.time() - getOption('Require.internetExistsTimeout', 30)) < .pkgEnv$internetExistsTime) {
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
                       verbose = verbose, verboseLevel = 2)
      }
      .pkgEnv$internetExistsTime <- Sys.time()
    }
  }
  TRUE
}


#' A list of R packages that should likely be installed from Source, not Binary
#'
#' The list of R packages that `Require` installs from source on Linux, even if
#' the `getOptions("repos")` is a binary repository. This list can be updated by
#' the user by modifying the options `Require.spatialPkgs` or
#' `Require.otherPkgs`. Default "force source only packages" are visible with `RequireOptions()`.
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
  if (is.null(spatialPkgs))
    spatialPkgs <- .spatialPkgs
  .otherPkgs <- getOption("Require.otherPkgs")
  if (is.null(otherPkgs))
    otherPkgs <- .otherPkgs
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
  out2 <- paste(Rpath, "-e \"do.call(install.packages, list(",
                paste(opts2[!hasName], ", ",
                      paste(names(opts2)[hasName], sep = " = ", opts2[hasName],
                            collapse = ", "),"))\""))
  out <- system(out2, intern = TRUE)
  return(out)
}

installByPak <- function(pkgDT, libPaths, doDeps, ...) {
  if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
  fas <- formals(pak::pkg_install)
  pakFormalsPassedHere <- names(list(...)) %in% names(fas)
  if (any(pakFormalsPassedHere)) {
    fas <- modifyList2(fas, list(...)[pakFormalsPassedHere], keep.null = TRUE)
  }
  if (!"ask" %in% pakFormalsPassedHere) {
    fas[["ask"]] <- FALSE
  }
  pkgsForPak <- pkgDT[pkgDT$needInstall %in% TRUE]
  out <- pak::pkg_install(trimVersionNumber(pkgsForPak$packageFullName),
                          lib = libPaths[1],
                          dependencies = FALSE,
                          ask = eval(fas[["ask"]]),
                          upgrade = fas[["upgrade"]])
  # pkgDT <- updateInstalled(pkgDT, pkgsForPak$Package, out)
}

#' Get the option for `Require.RPackageCache`
#'
#' First checks if an environment variable `Require.RPackageCache` is set and defines a path.
#' If not set, checks whether the `options("Require.RPackageCache")` is set.
#' If a character string, then it returns that.
#' If `TRUE`, then use `RequirePkgCacheDir()`. If `FALSE` then returns `NULL`.
#'
#' @export
getOptionRPackageCache <- function() {
  curVal <- getOption("Require.RPackageCache")
  try <- 1
  while (try < 3) {
    if (isTRUE(curVal)) {
      curVal <- RequirePkgCacheDir(FALSE)
      break
    } else if (isFALSE(curVal)) {
      curVal <- NULL
      break
    } else {
      if (identical("default", curVal)) {
        fromEnvVars <- Sys.getenv("Require.RPackageCache")
        if (nchar(fromEnvVars) == 0  ) {
          curVal <- RequirePkgCacheDir(FALSE)
          break
        } else {
          try <- try + 1
          curVal <- fromEnvVars
          if (identical("TRUE", curVal)) {
            curVal <- TRUE
          } else if (identical("FALSE", curVal)) {
            curVal <- NULL
          } else {
            break
          }
        }
      } else {
        break
      }
    }
  }
  if (!is.null(curVal)) {
    checkPath(curVal, create = TRUE)
  }
  curVal
}



masterMainHEAD <- function(url, need) {
  # masterMain <- c("main", "master")
  # masterMainGrep <- paste0("/", paste(masterMain, collapse = "|"), "(/|\\.)")
  # masterGrep <- paste0("/", "master", "(/|\\.)")
  # mainGrep <- paste0("/", "main", "(/|\\.)")
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

downloadFileMasterMainAuth <- function(url, destfile, need = "HEAD",
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
  if (!is.null(urls[["FALSE"]]))
    outNotMasterMain <-
    withCallingHandlers(Map(URL = urls[["FALSE"]], df = destfile, function(URL, df) {
      try(download.file(URL, destfile = destfile, quiet = TRUE), silent = TRUE)
    }),

    warning = function(w) {
      # strip the ghp from the warning message
      w$message <- gsub(paste0(ghp, ".*@"), "", w$message)
      browser()
      stop(w)
      invokeRestart("muffleWarning")

    })
  if (!is.null(urls[["TRUE"]])) # should be sequential because they are master OR main
    for (wh in seq(urls[["TRUE"]])) {
      outMasterMain <- try(download.file(urls[["TRUE"]][wh], destfile = destfile[wh], quiet = TRUE),
                           silent = TRUE)
      if (!is(outMasterMain, "try-error")) {
        names(outMasterMain) <- urls[["TRUE"]][wh]
        break
      }
    }
  c(outNotMasterMain, outMasterMain)

}

messageGithubPAT <- function(ghp, verbose = verbose, verboseLevel = 0) {
  if (nzchar(ghp)) {
    if (is.null(.pkgEnv$hasGHP)) {
      .pkgEnv$hasGHP <- TRUE
      messageVerbose("Using GITHUB_PAT to access files on GitHub",
                     verboseLevel = 0, verbose = verbose)
    }
  }
}

notInArchives <- "Not in Archives"


masterMain <- c("main", "master")
masterMainGrep <- paste0("/", paste(masterMain, collapse = "|"), "(/|\\.)")
masterGrep <- paste0("/", "master", "(/|\\.)")
mainGrep <- paste0("/", "main", "(/|\\.)")

extractPkgNameFromFileName <- function(x) {
  out <- gsub(".+\u2018(.+)_.+\u2019.+", "\\1", x) # those two escape characters are the inverted commas
  gsub(".+\u2018(.+)\u2019.+", "\\1", out)         # package XXX is in use and will not be installed
}


appendToWarns <- function(w, warns, Packages) {
  names(Packages) <- Packages
  newWarn <- lapply(Packages, function(p) {
    grep(p, w, value = TRUE)
  })
  newWarn <- newWarn[lengths(newWarn) > 0]
  append(warns, newWarn)
}

appendBuild <- function(pkgDT, installFrom, INSTALL_opts) {
  # THis will get false positives, which gives slightly more verbose outputs
  if ("needBuild" %in% colnames(pkgDT))
    if (any(pkgDT$needBuild[pkgDT$installFrom %in% installFrom]))
      INSTALL_opts <- unique(c("--build", INSTALL_opts))
  INSTALL_opts
}

needBuild <- function(pkgDT) {
  pkgDT$needInstall & (
    as.logical(getOption("Require.buildBinaries", TRUE)) &&
      !is.null(RequirePkgCacheDir()) )
}

availablePackagesCachedPath <- function(repos, type) {
  file.path(RequirePkgCacheDir(), gsub("https|[:/]", "", repos[1]), type, "availablePackages.rds")
}

installPackagesWithQuiet <- function(ipa) {
  if (isTRUE(ipa$quiet)) {
    messSupp2 <- capture.output(
      messSupp <- capture.output(
        type = "message", do.call(install.packages, ipa))
    )
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
                                        "", paste0(" (", inequality, pkgDT$Version, ")"))))
  } else {
    ifelse(!is.na(pkgDT$Repo) & nzchar(pkgDT$Repo),
           paste0(pkgDT$Account, "/", pkgDT$Package, "@", pkgDT$SHAonGH),
           paste0(pkgDT$Package, ifelse(is.na(pkgDT$Version) | is.na(pkgDT$inequality),
                                        "", paste0(" (", inequality, pkgDT$Version, ")"))))
  }
}

gitHubFileUrl <- function(hasSubFolder, Branch, GitSubFolder, Account, Repo, filename) {
  if (any(hasSubFolder)) {
    Branch <- paste0(Branch, "/", GitSubFolder)
  }
  file.path("https://raw.githubusercontent.com", Account, Repo, Branch, filename, fsep = "/")
}
