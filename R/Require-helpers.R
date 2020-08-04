#' @details
#' \code{parseGitHub} turns the single character string representation into 3 or 4:
#' \code{Account}, \code{Repo}, \code{Branch}, \code{SubFolder}.
#'
#' @return
#' \code{parseGitHub} returns a data.table with added columns.
#'
#' @param pkgDT A character string with full package names or a data.table
#'   with at least 2 columns \code{"Package"} and \code{"packageFullName"}.
#' @rdname GitHubTools
#' @export
parseGitHub <- function(pkgDT) {
  if (!is.data.table(pkgDT))
    pkgDT <- data.table(Package = extractPkgName(pkgDT), packageFullName = c(pkgDT))
  pkgDT[, githubPkgName := extractPkgGitHub(packageFullName)]
  isGH <- !is.na(pkgDT$githubPkgName)
  if (is.null(pkgDT$repoLocation)) {
    pkgDT[isGH, repoLocation := "GitHub"]
    pkgDT[!isGH, repoLocation := "CRAN"]
  }

  if (any(pkgDT$repoLocation == "GitHub")) {
    isGitHub <- pkgDT$repoLocation == "GitHub"
    pkgDT[isGitHub, fullGit := trimVersionNumber(packageFullName)]
    pkgDT[isGitHub, Account := gsub("^(.*)/.*$", "\\1", fullGit)]
    pkgDT[isGitHub, RepoWBranch := gsub("^(.*)/(.*)@*.*$", "\\2", fullGit)]
    pkgDT[isGitHub, hasSubFolder := grepl("/", pkgDT[isGitHub]$Account)]
    if (any(pkgDT$hasSubFolder, na.rm = TRUE)) { # fix both Account and RepoWBranch
      hasSubFold <- pkgDT$hasSubFolder
      subFoldIndices <- seq_len(NROW(pkgDT[hasSubFold]))
      pkgDT[hasSubFold, Account := gsub("^(.*)/(.*)$", "\\1", Account)]
      pkgDT[hasSubFold, RepoWBranch := gsub(paste0("^",Account,"/"), "", fullGit), by = subFoldIndices]
      pkgDT[hasSubFold, GitSubFolder := strsplit(pkgDT[hasSubFold]$RepoWBranch, split = "/|@")[[1]][2],
            by = subFoldIndices]
      pkgDT[hasSubFold, RepoWBranch := gsub(paste0("/",GitSubFolder), "", RepoWBranch), by = subFoldIndices]
    }
    pkgDT[isGitHub, Repo := gsub("^(.*)@(.*)$", "\\1", RepoWBranch)]
    pkgDT[isGitHub, Branch := "master"]
    pkgDT[isGitHub & grepl("@", RepoWBranch), Branch := gsub("^.*@(.*)$", "\\1", RepoWBranch)]
    set(pkgDT, NULL, c("RepoWBranch", "fullGit"), NULL)
  }
  pkgDT[]
}

#' Internals used by \code{Require}
#'
#' While these are not intended to be called manually by users, they may be
#' of some use for advanced users.
#'
#' @return
#' In general, these functions return a data.table with various package
#' information, installation status, version, available version etc.
#'
#' @importFrom data.table setorderv
#' @inheritParams Require
#' @inheritParams parseGitHub
#' @rdname Require-internals
#' @export
getPkgVersions <- function(pkgDT, install = TRUE) {
  if (!is.data.table(pkgDT))
    pkgDT <- data.table(Package = extractPkgName(pkgDT), packageFullName = c(pkgDT))

  pkgDT[, hasVersionSpec := grepl(.grepVersionNumber, packageFullName)]

  if (any(pkgDT$hasVersionSpec)) {
    # pkgDT <- pkgDT[hasVersionSpec == TRUE, versionSpec := gsub(grepExtractPkgs, "\\2", packageFullName)]
    pkgDT <- pkgDT[hasVersionSpec == TRUE, versionSpec := extractVersionNumber(packageFullName)]
    pkgDT[hasVersionSpec == TRUE, inequality := extractInequality(packageFullName)]


    # pkgDTNoMV[repoLocation == "GitHub", correctVersionAvailGH := TRUE]
    # setnames(pkgDTNoMV, old = colsToKeep, new = newColNames)
    pkgDT[hasVersionSpec == TRUE & grepl("<", inequality), versionSpec := as.character(min(package_version(versionSpec))),
          by = "Package"]

    setorderv(pkgDT, c("Package", "versionSpec"), order = -1L)

    # any duplicates with different minimum version number to be dealt with here --> only those with > in their inequality
    pkgDT[hasVersionSpec == TRUE, versionSpec := as.character(max(package_version(versionSpec))), by = "Package"]
    # pkgDT_maxVersion <- pkgDT[hasVersionSpec == TRUE & grepl(">", inequality),
    #                           list(versionSpec = as.character(max(package_version(versionSpec)))),
    #                           by = "Package"]
    # pkgDT <- rbindlist(list(pkgDT[hasVersionSpec == FALSE], pkgDT[pkgDT_maxVersion, on = c("Package", "versionSpec")]))
    setorderv(pkgDT, c("Package", "hasVersionSpec"), order = -1L)
    # pkgDT <- pkgDT[, .SD[1], by = c("Package", "inequality")]

    if ("Version" %in% colnames(pkgDT)) {
      pkgDT[!is.na(Version), compareVersion := .compareVersionV(Version, versionSpec)]
      pkgDT[!is.na(Version) & hasVersionSpec == TRUE, correctVersion := .evalV(.parseV(text = paste(compareVersion, inequality, "0")))]
      pkgDT[hasVersionSpec == FALSE, correctVersion := NA]
      # put FALSE at top of each package -- then take the first one, so we will know if all inequalities are satisfied
      setorderv(pkgDT, c("Package", "correctVersion"), order = 1L, na.last = TRUE)
      pkgDT <- pkgDT[, .SD[1], by = c("Package")]
    }
  } else {
    pkgDT[ , correctVersion := NA]
  }
  if (isTRUE(install == "force")) {
    pkgDT[, correctVersion := FALSE]
  }
  pkgDT[]
}

#' @inheritParams Require
#'
#' @export
#' @importFrom utils compareVersion download.file tail
#' @rdname Require-internals
getAvailable <- function(pkgDT, purge = FALSE, repos = repos) {
  if (NROW(pkgDT[correctVersion == FALSE | is.na(correctVersion)])) {
    whNotCorrect <- pkgDT[, .I[hasVersionSpec == TRUE & (correctVersion == FALSE | is.na(correctVersion))]]
    if (NROW(whNotCorrect)) {
      notCorrectVersions <- pkgDT[whNotCorrect]

      # do CRAN first
      if (any(notCorrectVersions$repoLocation == "CRAN")) {
        cachedAvailablePackages <- available.packagesCached(repos = repos, purge = purge)
        cachedAvailablePackages <- as.data.table(cachedAvailablePackages[, c("Package", "Version")])
        setnames(cachedAvailablePackages, "Version", "AvailableVersion")
        notCorrectVersions <- cachedAvailablePackages[notCorrectVersions, on = "Package"]
        notCorrectVersions[repoLocation != "GitHub" & is.na(AvailableVersion), AvailableVersion := "10000000"]
        notCorrectVersions[repoLocation != "GitHub",
                           compareVersionAvail := .compareVersionV(AvailableVersion, versionSpec)]
        notCorrectVersions[repoLocation != "GitHub",
                           correctVersionAvail :=
                             .evalV(.parseV(text = paste(compareVersionAvail, inequality, "0")))]
      }

      # do Older Versions

      needOlder <- notCorrectVersions$correctVersionAvail == FALSE & grepl("==|<=|<", notCorrectVersions$inequality)
      needOlderNotGH <- needOlder & notCorrectVersions$repoLocation != "GitHub"
      if (any(needOlderNotGH)) {

        oldAvailableVersions <- if (!exists("oldAvailableVersions", envir = .pkgEnv) || isTRUE(purge)) {
          pkg <- notCorrectVersions[repoLocation != "GitHub" & needOlder]$Package
          names(pkg) <- pkg
          ava <- lapply(pkg, function(p) {
            as.data.table(archiveVersionsAvailable(p, repos = getOption("repos")), keep.rownames = "PackageUrl")
          })
          assign("oldAvailableVersions", ava, envir = .pkgEnv)
          ava
          # versions::available.versions(notCorrectVersions[repoLocation != "GitHub" & needOlder]$Package)
        } else {
          get("oldAvailableVersions", envir = .pkgEnv, inherits = FALSE)
        }
        oldAvailableVersions <- rbindlist(oldAvailableVersions, idcol = "Package")
        # delete unwanted columns
        set(oldAvailableVersions, NULL, c("size", "isdir", "mode", "mtime",
                                          "ctime", "atime", "uid", "gid", "uname", "grname"),
            NULL)
        setDT(oldAvailableVersions)
        oldAvailableVersions[, OlderVersionsAvailable := gsub(".*_(.*)\\.tar\\.gz", "\\1", PackageUrl)]
        needOlderDT <- notCorrectVersions[needOlder & repoLocation != "GitHub"]
        oldAvailableVersions[, OlderVersionsAvailableCh := as.character(package_version(OlderVersionsAvailable))]

        oldAvailableVersions <- needOlderDT[oldAvailableVersions, on = c("Package"), roll = TRUE]
        oldAvailableVersions[, compareVersionAvail := .compareVersionV(OlderVersionsAvailableCh, versionSpec)]
        oldAvailableVersions[, correctVersionAvail :=
                               .evalV(.parseV(text = paste(compareVersionAvail, inequality, "0")))]
        if (any(oldAvailableVersions$correctVersionAvail)) {
          oldAvailableVersions[correctVersionAvail == TRUE, archiveSource := "Archive"]
          oldAvailableVersions <- oldAvailableVersions[correctVersionAvail == TRUE & archiveSource == "Archive"]
          oldAvailableVersions <- oldAvailableVersions[!is.na(archiveSource)]
          oldAvailableVersions[, repoLocation := archiveSource]
          setorderv(oldAvailableVersions, "OlderVersionsAvailableCh", order = -1L)
        }

        oldAvailableVersions <- oldAvailableVersions[, if (NROW(.SD) == 0) .SD else .SD[1], by = "Package"]
        set(oldAvailableVersions, NULL, c("OlderVersionsAvailableCh"), NULL)

        notCorrectVersions <- rbindlist(list(notCorrectVersions[!(repoLocation != "GitHub" & needOlder)],
                                             oldAvailableVersions), fill = TRUE, use.names = TRUE)
      }
      # do GitHub second
      if (any(notCorrectVersions$repoLocation == "GitHub")) {
        notCorrectVersions <- getGitHubDESCRIPTION(notCorrectVersions)
        notCorrectVersions[repoLocation == "GitHub", AvailableVersion := DESCRIPTIONFileVersion(DESCFile)]
        notCorrectVersions[repoLocation == "GitHub", compareVersionAvail := .compareVersionV(AvailableVersion, versionSpec)]
        notCorrectVersions[repoLocation == "GitHub", correctVersionAvail :=
                             .evalV(.parseV(text = paste(compareVersionAvail, inequality, "0")))]
        set(notCorrectVersions, NULL, c("url", "DESCFile"), NULL)

      }
      pkgDT <- rbindlist(list(pkgDT[!correctVersion == FALSE | hasVersionSpec == FALSE],
                              notCorrectVersions), fill = TRUE, use.names = TRUE)
    } else {
      pkgDT[, correctVersionAvail := NA]
    }
  }

  pkgDT
}

#' @inheritParams Require
#' @rdname Require-internals
#' @export
installFrom <- function(pkgDT) {
  cn <- colnames(pkgDT)
  if (!"installed" %in% cn) {
    stop("pkgDT needs a column named 'installed' to indicate whether it is installed or not")
  }
  pkgDT <- pkgDT[correctVersion == FALSE | is.na(correctVersion) & installed == FALSE, needInstall := TRUE]
  if (NROW(pkgDT[needInstall == TRUE])) {
    pkgDT[needInstall == TRUE & # installed == FALSE &
            (correctVersionAvail == TRUE | is.na(correctVersionAvail)) & repoLocation == "CRAN",
          installFrom := repoLocation]
    pkgDT[needInstall == TRUE & # installed == FALSE &
            (correctVersionAvail == TRUE | is.na(correctVersionAvail)) & repoLocation == "GitHub",
          installFrom := repoLocation]
    pkgDT[needInstall == TRUE & # installed == FALSE &
            correctVersionAvail == FALSE,
          installFrom := "Fail"]
    if ("OlderVersionsAvailable" %in% colnames(pkgDT)) {
      pkgDT[needInstall == TRUE & # installed == FALSE &
              (correctVersionAvail == TRUE) &
              repoLocation == "Archive", installFrom := "Archive"]
      # pkgDT[needInstall == TRUE & # installed == FALSE &
      #         (correctVersionAvail == TRUE) &
      #         repoLocation == "Versions", installFrom := repoLocation]
    }
  } else {
    pkgDT[, installFrom := NA_character_]
  }

  pkgDT
}

#' @rdname DESCRIPTION-helpers
#' @param file A file path to a DESCRIPTION file
DESCRIPTIONFileVersion <- function(file) {
  out <- lapply(file, function(f) {
    lines <- readLines(f);
    Sys.setlocale(locale = "C") # required to deal with non English characters in Author names
    on.exit(Sys.setlocale(locale = ""))
    vers_line <- lines[grep("^Version: *", lines)] # nolint
    gsub("Version: ", "", vers_line)
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
#' \code{getGitHubDESCRIPTION} retrieves the DESCRIPTION file from GitHub.com
#'
#' @rdname DESCRIPTION-helpers
#' @export
#' @param pkg A character string with a GitHub package specification (c.f. remotes)
getGitHubDESCRIPTION <- function(pkg) {
  ret <- if (length(pkg) > 0) {
    if (is.data.table(pkg)) {
      if (!all(c("Account", "Repo", "Branch") %in% colnames(pkg))) {
        if (any(c("packageFullName") %in% colnames(pkg))) {
          pkg <- pkg$packageFullName
        }
      }
    }

    pkgDT <- parseGitHub(pkg)
    pkgDT[repoLocation == "GitHub",
          url := {
            if (hasSubFolder) {
              Branch <- paste0(Branch, "/", GitSubFolder)
            }
            file.path("https://raw.githubusercontent.com", Account,
                      Repo, Branch, "DESCRIPTION", fsep = "/")
          },
          by = "Package"]

    checkPath(dirname(tempfile()), create = TRUE)
    pkgDT[repoLocation == "GitHub",
      DESCFile := {
        destFile <- tempfile()
        download.file(url, destFile, overwrite = TRUE, quiet = TRUE) ## TODO: overwrite?
        destFile
      }, by = c("Package", "Branch")]
    pkgDT[]
  } else {
    pkg
  }
  ret
}



updateInstalled <- function(pkgDT, installPkgNames, warn) {
  if (missing(warn)) warn <- warnings()
  warnOut <- unlist(lapply(installPkgNames, function(ip) grepl(ip, names(warn))))
  if (any(!warnOut) | length(warnOut) == 0) {
    pkgDT[Package %in% installPkgNames, installed := TRUE]
  }
  pkgDT[]
}

#' @inheritParams Require
#' @rdname Require-internals
#' @export
#' @details
#' \code{doInstall} is a wrapper around \code{install.packages},
#' \code{remotes::install_github}, and \code{remotes::install_version}.
doInstalls <- function(pkgDT, install_githubArgs, install.packagesArgs,
                       install = TRUE, repos = getOption("repos"), ...) {
  if (any(!pkgDT$installed | NROW(pkgDT[correctVersion == FALSE]) > 0) &&
      (isTRUE(install) || install == "force")) {

    toInstall <- pkgDT[installed == FALSE  | !correctVersion]
    if (any(!toInstall$installFrom %in% "Fail")) {
      if (any("CRAN" %in% toInstall$installFrom)) {
        installPkgNames <- toInstall[installFrom == "CRAN"]$Package
        names(installPkgNames) <- installPkgNames
        dots <- list(...)
        if (is.null(dots$dependencies))
          dots$dependencies <- NA # This should be dealt with manually, but apparently not

        ap <- available.packagesCached(repos, purge = FALSE)
        warn <- tryCatch({
          out <- do.call(install.packages,
                         append(append(list(installPkgNames, available = ap), install.packagesArgs),
                                dots))
        }, warning = function(condition) condition)

        if (!is.null(warn))
          warning(warn)
        pkgDT <- updateInstalled(pkgDT, installPkgNames, warn)
        permDen <- grepl("Permission denied", names(warn))
        packagesDen <- gsub("^.*[\\/](.*).dll.*$", "\\1", names(warn))
        if (any(permDen)) {
          stopMess <- character()
          if (any(pkgDT[Package %in% packagesDen]$installFrom == "CRAN"))
            stopMess <- c(
              stopMess,
              paste0("Due to permission denied, you will have to restart R, and reinstall:\n",
                     "------\n",
                     #"install.packages(c('",paste(pkgs, collapse = ", "),"'), lib = '", libPaths[1],"')",
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
      if (any("GitHub" %in% toInstall$installFrom)) {
        gitPkgNames <- toInstall[installFrom == "GitHub"]
        out5 <- install_githubV(gitPkgNames, install_githubArgs = install_githubArgs, ...)
        pkgDT <- updateInstalled(pkgDT, gitPkgNames$Package, warnings())
      }
      # if (any("Versions" %in% toInstall$installFrom)) {
      #   installPkgNames <- toInstall[installFrom == "Versions"]$Package
      #   out <- versions::install.versions(toInstall[installFrom == "Versions"]$Package,
      #                                     versions = toInstall[installFrom == "Versions"]$OlderVersionsAvailable)
      #   pkgDT <- updateInstalled(pkgDT, installPkgNames, warnings())
      # }
      Archive <- "Archive"
      if (any(Archive %in% toInstall$installFrom)) {
        message("installing older versions is still experimental and may cause package version conflicts")
        installPkgNames <- toInstall[installFrom == Archive]$Package
        names(installPkgNames) <- installPkgNames
        installVersions <- toInstall[installFrom == Archive]$OlderVersionsAvailable
        out <- Map(p = installPkgNames, v = installVersions, function(p, v, ...) {
          do.call(remotes::install_version, list(package = p, version = v, ...))
        })
        pkgDT <- updateInstalled(pkgDT, installPkgNames, warnings())
      }
    }
    if (NROW(pkgDT[installFrom == "Fail"])) {
      keepCols <- c("packageFullName", "installed", "correctVersion", "AvailableVersion")
      message("The following packages could not be installed because could not find a correct version")
      messageDF(pkgDT[installFrom == "Fail", ..keepCols])
      pkgDTFail <- pkgDT[installFrom == "Fail"]
    }
  }

  # Now that it is installed, add installed version to Version column
  pkgDT[needInstall == TRUE & installed == TRUE, Version :=
          unlist(lapply(Package, function(x) as.character(
            tryCatch(packageVersion(x), error = function(x) NA_character_))))]
  pkgDT[toLoad > 0, toLoad := toLoad * as.integer(is.na(installFrom) | installFrom != "Fail")]

  pkgDT
}

#' @details
#' \code{doLoading} is a wrapper around \code{require}.
#'
#' @export
#' @importFrom utils capture.output
#' @rdname Require-internals
doLoading <- function(pkgDT, require = TRUE, ...) {
  packages <- pkgDT[toLoad > 0]$Package
  packageOrder <- pkgDT[toLoad > 0]$toLoad
  if (isTRUE(require)) {
    # packages <- extractPkgName(pkgDT$Package)
    names(packages) <- packages
    packages <- packages[order(packageOrder)]
    requireOut <- lapply(packages, function(pkg) {
      outMess <- capture.output({
        out <- require(pkg, character.only = TRUE)
      }, type = "message")
      warn <- warnings()
      grep3a <- "no package called"
      grep3b <- "could not be found"
      missingDeps <- grepl(paste0(grep3a, "|", grep3b), outMess)

      grep2 <- "package or namespace load failed for"
      grep1 <- "onLoad failed in loadNamespace"
      otherErrors <- grepl(paste(grep1, "|", grep2), outMess)
      toInstall <- character()
      if (any(otherErrors) || any(missingDeps)) {
        if (any(otherErrors)) {
          error1 <- grepl(.libPaths()[1], outMess)
          error1Val <- gsub(paste0("^.*",.libPaths()[1], "\\/(.*)", "\\/.*$"), "\\1", outMess[error1])
          packageNames <- unique(unlist(lapply(strsplit(error1Val, "\\/"), function(x) x[[1]])))
          error2 <- grepl("no such symbol", outMess)
          if (any(error2)) {
            pkgs <- paste(packageNames, collapse = "', '")
            stop("Can't install ", pkgs, "; you will likely need to restart R and run:\n",
                 "-----\n",
                 "install.packages(c('",paste(pkgs, collapse = ", "),"'), lib = '",libPaths[1],"')",
                 "\n-----\n...before any other packages get loaded")
          }
          error3 <- grepl("is being loaded, but", outMess)
          packageNames <- gsub(paste0("^.*namespace.{2,2}(.*)[[:punct:]]{1} .*$"), "\\1", outMess[error3])
          if (any(error3)) {
            pkgs <- paste(packageNames, collapse = "', '")
            stop("Can't install ", pkgs, "; you will likely need to restart R and run:\n",
                 "-----\n", "install.packages(c('",
                 paste(pkgs, collapse = ", "), "'), lib = '", .libPaths()[1],
                 "')", "\n-----\n...before any other packages get loaded")
          }
        }
        doDeps <- if (!is.null(list(...)$dependencies)) list(...)$dependencies else TRUE
        if (any(missingDeps) && doDeps) {
          grep3a_1 <- paste0(".*",grep3a,".{2}(.*).{1}")
          packageNames <- character()
          if (any(grepl(grep3a, outMess[missingDeps])))
            packageNames <- unique(gsub(grep3a_1, "\\1", outMess[missingDeps]))
          grep3b_1 <- paste0(".*package.{2}(.*).{2}required.*$")
          if (any(grepl(grep3b, outMess[missingDeps])))
            packageNames <- unique(c(packageNames,
                                     unique(gsub(grep3b_1, "\\1", outMess[missingDeps]))))
          toInstall <- c(toInstall, packageNames)
          outMess <- grep(grep2, outMess, value = TRUE, invert = TRUE)
          outMess <- grep(grep3a, outMess, value = TRUE, invert = TRUE)
          outMess <- grep(grep3b, outMess, value = TRUE, invert = TRUE)
        }
      }
      if (length(outMess) > 0)
        message(paste0(outMess, collapse = "\n"))
      return(list(out = out, toInstall = toInstall))
    })
    requireOut
    out <- unlist(lapply(requireOut, function(x) x$out))
    toInstall <- unlist(lapply(requireOut, function(x) x$toInstall))

    if (length(toInstall)) {
      out2 <- Require(unique(toInstall), ...)
      out2 <- unlist(out2)
      names(out2) <- unique(toInstall)

      out <- c(out, out2)
    }

    pkgDT[, loaded := (pkgDT$Package %in% names(out)[unlist(out)] & toLoad > 0)]
  }
  pkgDT[]
}

#' @rdname Require-internals
#' @export
#' @param package A single package name (without version or github specifications)
#' @details
#' \code{archiveVersionsAvailable} searches CRAN Archives for available versions.
#' It has been borrowed from a sub-set of the code in a non-exported function:
#' \code{remotes:::download_version_url}
archiveVersionsAvailable <- function(package, repos) {
  for (repo in repos) {
    if (length(repos) > 1)
      message("Trying ", repo)
    archive <- tryCatch({
      con <- gzcon(url(sprintf("%s/src/contrib/Meta/archive.rds", repo), "rb"))
      on.exit(close(con))
      readRDS(con)
    }, warning = function(e) list(), error = function(e) list())
    info <- archive[[package]]
    if (!is.null(info)) {
      info$repo <- repo
      return(info)
    }
  }
  stop(sprintf("couldn't find package '%s'", package))
}

#' GitHub specific helpers
#'
#' \code{install_githubV} is a vectorized \code{remotes::install_github}.
#' This will attempt to identify all dependencies of all supplied packages first,
#' then load the packages in the correct order so that each of their dependencies
#' are met before each is installed.
#'
#' @param gitPkgNames Character vector of package to install from GitHub
#' @param install_githubArgs Any arguments passed to \code{install_github}
#' @param ... Another way to pass arguments to \code{install_github}
#'
#' @return
#' \code{install_githubV} returns a named character vector indicating packages
#'   successfully installed, unless the word "Failed" is returned, indicating
#'   installation failure. The names will be the full GitHub package name,
#'   as provided to \code{gitPkgNames} in the function call.
#' @export
#' @rdname GitHubTools
#' @examples
#' \dontrun{
#'   install_githubV(c("PredictiveEcology/Require", "PredictiveEcology/quickPlot"))
#' }
#'
install_githubV <- function(gitPkgNames, install_githubArgs = list(), ...) {
  if (!is.data.table(gitPkgNames)) {
    gitPkgNames <- data.table(Package = extractPkgName(gitPkgNames), packageFullName = c(gitPkgNames))
  }
  dots <- list(...)
  if (is.null(dots$dependencies))
    dots$dependencies <- NA # This is NA, which under normal circumstances should be irrelevant
  #  but there are weird cases where the internals of Require don't get correct
  #  version of dependencies e.g., achubaty/amc@development says "reproducible" on CRAN
  #  which has R.oo
  gitPkgs <- trimVersionNumber(gitPkgNames$packageFullName)
  names(gitPkgs) <- gitPkgs
  isTryError <- unlist(lapply(gitPkgs, is, "try-error"))
  attempts <- rep(0, length(gitPkgs))
  names(attempts) <- gitPkgs
  while (length(gitPkgs)) {
    gitPkgDeps2 <- gitPkgs[unlist(lapply(seq_along(gitPkgs), function(ind) {
      all(!extractPkgName(names(gitPkgs))[-ind] %in% extractPkgName(gitPkgs[[ind]]))
    }))]
    outRes <- lapply(gitPkgDeps2, function(p) {
      try(do.call(remotes::install_github, append(append(list(p), install_githubArgs), dots)))
    })
    attempts[names(outRes)] <- attempts[names(outRes)] + 1
    if (any(attempts > 1)) {
      failedAttempts <- attempts[attempts > 1]
      outRes[attempts > 1] <- "Failed"
    }
    isTryError <- unlist(lapply(outRes, is, "try-error"))
    gitPkgs1 <- gitPkgs[!names(gitPkgs) %in% names(outRes)[!isTryError]]
    if (identical(gitPkgs1, gitPkgs)) {
      failedAttempts <- names(gitPkgs)
      gitPkgs <- character()
    }
    gitPkgs <- gitPkgs1
  }
  outRes
}

getPkgDeps <- function(packages, which, purge = getOption("Require.purge", FALSE)) {
  pkgs <- trimVersionNumber(packages)
  out1 <- pkgDep(pkgs, recursive = TRUE, which = which, purge = purge)
  out1 <- unique(unname(unlist(out1)))
  out2 <- c(out1, pkgs)
  out3 <- c(out1, packages)
  dt <- data.table(github = extractPkgGitHub(out2), Package = out2,
                   depOrOrig = c(rep("dep", length(out1)), rep("orig", length(pkgs))),
                   packageFullName = out3)
  set(dt, NULL, "origOrder", seq_along(dt$github))
  dt[, bothDepAndOrig := length(depOrOrig) > 1, by = "Package"]
  dt[bothDepAndOrig == TRUE, depOrOrig := "both"]


  if ("github" %in% colnames(dt))
    setorderv(dt, na.last = TRUE, "github") # keep github packages up at top -- they take precedence
  haveVersion <- dt[Package != packageFullName] # have no version number or are github
  haveNoVersion <- dt[Package == packageFullName] # have no version number or are github
  dt <- rbindlist(list(haveVersion, haveNoVersion[!Package %in% haveVersion$Package][!duplicated(Package)]))
  setorderv(dt, "origOrder")
  dt$packageFullName
}

colsToKeep <- c("Package", "loaded", "LibPath", "Version", "packageFullName",
                "installed", "repoLocation", "correctVersion", "correctVersionAvail",
                "toLoad", "hasVersionSpec")

installedVers <- function(pkgDT) {
  if (NROW(pkgDT)) {
    pkgs <- unique(pkgDT$Package)
    names(pkgs) <- pkgs
    installedPkgsCurrent <- lapply(pkgs, function(p) {
      out <- tryCatch(find.package(p), error = function(x) NA)
      descV <- if (!is.na(out)) {
        descV <- DESCRIPTIONFileVersion(file.path(out, "DESCRIPTION"))
        cbind("Package" = p, LibPath = dirname(out), "Version" = descV)
      } else {
        cbind("Package" = p, LibPath = NA_character_, "Version" = NA_character_)
      }
      descV
    })
    installedPkgsCurrent <- do.call(rbind, installedPkgsCurrent)
    installedPkgsCurrent <- as.data.table(installedPkgsCurrent)
    pkgDT <- installedPkgsCurrent[pkgDT, on = "Package"]
  } else {
    pkgDT <- cbind(pkgDT, LibPath = NA_character_, "Version" = NA_character_)
  }
  pkgDT[]
}

#' @importFrom utils available.packages
available.packagesCached <- function(repos, purge) {
  repos <- getCRANrepos(repos)

  if (!exists("cachedAvailablePackages", envir = .pkgEnv) || isTRUE(purge)) {
    cap <- list()
    isMac <- tolower(Sys.info()["sysname"]) == "darwin"
    isOldMac <- isMac && compareVersion(as.character(getRversion()), "4.0.0") < 0
    isWindows <- (tolower(Sys.info()["sysname"]) == "windows")

    types <- if (isOldMac) {
      c("mac.binary.el-capitan", "source")
    } else if (!isWindows && !isMac) {
      c("source")
    } else {
      c("binary", "source")
    }

    for (type in types)
      cap[[type]] <- available.packages(repos = repos, type = type) #, ignore_repo_cache = isOldMac | !isInteractive())
    cap <- do.call(rbind, cap)
    if (length(types) > 1) {
      dups <- duplicated(cap[, c("Package", "Version")])
      cap <- cap[!dups,]
    }
    assign("cachedAvailablePackages", cap, envir = .pkgEnv)
    cap
  } else {
    get("cachedAvailablePackages", envir = .pkgEnv, inherits = FALSE)
  }
}
