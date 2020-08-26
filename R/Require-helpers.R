utils::globalVariables(c(
  "localFileName", "neededFiles", "i.neededFiles", "installFromFac",
  ".N", "Archs", "type"
))

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
  pkgDT <- toPkgDT(pkgDT)
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
  pkgDT <- toPkgDT(pkgDT)
  
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
#' @importFrom data.table setkeyv
#' @rdname Require-internals
getAvailable <- function(pkgDT, purge = FALSE, repos = getOption("repos")) {
  if (NROW(pkgDT[correctVersion == FALSE | is.na(correctVersion)])) {
    whNotCorrect <- pkgDT[, .I[hasVersionSpec == TRUE & (correctVersion == FALSE | is.na(correctVersion))]]
    if (NROW(whNotCorrect)) {
      notCorrectVersions <- pkgDT[whNotCorrect]

      # do CRAN first
      if (any(notCorrectVersions$repoLocation == "CRAN")) {
        cachedAvailablePackages <- available.packagesCached(repos = repos, purge = purge)
        cachedAvailablePackages <- as.data.table(cachedAvailablePackages[, c("Package", "Version", "Archs")])
        setnames(cachedAvailablePackages, "Version", "AvailableVersion")
        notCorrectVersions <- cachedAvailablePackages[notCorrectVersions, on = "Package"]
        notCorrectVersions[repoLocation != "GitHub" & is.na(AvailableVersion), AvailableVersion := "10000000"]
        notCorrectVersions[repoLocation != "GitHub",
                           compareVersionAvail := .compareVersionV(AvailableVersion, versionSpec)]
        notCorrectVersions[repoLocation != "GitHub",
                           correctVersionAvail :=
                             .evalV(.parseV(text = paste(compareVersionAvail, inequality, "0")))]

        # If package has both a binary and source available on CRAN, there will be 2 entries
        notCorrectVersions[correctVersionAvail == TRUE, N := .N, by = "packageFullName"]
        setorderv(notCorrectVersions, "correctVersionAvail", order = -1) # put TRUE first
        notCorrectVersions <- notCorrectVersions[, .SD[1], by = "packageFullName"] # Take first of multiples
        notCorrectVersions[, N := .N, by = "packageFullName"]
        if (any(notCorrectVersions[correctVersionAvail == TRUE]$N > 1)) {
          notCorrectVersions <- notCorrectVersions[correctVersionAvail == TRUE, .SD[1], by = "packageFullName"] # take smaller one, as it will be binary
          notCorrectVersions[N > 1, type := ifelse(is.na(Archs), "source", "binary")]
        }
        set(notCorrectVersions, NULL, "N", NULL)
      }

      # do Older Versions
      needOlder <- notCorrectVersions$correctVersionAvail == FALSE & grepl("==|<=|<", notCorrectVersions$inequality)
      needOlderNotGH <- needOlder & notCorrectVersions$repoLocation != "GitHub"
      if (any(needOlderNotGH)) {

        pkg <- notCorrectVersions[repoLocation != "GitHub" & needOlder]$Package
        oldAvailableVersions <- if (!is.null(.pkgEnv$oldAvailableVersions)) {.pkgEnv$oldAvailableVersions} else {list()}
        pkgsInOAV <- pkg %in% names(oldAvailableVersions)
        if (!all(pkgsInOAV)) {
          pkgs <- pkg[!pkgsInOAV]
          names(pkgs) <- pkgs
          # if (length(pkgs) > 20) message("Looking which archive versions are available; this could take a while")
          ava <- lapply(pkgs, function(p) {
            as.data.table(archiveVersionsAvailable(p, repos = repos), keep.rownames = "PackageUrl")
          })
          oldAvailableVersions <- append(oldAvailableVersions, ava)
          assign("oldAvailableVersions", oldAvailableVersions, envir = .pkgEnv)
        }
        oldAvailableVersions <- oldAvailableVersions[pkg]

        # oldAvailableVersions <- if (!exists("oldAvailableVersions", envir = .pkgEnv) || isTRUE(purge)) {
        #   names(pkg) <- pkg
        #   ava <- lapply(pkg, function(p) {
        #     as.data.table(archiveVersionsAvailable(p, repos = getOption("repos")), keep.rownames = "PackageUrl")
        #   })
        #   assign("oldAvailableVersions", ava, envir = .pkgEnv)
        #   ava
        #   # versions::available.versions(notCorrectVersions[repoLocation != "GitHub" & needOlder]$Package)
        # } else {
        #   get("oldAvailableVersions", envir = .pkgEnv, inherits = FALSE)
        # }

        oldAvailableVersions <- rbindlist(oldAvailableVersions, idcol = "Package", 
                                          fill = TRUE, use.names = TRUE)
        # delete unwanted columns
        set(oldAvailableVersions, NULL, c("size", "isdir", "mode", #"mtime",
                                          "ctime", "atime", "uid", "gid", "uname", "grname"),
            NULL)
        setDT(oldAvailableVersions)
        oldAvailableVersions[, OlderVersionsAvailable := gsub(".*_(.*)\\.tar\\.gz", "\\1", PackageUrl)]
        needOlderDT <- notCorrectVersions[needOlder & repoLocation != "GitHub"]
        oldAvailableVersions[, OlderVersionsAvailableCh := as.character(package_version(OlderVersionsAvailable))]

        oldAvailableVersions <- needOlderDT[oldAvailableVersions, on = c("Package"), roll = TRUE, allow.cartesian = TRUE]
        oldAvailableVersions[, compareVersionAvail := .compareVersionV(OlderVersionsAvailableCh, versionSpec)]
        oldAvailableVersions[, correctVersionAvail :=
                               .evalV(.parseV(text = paste(compareVersionAvail, inequality, "0")))]
        if (any(oldAvailableVersions$correctVersionAvail)) {
          oldAvailableVersions[correctVersionAvail == TRUE, archiveSource := "Archive"]
          currDates <- currentCRANPkgDates(unique(oldAvailableVersions$Package))
          oldAvailableVersions <- rbindlist(list(oldAvailableVersions, currDates), use.names = TRUE, fill = TRUE)
          data.table::setkeyv(oldAvailableVersions, c("Package", "mtime", "CRANVersion"))
          bb <- oldAvailableVersions[correctVersionAvail == TRUE & archiveSource == "Archive"]

          aa <- oldAvailableVersions[, list(nextRow = min(na.rm = TRUE, max(.I, na.rm = TRUE),
                                                       .I[correctVersionAvail == TRUE & archiveSource == "Archive"] + 1)), by = Package]
          desiredDates <- oldAvailableVersions[aa$nextRow, list(Package, newMtime = mtime - 60*60*24)]


          oldAvailableVersions <- desiredDates[bb, on = "Package"]
          oldAvailableVersions[, mtime := newMtime]

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
        notCorrectVersions[repoLocation == "GitHub", AvailableVersion := DESCRIPTIONFileVersionV(DESCFile)]
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
installFrom <- function(pkgDT, purge = FALSE, repos = getOption("repos")) {
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


  # Check for local copy of src or binary first
  if (!is.null(getOption("Require.RPackageCache"))) {
    localFiles <- dir(getOption("Require.RPackageCache"), full.names = TRUE)
    # sanity check -- there are bad files, quite often
    fileSizeEq0 <- file.size(localFiles) == 0
    if (any(fileSizeEq0)) {
      unlink(localFiles[fileSizeEq0])
      localFiles <- localFiles[!fileSizeEq0]
    }
    neededVersions <- pkgDT[!is.na(installFrom) & installFrom != "Fail"]
    if (length(localFiles) && NROW(neededVersions)) {
      localFiles <- basename(localFiles)
      set(neededVersions, NULL, "neededFiles", character(NROW(neededVersions)))
      if (any(neededVersions$installFrom == "Archive")) {
        neededVersions[installFrom == "Archive", neededFiles := paste0(Package, "_", OlderVersionsAvailable)]
      }
      if (any(neededVersions$installFrom == "CRAN") && "AvailableVersion" %in% colnames(pkgDT)) {
        neededVersions[installFrom == "CRAN", neededFiles := paste0(Package, "_", AvailableVersion)]
      }
      otherPoss <- nchar(neededVersions$neededFiles) == 0 | endsWith(neededVersions$neededFiles, "NA")
      if (any(otherPoss)) {
        dontKnowVersion <- neededVersions[otherPoss]
        if (NROW(dontKnowVersion)) {
          cachedAvailablePackages <- available.packagesCached(repos = repos, purge = purge)
          cachedAvailablePackages <- as.data.table(cachedAvailablePackages[, c("Package", "Version")])
          dontKnowVersion <- cachedAvailablePackages[dontKnowVersion, on = "Package"][, list(Package, Version)]
          dontKnowVersion[, neededFiles := paste0(Package, "_", Version)][, Version := NULL]
          neededVersions[dontKnowVersion, neededFiles := i.neededFiles , on = "Package"] # join -- keeping dontKnowVersion column
        }
      }

      if (NROW(neededVersions)) {
        nfs <- neededVersions$neededFiles
        names(nfs) <- nfs
        localFileName <- lapply(nfs, function(nf) as.data.table(grep(nf, localFiles, value = TRUE)))
        nfs <- rbindlist(localFileName, idcol = "neededFiles")
        setnames(nfs, old = "V1", new = "localFileName")
        nfs[, localType := ifelse(isBinary(localFileName), "binary", "source")]
        setkeyv(nfs, c("neededFiles", "localType")) # put binary first
        nfs <- nfs[, .SD[1], by = "neededFiles"]
        neededVersions <- nfs[neededVersions, on = c("neededFiles")]#, "type")]
        # neededVersions[, localFileName := grep(neededFiles, localFiles, value = TRUE), by = "neededFiles"]
        neededVersions <- neededVersions[!is.na(localFileName), list(Package, localFileName)]
        if (NROW(neededVersions)) {
          pkgDT[neededVersions, `:=`(installFrom = "Local", localFileName = localFileName), on = "Package"]
        }
      }
    }
  }

  pkgDT
}

#' @rdname DESCRIPTION-helpers
#' @param file A file path to a DESCRIPTION file
DESCRIPTIONFileVersionV <- function(file) {
  #origLocal <- Sys.setlocale(locale = "C") # required to deal with non English characters in Author names
  #on.exit({
  #  Sys.setlocale(locale = origLocal)
  #})
  out <- lapply(file, function(f) {
    if (length(f) == 1) {
      lines <- readLines(f);
    } else {
      lines <- f
    }
      vers_line <- lines[grep("^Version: *", lines)] # nolint
      gsub("Version: ", "", vers_line)
  })
  unlist(out)
}

#' @rdname DESCRIPTION-helpers
#' @param file A file path to a DESCRIPTION file
#' @param other Any other keyword in a DESCRIPTION file that preceeds a ":". The rest of the line will be
#'   retrieved.
DESCRIPTIONFileOtherV <- function(file, other = "RemoteSha") {
  # origLocal <- Sys.setlocale(locale = "C") # required to deal with non English characters in Author names
  # on.exit({
  #   Sys.setlocale(locale = origLocal)
  # })
  out <- lapply(file, function(f) {
    if (length(f) == 1) {
      lines <- readLines(f);
    } else {
      lines <- f
    }
    vers_line <- lines[grep(paste0("^",other,": *"), lines)] # nolint
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
  if (NROW(installPkgNames)) {
    if (missing(warn)) warn <- warnings()
    warnOut <- unlist(lapply(installPkgNames, function(ip) grepl(ip, names(warn))))
    if (any(!warnOut) | length(warnOut) == 0) {
      pkgDT[Package %in% installPkgNames, installed := TRUE]
    }
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
  browser(expr = exists("._doInstalls_0"))
  if (any(!pkgDT$installed | NROW(pkgDT[correctVersion == FALSE]) > 0) &&
      (isTRUE(install) || install == "force")) {
    dots <- list(...)

    toInstall <- pkgDT[installed == FALSE  | !correctVersion]
    hasRequireDeps <- toInstall$Package %in% "Require" #extractPkgName(c("Require", pkgDep("Require")[[1]])) # remove
    if (any(hasRequireDeps)) {
      installRequire()

      #RequireDepsNeeded <- paste0("'", paste(toInstall$Package[hasRequireDeps], collapse = "', '"), "'")
      #message(RequireDepsNeeded, " is needed, but Require cannot be installed by Require.\n",
      #        "You may need to restart R and from a fresh R session, install.packages(c(",
      #        RequireDepsNeeded,"), lib = '",.libPaths()[1],"'), then rerun the current call")
      #toInstall <- toInstall[!hasRequireDeps]
    }
    if (any(!toInstall$installFrom %in% "Fail")) {
      toInstall[, installFromFac := factor(installFrom, levels = c("Local", "CRAN", "Archive", "GitHub", "Fail"))]
      setkeyv(toInstall, "installFromFac")
      if (length(toInstall$packageFullName) > 20)
        message("Performing a topological sort of packages to install them in the right order; this may take some time")
      topoSorted <- pkgDepTopoSort(toInstall$packageFullName)
      toInstall <- toInstall[match(names(topoSorted), packageFullName)]
      out <- by(toInstall, seq(NROW(toInstall)), installAny, pkgDT = pkgDT, dots = dots,
                install.packagesArgs = install.packagesArgs, install_githubArgs = install_githubArgs)
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
      grep4a <- "version.*cannot be unloaded" # means that prior to changing .libPaths, the package was loaded; .libPaths version is older
      grep3a <- "no package called"
      grep3b <- "could not be found"
      missingDeps <- grepl(paste0(grep3a, "|", grep3b), outMess)

      grep2 <- "package or namespace load failed for"
      grep1 <- "onLoad failed in loadNamespace"
      otherErrors <- grepl(paste(grep1, "|", grep2), outMess)
      libPathsVersTooOld <- grepl(grep4a, outMess)
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
      if (any(libPathsVersTooOld)) {
        p <- pkgDT[Package == pkg]
        libPathsVers <- DESCRIPTIONFileVersionV(file.path(.libPaths()[1], pkg, "DESCRIPTION"))
        otherIsCorrect <- getPkgVersions(p, install = FALSE)$correctVersion
        firstPartMess <- paste0(pkg, " is already loaded from ", p$LibPath, " with version ", p$Version, ". ",
                                "The version in .libPaths() is ", libPathsVers)
        if (otherIsCorrect) {
          message(firstPartMess, ". Because the newer version still accommodates the minimum version number (",
                  p$packageFullName,", updating now.")
          oo <- Require(pkg, require = FALSE, install = "force")
          outMessToRm <- grep("Loading required|Failed with error|Error in unloadNamespace", outMess)
          outMessToRm <- c(outMessToRm, max(outMessToRm) + 1) # There is non ASCII character in the message that can't be explicitly used
          outMess <- outMess[-outMessToRm]
        } else {
          stop(firstPartMess, ". The newer version fails the version number test. Please either change the version number requested, ",
               "or prevent the newer version from loading by changing the .libPaths() prior to any packages being loaded")
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
    archiveFile <- sprintf("%s/src/contrib/Meta/archive.rds", repo)
    if (length(repos) > 1)
      message("Trying ", repo)
    if (!exists(archiveFile, envir = .pkgEnv, inherits = FALSE)) {
      archive <- tryCatch({
        con <- gzcon(url(archiveFile, "rb"))
        on.exit(close(con))
        readRDS(con)
      }, warning = function(e) list(), error = function(e) list())
      .pkgEnv[[archiveFile]] <- archive
    } else {
      archive <- get(archiveFile, envir = .pkgEnv)
    }
    info <- archive[[package]]
    if (!is.null(info)) {
      info$repo <- repo
      return(info)
    }
  }
  # warning(sprintf("couldn't find package '%s'", package))
  return(info)
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
#' @param dots A list of ..., e.g., list(...). Only for internal use.
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
install_githubV <- function(gitPkgNames, install_githubArgs = list(), dots = dots) {
  if (!is.data.table(gitPkgNames)) {
    gitPkgNames <- data.table(Package = extractPkgName(gitPkgNames), packageFullName = c(gitPkgNames))
  }
  if (is.null(dots$dependencies) && is.null(install_githubArgs$dependencies))
    dots$dependencies <- NA # This is NA, which under normal circumstances should be irrelevant
  #  but there are weird cases where the internals of Require don't get correct
  #  version of dependencies e.g., achubaty/amc@development says "reproducible" on CRAN
  #  which has R.oo
  #sortedTopologically <- pkgDepTopoSort(gitPkgNames$packageFullName)
  #installPkgNames <- names(sortedTopologically)
  installPkgNames <- gitPkgNames$packageFullName

  names(installPkgNames) <- installPkgNames

  ord <- match(extractPkgName(installPkgNames), gitPkgNames$Package)
  gitPkgNames <- gitPkgNames[ord]

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
  pkgDT <- toPkgDT(pkgDT)
  if (NROW(pkgDT)) {
    pkgs <- unique(pkgDT$Package)
    names(pkgs) <- pkgs
    installedPkgsCurrent <- lapply(pkgs, function(p) {
      out <- tryCatch(find.package(p, lib.loc = .libPaths()), error = function(x) NA)
      descV <- if (!is.na(out)) {
        descV <- DESCRIPTIONFileVersionV(file.path(out, "DESCRIPTION"))
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
      cap[[type]] <- tryCatch(available.packages(repos = repos, type = type),
                              error = function(x)
                                available.packages(ignore_repo_cache = TRUE, repos = repos, type = type))#, ignore_repo_cache = isOldMac | !isInteractive())
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

currentCRANPkgDates <- function(pkgs) {
  if (!exists("currentCranDates", envir = .pkgEnv)) {
    message("Getting dates of current CRAN packages")
    tf <- tempfile();
    download.file(file.path(getOption("repos")["CRAN"], "src/contrib/"), tf, quiet = TRUE)
    avail <- readLines(tf)
    if (is.null(names(pkgs))) names(pkgs) <- pkgs
    avail <- sapply(pkgs, function(pkg) grep(paste0("\"", pkg, "\\_.*\\.tar\\.gz"), avail, value = TRUE))
    pkgsDateAvail <- lapply(avail, function(ava) 
      data.table(date = unique(gsub(paste0(".*(20[0-2][0-9]-[0-1][0-9]-[0-3][0-9]).*"), "\\1", ava)),
                 CRANVersion = unique(gsub(paste0("^.+>[[:alnum:]]+\\_(.*)\\.tar\\.gz<.*"), "\\1", ava))))
    pkgsDateAvail <- rbindlist(pkgsDateAvail[sapply(pkgsDateAvail, function(x) length(x) > 0)], idcol = "Package")
    currentCranDates <- pkgsDateAvail[, mtime := as.POSIXct(date)]
    set(currentCranDates, NULL, "date", NULL)
    assign("currentCranDates", currentCranDates, envir = .pkgEnv)
  } else {
    currentCranDates <- get("currentCranDates", envir = .pkgEnv)
  }
  currentCranDates
}

installLocal <- function(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs) {
  installFromCur <- "Local"
  installPkgNames <- toInstall[installFrom == installFromCur]$Package

  # sortedTopologically <- pkgDepTopoSort(installPkgNames)
  # installPkgNames <- names(sortedTopologically)
  #
  names(installPkgNames) <- installPkgNames

  ord <- match(installPkgNames, toInstall[installFrom == installFromCur]$Package)
  toIn <- toInstall[installFrom == installFromCur][ord]

  if (is.null(dots$dependencies) & is.null(install.packagesArgs$dependencies))
    dots$dependencies <- NA # This was NA; which means let install.packages do it. But, failed in some cases:

  message("Using local cache of ", paste(toIn$localFileName, collapse = ", "))
  installPkgNames <- normPath(file.path(getOption("Require.RPackageCache"), toIn$localFileName))
  names(installPkgNames) <- installPkgNames

  installPkgNamesBoth <- split(installPkgNames, endsWith(installPkgNames, "zip"))

  warn <- lapply(installPkgNamesBoth, function(installPkgNames) {
    # Deal with "binary" mumbo jumbo
    type <- c("source", "binary")[endsWith(installPkgNames, "zip") + 1]
    isBin <- isBinary(installPkgNames)
    buildBinDots <- grepl("--build", dots)
    buildBinIPA <- grepl("--build", install.packagesArgs)
    buildBin <- any(buildBinDots, buildBinIPA)
    if (isBin && buildBin) {
      if (any(buildBinDots)) dots[buildBinDots] <- gsub("--build", "", dots[buildBinDots])
      if (any(buildBinIPA)) install.packagesArgs[buildBinIPA] <- gsub("--build", "", install.packagesArgs[buildBinIPA])
    }
    warns <- lapply(installPkgNames, function(installPkgName) { # use lapply so any one package fail won't stop whole thing
      warn <- suppressMessages(tryCatch({
        do.call(install.packages,
                # using ap meant that it was messing up the src vs bin paths
                append(append(list(installPkgName, repos = NULL, type = type), install.packagesArgs), # removed , available = ap
                       dots))
      }, warning = function(condition) condition)
      )
      if (!isBin && buildBin) copyTarball(basename(installPkgName), TRUE)
      warn
    })
  })

  warn <- unlist(warn)
  if (!is.null(warn))
    warning(warn)
  pkgDT <- updateInstalled(pkgDT, toInstall$Package, warn)
  permDen <- grepl("Permission denied", names(warn))
  packagesDen <- gsub("^.*[\\/](.*).dll.*$", "\\1", names(warn))
  if (any(permDen)) {
    stopMess <- character()
    if (any(pkgDT[Package %in% packagesDen]$installFrom == installFromCur))
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
  pkgDT
}

installCRAN <- function(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs) {
  browser(expr = exists("._installCRAN_0"))
  installPkgNames <- toInstall[installFrom == "CRAN"]$Package

  # sortedTopologically <- pkgDepTopoSort(installPkgNames)
  # installPkgNames <- names(sortedTopologically)

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

  warn <- tryCatch({
    out <- do.call(install.packages,
                   # using ap meant that it was messing up the src vs bin paths
                   append(append(list(installPkgNames), install.packagesArgs), # removed , available = ap
                          dots))
  }, warning = function(condition) condition)
  if (any(grepl("--build", c(dots, install.packagesArgs))))
    copyTarball(installPkgNames, TRUE)

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
  pkgDT
}

installArchive <- function(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs) {
  Archive <- "Archive"
  browser(expr = exists("._doInstalls_2"))
  message("installing older versions is still experimental and may cause package version conflicts")
  installPkgNames <- toInstall[installFrom == Archive]$Package
  # sortedTopologically <- pkgDepTopoSort(installPkgNames)
  # installPkgNames <- names(sortedTopologically)

  names(installPkgNames) <- installPkgNames

  ord <- match(installPkgNames, toInstall[installFrom == Archive]$Package)
  toIn <- toInstall[installFrom == Archive][ord]

  installVersions <- toIn$OlderVersionsAvailable

  # warns <- list()
  dateFromMRAN <- as.Date(gsub(" .*", "", toIn$mtime))
  onMRAN <- dateFromMRAN > "2015-06-06" && tolower(Sys.info()[["sysname"]]) == "windows"
  if (any(onMRAN)) {
    out <- Map(p = unname(installPkgNames)[onMRAN], date = dateFromMRAN[onMRAN], v = installVersions[onMRAN], function(p, date, v, ...) {
      warn <- list()
      tryCatch(
        out <- do.call(install.packages,
                       # using ap meant that it was messing up the src vs bin paths
                       append(append(list(p, repos = file.path("https://MRAN.revolutionanalytics.com/snapshot", date)), 
                                     install.packagesArgs), # removed , available = ap
                              dots)),
        # install.packages(p, repos = file.path("https://MRAN.revolutionanalytics.com/snapshot", date), ...),
        error = function(x) {
          warn <<- paste0("Version ", v, " of ", p, " could not be installed; perhaps try a newer version")
          return(p)
        },
        warning = function(w) {
          w$message
        })
      warn
    })
    out <- unlist(out)
    if (length(out)) lapply(out, warning)
  }
  if (any(!onMRAN)) {
    cranArchivePath <- file.path(getOption("repos"), "src/contrib/Archive/")
    out <- Map(p = toIn$PackageUrl[!onMRAN], v = installVersions[!onMRAN], function(p, v, ...) {
      warn <- list()
      p <- file.path(cranArchivePath, p)
      tryCatch(
        out <- do.call(install.packages,
                       # using ap meant that it was messing up the src vs bin paths
                       append(append(list(unname(p), repos = NULL), install.packagesArgs), # removed , available = ap
                              dots)),
        # ret <- do.call(remotes::install_version, append(list(package = unname(p), version = v, ...), install_githubArgs)),
        error = function(x) {
          warn <<- paste0("Version ", v, " of ", p, " could not be installed; perhaps try a newer version")
          return(p)
        })
      warn
    })
    out <- unlist(out)
    if (length(out)) lapply(out, warning)
  }
  updateInstalled(pkgDT, installPkgNames, warnings())
}

installGitHub <- function(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs) {
  gitPkgNames <- toInstall[installFrom == "GitHub"]
  out5 <- install_githubV(gitPkgNames, install_githubArgs = install_githubArgs, dots = dots)
  updateInstalled(pkgDT, gitPkgNames$Package, warnings())
}

installAny <- function(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs) {
  if (any("Local" %in% toInstall$installFrom)) {
    pkgDT <- installLocal(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs)
    if (!isTRUE(pkgDT[Package == toInstall$Package]$installed)) {
      pkgDT[Package == toInstall$Package, installFrom := "CRAN"]
      toInstall[, installFrom := "CRAN"]
    }
  }
  if (any("CRAN" %in% toInstall$installFrom))
    pkgDT <- installCRAN(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs)
  if (any("Archive" %in% toInstall$installFrom))
    pkgDT <- installArchive(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs)
  if (any("GitHub" %in% toInstall$installFrom))
    pkgDT <- installGitHub(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs)
  pkgDT
}

isBinary <- function(fn) {
  endsWith(fn, "zip") | grepl("R_x86", fn)
}

copyTarball <- function(pkg, builtBinary) {
  if (builtBinary) {
    newFiles <- dir(pattern = gsub("\\_.*", "", pkg), full.names = TRUE)
    if (length(newFiles)) {
      file.link(newFiles, file.path(getOption("Require.RPackageCache"), basename(newFiles)))
      unlink(newFiles)
    }
  }

}

installRequire <- function() {
  RequireDESCRIPTIONPath <- system.file("DESCRIPTION", package = "Require")
  DESCRIPTIONFileVersionV(RequireDESCRIPTIONPath)
  isGitHub <- DESCRIPTIONFileOtherV(RequireDESCRIPTIONPath, other = "github")
  done <- FALSE
  if (is.na(isGitHub)) {
    isLocal <- dir(pattern = "DESCRIPTION")
    if (length(isLocal)) {
      currentPackageName <- DESCRIPTIONFileOtherV("DESCRIPTION", "Package")
      if (identical("Require", currentPackageName)) {
        origDir <- setwd("..");
        on.exit(setwd(origDir), add = TRUE)
        system(paste0("R CMD INSTALL --library=", .libPaths()[1], " Require"), wait = TRUE)
        done <- TRUE
      }
    }
    if (isFALSE(done)) {
      system(paste0("Rscript -e \"install.packages(c('Require'), lib ='",.libPaths()[1],"', repos = '",getOption('repos')[["CRAN"]],"')\""), wait = TRUE)
      done <- TRUE
      # system(paste0("Rscript -e \"install.packages(c('data.table', 'remotes'), lib ='",.libPaths()[1],"', repos = '",getOption('repos')[["CRAN"]],"')\""), wait = TRUE)
    }
  } else {
    browser()
    system(paste0("Rscript -e \"install.packages(c('Require'), lib ='",.libPaths()[1],"', repos = '",getOption('repos')[["CRAN"]],"')\""), wait = TRUE)
  }


}

toPkgDT <- function(pkgDT) {
  if (!is.data.table(pkgDT))
    pkgDT <- data.table(Package = extractPkgName(pkgDT), packageFullName = c(pkgDT))
  pkgDT
}