utils::globalVariables(c(
  "localFileName", "neededFiles", "i.neededFiles", "installFromFac",
  ".N", ".I", "Archs", "type", "localType", "N", "installOrder",
  "installResult", "isGitPkg", "keep", "keep2", "github", "dup", "filepath", "destFile",
  "Names", "packageFullName", "Version", "hasVersionSpec", "correctVersion", "repoLocation",
  "inequality", " AvailableVersion", "Package", "mtime", "newMtime"
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
    set(pkgDT, which(isGH), "repoLocation", "GitHub")
    set(pkgDT, which(!isGH), "repoLocation", "CRAN")
    # pkgDT[isGH, repoLocation := "GitHub"]
    # pkgDT[!isGH, repoLocation := "CRAN"]
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
    pkgDT[isGitHub, Branch := "HEAD"]
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
    # pkgDT[hasVersionSpec == TRUE, versionSpec := as.character(max(package_version(versionSpec))), by = "Package"]



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
      # pkgDT <- pkgDT[, .SD[1], by = c("Package")]
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
        cachedAvailablePackages <- cachedAvailablePackages[, c("Package", "Version", "Archs")]
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
        oldAvailableVersions <- if (!is.null(.pkgEnv[["pkgDep"]][["oldAvailableVersions"]])) {
          .pkgEnv[["pkgDep"]][["oldAvailableVersions"]]
        } else {
            list()
          }
        pkgsInOAV <- pkg %in% names(oldAvailableVersions)
        if (!all(pkgsInOAV)) {
          pkgs <- pkg[!pkgsInOAV]
          names(pkgs) <- pkgs
          # if (length(pkgs) > 20) message("Looking which archive versions are available; this could take a while")
          ava <- lapply(archiveVersionsAvailable(pkgs, repos = repos), function(d) {
            as.data.table(d, keep.rownames = "PackageUrl")
          })

          oldAvailableVersions <- append(oldAvailableVersions, ava)
          assign("oldAvailableVersions", oldAvailableVersions, envir = .pkgEnv[["pkgDep"]])
        }
        oldAvailableVersions <- oldAvailableVersions[pkg]

        oldAvailableVersions <- rbindlist(oldAvailableVersions, idcol = "Package",
                                          fill = TRUE, use.names = TRUE)
        # delete unwanted columns
        set(oldAvailableVersions, NULL, c("size", "isdir", "mode", #"mtime",
                                          "ctime", "atime", "uid", "gid", "uname", "grname"),
            NULL)
        setDT(oldAvailableVersions)
        if (NROW(oldAvailableVersions) && "PackageUrl" %in% colnames(oldAvailableVersions)) {
          oldAvailableVersions[, OlderVersionsAvailable := gsub(".*_(.*)\\.tar\\.gz", "\\1", PackageUrl)]
          needOlderDT <- notCorrectVersions[needOlder & repoLocation != "GitHub"]

          # packages installed locally via devtools::install will have no known source -- will be NA
          oldAvailableVersions[!is.na(OlderVersionsAvailable), OlderVersionsAvailableCh := as.character(package_version(OlderVersionsAvailable))]

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

          notCorrectVersions1 <- rbindlist(list(notCorrectVersions[!(repoLocation != "GitHub" & needOlder)],
                                                oldAvailableVersions), fill = TRUE, use.names = TRUE)
          if (!identical(NROW(notCorrectVersions1), NROW(notCorrectVersions))) {
            stillDontHave <- notCorrectVersions[!notCorrectVersions1, on = "packageFullName"]
            if (NROW(stillDontHave)) {
              stillDontHave[, repoLocation := "Unknown"]
              notCorrectVersions1 <- rbindlist(list(notCorrectVersions1, stillDontHave), fill = TRUE, use.names = TRUE)
            }
          }

          notCorrectVersions <- notCorrectVersions1
        }
      }
      # do GitHub second
      if (any(notCorrectVersions$repoLocation == "GitHub")) {
        notCorrectVersions <- getGitHubDESCRIPTION(notCorrectVersions, purge = purge)
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
          `:=`(installFrom = "Fail", installResult = "No available version")]
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
  if (!is.null(rpackageFolder(getOption("Require.RPackageCache")))) {
    localFiles <- dir(rpackageFolder(getOption("Require.RPackageCache")), full.names = TRUE)
    # sanity check -- there are bad files, quite often
    fileSizeEq0 <- file.size(localFiles) == 0
    if (any(fileSizeEq0)) {
      unlink(localFiles[fileSizeEq0])
      localFiles <- localFiles[!fileSizeEq0]
    }
    neededVersions <- pkgDT[!is.na(installFrom) & !(installFrom %in% c("Fail", "Duplicate"))]
    if (length(localFiles) && NROW(neededVersions)) {
      localFiles <- basename(localFiles)
      set(neededVersions, NULL, "neededFiles", character(NROW(neededVersions)))
      if (any(neededVersions$installFrom == "Archive")) {
        neededVersions[installFrom == "Archive", neededFiles := paste0(Package, "_", OlderVersionsAvailable)]
      }
      if (any(neededVersions$installFrom == "CRAN" & is.na(neededVersions$correctVersionAvail))) {
        wh <- neededVersions[, which(installFrom == "CRAN")]
        if ("AvailableVersion" %in% colnames(pkgDT)) {
          av <- which(!is.na(neededVersions$AvailableVersion))
          av <- intersect(av, wh)
          nf <- paste0(neededVersions$Package[av], "_", neededVersions$AvailableVersion[av])
          neededVersions[av, neededFiles := nf]
        } else {
          nf <- neededVersions$Package[wh]
          neededVersions[installFrom == "CRAN", neededFiles := nf]
        }

      }
      if (any(neededVersions$installFrom == "GitHub")) {
        neededVersions[installFrom == "GitHub", neededFiles := paste0(Package, "_", Branch)]
      }
      otherPoss <- nchar(neededVersions$neededFiles) == 0 | endsWith(neededVersions$neededFiles, "NA")
      if (any(otherPoss)) {
        dontKnowVersion <- neededVersions[otherPoss]
        if (NROW(dontKnowVersion)) {
          cachedAvailablePackages <- available.packagesCached(repos = repos, purge = purge)
          cachedAvailablePackages <- cachedAvailablePackages[, c("Package", "Version")]
          dontKnowVersion <- cachedAvailablePackages[dontKnowVersion, on = "Package"][, list(Package, Version)]
          dontKnowVersion[, neededFiles := paste0(Package, "_", Version)]
          # Here, we don't know what version it should be, so take latest from CRAN as the needed version
          neededVersions[dontKnowVersion, neededFiles := i.neededFiles ,
              on = c("Package")] # join -- keeping dontKnowVersion column

          otherPoss1 <- nchar(neededVersions$neededFiles) == 0 | endsWith(neededVersions$neededFiles, "NA")
          if (any(otherPoss1)) {
            dontKnowVersion <- neededVersions[otherPoss1]

            set(neededVersions, NULL, "lineNumber", seq(NROW(neededVersions)))
            nv <- data.table::copy(neededVersions)
            neededVersions[dontKnowVersion, neededFiles := i.neededFiles ,
                           on = c("Package", "versionSpec" = "Version")] # join -- keeping dontKnowVersion column
            # Checks -- if there is no versionSpec, it will be NA --> check "Version" next
            naVS <- is.na(neededVersions$versionSpec)
            if (any(naVS)) {
              neededVersions1 <- neededVersions[!naVS]
              nv1 <- nv[naVS]
              nv1[dontKnowVersion, neededFiles := i.neededFiles ,
                             on = c("Package", "Version" = "Version")] # join -- keeping dontKnowVersion column
              neededVersions <- rbindlist(list(neededVersions1, nv1))
              setorderv(neededVersions, "lineNumber")
            }

            # Checks -- if there is no versionSpec or Version, it will be "" --> check "AvailableVersion" next
            stillEmpty <- nchar(neededVersions$neededFiles) == 0
            if (any(stillEmpty & neededVersions$correctVersionAvail)) { # means version number is not precise, but CRAN fulfills the inequality
              neededVersions <- neededVersions[!stillEmpty]
              nv <- nv[stillEmpty]
              nv[dontKnowVersion, neededFiles := i.neededFiles ,
                 on = c("Package", "AvailableVersion" = "Version")] # join -- keeping dontKnowVersion column]
              neededVersions <- rbindlist(list(neededVersions, nv))
              setorderv(neededVersions, "lineNumber")
            }
            set(neededVersions, NULL, "lineNumber", NULL)
          }

        }
      }

      if (NROW(neededVersions)) {
        nfs <- neededVersions$neededFiles
        names(nfs) <- nfs
        localFilesWOExt <- gsub("\\.[[:alpha:]]+[[:alnum:]]+$", "", localFiles)
        localFilesWOExt <- gsub("\\.[[:alpha:]]+[[:alnum:]]+$", "", localFilesWOExt)

        localFileName <- lapply(nfs, function(nf)
          as.data.table(localFiles[grep(paste0("^",nf,"$|^",nf,"\\_.+$"), localFilesWOExt)]))
        nfs <- rbindlist(localFileName, idcol = "neededFiles")
        setnames(nfs, old = "V1", new = "localFileName")
        nfs[, localType := ifelse(isBinary(localFileName), "binary", "source")]
        setkeyv(nfs, c("neededFiles", "localType")) # put binary first
        nfs <- nfs[, .SD[1], by = "neededFiles"]
        neededVersions <- nfs[neededVersions, on = c("neededFiles")]#, "type")]
        if (isWindows() && interactive() && NROW(nfs[localType == "source"])) {
          srcFromCRAN <- neededVersions$installFrom == "CRAN" & neededVersions$localType == "source"
          if (NROW(neededVersions[srcFromCRAN])) {
            messageDF(neededVersions[srcFromCRAN, c("packageFullName", "Package", "localFileName")])
            message(paste0("Local *source* file(s) exist for the above package(s).\nWould you like to delete it/them ",
                           "and let Require try to find the binary on CRAN (or MRAN if older)? Y or N: "))
            out <- if (interactive())
              readline()
            else
              "Y"
            if (identical("y", tolower(out))) {
              unlink(file.path(rpackageFolder(getOption("Require.RPackageCache")),
                               neededVersions[srcFromCRAN]$localFileName))
            }
          }
        }
        # neededVersions[, localFileName := grep(neededFiles, localFiles, value = TRUE), by = "neededFiles"]
        neededVersions <- neededVersions[!is.na(localFileName), list(Package, packageFullName, localFileName)]
        if (NROW(neededVersions)) {
          pkgDT[neededVersions, `:=`(installFrom = "Local", localFileName = localFileName), on = "packageFullName"]
        }
      }
    }
  }

  pkgDT
}

#' @rdname DESCRIPTION-helpers
#' @param file A file path to a DESCRIPTION file
DESCRIPTIONFileVersionV <- function(file, purge = getOption("Require.purge", FALSE)) {
  # origLocal <- Sys.setlocale(locale = "C") # required to deal with non English characters in Author names
  # on.exit({
  #   Sys.setlocale(locale = origLocal)
  # })
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
    #if (is.null(out)) {
      if (length(f) == 1) {
        lines <- try(readLines(f))
        if (is(lines, "try-error")) {
          warning(lines)
          lines <- character()
        }

      } else {
        lines <- f
      }
      suppressWarnings(vers_line <- lines[grep("^Version: *", lines)]) # nolint
      out <- gsub("Version: ", "", vers_line)
      if (length(out) == 0) out <- NA
      if (length(f) == 1)
        assign(f, out, envir = .pkgEnv[["pkgDep"]][["DESCRIPTIONFile"]])
    #}
    out
  })
  unlist(out)
}

#' @rdname DESCRIPTION-helpers
#' @param file A file path to a DESCRIPTION file
#' @param other Any other keyword in a DESCRIPTION file that precedes a ":". The rest of the line will be
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
    suppressWarnings(vers_line <- lines[grep(paste0("^",other,": *"), lines)]) # nolint
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
#' @inheritParams pkgDep
getGitHubDESCRIPTION <- function(pkg, purge = getOption("Require.purge", FALSE)) {
  getGitHubFile(pkg, "DESCRIPTION", purge = purge)
}


getGitHubNamespace <- function(pkg, purge = getOption("Require.purge", FALSE)) {
  getGitHubFile(pkg, "NAMESPACE", purge = purge)
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

getGitHubFile <- function(pkg, filename = "DESCRIPTION",
                          purge = getOption("Require.purge", FALSE)) {
  ret <- if (length(pkg) > 0) {
    needsParse <- TRUE
    cn <- colnames(pkg)
    if (!is.null(cn))
      needsParse <- !all(c("hasSubFolder", "Repo", "Branch", "Account") %in% cn)
    if (needsParse) {
      pkg <- pkgDTtoPackageFullName(pkg)
      pkgDT <- parseGitHub(pkg)
    } else {
      pkgDT <- pkg
    }
    pkgDT[repoLocation == "GitHub",
          url := {
            if (any(hasSubFolder)) {
              Branch <- paste0(Branch, "/", GitSubFolder)
            }
            file.path("https://raw.githubusercontent.com", Account,
                      Repo, Branch, filename, fsep = "/")
          },
          by = "Package"]

    checkPath(dirname(tempfile()), create = TRUE)
    set(pkgDT, NULL, "destFile",
        file.path(tempdir(), paste0(pkgDT$Package, "_", pkgDT$Version, "_", pkgDT$filename)))
    # if (colnames(pkgDT))
    pkgDT[repoLocation == "GitHub",
          filepath := {
            # destFile <- file.path(tempdir(), paste0(Package, "_", Version, "_", filename))
            if (!all(file.exists(destFile)))
              download.file(unique(url)[1], unique(destFile)[1], overwrite = TRUE, quiet = TRUE)
            destFile
          }, by = c("Package", "Branch")]
    if (identical("DESCRIPTION", filename))
      setnames(pkgDT, old = "filepath", new = "DESCFile")
    else
      setnames(pkgDT, old = "filepath", new = filename)
    pkgDT[]
  } else {
    pkg
  }
  ret
}



updateInstalled <- function(pkgDT, installPkgNames, warn) {
  if (NROW(installPkgNames)) {
    if (missing(warn)) warn <- warnings()
    if (is(warn, "simpleWarning"))
      warn <- warn$message
    if (is(warn, "warnings")) {
      warn <- names(warn)
    }
    warnOut <- unlist(lapply(installPkgNames, function(ip) grepl(ip, warn) || grepl(ip, warn[[1]])))
    if (isTRUE(any(!warnOut) || length(warnOut) == 0 || is.na(warnOut)) && is.null(warn) ) {
      set(pkgDT, which(pkgDT$Package %in% installPkgNames), "installed", TRUE)
      # pkgDT[pkgDT$Package %in% installPkgNames, `:=`(installed = TRUE)]
    } else if (!is.null(warn)) {
      set(pkgDT, which(pkgDT$Package %in% extractPkgName(names(warn))), "installed", FALSE)
    }
  }
  pkgDT[]
}

#' @inheritParams Require
#' @rdname Require-internals
#' @importFrom utils sessionInfo
#' @export
#' @details
#' \code{doInstall} is a wrapper around \code{install.packages},
#' \code{remotes::install_github}, and \code{remotes::install_version}.
doInstalls <- function(pkgDT, install_githubArgs, install.packagesArgs,
                       install = TRUE, repos = getOption("repos"), ...) {
  if (any(!pkgDT$installed | NROW(pkgDT[correctVersion == FALSE]) > 0) &&
      (isTRUE(install) || install == "force")) {
    dots <- list(...)

    toInstall <- pkgDT[(installed == FALSE  | !correctVersion) & !(installFrom %in% c("Fail", "Duplicate"))]
    hasRequireDeps <- pkgDT[(installed == FALSE  | !correctVersion) & Package == "Require"]
    if (NROW(hasRequireDeps) && NROW(pkgDT[Package == "Require" & installed != TRUE])) {
      installRequire()
      pkgDT[Package == "Require", installed := TRUE]
    }
    if (any(!toInstall$installFrom %in% c("Fail", "Duplicate"))) {
      toInstall[, installFromFac := factor(installFrom, levels = c("Local", "CRAN", "Archive", "GitHub", "Fail", "Duplicate"))]
      setkeyv(toInstall, "installFromFac")
      # if (length(toInstall$packageFullName) > 20)
      #   message("Performing a topological sort of packages to install them in the right order; this may take some time")
      topoSorted <- pkgDepTopoSort(toInstall$packageFullName, returnFull = TRUE)
      toInstall <- toInstall[match(names(topoSorted), packageFullName)]

      toInstall <- unique(toInstall, by = c("Package"))
      pkgsCleaned <- preparePkgNameToReport(toInstall$Package, toInstall$packageFullName)

      message("Installing: ", paste(pkgsCleaned, collapse = ", "))
      toInstall[, installOrder := seq(NROW(toInstall))]
      Package <- toInstall$Package
      names(Package) <- Package
      namespacesLoaded <- unlist(lapply(Package, isNamespaceLoaded))
      if (any(namespacesLoaded) && getOption("Require.unloadNamespaces", TRUE)) {
        si <- sessionInfo()
        allLoaded <- c(names(si$otherPkgs), names(si$loadedOnly))
        topoSortedAllLoaded <- try(names(pkgDepTopoSort(allLoaded)))
        if (is(topoSortedAllLoaded, "try-error"))
          stop("The attempt to unload loaded packages failed. Please restart R and run again")
        topoSortedAllLoaded <- setdiff(topoSortedAllLoaded, c("Require", "testit", "remotes", "data.table", "glue", "rlang"))
        detached <- detachAll(topoSortedAllLoaded, doSort = FALSE)
        # detached1 <- unloadNamespaces(topoSortedAllLoaded)
        if (NROW(detached)) {
          detached <- as.data.table(detached, keep.rownames = "Package")
          pkgDT <- detached[pkgDT, on = "Package"]
        }
      }
      startTime <- Sys.time()
      out <- by(toInstall, toInstall$installOrder, installAny, pkgDT = pkgDT, dots = dots,
                numPackages = NROW(toInstall), startTime = startTime,
                install.packagesArgs = install.packagesArgs,
                install_githubArgs = install_githubArgs, repos = repos)
    }
    failedToInstall <- pkgDT$installFrom == "Fail"
    if (NROW(pkgDT[failedToInstall]) ) {
      keepCols <- c("packageFullName", "installed", "correctVersion", "AvailableVersion")
      message("The following packages could not be installed because could not find a correct version")
      messageDF(pkgDT[failedToInstall, ..keepCols])
      pkgDTFail <- pkgDT[failedToInstall]
    }
  }

  # Now that it is installed, add installed version to Version column
  pkgDT[needInstall == TRUE & installed == TRUE, Version :=
          unlist(lapply(Package, function(x) as.character(
            tryCatch(packageVersion(x), error = function(x) NA_character_))))]
  pkgDT[loadOrder > 0, loadOrder := loadOrder * as.integer(is.na(installFrom) |
                                                             !(installFrom %in% c("Fail", "Duplicate")))]

  pkgDT
}

#' @details
#' \code{doLoading} is a wrapper around \code{require}.
#'
#' @export
#' @importFrom utils capture.output
#' @rdname Require-internals
doLoading <- function(pkgDT, require = TRUE, ...) {
  packages <- pkgDT[loadOrder > 0]$Package
  packageOrder <- pkgDT[loadOrder > 0]$loadOrder
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
      pkgsWarned <- c()
      if (any(otherErrors) || any(missingDeps)) {
        if (any(otherErrors)) {
          error1 <- grepl(.libPaths()[1], outMess)
          error1Val <- gsub(paste0("^.*",.libPaths()[1], "\\/(.*)", "\\/.*$"), "\\1", outMess[error1])
          packageNames <- unique(unlist(lapply(strsplit(error1Val, "\\/"), function(x) x[[1]])))
          error2 <- grepl("no such symbol", outMess)
          if (any(error2)) {
            pkgs <- paste(packageNames, collapse = "', '")
            pkgsWarned <- c(pkgsWarned, pkgs)
            warningCantInstall(pkgs)
          }
          error3 <- grepl("is being loaded, but", outMess)
          packageNames <- gsub(paste0("^.*namespace .{1}([[:alnum:][:punct:]]+).{1} .+is being.+$"),
                               "\\1", outMess[error3])
          if (any(error3)) {
            pkgs <- paste(packageNames, collapse = "', '")
            if (length(setdiff(pkgs, pkgsWarned)) > 0)
              warningCantInstall(pkgs)
            else
              pkgsWarned <- c(pkgsWarned, pkgs)
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
        libPathsVers <- DESCRIPTIONFileVersionV(file.path(.libPaths()[1], pkg, "DESCRIPTION"),
                                                purge = TRUE)
        otherIsCorrect <- getPkgVersions(p, install = FALSE)$correctVersion
        firstPartMess <- paste0(pkg, " is already loaded from ", p$LibPath, " with version ", p$Version, ". ",
                                "The version in .libPaths() is ", libPathsVers)
        if (isTRUE(otherIsCorrect)) {
          message(firstPartMess, ". Because the newer version still accommodates the minimum version number (",
                  p$packageFullName,", updating now.")
          oo <- Require(pkg, require = FALSE, install = "force")
          outMessToRm <- grep("Loading required|Failed with error|Error in unloadNamespace", outMess)
          outMessToRm <- c(outMessToRm, max(outMessToRm) + 1) # There is non ASCII character in the message that can't be explicitly used
          outMess <- outMess[-outMessToRm]
        } else {
          warning(firstPartMess, ". The newer version fails the version number test.",
                  " Please either change the version number requested,",
                  " or prevent the newer version from loading by changing the .libPaths() prior",
                  " to any packages being loaded.")
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

    pkgDT[, loaded := (pkgDT$Package %in% names(out)[unlist(out)] & loadOrder > 0)]
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
      # if (length(package) == 1)
      # return(info)
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
  gitPkgNames <- toPkgDT(gitPkgNames)
  # if (!is.data.table(gitPkgNames)) {
  #   gitPkgNames <- data.table(Package = extractPkgName(gitPkgNames), packageFullName = c(gitPkgNames))
  # }
  if (is.null(dots$dependencies) && is.null(install_githubArgs$dependencies))
    dots$dependencies <- NA # This is NA, which under normal circumstances should be irrelevant
  #  but there are weird cases where the internals of Require don't get correct
  #  version of dependencies e.g., achubaty/amc@development says "reproducible" on CRAN
  #  which has R.oo
  #sortedTopologically <- pkgDepTopoSort(gitPkgNames$packageFullName)
  #installPkgNames <- names(sortedTopologically)
  installPkgNames <- gitPkgNames$packageFullName

  names(installPkgNames) <- gitPkgNames$Package

  ord <- match(extractPkgName(installPkgNames), gitPkgNames$Package)
  gitPkgNames <- gitPkgNames[ord]
  installPkgNames <- installPkgNames[ord]

  gitPkgs <- trimVersionNumber(gitPkgNames$packageFullName)
  names(gitPkgs) <- gitPkgNames$Package
  isTryError <- unlist(lapply(gitPkgs, is, "try-error"))
  attempts <- rep(0, length(gitPkgs))
  names(attempts) <- gitPkgs
  while (length(gitPkgs)) {
    gitPkgDeps2 <- gitPkgs[unlist(lapply(seq_along(gitPkgs), function(ind) {
      all(!extractPkgName(names(gitPkgs))[-ind] %in% extractPkgName(gitPkgs[[ind]]))
    }))]
    ipa <- modifyList2(install_githubArgs, dots)
    outRes <- lapply(gitPkgDeps2, function(p) {
      out <- tryCatch(do.call(remotes::install_github, append(list(p), ipa)),
               warning = function(w) w,
               error = function(e) e)
      if (identical(out, extractPkgName(p)))
        out <- NULL
      out
    })
    attempts[names(outRes)] <- attempts[names(outRes)] + 1
    maxAttempts <- 0

    warn <- outRes
    if (is(warn[[1]], "simpleWarning") || is(warn[[1]], "install_error")) {
      warning(warn)
    }
    # if (any(attempts >= maxAttempts)) {
    #   failedAttempts <- attempts[attempts >= maxAttempts]
    #   outRes[attempts >= maxAttempts] <- "Failed"
    #   if (any(identical("message", names(warn[[1]]) ))) {
    #     if (is.character(warn[[1]]$message)) {
    #       outRes[attempts >= maxAttempts] <- warn[[1]]$message
    #     }
    #   }
    # }
    isTryError <- unlist(lapply(outRes, is, "try-error"))
    whichDone <- !names(gitPkgs) %in% names(outRes)[!isTryError]
    gitPkgs1 <- gitPkgs[whichDone]
    outRes <- outRes[whichDone]
    if (identical(gitPkgs1, gitPkgs)) {
      # failedAttempts <- names(gitPkgs)
      gitPkgs <- character()
    }
    gitPkgs <- gitPkgs1
    outRes <- unlist(outRes)

  }
  outRes
}

getPkgDeps <- function(packages, which, purge = getOption("Require.purge", FALSE)) {
  pkgs <- trimVersionNumber(packages)
  out1 <- pkgDep(pkgs, recursive = TRUE, which = which, purge = purge)
  out1 <- unique(unname(unlist(out1)))
  out2 <- c(out1, pkgs)
  out3 <- c(out1, packages)
  dt <- data.table(github = extractPkgGitHub(out2), Package = extractPkgName(out2),
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
  ret <- dt$packageFullName
  if (!is.null(names(packages))) {
    dt[depOrOrig == "orig", Names := names(packages)[match(packageFullName, packages)]]
    dt[is.na(Names), Names := ""]
    names(ret) <- dt$Names
  }
  ret
}

installedVers <- function(pkgDT) {
  pkgDT <- toPkgDT(pkgDT)
  if (NROW(pkgDT)) {
    pkgs <- unique(pkgDT$Package)
    names(pkgs) <- pkgs
    installedPkgsCurrent <- lapply(pkgs, function(p) {
      pkgPath <- file.path(.libPaths(), p)
      out <- pkgPath[file.exists(pkgPath)][1]
      # out <- tryCatch(find.package(p, lib.loc = .libPaths()), error = function(x) NA)
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

  if (!exists("cachedAvailablePackages", envir = .pkgEnv[["pkgDep"]]) || isTRUE(purge)) {
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

    for (type in types)
      cap[[type]] <- tryCatch(available.packages(repos = repos, type = type),
                              error = function(x)
                                available.packages(ignore_repo_cache = TRUE, repos = repos, type = type))#, ignore_repo_cache = isOldMac | !isInteractive())
    cap <- do.call(rbind, cap)
    if (length(types) > 1) {
      dups <- duplicated(cap[, c("Package", "Version")])
      cap <- cap[!dups,]
    }
    cap <- as.data.table(cap)
    assign("cachedAvailablePackages", cap, envir = .pkgEnv[["pkgDep"]])
    cap
  } else {
    get("cachedAvailablePackages", envir = .pkgEnv[["pkgDep"]], inherits = FALSE)
  }
}

currentCRANPkgDates <- function(pkgs) {
  if (!exists("currentCranDates", envir = .pkgEnv[["pkgDep"]])) {
    message("Getting dates of current CRAN packages")
    tf <- tempfile();
    out <- try(download.file(file.path(getOption("repos")["CRAN"], "src/contrib/"), tf, quiet = TRUE))
    if (is(out, "try-error")) stop(out, "Download from CRAN failed momentarily. Please try again shortly")
    currentCranDates <- readLines(tf)
    assign("currentCranDates", currentCranDates, envir = .pkgEnv[["pkgDep"]])
  } else {
    currentCranDates <- get("currentCranDates", envir = .pkgEnv[["pkgDep"]])
  }
  if (is.null(names(pkgs))) names(pkgs) <- pkgs

  aa <- substring(currentCranDates, nchar("      <a href=\"")+1, 200)
  bb <- unlist(lapply(paste0(pkgs, "_"), function(p) which(startsWith(aa, p))))
  currentCranDates2 <- aa[bb]
  dd <- gsub(paste0(".*(20[0-2][0-9]-[0-1][0-9]-[0-3][0-9]).*"), "\\1", currentCranDates2)
  ee <- gsub(paste0("^.+>[[:alnum:]\\.]+\\_(.*)\\.tar\\.gz<.*"), "\\1", currentCranDates2)
  ff <- gsub(paste0("^.+>([[:alnum:]\\.]+)\\_.*\\.tar\\.gz<.*"), "\\1", currentCranDates2)
  pkgsDateAvail <- data.table(Package = ff, date = dd, CRANVersion = ee)

  # currentCranDates1 <- sapply(pkgs, function(pkg)
  #   grep(paste0("\"", pkg, "\\_.*\\.tar\\.gz"), currentCranDates, value = TRUE))
  #
  #
  # pkgsDateAvail <- lapply(currentCranDates1, function(ava)
  #   data.table(date = unique(gsub(paste0(".*(20[0-2][0-9]-[0-1][0-9]-[0-3][0-9]).*"), "\\1", ava)),
  #              CRANVersion = unique(gsub(paste0("^.+>[[:alnum:]\\.]+\\_(.*)\\.tar\\.gz<.*"), "\\1", ava))))
  # pkgsDateAvail <- rbindlist(pkgsDateAvail[sapply(pkgsDateAvail, function(x) length(x) > 0)], idcol = "Package")
  currentCranDates <- pkgsDateAvail[, mtime := as.POSIXct(date)]
  set(currentCranDates, NULL, "date", NULL)

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
  installPkgNames <- normPath(file.path(rpackageFolder(getOption("Require.RPackageCache")), toIn$localFileName))
  names(installPkgNames) <- installPkgNames

  installPkgNamesBoth <- split(installPkgNames, endsWith(installPkgNames, "zip"))

  warn <- lapply(installPkgNamesBoth, function(installPkgNames) {
    # Deal with "binary" mumbo jumbo
    type <- c("source", "binary")[endsWith(installPkgNames, "zip") + 1]
    isBin <- isBinary(installPkgNames)
    buildBinDots <- grepl("--build", dots)
    buildBinIPA <- grepl("--build", install.packagesArgs)
    buildBin <- any(buildBinDots, buildBinIPA)
    if (buildBin && isBin) {
      if (any(buildBinDots)) dots[buildBinDots] <- setdiff(dots[buildBinIPA][[1]], "--build")
      if (any(buildBinIPA)) install.packagesArgs[buildBinIPA] <-
          list(setdiff(install.packagesArgs[buildBinIPA][[1]], "--build"))
    }
    ipa <- modifyList2(list(type = type), install.packagesArgs, dots, list(repos = NULL))
    warns <- lapply(installPkgNames, function(installPkgName) { # use lapply so any one package fail won't stop whole thing
      warn <- suppressMessages(tryCatch({
        do.call(install.packages,
                # using ap meant that it was messing up the src vs bin paths
                append(list(installPkgName), ipa))
      }, warning = function(condition) condition)
      )
      if (!isBin && buildBin) copyTarball(basename(installPkgName), TRUE)
      warn
    })
  })

  warn <- unlist(warn)
  if (!is.null(warn)) {
    warning(warn)
    warn <- warn[grep("message", names(warn))]
    pkgDT[Package == toInstall$Package, installResult := unlist(lapply(warn, function(x) x))]
  }
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

installCRAN <- function(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs,
                        repos = getOption("repos")) {
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

  ipa <- modifyList2(install.packagesArgs, dots)

  # manually override "type = 'both'" because it gets it wrong some of the time
  ap <- as.data.table(.pkgEnv[["pkgDep"]]$cachedAvailablePackages)
  if (NROW(ap) > 1 && isWindows()) {
    ap <- ap[Package == installPkgNames]
    if (NROW(ap)) {
      onVec <- c("Package")
      if (!is.null(toInstall$versionSpec))
        if (!is.na(toInstall$versionSpec) && !isTRUE(toInstall$correctVersionAvail))
          onVec <- c("Package", "Version" = "versionSpec")

      type <- c("source", "binary")[any(grepl("bin", ap[toInstall, on = onVec]$Repository)) + 1]
      install.packagesArgs["type"] <- type
      ipa <- modifyList2(list(type = type), ipa)
    }
  }

  warn <- tryCatch({
    out <- do.call(install.packages,
                   # using ap meant that it was messing up the src vs bin paths
                   append(list(installPkgNames), ipa))
  }, warning = function(condition) condition,
  error = function(e) {
    if (grepl('argument \\"av2\\" is missing', e))
      tryCatch(warning(paste0("package '" ,installPkgNames,"' is not available (for ",R.version.string,")")),
               warning = function(w) w)
    else
      stop(e)
  })

  if (any(grepl("--build", c(dots, install.packagesArgs))))
    copyTarball(installPkgNames, TRUE)

  if (!is.null(warn)) {
    warning(warn)
    pkgDT[Package == installPkgNames, installResult := warn$message]
  }
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

installArchive <- function(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs, repos = getOption("repos")) {
  Archive <- "Archive"
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
  onMRAN <- dateFromMRAN > "2015-06-06" && isWindows()
  if (any(onMRAN)) {
    origIgnoreRepoCache <- install.packagesArgs[["ignore_repo_cache"]]
    install.packagesArgs["ignore_repo_cache"] <- TRUE
    installedPkgs <- file.path(.libPaths()[1], unname(installPkgNames)[onMRAN])
    if (dir.exists(installedPkgs)) {
      try(unlink(installedPkgs, recursive = TRUE))
    }
    out <- Map(p = unname(installPkgNames)[onMRAN], date = dateFromMRAN[onMRAN], v = installVersions[onMRAN], function(p, date, v, ...) {
      warn <- list()
      ipa <- modifyList2(install.packagesArgs, dots,
                         list(repos = file.path("https://MRAN.revolutionanalytics.com/snapshot", date)))

      tryCatch(
        do.call(install.packages, append(list(p), ipa)),
        error = function(x) {
          x$message
        },
        warning = function(w) {
          w
        })
    })
    installedVers <- try(DESCRIPTIONFileVersionV(
      file.path(.libPaths()[1], toInstall$Package, "DESCRIPTION"), purge = TRUE))
    if (!identical(installedVers, installVersions[onMRAN])) {
      message("-- incorrect version installed from MRAN; trying CRAN Archive")
      onMRAN <- FALSE
    }

    install.packagesArgs["ignore_repo_cache"] <- origIgnoreRepoCache

    out <- unlist(out)
    if (length(out)) {
      lapply(out, warning)
      pkgDT[Package == toInstall$Package, installResult := unlist(out)[[1]]]
      onMRAN <- FALSE
    }
  }
  if (any(!onMRAN)) {
    install.packagesArgs <- modifyList2(install.packagesArgs, list(type = "source"))
    cranArchivePath <- file.path(getOption("repos"), "src/contrib/Archive/")
    out <- Map(p = toIn$PackageUrl[!onMRAN], v = installVersions[!onMRAN], function(p, v, ...) {
      warn <- list()
      p <- file.path(cranArchivePath, p)
      ipa <- modifyList2(install.packagesArgs, dots, list(repos = NULL))
      warn <- tryCatch(
        out <- do.call(install.packages,
                       # using ap meant that it was messing up the src vs bin paths
                       append(list(unname(p)), ipa)),
        # ret <- do.call(remotes::install_version, append(list(package = unname(p), version = v, ...), install_githubArgs)),
        error = function(x) {
          x$message
        },
        warning = function(condition) condition)
      warn
    })
    out <- unlist(out)
    if (length(out)) {
      warning(out[[1]])
      pkgDT[Package == toInstall$Package, installResult := unlist(out)[[1]]]
    }
  }
  if (any(grepl("--build", c(dots, install.packagesArgs))))
    copyTarball(installPkgNames, TRUE)

  updateInstalled(pkgDT, installPkgNames, warnings())
}

installGitHub <- function(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs) {
  gitPkgNames <- toInstall[installFrom == "GitHub"]
  out5 <- install_githubV(gitPkgNames, install_githubArgs = install_githubArgs, dots = dots)
  updateInstalled(pkgDT, gitPkgNames$Package, out5)
}

installAny <- function(pkgDT, toInstall, dots, numPackages, startTime, install.packagesArgs, install_githubArgs,
                       repos = getOption("repos")) {
  currentTime <- Sys.time()
  dft <- difftime(currentTime, startTime, units = "secs")
  timeLeft <- dft/toInstall$installOrder * (numPackages - toInstall$installOrder + 1)

  lotsOfTimeLeft <- dft > 10
  timeLeftAlt <- if (lotsOfTimeLeft) format(timeLeft, units = "auto", digits = 1) else "..."
  estTimeFinish <- if (lotsOfTimeLeft) Sys.time() + timeLeft else "...calculating"
  pkgToReport <- preparePkgNameToReport(toInstall$Package, toInstall$packageFullName)
  message(" -- Installing ", pkgToReport, " -- (", toInstall$installOrder, " of ", numPackages, ". Estimated time left: ",
          timeLeftAlt, "; est. finish: ", estTimeFinish, ")")

  if (any("Local" %in% toInstall$installFrom)) {
    pkgDT <- installLocal(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs)
    anyFaultyBinaries <- grepl("error 1 in extracting from zip file", pkgDT$installResult)
    if (isTRUE(anyFaultyBinaries)) {
      message("Local cache of ", paste(pkgDT[anyFaultyBinaries]$localFileName, collapse = ", "), " faulty; deleting")
      unlink(file.path(rpackageFolder(getOption("Require.RPackageCache")), pkgDT[anyFaultyBinaries]$localFileName))
    }
  }
  if (any("CRAN" %in% toInstall$installFrom))
    pkgDT <- installCRAN(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs,
                         repos = repos)
  if (any("Archive" %in% toInstall$installFrom))
    pkgDT <- installArchive(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs,
                            repos = repos)
  if (any("GitHub" %in% toInstall$installFrom)) {
    pkgDT <- installGitHub(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs)
  }
  pkgDT
}

isBinary <- function(fn) {
  endsWith(fn, "zip") | grepl("R_x86", fn)
}

copyTarball <- function(pkg, builtBinary) {
  if (builtBinary) {
    newFiles <- dir(pattern = gsub("\\_.*", "", pkg), full.names = TRUE)
    if (length(newFiles)) {
      newNames <- file.path(rpackageFolder(getOption("Require.RPackageCache")), unique(basename(newFiles)))
      if (all(!file.exists(newNames)))
        try(file.link(newFiles, newNames))
      unlink(newFiles)
    }
  }

}

installRequire <- function(requireHome = getOption("Require.Home")) {
  dFileAtInstalledRequire <- file.path(.libPaths()[1], "Require", "DESCRIPTION") # system.file("DESCRIPTION", package = "Require")
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
            message("Installing Require ver: ", pkgVersionAtRequireHome," from source at ", requireHome)
            system(paste0("R CMD INSTALL --no-multiarch --library=", .libPaths()[1], " Require"), wait = TRUE)
          }
        }
        done <- TRUE

      } else {
        if (!is.null(requireHome))
          message(pkgNameAtRequireHome, " did not contain Require source code")
      }
    }

    if (isFALSE(done)) {
      system(paste0("Rscript -e \"install.packages(c('Require'), lib ='",.libPaths()[1],"', repos = '",getOption('repos')[["CRAN"]],"')\""), wait = TRUE)
      done <- TRUE
      # system(paste0("Rscript -e \"install.packages(c('data.table', 'remotes'), lib ='",.libPaths()[1],"', repos = '",getOption('repos')[["CRAN"]],"')\""), wait = TRUE)
    }
  } else {
    stop("Require will need to be installed manually in", .libPaths()[1])
    system(paste0("Rscript -e \"install.packages(c('Require'), lib ='",.libPaths()[1],"', repos = '",getOption('repos')[["CRAN"]],"')\""), wait = TRUE)
  }
}

toPkgDT <- function(pkgDT, deepCopy = FALSE) {
  if (!is.data.table(pkgDT)) {
    pkgDT <- if (deepCopy)
      data.table(Package = extractPkgName(pkgDT), packageFullName = c(pkgDT))
    else
      toDT(Package = extractPkgName(pkgDT), packageFullName = pkgDT)
    # pkgDT2 <- setDT(list(Package = extractPkgName(pkgDT)))
    # set(pkgDT2, NULL, "packageFullName", pkgDT)
    # pkgDT <- pkgDT2
    # pkgDT <- data.table(Package = extractPkgName(pkgDT), packageFullName = c(pkgDT))
  }

  pkgDT
}

toDT <- function(...) {
  setDT(list(...))
}

rmDuplicatePkgs <- function(pkgDT) {
  dups <- pkgDT[installed == FALSE, .N, by = "Package"][N > 1]
  if (NROW(dups)) {
    message("Duplicate packages are Required; discarding older, or unavailable")
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
    pkgDT[installed == FALSE & keep == TRUE & seq(NROW(pkgDT)) != keep2, keep := NA]
    set(pkgDT, NULL, "duplicate", FALSE)
    pkgDT[is.na(keep), `:=`(keep = FALSE, installFrom = "Duplicate", duplicate = TRUE)] # Was "Fail" ...
    if (!all(pkgDT$keep)) {
      summaryOfDups <- pkgDT[dup == TRUE, list(Package, packageFullName, keep, installResult)]
      # summaryOfDups[is.na(keep), keep := FALSE]
      setorderv(summaryOfDups, c("Package", "keep"), order = c(1,-1))
      messageDF(summaryOfDups)
    }
    pkgDT[, `:=`(keep2 = NULL, keep = NULL, dup = NULL)]
  }
  pkgDT
}

#' Detach and unload all packages
#'
#' This uses \code{pkgDepTopoSort} internally so that the package
#' dependency tree is determined, and then packages are unloaded
#' in the reverse order. Some packages don't unload successfully for
#' a variety of reasons. Several known packages that have this problem
#' are identified internally and *not* unloaded. Currently, these are
#' \code{glue}, \code{rlang}, \code{ps}, \code{ellipsis}, and, \code{processx}.
#'
#' @return
#' A numeric named vector, with names of the packages that were attempted.
#' \code{2} means the package was successfully unloaded, \code{1} it was
#' tried, but failed, \code{3} it was in the search path and was detached
#' and unloaded.
#' @export
#' @param pkgs A character vector of packages to detach. Will be topologically sorted
#'   unless \code{doSort} is \code{FALSE}.
#' @param dontTry A character vector of packages to not try. This can be used
#'   by a user if they find a package fails in attempts to unload it, e.g., "ps"
#' @param doSort If \code{TRUE} (the default), then the \code{pkgs} will be
#'   topologically sorted. If \code{FALSE}, then it won't. Useful if the
#'   \code{pkgs} are already sorted.
#'
#'
detachAll <- function(pkgs, dontTry = NULL, doSort = TRUE) {
  message("Detaching is fraught with many potential problems; you may have to restart your session if things aren't working")
  srch <- search()
  pkgsOrig <- pkgs
  origDeps <- pkgDep(pkgs, recursive = TRUE)
  depsToUnload <- c(pkgs, unname(unlist(origDeps)))
  si <- sessionInfo()
  allLoaded <- c(names(si$otherPkgs), names(si$loadedOnly))
  others <- pkgDepTopoSort(pkgs, deps = allLoaded, reverse = TRUE)
  names(others) <- others
  depsToUnload <- c(others, depsToUnload)
  depsToUnload <- depsToUnload[!duplicated(depsToUnload)]

  if (length(depsToUnload) > 0) {
    out <- if (isTRUE(doSort)) pkgDepTopoSort(depsToUnload) else NULL
    pkgs <- rev(c(names(out), pkgs))
  }
  pkgs <- extractPkgName(pkgs)
  pkgs <- unique(pkgs)
  names(pkgs) <- pkgs

  dontTryExtra <- intersect(c("glue", "rlang", "ps", "ellipsis", "processx", "vctrs", "RCurl", "bitops"),
                            pkgs)

  if (length(dontTryExtra)) {
    message("some packages don't seem to unload their dlls correctly. ",
            "These will not be unloaded: ", paste(dontTryExtra, collapse = ", "))
    dontTry <- c(dontTry, dontTryExtra)
  }

  dontTry <- unique(c(c("Require", "remotes", "data.table"), dontTry))
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
    #if (length(pkgs)) {
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
  # detached[inSearchPath] <- NA
  detached[detached] <- 2
  detached[!detached] <- 1
  detached[is.na(detached)] <- 3
  detached
}

isWindows <- function() {
  tolower(Sys.info()["sysname"]) == "windows"
}

warningCantInstall <- function(pkgs) {
  warning("Can't install ", pkgs, "; you will likely need to restart R and run:\n",
          "-----\n",
          "install.packages(c('",paste(pkgs, collapse = ", "),"'), lib = '",.libPaths()[1],"')",
          "\n-----\n...before any other packages get loaded")

}

rpackageFolder <- function(path = getOption("Require.RPackageCache"), exact = FALSE)  {
  if (!is.null(path)) {
    if (isTRUE(exact))
      return(path)
    path <- path[1]
    rversion <- paste0(R.version$major, ".", strsplit(R.version$minor, split = "\\.")[[1]][1])
    if (normPath(path) %in% normPath(strsplit(Sys.getenv("R_LIBS_SITE"), split = ":")[[1]])) {
      path
    } else {
      if (!endsWith(path, rversion))
        file.path(path, rversion)
      else
        path
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
  unlist(lapply(pathsToCheck, function(lp)
    checkPath(rpackageFolder(lp, exact = exact), create = TRUE)))
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
