utils::globalVariables(c(
  c("..colsKeep", "..colsToNAfill", ".I", ".N", "Archs", "AvailableVersion",
    "correctVersion", "dayAfterPutOnCRAN", "DepVersion", "destFile", "dup",
    "filepath", "github", "groupCRANtogether", "groupCRANtogetherChange",
    "groupCRANtogetherDif", "hasVersionSpec", "i.neededFiles", "inequality",
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
    messageGithubPAT(ghp, verbose = verbose, verboseLevel = 0)

    isGH <- pkgDT$repoLocation == "GitHub"
    isGitHub <- which(isGH)
    set(pkgDT, isGitHub, "fullGit", trimVersionNumber(pkgDT$packageFullName[isGitHub]))
    # pkgDT[isGitHub, fullGit := trimVersionNumber(packageFullName)]
    set(pkgDT, isGitHub, "fullGit", masterMainToHead(pkgDT$fullGit[isGitHub]))
    # pkgDT[isGitHub, fullGit := masterMainToHead(fullGit)]
    set(pkgDT, isGitHub, "Account", gsub("^(.*)/.*$", "\\1", pkgDT$fullGit[isGitHub]))
    # pkgDT[isGitHub, Account := gsub("^(.*)/.*$", "\\1", fullGit)]
    set(pkgDT, isGitHub, "RepoWBranch", gsub("^(.*)/(.*)@*.*$", "\\2", pkgDT$fullGit[isGitHub]))
    # pkgDT[isGitHub, RepoWBranch := gsub("^(.*)/(.*)@*.*$", "\\2", fullGit)]
    set(pkgDT, isGitHub, "hasSubFolder", grepl("/", pkgDT[isGitHub]$Account[isGitHub]))
    # pkgDT[isGitHub, hasSubFolder := grepl("/", pkgDT[isGitHub]$Account)]
    if (any(pkgDT$hasSubFolder, na.rm = TRUE)) { # fix both Account and RepoWBranch
      hasSubFold <- which(pkgDT$hasSubFolder)
      subFoldIndices <- seq_len(NROW(pkgDT[hasSubFold]))
      #pkgDT[hasSubFold, Account := gsub("^(.*)/(.*)$", "\\1", Account)]


      # set(pkgDT, hasSubFold, "subFoldIndices", seq_len(NROW(pkgDT[hasSubFold])))
      set(pkgDT, hasSubFold, "Account", gsub("^(.*)/(.*)$", "\\1", pkgDT$Account[hasSubFold]))
      # pkgDT[hasSubFold, Account := gsub("^(.*)/(.*)$", "\\1", Account)]
      # set(pkgDT, hasSubFold, "RepoWBranch", gsub("^(.*)/(.*)$", "\\1", pkgDT$Account))
      set(pkgDT, hasSubFold, "RepoWBranch",
          gsub(paste0("^",pkgDT$Account[hasSubFold],"/"), "", pkgDT$fullGit[hasSubFold]))
      #pp <- data.table::copy(pkgDT)
      #lala <- try(pkgDT[hasSubFold, RepoWBranch := gsub(paste0("^",Account,"/"), "", fullGit), by = subFoldIndices])
      #if (is(lala, "try-error")) browser()
      set(pkgDT, hasSubFold, "GitSubFolder",
          strsplit(pkgDT$RepoWBranch[hasSubFold], split = "/|@")[[1]][2])
      #pkgDT[hasSubFold, GitSubFolder := strsplit(pkgDT[hasSubFold]$RepoWBranch, split = "/|@")[[1]][2],
      #      by = subFoldIndices]
      # pkgDT[hasSubFold, RepoWBranch := gsub(paste0("/",GitSubFolder), "", RepoWBranch), by = subFoldIndices]
      set(pkgDT, hasSubFold, "RepoWBranch",
          gsub(paste0("/",pkgDT$GitSubFolder[hasSubFold]), "", pkgDT$RepoWBranch[hasSubFold]))

    }
    set(pkgDT, isGitHub, "Repo", gsub("^(.*)@(.*)$", "\\1", pkgDT$RepoWBranch[isGitHub]))
    # pkgDT[isGitHub, Repo := gsub("^(.*)@(.*)$", "\\1", RepoWBranch)]
    set(pkgDT, isGitHub, "Branch", "HEAD")
    # pkgDT[isGitHub, Branch := "HEAD"]
    wh1 <- which(isGH & grepl("@", pkgDT$RepoWBranch))
    set(pkgDT, wh1, "Branch", gsub("^.*@(.*)$", "\\1", pkgDT$RepoWBranch[wh1]))
    # pkgDT[isGitHub & grepl("@", RepoWBranch), Branch := gsub("^.*@(.*)$", "\\1", RepoWBranch)]
    set(pkgDT, NULL, c("RepoWBranch", "fullGit"), NULL)
  }
  pkgDT[]
}


#' Internals used by `Require`
#'
#' While these are not intended to be called manually by users, they may be
#' of some use for advanced users.
#'
#' @return
#' In general, these functions return a `data.table` with various package
#' information, installation status, version, available version etc.
#'
#' @importFrom data.table setorderv
#' @inheritParams Require
#' @inheritParams parseGitHub
#' @rdname Require-internals
#' @export
getPkgVersions <- function(pkgDT, install = TRUE, verbose = getOption("Require.verbose")) {
  pkgDT <- toPkgDT(pkgDT)
  pkgDT[, hasVersionSpec := grepl(.grepVersionNumber, packageFullName)]

  if (any(pkgDT$hasVersionSpec)) {
    pkgDT <- pkgDT[hasVersionSpec == TRUE, versionSpec := extractVersionNumber(packageFullName)]
    pkgDT[hasVersionSpec == TRUE, inequality := extractInequality(packageFullName)]


    pkgDT[hasVersionSpec == TRUE & grepl("<", inequality), versionSpec := as.character(min(package_version(versionSpec))),
          by = "Package"]

    setorderv(pkgDT, c("Package", "versionSpec"), order = -1L)

    # any duplicates with different minimum version number to be dealt with here --> only those with > in their inequality
    setorderv(pkgDT, c("Package", "hasVersionSpec"), order = -1L)

    if ("Version" %in% colnames(pkgDT)) {
      theNAVersions <- is.na(pkgDT$Version)
      whNOTNAVersions <- which(!theNAVersions)
      set(pkgDT, NULL, "correctVersion", NA)
      if (all(theNAVersions)) {
        set(pkgDT, NULL, "compareVersion", NA)
      } else {
        set(pkgDT, whNOTNAVersions, "compareVersion",
            .compareVersionV(pkgDT$Version[whNOTNAVersions], pkgDT$versionSpec[whNOTNAVersions]))
        # pkgDT[!is.na(Version), compareVersion := .compareVersionV(Version, versionSpec)]
        wh <- which(!theNAVersions & pkgDT$hasVersionSpec == TRUE)
        set(pkgDT, wh, "correctVersion",
            .evalV(.parseV(text = paste(pkgDT$compareVersion[wh], pkgDT$inequality[wh], "0"))))
        # pkgDT[whNOTNAVersions & hasVersionSpec == TRUE, correctVersion := .evalV(.parseV(text = paste(compareVersion, inequality, "0")))]
        if (any(pkgDT$installed %in% TRUE & pkgDT$correctVersion %in% FALSE))
          pkgDT[installed %in% TRUE & correctVersion %in% FALSE, installed := FALSE]
      }
      # set(pkgDT, which(pkgDT$hasVersionSpec %in% FALSE), "correctVersion", NA)
      # pkgDT[hasVersionSpec == FALSE, correctVersion := NA]
      # put FALSE at top of each package -- then take the first one, so we will know if all inequalities are satisfied
      setorderv(pkgDT, c("Package", "correctVersion"), order = 1L, na.last = TRUE)
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
#' @importFrom stats na.omit
#' @importFrom data.table setkeyv
#' @rdname Require-internals
#' @inheritParams Require
getAvailable <- function(pkgDT, purge = FALSE, repos = getOption("repos"),
                         verbose = getOption("Require.verbose")) {
  if (NROW(pkgDT[correctVersion == FALSE | is.na(correctVersion)])) {
    whNotCorrect <- pkgDT[, .I[hasVersionSpec == TRUE & (correctVersion == FALSE | is.na(correctVersion))]]
    pDT <- pkgDT#[whNotCorrect]
    # takenOffCran <- FALSE # This is an object that will be modified only if in the CRAN section
    if (is.null(pDT$versionSpec)) {
      pDT[, `:=`(compareVersionAvail = NA, correctVersionAvail = NA, versionSpec = NA,
                                inequality = NA)]
    }

    if (internetExists(paste0("cannot check for available packages", verbose = verbose))) {
      # do CRAN first
      if (any(pDT$repoLocation == "CRAN")) {
        cachedAvailablePackages <- available.packagesCached(repos = repos, purge = purge, verbose = verbose)
        cachedAvailablePackages <- cachedAvailablePackages[, c("Package", "Version", "Archs")]
        setnames(cachedAvailablePackages, "Version", "AvailableVersion")
        pDT <- cachedAvailablePackages[pDT, on = "Package"]
        set(pDT, NULL, c("compareVersionAvail", "correctVersionAvail"), NA_integer_)
        if (!all(is.na(pDT$inequality))) {
          whChange <- which(!is.na(pDT$inequality))
          whCheckVersion <- which((pDT$repoLocation != "GitHub") & !is.na(pDT$inequality))
          set(pDT, whCheckVersion, "compareVersionAvail",
              .compareVersionV(pDT$AvailableVersion[whCheckVersion], pDT$versionSpec[whCheckVersion]))
          # pDT[!is.na(inequality) ,
          #                    compareVersionAvail := !is.na(AvailableVersion)]
          pDT[whCheckVersion, correctVersionAvail :=
                               # eval(parse(text = paste0("'", AvailableVersion,"'",
                               #                          inequality,
                               #                          "'", versionSpec, "'"))),
                .evalV(.parseV(text = paste(compareVersionAvail, inequality, "0")))]
                #             by = seq(sum(whCheckVersion))]
        }

        # pDT[repoLocation != "GitHub",
        #                    compareVersionAvail := {
        #                      v <- NA_integer_
        #                      v1 <- !is.na(AvailableVersion)
        #                      v[v1] <- .compareVersionV(AvailableVersion[v], versionSpec[v])
        #                      v
        #                    }]
        # pDT[
        #   repoLocation != "GitHub" ,
        #   correctVersionAvail := {
        #     v0 <- NA_integer
        #     needsAVersion <- hasVersionSpec %in% TRUE
        #     v0[
        #     v <- !is.na(AvailableVersion)
        #     v1 <- !is.na(inequality)
        #     v[v1] <- .evalV(
        #       .parseV(text = paste(compareVersionAvail[v1], inequality[v1], "0")))
        #     v
        #   }]

        pDT[Package %in% .basePkgs, correctVersionAvail := TRUE]

        # pDT$correctVersionAvail %in% FALSE &
        #   pDT$hasVersionSpec %in% TRUE &
        #   pDT$repoLocation %in% "CRAN"
        # takenOffCran <- is.na(pDT$inequality) &
        #   (!(pDT$correctVersionAvail | is.na(pDT$correctVersionAvail)) |
        #       is.na(pDT$AvailableVersion)) &
        #   !pDT$Package %in% .basePkgs & pDT$repoLocation == "CRAN"
        # possiblyArchivedPkg <- pDT$Package[takenOffCran]

        # If package has both a binary and source available on CRAN, there will be 2 entries
        pDT[correctVersionAvail == TRUE, N := .N, by = "packageFullName"]
        setorderv(pDT, c("correctVersionAvail"), order = -1, na.last = TRUE) # put TRUE first
        pDT[, keep := min(.I), by = "Package"]
        pDT <- pDT[pDT$keep,]

        # pDT[, keep :=
        #                      if (isTRUE(any(correctVersionAvail, na.rm = TRUE))) {
        #                        min(.I)
        #                      } else if (all(is.na(correctVersionAvail))) {
        #                        min(.I)
        #                      } else {
        #                        .I#"c"#NA_integer_ # The FALSE -- i.e need older packages
        #                      }
        #                    , by = "Package"]
        # pDT <- pDT[unique(pDT$keep), ]
        # pDT <- pDT[, .SD[1], by = c("Package")] # Take first of multiples; but should do nothing
        # pDT[, N := .N, by = "Package"]
        #if (any(pDT[correctVersionAvail == TRUE]$N > 1)) {
        #  pDT <- pDT[correctVersionAvail == TRUE, .SD[1], by = "packageFullName"] # take smaller one, as it will be binary
        #  pDT[N > 1, type := ifelse(is.na(Archs), "source", "binary")]
        #}
        set(pDT, NULL, "keep", NULL)
      }

      # do Older Versions
      needOlder <- pDT$correctVersionAvail %in% FALSE
      needOlderNotGH <- needOlder & pDT$repoLocation != "GitHub"
      if (is.na(any(needOlderNotGH))) stop("Error 642; please contact developer")
      if (any(needOlderNotGH)) {

        pkg <- pDT[needOlderNotGH]$Package #repoLocation != "GitHub" & needOlder]$Package
        oldAvailableVersions <- if (!is.null(.pkgEnv[["pkgDep"]][["oldAvailableVersions"]])) {
          .pkgEnv[["pkgDep"]][["oldAvailableVersions"]]
        } else {
          list()
        }
        pkgsInOAV <- pkg %in% names(oldAvailableVersions)
        if (!all(pkgsInOAV)) {
          pkgs <- pkg[!pkgsInOAV]
          names(pkgs) <- pkgs
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
        colsToDelete <- c("size", "isdir", "mode",
                          "ctime", "atime", "uid", "gid", "uname", "grname")
        colsToDelete <- intersect(colnames(oldAvailableVersions), colsToDelete)
        set(oldAvailableVersions, NULL, colsToDelete, NULL)
        setDT(oldAvailableVersions)
        if (NROW(oldAvailableVersions) && "PackageUrl" %in% colnames(oldAvailableVersions)) {
          oldAvailableVersions[, OlderVersionsAvailable := gsub(".*_(.*)\\.tar\\.gz", "\\1", PackageUrl)]
          needOlderDT <- pDT[needOlder & repoLocation != "GitHub"]

          # packages installed locally via devtools::install will have no known source -- will be NA
          oldAvailableVersions[!is.na(OlderVersionsAvailable), OlderVersionsAvailableCh := as.character(package_version(OlderVersionsAvailable))]

          oldAvailableVersions <- needOlderDT[oldAvailableVersions, on = c("Package"), roll = TRUE, allow.cartesian = TRUE]
          oldAvailableVersions[, compareVersionAvail := .compareVersionV(OlderVersionsAvailableCh, versionSpec)]
          oldAvailableVersions[, correctVersionAvail := {
            v <- rep(TRUE, .N)
            v1 <- is.na(inequality)
            v1[!v1] <- .evalV(.parseV(text = paste(compareVersionAvail[!v1], inequality[!v1], "0")))
            v1 > 0
          }]

          if (any(oldAvailableVersions$correctVersionAvail)) {
            oldAvailableVersions[correctVersionAvail == TRUE, archiveSource := "Archive"]
            currDates <- currentCRANPkgDates(unique(oldAvailableVersions$Package))
            oldAvailableVersions <- rbindlist(list(oldAvailableVersions, currDates), use.names = TRUE, fill = TRUE)
            data.table::setkeyv(oldAvailableVersions, c("Package", "mtime", "CRANVersion"))
            colsToNAfill <- setdiff(colnames(oldAvailableVersions),
                                    c("compareVersionAvail", "correctVersionAvail",
                                      "archiveSource", "PackageUrl", "OlderVersionsAvailable" ,
                                      "OlderVersionsAvailableCh", "mtime", "CRANVersion"))
            wh <- which(is.na(oldAvailableVersions$packageFullName))
            set(oldAvailableVersions, wh, colsToNAfill, oldAvailableVersions[wh - 1, ..colsToNAfill])

            bb <- oldAvailableVersions[correctVersionAvail == TRUE & archiveSource == "Archive"]

            suppressWarnings(aa <- oldAvailableVersions[
              ,{
                list(nextRow = as.integer(min(na.rm = TRUE,
                                   max(.I, na.rm = TRUE),
                                   max(.I[correctVersionAvail == TRUE & archiveSource == "Archive"
                                       ]
                                       , na.rm = TRUE))),
                     nextVersionRow = as.integer(max(.I[correctVersionAvail == TRUE & archiveSource == "Archive"
                     ], na.rm = TRUE) + 1),
                     lastRow = max(.I, na.rm = TRUE)
                )
              }, by = Package])

            aa[nextRow == lastRow, needLaterDate := TRUE]
            desiredDates <- oldAvailableVersions[aa$nextRow,
                                                 list(Package,
                                                      dayAfterPutOnCRAN = mtime + 60*60*24)]
            desiredDateEnd <- oldAvailableVersions[aa$nextVersionRow]$mtime - 60*60*24
            desiredDates <- data.table(desiredDates, desiredDateEnd)
            set(aa, NULL, "lastRow", NULL)

            oldAvailableVersions <- desiredDates[bb, on = "Package"]
            oldAvailableVersions[, mtime := dayAfterPutOnCRAN]

            oldAvailableVersions <- oldAvailableVersions[!is.na(archiveSource)]
            oldAvailableVersions[, repoLocation := archiveSource]
            # Order based on package_version object, not character string
            packVersOrd <- order(package_version(oldAvailableVersions$OlderVersionsAvailable),
                                 decreasing = TRUE)
            oldAvailableVersions <- oldAvailableVersions[packVersOrd, ]
          }

          oldAvailableVersions <- oldAvailableVersions[, if (NROW(.SD) == 0) .SD else .SD[1], by = "Package"]
          set(oldAvailableVersions, NULL, c("OlderVersionsAvailableCh"), NULL)

          notCorrectVersions1 <- rbindlist(
            list(pDT[!packageFullName %in% oldAvailableVersions$packageFullName],
                 oldAvailableVersions), fill = TRUE, use.names = TRUE)
          if (!identical(NROW(notCorrectVersions1), NROW(pDT))) {
            stillDontHave <- pDT[!notCorrectVersions1, on = "packageFullName"]
            if (NROW(stillDontHave)) {
              stillDontHave[, repoLocation := "Unknown"]
              notCorrectVersions1 <- rbindlist(list(notCorrectVersions1, stillDontHave), fill = TRUE, use.names = TRUE)
            }
          }

          pDT <- notCorrectVersions1
        }
      }

      possiblyArchived <- pDT$repoLocation != "GitHub" & is.na(pDT$AvailableVersion) &
        pDT$hasVersionSpec %in% FALSE &
        pDT$installed %in% FALSE

      possiblyArchivedPkg <- pDT$Package[possiblyArchived]
      if (length(possiblyArchivedPkg)) {
        messageVerbose(paste(possiblyArchivedPkg, collapse = ", "),
                       " not on the CRAN repo supplied. Checking Archives.",
                       verbose = verbose, verboseLevel = 2)
        if (length(possiblyArchivedPkg) > 10)
          messageVerbose("Checking the dates of the individual package Archives on the canonical CRAN mirror;",
                         " This will take some time")
        areTheyArchived <- Map(pk = possiblyArchivedPkg, counter = seq(possiblyArchivedPkg), USE.NAMES = TRUE,
                               function(pk, counter) {
          messageVerbose(counter, " of ", length(possiblyArchivedPkg), ": ", pk, verbose = verbose,
                         verboseLevel = 2)
          uu <- url(paste0("https://cran.r-project.org/package=", pk))
          on.exit(try(close(uu), silent = TRUE))
          rl <- suppressWarnings(try(readLines(uu), silent = TRUE))
          close(uu)
          wasRemoved <- any(grepl("was removed from the CRAN repository", rl))
          archivedOn <- ""
          if (wasRemoved) {
            # some CRAN repos e.g., RStudioPackage Manager is not a full CRAN mirror; try all repos
            if (all(isBinaryCRANRepo(repos))) {
              repos <- c(repos, CRAN = srcPackageURLOnCRAN)
            }
            for (repo in repos) {
              yy <- url(file.path(repo, srcContrib, "Archive", pk))
              on.exit(try(close(yy), silent = TRUE))
              rl2 <- suppressWarnings(try(readLines(yy), silent = TRUE))
              close(yy)
              if (!is(rl2, "try-error")) {
                break
              }
              messageVerbose("Could not get ", pk, " at ", repo, verbose = verbose, verboseLevel = 2)
              if (length(repos) > 1)
                messageVerbose("; trying next CRAN repo", verbose = verbose, verboseLevel = 2)

            }

            archivedOn <- grep("Archived on", rl, value = TRUE)
            lineWDateAndPkgFilename <- tail(grep(paste0(pk, ".*tar.gz"), rl2, value = TRUE), 1)
            pkgFilename <- gsub(paste0(".+(",pk,"_.+tar.gz).+.+"), "\\1", lineWDateAndPkgFilename)
            PackageUrl <- file.path(pk, pkgFilename)

            if (length(archivedOn)) {
              archivedOn <- as.POSIXct(gsub("Archived on (.+) as.+", "\\1", archivedOn))
            } else {
              archivedOn <- gsub(".+([[:digit:]]{4,4}-[[:digit:]]{2,2}-[[:digit:]]{2,2}).+", "\\1", lineWDateAndPkgFilename)
              archivedOn <- as.POSIXct(archivedOn) + 5 * 3600 * 24
            }
            archivedOn <- as.character(as.POSIXct(archivedOn) - 3600 * 24)
          } else {
            archivedOn <- notInArchives
            PackageUrl <- ""
          }
          list(PackageUrl = PackageUrl, archivedOn = archivedOn)
        })
        areTheyArchived <- invertList(areTheyArchived)
        inArchives <- !grepl(notInArchives, areTheyArchived$archivedOn)
        notArchived <- areTheyArchived$archivedOn[inArchives %in% FALSE]
        areTheyArchived$archivedOn[inArchives %in% FALSE] <- as.POSIXct(NA_character_)
        # areTheyArchived <- areTheyArchived[inArchives]
        latestDates <- data.table(Package = names(areTheyArchived$archivedOn),
                                  dayAfterPutOnCRAN = as.POSIXct(unlist(areTheyArchived$archivedOn)),
                                  PackageUrl = unlist(areTheyArchived$PackageUrl))
        removeCols <- intersect(c("dayAfterPutOnCRAN", "PackageUrl"), colnames(pDT))
        aa <- data.table::copy(pDT)
        if (length(removeCols)) {
          aa <- aa[, -c(..removeCols)]
      }
        aa <- aa[latestDates, on = "Package"]
        set(aa, NULL, "correctVersionAvail", !is.na(aa$dayAfterPutOnCRAN))
        whGood <- aa$correctVersionAvail %in% TRUE
        set(aa, which(!whGood), "repoLocation", "Fail")
        set(aa, which(whGood), c("repoLocation", "installFrom"), "Archive")
        set(aa, NULL, c("OlderVersionsAvailable"), gsub(".+_(.+)\\.tar\\.gz", "\\1", unlist(aa$PackageUrl)))

        pDT <-
          rbindlist(list(aa, pDT[!Package %in% aa$Package]), use.names = TRUE, fill = TRUE)
      }

      # do GitHub second
      # if (any(pkgDT$Package %in% "LandR.CS")) browser()
      githubNeedInstall <- !pDT$correctVersion %in% FALSE & pDT$installed %in% FALSE & pDT$repoLocation %in% "GitHub"
      if (any(githubNeedInstall)) {
        set(pDT, NULL, "tmpOrder", seq(NROW(pDT)))
        needInstallInd <- githubNeedInstall %in% TRUE # deals with NAs
        whNeedInstall <- which(needInstallInd)
        pDT2 <- getGitHubFile(pDT[whNeedInstall, ])
        pDT1 <- pDT[which(!needInstallInd), ]
        pDT <- rbindlist(list(pDT1, pDT2), use.names = TRUE, fill = TRUE)[]
        setorderv(pDT, "tmpOrder")
        set(pDT, NULL, "tmpOrder", NULL)

        # pDT <- getGitHubDESCRIPTION(pDT, purge = purge)
        # whGH <- which(pDT$repoLocation == "GitHub")
        # if (length(whGH)) {
        set(pDT, whNeedInstall, "AvailableVersion", DESCRIPTIONFileVersionV(pDT$DESCFile[whNeedInstall]))
        # pDT[repoLocation == "GitHub", AvailableVersion := DESCRIPTIONFileVersionV(DESCFile)]
        set(pDT, whNeedInstall, "compareVersionAvail", .compareVersionV(pDT$AvailableVersion[whNeedInstall], pDT$versionSpec[whNeedInstall]))
        # pDT[repoLocation == "GitHub",
        #                    compareVersionAvail := .compareVersionV(AvailableVersion, versionSpec)]
        pDT[repoLocation == "GitHub" & !is.na(inequality),
            correctVersionAvail := .evalV(.parseV(text = paste(compareVersionAvail, inequality, "0")))]
        # }
        set(pDT, NULL, c("url", "DESCFile"), NULL)

      }

      pkgDT <- pDT
      if (any(possiblyArchived)) {
        takenOffCranDT <- pDT[Package %in% possiblyArchivedPkg]
        if (any(takenOffCranDT$correctVersionAvail))
          messageVerbose(paste(takenOffCranDT$Package[takenOffCranDT$correctVersionAvail],
                          collapse = ", "), " not on CRAN; found an older Archive",
                         verbose = verbose, verboseLevel = 0)
        if (!any(takenOffCranDT$correctVersionAvail))
          messageVerbose(paste(takenOffCranDT$Package[!takenOffCranDT$correctVersionAvail],
                               collapse = ", "), " not on CRAN; did not find Archive",
                         verbose = verbose, verboseLevel = 0)
      }

    } else {
      pkgDT[, correctVersionAvail := NA]
    }
  }

  pkgDT
}

#' @inheritParams Require
#' @rdname Require-internals
#' @export
installFrom <- function(pkgDT, purge = FALSE, repos = getOption("repos"),
                        verbose = getOption("Require.verbose")) {
  cn <- colnames(pkgDT)

  if (!"installed" %in% cn) {
    stop("pkgDT needs a column named 'installed' to indicate whether it is installed or not")
  }

  pkgDT <- pkgDT[correctVersion == FALSE | is.na(correctVersion) & installed == FALSE, needInstall := TRUE]
  if (NROW(pkgDT[needInstall == TRUE])) {
    pkgDT[needInstall == TRUE &
            (correctVersionAvail == TRUE | is.na(correctVersionAvail)) & repoLocation == "CRAN",
          installFrom := repoLocation]
    pkgDT[needInstall == TRUE &
            (correctVersionAvail == TRUE | is.na(correctVersionAvail)) & repoLocation == "GitHub",
          installFrom := repoLocation]
    whFails <- ifelse(is.na(pkgDT$correctVersionAvail), FALSE,
                      ifelse(pkgDT$needInstall == TRUE & pkgDT$correctVersionAvail == FALSE, TRUE, FALSE))
    pkgDT[whFails, `:=`(installFrom = "Fail", installResult = "No available version")]
    anyWhFails <- any(whFails, na.rm = TRUE)
    if (anyWhFails) {
      messageVerbose("\033[36m", paste(unique(pkgDT$packageFullName[whFails %in% TRUE]), collapse = ", "),
                              " could not be installed; the version specification cannot be met\033[39m",
                     verbose = verbose, verboseLevel = 1)
    }
    if ("OlderVersionsAvailable" %in% colnames(pkgDT)) {
      pkgDT[needInstall == TRUE &
              (correctVersionAvail == TRUE) &
              repoLocation == "Archive", installFrom := "Archive"]
    }
  } else {
    pkgDT[, installFrom := NA_character_]
  }

  # Check for local copy of src or binary first
  if (!is.null(rpackageFolder(getOptionRPackageCache()))) {
    localFiles <- dir(rpackageFolder(getOptionRPackageCache()), full.names = TRUE)
    # sanity check -- there are bad files, quite often
    fileSizeEq0 <- is.na(file.size(localFiles)) | file.size(localFiles) == 0
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
          cachedAvailablePackages <- available.packagesCached(repos = repos, purge = purge, verbose = verbose)
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
        neededVersions <- nfs[neededVersions, on = c("neededFiles")]
        if (isWindows() && interactive() && NROW(nfs[localType == "source"])) {
          srcFromCRAN <- neededVersions$installFrom == "CRAN" & neededVersions$localType == "source"
          if (NROW(neededVersions[srcFromCRAN])) {
            messageDF(unique(neededVersions[srcFromCRAN, c("packageFullName", "Package", "localFileName")]),
                      verbose = verbose, verboseLevel = 0)
            messageVerbose(paste0("Local *source* file(s) exist for the above package(s).\nWould you like to delete it/them ",
                                  "and let Require try to find the binary on CRAN (or MRAN if older)? Y or N: "),
                           verbose = verbose, verboseLevel = 0)
            out <- if (interactive())
              readline()
            else
              "Y"
            if (identical("y", tolower(out))) {
              unlink(file.path(rpackageFolder(getOptionRPackageCache()),
                               neededVersions[srcFromCRAN]$localFileName))
            }
          }
        }
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
            if (any(hasSubFolder)) {
              Branch <- paste0(Branch, "/", GitSubFolder)
            }
            file.path("https://raw.githubusercontent.com", Account, Repo, Branch, filename, fsep = "/")
          },
          by = "Package"]

    checkPath(dirname(tempfile()), create = TRUE)
    set(pkgDT, NULL, "destFile",
        file.path(tempdir(), paste0(pkgDT$Package, "_", pkgDT$Version, "_", filename)))
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

updateInstalled <- function(pkgDT, installPkgNames, warn = NULL) {
  if (is(warn, "pkg_install_result")) {
    pkgs <- extractPkgName(warn$ref[warn$type %in% c("standard", "github")])
    pkgDT[match(installPkgNames, Package),
          `:=`(installResult = warn[match(installPkgNames,pkgs), "status"],
               installed = warn[match(installPkgNames, pkgs), "status"] == "OK")]
  } else {
    if (NROW(installPkgNames)) {
      pkgsWithWarns <- names(warn)
      pkgsWithoutWarns <- setdiff(installPkgNames, pkgsWithWarns)
      if (NROW(pkgsWithoutWarns)) {
        set(pkgDT, which(pkgDT$Package %in% installPkgNames), "installed", TRUE)
      }
      if (NROW(pkgsWithWarns)) {
        set(pkgDT, which(pkgDT$Package %in% extractPkgName(names(warn))), "installed", FALSE)
      }

      if (length(unlist(warn))) {
        # warning(warn)
        warnNamesUniq <- unique(names(warn))
        names(warnNamesUniq) <- warnNamesUniq
        warnGrp <- split(seq(length(warn)), names(warn))
        concat <- lapply(warnGrp, function(x) paste(unlist(warn[x]), collapse = "; "))
        pkgDT[match(names(concat), Package), installResult := unlist(concat)]
      }


    }
  }

  pkgDT[]
}

#' @inheritParams Require
#' @rdname Require-internals
#' @importFrom utils sessionInfo
#' @export
#' @details
#' `doInstall` is a wrapper around `utils::install.packages`,
#' `installGithub`, and `installCRAN`, and `installArchive`
doInstalls <- function(pkgDT, install_githubArgs, install.packagesArgs,
                       install = TRUE, repos = getOption("repos"),
                       verbose = getOption("Require.verbose"),
                       ...) {

  doPackages <- ((pkgDT$needInstall %in% TRUE) &  # needs to be installed
    !(pkgDT$installFrom %in% c("Fail", "Duplicate")) &  # can be installed
    pkgDT$installed %in% FALSE)  # and isn't already installed
  if (any(doPackages) ) {
    install.packagesArgs["INSTALL_opts"] <- unique(c("--no-multiarch", install.packagesArgs[["INSTALL_opts"]]))
    install_githubArgs["INSTALL_opts"] <- unique(c("--no-multiarch", install_githubArgs[["INSTALL_opts"]]))
    if (is.null(list(...)$destdir) && (isTRUE(install) || identical(install, "force"))) {
      if (!is.null(rpackageFolder(getOptionRPackageCache()))) {
        ip <- .installed.pkgs()
        isCranCacheInstalled <- any(grepl("crancache", ip[, "Package"])) && identical(Sys.getenv("CRANCACHE_DISABLE"), "")
        if (isTRUE(isCranCacheInstalled)) {
          messageVerbose(
              "Package crancache is installed and option('Require.RPackageCache') is set; it is unlikely that both are needed. ",
              "turning off crancache with Sys.setenv('CRANCACHE_DISABLE' = TRUE). ",
              "To use only crancache's caching mechanism, set both:",
              "\noptions('Require.RPackageCache' = NULL)\n",
              "Sys.setenv('CRANCACHE_DISABLE' = '')",
              verbose = verbose, verboseLevel = 0
            )
          Sys.setenv("CRANCACHE_DISABLE" = TRUE)
        }

        checkPath(rpackageFolder(getOptionRPackageCache()), create = TRUE)
        install.packagesArgs["destdir"] <- paste0(gsub("/$", "", rpackageFolder(getOptionRPackageCache())), "/") ## TODO: why need trailing slash here?
        if (getOption("Require.buildBinaries", TRUE)) {
          install.packagesArgs[["INSTALL_opts"]] <- unique(c("--build", install.packagesArgs[["INSTALL_opts"]]))
        }

        install_githubArgs["destdir"] <- install.packagesArgs["destdir"]
      }
    }
  }

  # if (any(pkgDT$Package %in% "LandR.CS")) browser()

  if (any(doPackages) && (isTRUE(install) || install == "force")) {
    dots <- list(...)
    set(pkgDT, NULL, "tmpOrder", seq_len(NROW(pkgDT)))
    # pkgDT[, tmpOrder := seq_len(NROW(pkgDT))]

    toInstall <- pkgDT[(installed == FALSE  | !correctVersion) & !(installFrom %in% c("Fail", "Duplicate"))]
    hasRequireDeps <- pkgDT[(installed == FALSE  | !correctVersion) & Package == "Require"]
    if (NROW(hasRequireDeps) && NROW(pkgDT[Package == "Require" & installed != TRUE])) {
      installRequire(verbose = verbose)
      ip <- as.data.table(installed.packages())
      installedRequireVersion <- ip[Package == "Require"]$Version
      if (!is.na(pkgDT[Package == "Require"]$versionSpec)) {
        test <- paste0("compareVersion('", installedRequireVersion, "'", ", '",
                       pkgDT[Package == "Require"]$versionSpec, "')")
        comp <- eval(parse(text = test))
        test2 <- paste(comp, pkgDT[Package == "Require"]$inequality, "0")
        comp2 <- eval(parse(text = test2))
        if (isFALSE(comp2))
          messageVerbose("Require was not able to install the requested version of itself because it is in use; ",
                         "Version ", installedRequireVersion, " installed", verbose = verbose, verboseLevel = 1)
      }
      toInstall <- toInstall[!Package %in% "Require"]
      pkgDT[Package == "Require", `:=`(needInstall = FALSE, installed = TRUE)]
    }
    if (any(!toInstall$installFrom %in% c("Fail", "Duplicate"))) {
      toInstall[, installFromFac := factor(installFrom, levels = c("Local", "CRAN", "Archive", "GitHub", "Fail", "Duplicate"))]
      setkeyv(toInstall, "installFromFac")
      topoSorted <- pkgDepTopoSortMemoise(toInstall$packageFullName, returnFull = TRUE, verbose = verbose, purge = FALSE)
      installSafeGroups <- attr(topoSorted, "installSafeGroups")
      correctOrder <- match(names(topoSorted), toInstall$packageFullName)
      toInstall <- toInstall[correctOrder, ]
      set(toInstall, NULL, "installSafeGroups", unname(unlist(installSafeGroups)))

      toInstall <- unique(toInstall, by = c("Package"))
      pkgsCleaned <- preparePkgNameToReport(toInstall$Package, toInstall$packageFullName)

      toInstall[, installOrder := seq(NROW(toInstall))]
      Package <- toInstall$Package
      names(Package) <- Package
      namespacesLoaded <- unlist(lapply(Package, isNamespaceLoaded))
      if (any(namespacesLoaded) && getOption("Require.unloadNamespaces", TRUE)) {
        si <- try(sessionInfo(), silent = TRUE)
        stopErrMess <- "The attempt to unload loaded packages failed. Please restart R and run again"
        if (is(si, "try-error")) stop(stopErrMess)
        allLoaded <- c(names(si$otherPkgs), names(si$loadedOnly))
        topoSortedAllLoaded <- try(names(pkgDepTopoSortMemoise(allLoaded, verbose = verbose, purge = FALSE)))

        if (is(topoSortedAllLoaded, "try-error"))
          stop(stopErrMess)
        topoSortedAllLoaded <- setdiff(topoSortedAllLoaded, c("Require", "testit", "remotes", "data.table", "glue", "rlang"))

        detached <- detachAll(topoSortedAllLoaded, doSort = FALSE, verbose = verbose)
        if (NROW(detached)) {
          detached <- as.data.table(detached, keep.rownames = "Package")
          pkgDT <- detached[pkgDT, on = "Package"]
        }
      }
      startTime <- Sys.time()

      if (isWindows()) { # binaries on CRAN
        toInstall[installFrom %in% c("CRAN", "Local"), installSafeGroups := -1]
      }
      data.table::setorderv(toInstall, c("installSafeGroups", "Package")) # alphabetical order
      toInstall[, installOrder := seq(.N)]
      set(toInstall, NULL, "installSafeGroups", as.integer(factor(toInstall$installSafeGroups)))
      maxGroup <- max(toInstall$installSafeGroups)
      if (maxGroup > 1)
        messageVerbose("Installing in groups to maintain dependencies: ", paste(pkgsCleaned, collapse = ", "),
                       verbose = verbose, verboseLevel = 1)

      out <- by(toInstall, toInstall$installSafeGroups, installAny, pkgDT = pkgDT,
                dots = dots, numGroups = maxGroup,
                numPackages = NROW(toInstall), startTime = startTime,
                install.packagesArgs = install.packagesArgs,
                install_githubArgs = install_githubArgs, repos = repos, verbose = verbose)
      setorderv(pkgDT, "tmpOrder")
      set(pkgDT, NULL, "tmpOrder", NULL)
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
    # requireOut <- lapply(packages, function(pkg) {
    #   outMess <- capture.output({
    #     out <- require(pkg, character.only = TRUE)
    #   }, type = "message")
    #   warn <- warnings()
    #   grep4a <- "version.*cannot be unloaded" # means that prior to changing .libPaths, the package was loaded; .libPaths version is older
    #   grep3a <- "no package called"
    #   grep3b <- "could not be found"
    #   missingDeps <- grepl(paste0(grep3a, "|", grep3b), outMess)
    #
    #   grep2 <- "package or namespace load failed for"
    #   grep1 <- "onLoad failed in loadNamespace"
    #   otherErrors <- grepl(paste(grep1, "|", grep2), outMess)
    #   libPathsVersTooOld <- grepl(grep4a, outMess)
    #   toInstall <- character()
    #   pkgsWarned <- c()
    #   if (any(otherErrors) || any(missingDeps)) {
    #     if (any(otherErrors)) {
    #       error1 <- grepl(.libPaths()[1], outMess)
    #       error1Val <- gsub(paste0("^.*",.libPaths()[1], "\\/(.*)", "\\/.*$"), "\\1", outMess[error1])
    #       packageNames <- unique(unlist(lapply(strsplit(error1Val, "\\/"), function(x) x[[1]])))
    #       error2 <- grepl("no such symbol", outMess)
    #       if (any(error2)) {
    #         pkgs <- paste(packageNames, collapse = "', '")
    #         pkgsWarned <- c(pkgsWarned, pkgs)
    #         warningCantInstall(pkgs)
    #       }
    #       error3 <- grepl("is being loaded, but", outMess)
    #       packageNames <- gsub(paste0("^.*namespace .{1}([[:alnum:][:punct:]]+).{1} .+is being.+$"),
    #                            "\\1", outMess[error3])
    #       if (any(error3)) {
    #         pkgs <- paste(packageNames, collapse = "', '")
    #         if (length(setdiff(pkgs, pkgsWarned)) > 0)
    #           warningCantInstall(pkgs)
    #         else
    #           pkgsWarned <- c(pkgsWarned, pkgs)
    #       }
    #     }
    #     doDeps <- if (!is.null(list(...)$dependencies)) list(...)$dependencies else TRUE
    #     if (any(missingDeps) && doDeps) {
    #       grep3a_1 <- paste0(".*",grep3a,".{2}(.*).{1}")
    #       packageNames <- character()
    #       if (any(grepl(grep3a, outMess[missingDeps])))
    #         packageNames <- unique(gsub(grep3a_1, "\\1", outMess[missingDeps]))
    #       grep3b_1 <- paste0(".*package.{2}(.*).{2}required.*$")
    #       if (any(grepl(grep3b, outMess[missingDeps])))
    #         packageNames <- unique(c(packageNames,
    #                                  unique(gsub(grep3b_1, "\\1", outMess[missingDeps]))))
    #       toInstall <- c(toInstall, packageNames)
    #       outMess <- grep(grep2, outMess, value = TRUE, invert = TRUE)
    #       outMess <- grep(grep3a, outMess, value = TRUE, invert = TRUE)
    #       outMess <- grep(grep3b, outMess, value = TRUE, invert = TRUE)
    #     }
    #   }
    #   if (any(libPathsVersTooOld)) {
    #     p <- pkgDT[Package == pkg]
    #     libPathsVers <- DESCRIPTIONFileVersionV(file.path(.libPaths()[1], pkg, "DESCRIPTION"),
    #                                             purge = TRUE)
    #     otherIsCorrect <- getPkgVersions(p, install = FALSE)$correctVersion
    #     firstPartMess <- paste0(pkg, " is already loaded from ", p$LibPath, " with version ", p$Version, ". ",
    #                             "The version in .libPaths() is ", libPathsVers)
    #     if (isTRUE(otherIsCorrect)) {
    #       messageVerbose(firstPartMess, ". Because the newer version still accommodates the minimum version number (",
    #                      p$packageFullName,", updating now.",
    #                      verbose = verbose, verboseLevel = 1)
    #       browser()
    #
    #       oo <- Require(pkg, require = FALSE, install = "force")
    #       outMessToRm <- grep("Loading required|Failed with error|Error in unloadNamespace", outMess)
    #       outMessToRm <- c(outMessToRm, max(outMessToRm) + 1) # There is non ASCII character in the message that can't be explicitly used
    #       outMess <- outMess[-outMessToRm]
    #     } else {
    #       warning(firstPartMess, ". The newer version fails the version number test.",
    #               " Please either change the version number requested,",
    #               " or prevent the newer version from loading by changing the .libPaths() prior",
    #               " to any packages being loaded.")
    #     }
    #   }
    #   if (length(outMess) > 0)
    #     messageVerbose(paste0(outMess, collapse = "\n"),
    #                    verbose = verbose, verboseLevel = 2)
    #   return(list(out = out, toInstall = toInstall))
    # })
    # out <- unlist(lapply(requireOut, function(x) x$out))
    # toInstall <- unlist(lapply(requireOut, function(x) x$toInstall))

    # if (length(toInstall)) {
    #   messageVerbose("Installed package(s) didn't have all dependencies installed,",
    #             " possibly because they were unknown; trying again",
    #             verbose = verbose, verboseLevel = 1)
    #   # These should only be loaded if they are in the original pkgDT,
    #   #   which in all cases should be "none of the toInstall should be loaded"
    #   browser()
    #
    #   out2 <- Require(unique(toInstall), require = FALSE, ...)
    #   out2 <- unlist(out2)
    #   names(out2) <- unique(toInstall)
    #   if (any(!out)) {
    #
    #     pkgToTryAgain <- pkgDTForLoad[Package %in% names(out)]
    #     if (NROW(pkgToTryAgain)) {
    #       retryOut <- doLoading(pkgToTryAgain)
    #       if (isTRUE(any(retryOut$loaded))) {
    #         out[match(retryOut[loaded == TRUE]$Package, names(out))] <- TRUE
    #       }
    #     }
    #   }
    #
    # }

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
    ipa <- modifyList2(install_githubArgs, dots)
    ipa <- modifyList2(ipa, list(verbose = verbose))
    warns <- messes <- errors <- list()

    if (NROW(gitPkgsToInstall)) {
      #out1 <- withCallingHandlers(
        do.call(installGithubPackage, append(list(gitPkgsToInstall), ipa))#,
        # warning = function(w) {
        #   browser()
        #   warns <<- appendToWarns(w$message, warns)
        #   invokeRestart("muffleWarning")
        # })
    }

    # pkgDT <- updateInstalled(pkgDT, gitPkgNamesSimple, warns)
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
  pkgDT[]
}

#' @importFrom utils available.packages
#' @inheritParams Require
available.packagesCached <- function(repos, purge, verbose = getOption("Require.verbose")) {
  if (internetExists("cannot get available packages", verbose = verbose)) {
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
                                  available.packages(ignore_repo_cache = TRUE, repos = repos, type = type))
      cap <- do.call(rbind, cap)
      if (length(types) > 1) {
        dups <- duplicated(cap[, c("Package", "Version")])
        cap <- cap[!dups,]
      }
      cap <- as.data.table(cap)
      assign("cachedAvailablePackages", cap, envir = .pkgEnv[["pkgDep"]])
      out <- cap
    } else {
      out <- get("cachedAvailablePackages", envir = .pkgEnv[["pkgDep"]], inherits = FALSE)
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
                # Deal with "binary" mumbo jumbo
                isBin <- isBinary(installPkgNamesInner)
                isBinNotLinux <- all(isBin ) && (isWindows() || isMacOSX())
                type <- c("source", "binary")[isBinNotLinux + 1]
                buildBinDots <- grepl("--build", dots)
                buildBinIPA <- grepl("--build", install.packagesArgs)
                buildBin <- any(buildBinDots, buildBinIPA)
                if (buildBin && any(isBin)) {
                  if (any(buildBinDots)) dots[buildBinDots] <- setdiff(dots[buildBinIPA][[1]], "--build")
                  if (any(buildBinIPA)) install.packagesArgs[buildBinIPA] <-
                      list(setdiff(install.packagesArgs[buildBinIPA][[1]], "--build"))
                }
                ipa <- modifyList2(list(type = type), install.packagesArgs, dots)
                ipa <- append(ipa, list(repos = NULL))
                ipa <- modifyList2(ipa, list(quiet = !(verbose >= 1)))
                prevWD <- setwd(tempdir2(.rndstr(1)))
                on.exit(setwd(prevWD), add = TRUE)
                curPkgs <- toIn$Package
                #while (NROW(installPkgNamesInner)) {
                  # a <- try(withCallingHandlers({
                    do.call(install.packages, append(list(installPkgNamesInner), ipa))
                  # }, warning = function(w) {
                  #   warns <<- appendToWarns(w$message, warns)
                  # }))
                  # if (is(a, "try-error")) {
                  #   # because previous line is vectorized
                  #   curPkgsFailed <- unlist(lapply(curPkgs, function(cp)
                  #     any(grep(x = a, pattern = cp), na.rm = TRUE)))
                  #   failedDueToUCRT <- grepl("UCRT", a)
                  #   if (failedDueToUCRT) {
                  #     unlink(installPkgNamesInner[curPkgsFailed])
                  #     curPkgsFailed <- curPkgs[curPkgsFailed]
                  #     pkgDT[Package %in% curPkgsFailed, `:=`(
                  #       installFrom = "Archive",
                  #       localFileName = NA)]
                  #     upToDone <- grep(curPkgsFailed, curPkgs)
                  #     installPkgNamesInner <- installPkgNamesInner[!seq(upToDone)]
                  #     if (upToDone > 1) {
                  #       pkgDT[Package %in% curPkgs[seq(upToDone - 1)], installed := TRUE]
                  #     }
                  #   } else {
                  #     stop(a)
                  #   }
                  # } else {
                  #   installPkgNamesInner <- character()
                  # }
                #}
                if (!all(isBin) && buildBin) {
                  copyTarball(Package, TRUE)
                }

              })

  #   warns <- lapply(installPkgNames, function(installPkgName) { # use lapply so any one package fail won't stop whole thing
  #     warn <- suppressMessages(withCallingHandlers({
  #       do.call(install.packages,
  #               # using ap meant that it was messing up the src vs bin paths
  #               append(list(installPkgName), ipa))
  #     }, warning = function(w) {
  #       ww <- list(w)
  #       pack <- names(installPackage)[unlist(lapply(names(installPackage),
  #                                                   function(pak) grepl(pak, w$message)))]
  #       names(ww) <- pack
  #       warnings1 <<- append(warnings1, ww)
  #       w
  #       })
  #     )
  #     if (!all(isBin) && buildBin) copyTarball(basename(installPkgNames), TRUE)
  #     warn
  #   })
  # })

  # pkgDT <- updateInstalled(pkgDT, toInstall$Package, warns)
  # permDen <- grepl("Permission denied", sapply(warns, function(w) w$message))
  # packagesDen <- gsub("^.*[\\/](.*).dll.*$", "\\1", names(warnings1))
  # if (any(permDen)) {
  #   stopMess <- character()
  #   if (any(pkgDT[Package %in% packagesDen]$installFrom == installFromCur))
  #     stopMess <- c(
  #       stopMess,
  #       paste0("Due to permission denied, you will have to restart R, and reinstall:\n",
  #              "------\n",
  #              "install.packages(c('", paste(packagesDen, collapse = "', '"), "'), lib = '",
  #              libPaths[1],"')")
  #     )
  #   if (any(pkgDT[Package %in% packagesDen]$installFrom == "GitHub"))
  #     stopMess <- c(
  #       stopMess,
  #       paste0("Due to permission denied, you will have to restart R, and reinstall:\n",
  #              "------\n", "remotes::install_github(c('",
  #              paste0(trimVersionNumber(pkgDT[Package %in% packagesDen]$packageFullName),
  #                     collapse = "', '"), "'), lib = '",libPaths[1],"')")
  #     )
  #   stop(stopMess)
  #}
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

  ipa <- modifyList2(install.packagesArgs, dots)

  # manually override "type = 'both'" because it gets it wrong some of the time
  ap <- as.data.table(.pkgEnv[["pkgDep"]]$cachedAvailablePackages)
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
      ipa <- modifyList2(list(type = type), ipa)
    }
  }

  needSomeSrc <- if (isWindows()) {
    rep(FALSE, NROW(pkgDT))
  } else {
    anyFromSrc <- installPkgNames %in% sourcePkgs()
    if (any(anyFromSrc))
      messageVerbose(verboseLevel = 1, verbose = verbose,
                     paste(installPkgNames[anyFromSrc], collapse = ", "),
                     " being forcibly installed",
                     " from source. To modify this, modify options('Require.spatialPkgs') or ",
                     "options('Require.otherPkgs')")
    anyFromSrc
  }
  installPkgNamesList <- list()
  reposList <- list()
  if (any(needSomeSrc) && !identical(stripHTTPAddress(repos), stripHTTPAddress(srcPackageURLOnCRAN))) {
    installPkgNamesList$Src <- installPkgNames[needSomeSrc]
    installPkgNamesList$Reg <- installPkgNames[!needSomeSrc]
    messageVerbose("The following package(s) need to be (and will be) installed from source: ",
                   paste(installPkgNamesList$Src, collapse = ", "),
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
    # warn <- NULL
    tmpPath <- tempdir2(.rndstr())
    orig <- setwd(tmpPath)
    on.exit({
      unlink(tmpPath, recursive = TRUE)
      setwd(orig)
    })
    if (!is.null(getOption("Ncpus"))) tries <- 5
    for (attempt in seq(tries)) {
      warns <- list()
      Map(installPkgNames = installPkgNamesList, repos = reposList,
          function(installPkgNames, repos) {

            ipa <- modifyList2(ipa, list(quiet = !(verbose >= 1)))
            ipaFull <- append(list(installPkgNames, repos = repos), ipa)

            installPackagesQuoted <-
              quote(do.call(install.packages, ipaFull))

            #withCallingHandlers({
              out <- eval(installPackagesQuoted)
            # }, warning=function(w) {
            #   if (isTRUE(grepl("cannot open URL.+PACKAGES.rds", w$message))) {
            #     outFromWarn <- tryInstallAgainWithoutAPCache(installPackagesQuoted)
            #   }
            #   warns <<- appendToWarns(w$message, warns)
            #   # invokeRestart("muffleWarning")
            # }, error = function(e) {
            #   av3CacheFile <- dir(tempdir(), pattern = paste0("^repos.+", gsub(".*\\/\\/", "", repos)), full.names = TRUE)
            #   if (grepl('argument \\"av2\\" is missing', e)) {
            #     tryCatch(warning(paste0("package '" ,installPkgNames,"' is not available (for ", R.version.string,")")),
            #              warning = function(w) w)
            #   } else if (length(av3CacheFile) || isTRUE(grepl("cannot open URL.+PACKAGES.rds", e))) {
            #     tryInstallAgainWithoutAPCache(installPackagesQuoted)
            #   } else {
            #     stop(e)
            #   }
            # })
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

    if (any(grepl("--build", c(dots, install.packagesArgs))))
      copyTarball(installPkgNames, TRUE)

    # pkgDT <- updateInstalled(pkgDT, installPkgNames, warns)
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

  prevWD <- setwd(tempdir2(.rndstr(1)))
  on.exit(setwd(prevWD), add = TRUE)

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

    ipa <- modifyList2(install.packagesArgs, dots)
    ipa <- append(ipa, list(repos = NULL, type = "bin"))


    urlsSuccess <- urlsOuter[urlsOuter != "Fail"]
    urlsFail <- urlsOuter[urlsOuter == "Fail"]
    ipa <- modifyList2(ipa, list(quiet = !(verbose >= 1)))

    #withCallingHandlers(
        do.call(install.packages, append(list(unname(urlsSuccess)), ipa))#,
      # warning = function(w) {
      #   ww <- list(w)
      #   pack <- names(urlsSuccess)[unlist(lapply(names(urlsSuccess), function(pak) grepl(pak, w$message)))]
      #   names(ww) <- pack
      #   warnings1 <<- append(warnings1, ww)
      # })
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
    install.packagesArgs <- modifyList2(install.packagesArgs, list(type = "source"))
    installPkgNamesArchiveOnly <- toIn$Package[!onMRAN]
    for (repo in repos) {
      cranArchivePath <- file.path(repo, "src/contrib/Archive/")
      errorMess <- list()
      warn <- list()
      p <- toIn$PackageUrl[!onMRAN]
      p <- file.path(cranArchivePath, p)
      dots$type <- "source" # must be a source
      ipa <- modifyList2(install.packagesArgs, dots)
      ipa <- append(ipa, list(repos = NULL))
      ipa <- modifyList2(ipa, list(quiet = !(verbose >= 1)))

      # out <- withCallingHandlers({
        do.call(install.packages,
                # using ap meant that it was messing up the src vs bin paths
                append(list(unname(p)), ipa))
      # },
      # warning = function(w) {
      #   browser()
      #   warn <<- appendToWarns(w$message, warns)
      #   # invokeRestart("muffleWarning")
      # }
      #
      # )
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
    # pkgDT <- updateInstalled(pkgDT, installPkgNamesArchiveOnly, warn)
  }
  if (any(grepl("--build", c(dots, install.packagesArgs))))
    copyTarball(installPkgNames, TRUE)

  pkgDT
}

installAny <- function(pkgDT, toInstall, dots, numPackages, numGroups, startTime, install.packagesArgs,
                       install_githubArgs, repos = getOption("repos"), verbose = verbose) {
  currentTime <- Sys.time()
  dft <- difftime(currentTime, startTime, units = "secs")
  installRange <- unique(c(toInstall$installOrder[1], tail(toInstall$installOrder, 1) ))
  timeLeft <- dft/installRange[1] * (numPackages - installRange[1] + 1)

  lotsOfTimeLeft <- dft > 10
  timeLeftAlt <- if (lotsOfTimeLeft) format(timeLeft, units = "auto", digits = 1) else "..."
  estTimeFinish <- if (lotsOfTimeLeft) Sys.time() + timeLeft else "...calculating"
  pkgToReport <- paste(preparePkgNameToReport(toInstall$Package, toInstall$packageFullName), collapse = ", ")
  pkgToReportBySource <- split(toInstall$Package, toInstall$installFrom)
  installRangeCh <- paste(installRange, collapse = ":")


  srces <- names(pkgToReportBySource)
  messageVerbose("-- Installing from:", verbose = verbose, verboseLevel = 0)
  nxtSrc <- "Local"
  if (nxtSrc %in% srces)
    messageVerbose("  -- ", nxtSrc,": ", paste(pkgToReportBySource[[nxtSrc]], collapse = ", ")
                   , verbose = verbose, verboseLevel = 0)
  nxtSrc <- "CRAN"
  if (nxtSrc %in% srces)
    messageVerbose("  -- ", nxtSrc,": ", paste(pkgToReportBySource[[nxtSrc]], collapse = ", ")
                   , verbose = verbose, verboseLevel = 0)
  nxtSrc <- "Archive"
  if ("Archive" %in% srces)
    messageVerbose("  -- ", nxtSrc,": ", paste(pkgToReportBySource[[nxtSrc]], collapse = ", ")
                   , verbose = verbose, verboseLevel = 0)
  nxtSrc <- "GitHub"
  if (nxtSrc %in% srces)
    messageVerbose("  -- ", nxtSrc,": ", paste(pkgToReportBySource[[nxtSrc]], collapse = ", ")
                   , verbose = verbose, verboseLevel = 0)
  messageVerbose("\033[34m-- ", installRangeCh, " of ", numPackages,
                 if (numGroups > 1)
                   paste0(" (grp ",unique(toInstall$installSafeGroups)," of ", numGroups,")")
                 else  "",
                 ". Estimated time left: ",
                 timeLeftAlt, "; est. finish: ", estTimeFinish, "\033[39m",
                 verbose = verbose, verboseLevel = 0)

  if (any("Local" %in% toInstall$installFrom)) {
    pkgDT <- installLocal(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs,
                          verbose = verbose)
    anyFaultyBinaries <- grepl("error 1 in extracting from zip file", pkgDT$installResult)
    if (isTRUE(anyFaultyBinaries)) {
      messageVerbose("Local cache of ", paste(pkgDT[anyFaultyBinaries]$localFileName, collapse = ", "),
                     " faulty; deleting",
                     verbose = verbose, verboseLevel = 2)
      unlink(file.path(rpackageFolder(getOptionRPackageCache()), pkgDT[anyFaultyBinaries]$localFileName))
    }
  }

  warnings1 <- c()
  messages <- c()
  if (internetExists("cannot install packages", verbose = verbose)) {
    if (any("CRAN" %in% toInstall$installFrom))
        pkgDT <- installCRAN(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs,
                             repos = repos, verbose = verbose)

    if (any("Archive" %in% toInstall$installFrom))
        pkgDT <- installArchive(pkgDT, toInstall, dots, install.packagesArgs, install_githubArgs,
                                                  repos = repos, verbose = verbose)

    if (any("GitHub" %in% toInstall$installFrom)) {
        pkgDT <- installGitHub(pkgDT, toInstall, install_githubArgs, dots,
                               verbose = verbose)
    }
  } else {
    pkgDT[pkgDT$packageFullName %in% toInstall$packageFullName, installFrom := "Fail"]
  }

  pkgDT
}

isBinary <- function(fn, fromCRAN = TRUE) {
  theTest <- endsWith(fn, "zip") | grepl("R_x86", fn)
  if (isTRUE(fromCRAN)) {
    binRepo <- isBinaryCRANRepo()
    # rspmURL <- formals(setLinuxBinaryRepo)[["binaryLinux"]]
    # binRepo <- startsWith(prefix = rspmURL, getOption("repos")[["CRAN"]])
    theTest <- theTest | binRepo
  }
  theTest
}


isBinaryCRANRepo <- function(curCRANRepo = getOption("repos")[["CRAN"]],
                             repoToTest = formals(setLinuxBinaryRepo)[["binaryLinux"]]) {
  startsWith(prefix = repoToTest, curCRANRepo)
}

copyTarball <- function(pkg, builtBinary) {
  if (builtBinary) {
    theDir <- dir(full.names = TRUE)
    newFiles <- lapply(pkg, function(pat) grep(pattern = paste0("/", pat, "_"), x = theDir, value = TRUE))
    if (length(unlist(newFiles))) {
      newFiles <- unlist(newFiles)
      newNames <- file.path(rpackageFolder(getOptionRPackageCache()), unique(basename(newFiles)))
      filesAlreadyExist <- file.exists(newNames)
      if (any(!filesAlreadyExist))
        try(linkOrCopy(newFiles[!filesAlreadyExist], newNames[!filesAlreadyExist]))
      unlink(newFiles)
      return(invisible(newNames))
    }
  }
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
  others <- pkgDepTopoSortMemoise(pkgs, deps = allLoaded, reverse = TRUE, verbose = verbose, purge = FALSE)
  names(others) <- others
  depsToUnload <- c(others, depsToUnload)
  depsToUnload <- depsToUnload[!duplicated(depsToUnload)]
  depsToUnload <- setdiff(depsToUnload, dontTry)

  if (length(depsToUnload) > 0) {
    out <- if (isTRUE(doSort)) pkgDepTopoSortMemoise(depsToUnload, purge = FALSE) else NULL
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
    if (normPath(path) %in% normPath(strsplit(Sys.getenv("R_LIBS_SITE"), split = ":")[[1]])) {
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
  gr <- splitGitRepo(gitRepo)
  dots <- list(...)
  quiet <- dots$quiet
  if (is.null(quiet)) quiet <- !(verbose >= 0)
  tmpPath <- tempdir2(.rndstr())
  checkPath(tmpPath, create = TRUE)
  # Check if it needs new install
  alreadyExistingDESCRIPTIONFile <- lapply(gr$repo, function(repo) file.path(libPath, repo, "DESCRIPTION"))
  useRemotes <- FALSE
  filesExist <- file.exists(unlist(alreadyExistingDESCRIPTIONFile))
  orig <- setwd(tmpPath)
  on.exit({
    setwd(orig)
  })
  skipDLandBuild <- FALSE

  if (!is.null(getOptionRPackageCache()) || any(filesExist)) {
    if (exists("aaaa")) print(paste("11 getOptionRPackageCache ", getOptionRPackageCache()))

    shaOnGitHub <-
      unlist(Map(repoInner = gr$repo, acctInner = gr$acct, brInner = gr$br,
              function(repoInner, acctInner, brInner)
                getSHAfromGitHub(repo = repoInner, acct = acctInner, br = brInner)))

    if (exists("aaaa")) print(paste("12 shaOnGitHub ", shaOnGitHub))

    if (any(filesExist)) { # means already installed here; no new install
      if (is(shaOnGitHub, "try-error")) {
        useRemotes <- TRUE
      } else {
        shaLocal <- DESCRIPTIONFileOtherV(alreadyExistingDESCRIPTIONFile, other = "GithubSHA1")
        if (exists("aaaa")) print(paste("13 shaLocal ", shaLocal))
        if (identical(unname(shaLocal), unname(shaOnGitHub))) {
          messageVerbose("Skipping install of ", gitRepo, ", the SHA1 has not changed from last install",
                         verbose = verbose, verboseLevel = 1)
          return(invisible())
        }
      }
    } else { # this means, not installed, but it may be in Cache
      dd <- dir(getOptionRPackageCache(), full.names = TRUE)
      cachedFiles <- lapply(shaOnGitHub, function(sha) grep(sha, dd, value = TRUE))
      if (exists("aaaa")) print(paste("14 cachedFiles ", cachedFiles))
      if (any(lengths(cachedFiles) > 0)) {
        cachedFilesHere <- Map(cf = cachedFiles, sha = shaOnGitHub, function(cf, sha)
          file.copy(cf, gsub(paste0("(", sha, ")."), "", basename(cf))))
        skipDLandBuild <- lengths(cachedFilesHere) > 0
        if (exists("aaaa")) print(paste("15 skipDLandBuild ", skipDLandBuild))
        if (any(skipDLandBuild)) {
          messageVerbose("Identical SHA for '", paste(names(skipDLandBuild)[skipDLandBuild], collapse = "', '"),
                         "' found in ", RequirePkgCacheDir(), "; using it",
                         verbose = verbose, verboseLevel = 0)
        }


      }
    }
  }

  stillNeedDLandBuild <- !skipDLandBuild
  if (any(stillNeedDLandBuild)) {
    if (exists("aaaa")) print(paste("16 stillNeedDLandBuild ", stillNeedDLandBuild))

    if (!useRemotes) {
      out <- downloadRepo(gitRepo[stillNeedDLandBuild], overwrite = TRUE, destDir = tmpPath, verbose = verbose)
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
      if (exists("aaaa")) print(paste("17 stillNeedDLandBuild ", stillNeedDLandBuild))
      messageVerbose("building package (R CMD build)",
                     verbose = verbose, verboseLevel = 1)
      internal <- !interactive()
      extras <- c("--no-resave-data", "--no-manual",
                  "--no-build-vignettes")
      Rpath1 <- Sys.getenv("R_HOME")
      Rpath <- file.path(Rpath1, "bin/R") # need to use Path https://stat.ethz.ch/pipermail/r-devel/2018-February/075507.html
      out1 <- lapply(gr$repo[stillNeedDLandBuild], function(repo) {
        system(paste(Rpath, "CMD build ", repo, paste(extras, collapse = " ")),
               intern = internal, ignore.stdout = quiet, ignore.stderr = quiet)
      })
      if (any(unlist(out) == 1L)) stop("Error 456; contact developer")
      theDESCRIPTIONfile <- dir(out, pattern = "DESCRIPTION", full.names = TRUE)
      if (exists("aaaa")) print(paste("18 theDESCRIPTIONfile ", theDESCRIPTIONfile))
      packageName <- DESCRIPTIONFileOtherV(theDESCRIPTIONfile, other = "Package")
      if (exists("aaaa")) print(paste("19 packageName ", packageName))
      messageVerbose("  ... Built!",
                     verbose = verbose, verboseLevel = 1)
      if (exists("aaaa")) print(paste("1 packageName ", packageName))
      shaOnGitHub2 <- shaOnGitHub[packageName]
      Map(pack = packageName, sha = shaOnGitHub2, function(pack, sha) {
        fns <- copyTarball(pack, builtBinary = TRUE)
        if (exists("aaaa")) print(paste("2 fns ", fns))
        out <- file.rename(fns, file.path(dirname(fns), paste0(sha, ".", basename(fns))))
        if (exists("aaaa")) print(paste("3 out ", out))
        return(out)
      })


    } else {
      stop("Can't install packages this way because R is not on the search path")
    }
  }
  localDir <- dir()
  packageFNtoInstall <- lapply(gr$repo, function(pak) {
    packageFNtoInstall <- grep(pattern = paste0(pak, ".+(tar.gz|zip)"), localDir, value = TRUE)
    isBin <- isBinary(packageFNtoInstall, fromCRAN = FALSE)
    if (any(isBin)) {
      packageFNtoInstall <- packageFNtoInstall[isBin]
    }
    packageFNtoInstall
  })

  if (!isTRUE(grepl("--build", dots$INSTALL_opts)) && !is.null(getOptionRPackageCache())) {
    if (!all(isBinary(unlist(packageFNtoInstall), fromCRAN = FALSE)))
      dots$INSTALL_opts <- paste(dots$INSTALL_opts, "--build")
  }

  packageFNtoInstall <- unlist(packageFNtoInstall[pmatch(names(gitRepo), packageFNtoInstall)])
  packageName <- names(packageFNtoInstall)
  names(packageName) <- packageName

  if (exists("aaaa")) print(paste("4 packageName ", packageName))
  opts2 <- append(dots,
                  list(packageFNtoInstall,
                       repos = NULL,
                       lib = normalizePath(libPath, winslash = "/")))
  opts2 <- append(opts2, list(type = "source")) # it may have "binary", which is incorrect
  opts2 <- modifyList2(opts2, list(quiet = !(verbose >= 1)))
  if (exists("aaaa")) print(paste("5 opts2 ", opts2))
  if (is.null(opts2$destdir)) {
    cachePath <- getOptionRPackageCache()
    opts2$destdir <- if (is.null(cachePath)) tmpPath else cachePath
  }
  warns <- list()
  # Need this internal wCH because need to know which one failed
  withCallingHandlers(
    do.call(install.packages, opts2),
    warning = function(w) {
      warns <<- appendToWarns(w = w$message, warns = warns, Packages = packageName)
    }
  )
  if (exists("aaaa")) print(paste("6 Done ", "Done"))

  installStatus <- vapply(packageName, function(x) TRUE, logical(1))
  pkgsToModify <- packageName
  if (length(unlist(warns))) {
    if (exists("aaaa")) print(paste("7 warns ", warns))
    problems <- unlist(lapply(packageName, function(pn) grepl(pn, warns)))
    installStatus[problems] <- FALSE
    pkgsToModify <- packageName[problems %in% FALSE]
    if (exists("aaaa")) print(paste("8 pkgsToModify ", pkgsToModify))
  }

  lapply(pkgsToModify, function(pack) {
    postInstallDESCRIPTIONMods(pkg = pack, repo = gr$repo[[pack]],
                               acct = gr$acct[[pack]], br = gr$br[[pack]],
                               lib = normalizePath(libPath, winslash = "/", mustWork = FALSE))
    if (!is.null(getOptionRPackageCache())) {
      fns <- copyTarball(pack, builtBinary = TRUE)
      if (exists("aaaa")) print(paste("9 fns ", fns))
      theDESCRIPTIONfile <- file.path(.libPaths()[1], pack, "DESCRIPTION")
      # system.file("DESCRIPTION", package = "peutils", lib.loc = .libPaths()[1])
      sha <- DESCRIPTIONFileOtherV(theDESCRIPTIONfile, other = "RemoteSha")
      if (exists("aaaa")) print(paste("10 sha ", sha))
      file.rename(fns, file.path(dirname(fns), paste0(sha, ".", basename(fns))))
    }
  })
  if (exists("aaaa")) print(paste("11 installStatus ", installStatus))

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
downloadRepo <- function(gitRepo, overwrite = FALSE, destDir = ".",
                         verbose = getOption("Require.verbose")) {
  dir.create(destDir, recursive = TRUE, showWarnings = FALSE)
  gr <- splitGitRepo(gitRepo)
  ar <- file.path(gr$acct, gr$repo)
  repoFull <- file.path(destDir, gr$repo)
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
  Map(bad = badDirname, repo = gr$repo, function(bad, repo) {
    file.rename(bad, gsub(basename(bad), repo, bad)) # it was downloaded with a branch suffix
  })

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

tryInstallAgainWithoutAPCache <- function(installPackagesQuoted, envir = parent.frame()) {
  nameOfEnvVari <- "R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE"
  prevCacheExpiry <- Sys.getenv(nameOfEnvVari)
  val <- 0
  val <- setNames(list(val), nm = nameOfEnvVari)
  do.call(Sys.setenv, val)
  prevCacheExpiry <- setNames(list(prevCacheExpiry), nm = nameOfEnvVari)
  on.exit(do.call(Sys.setenv, prevCacheExpiry), add = TRUE)
  out <- eval(installPackagesQuoted, envir = envir)
}

dealWithViolations <- function(pkgSnapshotObj, verbose = getOption("Require.verbose")) {
  dd <- pkgSnapshotObj
  ff <- ifelse(!is.na(dd$GithubRepo) & nzchar(dd$GithubRepo),
               paste0(dd$GithubUsername, "/", dd$Package, "@", dd$GithubSHA1), paste0(dd$Package, " (==", dd$Version, ")"))
  gg <- pkgDep(ff, recursive = TRUE)
  hh <- sort(unique(gsub(" ", "", gsub("\n", "", unname(unlist(gg))))))
  ii <- data.table::data.table(packageNameFull = hh,
                               Package = extractPkgName(hh),
                               DepVersion = extractVersionNumber(hh))
  suppressWarnings(ii[, maxVers := max(DepVersion, na.rm = TRUE), by = "Package"])
  ii[, keep := DepVersion == maxVers, by = "Package"]
  data.table::setorderv(ii, c("Package", "keep"), na.last = TRUE, order = -1L)
  ii <- ii[, .SD[1], by = "Package"]
  data.table::setorderv(ii, c("Package", "keep"), order = 1L)
  data.table::set(ii, NULL, c("maxVers", "keep"), NULL)
  kk <- ii[dd, on = "Package"]
  kk[is.na(DepVersion), DepVersion := Version]
  mm <- numeric_version(kk$Version) >= numeric_version(kk$DepVersion)
  data.table::setnames(kk, old = "Version", "InstalledVersion")
  kk[, violations := mm %in% FALSE][violations == TRUE, c("Package", "InstalledVersion", "DepVersion")]
  dd <- dd[kk[, c("Package", "DepVersion", "violations")], on = "Package"]
  dd[violations == TRUE, Version := DepVersion]
  set(dd, NULL, c("DepVersion"), NULL)
  dd <- unique(dd)
  dd[]
}

installPackagesSystem <- function(pkg, args, libPath) {
  opts2 <- append(args, list(lib = normalizePath(libPath, winslash = "/")))
  opts2 <- modifyList2(list(Ncpus = getOption("Ncpus")), opts2)
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
    fas <- modifyList2(fas, list(...)[pakFormalsPassedHere])
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
#' If a characater string, then it returns that.
#' If `TRUE`, then use `RequirePkgCacheDir()`. If `FALSE` then returns `NULL`.
#'
#' @export
getOptionRPackageCache <- function() {
  curVal <- getOption("Require.RPackageCache")
  try <- 1
  while (try < 3) {
    if (isTRUE(curVal)) {
      curVal <- RequirePkgCacheDir()
      break
    } else if (isFALSE(curVal)) {
      curVal <- NULL
      break
    } else {
      if (identical("default", curVal)) {
        fromEnvVars <- Sys.getenv("Require.RPackageCache")
        if (nchar(fromEnvVars) == 0  ) {
          curVal <- RequirePkgCacheDir()
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
  #masterMain <- c("main", "master")
  #masterMainGrep <- paste0("/", paste(masterMain, collapse = "|"), "(/|\\.)")
  # masterGrep <- paste0("/", "master", "(/|\\.)")
  # mainGrep <- paste0("/", "main", "(/|\\.)")
  hasMasterMain <- grepl(masterMainGrep, url)
  # hasMaster <- grepl(masterGrep, url)
  # hasMain <- grepl(mainGrep, url)
  # if (any(hasMasterMain) && need %in% masterMain) {
  #   # Good -- try both master and main
  #   br <- need
  # } else if (any(hasMasterMain) && need %in% "HEAD") {
  #   # need change
  #   br <- "HEAD"
  #   url <- gsub(masterMainGrep, paste0("/", br, "\\1"), url)
  # }
  # HEADgrep <- paste0("/", paste("HEAD", collapse = "|"), "(/|\\.)")
  # hasHEAD <- grepl(HEADgrep, url)
  # if (any(hasHEAD) && need %in% masterMain) {
  #   br <- need
  #   url <- gsub(HEADgrep, paste0("/", br, "\\1"), url)
  # }
  # if (any(hasHEAD) && need %in% "HEAD") {
  #   br <- "HEAD"
  # }
  # if (any(hasMasterMain) && length(url) == 1) {
  #   newBr <- masterMain[hasMain + 1]
  #   url[[2]] <- gsub(masterMainGrep, paste0("/", newBr, "\\1"), url)
  # }
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
    outNotMasterMain <- Map(URL = urls[["FALSE"]], df = destfile, function(URL, df)
      try(download.file(URL, destfile = df, quiet = TRUE), silent = TRUE))
  if (!is.null(urls[["TRUE"]])) # should be sequential because they are master OR main
    for (wh in seq(urls[["TRUE"]])) {
      outMasterMain <- try(download.file(urls[["TRUE"]][wh], destfile = destfile[wh], quiet = TRUE), silent = TRUE)
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
