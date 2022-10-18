Require2 <- function(packages, packageVersionFile,
                     libPaths, # nolint
                     install_githubArgs = list(),
                     install.packagesArgs = list(),
                     standAlone = getOption("Require.standAlone", FALSE),
                     install = getOption("Require.install", TRUE),
                     require = getOption("Require.require", TRUE),
                     repos = getOption("repos"),
                     purge = getOption("Require.purge", FALSE),
                     verbose = getOption("Require.verbose", FALSE),
                     ...) {
  dots <- list(...)
  if (missing(libPaths))
    libPaths <- .libPaths()
  deps <- pkgDep(packages)
  allPackages <- unname(unlist(deps))
  pkgDT <- toPkgDT(allPackages, deepCopy = TRUE)
  pkgDT <- parseGitHub(pkgDT)
  pkgDT <- pkgDT[!duplicated(pkgDT$packageFullName)]
  pkgDT <- installedVers(pkgDT)
  pkgDT[, versionSpec := extractVersionNumber(packageFullName)]
  setorderv(pkgDT, c("Package", "versionSpec"), na.last = TRUE)
  pkgDT[, keep := if (any(!is.na(versionSpec))) .I[1] else .I, by = "Package"]
  pkgDT <- pkgDT[unique(pkgDT$keep)]
  set(pkgDT, NULL, "keep", NULL)
  set(pkgDT, NULL, "isPkgInstalled", !is.na(pkgDT$Version))
  pkgDT[!is.na(versionSpec), inequality := extractInequality(packageFullName)]
  set(pkgDT, NULL, "installedVersionOK", !is.na(pkgDT$Version))
  hasVersionsToCompare <- (nchar(pkgDT$inequality) > 0) %in% TRUE & !is.na(pkgDT$Version)
  pkgDT[hasVersionsToCompare, installedVersionOK := {
    do.call(inequality, list(package_version(Version), versionSpec))
    }, by = seq(sum(hasVersionsToCompare))]
  set(pkgDT, NULL, "needInstall", c("dontInstall", "install")[pkgDT$installedVersionOK %in% FALSE + 1])
  if (any(pkgDT$needInstall %in% "install")) {
    tmpdir <- if (is.null(getOptionRPackageCache())) tempdir2(.rndstr(1)) else getOptionRPackageCache()
    origGetwd <- getwd()
    on.exit(setwd(origGetwd))
    out <- setwd(tmpdir)

    pkgDT <- doInstalls2(pkgDT, repos = repos, purge = purge, libPaths = libPaths, verbose = verbose,
                         dots = dots, install.packagesArgs = install.packagesArgs)
  }
  browser()
  # if ()
  pkgDT
}

rbindlistRecursive <- function(ll) {
  if (is(ll, "list")) {
    ll <- lapply(ll, rbindlistRecursive)
    ll <- rbindlist(ll, fill = TRUE, use.names = TRUE, idcol = FALSE)
  }
  ll
}


build <- function(Package, verbose, quiet, out) {
  if (nchar(Sys.which("R")) > 0) {
    messageVerbose("building package (R CMD build)",
                   verbose = verbose, verboseLevel = 1)
    internal <- !interactive()
    extras <- c("--no-resave-data", "--no-manual",
                "--no-build-vignettes")
    Rpath1 <- Sys.getenv("R_HOME")
    Rpath <- file.path(Rpath1, "bin/R") # need to use Path https://stat.ethz.ch/pipermail/r-devel/2018-February/075507.html
    out1 <- lapply(Package, function(pack) {
      system(paste(Rpath, "CMD build ", pack, paste(extras, collapse = " ")),
             intern = internal, ignore.stdout = quiet, ignore.stderr = quiet)
    })
    if (any(unlist(out1) == 1L)) stop("Error 456; contact developer")
    messageVerbose("  ... Built!",
                   verbose = verbose, verboseLevel = 1)
    localDir <- dir(pattern = paste0(Package, ".+.tar.gz"), full.names = TRUE)
    localDir
  } else {
    stop("Can't install packages this way because R is not on the search path")
  }
}


installAll <- function(toInstall, repos = getOptions("repos")) {
  type <- unique(c("source", "binary")[toInstall$isBinaryInstall + 1])
  install.packages(toInstall$localFile, repos = NULL, type = type, dependencies = FALSE)
}

doInstalls2 <- function(pkgDT, repos, purge, tmpdir, libPaths, verbose, dots, install.packagesArgs) {
  pkgDTList <- split(pkgDT, by = c("needInstall"))
  pkgNeedInstall <- pkgDTList[["install"]] # make a new pointer

  topoSorted <- pkgDepTopoSort(pkgNeedInstall$packageFullName)
  installSafeGroups <- attr(topoSorted, "installSafeGroups")
  correctOrder <- match(names(topoSorted), pkgNeedInstall$packageFullName)
  pkgNeedInstall <- pkgNeedInstall[correctOrder, ]
  set(pkgNeedInstall, NULL, "installSafeGroups", unname(unlist(installSafeGroups)))

  # pkgNeedInstall[, installFrom := repoLocation]
  set(pkgNeedInstall, NULL, "haveLocal", "noLocal")
  # check local cache
  if (!is.null(getOptionRPackageCache())) {
    localFiles <- dir(getOptionRPackageCache(), full.names = FALSE)
    origFiles <- mapply(pat = pkgNeedInstall$Package,
                        function(pat) {
                          paste(grep(pattern = paste0(".*", pat, "_"), x = localFiles, value = TRUE),
                                collapse = ",")
                        },
                        USE.NAMES = TRUE)
    pkgNeedInstall[, localFile := origFiles]
    pkgNeedInstall[, haveLocal :=
                     unlist(lapply(origFiles, function(x) c("noLocal", "Local")[isTRUE(nchar(x) > 0) + 1]))]
    pkgDTList[["install"]] <- split(pkgNeedInstall, by = "haveLocal")
    if (!is.null(pkgDTList[["install"]][["Local"]]))
      pkgDTList[["install"]][["Local"]][, installFrom := haveLocal]
  }
  pkgNeedInternet <- pkgDTList[["install"]][["noLocal"]] # pointer
  if (NROW(pkgNeedInternet)) {
    pkgDTList[["install"]][["noLocal"]] <- split(pkgDTList[["install"]][["noLocal"]], by = "repoLocation")
    pkgCRAN <- pkgDTList[["install"]][["noLocal"]][["CRAN"]]

    # CRAN
    if (NROW(pkgCRAN)) { # CRAN, Archive, MRAN
      ap <- available.packagesCached(repos = repos, purge = purge)[, c("Package", "Repository", "Version")]
      setnames(ap, old = "Version", new = "VersionOnRepos")
      pkgDTList[["install"]][["noLocal"]][["CRAN"]] <- ap[pkgCRAN, on = "Package"]
      pkgCRAN <- pkgDTList[["install"]][["noLocal"]][["CRAN"]] # pointer
      set(pkgCRAN, NULL, "tmpOrder", seq(NROW(pkgCRAN)))
      pkgCRAN[, availableVersionOK := !is.na(VersionOnRepos)]

      # Not on CRAN; so likely Archive
      if (any(pkgCRAN$availableVersionOK %in% FALSE)) {
        pkgCRAN[availableVersionOK %in% FALSE, repoLocation := "Archive"]
        pkgDTList[["install"]][["noLocal"]] <- rbindlistRecursive(pkgDTList[["install"]][["noLocal"]])
        pkgDTList[["install"]][["noLocal"]] <- split(pkgDTList[["install"]][["noLocal"]], by = "repoLocation")
        pkgCRAN <- pkgDTList[["install"]][["noLocal"]][["CRAN"]]
      }
      if (NROW(pkgCRAN)) {
        pkgCRAN[!is.na(inequality), availableVersionOK := {
          do.call(inequality, list(package_version(VersionOnRepos), versionSpec))
        }, by = "tmpOrder"]
        if (any(pkgCRAN[["availableVersionOK"]]))
          pkgCRAN[, installFrom := "CRAN"]
        pkgCRAN[, localFile := download.packages(pkgCRAN$Package, destdir = ".", type = "binary")[,2]]
      }
    }

    # Archive
    pkgArchive <- pkgDTList[["install"]][["noLocal"]][["Archive"]]
    if (NROW(pkgArchive)) {
      ava <- lapply(archiveVersionsAvailable(pkgArchive$Package[pkgArchive$repoLocation %in% "Archive"],
                                             repos = repos), function(d) {
                                               aa <- as.data.table(d, keep.rownames = "PackageUrl")
                                               setorderv(aa, "mtime")
                                             })
      cols <- c("PackageUrl", "dayAfterPutOnCRAN", "dayBeforeTakenOffCRAN", "repo", "VersionOnRepos", "availableVersionOK")
      pkgArchive[, c("PackageUrl", "dayAfterPutOnCRAN", "dayBeforeTakenOffCRAN", "repo", "VersionOnRepos", "availableVersionOK") := {
        Version2 <-  gsub(".*_(.*)\\.tar\\.gz", "\\1", ava[[Package]]$PackageUrl)
        if (is.na(versionSpec)) {
          correctVersions <- NROW(ava[[Package]])
        } else {
          correctVersions <- do.call(inequality, list(package_version(Version2), versionSpec))
          if (all(correctVersions %in% FALSE))
            correctVersions <- NA
          else
            correctVersions <- unique(c(which(correctVersions), min(which(correctVersions), length(correctVersions))))
        }
        if (length(correctVersions) == 1) correctVersions <- c(correctVersions, NA_integer_)
        earlyDate <- ava[[Package]][correctVersions[1]][["mtime"]] + secondsInADay
        ret <- ava[[Package]][correctVersions[1]][, c("PackageUrl", "mtime", "repo")]
        dayBeforeTakenOffCRAN <- ava[[Package]][correctVersions[2]][["mtime"]]
        if (is.na(dayBeforeTakenOffCRAN)) {
          dayBeforeTakenOffCRAN <- archivedOn(Package, verbose, repos, srcPackageURLOnCRAN, repo, srcContrib, notInArchives)
          dayBeforeTakenOffCRAN <- dayBeforeTakenOffCRAN[[1]]$archivedOn
        }

        set(ret, NULL, "dayBeforeTakenOffCRAN", dayBeforeTakenOffCRAN)
        setnames(ret, "mtime", "dayAfterPutOnCRAN")
        set(ret, NULL, "VersionOnRepos", Version2[correctVersions[1]])
        if (!is.na(correctVersions)[1])
          set(ret, NULL, "availableVersionOK", TRUE)
        data.table::setcolorder(ret, cols)
        ret
      }, by = "Package"]
      # Check MRAN
      outs <- downloadMRAN(pkgArchive, install.packagesArgs, verbose, dots)
      browser()


    }


    # GitHub
    pkgGitHub <- pkgDTList[["install"]][["noLocal"]][["GitHub"]]
    if (NROW(pkgGitHub)) { # GitHub
      pkgGitHub <- getGitHubFile(pkgGitHub)
      if (any(!pkgGitHub$isPkgInstalled)) {
        shaOnGitHub <-
          unlist(Map(repoInner = pkgGitHub$Repo, acctInner = pkgGitHub$Account,
                     brInner = pkgGitHub$Branch,
                     function(repoInner, acctInner, brInner) {
                       alreadyExistingDESCRIPTIONFile <- file.path(libPaths[1], repoInner, "DESCRIPTION")
                       SHAonGH <- getSHAfromGitHub(repo = repoInner, acct = acctInner, br = brInner)
                       if (file.exists(alreadyExistingDESCRIPTIONFile)) {
                         SHAonLocal <- DESCRIPTIONFileOtherV(alreadyExistingDESCRIPTIONFile, other = "GithubSHA1")
                         SHAonGH <- if (identical(SHAohGH, SHAonLocal)) FALSE else SHAonGH
                         if (isFALSE(SHAonGH))
                           messageVerbose("Skipping install of ", paste0(acctInner, "/", repoInner, "@", brInner), ", the SHA1 has not changed from last install",
                                          verbose = verbose, verboseLevel = 1)

                       }
                       SHAonGH
                     }
          ))
        pkgGitHub[, SHAonGH := shaOnGitHub]
        pkgGitHub[, localFile := {
          toDL <- pkgGitHub[!SHAonGH %in% FALSE]
          out <- downloadRepo(paste0(toDL$Account, "/", toDL$Repo, "@", toDL$Branch), overwrite = TRUE, destDir = ".", #tmpPath,
                              verbose = verbose)
          fn <- build(Package, verbose = verbose, quiet = FALSE)
          normPath(fn)
        }]
        browser()
        if (!is.null(getOptionRPackageCache()))
          copyTarballsToCache(pkgGitHub$Package, builtBinary = TRUE, unlink = FALSE)
      }
    }
  }
  pkgDTList[["install"]] <- rbindlistRecursive(pkgDTList[["install"]])
  pkgInstall <- pkgDTList[["install"]]
  pkgInstall[, isBinaryInstall := isBinary(pkgInstall$localFile)]

  # The install
  by(pkgInstall, list(pkgInstall$installSafeGroups, !pkgInstall$isBinary), installAll, repos = repos)

  pkgInstall[, installResult := "OK"]
  pkgDT <- rbindlistRecursive(pkgDTList)
}




downloadMRAN <- function(toInstall, install.packagesArgs, verbose, dots) {
  installPkgNames <- toInstall$Package
  names(installPkgNames) <- installPkgNames
  toIn <- toInstall

  earliestDateOnMRAN <- as.Date(gsub(" .*", "", toIn$dayAfterPutOnCRAN))
  latestDateOnMRAN <- pmin(.latestMRANDate, as.Date(gsub(" .*", "", toIn$dayBeforeTakenOffCRAN)))
  onMRANvec <- earliestDateOnMRAN > .earliestMRANDate
  earliestDateOnMRAN[!onMRANvec] <- as.Date(.earliestMRANDate) + 10
  onMRAN <- earliestDateOnMRAN > .earliestMRANDate & unname( isWindows() | isMacOSX() )
  # prevWD <- setwd(tempdir2(.rndstr(1)))
  # on.exit(setwd(prevWD), add = TRUE)

  if (any(onMRAN)) {
    browser()
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
    installVersions <- toInstall$VersionOnRepos
    out <- Map(p = unname(installPkgNames)[onMRAN], earliestDateMRAN = earliestDateOnMRAN[onMRAN],
               lastestDateMRAN = latestDateOnMRAN[onMRAN], tot = total, counter = seq(total),
               v = installVersions[onMRAN], function(p, earliestDateMRAN, lastestDateMRAN, v, tot, counter, ...) {
                 if (tot > 1)
                   messageVerboseCounter(total = tot, verbose = verbose, verboseLevel = 1, counter = counter)

                 browser()
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
                 browser()
                 names(urls) <- p
                 urlsOuter <<- c(urlsOuter, urls)
               })

    ipa <- modifyList2(install.packagesArgs, dots, keep.null = TRUE)
    ipa <- append(ipa, list(repos = NULL, type = "bin"))


    urlsSuccess <- urlsOuter[urlsOuter != "Fail"]
    urlsFail <- urlsOuter[urlsOuter == "Fail"]
    ipa <- modifyList2(list(quiet = !(verbose >= 1)), ipa, keep.null = TRUE)
    ipa <- append(list(pkgs = unname(urlsSuccess)), ipa)
  }
}

secondsInADay <- 3600 * 24



archivedOn <- function(possiblyArchivedPkg, verbose, repos, srcPackageURLOnCRAN, repo, srcContrib, notInArchives) {
  Map(pk = possiblyArchivedPkg, counter = seq(possiblyArchivedPkg), USE.NAMES = TRUE,
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
}
