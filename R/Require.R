utils::globalVariables(c(
  ".", "..colsToKeep", "..colsToKeep2", "..keepCols",
  "Account",  "archiveSource", "AvailableVersion", "bothDepAndOrig", "Branch",
  "compareVersionAvail", "correctVersion", "correctVersionAvail",
  "depOrOrig", "DESCFile", "detached", "download.file", "fullGit", "githubPkgName", "GitSubFolder",
  "hasSubFolder", "hasVersionSpec", "inequality", "installed", "isGH", "isInteractive",
  "LibPath", "libPaths", "loaded", "loadOrder", "needInstall",
  "OlderVersionsAvailable", "OlderVersionsAvailableCh",
  "Package", "packageFullName", "packagesRequired", "PackageUrl", "pkgDepTopoSort",
  "Repo", "repoLocation", "RepoWBranch", "userRequestedOrder", "Version", "versionSpec"
))

#' Repeatability-safe install and load packages, optionally with specific versions
#'
#' This is an "all in one" function that will run `install.packages` for CRAN and
#' GitHub <https://github.com/> packages and will install
#' specific versions of each package if versions are specified either via an (in)equality
#' (e.g., `"glue (>=1.6.2)"` or `"glue (==1.6.2)"` for an exact version) or with a
#' `packageVersionFile`.
#' If `require = TRUE`, the default, the function will then run `require` on all
#' named packages that satisfy their version requirements. If packages are already installed
#' (`packages` supplied), and their optional version numbers are satisfied,
#' then the "install" component will be skipped.
#'
#' `standAlone` will either put the `Require`d packages and their
#' dependencies *all* within the `libPaths` (if `TRUE`) or if
#' `FALSE` will only install packages and their dependencies that are
#' otherwise not installed in `.libPaths()[1]`, i.e., the current active
#' R package directory. Any packages or dependencies that are not yet installed will
#' be installed in `libPaths`.
#'
#' @section GitHub Package:
#' Follows `remotes::install_github` standard.
#' As with `remotes::install_github`, it is not possible to specify a past
#' version of a GitHub package unless that version is a tag or the user passes
#' the SHA that had that package version. Similarly, if a developer does a
#' local install e.g., via `pkgload::install`, of an active project, this package
#' will not be able know of the GitHub state, and thus `pkgSnapshot` will not be able to
#' recover this state as there is no SHA associated with a local
#' installation. Use `Require` (or `remotes::install_github`) to create
#' a record of the GitHub state.
#'
#' @section Package Snapshots:
#' To build a snapshot of the desired packages and their versions,
#' first run `Require` with all packages, then `pkgSnapshot`.
#' If a `libPaths` is used, it must be used in both functions.
#'
#' @section Mutual Dependencies:
#' This function works best if all required packages are called within one
#' `Require` call, as all dependencies can be identified together, and all
#' package versions will be addressed (if there are no conflicts),
#' allowing a call to [pkgSnapshot()] to take a snapshot or "record" of
#' the current collection of packages and versions.
#'
#' @section Local Cache of Packages:
#' When installing new packages, `Require` will put all source and binary files
#' in an R-version specific subfolder of
#' `getOption("Require.RPackageCache")` whose default is `RPackageCache()`, meaning
#' *cache packages locally in a project-independent location*,
#' and will reuse them if needed. To turn
#' off this feature, set `options("Require.RPackageCache" = FALSE)`.
#'
#' @note
#' For advanced use and diagnosis, the user can set `verbose = TRUE` or
#' `1` or `2` (or via `options("Require.verbose")`). This will
#' attach an attribute `attr(obj, "Require")` to the output of this
#' function.
#'
#' @param install Logical or "force". If `FALSE`, this will not try to install anything.
#'   If `"force"`, then it will force installation of requested packages,
#'   mimicking a call to e.g., `install.packages`.
#'   If `TRUE`, the default, then this function will try to install any missing
#'   packages or dependencies.
#' @param require Logical. If `TRUE`, the default, then the function will
#'   attempt to call `require` on all requested `packages`, possibly
#'   after they are installed.
#' @param packages Character vector of packages to install via
#'   `install.packages`, then load (i.e., with `library`). If it is
#'   one package, it can be unquoted (as in `require`). In the case of a
#'   GitHub package, it will be assumed that the name of the repository is the
#'   name of the package. If this is not the case, then pass a named character
#'   vector here, where the names are the package names that could be different
#'   than the GitHub repository name.
#' @param packageVersionFile If provided, then this will override all
#'   `install.package` calls with `versions::install.versions`
#' @param libPaths The library path (or libraries) where all packages should be
#'   installed, and looked for to load (i.e., call `library`). This can be
#'   used to create isolated, stand alone package installations, if used with
#'   `standAlone = TRUE`. Currently, the path supplied here will be
#'   prepended to `.libPaths()` (temporarily during this call) to
#'   `Require` if `standAlone = FALSE` or will set (temporarily)
#'        `.libPaths()` to `c(libPaths, tail(libPaths(), 1)` to keep base packages.
#' @param repos The remote repository (e.g., a CRAN mirror), passed to either
#'              `install.packages`, `install_github` or `installVersions`.
#' @param install_githubArgs List of optional named arguments, passed to `install.packages`
#'   inside `installGitHubPackage`.
#' @param install.packagesArgs List of optional named arguments, passed to `install.packages`.
#' @param standAlone Logical. If `TRUE`, all packages will be installed to and loaded from
#'   the `libPaths` only. NOTE: If `TRUE`, THIS WILL CHANGE THE USER'S `.libPaths()`, similar
#'   to e.g., the `checkpoint` package.
#'   If `FALSE`, then `libPath` will be prepended to `.libPaths()` during the `Require` call,
#'   resulting in shared packages, i.e., it will include the user's default package folder(s).
#'   This can be create dramatically faster installs if the user has a substantial number of
#'   the packages already in their personal library.
#'   Default `FALSE` to minimize package installing.
#' @param purge Logical. Should all caches be purged?
#'   Default is `getOption("Require.purge", FALSE)`.
#'   There is a lot of internal caching of results throughout the `Require` package.
#'   These help with speed and reduce calls to internet sources.
#'   However, sometimes these caches must be purged.
#'   The cached values are renewed when found to be too old, with the age limit.
#'   This maximum age can be set in seconds with the environment variable
#'   `R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE`, or if unset,
#'   defaults to 3600  (one hour -- see [`utils::available.packages`]).
#'
#'   Internally, there are calls to `available.packages`.
#' @param verbose Numeric or logical indicating how verbose should the function be.
#'   If -1 or less, then as little verbosity as possible.
#'   If 0 or FALSE, then minimal outputs; if `1` or TRUE, more outputs; `2` even more.
#' @param ... Passed to `install.packages`.
#'   Good candidates are e.g., `type` or `dependencies`. This can be
#'   used with `install_githubArgs` or `install.packageArgs` which
#'   give individual options for those 2 internal function calls.
#'
#' @export
#' @importFrom data.table data.table as.data.table setDT set is.data.table
#'   rbindlist
#' @importFrom data.table  :=  .I .SD setnames setorderv
#' @importFrom utils available.packages capture.output compareVersion
#' @importFrom utils install.packages packageVersion
#'
#' @examples
#' \dontrun{
#' # simple usage, like conditional install.packages then library
#' library(Require)
#' Require("stats") # analogous to require(stats), but it checks for
#' #   pkg dependencies, and installs them, if missing
#' tempPkgFolder <- file.path(tempdir(), "Packages")
#'
#' # use standAlone, means it will put it in libPaths, even if it already exists
#' #   in another local library (e.g., personal library)
#' Require("crayon", libPaths = tempPkgFolder, standAlone = TRUE)
#'
#' # make a package version snapshot of installed packages
#' packageVersionFile <- "_.packageVersionTest.txt"
#' (pkgSnapshot(libPath = tempPkgFolder, packageVersionFile, standAlone = TRUE))
#'
#' # Restart R -- to remove the old temp folder (it disappears with restarting R)
#' library(Require)
#' tempPkgFolder <- file.path(tempdir(), "Packages")
#' packageVersionFile <- "_.packageVersionTest.txt"
#' # Reinstall and reload the exact version from previous
#' Require(packageVersionFile = packageVersionFile, libPaths = tempPkgFolder, standAlone = TRUE)
#'
#' # Create mismatching versions -- desired version is older than current installed
#' # This will try to install the older version, overwriting the newer version
#' desiredVersion <- data.frame(instPkgs = "crayon", instVers = "1.3.2", stringsAsFactors = FALSE)
#' write.table(file = packageVersionFile, desiredVersion, row.names = FALSE)
#' newTempPkgFolder <- file.path(tempdir(), "Packages2")
#'
#' # Note this will install the 1.3.2 version (older that current on CRAN), but
#' #   because crayon is still loaded in memory, it will return TRUE, using the current version
#' #   of crayon. To start using the older 1.3.2, need to unload or restart R
#' Require("crayon",
#'   packageVersionFile = packageVersionFile,
#'   libPaths = newTempPkgFolder, standAlone = TRUE
#' )
#'
#' # restart R again to get access to older version
#' # run again, this time, correct "older" version installs in place of newer one
#' library(Require)
#' packageVersionFile <- "_.packageVersionTest.txt"
#' newTempPkgFolder <- file.path(tempdir(), "Packages3")
#' Require("crayon",
#'   packageVersionFile = packageVersionFile,
#'   libPaths = newTempPkgFolder, standAlone = TRUE
#' )
#'
#' # Mutual dependencies, only installs once -- e.g., httr
#' tempPkgFolder <- file.path(tempdir(), "Packages")
#' Require(c("cranlogs", "covr"), libPaths = tempPkgFolder, standAlone = TRUE)
#'
#' ##########################################################################################
#' # Isolated projects -- Just use a project folder and pass to libPaths or set .libPaths() #
#' ##########################################################################################
#' # GitHub packages -- restart R because crayon is needed
#' library(Require)
#' ProjectPackageFolder <- file.path(tempdir(), "ProjectA")
#' #  THIS ONE IS LARGE -- > 100 dependencies -- use standAlone = FALSE to
#' #    reuse already installed packages --> this won't allow as much control
#' #    of package versioning
#' Require("PredictiveEcology/SpaDES@development",
#'   libPaths = ProjectPackageFolder, standAlone = FALSE
#' )
#'
#' # To keep totally isolated: use standAlone = TRUE
#' #   --> setting .libPaths() directly means standAlone is not necessary; it will only
#' #   use .libPaths()
#' library(Require)
#' ProjectPackageFolder <- file.path("~", "ProjectA")
#' setLibPaths(ProjectPackageFolder)
#' Require("PredictiveEcology/SpaDES@development") # the latest version on GitHub
#' Require("PredictiveEcology/SpaDES@23002b2a92a92df4ccba7f51cdd82798800b2fa7")
#' # a specific commit (by using the SHA)
#'
#'
#' ############################################################################
#' # Mixing and matching GitHub, CRAN, with and without version numbering
#' ############################################################################
#' # Restart R -- when installing/loading packages, start fresh
#' pkgs <- c(
#'   "glue (<=1.0.4)", "digest (<= 0.6.28)", "glmm (<=1.3.0)",
#'   "achubaty/amc@development", "PredictiveEcology/LandR@development (>=0.0.1)",
#'   "PredictiveEcology/LandR@development (>=0.0.2)", "ianmseddy/LandR.CS (<=0.0.1)"
#' )
#' Require::Require(pkgs)
#'
#' ############################################################################
#' # Using libPaths -- This will only be used inside this function;
#' # To change .libPaths() for the whole session use a manually call to
#' # setLibPaths(newPath) first
#' ############################################################################
#' Require::Require("SpaDES", libPaths = "~/TempLib2", standAlone = FALSE)
#'
#' ############################################################################
#' # Persistent separate packages
#' ############################################################################
#' setLibPaths("~/TempLib2", standAlone = TRUE)
#' Require::Require("SpaDES") # not necessary to specify standAlone here because .libPaths are set
#'
#' ############################################################################
#' # Installing on many machines that are connected by a shared drive
#' ############################################################################
#' options("Require.RPackageCache" = TRUE) # will binaries on the fly.
#' # Put thes in a shared location.
#' # May need to install Require in main user library before setting library paths for project
#' if (!require("Require")) install.packages("Require")
#' setLibPaths("./packages") # not shared location for library path; no longer using main user lib
#' Require::Require(
#'   packageVersionFile = "./packageVersions.txt",
#'   standAlone = TRUE
#' )
#' }
#'
Require <- function(packages, packageVersionFile,
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

  if (verbose == 0 || verbose %in% FALSE) {
    install.packagesArgs <- modifyList2(install.packagesArgs, list(quiet = TRUE))
    install_githubArgs <-  modifyList2(install.packagesArgs, list(quiet = TRUE))
  }
  libPaths <- checkLibPaths(libPaths = libPaths)
  doDeps <- if (!is.null(list(...)$dependencies)) list(...)$dependencies else NA
  allrepos <- c(repos, getOption("repos"))
  allrepos <- allrepos[!(duplicated(names(allrepos)) & duplicated(allrepos))]
  opts <- options(repos = allrepos)

  Ncpus <- getOption("Ncpus")
  if (is.null(Ncpus)) opts <- append(opts, options(Ncpus = 16))
  on.exit({
    options(opts)}
    , add = TRUE)

  origDTThreads <- data.table::setDTthreads(1)
  on.exit(data.table::setDTthreads(origDTThreads), add = TRUE)

  purge <- dealWithCache(purge)
  which <- whichToDILES(doDeps)

  # if (missing(libPaths))
  #   libPaths <- .libPaths()
  suppressMessages({origLibPaths <- setLibPaths(libPaths, standAlone, exact = TRUE)})

  if (!missing(packageVersionFile)) {
    packages <- data.table::fread(packageVersionFile)

    packages <- dealWithViolations(packages)
    packages <- packages[!packages$Package %in% .basePkgs]
    uniqueLibPaths <- unique(packages$LibPath)
    if (length(uniqueLibPaths) > 1) {
      dt <- data.table(
        libPathInSnapshot = uniqueLibPaths,
        newLibPaths = normPath(c(
          libPaths[1],
          file.path(
            libPaths[1],
            gsub(":", "", uniqueLibPaths[-1])
          )
        ))
      )
      messageVerbose(
        "packageVersionFile is covering more than one library; installing packages in reverse order; ",
        "also -- .libPaths() will be altered to be\n",
        verbose = verbose, verboseLevel = 1
      )
      messageDF(dt, verbose = verbose, verboseLevel = 0)

      callArgs <- as.list(match.call())[-1]
      out <- Map(
        lib = rev(dt$libPathInSnapshot),
        newLib = rev(dt$newLibPaths), function(lib, newLib) {
          tf <- tempfile2("RequireSnapshot")
          packages <- packages[packages$LibPath == lib]
          data.table::fwrite(packages, file = tf)
          callArgs[["packageVersionFile"]] <- tf
          callArgs[["libPaths"]] <- newLib
          callArgs[["standAlone"]] <- TRUE
          out <- do.call(Require, args = callArgs)
        }
      )
      out <- unlist(out)
      setLibPaths(dt$newLibPaths, standAlone = TRUE)
      messageVerbose(" to echo the multiple paths in ", packageVersionFile,
                     verbose = verbose, verboseLevel = 1)

      if (isTRUE(require)) {
        return(out)
      } else {
        return(invisible(out))
      }
      packages[, LibPath := .libPaths()[1]]
    }
    if (NROW(packages)) {
      set(packages, NULL, "Package", paste0(packages$Package, " (==", packages$Version, ")"))
    } else {
      character()
    }
    if (any(grep("github", tolower(colnames(packages))))) {
      haveGit <- nchar(packages[["GithubSHA1"]]) > 0
      if (sum(haveGit, na.rm = TRUE)) {
        packages[haveGit, `:=`(Package = paste0(GithubUsername, "/", GithubRepo, "@", GithubSHA1))]
      }
    }
    packages <- packages$Package
    which <- NULL
    install_githubArgs[c("dependencies", "upgrade")] <- list(FALSE, FALSE)
    install.packagesArgs["dependencies"] <- FALSE
    require <- FALSE
    oldEnv <- Sys.getenv("R_REMOTES_UPGRADE")
    Sys.setenv(R_REMOTES_UPGRADE = "never")
    on.exit(
      {
        Sys.setenv("R_REMOTES_UPGRADE" = oldEnv)
      },
      add = TRUE
    )
    messageVerbose("Using ", packageVersionFile, "; setting `require = FALSE`",
                   verbose = verbose, verboseLevel = 1)


    if (NROW(packages)) {

      # Some package names are not derived from their GitHub repo names -- user can supply named packages
      origPackagesHaveNames <- nchar(names(packages)) > 0
      if (any(origPackagesHaveNames)) {
        packages <- packages[order(names(packages), decreasing = TRUE)]
      }
      dups <- duplicated(packages)
      packages <- packages[!dups] # (unique removes names) sometimes people pass identical packages -- only <10s microseconds
      origPackagesHaveNames <- nchar(names(packages)) > 0 # redo -- changed order
      packagesOrig <- packages
      packageNamesOrig <- packages

      if (any(origPackagesHaveNames)) {
        packageNamesOrig[origPackagesHaveNames] <- packagesOrig[origPackagesHaveNames]
      }
      packagesOrder <- seq(packagesOrig)
      names(packagesOrder) <- extractPkgName(packageNamesOrig)

      packagesFullNameOrder <- packagesOrder
      names(packagesFullNameOrder) <- packageNamesOrig


      if (length(which) && (isTRUE(install) || identical(install, "force"))) {
        messageVerbose("Identifying package dependencies...",
                       verbose = verbose, verboseLevel = 1)
        packages <- getPkgDeps(packages, which = which, purge = purge)
      }


      # Create data.table of Require workflow
      if (is(packages, "list")) packages <- unlist(packages, recursive = FALSE)

      pkgDT <- toPkgDT(packages, deepCopy = TRUE)
      # identify the packages that were asked by user to load -- later dependencies will be in table too
      # some cases, original was without version, but due to a dependency that does have a version,
      # it is no longer the same as orig package name
      pkgDT[
        packageFullName %in% unique(packageNamesOrig) | Package %in% unique(packageNamesOrig),
        `:=`(
          packagesRequired = packagesOrder[match(Package, names(packagesOrder))],
          userRequestedOrder = packagesFullNameOrder[match(packageFullName, names(packagesFullNameOrder))])
      ]
      pkgDT[, loadOrder := userRequestedOrder] # this will start out as loadOrder = TRUE, but if install fails, will turn to FALSE

      if (any(origPackagesHaveNames)) {
        pkgDT[
          packageFullName %in% packagesOrig[origPackagesHaveNames],
          Package := names(packagesOrig[origPackagesHaveNames])
        ]
      }

      data.table::setorderv(pkgDT, c("userRequestedOrder"), na.last = TRUE)
      # Join installed with requested
      pkgDT <- installedVers(pkgDT)
      pkgDT <- pkgDT[, .SD[1], by = "packageFullName"] # remove duplicates
      pkgDT[, `:=`(installed = !is.na(Version), loaded = FALSE)]
      if (isTRUE(standAlone)) {
        # Remove any packages that are not in .libPaths()[1], i.e., the main R library
        notInLibPaths1 <- (!pkgDT$Package %in% .basePkgs) &
          (!normPath(pkgDT$LibPath) %in% normPath(.libPaths()[1]))
        if (any(notInLibPaths1))
          pkgDT[notInLibPaths1, installed := FALSE]
      }

      if (length(packages)) {
        if (isTRUE(install) || identical(install, "force")) {
          pkgDT <- parseGitHub(pkgDT, verbose = verbose)
          pkgDT <- getPkgVersions(pkgDT, install = install)
          pkgDT <- getAvailable(pkgDT, purge = purge, repos = repos, verbose = verbose)
          pkgDT <- installFrom(pkgDT, purge = purge, repos = repos)
          pkgDT <- rmDuplicatePkgs(pkgDT, verbose = verbose)
          pkgDT <- pkgDT[Package %in% .basePkgs, needInstall := NA] ## TODO: if number of GitHub pkgs > N; prompt sure to ensure GITHUB_PAT setup
          canusepak <- usepak(packageFullName = pkgDT$packageFullName,
                              needInstall = pkgDT$needInstall,
                              installFrom = pkgDT$repoLocation, toplevel = TRUE)
          if (canusepak) {
            pakOut <- try(installByPak(pkgDT, libPaths, doDeps, ...))
            if (is(pakOut, "try-error"))
              canusepak <- FALSE
          }
          if (!canusepak) {
            pkgDT <- doInstalls(pkgDT,
                                install_githubArgs = install_githubArgs,
                                install.packagesArgs = install.packagesArgs,
                                install = install, repos = repos, verbose = verbose,
                                ...
            )
          }
          if ("detached" %in% colnames(pkgDT)) {
            unloaded <- pkgDT[!is.na(detached)]
            if (NROW(unloaded)) {
              reloaded <- lapply(unloaded[detached == 2]$Package, loadNamespace)
              relibraried <- lapply(unloaded[detached == 3]$Package, require, character.only = TRUE)
              messageVerbose("Attempting to reload namespaces that were detached: ",
                             paste(unloaded[detached == 2]$Package, collapse = ", "),
                             verbose = verbose, verboseLevel = 1)
              messageVerbose("Attempting to reattach to the search path: ",
                             paste(unloaded[detached == 2]$Package, collapse = ", "),
                             verbose = verbose, verboseLevel = 1)

            }
          }
        }
        if (isTRUE(require)) {
          pkgDT <- doLoading(pkgDT, ...)
        }
      }
      out <- pkgDT[packagesRequired > 0]$loaded
      # outOrder <- pkgDT[packagesRequired > 0]$packagesRequired
      names(out) <- pkgDT[packagesRequired > 0]$packageFullName

      # put back in original order
      outOrder <- match(pkgDT[packagesRequired > 0]$packageFullName, packagesOrig)
      orderNAs <- is.na(outOrder)
      if (any(orderNAs)) {
        outOrder[orderNAs] <- match(pkgDT[packagesRequired > 0][orderNAs]$Package, packagesOrig)
      }
      out <- out[order(outOrder)]

      if (verbose > 0) {
        if (verbose < 2) {
          colsToKeep <- c(
            "packageFullName", "Package", "installed", "loadOrder", "loaded", "installFrom",
            "Version", "repoLocation", "correctVersion", "hasVersionSpec",
            "correctVersionAvail", "installResult"
          )
          colsToKeep <- intersect(colsToKeep, colnames(pkgDT))
          pkgDT <- pkgDT[, ..colsToKeep]
        }

      }
      attr(out, "Require") <- pkgDT[]

      stillNeeded <- if (!is.null(pkgDT$installResult)) {
        if (any(grep("No available", pkgDT$installResult))) {
          pkgDT[installed == FALSE, list(Package, packageFullName, installResult)]
        } else {
          pkgDT[0]
        }
      }
      if (NROW(stillNeeded)) {
        messageVerbose(
          "Several packages are not on CRAN, its archives (for this OS), or don't have GitHub tracking ",
          "information and thus will not be installed. ",
          "These may have been installed locally from source, or are on another ",
          "repository system, such as BioConductor:",
          verbose = verbose, verboseLevel = 1
        )
        messageDF(stillNeeded[, list(Package, packageFullName, installResult)],
                  verbose = verbose, verboseLevel = 1)
      }
    }
    notCorrectly <- pkgDT$installed == FALSE & pkgDT$needInstall == TRUE
    if (isTRUE(any(notCorrectly))) {
      messageVerbose("The following packages did not get installed correctly.",
                     verbose = verbose, verboseLevel = 0)
      colsToKeep2 <- c(
        "packageFullName", "Package", "LibPath", "Version",
        "repoLocation", "installFrom", "installResult"
      )
      messageDF(pkgDT[notCorrectly == TRUE, ..colsToKeep2],
                verbose = verbose, verboseLevel = 0)
      nonZeroExit <- grepl("had non-zero", pkgDT[notCorrectly == TRUE, ..colsToKeep2]$installResult)
      if (any(nonZeroExit)) {
        nonZ <- pkgDT[notCorrectly == TRUE, ..colsToKeep2]
        messageVerbose(
          "It may be necessary to simply run:\ninstall.packages(c('", paste(nonZ$Package, collapse = "', '"), "'))",
          "\nbut this will cause a different version to be installed.",
          verbose = verbose, verboseLevel = 0
        )
        if (!missing(packageVersionFile)) {
          messageVerbose("If packages are installed as per above, you may wish to rerun pkgSnapshot('",
                         packageVersionFile, "') to update with the new version",
                         verbose = verbose, verboseLevel = 0)
        }
      }
    } else {
      if (!is.null(pkgDT$needInstall)) {
        nas <- is.na(pkgDT$needInstall)
        if (!all(nas)) {
          allCorrect <- pkgDT$needInstall[!nas] == TRUE & pkgDT$installed[!nas] == FALSE
          if (length(allCorrect)) {
            allInstalled <- pkgDT[!nas][allCorrect]
            if (NROW(allInstalled) == 0) {
              messageVerbose("All packages appear to have installed correctly",
                             verbose = verbose, verboseLevel = 0)
            }
          }
        }
      }
    }
  } else {
    out <- logical()
  }
  #}


  if (isTRUE(require)) {
    return(out)
  } else {
    return(invisible(out))
  }
}

usepak <- function(packageFullName, needInstall, installFrom = NULL, toplevel = FALSE,
                   verbose = getOption("Require.verbose")) {

  wantpak <- isTRUE(getOption("Require.usepak", FALSE))
  if (!is.null(installFrom) && wantpak) {
    wantpak <- all(installFrom[needInstall %in% TRUE] %in% c("GitHub", "CRAN"))
    return(wantpak)
  }

  if (!missing(packageFullName)) { # would occur when using packageVersionFile
    hasVersionNumberSpec <- !identical(trimVersionNumber(packageFullName), packageFullName)
    wantpak <- isTRUE(getOption("Require.usepak", FALSE))
  } else {
    wantpak <- FALSE
    hasVersionNumberSpec <- FALSE
  }
  if (isTRUE(wantpak)) {
    if (!requireNamespace("pak"))
      wantpak <- FALSE
  }

  if (wantpak && hasVersionNumberSpec) {
    wantpak <- FALSE
    if (isTRUE(toplevel)) {
      messageVerbose("Using hybrid install via both *pak::pkg_install* and *Require*",
                     "because version numbers are specified",
                     verbose = verbose, verboseLevel = 0)
    }

  }


  wantpak
}
