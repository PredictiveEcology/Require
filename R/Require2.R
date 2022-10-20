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
#' @param require Logical or character string. If `TRUE`, the default, then the function will
#'   attempt to call `require` on all requested `packages`, possibly
#'   after they are installed. If a character string, then it will only call `require`
#'   on those specific packages (i.e., it will install the ones listed in `packages`, but
#'   load the packages listed in `require`)
#' @param packages Character vector of packages to install via
#'   `install.packages`, then load (i.e., with `library`). If it is
#'   one package, it can be unquoted (as in `require`). In the case of a
#'   GitHub package, it will be assumed that the name of the repository is the
#'   name of the package. If this is not the case, then pass a named character
#'   vector here, where the names are the package names that could be different
#'   than the GitHub repository name.
#' @param packageVersionFile  Character string of a file name or logical. If `TRUE`,
#'   then this function will load the default file, `getOption("Require.packageVersionFile").
#'   If this argument is provided, then this will override all any packages passed to `packages`.
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
#'   NOTE: in `Require` function, when `verbose >= 2`, the return object will have an attribute:
#'   `attr(.., "Require")` which has lots of information about the processes of the installs.
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
  messageVerbose(verbose = verbose, verboseLevel = 1, "\033[33mUsing New Require\033[39m")
  .pkgEnv$hasGHP <- NULL # clear GITHUB_PAT message; only once per Require session

  dots <- list(...)
  install.packagesArgs <- modifyList2(list(quiet = !(verbose >= 1)), install.packagesArgs,
                                      dots, keep.null = TRUE)
  install_githubArgs <-  modifyList2(list(quiet = !(verbose >= 0)), install_githubArgs,
                                     dots, keep.null = TRUE)
  install.packagesArgs <- modifyList2(install.packagesArgs,
                                      list(destdir = ".", repos = repos, type = types()))

  if (missing(libPaths))
    libPaths <- .libPaths()
  libPaths <- checkLibPaths(libPaths = libPaths)
  suppressMessages({origLibPaths <- setLibPaths(libPaths, standAlone, exact = TRUE)})

  if (!missing(packageVersionFile) ) {
    pkgSnapshotOut <- doPkgSnapshot(packageVersionFile, verbose, purge, libPaths,
                         install_githubArgs, install.packagesArgs, standAlone)
    messageVerbose(NoPkgsSupplied, verbose = verbose, verboseLevel = 1)
    return(pkgSnapshotOut)
  }
  if (missing(packages)) {
    messageVerbose(NoPkgsSupplied, verbose = verbose, verboseLevel = 1)
    return(invisible(NULL))
  }

  if (NROW(packages)) {
    deps <- pkgDep(packages)
    allPackages <- unname(unlist(deps))
    pkgDT <- toPkgDT(allPackages, deepCopy = TRUE)
    pkgDT <- updatePackagesWithNames(pkgDT, packages)
    pkgDT <- recordLoadOrder(packages, pkgDT)
    pkgDT <- parseGitHub(pkgDT)
    pkgDT <- removeDups(pkgDT)
    pkgDT <- installedVers(pkgDT)
    pkgDT <- dealWithStandAlone(pkgDT, standAlone)
    pkgDT <- whichToInstall(pkgDT, install)
    if ((any(pkgDT$needInstall %in% "install") && (isTRUE(install))) || install %in% "force") {
      pkgDT <- doInstalls2(pkgDT, repos = repos, purge = purge, libPaths = libPaths, verbose = verbose,
                           install.packagesArgs = install.packagesArgs)
    }
    out <- doLoads(require, pkgDT)

    if (verbose >= 2)
      attr(out, "Require") <- pkgDT[]
  } else {
    out <- logical()
  }
  out
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


installAll <- function(toInstall, repos = getOptions("repos"), install.packagesArgs,
                       numPackages, numGroups, startTime, verbose) {

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


  type <- unique(c("source", "binary")[toInstall$isBinaryInstall + 1])
  if (identical(type, "source") && (isWindows() || isMacOSX())) {
    install.packagesArgs$INSTALL_opts <- unique(c(install.packagesArgs$INSTALL_opts, "--build"))
  }

  ipa <- modifyList2(install.packagesArgs,
                     list(pkgs = basename(toInstall$localFile), repos = NULL, type = type, dependencies = FALSE),
                    keep.null = TRUE)
  do.call(install.packages, ipa)
}

doInstalls2 <- function(pkgDT, repos, purge, tmpdir, libPaths, verbose, install.packagesArgs) {

  tmpdir <- if (is.null(getOptionRPackageCache())) tempdir2(.rndstr(1)) else getOptionRPackageCache()
  origGetwd <- getwd()
  on.exit(setwd(origGetwd))
  out <- setwd(tmpdir)

  pkgDTList <- split(pkgDT, by = c("needInstall"))
  pkgInstall <- pkgDTList[["install"]] # pointer

  pkgInstall <- doDownloads(pkgInstall, repos, purge, verbose, install.packagesArgs, libPaths)
  pkgDTList[["install"]] <- pkgInstall
  pkgInstallList <- split(pkgInstall, by = "needInstall") # There are now ones that can't be installed b/c noneAvailable
  pkgInstall <- pkgInstallList[["install"]]
  if (!is.null(pkgInstallList[[noneAvailable]]))
    messageVerbose("\033[36m",
                   paste(unique(pkgInstallList[[noneAvailable]]$packageFullName), collapse = ", "),
                   " could not be installed; the version specification cannot be met\033[39m",
                   verbose = verbose, verboseLevel = 1)
  if (!is.null(pkgInstall)) {
    pkgInstall[, isBinaryInstall := isBinary(pkgInstall$localFile)]

    startTime <- Sys.time()

    # The install
    set(pkgInstall, NULL, "installSafeGroups", as.integer(factor(paste0(pkgInstall[["installSafeGroups"]], "_",
                                                                        !pkgInstall[["isBinaryInstall"]]))))
    data.table::setorderv(pkgInstall, c("installSafeGroups", "Package")) # alphabetical order
    maxGroup <- max(pkgInstall[["installSafeGroups"]])
    numPackages <- NROW(pkgInstall)
    pkgInstall[, installOrder := seq(.N)]

    by(pkgInstall, list(pkgInstall[["installSafeGroups"]]),
       installAll, repos = repos, install.packagesArgs, numPackages,
       numGroups = maxGroup, startTime, verbose)

    pkgInstall[, installResult := "OK"]
  }
  pkgDT <- rbindlistRecursive(pkgDTList)
}




downloadMRAN <- function(toInstall, install.packagesArgs, verbose) {
  installPkgNames <- toInstall$Package
  names(installPkgNames) <- installPkgNames
  toIn <- toInstall

  earliestDateOnMRAN <- as.Date(gsub(" .*", "", toIn$dayAfterPutOnCRAN))
  latestDateOnMRAN <- pmin(.latestMRANDate, as.Date(gsub(" .*", "", toIn$dayBeforeTakenOffCRAN)))
  onMRANvec <- earliestDateOnMRAN > .earliestMRANDate
  earliestDateOnMRAN[!onMRANvec] <- as.Date(.earliestMRANDate) + 10
  onMRAN <- earliestDateOnMRAN > .earliestMRANDate & unname( isWindows() | isMacOSX() )
  onMRAN[is.na(onMRAN)] <- FALSE

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
    installVersions <- toInstall$VersionOnRepos
    out <- Map(p = unname(installPkgNames)[onMRAN], earliestDateMRAN = earliestDateOnMRAN[onMRAN],
               latestDateMRAN = latestDateOnMRAN[onMRAN], tot = total, counter = seq(total),
               v = installVersions[onMRAN], function(p, earliestDateMRAN, latestDateMRAN, v, tot, counter, ...) {
                 if (tot > 1)
                   messageVerboseCounter(total = tot, verbose = verbose, verboseLevel = 1, counter = counter)

                 for (attempt in 0:15 ) { # Try up to 15 days from known earliestDateMRAN or latestDateMRAN of the package being available on CRAN
                   rver <- rversion()
                   evenOrOdd <- attempt %% 2 == 0
                   date <- if (evenOrOdd) latestDateMRAN else  earliestDateMRAN
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

    urlsSuccess <- urlsOuter[urlsOuter != "Fail"]
    urlsFail <- urlsOuter[urlsOuter == "Fail"]

    toInstall[match(names(urlsSuccess), Package), `:=`(PackageUrl = urlsSuccess,
                                                       repoLocation = "MRAN",
                                                       localFile = basename(urlsSuccess))]
    if (length(urlsFail)) {
      cantGet <- toInstall$packageFullName[toInstall$Package %in% names(urlsFail)]
      messageVerbose("Could not find a binary version of ", paste(cantGet, collapse = ", "), " on MRAN; ",
                     "trying source archives", verbose = verbose, verboseLevel = 1)
    }
    if (sum(toInstall$repoLocation %in% "MRAN"))
      toInstall[repoLocation %in% "MRAN", {
        ipa <- modifyList2(list(url = PackageUrl, destfile = localFile), install.packagesArgs)
        do.call(download.file, ipa)
      }]
  }

  toInstall
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

        } else {
          publishedDate <- grep("Published", rl)
          if (length(publishedDate)) {
            theDateGrep <- "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}"
            theDate <- grep(theDateGrep, rl[seq(publishedDate, publishedDate + 2)], value = TRUE)
            publishedDate <- gsub(paste0(".+(",theDateGrep,").+"), "\\1", theDate)
          }

          archivedOn <- publishedDate
          PackageUrl <- ""#file.path(repos, srcContrib, )
        }
        archivedOn <- as.character(as.POSIXct(archivedOn) - 3600 * 24)
        list(PackageUrl = PackageUrl, archivedOn = archivedOn)
      })
}

whichToInstall <- function(pkgDT, install) {
  pkgDT[, versionSpec := extractVersionNumber(packageFullName)]
  setorderv(pkgDT, c("Package", "versionSpec"), na.last = TRUE)
  pkgDT[, keep := if (any(!is.na(versionSpec))) .I[1] else .I, by = "Package"]
  pkgDT <- pkgDT[unique(pkgDT$keep)]
  set(pkgDT, NULL, "keep", NULL)
  set(pkgDT, NULL, "isPkgInstalled", !is.na(pkgDT$Version))
  pkgDT[!is.na(versionSpec), inequality := extractInequality(packageFullName)]
  set(pkgDT, NULL, "installedVersionOK", !is.na(pkgDT$Version))
  set(pkgDT, NULL, "hasVersionsToCompare",
      (nchar(pkgDT$inequality) > 0) %in% TRUE & !is.na(pkgDT$Version))
  if (any(pkgDT$hasVersionsToCompare %in% TRUE))
    pkgDT[hasVersionsToCompare %in% TRUE, installedVersionOK :=
            compareVersion2(Version, versionSpec, inequality)
            # do.call(inequality, list(package_version(Version), versionSpec))
          , by = seq(sum(hasVersionsToCompare))]
  if (identical(install, "force"))
    set(pkgDT, NULL, "needInstall", "install")
  else
    set(pkgDT, NULL, "needInstall", c("dontInstall", "install")[pkgDT$installedVersionOK %in% FALSE + 1])
  pkgDT
}


doLoads <- function(require, pkgDT) {
  needRequire <- require
  if (is.character(require)) {
    pkgDT[Package %in% require, require := TRUE]
  } else if (isTRUE(require)) {
    pkgDT[!is.na(loadOrder), require := TRUE]
  } else if (isFALSE(require)) {
    pkgDT[!is.na(loadOrder), require := FALSE]
  }

  # override if version was not OK
  if (any(pkgDT$require %in% TRUE)) {
    pkgDT[, require := installedVersionOK %in% TRUE]
    if (!is.null(pkgDT$availableVersionOK))
      pkgDT[!installedVersionOK %in% TRUE, require := availableVersionOK %in% TRUE]
  }

  out <- list()
  if (any(pkgDT$require %in% TRUE)) {
    setorderv(pkgDT, "loadOrder")
    # rstudio intercepts `require` and doesn't work internally
    out[[1]] <- mapply(x = pkgDT$Package[pkgDT$require %in% TRUE], function(x) base::require(x, character.only = TRUE), USE.NAMES = TRUE)
  }

  if (any(pkgDT$require %in% FALSE)) {
    out[[2]] <- mapply(x = pkgDT$Package[pkgDT$require %in% FALSE], function(x) FALSE, USE.NAMES = TRUE)
  }
  out <- do.call(c, out)
  out[na.omit(pkgDT$Package[pkgDT$loadOrder])] # put in order, based on loadOrder

}

recordLoadOrder <- function(packages, pkgDT) {
  pkgDT[packageFullName %in% packages, loadOrder := seq_along(packages)]
  pkgDT
}

removeDups <- function(pkgDT)
  pkgDT[!duplicated(pkgDT[["packageFullName"]])]

dealWithStandAlone <- function(pkgDT, standAlone) {
  if (isTRUE(standAlone)) {
    # Remove any packages that are not in .libPaths()[1], i.e., the main R library
    notInLibPaths1 <- (!pkgDT$Package %in% .basePkgs) &
      (!normPath(pkgDT$LibPath) %in% normPath(.libPaths()[1]))
    if (any(notInLibPaths1))
      pkgDT[notInLibPaths1, `:=`(
        installed = FALSE,
        LibPath = NA_character_,
        Version = NA_character_)]
  }
  pkgDT
}


doDownloads <- function(pkgInstall, repos, purge, verbose, install.packagesArgs,  libPaths) {

  topoSorted <- pkgDepTopoSort(pkgInstall[["packageFullName"]])
  installSafeGroups <- attr(topoSorted, "installSafeGroups")
  correctOrder <- match(names(topoSorted), pkgInstall[["packageFullName"]])
  pkgInstall <- pkgInstall[correctOrder, ]
  set(pkgInstall, NULL, "installSafeGroups", unname(unlist(installSafeGroups)))

  # this is a placeholder; set noLocal by default
  set(pkgInstall, NULL, "haveLocal", "noLocal")

  # check local cache
  pkgInstall <- identifyLocalDownloads(pkgInstall, repos, purge)

  pkgInstallList <- split(pkgInstall, by = "haveLocal")
  pkgNeedInternet <- pkgInstallList[["noLocal"]] # pointer
  if (NROW(pkgNeedInternet)) {
    pkgNeedInternet <- split(pkgNeedInternet, by = "repoLocation")

    # CRAN
    pkgNeedInternet <- downloadCRAN(pkgNeedInternet, repos, purge, install.packagesArgs)

    # Archive
    pkgNeedInternet <- downloadArchive(pkgNeedInternet, repos, verbose, install.packagesArgs)

    # GitHub
    pkgNeedInternet <- downloadGitHub(pkgNeedInternet, libPaths, verbose, install.packagesArgs)
    pkgInstallList[["noLocal"]] <- pkgNeedInternet # pointer
  }

  pkgInstall <- rbindlistRecursive(pkgInstallList)
  pkgInstall[nchar(localFile) == 0, needInstall := noneAvailable]
  pkgInstall
}

identifyLocalDownloads <- function(pkgInstall, repos, purge) {
  if (!is.null(getOptionRPackageCache())) {
    ap <- available.packagesCached(repos = repos, purge = purge)[, ..apCachedCols]
    setnames(ap, old = "Version", new = "VersionOnRepos")
    pkgInstall <- ap[pkgInstall, on = "Package"]
    N <- pkgInstall[, .N, by = Package]
    if (any(N$N > 1)) { # this would be binary and source; binary is first in alphabetical
      pkgInstall <- pkgInstall[, .SD[1], by = "Package"]
    }


    localFiles <- dir(getOptionRPackageCache(), full.names = FALSE)
    origFiles <- filenameOK(pkgInstall$Package, pkgInstall$versionSpec, pkgInstall$inequality, pkgInstall$VersionOnRepos,
                            localFiles = localFiles)
    pkgInstall[, localFile := origFiles]
    pkgInstall[, haveLocal :=
                 unlist(lapply(origFiles, function(x) c("noLocal", "Local")[isTRUE(nchar(x) > 0) + 1]))]
    pkgInstall[haveLocal %in% "Local", `:=`(installFrom = haveLocal,
                                            availableVersionOK = TRUE)]
  }
  pkgInstall
}


# CRAN
downloadCRAN <- function(pkgNoLocal, repos, purge, install.packagesArgs) {
  pkgCRAN <- pkgNoLocal[["CRAN"]]
  if (NROW(pkgCRAN)) { # CRAN, Archive, MRAN
    if (!all(apCachedCols %in% colnames(pkgCRAN))) {
      ap <- available.packagesCached(repos = repos, purge = purge)[, ..apCachedCols]
      setnames(ap, old = "Version", new = "VersionOnRepos")
      pkgNoLocal[["CRAN"]] <- ap[pkgCRAN, on = "Package"]
      pkgCRAN <- pkgNoLocal[["CRAN"]] # pointer
    }
    N <- pkgCRAN[, .N, by = Package]
    if (any(N$N > 1)) {
      pkgCRAN <- pkgCRAN[, .SD[1], by = "Package"]
    }
    pkgCRAN <- availableVersionOK(pkgCRAN)

    # Not on CRAN; so likely Archive
    if (any(pkgCRAN$availableVersionOK %in% FALSE)) {
      pkgCRAN[availableVersionOK %in% FALSE, repoLocation := "Archive"]
      pkgNoLocal[["CRAN"]] <- pkgCRAN
      pkgNoLocal <- rbindlistRecursive(pkgNoLocal)
      pkgNoLocal <- split(pkgNoLocal, by = "repoLocation")
      pkgCRAN <- pkgNoLocal[["CRAN"]]
    }
    if (NROW(pkgCRAN)) {
      pkgCRAN[availableVersionOK %in% TRUE, installFrom := "CRAN"]
      ipa <- modifyList2(list(pkgs = pkgCRAN$Package), install.packagesArgs)
      pkgCRAN[, localFile := basename(do.call(download.packages, ipa)[,2])]
    }
  }
  pkgNoLocal # pkgCRAN is already in this because it was a pointer
}

downloadArchive <- function(pkgNonLocal, repos, verbose, install.packagesArgs) {
  pkgArchive <- pkgNonLocal[["Archive"]]
  if (NROW(pkgArchive)) {
    ava <- lapply(archiveVersionsAvailable(pkgArchive$Package[pkgArchive$repoLocation %in% "Archive"],
                                           repos = repos), function(d) {
                                             aa <- as.data.table(d, keep.rownames = "PackageUrl")
                                             if (!is.null(aa[["mtime"]])) setorderv(aa, "mtime")
                                             aa
                                           })
    cols <- c("PackageUrl", "dayAfterPutOnCRAN", "dayBeforeTakenOffCRAN", "repo", "VersionOnRepos", "availableVersionOK")
    pkgArchive[, c("PackageUrl", "dayAfterPutOnCRAN", "dayBeforeTakenOffCRAN", "repo", "VersionOnRepos", "availableVersionOK") := {
      Version2 <-  gsub(".*_(.*)\\.tar\\.gz", "\\1", ava[[Package]]$PackageUrl)
      if (is.na(versionSpec)) {
        correctVersions <- NROW(ava[[Package]])
      } else {
        # correctVersions <- do.call(inequality, list(package_version(Version2), versionSpec))
        correctVersions <- compareVersion2(Version2, versionSpec, inequality)

        if (all(correctVersions %in% FALSE))
          correctVersions <- NA
        else {
          latestCorrect <- tail(which(correctVersions), 1)
          correctVersions <- unique(c(latestCorrect, max(latestCorrect, length(correctVersions))))
        }

      }
      if (any(!is.na(correctVersions))) { # nothing on Archive that will fulfill the Version requirements
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
      } else {
        ret <- mapply(x = cols, USE.NAMES = TRUE, function(x) NA_character_, SIMPLIFY = FALSE)
        ret <- as.data.table(ret)
        ret[, repo := ava[[Package]][1][, c("repo")]]
        set(ret, NULL, "availableVersionOK", FALSE)
      }
      data.table::setcolorder(ret, cols)
      ret
    }, by = "Package"]
    # Check MRAN
    pkgArchive <- downloadMRAN(pkgArchive, install.packagesArgs, verbose)

    if (any(pkgArchive$repoLocation %in% "Archive" & pkgArchive$availableVersionOK %in% TRUE)) {
      pkgArchive <- split(pkgArchive, pkgArchive$repoLocation)
      pkgArchOnly <- pkgArchive[["Archive"]]
      pkgArchOnly[, PackageUrl := file.path(repo, srcContrib, "Archive", PackageUrl)]
      pkgArchOnly[, localFile := basename(PackageUrl)]
      pkgArchOnly[, {
        ipa <- modifyList2(list(url = PackageUrl, destfile = localFile), install.packagesArgs)
        do.call(download.file, ipa)
      }, by = seq(NROW(pkgArchOnly))]
    }
    pkgArchive <- rbindlistRecursive(pkgArchive)
  }
  pkgNonLocal[["Archive"]] <- pkgArchive
  pkgNonLocal
}

downloadGitHub <- function(pkgNoLocal, libPaths, verbose, install.packagesArgs) {
  pkgGitHub <- pkgNoLocal[["GitHub"]]
  if (NROW(pkgGitHub)) { # GitHub
    pkgGitHub <- getGitHubFile(pkgGitHub)
    pkgGitHub <- availableVersionOK(pkgGitHub)
    if (any(pkgGitHub$availableVersionOK)) {
      pkgGHList <- split(pkgGitHub, pkgGitHub$availableVersionOK)
      pkgGHtoDL <- pkgGHList[["TRUE"]]
      if (any(!pkgGHtoDL$isPkgInstalled)) {
        shaOnGitHub <-
          unlist(Map(repoInner = pkgGHtoDL$Repo, acctInner = pkgGHtoDL$Account,
                     brInner = pkgGHtoDL$Branch,
                     function(repoInner, acctInner, brInner) {
                       alreadyExistingDESCRIPTIONFile <- file.path(libPaths[1], repoInner, "DESCRIPTION")
                       SHAonGH <- getSHAfromGitHub(repo = repoInner, acct = acctInner, br = brInner)
                       if (file.exists(alreadyExistingDESCRIPTIONFile)) {
                         SHAonLocal <- DESCRIPTIONFileOtherV(alreadyExistingDESCRIPTIONFile, other = "GithubSHA1")
                         SHAonGH <- if (identical(SHAonGH, SHAonLocal)) FALSE else SHAonGH
                         if (isFALSE(SHAonGH))
                           messageVerbose("Skipping install of ", paste0(acctInner, "/", repoInner, "@", brInner),
                                          ", the SHA1 has not changed from last install",
                                          verbose = verbose, verboseLevel = 1)

                       }
                       SHAonGH
                     }
          ))
        pkgGHtoDL[, SHAonGH := shaOnGitHub]
        pkgGHtoDL[, localFile := {
          toDL <- .SD[!SHAonGH %in% FALSE]
          gitRepo <- paste0(toDL$Account, "/", toDL$Repo, "@", toDL$Branch)
          names(gitRepo) <- toDL$Package
          out <- downloadRepo(gitRepo, subFolder = toDL$GitSubFolder, overwrite = TRUE, destDir = ".", verbose = verbose)
          fn <- build(Package, verbose = verbose, quiet = FALSE)
          normPath(fn)
        }, by = seq(NROW(pkgGHtoDL))]
      }
      pkgGitHub <- rbindlistRecursive(pkgGHList)
    }
  }
  pkgNoLocal[["GitHub"]] <- pkgGitHub
  pkgNoLocal
}

types <- function(length = 1L) {
  isOldMac <- isMacOSX() && compareVersion(as.character(getRversion()), "4.0.0") < 0
  types <- if (isOldMac) {
    c("mac.binary.el-capitan", "source")
  } else if (!isWindows() && !isMacOSX()) {
    c("source")
  } else {
    c("binary", "source")
  }
  if (identical(length, 1L))
    types <- types[1]
}


doPkgSnapshot <- function(packageVersionFile, verbose, purge, libPaths,
                          install_githubArgs, install.packagesArgs, standAlone = TRUE) {
  if (!isFALSE(packageVersionFile)) {
    if (isTRUE(packageVersionFile)) {
      packageVersionFile <- getOption("Require.packageVersionFile")
    }
    packages <- data.table::fread(packageVersionFile)
    packages <- dealWithViolations(packages, verbose = verbose, purge = purge) # i.e., packages that can't coexist
    packages <- packages[!packages$Package %in% .basePkgs]
    packagesForRequire <- packageFullName(packages)
    out <- Require(packagesForRequire, verbose = verbose, purge = purge, libPaths = libPaths,
                   install_githubArgs = install_githubArgs, install.packagesArgs = install.packagesArgs,
                   standAlone = standAlone, require = FALSE, install = TRUE)
  } else {
    out <- FALSE
  }
  out
}

apCachedCols <- c("Package", "Repository", "Version")


filenameOK <- function(Package, versionSpec, inequality, VersionOnRepos, localFiles) {
  mapply(pat = Package, ver = versionSpec, ineq = inequality, verAv = VersionOnRepos,
         function(pat, ver, ineq, verAv) {
           pat <- paste0(".*", pat, "_")
           fn <- grep(pattern = pat, x = localFiles, value = TRUE)

           if (length(fn)) {
             if (any(isBinary(fn))) {
               fn <- fn[isBinary(fn)]
             }
             localVer <- extractVersionNumber(filenames = fn) # there may be >1 file for a given package; take
             if (is.na(ineq)) { # need max --> which will be the ver; so see if localVer is same as ver
               ord <- order(c(package_version(tail(localVer, 1)), package_version(verAv)), decreasing = TRUE) # local first
               keepLoc <- (ord[1] == 1) # if it is first, then it is bigger or equal, so keep
               fn <- tail(fn, 1)[keepLoc]
             } else {
               keepLoc <- compareVersion2(localVer, ver, ineq)
               # keepLoc <- do.call(ineq, list(package_version(localVer), ver)) # can be length > 1
               if (!identical(ineq, "==")) {
                 keepRep <- compareVersion2(verAv, ver, ineq)
                 # keepRep <- do.call(ineq, list(package_version(verAv), ver))
                 if (any(keepLoc %in% TRUE)) { # local has at least 1 that is good
                   if (any(keepRep %in% TRUE)) { # remote has as at least 1 that is good
                     ord <- order(c(package_version(tail(localVer[keepLoc], 1)), package_version(verAv)), decreasing = TRUE) # local first
                     keepLoc <- (ord[1] == 1) # if it is first, then it is bigger or equal, so keep
                     fn <- tail(fn, 1)[keepLoc] # keepLoc is guaranteed to be length 1
                   } else {
                     fn <- tail(fn[keepLoc], 1) # keepLoc is possibly length > 1
                   }
                 } else {
                   fn <- ""
                 }

               } else {
                 fn <- tail(fn[keepLoc], 1) # keepLoc is possibly length > 1
               }

             }
           } else {
             fn <- ""
           }
           fn
         },
         USE.NAMES = TRUE)
}



availableVersionOK <- function(pkgDT) {
  tmpOrder <- "tmpOrder"
  set(pkgDT, NULL, tmpOrder, seq(NROW(pkgDT)))
  # First set all to availableVersionOK if there is a version available
  pkgDT[, availableVersionOK := !is.na(VersionOnRepos)]
  # Then update this for the subset that have an actual inequality
  pkgDT[!is.na(inequality), availableVersionOK := {
    compareVersion2(VersionOnRepos, versionSpec, inequality)
    # do.call(inequality, list(package_version(VersionOnRepos), versionSpec))
  }, by = tmpOrder]
  set(pkgDT, NULL, tmpOrder, NULL)
}

compareVersion2 <- function(version, versionSpec, inequality) {
  do.call(inequality, list(package_version(version), versionSpec))
}

noneAvailable <- "noneAvailable"

updatePackagesWithNames <- function(pkgDT, packages) {
  origPackagesHaveNames <- nchar(names(packages)) > 0
  if (any(origPackagesHaveNames))
    pkgDT[match(packages[origPackagesHaveNames], packageFullName),
          Package := names(packages[origPackagesHaveNames])]
  pkgDT
}
