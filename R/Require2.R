utils::globalVariables(c(
  "..apCachedCols", "..cols", "..cols3", ".GRP",
  "Account", "archiveSource", "availableVersionOKthisOne", "binOrSrc", "bothDepAndOrig",
  "Branch", "comp", "contrib.url", "correctVersionAvail", "depOrOrig",
  "DESCFile", "EqualsDoesntViolate", "getOptions", "GitSubFolder",
  "hasAtLeastOneNonNA", "hasEqualsAndInequals", "hasSubFolder", "hasVersionsToCompare",
  "haveLocal", "ineq", "installed", "installedVersionOK", "installSafeGroups", "isBinaryInstall",
  "isEquals", "keepBasedOnRedundantInequalities", "libPaths",
  "loaded", "loadOrder", "localFile", "needInstall", "NoPkgsSupplied",
  "OlderVersionsAvailable", "OlderVersionsAvailableCh", "PackageUrl",
  "repo", "Repo", "Repository", "SHAonGH", "verbose", "Version",
  "VersionOnRepos", "versionSpec", "versionToKeep", "violation"
))


#' Repeatability-safe install and load packages, optionally with specific
#' versions
#'
#' This is an "all in one" function that will run `install.packages` for CRAN
#' and GitHub <https://github.com/> packages and will install specific versions
#' of each package if versions are specified either via an (in)equality (e.g.,
#' `"glue (>=1.6.2)"` or `"glue (==1.6.2)"` for an exact version) or with a
#' `packageVersionFile`. If `require = TRUE`, the default, the function will
#' then run `require` on all named packages that satisfy their version
#' requirements. If packages are already installed (`packages` supplied), and
#' their optional version numbers are satisfied, then the "install" component
#' will be skipped.
#'
#' @return `Require` is intended to replace `base::require`, thus it returns a
#' logical, named vector indicating whether the named packages have been loaded.
#' Because `Require` also has the ability to install packages, a return value of
#' `FALSE` does not mean that it did not install correctly; rather, it means it
#' did not attach with `require`, which could be because it did not install
#' correctly, or also because e.g., `require = FALSE`.
#'
#' `standAlone` will either put the `Require`d packages and their dependencies
#' *all* within the `libPaths` (if `TRUE`) or if `FALSE` will only install
#' packages and their dependencies that are otherwise not installed in
#' `.libPaths()[1]`, i.e., the current active R package directory. Any packages
#' or dependencies that are not yet installed will be installed in `libPaths`.
#'
#'
#' @section GitHub Package: Follows `remotes::install_github` standard. As with
#'   `remotes::install_github`, it is not possible to specify a past version of
#'   a GitHub package unless that version is a tag or the user passes the SHA
#'   that had that package version. Similarly, if a developer does a local
#'   install e.g., via `pkgload::install`, of an active project, this package
#'   will not be able know of the GitHub state, and thus `pkgSnapshot` will not
#'   be able to recover this state as there is no SHA associated with a local
#'   installation. Use `Require` (or `remotes::install_github`) to create a
#'   record of the GitHub state.
#'
#' @section Package Snapshots: To build a snapshot of the desired packages and
#'   their versions, first run `Require` with all packages, then `pkgSnapshot`.
#'   If a `libPaths` is used, it must be used in both functions.
#'
#' @section Mutual Dependencies: This function works best if all required
#'   packages are called within one `Require` call, as all dependencies can be
#'   identified together, and all package versions will be addressed (if there
#'   are no conflicts), allowing a call to `pkgSnapshot()` to take a snapshot or
#'   "record" of the current collection of packages and versions.
#'
#' @section Local Cache of Packages: When installing new packages, `Require`
#'   will put all source and binary files in an R-version specific subfolder of
#'   `getOption("Require.RPackageCache")` whose default is `RPackageCache()`,
#'   meaning *cache packages locally in a project-independent location*, and
#'   will reuse them if needed. To turn off this feature, set
#'   `options("Require.RPackageCache" = FALSE)`.
#'
#' @note For advanced use and diagnosis, the user can set `verbose = TRUE` or
#' `1` or `2` (or via `options("Require.verbose")`). This will attach an
#' attribute `attr(obj, "Require")` to the output of this function.
#'
#' @param install Logical or "force". If `FALSE`, this will not try to install
#'   anything. If `"force"`, then it will force installation of requested
#'   packages, mimicking a call to e.g., `install.packages`. If `TRUE`, the
#'   default, then this function will try to install any missing packages or
#'   dependencies.
#' @param require Logical or character string. If `TRUE`, the default, then the
#'   function will attempt to call `require` on all requested `packages`,
#'   possibly after they are installed. If a character string, then it will only
#'   call `require` on those specific packages (i.e., it will install the ones
#'   listed in `packages`, but load the packages listed in `require`)
#' @param packages Character vector of packages to install via
#'   `install.packages`, then load (i.e., with `library`). If it is one package,
#'   it can be unquoted (as in `require`). In the case of a GitHub package, it
#'   will be assumed that the name of the repository is the name of the package.
#'   If this is not the case, then pass a named character vector here, where the
#'   names are the package names that could be different than the GitHub
#'   repository name.
#' @param packageVersionFile  Character string of a file name or logical. If
#'   `TRUE`, then this function will load the default file,
#'   `getOption("Require.packageVersionFile")`. If this argument is provided,
#'   then this will override all any packages passed to `packages`.
#' @param libPaths The library path (or libraries) where all packages should be
#'   installed, and looked for to load (i.e., call `library`). This can be used
#'   to create isolated, stand alone package installations, if used with
#'   `standAlone = TRUE`. Currently, the path supplied here will be prepended to
#'   `.libPaths()` (temporarily during this call) to `Require` if
#'   `standAlone = FALSE` or will set (temporarily) `.libPaths()` to
#'   `c(libPaths, tail(libPaths(), 1)` to keep base packages.
#' @param repos The remote repository (e.g., a CRAN mirror), passed to either
#'   `install.packages`, `install_github` or `installVersions`.
#' @param install_githubArgs Deprecated. Values passed here are merged with
#'   `install.packagesArgs`, with the `install.packagesArgs` taking precedence
#'   if conflicting.
#' @param install.packagesArgs List of optional named arguments, passed to
#'   `install.packages`. Default is only `--no-multi-arch`, meaning that only
#'   the current architecture will be built and installed (e.g., 64 bit, not 32 bit,
#'   in many cases).
#' @param standAlone Logical. If `TRUE`, all packages will be installed to and
#'   loaded from the `libPaths` only. NOTE: If `TRUE`, THIS WILL CHANGE THE
#'   USER'S `.libPaths()`, similar to e.g., the `checkpoint` package. If
#'   `FALSE`, then `libPath` will be prepended to `.libPaths()` during the
#'   `Require` call, resulting in shared packages, i.e., it will include the
#'   user's default package folder(s). This can be create dramatically faster
#'   installs if the user has a substantial number of the packages already in
#'   their personal library. Default `FALSE` to minimize package installing.
#' @param purge Logical. Should all caches be purged? Default is
#'   `getOption("Require.purge", FALSE)`. There is a lot of internal caching of
#'   results throughout the `Require` package. These help with speed and reduce
#'   calls to internet sources. However, sometimes these caches must be purged.
#'   The cached values are renewed when found to be too old, with the age limit.
#'   This maximum age can be set in seconds with the environment variable
#'   `R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE`, or if unset, defaults to 3600
#'   (one hour -- see [`utils::available.packages`]).
#'
#'   Internally, there are calls to `available.packages`.
#' @param verbose Numeric or logical indicating how verbose should the function
#'   be. If -1 or -2, then as little verbosity as possible. If 0 or FALSE,
#'   then minimal outputs; if `1` or TRUE, more outputs; `2` even more. NOTE: in
#'   `Require` function, when `verbose >= 2`, the return object will have an
#'   attribute: `attr(.., "Require")` which has lots of information about the
#'   processes of the installs.
#' @param type See `utils::install.packages`
#' @param upgrade When `FALSE`, the default, will only upgrade a package when the
#'   version on in the local library is not adequate for the version requirements
#'   of the `packages`. Note: for convenience, `update`
#'   can be used for this argument.
#' @param ... Passed to `install.packages`. Good candidates are e.g., `type` or
#'   `dependencies`. This can be used with `install_githubArgs` or
#'   `install.packageArgs` which give individual options for those 2 internal
#'   function calls.
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
#' opts <- Require:::.setupExample()
#'
#' library(Require)
#' getCRANrepos(ind = 1)
#' Require("stats") # analogous to require(stats), but it checks for
#' #   pkg dependencies, and installs them, if missing
#'
#' if (Require:::.runLongExamples()) {
#'   # Install in a new local library (libPaths)
#'   tempPkgFolder <- file.path(tempdir(), "Packages")
#'   # use standAlone, means it will put it in libPaths, even if it already exists
#'   #   in another local library (e.g., personal library)
#'   Install("crayon", libPaths = tempPkgFolder, standAlone = TRUE)
#'
#'   # make a package version snapshot of installed packages
#'   tf <- tempfile()
#'   (pkgSnapshot(tf, standAlone = TRUE))
#'
#'   # Change the libPaths to emulate a new computer or project
#'   tempPkgFolder <- file.path(tempdir(), "Packages2")
#'   # Reinstall and reload the exact version from previous
#'   Require(packageVersionFile = tf, libPaths = tempPkgFolder, standAlone = TRUE)
#'
#'   # Mutual dependencies, only installs once -- e.g., curl
#'   tempPkgFolder <- file.path(tempdir(), "Packages")
#'   Install(c("remotes", "testit"), libPaths = tempPkgFolder, standAlone = TRUE)
#'
#'   # Mutual dependencies, only installs once -- e.g., curl
#'   tempPkgFolder <- file.path(tempdir(), "Packages")
#'   Install(c("covr", "httr"), libPaths = tempPkgFolder, standAlone = TRUE)
#'
#'   #####################################################################################
#'   # Isolated projects -- Use a project folder and pass to libPaths or set .libPaths() #
#'   #####################################################################################
#'   # GitHub packages
#'   ProjectPackageFolder <- file.path(tempdir(), "ProjectA")
#'   Require("PredictiveEcology/fpCompare@development",
#'     libPaths = ProjectPackageFolder, standAlone = FALSE
#'   )
#'
#'   Install("PredictiveEcology/fpCompare@development",
#'     libPaths = ProjectPackageFolder,
#'     standAlone = TRUE
#'   ) # the latest version on GitHub
#'
#'   ############################################################################
#'   # Mixing and matching GitHub, CRAN, with and without version numbering
#'   ############################################################################
#'   pkgs <- c(
#'     "remotes (<=2.4.1)", # old version
#'     "digest (>= 0.6.28)", # recent version
#'     "PredictiveEcology/fpCompare@a0260b8476b06628bba0ae73af3430cce9620ca0" # exact version
#'   )
#'   Require::Require(pkgs, libPaths = ProjectPackageFolder)
#'   Require:::.cleanup(opts)
#' }
#' }
#'
Require <- function(packages, packageVersionFile,
                    libPaths, # nolint
                    install_githubArgs = list(),
                    install.packagesArgs = list(INSTALL_opts = "--no-multiarch"),
                    standAlone = getOption("Require.standAlone", FALSE),
                    install = getOption("Require.install", TRUE),
                    require = getOption("Require.require", TRUE),
                    repos = getOption("repos"),
                    purge = getOption("Require.purge", FALSE),
                    verbose = getOption("Require.verbose", FALSE),
                    type = getOption("pkgType"),
                    upgrade = FALSE,
                    ...) {
  .pkgEnv$hasGHP <- NULL # clear GITHUB_PAT message; only once per Require session
  opts <- setNcpus()
  checkAutomaticOfflineMode() # This will turn off offlineMode if it had been turned on automatically
  on.exit(
    {
      options(opts)
    },
    add = TRUE
  )

  dots <- list(...)
  if (!is.null(dots$update)) {
    upgrade <- dots$update
  }
  dealWithCache(purge)
  purge <- FALSE

  if (length(install.packagesArgs))
    if (is.null(names(install.packagesArgs)))
      stop("install.packagesArgs must be a list with *named* elements, e.g., INSTALL_opts")

  install.packagesArgs <- modifyList2(list(quiet = !(verbose >= 1)), install.packagesArgs,
    dots,
    keep.null = TRUE
  )
  install_githubArgs <- modifyList2(list(quiet = !(verbose >= 0)), install_githubArgs,
    dots,
    keep.null = TRUE
  )
  install.packagesArgs <- modifyList2(
    install.packagesArgs,
    list(destdir = ".", repos = repos, type = types())
  )

  if (missing(libPaths)) {
    libPaths <- .libPaths()
  }
  libPaths <- checkLibPaths(libPaths = libPaths, exact = TRUE)
  suppressMessages({
    origLibPaths <- setLibPaths(libPaths, standAlone, exact = TRUE)
  })

  doDeps <- if (!is.null(list(...)$dependencies)) list(...)$dependencies else NA
  which <- whichToDILES(doDeps)

  if (!missing(packageVersionFile)) {
    if (isFALSE(packageVersionFile)) {
      messageVerbose(NoPkgsSupplied, verbose = verbose, verboseLevel = 1)
    }

    pkgSnapshotOut <- doPkgSnapshot(packageVersionFile, verbose, purge, libPaths,
      install_githubArgs, install.packagesArgs, standAlone,
      type = type
    )
    return(invisible(pkgSnapshotOut))
  }
  if (missing(packages)) {
    messageVerbose(NoPkgsSupplied, verbose = verbose, verboseLevel = 1)
    return(invisible(NULL))
  }

  # Proceed to evaluate install and load need if there are any packages
  if (NROW(packages)) {
    packages <- anyHaveHEAD(packages)

    deps <- pkgDep(packages,
      purge = purge, libPath = libPaths, recursive = TRUE,
      which = which, type = type, verbose = verbose
    )
    basePkgsToLoad <- packages[packages %in% .basePkgs]
    if (NROW(deps)) {
      allPackages <- unique(unname(unlist(deps)))
      pkgDT <- toPkgDT(allPackages, deepCopy = TRUE)
      pkgDT <- updatePackagesWithNames(pkgDT, packages)
      pkgDT <- parsePackageFullname(pkgDT)
      pkgDT <- parseGitHub(pkgDT)
      pkgDT <- removeDups(pkgDT)
      # pkgDT <- removeBasePkgs(pkgDT)
      pkgDT <- recordLoadOrder(packages, pkgDT)
      pkgDT <- installedVers(pkgDT)
      if (isTRUE(upgrade)) {
        pkgDT <- getVersionOnRepos(pkgDT, repos = repos, purge = purge, libPaths = libPaths)
        if (any(pkgDT$VersionOnRepos != pkgDT$Version, na.rm = TRUE)) {
          pkgDT[VersionOnRepos != Version, comp := compareVersion2(VersionOnRepos, Version, ">=")]
          pkgDT[VersionOnRepos != Version & comp %in% TRUE, `:=`(Version = NA, installed = FALSE)]
          set(pkgDT, NULL, "comp", NULL)
        }
      }
      pkgDT <- dealWithStandAlone(pkgDT, standAlone)
      pkgDT <- whichToInstall(pkgDT, install, verbose)
      if ((any(pkgDT$needInstall %in% "install") && (isTRUE(install))) || install %in% "force") {
        pkgDT <-
          doInstalls(pkgDT,
            repos = repos, purge = purge, libPaths = libPaths, verbose = verbose,
            install.packagesArgs = install.packagesArgs,
            type = type
          )
      }
    }
    if (length(basePkgsToLoad)) {
      pkgDTBase <- toPkgDT(basePkgsToLoad)
      set(pkgDTBase, NULL, c("loadOrder", "installedVersionOK"), list(1L, TRUE))
      if (exists("pkgDT", inherits = FALSE)) {
        pkgDTBase <- rbindlist(list(pkgDT, pkgDTBase), use.names = TRUE, fill = TRUE)
      }
      pkgDT <- pkgDTBase
    }

    out <- doLoads(require, pkgDT)

    if (verbose >= 2) {
      attr(out, "Require") <- pkgDT[]
    }
  } else {
    out <- logical()
  }
  return(invisible(out))
}

rbindlistRecursive <- function(ll) {
  if (is(ll, "list")) {
    ll <- lapply(ll, rbindlistRecursive)
    ll <- rbindlist(ll, fill = TRUE, use.names = TRUE, idcol = FALSE)
  }
  ll
}


build <- function(Package, VersionOnRepos, verbose, quiet, out) {
  if (nchar(Sys.which("R")) > 0) {
    messageVerbose("building package (R CMD build)",
      verbose = verbose, verboseLevel = 1
    )
    internal <- !interactive()
    extras <- c(
      "--no-resave-data", "--no-manual",
      "--no-build-vignettes"
    )
    Rpath1 <- Sys.getenv("R_HOME")
    Rpath <- file.path(Rpath1, "bin/R") # need to use Path https://stat.ethz.ch/pipermail/r-devel/2018-February/075507.html
    filePat <- paste0(Package, "_", VersionOnRepos, ".tar.gz")
    out1 <- Map(pack = Package, function(pack) {
      system(paste(Rpath, "CMD build ", pack, paste(extras, collapse = " ")),
        intern = internal, ignore.stdout = quiet, ignore.stderr = quiet
      )
    })
    # if (any(unlist(out1) == 1L)) {
    #   browser()
    #   browserDeveloper("Error 456; contact developer")
    # }
    messageVerbose("  ... Built!",
      verbose = verbose, verboseLevel = 1
    )
  } else {
    stop("Can't install packages this way because R is not on the search path")
  }
}


installAll <- function(toInstall, repos = getOptions("repos"), purge = FALSE, install.packagesArgs,
                       numPackages, numGroups, startTime, verbose, type = type) {
  messageForInstall(startTime, toInstall, numPackages, verbose, numGroups)
  type <- if (isWindows() || isMacOSX()) {
    # "binary"
    unique(c("source", "binary")[toInstall$isBinaryInstall + 1])
  } else {
    "source"
  }

  install.packagesArgs$INSTALL_opts <- unique(c(install.packagesArgs$INSTALL_opts, "--build"))

  if (isWindows() & getRversion() < "4.2") { # older windows can't update packages before removing them
    toInstall <- rmPackageFirst(toInstall, verbose)
  }

  ap <- try(availablePackagesOverride(toInstall, repos, purge, type = type))
  if (is(ap, "try-error")) {
    browserDeveloper("Error 9566")
  }

  # "repos" is interesting -- must be NULL, not just unspecified, for Local; must be unspecified or specified for Archive & CRAN
  #  This means that we can't get parallel installs for GitHub or Cache
  install.packagesArgs <- modifyList2(install.packagesArgs, list(destdir = NULL), keep.null = TRUE)
  ipa <- modifyList2(install.packagesArgs,
    list(pkgs = toInstall$Package, available = ap, type = type, dependencies = FALSE),
    keep.null = TRUE
  )

  toInstallOut <- withCallingHandlers(
    installPackagesWithQuiet(ipa),
    warning = function(w) {
      messagesAboutWarnings(w, toInstall) # changes to toInstall are by reference; so they are in the return below
      invokeRestart("muffleWarning") # muffle them because if they were necessary, they were redone in `messagesAboutWarnings`
    }
  )
  toInstall
}

doInstalls <- function(pkgDT, repos, purge, tmpdir, libPaths, verbose, install.packagesArgs,
                       type = getOption("pkgType")) {
  tmpdir <- tempdir2(.rndstr(1)) # do all downloads and installs to here; then copy to Cache, if used
  origDir <- setwd(tmpdir)
  on.exit(setwd(origDir))

  pkgDTList <- split(pkgDT, by = c("needInstall"))
  pkgInstall <- pkgDTList[["install"]] # pointer

  on.exit(
    {
      # this copies any tar.gz files to the package cache; works even if partial install.packages
      tmpdirPkgs <- file.path(tempdir(), "downloaded_packages") # from CRAN installs
      copyBuiltToCache(pkgInstall, tmpdirs = c(tmpdir, tmpdirPkgs))
      suppressWarnings(try(postInstallDESCRIPTIONMods(pkgInstall, libPaths), silent = TRUE)) # CRAN is read only after pkgs installed
    },
    add = TRUE
  )

  pkgInstall <- doDownloads(pkgInstall,
    repos = repos, purge = purge, verbose = verbose,
    install.packagesArgs = install.packagesArgs, libPaths = libPaths,
    type = type
  )
  pkgDTList[["install"]] <- pkgInstall
  pkgInstallList <- split(pkgInstall, by = "needInstall") # There are now ones that can't be installed b/c noneAvailable
  pkgInstall <- pkgInstallList[["install"]]
  if (!is.null(pkgInstallList[[noneAvailable]])) {
    messageVerbose(messageCantInstallNoVersion(pkgInstallList[[noneAvailable]]$packageFullName),
                   verbose = verbose, verboseLevel = 1)
  }
  if (!is.null(pkgInstall)) {
    pkgInstall[, isBinaryInstall := isBinary(localFile, needRepoCheck = FALSE)] # filename-based
    pkgInstall[localFile %in% useRepository, isBinaryInstall := isBinaryCRANRepo(Repository)] # repository-based
    pkgInstall <- updateReposForSrcPkgs(pkgInstall)

    startTime <- Sys.time()

    # The install
    pkgInstall[, installSafeGroups := 1L]
    if (isWindows() || isMacOSX()) {
      pkgInstall[, installSafeGroups := (isBinaryInstall %in% FALSE) + 1L]
      pkgInstall <- updateInstallSafeGroups(pkgInstall)
    }
    # pkgInstall <- updateInstallSafeGroups(pkgInstall)

    maxGroup <- max(pkgInstall[["installSafeGroups"]])
    numPackages <- NROW(pkgInstall)
    setorderv(pkgInstall, c("installSafeGroups", "Package"))
    pkgInstall[, installOrder := seq(.N)]

    toInstallList <- split(pkgInstall, by = "installSafeGroups")
    toInstallList <-
      Map(
        toInstall = toInstallList,
        MoreArgs = list(
          repos = repos, purge = purge,
          install.packagesArgs = install.packagesArgs, numPackages = numPackages,
          numGroups = maxGroup, startTime = startTime, verbose = verbose, type = type
        ),
        installAll
      )
    pkgInstallTmp <- rbindlistRecursive(toInstallList)
    needRebuild <- startsWith(basename(pkgInstall$localFile), "NeedRebuild")
    if (any(needRebuild)) {
      pkgInstall <- needRebuildAndInstall(needRebuild = needRebuild, pkgInstall = pkgInstall,
                                          libPaths = libPaths, verbose = verbose,
                                          install.packagesArgs = install.packagesArgs,
                                          repos = repos, purge = purge, startTime = startTime, type = type,
                                          pkgInstallTmp = pkgInstallTmp)
    } else {
      pkgInstall <- pkgInstallTmp
    }

    addOK <- if (!is.null(pkgInstall[["installResult"]])) {
      which(is.na(pkgInstall[["installResult"]]))
    } else {
      NULL
    }
    set(pkgInstall, addOK, c("installResult", "installed"), list("OK", TRUE))
    pkgInstallList[["install"]] <- pkgInstall
  }
  if (!is.null(pkgInstallList[[noneAvailable]])) {
    pkgInstallList[[noneAvailable]][, `:=`(installResult = needInstall, installed = FALSE)]
  }
  pkgDTList[["install"]] <- rbindlistRecursive(pkgInstallList)
  pkgDT <- rbindlistRecursive(pkgDTList)
}




downloadRSPM <- function(toInstall, install.packagesArgs, verbose) {

  # if (isWindows() || isMacOSX()) {

  if (any(grepl(dirname(urlForArchivedPkgs), toInstall$Repository)) ||
      isWindows() || isMacOSX()) {
    installPkgNames <- toInstall$Package
    names(installPkgNames) <- installPkgNames
    toIn <- toInstall
    rver <- rversion()

    earliestDateOnRSPM <- as.Date(gsub(" .*", "", toIn$dayAfterPutOnCRAN))
    latestDateOnRSPM <- pmin(.latestRSPMDate, as.Date(gsub(" .*", "", toIn$dayBeforeTakenOffCRAN)))

    # isWinOrMac <- unname(isWindows() | isMacOSX())
    # if (isWinOrMac) {
    # Get rversions that were live at that time
    packageVersionTooOldForThisR <- unlist(lapply(latestDateOnRSPM, function(ldom) {
      a <- rversionHistory[(as.Date(date) - 200) <= ldom][.N] # CRAN builds packages for R-devel which is before release; picked 200 days here.
      isTRUE(package_version(a$version) <= rver) # if latestDateOnRSPM is NA because asking for a "future" version of package
    }))

    packageVersionOnRSPM <- earliestDateOnRSPM > .earliestRSPMDate
    if (any(packageVersionTooOldForThisR)) {
      messageVerbose(
        paste(toInstall$packageFullName[packageVersionTooOldForThisR], collapse = ", "),
        " will not have binary versions on RSPM because this version of R",
        " was not yet available"
      )
    }
    earliestDateOnRSPM[!packageVersionOnRSPM] <- as.Date(.earliestRSPMDate) + 10
    onRSPM <- earliestDateOnRSPM > .earliestRSPMDate # & isWinOrMac
    onRSPM[is.na(onRSPM)] <- FALSE
    onRSPM <- onRSPM & packageVersionTooOldForThisR %in% FALSE

    if (any(onRSPM)) {
      origIgnoreRepoCache <- install.packagesArgs[["ignore_repo_cache"]]
      install.packagesArgs["ignore_repo_cache"] <- TRUE
      installedPkgs <- file.path(.libPaths()[1], unname(installPkgNames)[onRSPM])
      dirsAlreadyExist <- dir.exists(installedPkgs)
      if (any(dirsAlreadyExist)) {
        try(unlink(installedPkgs[dirsAlreadyExist], recursive = TRUE))
      }
      warnings1 <- list()

      urlsOuter <- c()
      extension <- if (isWindows()) {
        ".zip"
        } else if (isMacOSX()) {
          ".tgz"
        } else {
          ".tar.gz"
        }
      osNameOnRSPM <- if (isWindows()) {
        "windows"
        } else if (isMacOSX()) {
          "macosx"
        } else {
          file.path("__linux__", linuxRelease())
        }
      messageVerbose("-- Determining dates on RSPM to get binaries with correct versions ... ",
                     verbose = verbose, verboseLevel = 1
      )
      total <- length(unname(installPkgNames)[onRSPM])
      installVersions <- toInstall[["VersionOnRepos"]]
      out <- Map(
        p = unname(installPkgNames)[onRSPM], earliestDateRSPM = earliestDateOnRSPM[onRSPM],
        latestDateRSPM = latestDateOnRSPM[onRSPM], tot = total, counter = seq(total),
        v = installVersions[onRSPM], function(p, earliestDateRSPM, latestDateRSPM, v, tot, counter, ...) {
          if (tot > 1) {
            messageVerboseCounter(total = tot, verbose = verbose, verboseLevel = 1, counter = counter)
          }

          for (attempt in 0:15) { # Try up to 15 days from known earliestDateRSPM or latestDateRSPM of the package being available on CRAN
            evenOrOdd <- attempt %% 2 == 0
            date <- if (evenOrOdd) latestDateRSPM else earliestDateRSPM
            if (!is.na(date)) {
              dif <- floor(attempt / 2)
              date <- if (evenOrOdd) date + dif else date - dif
              dow <- weekdays(date)
              wee <- c("Saturday", "Sunday")
              if (dow %in% wee) {
                tweak <- if (evenOrOdd) (- which(wee %in% dow)) else which(rev(wee) %in% dow)
                date <- date + tweak
                if (evenOrOdd)
                  latestDateRSPM <- latestDateRSPM + tweak
                else
                  earliestDateRSPM <- earliestDateRSPM + tweak
              }

              if (isWindows() || isMacOSX()) {
                urls <- file.path(
                  urlForArchivedPkgs, date, "bin", osNameOnRSPM,
                  "contrib", rver,
                  paste0(p, "_", v, extension)
                )
              } else {
                urls <- file.path(
                  urlForArchivedPkgs, osNameOnRSPM, date, "src",
                  "contrib", rver,
                  paste0(p, "_", v, extension)
                )
              }
              con <- url(urls)
              on.exit(try(close(con), silent = TRUE), add = TRUE)
              a <- try(suppressWarnings(readLines(con, n = 1)), silent = TRUE)
              close(con)
              if (is(a, "try-error")) {
                earliestDateRSPM <- earliestDateRSPM + 1
                latestDateRSPM <- latestDateRSPM - 1
                urls <- "Fail"
              } else {
                break
              }
            }

          }
          names(urls) <- p
          urlsOuter <<- c(urlsOuter, urls)
        }
      )

      urlsSuccess <- urlsOuter[urlsOuter != "Fail"]
      urlsFail <- urlsOuter[urlsOuter == "Fail"]

      toInstall[match(names(urlsSuccess), Package), `:=`(
        PackageUrl = urlsSuccess,
        Repository = dirname(urlsSuccess),
        repoLocation = "RSPM",
        localFile = useRepository
      )] #  basename(urlsSuccess))]
      if (length(urlsFail)) {
        cantGet <- toInstall[toInstall$Package %in% names(urlsFail)]
        hasNoVersion <- cantGet$packageFullName == cantGet$Package
        toReport <- list()
        if (any(hasNoVersion)) {
          toReport[[1]] <- paste("latest of", paste0(cantGet$Package[hasNoVersion], collapse = ", "))
        }
        if (any(!hasNoVersion)) {
          toReport[[2]] <- paste(cantGet$packageFullName, collapse = ", ")
        }
        messageVerbose("Could not find a binary of ", # paste(cantGet, collapse = ", "),
                       unlist(toReport, recursive = FALSE),
                       "\nfor R ", rver, " on RSPM; ",
                       "trying source archives",
                       verbose = verbose, verboseLevel = 1
        )
      }
      # if (sum(toInstall$repoLocation %in% "RSPM"))
      #   toInstall[repoLocation %in% "RSPM", {
      #     ipa <- modifyList2(list(url = PackageUrl, destfile = localFile), install.packagesArgs)
      #     do.call(download.file, ipa)
      #   }]
    }
    #}
  } # can do Linux now? Appears no March 13, 2023

  toInstall
}

secondsInADay <- 3600 * 24

urlForArchivedPkgs <- "https://packagemanager.posit.co/cran"
# urlForArchivedPkgs <- "https://packagemanager.rstudio.com/cran/"
# urlForArchivedPkgs <- "https://MRAN.revolutionanalytics.com/snapshot"

archivedOn <- function(possiblyArchivedPkg, verbose, repos, numGroups, counter,
                       srcPackageURLOnCRAN, repo) {
  Map(
    pk = possiblyArchivedPkg, counter = counter, USE.NAMES = TRUE,
    function(pk, counter) {
      # messageVerbose(counter, " of ", numGroups, ": ", pk, verbose = verbose,
      #                verboseLevel = 2)
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
          yy <- url(getArchiveURL(repo, pk))
          on.exit(try(close(yy), silent = TRUE))
          rl2 <- suppressWarnings(try(readLines(yy), silent = TRUE))
          close(yy)
          if (!is(rl2, "try-error")) {
            break
          }
          messageVerbose("Could not get ", pk, " at ", repo, verbose = verbose, verboseLevel = 2)
          if (length(repos) > 1) {
            messageVerbose("; trying next CRAN repo", verbose = verbose, verboseLevel = 2)
          }
        }

        archivedOn <- grep("Archived on", rl, value = TRUE)
        lineWDateAndPkgFilename <- tail(grep(paste0(pk, ".*tar.gz"), rl2, value = TRUE), 1)
        pkgFilename <- gsub(paste0(".+(", pk, "_.+tar.gz).+.+"), "\\1", lineWDateAndPkgFilename)
        PackageUrl <- file.path(pk, pkgFilename)

        if (length(archivedOn)) {
          archivedOn <- as.POSIXct(gsub("Archived on (.+) (.)+", "\\1", archivedOn))
        } else {
          archivedOn <- gsub(".+([[:digit:]]{4,4}-[[:digit:]]{2,2}-[[:digit:]]{2,2}).+", "\\1", lineWDateAndPkgFilename)
          archivedOn <- as.POSIXct(archivedOn) + 5 * 3600 * 24
        }
      } else {
        publishedDate <- grep("Published", rl)
        if (length(publishedDate)) {
          theDateGrep <- "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}"
          theDate <- grep(theDateGrep, rl[seq(publishedDate, publishedDate + 2)], value = TRUE)
          publishedDate <- gsub(paste0(".+(", theDateGrep, ").+"), "\\1", theDate)
        }

        archivedOn <- publishedDate
        PackageUrl <- ""
      }
      archivedOn <- as.character(as.POSIXct(archivedOn) - 3600 * 24)
      list(PackageUrl = PackageUrl, archivedOn = archivedOn)
    }
  )
}

whichToInstall <- function(pkgDT, install, verbose) {
  set(pkgDT, NULL, "isPkgInstalled", !is.na(pkgDT$Version))
  set(pkgDT, NULL, "installedVersionOK", !is.na(pkgDT$Version)) # default: if it is installed,  say "OK"
  if (!is.null(pkgDT[["hasHEAD"]])) {
    whHasHead <- which(pkgDT[["hasHEAD"]] %in% TRUE)
    set(pkgDT, whHasHead, c("installedVersionOK", "isPkgInstalled"), list(FALSE, FALSE))
  }
  set(
    pkgDT, NULL, "hasVersionsToCompare",
    (nchar(pkgDT$inequality) > 0) %in% TRUE & !is.na(pkgDT$Version)
  )
  if (any(pkgDT$hasVersionsToCompare %in% TRUE)) {
    pkgDT[hasVersionsToCompare %in% TRUE, installedVersionOK :=
      compareVersion2(Version, versionSpec, inequality)#,
    #by = seq(sum(hasVersionsToCompare))
    ]
  }

  set(pkgDT, NULL, "needInstall", c("dontInstall", "install")[pkgDT$installedVersionOK %in% FALSE + 1])
  whDontInstall <- pkgDT[["needInstall"]] %in% "dontInstall"
  if (any(whDontInstall & pkgDT$installedVersionOK %in% FALSE)) {
    messageVerbose(messageCantInstallNoVersion(pkgDT$packageFullName[whDontInstall]),
                   verbose = verbose, verboseLevel = 1)
  }
  if (identical(install, "force")) {
    askedByUser <- !is.na(pkgDT$loadOrder)
    set(pkgDT, which(askedByUser), "needInstall", "install")
  }

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
    missingCols <- setdiff(c("installedVersionOK", "availableVersionOK", "installResult"), colnames(pkgDT))
    if (length(missingCols)) set(pkgDT, NULL, missingCols, NA)
    pkgDT[require %in% TRUE, require := (installedVersionOK %in% TRUE | installResult %in% "OK")]
  }

  out <- list()
  if (any(pkgDT$require %in% TRUE)) {
    setorderv(pkgDT, "loadOrder", na.last = TRUE)
    # rstudio intercepts `require` and doesn't work internally
    out[[1]] <- mapply(x = unique(pkgDT$Package[pkgDT$require %in% TRUE]), function(x) {
      base::require(x, character.only = TRUE)
    }, USE.NAMES = TRUE)
  }

  if (any(pkgDT$require %in% FALSE)) {
    out[[2]] <- mapply(x = pkgDT$Package[pkgDT$require %in% FALSE], function(x) FALSE, USE.NAMES = TRUE)
  }
  out <- do.call(c, out)
  out[na.omit(pkgDT$Package[!is.na(pkgDT$loadOrder)])] # put in order, based on loadOrder

  out
}

recordLoadOrder <- function(packages, pkgDT) {
  dups <- duplicated(packages)
  if (any(dups)) {
    packages <- packages[!dups]
  }
  packagesWOVersion <- trimVersionNumber(packages)
  packagesWObase <- setdiff(packagesWOVersion, .basePkgs)
  pfn <- trimVersionNumber(pkgDT$packageFullName)
  wh <- pfn %in% packagesWObase
  out <- try(pkgDT[wh, loadOrder := seq(sum(wh))])
  pkgDT[, loadOrder := na.omit(unique(loadOrder))[1], by = "Package"] # all but one will be removed in trimRedundancies

  if (is(out, "try-error")) {
    browserDeveloper("Error 1253; please contact developer")
  }
  pkgDT
}

removeDups <- function(pkgDT) {
  pkgDT[!duplicated(pkgDT[["packageFullName"]])]
}

dealWithStandAlone <- function(pkgDT, standAlone) {
  if (isTRUE(standAlone)) {
    # Remove any packages that are not in .libPaths()[1], i.e., the main R library
    notInLibPaths1 <- (!pkgDT$Package %in% .basePkgs) &
      (!normPath(pkgDT$LibPath) %in% normPath(.libPaths()[1])) & !is.na(pkgDT$LibPath)
    if (any(notInLibPaths1)) {
      notInLP1 <- "notInLibPaths1"
      set(pkgDT, which(notInLibPaths1), notInLP1, TRUE)

      # This will set the Version in that libPaths to NA, meaning it will be considered uninstalled,
      #   so the machinery will install it in libPaths[1]
      pkgDT[notInLibPaths1, `:=`(
        installed = FALSE,
        LibPath = NA_character_,
        Version = NA_character_
      )]
      pkgDT <- pkgDT[!(duplicated(pkgDT, by = "packageFullName") &
        notInLibPaths1 %in% TRUE)] # this will remove any that are identical packageFullName, which will be because they are in different libPaths
      set(pkgDT, NULL, notInLP1, NULL)
    }
    setorderv(pkgDT, c("Package", "LibPath"), na.last = TRUE)
  }
  pkgDT
}


doDownloads <- function(pkgInstall, repos, purge, verbose, install.packagesArgs,
                        libPaths, type = getOption("pkgType")) {
  pkgInstall[, installSafeGroups := 1L]


  # on.exit()
  # this is a placeholder; set noLocal by default
  set(pkgInstall, NULL, "haveLocal", "noLocal")

  # This sequence checks for the many redundancies, i.e., >= 1.0.0 is redundant with >= 0.9.0; so keep just first
  pkgInstall <- trimRedundancies(pkgInstall, repos, purge, libPaths,
    verbose = verbose,
    type = type
  )
  # When GITHUB_PAT rate limit is hit, the error message in Require is useless;
  #   use `remotes`
  pkgInstall <-
    withCallingHandlers(checkAvailableVersions(pkgInstall, repos, purge, libPaths,
                                               verbose = verbose, type = type),
                        error = function(e) {
                          e$message <- gsub(paste0("(https://).*@"), "\\1", e$message)
                          if (requireNamespace("remotes")) {
                            # THIS WILL TRY `remotes` TO GET THE CORRECT REASON FOR THE ERROR -- but will still error
                            pkgInstallJustGitHub <- pkgInstall[pkgInstall$repoLocation %in% "GitHub"]
                            lapply(seq(NROW(pkgInstallJustGitHub)),
                                   function(ind) {
                                     pijg <- pkgInstallJustGitHub[ind]
                                     thisOne <- any(grepl(paste(pijg$Account, pijg$Repo, sep = ".+"), e))
                                     if (isTRUE(thisOne))
                                       remotes::install_github(pijg$packageFullName)
                                   })

                          } else {
                            stop(e, "\nDoes the url exist? ",
                                 "Are you not using a GITHUB_PAT? Do you have an old GITHUB_PAT?\n",
                                 "For better error handling, install.packages('remotes')")
                          }
                        })
  pkgInstall <- identifyLocalFiles(pkgInstall, repos, purge, libPaths, verbose = verbose)

  pkgInstallList <- split(pkgInstall, by = "haveLocal")
  pkgNeedInternet <- pkgInstallList[["noLocal"]] # pointer
  numToDownload <- NROW(pkgInstallList[["noLocal"]])
  if (NROW(pkgNeedInternet)) {
    pkgNeedInternet <- split(pkgNeedInternet, by = "repoLocation")

    # CRAN
    pkgNeedInternet <- downloadCRAN(pkgNeedInternet, repos, purge, install.packagesArgs,
      verbose, numToDownload,
      type = type
    )

    # Archive
    pkgNeedInternet <- downloadArchive(
      pkgNeedInternet, repos, verbose, install.packagesArgs,
      numToDownload
    )

    # GitHub
    pkgNeedInternet <- downloadGitHub(
      pkgNeedInternet, libPaths, verbose, install.packagesArgs,
      numToDownload
    )

    pkgInstallList[["noLocal"]] <- pkgNeedInternet # pointer
  }

  pkgInstall <- rbindlistRecursive(pkgInstallList)
  pkgInstall[nchar(localFile) == 0, needInstall := noneAvailable]
  pkgInstall
}

getVersionOnRepos <- function(pkgInstall, repos, purge, libPaths, type = getOption("pkgType")) {
  ap <- available.packagesCached(repos = repos, purge = purge, type = type)[, ..apCachedCols]
  setnames(ap, old = "Version", new = "VersionOnRepos")
  pkgInstall <- ap[pkgInstall, on = "Package"]
  # packages that are both on GitHub and CRAN will get a VersionOnRepos; if the request is to load from GH, then change to NA
  pkgInstall[repoLocation %in% "GitHub", VersionOnRepos := NA]
  pkgInstallList <- split(pkgInstall, by = "repoLocation")
  if (!is.null(pkgInstallList[["GitHub"]])) {
    pkgInstallList[["GitHub"]] <- getGitHubVersionOnRepos(pkgInstallList[["GitHub"]])
  }
  pkgInstall <- rbindlistRecursive(pkgInstallList)
  pkgInstall <- getVersionOnReposLocal(pkgInstall)
  pkgInstall
}


# CRAN
downloadCRAN <- function(pkgNoLocal, repos, purge, install.packagesArgs, verbose, numToDownload,
                         type = getOption("pkgType")) {
  pkgCRAN <- pkgNoLocal[["CRAN"]]
  if (NROW(pkgCRAN)) { # CRAN, Archive, RSPM
    # messageVerbose(messageDownload(pkgCRAN, NROW(pkgCRAN), "CRAN"), verbose = verbose, verboseLevel = 2)
    if (!all(apCachedCols %in% colnames(pkgCRAN))) {
      ap <- available.packagesCached(repos = repos, purge = purge, type = type)[, ..apCachedCols]
      setnames(ap, old = "Version", new = "VersionOnRepos")
      pkgNoLocal[["CRAN"]] <- ap[pkgCRAN, on = "Package"]
      pkgCRAN <- pkgNoLocal[["CRAN"]] # pointer
    }
    # N <- pkgCRAN[, .N, by = Package]
    # if (any(N$N > 1)) {
    #   pkgCRAN <- pkgCRAN[, .SD[1], by = "Package"]
    # }
    # pkgCRAN <- availableVersionOK(pkgCRAN)

    # Not on CRAN; so likely Archive
    notOK <- !pkgCRAN$availableVersionOK %in% TRUE # FALSE means it is on CRAN, but not that version; NA means it is not on CRAN currently
    if (any(notOK)) {
      messageVerbose(
        blue(
          "  -- ",
          paste(pkgCRAN$packageFullName[notOK],
            collapse = ", "
          ), " ",
          isAre(l = pkgCRAN$packageFullName[notOK]),
          " not on CRAN; trying Archives"
        ),
        verbose = verbose, verboseLevel = 1
      )
      pkgCRAN[notOK, `:=`(repoLocation = "Archive", installFrom = "Archive")]
      pkgNoLocal[["CRAN"]] <- pkgCRAN
      pkgNoLocal <- rbindlistRecursive(pkgNoLocal)
      pkgNoLocal <- split(pkgNoLocal, by = "repoLocation")
      pkgCRAN <- pkgNoLocal[["CRAN"]]
    }
    if (NROW(pkgCRAN)) {
      pkgCRAN[availableVersionOK %in% TRUE, installFrom := "CRAN"]
      pkgCRAN[, localFile := useRepository]
    }
  }
  pkgNoLocal # pkgCRAN is already in this because it was a pointer
}

#' @importFrom utils contrib.url
downloadArchive <- function(pkgNonLocal, repos, verbose, install.packagesArgs, numToDownload) {
  pkgArchive <- pkgNonLocal[["Archive"]]
  if (NROW(pkgArchive)) {
    ava <- lapply(archiveVersionsAvailable(pkgArchive$Package[pkgArchive$repoLocation %in% "Archive"],
      repos = repos
    ), function(d) {
      aa <- as.data.table(d, keep.rownames = "PackageUrl")
      if (!is.null(aa[["mtime"]])) setorderv(aa, "mtime")
      aa
    })
    if (isFALSE(getOption("Require.offlineMode", FALSE))) {
      pkgArchive <- getArchiveDetails(pkgArchive, ava, verbose, repos)
      # Check RSPM
      pkgArchive <- downloadRSPM(pkgArchive, install.packagesArgs, verbose)

      if (any(pkgArchive$repoLocation %in% "Archive" & pkgArchive$availableVersionOK %in% TRUE)) {
        pkgArchive <- split(pkgArchive, pkgArchive[["repoLocation"]])
        pkgArchOnly <- pkgArchive[["Archive"]]
        pkgArchOnly[, Repository := file.path(contrib.url(repos[1], type = "source"), "Archive", Package)]
        pkgArchOnly[, localFile := useRepository]
      }
      pkgArchive <- rbindlistRecursive(pkgArchive)
    }
  }
  pkgNonLocal[["Archive"]] <- pkgArchive
  pkgNonLocal
}

downloadGitHub <- function(pkgNoLocal, libPaths, verbose, install.packagesArgs, numToDownload) {
  pkgGitHub <- pkgNoLocal[["GitHub"]]
  if (NROW(pkgGitHub)) { # GitHub
    messageVerbose(messageDownload(pkgGitHub, NROW(pkgGitHub), "GitHub"), verbose = verbose, verboseLevel = 2)

    # If there was a local cache check, then this was already done; internally this will be fast/skip check
    pkgGitHub <- getGitHubVersionOnRepos(pkgGitHub)
    pkgGitHub <- availableVersionOK(pkgGitHub)
    avOK <- which(pkgGitHub$availableVersionOK %in% TRUE)

    if (length(avOK)) {
      if (is.null(pkgGitHub[["SHAonGH"]])) {
        colsToUpdate <- c("SHAonGH")
        set(pkgGitHub, NULL, colsToUpdate, list(NA_character_)) # fast to just do all; then next lines may update
        pkgGitHub[avOK, (colsToUpdate) := {
          SHAonGH <- getSHAfromGitHubMemoise(repo = Repo, acct = Account, br = Branch, verbose = verbose)
        }, by = "Package"]
        saveGitHubSHAsToDisk()
      }

      pkgGHList <- split(pkgGitHub, pkgGitHub$availableVersionOK)
      pkgGHtoDL <- pkgGHList[["TRUE"]]
      if (any(!pkgGHtoDL[["installedVersionOK"]])) {
        # download and build in a separate dir; gets difficult to separate this addition from existing files otherwise
        td <- tempdir2(.rndstr(1))
        prevDir <- setwd(td)
        on.exit({
          cleanUpNewBuilds(pkgGHtoDL, prevDir) # no need to assign this because it is on.exit fail
          if (isTRUE(needRmGSF))
            set(pkgGHtoDL, NULL, "GitSubFolder", NULL)
        })
        needRmGSF <- FALSE
        if (!"GitSubFolder" %in% names(pkgGHtoDL)) {
          set(pkgGHtoDL, NULL, "GitSubFolder", NA_character_)
          needRmGSF <- TRUE
        }
        pkgGHtoDL[!SHAonGH %in% FALSE, localFile := {
        #  toDL <- .SD[!SHAonGH %in% FALSE]
          downloadAndBuildToLocalFile(Account, Repo, Branch, Package, GitSubFolder, verbose, VersionOnRepos)
            # gitRepo <- paste0(Account, "/", Repo, "@", Branch)
            # names(gitRepo) <- Package
            # out <- downloadRepo(gitRepo, subFolder = GitSubFolder,
            #                     overwrite = TRUE, destDir = ".", verbose = verbose)
            # out1 <- try(build(Package, verbose = verbose, quiet = FALSE, VersionOnRepos = VersionOnRepos))
            # fn <- dir(pattern = paste0("^", Package, "_.+tar.gz"))
            # normPath(fn)
        }, by = "Package"] # seq(NROW(pkgGHtoDL))]

        empty <- !nzchar(pkgGHtoDL[!SHAonGH %in% FALSE]$localFile)
        if (any(empty))
            pkgGHtoDL[which(!SHAonGH %in% FALSE)[empty %in% TRUE], localFile := "NeedRebuild"]
        # A bit of cleaning; this will get rid of the source files; we have the tar.gz after `build`
        pkgGHtoDL[, installFrom := "GitHub"]
        pkgGHtoDL <- renameLocalGitPkgDT(pkgGHtoDL)
        pkgGHtoDL <- cleanUpNewBuilds(pkgGHtoDL, prevDir)
        if (isTRUE(needRmGSF))
          set(pkgGHtoDL, NULL, "GitSubFolder", NULL)
        on.exit() # don't run on.exit above if this was successful
      }
      pkgGitHub <- rbindlistRecursive(pkgGHList)
    }
  }
  pkgNoLocal[["GitHub"]] <- pkgGitHub
  pkgNoLocal
}

cleanUpNewBuilds <- function(pkgDT, prevDir) {
  files <- dir()
  unlink(files[!endsWith(files, "tar.gz")]) # There was a random .zip file; couldn't tell why; remove it
  unlink(files[dir.exists(files)], recursive = TRUE) # remove directories
  moveFileToCacheOrTmp(pkgDT)
  setwd(prevDir)
  pkgDT
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
  if (identical(length, 1L)) {
    types <- types[1]
  }
}

doPkgSnapshot <- function(packageVersionFile, verbose, purge, libPaths,
                          install_githubArgs, install.packagesArgs, standAlone = TRUE,
                          type = getOption("pkgType")) {
  if (!isFALSE(packageVersionFile)) {
    if (isTRUE(packageVersionFile)) {
      packageVersionFile <- getOption("Require.packageVersionFile")
    }
    packages <- data.table::fread(packageVersionFile)
    packages <- dealWithSnapshotViolations(packages,
      verbose = verbose, purge = purge,
      libPaths = libPaths, type = type
    ) # i.e., packages that can't coexist
    packages <- packages[!packages$Package %in% .basePkgs]
    out <- Require(packages$packageFullName,
      verbose = verbose, purge = purge, libPaths = libPaths,
      install_githubArgs = install_githubArgs, install.packagesArgs = install.packagesArgs,
      standAlone = standAlone, require = FALSE, install = TRUE
    )
  } else {
    out <- FALSE
  }
  out
}

dealWithSnapshotViolations <- function(pkgSnapshotObj, verbose = getOption("Require.verbose"),
                                       purge = getOption("Require.purge", FALSE),
                                       libPaths = .libPaths(),
                                       repos = getOption("repos"), type = getOption("pkgType")) {
  dd <- pkgSnapshotObj
  ff <- packageFullNameFromSnapshot(dd)
  gg <- pkgDep(ff, recursive = TRUE, purge = purge)
  hh <- sort(unique(gsub("(\\(.*)( )+(.*\\))$", "\\1\\3", gsub("\n", "", unname(unlist(gg))))))
  pkgDT <- toPkgDT(hh, deepCopy = TRUE)
  pkgDT <- parsePackageFullname(pkgDT)
  pkgDT <- parseGitHub(pkgDT)
  pkgDT <- trimRedundancies(pkgDT,
    purge = purge, repos = repos, libPaths = libPaths,
    type = type
  )
  pkgDT <- checkAvailableVersions(pkgDT, repos = repos, purge = purge, libPaths = libPaths,
                                  verbose = verbose, type = type)

  pkgDT[, c("Package", "packageFullName")]
}

apCachedCols <- c("Package", "Repository", "Version", "Archs", "Depends", "Imports", "Suggests", "LinkingTo")


localFilename <- function(pkgInstall, localFiles, libPaths, verbose) {
  pkgWhere <- split(pkgInstall, pkgInstall[["repoLocation"]])
  pkgGitHub <- pkgWhere[["GitHub"]] # pointer
  if (NROW(pkgWhere[["GitHub"]])) {
    pkgGitHub <- getGitHubVersionOnRepos(pkgGitHub)
    pkgGitHub <- availableVersionOK(pkgGitHub)
    avOK <- which(pkgGitHub$availableVersionOK %in% TRUE)
    colsToUpdate <- c("SHAonLocal", "SHAonGH")
    set(pkgGitHub, NULL, colsToUpdate, list(NA_character_, NA_character_)) # fast to just do all; then next lines may update
    if (length(avOK)) {
      pkgGitHub[avOK, (colsToUpdate) := {
        alreadyExistingDESCRIPTIONFile <- file.path(libPaths[1], Repo, "DESCRIPTION")
        SHAonGH <- getSHAfromGitHubMemoise(repo = Repo, acct = Account, br = Branch, verbose = verbose)
        if (file.exists(alreadyExistingDESCRIPTIONFile)) {
          SHAonLocal <- DESCRIPTIONFileOtherV(alreadyExistingDESCRIPTIONFile, other = "GithubSHA1")
          SHAonLocal <- unique(SHAonLocal)[1] # seems like it can be >1, which is an error
          # SHAonGH <- if (identical(SHAonGH, SHAonLocal)) FALSE else SHAonGH
          if (identical(SHAonGH, SHAonLocal)) {
            messageVerbose("Skipping install of ", paste0(Account, "/", Repo, "@", Branch),
              ", the SHA1 has not changed from last install",
              verbose = verbose, verboseLevel = 1
            )
          }
        } else {
          SHAonLocal <- ""
        }
        list(SHAonLocal, SHAonGH)
      }, by = "packageFullName"]
    }
    saveGitHubSHAsToDisk()
    pkgGitHub[SHAonLocal == SHAonGH, `:=`(needInstall = FALSE, haveLocal = "Local",
                                          installedVersionOK = TRUE, installResult = "OK")]
    pkgWhere[["GitHub"]] <- pkgGitHub
    pkgInstall <- rbindlistRecursive(pkgWhere)
  }

  pkgInstall[, localFile := localFileID(
    Package, localFiles, repoLocation, SHAonGH,
    inequality, VersionOnRepos, versionSpec
  ), by = seq(NROW(pkgInstall))]

  pkgInstall
}

#' Needs `VersionOnRepos`, `versionSpec` and `inequality` columns
#' @param pkgDT A `pkgDT` object
availableVersionOK <- function(pkgDT) {
  # First set all to availableVersionOK if there is a version available
  pkgDT[, availableVersionOK := !is.na(VersionOnRepos)]

  availableOKcols <- c("availableVersionOK", "availableVersionOKthisOne")
  hasAtLeastOneNonNA <- !is.na(pkgDT$inequality) & !is.na(pkgDT$VersionOnRepos)
  if (any(hasAtLeastOneNonNA)) {
    pkgDT[, hasAtLeastOneNonNA := any(hasAtLeastOneNonNA), by = "Package"]
    out <- try(pkgDT[hasAtLeastOneNonNA %in% TRUE, (availableOKcols) := {
      out <- Map(vor = VersionOnRepos, function(vor) any(!compareVersion2(vor, versionSpec, inequality) %in% FALSE))
      avokto <- Map(vor = VersionOnRepos, function(vor) any(compareVersion2(vor, versionSpec, inequality) %in% TRUE))
      avok <- unlist(out)
      list(availableVersionOK = avok, availableVersionOKthisOne = unlist(avokto))
    }, by = "Package"])
    if (is(out, "try-error")) {
      browserDeveloper("Error 553; please contact developer")
    }
  } else {
    pkgDT[!is.na(VersionOnRepos), (availableOKcols) := list(TRUE, TRUE)]
  }
  pkgDT[is.na(VersionOnRepos), (availableOKcols) := list(FALSE, FALSE)]

  # Then update this for the subset that have an actual inequality

  pkgDT
}

compareVersion2 <- function(version, versionSpec, inequality) {
  if (isTRUE(any(is(version, "numeric_version"))))
    version <- as.character(version)
  if (isTRUE(any(is(versionSpec, "numeric_version"))))
    versionSpec <- as.character(versionSpec)
  out <- Map(
    vers = version, ineq = inequality, verSpec = versionSpec, # this will recycle, which may be bad
    function(ineq, vers, verSpec) {
      if (!is.na(ineq) && !is.na(vers) && !is.na(verSpec)) {
        a <- compareVersion(verSpec, vers)
        out <- do.call(ineq, list(0, a))
        # out <- do.call(ineq, list(package_version(vers), verSpec))
      } else {
        NA
      }
    }
  )
  out <- unlist(out)
  out
}

noneAvailable <- "noneAvailable"

updatePackagesWithNames <- function(pkgDT, packages) {
  origPackagesHaveNames <- nchar(names(packages)) > 0
  if (any(origPackagesHaveNames, na.rm = TRUE)) {
    whHasHEAD <- grep(HEADgrep, names(packages))
    packageFullNameWithHEAD <- packages[whHasHEAD]
    if (length(whHasHEAD)) {
      pkgDT[
        match(packages[origPackagesHaveNames], packageFullName),
        hasHEAD := packageFullName %in% packageFullNameWithHEAD
      ]
      names(packages)[whHasHEAD] <- NA
      if (all(is.na(names(packages)))) names(packages) <- NULL
    }
    if (!is.null(names(packages))) {
      pkgDT[
        match(packages[origPackagesHaveNames], packageFullName),
        Package := names(packages[origPackagesHaveNames])
      ]
    }
  }
  pkgDT
}

messageDownload <- function(pkgDT, numToDownload, fromWhere) {
  paste0(blue(
    " -- downloading ", numToDownload, " packages from ", fromWhere,
    ": ", paste(pkgDT$Package, collapse = ", "), " --"
  ))
}

colr <- function(..., digit = 32) paste0("\033[", digit, "m", paste0(...), "\033[39m")
purple <- function(...) colr(..., digit = 30)
green <- function(...) colr(..., digit = 32)
yellow <- function(...) colr(..., digit = 33)
blue <- function(...) colr(..., digit = 34)
turquoise <- function(...) colr(..., digit = 36)
greyLight <- function(...) colr(..., digit = 37)

messageForInstall <- function(startTime, toInstall, numPackages, verbose, numGroups) {
  currentTime <- Sys.time()
  dft <- difftime(currentTime, startTime, units = "secs")
  installRange <- unique(c(toInstall$installOrder[1], tail(toInstall$installOrder, 1)))
  timeLeft <- dft / installRange[1] * (numPackages - installRange[1] + 1)

  lotsOfTimeLeft <- dft > 10
  timeLeftAlt <- if (lotsOfTimeLeft) format(timeLeft, units = "auto", digits = 1) else "..."
  estTimeFinish <- if (lotsOfTimeLeft) Sys.time() + timeLeft else "...calculating"
  pkgToReport <- paste(preparePkgNameToReport(toInstall$Package, toInstall$packageFullName),
                       collapse = ", ")
  Source <- ifelse(toInstall$installFrom %in% "Local", "Local", toInstall$repoLocation)
  pkgToReportBySource <- split(toInstall$Package, Source)
  pkgFullNameToReportBySource <- split(toInstall$packageFullName, Source)
  installRangeCh <- paste(installRange, collapse = ":")

  srces <- names(pkgToReportBySource)
  messageVerbose("-- Installing from:", verbose = verbose, verboseLevel = 0)
  nxtSrc <- c(yellow = "Local", blue = "CRAN", turquoise = "Archive", green = "GitHub", purple = "RSPM")
  Map(colr = names(nxtSrc), type = nxtSrc, function(colr, type) {
    pp <- pkgToReportBySource[[type]]
    if (type %in% srces) {
      if (type %in% "GitHub") {
        pp <- pkgFullNameToReportBySource[[type]]
      }
      messageVerbose(get(colr)("  -- ", type, ": ", paste(pp, collapse = ", ")),
        verbose = verbose, verboseLevel = 0
      )
    }
  })
  messageVerbose(
    blue(
      "-- ", installRangeCh, " of ", numPackages,
      if (numGroups > 1) {
        paste0(" (grp ", unique(toInstall$installSafeGroups), " of ", numGroups, ")")
      } else {
        ""
      },
      ". Estimated time left: ",
      timeLeftAlt, "; est. finish: ", estTimeFinish
    ),
    verbose = verbose, verboseLevel = 0
  )
}

#' Create a custom "available.packages" object
#'
#' This is the mechanism by which `install.packages` determines which packages
#' should be installed from where. With this override, we can indicate arbitrary
#' `repos`, `Package`, `File` for each individual package.
#' @param toInstall A `pkgDT` object
#' @inheritParams Require
availablePackagesOverride <- function(toInstall, repos, purge, type = getOption("pkgType")) {
  whLocal <- startsWith(unique(dirname(dirname(toInstall$Repository))), "file")
  if (any(whLocal %in% FALSE) && any(toInstall$repoLocation %in% "CRAN") &&
    !any(grepl("contrib", toInstall$Repository))) {
    repos <- unique(dirname(dirname(toInstall$Repository)))
  }
  ap <- available.packagesCached(repos = repos, purge = purge, returnDataTable = FALSE, type = type)
  pkgsNotInAP <- toInstall$Package[!toInstall$Package %in% ap[, "Package"]]
  NumPkgsNotInAP <- length(pkgsNotInAP)

  if (NumPkgsNotInAP) { # basically no version is there

    # Have to make the ap object be the same length as the number of pkgsNotInAP
    if (NROW(ap) < NumPkgsNotInAP) { # no nough rows; must add
      ap3 <- rbind(ap, matrix(nrow = NumPkgsNotInAP, rep(rep(NA, NCOL(ap)), NumPkgsNotInAP)))
    } else { #  too many rows, keep first NumPkgsNotInAP
      ap3 <- ap[seq(NumPkgsNotInAP), , drop = FALSE]
    }

    ap3[, "Package"] <- pkgsNotInAP
    rownames(ap3) <- ap3[, "Package"]
    ap3[, "Version"] <- toInstall[Package %in% pkgsNotInAP]$VersionOnRepos
    if (!is.null(toInstall$Repository)) {
      ap3[, "Repository"] <- toInstall[Package %in% pkgsNotInAP]$Repository
    }
    ap3[, "Depends"] <- NA
    deps <- pkgDep(toInstall[Package %in% pkgsNotInAP]$packageFullName, recursive = T)
    pkgHasNameDiffrntThanRepo <- extractPkgName(names(deps)) != toInstall[Package %in% pkgsNotInAP]$Package
    if (any(pkgHasNameDiffrntThanRepo)) {
      names(deps)[pkgHasNameDiffrntThanRepo] <- toInstall[Package %in% pkgsNotInAP]$Package[pkgHasNameDiffrntThanRepo]
    }
    deps2 <- unlist(Map(dep = deps, nam = names(deps), function(dep, nam) {
      paste(setdiff(extractPkgName(dep), extractPkgName(nam)), collapse = ", ")
    })) # -1 is "drop self"
    ap3[match(extractPkgName(names(deps2)), ap3[, "Package"]), "Imports"] <- deps2
    ap3[, "Suggests"] <- NA
    rownames(ap3) <- pkgsNotInAP
  } else {
    ap3 <- ap[0, , drop = FALSE]
  }

  ap <- ap[ap[, "Package"] %in% toInstall$Package, , drop = FALSE]
  if (NROW(ap3)) {
    ap <- rbind(ap, ap3)
  }

  toInstallList <- split(toInstall, by = "installFrom")
  apList <- list()
  apOrig <- ap
  for (i in names(toInstallList)) {
    # First do version number -- this is same for all locations
    whUpdate <- match(toInstallList[[i]]$Package, apOrig[, "Package"])
    ap <- apOrig[whUpdate, , drop = FALSE]
    isNA <- is.na(toInstallList[[i]]$VersionOnRepos)
    if (any(isNA)) {
      whUseRepository <- !toInstallList[[i]][isNA]$localFile %in% useRepository
      if (any(whUseRepository)) {
        ap[isNA, "Version"] <- extractVersionNumber(filenames = toInstallList[[i]][isNA][whUseRepository]$localFile)
      }
    }
    if (any(!isNA)) {
      ap[!isNA, "Version"] <- toInstallList[[i]]$VersionOnRepos[!isNA]
    }
    if (i %in% c("Archive", "CRAN")) {
      ap[, "Repository"] <- toInstallList[[i]]$Repository
    }
    if (i %in% c("Local", "GitHub")) {
      localFile2 <- toInstallList[[i]]$localFile
      fnBase <- basename(localFile2)
      file.copy(localFile2, fnBase, overwrite = TRUE) # copy it to "here"
      newNameWithoutSHA <- gsub("(-[[:alnum:]]{40})_", "_", fnBase)
      fileRenameOrMove(fnBase, newNameWithoutSHA)
      ap[, "File"] <- newNameWithoutSHA
      ap[, "Repository"] <-
        paste0("file:///", normPath("."))
    }
    apList[[i]] <- ap
  }

  do.call(rbind, apList)
}

useRepository <- "useRepository"

setNcpus <- function() {
  Ncpus <- getOption("Ncpus")
  if (is.null(Ncpus)) {
    newVal <- if (requireNamespace("parallel", quietly = TRUE)) {
      min(8, parallel::detectCores())
    } else {
      4
    }

    opts <- options(Ncpus = newVal)
  } else {
    opts <- NULL
  }
  opts
}

keepOnlyBinary <- function(fn, keepSourceIfOnlyOne = TRUE) {
  if (length(fn) > 1) {
    isBin <- isBinary(fn, needRepoCheck = FALSE)
    if (any(isBin)) { # pick binary if it exists; faster to install
      versions <- extractVersionNumber(filenames = fn)
      pkgs <- extractPkgName(filenames = basename(fn))
      pkgVer <- paste0(pkgs, "_", versions)
      N <- table(pkgVer)
      pkgVerKeep <- names(N)[N == 1]
      pkgVerDups <- names(N)[N > 1]
      if ((length(pkgVerDups) == 0) || keepSourceIfOnlyOne %in% FALSE) {
        fn <- fn[isBin]
      } else {
        fnKeepSingles <- fn[match(pkgVerKeep, pkgVer)]
        fnMoreThanOne <- fn[pkgVer %in% pkgVerDups]
        fnMoreThanOne <- fnMoreThanOne[isBinary(fnMoreThanOne, needRepoCheck = FALSE)]
        fn <- c(fnMoreThanOne, fnKeepSingles)
      }
    }
  }
  fn
}

moveFileToCacheOrTmp <- function(pkgInstall) {
  localFileDir <- if (!is.null(getOptionRPackageCache())) {
    getOptionRPackageCache()
  } else {
    tempdir2(.rndstr(1))
  }
  fns <- pkgInstall$localFile
  movedFilename <- file.path(localFileDir, basename(fns))
  if (!identical(fns, movedFilename)) {
    fileRenameOrMove(fns, movedFilename)
    set(pkgInstall, NULL, "localFile", movedFilename)
  }
  pkgInstall
}

getGitHubVersionOnRepos <- function(pkgGitHub) {
  if (isFALSE(getOption("Require.offlineMode", FALSE))) {
    notYet <- is.na(pkgGitHub$VersionOnRepos)
    if (any(notYet)) {
      pkgGitHub <- getGitHubFile(pkgGitHub)
      pkgGitHub[!is.na(DESCFile), VersionOnRepos := DESCRIPTIONFileVersionV(DESCFile)]
    }
  }
  pkgGitHub
}

#' @importFrom utils tail
localFileID <- function(Package, localFiles, repoLocation, SHAonGH, inequality, VersionOnRepos, versionSpec) {
  ##### Not vectorized ######
  PackagePattern <- paste0("^", Package, "(\\_|\\-)+.*")
  whLocalFile <- grep(pattern = PackagePattern, x = basename(localFiles))
  fn <- localFiles[whLocalFile]
  systemSpecificFileTypes <- if (isWindows()) {
    endsWith(fn, "zip") | endsWith(fn, "tar.gz")
  } else if (isMacOSX()) {
    endsWith(fn, "tgz") | endsWith(fn, "tar.gz")
  } else {
    endsWith(fn, "tar.gz")
  }
  fn <- fn[systemSpecificFileTypes]

  if (repoLocation %in% "GitHub") {
    fn <- if (is.na(SHAonGH)) "" else grep(SHAonGH, fn, value = TRUE)
    fn <- keepOnlyBinary(fn)
  } else {
    if (length(fn)) {
      fn <- keepOnlyBinary(fn)
      localVer <- extractVersionNumber(filenames = fn) # there may be >1 file for a given package; take
      if (is.na(inequality)) { # need max --> which will be the versionSpec; so see if localVer is same as versionSpec
        if (!is.na(VersionOnRepos)) {
          ord <- order(c(package_version(localVer), package_version(VersionOnRepos)), decreasing = TRUE) # local first
        } else {
          ord <- 1
        }

        if (ord[1] %in% seq_along(localVer)) { # if it is first, then it is bigger or equal, so keep
          fn <- fn[ord[1]]
        } else {
          fn <- ""
        }
      } else {
        keepLoc <- try(unlist(compareVersion2(localVer, versionSpec, inequality)))
        if (is(keepLoc, "try-error")) {
          browserDeveloper("Error 978; please contact developer")
        }
        if (!identical(inequality, "==") && !is.na(VersionOnRepos)) {
          keepRep <- compareVersion2(VersionOnRepos, versionSpec, inequality)
          if (any(keepLoc %in% TRUE)) { # local has at least 1 that is good -- could be many versions of the package locally
            locPacVer <- package_version(localVer[keepLoc]) # keep all that pass version check
            keepLoc <- which(keepLoc)[which(max(locPacVer) == locPacVer)][1] # take the one that passes version check that is max version
            if (any(keepRep %in% TRUE)) { # remote has as at least 1 that is good # next, take local if local is same or newer than remote
              ord <- order(c(package_version(localVer[keepLoc]), package_version(VersionOnRepos)), decreasing = TRUE) # local first
              if (ord[1] %in% seq_along(localVer[keepLoc])) { # if it is first, then it is bigger or equal, so keep
                fn <- fn[keepLoc][ord[1]]
              } else {
                fn <- ""
              }
            } else {
              fn <- fn[keepLoc]
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
  }
  if (length(fn) > 1) {
    isGH <- isGitHub(filenames = fn)
    if (any(!isGH)) {
      fn <- fn[!isGH]
    } else {
      fn <- fn[1]
    }
  }
  if (length(fn) == 0) fn <- ""
  fn
}

identifyLocalFiles <- function(pkgInstall, repos, purge, libPaths, verbose) {
  #### Uses pkgInstall #####
  if (!is.null(getOptionRPackageCache())) {
    # check for crancache copies
    localFiles <- dir(getOptionRPackageCache(), full.names = TRUE)
    localFiles <- doCranCacheCheck(localFiles, verbose)
    pkgInstall <- localFilename(pkgInstall, localFiles, libPaths = libPaths, verbose = verbose)
    pkgInstall[, haveLocal :=
      unlist(lapply(localFile, function(x) c("noLocal", "Local")[isTRUE(nchar(x) > 0) + 1]))]
    pkgInstall[haveLocal %in% "Local", `:=`(
      installFrom = haveLocal,
      availableVersionOK = TRUE,
      Repository = paste0("file:///", getOptionRPackageCache())
    )]
  } else {
    set(pkgInstall, NULL, c("localFile"), "")
  }
}


confirmEqualsDontViolateInequalitiesThenTrim <- function(pkgDT,
                                                         ifViolation = c("removeEquals", "stop"),
                                                         verbose = getOption("Require.verbose")) {
  set(pkgDT, NULL, "isEquals", pkgDT[["inequality"]] == "==")
  pkgDT[, hasEqualsAndInequals := any(isEquals %in% TRUE) && any(isEquals %in% FALSE), by = "Package"]
  pkgDT[hasEqualsAndInequals %in% TRUE, EqualsDoesntViolate := {
    out <- rep(NA, length = .N)
    wh <- which(isEquals)
    whNot <- which(!isEquals)
    if (length(wh)) {
      out[wh] <- try(unlist(Map(verSpec = versionSpec[wh], function(verSpec) {
        all(compareVersion2(verSpec, versionSpec[whNot], inequality[whNot]))
      })))
      if (is(out, "try-error")) {
        browserDeveloper("Error 844; please contact developer")
      }
      out
    }
    out
  },
  by = "Package"
  ]
  set(pkgDT, which(pkgDT$isEquals %in% TRUE & pkgDT$hasEqualsAndInequals %in% FALSE), "EqualsDoesntViolate", TRUE)

  pkgDT[, violation := if (any(isEquals %in% TRUE) && all(!EqualsDoesntViolate %in% TRUE)) {
    TRUE
  } else {
    FALSE
  },
  by = "Package"
  ]
  if (any(pkgDT$violation %in% TRUE)) {
    messageVerbose(green(
      "The following shows packages whose version requirements can not be met; ",
      "keeping the newer version: "
    ), verbose = verbose, verboseLevel = 1)
    cols <- c("Package", "packageFullName", "versionSpec")
    cols2 <- setdiff(cols, "packageFullName")
    cols3 <- c(cols, "versionToKeep")
    violationsDF <- pkgDT[violation %in% TRUE & !is.na(versionSpec), ..cols]
    # can't use setorderv on versions b/c stored as character vector
    orderVersions <- order(violationsDF[[cols2[1]]],
                           package_version(violationsDF[[cols2[[2]]]]),
                           decreasing = TRUE)
    violationsDF <- violationsDF[orderVersions,]
    violationsDF <- violationsDF[, versionToKeep := {
      ver <- rep("", length = .N)
      ver[1] <- versionSpec[1]
      ver
    }, by = "Package"][, ..cols3]
    messageDF(verbose = verbose, verboseLevel = 1, violationsDF)
    if (grepl("remove|rm", ifViolation[1])) {
      pkgDT <- pkgDT[violation %in% TRUE & !inequality %in% "==" | violation %in% FALSE]
    }
  }

  pkgDT[, keep := if (any(isEquals %in% TRUE) && any(EqualsDoesntViolate %in% TRUE)) {
    .I[isEquals %in% TRUE & EqualsDoesntViolate %in% TRUE][1]
  } else {
    .I
  },
  by = "Package"
  ]
  pkgDT <- pkgDT[unique(keep)]
  set(pkgDT, NULL, c("isEquals", "hasEqualsAndInequals", "EqualsDoesntViolate", "keep"), NULL)
  pkgDT
}

keepOnlyGitHubAtLines <- function(pkgDT, verbose = getOption("Require.verbose")) {
  gitRepos <- pkgDT$repoLocation %in% "GitHub"
  if (any(gitRepos)) {
    pkgDT[gitRepos %in% TRUE, c("versionSpec", "inequality") := {
      vs <- versionSpec
      ineq <- inequality
      whHasSHAasBranch <- nchar(Branch) == 40
      if (any(whHasSHAasBranch, na.rm = TRUE)) {
        wh <- which(whHasSHAasBranch %in% TRUE)
        vs[wh] <- VersionOnRepos[wh]
        ineq[wh] <- "=="
      }
      list(vs, ineq)
    }, by = "Package"]
    pkgDT <- confirmEqualsDontViolateInequalitiesThenTrim(pkgDT, verbose = verbose)
  }
  pkgDT
}

#' @importFrom data.table rleid
trimRedundancies <- function(pkgInstall, repos, purge, libPaths, verbose = getOption("Require.verbose"),
                             type = getOption("pkgType")) {
  if (!is.null(pkgInstall[["hasHEAD"]])) {
    hasHeadRows <- which(pkgInstall[["hasHEAD"]] %in% TRUE)
    whToRM <- which(pkgInstall[["Package"]] %in% pkgInstall[hasHeadRows][["Package"]])
    whToRM <- setdiff(whToRM, hasHeadRows)
    if (length(whToRM))
      pkgInstall <- pkgInstall[-whToRM]
  }

  pkgAndInequality <- c("Package", "inequality")
  versionSpecNotNA <- !is.na(pkgInstall$versionSpec)
  if (any(versionSpecNotNA)) {
    ord <- order(package_version(pkgInstall$versionSpec[versionSpecNotNA]),
      decreasing = TRUE, na.last = TRUE
    ) # can't use setorderv because data.table can't sort on package_version class
  } else {
    ord <- seq(NROW(pkgInstall))
  }

  pkgInstallTmp <- list()
  if (any(versionSpecNotNA)) {
    pkgInstallTmp[[1]] <- pkgInstall[versionSpecNotNA][ord]
  } # use the order to reorder them. It is not sort key.
  if (any(!versionSpecNotNA)) {
    pkgInstallTmp[[2]] <- pkgInstall[versionSpecNotNA %in% FALSE]
  }
  pkgInstall <- rbindlist(pkgInstallTmp)
  set(pkgInstall, NULL, "versionSpecGroup", data.table::rleid(pkgInstall$versionSpec))
  setorderv(pkgInstall, c("Package", "versionSpecGroup", "inequality", "repoLocation"), order = c(1L, 1L, -1L, 1L), na.last = TRUE)

  pkgInstall[, keepBasedOnRedundantInequalities :=
    unlist(lapply(.I, function(ind) {
      ifelse(is.na(inequality), ind,
        ifelse(inequality == ">=", .I[1], ifelse(inequality == "<=", tail(.I, 1), .I))
      )
    })),
  by = pkgAndInequality
  ]
  pkgInstall <- pkgInstall[unique(keepBasedOnRedundantInequalities)]
  set(pkgInstall, NULL, "keepBasedOnRedundantInequalities", NULL)
  pkgInstall <- confirmEqualsDontViolateInequalitiesThenTrim(pkgInstall)
  pkgInstall[]
}


checkAvailableVersions <- function(pkgInstall, repos, purge, libPaths, verbose = getOption("Require.verbose"),
                             type = getOption("pkgType")) {

  pkgInstall <- getVersionOnRepos(pkgInstall, repos, purge, libPaths, type = type)
  # coming out of getVersionOnRepos, will be some with bin and src on windows; possibly different versions; take only first, if identical
  pkgInstall <- unique(pkgInstall, by = c("Package", "VersionOnRepos"))
  pkgInstall <- keepOnlyGitHubAtLines(pkgInstall, verbose = verbose)
  pkgInstall <- availableVersionOK(pkgInstall)
  pkgInstall[, binOrSrc := c("src", "bin")[grepl("\\<bin\\>", Repository) + 1]]
  setorderv(pkgInstall, c("Package", "availableVersionOKthisOne", "binOrSrc"), order = c(1L, -1L, 1L)) # OK = TRUE first, bin before src

  pkgInstall[, keep := {
    # This will pick the one that is OK, or if they are all NA (meaning no version spec), or all FALSE (meaning need to try Archive)
    #    then also pick first one that OK
    ok <- any(availableVersionOKthisOne %in% TRUE) || all(is.na(availableVersionOKthisOne)) || all(availableVersionOKthisOne %in% FALSE)
    if (ok) .I[ok][1] else .I
  }, by = "Package"]
  pkgInstall <- pkgInstall[unique(keep)]
  set(pkgInstall, NULL, "keep", NULL)
  pkgInstall
}

updateInstallSafeGroups <- function(pkgInstall) {
  set(
    pkgInstall, NULL, "installSafeGroups",
    as.integer(factor(paste0(
      paddedFloatToChar(pkgInstall$installSafeGroups,
        padL = max(nchar(pkgInstall$installSafeGroups), na.rm = TRUE)
      ), "_",
      # !pkgInstall[["isBinaryInstall"]],
      !pkgInstall[["localFile"]] %in% useRepository
    )))
  )
  data.table::setorderv(pkgInstall, c("installSafeGroups", "Package")) # alphabetical order
}

getArchiveDetails <- function(pkgArchive, ava, verbose, repos) {
  cols <- c("PackageUrl", "dayAfterPutOnCRAN", "dayBeforeTakenOffCRAN", "repo", "VersionOnRepos", "availableVersionOK")
  numGroups <- NROW(pkgArchive)

  # if (isWindows() || isMacOSX()) {
    messageVerbose("Identifying date range when package versions appear in Archive for ",
      NROW(pkgArchive), " packages",
      verbose = verbose,
      verboseLevel = 2
    )
  # }

  pkgArchive[, (cols) := {
    Version2 <- gsub(".*_(.*)\\.tar\\.gz", "\\1", ava[[Package]]$PackageUrl)
    if (length(Version2) > 0) {
      if (is.na(versionSpec)) { # Version2 is length 0 when the package has nothing available
        correctVersions <- NROW(ava[[Package]])
      } else {
        correctVersions <- compareVersion2(Version2, versionSpec, inequality)

        if (all(correctVersions %in% FALSE)) {
          correctVersions <- NA
        } else {
          latestCorrect <- try(tail(which(correctVersions), 1))
          if (is(latestCorrect, "try-error")) {
            browserDeveloper("Error 111; please contact developer")
          }
          correctVersions <- unique(c(latestCorrect, min(latestCorrect + 1, length(correctVersions))))
        }
      }
    } else {
      correctVersions <- NA
    }
    if (any(!is.na(correctVersions))) { # nothing on Archive that will fulfill the Version requirements
      if (length(correctVersions) == 1) correctVersions <- c(correctVersions, NA_integer_)
      earlyDate <- ava[[Package]][correctVersions[1]][["mtime"]] + secondsInADay
      ret <- ava[[Package]][correctVersions[1]][, c("PackageUrl", "mtime", "repo")]

      # if (isWindows() || isMacOSX()) { # relevant for RSPM
        messageVerbose(.GRP, " of ", numGroups, ": ", Package,
          verbose = verbose,
          verboseLevel = 2
        )
        if (is.na(correctVersions[2])) {
          dayBeforeTakenOffCRAN <- archivedOn(Package, verbose, repos,
            numGroups = numGroups,
            counter = .GRP,
            srcPackageURLOnCRAN, repo
          )
          dayBeforeTakenOffCRAN <- dayBeforeTakenOffCRAN[[1]]$archivedOn
        } else {
          dayBeforeTakenOffCRAN <- ava[[Package]][correctVersions[2]][["mtime"]]
        }

        set(ret, NULL, "dayBeforeTakenOffCRAN", dayBeforeTakenOffCRAN)
      #} else {
      #  set(ret, NULL, "dayBeforeTakenOffCRAN", NA_character_)
      #}
      setnames(ret, "mtime", "dayAfterPutOnCRAN")
      set(ret, NULL, "dayAfterPutOnCRAN", as.character(as.Date(ret$dayAfterPutOnCRAN)))
      set(ret, NULL, "dayBeforeTakenOffCRAN", as.character(as.Date(ret$dayBeforeTakenOffCRAN)))

      set(ret, NULL, "VersionOnRepos", Version2[correctVersions[1]])
      if (!is.na(correctVersions)[1]) {
        set(ret, NULL, "availableVersionOK", TRUE)
      }
    } else {
      ret <- mapply(x = cols, USE.NAMES = TRUE, function(x) NA_character_, SIMPLIFY = FALSE)
      ret <- as.data.table(ret)
      ret[, repo := ava[[Package]][1][, c("repo")]]
      set(ret, NULL, "availableVersionOK", FALSE)
    }
    data.table::setcolorder(ret, cols)
    ret
  }, by = "Package"]

  pkgArchive
}

parsePackageFullname <- function(pkgDT, sorted = TRUE) {
  set(pkgDT, NULL, "versionSpec", extractVersionNumber(pkgDT$packageFullName))
  # pkgDT[, versionSpec := extractVersionNumber(pkgDT$packageFullName)]
  wh <- which(!is.na(pkgDT$versionSpec))
  if (length(wh)) {
    set(pkgDT, wh, "inequality", extractInequality(pkgDT$packageFullName[wh]))
  } else {
    set(pkgDT, NULL, "inequality", NA_character_)
  }
  # pkgDT[!is.na(pkgDT$versionSpec), inequality := extractInequality(pkgDT$packageFullName)]
  if (isTRUE(sorted))
    setorderv(pkgDT, c("Package", "versionSpec"), order = c(1L, -1L), na.last = TRUE)
  pkgDT
}

removeBasePkgs <- function(pkgDT) {
  pkgDT[!Package %in% .basePkgs]
}

renameLocalGitPkgDT <- function(pkgInstall) {
  whGitHub2 <- pkgInstall$repoLocation %in% "GitHub"
  fns <- pkgInstall$localFile
  if (any(whGitHub2)) {
    pkgInstall[whGitHub2 %in% TRUE, localFile := {
      out <- basename(renameLocalGitTarWSHA(localFile, SHAonGH))
      keepOnlyBinary(out)
    }, by = seq(sum(whGitHub2))]
  }
}

renameLocalGitTarWSHA <- function(localFile, SHAonGH) {
  ##### Vectorized on localFile #####
  if (length(localFile)) {
    splitted <- lapply(basename(localFile), function(lf) strsplit(lf, "_")[[1]])
    newSHAname <- Map(spli = splitted, SHA = SHAonGH, function(spli, SHA) {
      paste0(spli[1], "-", SHA, "_", paste(spli[-1], collapse = "_"))
    })
    newSHAname <- unlist(file.path(dirname(localFile), newSHAname))
    fileRenameOrMove(localFile, newSHAname)
    out <- newSHAname
  } else {
    out <- ""
  }
  out
}

#' @importFrom stats na.omit
copyBuiltToCache <- function(pkgInstall, tmpdirs) {
  if (!is.null(pkgInstall)) {
    if (!is.null(getOptionRPackageCache())) {
      cacheFiles <- dir(getOptionRPackageCache())
      out <- try(Map(td = tmpdirs, function(td) {
        tdPkgs <- dir(td, full.names = TRUE)
        if (length(tdPkgs)) {
          pkgs <- Map(td = tdPkgs, function(td) strsplit(basename(td), split = "_")[[1]][1])
          pkgsInstalled <- pkgInstall[match(pkgs, Package)]
          isGitHub <- pkgsInstalled$repoLocation %in% "GitHub"
          if (any(isGitHub)) {
            SHA <- pkgsInstalled$SHAonGH[isGitHub]
            tdPkgs[isGitHub] <- renameLocalGitTarWSHA(tdPkgs[isGitHub], SHA)
          }

          whAlreadyInCache <- na.omit(match(
            cacheFiles,
            basename(tdPkgs)
          ))
          if (length(whAlreadyInCache)) {
            tdPkgs <- tdPkgs[-whAlreadyInCache]
          }
          if (length(tdPkgs)) {
            newFiles <- file.path(getOptionRPackageCache(), basename(tdPkgs))
            suppressWarnings(fileRenameOrMove(tdPkgs, newFiles))
          }
        }
      }))
      if (is(out, "try-error")) {
        browserDeveloper("Error 253; please contact developer")
      }
    }
  }
}

NoPkgsSupplied <- "No packages supplied"

anyHaveHEAD <- function(packages) {
  # Vectorize
  haveHead <- grepl(HEADgrep, packages)
  if (any(haveHead)) {
    origPackages <- packages
    packages[haveHead] <- trimVersionNumber(packages[haveHead])
    names(packages)[haveHead] <- origPackages[haveHead]
  }
  packages
}

HEADgrep <- " *\\(HEAD\\)"

packageFullNameFromSnapshot <- function(snapshot) {
  ifelse(!is.na(snapshot$GithubRepo) & nzchar(snapshot$GithubRepo),
    paste0(
      snapshot$GithubUsername, "/", snapshot$Package, "@",
      snapshot$GithubSHA1
    ), paste0(
      snapshot$Package,
      " (==", snapshot$Version, ")"
    )
  )
}

getVersionOnReposLocal <- function(pkgDT) {
  if (!is.null(getOptionRPackageCache())) {
    set(pkgDT, NULL, "tmpOrder", seq(NROW(pkgDT)))
    if (any(is.na(pkgDT$VersionOnRepos))) {
      pkgDTList <- split(pkgDT, !is.na(pkgDT$VersionOnRepos))
      if (!is.null(pkgDTList$`FALSE`)) {
        localFilesOuter <- dir(getOptionRPackageCache())
        pkgNoVoR <- pkgDTList$`FALSE`
        wh <- pkgNoVoR[["repoLocation"]] %in% "GitHub"
        if (any(wh)) {
          pkgNoVoR[which(wh %in% TRUE), localFile := {
            PackagePattern <- paste0("^", Package, ".*(\\_|\\-)+.*", Branch)
            localFiles <- grep(PackagePattern, localFilesOuter, value = TRUE) # can be length 0, 1, or >1 e.g., tar.gz, zip
            if (length(localFiles) == 0) {
              localFiles <- ""
            } else {
              isBinaryFile <- isBinary(localFiles)
              if (any(isBinaryFile %in% TRUE)) {
                localFiles <- localFiles[isBinaryFile]
              }
              if (length(localFiles) > 1) localFiles <- localFiles[1]
            }
            localFiles
          },
          by = seq(sum(wh))
          ]
          hasLocalNow <- (nchar(pkgNoVoR$localFile) > 0) %in% TRUE
          if (any(hasLocalNow %in% TRUE)) {
            pkgNoVoR[hasLocalNow, VersionOnRepos := extractVersionNumber(filenames = pkgNoVoR$localFile)]
            pkgNoVoR[hasLocalNow, haveLocal := "Local"]
          }
          pkgDT <- rbindlistRecursive(pkgDTList) # the pkgNoVoR was just a pointer
          setorderv(pkgDT, "tmpOrder")
          set(pkgDT, NULL, "tmpOrder", NULL)
        }
      }
    }
  }

  pkgDT
}

browserDeveloper <- function(mess = "") {
  if (identical(SysInfo[["user"]], "emcintir")) {
    # print(mess)
    # pf <- parent.frame()
    # attach(pf)
    # on.exit(detach(pf))
    print(mess)
    browser()
  } else {
    stop(mess)
  }
}

updateReposForSrcPkgs <- function(pkgInstall) {
  if (!isWindows() && !isMacOSX() && any(pkgInstall$isBinaryInstall & pkgInstall$localFile %in% useRepository)) {
    mayNeedSwitchToSrc <- pkgInstall$localFile %in% useRepository & pkgInstall$Package %in% sourcePkgs()
    pkgInstall[
      which(mayNeedSwitchToSrc),
      isBinaryInstall := isWindows() | isMacOSX()
    ]
    needSwitchToSrc <- mayNeedSwitchToSrc & pkgInstall$isBinaryInstall %in% FALSE
    if (any(needSwitchToSrc %in% TRUE)) {
      if (all(isBinaryCRANRepo(getOption("repos")))) {
        warning(
          paste(pkgInstall[needSwitchToSrc]$Package, collapse = ", "), " is identified in `sourcePkgs()`, ",
          "indicating it should normally be installed from source; however, there is no source CRAN repository.",
          "Please add one to the `options(repos)`, e.g., with ",
          "options(repos = c(getOption('repos'), CRAN = 'https://cloud.r-project.org')).",
          "Proceeding with the binary repository, which may not work"
        )
      } else {
        nonBinaryRepos <- getOption("repos")[!isBinaryCRANRepo(getOption("repos"))]
        whArchive <- pkgInstall$installFrom %in% "Archive"
        pkgInstall[whArchive %in% TRUE & needSwitchToSrc, Repository := getArchiveURL(nonBinaryRepos, Package)]

        # if there are multiple non-binary repos
        if (length(nonBinaryRepos) > 1) {
          packageExists <- FALSE
          for (ind in seq(nonBinaryRepos)) {
            nbrContrib <- contrib.url(nonBinaryRepos)
            pkgInstallNeededHere <- pkgInstall[!whArchive %in% TRUE & needSwitchToSrc]
            apTmp <- available.packages(contriburl = nbrContrib[ind])
            packageExists <- pkgInstallNeededHere$Package %in% apTmp[, "Package"]
            if (any(packageExists)) {
              pkgInstall[Package %in% pkgInstallNeededHere$Package[packageExists],
                         `:=`(Repository, nbrContrib[ind])]
              needSwitchToSrc <- mayNeedSwitchToSrc & pkgInstall$isBinaryInstall %in% FALSE
            }
            if (all(packageExists))
              break
          }
        } else {
          pkgInstall[!whArchive %in% TRUE & needSwitchToSrc, Repository := contrib.url(nonBinaryRepos)]
        }

      }
    }
  }
  pkgInstall
}

messagesAboutWarnings <- function(w, toInstall) {
  # This is a key error; cached copy is corrupt; this will intercept, delete it and reinstall all right here
  pkgName <- extractPkgNameFromWarning(w$message)
  outcome <- FALSE
  needWarning <- FALSE
  if (identical(pkgName, w$message)) { # didn't work
    pkgName <- gsub(".+\u2018(.+)\u2019.*", "\\1", w$message)
  }
  if (identical(pkgName, w$message)) { # didn't work again
    if (any(grepl("cannot open URL", pkgName))) { # means needs purge b/c package is on CRAN, but not that url
      url <- gsub(".+(https://.+\\.zip).+", "\\1", pkgName)
      url <- gsub(".+(https://.+\\.tar\\.gz).+", "\\1", url)
      url <- gsub(".+(https://.+\\.tgz).+", "\\1", url)
      pkgName <- extractPkgName(filenames = basename(url))

      try(dealWithCache(purge = TRUE, checkAge = FALSE))
      message("purging availablePackages; trying to download ", pkgName, " again")
      res <- try(Install(pkgName))
      if (is(res, "try-error")) {
        needWarning <- TRUE
      } else {
        outcome2 <- attr(res, "Require")
        if (identical("noneAvailable", outcome2$installResult)) {
          needWarning <- TRUE
        } else {
          needWarning <- FALSE
          outcome <- TRUE
          rowsInPkgDT <- grep(pkgName, toInstall$Package)
          if (!is.null(outcome2$installed))
            toInstall[rowsInPkgDT, installed := outcome2$installed]
          if (!is.null(outcome2$installResult))
            toInstall[rowsInPkgDT, installResult := outcome2$installResult]
        }
      }
    }

  }

  rowsInPkgDT <- grep(pkgName, toInstall$Package)
  if (length(rowsInPkgDT) && any(toInstall[rowsInPkgDT]$installed %in% FALSE)) {
    toInstall[rowsInPkgDT, installed := outcome]
    toInstall[rowsInPkgDT, installResult := w$message]

    if (any(grepl("cannot remove prior installation of package", w$message))) {
      warning("Is ", pkgName, " loaded in another R session? Please close all sessions before installing packages")
      needWarning <- FALSE
    } else {
      needWarning <- TRUE
    }
  }
  if (!is.null(getOptionRPackageCache())) {
    if (startsWith(pkgName, getOptionRPackageCache())) {
      messageVerbose(verbose = verbose, verboseLevel = 2, "Cached copy of ", basename(pkgName), " was corrupt; deleting; retrying")
      unlink(dir(getOptionRPackageCache(), pattern = basename(pkgName), full.names = TRUE)) # delete the erroneous Cache item
      retrying <- try(Require(toInstall[Package %in% basename(pkgName)]$packageFullName, require = FALSE))
      if (is(retrying, "try-error")) {
        needWarning <- TRUE
      }
    } else {
      needWarning <- TRUE
    }
  } else {
    needWarning <- TRUE
  }

  if (isTRUE(needWarning)) {
    warning(w)
  }
}

getArchiveURL <- function(repo, pkg) {
  file.path(contrib.url(repo), "Archive", pkg)
}

isGitHub <- function(pkg, filenames) {
  if (!missing(filenames)) {
    isGH <- grepl("-[[:alnum:]]{40}_", basename(filenames))
  } else {
    isGH <- extractPkgGitHub(pkg)
    isGH <- !is.na(isGH)
  }
  isGH
}

#' @rdname Require
#' @export
#' @details
#' `Install` is the same as `Require(..., require = FALSE)`, for convenience.
Install <- function(packages, packageVersionFile,
                    libPaths, # nolint
                    install_githubArgs = list(),
                    install.packagesArgs = list(INSTALL_opts = "--no-multiarch"),
                    standAlone = getOption("Require.standAlone", FALSE),
                    install = TRUE,
                    repos = getOption("repos"),
                    purge = getOption("Require.purge", FALSE),
                    verbose = getOption("Require.verbose", FALSE),
                    type = getOption("pkgType"),
                    upgrade = FALSE,
                    ...) {
  Require(packages,
    packageVersionFile,
    libPaths,
    install_githubArgs,
    install.packagesArgs,
    standAlone,
    install = install,
    require = FALSE,
    repos,
    purge,
    verbose,
    type,
    upgrade,
    ...
  )
}

rmPackageFirst <- function(toInstall, verbose) {
  cantUpdateDeps <- toInstall$Package %in% c("Require", "data.table")
  if (any(cantUpdateDeps))
    toInstall <- toInstall[-which(cantUpdateDeps)]
  cantUpdateLoaded <- toInstall$Package %in% loadedNamespaces()
  if (any(cantUpdateLoaded)) {
    toRm <- toInstall$Package[!cantUpdateLoaded]
    toInstall <- toInstall[which(!cantUpdateLoaded)]
    remove.packages(toRm)
    messageVerbose("removed all the packages that are being installed", verbose = verbose)
  }
  toInstall
}

downloadAndBuildToLocalFile <- function(Account, Repo, Branch, Package, GitSubFolder, verbose, VersionOnRepos) {
  gitRepo <- paste0(Account, "/", Repo, "@", Branch)
  names(gitRepo) <- Package
  out <- downloadRepo(gitRepo, subFolder = GitSubFolder,
                      overwrite = TRUE, destDir = ".", verbose = verbose)
  out1 <- try(build(Package, verbose = verbose, quiet = FALSE, VersionOnRepos = VersionOnRepos))
  fn <- dir(pattern = paste0("^", Package, "_.+tar.gz"))
  normPath(fn)
}


needRebuildAndInstall <- function(needRebuild, pkgInstall, libPaths, verbose, install.packagesArgs,
                                  repos, purge, startTime, type,
                                  pkgInstallTmp) {
  if (any(needRebuild)) {
    message("Trying to rebuild and install GitHub build fails... ")
    pkgInstall[, needRebuild := needRebuild]
    pkgInstallList <- split(pkgInstall, by = "needRebuild")
    names(pkgInstallList) <- c("No", "GitHub")
    pkgInstallList <- downloadGitHub(pkgInstallList, libPaths, verbose, install.packagesArgs)
    maxGroup <- 1
    numPackages <- NROW(pkgInstallList[["GitHub"]])
    pkgInstallList[["GitHub"]][, installOrder := seq(.N)] # renumber the installOrder
    pkgInstallList <- Map(
      toInstall = pkgInstallList["GitHub"],
      MoreArgs = list(
        repos = repos, purge = purge,
        install.packagesArgs = install.packagesArgs, numPackages = numPackages,
        numGroups = maxGroup, startTime = startTime, verbose = verbose, type = type
      ),
      installAll
    )
    pkgInstall <- rbindlist(list(pkgInstallTmp[!Package %in% pkgInstallList[["GitHub"]]$Package],
                                 pkgInstallList[["GitHub"]]), use.names = TRUE, fill = TRUE)
  }
  pkgInstall
}
