utils::globalVariables(c(
  "..apCachedCols", "..cols", "..cols3", ".GRP",
  "Additional_repositories", "Account", "Branch", "DESCFile",
  "EqualsDoesntViolate", "GitSubFolder", "Repo", "Repository", "SHAonGH", "Version",
  "VersionOnRepos", "availableVersionOKthisOne", "binOrSrc", "bothDepAndOrig", "comp",
  "depOrOrig", "getOptions", "GTDoesntViolate",
  "hasEqualsAndInequals", "hasGTAndInequals", "hasSubFolder", "hasVers", "hasVersionsToCompare",
  "haveLocal", "ineq", "i.VersionOnRepos", "installSafeGroups", "installed",
  "installedVersionOK", "isBinaryInstall", "isEquals", "isGT",
  "keepBasedOnRedundantInequalities", "loadOrder", "localFile", "needInstall",
  "PackageUrl", "repo", "oppositeInequals", "violationsDoubleInequals",
  "verbose", "VersionOK", "versionSpec", "versionToKeep",
  "violation", "violation2", "..keepCols1", "..colsToKeep",
  "forceInstall", "keepForUpdate", "SHAonLocal", "hasInequality",
  "i.localFile", "keep44", "keep55", "keepCols3",
  "keepCols4", "keepCols5", "mayNeedSwitchToSrc", "needKeep", "newLocalFile",
  "parentPackage", "whArchive"
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
#'   `getOption("Require.cachePkgDir")` whose default is `RPackageCache()`,
#'   meaning *cache packages locally in a project-independent location*, and
#'   will reuse them if needed. To turn off this feature, set
#'   `options("Require.cachePkgDir" = FALSE)`.
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
#' @param packages Either a character vector of packages to install via
#'   `install.packages`, then load (i.e., with `library`), or, for convenience,
#'   a vector or list (using `c` or `list`) of unquoted package names to install
#'   and/or load (as in `require`, but vectorized). Passing vectors of names may
#'   not work in all cases, so user should confirm before relying on this behaviour
#'   in operational code.
#'   In the case of a GitHub package, it
#'   will be assumed that the name of the repository is the name of the package.
#'   If this is not the case, then pass a *named* character vector here, where the
#'   names are the package names that could be different than the GitHub
#'   repository name.
#' @param packageVersionFile  Character string of a file name or logical. If
#'   `TRUE`, then this function will load the default file,
#'   `getOption("Require.packageVersionFile")`. If this argument is provided,
#'   then this will override any packages passed to `packages`. By default,
#'   `Require` will attempt to resolve dependency violations (i.e., if this
#'   `packageVersionFile` specifies a version of a package that violates
#'   the dependency specification of another package). If a user wishes to attempt
#'   to install the `packageVersionFile` without assessing the dependencies,
#'   set `dependencies = FALSE`.
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
#'   `Require` function, when `verbose >= 2`, also returns details as if
#'   `returnDetails = TRUE` (for backwards compatibility).
#' @param returnDetails Logical. If `TRUE` the return object will have an
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
#' opts <- Require:::.setupExample()
#'
#' library(Require)
#' getCRANrepos(ind = 1)
#' Require("utils") # analogous to require(stats), but it checks for
#' #   pkg dependencies, and installs them, if missing
#'
#' # unquoted version
#' Require(c(tools, utils))
#'
#' if (Require:::.runLongExamples()) {
#'   # Install in a new local library (libPaths)
#'   tempPkgFolder <- file.path(tempdir(), "Require/Packages")
#'   # use standAlone, means it will put it in libPaths, even if it already exists
#'   #   in another local library (e.g., personal library)
#'   Install("crayon", libPaths = tempPkgFolder, standAlone = TRUE)
#'
#'   # Mutual dependencies, only installs once -- e.g., cli
#'   tempPkgFolder <- file.path(tempdir(), "Require/Packages")
#'   Install(c("cli", "R6"), libPaths = tempPkgFolder, standAlone = TRUE)
#'
#'   # Mutual dependencies, only installs once -- e.g., rlang
#'   tempPkgFolder <- file.path(tempdir(), "Require/Packages")
#'   Install(c("rlang", "ellipsis"), libPaths = tempPkgFolder, standAlone = TRUE)
#'
#'   #####################################################################################
#'   # Isolated projects -- Use a project folder and pass to libPaths or set .libPaths() #
#'   #####################################################################################
#'   # GitHub packages
#'   if (requireNamespace("gitcreds", quietly = TRUE)) {
#'     #if (is(try(gitcreds::gitcreds_get(), silent = TRUE), "gitcreds")) {
#'       ProjectPackageFolder <- file.path(tempdir(), "Require/ProjectA")
#'       if (requireNamespace("curl")) {
#'         Require("PredictiveEcology/fpCompare@development",
#'           libPaths = ProjectPackageFolder,
#'         )
#'       }
#'
#'       # No install because it is there already
#'       Install("PredictiveEcology/fpCompare@development",
#'         libPaths = ProjectPackageFolder,
#'       ) # the latest version on GitHub
#'
#'       ############################################################################
#'       # Mixing and matching GitHub, CRAN, with and without version numbering
#'       ############################################################################
#'       pkgs <- c(
#'         "remotes (<=2.4.1)", # old version
#'         "digest (>= 0.6.28)", # recent version
#'         "PredictiveEcology/fpCompare@a0260b8476b06628bba0ae73af3430cce9620ca0" # exact version
#'       )
#'       Require::Require(pkgs, libPaths = ProjectPackageFolder)
#'     #}
#'   }
#'   Require:::.cleanup(opts)
#' }
#' }
#'
Require <- function(packages,
                    packageVersionFile,
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
                    returnDetails = FALSE,
                    ...) {

  st <- Sys.time()
  if (is.null(require)) require <- FALSE
  assign("hasGHP", NULL, envir = pkgEnv()) # clear GITHUB_PAT message; only once per Require session
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

  verboseSpecial <- verbose < 5
  install.packagesArgs <- modifyList2(list(quiet = !(verbose >= 1 && verboseSpecial)), install.packagesArgs,
                                      dots,
                                      keep.null = TRUE
  )
  install_githubArgs <- modifyList2(list(quiet = !(verbose >= 0)  && verboseSpecial), install_githubArgs,
                                    dots,
                                    keep.null = TRUE
  )
  install.packagesArgs <- modifyList2(
    install.packagesArgs,
    list(destdir = ".", repos = repos, type = types())
  )

  libPaths <- doLibPaths(libPaths, standAlone = standAlone)
  libPaths <- checkLibPaths(libPaths = libPaths, exact = TRUE, ...)

  doDeps <- if (!is.null(list(...)$dependencies)) list(...)$dependencies else NA
  which <- whichToDILES(doDeps)

  if (!missing(packageVersionFile)) {
    if (isFALSE(packageVersionFile)) {
      messageVerbose(NoPkgsSupplied, verbose = verbose, verboseLevel = 1)
    }
    opts2 <- getOption("Require.usePak")# ; on.exit(options(opts2), add = TRUE)
    if (isTRUE(opts2))
      warning(.txtPakCurrentlyPakNoSnapshots, "; \n",
              "if problems occur, set `options(Require.usePak = FALSE)`")
    pkgSnapshotOut <- doPkgSnapshot(packageVersionFile, purge, libPaths = libPaths,
                                    install_githubArgs, install.packagesArgs, standAlone,
                                    type = type, verbose = verbose, returnDetails = returnDetails, ...
    )
    return(invisible(pkgSnapshotOut))
  }
  if (missing(packages)) {
    messageVerbose(NoPkgsSupplied, verbose = verbose, verboseLevel = 1)
    return(invisible(NULL))
  }

  packagesSubstituted <- substitute(packages) # can be c(xx), list(xx), "hi", a$b is a call, but it is likely already evaluated
  packages <- substitutePackages(packagesSubstituted, envir = parent.frame())

  hasInitSlash <- grepl("^\\\"", packages)
  if (any(hasInitSlash))
    packages[hasInitSlash] <- gsub("\\\"", "", packages[hasInitSlash])

  # Proceed to evaluate install and load need if there are any packages
  packagesOrig <- packages
  if (NROW(packages)) {
    repos <- getCRANrepos(repos, ind = 1)

    basePkgsToLoad <- packages[packages %in% .basePkgs]

    if (getOption("Require.usePak", TRUE)) {
      opts <- options(repos = repos); on.exit(options(opts), add = TRUE)

      log <- tempfile2(fileext = ".txt")
      withCallingHandlers(
        pkgDT <- pakRequire(packages, libPaths, doDeps, upgrade, verbose = verbose, packagesOrig)
        , message = function(m) {
          if (verbose > 1)
            cat(m$message, file = log, append = TRUE)
          if (verbose < 1)
            invokeRestart("muffleMessage")
        }
      )

    } else {


      if (length(which)) {
        deps <- pkgDep(packages, simplify = FALSE,
                       purge = purge, libPaths = libPaths, recursive = TRUE,
                       which = which, type = type, verbose = verbose, repos = repos,
                       Additional_repositories = TRUE
        )
        deps2 <- rbindlist(deps$deps, fill = TRUE, use.names = TRUE)
        # If there were archives in pkgDep, it had to go get them, so it has them locally, don't want to waste them
        if (!is.null(deps[["localFile"]])) {
          deps2 <- deps2[deps[, c("packageFullName", "newLocalFile", "localFile")], on = "packageFullName"]
        }
        deps <- unique(deps2)
        # allPackages <- sort(unique(unname(unlist(deps[["packageFullName"]]))))
        pkgDT <- deps
      } else {
        pkgDT <- toPkgDTFull(packages)
      }

      if (NROW(pkgDT)) {
        pkgDT <- checkHEAD(pkgDT)

        pkgDT <- confirmEqualsDontViolateInequalitiesThenTrim(pkgDT)

        # pkgDT <- toPkgDT(allPackages, deepCopy = TRUE)
        # if (!is.null(deps$Additional_repositories))
        #  pkgDT <- deps[!is.na(Additional_repositories)][pkgDT, on = "packageFullName"]
        pkgDT <- updatePackagesWithNames(pkgDT, packages)
        # pkgDT <- parsePackageFullname(pkgDT)
        # pkgDT <- parseGitHub(pkgDT)
        # pkgDT <- removeDups(pkgDT)
        # pkgDT <- removeBasePkgs(pkgDT)
        pkgDT <- recordLoadOrder(packages, pkgDT)
        if (!is.null(pkgDT[["Version"]]))
          setnames(pkgDT, old = "Version", new = "VersionOnRepos")
        pkgDT <- installedVers(pkgDT, libPaths = libPaths)
        if (isTRUE(upgrade)) {
          pkgDT <- getVersionOnRepos(pkgDT, repos = repos, purge = purge, libPaths = libPaths)
          if (any(pkgDT[["VersionOnRepos"]] != pkgDT[["Version"]], na.rm = TRUE)) {
            sameVersion <- compareVersion2(pkgDT[["VersionOnRepos"]], pkgDT[["Version"]], "==")
            pkgDT[!sameVersion, comp := compareVersion2(VersionOnRepos, Version, ">=")]
            pkgDT[!sameVersion & comp %in% TRUE,
                  `:=`(Version = NA, installed = FALSE, versionSpec = VersionOnRepos)]
            set(pkgDT, NULL, "comp", NULL)
          }
        }
        pkgDT <- dealWithStandAlone(pkgDT, libPaths, standAlone)
        pkgDT <- whichToInstall(pkgDT, install, verbose)

        pkgDT <- removeRequireDeps(pkgDT, verbose)

        # Deal with "force" installs
        set(pkgDT, NULL, "forceInstall", FALSE)
        if (install %in% "force") {
          wh <- which(pkgDT$Package %in% extractPkgName(packages))
          set(pkgDT, wh, "installedVersionOK", FALSE)
          set(pkgDT, wh, "forceInstall", FALSE)
        }

        needInstalls <- (any(pkgDT$needInstall %in% .txtInstall) && (isTRUE(install))) || install %in% "force"
        if (needInstalls) {
          pkgDT <- doInstalls(pkgDT,
                              repos = repos, purge = purge, libPaths = libPaths,
                              install.packagesArgs = install.packagesArgs,
                              type = type, returnDetails = returnDetails,
                              verbose = verbose
          )
        } else {
          messageVerbose("No packages to install/update", verbose = verbose)
        }
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

    whRestartNeeded <- which(grepl("restart", pkgDT$installResult))
    if  (length(whRestartNeeded)) {
      warning(.txtPleaseRestart, "; ", paste(pkgDT[whRestartNeeded]$Package, collapse = ", "),
              singularPlural(c(" was", " were"), l = whRestartNeeded), " already loaded and ",
              paste(pkgDT[whRestartNeeded]$packageFullName, collapse = ", "),
              singularPlural(c(" was", " were"), l = whRestartNeeded), " installed.")

    }

    # This only has access to "trimRedundancies", so it cannot know the right answer about which was loaded or not
    out <- doLoads(require, pkgDT, libPaths = libPaths, verbose = verbose)

    packagesDT <- matchWithOriginalPackages(pkgDT, packages)
    out <- packagesDT$require
    names(out) <- packagesDT[["packageFullName"]]

    if (verbose >= 2 || returnDetails %in% TRUE) {
      if (is.null(require)) {
        out <- character()
      }
      attr(out, "Require") <- pkgDT[]
    }
  } else {
    out <- logical()
  }

  et <- Sys.time()
  et <- difftime(et, st)
  numPacksInstalled <- NROW(pkgDT$installed[(pkgDT$installed %in% TRUE | is.na(pkgDT$installed)) &
                                              (pkgDT$needInstall %in% .txtInstall | is.na(pkgDT$installed)) ])
  if (numPacksInstalled > 0)
    messageVerbose(paste0("Installed ", numPacksInstalled,
                          " packages in "),
                   round(et, 1), " ", attr(et, "units"), verbose = verbose)

  noneAv <- pkgDT$installResult %in% .txtNoneAvailable
  if (isTRUE(any(noneAv))) {
    warning(messageCantInstallNoVersion(
      paste(pkgDT[["packageFullName"]][which(noneAv)], collapse = ", ")))
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

rbindlistNULL <- function(ll, ...) {
  theNulls <- sapply(ll, is.null)
  if (sum(theNulls) == 1)
    return(ll[[which(!theNulls)]])
  else
    rbindlist(ll, ...)
}


#' @importFrom sys exec_wait exec_background
build <- function(Package, VersionOnRepos, verbose, quiet) {
  if (nchar(Sys.which("R")) > 0) {
    messageVerbose("building package (R CMD build)",
                   verbose = verbose, verboseLevel = 2
    )
    # internal <- !interactive()
    extras <- c(
      "--no-resave-data", "--no-manual",
      "--no-build-vignettes"
    )
    Rpath1 <- Sys.getenv("R_HOME")
    Rpath <- file.path(Rpath1, "bin/R") # need to use Path https://stat.ethz.ch/pipermail/r-devel/2018-February/075507.html
    filePat <- paste0(Package, "_", VersionOnRepos, ".tar.gz")
    logFile <- tempfile2(fileext = ".log")
    out1 <- Map(pack = Package, function(pack) {
      # R CMD build can't handle DESCRIPTION with blank lines -- this is like
      #   https://github.com/r-lib/devtools/pull/439
      descFile <- file.path(Package, "DESCRIPTION")
      a <- readLines(descFile)
      if (any(nchar(a)))
        writeLines(a[nzchar(a)], descFile)
      # cmdLine <- paste("CMD build ", pack, paste(extras, collapse = " "))
      cmdLine <- c("CMD", "build", pack, extras)
      if (getOption("Require.installPackagesSys", 0L)) {

        pid <- sys::exec_wait(
          Rpath, I(cmdLine), # std_out = con, std_err = con
          std_out = function(x) {
            mess <- rawToChar(x)

            msgStdOutForBuild(mess, logFile, verbose)
            # pkg <- extractPkgNameFromWarning(mess)
            # cat(blue(mess), file = logFile, append = TRUE)
            # appendLF <- endsWith(mess, "\n") %in% FALSE
            # if (verbose >= 2) {
            #   messageVerbose(blue(mess), verbose = verbose, appendLF = appendLF)
            # } else {
            #   if (grepl("building", mess)) {
            #     mess <- gsub(".+(building.+)", "\\1", mess)
            #     messageVerbose(blue(mess), appendLF = appendLF)
            #   }
            # }

            #
            invisible()
          },
          std_err = function(x){
            mess <- rawToChar(x)
            msgStdErrForBuild(mess, logFile, verbose)
            # cat(greyLight(mess), file = logFile, append = TRUE)
            # appendLF <- endsWith(mess, "\n") %in% FALSE
            # if (verbose <= 1) {
            #   appendLF <- endsWith(mess, "\n") %in% FALSE
            #   messageVerbose(greyLight(mess), verbose = verbose, appendLF = appendLF)
            # } else {
            #   messageVerbose(greyLight(mess), verbose = verbose, appendLF = appendLF)
            # }

            invisible()
          }
        )
        tools::pskill(pid)
      } else {
        ver <- ifelse(installPackageVerbose(verbose, verboseLevel = 1), "", FALSE)
        system2(Rpath, cmdLine,
                stdout = ver,
                stderr = ver)
      }
    })
    messageVerbose("\b\b ... Built!",
                   verbose = verbose, verboseLevel = 1
    )
  } else {
    stop("Can't install packages this way because R is not on the search path")
  }
}


installAll <- function(toInstall, repos = getOptions("repos"), purge = FALSE, install.packagesArgs,
                       numPackages, numGroups, startTime, type = type, returnDetails,
                       tmpdir = tempdir(), libPaths,
                       verbose = getOption("Require.verbose")) {

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

  # for (i in 1:2) {
  (ap <- availablePackagesOverride(toInstall, repos, purge, type = type, verbose = verbose) )

  if (is(ap, "try-error")) {
    browserDeveloper("Error 9566")
  }

  # "repos" is interesting -- must be NULL, not just unspecified, for Local; must be unspecified or specified for Archive & CRAN
  #  This means that we can't get parallel installs for GitHub or Cache
  install.packagesArgs <- modifyList2(install.packagesArgs, list(destdir = NULL), keep.null = TRUE)
  ipa <- modifyList2(install.packagesArgs,
                     list(pkgs = toInstall[["Package"]], available = ap, type = type, dependencies = FALSE),
                     keep.null = TRUE
  )

  rcf <- getOption("Require.cloneFrom")
  if (isTRUE(is.character(rcf))) {
    ipa <- clonePackages(rcf, ipa, libPaths = libPaths, verbose)
  }

  if (NROW(ipa$available)) {
    ipa$destdir <- tmpdir
    ipa$lib <- libPaths[1]

    on.exit({
      logFile <- if (exists("toInstallOut", inherits = FALSE)) toInstallOut else NULL
      rmErroredPkgInstalls(logFile = logFile, toInstall, verbose)
    },
    add = TRUE)
    tries <- seq(1, 3)
    for (attempt in tries) {
      toInstallOut <- try(withCallingHandlers(
        installPackagesWithQuiet(ipa, verbose = verbose),
        warning = function(w) {
          messagesAboutWarnings(w, toInstall, returnDetails = returnDetails,
                                tmpdir = tmpdir, verbose = verbose) # changes to toInstall are by reference; so they are in the return below
          invokeRestart("muffleWarning") # muffle them because if they were necessary, they were redone in `messagesAboutWarnings`
        }
      ))
      if (!is(toInstallOut, "try-error"))
        break
      ipa <- recoverFromFail(toInstallOut, toInstall, ipa,
                             attempt = attempt, tries = tries,
                             repos = repos, tmpdir = tmpdir, libPaths = libPaths,
                             verbose, returnDetails)
    }
  }
  toInstall
}

doInstalls <- function(pkgDT, repos, purge, libPaths, install.packagesArgs,
                       type = getOption("pkgType"), returnDetails, verbose) {
  tmpdir <- tempdir3() # do all downloads and installs to here; then copy to Cache, if used

  pkgDTList <- split(pkgDT, by = c("needInstall"))
  if (NROW(pkgDTList[[.txtInstall]])) {
    pkgInstall <- pkgDTList[[.txtInstall]] # pointer

    on.exit(
      {
        # this copies any tar.gz files to the package cache; works even if partial install.packages
        tmpdirPkgs <- file.path(tempdir(), "downloaded_packages") # from CRAN installs
        if (length(dir(c(tmpdir, tmpdirPkgs)))) {
          copyBuiltToCache(pkgInstall, tmpdirs = c(tmpdir, tmpdirPkgs))
        }
        suppressWarnings(try(postInstallDESCRIPTIONMods(pkgInstall, libPaths), silent = TRUE)) # CRAN is read only after pkgs installed
        unlink(tmpdir, recursive = TRUE)
      },
      add = TRUE
    )

    #if (!getOption("Require.cachePkgDir") %in% FALSE)
    #  wd <- cachePkgDir()
    #else
    wd <- tmpdir

    # problem... building in cache dir has nice feature that zip is there immediately ...
    #   so when say 100 pkgs are being installed from source, the zips are built along
    #   the way, so if it crashes after 90 installs, the zips are in the Cache
    #   BUT -- when a GH package gets built here, it has to get the "simple" name without SHA
    #   but that means that it will collide with the same package from CRAN with same version
    #   number ... BUT, we may want to keep them separate e.g.,
    #  fpCompare-a0260b8476b06628bba0ae73af3430cce9620ca0_0.2.4.tar.gz
    #  is built to fpCompare_0.2.4.zip ... so GH packages should be build in tmpdir, so they can
    #  be renamed to fpCompare-a0260b8476b06628bba0ae73af3430cce9620ca0_0.2.4.zip afterwards,
    #  moved to cache, without colliding with a CRAN package fpCompare_0.2.4.zip ... obviously
    #  these could be the same, based on version number, but we shouldn't assume that

    origDir <- setwd(wd) # this is where zip gets built
    on.exit(setwd(origDir), add = TRUE)

    # needInstall can switch from "install" to "dontInstall" inside because of "HEAD", which gets compared to current
    pkgInstall <- doDownloads(pkgInstall,
                              repos = repos, purge = purge, verbose = verbose,
                              install.packagesArgs = install.packagesArgs, libPaths = libPaths,
                              type = type, tmpdir = tmpdir
    )

    pkgInstallList <- split(pkgInstall, by = c("needInstall"))
    for (i in setdiff(names(pkgInstallList), .txtDontInstall))
      pkgDTList[[i]] <- pkgInstallList[[i]]
    if (!is.null(pkgInstallList[[.txtDontInstall]]))
      pkgDTList[[.txtDontInstall]] <- rbindlist(list(pkgDTList[[.txtDontInstall]], pkgInstallList[[.txtDontInstall]]),
                                                fill = TRUE, use.names = TRUE)
    if (NROW(pkgDTList[[.txtInstall]])) {
      pkgInstallList <- split(pkgInstall, by = "needInstall") # There are now ones that can't be installed b/c .txtNoneAvailable
      pkgInstall <- pkgInstallList[[.txtInstall]]
      if (!is.null(pkgInstallList[[.txtNoneAvailable]])) {
        messageVerbose(messageCantInstallNoVersion(pkgInstallList[[.txtNoneAvailable]][["packageFullName"]]),
                       verbose = verbose, verboseLevel = 1)
      }
      if (!is.null(pkgInstallList[[.txtShaUnchangedNoInstall]])) {
        messageVerbose(.txtShaUnchangedNoInstall, ": ", pkgInstallList[[.txtShaUnchangedNoInstall]][["packageFullName"]],
                       verbose = verbose, verboseLevel = 1)
      }

      if (isMacOSX() && "covr" %in% pkgInstall$Package)
        print(pkgInstall)

      if (!is.null(pkgInstall)) {
        pkgInstall[, isBinaryInstall := isBinary(localFile, needRepoCheck = FALSE)] # filename-based
        pkgInstall[localFile %in% useRepository, isBinaryInstall := isBinaryCRANRepo(Repository)] # repository-based
        pkgInstall <- updateReposForSrcPkgs(pkgInstall, verbose = verbose)

        startTime <- Sys.time()

        # The install
        pkgInstall[, installSafeGroups := 1L]
        if (isWindows() || isMacOSX()) {
          pkgInstall[, installSafeGroups := (isBinaryInstall %in% FALSE) + 1L]
          pkgInstall <- updateInstallSafeGroups(pkgInstall)
        }

        maxGroup <- max(pkgInstall[["installSafeGroups"]])
        numPackages <- NROW(pkgInstall)
        setorderv(pkgInstall, c("installSafeGroups", "Package"))
        pkgInstall[, installOrder := seq(.N)]

        if (isWindows())
          toInstallList <- split(pkgInstall, by = "installSafeGroups")
        else
          toInstallList <- list(pkgInstall)

        # The INSTALL
        toInstallList <-
          Map(
            toInstall = toInstallList,
            MoreArgs = list(
              repos = repos, purge = purge,
              install.packagesArgs = install.packagesArgs, numPackages = numPackages,
              numGroups = maxGroup, startTime = startTime, type = type,
              returnDetails = returnDetails,
              libPaths = libPaths,
              tmpdir = tmpdir, verbose = verbose
            ),
            installAll
          )
        pkgInstallTmp <- rbindlistRecursive(toInstallList)
        needRebuild <- startsWith(basename(pkgInstall$localFile), "NeedRebuild")
        if (any(needRebuild)) {
          pkgInstall <- needRebuildAndInstall(needRebuild = needRebuild, pkgInstall = pkgInstall,
                                              libPaths = libPaths,
                                              install.packagesArgs = install.packagesArgs,
                                              repos = repos, purge = purge, startTime = startTime, type = type,
                                              pkgInstallTmp = pkgInstallTmp,
                                              tmpdir = tmpdir, verbose = verbose)
        } else {
          pkgInstall <- pkgInstallTmp
        }

        addOK <- if (!is.null(pkgInstall[["installResult"]])) {
          which(is.na(pkgInstall[["installResult"]]))
        } else {
          seq_len(NROW(pkgInstall)) # was NULL, but that is "all rows" in data.table
        }

        set(pkgInstall, addOK,
            c("installed", "Version", "LibPath"),
            list(TRUE, pkgInstall[["VersionOnRepos"]][addOK], libPaths[1])
        )
        pkgInstall <- appendInstallResult(pkgInstall, addOK, installResult = "OK")
        pkgInstallList[[.txtInstall]] <- pkgInstall
      }
      if (!is.null(pkgInstallList[[.txtNoneAvailable]])) {
        pkgInstallList[[.txtNoneAvailable]] <-
          appendInstallResult(pkgInstallList[[.txtNoneAvailable]], seq_len(NROW(pkgInstallList[[.txtNoneAvailable]])),
                              pkgInstallList[[.txtNoneAvailable]]$needInstall)
        set(pkgInstallList[[.txtNoneAvailable]], NULL, "installed", FALSE)
      }

      # pkgInstallList can have install and .txtNoneAvailable
      pkgDTList <- append(pkgDTList[.txtDontInstall], pkgInstallList)
    }
  }
  pkgDT <- rbindlistRecursive(pkgDTList)
}




secondsInADay <- 3600 * 24

urlForArchivedPkgs <- "https://packagemanager.posit.co/cran/"
urlForPositPACKAGES <- file.path(urlForArchivedPkgs, "latest")
# urlForArchivedPkgs <- "https://packagemanager.rstudio.com/cran/"
# urlForArchivedPkgs <- "https://MRAN.revolutionanalytics.com/snapshot"

archivedOn <- function(possiblyArchivedPkg, pkgRelPath, verbose, repos, numGroups, counter,
                       srcPackageURLOnCRAN, repo) {
  Map(
    pk = possiblyArchivedPkg, prp = pkgRelPath, counter = counter, USE.NAMES = TRUE,
    function(pk, prp, counter) {
      uu <- url(paste0("https://cran.r-project.org/package=", pk))
      on.exit(try(close(uu), silent = TRUE))
      rl <- suppressWarnings(try(readLines(uu), silent = TRUE))
      close(uu)
      wasRemoved <- any(grepl("was removed from the CRAN repository", rl))

      archivedOn <- ""
      if (wasRemoved) {
        archivedOn <- grep("Archived on", rl, value = TRUE)
        if (length(archivedOn)) {
          archivedOn <- as.POSIXct(gsub("Archived on (.+) (.)+", "\\1", archivedOn))
          PackageUrl <- prp
        } else {
          # some CRAN repos e.g., RStudioPackage Manager is not a full CRAN mirror; try all repos
          if (all(isBinaryCRANRepo(repos))) {
            repos <- c(repos, CRAN = srcPackageURLOnCRAN)
          }
          for (repo in repos) {
            pp <- if (grepl(urlForArchivedPkgs, repo)) prp else pk
            yy <- url(getArchiveURL(repo, pp)) # posit.co won't return something at repo, pk --> needs prp


            on.exit(try(close(yy), silent = TRUE))
            rl2 <- suppressWarnings(try(readLines(yy), silent = TRUE))
            close(yy)
            suppressWarnings(lineWDateAndPkgFilename <- tail(grep(paste0(pk, ".*tar.gz"), rl2, value = TRUE), 1))
            if (!is(rl2, "try-error")) {
              if (length(lineWDateAndPkgFilename) == 0) {
                next
              }
              if (length(repos) > 1 && !identical(repo, repos[1])) {
                messageVerbose("Found archive at: ", repo, verbose = verbose, verboseLevel = 2)
              }
              break
            }
            messageVerbose("Could not get ", pk, " at ", repo, verbose = verbose, verboseLevel = 2)
            if (length(repos) > 1) {
              messageVerbose("; trying next CRAN repo", verbose = verbose, verboseLevel = 2)
            }
          }


          pkgFilename <- gsub(paste0(".+(", pk, "_.+tar.gz).+.+"), "\\1", lineWDateAndPkgFilename)
          PackageUrl <- file.path(pk, pkgFilename)
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
  set(pkgDT, NULL, "isPkgInstalled", !is.na(pkgDT[["Version"]]))
  set(pkgDT, NULL, "installedVersionOK", !is.na(pkgDT[["Version"]])) # default: if it is installed,  say "OK"
  if (!is.null(pkgDT[["hasHEAD"]])) {
    whHasHead <- which(pkgDT[["hasHEAD"]] %in% TRUE)
    set(pkgDT, whHasHead, c("installedVersionOK", "isPkgInstalled"), list(FALSE, FALSE))
  }
  set(
    pkgDT, NULL, "hasVersionsToCompare",
    (nchar(pkgDT[["inequality"]]) > 0) %in% TRUE & !is.na(pkgDT[["Version"]])
  )
  if (any(pkgDT$hasVersionsToCompare %in% TRUE)) {
    pkgDT[hasVersionsToCompare %in% TRUE, installedVersionOK :=
            compareVersion2(Version, versionSpec, inequality)#,
          #by = seq(sum(hasVersionsToCompare))
    ]
  }

  pkgDT <- checkHEAD(pkgDT)
  if (any(pkgDT[["hasHEAD"]])) {
    set(pkgDT, which(pkgDT[["hasHEAD"]]), "installedVersionOK", FALSE)
  }

  set(pkgDT, NULL, "needInstall", c(.txtDontInstall, .txtInstall)[pkgDT$installedVersionOK %in% FALSE + 1])
  whDontInstall <- pkgDT[["needInstall"]] %in% .txtDontInstall
  if (any(whDontInstall & pkgDT$installedVersionOK %in% FALSE)) {
    messageVerbose(messageCantInstallNoVersion(pkgDT[["packageFullName"]][whDontInstall]),
                   verbose = verbose, verboseLevel = 1)
  }
  if (identical(install, "force")) {
    askedByUser <- !is.na(pkgDT$loadOrder)
    set(pkgDT, which(askedByUser), "needInstall", .txtInstall)
  }

  pkgDT
}


doLoads <- function(require, pkgDT, libPaths, verbose = getOption("Require.verbose")) {
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
    out[[1]] <- mapply(x = unique(pkgDT[["Package"]][pkgDT$require %in% TRUE]), function(x) {
      base::require(x, lib.loc = libPaths, character.only = TRUE, quietly = verbose <= 0)
    }, USE.NAMES = TRUE)
  }

  if (any(pkgDT$require %in% FALSE)) {
    out[[2]] <- mapply(x = pkgDT[["Package"]][pkgDT$require %in% FALSE], function(x) FALSE, USE.NAMES = TRUE)
  }
  out <- do.call(c, out)
  out[na.omit(pkgDT[["Package"]][!is.na(pkgDT$loadOrder)])] # put in order, based on loadOrder

  out
}

recordLoadOrder <- function(packages, pkgDT) {
  dups <- duplicated(packages)
  if (any(dups)) {
    packages <- packages[!dups]
  }
  packagesWOVersion <- trimVersionNumber(packages)
  packagesWObase <- setdiff(packagesWOVersion, .basePkgs)
  pfn <- trimVersionNumber(pkgDT[["packageFullName"]])
  wh <- pfn %in% packagesWObase
  out <- try(pkgDT[wh, loadOrder := seq(sum(wh))])
  pkgDT[, loadOrder := na.omit(unique(loadOrder))[1], by = "Package"]

  if (is(out, "try-error")) {
    browserDeveloper("Error 1253; please contact developer")
  }
  pkgDT
}

removeDups <- function(pkgDT) {
  dups <- duplicated(pkgDT[["packageFullName"]])
  pkgDT[!dups]
}

dealWithStandAlone <- function(pkgDT, libPaths, standAlone) {
  if (isTRUE(standAlone)) {
    # Remove any packages that are not in libPaths[1], i.e., the main R library
    notInLibPaths1 <- (!pkgDT[["Package"]] %in% .basePkgs) &
      (!normPath(pkgDT$LibPath) %in% normPath(libPaths)) & !is.na(pkgDT$LibPath)
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
                        libPaths, tmpdir, type = getOption("pkgType")) {
  pkgInstall[, installSafeGroups := 1L]

  # this is a placeholder; set noLocal by default
  set(pkgInstall, NULL, "haveLocal", .txtNoLocal)

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
                          # if (requireNamespace("remotes")) {
                          #   # THIS WILL TRY `remotes` TO GET THE CORRECT REASON FOR THE ERROR -- but will still error
                          #   pkgInstallJustGitHub <- pkgInstall[pkgInstall$repoLocation %in% .txtGitHub]
                          #   lapply(seq(NROW(pkgInstallJustGitHub)),
                          #          function(ind) {
                          #            pijg <- pkgInstallJustGitHub[ind]
                          #            thisOne <- any(grepl(paste(pijg$Account, pijg$Repo, sep = ".+"), e))
                          #            if (isTRUE(thisOne))
                          #              remotes::install_github(pijg[["packageFullName"]])
                          #          })
                          #
                          # } else {
                          stop(e, "\nDoes the url exist? ",
                               "Are you not using a GITHUB_PAT? Do you have an old GITHUB_PAT?\n",
                               "For better error handling, install.packages('remotes')")
                          # }
                        })
  naRepository <- is.na(pkgInstall$Repository)
  if (any(naRepository)) {
    if (!is.null(pkgInstall$Additional_repositories))
      pkgInstall[naRepository, Repository := Additional_repositories]
  }

  # Will set `haveLocal = .txtLocal` and `installFrom = .txtLocal`
  pkgInstall <- identifyLocalFiles(pkgInstall, repos, purge, libPaths = libPaths, verbose = verbose)
  pkgInstallList <- split(pkgInstall, by = "haveLocal")
  if (!is.null(pkgInstallList$Local)) {
    # could be duplicated if repos had multiple versions of the package; only need 1 from local installed
    pkgInstallList$Local <- pkgInstallList$Local[, .SD[availableVersionOK %in% TRUE][1], by = "Package"]
  }

  pkgNeedInternet <- pkgInstallList[[.txtNoLocal]] # pointer
  numToDownload <- NROW(pkgInstallList[[.txtNoLocal]])

  if (NROW(pkgNeedInternet)) {
    pkgNeedInternet <- split(pkgNeedInternet, by = "repoLocation")

    # CRAN
    pkgNeedInternet <- downloadCRAN(pkgNeedInternet, repos, purge, install.packagesArgs,
                                    verbose, numToDownload,
                                    type = type, tmpdir = tmpdir
    )

    # Archive
    pkgNeedInternet <- downloadArchive(
      pkgNeedInternet, repos, purge = purge, install.packagesArgs,
      numToDownload, tmpdir = tmpdir, verbose = verbose - 1
    )
    if (!is.null(pkgNeedInternet$Archive))
      pkgNeedInternet$Archive[nchar(localFile) == 0 | is.na(localFile), needInstall := .txtNoneAvailable]

    # GitHub
    pkgNeedInternet <- downloadGitHub(
      pkgNeedInternet, libPaths, tmpdir = tmpdir, verbose, install.packagesArgs,
      numToDownload
    )
    if (!is.null(pkgNeedInternet$GitHub)) {
      if (!is.null(pkgNeedInternet$GitHub[["SHAonLocal"]])) {
        pkgNeedInternet$GitHub[SHAonGH == SHAonLocal, needInstall := .txtShaUnchangedNoInstall]
      }
      pkgNeedInternet$GitHub[(nchar(localFile) == 0 | is.na(localFile)) & needInstall != .txtShaUnchangedNoInstall,
                             needInstall := .txtNoneAvailable]
    }

    pkgInstallList[[.txtNoLocal]] <- pkgNeedInternet # pointer
  }

  pkgInstall <- rbindlistRecursive(pkgInstallList)

  # This will potentially do Archive (HEAD)
  pkgInstall <- removeHEADpkgsIfNoUpdateNeeded(pkgInstall, verbose = verbose)

  pkgInstall
}

getVersionOnRepos <- function(pkgInstall, repos, purge, libPaths, type = getOption("pkgType")) {

  for (i in 1:2) {
    ap <- available.packagesCached(repos = repos, purge = purge, type = type)[, ..apCachedCols]
    hasRepos <- unlist(lapply(repos, function(xx) any(grepl(pattern = xx, x = ap$Repository))))
    if (all(hasRepos)) {
      break
    }
    purgeAvailablePackages(repos, purge = TRUE)
  }

  setnames(ap, old = "Version", new = "VersionOnReposCurrent")

  # if both have a column, this creates i.XXX columns
  rmFromPkgInstall <- intersect(colnames(ap), colnames(pkgInstall)) |> setdiff("Package")
  if (length(rmFromPkgInstall))
    set(pkgInstall, NULL, rmFromPkgInstall, NULL)
  pkgInstall <- ap[pkgInstall, on = "Package"]
  whHasVoR <- which(!is.na(pkgInstall[["VersionOnReposCurrent"]]))
  if (is.null(pkgInstall[["VersionOnRepos"]]))
    set(pkgInstall, NULL, "VersionOnRepos", pkgInstall[["VersionOnReposCurrent"]])
  if (length(whHasVoR))
    set(pkgInstall, whHasVoR, "VersionOnRepos", pkgInstall[["VersionOnReposCurrent"]][whHasVoR])
  set(pkgInstall, NULL, "VersionOnReposCurrent", NULL)

  if (any(pkgInstall$repoLocation %in% .txtGitHub)) {
    if (!is.null(pkgInstall[["i.VersionOnRepos"]]))
      pkgInstall[repoLocation %in% .txtGitHub, VersionOnRepos := i.VersionOnRepos]
  }

  pkgInstallList <- split(pkgInstall, by = "repoLocation")
  if (!is.null(pkgInstallList[[.txtGitHub]])) {
    # If there are 2 repos for a package, must clear out one of them for GitHub packages
    #   -- the repos are essentially moot for GitHub packages, but will mess downstream with version numbers
    dupReposForGHPkgs <- pkgInstallList[[.txtGitHub]][, .N, by = "Package"][N > 1]
    if (NROW(dupReposForGHPkgs)) {
      pkgInstallList[[.txtGitHub]][Package %in% dupReposForGHPkgs[["Package"]] & hasHEAD,
                                   VersionOK := FALSE]

      pkgInstallList[[.txtGitHub]][Package %in% dupReposForGHPkgs[["Package"]] & !hasHEAD,
                                   VersionOK := compareVersion2(version = VersionOnRepos,
                                                                versionSpec = versionSpec,
                                                                inequality = inequality),
                                   by = "Package"]
      setorderv(pkgInstallList[[.txtGitHub]], "VersionOK", order = -1L, na.last = TRUE)
      pkgInstallList[[.txtGitHub]] <- pkgInstallList[[.txtGitHub]][, .SD[1], by = "Package"]
      set(pkgInstallList[[.txtGitHub]], NULL, "VersionOK", NULL)
    }
    # packages that are both on GitHub and CRAN will get a VersionOnRepos; if the request is to load from GH, then change to NA
    pkgInstallList[[.txtGitHub]][repoLocation %in% .txtGitHub, VersionOnRepos := NA]
    pkgInstallList[[.txtGitHub]] <- getGitHubVersionOnRepos(pkgInstallList[[.txtGitHub]])
  }
  pkgInstall <- rbindlistRecursive(pkgInstallList)
  pkgInstall <- getVersionOnReposLocal(pkgInstall)
  pkgInstall
}


# CRAN
downloadCRAN <- function(pkgNoLocal, repos, purge, install.packagesArgs, verbose, numToDownload,
                         tmpdir, type = getOption("pkgType")) {
  pkgCRAN <- pkgNoLocal[["CRAN"]]
  if (NROW(pkgCRAN)) { # CRAN, Archive, RSPM
    # messageVerbose(messageDownload(pkgCRAN, NROW(pkgCRAN), "CRAN"), verbose = verbose, verboseLevel = 2)
    if (!all(apCachedCols %in% colnames(pkgCRAN))) {
      ap <- available.packagesCached(repos = repos, purge = purge, type = type)[, ..apCachedCols]
      setnames(ap, old = "Version", new = "VersionOnRepos")
      pkgNoLocal[["CRAN"]] <- ap[pkgCRAN, on = "Package"]
      pkgCRAN <- pkgNoLocal[["CRAN"]] # pointer
    }

    # Not on CRAN; so likely Archive
    notOK <- !pkgCRAN$availableVersionOK %in% TRUE # FALSE means it is on CRAN, but not that version; NA means it is not on CRAN currently
    if (any(notOK)) {
      pkgNot <- unique(pkgCRAN[["packageFullName"]][notOK])
      pkgCRAN[notOK, `:=`(repoLocation = "Archive", installFrom = "Archive")]
      pkgNoLocal[["CRAN"]] <- pkgCRAN
      pkgNoLocal <- rbindlistRecursive(pkgNoLocal)
      pkgNoLocal <- split(pkgNoLocal, by = "repoLocation")
      pkgCRAN <- pkgNoLocal[["CRAN"]]
    }
    if (NROW(pkgCRAN)) {
      pkgCRAN <- updateReposForSrcPkgs(pkgCRAN, verbose = verbose)

      if (getOption("Require.installPackagesSys") == 2) {
        ap <- pkgCRAN[pkgCRAN$availableVersionOK %in% TRUE]
        args <- list(repos = repos, type = type)
        file <- paste0(ap[["Package"]], "_", ap$VersionOnRepos)
        if (isWindows() && (identical(type, "both") || grepl("bin", type))) {
          # args$type <- "binary"
          packageUrl <- file
          fileext <- c(".tar.gz", ".zip")[(ap[["binOrSrc"]] %in% "bin") + 1]
        } else {
          if (any(ap$repoLocation %in% "CRAN"))
            packageUrl <- file
          else
            packageUrl <- file.path(ap[["Package"]], file)
          if (isMacOSX())
            fileext <- "tgz"
          else
            fileext <- ".tar.gz"
        }
        packageUrl <- paste0(packageUrl, fileext)
        args$url <- file.path(ap$Repository, packageUrl)
        args$destfile <- file.path(tmpdir, basename(args$url))

        on.exit(copyBuiltToCache(pkgCRAN, tmpdirs = tmpdir, copyOnly = TRUE), add = TRUE)

        st <- system.time(
          dt <- sysInstallAndDownload(args = args, splitOn = c("url", "destfile"),
                                      doLine = "outfiles <- do.call(download.file, args)",
                                      tmpdir = tmpdir,
                                      verbose = verbose)
        )
        messageVerbose("  CRAN ", downloadedInSeconds(st[[3]]), verbose = verbose)
        pkgCRAN[dt, localFile := i.localFile, on = "Package"]
        pkgCRAN[availableVersionOK %in% TRUE, installFrom := .txtLocal]
        pkgCRAN[availableVersionOK %in% TRUE, newLocalFile := TRUE]
      } else {
        pkgCRAN[availableVersionOK %in% TRUE, installFrom := "CRAN"]
        pkgCRAN[, localFile := useRepository]
      }

    }
  }
  pkgNoLocal # pkgCRAN is already in this because it was a pointer
}

#' @importFrom utils contrib.url
downloadArchive <- function(pkgNonLocal, repos, purge = FALSE, install.packagesArgs,
                            numToDownload, tmpdir, verbose) {
  # fillDefaults(pkgDep)
  pkgArchive <- pkgNonLocal[["Archive"]]

  if (NROW(pkgArchive)) {
    ava <- dlArchiveVersionsAvailable(unique(pkgArchive[["Package"]][pkgArchive$repoLocation %in% "Archive"]),
                                      repos = repos, verbose = verbose
    )
    if (!isTRUE(getOption("Require.offlineMode"))) {
      pkgArchive <- getArchiveDetails(pkgArchive, ava, verbose, repos)
      hasPackageUrl <- !is.na(basename(pkgArchive$PackageUrl))

      if (any(hasPackageUrl)) {
        pkgArchiveHasPU <- split(pkgArchive, f = hasPackageUrl)

        tf <- file.path(cachePkgDir(), basename(pkgArchiveHasPU$`TRUE`$PackageUrl))
        fe <- file.exists(tf)
        if (any(fe)) {
          messageVerbose(
            blue("  -- ", unique(paste(pkgArchiveHasPU$`TRUE`[["packageFullName"]][fe], collapse = comma)), " ",
                 isAre(l = pkgArchiveHasPU$`TRUE`[["packageFullName"]][fe]),
                 " not on CRAN; have local cached copy"),
            verbose = verbose, verboseLevel = 1
          )
          whfe <- which(fe)
          pkgArchiveHasPU$`TRUE`[whfe, haveLocal := .txtLocal]
          pkgArchiveHasPU$`TRUE`[whfe, localFile := tf[whfe]]
          pkgArchiveHasPU$`TRUE`[whfe, installFrom := haveLocal]

        }
        whNotfe <- which(fe %in% FALSE)
        if (any(!fe)) {
          messageVerbose(
            blue("  -- ", unique(paste(pkgArchiveHasPU$`TRUE`[["packageFullName"]][whNotfe], collapse = comma)), " ",
                 isAre(l = pkgArchiveHasPU$`TRUE`[["packageFullName"]][whNotfe]),
                 " not on CRAN; trying Archives"),
            verbose = verbose, verboseLevel = 1
          )
          #}

          if (any(pkgArchiveHasPU$`TRUE`[, .N, by = "Package"]$N > 1)) {
            # keep user supplied repos order, in case there are multiple repos that have the package
            set(pkgArchiveHasPU$`TRUE`, NULL, "repoOrder", match(pkgArchiveHasPU$`TRUE`$repo, repos))
            # The na.last are the repositories that didn't have the package in archives; get rid of it
            setorderv(pkgArchiveHasPU$`TRUE`, c("Package", "repoOrder"), order = c(1L, 1L), na.last = TRUE)
            pkgArchiveHasPU$`TRUE` <- pkgArchiveHasPU$`TRUE`[, .SD[1], by = "Package"]
            set(pkgArchiveHasPU$`TRUE`, NULL, "repoOrder", NULL)
          }
          # Check RSPM
          # pkgArchiveHasPU$`TRUE` <- downloadRSPM(pkgArchiveHasPU$`TRUE`, install.packagesArgs, verbose)

          if (any(pkgArchiveHasPU$`TRUE`$repoLocation %in% "Archive" &
                  pkgArchiveHasPU$`TRUE`$availableVersionOK %in% TRUE)) {
            pkgArchiveHasPU$`TRUE` <- split(pkgArchiveHasPU$`TRUE`, pkgArchiveHasPU$`TRUE`[["repoLocation"]])
            pkgArchOnly <- pkgArchiveHasPU$`TRUE`[["Archive"]]

            if (getOption("Require.installPackagesSys") >= 1) {
              on.exit(copyBuiltToCache(pkgArchOnly, tmpdir, copyOnly = TRUE))

              pkgArchOnly <- archiveDownloadSys(pkgArchOnly, whNotfe, tmpdir = tmpdir, verbose)
              pkgArchiveHasPU$`TRUE`[["Archive"]] <- pkgArchOnly

            } else {
              pkgArchOnly[whNotfe, Repository := file.path(contrib.url(repo, type = "source"), "Archive", Package)]
              pkgArchOnly[whNotfe, localFile := useRepository]

            }

          }
        }
        pkgArchiveHasPU$`TRUE` <- rbindlistRecursive(pkgArchiveHasPU$`TRUE`)
        pkgArchive <- rbindlist(pkgArchiveHasPU, fill = TRUE, use.names = TRUE)
      }
    }
  }

  pkgNonLocal[["Archive"]] <- pkgArchive

  pkgNonLocal
}

downloadGitHub <- function(pkgNoLocal, libPaths, verbose, install.packagesArgs, tmpdir, numToDownload) {
  pkgGitHub <- pkgNoLocal[[.txtGitHub]]
  if (NROW(pkgGitHub)) { # GitHub
    messageVerbose(messageDownload(pkgGitHub, NROW(pkgGitHub), .txtGitHub), verbose = verbose, verboseLevel = 2)

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
        td <- tempdir3()
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

        if (getOption("Require.installPackagesSys") == 2) {
          aa <- pkgGHtoDL[!SHAonGH %in% FALSE]
          args <- list(Account = aa$Account, Repo = aa$Repo, Branch = aa$Branch,
                       GitSubFolder = aa$GitSubFolder, verbose = verbose, Package = aa[["Package"]],
                       VersionOnRepos = aa$VersionOnRepos)
          splitOn = c("Account", "Repo", "Branch", "GitSubFolder", "VersionOnRepos", "Package")
          st <- system.time(
            dt <- sysInstallAndDownload(args, splitOn = splitOn, tmpdir = tmpdir, doLineVectorized = FALSE,
                                        "outfiles <- do.call(Require:::downloadAndBuildToLocalFile, args)",
                                        libPaths = libPaths, verbose = verbose)
          )
          messageVerbose("  GitHub ", downloadedInSeconds(st[[3]]), verbose = verbose)
          pkgGHtoDL[dt, localFile := i.localFile, on = "Package"]
          pkgGHtoDL[!SHAonGH %in% FALSE, installFrom := .txtLocal]
          pkgGHtoDL[!SHAonGH %in% FALSE, newLocalFile := TRUE]
        } else {
          pkgGHtoDL[!SHAonGH %in% FALSE, localFile := {
            downloadAndBuildToLocalFile(Account, Repo, Branch, Package, GitSubFolder, verbose, VersionOnRepos)
          }, by = "Package"]
        }

        empty <- !nzchar(pkgGHtoDL[!SHAonGH %in% FALSE]$localFile)
        if (any(empty))
          pkgGHtoDL[which(!SHAonGH %in% FALSE)[empty %in% TRUE], localFile := "NeedRebuild"]
        # A bit of cleaning; this will get rid of the source files; we have the tar.gz after `build`
        pkgGHtoDL[, installFrom := .txtGitHub]
        pkgGHtoDL <- renameLocalGitPkgDT(pkgGHtoDL)
        pkgGHtoDL <- cleanUpNewBuilds(pkgGHtoDL, prevDir)
        if (isTRUE(needRmGSF))
          set(pkgGHtoDL, NULL, "GitSubFolder", NULL)
        on.exit() # don't run on.exit above if this was successful
      }
      pkgGitHub <- rbindlistRecursive(pkgGHList)
    }
  }
  pkgNoLocal[[.txtGitHub]] <- pkgGitHub
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

doPkgSnapshot <- function(packageVersionFile, purge, libPaths,
                          install_githubArgs, install.packagesArgs, standAlone = TRUE,
                          type = getOption("pkgType"), verbose = getOption("Require.verbose"),
                          returnDetails, ...) {
  if (!isFALSE(packageVersionFile)) {
    if (isTRUE(packageVersionFile)) {
      packageVersionFile <- getOption("Require.packageVersionFile")
    }
    packages <- data.table::fread(packageVersionFile)

    if (packages[1, "Package"] == "R") {
      Rversion <- packages[["Version"]][1] |> versionMajorMinor()
      if (!compareVersion2(versionMajorMinor(), Rversion, inequality = "=="))
        messageVerbose("The package snapshot was made using R ", Rversion,
                       ". Current session is running R ", getRversion(),
                       "\nThere may be difficulties installing packages. ",
                       "If there are please restart session using the appropriate R version",
                       verbose = verbose)
      packages <- packages[-1, ]
    }
    packages <- dealWithSnapshotViolations(packages,
                                           verbose = verbose, purge = purge,
                                           libPaths = libPaths, type = type, install_githubArgs = install_githubArgs,
                                           install.packagesArgs = install.packagesArgs
    ) # i.e., packages that can't coexist
    packages <- packages[!packages[["Package"]] %in% .basePkgs]
    repos <- getOption("repos")
    if (!is.null(packages$Repository)) {
      reposPoss <- unique(na.omit(packages$Repository))
      reposPoss <- setdiff(reposPoss, "CRAN")
      isRSPM <- reposPoss == "RSPM"
      RSPM <- if (any(isRSPM))
        reposPoss <- c(RSPM = urlForPositPACKAGES, reposPoss[!isRSPM])
      repos <- c(reposPoss, repos)
      dups <- duplicated(repos)
      if (any(dups)) {
        repos <- repos[!dups]
      }
    }

    # Get Require dependencies to omit them: it has to exist locally unless this is first install
    # packages <- removeRequireDeps(packages)

    need <- packages[["packageFullName"]]
    names(need) <- rep("", NROW(need))
    # isGH <- isGitHub(packages[["packageFullName"]])
    funnyNames <- packages[["Package"]] != extractPkgName(packages[["packageFullName"]])
    if (any(funnyNames)) {
      names(need)[funnyNames] <- packages[["Package"]][funnyNames]
    }
    out <- Require(need,
                   verbose = verbose, purge = purge, libPaths = libPaths, repos = repos,
                   install_githubArgs = install_githubArgs,
                   install.packagesArgs = install.packagesArgs,
                   standAlone = standAlone, require = FALSE, install = TRUE,
                   returnDetails = returnDetails, ...
    )
  } else {
    out <- FALSE
  }
  out
}

dealWithSnapshotViolations <- function(pkgSnapshotObj, install_githubArgs, install.packagesArgs,
                                       verbose = getOption("Require.verbose"),
                                       purge = getOption("Require.purge", FALSE),
                                       libPaths = .libPaths(),
                                       repos = getOption("repos"), type = getOption("pkgType")) {
  dd <- pkgSnapshotObj
  pkgDT <- toPkgDT(packageFullNameFromSnapshot(dd))
  toRm <- c(# "Package",
    "Version", "LibPath", "Depends", "Imports", "LinkingTo",
    "Remotes", "GithubRepo", "GithubUsername", "GithubRef", "GithubSHA1")#,
  # "Repository")
  keepCols1 <- setdiff(colnames(dd), toRm)
  set(pkgDT, NULL, keepCols1, dd[, ..keepCols1])

  pkgDT <- trimRedundancies(pkgDT, repos = repos, purge = purge, libPaths = libPaths,
                            verbose = verbose, type = type)
  return(pkgDT)
}

apCachedCols <- c("Package", "Repository", "Version", "Archs", "Depends", "Imports", "Suggests", "LinkingTo")


localFilename <- function(pkgInstall, localFiles, libPaths, verbose) {
  pkgWhere <- split(pkgInstall, pkgInstall[["repoLocation"]])
  pkgGitHub <- pkgWhere[[.txtGitHub]] # pointer
  if (NROW(pkgWhere[[.txtGitHub]])) {
    pkgGitHub <- getGitHubVersionOnRepos(pkgGitHub)
    pkgGitHub <- availableVersionOK(pkgGitHub)
    avOK <- which(pkgGitHub$availableVersionOK %in% TRUE)
    colsToUpdate <- c("SHAonLocal", "SHAonGH", "installResult")
    set(pkgGitHub, NULL, colsToUpdate[1:2], list(NA_character_, NA_character_)) # fast to just do all; then next lines may update
    if (is.null(pkgGitHub[["installResult"]]))
      set(pkgGitHub, NULL, "installResult", NA_character_)

    if (length(avOK)) {
      pkgGitHub[avOK, (colsToUpdate) := {
        alreadyExistingDESCFile(libPaths = libPaths, Repo, Account, Branch, installResult, verbose)
      }, by = "packageFullName"]
    }
    saveGitHubSHAsToDisk()
    prevInstallResult <- pkgGitHub$installResult
    rowsToUpdate <- which(pkgGitHub$SHAonLocal == pkgGitHub$SHAonGH & pkgGitHub[["installResult"]] == .txtShaUnchangedNoInstall)

    pkgGitHub[rowsToUpdate, `:=`(needInstall = FALSE, haveLocal = .txtLocal, installedVersionOK = TRUE)]
    pkgGitHub <- appendInstallResult(pkgGitHub, rowsToUpdate, installResult = "OK", sep = "; ")

    pkgWhere[[.txtGitHub]] <- pkgGitHub
    pkgInstall <- rbindlistRecursive(pkgWhere)
  }

  pkgInstall[, localFile := localFileID(
    Package, localFiles, repoLocation, SHAonGH,
    inequality, VersionOnRepos, versionSpec, verbose = verbose
  ), by = seq(NROW(pkgInstall))]

  pkgInstall
}

#' Needs `VersionOnRepos`, `versionSpec` and `inequality` columns
#' @param pkgDT A `pkgDT` object
availableVersionOK <- function(pkgDT) {
  # First set all to availableVersionOK if there is a version available
  set(pkgDT, NULL, "availableVersionOK", !is.na(pkgDT[["VersionOnRepos"]]))

  availableOKcols <- c("availableVersionOK", "availableVersionOKthisOne")
  hasAtLeastOneNonNA <- !is.na(pkgDT[["inequality"]]) & !is.na(pkgDT[["VersionOnRepos"]])
  if (any(hasAtLeastOneNonNA)) {
    pkgDT[, hasAtLeastOneNonNA := any(hasAtLeastOneNonNA), by = "Package"]
    toUpdate <- hasAtLeastOneNonNA %in% TRUE
    whToUpdate <- which(toUpdate)
    if (!is.null(pkgDT[["hasHEAD"]])) {
      if (any(pkgDT[["hasHEAD"]])) {
        whToUpdate1 <- which(toUpdate & pkgDT[["hasHEAD"]])
        whToUpdate <- which(toUpdate & pkgDT[["hasHEAD"]] %in% FALSE)
        set(pkgDT, whToUpdate1, "availableVersionOK", TRUE)
        set(pkgDT, whToUpdate1, "availableVersionOKthisOne", TRUE)
      }
    }

    if (length(whToUpdate)) {
      out <- try(pkgDT[whToUpdate, (availableOKcols) := {
        avokto(versionSpec, VersionOnRepos, inequality)
      }, by = "Package"])

      if (is(out, "try-error")) {
        browserDeveloper("Error 553; please contact developer")
      }
    }
  } else {
    pkgDT[!is.na(VersionOnRepos), #  | is.na(versionSpec),
          (availableOKcols) := list(TRUE, TRUE)]
  }
  pkgDT
}

#' Compare package versions
#'
#' Alternative to `utils::compareVersion` that is vectorized on `version`,
#' `versionSpec` and/or `inequality`. This will also return an NA element
#' in the returned vector if one of the arguments has NA for that element.
#'
#' @param version One or more package versions. Can be `character` or `numeric_version`.
#' @param versionSpec One or more versions to compare to.
#'   Can be `character` or `numeric_version`.
#' @param inequality The inequality to use, i.e., `>=`.
#' @return a logical vector of the length of the longest of the 3 arguments.
#'
#' @export
compareVersion2 <- function(version, versionSpec, inequality) {
  if (isTRUE(any(is(version, "numeric_version"))))
    version <- as.character(version)
  if (isTRUE(any(is(versionSpec, "numeric_version"))))
    versionSpec <- as.character(versionSpec)
  vsIsCharNA <- versionSpec %in% "NA"
  if (any(vsIsCharNA))
    versionSpec[vsIsCharNA] <- NA
  verIsCharNA <- version %in% "NA"
  if (any(verIsCharNA))
    version[verIsCharNA] <- NA
  out <- Map(
    vers = version, ineq = inequality, verSpec = versionSpec, # this will recycle, which may be bad
    function(ineq, vers, verSpec) {
      if (!is.na(ineq) && !is.na(vers) && !is.na(verSpec)) {
        a <- compareVersion(verSpec, vers)
        out <- do.call(ineq, list(0, a))
      } else {
        NA
      }
    }
  )
  out <- unlist(out)
  out
}

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
      origPackagesHaveNames[whHasHEAD] <- FALSE # Dealt with in previous line
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


addNamesToPackageFullName <- function(packageFullName, Package) {
  gg <- extractPkgName(packageFullName)
  dif <- !Package %in% gg
  alternateNamedPkgs <- Package[dif]
  if (NROW(alternateNamedPkgs)) {
    names(packageFullName) <- rep("", length(packageFullName))
    names(packageFullName)[dif] <- Package[dif]
  }
  packageFullName
}

messageDownload <- function(pkgDT, numToDownload, fromWhere) {
  paste0(blue(
    " -- downloading ", numToDownload, " packages from ", fromWhere,
    ": ", paste(pkgDT[["Package"]], collapse = comma), " --"
  ))
}

messageForInstall <- function(startTime, toInstall, numPackages, verbose, numGroups) {
  # currentTime <- Sys.time()
  if (NROW(toInstall)) {
    # dft <- difftime(currentTime, startTime, units = "secs")
    installRange <- unique(c(toInstall$installOrder[1], tail(toInstall$installOrder, 1)))
    # timeLeft <- dft / installRange[1] * (numPackages - installRange[1] + 1)

    # lotsOfTimeLeft <- dft > 10
    # timeLeftAlt <- if (lotsOfTimeLeft) format(timeLeft, units = "auto", digits = 1) else "..."
    # estTimeFinish <- if (lotsOfTimeLeft) Sys.time() + timeLeft else "...calculating"
    pkgToReport <- paste(preparePkgNameToReport(toInstall[["Package"]], toInstall[["packageFullName"]]),
                         collapse = comma)
    Source <- ifelse(toInstall$installFrom %in% .txtLocal, .txtLocal, toInstall$repoLocation)
    if (!is.null(toInstall[["newLocalFile"]])) {
      Source <- ifelse(toInstall[["newLocalFile"]] %in% TRUE, toInstall$repoLocation, toInstall[["installFrom"]])
    }
    pkgToReportBySource <- split(toInstall[["Package"]], Source)
    pkgFullNameToReportBySource <- split(toInstall[["packageFullName"]], Source)
    installRangeCh <- paste(installRange, collapse = ":")

    srces <- names(pkgToReportBySource)
    messageVerbose("  -- To install from:", verbose = verbose)
    nxtSrc <- c(yellow = .txtLocal, blue = "CRAN", turquoise = "Archive", green = .txtGitHub, black = "RSPM")
    Map(colr = names(nxtSrc), type = nxtSrc, function(colr, type) {
      pp <- pkgToReportBySource[[type]]
      if (type %in% srces) {
        if (type %in% .txtGitHub) {
          pp <- pkgFullNameToReportBySource[[type]]
        }
        mess <- paste0("  -- ", type, ": ", paste(pp, collapse = comma))
        mess <- paste0WithLineFeed(mess)
        messageVerbose(get(colr)(mess), verbose = verbose)
      }
    })
    messageVerbose(
      blue(
        "  -- ", installRangeCh, " of ", numPackages,
        if (numGroups > 1) {
          paste0(" (grp ", unique(toInstall$installSafeGroups), " of ", numGroups, ")")
        } else {
          ""
        }#,
        #". Estimated time left: ",
        #timeLeftAlt, "; est. finish: ", estTimeFinish
      ),
      verbose = verbose, verboseLevel = 0
    )
  }
}

#' Create a custom "available.packages" object
#'
#' This is the mechanism by which `install.packages` determines which packages
#' should be installed from where. With this override, we can indicate arbitrary
#' `repos`, `Package`, `File` for each individual package.
#' @param toInstall A `pkgDT` object
#' @inheritParams Require
availablePackagesOverride <- function(toInstall, repos, purge, type = getOption("pkgType"),
                                      verbose = getOption("Require.verbose")) {
  whLocal <- startsWith(unique(dirname(dirname(toInstall$Repository))), "file")
  if (any(whLocal %in% FALSE) && any(toInstall$repoLocation %in% "CRAN") &&
      !any(grepl("contrib", toInstall$Repository))) {
    repos <- unique(dirname(dirname(toInstall$Repository)))
  }

  if (!is.null(toInstall$Additional_repositories)) {
    repos <- c(repos, na.omit(toInstall$Additional_repositories))
  }
  ap <- available.packagesCached(repos = repos, purge = purge, returnDataTable = FALSE, type = type)
  pkgsNotInAP <- toInstall[["Package"]][!toInstall[["Package"]] %in% ap[, "Package"]]
  NumPkgsNotInAP <- length(pkgsNotInAP)

  if (NumPkgsNotInAP) { # basically no version is there

    # Have to make the ap object be the same length as the number of pkgsNotInAP
    if (NROW(ap) < NumPkgsNotInAP) { # no nough rows; must add
      ap3 <- matrix(nrow = NumPkgsNotInAP, rep(rep(NA, NCOL(ap)), NumPkgsNotInAP))
      colnames(ap3) <- colnames(ap)
    } else { #  too many rows, keep first NumPkgsNotInAP
      ap3 <- ap[seq(NumPkgsNotInAP), , drop = FALSE]
    }

    ap3[, "Package"] <- pkgsNotInAP
    rownames(ap3) <- ap3[, "Package"]
    ap3[, "Version"] <- toInstall[Package %in% pkgsNotInAP][["VersionOnRepos"]]
    if (!is.null(toInstall$Repository)) {
      ap3[, "Repository"] <- toInstall[Package %in% pkgsNotInAP]$Repository
    }
    ap3[, "Depends"] <- NA
    deps <- pkgDep(toInstall[Package %in% pkgsNotInAP][["packageFullName"]], recursive = TRUE,
                   verbose = verbose)
    pkgHasNameDiffrntThanRepo <- extractPkgName(names(deps)) != toInstall[Package %in% pkgsNotInAP][["Package"]]
    if (any(pkgHasNameDiffrntThanRepo)) {
      names(deps)[pkgHasNameDiffrntThanRepo] <- toInstall[Package %in% pkgsNotInAP][["Package"]][pkgHasNameDiffrntThanRepo]
    }
    deps2 <- unlist(Map(dep = deps, nam = names(deps), function(dep, nam) {
      paste(setdiff(extractPkgName(dep), extractPkgName(nam)), collapse = comma)
    })) # -1 is "drop self"
    ap3[match(extractPkgName(names(deps2)), ap3[, "Package"]), "Imports"] <- deps2
    ap3[, "Suggests"] <- NA
    rownames(ap3) <- pkgsNotInAP
  } else {
    ap3 <- ap[0, , drop = FALSE]
  }

  ap <- ap[ap[, "Package"] %in% toInstall[["Package"]], , drop = FALSE]
  if (NROW(ap3)) {
    ap <- rbind(ap, ap3)
  }

  toInstallList <- split(toInstall, by = "installFrom")
  apList <- list()
  apOrig <- ap
  for (i in names(toInstallList)) {
    # First do version number -- this is same for all locations
    whUpdate <- match(toInstallList[[i]][["Package"]], apOrig[, "Package"])
    ap <- apOrig[whUpdate, , drop = FALSE]
    isNA <- is.na(toInstallList[[i]][["VersionOnRepos"]])
    if (any(isNA)) {
      whUseRepository <- !toInstallList[[i]][isNA]$localFile %in% useRepository &
        !is.na(toInstallList[[i]][isNA]$localFile)
      if (any(whUseRepository)) {
        evn <- extractVersionNumber(filenames = toInstallList[[i]][isNA][whUseRepository]$localFile)
        ap[isNA, "Version"] <- evn
      }
    }
    if (any(!isNA)) {
      ap[!isNA, "Version"] <- toInstallList[[i]][["VersionOnRepos"]][!isNA]
    }
    if (i %in% c("Archive", "CRAN")) {
      ap[, "Repository"] <- toInstallList[[i]]$Repository
    }
    if (i %in% c(.txtLocal, .txtGitHub)) {
      localFile2 <- toInstallList[[i]]$localFile
      fnBase <- basename(localFile2)

      # This is necessary for `clonePackages` ... install.packages doesn't care if Version in "Version" column
      #   doesn't match "File" in `available`
      versionFromFn <- extractVersionNumber(filenames = fnBase)
      needVersionUpdateFromLocalFile <- versionFromFn != ap[, "Version"]
      if (any(needVersionUpdateFromLocalFile %in% TRUE)) {
        ap[needVersionUpdateFromLocalFile, "Version"] <- versionFromFn[needVersionUpdateFromLocalFile]
      }

      fe <- file.exists(fnBase)
      if (any(!fe)) # now the files will be in tmpdir already
        file.copy(localFile2[!fe], fnBase[!fe], overwrite = TRUE) # copy it to "here"
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
      if (keepSourceIfOnlyOne %in% FALSE) {
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
  localFileDir <- if (!is.null(cacheGetOptionCachePkgDir())) {
    cacheGetOptionCachePkgDir()
  } else {
    tempdir3()
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
  if (!isTRUE(getOption("Require.offlineMode"))) {
    notYet <- is.na(pkgGitHub[["VersionOnRepos"]])
    if (any(notYet)) {
      pkgGitHub <- dlGitHubFile(pkgGitHub)
      dFile <- pkgGitHub[["DESCFile"]]
      hasDFile <- which(!is.na(dFile))
      set(pkgGitHub, hasDFile, "VersionOnRepos",  DESCRIPTIONFileVersionV(dFile))
      mayNeedPackageNameChange <- DESCRIPTIONFileOtherV(dFile, other = "Package")
      alreadyCorrect <- pkgGitHub[["Package"]][hasDFile] == mayNeedPackageNameChange
      notAlreadyCorrect <- alreadyCorrect %in% FALSE
      if (any(notAlreadyCorrect)) {
        set(pkgGitHub, hasDFile[notAlreadyCorrect], "Package",  mayNeedPackageNameChange[notAlreadyCorrect])
      }
      # pkgGitHub[!is.na(DESCFile), VersionOnRepos := DESCRIPTIONFileVersionV(DESCFile)]
    }
  }
  pkgGitHub
}

#' @importFrom utils tail
localFileID <- function(Package, localFiles, repoLocation, SHAonGH, inequality,
                        VersionOnRepos, versionSpec, verbose = getOption("Require.verbose")) {

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

  if (repoLocation %in% .txtGitHub) {
    if (grepl("Error in file", SHAonGH) && isTRUE(getOption("Require.offlineMode"))) {
      messageVerbose("Using Require.offlineMode; could not identify SHA on Github for ",
                     green(Package), "; using the latest version that exists locally, ",
                     "which may not be a GitHub version", verbose = verbose)
      fn <- fn[order(package_version(extractVersionNumber(filenames = fn)), decreasing = TRUE)]
    } else {
      fn <- if (is.na(SHAonGH)) "" else grep(SHAonGH, fn, value = TRUE)
    }
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
        isHEAD <- identical(versionSpec, "HEAD")
        if (isHEAD) {
          keepLoc <- try(unlist(compareVersion2(localVer[isHEAD], VersionOnRepos[isHEAD], "==")))
        } else {
          keepLoc <- try(unlist(compareVersion2(localVer[!isHEAD], versionSpec[!isHEAD], inequality[!isHEAD])))
        }
        if (is(keepLoc, "try-error")) {
          browserDeveloper("Error 978; please contact developer")
        }
        if (!identical(inequality, "==") && !is.na(VersionOnRepos) && !isHEAD) {
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
  # } else {
  #   order(package_version(extractVersionNumber(filenames = fn)), decreasing = TRUE)
  # }
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
  if (!is.null(cacheGetOptionCachePkgDir())) {
    # check for crancache copies
    localFiles <- dir(cacheGetOptionCachePkgDir(), full.names = TRUE)
    localFiles <- doCranCacheCheck(localFiles, verbose)
    pkgInstall <- localFilename(pkgInstall, localFiles, libPaths = libPaths, verbose = verbose)
    if (isTRUE(getOption("Require.offlineMode"))) {
      noneFound <- nchar(pkgInstall$localFile) == 0
      if (isTRUE(any(noneFound))) {
        pkgInstall[noneFound]
      }
    }
    pkgInstall[, haveLocal :=
                 unlist(lapply(localFile, function(x) c(.txtNoLocal, .txtLocal)[isTRUE(nchar(x) > 0) + 1]))]
    pkgInstall[haveLocal %in% .txtLocal, `:=`(
      installFrom = haveLocal,
      availableVersionOK = TRUE,
      Repository = paste0("file:///", cacheGetOptionCachePkgDir())
    )]
  } else {
    set(pkgInstall, NULL, c("localFile"), "")
  }
}


confirmEqualsDontViolateInequalitiesThenTrim <- function(pkgDT,
                                                         ifViolation = c("removeEquals", "stop"),
                                                         verbose = getOption("Require.verbose")) {

  # Basically, if the package has no version specification, it can take "any version", so don't use it
  #   at all, unless it is the only description
  set(pkgDT, NULL, "hasInequality", !is.na(pkgDT[["inequality"]]))
  pp <- pkgDT[, keep44 := if (any(hasInequality)) ifelse(hasInequality, .I, NA) else .I, by = "Package"]
  pkgDT <- pp[unique(na.omit(keep44))]
  if (!is.null(pkgDT[["parentPackage"]])) {
    pkgDT[, needKeep := grepl("\\(", parentPackage) | is.na(parentPackage)]
    pkgDT[, keep55 := if(any(needKeep)) ifelse(needKeep | is.na(parentPackage), .I, NA) else .I, by = "Package"]
    pkgDT <- pkgDT[unique(na.omit(keep55))]
  }


  set(pkgDT, NULL, "isEquals", pkgDT[["inequality"]] == "==")
  pkgDT[,  oppositeInequals := any(grepl("<", inequality)) & any(grepl(">", inequality)), by = "Package"]
  pkgDT[, hasEqualsAndInequals := any(isEquals %in% TRUE) && any(isEquals %in% FALSE), by = "Package"]

  if (any(pkgDT$hasEqualsAndInequals) ) {
    pkgDT[hasEqualsAndInequals %in% TRUE, EqualsDoesntViolate := {
      out <- rep(NA, length.out = .N)
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

    set(pkgDT, NULL, "isGT", pkgDT[["inequality"]] == ">")
    pkgDT[, hasGTAndInequals := any(isGT %in% TRUE) && any(isGT %in% FALSE), by = "Package"]
    pkgDT[hasGTAndInequals %in% TRUE, GTDoesntViolate := {
      compareVersion3(.N, isGT, versionSpec, inequality)
      # out <- rep(NA, length = .N)
      # wh <- which(isGT)
      # whNot <- which(!isGT)
      # if (length(wh)) {
      #   out[wh] <- try(unlist(Map(verSpec = versionSpec[wh], function(verSpec) {
      #     withCallingHandlers(
      #       all(compareVersion2(versionSpec[whNot], verSpec, inequality[wh]))
      #       , warning = function(w)
      #     )
      #   })))
      #   if (is(out, "try-error")) {
      #     browserDeveloper("Error 844; please contact developer")
      #   }
      #   out
      # }
      # out
    },
    by = "Package"
    ]

    pkgDT[, violation2 := if (any(isGT %in% TRUE) && all(GTDoesntViolate %in% FALSE)) {
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
        keepCols2 <- pkgDT$violation %in% TRUE & !pkgDT[["inequality"]] %in% "==" | pkgDT$violation %in% FALSE
        rm <- pkgDT[which(!keepCols2)]
        pkgDT <- pkgDT[which(keepCols2)]
        pkgDT[Package %in% rm[["Package"]], installResult := msgPackageViolation]
      }
    }

    if (any(pkgDT$violation2 %in% TRUE)) {
      messageVerbose(green(
        "The following shows packages whose version requirements can not be met; ",
        "keeping the newer version: "
      ), verbose = verbose, verboseLevel = 1)
      cols <- c("Package", "packageFullName", "versionSpec")
      cols2 <- setdiff(cols, "packageFullName")
      cols3 <- c(cols, "versionToKeep")
      violationsDF <- pkgDT[violation2 %in% TRUE & !is.na(versionSpec), ..cols]
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
        pkgDT <- pkgDT[violation2 %in% TRUE & !inequality %in% "==" | violation2 %in% FALSE]
      }
    }

    pkgDT[, keepCols3 := if (any(isEquals %in% TRUE) && any(EqualsDoesntViolate %in% TRUE)) {
      .I[isEquals %in% TRUE & EqualsDoesntViolate %in% TRUE][1]
    } else {
      .I
    },
    by = "Package"
    ]

    pkgDT <- pkgDT[unique(keepCols3)]

    # get rid of the GT if they are TRUE
    pkgDT[, remove := (isGT %in% TRUE & GTDoesntViolate %in% TRUE)]
    if (any(pkgDT$remove))
      pkgDT <- pkgDT[!remove %in% TRUE]


    set(pkgDT, NULL, c("isEquals", "hasEqualsAndInequals", "EqualsDoesntViolate", "keepCols3"), NULL)
  }

  if (any(pkgDT$oppositeInequals)) {
    whOppositeInequals <- which(pkgDT$oppositeInequals)
    pkgDT[whOppositeInequals,
          violationsDoubleInequals := detectDoubleInequalsViolations(inequality, versionSpec, .N)
          , by = "Package"]
    if (grepl("remove|rm", ifViolation[1]) && any(pkgDT$violationsDoubleInequals[whOppositeInequals])) {
      set(pkgDT, NULL, "keepCols4", TRUE)
      pkgDT[whOppositeInequals, keepCols4 := !grepl("<", pkgDT[["inequality"]][whOppositeInequals]), by = "Package"]
      rm <- pkgDT[which(!keepCols4)]
      pkgDT <- pkgDT[which(keepCols4)]
      pkgDT[Package %in% rm[["Package"]], installResult := msgPackageViolation]
      set(pkgDT, NULL, c("violationsDoubleInequals", "keepCols4"), NULL)
    }

  }
  pkgDT
}

detectDoubleInequalsViolations <- function(inequality, versionSpec, N) {
  {
    res <- logical(N)
    whGT <- grep(">", inequality)
    whLT <- grep("<", inequality)
    res[whLT] <- mapply(whL = whLT, function(whL) {
      all(compareVersion2(rep(versionSpec[whL], length(whGT)), versionSpec[whGT], inequality[whGT]))
    })
    res[whGT] <- mapply(whG = whGT, function(whG)
      compareVersion2(rep(versionSpec[whG], length(whLT)), versionSpec[whLT], inequality[whLT]))
    return(!res)
  }
}

compareVersion3 <- function(.N, isGT, versionSpec, inequality) {
  out <- rep(NA, length = .N)
  wh <- which(isGT)
  whNot <- which(!isGT)
  if (length(wh)) {
    out[wh] <-
      unlist(
        Map(verSpec = versionSpec[wh], function(verSpec) {
          all(compareVersion2(versionSpec[whNot], verSpec, inequality[whNot]))
        })
      )
    if (is(out[wh], "try-error")) {
      browserDeveloper("Error 844; please contact developer")
    }
    out
  }
  out
}

keepOnlyGitHubAtLines <- function(pkgDT, verbose = getOption("Require.verbose")) {
  gitRepos <- pkgDT$repoLocation %in% .txtGitHub
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
  if (!is.data.table(pkgInstall)) {
    pkgInstall <- toPkgDT(pkgInstall)
    pkgInstall <- parsePackageFullname(pkgInstall)
  }
  if (NROW(pkgInstall)) {
    pkgInstall <- removeDups(pkgInstall)
    if (anyDuplicated(pkgInstall[["Package"]])) {
      if (!is.null(pkgInstall[["hasHEAD"]])) {
        hasHeadRows <- which(pkgInstall[["hasHEAD"]] %in% TRUE)
        whToRM <- which(pkgInstall[["Package"]] %in% pkgInstall[hasHeadRows][["Package"]])
        whToRM <- setdiff(whToRM, hasHeadRows)
        if (length(whToRM))
          pkgInstall <- pkgInstall[-whToRM]
      }

      versionSpecNotNA <- !is.na(pkgInstall$versionSpec)
      if (any(versionSpecNotNA)) {
        verSpec <- pkgInstall$versionSpec[versionSpecNotNA]
        isHEAD <- verSpec %in% "HEAD"
        verSpec[isHEAD] <- "0.0.0"
        ord <- order(package_version(verSpec),
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
      # set(pkgInstall, NULL, "versionSpecGroup", data.table::rleid(pkgInstall$versionSpec))
      setOrderOn <- list(colm = c("Package", # "versionSpecGroup",
                                  "inequality", "repoLocation"),
                         ordr = c(1L, # 1L,
                                  -1L, 1L))
      setDT(setOrderOn)
      setOrderOn <- setOrderOn[setOrderOn$colm %in% colnames(pkgInstall)]
      setorderv(pkgInstall, setOrderOn$colm, #c("Package", "versionSpecGroup", "inequality", "repoLocation"),
                order = setOrderOn$ordr, na.last = TRUE)

      pkgAndInequality <-
        intersect(colnames(pkgInstall), c("Package", "inequality", "repoLocation"))#, "versionSpecGroup")

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

      if (any(pkgInstall[["inequality"]] %in% c(">", "<"))) {
        pkgInstall[, keepBasedOnRedundantInequalities :=
                     unlist(lapply(.I, function(ind) {
                       ifelse(is.na(inequality), ind,
                              ifelse(inequality == ">", .I[1], ifelse(inequality == "<", tail(.I, 1), .I))
                       )
                     })),
                   by = pkgAndInequality
        ]
        pkgInstall <- pkgInstall[unique(keepBasedOnRedundantInequalities)]
        set(pkgInstall, NULL, "keepBasedOnRedundantInequalities", NULL)
        pkgInstall <- confirmEqualsDontViolateInequalitiesThenTrim(pkgInstall)

      }
      pkgInstall <- trimRedundantVersionAndNoVersion(pkgInstall)
      # pkgInstall <- removeDups(pkgInstall)
    }
  }
  pkgInstall[]
}

trimRedundantVersionAndNoVersion <- function(pkgInstall) {
  set(pkgInstall, NULL, "hasVers", !is.na(pkgInstall$versionSpec))
  hasV <- pkgInstall$hasVers == TRUE
  pkgInstall[, `:=`(
    atLeastOneWithVersionSpec = any(hasVers)
    # Current = all(Current == TRUE)
  ), by = "Package"]
  pkgInstall <-
    pkgInstall[!(pkgInstall$atLeastOneWithVersionSpec == TRUE &
                   pkgInstall$hasVers == FALSE)] # remove cases where no version spec >1 case
}


checkAvailableVersions <- function(pkgInstall, repos, purge, libPaths, verbose = getOption("Require.verbose"),
                                   type = getOption("pkgType")) {

  pkgInstallTmp <- getVersionOnRepos(pkgInstall, repos, purge, libPaths, type = type)
  pkgInstall <- pkgInstallTmp
  # coming out of getVersionOnRepos, will be some with bin and src on windows; possibly different versions; take only first, if identical URL at top level
  #  https://cran.rstudio.com/bin/windows/contrib/4.3 should be same Repo as https://cran.rstudio.com/src/contrib
  set(pkgInstall, NULL, "RepositoryTop", strsplit(gsub("http://|https://", "", pkgInstall$Repository), "/")[[c(1, 1)]])
  pkgInstall <- availableVersionOK(pkgInstall)
  pkgInstall <-
    rbindlist(list(
      pkgInstall[!availableVersionOK %in% TRUE],
      unique(pkgInstall[availableVersionOK %in% TRUE], by = c("Package", "VersionOnRepos", "RepositoryTop"))
    ))
  set(pkgInstall, NULL, "RepositoryTop", NULL)
  pkgInstall <- keepOnlyGitHubAtLines(pkgInstall, verbose = verbose)
  pkgInstall[, binOrSrc := binOrSrc(grepl("\\<bin\\>", Repository))]
  # pkgInstall[, binOrSrc := c("src", "bin")[grepl("\\<bin\\>", Repository) + 1]]
  setorderv(pkgInstall, c("Package", "availableVersionOKthisOne", "binOrSrc"), order = c(1L, -1L, 1L)) # OK = TRUE first, bin before src

  pkgInstall[, keepCols5 := {
    # This will pick the one that is OK, or if they are all NA (meaning no version spec),
    #    or all FALSE (meaning need to try Archive) <-- this was removed because can have 2 repositories, e.g., predictiveecology.r-universe.dev and CRAN MUST TRY BOTH
    #    then also pick first one that OK
    ok <- any(availableVersionOKthisOne %in% TRUE) || all(is.na(availableVersionOKthisOne)) # || all(availableVersionOKthisOne %in% FALSE)
    if (ok) .I[ok][1] else .I
  }, by = c("Package")]
  pkgInstall <- pkgInstall[unique(keepCols5)]

  # next line can switch a "install" to "dontInstall"
  pkgInstall <- removeHEADpkgsIfNoUpdateNeeded(pkgInstall, verbose = verbose)
  pkgInstallList <- split(pkgInstall, by = "needInstall")

  if (NROW(pkgInstallList[[.txtInstall]])) {
    pkgInstallTmp  <- pkgInstallList[[.txtInstall]] # pointer only
    # pkgInstallTmp <- pkgInstall
    if (any(!pkgInstallTmp$availableVersionOK)) {
      if (!is.null(pkgInstallTmp$Additional_repositories)) {
        pkgInstallAvails <- split(pkgInstallTmp, pkgInstallTmp$availableVersionOK)
        if (any(!is.na(pkgInstallAvails[["FALSE"]]$Additional_repositories))) {
          pkgAddRep <- split(pkgInstallAvails[["FALSE"]], pkgInstallAvails[["FALSE"]]$Additional_repositories)
          pkgAddRepTmp <- Map(repo = names(pkgAddRep), pkgAr = pkgAddRep, function(repo, pkgAr) {
            pkgArTmp <- getVersionOnRepos(pkgAr, repo, purge, libPaths, type = type)
          })
          pkgAddRepTmp <- rbindlist(pkgAddRepTmp)
          pkgInstallTmp2 <- pkgInstallTmp[, c("packageFullName", "tmpOrder", "Additional_repositories")]
          pkgInstallTmp2 <- unique(pkgInstallTmp2, by = c("packageFullName"))
          pkgAddRep <- pkgInstallTmp2[!is.na(Additional_repositories)][pkgAddRepTmp, on = "packageFullName"]#, allow.cartesian = TRUE]
          set(pkgAddRep, NULL, intersect("i.tmpOrder", colnames(pkgAddRep)), NULL)
          pkgInstallTmp <- pkgInstallTmp[is.na(Additional_repositories)] # keep this for end of this chunk
          pkgInstallAvails[["FALSE"]] <- rbindlist(list(pkgInstallTmp, pkgAddRep), fill = TRUE, use.names = TRUE)
          setorderv(pkgInstallAvails[["FALSE"]], "tmpOrder")
        }
        pkgInstallList[[.txtInstall]] <- rbindlist(pkgInstallAvails, fill = TRUE, use.names = TRUE)
      }

    }

    set(pkgInstallList[[.txtInstall]], NULL, "keepCols5", NULL)
  }
  pkgInstall <- rbindlist(pkgInstallList, fill = TRUE, use.names = TRUE)

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
  cols <- c("PackageUrl", "dayAfterPutOnCRAN", "dayBeforeTakenOffCRAN", "repo",
            "VersionOnRepos", "availableVersionOK")
  commonCols <- intersect(cols, colnames(pkgArchive))

  numGroups <- NROW(unique(pkgArchive[["Package"]]))
  messageVerbose("Identifying date range when package versions appear in Archive for ",
                 numGroups, " package(s)",
                 verbose = verbose,
                 verboseLevel = 2
  )

  pkgArchive[, (cols) := {
    ret <- getArchiveDetailsInnerMemoise(Repository, ava, Package, cols, versionSpec, inequality, secondsInADay, .GRP,
                                         numGroups, verbose, repos, srcPackageURLOnCRAN)
  }, by = c("Package")]#, "Repository")] # multiple repositories may have same version and it is OK ... do loop inside

  # need to update binary or src because the getArchiveDetailsInner doesn't address this --
  #   it just returns the PackageURL
  bins <- isBinary(pkgArchive$PackageUrl, needRepoCheck = FALSE)
  bins <- isBinaryCRANRepo(pkgArchive$Repository) %in% TRUE | bins %in% TRUE
  needBinsUpdate <- !is.na(bins %in% TRUE)
  if (any(needBinsUpdate)) {
    set(pkgArchive, which(needBinsUpdate), "binOrSrc", binOrSrc(bins[needBinsUpdate]))
    pkgArchive <- updateReposForSrcPkgs(pkgArchive)
  }

  hasVoR <- which(!is.na(pkgArchive[["VersionOnRepos"]]))
  set(pkgArchive, hasVoR, "Version", pkgArchive[["VersionOnRepos"]][hasVoR])
  pkgArchive <- unique(pkgArchive, by = c("Package", "repo", "availableVersionOK"))

  pkgArchive
}

parsePackageFullname <- function(pkgDT, sorted = TRUE) {
  set(pkgDT, NULL, "versionSpec", extractVersionNumber(pkgDT[["packageFullName"]]))
  wh <- which(!is.na(pkgDT$versionSpec))
  if (length(wh)) {
    set(pkgDT, wh, "inequality", extractInequality(pkgDT[["packageFullName"]][wh]))
  } else {
    set(pkgDT, NULL, "inequality", NA_character_)
  }
  # pkgDT[!is.na(pkgDT$versionSpec), inequality := extractInequality(pkgDT[["packageFullName"]])]
  if (isTRUE(sorted))
    setorderv(pkgDT, c("Package", "versionSpec"), order = c(1L, -1L), na.last = TRUE)
  pkgDT
}

removeBasePkgs <- function(pkgDT) {
  pkgDT[!Package %in% .basePkgs]
}

renameLocalGitPkgDT <- function(pkgInstall) {
  whGitHub2 <- pkgInstall$repoLocation %in% .txtGitHub
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
copyBuiltToCache <- function(pkgInstall, tmpdirs, copyOnly = FALSE) {
  if (!is.null(pkgInstall)) {
    if (!is.null(cacheGetOptionCachePkgDir())) {
      cacheFiles <- dir(cacheGetOptionCachePkgDir())
      out <- try(Map(td = tmpdirs, function(td) {
        tdPkgs <- dir(td, full.names = TRUE, pattern = "\\.zip|\\.tar\\.gz")
        if (length(tdPkgs)) {
          pkgs <- Map(td = tdPkgs, function(td) strsplit(basename(td), split = "_")[[1]][1])
          pkgsInstalled <- pkgInstall[match(pkgs, Package)]
          isGitHub <- pkgsInstalled$repoLocation %in% .txtGitHub
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
            newFiles <- file.path(cacheGetOptionCachePkgDir(), basename(tdPkgs))
            if (isTRUE(copyOnly)) {
              suppressWarnings(file.copy(tdPkgs, newFiles))
            } else {
              suppressWarnings(fileRenameOrMove(tdPkgs, newFiles))
            }

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
  out <- ifelse(!is.na(snapshot$GithubRepo) & nzchar(snapshot$GithubRepo),
                paste0(
                  snapshot$GithubUsername, "/", snapshot$GithubRepo, "@",
                  snapshot$GithubSHA1
                ), snapshot[["Package"]]
  )
  out <- paste0(
    out,
    " (==", snapshot[["Version"]], ")"
  )
  out <- addNamesToPackageFullName(out, snapshot[["Package"]])
  out
}

#' Update installed packages with latest available versions
#'
#' Similar to `update.packages`, but works for archived, non-archived,
#' and Github packages.
#'
#' @param libPaths The library to update; defaults to `.libPaths()[1]`
#' @param purge Logical. Should the assessment of `installed.packages` purge the cached
#'   version. Default is `FALSE`
#' @inheritParams Require
#' @return Run for its side effect, namely, updating installed packages to their latest
#' possible state, whether they are on CRAN currently, archived, or on GitHub.
#' @export
#'
updatePackages <- function(libPaths = .libPaths()[1], purge = FALSE,
                           verbose = getOption("Require.verbose")) {

  libPaths <- doLibPaths(libPaths, standAlone = FALSE)

  ip <- doInstalledPackages(libPaths, purge, includeBase = FALSE)

  ref <- ip$GithubRef
  ineq <- "HEAD"
  head <- paste0(" (", ineq, ")")
  pkgs <- paste0(ifelse(
    !is.na(ip$GithubRepo),
    paste0(ip$GithubUsername, "/", ip$GithubRepo,
           ifelse(is.na(ip$GithubSubFolder), "", paste0("/", ip$GithubSubFolder)),
           "@", ref, head),
    # github
    paste0(ip[["Package"]], head) # cran
  ))
  Install(pkgs, purge = purge, verbose = verbose)
}

getVersionOnReposLocal <- function(pkgDT) {
  if (!is.null(cacheGetOptionCachePkgDir())) {
    set(pkgDT, NULL, "tmpOrder", seq(NROW(pkgDT)))
    if (any(is.na(pkgDT[["VersionOnRepos"]]))) {
      pkgDTList <- split(pkgDT, !is.na(pkgDT[["VersionOnRepos"]]))
      if (!is.null(pkgDTList$`FALSE`)) {
        localFilesOuter <- dir(cacheGetOptionCachePkgDir())
        pkgNoVoR <- pkgDTList$`FALSE`
        wh <- pkgNoVoR[["repoLocation"]] %in% .txtGitHub
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
            pkgNoVoR[hasLocalNow, haveLocal := .txtLocal]
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

browserDeveloper <- function(mess = "", envir = parent.frame()) {
  if (identical(SysInfo[["user"]], "emcintir")) {
    print(mess)
    browser()
  } else {
    stop(mess)
  }
}

updateReposForSrcPkgs <- function(pkgInstall, verbose = getOption("Require.verbose")) {

  if (isLinux()) {
    isBinary <- isBinaryCRANRepo(pkgInstall$Repository)
    dontInstallBecauseForceSrc <- pkgInstall[["Package"]] %in% sourcePkgs()
    needSrc <- isBinary & dontInstallBecauseForceSrc
    if ("lme4" %in% pkgInstall[["Package"]]) {
      if (packageVersion("Matrix") < "1.6.5") {
        needSrc[pkgInstall[["Package"]] %in% "lme4"] <- TRUE
        messageVerbose("lme4 is being installed. Matrix version is <1.6.5, which means that lme4 must ",
                       "be installed from source. Proceeding with installing lme4 from source. Alternatively, ",
                       "cancel this, Install('Matrix (>=1.6.5)' and try again.", verbose = verbose)
      }
    }

    # }
    #
    # if (!isWindows() && !isMacOSX() &&
    #     any(pkgInstall$isBinaryInstall & pkgInstall$localFile %in% useRepository)) {
    #   dontInstallBecauseForceSrc <- pkgInstall[["Package"]] %in% sourcePkgs()
    #   mayNeedSwitchToSrc <- pkgInstall$localFile %in% useRepository & dontInstallBecauseForceSrc
    #   pkgInstall[
    #     which(mayNeedSwitchToSrc),
    #     isBinaryInstall := isWindows() | isMacOSX()
    #   ]
    #   needSwitchToSrc <- mayNeedSwitchToSrc & pkgInstall$isBinaryInstall %in% FALSE
    #   if (any(needSwitchToSrc %in% TRUE)) {
    # Eliot commented this March 28, 2024 -- posit Package Manager can handle this now
    # if (any(isBinaryCRANRepo(getOption("repos")[1]))) {
    #   nams <- pkgInstall[needSwitchToSrc][["Package"]]
    #   warning(
    #     "The CRAN repository is a binary repository. However, ",
    #     paste(nams, collapse = comma), isAre(nams), " identified in `sourcePkgs()`, ",
    #     " indicating installation from source; if these source installs fail, try changing ",
    #     "to set \noptions(Require.otherPkgs = c('",
    #     paste(setdiff(getOption("Require.otherPkgs"), nams), collapse = "', '"), "'))",
    #     "\nremoving ", paste(nams, collapse = comma)
    #   )
    # }
    if (all(isBinaryCRANRepo(getOption("repos")))) {
      warning(
        paste(pkgInstall[needSwitchToSrc][["Package"]], collapse = comma), " is identified in `sourcePkgs()`, ",
        "indicating it should normally be installed from source; however, there is no source CRAN repository.",
        "Please add one to the `options(repos)`, e.g., with ",
        "options(repos = c(getOption('repos'), CRAN = 'https://cloud.r-project.org')).",
        "Proceeding with the binary repository, which may not work"
      )
    } else {
      nonBinaryRepos <- getOption("repos")[!isBinaryCRANRepo(getOption("repos"))]
      if (length(nonBinaryRepos) == 1) {
        pkgInstall[which(needSrc), `:=`(Repository = contrib.url(nonBinaryRepos),
                                        repo = nonBinaryRepos,
                                        binOrSrc = binOrSrc(FALSE))]
        # pkgInstall[which(needSrc), Repository := nonBinaryRepos]

        # pkgInstall[whArchive %in% TRUE & needSwitchToSrc, Repository := getArchiveURL(nonBinaryRepos, Package)]

        # if there are multiple non-binary repos
      } else { # (length(nonBinaryRepos) > 1) {
        packageExists <- FALSE
        for (ind in seq(nonBinaryRepos)) {
          nbrContrib <- contrib.url(nonBinaryRepos)
          whArchive <- pkgInstall$installFrom %in% "Archive"
          dontInstallBecauseForceSrc <- pkgInstall[["Package"]] %in% sourcePkgs()
          mayNeedSwitchToSrc <- pkgInstall$localFile %in% useRepository & dontInstallBecauseForceSrc
          needSwitchToSrc <- mayNeedSwitchToSrc & pkgInstall$isBinaryInstall %in% FALSE

          pkgInstallNeededHere <- pkgInstall[!whArchive %in% TRUE & needSwitchToSrc]
          apTmp <- available.packages(contriburl = nbrContrib[ind])
          packageExists <- pkgInstallNeededHere[["Package"]] %in% apTmp[, "Package"]
          if (any(packageExists)) {
            pkgInstall[Package %in% pkgInstallNeededHere[["Package"]][packageExists],
                       `:=`(Repository, nbrContrib[ind])]
            needSwitchToSrc <- mayNeedSwitchToSrc & pkgInstall$isBinaryInstall %in% FALSE
          }
          if (all(packageExists))
            break
        }
        # } else {
        #   pkgInstall[!whArchive %in% TRUE & needSwitchToSrc, Repository := contrib.url(nonBinaryRepos)]
      }

      #   }
    }
  }
  pkgInstall
}

messagesAboutWarnings <- function(w, toInstall, returnDetails, tmpdir, verbose = getOption("Require.verbose")) {
  # This is a key error; cached copy is corrupt; this will intercept, delete it and reinstall all right here

  pkgName <- extractPkgNameFromWarning(w$message)
  outcome <- FALSE
  needWarning <- TRUE
  if (identical(pkgName, w$message)) { # didn't work
    pkgName <- gsub(".+\u2018(.+)\u2019.*", "\\1", w$message)
  }
  if (identical(pkgName, w$message)) { # didn't work again
    if (any(grepl("cannot open URL", pkgName))) { # means needs purge b/c package is on CRAN, but not that url
      url <- gsub(".+(https://.+\\.zip).+", "\\1", pkgName)
      url <- gsub(".+(https://.+\\.tar\\.gz).+", "\\1", url)
      url <- gsub(".+(https://.+\\.tgz).+", "\\1", url)
      pkgName <- extractPkgName(filenames = basename(url))

      repos <- unique(toInstall[["repos"]])
      try(lapply(repos, function(repo) purgeAvailablePackages(repo, purge = TRUE)))
      # try(purgeAvailablePackages(repos = repos, purge = TRUE))
      # try(dealWithCache(purge = TRUE, checkAge = FALSE))
      messageVerbose("purging availablePackages; trying to download ", pkgName, " again",
                     verbose = verbose)
      res <- try(Install(pkgName))
      if (is(res, "try-error")) {
        needWarning <- TRUE
      } else {
        outcome2 <- attr(res, "Require")
        if (identical(.txtNoneAvailable, outcome2$installResult)) {
          needWarning <- TRUE
        } else {
          needWarning <- FALSE
          outcome <- TRUE
          rowsInPkgDT <- grep(pkgName, toInstall[["Package"]])
          outcome2JustOne <- outcome2[outcome2[["Package"]] %in% toInstall[["Package"]]]
          if (!is.null(outcome2JustOne$installed))
            toInstall[rowsInPkgDT, installed := outcome2JustOne$installed]
          if (!is.null(outcome2JustOne$installResult))
            toInstall[rowsInPkgDT, installResult := outcome2JustOne$installResult]
        }
      }
    }

  }

  rowsInPkgDT <- which(toInstall[["Package"]] %in% pkgName)
  if (length(rowsInPkgDT) && any(toInstall[rowsInPkgDT]$installed %in% FALSE)) {
    toInstall[rowsInPkgDT, installed := outcome]
    toInstall <- appendInstallResult(toInstall, rowsInPkgDT, w$message, sep = "; ")
    # toInstall[rowsInPkgDT, installResult := w$message]

    if (any(grepl("cannot remove prior installation of package", w$message))) {
      warning("Is ", pkgName, " loaded in another R session? Please close all sessions before installing packages")
      needWarning <- FALSE
    } else {
      needWarning <- TRUE
    }
  }

  inUse <- "in use and will not be installed"
  isInUse <- grepl(inUse, w$message)
  if (isInUse) {
    toInstall[Package %in% pkgName, installResult := inUse]
  }
  if (length(rowsInPkgDT) && any(isInUse)) {
    if (!all(toInstall$installed[rowsInPkgDT] %in% FALSE) &&
        !all(toInstall$installResult[rowsInPkgDT] %in% w$message)) {
      set(toInstall, rowsInPkgDT, "installed", FALSE)
      # toInstall[rowsInPkgDT, installed := FALSE]
      set(toInstall, rowsInPkgDT, "installResult", w$message)
    }
    messageVerbose(w$message, verbose = verbose)
    if (length(grep("Require", pkgName)))
      needWarning <- FALSE
  }

  # Case where the local file may be corrupt
  needReInstall <- FALSE
  failedMsg <- grepl(
    paste(
      .txtInstallationNonZeroExit,
      .txtInstallationPkgFailed,
      .txtCannotOpenFile, sep = "|"),
    w$message)
  if (any(failedMsg)) {
    unlink(toInstall[Package %in% pkgName]$localFile)
    unlink(dir(tmpdir, pattern = paste0("^", pkgName, "_[[:digit:]]", collapse = "|"), full.names = TRUE))
    if (isTRUE(any(grepl(.txtCannotOpenFile, w$message)))) {
      stop(.txtPkgFailed, pkgName, .txtRetrying)
    }
  }

  if (!is.null(cacheGetOptionCachePkgDir())) {
    if (isTRUE(unlist(grepV(pkgName, cacheGetOptionCachePkgDir()))) || needReInstall) {
      messageVerbose(verbose = verbose, verboseLevel = 1, "Cached copy of ", basename(pkgName), " was corrupt; deleting; retrying")
      unlink(dir(cacheGetOptionCachePkgDir(), pattern = basename(pkgName), full.names = TRUE)) # delete the erroneous Cache item
      retrying <- try(Require(toInstall[Package %in% basename(pkgName)][["packageFullName"]],
                              require = FALSE,
                              verbose = verbose, returnDetails = returnDetails,
                              dependencies = FALSE)) # dependencies = FALSE b/c it should already have what it needs
      if (is(retrying, "try-error")) {
        needWarning <- TRUE
      } else {
        needWarning <- FALSE
      }
    }
  } else {
    needWarning <- TRUE
  }

  if (isTRUE(needWarning)) {
    warning(w)
  }
}

getArchiveURL <- function(repo, pkg, type = getOption("pkgType")) {
  file.path(contrib.url(repo, type), "Archive", pkg)
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

  if (!missing(packages)) {
    packagesSubstituted <- substitute(packages)
    packages <- substitutePackages(packagesSubstituted, parent.frame())
  }

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
  cantUpdateDeps <- toInstall[["Package"]] %in% c("Require", "data.table")
  if (any(cantUpdateDeps))
    toInstall <- toInstall[-which(cantUpdateDeps)]
  cantUpdateLoaded <- toInstall[["Package"]] %in% loadedNamespaces()
  if (any(cantUpdateLoaded)) {
    toRm <- toInstall[["Package"]][!cantUpdateLoaded]
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
  out1 <- try(build(Package, verbose = verbose, quiet = verbose <= 0 || verbose >= 5,
                    VersionOnRepos = VersionOnRepos))
  fn <- dir(pattern = paste0("^", Package, "_.+tar.gz"))
  normPath(fn)
}


needRebuildAndInstall <- function(needRebuild, pkgInstall, libPaths, install.packagesArgs,
                                  repos, purge, startTime, type,
                                  pkgInstallTmp, tmpdir, verbose = getOption("Require.verbose")) {
  if (any(needRebuild)) {
    messageVerbose("Trying to rebuild and install GitHub build fails... ", verbose = verbose)
    pkgInstall[which(needRebuild), needRebuild := repoLocation]
    pkgInstallList <- split(pkgInstall, by = "needRebuild")
    # names(pkgInstallList) <- c("No", .txtGitHub)
    pkgInstallList <- downloadGitHub(pkgInstallList, libPaths, verbose, install.packagesArgs)
    maxGroup <- 1
    numPackages <- NROW(pkgInstallList[[.txtGitHub]])
    pkgInstallList[[.txtGitHub]][, installOrder := seq(.N)] # renumber the installOrder
    pkgInstallList <- Map(
      toInstall = pkgInstallList[.txtGitHub],
      MoreArgs = list(
        repos = repos, purge = purge,
        install.packagesArgs = install.packagesArgs, numPackages = numPackages,
        numGroups = maxGroup, startTime = startTime, type = type,
        tmpdir = tmpdir, verbose = verbose
      ),
      installAll
    )
    pkgInstall <- rbindlist(list(pkgInstallTmp[!Package %in% pkgInstallList[[.txtGitHub]][["Package"]]],
                                 pkgInstallList[[.txtGitHub]]), use.names = TRUE, fill = TRUE)
  }
  pkgInstall
}

substitutePackages <- function(packagesSubstituted, envir = parent.frame()) {

  # Deal with non character strings first
  packages2 <- if (isTRUE(all(is.character(packagesSubstituted)))) {
    packagesSubstituted
  } else {
    deparse(packagesSubstituted)
  }
  isName <- is.name(packagesSubstituted)
  # isName <- is.name(packages2)
  if (isName) {
    packagesTmp2 <- get0(packages2, envir = envir)
    if (is(packagesTmp2, "list")) {
      kk <- lapply(packagesTmp2, substitutePackages, envir = envir)
    }
    if (is.character(packagesTmp2))
      packages <- packagesTmp2

    # packages <- packages2
  } else {
    isGH <- isGitHub(packages2)
    if (isTRUE(isGH)) {
      packages <- packages2
    } else {
      # if (length(packages2) > 1) {
      #   packagesTmp2 <- packages2
      # } else {
      #   # packagesTmp2 <- get0(packages2, envir = envir)
      #   # if (is.character(packagesTmp2))
      #   #   packages <- packagesTmp2
      # }
      # }

      # Recursion internally to c or list
      if (is.call(packagesSubstituted) && !is.character(packagesSubstituted) || is(packagesSubstituted, "list")) {
        packages <- try(eval(packagesSubstituted, envir = envir), silent = TRUE)
        if (is(packages, "try-error") || is(packagesSubstituted, "list")) {
          nams <- names(packagesSubstituted[-1])
          if (is.null(nams)) nams <- rep("", length(packagesSubstituted[-1]))
          packages <- unlist(Map(i = seq_len(length(packagesSubstituted[-1])) + 1,
                                 nam = nams, function(i, nam) {
                                   # First try to evaluate; only if fails do we do the recursion on each element
                                   out <- try(eval(packagesSubstituted[i], envir = envir), silent = TRUE) # e.g., paste("SpaDES (>= ",version,")")
                                   if (is(out, "try-error"))
                                     out <- substitutePackages(packagesSubstituted = as.list(packagesSubstituted)[[i]], envir = envir)
                                   if (!is.null(nam))
                                     if (nzchar(nam))
                                       names(out) <- nam
                                   out
                                 }))
        }
      }
    }
  }

  if (!exists("packages", inherits = FALSE))
    packages <- packages2

  packages
}

clonePackages <- function(rcf, ipa, libPaths, verbose = getOption("Require.verbose")) {
  mess <- capture.output(
    type = "message",
    ip <- installed.packages(lib.loc = rcf, fields = c("Built", "NeedsCompilation"))
  )
  clearErrorReadRDSFile(mess, rcf)
  #
  # oo <- capture.output(type = "message",
  #                      ip <- installed.packages(lib.loc = rcf, fields = c("Built", "NeedsCompilation")))
  #                      # ip <- .installed.pkgs(lib.loc = rcf, which = c("Built", "NeedsCompilation")))
  ignorePackages <- character()
  ipCanTryNeedsNoCompilAndGoodRVer <- canClone(ip)
  alreadyInstalledCanClone <- intersect(rownames(ipCanTryNeedsNoCompilAndGoodRVer), ipa$pkgs)
  alreadyInstalledCanClone <- ipCanTryNeedsNoCompilAndGoodRVer[, "Version"][alreadyInstalledCanClone]
  alreadyInstalledCanClone <- alreadyInstalledCanClone[!names(alreadyInstalledCanClone) %in% sourcePkgs()]

  if (length(grep("Error in readRDS", mess))) {
    # This means that the packages in rcf are broken
    aa <- packageHasError(rcf)
    igns <- unlist(unname(aa))
    if (length(igns))
      ignorePackages <- names(alreadyInstalledCanClone)[names(alreadyInstalledCanClone) %in% igns]
  }

  if (length(ignorePackages))
    alreadyInstalledCanClone <- alreadyInstalledCanClone[!names(alreadyInstalledCanClone) %in% ignorePackages]
  wantToInstall <- ipa$available[, "Version"]
  cantClone <- setdiffNamed(wantToInstall, alreadyInstalledCanClone)# if (isWindows()) {
  canClone <- setdiffNamed(wantToInstall, cantClone)
  canClone <- names(canClone)
  if (length(canClone)) {
    mess <- paste0("  -- Cloning (", length(canClone)," of ", length(wantToInstall),
                   ") instead of Installing (don't need compiling): ",
                   paste(canClone, collapse = comma))
    mess <- paste0WithLineFeed(mess)
    messageVerbose(green(mess), verbose = verbose)

    linkOrCopyPackageFiles(Packages = canClone, fromLib = rcf[1], toLib = libPaths[1], ip = ip)
    ipa$pkgs <- names(cantClone)
    messageVerbose(green("... Done!"), verbose = verbose)
    ipa$available <- ipa$available[ipa$pkgs, , drop = FALSE]
  }
  ipa
}

linkOrCopyPackageFiles <- function(Packages, fromLib, toLib, ip) {
  if (missing(ip)) {
    mess <- capture.output(
      type = "message",
      ip <- installed.packages(fromLib, fields = c("Built", "NeedsCompilation"))
    )
    clearErrorReadRDSFile(mess, fromLib)
  }
  cant <- cantClone(ip)
  cant <- unique(c(sourcePkgs(), cant[, "Package"]))
  Packages <- setdiff(Packages, cant)
  linkOrCopyPackageFilesInner(Packages, fromLib, toLib)
  return(invisible())
}


linkOrCopyPackageFilesInner <- function(Packages, fromLib, toLib) {
  ret <- lapply(Packages, function(packToClone) {
    from <- dir(dir(fromLib, pattern = paste0("^", packToClone, "$"), full.names = TRUE), recursive = TRUE, all.files = TRUE)
    fromFull <- file.path(fromLib, packToClone, from)
    to <- file.path(toLib, packToClone, from)
    dups <- fromFull %in% to
    if (any(dups)) {
      fromFull <- fromFull[!dups]
      to <- to[!dups]
    }
    if (length(fromFull) > 0) {
      dirs <- unique(dirname(to))
      checkPath(dirs[order(nchar(dirs), decreasing = TRUE)], create = TRUE)
      outs <- linkOrCopy(fromFull, to, allowSymlink = FALSE)
    }
  })
}


getArchiveDetailsInner <- function(Repository, ava, Package, cols, versionSpec, inequality,
                                   secondsInADay, .GRP,
                                   numGroups, verbose, repos, srcPackageURLOnCRAN) {
  if (any(is.na(Repository)) || is.null(Repository)) {
    Repository <- unique(ava[[Package]]$repo) # this can make it longer than other items (Package, versionSpec, inequality)
    Repository <- Repository[match(repos, Repository)]
    if (length(Repository) > length(Package)) {
      len <- length(Repository)
      # Package <- rep(Package, len)
      versionSpec <- rep(versionSpec, len)
      inequality <- rep(inequality, len)
    }
  }
  ret <- mapply(x = cols, USE.NAMES = TRUE, function(x) NA_character_, SIMPLIFY = FALSE)
  ret <- as.data.table(ret)
  set(ret, NULL, "availableVersionOK", FALSE)

  for (ind in seq(Repository)) {
    repositoryHasArchives <- if (!is.na(Repository[ind]) && !is.null(ava[[Package]]$repo)) {
      unlist(lapply(ava[[Package]]$repo, grepl, x = Repository[ind]))
    } else {
      FALSE
    }
    if (all(!repositoryHasArchives) && length(repos) > length(unique(Repository[ind]))) {
      repositoryHasArchives <- unlist(lapply(ava[[Package]]$repo, grepl, x = repos))
    }

    if (any(repositoryHasArchives)) {
      Version2 <- extractVersionNumber(filenames = ava[[Package]]$PackageUrl)
      versionSpec2 <- unique(versionSpec[ind])
      vs2IsCharNA <- versionSpec2 %in% "NA"
      if (any(vs2IsCharNA))
        versionSpec2[vs2IsCharNA] <- NA
      if (length(Version2) > 0) {
        if (is.na(versionSpec2)) { # Version2 is length 0 when the package has nothing available
          nver <- numeric_version(Version2)
          highest <- max(nver)
          correctVersions <- which(highest == nver)
          if (length(correctVersions) > 1) {
            correctVersions <- correctVersions[ava[[Package]][correctVersions]$repo %in% Repository[ind]]
          }
        } else {
          if (identical(versionSpec2, "HEAD")) {
            correctVersions <- rep(FALSE, length(Version2))
            correctVersions[length(correctVersions)] <- TRUE
            names(correctVersions) <- Version2
          } else {
            # creates a named logical, where name is version2
            correctVersions <- compareVersion2(Version2, versionSpec2, inequality[ind])
          }

          if (all(correctVersions %in% FALSE)) {
            # This is case where the inequality cannot be fulfilled... check why:
            # e.g., NLMR == 1.1.1 ... did not exist on CRAN archives
            # case 1 -- package exists in archive, but not specific version
            # no version on CRAN
            if (length(correctVersions)) {
              ineq <- ">="
              altVersion <- compareVersion2(Version2, versionSpec2, ineq)
              if (!any(altVersion)) {
                ineq <- "<="
                altVersion <- compareVersion2(Version2, versionSpec2, ineq)
              }
              correctVersions <- NA
              if (any(altVersion)) {
                messageVerbose("Package ",
                               Package, " (", inequality[ind], "", versionSpec2,
                               ") is not available in `repos` supplied ",
                               "The recent available versions are:", verbose = verbose
                )
                messageDF(tail(n = 10, data.frame(Version = Version2,
                                                  ava[[Package]][, c("repo", "PackageUrl", "mtime")])), verbose = verbose)
                warnTxt <- msgPleaseChangeRqdVersion(Package, ineq, newVersion = tail(Version2[altVersion], 1))
                warning(warnTxt, call. = FALSE)

                break
              }
              # correctVersions <- altVersion
              # }

            }

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
        if (is.na(correctVersions[2])) { # if 2nd one is NA, it means it an archived package
          tf <- file.path(cachePkgDir(), basename(ret$PackageUrl))
          fe <- file.exists(tf)
          if (fe) {
            dayBeforeTakenOffCRAN <- ava[[Package]][correctVersions[2]][["mtime"]]
          } else {
            dayBeforeTakenOffCRAN <- archivedOn(Package, pkgRelPath = ret$PackageUrl, verbose, repos,
                                                numGroups = numGroups,
                                                counter = .GRP,
                                                srcPackageURLOnCRAN, repo
            )
            dayBeforeTakenOffCRAN <- dayBeforeTakenOffCRAN[[1]]$archivedOn
          }
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
        if (isTRUE(ret$availableVersionOK)) break
      } else {
        repoToAdd <- ava[[Package]][1][, c("repo")]
        if (!is.null(repoToAdd))
          ret[, repo := repoToAdd]
      }
    }
  }
  data.table::setcolorder(ret, cols)
  ret
}


# If a list of dependencies includes Remotes from DESCRIPTION file
# mergeRemotes <- function(depsList) {
#   lapply(depsList, function(dep) {
#     pkgDT <- parseGitHub(dep)
#     isGH <- !is.na(pkgDT$githubPkgName)
#     ver <- extractVersionNumber(pkgDT[["packageFullName"]])
#     verNotGH <- ver[!isGH]
#     hasVerNotGH <- !is.na(verNotGH)
#     verGH <- ver[isGH]
#     wh1 <- which(!isGH)[hasVerNotGH]
#     ma1 <- match(pkgDT[["Package"]][!isGH], pkgDT[["Package"]][isGH]) |> na.omit()
#     ma2 <- match(pkgDT[["Package"]][isGH], pkgDT[["Package"]][!isGH]) |> na.omit()
#     wh <- which(!is.na(ma2))
#     hasVer2 <- hasVerNotGH[ma2]#[wh]
#     ineq <- extractInequality(pkgDT[["packageFullName"]][ma2][hasVer2])#[!isGH])
#     # ma[!is.na(ver)]
#     newPFN <- paste0(pkgDT[["packageFullName"]][isGH][ma1][hasVer2], " (", ineq, verNotGH[ma2][hasVer2], ")")
#     pkgDT[which(!isGH)[ma2][hasVer2], packageFullName := newPFN]
#     pkgDT <- pkgDT[-which(isGH)[hasVer2]]
#     pkgDT[["packageFullName"]]
#   })
# }

canClone <- function(ip) {
  RversionDot <- RversionDot()
  correctBuilt <- correctBuilt(ip, RversionDot)
  # correctBuilt <- Rversion == BuiltVersion # needs to re-add package_version to both those functions
  keepCols6 <- correctBuilt %in% TRUE
  # if (isWindows()) {
  keepCols6 <- keepCols6 & ip[,  "NeedsCompilation"] == "no"
  # }
  ip[keepCols6 %in% TRUE ,, drop = FALSE]
}

cantClone <- function(ip) {
  RversionDot <- RversionDot()
  correctBuilt <- correctBuilt(ip, RversionDot)
  ip[ (ip[, "NeedsCompilation"] == "yes" | correctBuilt %in% FALSE)  %in% TRUE ,, drop = FALSE]
}

RversionDot <- function() {
  # This adds a trailing "." so that it is e.g., 4.3., not 4.3  because
  paste0(paste(R.version$major,
               gsub("^(.{1,2})\\..+$", "\\1", R.version$minor),
               sep = "."), ".")
}

BuiltVersion2 <- function(ip) {
  paste(gsub("^(.{1,2}\\..{1,2})\\..+$", "\\1", ip[, "Built"]),
        sep = ".")
}

correctBuilt <- function(ip, RversionDot) {
  startsWith(ip[, "Built"], prefix = RversionDot)
}

colsOfDeps <- c("Depends", "Imports", "LinkingTo", "Remotes", "Suggests")



# Get Require dependencies to omit them: it has to exist locally unless this is first install
removeRequireDeps <- function(pkgDT, verbose) {
  if (!is.data.table(pkgDT))
    pkgDT <- toPkgDT(pkgDT)

  # localRequireDir <- file.path(.libPaths(), "Require")
  # de <- dir.exists(localRequireDir)
  # if (any(de)) {
  #   localRequireDir <- localRequireDir[de][1]
  #   RequireDeps <- DESCRIPTIONFileDeps(file.path(localRequireDir, "DESCRIPTION"))
  # } else {
  #   # if the package is loaded to memory from a different .libPaths() that is no longer on the current .libPaths()
  #   #  then the next line will work to find it
  #   deps <- packageDescription("Require", lib.loc = NULL, fields = "Imports")
  #   if (nzchar(deps)) {
  #     RequireDeps <- depsWithCommasToVector("Require", depsWithCommas = deps)
  #   } else {
  #     RequireDeps <- pkgDep("Require", simplify = TRUE, verbose = 0)
  #   }
  #
  # }

  whNeedInstall <- pkgDT[["needInstall"]] %in% .txtInstall
  toRm <- pkgDT[["Package"]][whNeedInstall] %in% extractPkgName(unlist(.RequireDependencies))
  if (any(toRm)) {
    NeedRestart <- if (getOption("Require.installPackagesSys") > 0) {
      # Can install when in a different process
      pkgDT[["Package"]][whNeedInstall][toRm] %in% "Require"
    } else {
      FALSE
    }

    whRm <- which(whNeedInstall)[toRm]

    # Try to install them anyway, but it will fail and report error
    # set(pkgDT, whRm, "needInstall", .txtDontInstall)

    set(pkgDT, whRm, "installed", TRUE)
    set(pkgDT, whRm, "installResult", "Can't install Require dependency")
    if (any(NeedRestart))
      set(pkgDT, whRm, "installResult", "Need to restart R")
  }
  pkgDT
}


matchWithOriginalPackages <- function(pkgDT, packages) {

  # might be missing `require` column
  colsToKeep <- intersect(colnames(pkgDT), c("Package", "loadOrder", "require"))
  packagesDT <- pkgDT[, ..colsToKeep]
  if (isTRUE(!all(pkgDT[["packageFullName"]] %in% packages))) {
    # Some packages will have disappeared from the pkgDT b/c of trimRedundancies
    packagesDT <- unique(packagesDT, on = "Package")[
      toPkgDT(packages)[, c("Package", "packageFullName")], on = c("Package")]
  }
  unique(packagesDT)[]
}


appendInstallResult <- function(pkgDT, rowsToUpdate, installResult, sep = "; ") {
  if (length(rowsToUpdate)) {
    prevInstallResult <- pkgDT$installResult[rowsToUpdate]
    ir <- installResult # needed for next line s
    pkgDT[rowsToUpdate, `:=`(installResult = ir)]
    if (!is.null(prevInstallResult)) {
      nas <- is.na(prevInstallResult)
      if (any(!nas)) {
        pkgDT[rowsToUpdate[which(!nas)],
              installResult := paste(installResult, prevInstallResult[which(!nas)], sep = sep)]
      }
    }}
  pkgDT

}

packageHasError <- function(libs = .libPaths(), packages = NULL) {
  problems <- Map(lib = libs, function(lib) {
    pkgs <- dir(lib)

    # need to intersect packages with pkgs
    if (!is.null(packages))
      pkgs <- intersect(packages, pkgs)

    suppressWarnings(
      out <- Map(pkg = pkgs, function(pkg) {
        f <- system.file('Meta', 'package.rds', package = pkg, lib.loc = lib)
        tryCatch({readRDS(f); FALSE}, error = function(e) TRUE)
      })
    )
    out <- out[unlist(out)]
    names(out)
  })
  if (!is.null(packages)) {
    problems <- Map(p = problems, function(p) p[p %in% packages])
  }
  problems
}

binOrSrc <- function(areBinary) {
  c("src", "bin")[areBinary + 1]
}

#' @include messages.R
removeHEADpkgsIfNoUpdateNeeded <- function(pkgInstall, verbose = getOption("Require.verbose")) {
  # needs both Version and VersionOnRepos
  if (any(pkgInstall$hasHEAD)) {
    hasHead <- pkgInstall$hasHEAD
    notGH <- !pkgInstall$isGitPkg
    whNoKeep <- which(hasHead & notGH)
    if (length(whNoKeep)) {
      # Only keepForUpdate if it is identical ... a HEAD could be of a different branch and thus a newer or older version
      pkgInstall[whNoKeep, keepForUpdate := compareVersion2(Version, VersionOnRepos, "<") | forceInstall]
      # pkgInstall[whHasHead & whGH, DESCRIPTIONFileVersionV(DESCFile)]
      # The next logical has 2 options:
      #    NAs include packages that were not "HEAD", but are needed by the "HEAD", but aren't already installed
      #    TRUE is needs installing
      #    FALSE is no need installing
      whDontInstall <- which(pkgInstall$keepForUpdate %in% FALSE)
      if (length(whDontInstall)) {
        set(pkgInstall, whDontInstall, "needInstall", .txtDontInstall)
      }
    }

  }
  pkgInstall
}



#' download.files or install.packages in a separate process
#'
#' This uses `sys` package so that messaging can be controlled. This also provides
#' the option to parallelize by spawning multiple `background` process to allow
#' parallel e.g., downloads. Noting that if `libcurl` is installed (and detected
#' using `capabilities("libcurl")`), then no explicit parallelism will be allowed,
#' instead `method = "libcurl"` will be passed enabling parallel downloads.
#'
#'
#' @param args A list with all arguments for a do.call to either `download.file,
#'        `install.packages` or a custom other function e.g., `downloadAndBuildToLocalFile`.
#' @param splitOn A character vector of the names in `args` to parallelize over.
#'        Defaults to `pkgs`. All other named elements in `args` will be assumed to
#'        be length 1 and used for every parallel process.
#' @param doLine A character string with the `"outfiles <- do.call(..., args)"` line.
#' @param returnOutfile A logical. If `TRUE`, then the names of the `outfiles` will
#'        be returned.
#' @param doLineVectorized A logical. If `TRUE`, and parallism is being used, this
#'        indicates that the `doLine` is a function that allows for multiple elements
#'        in `args[[splitOn[[1]]]`. If `FALSE`, the function will make multiple
#'        sequential calls within each parallel process to the `doLine` call.
#' @param tmpdir A single path where all downloads will be put
#' @inheritParams Require
#' @return Mostly for side effects, namely installed packages or downloaded packages or
#'        files. However, in the case of `returnOutfile = TRUE`, then a list of
#'        filenames will be returned with any outputs from the `doLine`.
sysInstallAndDownload <- function(args, splitOn = "pkgs",
                                  doLine = "outfiles <- do.call(download.packages, args)",
                                  returnOutfile = FALSE, doLineVectorized = TRUE, tmpdir, libPaths, verbose) {
  downPack <- grepl("download.packages", doLine)
  downFile <- grepl("download.file", doLine)
  installPackages <- grepl("install.packages", doLine)
  downAndBuildLocal <- grepl("downloadAndBuildToLocalFile", doLine)
  downOther <- downPack %in% FALSE & downFile %in% FALSE
  argsOrig <- args
  if (downPack %in% TRUE || downFile %in% TRUE || installPackages %in% TRUE) {
    args$method <- if (isTRUE(capabilities("libcurl"))) "libcurl" else "auto"
    if (!args$method %in% "libcurl")
      doLineVectorized <- FALSE
  }
  vecList <- splitVectors(args, splitOn, method = args$method, installPackages)
  # if (installPackages)
  #   libTemps <- lapply(vecList, function(x) tempdir3())


  pids <- numeric(length(vecList))
  outfiles <- lapply(pids, function(i) {
    fn1 <- file.path(tmpdir, basename(tempfile(fileext = ".rds")))
    normalizePath(fn1, winslash = "/", mustWork = FALSE)
  })

  doLineOrig <- doLine
  repos <- paste(args$repos, collapse = ", ")
  preMess <- if (installPackages) {
    "\n  -- To install: "
  } else {
    paste0("  -- Downloading", ifelse(nzchar(repos), paste0(" (from ", repos,")"), ""), ":\n")
  }
  fullMess <- character() # this will accumulate and regularly clear
  for (j in seq_along(vecList)) {
    st <- Sys.time()
    i <- vecList[[j]]
    fn <- file.path(tmpdir, basename(tempfile(fileext = ".rds")))
    fn <- normalizePath(fn, winslash = "/", mustWork = FALSE)

    for (jjj in splitOn)
      args[[jjj]] <- argsOrig[[jjj]][i]

    if (doLineVectorized %in% FALSE && length(args[[splitOn[1]]]) > 1) {
      doLine <- updateDoLine(tmpdir, splitOn, doLineOrig)
    }
    # if (installPackages) {
    #   args$destdir <- args$lib <- libTemps[[j]]
    # }
    saveRDS(args, file = fn)

    cmdLine <- buildCmdLine(tmpdir, fn, doLine, downAndBuildLocal = downAndBuildLocal,
                            outfile = outfiles[j], libPaths = libPaths)

    fullMess <- makeFullMessage(fullMess, args, installPackages, downPack, downFile)

    if (installPackages)
      logFile <- basename(tempfile2(fileext = ".log")) # already in tmpdir

    if (installPackages) {
      if (length(fullMess)) {
        mess <- messInstallingOrDwnlding(preMess, fullMess)
        mess <- paste0WithLineFeed(mess)
        messageVerbose(mess, verbose = verbose)
        fullMess <- character()
      }
    } else {
      fullMess <- paste0WithLineFeed(gsub("\\s,", ",", gsub("\n", " ", fullMess)))
    }

    pids[j] <- sysDo(installPackages, cmdLine, logFile, verbose)
  }

  if (installPackages %in% FALSE)
    if (length(fullMess))
      messageVerbose(messInstallingOrDwnlding(preMess, fullMess), verbose = verbose)

  on.exit(sapply(pids, tools::pskill))
  isRstudio <- isRstudio()

  fullMess <- character()
  preMess <- if (installPackages)  "" else "Downloaded: "

  for (pid in pids) {
    st <- Sys.time()
    if (!installPackages) {
      spinnerOnPid(pid, isRstudio, st, verbose)
    }

    whPid <- match(pid, pids)
    if (downPack || installPackages) {
      pkgs <- argsOrig$pkgs[vecList[[whPid]]]
      if (installPackages) {
        # check installations
        if (!file.exists(logFile))
          file.create(logFile)
        log <- readLines(logFile) # won't exist if `verbose < 1`
        if (any(grepl(paste(.txtInstallationNonZeroExit, .txtInstallationPkgFailed, sep = "|"), log))) {
          return(logFile)
        }
        aa <- Map(p = args$pkgs, function(p) as.character(packVer(p, args$lib)))
        # aa <- Map(p = args$pkgs, function(p) packVer(package = p, args$lib))
        dt <- data.table(pkg = names(aa), vers = unlist(aa, use.names = FALSE), versionSpec = args$available[, "Version"])
        # the "==" doesn't work directly because of e.g., 2.2.8 and 2.2-8 which should be equal
        whFailed <- try(!compareVersion2(dt$vers, dt$versionSpec, inequality = "=="))
        whFailed <- whFailed %in% TRUE
        if (isTRUE(any(whFailed))) {
          pkgsFailed <- dt$pkg[whFailed]
          pkgs <- setdiff(pkgs, pkgsFailed)
        }
      }
      mess <- "\n"
    } else if (downFile) {
      mess <- paste(extractPkgName(filenames = basename(argsOrig$url[vecList[[whPid]]])), collapse = comma)
    } else {
      w <- vecList[[whPid]]
      mess <- paste0(argsOrig$Account[w], "/", argsOrig$Repo[w], "@", argsOrig$Branch[w], collapse = comma)
    }

    fullMess <- if (length(fullMess)) paste(fullMess, mess, sep = ", ") else mess
    if ( (Sys.time() - st) > 2 && nzchar(fullMess)) {
      fullMess <- paste0WithLineFeed(fullMess)
      messageVerbose(blue(paste0("  ", preMess, fullMess)), verbose = verbose)
      fullMess <- character()
    }

  }
  # if (installPackages) {
  #   sup <- Map(libFrom = libTemps, function(libFrom) {
  #     linkOrCopyPackageFilesInner(dir(libFrom), fromLib = libFrom, toLib = argsOrig$lib)
  #   })
  #
  # }

  if (length(fullMess) && nzchar(fullMess)) {
    fullMess <- gsub("\n", " ", fullMess)
    fullMess <- paste0WithLineFeed(fullMess)
    messageVerbose(blue(paste0("  ", preMess, fullMess)), verbose = verbose)
  }

  ll <- try(lapply(outfiles, readRDS), silent = TRUE)
  if (downPack && !downFile && !installPackages) {
    if (downPack)
      dt <- as.data.table(do.call(rbind, ll))
    else
      dt <- as.data.table(cbind(Package = argsOrig[["Package"]], do.call(rbind, ll)))
    setnames(dt, new = c("Package", "localFile"))
  } else if (downFile) {
    dt <- list(Package = extractPkgName(filenames = basename(argsOrig$destfile)),
               localFile = argsOrig$destfile) |> setDT()
  } else if (downAndBuildLocal) {
    dt <- list(Package = argsOrig[["Package"]],
               localFile = unlist(ll))
    setDT(dt)
    # if (length(dt$localFile) != length(dt$Package))
    #   dt$localFile <- rep("", length(dt$Package))
    # a <- try(setDT(dt), silent = TRUE)
    # if (is(a, 'try-error')) {
    #   # out <- mget(ls())
    #   # out$token <- tryCatch(
    #   #   gitcreds::gitcreds_get(use_cache = FALSE),
    #   #   error = function(e) NULL
    #   # )
    #   #
    #   # save(out, file = "/home/emcintir/tmp/dt.rda")
    #   stop(a)
    # }
  } else  { # installPackages and Other
    dt <- logFile
  }

  dt
}

rmErroredPkgInstalls <- function(logFile, toInstall, verbose) {
  if (is.null(logFile))
    logFile <- dir(pattern = "\\.log")
  if (!is.null(logFile)) {
    if (isTRUE(file.exists(logFile))) {
      rl <- readLines(logFile)
      errs <- c("Makefile.+Error 1", "ERROR:")
      errsGrep <- paste(errs, collapse = "|")
      anyErrors <- grep(paste("Execution halted", errsGrep, sep = "|"), rl)
      if (length(anyErrors)) {
        ERRORLine <- lapply(errs, grep, x = rl, value = TRUE)
        if (length(unlist(ERRORLine))) {
          pkgFail1 <- extractPkgNameFromWarning(ERRORLine[[2]])
          rmFiles <- character()
          if (length(pkgFail1)) {
            DONELines <- grep("\\* DONE \\(", rl, value = TRUE)
            pkgSucceed <- gsub("\\* DONE \\((.+)\\)", "\\1", DONELines)
            # ipa$pkgs <- setdiff(ipa$pkgs, pkgSucceed)
            # ipa$available <- ipa$available[!ipa$available[, "Package"] %in% pkgSucceed, , drop = FALSE]
            toInstall2 <- toInstall[!toInstall[["Package"]] %in% pkgSucceed]
            rmFiles <- c(rmFiles, toInstall2[["localFile"]], basename(toInstall2[["localFile"]]))
          }
          pkgFail2 <- gsub("^.+[[:digit:]]: (.+).ts.+$", "\\1", ERRORLine[[1]])
          pkgFail2 <- setdiff(pkgFail2, pkgFail1)
          if (length(pkgFail2)) {
            toInstall2 <- toInstall[toInstall[["Package"]] %in% pkgFail2]
            rmFiles <- c(rmFiles, toInstall2[["localFile"]], basename(toInstall2[["localFile"]]))
          }

          messageVerbose("Because of ERROR, removing Cached copy of ", paste(toInstall2[["packageFullName"]], collapse = ", "),
                         verbose = verbose)
          unlink(rmFiles)
        }
      }
    }
  }
}


archiveDownloadSys <- function(pkgArchOnly, whNotfe, tmpdir, verbose) {
  set(pkgArchOnly, whNotfe, "fileExists", FALSE)
  pkg <- split(pkgArchOnly, by = "fileExists")
  p <- pkg[["FALSE"]]
  set(p, NULL, "Repository", file.path(contrib.url(p$repo, type = "source"), "Archive"))
  # p[, Repository := file.path(contrib.url(repo, type = "source"), "Archive")]
  p$repo <- naToEmpty(p$repo)
  p$PackageUrl <- naToEmpty(p$PackageUrl)
  url <- if (any(greplV(unique(p$repo), p$PackageUrl))) {
    p$PackageUrl
  } else {
    file.path(p$Repository, p$PackageUrl)
  }
  nonEmpties <- which(nchar(url) > 0)
  if (length(nonEmpties)) {
    args <- list(url = url[nonEmpties],
                 destfile = file.path(tmpdir, basename(url)[nonEmpties]))

    dt <- sysInstallAndDownload(args = args, splitOn = c("url", "destfile"),
                                doLine = "outfiles <- do.call(download.file, args)",
                                tmpdir = tmpdir,
                                verbose = verbose)
    p[dt, localFile := i.localFile, on = "Package"]
    p[, installFrom := .txtLocal]
    p[, newLocalFile := TRUE]
  }
  pkg[["FALSE"]] <- p
  pkgArchOnly <- rbindlist(pkg, use.names = TRUE, fill = TRUE)
  set(pkgArchOnly, NULL, "fileExists", NULL)
  pkgArchOnly[]
}


naToEmpty <- function(vec) {
  naToEmpty <- is.na(vec)
  if (any(naToEmpty)) vec[naToEmpty] <- ""
  vec
}


splitVectors <- function(argsOrig, splitOn, method, installPackages) {
  if (identical(method, "libcurl") || isTRUE(installPackages)) {
    vecList <- list(seq_along(argsOrig[[splitOn[1]]]))
  } else {
    vec <- seq_along(argsOrig[[splitOn[1]]])
    chunk2 <- function(x,n) {
      if (n == 1)
        list(1)
      else
        split(x, cut(seq_along(x), n, labels = FALSE))
    }
    vecList <- chunk2(vec, min(length(argsOrig[[splitOn[1]]]), min(8, getOption("Ncpus"))))
  }
}

spinnerOnPid <- function(pid, isRstudio, st, verbose) {
  # if (isRstudio %in% FALSE) {
  #    messageVerbose(".", verbose)
  #  } else {
  aa <- NA
  spinner <- "|"
  mess <- if (verbose > 1) " \n" else " "

  messageVerbose(mess, verbose = verbose)
  if (Sys.time() - st > 1)
    messageVerbose("  \b\b ...  ", verbose = verbose)
  while (is.na(aa)) {
    aa <- sys::exec_status(pid, wait = FALSE)
    Sys.sleep(0.05)
    spinner <- ifelse (spinner == "|", "/",
                       ifelse(spinner == "/", "-",
                              ifelse(spinner == "-", "\\",
                                     ifelse(spinner == "\\", "|"))))
    if (Sys.time() - st > 1) {
      mess <- paste0("\b\b", spinner)
      messageVerbose(mess, verbose = verbose)
    }
  }
  messageVerbose("\b\b\b", verbose = verbose)
  # }

}


sysDo <- function(installPackages, cmdLine, logFile, verbose) {
  Rscript <- file.path(R.home("bin"), "Rscript")
  if (installPackages) {
    # if (isWindows())
    #   messageVerbose("  -- ", .txtInstallingColon,"\n", verbose = verbose, appendLF = FALSE)
    pid <- sys::exec_wait(
      Rscript, cmdLine,
      std_out = function(x) {
        mess <- rawToChar(x)
        msgStdOut(mess, logFile, verbose)
      },
      std_err = function(x) {
        mess <- rawToChar(x)
        msgStdErr(mess, logFile, verbose)
      }
    )

  } else {
    cmd <- sys::exec_background
    pid <- cmd(
      Rscript, cmdLine,
      std_out = installPackageVerbose(verbose, verboseLevel = 2),
      std_err = installPackageVerbose(verbose, verboseLevel = 2)
    )
  }
  pid
}

buildCmdLine <- function(tmpdir, fn, doLine, downAndBuildLocal, outfile, libPaths) {
  o <- options()[c('HTTPUserAgent', 'Ncpus')]
  tf <- file.path(tmpdir, basename(tempfile(fileext = ".rds")))
  tf <- normalizePath(tf, winslash = "/", mustWork = FALSE)
  saveRDS(o, file = tf)

  ar <- c(paste0("o <- readRDS('",tf,"')"),
          "options(o)",
          paste0("setwd('", normalizePath(getwd(), winslash = "/", mustWork = FALSE),"')"),
          paste0("args <- readRDS('", fn, "')"),
          doLine,
          paste0("saveRDS(outfiles, '",outfile,"')"))

  if (downAndBuildLocal) { # because only it uses Require in the spawned R system
    hasRequireDepsInstalled <- dir(libPaths)#, pattern = "Require")
    grp <- paste0("^", c(.RequireDependenciesNoBase, "gitcreds"), "$", collapse = "|")
    installed <- grep(grp, hasRequireDepsInstalled, value = TRUE)
    notInstalled <- setdiff(.RequireDependenciesNoBase, installed)
    if (length(notInstalled)) {
      # warning is about restart R; not relevant here
      suppressWarnings(Require::Install(notInstalled, verbose = -2, libPaths = libPaths[1]))
    }
    ar <- c(paste0(".libPaths('", libPaths[1], "')"), ar)
  }

  cmdLine <- unlist(lapply(ar, function(x) c("-e", x)))
  cmdLine
}

updateDoLine <- function(tmpdir, splitOn, doLineOrig) {
  tf3 <- file.path(tmpdir, basename(tempfile(fileext = ".rds")))
  tf3 <- normalizePath(tf3, winslash = "/", mustWork = FALSE)
  saveRDS(splitOn, file = tf3)
  doLine <-
    paste0("splitOn <- readRDS('",tf3,"')
                   a <- try(lapply(seq_along(args[[splitOn[1]]]), function(ind) lapply(args[splitOn], '[[', ind)))
                   extraArgs <- setdiff(names(args), splitOn)
                   for (ii in seq_along(a)) for (ea in extraArgs) a[[ii]][[ea]] <- args[[ea]]
                   try(outfiles <- lapply(a, function(args) ", doLineOrig, "))")
  doLine <- strsplit(doLine, "\n")[[1]]
  doLine
}

makeFullMessage <- function(fullMess, args, installPackages, downPack, downFile) {

  if (downPack || installPackages) {
    mess <- paste(args$pkgs, collapse = comma)
  } else if (downFile) {
    mess <- paste(extractPkgName(filenames = basename(args$url)), collapse = comma)
  } else {
    mess <- paste0(args$Account, "/", args$Repo, "@", args$Branch)
  }

  fullMess <- if (length(fullMess)) paste(fullMess, mess, sep = ", ") else mess
  fullMess
}

downloadedInSeconds <- function(et) {
  msgEndsInSeconds("packages downloaded in ", et)
  # paste("packages downloaded in ", format(et, digits = 2), " seconds.")
}

msgEndsInSeconds <- function(mess, et) {
  paste0(mess, format(et, digits = 2), " seconds.")
}

messInstallingOrDwnlding <- function(preMess, fullMess) {
  greyLight(paste0(preMess, paste(fullMess, collapse = ", ")))
}


alreadyExistingDESCFile <- function(libPaths, Repo, Account, Branch, installResult, verbose) {
  alreadyExistingDESCRIPTIONFile <- file.path(libPaths[1], Repo, "DESCRIPTION")
  SHAonGH <- getSHAfromGitHubMemoise(repo = Repo, acct = Account, br = Branch, verbose = verbose)
  if (file.exists(alreadyExistingDESCRIPTIONFile)) {
    SHAonLocal <- DESCRIPTIONFileOtherV(alreadyExistingDESCRIPTIONFile, other = "GithubSHA1")
    SHAonLocal <- unique(SHAonLocal)[1] # seems like it can be >1, which is an error
    # SHAonGH <- if (identical(SHAonGH, SHAonLocal)) FALSE else SHAonGH
    if (identical(SHAonGH, SHAonLocal)) {
      messageVerbose(msgShaNotChanged(Account, Repo, Branch),
                     verbose = verbose, verboseLevel = 1
      )
      installResult <- .txtShaUnchangedNoInstall
    }
  } else {
    SHAonLocal <- ""
  }
  list(SHAonLocal, SHAonGH, installResult)
}

avokto <- function(versionSpec, VersionOnRepos, inequality) {
  isHEAD <- versionSpec == "HEAD"
  if (isTRUE(any(isHEAD %in% TRUE))) {
    avok <- rep(TRUE, length(versionSpec))
    avokto <- avok
  }
  if (isTRUE(any(isHEAD %in% FALSE))) {
    out <- Map(vor = VersionOnRepos[!isHEAD], function(vor) any(!compareVersion2(vor, versionSpec, inequality) %in% FALSE))
    avokto <- Map(vor = VersionOnRepos[!isHEAD], function(vor) {
      all(compareVersion2(vor, versionSpec, inequality) %in% TRUE)
    })
    avok <- any(unlist(out))
  }

  list(availableVersionOK = avok, availableVersionOKthisOne = unlist(avokto))
}

packVer <- function(package, lib.loc = .libPaths()[1]) {
  DESCRIPTIONFileVersionV(file.path(lib.loc, package, "DESCRIPTION"))
}


recoverFromFail <- function(toInstallOut, toInstall, ipa, attempt, tries, repos,
                            tmpdir, libPaths, verbose, returnDetails) {
  if (attempt <= max(tries)) {
    pkgName <- strsplit(toInstallOut, .txtPkgFailed)[[1]][2]
    pkgName <- strsplit(pkgName, .txtRetrying)[[1]][1]
    if (isTRUE(!is.na(pkgName))) { #
      fileToDelete <- toInstall$localFile[toInstall$Package %in% pkgName]
      if (isTRUE(file.exists(fileToDelete)))
        unlink(fileToDelete)

      pkgInstall <- doDownloads(toInstall[Package %in% basename(pkgName)], repos = repos,
                                purge = FALSE, verbose = verbose,

                                tmpdir = tempdir(), libPaths = libPaths[1])
      doneUpTo <- which(rownames(ipa$available) == pkgName)
      keep <- seq(min(NROW(ipa$available), doneUpTo[1]),
                  NROW(ipa$available))
      newAv <- ipa$available[keep, , drop = FALSE]
      ipa$available <- newAv
      ipa$pkgs <- ipa$pkgs[keep]
      # try(Install(toInstall[Package %in% basename(pkgName)][["packageFullName"]],
      #             verbose = verbose - 1, returnDetails = returnDetails,
      #             dependencies = FALSE)) # dependencies = FALSE b/c it should already have what it needs
    } else {
      stop()
    }
  } else {
    stop("Errors installing packages, likely due to corrupt local cache files; ",
         "attempts were made to remove the corrupt cache entries. \n",
         "Try rerunning again.")
  }
  ipa
}


clearErrorReadRDSFile <- function(mess, libPath = .libPaths()[1]) {
  if (length(mess)) {
    if (isTRUE(any(grepl("Error in readRDS", mess)))) {
      dd <- dir(libPath, full.names = TRUE)
      a <- lapply(grep("_cache$", dd, value = TRUE, invert = TRUE), function(d)
        tryCatch(readRDS(file.path(d, "Meta", "package.rds")), silent = TRUE,
                 error = function(e) {print(d); unlink(d, recursive = TRUE)}))
    }
  }
}
