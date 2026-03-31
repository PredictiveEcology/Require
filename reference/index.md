# Package index

## All functions

- [`DESCRIPTIONFileVersionV()`](https://Require.predictiveecology.org/reference/DESCRIPTION-helpers.md)
  [`DESCRIPTIONFileOtherV()`](https://Require.predictiveecology.org/reference/DESCRIPTION-helpers.md)
  [`dlGitHubDESCRIPTION()`](https://Require.predictiveecology.org/reference/DESCRIPTION-helpers.md)
  : GitHub package tools

- [`parseGitHub()`](https://Require.predictiveecology.org/reference/GitHubTools.md)
  : Parse a github package specification

- [`Require()`](https://Require.predictiveecology.org/reference/Require.md)
  [`Install()`](https://Require.predictiveecology.org/reference/Require.md)
  : Require: Installing and Loading R Packages for Reproducible
  Workflows

- [`RequireOptions()`](https://Require.predictiveecology.org/reference/RequireOptions.md)
  [`getRequireOptions()`](https://Require.predictiveecology.org/reference/RequireOptions.md)
  :

  `Require` options

- [`availablePackagesOverride()`](https://Require.predictiveecology.org/reference/availablePackagesOverride.md)
  : Create a custom "available.packages" object

- [`availableVersionOK()`](https://Require.predictiveecology.org/reference/availableVersionOK.md)
  :

  Needs `VersionOnRepos`, `versionSpec` and `inequality` columns

- [`dlArchiveVersionsAvailable()`](https://Require.predictiveecology.org/reference/availableVersions.md)
  [`available.packagesCached()`](https://Require.predictiveecology.org/reference/availableVersions.md)
  : Available and archived versions

- [`cacheDefaultDir()`](https://Require.predictiveecology.org/reference/cacheDefaultDir.md)
  : The default cache directory for Require Cache

- [`cacheDir()`](https://Require.predictiveecology.org/reference/cacheDir.md)
  [`cachePkgDir()`](https://Require.predictiveecology.org/reference/cacheDir.md)
  : Path to (package) cache directory

- [`cacheGetOptionCachePkgDir()`](https://Require.predictiveecology.org/reference/cacheGetOptionCachePkgDir.md)
  :

  Get the option for `Require.cachePkgDir`

- [`cachePurge()`](https://Require.predictiveecology.org/reference/cachePurge.md)
  [`purgeCache()`](https://Require.predictiveecology.org/reference/cachePurge.md)
  : Purge everything in the Require cache

- [`checkLibPaths()`](https://Require.predictiveecology.org/reference/checkLibPaths.md)
  : Creates the directories, and adds version number

- [`checkPath()`](https://Require.predictiveecology.org/reference/checkPath.md)
  : Check directory path

- [`cacheClearPackages()`](https://Require.predictiveecology.org/reference/clearRequire.md)
  [`clearRequirePackageCache()`](https://Require.predictiveecology.org/reference/clearRequire.md)
  : Clear Require Cache elements

- [`compareVersion2()`](https://Require.predictiveecology.org/reference/compareVersion2.md)
  : Compare package versions

- [`dealWithMissingLibPaths()`](https://Require.predictiveecology.org/reference/dealWithMissingLibPaths.md)
  : Only checks for deprecated libPath argument (singular)

- [`detachAll()`](https://Require.predictiveecology.org/reference/detachAll.md)
  : Detach and unload all packages

- [`doLibPaths()`](https://Require.predictiveecology.org/reference/doLibPaths.md)
  : Deals with missing libPaths arg, and takes first

- [`.downloadFileMasterMainAuth()`](https://Require.predictiveecology.org/reference/dot-downloadFileMasterMainAuth.md)
  :

  GITHUB_PAT-aware and `main`-`master`-aware download from GitHub

- [`.installed.pkgs()`](https://Require.predictiveecology.org/reference/dot-installed.pkgs.md)
  :

  Partial alternative (faster) to `installed.packages`

- [`envPkgCreate()`](https://Require.predictiveecology.org/reference/envPkgCreate.md)
  : 1st level –\> create the .pkgEnv object in Require

- [`envPkgDepDESCFileCreate()`](https://Require.predictiveecology.org/reference/envPkgDepDESCFileCreate.md)
  : 3rd level for DESCRIPTIONFile

- [`envPkgDepDepsCreate()`](https://Require.predictiveecology.org/reference/envPkgDepDepsCreate.md)
  : 3rd level for deps \#############################################

- [`extractPkgName()`](https://Require.predictiveecology.org/reference/extractPkgName.md)
  [`extractVersionNumber()`](https://Require.predictiveecology.org/reference/extractPkgName.md)
  [`extractInequality()`](https://Require.predictiveecology.org/reference/extractPkgName.md)
  [`extractPkgGitHub()`](https://Require.predictiveecology.org/reference/extractPkgName.md)
  : Extract info from package character strings

- [`getDeps()`](https://Require.predictiveecology.org/reference/getDeps.md)
  :

  The `packages` argument may have up to 4 pieces of information for
  GitHub packages: name, repository, branch, version. For CRAN-alikes,
  it will only be 2 pieces: name, version. There can also be an
  inequality or equality, if there is a version.

- [`invertList()`](https://Require.predictiveecology.org/reference/invertList.md)
  : Invert a 2-level list

- [`joinToAvailablePackages()`](https://Require.predictiveecology.org/reference/joinToAvailablePackages.md)
  :

  Join a data.table with a `Package` column to `available.packages`

- [`linkOrCopy()`](https://Require.predictiveecology.org/reference/linkOrCopy.md)
  [`fileRenameOrMove()`](https://Require.predictiveecology.org/reference/linkOrCopy.md)
  : Create link to file, falling back to making a copy if linking fails.

- [`masterMainToHead()`](https://Require.predictiveecology.org/reference/masterMainToHead.md)
  : This converts master or main to HEAD for a git repo

- [`messageDF()`](https://Require.predictiveecology.org/reference/messageVerbose.md)
  [`messageVerbose()`](https://Require.predictiveecology.org/reference/messageVerbose.md)
  [`messageVerboseCounter()`](https://Require.predictiveecology.org/reference/messageVerbose.md)
  : Use message to print a clean square data structure

- [`modifyList2()`](https://Require.predictiveecology.org/reference/modifyList2.md)
  [`modifyList3()`](https://Require.predictiveecology.org/reference/modifyList2.md)
  :

  `modifyList` for multiple lists

- [`normPath()`](https://Require.predictiveecology.org/reference/normPath.md)
  : Normalize filepath

- [`paddedFloatToChar()`](https://Require.predictiveecology.org/reference/paddedFloatToChar.md)
  : Convert numeric to character with padding

- [`pakEnv()`](https://Require.predictiveecology.org/reference/pakEnv.md)
  : 2nd level

- [`pkgDepTopoSort()`](https://Require.predictiveecology.org/reference/pkgDep.md)
  [`pkgDep2()`](https://Require.predictiveecology.org/reference/pkgDep.md)
  [`pkgDep()`](https://Require.predictiveecology.org/reference/pkgDep.md)
  : Reverse package depends

- [`pkgDepEnv()`](https://Require.predictiveecology.org/reference/pkgDepEnv.md)
  : 2nd level

- [`pkgDepIfDepRemoved()`](https://Require.predictiveecology.org/reference/pkgDepIfDepRemoved.md)
  : Package dependencies when one or more packages removed

- [`pkgSnapshot()`](https://Require.predictiveecology.org/reference/pkgSnapshot.md)
  [`pkgSnapshot2()`](https://Require.predictiveecology.org/reference/pkgSnapshot.md)
  : Take a snapshot of all the packages and version numbers

- [`rmBase()`](https://Require.predictiveecology.org/reference/rmBase.md)
  :

  Recursive function to remove `.basePkgs`

- [`rversions`](https://Require.predictiveecology.org/reference/rversions.md)
  : R versions

- [`setLibPaths()`](https://Require.predictiveecology.org/reference/setLibPaths.md)
  :

  Set `.libPaths`

- [`setLinuxBinaryRepo()`](https://Require.predictiveecology.org/reference/setLinuxBinaryRepo.md)
  : Setup for binary Linux repositories

- [`setdiffNamed()`](https://Require.predictiveecology.org/reference/setdiffNamed.md)
  :

  Like `setdiff`, but takes into account names

- [`setup()`](https://Require.predictiveecology.org/reference/setup.md)
  [`setupOff()`](https://Require.predictiveecology.org/reference/setup.md)
  : Setup a project library, cache, options

- [`sourcePkgs()`](https://Require.predictiveecology.org/reference/sourcePkgs.md)
  : A list of R packages that should likely be installed from Source,
  not Binary

- [`splitKeepOrderAndDTIntegrity()`](https://Require.predictiveecology.org/reference/splitKeepOrderAndDTIntegrity.md)
  :

  `split` for a data.table that keeps integrity of a column of lists of
  data.table objects

- [`sysInstallAndDownload()`](https://Require.predictiveecology.org/reference/sysInstallAndDownload.md)
  : download.files or install.packages in a separate process

- [`tempdir2()`](https://Require.predictiveecology.org/reference/tempdir2.md)
  : Make a temporary (sub-)directory

- [`tempfile2()`](https://Require.predictiveecology.org/reference/tempfile2.md)
  : Make a temporary subfile in a temporary (sub-)directory

- [`trimVersionNumber()`](https://Require.predictiveecology.org/reference/trimVersionNumber.md)
  : Trim version number off a compound package name

- [`updatePackages()`](https://Require.predictiveecology.org/reference/updatePackages.md)
  : Update installed packages with latest available versions
