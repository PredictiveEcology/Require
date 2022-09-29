thisFilename <- "test-7pkgSnapshotLong.R"
startTime <- Sys.time()
message("\033[32m --------------------------------- Starting ",thisFilename,"  at: ",format(startTime),"---------------------------\033[39m")
messageVerbose("\033[34m getOption('Require.verbose'): ", getOption("Require.verbose"), "\033[39m", verboseLevel = 0)
origLibPathsAllTests <- .libPaths()

if (interactive()) {
  library(testit)
  library(Require)
  Sys.setenv("R_REMOTES_UPGRADE" = "never")
  Sys.setenv('CRANCACHE_DISABLE' = TRUE)
  outOpts <- options("Require.persistentPkgEnv" = TRUE,
                     "install.packages.check.source" = "never",
                     "install.packages.compile.from.source" = "never",
                     "Require.unloadNamespaces" = FALSE)


  ## Long pkgSnapshot -- issue 41
  pkgPath <- file.path(tempdir2("packages"))
  checkPath(pkgPath, create = TRUE)
  download.file("https://raw.githubusercontent.com/PredictiveEcology/LandR-Manual/30a51761e0f0ce27698185985dc0fa763640d4ae/packages/pkgSnapshot.txt", destfile = file.path(pkgPath, "pkgSnapshot.txt"))
  origLibPaths <- setLibPaths(pkgPath, standAlone = TRUE)
  fn <- file.path(pkgPath, "pkgSnapshot.txt")
  pkgs <- data.table::fread(fn)
  # pks <- c("ymlthis", "SpaDES.tools", "amc")
  # pkgs <- pkgs[Package %in% pks]
  # data.table::fwrite(pkgs, fn)
  packageFullName <- ifelse(is.na(pkgs$GithubRepo), paste0(pkgs$Package, " (==", pkgs$Version, ")"),
                            paste0(pkgs$GithubUsername, "/", pkgs$GithubRepo, "@", pkgs$GithubSHA1))
  names(packageFullName) <- packageFullName
  out11 <- pkgDep(packageFullName, recursive = TRUE)
  unique(extractPkgName(unname(c(names(out11), unlist(out11)))))

  # remove.packages(pks)
  # unlink(dir(RequirePkgCacheDir(), pattern = paste(pks, collapse = "|"), full.names = TRUE))
  out <- Require(packageVersionFile = fn, require = FALSE)
  persLibPathOld <- pkgs$LibPath[which(pkgs$Package == "amc")]
  pkgDT <- attr(out, "Require")
  # pkgsInOut <- extractPkgName(pkgDT$Package[pkgDT$installed])
  installedInFistLib <- pkgs[LibPath == persLibPathOld]
  # testit::assert(all(installed))
  ip <- data.table::as.data.table(installed.packages(lib.loc = .libPaths()[1], noCache = TRUE))
  allInIPareInpkgDT <- all(ip$Package %in% pkgDT$Package[pkgDT$installed] )
  allInpkgDTareInIP <- all(pkgDT$Package[pkgDT$installed] %in% ip$Package  )


  testit::assert(isTRUE(allInIPareInpkgDT))
  testit::assert(isTRUE(allInpkgDTareInIP))
  installedNotInIP <- pkgDT[!unique(pkgDT$Package) %in% ip$Package]
  testit::assert(all(installedNotInIP$installResult == "No available version"))

  pkgsInOut <- pkgDT[installed %in% TRUE]
  theTest <- NROW(ip) >= NROW(pkgsInOut)
  testit::assert(isTRUE(theTest))

  lala <- capture.output(type = "message",
                         Require(packageVersionFile = file.path(pkgPath, "pkgSnapshot.txt"),
                                 require = FALSE, verbose = 2))
  missings <- grep("pkgSnapshot appears to be missing", lala, value = TRUE)
  missings <- gsub(".+: (.+); adding .+", "\\1", missings)
  missings <- strsplit(missings, ", ")[[1]]

  lastLineOfMessageDF <- tail(grep(":", lala), 1)
  NnotInstalled <- as.integer(strsplit(lala[lastLineOfMessageDF], split = ":")[[1]][1])
  testit::assert(NROW(pkgDT) - NnotInstalled == NROW(pkgsInOut))

  testit::assert(NROW(ip) == NROW(installedInFistLib) + length(missings) - NnotInstalled)

  setLibPaths(origLibPaths)
}

try(startTimeAll <- readRDS(file = file.path(tdOuter, "startTimeAll")), silent = TRUE) # doesn't seem to keep globals from other scripts; recreate here
unlink(tempdir2(), recursive = TRUE)
endTime <- Sys.time()
message("\033[32m ----------------------------------",thisFilename, ": ", format(endTime - startTime)," \033[39m")
try(message("\033[32m ----------------------------------All Tests: ",format(endTime - startTimeAll)," \033[39m"), silent = TRUE)
