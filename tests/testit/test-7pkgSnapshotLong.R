setupInitial <- setupTest()

if (isDevAndInteractive && !isMacOSX()) { ## TODO: source installs failing on macOS
  ## Long pkgSnapshot -- issue 41
  pkgPath <- file.path(tempdir2(Require:::.rndstr(1)))
  checkPath(pkgPath, create = TRUE)
  download.file("https://raw.githubusercontent.com/PredictiveEcology/LandR-Manual/30a51761e0f0ce27698185985dc0fa763640d4ae/packages/pkgSnapshot.txt",
    destfile = file.path(pkgPath, "pkgSnapshot.txt")
  )
  origLibPaths <- setLibPaths(pkgPath, standAlone = TRUE)
  fn <- file.path(pkgPath, "pkgSnapshot.txt")
  pkgs <- data.table::fread(fn)
  pkgs <- pkgs[!(Package %in% "SpaDES.install")]
  pkgs[Package %in% "sf", Version := "1.0-9"] # the version 1.0-7 is corrupt on RSPM
  pkgs[Package %in% "SpaDES.core", `:=`(Version = "1.1.1", GithubRepo = "SpaDES.core",
                                        GithubUsername = "PredictiveEcology", GithubRef = "development",
                                        GithubSHA1 = "535cd39d84aeb35de29f88b0245c9538d86a1223")]
  # pks <- c("ymlthis", "SpaDES.tools", "amc")
  # pkgs <- pkgs[Package %in% pks]
  data.table::fwrite(pkgs, file = fn) # have to get rid of SpaDES.install
  packageFullName <- ifelse(is.na(pkgs$GithubRepo), paste0(pkgs$Package, " (==", pkgs$Version, ")"),
    paste0(pkgs$GithubUsername, "/", pkgs$GithubRepo, "@", pkgs$GithubSHA1)
  )
  names(packageFullName) <- packageFullName

  # remove.packages(pks)
  # unlink(dir(RequirePkgCacheDir(), pattern = paste(pks, collapse = "|"), full.names = TRUE))
  out <- Require(packageVersionFile = fn, require = FALSE)
  out11 <- pkgDep(packageFullName, recursive = TRUE)
  allNeeded <- unique(extractPkgName(unname(c(names(out11), unlist(out11)))))
  allNeeded <- allNeeded[!allNeeded %in% .basePkgs]
  persLibPathOld <- pkgs$LibPath[which(pkgs$Package == "amc")]
  # pkgDT <- attr(out, "Require")
  # pkgsInOut <- extractPkgName(pkgDT$Package[pkgDT$installed])
  installedInFistLib <- pkgs[LibPath == persLibPathOld]
  # testit::assert(all(installed))
  ip <- data.table::as.data.table(installed.packages(lib.loc = .libPaths()[1], noCache = TRUE))
  ip <- ip[!Package %in% .basePkgs]
  allInIPareInpkgDT <- all(ip$Package %in% allNeeded)
  installedNotInIP <- setdiff(allNeeded, ip$Package)

  installedPkgs <- setdiff(allNeeded, installedNotInIP)
  allInpkgDTareInIP <- all(installedPkgs %in% ip$Package)
  if (!isTRUE(allInpkgDTareInIP)) browser()
  if (!isTRUE(allInIPareInpkgDT)) browser()

  testit::assert(isTRUE(allInIPareInpkgDT))
  testit::assert(isTRUE(allInpkgDTareInIP))
  # testit::assert(all(installedNotInIP$installResult == "No available version"))

  pkgsInOut <- allInpkgDTareInIP
  theTest <- NROW(ip) >= NROW(pkgsInOut)
  testit::assert(isTRUE(theTest))

  lala <- capture.output(type = "message", {
    out <- Require(
      packageVersionFile = file.path(pkgPath, "pkgSnapshot.txt"),
      require = FALSE, verbose = 2, purge = TRUE
    )
  })
  # missings <- grep("The following shows packages", lala, value = TRUE)
  # missings <- gsub(".+: (.+); adding .+", "\\1", missings)
  # missings <- strsplit(missings, ", ")[[1]]
  #
  # if (any(grepl(Require:::messageFollowingPackagesIncorrect, lala))) {
  #   lastLineOfMessageDF <- tail(grep(":", lala), 1)
  #   NnotInstalled <- as.integer(strsplit(lala[lastLineOfMessageDF], split = ":")[[1]][1])
  # } else {
  #   NnotInstalled <- 0
  # }
  allNeeded <- setdiff(allNeeded, "Require")
  installedPkgs <- setdiff(installedPkgs, "Require")

  theTest <- NROW(installedPkgs) == NROW(allNeeded)
  if (isDevAndInteractive) if (!isTRUE(theTest)) browser()
  testit::assert(isTRUE(theTest))

  theTest2 <- NROW(ip[Package %in% allNeeded]) == NROW(allNeeded)
  testit::assert(isTRUE(theTest2))

  setLibPaths(origLibPaths)
}
endTest(setupInitial)
