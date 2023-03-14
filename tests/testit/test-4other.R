setupInitial <- setupTest()
library(Require)
# Test misspelled
out <- capture.output(type = "message", lala <- Require("data.tt", verbose = 1))
testit::assert(any(grepl("could not be installed", out))) # {out, "simpleWarning")})

# for coverages that were missing
pkgDTEmpty <- Require:::toPkgDT(character())
out <- Require:::installedVers(pkgDTEmpty) #


pkgDep("data.table", purge = FALSE)
if (!isDev) {
  pkgDep("data.table", purge = TRUE)
}
pkgDep2("Require")


pkgDepTopoSort(c("data.table"), useAllInSearch = TRUE)
pkgDepTopoSort(c("data.table"), useAllInSearch = TRUE, deps = "Require")
pkgDepTopoSort(c("Require", "data.table"))
pkgDepTopoSort(c("Require", "data.table"),
  useAllInSearch = TRUE,
  deps = "Require", returnFull = FALSE, reverse = TRUE
)

Require:::pkgDepCRAN("Require", keepVersionNumber = TRUE, purge = TRUE)


if (Sys.info()["user"] == "emcintir2") {
  options(
    "Require.RPackageCache" = TRUE,
    "Require.unloadNamespaces" = FALSE
  )
}
Require("data.table",
  install = "force", require = FALSE, libPaths = tempdir2("other"),
  quiet = !(getOption("Require.verbose") >= 1)
)
suppressWarnings(Require("Require",
  install = "force", require = FALSE,
  libPaths = tempdir2("other")
))

pkg <- c("data.table", "data.table")
pkgDT <- Require:::toPkgDT(pkg)
data.table::set(pkgDT, NULL, "installFrom", "CRAN")
data.table::set(pkgDT, NULL, "installed", FALSE)
data.table::set(pkgDT, NULL, "installResult", TRUE)

data.table::set(pkgDT, NULL, "versionSpec", NA)

out <- detachAll("data.table", dontTry = "testit")
testit::assert({
  isTRUE(out["data.table"] == 1)
})

warn <- tryCatch(Require:::warningCantInstall("devtolls"), warning = function(w) w$message)
testit::assert({
  grepl("you will likely", warn)
})

origLP <- setLibPaths(tempdir2("other"), updateRprofile = FALSE)
warn <- tryCatch(Require("data.table", require = FALSE), warning = function(w) w$message)
silent <- setLibPaths(origLP, updateRprofile = FALSE)

# Test the setLibPaths with changed .Rprofile
origDir <- setwd(tempdir2("other"))
setLibPaths("newProjectLib", updateRprofile = TRUE) # set a new R package library locally
setLibPaths() # reset it to original
setwd(origDir)

## setup
# setupTestDir <- normPath(tempdir2("setupTests"))
# ccc <- checkPath(file.path(setupTestDir, ".cache"), create = TRUE)
# out2222 <- capture.output(setup(setupTestDir, RPackageCache = ccc))
# testit::assert(identical(getOption("Require.RPackageCache"), ccc)) ## TODO: warnings in readLines() cannot open DESCRIPTION file
# out2222 <- capture.output(setupOff())
# Require:::messageVerbose("This is getOption('Require.RPackageCache'): ", Require:::getOptionRPackageCache(),
#                verboseLevel = 0)
# RPackageCacheSysEnv <- Sys.getenv("R_REQUIRE_PKG_CACHE")
# if (identical(RPackageCacheSysEnv, "FALSE") ) {
#   testit::assert(identical(NULL, getOptionRPackageCache()))
# } else {
#   if (!(is.null(Require:::getOptionRPackageCache()) || Require:::getOptionRPackageCache() == "FALSE"))
#     testit::assert(identical(normPath(Require:::getOptionRPackageCache()), normPath(Require::RequirePkgCacheDir())))
# }

# reset options after setupOff()
# secondTry <- normPath(file.path(setupTestDir, ".cacheSecond"))
# opt22 <- options("Require.RPackageCache" = secondTry)
# ccc <- checkPath(secondTry, create = TRUE)
# out2222 <- capture.output(setup(setupTestDir, RPackageCache = ccc)) ## TODO: warnings in file() cannot open DESCRIPTION files
# testit::assert(identical(Require:::getOptionRPackageCache(), ccc))
# out2222 <- capture.output(setupOff())
# testit::assert(identical(Require:::getOptionRPackageCache(), secondTry)) # BECAUSE THIS IS A MANUAL OVERRIDE of options; doesn't return Sys.getenv

ooo <- options(Require.RPackageCache = TRUE)
testit::assert(identical(getOptionRPackageCache(), RequirePkgCacheDir()))
ooo <- options(Require.RPackageCache = FALSE)
testit::assert(identical(getOptionRPackageCache(), NULL))
ooo <- options(Require.RPackageCache = tempdir())
testit::assert(identical(getOptionRPackageCache(), tempdir()))
ooo <- options(Require.RPackageCache = "default")
RPackageCacheSysEnv <- Sys.getenv("R_REQUIRE_PKG_CACHE")
if (identical(RPackageCacheSysEnv, "FALSE")) {
  testit::assert(identical(NULL, getOptionRPackageCache()))
} else {
  if (!(is.null(Require:::getOptionRPackageCache()) || Require:::getOptionRPackageCache() == "FALSE")) {
    testit::assert(identical(normPath(Require:::getOptionRPackageCache()), normPath(Require::RequirePkgCacheDir())))
  }
}

ro <- RequireOptions()
gro <- getRequireOptions()

testit::assert(is.list(ro))
testit::assert(all(startsWith(names(ro), "Require")))
testit::assert(is.list(gro))
testit::assert(all(startsWith(names(gro), "Require")))

# Ensure the "which" for pkgDep are working correctly
wh <- expand.grid(suggests = c(TRUE, FALSE), depends = c(TRUE, FALSE), imports = c(TRUE, FALSE), linkingTo = c(TRUE, FALSE))
utilsOut <- list()
for (i in c("Suggests", "Imports", "Depends", "LinkingTo"))
  utilsOut[[i]] <- strsplit(gsub("\n", "", utils::packageDescription("Require", fields = i)), ",")[[1]]

out2 <- by(wh, seq(NROW(wh)), function(wh1Row) {
  out <- do.call(pkgDep, append(list("Require"), as.list(wh1Row[1, , drop = TRUE])))[[1]]
  o2 <- tools::toTitleCase(names(wh1Row)[unlist(wh1Row)])
  if (length(o2)) {
    pkgs <- unname(unlist(utilsOut[o2]))
    out <- setdiff(out, grep("R .+", pkgs, value = TRUE, invert = TRUE))
  }
  setdiff(out, "remotes") # remotes was removed in version 0.2.6.9020
  })
testArgs <- all("Require" == unlist(as.list(out2)))
testit::assert(isTRUE(testArgs))



ooo <- options(Require.RPackageCache = NULL)
testit::assert(identical(getOptionRPackageCache(), NULL))
options(ooo)

endTest(setupInitial)
