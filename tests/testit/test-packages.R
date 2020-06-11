if (Sys.info()["user"] != "emcintir")
  testit::assert(identical(isInteractive(), interactive()))
Require:::chooseCRANmirror2(ind = 1)

# Mock the internal functions
chooseCRANmirror2 <- function() {
  repos <- NULL
  repos2 <- "https://cloud.r-project.org"
  repos["CRAN"] <- repos2
  options("repos" = repos)
  repos
}
assignInNamespace("chooseCRANmirror2", chooseCRANmirror2, ns = "Require")

isInteractiveOrig <- isInteractive
isInteractive <- function() TRUE
assignInNamespace("isInteractive", isInteractive, ns = "Require")

### cover CRAN in case of having a environment variable set, which TRAVIS seems to
origCRAN_REPO <- Sys.getenv("CRAN_REPO")
Sys.setenv("CRAN_REPO" = "")
out <- getCRANrepos("")
isInteractive <- function() FALSE
assignInNamespace("isInteractive", isInteractive, ns = "Require")
out <- getCRANrepos("")
Sys.setenv("CRAN_REPO" = origCRAN_REPO)

assignInNamespace("isInteractive", isInteractiveOrig, ns = "Require")
repos <- Require:::getCRANrepos("")
testit::assert(is.character(repos))
testit::assert(nchar(repos) > 0)

repos <- NULL
repos2 <- "https://cloud.r-project.org"
repos["CRAN"] <- repos2

options("repos" = repos) # shouldn't be necessary now
options("Require.purge" = FALSE)

# Failure on Travis:
# cannot open file 'startup.Rs': No such file or directory
# suggested solution https://stackoverflow.com/a/27994299/3890027
Sys.setenv("R_TESTS" = "")
Sys.setenv("R_REMOTES_UPGRADE" = "never")

library(testit)

dir1 <- tempdir2("test1")
options("Require.verbose" = TRUE)
out <- Require::Require("TimeWarp (<= 2.3.1)", standAlone = TRUE, libPaths = dir1)
testit::assert(data.table::is.data.table(attr(out, "Require")))
testit::assert(isTRUE(out))
isInstalled <- tryCatch({
  out <- find.package("TimeWarp", lib.loc = dir1)
  if (length(out)) TRUE else FALSE
  }, error = function(x) FALSE)
testit::assert(isTRUE(isInstalled))
detach("package:TimeWarp", unload = TRUE)
remove.packages("TimeWarp", lib = dir1)

# Try older version
if (identical(tolower(Sys.getenv("CI")), "true") ||  # travis & appveyor
    interactive() || # interactive
    identical(Sys.getenv("NOT_CRAN"), "true")) { # CTRL-SHIFT-E
  dir2 <- tempdir2("test2")
  pvWant <- "1.0-7"
  inst <- Require::Require(paste0("TimeWarp (<=", pvWant, ")"), standAlone = TRUE,
                           libPaths = dir2, dependencies = FALSE)
  pv <- packageVersion("TimeWarp", lib.loc = dir2)
  testit::assert(pv == pvWant)
  detach("package:TimeWarp", unload = TRUE)

  # Test snapshot file
  orig <- setLibPaths(dir2, standAlone = TRUE)
  pkgSnapFile <- tempfile()
  pkgSnapshot(pkgSnapFile, .libPaths()[-length(.libPaths())])
  pkgSnapFileRes <- data.table::fread(pkgSnapFile)

  dir6 <- tempdir2("test6")
  out <- Require::Require(packageVersionFile = pkgSnapFile, libPaths = dir6,
                          install = "force")
  testit::assert(identical(packageVersion("TimeWarp", lib.loc = dir2),
                           packageVersion("TimeWarp", lib.loc = dir6)))
  detach("package:TimeWarp", unload = TRUE)
  remove.packages("TimeWarp", lib = dir2)
  remove.packages("TimeWarp", lib = dir6)

  setLibPaths(orig)

  # Test snapshot file with no args
  out <- pkgSnapshot()
  pkgSnapFileRes <- data.table::fread(formals("pkgSnapshot")$packageVersionFile)
  testit::assert(is.data.frame(out))
  testit::assert(file.exists(formals("pkgSnapshot")$packageVersionFile))
  testit::assert(isTRUE(all.equal(data.table::as.data.table(out), pkgSnapFileRes)))

  # Skip on CRAN
  dir3 <- tempdir2("test3")
  # Try github
  try(inst <- Require::Require("achubaty/fpCompare", install = "force",
                               require = FALSE, standAlone = TRUE, libPaths = dir3), silent = TRUE)
  pkgs <- c("fpCompare")

  isInstalled <- tryCatch( {
    out <- find.package(pkgs, lib.loc = dir3)
    if(length(out)) TRUE else FALSE
  }, error = function(x) FALSE)
  testit::assert(isTRUE(isInstalled))

  # Try github with version
  dir4 <- Require::tempdir2("test4")
  mess <- utils::capture.output({
    inst <- Require::Require("achubaty/fpCompare (>=2.0.0)",
                             require = FALSE, standAlone = FALSE, libPaths = dir4)
  }, type = "message")
  testit::assert(isFALSE(inst))
  testit::assert(length(mess) > 0)
  testit::assert(sum(grepl("could not be installed", mess)) == 1)
  unlink(dirname(dir3), recursive = TRUE)
}

# Code coverage
pkg <- c("rforge/mumin/pkg", "Require")
names(pkg) <- c("MuMIn", "")
out <- Require(pkg, install = FALSE, require = FALSE)
testit::assert(isFALSE(all(out)))

out <- getPkgVersions("Require")
testit::assert(is.data.table(out))
testit::assert(is.na(out$correctVersion))
out2 <- getAvailable(out)
testit::assert(is.na(out2$correctVersion))
out3 <- tryCatch(out2 <- installFrom(out2), error = function(condition) condition)
testit::assert(is(out3, "simpleError"))
out2[, installed := TRUE]
out3 <- installFrom(out2)
testit::assert(is.na(out3$correctVersion))
testit::assert(is.na(out3$installFrom))
testit::assert(is.na(out3$needInstall))

out <- getGitHubDESCRIPTION(data.table::data.table(packageFullName = "rforge/mumin/pkg"))
testit::assert(is.data.table(out))
testit::assert(!is.null(out$DESCFile))
testit::assert(file.exists(out$DESCFile))

out <- getGitHubDESCRIPTION(pkg = character())
testit::assert(length(out) == 0)

# Trigger the save available.packages and archiveAvailable
warn <- tryCatch(out <- Require("Require (>=0.0.1)", dependencies = FALSE,
                                install = "force"),
                 warning = function(x) x)
warn <- tryCatch(out <- Require("Require (>=0.0.1)", dependencies = FALSE,
                                install = "force"),
                 warning = function(x) x)
warn <- tryCatch(out <- Require("A3 (<=0.0.1)", dependencies = FALSE,
                                install = "force"),
                 warning = function(x) x)
warn <- tryCatch(out <- Require("A3 (<=0.0.1)", dependencies = FALSE,
                                install = "force"),
                 warning = function(x) x)
