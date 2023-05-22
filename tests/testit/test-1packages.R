setupInitial <- setupTest()

### cover CRAN in case of having a environment variable set, which TRAVIS seems to
origCRAN_REPO <- Sys.getenv("CRAN_REPO")
Sys.unsetenv("CRAN_REPO")
isInteractive <- function() FALSE
assignInNamespace("isInteractive", isInteractive, ns = "Require")
out <- Require:::getCRANrepos("")
Sys.setenv("CRAN_REPO" = origCRAN_REPO)

repos <- Require:::getCRANrepos("")
testit::assert({
  is.character(repos)
})
testit::assert({
  nchar(repos) > 0
})

# # cannot open file 'startup.Rs': No such file or directory
# # suggested solution https://stackoverflow.com/a/27994299/3890027
# Sys.setenv("R_TESTS" = "")
# Sys.setenv("R_REMOTES_UPGRADE" = "never")

library(testit)

dir1 <- Require:::rpackageFolder(Require::tempdir2("test1"))
dir1 <- Require::checkPath(dir1, create = TRUE)
out <- suppressMessages(Require::Require("fpCompare (<= 1.2.3)",
  standAlone = TRUE, libPaths = dir1,
  quiet = TRUE, verbose = 2
))
testit::assert({
  data.table::is.data.table(attr(out, "Require"))
})
testit::assert({
  isTRUE(out)
})
isInstalled <- tryCatch(
  {
    out <- find.package("fpCompare", lib.loc = dir1)
    if (length(out)) TRUE else FALSE
  },
  error = function(x) FALSE
)
testit::assert({
  isTRUE(isInstalled)
})
out <- detachAll(c("Require", "fpCompare", "sdfd"), dontTry = "testit")
out <- out[names(out) != "testit"]
expectedPkgs <- c(sdfd = 3, fpCompare = 2, Require = 1, data.table = 1)
keep <- intersect(names(expectedPkgs), names(out))
out <- out[keep]
testit::assert({
  identical(sort(out), sort(expectedPkgs))
})
testit::assert({
  names(out)[out == 2] == "fpCompare"
})

# detach("package:fpCompare", unload = TRUE)
remove.packages("fpCompare", lib = dir1)

# Try older version
if (identical(tolower(Sys.getenv("CI")), "true") || # travis
  isDevAndInteractive || # interactive
  identical(Sys.getenv("NOT_CRAN"), "true")) { # CTRL-SHIFT-E
  dir2 <- Require:::rpackageFolder(Require::tempdir2("test2"))
  dir2 <- Require::checkPath(dir2, create = TRUE)
  pvWant <- "0.2.2"
  inst <- Require::Require(paste0("fpCompare (<=", pvWant, ")"),
    standAlone = TRUE,
    libPaths = dir2, dependencies = FALSE, quiet = TRUE, require = FALSE
  )
  pv <- packageVersion("fpCompare", lib.loc = dir2)
  testit::assert({
    pv == pvWant
  })
  # Test snapshot file
  orig <- setLibPaths(dir2, standAlone = TRUE, updateRprofile = FALSE)
  pkgSnapFile <- tempfile()
  pkgSnapshot(pkgSnapFile, .libPaths()[-length(.libPaths())])
  pkgSnapFileRes <- data.table::fread(pkgSnapFile)
  dir6 <- Require:::rpackageFolder(Require::tempdir2("test6"))
  dir6 <- Require::checkPath(dir6, create = TRUE)
  out <- Require::Require(
    packageVersionFile = pkgSnapFile, libPaths = dir6,
    quiet = TRUE, install = "force"
  )
  testit::assert({
    identical(
      packageVersion("fpCompare", lib.loc = dir2),
      packageVersion("fpCompare", lib.loc = dir6)
    )
  })


  remove.packages("fpCompare", lib = dir2)
  remove.packages("fpCompare", lib = dir6)

  setLibPaths(orig, updateRprofile = FALSE)

  # Test snapshot file with no args # on CRAN and GA, this is likely empty
  prevDir <- setwd(Require::tempdir2("test11"))
  out <- pkgSnapshot()
  pkgSnapFileRes <- data.table::fread(eval(formals("pkgSnapshot")$packageVersionFile),
    colClasses = "character"
  ) # if empty, they become logical
  testit::assert({
    is.data.frame(out)
  })
  testit::assert({
    file.exists(eval(formals("pkgSnapshot")$packageVersionFile))
  })
  out1 <- data.table::as.data.table(out)
  testit::assert({
    isTRUE(all.equal(out1[], pkgSnapFileRes[], check.attributes = FALSE))
  })

  out3 <- pkgSnapshot2()
  testit::assert(is(out3, "character"))
  setwd(prevDir)

  # Check for packageVersionFile = FALSE
  mess11 <- capture.output(type = "message", {
    outInner <- Require(packageVersionFile = FALSE, verbose = 1, quiet = TRUE)
  })
  testit::assert(any(grepl(NoPkgsSupplied, mess11)))
  testit::assert(isFALSE(outInner))

  # Skip on CRAN
  dir3 <- Require:::rpackageFolder(Require::tempdir2(Require:::.rndstr(1)))
  dir3 <- Require::checkPath(dir3, create = TRUE)
  dir.create(dir3, recursive = TRUE, showWarnings = FALSE)
  # try({
  inst <- suppressMessages(Require::Require("achubaty/fpCompare",
    install = "force", verbose = 2,
    quiet = TRUE, require = FALSE, standAlone = TRUE, libPaths = dir3
  ))
  attrOut <- capture.output(type = "message", Require:::messageDF(attr(inst, "Require")))
  # }, silent = TRUE)
  pkgs <- c("fpCompare")

  isInstalled <- tryCatch(
    {
      out <- find.package(pkgs, lib.loc = dir3)
      if (length(out)) TRUE else FALSE
    },
    error = function(x) FALSE
  )
  testit::assert({
    isTRUE(isInstalled)
  })

  # Try github with version
  dir4 <- Require:::rpackageFolder(Require::tempdir2("test4"))
  dir4 <- Require::checkPath(dir4, create = TRUE)
  inst <- Require::Require("achubaty/fpCompare (>=2.0.0)",
    quiet = TRUE, require = FALSE, standAlone = FALSE, libPaths = dir4
  )
  testit::assert({
    isFALSE(inst)
  })
  mess <- utils::capture.output(
    {
      inst <- Require::Require("achubaty/fpCompare (>=2.0.0)",
        verbose = 1,
        quiet = TRUE, require = FALSE, standAlone = FALSE, libPaths = dir4
      )
    },
    type = "message"
  )
  testit::assert({
    length(mess) > 0
  })
  testit::assert({
    sum(grepl("could not be installed", mess)) == 1
  })
  unlink(dirname(dir3), recursive = TRUE)
  unlink(dirname(dir4), recursive = TRUE)
}

# Code coverage
if (isDev) { # i.e., GA, R CMD check etc.
  pkg <- c("r-forge/mumin/pkg", "Require")
  names(pkg) <- c("MuMIn", "")
  aaaa <<- 1
  out <- Require(pkg, install = FALSE, require = FALSE)
  testit::assert({
    isFALSE(all(out))
  })

  # Try a package taken off CRAN
  reallyOldPkg <- "ggplot"
  out <- Require(reallyOldPkg, require = FALSE)
  ip <- data.table::as.data.table(installed.packages())
  testit::assert(NROW(ip[Package == reallyOldPkg]) == 1)

  out <- getGitHubDESCRIPTION(data.table::data.table(packageFullName = "r-forge/mumin/pkg"))
  testit::assert({
    data.table::is.data.table(out)
  })
  testit::assert({
    !is.null(out$DESCFile)
  })
  testit::assert({
    file.exists(out$DESCFile)
  })

  out <- getGitHubDESCRIPTION(pkg = character())
  testit::assert({
    length(out) == 0
  })

  # Trigger the save available.packages and archiveAvailable
  # warn <- tryCatch(out <- Require("Require (>=0.0.1)", dependencies = FALSE,
  #                                 install = "force"),
  #                  error = function(x) x)
  # warn <- tryCatch(out <- Require("Require (>=0.0.1)", dependencies = FALSE,
  #                                 install = "force"),
  #                  error = function(x) x)
  if (isDevAndInteractive) {
    warn <- tryCatch(
      {
        out <- Require("A3 (<=0.0.1)", dependencies = FALSE, install = "force")
      },
      warning = function(x) x
    )
    warn <- tryCatch(
      {
        out <- Require("A3 (<=0.0.1)", dependencies = FALSE, install = "force")
      },
      warning = function(x) x
    )
  }
}
endTest(setupInitial)
