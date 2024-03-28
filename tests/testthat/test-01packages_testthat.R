test_that("test 1", {

  setupInitial <- setupTest()
  on.exit(endTest(setupInitial))

  isDev <- getOption("Require.isDev")

  ### cover CRAN in case of having a environment variable set, which TRAVIS seems to
  origCRAN_REPO <- Sys.getenv("CRAN_REPO")
  Sys.unsetenv("CRAN_REPO")
  isInteractive <- function() FALSE
  assignInNamespace("isInteractive", isInteractive, ns = "Require")
  # browser()
  out <- getCRANrepos("")
  Sys.setenv("CRAN_REPO" = origCRAN_REPO)

  repos <- getCRANrepos("")
  testthat::expect_true({
    is.character(repos)
  })
  testthat::expect_true({
    nchar(repos) > 0
  })

  # # cannot open file 'startup.Rs': No such file or directory
  # # suggested solution https://stackoverflow.com/a/27994299/3890027
  # Sys.setenv("R_TESTS" = "")
  # Sys.setenv("R_REMOTES_UPGRADE" = "never")

  dir1 <- Require:::rpackageFolder(Require::tempdir2("test1"))
  dir1 <- Require::checkPath(dir1, create = TRUE)
  out <- suppressMessages(Require::Require("fpCompare (<= 1.2.3)",
                                           standAlone = TRUE, libPaths = dir1,
                                           quiet = TRUE, returnDetails = TRUE
  ))
  testthat::expect_true({
    data.table::is.data.table(attr(out, "Require"))
  })
  testthat::expect_true({
    isTRUE(out)
  })
  isInstalled <- tryCatch(
    {
      out <- find.package("fpCompare", lib.loc = dir1)
      if (length(out)) TRUE else FALSE
    },
    error = function(x) FALSE
  )
  testthat::expect_true({
    isTRUE(isInstalled)
  })
  out <- try(
    detachAll(
      c("Require", "fpCompare", "sdfd", "reproducible"),
      dontTry = dontDetach()),
    silent = TRUE) |>
    suppressWarnings()
  expectedPkgs <- c(sdfd = 3, fpCompare = 2, Require = 1, data.table = 1)
  keep <- intersect(names(expectedPkgs), names(out))
  out <- out[keep]
  testthat::expect_true({
    identical(sort(out), sort(expectedPkgs))
  })
  testthat::expect_true({
    names(out)[out == 2] == "fpCompare"
  })

  # detach("package:fpCompare", unload = TRUE)
  remove.packages("fpCompare", lib = dir1) |> suppressMessages()

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
    testthat::expect_true({
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
    testthat::expect_true({
      identical(
        packageVersion("fpCompare", lib.loc = dir2),
        packageVersion("fpCompare", lib.loc = dir6)
      )
    })


    remove.packages("fpCompare", lib = dir2) |> suppressMessages()
    remove.packages("fpCompare", lib = dir6) |> suppressMessages()

    setLibPaths(orig, updateRprofile = FALSE)

    # Test snapshot file with no args # on CRAN and GA, this is likely empty
    prevDir <- setwd(Require::tempdir2("test11"))
    out <- pkgSnapshot()
    pkgSnapFileRes <- data.table::fread(eval(formals("pkgSnapshot")$packageVersionFile),
                                        colClasses = "character"
    ) # if empty, they become logical
    testthat::expect_true({
      is.data.frame(out)
    })
    testthat::expect_true({
      file.exists(eval(formals("pkgSnapshot")$packageVersionFile))
    })
    out1 <- data.table::as.data.table(out)
    testthat::expect_true({
      isTRUE(all.equal(out1[], pkgSnapFileRes[], check.attributes = FALSE))
    })

    out3 <- pkgSnapshot2()
    testthat::expect_true(is(out3, "character"))
    setwd(prevDir)

    # Check for packageVersionFile = FALSE
    mess11 <- capture.output(type = "message", {
      outInner <- Require(packageVersionFile = FALSE, verbose = 5, quiet = TRUE)
    })
    testthat::expect_true(any(grepl(NoPkgsSupplied, mess11)))
    testthat::expect_true(isFALSE(outInner))

    # Skip on CRAN
    dir3 <- Require:::rpackageFolder(Require::tempdir2(Require:::.rndstr(1)))
    dir3 <- Require::checkPath(dir3, create = TRUE)
    dir.create(dir3, recursive = TRUE, showWarnings = FALSE)
    # try({
    inst <- suppressMessages(
      Require::Require("achubaty/fpCompare",
                       install = "force", returnDetails = TRUE,
                       quiet = TRUE, require = FALSE, standAlone = TRUE, libPaths = dir3
      )
    )
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
    testthat::expect_true({
      isTRUE(isInstalled)
    })

    # Try github with version
    dir4 <- Require:::rpackageFolder(Require::tempdir2("test4"))
    dir4 <- Require::checkPath(dir4, create = TRUE)
    inst <- Require::Require("achubaty/fpCompare (>=2.0.0)",
                             quiet = TRUE, require = FALSE, standAlone = FALSE, libPaths = dir4
    )
    testthat::expect_true({
      isFALSE(inst)
    })
    mess <- utils::capture.output(
      {
        inst <- Require::Require("achubaty/fpCompare (>=2.0.0)",
                                 verbose = 5,
                                 quiet = TRUE, require = FALSE, standAlone = FALSE, libPaths = dir4
        )
      },
      type = "message"
    )
    testthat::expect_true({
      length(mess) > 0
    })
    testthat::expect_true({
      sum(grepl("could not be installed", mess)) == 1
    })
    unlink(dirname(dir3), recursive = TRUE)
    unlink(dirname(dir4), recursive = TRUE)
  }

  # Code coverage
  if (isDev) { # i.e., GA, R CMD check etc.

    # Issue 87
    try(remove.packages("reproducible"), silent = TRUE) |> suppressMessages()
    Require::clearRequirePackageCache("reproducible", ask = FALSE) # just in case some previous one had the bug

    ap <- available.packagesCached(repos = getOption("repos"), purge = FALSE, type = "both")
    curVer <- unique(ap[Package %in% "reproducible"]$Version)

    Require::Install(paste0("reproducible (==", curVer, ")")) # installs current CRAN version, which is older than SHA below
    Require::Install("reproducible") |> suppressWarnings() # "package 'reproducible' was built under ..." ... load it

    (Require::Install(c("CeresBarros/reproducible@51ecfd2b1b9915da3bd012ce23f47d4b98a9f212 (HEAD)"))) |>
      capture_warnings() -> warns

    test <- testWarnsInUsePleaseChange(warns)
    expect_true(test)

    # if (length(warns))
    #   expect_true(all(grepl("in use", warns)))


    # )
    on.exit({
      try(out <- detachAll(c("Require", "fpCompare", "sdfd", "reproducible", "digest"),
                           dontTry = dontDetach())
      , silent = TRUE) |>
        suppressWarnings() |> suppressMessages()
      # unloadNamespace("package:fpCompare")
      # try(detach("package:reproducible", unload = TRUE), silent = TRUE)
    }, add = TRUE)
    testthat::expect_true(packageVersion("reproducible") == "2.0.2.9001") #
    # detach("package:reproducible", unload = TRUE);
    unloadNamespace("package:fpCompare")
    # now installs correct SHA which is 2.0.2.9001
    Require::Install(c("CeresBarros/reproducible@51ecfd2b1b9915da3bd012ce23f47d4b98a9f212 (HEAD)")) |> suppressWarnings() # "package 'reproducible' was built under ...
    testthat::expect_true(packageVersion("reproducible") == "2.0.2.9001") # was incorrectly 2.0.2 from CRAN prior to PR #87
    # End issue 87
    out <- try(
      detachAll(c("Require", "fpCompare", "sdfd", "reproducible"),
                dontTry = dontDetach()),
      silent = TRUE) |>
      suppressWarnings()

    # detach("package:reproducible", unload = TRUE)

    ####
    pkg <- c("r-forge/mumin/pkg", "Require")
    names(pkg) <- c("MuMIn", "")
    out <- Require(pkg, install = FALSE, require = FALSE)
    testthat::expect_true({
      isFALSE(all(out))
    })

    # Try a package taken off CRAN
    reallyOldPkg <- "knn"
    out <- Require(reallyOldPkg, require = FALSE)
    ip <- data.table::as.data.table(installed.packages())
    testthat::expect_true(NROW(ip[Package == reallyOldPkg]) == 1)

    out <- dlGitHubDESCRIPTION(data.table::data.table(packageFullName = "r-forge/mumin/pkg"))
    testthat::expect_true({
      data.table::is.data.table(out)
    })
    testthat::expect_true({
      !is.null(out$DESCFile)
    })
    testthat::expect_true({
      file.exists(out$DESCFile)
    })

    out <- dlGitHubDESCRIPTION(pkg = character())
    testthat::expect_true({
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


    # Test substitute(packages)
    suppressWarnings(try(remove.packages(c("quickPlot", "NetLogoR", "SpaDES", "fpCompare", "reproducible")),
                         silent = TRUE)) |> suppressMessages()
    verToCompare <- "1.0.0"
    clearRequirePackageCache(c("quickPlot", "NetLogoR", "SpaDES"), ask = FALSE)

    # The warning is about "package ‘Require’ is in use and will not be installed"

    out2 <- Require::Install(
      c("quickPlot (< 1.0.0)", "NetLogoR",
        # This is needed b/c .unwrap is not exported from reproducible on CRAN
        ifelse(isWindows(), "reproducible", "PredictiveEcology/reproducible@modsForLargeArchives (HEAD)"),
        "SpaDES")#,
      # repos = c("https://predictiveecology.r-universe.dev", getOption("repos"))
      ) |>
      capture_warnings() -> warns

    test <- testWarnsInUsePleaseChange(warns)
    expect_true(test)

    # if (length(warns)) {
    #   test <- all(grepl("in use|Please change required", warns)) # "Please change" comes with verbose >= 1
    #   expect_true(test)
    #   # expect_true(all(grepl("in use", warns)))
    # }


    testthat::expect_true(packageVersion("SpaDES") >= verToCompare)
    try(remove.packages(c("quickPlot", "NetLogoR", "SpaDES", "fpCompare", "SpaDES.core"))) |> suppressMessages()
    clearRequirePackageCache(c("quickPlot", "NetLogoR", "SpaDES", "SpaDES.core"), ask = F)
    a <- list(pkg = "fpCompare")

    warns <- capture_warnings(
      out <- Require::Install(
        c(paste0("quickPlot (< ",verToCompare,")"), NetLogoR,
          "quickPlot (>= 1.0.1)", "quickPlot (>= 0.9.0)",
          "SpaDES.core (== 2.0.3)", a$pkg),
        repos = c("https://predictiveecology.r-universe.dev", getOption("repos")),
        returnDetails = TRUE)
    )
    testthat::expect_true(packageVersion("quickPlot") != verToCompare)

    warns <- capture_warnings(
      out <- Require::Install(
        c(paste0("quickPlot (< ",verToCompare,")")),
        repos = c("https://predictiveecology.r-universe.dev", getOption("repos")),
        returnDetails = TRUE)
    )
    testthat::expect_true(packageVersion("quickPlot") < verToCompare)

  }
})
