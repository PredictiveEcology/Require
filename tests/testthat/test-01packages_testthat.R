test_that("test 1", {

  setupInitial <- setupTest()
  # on.exit(endTest(setupInitial))

  isDev <- getOption("Require.isDev")

  ### cover CRAN in case of having a environment variable set, which TRAVIS seems to
  origCRAN_REPO <- Sys.getenv("CRAN_REPO")
  Sys.unsetenv("CRAN_REPO")
  isInteractive <- function() FALSE
  assignInNamespace("isInteractive", isInteractive, ns = "Require")
  out <- getCRANrepos("")
  Sys.setenv("CRAN_REPO" = origCRAN_REPO)

  repos <- getCRANrepos("")
  testthat::expect_true({
    is.character(repos)
  })
  testthat::expect_true({
    all(nchar(repos) > 0) # may have binary also
  })

  # # cannot open file 'startup.Rs': No such file or directory
  # # suggested solution https://stackoverflow.com/a/27994299/3890027
  # Sys.setenv("R_TESTS" = "")
  # Sys.setenv("R_REMOTES_UPGRADE" = "never")

  dir1 <- Require:::rpackageFolder(Require:::tempdir3("test1"))
  dir1 <- Require::checkPath(dir1, create = TRUE)
  (out <- suppressMessages(Require::Require("fpCompare (<= 1.2.3)",
                                            standAlone = TRUE, libPaths = dir1,
                                            # quiet = TRUE,
                                            returnDetails = TRUE
  ))) |> capture_warnings() -> warns
  if (length(warns))
    expect_true(all(grepl("was built under", warns)))

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
  if (!getOption("Require.usePak")) {
    out <- try(
      detachAll(
        c("Require", "fpCompare", "sdfd", "reproducible"),
        dontTry = dontDetach()),
      silent = TRUE) |>
      suppressWarnings()
    if (!is(out, "try-error")) {
      expectedPkgs <- c(sdfd = 3, fpCompare = 2, Require = 1, data.table = 1)
      keep <- intersect(names(expectedPkgs), names(out))
      out <- out[keep]
      expect_identical(sort(out), sort(expectedPkgs))

      expect_identical(names(out)[out == 2], "fpCompare")
    }
  }

  # detach("package:fpCompare", unload = TRUE)
  remove.packages("fpCompare", lib = dir1) |> suppressMessages()

  # Try older version
  if (identical(tolower(Sys.getenv("CI")), "true") || # travis
      isDevAndInteractive || # interactive
      identical(Sys.getenv("NOT_CRAN"), "true")) { # CTRL-SHIFT-E
    dir2 <- rpackageFolder(tempdir3())
    dir2 <- checkPath(dir2, create = TRUE)
    pvWant <- "0.2.2"
    warns <- capture_warnings(
      inst <- Install(paste0("fpCompare (<=", pvWant, ")"),
                      standAlone = TRUE,
                      libPaths = dir2, dependencies = FALSE, returnDetails = TRUE
      )
    )
    fpC <- "fpCompare"
    pv <- packVer(fpC, dir2)
    # pv <- DESCRIPTIONFileVersionV(file.path(dir2, "fpCompare/DESCRIPTION"))
    # pv <- packageVersion(vers, lib.loc = dir2)
    testthat::expect_true({
      pv <= pvWant
    })
    # Test snapshot file
    orig <- setLibPaths(dir2, standAlone = TRUE, updateRprofile = FALSE)
    pkgSnapFile <- tempfile()
    pkgSnapshot(pkgSnapFile, libPaths = .libPaths()[-length(.libPaths())])
    pkgSnapFileRes <- data.table::fread(pkgSnapFile)
    dir6 <- Require:::rpackageFolder(Require::tempdir2("test6"))
    dir6 <- Require::checkPath(dir6, create = TRUE)
    warns <- capture_warnings(
      out <- Require::Require(
        packageVersionFile = pkgSnapFile, libPaths = dir6,
        quiet = TRUE, install = "force"
      )
    )
    if (isTRUE(getOption("Require.usePak"))) {
      okWarn <- grepl(.txtPakCurrentlyPakNoSnapshots, warns)
      expect_true(okWarn)
    }

    vers2 <- packVer(fpC, dir2)
    vers6 <- packVer(fpC, dir6)
    #
    # vers2 <- DESCRIPTIONFileVersionV(file.path(dir2, "fpCompare/DESCRIPTION"))
    # vers6 <- DESCRIPTIONFileVersionV(file.path(dir6, "fpCompare/DESCRIPTION"))
    testthat::expect_equal(vers2, vers6)


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

    #warns <- capture_warnings(
      out3 <- pkgSnapshot2()
    #)
    #if (isTRUE(getOption("Require.usePak"))) {
    #  browser()
    #  okWarn <- grepl(.txtPakCurrentlyPakNoSnapshots, warns)
    #  expect_true(okWarn)
    #}

    testthat::expect_true(is(out3, "character"))
    setwd(prevDir)

    # Check for packageVersionFile = FALSE
    warns <- capture_warnings(
      mess11 <- capture.output(type = "message", {
        outInner <- Require(packageVersionFile = FALSE, verbose = 5, quiet = TRUE)
      })
    )

    if (isTRUE(getOption("Require.usePak"))) {
     okWarn <- grepl(.txtPakCurrentlyPakNoSnapshots, warns)
     expect_true(okWarn)
    }


    testthat::expect_true(any(grepl(NoPkgsSupplied, mess11)))
    testthat::expect_true(isFALSE(outInner))

    # Skip on CRAN
    dir3 <- Require:::rpackageFolder(tempdir3())
    dir3 <- Require::checkPath(dir3, create = TRUE)
    # dir.create(dir3, recursive = TRUE, showWarnings = FALSE)
    # try({

    # This next one is correct version, but it was installed from CRAN, so it fails
    #   the GH SHA test (i.e., one has the SHA the other does not); so
    #   installs from source
    Require:::linkOrCopyPackageFilesInner(c("Require", "sys", "data.table", "gitcreds"),
                                     Sys.getenv("R_LIBS_USER"),toLib = dir3)
    inst <- suppressMessages(
      Require::Require("achubaty/fpCompare",
                       install = "force", returnDetails = TRUE,
                       # quiet = TRUE,
                       require = FALSE, standAlone = TRUE, libPaths = dir3
      )
    )

    inst22 <- suppressMessages(
      Require::Require("achubaty/fpCompare",
                       install = "force", returnDetails = TRUE,
                       # quiet = TRUE,
                       require = FALSE, standAlone = TRUE, libPaths = dir3
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
    err <- capture_error(
      warns <- capture_warnings(
        inst <- Require::Require("achubaty/fpCompare (>=2.0.0)",
                                 quiet = TRUE, require = FALSE, standAlone = FALSE, libPaths = dir4
        )
      )
    )
    test <- testWarnsInUsePleaseChange(warns)

    testthat::expect_true({
      isFALSE(inst)
    })
    err <- capture_error(
      warns <- capture_warnings(
        mess <- utils::capture.output(
          {
            inst <- Require::Require("achubaty/fpCompare (>=2.0.0)",
                                     verbose = 5,
                                     quiet = TRUE, require = FALSE, standAlone = FALSE, libPaths = dir4
            )
          },
          type = "message"
        )
      )
    )
    test <- testWarnsInUsePleaseChange(warns)

    if (!getOption("Require.usePak")) {
      testthat::expect_true({
        length(mess) > 0
      })
      testthat::expect_true({
        sum(grepl("could not be installed", mess)) == 1
      })
    }
    unlink(dirname(dir3), recursive = TRUE)
    unlink(dirname(dir4), recursive = TRUE)
  }

  # Code coverage
  skip_on_cran()
  # if (isDev) { # i.e., GA, R CMD check etc.

  # Issue 87
  try(remove.packages("reproducible"), silent = TRUE) |> suppressMessages()
  Require::cacheClearPackages("reproducible", ask = FALSE) # just in case some previous one had the bug

  ap <- available.packagesCached(repos = getOption("repos"), purge = FALSE, type = "both")
  curVer <- unique(ap[Package %in% "reproducible"]$Version)

  Require::Install(paste0("reproducible (==", curVer, ")")) # installs current CRAN version, which is older than SHA below
  Require::Install("reproducible") |> suppressWarnings() # "package 'reproducible' was built under ..." ... load it

  warnsReq <- capture_warnings(Require::Install("Require"))
  (Require::Install(c("CeresBarros/reproducible@51ecfd2b1b9915da3bd012ce23f47d4b98a9f212 (HEAD)"))) |>
    capture_warnings() -> warns

  test <- testWarnsInUsePleaseChange(warns)
  expect_true(test)

  # on.exit({
  #   try(out <- detachAll(c("Require", "fpCompare", "sdfd", "reproducible", "digest"),
  #                        dontTry = dontDetach())
  #       , silent = TRUE) |>
  #     suppressWarnings() |> suppressMessages()
  #   # unloadNamespace("package:fpCompare")
  #   # try(detach("package:reproducible", unload = TRUE), silent = TRUE)
  # }, add = TRUE)
  vers <- packVer("reproducible", .libPaths()[1])
  # vers <- DESCRIPTIONFileVersionV(file.path(.libPaths()[1], "reproducible/DESCRIPTION"))
  testthat::expect_equal(vers, "2.0.2.9001") #
  # detach("package:reproducible", unload = TRUE);
  unloadNamespace("package:fpCompare")
  # now installs correct SHA which is 2.0.2.9001
  warnsHere <- capture_warnings(  # "package 'reproducible' was built under ...
    Require::Install(c("CeresBarros/reproducible@51ecfd2b1b9915da3bd012ce23f47d4b98a9f212 (HEAD)"))
  )
  vers <- packVer("reproducible", .libPaths()[1])
  # vers <- DESCRIPTIONFileVersionV(file.path(.libPaths()[1], "reproducible/DESCRIPTION"))
  testthat::expect_equal(vers, "2.0.2.9001") # was incorrectly 2.0.2 from CRAN prior to PR #87
  # End issue 87

  suggests <- getOption("Require.packagesLeaveAttached")

  if (!getOption("Require.usePak")) {

    out <- try(
      detachAll(c("Require", "fpCompare", "sdfd", "reproducible"),
                dontTry = unique(c(suggests, dontDetach()))),
      silent = TRUE) |>
      suppressWarnings()
  }
  # detach("package:reproducible", unload = TRUE)

  #### MuMIn is currently failing to build from source
  if (FALSE) {
    pkg <- c("r-forge/mumin/pkg", "Require")
    names(pkg) <- c("MuMIn", "")
    out <- Require(pkg, install = FALSE, require = FALSE)
    testthat::expect_true({
      isFALSE(all(out))
    })
  }

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
    !is.null(out[["DESCFile"]])
  })
  testthat::expect_true({
    file.exists(out[["DESCFile"]])
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
  suppressWarnings(try(remove.packages(c("magrittr", "crayon", "fpCompare", "lobstr")),
                       silent = TRUE)) |> suppressMessages()
  verToCompare <- "2.0.2"
  cacheClearPackages(c("magrittr", "crayon"), ask = FALSE)

  # The warning is about "package ‘Require’ is in use and will not be installed"

  pkgsHere <- c("magrittr", "crayon", "lobstr")
  pkgdeps <- pkgDep(pkgsHere)
  out2 <- Require::Install(pkgsHere) |>
    capture_warnings() -> warns
  test <- testWarnsInUsePleaseChange(warns)
  expect_true(test)

  # testthat::expect_true(packageVersion("SpaDES") >= verToCompare)
  try(remove.packages(pkgsHere)) |> suppressMessages()
  cacheClearPackages(pkgsHere, ask = FALSE)
  a <- list(pkg = "fpCompare")

  warns <- capture_warnings(
    out <- Require::Install(pkgsHere, returnDetails = TRUE)
  )
  vers <- packVer("magrittr", .libPaths()[1])
  # vers <- DESCRIPTIONFileVersionV(file.path(.libPaths()[1], "magrittr/DESCRIPTION"))
  testthat::expect_true(vers != verToCompare)

  warns <- capture_warnings(
    out <- Require::Install(pkgsHere, returnDetails = TRUE)
  )
  vers <- packVer("magrittr", .libPaths()[1])
  # vers <- DESCRIPTIONFileVersionV(file.path(.libPaths()[1], "magrittr/DESCRIPTION"))

  testthat::expect_true(vers > verToCompare)

  #   }
})
