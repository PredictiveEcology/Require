test_that("test 5", {

  skip_on_cran()
  skip_on_ci()
  setupInitial <- setupTest()
  # on.exit(endTest(setupInitial))

  # isDev <- getOption("Require.isDev")
  # isDevAndInteractive <- getOption("Require.isDevAndInteractive")

  # if (isDevAndInteractive) {
  tmpdir <- file.path(tempdir2(paste0("RequireTmp", sample(1e5, 1))))

  dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
  # repo <- chooseCRANmirror(ind = 1)

  opts <- options(repos = PEUniverseRepo()); on.exit(options(opts), add = TRUE)

  pkgDepTest1 <- Require::pkgDep("Require", includeSelf = FALSE, includeBase = FALSE)
  pkgDepTest2 <- Require::pkgDep2(c("Require"), # simplify = FALSE,
                                  # which = c("Depends", "Imports"),
                                  includeSelf = FALSE)
  orig <- Require::setLibPaths(tmpdir, standAlone = TRUE, updateRprofile = FALSE)

  testthat::expect_true({
    length(pkgDepTest1) == 1
  })
  testthat::expect_true({
    any(sort(pkgDepTest1[[1]]) %in% c("data.table (>= 1.10.4)"))
  })

  testthat::expect_true({
    length(pkgDepTest2[[1]]) == 2
  })
  testthat::expect_true({
    all(sort(names(pkgDepTest2$Require)) == sort(pkgDepTest1$Require))
  })

  tmpdirForPkgs <- gsub(".+ ([[:digit:]]\\.[[:digit:]])\\.[[:digit:]].+", "\\1", R.version.string)
  pkgsInstalled <- dir(tmpdirForPkgs, full.names = TRUE)
  RequireDeps <- c(
    "data.table", "utils", "callr", "cli", "covr",
    "crayon", "desc", "digest", "DT", "ellipsis", "BH", "units",
    "git2r", "glue", "httr", "jsonlite", "memoise", "pkgbuild", "pkgload",
    "rcmdcheck", "rlang", "roxygen2", "rstudioapi", "rversions",
    "sessioninfo", "stats", "testthat", "tools", "usethis", "utils", "withr", "Require"
  )
  pkgsToRm <- setdiff(
    sample(basename(pkgsInstalled), min(length(pkgsInstalled), 5)),
    RequireDeps
  )
  out <- unlink(pkgsToRm, recursive = TRUE)

  pkgs <- list(
    c("LearnBayes (>=2.0.4)", "tinytest (>= 1.0.3)", "glmm (>=1.4.3)",
      # "LearnBayes (<=4.0.4)", "tinytest (<= 1.0.3)", "glmm (<=1.4.3)",
      "SpaDES.tools (>=2.0.5)", "terra (>=1.7-71)",
      "reproducible (>=2.0.2)", "PredictiveEcology/reproducible@development (>=2.0.0)", # Until reproducible 2.0.2 is on CRAN
      "achubaty/amc@development", "PredictiveEcology/LandR@development (>=0.0.1)",
      "PredictiveEcology/LandR@development (>=0.0.2)", "ianmseddy/LandR.CS (<=0.0.1)"
    ),
    c(
      "PredictiveEcology/SpaDES.core@development (>=1.1.2)",
      "PredictiveEcology/map@development (>= 4.0.9)",
      "achubaty/amc@development (>=0.1.5)", "data.table (>=100.0)",
      "tinytest (>=1.3.1)", "PredictiveEcology/LandR@development (>= 1.0.2)",
      "versions (>=0.3)",
      "fastdigest (>=0.0.0.9)", "PredictiveEcology/map@development (>= 0.1.0.9)",
      "achubaty/amc@development (>=0.0.0.9)", "data.table (>=0.0.0.9)",
      "PredictiveEcology/LandR@development(>= 0.0.0.9)", "fastdigest (>=1000.0.0.8)",
      "fastdigest", "quickPlot", "testthat",
      "PredictiveEcology/map@development (>= 0.0.0.9)",
      "PredictiveEcology/map@development (>= 0.0.0.10)",
      "PredictiveEcology/map@development (>= 1111.0.9)",
      "PredictiveEcology/map@master (>= 0.0.0.9)",
      "PredictiveEcology/map@master (>= 0.0.0.10)"
    ),
    c(
      "PredictiveEcology/SpaDES.core@development (>=1.1.2)",
      "PredictiveEcology/map@development (>= 5.0.0.9)",
      "achubaty/amc@development (>=0.1.5)",
      "data.table (>=100.0)",
      paste0("tinytest (>=1.3.1)"),
      "PredictiveEcology/LandR@development (>= 1.0.2)"
    ),
    c(
      "fastdigest (>=0.0.0.9)",
      "PredictiveEcology/map@development (>= 0.0.0.9)",
      "achubaty/amc@development (>=0.0.0.9)",
      "data.table (>=0.0.0.9)",
      paste0("tinytest (>=1.3.1)"),
      "PredictiveEcology/LandR@development(>= 0.0.0.9)"
    ),
    # Multiple conflicting version numbers, and with NO version number
    c("fastdigest (>=0.0.0.8)", "fastdigest (>=0.0.0.9)", "fastdigest"), # "quickPlot", "testthat"),
    c("fastdigest (>=1000.0.0.8)", "fastdigest (>=0.0.0.9)", "fastdigest"),
    #          "quickPlot", "testthat"),
    c(
      "fastdigest (>=0.0.0.9)",
      "PredictiveEcology/map@development (>= 0.0.0.9)",
      "PredictiveEcology/map@development (>= 0.0.0.10)",
      "PredictiveEcology/map@development (>= 110.0.9)",
      "achubaty/amc@development (>=0.0.0.9)",
      "data.table (>=0.0.0.9)",
      paste0("tinytest (>=1.3.1)"),
      "PredictiveEcology/LandR@development(>= 0.0.0.9)"
    ),
    # "LearnBayes (>=1000.3.1)", # this is alone, so causes a fail/stop, which is OK. just not here.
    c("LearnBayes (>=1.0.1)", "fpCompare"),
    "LearnBayes (>=2.15.1)"#,
    #c("r-forge/mumin/pkg", MuMIn = "r-forge/mumin/pkg", "A3")
  )
  #   options("reproducible.Require.install" = TRUE)

  i <- 0
  pkg <- pkgs[[i + 1]] # redundant, but kept for interactive use
  for (pkg in pkgs) {
    # basically, it needs to be installed in the active library, which it isn't
    # warnsReq <- capture_warnings(Require::Install("Require"))
    (outFromRequire <- Require(pkg, standAlone = FALSE, require = FALSE)) |>
      capture_warnings() -> warns

    warns <- grep(.txtCouldNotBeInstalled, warns, invert = TRUE, value = TRUE)

    test <- testWarnsInUsePleaseChange(warns)
    if (!isTRUE(test)) browser()
    expect_true(test)

    # Rerun it to get output table, but capture messages for quiet; should be no installs
    (out <- Require(pkg, standAlone = FALSE, require = FALSE, returnDetails = TRUE)) |>
      capture_warnings() -> warns

    test <- testWarnsInUsePleaseChange(warns)
    # test <- testCouldNotBeInstalled(test)
    expect_true(test)

    testthat::expect_true(
      all.equal(out, outFromRequire, check.attributes = FALSE)
    )
    have <- attr(out, "Require")
    have <- have[!Package %in% c("Require", "testthat")] # these don't have Version number because they may be load_all'd
    pkgsToTest <- unique(Require::extractPkgName(pkg))
    names(pkgsToTest) <- pkgsToTest
    runTests(have, pkg)

    endTime <- Sys.time()
  }

})
