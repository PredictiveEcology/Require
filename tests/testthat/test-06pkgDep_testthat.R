test_that("test 6", {

  skip_on_cran()

  setupInitial <- setupTest()
  tmpdir <- tempdir2(.rndstr())
  .libPaths(tmpdir)

  isDev <- getOption("Require.isDev")
  # isDevAndInteractive <- getOption("Require.isDevAndInteractive")

  a <- pkgDep("Require", recursive = TRUE)
  testthat::expect_true({
    length(a) == 1
  })
  testthat::expect_true({
    !isTRUE(all.equal(lapply(a, trimVersionNumber), a))
  })
  a1 <- pkgDep("Require", keepVersionNumber = FALSE, recursive = TRUE) # just names
  testthat::expect_true({
    isTRUE(all.equal(lapply(a1, trimVersionNumber), a1))
  })

  pkg <- "PredictiveEcology/reproducible"
  a2 <- pkgDep(pkg, purge = TRUE) # GitHub
  testthat::expect_true({
    length(a2) == 1
  })
  testthat::expect_true({
    all(names(a2) == pkg)
  })

  b <- pkgDep(pkg, recursive = TRUE) # GitHub
  testthat::expect_true({
    length(b) == 1
  })
  testthat::expect_true({
    all(names(b) == pkg)
  })
  testthat::expect_true({
    length(b[[1]]) > length(a1[[1]])
  })

  # bAlt <- pkgDepAlt(pkg, recursive = TRUE, purge = TRUE) # GitHub
  # testthat::expect_true({length(setdiff(extractPkgName(b[[1]]), extractPkgName(bAlt[[1]]))) == 0})

  pkg2 <- c(pkg, "Require")
  d <- pkgDep(pkg2) # GitHub package and CRAN package
  testthat::expect_true({
    length(d) == 2
  })
  # Dependencies changed... remotes removed
  # remotes was in, now it isn't; depending on which version of R, result shows up different;
  #   ignore `remotes` for now
  testthat::expect_true({
    isTRUE(all.equal(
      setdiff(a$Require, "remotes"),
      setdiff(d$Require, "remotes")
    ))
  })

  # dAlt <- pkgDepAlt(pkg2, recursive = TRUE)
  # testthat::expect_true({length(setdiff(extractPkgName(d[[1]]), extractPkgName(dAlt[[1]]))) == 0})
  # testthat::expect_true({length(setdiff(extractPkgName(d[[2]]), extractPkgName(dAlt[[2]]))) == 0})
  # testthat::expect_true({length(d) == length(dAlt)})
  # testthat::expect_true({names(d) == names(dAlt)})

  pkg3 <- c(pkg2, "plyr")
  e <- pkgDep(pkg3) # GitHub, local, and CRAN packages
  testthat::expect_true({
    length(e) == 3
  })
  testthat::expect_true({
    isTRUE(all.equal(e[[pkg]], d[[pkg]]))
  })
  testthat::expect_true({
    isTRUE(all.equal(d$Require, e$Require))
  })

  # eAlt <- pkgDepAlt(pkg3, recursive = TRUE)
  # testthat::expect_true({length(setdiff(extractPkgName(e[[1]]), extractPkgName(eAlt[[1]]))) == 0})
  # testthat::expect_true({length(setdiff(extractPkgName(e[[2]]), extractPkgName(eAlt[[2]]))) == 0})
  # testthat::expect_true({length(setdiff(extractPkgName(e[[3]]), extractPkgName(eAlt[[3]]))) == 0})
  # testthat::expect_true({length(e) == length(eAlt)})
  # testthat::expect_true({names(e) == names(eAlt)})

  a <- pkgDep("Require", which = "all", recursive = FALSE)
  b <- pkgDep("Require", which = "most", recursive = FALSE)
  d <- pkgDep("Require", which = TRUE, recursive = FALSE)
  e <- pkgDep("Require", recursive = FALSE)
  testthat::expect_true({
    isTRUE(all.equal(a, b))
  })
  testthat::expect_true({
    isTRUE(all.equal(a, d))
  })
  testthat::expect_true({
    !isTRUE(all.equal(a, e))
  })
  # aAlt <- pkgDepAlt("Require", which = "all", recursive = FALSE, purge = TRUE)
  # bAlt <- pkgDepAlt("Require", which = "most", recursive = FALSE)
  # dAlt <- pkgDepAlt("Require", which = TRUE, recursive = FALSE)
  # eAlt <- pkgDepAlt("Require", recursive = FALSE)
  # testthat::expect_true({isTRUE(all.equal(a, aAlt))})
  # testthat::expect_true({isTRUE(all.equal(b, bAlt))})
  # testthat::expect_true({isTRUE(all.equal(d, dAlt))})
  # testthat::expect_true({isTRUE(all.equal(e, eAlt))})

  ### pkgDepTopoSort

  # MUST HAVE the "knownRevDeps" installed first
  skip_on_ci()
  knownRevDeps <- list(
    Require = c(
      # "reproducible",
      "SpaDES",
      "SpaDES.addins", "SpaDES.core",
      "SpaDES.experiment", "SpaDES.project"
    )
  )
  # withr::local_options(Require.verbose = 2)
  # needs Require or else it will try from predictiveecology.r-universe.dev ... but version is too low
  # warnsReq <- capture_warnings(Require::Install("Require"))
  warns22 <- capture_warnings(
    Install("PredictiveEcology/Require@simplify4 (>=0.3.1.9021)", install = "force")
  )
  # withr::local_options(warn = 2)
  # withr::local_options(Require.verbose = 2)

  (outtt <- Require::Install(c(knownRevDeps$Require),#, "PredictiveEcology/Require@simplify2 (>=0.3.1.9021)"),
                   repos = c("https://predictiveecology.r-universe.dev", getOption("repos")))) |>
    capture_warnings() -> warns

  # the repos with predictiveecology.r-universe.dev doesn't seem to have the PACKAGES
  warns <- grep("cannot open URL .+PACKAGES.rds'", warns, invert = TRUE, value = TRUE) #
  # something wrong with SpaDES.addins ... it fails to install at first, but retries and succeeds.
  #  this isn't a Require problem
  warns <- grep("SpaDES.addins|cannot open", warns, invert = TRUE, value = TRUE) #
  test <- testWarnsInUsePleaseChange(warns)
  if  (identical(Sys.info()[["user"]], "emcintir") && interactive()) if (isFALSE(test)) browser()
  if (isFALSE(test)) {
    print(warns)
    print(test)
  }
  expect_true(test)

  out <- pkgDepTopoSort(c("data.table", "Require"), reverse = TRUE, recursive = TRUE)
  knownRevDeps <- append(
    knownRevDeps,
    list(data.table = c(knownRevDeps$Require, "Require"))
  )
  installedPkgs <- dir(.libPaths()[1])
  knownRevDeps <- lapply(knownRevDeps, function(krd) intersect(krd, installedPkgs))

  test <- unlist(lapply(names(out), function(p) {
    knownRevDeps[[p]][!knownRevDeps[[p]] %in% out[[p]]]
  }))

  # if (isDev) {
  testthat::expect_true({
    length(test) == 0
  })
  # }

  repr <- pkgDep2("reproducible", recursive = TRUE)
  reprWRSQLIte <- unique(extractPkgName(c(names(repr), unname(unlist(repr)))))
  reprSimple <- pkgDepIfDepRemoved("reproducible", "RSQLite")
  repr[["RSQLite"]] <- NULL
  reprWORSQLIte <- unique(extractPkgName(c(names(repr), unname(unlist(repr)))))
  testthat::expect_true(identical(sort(reprSimple$Recursive$Remaining), sort(reprWORSQLIte)))

})
