test_that("parentChain shows in 'not on CRAN' message for deps of a local package", {
  # Integration test for the parentChain feature (Issue: show why a package is needed).
  #
  # Strategy:
  #   - Build a minimal dummypkg_1.0.tar.gz with `Imports: pryr` in its DESCRIPTION.
  #   - Place the tarball in the Require package cache dir for the CRAN repos.
  #     identifyLocalFiles() scans that dir, so Require will find it there and read
  #     its DESCRIPTION without needing the package to be on CRAN or installed.
  #   - Call pkgDep("dummypkg"). The flow:
  #       1. pkgDepCRAN sees dummypkg not on CRAN → Archive path
  #       2. getArchiveDESCRIPTION → identifyLocalFiles finds dummypkg_1.0.tar.gz
  #       3. Extracts DESCRIPTION → reads Imports: pryr
  #       4. Recurses: pkgDepCRAN("pryr", parentChain = "dummypkg")
  #       5. pryr is archived → message includes "(required by: dummypkg)"
  #
  # pryr was removed from CRAN and lives only in the CRAN archive.

  skip_if_offline2()
  setupInitial <- setupTest()

  repos <- "https://cloud.r-project.org"
  td <- Require:::tempdir2("test_parentChain_integration")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  # --- 1. Build dummypkg_1.0.tar.gz ---
  pkgname <- "dummypkgwithpryr"
  ver <- "1.0"
  tarname <- paste0(pkgname, "_", ver, ".tar.gz")

  srcDir <- file.path(td, "src")
  pkgDir <- file.path(srcDir, pkgname)
  dir.create(pkgDir, recursive = TRUE)
  writeLines(c(
    paste0("Package: ", pkgname),
    paste0("Version: ", ver),
    "Title: Dummy package for testing parentChain messaging",
    "Description: Imports pryr so that pkgDep will hit the archived-CRAN path for pryr.",
    "Imports: pryr",
    "License: GPL-3"
  ), file.path(pkgDir, "DESCRIPTION"))

  tarfile <- file.path(td, tarname)
  withr::with_dir(srcDir,
    utils::tar(tarfile, files = pkgname, compression = "gzip", tar = "internal"))

  # --- 2. Place tarball in the Require cache for the repos ---
  cacheDir <- Require:::cachePkgDirForRepo(repos, create = TRUE)
  file.copy(tarfile, file.path(cacheDir, tarname))

  # --- 3. Run pkgDep and capture messages ---
  msgs <- character(0)
  withCallingHandlers(
    tryCatch(
      pkgDep(pkgname,
             repos       = repos,
             verbose     = 1,
             recursive   = TRUE),    # recursive=TRUE so pryr is processed and its "not on CRAN" msg is emitted
      error = function(e) NULL       # swallow downstream errors (e.g. network failures for pryr archive)
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  # --- 4. Assert the dependency chain appears in the message ---
  # Use word-boundary pattern so "dummypkgwithpryr" (which contains "pryr") is not matched.
  not_on_cran_msgs <- msgs[grepl("not on CRAN", msgs, fixed = TRUE)]
  pryr_not_on_cran <- not_on_cran_msgs[grepl("\\bpryr\\b", not_on_cran_msgs)]

  testthat::expect_true(
    length(pryr_not_on_cran) > 0,
    info = paste("Expected a 'pryr ... not on CRAN' message. Messages captured:\n",
                 paste(msgs, collapse = "\n"))
  )
  testthat::expect_true(
    any(grepl(paste0("required by: ", pkgname), pryr_not_on_cran, fixed = TRUE)),
    info = paste("Expected '(required by:", pkgname, ")' in the pryr 'not on CRAN' message. Got:\n",
                 paste(pryr_not_on_cran, collapse = "\n"))
  )
})
