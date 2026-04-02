test_that("parentChain shows in 'not on CRAN' message for deps of a local package", {
  # Integration test for the parentChain feature.
  #
  # We mock joinToAvailablePackages so that:
  #   - dummypkgwithpryr appears as a current-CRAN package (VersionOnRepos = "1.0",
  #     Imports = "pryr"), so pkgDepCRAN treats it as "on CRAN" and reads Imports = pryr
  #   - all other packages (e.g. pryr) keep VersionOnRepos = NA → "not on CRAN" path
  #
  # The mock makes NO network calls itself, so there is no risk of setting offlineMode.
  # We also reset Require.offlineMode and Require.useCache to avoid pollution from
  # earlier tests in the same session.
  #
  # Expected call chain:
  #   pkgDep("dummypkgwithpryr") -> getDeps -> getDepsNonGH -> pkgDepCRAN
  #        -> (mock returns VersionOnRepos=1.0, Imports=pryr for dummypkgwithpryr)
  #        -> assignPkgDTtoSaveNames discovers pryr
  #        -> recursive getPkgDeps("pryr", parentChain="dummypkgwithpryr")
  #        -> pkgDepCRAN("pryr", parentChain="dummypkgwithpryr")
  #        -> "pryr (required by: dummypkgwithpryr) not on CRAN; checking CRAN archives"

  skip_if_offline2()
  setupInitial <- setupTest()

  pkgname <- "dummypkgwithpryr"
  repos   <- "https://cloud.r-project.org"

  # Reset options that may have been left TRUE by earlier tests in this session.
  # offlineMode = TRUE would cause pkgDepCRAN to skip the entire processing block.
  # useCache = TRUE might return a cached pryr entry, bypassing pkgDepCRAN for pryr.
  old_offline <- getOption("Require.offlineMode")
  old_cache   <- getOption("Require.useCache")
  on.exit({
    options(Require.offlineMode = old_offline)
    options(Require.useCache    = old_cache)
  }, add = TRUE)
  options(Require.offlineMode = FALSE)
  options(Require.useCache    = FALSE)

  # Mock joinToAvailablePackages: inject VersionOnRepos + Imports for dummypkgwithpryr
  # so pkgDepCRAN treats it as a current-CRAN package whose Imports we already know.
  # For all other packages (e.g. pryr), keep VersionOnRepos = NA (not on CRAN).
  # The mock never calls the real function, so no network I/O and no offlineMode risk.
  testthat::local_mocked_bindings(
    joinToAvailablePackages = function(pkgDT, repos, type, which, verbose) {
      # Ensure VersionOnRepos and Repository columns exist
      if (is.null(pkgDT[["VersionOnRepos"]]))
        data.table::set(pkgDT, NULL, "VersionOnRepos", NA_character_)
      if (is.null(pkgDT[["Repository"]]))
        data.table::set(pkgDT, NULL, "Repository", NA_character_)
      # Ensure all `which` dep columns exist (so assignPkgDTtoSaveNames can read them)
      for (col in which) {
        if (is.null(pkgDT[[col]]))
          data.table::set(pkgDT, NULL, col, NA_character_)
      }
      # Inject fake CRAN presence + Imports for the dummy package only
      isDummy <- pkgDT$Package %in% pkgname
      if (any(isDummy)) {
        data.table::set(pkgDT, which(isDummy), "VersionOnRepos", "1.0")
        data.table::set(pkgDT, which(isDummy), "Repository",
                        "https://cloud.r-project.org")
        data.table::set(pkgDT, which(isDummy), "Imports", "pryr")
      }
      pkgDT
    },
    .package = "Require"
  )

  msgs <- character(0)
  withCallingHandlers(
    tryCatch(
      pkgDep(pkgname,
             repos     = repos,
             verbose   = 1,
             recursive = TRUE,
             purge     = TRUE),
      error = function(e) NULL
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  # Word-boundary grep so "dummypkgwithpryr" (which contains "pryr") is not matched
  not_on_cran_msgs <- msgs[grepl("not on CRAN", msgs, fixed = TRUE)]
  pryr_not_on_cran <- not_on_cran_msgs[grepl("\\bpryr\\b", not_on_cran_msgs)]

  testthat::expect_true(
    length(pryr_not_on_cran) > 0,
    info = paste("Expected a 'pryr ... not on CRAN' message. Messages captured:\n",
                 paste(msgs, collapse = "\n"))
  )
  testthat::expect_true(
    any(grepl(paste0("required by: ", pkgname), pryr_not_on_cran, fixed = TRUE)),
    info = paste0("Expected '(required by: ", pkgname, ")' in the pryr 'not on CRAN' ",
                  "message. Got:\n", paste(pryr_not_on_cran, collapse = "\n"))
  )
})
