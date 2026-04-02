test_that("parentChain shows in 'not on CRAN' message for deps of a local package", {
  # Integration test for the parentChain feature.
  #
  # We mock joinToAvailablePackages so it returns dummypkgwithpryr as a current-CRAN
  # package (VersionOnRepos = "1.0", Imports = "pryr").  This avoids any file:// or
  # network-reliability issues while still exercising the full recursive code path:
  #   getPkgDeps -> getDeps -> getDepsNonGH -> pkgDepCRAN -> (recurse for pryr)
  #        -> pkgDepCRAN("pryr", parentChain="dummypkgwithpryr")
  #        -> "pryr (required by: dummypkgwithpryr) not on CRAN; checking CRAN archives"
  #
  # pryr is a real archived-CRAN package, so the final "not on CRAN" check is live.

  skip_if_offline2()
  setupInitial <- setupTest()

  pkgname <- "dummypkgwithpryr"
  repos   <- "https://cloud.r-project.org"

  # Mock joinToAvailablePackages: for dummypkgwithpryr, inject VersionOnRepos + Imports
  # so pkgDepCRAN treats it as a current-CRAN package whose DESCRIPTION we already have.
  # For all other packages (e.g. pryr), call the real function.
  real_join <- Require:::joinToAvailablePackages
  testthat::local_mocked_bindings(
    joinToAvailablePackages = function(pkgDT, repos, type, which, verbose) {
      out <- real_join(pkgDT, repos, type, which, verbose)
      isDummy <- out$Package %in% pkgname
      if (any(isDummy)) {
        data.table::set(out, which(isDummy), "VersionOnRepos", "1.0")
        data.table::set(out, which(isDummy), "Repository",
                        "https://cloud.r-project.org")
        # Inject Imports so assignPkgDTtoSaveNames discovers pryr as a dep
        for (col in which) {
          if (is.null(out[[col]]))
            data.table::set(out, NULL, col, NA_character_)
        }
        data.table::set(out, which(isDummy), "Imports", "pryr")
      }
      out
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
