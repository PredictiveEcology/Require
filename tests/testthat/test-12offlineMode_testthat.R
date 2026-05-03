test_that("Require.offlineMode installs from pak cache, fails cleanly when cache empty", {
  # Verifies that with options(Require.usePak = TRUE) + Require.offlineMode = TRUE,
  # Require can install a previously-cached package without ANY network access,
  # and emits a clean "could not be installed" warning when the cache is empty.
  #
  # Uses an isolated standAlone libPath so installed.packages() cleanly reflects
  # whether the install actually wrote files (vs. being satisfied by a parent
  # libPath copy from the test harness's Suggests prelude).
  skip_on_cran()
  skip_if_offline2()
  skip_if_not_installed("pak")

  # Need usePak = TRUE for this test — the offline path is pak-specific.
  withr::local_options(Require.usePak = TRUE)

  pkg <- "fpCompare"

  # Use a fresh standAlone lib so installed.packages(lib.loc = testlib) is the
  # ground-truth for whether Require's install put fpCompare on disk here.
  testlib <- file.path(tempdir(),
                       paste0("rqlib_offline_", as.integer(Sys.time())))
  dir.create(testlib, recursive = TRUE)
  on.exit(unlink(testlib, recursive = TRUE), add = TRUE)

  isInTestlib <- function() pkg %in% rownames(installed.packages(lib.loc = testlib, noCache = TRUE))

  # ---- 1. Online install seeds pak's download cache + writes to testlib ----
  withr::local_options(Require.offlineMode = FALSE)
  warns1 <- capture_warnings(
    Require::Install(pkg, libPaths = testlib, standAlone = TRUE)
  )
  expect_true(isInTestlib(),
              info = paste("warns1 =", paste(warns1, collapse = " | ")))
  inPakCacheBefore <- sum(pak::cache_list()$package %in% pkg, na.rm = TRUE) > 0L
  expect_true(inPakCacheBefore,
              info = "online install must populate pak's download cache")

  # ---- 2. Wipe testlib only (keep pak cache) + offline → install succeeds ----
  suppressMessages(remove.packages(pkg, lib = testlib))
  expect_false(isInTestlib(),
               info = "after remove.packages, pkg must be gone from testlib")

  withr::local_options(Require.offlineMode = TRUE)
  warns2 <- capture_warnings(
    Require::Install(pkg, libPaths = testlib, standAlone = TRUE)
  )
  expect_true(isInTestlib(),
              info = paste("offline install with cache must succeed; warns2 =",
                           paste(warns2, collapse = " | ")))
  expect_length(warns2, 0L)

  # ---- 3. Wipe testlib AND pak cache + offline → install fails cleanly ----
  suppressMessages(remove.packages(pkg, lib = testlib))
  cl <- pak::cache_list()
  pakCachedPaths <- cl$fullpath[!is.na(cl$package) & cl$package == pkg]
  unlink(pakCachedPaths)
  # cache_list() can be index-cached; a refresh is not strictly needed because
  # pakCachedTarball() guards the entries with file.exists().

  warns3 <- capture_warnings(
    Require::Install(pkg, libPaths = testlib, standAlone = TRUE)
  )
  expect_false(isInTestlib(),
               info = "offline install without cache must NOT put pkg in testlib")
  expect_true(any(grepl(.txtCouldNotBeInstalled, warns3, fixed = TRUE)),
              info = paste("expected 'could not be installed' warning; warns3 =",
                           paste(warns3, collapse = " | ")))
})
