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
  # Use pak's official API: cache_delete drops both the file and the index entry,
  # whereas plain unlink leaves index rows that downstream lookups still see.
  invisible(tryCatch(pak::cache_delete(package = pkg),
                     error = function(e) NULL))

  warns3 <- capture_warnings(
    Require::Install(pkg, libPaths = testlib, standAlone = TRUE)
  )
  expect_false(isInTestlib(),
               info = "offline install without cache must NOT put pkg in testlib")
  expect_true(any(grepl(.txtCouldNotBeInstalled, warns3, fixed = TRUE)),
              info = paste("expected 'could not be installed' warning; warns3 =",
                           paste(warns3, collapse = " | ")))
})

test_that("Require.offlineMode installs AND loads from pak cache via Require()", {
  # Companion to the install-only test above: exercises the full Require()
  # flow (install + library()) under offlineMode, confirming that after
  # pakOfflineInstall writes the package to disk, doLoads() successfully
  # loads it without any network. The install-only test uses Install()
  # which sets require = FALSE; this one uses Require() so the load branch
  # is on the critical path.
  skip_on_cran()
  skip_if_offline2()
  skip_if_not_installed("pak")

  withr::local_options(Require.usePak = TRUE)
  pkg <- "fpCompare"

  testlib <- file.path(tempdir(),
                       paste0("rqlib_offline_load_", as.integer(Sys.time())))
  dir.create(testlib, recursive = TRUE)
  on.exit(unlink(testlib, recursive = TRUE), add = TRUE)

  isInTestlib <- function() {
    pkg %in% rownames(installed.packages(lib.loc = testlib, noCache = TRUE))
  }
  unloadIfLoaded <- function() {
    if (paste0("package:", pkg) %in% search()) {
      suppressWarnings(detach(paste0("package:", pkg), unload = TRUE,
                              character.only = TRUE))
    }
    if (pkg %in% loadedNamespaces()) {
      suppressWarnings(unloadNamespace(pkg))
    }
  }
  on.exit(unloadIfLoaded(), add = TRUE)

  # Start from a clean slate so the load assertion below is meaningful:
  # a parent libPath may already have fpCompare loaded from prior tests.
  unloadIfLoaded()

  # ---- Seed pak's download cache online + put pkg in testlib ----
  withr::local_options(Require.offlineMode = FALSE)
  Require::Install(pkg, libPaths = testlib, standAlone = TRUE)
  expect_true(isInTestlib(),
              info = "online seed must put pkg in testlib")

  # Wipe testlib so the offline Require() call has to *actually* install
  # (not be a no-op satisfied by an existing testlib copy). Unload too so
  # the load assertion is genuine.
  suppressMessages(remove.packages(pkg, lib = testlib))
  unloadIfLoaded()
  expect_false(isInTestlib(),
               info = "after remove.packages, pkg must be gone from testlib")
  expect_false(pkg %in% loadedNamespaces(),
               info = "after unload, pkg namespace must be unregistered")

  # ---- Offline Require(): install from pak cache + load ----
  withr::local_options(Require.offlineMode = TRUE)
  res <- Require::Require(pkg, libPaths = testlib, standAlone = TRUE)

  expect_true(all(res),
              info = paste("Require() under offlineMode returned a FALSE; res =",
                           paste(res, collapse = ", ")))
  expect_true(isInTestlib(),
              info = "Require() under offlineMode must install pkg from pak cache to testlib")
  expect_true(pkg %in% loadedNamespaces(),
              info = "Require() under offlineMode must load pkg via doLoads()")
})
