# Regression guard for the CRAN-cancelling privilege-escalation issue.
#
# Background: pak's pkgdepends has an automatic system-requirements
# subsystem. On Linux it probes for sudo (`sudo ... id`) and will
# `apt-get install` missing system libraries. Require adopting pak as
# its default backbone without disabling this caused pak to attempt
# sudo on CRAN's check machine (user not in sudoers) -> submission
# cancelled (Uwe Ligges, 2026-05-15).
#
# Fix (R/zzz.R .onLoad + R/pak.R pakCall): Require forces pak's sysreqs
# subsystem OFF by default, unless the user has *explicitly* opted in
# (PKG_SYSREQS env truthy, or options(pkg.sysreqs = TRUE)) before load.
# An informed opt-in is honoured everywhere -- the user's machine, the
# user's sudo, the user's choice. CRAN stays safe because CRAN's check
# environment never sets the opt-in.
#
# Two layers below:
#   (A) Cheap, deterministic, ALWAYS-ON unit tests of the contract
#       (no network, no pak subprocess) -- these run on CRAN too and
#       guard the fix everywhere.
#   (B) A faithful sudo-trap integration test (skip_on_cran, Linux
#       only) that reproduces the exact failure mechanism with a
#       PATH-shadowing fake `sudo`, including an in-test negative
#       control so the protective assertion can never pass vacuously.

# ---------------------------------------------------------------------------
# (A) Contract unit tests -- always on
# ---------------------------------------------------------------------------

test_that(".sysreqsTruthy interprets env/option values correctly", {
  truthy <- Require:::.sysreqsTruthy
  for (v in c("true", "TRUE", "True", "yes", "on", "1", " true ")) {
    testthat::expect_true(truthy(v), info = v)
  }
  for (v in c("false", "FALSE", "no", "off", "0", "", "sudo", NA_character_)) {
    testthat::expect_false(isTRUE(truthy(v)), info = paste(v))
  }
  testthat::expect_true(truthy(TRUE))
  testthat::expect_false(isTRUE(truthy(FALSE)))
})

test_that(".sysreqsUserOptedIn is FALSE by default, TRUE only on explicit opt-in", {
  optedIn <- Require:::.sysreqsUserOptedIn

  ## Nothing set -> opt-OUT (the safe / CRAN default).
  withr::with_envvar(c(PKG_SYSREQS = NA, PKG_SYSREQS_SUDO = NA), {
    withr::with_options(list(pkg.sysreqs = NULL, pkg.sysreqs_sudo = NULL), {
      testthat::expect_false(optedIn())
    })
  })

  ## Explicit truthy env var -> opt-IN.
  withr::with_envvar(c(PKG_SYSREQS = "true"), {
    withr::with_options(list(pkg.sysreqs = NULL), {
      testthat::expect_true(optedIn())
    })
  })

  ## Explicit option TRUE -> opt-IN.
  withr::with_envvar(c(PKG_SYSREQS = NA), {
    withr::with_options(list(pkg.sysreqs = TRUE), {
      testthat::expect_true(optedIn())
    })
  })

  ## Explicit FALSE env var is NOT an opt-in (it is opt-out, same as default).
  withr::with_envvar(c(PKG_SYSREQS = "false"), {
    withr::with_options(list(pkg.sysreqs = NULL), {
      testthat::expect_false(optedIn())
    })
  })
})

test_that("pakCall forces sysreqs OFF by default but never clobbers an explicit opt-in", {
  pe <- Require:::pkgEnv()
  orig <- get0(".sysreqsUserOptIn", envir = pe, inherits = FALSE)
  on.exit({
    if (is.null(orig)) {
      if (exists(".sysreqsUserOptIn", envir = pe, inherits = FALSE))
        rm(".sysreqsUserOptIn", envir = pe)
    } else {
      assign(".sysreqsUserOptIn", orig, envir = pe)
    }
  }, add = TRUE)

  ## Default (flag FALSE): pakCall must force PKG_SYSREQS="false".
  assign(".sysreqsUserOptIn", FALSE, envir = pe)
  withr::with_envvar(c(PKG_SYSREQS = "true", PKG_SYSREQS_SUDO = "true"), {
    Require:::pakCall(1 + 1, verbose = 0)
    testthat::expect_identical(Sys.getenv("PKG_SYSREQS"), "false")
    testthat::expect_identical(Sys.getenv("PKG_SYSREQS_SUDO"), "false")
  })

  ## Opt-in (flag TRUE): pakCall must NOT override the user's "true".
  assign(".sysreqsUserOptIn", TRUE, envir = pe)
  withr::with_envvar(c(PKG_SYSREQS = "true", PKG_SYSREQS_SUDO = "true"), {
    Require:::pakCall(1 + 1, verbose = 0)
    testthat::expect_identical(Sys.getenv("PKG_SYSREQS"), "true")
    testthat::expect_identical(Sys.getenv("PKG_SYSREQS_SUDO"), "true")
  })
})

# ---------------------------------------------------------------------------
# (B) sudo-trap integration test -- the faithful reproduction
# ---------------------------------------------------------------------------

test_that("a pak-backed Require install never invokes sudo (with in-test negative control)", {
  skip_on_cran()
  testthat::skip_on_os("windows")          # pak's sudo/apt path is Linux/macOS
  skip_if_not_installed("pak")
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")

  pkgRoot <- normalizePath(".")
  tmp <- withr::local_tempdir()

  ## pak's sysreqs->sudo path only triggers for a REAL system package it
  ## knows how to install but finds missing. Pick a real apt -dev package
  ## that is genuinely absent on this host (dpkg query). It is never
  ## actually installed: under the negative control the fake sudo exits 1
  ## before any apt runs; under the protective case pak is suppressed
  ## entirely. Skip if we can't establish an absent package (non-Debian,
  ## or a fat image with all candidates present) -- the always-on unit
  ## tests above still guard the contract on every platform.
  if (nzchar(Sys.which("dpkg-query"))) {
    cand <- c("libpoppler-cpp-dev", "libtesseract-dev", "libv8-dev",
              "libmariadb-dev", "libavfilter-dev", "libsane-dev",
              "libfftw3-dev", "libgsl-dev", "librsvg2-dev")
    absent <- Filter(function(p) {
      st <- suppressWarnings(system2("dpkg-query", c("-W", "-f='${Status}'", p),
                                     stdout = TRUE, stderr = FALSE))
      !any(grepl("install ok installed", st))
    }, cand)
  } else {
    absent <- character(0)
  }
  testthat::skip_if(length(absent) == 0L,
    "no known-absent real system package available to drive pak's sysreqs path")
  absentLib <- absent[[1L]]

  ## A minimal fixture package whose only feature is the chosen
  ## absent-but-real system requirement -- this forces pak's sysreqs
  ## path ("a system package is missing -> probe sudo to install it").
  makeFixture <- function(nm) {
    d <- file.path(tmp, nm)
    dir.create(file.path(d, "R"), recursive = TRUE)
    writeLines(c(
      paste0("Package: ", nm),
      "Type: Package",
      "Title: Fixture With An Absent System Requirement",
      "Version: 0.0.1",
      "Authors@R: person('T','U',role=c('aut','cre'),email='t@u.org')",
      "Description: Triggers pak's sysreqs path for testing.",
      "License: MIT + file LICENSE",
      "Encoding: UTF-8",
      paste0("Config/pak/sysreqs: ", absentLib)
    ), file.path(d, "DESCRIPTION"))
    writeLines(c("YEAR: 2026", "COPYRIGHT HOLDER: T"),
               file.path(d, "LICENSE"))
    writeLines(".f <- function() invisible(NULL)",
               file.path(d, "R", "zzz.R"))
    d
  }
  ## Unique package names per run. pak's build cache is content-addressed
  ## and persists across processes (R_USER_CACHE_DIR); a fixed name+version
  ## would be served from cache on any re-run, skipping the build and
  ## silently never reaching the sysreqs/sudo path (vacuous pass). A random
  ## token guarantees a fresh build every time, so the negative control
  ## genuinely exercises pak's escalation path.
  tok <- paste0(sample(c(letters, 0:9), 10, replace = TRUE), collapse = "")
  fixtureNeg <- makeFixture(paste0("sysreqfix", tok, "a"))
  fixturePos <- makeFixture(paste0("sysreqfix", tok, "b"))

  ## --- PATH-shadowing fake sudo: records any invocation, exits 1
  ##     (mimicking CRAN's "user NOT in sudoers"). It NEVER calls the
  ##     real sudo and can install nothing.
  trapDir <- file.path(tmp, "sudotrap")
  dir.create(trapDir)
  trapLog <- file.path(trapDir, "calls.log")
  writeLines(c("#!/bin/sh",
               sprintf('echo "SUDO INVOKED: $*" >> %s', shQuote(trapLog)),
               "exit 1"),
             file.path(trapDir, "sudo"))
  Sys.chmod(file.path(trapDir, "sudo"), "0755")
  pathWithTrap <- paste(trapDir, Sys.getenv("PATH"), sep = .Platform$path.sep)

  ## --- trap self-check: prove the shim is live, so a broken shim can
  ##     never make the protective assertion pass vacuously.
  system2(file.path(trapDir, "sudo"), "self-check")
  testthat::expect_true(file.exists(trapLog) &&
                          length(readLines(trapLog)) >= 1L,
    info = "sudo trap shim must record invocations")
  unlink(trapLog)

  ## Fresh R subprocess so Require's .onLoad runs (or not) against the
  ## env we hand it. Inherit the full real environment (so R / compilers
  ## / pak work), strip any PKG_SYSREQS*, and shim PATH so the fake sudo
  ## shadows the real one. The ONLY difference between scenarios is
  ## whether Require is loaded -- which is exactly the contract under
  ## test: Require's presence must neutralise pak's sudo path.
  runPak <- function(loadRequire, fixture) {
    e <- Sys.getenv()
    e["PATH"] <- pathWithTrap
    ## callr/processx INHERIT the parent env and apply `env=` as
    ## overrides -- they do not replace it. The test parent has Require
    ## loaded, so its .onLoad already set PKG_SYSREQS=false in this
    ## process; merely dropping the name from `e` would leave the child
    ## still inheriting it. Force-unset in the child via NA (processx's
    ## "remove this variable" sentinel) so the negative control truly
    ## sees pak's default, and the protective run lets Require's own
    ## .onLoad set it in the child.
    e["PKG_SYSREQS"] <- NA_character_
    e["PKG_SYSREQS_SUDO"] <- NA_character_
    callr::r(
      function(pkgRoot, fixture, loadRequire) {
        if (loadRequire)
          suppressMessages(pkgload::load_all(pkgRoot, quiet = TRUE))
        lib <- tempfile("lp"); dir.create(lib)
        tryCatch(
          pak::pkg_install(paste0("local::", fixture),
                           lib = lib, dependencies = FALSE, ask = FALSE),
          error = function(e) NULL)
        invisible(NULL)
      },
      args = list(pkgRoot = pkgRoot, fixture = fixture,
                  loadRequire = loadRequire),
      env = e, show = FALSE
    )
  }

  ## --- NEGATIVE CONTROL: pak alone (Require NOT loaded) -> pak's
  ##     default sysreqs path runs -> it must probe sudo -> trap fires.
  ##     Proves the trap is wired to pak's real escalation path, so the
  ##     protective assertion below cannot pass vacuously.
  runPak(loadRequire = FALSE, fixture = fixtureNeg)
  testthat::expect_true(
    file.exists(trapLog) && length(readLines(trapLog)) >= 1L,
    info = paste("negative control: bare pak (no Require) must reach its",
                 "sudo probe -- if this fails the trap is not wired to the",
                 "real escalation path and the assertion below is vacuous"))
  unlink(trapLog)

  ## --- PROTECTIVE ASSERTION: identical call, but Require IS loaded.
  ##     Require's .onLoad must have forced pak's sysreqs OFF (env var
  ##     inherited by pak's subprocess) -> pak never probes sudo -> trap
  ##     stays empty. If this fires, the CRAN privilege-escalation
  ##     regression has returned.
  runPak(loadRequire = TRUE, fixture = fixturePos)
  testthat::expect_false(
    file.exists(trapLog) && length(readLines(trapLog)) >= 1L,
    info = paste("loading Require must neutralise pak's sudo path;",
                 "trap fired ->",
                 if (file.exists(trapLog)) paste(readLines(trapLog), collapse = " | ") else ""))
})
