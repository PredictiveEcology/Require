## Require.cloneFrom: hard-link (or copy) packages from the cloneFrom libraries instead of installing
## them. Under pak (pakClonePackages()) a copy is used when it meets every version requirement; a
## GitHub ref needs the commit pak resolved, and a CRAN-style ref never takes a development build.

tinyPkg <- function(lib, name, version = "0.0.1", imports = NULL, extra = NULL) {
  src <- withr::local_tempdir()
  p <- file.path(src, name)
  dir.create(file.path(p, "R"), recursive = TRUE)
  writeLines(c(paste0("Package: ", name), paste0("Version: ", version), "Title: t", "Description: t.",
               "License: MIT", "Encoding: UTF-8", "NeedsCompilation: no",
               if (length(imports)) paste0("Imports: ", paste(imports, collapse = ", ")), extra),
             file.path(p, "DESCRIPTION"))
  writeLines("export(f)", file.path(p, "NAMESPACE"))
  writeLines("f <- function() 1", file.path(p, "R", "f.R"))
  system2(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", "--no-test-load", "-l", shQuote(lib), shQuote(p)),
          stdout = TRUE, stderr = TRUE)
  invisible(lib)
}

## A cloneFrom library holding tinyCloneA 0.0.1 and tinyCloneB 0.0.1 (which Imports tinyCloneA).
cloneFromLib <- function(envir = parent.frame()) {
  lib <- withr::local_tempdir(.local_envir = envir)
  tinyPkg(lib, "tinyCloneA")
  tinyPkg(lib, "tinyCloneB", imports = "tinyCloneA (>= 0.0.1)")
  lib
}

planDT <- function(pkgs = c("tinyCloneA", "tinyCloneB"), refs = pkgs,
                   versionSpec = NA_character_, inequality = NA_character_) {
  data.table::data.table(Package = pkgs, packageFullName = refs, needInstall = Require:::.txtInstall,
                         versionSpec = versionSpec, inequality = inequality)
}

## What pakDepsToPkgDT leaves in pakEnv() after a global solve.
setSolve <- function(resolved = c(tinyCloneA = "0.0.1", tinyCloneB = "0.0.1"),
                     edges = data.table::data.table(from = "tinyCloneB", package = "tinyCloneA",
                                                    op = ">=", version = "0.0.1"),
                     shas = character(), envir = parent.frame()) {
  env <- Require:::pakEnv()
  assign("pakResolvedVersionMap", resolved, envir = env)
  assign("pakDepEdges", edges, envir = env)
  assign("pakResolvedShaMap", shas, envir = env)
  withr::defer(suppressWarnings(rm(list = c("pakResolvedVersionMap", "pakDepEdges", "pakResolvedShaMap"),
                                   envir = env)), envir = envir)
}

test_that("pakClonePackages hard-links packages that meet every requirement", {
  from <- cloneFromLib()
  to <- withr::local_tempdir()
  withr::local_options(Require.cloneFrom = from)
  setSolve()
  out <- Require:::pakClonePackages(planDT(), libPaths = to, verbose = 0)
  expect_true(all(out$needInstall == Require:::.txtDontInstall))
  expect_true(all(out$installResult == "OK"))
  expect_true(all(file.exists(file.path(to, c("tinyCloneA", "tinyCloneB"), "DESCRIPTION"))))
  if (!isWindows()) {
    f <- file.path(c(from, to), "tinyCloneA", "DESCRIPTION")
    inodes <- system2("stat", c("-c", "%i", shQuote(f)), stdout = TRUE)
    expect_identical(inodes[1], inodes[2])
  }
  ## the edges are consumed: a later call without a fresh solve clones nothing
  expect_false(exists("pakDepEdges", envir = Require:::pakEnv(), inherits = FALSE))
})

test_that("pakClonePackages skips a clone that misses the user's requirement", {
  from <- cloneFromLib()
  to <- withr::local_tempdir()
  withr::local_options(Require.cloneFrom = from)
  setSolve(resolved = c(tinyCloneA = "0.0.2", tinyCloneB = "0.0.1"))
  plan <- planDT(versionSpec = c("0.0.2", NA), inequality = c(">=", NA))
  out <- Require:::pakClonePackages(plan, libPaths = to, verbose = 0)
  expect_identical(out[Package == "tinyCloneA"]$needInstall, Require:::.txtInstall)
  expect_identical(out[Package == "tinyCloneB"]$needInstall, Require:::.txtDontInstall)
  expect_false(dir.exists(file.path(to, "tinyCloneA")))
})

test_that("pakClonePackages skips a clone that is too old for a package in pak's plan", {
  from <- cloneFromLib()
  to <- withr::local_tempdir()
  withr::local_options(Require.cloneFrom = from)
  ## pak planned tinyCloneC, which is not in cloneFrom and needs tinyCloneA >= 0.0.2: the 0.0.1
  ## copy won't do. tinyCloneB's copy still works against the planned tinyCloneA 0.0.2.
  setSolve(resolved = c(tinyCloneA = "0.0.2", tinyCloneB = "0.0.1", tinyCloneC = "1.0"),
           edges = data.table::data.table(from = c("tinyCloneB", "tinyCloneC"),
                                          package = "tinyCloneA", op = ">=",
                                          version = c("0.0.1", "0.0.2")))
  out <- Require:::pakClonePackages(planDT(c("tinyCloneA", "tinyCloneB", "tinyCloneC")),
                                    libPaths = to, verbose = 0)
  expect_identical(out[Package == "tinyCloneA"]$needInstall, Require:::.txtInstall)
  expect_identical(out[Package == "tinyCloneB"]$needInstall, Require:::.txtDontInstall)
  expect_identical(out[Package == "tinyCloneC"]$needInstall, Require:::.txtInstall)
})

test_that("pakClonePackages skips a clone whose own dependency is not in the plan", {
  from <- cloneFromLib()
  to <- withr::local_tempdir()
  withr::local_options(Require.cloneFrom = from)
  setSolve(resolved = c(tinyCloneB = "0.0.1"), edges = data.table::data.table(
    from = character(), package = character(), op = character(), version = character()))
  out <- Require:::pakClonePackages(planDT("tinyCloneB"), libPaths = to, verbose = 0)
  expect_identical(out$needInstall, Require:::.txtInstall)
  expect_false(dir.exists(file.path(to, "tinyCloneB")))
})

test_that("pakClonePackages leaves GitHub refs, installed packages and stale solves alone", {
  from <- cloneFromLib()
  withr::local_options(Require.cloneFrom = from)

  ## GitHub ref, but the copy records no commit
  to <- withr::local_tempdir()
  setSolve(shas = c(tinyCloneA = "abc123"))
  out <- Require:::pakClonePackages(planDT("tinyCloneA", refs = "Owner/tinyCloneA@main"),
                                    libPaths = to, verbose = 0)
  expect_identical(out$needInstall, Require:::.txtInstall)

  ## already in the target library: never overwritten
  to <- withr::local_tempdir()
  tinyPkg(to, "tinyCloneA", version = "0.0.0")
  setSolve()
  out <- Require:::pakClonePackages(planDT("tinyCloneA"), libPaths = to, verbose = 0)
  expect_identical(out$needInstall, Require:::.txtInstall)
  expect_identical(read.dcf(file.path(to, "tinyCloneA", "DESCRIPTION"), "Version")[[1]], "0.0.0")

  ## no fresh global solve
  to <- withr::local_tempdir()
  out <- Require:::pakClonePackages(planDT(), libPaths = to, verbose = 0)
  expect_true(all(out$needInstall == Require:::.txtInstall))
})

test_that("canClone reads the Built field as installed.packages() gives it", {
  ## Built is "R 4.6.1; ; <date>; unix"; comparing it as "4.6.1" rejected every package, so
  ## cloneFrom cloned nothing on either install path.
  lib <- withr::local_tempdir()
  tinyPkg(lib, "tinyCloneA")
  ip <- installed.packages(lib, fields = c("Built", "NeedsCompilation"), noCache = TRUE)
  expect_identical(rownames(Require:::canClone(ip)), "tinyCloneA")
})

test_that("a GitHub ref is cloned only from a copy of the commit pak resolved", {
  sha <- "0012d2ffb58646ffd2eb5d6d6381f0ba220a97e4"
  from <- withr::local_tempdir()
  tinyPkg(from, "tinyCloneA", extra = c("RemoteType: github", paste0("RemoteSha: ", sha)))
  withr::local_options(Require.cloneFrom = from)
  plan <- planDT("tinyCloneA", refs = "Owner/tinyCloneA@main")
  edges0 <- data.table::data.table(from = character(), package = character(), op = character(),
                                   version = character())

  to <- withr::local_tempdir()
  setSolve(resolved = c(tinyCloneA = "0.0.1"), edges = edges0, shas = c(tinyCloneA = "ffff"))
  expect_identical(Require:::pakClonePackages(plan, libPaths = to, verbose = 0)$needInstall,
                   Require:::.txtInstall)

  to <- withr::local_tempdir()
  setSolve(resolved = c(tinyCloneA = "0.0.1"), edges = edges0, shas = c(tinyCloneA = sha))
  expect_identical(Require:::pakClonePackages(plan, libPaths = to, verbose = 0)$needInstall,
                   Require:::.txtDontInstall)
})

test_that("a CRAN-style ref never clones a development build", {
  from <- withr::local_tempdir()
  tinyPkg(from, "tinyCloneA", extra = c("RemoteType: github", "RemoteSha: abc"))
  withr::local_options(Require.cloneFrom = from)
  to <- withr::local_tempdir()
  setSolve(resolved = c(tinyCloneA = "0.0.1"), edges = data.table::data.table(
    from = character(), package = character(), op = character(), version = character()))
  out <- Require:::pakClonePackages(planDT("tinyCloneA"), libPaths = to, verbose = 0)
  expect_identical(out$needInstall, Require:::.txtInstall)
})

test_that("of several copies, pak's resolved version wins, else the highest", {
  lib1 <- withr::local_tempdir(); lib2 <- withr::local_tempdir(); lib3 <- withr::local_tempdir()
  tinyPkg(lib1, "tinyCloneA", version = "0.0.1")
  tinyPkg(lib2, "tinyCloneA", version = "0.0.3")
  tinyPkg(lib3, "tinyCloneA", version = "0.0.2")
  withr::local_options(Require.cloneFrom = c(lib1, lib2, lib3))
  edges0 <- data.table::data.table(from = character(), package = character(), op = character(),
                                   version = character())
  ver <- function(to) read.dcf(file.path(to, "tinyCloneA", "DESCRIPTION"), "Version")[[1]]

  to <- withr::local_tempdir()
  setSolve(resolved = c(tinyCloneA = "0.0.2"), edges = edges0)
  Require:::pakClonePackages(planDT("tinyCloneA"), libPaths = to, verbose = 0)
  expect_identical(ver(to), "0.0.2")

  to <- withr::local_tempdir()
  setSolve(resolved = c(tinyCloneA = "0.0.9"), edges = edges0)
  Require:::pakClonePackages(planDT("tinyCloneA"), libPaths = to, verbose = 0)
  expect_identical(ver(to), "0.0.3")
})

test_that("a package mid-install (00LOCK) is not cloned", {
  from <- cloneFromLib()
  dir.create(file.path(from, "00LOCK-tinyCloneA"))
  expect_identical(Require:::clonablePackages(from, c("tinyCloneA", "tinyCloneB"))$Package, "tinyCloneB")
})

test_that("cloneFrom = TRUE means R_LIBS, R_LIBS_USER and R_LIBS_SITE, never the target", {
  a <- withr::local_tempdir(); b <- withr::local_tempdir(); target <- withr::local_tempdir()
  withr::local_envvar(R_LIBS = paste(a, target, sep = .Platform$path.sep), R_LIBS_USER = b,
                      R_LIBS_SITE = file.path(b, "doesNotExist"))
  norm <- function(x) normalizePath(x, winslash = "/")
  expect_identical(Require:::cloneSources(TRUE, toLib = target), norm(c(a, b)))
  expect_identical(Require:::cloneSources(FALSE, toLib = target), character())
  expect_identical(Require:::cloneSources(NULL, toLib = target), character())
  expect_identical(Require:::cloneSources(a, toLib = target), norm(a))
})

test_that("the legacy path clones the exact version, never a development build or a GitHub install", {
  from <- withr::local_tempdir()
  tinyPkg(from, "tinyCloneA")
  tinyPkg(from, "tinyCloneB")
  tinyPkg(from, "tinyCloneC", extra = c("RemoteType: github", "RemoteSha: abc"))
  tinyPkg(from, "tinyCloneD")
  av <- cbind(Package = c("tinyCloneA", "tinyCloneB", "tinyCloneC", "tinyCloneD"),
              Version = c("0.0.1", "0.0.2", "0.0.1", "0.0.1"))
  rownames(av) <- av[, "Package"]
  ipa <- list(pkgs = rownames(av), available = av)
  to <- withr::local_tempdir()
  out <- Require:::clonePackages(from, ipa, libPaths = to, skip = "tinyCloneD", verbose = 0)
  expect_identical(dir(to), "tinyCloneA")
  expect_identical(out$pkgs, c("tinyCloneB", "tinyCloneC", "tinyCloneD"))
})

test_that("a clone's version requirement on a base package is checked against R's own copy", {
  ## J4R Imports "utils (>= 3.4), methods (>= 3.4)"; base packages are never in pak's plan, so this
  ## requirement used to fail and the clone was dropped.
  from <- withr::local_tempdir()
  tinyPkg(from, "tinyCloneA", imports = "utils (>= 3.4)")
  withr::local_options(Require.cloneFrom = from)
  to <- withr::local_tempdir()
  setSolve(resolved = c(tinyCloneA = "0.0.1"), edges = data.table::data.table(
    from = character(), package = character(), op = character(), version = character()))
  out <- Require:::pakClonePackages(planDT("tinyCloneA"), libPaths = to, verbose = 0)
  expect_identical(out$needInstall, Require:::.txtDontInstall)
})
