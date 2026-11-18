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
                     shas = character(), envir = parent.frame()) {
  env <- Require:::pakEnv()
  assign("pakResolvedVersionMap", resolved, envir = env)
  assign("pakResolvedShaMap", shas, envir = env)
  withr::defer({
    suppressWarnings(rm(list = c("pakResolvedVersionMap", "pakResolvedShaMap"), envir = env))
    suppressWarnings(rm(list = "cloneCheckPending", envir = Require:::pkgEnv()))
  }, envir = envir)
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
  ## the solve is consumed: a later call without a fresh one clones nothing
  expect_false(exists("pakResolvedShaMap", envir = Require:::pakEnv(), inherits = FALSE))
})

test_that("pakClonePackages skips a clone that misses a requirement in the plan", {
  ## pkgDT carries the user's requirements and, from pak's solve, those of the packages that
  ## depend on each one (tinyCloneA (>= 0.0.2) when a planned package needs it).
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

test_that("pakClonePackages skips a clone whose own dependency is not in the plan", {
  from <- cloneFromLib()
  to <- withr::local_tempdir()
  withr::local_options(Require.cloneFrom = from)
  setSolve(resolved = c(tinyCloneB = "0.0.1"))
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

## A package with compiled C code: f() returns 42L through .Call.
tinyCompiledPkg <- function(lib, name) {
  src <- withr::local_tempdir()
  p <- file.path(src, name)
  dir.create(file.path(p, "R"), recursive = TRUE)
  dir.create(file.path(p, "src"))
  writeLines(c(paste0("Package: ", name), "Version: 0.0.1", "Title: t", "Description: t.",
               "License: MIT", "Encoding: UTF-8", "NeedsCompilation: yes"), file.path(p, "DESCRIPTION"))
  writeLines(c(paste0("useDynLib(", name, ", .registration = TRUE)"), "export(f)"),
             file.path(p, "NAMESPACE"))
  writeLines("f <- function() .Call(c_f)", file.path(p, "R", "f.R"))
  writeLines(c("#include <R.h>", "#include <Rinternals.h>", "#include <R_ext/Rdynload.h>",
               "SEXP c_f(void) { return ScalarInteger(42); }",
               "static const R_CallMethodDef calls[] = {{\"c_f\", (DL_FUNC) &c_f, 0}, {NULL, NULL, 0}};",
               paste0("void R_init_", name, "(DllInfo *dll) { R_registerRoutines(dll, NULL, calls, NULL, NULL); }")),
             file.path(p, "src", "f.c"))
  system2(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", "-l", shQuote(lib), shQuote(p)),
          stdout = TRUE, stderr = TRUE)
  invisible(lib)
}

test_that("a compiled package built here is cloned under pak and works from the new library", {
  skip_on_os("windows")
  from <- withr::local_tempdir()
  tinyCompiledPkg(from, "tinyCloneComp")
  skip_if_not(dir.exists(file.path(from, "tinyCloneComp")), "no C compiler")
  withr::local_options(Require.cloneFrom = from)
  setSolve(resolved = c(tinyCloneComp = "0.0.1"))
  to <- withr::local_tempdir()
  out <- Require:::pakClonePackages(planDT("tinyCloneComp"), libPaths = to, verbose = 0)
  expect_identical(out$needInstall, Require:::.txtDontInstall)
  ## the shared library came along and loads from the new library, in a fresh R (both paths
  ## normalized: on macOS the temp dir is under /var, a link to /private/var)
  res <- system2(file.path(R.home("bin"), "Rscript"),
                 c("-e", shQuote(paste0("cat(tinyCloneComp::f(), normalizePath(find.package('tinyCloneComp', lib.loc = '",
                                        to, "')))"))),
                 env = paste0("R_LIBS=", to), stdout = TRUE)
  expect_identical(res, paste(42L, normalizePath(file.path(to, "tinyCloneComp"))))
})

test_that("canClone takes a compiled copy only if built for this platform, and never on Windows", {
  ## compiled = R installed a libs/ folder; the platform comes from the installed DESCRIPTION,
  ## because before R 4.6 installed.packages() gives Built as just "4.5.3"
  lib <- withr::local_tempdir()
  fakeBuild <- function(name, platform) {
    tinyPkg(lib, name)
    d <- file.path(lib, name)
    dir.create(file.path(d, "libs"))
    desc <- read.dcf(file.path(d, "DESCRIPTION"))
    desc[, "Built"] <- sub("^(R [^;]*); *[^;]*;", paste0("\\1; ", platform, ";"), desc[, "Built"])
    write.dcf(desc, file.path(d, "DESCRIPTION"))
  }
  tinyPkg(lib, "pure")
  tinyPkg(lib, "saysYesButPure")
  f <- file.path(lib, "saysYesButPure", "DESCRIPTION")
  writeLines(sub("^NeedsCompilation: no$", "NeedsCompilation: yes", readLines(f)), f)
  fakeBuild("compHere", R.version$platform)
  fakeBuild("compElsewhere", "aarch64-apple-darwin20")
  ip <- installed.packages(lib, fields = c("Built", "NeedsCompilation"), noCache = TRUE)
  local_mocked_bindings(isWindows = function() FALSE, .package = "Require")
  expect_identical(sort(rownames(Require:::canClone(ip))), c("compHere", "pure", "saysYesButPure"))
  local_mocked_bindings(isWindows = function() TRUE, .package = "Require")
  expect_identical(sort(rownames(Require:::canClone(ip))), c("pure", "saysYesButPure"))
})

test_that("always-from-source packages are cloned under pak, but not by the legacy path", {
  from <- cloneFromLib()
  withr::local_options(Require.cloneFrom = from, Require.otherPkgs = "tinyCloneA")
  setSolve(resolved = c(tinyCloneA = "0.0.1"))
  to <- withr::local_tempdir()
  Require:::pakClonePackages(planDT("tinyCloneA"), libPaths = to, verbose = 0)
  expect_identical(dir(to), "tinyCloneA")

  av <- cbind(Package = "tinyCloneA", Version = "0.0.1"); rownames(av) <- "tinyCloneA"
  to2 <- withr::local_tempdir()
  Require:::clonePackages(from, list(pkgs = "tinyCloneA", available = av), libPaths = to2, verbose = 0)
  expect_identical(dir(to2), character())
})

test_that("a GitHub ref is cloned only from a copy of the commit pak resolved", {
  sha <- "0012d2ffb58646ffd2eb5d6d6381f0ba220a97e4"
  from <- withr::local_tempdir()
  tinyPkg(from, "tinyCloneA", extra = c("RemoteType: github", paste0("RemoteSha: ", sha)))
  withr::local_options(Require.cloneFrom = from)
  plan <- planDT("tinyCloneA", refs = "Owner/tinyCloneA@main")

  to <- withr::local_tempdir()
  setSolve(resolved = c(tinyCloneA = "0.0.1"), shas = c(tinyCloneA = "ffff"))
  expect_identical(Require:::pakClonePackages(plan, libPaths = to, verbose = 0)$needInstall,
                   Require:::.txtInstall)

  to <- withr::local_tempdir()
  setSolve(resolved = c(tinyCloneA = "0.0.1"), shas = c(tinyCloneA = sha))
  expect_identical(Require:::pakClonePackages(plan, libPaths = to, verbose = 0)$needInstall,
                   Require:::.txtDontInstall)
})

test_that("a CRAN-style ref never clones a development build", {
  from <- withr::local_tempdir()
  tinyPkg(from, "tinyCloneA", extra = c("RemoteType: github", "RemoteSha: abc"))
  withr::local_options(Require.cloneFrom = from)
  to <- withr::local_tempdir()
  setSolve(resolved = c(tinyCloneA = "0.0.1"))
  out <- Require:::pakClonePackages(planDT("tinyCloneA"), libPaths = to, verbose = 0)
  expect_identical(out$needInstall, Require:::.txtInstall)
})

test_that("of several copies, pak's resolved version wins, else the highest", {
  lib1 <- withr::local_tempdir(); lib2 <- withr::local_tempdir(); lib3 <- withr::local_tempdir()
  tinyPkg(lib1, "tinyCloneA", version = "0.0.1")
  tinyPkg(lib2, "tinyCloneA", version = "0.0.3")
  tinyPkg(lib3, "tinyCloneA", version = "0.0.2")
  withr::local_options(Require.cloneFrom = c(lib1, lib2, lib3))
  ver <- function(to) read.dcf(file.path(to, "tinyCloneA", "DESCRIPTION"), "Version")[[1]]

  to <- withr::local_tempdir()
  setSolve(resolved = c(tinyCloneA = "0.0.2"))
  Require:::pakClonePackages(planDT("tinyCloneA"), libPaths = to, verbose = 0)
  expect_identical(ver(to), "0.0.2")

  to <- withr::local_tempdir()
  setSolve(resolved = c(tinyCloneA = "0.0.9"))
  Require:::pakClonePackages(planDT("tinyCloneA"), libPaths = to, verbose = 0)
  expect_identical(ver(to), "0.0.3")
})

test_that("a package mid-install (00LOCK) is not cloned", {
  from <- cloneFromLib()
  dir.create(file.path(from, "00LOCK-tinyCloneA"))
  expect_identical(Require:::clonablePackages(from, c("tinyCloneA", "tinyCloneB"))$Package, "tinyCloneB")
})

test_that("a cloneFrom library holding none of the packages is skipped, not an error", {
  ## CI runners have an empty or unrelated library on the path; its zero rows made the 00LOCK
  ## filter fail with "logical subscript too long", so Require() itself failed under pak.
  from <- cloneFromLib()
  empty <- withr::local_tempdir()
  other <- withr::local_tempdir()
  tinyPkg(other, "tinyCloneOther")
  for (libs in list(c(empty, from), c(other, from), c(from, empty)))
    expect_identical(sort(Require:::clonablePackages(libs, c("tinyCloneA", "tinyCloneB"))$Package),
                     c("tinyCloneA", "tinyCloneB"))
  expect_identical(nrow(Require:::clonablePackages(c(empty, other), "tinyCloneA")), 0L)

  withr::local_options(Require.cloneFrom = c(empty, other, from))
  setSolve()
  to <- withr::local_tempdir()
  out <- Require:::pakClonePackages(planDT(), libPaths = to, verbose = 0)
  expect_true(all(out$needInstall == Require:::.txtDontInstall))
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
  setSolve(resolved = c(tinyCloneA = "0.0.1"))
  out <- Require:::pakClonePackages(planDT("tinyCloneA"), libPaths = to, verbose = 0)
  expect_identical(out$needInstall, Require:::.txtDontInstall)
})

test_that("dropping one clone drops the clones that need it, round by round", {
  ## tinyCloneA's copy misses the user's floor, so pak installs 0.0.2. tinyCloneB's copy needs
  ## tinyCloneA <= 0.0.1, so it goes next round; tinyCloneC's copy needs tinyCloneB <= 0.0.1, so it
  ## goes the round after. tinyCloneD depends on nothing and stays cloned.
  from <- withr::local_tempdir()
  tinyPkg(from, "tinyCloneA")
  tinyPkg(from, "tinyCloneB", imports = "tinyCloneA (<= 0.0.1)")
  tinyPkg(from, "tinyCloneC", imports = "tinyCloneB (<= 0.0.1)")
  tinyPkg(from, "tinyCloneD")
  withr::local_options(Require.cloneFrom = from)
  to <- withr::local_tempdir()
  setSolve(resolved = c(tinyCloneA = "0.0.2", tinyCloneB = "0.0.2", tinyCloneC = "0.0.1",
                        tinyCloneD = "0.0.1"))
  pkgs <- c("tinyCloneA", "tinyCloneB", "tinyCloneC", "tinyCloneD")
  plan <- planDT(pkgs, versionSpec = c("0.0.2", NA, NA, NA), inequality = c(">=", NA, NA, NA))
  out <- Require:::pakClonePackages(plan, libPaths = to, verbose = 0)
  expect_identical(out[match(pkgs, Package)]$needInstall,
                   c(rep(Require:::.txtInstall, 3), Require:::.txtDontInstall))
  expect_identical(dir(to), "tinyCloneD")
})

## Source tarballs of tiny packages in a local CRAN-like repository: list(name, version, imports).
tinyRepo <- function(pkgs, envir = parent.frame()) {
  repo <- withr::local_tempdir(.local_envir = envir)
  contrib <- file.path(repo, "src", "contrib")
  dir.create(contrib, recursive = TRUE)
  src <- withr::local_tempdir()
  for (p in pkgs) {
    d <- file.path(src, paste0(p$name, "_", p$version), p$name)
    dir.create(file.path(d, "R"), recursive = TRUE)
    writeLines(c(paste0("Package: ", p$name), paste0("Version: ", p$version), "Title: t",
                 "Description: t.", "License: MIT", "Encoding: UTF-8", "NeedsCompilation: no",
                 if (length(p$imports)) paste0("Imports: ", paste(p$imports, collapse = ", "))),
               file.path(d, "DESCRIPTION"))
    writeLines("export(f)", file.path(d, "NAMESPACE"))
    writeLines("f <- function() 1", file.path(d, "R", "f.R"))
    withr::with_dir(contrib, system2(file.path(R.home("bin"), "R"),
                                     c("CMD", "build", "--no-build-vignettes", shQuote(d)),
                                     stdout = TRUE, stderr = TRUE))
  }
  tools::write_PACKAGES(contrib, type = "source")
  paste0("file://", normalizePath(repo, winslash = "/"))
}

test_that("Install() under pak clones what it can and pak installs the rest around the clones", {
  skip_on_cran()
  skip_if_not_installed("pak")
  ## Three levels: tinyIntC Imports tinyIntB and tinyIntA (>= 1.1); tinyIntB Imports tinyIntA.
  ## tinyIntD is a sibling with no deps. The repository has newer A and D than the clone library.
  repo <- tinyRepo(list(
    list(name = "tinyIntA", version = "1.1"),
    list(name = "tinyIntB", version = "1.0", imports = "tinyIntA (>= 1.0)"),
    list(name = "tinyIntC", version = "1.0", imports = c("tinyIntB", "tinyIntA (>= 1.1)")),
    list(name = "tinyIntD", version = "2.0")))
  from <- withr::local_tempdir()
  tinyPkg(from, "tinyIntA", version = "1.0")     # too old for tinyIntC: must not be cloned
  tinyPkg(from, "tinyIntB", version = "1.0", imports = "tinyIntA (>= 1.0)")
  tinyPkg(from, "tinyIntD", version = "1.5")     # older than the repo's, but nothing needs newer
  to <- withr::local_tempdir()
  withr::local_options(Require.cloneFrom = from, Require.usePak = TRUE,
                       repos = c(tiny = repo, getOption("repos")))

  msgs <- capture_messages(
    out <- Install(c("tinyIntC", "tinyIntD"), libPaths = to, standAlone = TRUE, verbose = 1))
  ver <- function(p) read.dcf(file.path(to, p, "DESCRIPTION"), "Version")[[1]]
  cloneMsg <- grep("Cloning", msgs, value = TRUE)
  expect_length(cloneMsg, 1)
  expect_true(all(vapply(c("tinyIntB", "tinyIntD"), grepl, logical(1), x = cloneMsg)))
  expect_false(any(vapply(c("tinyIntA", "tinyIntC"), grepl, logical(1), x = cloneMsg)))
  expect_identical(ver("tinyIntA"), "1.1")     # installed by pak from the repository
  expect_identical(ver("tinyIntB"), "1.0")     # cloned
  expect_identical(ver("tinyIntC"), "1.0")     # installed by pak
  expect_identical(ver("tinyIntD"), "1.5")     # cloned, and pak did not upgrade it
})

## ---- load check of clones (cloneCheck.R) ----

## A package whose .onLoad fails when loaded from the library named by TINY_BROKEN_LIB: it loads
## from its source library but not as a clone there.
tinyBrokenPkg <- function(lib, name, version = "0.0.1") {
  src <- withr::local_tempdir()
  p <- file.path(src, name)
  dir.create(file.path(p, "R"), recursive = TRUE)
  writeLines(c(paste0("Package: ", name), paste0("Version: ", version), "Title: t", "Description: t.",
               "License: MIT", "Encoding: UTF-8", "NeedsCompilation: no"), file.path(p, "DESCRIPTION"))
  writeLines("export(f)", file.path(p, "NAMESPACE"))
  writeLines(c("f <- function() 1",
               ".onLoad <- function(libname, pkgname)",
               "  if (identical(normalizePath(libname), normalizePath(Sys.getenv('TINY_BROKEN_LIB', '/none'), mustWork = FALSE)))",
               "    stop('cannot load from here')"), file.path(p, "R", "f.R"))
  system2(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", "--no-test-load", "-l", shQuote(lib), shQuote(p)),
          stdout = TRUE, stderr = TRUE)
  invisible(lib)
}

checkFiles <- function(lib) Require:::cloneCheckFiles(lib)

test_that("a clone that loads passes the load check and leaves no file behind", {
  from <- cloneFromLib()
  to <- withr::local_tempdir()
  withr::local_options(Require.cloneFrom = from)
  setSolve()
  Require:::pakClonePackages(planDT(), libPaths = to, verbose = 0)
  f <- Require:::cloneCheckStart(to, wait = TRUE, verbose = 0)
  expect_setequal(Require:::cloneCheckRead(f)$Status, "ok")
  expect_identical(Require:::cloneCheckRepair(to, verbose = 0), character())
  expect_length(checkFiles(to), 0)
  expect_true(dir.exists(file.path(to, "tinyCloneA")))
})

test_that("a clone that fails to load is deleted, never cloned again, and its source is untouched", {
  from <- withr::local_tempdir()
  tinyBrokenPkg(from, "tinyBroken")
  to <- withr::local_tempdir()
  withr::local_envvar(TINY_BROKEN_LIB = to)
  withr::local_options(Require.cloneFrom = from)
  setSolve(resolved = c(tinyBroken = "0.0.1"))
  Require:::pakClonePackages(planDT("tinyBroken"), libPaths = to, verbose = 0)
  expect_true(dir.exists(file.path(to, "tinyBroken")))
  f <- Require:::cloneCheckStart(to, wait = TRUE, verbose = 0)
  st <- Require:::cloneCheckRead(f)
  expect_identical(st$Status, "fail")
  expect_match(st$Reason, "cannot load from here")
  expect_message(removed <- Require:::cloneCheckRepair(to, verbose = 0), "failed to load")
  expect_identical(removed, "tinyBroken")
  expect_false(dir.exists(file.path(to, "tinyBroken")))
  expect_true(file.exists(file.path(from, "tinyBroken", "R", "tinyBroken.rdb")))
  ## the failed version is not offered for cloning into that library again; elsewhere it still is
  expect_identical(nrow(Require:::clonablePackages(from, "tinyBroken", toLib = to)), 0L)
  expect_identical(nrow(Require:::clonablePackages(from, "tinyBroken", toLib = withr::local_tempdir())), 1L)
  expect_identical(nrow(Require:::clonablePackages(from, "tinyBroken")), 1L)
})

test_that("a check still running is left alone; one that died counts as failed", {
  from <- cloneFromLib()
  to <- withr::local_tempdir()
  withr::local_options(Require.cloneFrom = from)
  setSolve()
  Require:::pakClonePackages(planDT(), libPaths = to, verbose = 0)
  rec <- Require:::pkgEnv()$cloneCheckPending[[to]]
  f <- file.path(to, "._RequireCloneCheck_test")
  Require:::cloneCheckWrite(rec, f)                       # all "untested", as when the check starts
  expect_identical(Require:::cloneCheckRepair(to, verbose = 0), character())
  expect_true(file.exists(f))
  Sys.setFileTime(f, Sys.time() - 2 * Require:::cloneCheckStaleSecs)
  expect_message(removed <- Require:::cloneCheckRepair(to, verbose = 0), "stopped before")
  expect_setequal(removed, c("tinyCloneA", "tinyCloneB"))
})

test_that("a failed clone that was reinstalled since its check is not deleted", {
  from <- cloneFromLib()
  to <- withr::local_tempdir()
  withr::local_options(Require.cloneFrom = from)
  setSolve(resolved = c(tinyCloneA = "0.0.1"))
  Require:::pakClonePackages(planDT("tinyCloneA"), libPaths = to, verbose = 0)
  rec <- Require:::pkgEnv()$cloneCheckPending[[to]]
  rec$Status <- "fail"
  rec$Mtime <- "1"                                        # not the DESCRIPTION now in `to`
  Require:::cloneCheckWrite(rec, file.path(to, "._RequireCloneCheck_test"))
  expect_identical(Require:::cloneCheckRepair(to, verbose = 0), character())
  expect_true(dir.exists(file.path(to, "tinyCloneA")))
  expect_identical(Require:::cloneCheckExcluded(to), "tinyCloneA 0.0.1")
})

test_that("in the background the check returns at once and finishes on its own", {
  from <- cloneFromLib()
  to <- withr::local_tempdir()
  withr::local_options(Require.cloneFrom = from)
  setSolve()
  Require:::pakClonePackages(planDT(), libPaths = to, verbose = 0)
  f <- Require:::cloneCheckStart(to, wait = FALSE, verbose = 0)
  for (i in 1:120) {
    st <- Require:::cloneCheckRead(f)
    if (!is.null(st) && !any(st$Status == "untested")) break
    Sys.sleep(0.5)
  }
  expect_setequal(st$Status, "ok")
  expect_false(file.exists(paste0(f, ".R")))              # the script removes itself
})

test_that("compiled copies are not cloned from a library that is not on a local disk", {
  skip_on_os("windows")
  expect_true(Require:::isLocalLib(tempdir()))
  lib <- withr::local_tempdir()
  tinyPkg(lib, "pure")
  tinyPkg(lib, "comp")
  dir.create(file.path(lib, "comp", "libs"))
  d <- file.path(lib, "comp", "DESCRIPTION")
  desc <- read.dcf(d)
  desc[, "Built"] <- sub("^(R [^;]*); *[^;]*;", paste0("\\1; ", R.version$platform, ";"), desc[, "Built"])
  write.dcf(desc, d)
  ip <- installed.packages(lib, fields = c("Built", "NeedsCompilation"), noCache = TRUE)
  expect_setequal(rownames(Require:::canClone(ip)), c("comp", "pure"))
  local_mocked_bindings(isLocalLib = function(libs) rep(FALSE, length(libs)), .package = "Require")
  expect_identical(rownames(Require:::canClone(ip)), "pure")
})

test_that("Install() outside an interactive session load-checks clones and installs any that fail", {
  skip_on_cran()
  skip_if_not_installed("pak")
  repo <- tinyRepo(list(list(name = "tinyIntE", version = "2.0"),
                        list(name = "tinyIntF", version = "0.0.1")))
  from <- withr::local_tempdir()
  tinyBrokenPkg(from, "tinyIntE", version = "1.0")      # meets the request, but fails as a clone
  tinyPkg(from, "tinyIntF")
  to <- withr::local_tempdir()
  withr::local_envvar(TINY_BROKEN_LIB = to)
  withr::local_options(Require.cloneFrom = from, Require.usePak = TRUE,
                       repos = c(tiny = repo, getOption("repos")))
  msgs <- capture_messages(Install(c("tinyIntE", "tinyIntF"), libPaths = to, standAlone = TRUE, verbose = 1))
  expect_true(any(grepl("Cloning.*tinyIntE", msgs)))
  expect_true(any(grepl("failed to load", msgs)))
  ver <- function(p) read.dcf(file.path(to, p, "DESCRIPTION"), "Version")[[1]]
  expect_identical(ver("tinyIntE"), "2.0")               # installed from the repository instead
  expect_identical(ver("tinyIntF"), "0.0.1")             # the clone that loaded stays
  expect_identical(Require:::cloneCheckExcluded(to), "tinyIntE 1.0")
})
