## Require.cloneFrom: copy packages from the cloneFrom libraries instead of installing
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

## `stat` columns "inode links size" for each file (GNU stat on Linux; BSD stat on macOS).
statFiles <- function(f) {
  fmt <- if (Sys.info()[["sysname"]] == "Darwin") c("-f", shQuote("%i %l %z")) else c("-c", shQuote("%i %h %s"))
  out <- system2("stat", c(fmt, shQuote(f)), stdout = TRUE)
  stopifnot(length(out) == length(f))
  m <- do.call(rbind, strsplit(out, " "))
  data.frame(file = f, inode = m[, 1], links = as.integer(m[, 2]), size = as.numeric(m[, 3]))
}
pkgFiles <- function(lib, pkgs) {
  unlist(lapply(pkgs, function(p) dir(file.path(lib, p), recursive = TRUE, all.files = TRUE, full.names = TRUE)))
}

test_that("pakClonePackages copies packages that meet every requirement, never hard-links them", {
  from <- cloneFromLib()
  to <- withr::local_tempdir()
  withr::local_options(Require.cloneFrom = from)
  setSolve()
  out <- Require:::pakClonePackages(planDT(), libPaths = to, verbose = 0)
  expect_true(all(out$needInstall == Require:::.txtDontInstall))
  expect_true(all(out$installResult == "OK"))
  expect_true(all(file.exists(file.path(to, c("tinyCloneA", "tinyCloneB"), "DESCRIPTION"))))
  if (!isWindows()) {
    ## (statFiles() uses the stat syntax of the platform: BSD stat on macOS has no `-c`, and an
    ## empty result there made the two inodes look identical)
    st <- statFiles(file.path(c(from, to), "tinyCloneA", "DESCRIPTION"))
    expect_identical(nrow(st), 2L)
    expect_false(identical(st$inode[1], st$inode[2]))
    expect_true(all(st$links == 1L))
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
  # the check leaves nothing in the library but its status file
  expect_identical(grep("RequireCloneCheck", dir(to, all.files = TRUE), value = TRUE), basename(f))
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

## ---- clones are copies, never hard links; concurrent clones cannot damage each other ----

test_that("every file of a clone is its own copy: link count 1, new inode", {
  skip_on_os("windows")
  from <- cloneFromLib()
  to <- withr::local_tempdir()
  withr::local_options(Require.cloneFrom = from)
  setSolve()
  Require:::pakClonePackages(planDT(), libPaths = to, verbose = 0)
  pk <- c("tinyCloneA", "tinyCloneB")
  src <- statFiles(pkgFiles(from, pk))
  dst <- statFiles(pkgFiles(to, pk))
  expect_gt(nrow(dst), 0)
  expect_identical(nrow(dst), nrow(src))
  expect_true(all(dst$links == 1L))
  expect_true(all(src$links == 1L))
  expect_length(intersect(src$inode, dst$inode), 0)
  expect_identical(dst$size, src$size)
  ## nothing of the clone's own working is left in the library
  expect_setequal(dir(to, all.files = TRUE, no.. = TRUE), pk)
})

test_that("cloning over an existing clone replaces it and leaves the source alone", {
  skip_on_os("windows")
  from <- cloneFromLib()
  to <- withr::local_tempdir()
  cp <- Require:::clonablePackages(from, c("tinyCloneA", "tinyCloneB"))
  Require:::cloneCopies(cp, toLib = to)
  before <- statFiles(pkgFiles(from, c("tinyCloneA", "tinyCloneB")))
  md5Before <- tools::md5sum(before$file)
  ## damage the clone, as a later in-place write would
  rdb <- file.path(to, "tinyCloneA", "R", "tinyCloneA.rdb")
  writeLines("damaged", rdb)
  Require:::cloneCopies(cp, toLib = to)
  after <- statFiles(before$file)
  expect_identical(after, before)
  expect_identical(tools::md5sum(before$file), md5Before)
  expect_identical(unname(tools::md5sum(rdb)), unname(md5Before[file.path(from, "tinyCloneA", "R", "tinyCloneA.rdb")]))
  expect_true(all(statFiles(pkgFiles(to, c("tinyCloneA", "tinyCloneB")))$links == 1L))
  expect_setequal(dir(to, all.files = TRUE, no.. = TRUE), c("tinyCloneA", "tinyCloneB"))
})

for (method in c("copy", "hardlinkReadOnly")) test_that(paste0("two processes cloning the same packages into one library at once damage nothing (", method, ")"), {
  skip_on_os("windows")
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")
  pkgs <- paste0("tinyRace", LETTERS[1:6])
  from <- withr::local_tempdir()
  for (p in pkgs) tinyPkg(from, p)
  to <- withr::local_tempdir()
  sync <- withr::local_tempdir()
  before <- statFiles(pkgFiles(from, pkgs))
  md5Before <- tools::md5sum(before$file)
  ## the source of the code under test: the package being tested, installed or loaded with pkgload
  reqPath <- if (pkgload::is_dev_package("Require")) pkgload::pkg_path(system.file(package = "Require")) else NULL
  ## earlier tests may have switched .libPaths(); the worker needs wherever the loaded packages are
  libs <- unique(c(.libPaths(), dirname(vapply(loadedNamespaces(), function(n)
    tryCatch(getNamespaceInfo(n, "path"), error = function(e) NA_character_), ""))))
  libs <- libs[!is.na(libs) & dir.exists(libs)]
  worker <- function(reqPath, from, to, pkgs, sync, id, method) {
    if (is.null(reqPath)) loadNamespace("Require") else pkgload::load_all(reqPath, quiet = TRUE)
    options(Require.cloneMethod = method)
    file.create(file.path(sync, paste0("ready", id)))
    while (!file.exists(file.path(sync, "go"))) Sys.sleep(0.005)
    for (i in 1:8) {
      cp <- Require:::clonablePackages(from, pkgs)
      Require:::cloneCopies(cp, toLib = to)
    }
    "done"
  }
  procs <- lapply(1:2, function(id)
    callr::r_bg(worker, args = list(reqPath, from, to, pkgs, sync, id, method), stderr = "2>&1", libpath = libs))
  while (length(dir(sync, pattern = "^ready")) < 2 && all(vapply(procs, function(p) p$is_alive(), logical(1))))
    Sys.sleep(0.05)
  file.create(file.path(sync, "go"))
  for (p in procs) p$wait(120000)
  for (p in procs) expect_identical(p$get_result(), "done")
  ## the sources are untouched
  after <- statFiles(before$file)
  expect_identical(after[c("file", "inode", "size")], before[c("file", "inode", "size")])
  expect_identical(tools::md5sum(before$file), md5Before)
  ## the clones are whole, loadable, and no file is empty unless its source is
  dst <- statFiles(pkgFiles(to, pkgs))
  expect_identical(sort(sub(paste0("^", to), "", dst$file)), sort(sub(paste0("^", from), "", before$file)))
  expect_false(any(dst$size == 0 & !sub(paste0("^", to), "", dst$file) %in% sub(paste0("^", from), "", before$file[before$size == 0])))
  expect_true(all(dst$links == if (method == "copy") 1L else 2L)) # 2: this file and its source
  res <- system2(file.path(R.home("bin"), "Rscript"),
                 c("-e", shQuote(paste0("cat(vapply(c(", paste0("'", pkgs, "'", collapse = ","),
                                        "), function(p) as.character(requireNamespace(p)), ''))"))),
                 env = paste0("R_LIBS=", to), stdout = TRUE, stderr = FALSE)
  expect_identical(res, paste(rep("TRUE", length(pkgs)), collapse = " "))
  expect_setequal(dir(to, all.files = TRUE, no.. = TRUE), pkgs)
})

## A lock folder as withCloneLock() makes it, held by `pid` on `host`, last refreshed `ageSecs` ago.
makeLock <- function(lib, pid, host = Sys.info()[["nodename"]], ageSecs = 0) {
  lock <- file.path(lib, "00LOCK-RequireClone")
  dir.create(lock)
  writeLines(paste(pid, host, "1 1"), file.path(lock, "owner"))
  Sys.setFileTime(lock, Sys.time() - ageSecs)
  lock
}
## The pid of a process that has finished.
deadPid <- function() callr::r(function() Sys.getpid())

test_that("a lock whose owner is still running is kept, however long it has been held", {
  skip_on_os("windows")
  lib <- withr::local_tempdir()
  lock <- makeLock(lib, Sys.getpid(), ageSecs = 3600)        # an hour: well past the old 5-minute limit
  expect_error(Require:::withCloneLock(lib, function(refresh) "ran", wait = 1), "Another process")
  expect_true(dir.exists(lock))
  expect_identical(readLines(file.path(lock, "owner")), paste(Sys.getpid(), Sys.info()[["nodename"]], "1 1"))
})

test_that("a lock whose owner has died is taken over and released afterwards", {
  skip_on_os("windows")
  lib <- withr::local_tempdir()
  lock <- makeLock(lib, deadPid(), ageSecs = 1)
  dir.create(file.path(lib, "00CLONE-leftover")); dir.create(file.path(lib, "00OLD-leftover"))
  dir.create(file.path(lib, "somePkg"))
  expect_identical(Require:::withCloneLock(lib, function(refresh) "ran", wait = 5), "ran")
  expect_false(dir.exists(lock))
  ## leftovers of an interrupted clone are removed, packages are not
  expect_identical(dir(lib, all.files = TRUE, no.. = TRUE), "somePkg")
})

test_that("a lock from another host is kept until it has not been refreshed for staleOtherHost seconds", {
  lib <- withr::local_tempdir()
  lock <- makeLock(lib, 1, host = "some-other-host", ageSecs = 100)
  expect_error(Require:::withCloneLock(lib, function(refresh) "ran", wait = 1, staleOtherHost = 3600), "Another process")
  expect_true(dir.exists(lock))
  expect_identical(Require:::withCloneLock(lib, function(refresh) "ran", wait = 5, staleOtherHost = 10), "ran")
})

test_that("a clone refreshes its lock; if the lock is lost the clone goes back to acquire it and runs again", {
  skip_on_os("windows")
  lib <- withr::local_tempdir()
  lock <- file.path(lib, "00LOCK-RequireClone")
  runs <- 0L
  out <- Require:::withCloneLock(lib, function(refresh) {
    runs <<- runs + 1L
    if (runs == 1L) {
      Sys.setFileTime(lock, Sys.time() - 1000)
      refresh()
      expect_lt(as.numeric(difftime(Sys.time(), file.mtime(lock), units = "secs")), 100)
      ## the lock is taken over by another process (here: replaced by a dead process's)
      writeLines(paste(deadPid(), Sys.info()[["nodename"]], "1 1"), file.path(lock, "owner"))
      refresh()                                              # finds it is no longer ours
      stop("not reached")
    } else if (runs == 2L) {
      unlink(lock, recursive = TRUE)                         # or: the folder is gone
      refresh()
      stop("not reached")
    }
    "finished"
  }, wait = 10)
  expect_identical(out, "finished")
  expect_identical(runs, 3L)
  expect_identical(dir(lib, all.files = TRUE, no.. = TRUE), character())
})

test_that("two processes taking over one abandoned lock never hold it together", {
  skip_on_os("windows")
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")
  lib <- withr::local_tempdir()
  sync <- withr::local_tempdir()
  makeLock(lib, deadPid(), ageSecs = 1)
  reqPath <- if (pkgload::is_dev_package("Require")) pkgload::pkg_path(system.file(package = "Require")) else NULL
  libs <- unique(c(.libPaths(), dirname(vapply(loadedNamespaces(), function(n)
    tryCatch(getNamespaceInfo(n, "path"), error = function(e) NA_character_), ""))))
  libs <- libs[!is.na(libs) & dir.exists(libs)]
  worker <- function(reqPath, lib, sync, id) {
    if (is.null(reqPath)) loadNamespace("Require") else pkgload::load_all(reqPath, quiet = TRUE)
    file.create(file.path(sync, paste0("ready", id)))
    while (!file.exists(file.path(sync, "go"))) Sys.sleep(0.002)
    both <- 0L
    for (i in 1:15) Require:::withCloneLock(lib, function(refresh) {
      if (!dir.create(file.path(sync, "inside"), showWarnings = FALSE)) both <<- both + 1L
      Sys.sleep(0.05)
      unlink(file.path(sync, "inside"), recursive = TRUE)
    }, wait = 60)
    both
  }
  procs <- lapply(1:2, function(id)
    callr::r_bg(worker, args = list(reqPath, lib, sync, id), stderr = "2>&1", libpath = libs))
  while (length(dir(sync, pattern = "^ready")) < 2 && all(vapply(procs, function(p) p$is_alive(), logical(1))))
    Sys.sleep(0.05)
  file.create(file.path(sync, "go"))
  for (p in procs) p$wait(120000)
  for (p in procs) expect_identical(p$get_result(), 0L)
  expect_false(dir.exists(file.path(lib, "00LOCK-RequireClone")))
  expect_identical(dir(lib, all.files = TRUE, no.. = TRUE), character())
})

## ---- Require.cloneMethod = "hardlinkReadOnly" (experimental) ----------------------------------

## Clone `pkgs` from `from` into a new library with the given method; return the library.
cloneWith <- function(from, pkgs, method, envir = parent.frame()) {
  to <- withr::local_tempdir(.local_envir = envir)
  withr::local_options(Require.cloneMethod = method)
  Require:::cloneCopies(Require:::clonablePackages(from, pkgs), toLib = to)
  to
}
## Version of `pkg` as a fresh R process loads it from `lib` ("FAIL" if it does not load).
versionIn <- function(lib, pkg) {
  out <- system2(file.path(R.home("bin"), "Rscript"),
                 c("-e", shQuote(paste0("cat(if (suppressWarnings(requireNamespace('", pkg, "', quietly = TRUE))) as.character(packageVersion('", pkg, "')) else 'FAIL')"))),
                 env = paste0("R_LIBS=", lib), stdout = TRUE, stderr = FALSE)
  if (length(out)) out else "FAIL"
}
pkgsAB <- c("tinyCloneA", "tinyCloneB")
## chmod does not stop root, so the in-place writes below would succeed
skipIfRoot <- function() testthat::skip_if(identical(Sys.info()[["effective_user"]], "root"), "root ignores file permissions")

test_that("the default clone method is copy, also from the environment variable", {
  expect_identical(RequireOptions()$Require.cloneMethod, "copy")
  expect_identical(getOption("Require.cloneMethod"), "copy")
  withr::local_envvar(R_REQUIRE_CLONE_METHOD = "hardlinkReadOnly")
  expect_identical(RequireOptions()$Require.cloneMethod, "hardlinkReadOnly")
  expect_identical(Require:::cloneMethod("copy"), "copy")
  expect_warning(expect_identical(Require:::cloneMethod("symlink"), "copy"), "must be 'copy' or 'hardlinkReadOnly'")
})

test_that("hardlinkReadOnly: files are shared and read-only; an in-place write fails and truncates nothing", {
  skip_on_os("windows")
  skipIfRoot()
  from <- cloneFromLib()
  to <- cloneWith(from, pkgsAB, "hardlinkReadOnly")
  src <- pkgFiles(from, pkgsAB)
  rel <- sub(paste0("^", from, "/"), "", src)
  st <- statFiles(c(src, file.path(to, rel)))
  n <- length(src)
  expect_identical(st$inode[1:n], st$inode[n + 1:n]) # one file, two names
  expect_true(all(st$links == 2L))
  expect_false(any(file.access(c(src, file.path(to, rel)), 2) == 0)) # read-only under both names
  expect_true(all(dir.exists(file.path(to, pkgsAB))) && all(file.access(file.path(to, pkgsAB), 2) == 0)) # folders are new and writable
  expect_setequal(dir(to, all.files = TRUE, no.. = TRUE), pkgsAB) # nothing of the clone's own working left

  ## the 2026-11-25 incident: an in-place write onto a shared file. It fails; both names keep their bytes.
  rdb <- "tinyCloneA/R/tinyCloneA.rdb"
  empty <- withr::local_tempfile()
  file.create(empty)
  size <- file.size(file.path(from, rdb))
  expect_gt(size, 0)
  for (f in file.path(c(from, to), rdb)) {
    expect_warning(expect_false(file.copy(empty, f, overwrite = TRUE)))
    expect_error(suppressWarnings(writeBin(raw(0), f)))
    expect_error(suppressWarnings(cat("", file = f)))
  }
  expect_identical(file.size(file.path(c(from, to), rdb)), c(size, size))

  ## contrast: the plain hard link the old code made, with no chmod, is emptied under both names
  plainFrom <- withr::local_tempdir()
  plainTo <- withr::local_tempdir()
  tinyPkg(plainFrom, "tinyCloneA")
  old <- file.path(plainFrom, rdb)
  expect_true(file.link(old, file.path(plainTo, "x.rdb")))
  expect_true(file.copy(empty, file.path(plainTo, "x.rdb"), overwrite = TRUE))
  expect_identical(file.size(c(old, file.path(plainTo, "x.rdb"))), c(0, 0))
})

test_that("hardlinkReadOnly through pakClonePackages: the clone loads and the source is still intact", {
  skip_on_os("windows")
  from <- cloneFromLib()
  to <- withr::local_tempdir()
  withr::local_options(Require.cloneFrom = from, Require.cloneMethod = "hardlinkReadOnly")
  setSolve()
  out <- Require:::pakClonePackages(planDT(), libPaths = to, verbose = 0)
  expect_true(all(out$installResult == "OK"))
  expect_true(all(statFiles(pkgFiles(to, pkgsAB))$links == 2L))
  expect_identical(versionIn(to, "tinyCloneB"), "0.0.1")
  expect_identical(versionIn(from, "tinyCloneB"), "0.0.1")
})

test_that("hardlinkReadOnly: a newer version installed into the SOURCE library leaves the clone intact and loadable", {
  skip_on_os("windows")
  skipIfRoot()
  from <- cloneFromLib()
  to <- cloneWith(from, pkgsAB, "hardlinkReadOnly")
  cloned <- pkgFiles(to, pkgsAB)
  md5 <- tools::md5sum(cloned)
  repo <- tinyRepo(list(list(name = "tinyCloneA", version = "0.0.2")))
  withr::local_options(repos = c(tiny = repo))
  utils::install.packages("tinyCloneA", lib = from, type = "source", quiet = TRUE) # R CMD INSTALL over read-only, linked files
  expect_identical(versionIn(from, "tinyCloneA"), "0.0.2")
  expect_identical(versionIn(to, "tinyCloneA"), "0.0.1")  # the clone keeps the old one
  expect_identical(versionIn(to, "tinyCloneB"), "0.0.1")  # and still loads what imports it
  expect_identical(tools::md5sum(cloned), md5)
  clonedA <- pkgFiles(to, "tinyCloneA")
  expect_true(all(statFiles(clonedA)$links == 1L)) # its files lost their other name, and are still read-only
  expect_true(all(statFiles(pkgFiles(to, "tinyCloneB"))$links == 2L)) # B is still shared with the source
  expect_false(any(file.access(cloned, 2) == 0))
  expect_length(dir(from, pattern = "^00LOCK|^00OLD", all.files = TRUE), 0)
  ## and the other way: a newer version into the CLONE's library
  utils::install.packages("tinyCloneA", lib = to, type = "source", quiet = TRUE)
  expect_identical(versionIn(to, "tinyCloneA"), "0.0.2")
  expect_identical(versionIn(from, "tinyCloneA"), "0.0.2")
})

test_that("hardlinkReadOnly: remove.packages() and unlink() in either library leave the other intact", {
  skip_on_os("windows")
  skipIfRoot()
  from <- cloneFromLib()
  to <- cloneWith(from, pkgsAB, "hardlinkReadOnly")
  to2 <- cloneWith(from, pkgsAB, "hardlinkReadOnly")
  md5 <- tools::md5sum(pkgFiles(from, pkgsAB))
  utils::remove.packages("tinyCloneB", lib = to)           # R's own remover
  expect_false(dir.exists(file.path(to, "tinyCloneB")))
  expect_identical(unlink(file.path(to2, "tinyCloneA"), recursive = TRUE), 0L) # a plain recursive unlink
  expect_false(dir.exists(file.path(to2, "tinyCloneA")))
  expect_identical(tools::md5sum(names(md5)), md5)
  expect_identical(versionIn(from, "tinyCloneB"), "0.0.1")
  expect_identical(versionIn(to, "tinyCloneA"), "0.0.1")
  expect_identical(versionIn(to2, "tinyCloneA"), "FAIL") # removed there only
  ## removing from the source library: the clone is intact
  utils::remove.packages(pkgsAB, lib = from)
  expect_identical(versionIn(to, "tinyCloneA"), "0.0.1")
  expect_true(all(file.exists(pkgFiles(to, "tinyCloneA"))))
})

test_that("hardlinkReadOnly gives up a package cleanly when a link or chmod fails, and the clone then copies", {
  skip_on_os("windows")
  from <- cloneFromLib()
  pkg <- file.path(from, "tinyCloneA")
  modes <- file.mode(pkgFiles(from, "tinyCloneA"))
  noLink <- function(from, to) rep(FALSE, length(from))
  ## a link that fails part-way through (the first file is linked, as before a cross-file-system failure)
  halfLink <- function(from, to) c(base::file.link(from[1], to[1]), rep(FALSE, length(from) - 1))
  noChmod <- function(paths, mode, ...) rep(FALSE, length(paths))
  ## (chmod first: a source file already made read-only is not chmodded again)
  for (fns in list(list(chmod = noChmod), list(link = noLink), list(link = halfLink))) {
    work <- withr::local_tempdir()
    expect_false(do.call(Require:::linkPackageReadOnly, c(list(pkg, work), fns)))
    expect_length(dir(work, all.files = TRUE, no.. = TRUE), 0) # nothing half-linked is left behind
    if (!is.null(fns$chmod)) expect_identical(file.mode(pkgFiles(from, "tinyCloneA")), modes) # chmod never succeeded
  }
  ## a symlink in a package cannot be linked as a file: the whole package is copied instead
  file.symlink("DESCRIPTION", file.path(pkg, "linkToDESCRIPTION"))
  to <- cloneWith(from, pkgsAB, "hardlinkReadOnly")
  st <- statFiles(pkgFiles(to, "tinyCloneA"))
  expect_true(all(st$links == 1L)) # copied, so no link to the source
  expect_true(all(statFiles(pkgFiles(to, "tinyCloneB"))$links == 2L)) # the other package was linked
  expect_identical(versionIn(to, "tinyCloneA"), "0.0.1")
  ## across file systems, where there is one to try: /dev/shm is not the disk of tempdir()
  shm <- "/dev/shm"
  if (dir.exists(shm) && file.access(shm, 2) == 0) {
    fromShm <- withr::local_tempdir(tmpdir = shm)
    file.copy(file.path(from, pkgsAB), fromShm, recursive = TRUE)
    dev <- function(p) as.numeric(system2("stat", c(if (Sys.info()[["sysname"]] == "Darwin") c("-f", "%d") else c("-c", "%d"), shQuote(p)), stdout = TRUE))
    toDisk <- cloneWith(fromShm, pkgsAB, "hardlinkReadOnly")
    if (dev(fromShm) != dev(toDisk)) expect_true(all(statFiles(pkgFiles(toDisk, pkgsAB))$links == 1L))
    expect_identical(versionIn(toDisk, "tinyCloneB"), "0.0.1")
  }
})

test_that("hardlinkReadOnly is never used on Windows", {
  testthat::local_mocked_bindings(isWindows = function() TRUE, .package = "Require")
  expect_identical(Require:::cloneMethod("hardlinkReadOnly"), "copy")
})

test_that("hardlinkReadOnly: space and time against copy, on about 20 packages", {
  skip_on_os("windows")
  skip_if_not(nzchar(Sys.which("du")))
  ip <- utils::installed.packages(lib.loc = .libPaths())
  pkgs <- unique(ip[, "Package"][!ip[, "Package"] %in% Require:::.pakNoCopyPkgs()])
  pkgs <- head(pkgs[order(ip[match(pkgs, ip[, "Package"]), "Priority"] %in% "base")], 20) # not-base first
  from <- withr::local_tempdir()
  for (p in pkgs) file.copy(file.path(ip[match(p, ip[, "Package"]), "LibPath"], p), from, recursive = TRUE) # a temporary source library
  pkgs <- intersect(pkgs, dir(from))
  res <- lapply(c("copy", "hardlinkReadOnly"), function(method) {
    to <- withr::local_tempdir()
    withr::local_options(Require.cloneMethod = method)
    cp <- Require:::clonablePackages(from, pkgs)
    secs <- system.time(Require:::cloneCopies(cp, toLib = to))[["elapsed"]]
    ## `du` given both libraries counts a file once: the second line is what the clone added
    kb <- as.numeric(sub("\\t.*", "", system2("du", c("-sk", shQuote(from), shQuote(to)), stdout = TRUE)))
    data.frame(method = method, packages = length(cp$Package), files = length(pkgFiles(to, pkgs)),
               sourceKB = kb[1], addedKB = kb[2], seconds = round(secs, 2))
  })
  res <- do.call(rbind, res)
  cat("\n\nclone cost, ", res$packages[1], " packages from a temporary library:\n", sep = "")
  print(res, row.names = FALSE)
  cat("\n")
  expect_identical(res$packages[1], res$packages[2])
  expect_identical(res$files[1], res$files[2])
  expect_gt(res$addedKB[1], 0)
})
