origLibPathsAllTests <- .libPaths()

Sys.setenv("R_REMOTES_UPGRADE" = "never")
Sys.setenv('CRANCACHE_DISABLE' = TRUE)
outOpts <- options("Require.persistentPkgEnv" = TRUE,
                   "install.packages.check.source" = "never",
                   "install.packages.compile.from.source" = "never",
                   "Require.unloadNamespaces" = TRUE)
if (Sys.info()["user"] == "emcintir2") {
  outOpts2 <- options("Require.Home" = "~/GitHub/Require",
                      "Require.RPackageCache" = "~/._RPackageCache/")
} else {
  outOpts2 <- options("Require.Home" = "~/GitHub/PredictiveEcology/Require")
}

out <- utils::capture.output(type = "message", Require:::messageDF(cbind(a = 1.1232), round = 2))
testit::assert({is.character(out)})
testit::assert({is.numeric(as.numeric(gsub(".*: ", "", out)[2]))})

# don't use checkPath here because we are testing normPath!
tmpdir <- Require::tempdir2("test_normPath")
tmpdir <- normalizePath(tmpdir, winslash = "/", mustWork = FALSE)

setwd(tmpdir)

paths <- list("./aaa/zzz",
              "./aaa/zzz/",
              ".//aaa//zzz",
              ".//aaa//zzz/",
              ".\\aaa\\zzz",
              ".\\aaa\\zzz\\",
              paste0(tmpdir, "/aaa/zzz"), # nolint
              paste0(tmpdir, "/aaa/zzz/"), # nolint
              file.path(tmpdir, "aaa", "zzz"))

checked <- Require::normPath(paths)
testit::assert({isTRUE(all.equal(length(unique(checked)), 1))})

# extra checks for missing/NA/NULL
testit::assert({isTRUE(all.equal(Require::normPath(), character()))})
testit::assert({all(is.na(Require::normPath(list(NA, NA_character_))))})
testit::assert({isTRUE(all.equal(Require::normPath(NULL), character()))})

currdir <- getwd()

# don't use checkPath here because we are testing checkPath
tmpdir <- Require::tempdir2("test_checkPath")

setwd(tmpdir)

dir.create("aaa/zzz", recursive = TRUE, showWarnings = FALSE)
paths <- list("./aaa/zzz",
              "./aaa/zzz/",
              ".//aaa//zzz",
              ".//aaa//zzz/",
              ".\\aaa\\zzz",
              ".\\aaa\\zzz\\",
              paste0(tmpdir, "/aaa/zzz"), # nolint
              paste0(tmpdir, "/aaa/zzz/"), # nolint
              file.path(tmpdir, "aaa", "zzz"))

checked <- lapply(paths, checkPath, create = FALSE)
testit::assert({isTRUE(all.equal(length(unique(checked)), 1))})
unlink(tmpdir, recursive = TRUE)

# extra checks for missing/NA/NULL
testit::assert({isTRUE(
  tryCatch(Require::checkPath(),
           "Invalid path: no path specified.", error = function(x) TRUE))})

testit::assert({isTRUE(
  tryCatch(Require::checkPath(NULL), "Invalid path: cannot be NULL.", error = function(x) TRUE))})

testit::assert({isTRUE(
  tryCatch(Require::checkPath(NA_character_),
           "Invalid path: cannot be NA.", error = function(x) TRUE))})

# Case where it is an existing fle
f1 <- tempfile()
testit::assert({file.create(f1)}) ## TRUE
testit::assert({file.exists(f1)}) ## TRUE

out <- utils::capture.output(type = "message", {
  a <- checkPath(f1)
})
testit::assert({isTRUE(grepl("is an existing file", out))})

rst <- .rndstr(1, 6)
testit::assert({is.character(rst)})
testit::assert({nchar(rst) == 6})

a <- list(a = list(d = 1, e = 2:3, f = 4:6), b = list(d = 5, e = 55))
b <- Require::invertList(a) # creates 2-deep, now 3 levels outer --> 2 levels inner
testit::assert({length(b[[1]]) == length(a)})
testit::assert({length(b) == length(a[[1]])})

out <- Require::tempfile2("rand")
testit::assert({isTRUE(all.equal(Require::normPath(dirname(out)),
                                 Require::normPath(file.path(Require::tempdir2(), "rand"))))})

f1 <- list(a = 1, e = 3)
f2 <- list(a = 2, b = 3)
f3 <- list(d = 3, b = 5)
a <- modifyList2(f1)
b <- modifyList2(f1, f2)
d <- modifyList2(f1, f2, f3)
testit::assert({identical(a, f1)})
testit::assert({identical(modifyList(f1, f2), b)})
testit::assert({identical(modifyList(modifyList(f1, f2), f3), d)})

if (!identical(origLibPathsAllTests, .libPaths()))
  Require::setLibPaths(origLibPathsAllTests, standAlone = TRUE, exact = TRUE)
options(outOpts)
options(outOpts2)
