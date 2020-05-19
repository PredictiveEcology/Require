out <- capture.output(type = "message", messageDF(data.frame(a = 1)))
testit::assert(is.character(out))

# don't use checkPath here because we are testing normPath!
tmpdir <- tempdir2("test_normPath")
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

checked <- normPath(paths)
testit::assert(isTRUE(all.equal(length(unique(checked)), 1)))

# extra checks for missing/NA/NULL
testit::assert(isTRUE(all.equal(normPath(), character())))
testit::assert(all(is.na(normPath(list(NA, NA_character_)))))
testit::assert(isTRUE(all.equal(normPath(NULL), character())))

currdir <- getwd()

# don't use checkPath here because we are testing checkPath
tmpdir <- tempdir2("test_checkPath")

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
testit::assert(isTRUE(all.equal(length(unique(checked)), 1)))
unlink(tmpdir, recursive = TRUE)

# extra checks for missing/NA/NULL
testit::assert(isTRUE(
  tryCatch(checkPath(),
           "Invalid path: no path specified.", error = function(x) TRUE)))

testit::assert(isTRUE(
  tryCatch(checkPath(NULL), "Invalid path: cannot be NULL.", error = function(x) TRUE)))

testit::assert(isTRUE(
  tryCatch(checkPath(NA_character_),
           "Invalid path: cannot be NA.", error = function(x) TRUE)))

# Case where it is an existing fle
f1 <- tempfile()
testit::assert(file.create(f1)) ## TRUE
testit::assert(file.exists(f1)) ## TRUE

out <- capture.output(type = "message", { a <- checkPath(f1) })
testit::assert(isTRUE(grepl("is an existing file", out)))

rst <- rndstr(1, 6)
testit::assert(is.character(rst))
testit::assert(nchar(rst) == 6)
