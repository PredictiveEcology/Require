out <- capture.output(type = "message", messageDF(cbind(a = 1.1232), round = 2))
testit::assert(is.character(out))
testit::assert(is.numeric(as.numeric(gsub(".*: ", "", out)[2])))



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


a <- list(a = list(d = 1, e = 2:3, f = 4:6), b = list(d = 5, e = 55))
b <- invertList(a) # creates 2-deep, now 3 levels outer --> 2 levels inner
testit::assert(length(b[[1]]) == length(a))
testit::assert(length(b) == length(a[[1]]))

out <- tempfile2("rand")
testit::assert(isTRUE(all.equal(normPath(dirname(out)), normPath(file.path(tempdir2(), "rand")))))
