## setLibPaths(updateRprofile = TRUE) writes a block into .Rprofile; R must still start from it.
## #234 lengthened a call inside setLibPaths(); the block was built from its deparsed source by
## deleting lines by count, which left a stray "}" and R failed with "unexpected 'else'".

## The block as setLibPaths() wrote it before #234 (`}` and `else {` on separate lines).
oldFormatBlock <- function(lib, prev = "/prev/lib") {
  c("", "#### setLibPaths start #### New File:FALSE # DO NOT EDIT BETWEEN THESE LINES",
    "### DELETE THESE LINES BELOW TO RESTORE STANDARD R Package LIBRARY",
    paste0("### Previous .libPaths: ", prev),
    paste0("._libPaths <- c('", lib, "')"),
    "._standAlone <- TRUE",
    "{",
    "    .oldLibPaths <- .libPaths()",
    "    if (!dir.exists(._libPaths[1])) dir.create(._libPaths[1], recursive = TRUE)",
    "    gte4.1 <- isTRUE(getRversion() >= \"4.1\")",
    "    if (gte4.1) {",
    "        do.call(.libPaths, list(new = ._libPaths, if (gte4.1) include.site <- !._standAlone))",
    "    }",
    "    .shim_fun <- .libPaths",
    "    .shim_env <- new.env(parent = environment(.shim_fun))",
    "    if (isTRUE(._standAlone)) {",
    "        .shim_env$.Library <- utils::tail(.libPaths(), 1)",
    "    }",
    "    else {",
    "        .shim_env$.Library <- .libPaths()",
    "    }",
    "    .shim_env$.Library.site <- character()",
    "    environment(.shim_fun) <- .shim_env",
    "    .shim_fun(unique(._libPaths))",
    "    message(\".libPaths() is now: \", paste(.libPaths(), collapse = \", \"))",
    "}",
    "message(\"To reset libPaths to previous state, run: Require::setupOff() (or delete section in .Rprofile file)\") ",
    "#### setLibPaths end")
}

## First library R reports in a fresh Rscript started in `dir`, reading dir/.Rprofile. The
## result carries all the output as attribute "all", for the failure message.
freshLibPath1 <- function(dir) {
  ## processx, not system2(): system2(env = ) is ignored on Windows, and the child must see
  ## R_PROFILE_USER and start in `dir` whatever R CMD check set for this session
  ## the environment of this session without R_PROFILE_USER, R_ENVIRON_USER and R_LIBS: callr sets
  ## them for its sessions (R CMD check runs the tests in one), and the child R then re-reads them
  ## from the R_ENVIRON_USER file and loads callr's profile instead of dir/.Rprofile. Without them
  ## R reads dir/.Rprofile as it does for a user.
  childEnv <- Sys.getenv()
  childEnv <- childEnv[!toupper(names(childEnv)) %in% c("R_PROFILE_USER", "R_PROFILE", "R_ENVIRON_USER", "R_ENVIRON", "R_LIBS", "R_LIBS_USER")]
  res <- processx::run(
    file.path(R.home("bin"), "Rscript"),
    c("-e", paste0("cat('PROFILE_RAN=', exists('._libPaths'), '\\n', ",
                   "'R_PROFILE_USER=', Sys.getenv('R_PROFILE_USER'), ' exists=', file.exists(Sys.getenv('R_PROFILE_USER')), '\\n', ",
                   "'R_LIBS=', Sys.getenv('R_LIBS'), ' R_LIBS_USER=', Sys.getenv('R_LIBS_USER'), ",
                   "' R_LIBS_SITE=', Sys.getenv('R_LIBS_SITE'), '\\n', 'getwd=', getwd(), '\\n', ",
                   "'libPaths=', paste(.libPaths(), collapse = ' | '), '\\n', sep = ''); ",
                   "cat('LIBPATH1=', .libPaths()[1], '\\n', sep = '')")),
    wd = dir, env = childEnv,
    stderr = "2>&1", error_on_status = FALSE)
  out <- strsplit(gsub("\r", "", res$stdout), "\n")[[1]]
  lib <- sub("^LIBPATH1=", "", grep("^LIBPATH1=", out, value = TRUE))
  structure(if (length(lib)) lib[length(lib)] else NA_character_, all = paste(out, collapse = "\n"))
}
expectLibPath1 <- function(dir, lib) {
  got <- freshLibPath1(dir)
  expect_identical(normalizePath(got, mustWork = FALSE), normalizePath(lib, mustWork = FALSE),
                   info = attr(got, "all"))
}

local_rprofileDir <- function(envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = envir)
  withr::local_dir(dir, .local_envir = envir)
  oldLP <- .libPaths()
  withr::defer(.libPaths(oldLP), envir = envir)
  withr::local_options(Require.cloneFrom = FALSE, .local_envir = envir)
  normalizePath(dir)
}

test_that("an old-format block for library A is replaced by one for B that parses and starts R on B", {
  dir <- local_rprofileDir()
  A <- file.path(dir, "A"); B <- file.path(dir, "B")
  writeLines(c("# my own line", "message(\"PROFILE RAN\")", oldFormatBlock(normPath(A))), ".Rprofile")
  expect_no_error(parse(".Rprofile"))
  suppressMessages(setLibPaths(B, updateRprofile = TRUE, exact = TRUE))
  expect_no_error(parse(".Rprofile"))
  expect_identical(sum(grepl("^#### setLibPaths start", readLines(".Rprofile"))), 1L)
  expectLibPath1(dir, B)
})

test_that("a new-format block, and a fresh file, parse and start R on the new library", {
  dir <- local_rprofileDir()
  A <- file.path(dir, "A"); B <- file.path(dir, "B")
  ## a file with only the user's own line (setLibPaths adds its block after it)
  writeLines("message(\"PROFILE RAN\")", ".Rprofile")
  suppressMessages(setLibPaths(A, updateRprofile = TRUE, exact = TRUE))
  expect_no_error(parse(".Rprofile"))
  expectLibPath1(dir, A)
  ## now a block written by this version, replaced by another
  suppressMessages(setLibPaths(B, updateRprofile = TRUE, exact = TRUE))
  expect_no_error(parse(".Rprofile"))
  expectLibPath1(dir, B)
  ## standAlone = FALSE as well
  suppressMessages(setLibPaths(A, standAlone = FALSE, updateRprofile = TRUE, exact = TRUE))
  expect_no_error(parse(".Rprofile"))
  expectLibPath1(dir, A)
})

test_that("setupOff() removes the block and keeps the user's own lines", {
  dir <- local_rprofileDir()
  A <- file.path(dir, "A"); B <- file.path(dir, "B")
  writeLines(c("# my own line", oldFormatBlock(A), "x <- 1"), ".Rprofile")
  suppressMessages(setLibPaths(B, updateRprofile = TRUE, exact = TRUE))
  suppressMessages(setupOff())
  expect_identical(readLines(".Rprofile"), c("# my own line", "x <- 1"))
  ## a block written into a file setLibPaths created: the file goes with it
  unlink(".Rprofile")
  suppressMessages(setLibPaths(B, updateRprofile = TRUE, exact = TRUE))
  suppressMessages(setupOff())
  expect_false(file.exists(".Rprofile"))
})

test_that("a Windows-style path with backslashes is written as a valid string", {
  ## `C:\Users\...` is not a valid R string literal ("'\U' used without hex digits")
  dir <- local_rprofileDir()
  winLib <- "C:\\Users\\someone\\Documents\\R\\win-library\\4.6"
  winLib2 <- "D:\\a\\b'quoted"
  Require:::setLibPathsUpdateRprofile(c(winLib, winLib2), standAlone = TRUE, updateRprofile = ".Rprofile",
                                      verbose = 0)
  expect_no_error(exprs <- parse(".Rprofile"))
  env <- new.env()
  eval(exprs[1:2], env)                       # ._libPaths and ._standAlone, as R reads them at startup
  expect_identical(env$._libPaths, c(winLib, winLib2))
  expect_true(env$._standAlone)
  ## the same block is recognised as unchanged on the next call
  before <- readLines(".Rprofile")
  Require:::setLibPathsUpdateRprofile(c(winLib, winLib2), standAlone = TRUE, updateRprofile = ".Rprofile",
                                      verbose = 0)
  expect_identical(readLines(".Rprofile"), before)
})

test_that("a fresh .Rprofile, created by setLibPaths, parses and starts R on the new library", {
  dir <- local_rprofileDir()
  A <- file.path(dir, "A")
  expect_false(file.exists(".Rprofile"))
  suppressMessages(setLibPaths(A, updateRprofile = TRUE, exact = TRUE))
  expect_no_error(parse(".Rprofile"))
  expectLibPath1(dir, A)
})
