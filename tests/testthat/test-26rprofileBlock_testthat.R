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

## First library R reports in a fresh Rscript started in `dir`, which reads dir/.Rprofile.
freshLibPath1 <- function(dir) {
  withr::with_dir(dir, withr::with_envvar(c(R_PROFILE_USER = NA, R_LIBS = NA, R_LIBS_USER = NA),
    system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote("cat(.libPaths()[1])")),
            stdout = TRUE, stderr = FALSE)))
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
  writeLines(c("# my own line", oldFormatBlock(A)), ".Rprofile")
  expect_no_error(parse(".Rprofile"))
  suppressMessages(setLibPaths(B, updateRprofile = TRUE, exact = TRUE))
  expect_no_error(parse(".Rprofile"))
  expect_identical(sum(grepl("^#### setLibPaths start", readLines(".Rprofile"))), 1L)
  expect_identical(normalizePath(freshLibPath1(dir)), normalizePath(B))
})

test_that("a new-format block, and a fresh file, parse and start R on the new library", {
  dir <- local_rprofileDir()
  A <- file.path(dir, "A"); B <- file.path(dir, "B")
  ## fresh file
  suppressMessages(setLibPaths(A, updateRprofile = TRUE, exact = TRUE))
  expect_no_error(parse(".Rprofile"))
  expect_identical(normalizePath(freshLibPath1(dir)), normalizePath(A))
  ## now a block written by this version, replaced by another
  suppressMessages(setLibPaths(B, updateRprofile = TRUE, exact = TRUE))
  expect_no_error(parse(".Rprofile"))
  expect_identical(normalizePath(freshLibPath1(dir)), normalizePath(B))
  ## standAlone = FALSE as well
  suppressMessages(setLibPaths(A, standAlone = FALSE, updateRprofile = TRUE, exact = TRUE))
  expect_no_error(parse(".Rprofile"))
  expect_identical(normalizePath(freshLibPath1(dir)), normalizePath(A))
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
