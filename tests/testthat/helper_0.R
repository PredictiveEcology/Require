## pakLibForTests(): one shared, single-package library carrying pak, built at
## most once per session.
##
## setupTest() narrows .libPaths() to a per-file temp lib, which puts the real
## pak out of reach of pak's callr subprocess: that subprocess runs its own
## loadNamespace("pak") against the *inherited* .libPaths() and dies with
## "there is no package called 'pak'" if no entry has it -- a pak merely loaded
## in this session does not reach it. Require's ensurePakInProjectLib() covers
## that by installing a fresh 12 MB pak into the project lib, correct but ~5.6s
## per lib, once for each of the 15 test files that call setupTest(). Carrying
## one shared lib as the *second* .libPaths() entry serves all of them, and
## works under standAlone = TRUE too, since that only controls whether the user
## libs get appended -- not how many entries you may pass.
##
## Kept to a single package so it adds only pak to anything reading across
## .libPaths() (installed.packages(), pkgSnapshot(), ...). Defined here rather
## than in setup.R so it does not depend on which environment testthat sources
## setup files into, and lazily so a run that never calls setupTest() never
## pays for it.
pakLibForTests <- local({
  lib <- NULL
  function() {
    if (!is.null(lib)) return(lib)
    lib <<- if (isTRUE(getOption("Require.usePak", TRUE))) {
      l <- tempdir2("RequirePakLibForTests")
      if (!length(find.package("pak", lib.loc = l, quiet = TRUE)))
        utils::install.packages("pak", lib = l, repos = getOption("repos"),
                                quiet = TRUE)
      l
    } else {
      character(0)
    }
    lib
  }
})

setupTest <- function(verbose = getOption("Require.verbose"),
                      needRequireInNewLib = FALSE, envir = parent.frame()) {
  newLib <- tempdir3("Require_test_libs")
  if (needRequireInNewLib) {
    linkOrCopyPackageFiles("Require", fromLib = .libPaths()[1], newLib)
  }
  ## Narrow .libPaths() to c(newLib, pakLibForTests, .Library) so
  ## `installed.packages()` returns clean per-test results -- leaving the wider
  ## path in causes duplicate rows from packages like fpCompare that exist in
  ## several libs, which breaks version-pin tests.
  ##
  ## pakLibForTests() is documented above. pak has to be *on the path*, not
  ## merely loaded. An earlier version of this function force-loaded pak here
  ## and claimed that was what kept pak usable inside tests; it was not -- what
  ## actually did the work was Require's ensurePakInProjectLib() reinstalling
  ## pak into every newLib.
  ## Don't preload Require either: under covr, Require's namespace is the
  ## instrumented copy and re-loading via loadNamespace can interfere with
  ## coverage tracking.
  withr::local_libpaths(c(newLib, pakLibForTests(), .Library), .local_envir = envir)

  ## Always use temporary package cache for tests (#128):
  ## - we don't want to modify the user's cache;
  ## - user's cache may have package versions that are newer than those requested in the tests;
  withr::local_envvar("R_REQUIRE_CACHE" = tempdir2("RequireCacheForTests"), .local_envir = envir)

  Install(c("curl", "httr", "waldo")) ## needed by testthat but not installed in tmp libPath

  messageVerbose(blue(" getOption('Require.verbose'): ",
    getOption("Require.verbose")),
    verboseLevel = 0
  )
  messageVerbose(blue(" getOption('repos'): ",
    paste(getOption("repos"), collapse = comma)),
    verboseLevel = 0
  )
  return()
}

skip_if_offline2 <- function() {
  # default with testthat::skip_if_offline is apple.com
  #   which was returning true when wifi connection exists, but no internet e.g., on a plane
  skip_if_offline("github.com")
}
  omitPkgsTemporarily <- function(pkgs) {
  if (getRversion() < "4.2") {
    pkgs <- grep("mumin", pkgs, invert = TRUE, value = TRUE) # MuMIn requires R >= 4.2
    pkgs <- grep("LandR", pkgs, invert = TRUE, value = TRUE) # LandR requires R >= 4.2
    pkgs <- grep("fireSenseUtils", pkgs, invert = TRUE, value = TRUE) # LandR requires R >= 4.2
  }
  # while not on CRAN
  pkgs <- grep("^SpaDES.core", pkgs, invert = TRUE, value = TRUE) # not on CRAN
  pkgs
}

dontTryDetach <- c("devtools", "testthat", "googledrive", "rmarkdown")

dontTryDetachCurrent <- c("pak", "R6", "Rcpp", "askpass", "base64enc", "brew", "brio",
                          "bslib", "cachem", "callr", "cli", "clipr", "commonmark", "cpp11",
                          "crayon", "credentials", "curl", "desc", "devtools", "diffobj",
                          "digest", "downlit", "ellipsis", "evaluate", "fansi", "fastmap",
                          "fontawesome", "fs", "gert", "gh", "gitcreds", "glue", "highr",
                          "htmltools", "htmlwidgets", "httpuv", "httr2", "ini", "jquerylib",
                          "jsonlite", "knitr", "later", "lifecycle", "magrittr", "memoise",
                          "mime", "miniUI", "openssl", "pillar", "pkgbuild", "pkgconfig",
                          "pkgdown", "pkgload", "praise", "prettyunits", "processx", "profvis",
                          "promises", "ps", "purrr", "ragg", "rappdirs", "rcmdcheck", "rematch2",
                          "remotes", "rlang", "rmarkdown", "roxygen2", "rprojroot", "rstudioapi",
                          "rversions", "sass", "sessioninfo", "shiny", "sourcetools", "stringi",
                          "stringr", "sys", "systemfonts", "testthat", "textshaping", "tibble",
                          "tinytex", "urlchecker", "usethis", "utf8", "vctrs", "waldo",
                          "whisker", "withr", "xfun", "xml2", "xopen", "xtable", "yaml",
                          "zip", "R6", "brio", "callr", "cli", "crayon", "desc", "diffobj",
                          "digest", "evaluate", "fansi", "fs", "glue", "jsonlite", "lifecycle",
                          "magrittr", "pillar", "pkgbuild", "pkgconfig", "pkgload", "praise",
                          "processx", "ps", "rematch2", "rlang", "rprojroot", "testthat",
                          "tibble", "utf8", "vctrs", "waldo", "withr", "R6", "askpass",
                          "cli", "curl", "fansi", "fs", "gargle", "glue", "googledrive",
                          "httr", "jsonlite", "lifecycle", "magrittr", "mime", "openssl",
                          "pillar", "pkgconfig", "purrr", "rappdirs", "rlang", "sys", "tibble",
                          "utf8", "uuid", "vctrs", "withr", "R6", "base64enc", "bslib",
                          "cachem", "cli", "digest", "evaluate", "fastmap", "fontawesome",
                          "fs", "glue", "highr", "htmltools", "jquerylib", "jsonlite",
                          "knitr", "lifecycle", "memoise", "mime", "rappdirs", "rlang",
                          "rmarkdown", "sass", "tinytex", "xfun", "yaml")

dontDetach <- function() {
  deps <- pkgDep(dontTryDetach, recursive = TRUE)
  nms <- names(deps)
  dtd <- extractPkgName(c(nms, unlist(unname(deps))))

  if (!all(c("rmarkdown", "pak") %in% dtd)) {
    dtd <- dontTryDetachCurrent
  }

  dtd
}
