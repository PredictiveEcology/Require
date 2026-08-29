## linkTestSupportInto(): put the packages testthat needs -- curl, httr, waldo
## and their dependency closure -- into `destLib` as symlinks, from wherever
## they are already installed.
##
## Into destLib, deliberately, rather than into a separate library placed on
## .libPaths(). Tests inspect the contents of their own library:
##
##   test-06:175   installedPkgs <- dir(.libPaths()[1])
##                 knownRevDeps  <- lapply(knownRevDeps, intersect, installedPkgs)
##
## so a package that is merely *reachable* rather than *present* silently
## shrinks what that test checks. Symlinking into destLib keeps dir(),
## installed.packages() and .libPaths() exactly as they were when setupTest()
## installed these for real -- the suite asserts the same things, it just does
## not spend 13s per file doing it.
##
## Symlinks, not copies: R resolves them when loading, so each namespace binds
## to the permanent library rather than to this temporary one.
##
## pak is excluded on purpose. Reachable pak stops ensurePakInProjectLib()
## reinstalling it per project lib, and that reinstall is what re-binds pak's
## namespace to a live directory; without it the namespace stays pointing at a
## library a later test deletes, and pak:::loaded_packages() then warns on a
## dead path, failing test-01, test-04 and test-12. pak keeps its own symlink
## path inside Require.
## requireIntoTestLib(): make Require -- and the deps its own DESCRIPTION
## declares -- present *in* the test library.
##
## Under devtools::test() Require is loaded from source by pkgload, so there is
## no installed copy anywhere to symlink (a developer machine may have none in
## its personal library at all). Anything that must *build* against Require --
## test-08 installs SpaDES.project, which Imports Require -- then fails with
## "dependency 'Require' is not available".
##
## Relying on the personal library via Require.cloneFrom is not a fix: it makes
## a standAlone test's outcome depend on what happens to be installed there,
## which is why test-08 passed under one R version and skipped under another on
## the same machine. So install the package *under test* once per session into
## a cache library and symlink that in -- the test library then holds the code
## actually being tested, not whatever the developer happens to have.
requireSourceLib <- local({
  cached <- NULL
  function() {
    if (!is.null(cached) && dir.exists(file.path(cached, "Require"))) return(cached)
    src <- tryCatch(find.package("Require"), error = function(e) NULL)
    ## a source checkout, not an installed package: no Meta/ directory
    if (is.null(src) || !dir.exists(file.path(src, "R")) || dir.exists(file.path(src, "Meta")))
      return(NULL)
    lib <- file.path(tempdir(), "Require_source_lib")
    dir.create(lib, showWarnings = FALSE, recursive = TRUE)
    ok <- tryCatch(system2(file.path(R.home("bin"), "R"),
                           c("CMD", "INSTALL", "--no-docs", "--no-help", "--no-byte-compile",
                             "-l", shQuote(lib), shQuote(src)),
                           stdout = FALSE, stderr = FALSE), error = function(e) 1L)
    if (!identical(as.integer(ok), 0L) || !dir.exists(file.path(lib, "Require"))) return(NULL)
    cached <<- lib
    lib
  }
})

requireIntoTestLib <- function(destLib) {
  ## Require's own non-base Imports; pak is excluded on purpose, as above
  linkTestSupportInto(destLib, pkgs = c("callr", "data.table", "processx", "sys"))
  srcLib <- requireSourceLib()
  if (!is.null(srcLib)) {
    linkTestSupportInto(destLib, pkgs = "Require", srcLibs = srcLib)
  } else {
    ## Require is installed for real (e.g. under R CMD check): link it from there
    linkTestSupportInto(destLib, pkgs = "Require")
  }
  invisible(dir.exists(file.path(destLib, "Require")))
}

linkTestSupportInto <- function(destLib, pkgs = c("curl", "httr", "waldo"),
                                srcLibs = .libPaths()) {
  srcLibs <- setdiff(srcLibs, destLib)
  ip <- tryCatch(installed.packages(lib.loc = srcLibs), error = function(e) NULL)
  if (is.null(ip) || !NROW(ip)) return(invisible(character(0)))
  closure <- pkgs
  repeat {
    deps <- unique(unlist(tools::package_dependencies(
      intersect(closure, rownames(ip)), db = ip,
      which = c("Depends", "Imports", "LinkingTo"))))
    extra <- setdiff(deps, closure)
    if (!length(extra)) break
    closure <- c(closure, extra)
  }
  linked <- character(0)
  for (pkg in intersect(closure, rownames(ip))) {
    dest <- file.path(destLib, pkg)
    if (file.exists(dest)) next
    src <- tryCatch(normalizePath(file.path(ip[pkg, "LibPath"], pkg), mustWork = FALSE),
                    error = function(e) "")
    if (nzchar(src) && dir.exists(src) &&
        isTRUE(tryCatch(file.symlink(src, dest), error = function(e) FALSE)))
      linked <- c(linked, pkg)
  }
  invisible(linked)
}

setupTest <- function(verbose = getOption("Require.verbose"),
                      needRequireInNewLib = FALSE, envir = parent.frame()) {
  newLib <- tempdir3("Require_test_libs")
  if (needRequireInNewLib) {
    requireIntoTestLib(newLib)
  }
  ## Force-load pak BEFORE narrowing .libPaths(): once a namespace is loaded,
  ## R remembers where it came from even if the lib is no longer on .libPaths().
  ## This lets us narrow the path to c(newLib, .Library) so `installed.packages()`
  ## returns clean per-test results, while still being able to call pak inside
  ## tests. Replacing the path without this preload hides pak under R CMD check
  ## (it lives in a temporary RLIBS dir); leaving the wider path in causes
  ## duplicate rows from packages like fpCompare that exist in multiple libs,
  ## which break version-pin tests.
  ## Don't preload Require: under covr, Require's namespace is the instrumented
  ## copy and re-loading via loadNamespace can interfere with coverage tracking.
  tryCatch(loadNamespace("pak"), error = function(e) NULL)
  ## Populate newLib before narrowing, while the source libraries are still
  ## on .libPaths() to link from.
  linkTestSupportInto(newLib)
  withr::local_libpaths(c(newLib, .Library), .local_envir = envir)

  ## Always use temporary package cache for tests (#128):
  ## - we don't want to modify the user's cache;
  ## - user's cache may have package versions that are newer than those requested in the tests;
  withr::local_envvar("R_REQUIRE_CACHE" = tempdir2("RequireCacheForTests"), .local_envir = envir)

  ## testthat needs these inside the narrowed .libPaths(). Install() places a
  ## package in libPaths[1] even when it is already reachable further down the
  ## path -- measured: with curl visible via another library entry, Install()
  ## still installed it, 9.21s vs 9.18s without. So the saving is not in making
  ## the call cheap, it is in not making it: skip when every one of them is
  ## already loadable from the current path.
  .testthatSupport <- c("curl", "httr", "waldo")
  .haveSupport <- vapply(.testthatSupport, function(p)
    length(suppressWarnings(find.package(p, lib.loc = .libPaths(), quiet = TRUE))) > 0,
    logical(1))
  if (!all(.haveSupport)) Install(.testthatSupport[!.haveSupport])

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
