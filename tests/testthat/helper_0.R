## testSupportLib(): one library holding the packages testthat needs inside a
## narrowed .libPaths() -- curl, httr, waldo and their dependency closure --
## symlinked from wherever they are already installed. Built at most once per
## session.
##
## setupTest() narrows .libPaths() to a fresh per-file temp lib, so those
## packages are absent and `Install(c("curl", "httr", "waldo"))` installed 14
## packages into every one of them. Measured on landr: 4.1s per test file, the
## single largest per-file cost -- larger than the pak install everyone
## (including me) assumed was the problem.
##
## pak is deliberately NOT put here. If pak is reachable on .libPaths(),
## ensurePakInProjectLib() stops installing it into each project lib, and it is
## that install which re-binds pak's namespace to a live directory each time a
## test moves to a new library. Without it the namespace stays pointing at a
## library a later test deletes, and every subsequent pak:::loaded_packages()
## warns on the dead path -- which fails test-01, test-04 and test-12. Four
## attempts at "make pak reachable" all died there; pak keeps its per-lib
## symlink instead.
##
## Symlinks, not copies: R resolves them when loading, so each namespace binds
## to the permanent library rather than to this temporary one.
testSupportLib <- local({
  lib <- NULL
  function(pkgs = c("curl", "httr", "waldo")) {
    if (!is.null(lib) && dir.exists(lib)) return(lib)
    l <- file.path(tempdir(), "RequireTestSupportLib")
    dir.create(l, recursive = TRUE, showWarnings = FALSE)
    srcLibs <- setdiff(.libPaths(), l)
    ip <- tryCatch(installed.packages(lib.loc = srcLibs), error = function(e) NULL)
    if (!is.null(ip) && NROW(ip)) {
      closure <- pkgs
      repeat {
        deps <- unique(unlist(tools::package_dependencies(
          intersect(closure, rownames(ip)), db = ip,
          which = c("Depends", "Imports", "LinkingTo"))))
        extra <- setdiff(deps, closure)
        if (!length(extra)) break
        closure <- c(closure, extra)
      }
      for (pkg in intersect(closure, rownames(ip))) {
        dest <- file.path(l, pkg)
        if (file.exists(dest)) next
        src <- tryCatch(normalizePath(file.path(ip[pkg, "LibPath"], pkg),
                                      mustWork = FALSE),
                        error = function(e) "")
        if (nzchar(src) && dir.exists(src))
          try(file.symlink(src, dest), silent = TRUE)
      }
    }
    lib <<- l
    l
  }
})

setupTest <- function(verbose = getOption("Require.verbose"),
                      needRequireInNewLib = FALSE, envir = parent.frame()) {
  newLib <- tempdir3("Require_test_libs")
  if (needRequireInNewLib) {
    linkOrCopyPackageFiles("Require", fromLib = .libPaths()[1], newLib)
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
  withr::local_libpaths(c(newLib, testSupportLib(), .Library), .local_envir = envir)

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
