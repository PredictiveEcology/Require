setupTest <- function(verbose = getOption("Require.verbose"),
                      needRequireInNewLib = FALSE, envir = parent.frame()) {
  newLib <- tempdir3()
  if (needRequireInNewLib)
    linkOrCopyPackageFiles("Require", fromLib = .libPaths()[1], newLib)
  withr::local_libpaths(newLib, .local_envir = envir)
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

