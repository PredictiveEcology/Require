# puts tmpdir, tmpCache, tmpfile (can be vectorized with length >1 tmpFileExt),
#   optsAsk in this environment,
# loads and libraries indicated plus testthat,
# sets options("reproducible.ask" = FALSE) if ask = FALSE
testInit <- function(libraries, ask = FALSE, verbose = FALSE, tmpFileExt = "",
                     opts = NULL, needGoogle = FALSE) {

  optsAsk <- if (!ask)
    options("reproducible.ask" = ask)
  else
    list()

  optsVerbose <- if (verbose)
    options(reproducible.verbose = verbose)
  else
    list()

  if (missing(libraries)) libraries <- list()
  unlist(lapply(libraries, require, character.only = TRUE))
  require("testthat")
  tmpdir <- tempdir2(rndstr(1, 6))

  checkPath(tmpdir, create = TRUE)
  origDir <- setwd(tmpdir)
  tmpCache <- normPath(file.path(tmpdir, "testCache"))
  checkPath(tmpCache, create = TRUE)

  defaultOpts <- list()
  if (length(opts) > 0)
    defaultOpts[names(opts)] <- opts
  opts <- defaultOpts

  if (!is.null(opts)) {
    if (needGoogle) {
      optsGoogle <- if (utils::packageVersion("googledrive") >= "1.0.0") {
        # list(httr_oob_default = quickPlot::isRstudioServer(),
        #      httr_oauth_cache = "~/.httr-oauth")
      } else {
        list(httr_oob_default = quickPlot::isRstudioServer())
      }
      opts <- append(opts, optsGoogle)
    }
    opts <- options(opts)
  }

  if (!is.null(tmpFileExt)) {
    ranfiles <- unlist(lapply(tmpFileExt, function(x) paste0(rndstr(1, 7), ".", x)))
    tmpfile <- file.path(tmpdir, ranfiles)
    tmpfile <- gsub(pattern = "\\.\\.", tmpfile, replacement = "\\.")
    file.create(tmpfile)
    tmpfile <- normPath(tmpfile)
  }

  try(clearCache(tmpdir, ask = FALSE), silent = TRUE)
  try(clearCache(tmpCache, ask = FALSE), silent = TRUE)

  outList <- list(tmpdir = tmpdir, origDir = origDir, libs = libraries,
                  tmpCache = tmpCache, optsAsk = optsAsk,
                  optsVerbose = optsVerbose, tmpfile = tmpfile,
                  opts = opts, needGoogle = needGoogle)
  list2env(outList, envir = parent.frame())
  return(outList)
}

testOnExit <- function(testInitOut) {
  if (length(testInitOut$optsVerbose))
    options("reproducible.verbose" = testInitOut$optsVerbose[[1]])
  if (length(testInitOut$optsAsk))
    options("reproducible.ask" = testInitOut$optsAsk[[1]])
  if (length(testInitOut$opts))
    options(testInitOut$opts)
  setwd(testInitOut$origDir)
  unlink(testInitOut$tmpdir, recursive = TRUE)
  if (isTRUE(testInitOut$needGoogle)) {
    if (utils::packageVersion("googledrive") < "1.0.0")
      googledrive::drive_auth_config(active = FALSE)
  }
  unlink(testInitOut$tmpCache, recursive = TRUE, force = TRUE)
  unlink(testInitOut$tmpdir, recursive = TRUE, force = TRUE)

  if (grepl("Pq", class(getOption("reproducible.conn", NULL)))) {
    tabs <- DBI::dbListTables(conn = getOption("reproducible.conn", NULL))
    tab1 <- grep(value = TRUE, tabs, pattern =
                   paste(collapse = "_", c(basename2(dirname(testInitOut$tmpCache)),
                                           basename2(testInitOut$tmpCache))))
    tab2 <- grep(value = TRUE, tabs, pattern =
                   paste(collapse = "_", c(basename2(dirname(testInitOut$tmpdir)),
                                           basename2(testInitOut$tmpdir))))
    if (length(tab1))
      try(DBI::dbRemoveTable(conn = getOption("reproducible.conn", NULL), tab1))
    if (length(tab2))
      try(DBI::dbRemoveTable(conn = getOption("reproducible.conn", NULL), tab2))
  }

  lapply(testInitOut$libs, function(lib) {
    try(detach(paste0("package:", lib), character.only = TRUE), silent = TRUE)}
  )
}




installedAndLoaded <- function(pkgs, ip) {
  pkgName <- extractPkgName(pkgs)
  if (missing(ip)) ip <- installed.packages()
  loadedPkgs <- unlist(lapply(pkgName, isNamespaceLoaded))
  out <- unlist(Map(p = pkgName, function(p) {
    p %in% gsub("^.*:", "", search())
  }))
  loadedPkgs | out

}

dealWithWarns <- function(warn, outFromRequire) {
  pkgSkipped <- noPackage1 <- noPackage2 <- character()
  if (length(warn)) {
    warn <- strsplit(warn, "\n")
    warn <- unlist(warn)
    noPackageWarn1 <- grepl("there is no package", warn)
    noPackageWarn2 <- grepl("could not be found", warn)
    if (any(c(noPackageWarn1, noPackageWarn2))) {
      if (length(warn[noPackageWarn1])) {
        noPackage1 <- strsplit(warn[noPackageWarn1], "there is no package called")[[1]]
        noPackage1 <- gsub("^.* \'(.*)\'.*", "\\1", noPackage1)
        noPackage1 <- noPackage1[nzchar(noPackage1)]
      }
      if (length(warn[noPackageWarn2])) {
        noPackage2 <- gsub("^.* \'(.*)\' could.*$", "\\1", warn[noPackageWarn2])
        noPackage2 <- noPackage2[nzchar(noPackage2)]
      }
      pkgSkipped <- unique(c(noPackage1, noPackage2))
      test = "test1"
    } else {
      test = "test2"
    }
  } else {
    test = "test2"
  }
  skips <- grep("skipping loading", warn, value = TRUE)
  if (length(skips)) {
    skips <- gsub(".*skipping loading: ", "", skips)
    pkgSkipped <- unique(c(pkgSkipped, strsplit(skips, ", ")[[1]]))
    test = "test1"
  }

  switch(test,
         "test1" = expect_true(sum(outFromRequire) == sum(!names(outFromRequire) %in% pkgSkipped)),
         "test2" = expect_true(sum(outFromRequire) == length(outFromRequire)))
  return(invisible())
}


unloadRandom <- function(pkgs, keepDepsOf = "reproducible",
                         unloadPkgs = c("amc", "plyr", "LandR"), num = 2,
                         keepLoaded = c("git2r", "qs", "testthat", "purrr", "remotes", "sf", "fs",
                                        "memoise", "MASS", "curl", "usethis", "DBI",
                                        'httr', 'versions', 'R.oo', 'bit64', 'blob', "R.methodsS3",
                                        "googledrive", "R.utils", "RSQLite", "base64enc",
                                        "datasets", "parallel", "compiler",
                                        "RCurl", "tcltk", "dplyr")) {
  ip <- as.data.table(installed.packages())[[1]]
  loaded <- sapply(ip, isNamespaceLoaded)
  loaded <- names(loaded)[loaded]

  pkgDepsToKeep <- pkgDep(c(keepDepsOf, "devtools"))
  deps <- pkgDep(unique(extractPkgName(pkgs)))
  deps <- unique(c(names(deps), unique(unlist(deps))))
  toUnload <- setdiff(deps, unique(c(keepDepsOf, unlist(pkgDepsToKeep))))
  toUnload <- unique(c(toUnload, unloadPkgs))
  toUnload <- setdiff(toUnload, keepLoaded)

  if (length(toUnload)) {
    toUnload <- sample(toUnload, min(length(toUnload), num))
    try(lapply(toUnload, function(p) detach(paste0("package:", p), unload = TRUE)), silent = TRUE)
    suppressWarnings(try(lapply(toUnload, unloadRecursive)))
    suppressWarnings(out <- capture.output(try(remove.packages(toUnload), silent = TRUE)))

  }
  return(toUnload)
}


