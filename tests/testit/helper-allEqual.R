


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


