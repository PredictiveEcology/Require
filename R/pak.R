utils::globalVariables(c(
  "..keepCols", "ref", "op", "package"
))


.txtFailedToBuildSrcPkg <- "Failed to build source package"
.txtCantFindPackage <- "Can't find package called "

pakErrorHandling <- function(err, pkg, packages, verbose = getOption("Require.verbose")) {

  grp <- c(.txtCntInstllDep, .txtFailedToBuildSrcPkg, .txtConflictsWith, .txtCantFindPackage,
           .txtMissingValueWhereTFNeeded, .txtCldNotSlvPkgDeps, .txtFailedToDLFrom, .txtPakNoPkgCalledPak,
           .txtUnknownArchiveType)
  spl <- c(" |\\)", "\033\\[..{0,1}m", "\033\\[..{0,1}m| |@", " |\\. ", "NULL", "NULL", "NULL", "NULL", "NULL")
  pat <- c("dependency", grp[2], "with", "called", "NULL", "NULL", "NULL", "NULL", "NULL")
  for (i in seq_along(grp)) {
    splitStr <- strsplit(err, split = "\n")[[1]]
    a <- grep(grp[i], splitStr, value = TRUE)
    if (length(a)) {
      a1 <- gsub("\\.$", "", a)

      b <- strsplit(a1, split = spl[i])
      whDeps <- sapply(b, grep, pattern = pat[i])

      pkg2 <- gsub("@.+$", "", pkg)

      if (grp[i] == .txtUnknownArchiveType) {
        # redo
        splitStr
        whDeps <- grep(grp[i], splitStr)
        pkgLong <- splitStr[whDeps-1]
        pkgLong <- strsplit(pkgLong, spl[2])[[1]]
        pkgLong <- basename(pkgLong)
        filename <- pkgLong[nchar(pkgLong) > 2]
        pkg2 <- extractPkgName(filenames = basename(filename))
        whRm <- grep(pkg2, packages)
        packages2 <- pakCacheDeleteTryAgain(pkg2 = pkg2, packages = packages, whRm = whRm)
        if (!identical(length(packages2), length(packages)))
          packages <- packages[-whRm]
      }
      if (length(pkg2) > 1) {
        d <- Map(x = b, whDep = whDeps, function(x, whDep) x[[whDep + 1]])
        pkg2 <- gsub("@.+$", "", d)
      }
      pkgNoVersion <- trimVersionNumber(pkg2)

      vers <- tryCatch(Map(x = b, whDep = whDeps, function(x, whDep) x[[whDep + 3]]),
                       error = function(x) "")
      whRm <- unlist(unname(lapply(
        paste0("^", pkgNoVersion, ".*", vers, "|/", pkgNoVersion, ".*", vers), grep, x = pkg)))

      if (grp[i] == .txtMissingValueWhereTFNeeded) {
        packages <- pakGetArchive(pkgNoVersion, packages = packages, whRm = whRm)
        break
      }
      if (grp[i] == .txtFailedToDLFrom) {
        browser()
      }

      if (grp[i] == .txtCntInstllDep) {
        whRmAll <- integer()
        for (j in seq_along(pkgNoVersion)) {
          if (isGH(pkgNoVersion[j])) { # "achubaty/fpCompare (>=2.0.0)"
            if (is.na(pkg[whRm[j]])) browser()
            isOK <- pakCheckGHversionOK(pkg[whRm[j]])
            # pkgDT <- toPkgDTFull(pkg)
            # dl <- pak::pkg_download(trimVersionNumber(pkg), dest_dir = tempdir2())
            # vers <- extractVersionNumber(filenames = basename(dl$fulltarget))
            # isOK <- compareVersion2(vers, versionSpec = pkgDT$versionSpec, inequality = pkgDT$inequality)
            if (isOK %in% FALSE)
              whRmAll <- c(whRmAll, whRm[j])
              # packages <- packages[-whRm[j]]
            next
          }
          packages2 <- pakGetArchive(pkgNoVersion[j], packages = packages, whRm = whRm[j])
          if (!identical(length(packages2), length(packages)))
            whRmAll <- c(whRmAll, whRm[j])

        }
        packages <- packages[-whRmAll]
        break
      }

      dups <- Map(x = b, function(x) duplicated(x))


      if (sum(unlist(dups)) >= 2 || grp[i] == .txtCldNotSlvPkgDeps) {
        # likely a repository that has a 4th version number element,
        #  e.g., NetLogoR 1.0.5.9001 on e.g., predictiveecology.r-universe.dev
        repoToUse <- unlist(whIsOfficialCRANrepo(currentRepos = getOption("repos")))
        packages <- pakGetArchive(pkgNoVersion, packages = packages, whRm = whRm)
        # options(repos = repoToUse)
        break
      }
      pkgPossOther <- extractPkgName(filenames = basename(pkg))
      if (identical(pkg2, pkgPossOther)) {
        # vers <- tryCatch(Map(x = b, whDep = whDeps, function(x, whDep) x[[whDep + 3]]),
        #                  error = function(x) "")
        # whRm <- unlist(unname(lapply(paste0("^", pkg2, ".*", vers), grep, x = pkg)))
        if (length(whRm) > 0) {
          if (grp[i] == .txtCantFindPackage) {
            # This is the case when a package is archived
            packages2 <- pakGetArchive(pkg2, packages, whRm)
            if (identical(packages2, packages)) { # doesn't exist
              packages <- packages[-whRm]
              warning(err)
              break
            }
            messageVerbose(packages2, " may be archived from CRAN; checking archives... ",
                           verbose = verbose)
            packages <- packages2
            break
          } else {
            if (grp[i] == .txtFailedToBuildSrcPkg) {
              packages <- pakCacheDeleteTryAgain(pkg2, packages, whRm)
              break
            }
            if (grp[i] == .txtPakNoPkgCalledPak) {
              stop("\nTry running: \npak::meta_clean()")
              # stop(err)
            }
            packages <- packages[-whRm]
            break
          }
        } else {
          stop(err)
        }
      } else {
        packages <- pakCacheDeleteTryAgain(pkg2, packages, whRm)
        break
      }
    }
  }
  packages
}

pakPkgSetup <- function(pkgs, doDeps) {

  # rm spaces
  pkgs <- gsub(" {0,3}(\\()(..{0,1}) {0,4}(.+)(\\))", " \\1\\2\\3\\4", pkgs)

  if (TRUE) {
    deps <- list()
    deps <- pkgDep(pkgs, which = doDeps) # |> Cache()

    depsFlat <- unlist(unname(deps))
    depsFlat <- unique(depsFlat)
    pkgDT <- toPkgDTFull(depsFlat)
    if (FALSE) {
      setorderv(pkgDT, "Package")
      a <- pkgDT[, any(any(grep("==", inequality)) & any(grep(">=", inequality))), by = "Package"][V1 %in% TRUE]
      a <- pkgDT[a, on = "Package"]
      a <- a[!is.na(inequality)]
    }
    pkgDT <- trimRedundancies(pkgDT)
    pkgs <- pkgDT$packageFullName

  }
  # Convert equals to @
  whLT <- grep("<", pkgs)
  whEquals <- whEquals(pkgs) # grep("==", pkgs)
  isGH <- isGH(pkgs) # grepl("^[[:alpha:]]+/.+", pkgs)

  if (length(whLT)) {
    vers <- Map(pkg = pkgs[whLT], isGH = isGH[whLT], function(pkg, isGH) {
      pkgDT <- toPkgDTFull(pkg)
      if (isGH) {
        his <- pak::pkg_deps(trimVersionNumber(pkg))
        his <- his[his$package %in% extractPkgName(pkg), ]
        setnames(his, old = "version", new = "Version")
      } else {
        his <- pak::pkg_history(trimVersionNumber(pkg))
      }
      versOK <- compareVersion2(his$Version, pkgDT$versionSpec, pkgDT$inequality)
      if (all(versOK %in% FALSE)) {
        warning(msgPleaseChangeRqdVersion(trimVersionNumber(pkg), ">=", names(versOK)))
        NA
      } else {
        keep <- max(which(versOK))
        his[keep, ]$Version
      }

    })
    NAvers <- is.na(vers)
    if (any(NAvers)) {# means none available
      pkgs <- pkgs[-whLT[NAvers]]
      whLT <- whLT[!NAvers]
      # need redo these
      whEquals <- whEquals(pkgs) # grep("==", pkgs)
      isGH <- isGH(pkgs) # grepl("^[[:alpha:]]+/.+", pkgs)
    } else {
      pkgs[whLT] <- gsub("\\(<={0,1}(.+)\\)", paste0("(==", vers, ")"), pkgs[whLT])
    }
  }

  ind <- seq_along(pkgs)

  whEquals <- sort(c(whEquals, whLT))

  whGH <- which(isGH)

  isGT <- isGT(pkgs) # grepl(">", pkgs)
  whGT <- which(isGT)

  whHEAD <- grep("\\(HEAD\\)", pkgs)

  whAlreadyColoned <- grep("::", pkgs)

  whNormal <- ind[-sort(c(whEquals, whGT, whLT, whHEAD, whGH, whAlreadyColoned))]

  if (length(whEquals))
    pkgs[whEquals] <- equalsToAt(pkgs[whEquals])
    # pkgs[whEquals] <- gsub(" {0,3}\\(== {0,4}(.+)\\)", "@\\1", pkgs[whEquals])
  if (length(whLT))
    pkgs[whLT] <- lessThanToAt(pkgs[whLT])
    # pkgs[whLT] <- gsub(" {0,3}\\(<= {0,4}(.+)\\)", "@\\1", pkgs[whLT])
  if (length(whHEAD))
    pkgs[whHEAD] <- HEADtoNone(pkgs[whHEAD])
    # pkgs[whHEAD] <- gsub(" {0,3}\\(HEAD\\)", "", pkgs[whHEAD])

  if (length(whNormal))
    pkgs[whNormal] <- paste0("any::", pkgs[whNormal])

  pkgsForDESCRIPTIONFile <- if (length(whGT)) pkgs[whGT] else character()
  pkgsForDirect <- if (length(whGT)) pkgs[-whGT] else pkgs

  list(DESC = pkgsForDESCRIPTIONFile, direct = pkgsForDirect)
}


pakRequire <- function(packages, libPaths, doDeps, upgrade, verbose, packagesOrig) {
  if (!requireNamespace("pak")) stop("Please install pak")

  packages <- unique(packages)
  packages <- packages[!extractPkgName(packages) %in% .basePkgs]

  pkgs <- list()
  for (i in 1:15) {
  # while(!identical(packages, pkgs) ) {
    pkgs <- packages
    if (length(pkgs)) {
      pkgsList <- pakPkgSetup(pkgs, doDeps = doDeps)
      td3 <- tempdir3()
      on.exit({unlink(dirname(td3))}, add = TRUE)
      # if (any(grepl("quickPlot", pkgsList$DESC))) browser()
      dfile <- DESCRIPTIONfileFromModule(verbose = -2,
                                         packageFolderName = td3,
                                         .txtDummyPackage,
                                         md = list(name = .txtDummyPackage, description = .txtDummyPackage,
                                                   version = list(a = 1, dummy = "0.0.1"),
                                                   authors =
                                                     'person(given = "Eliot",
                             family = "McIntire",
                             role = c("aut", "cre"),
                             email = "eliot.mcintire@canada.ca",
                             comment = c(ORCID = "0000-0002-6914-8316"))'
                                         ),
                                         deps = pkgsList$DESC,
                                         hasNamespaceFile = FALSE)
      err <- try(outs <- pak::pak(c(
        paste0("deps::", td3),
        pkgsList$direct
      ), lib = libPaths[1], ask = FALSE,

      # already done in pakPkgSetup # doDeps,
      # FALSE doesn't work when `deps::` is used
      dependencies = doDeps,
      upgrade = upgrade),
      silent = TRUE)

      if (!is(err, "try-error"))
        break

      # deal with errors
      packages <- pakErrorHandling(err, pkgs, packages, verbose = verbose)
      if (length(packages) == 0)
        stop(err)

    } else {
      outs <- list()
      break
    }
  }

  pkgDT <- try(as.data.table(outs))
  pkgDT <- pkgDT[package != paste0(.txtDummyPackage, "-deps")]
  if (is(pkgDT, "try-error")) browser()
  setnames(pkgDT, old = c("package", "status"), new = c("Package", "installResult"))
  loadSequence <- match(extractPkgName(packagesOrig), pkgDT$Package)
  loadSequence <- na.omit(loadSequence)
  pkgDT[loadSequence, loadOrder := seq_along(loadSequence)]
  # if it didn't fail, then it is OK
  pkgDT[is.na(pkgDT$installed), needInstall := .txtInstall]
  set(pkgDT, NULL, c("installedVersionOK", "availableVersionOK"), TRUE)
  pkgDT[, packageFullName:= Package]
}

whEquals <- function(pkgs) {
  grep("==", pkgs)
}

isGH <- function(pkgs) {
  grepl("^[[:alpha:]]+/.+", pkgs)
}

pakPkgDep <- function(packages, which, simplify, includeSelf, includeBase,
                      keepVersionNumber, verbose = getOption("Require.verbose")) {
  if (!requireNamespace("pak")) stop("Please install pak")

  deps <- list()

  packagesOrig <- packages

  useCache <- FALSE
  if (useCache) {
    depsList <- get0("depsList", envir = pakEnv())
    haveAlready <- NULL
    if (!is.null(depsList)) {
      haveAlready <- match(packages, names(depsList)) |> na.omit()
      packages <- packages[-haveAlready]
    }

  }

  deps <- Map(pkg1 = packages, function(pkg1) {
    reposOrig <- getOption("repos")
    on.exit({
      options(repos = reposOrig)
    }, add = TRUE)
    pkgOrig <- pkg1
    pkg <- pkg1
    valExtra <- list()
    wh <- ifelse(any(grepl("suggests", tolower(unlist(which)))), TRUE,
                 ifelse (length(which), NA, FALSE))

    pkgDone <- character()
    i <- 0
    while(length(pkg1) > 0) {
      i <- i + 1 # counter
      pkg <- pkg1[1]
      # for (pkg in pkg1) {
      #for (i in 1:2) {
      # for (pkg in pkg1) { # will only be longer than 1 if added with pakErrorHandling below
      isGH <- isGH(pkg) # grepl("^[[:alpha:]]+/.+", pkgs)
      notGH <- isGH %in% FALSE
      if (any(notGH)) {
        pkg[notGH] <- equalsToAt(pkg[notGH])
        pkg2 <- lessThanToAt(pkg[notGH]) # can remove a pkg if not an option
      } else {
        pkg <- equalsToAt(pkg)
        pkg2 <- lessThanToAt(pkg) # can remove a pkg if not an option
      }
      if (length(pkg2) == 0) {
        pkg1 <- pkg2
        val <- character()
        break
      }
      if (any(notGH)) pkg[notGH] <- pkg2

      pkg <- HEADtoNone(pkg)
      isGT <- isGT(pkg) # grep(">", pkgs)
      needRm <- isGH | isGT
      if (any(needRm))
        pkg[needRm] <- trimVersionNumber(pkg[needRm])

      # give up for archives of archives
      if (i > 1 && pkg %in% pkgDone) wh <- FALSE

      val <- try(pak::pkg_deps(pkg, dependencies = wh), silent = TRUE)
      if (is(val, "try-error")) {
        pkgDone <- unique(c(pkg, pkgDone))
        pkgOrig2 <- pkg
        pkg <- pakErrorHandling(val, pkg, pkg, verbose = verbose)
        if (length(pkg)) {
          if (length(pkg) > length(pkgOrig2)) {
            pkg1 <- pkg
            # break
            # added a package dep
          } else {
            pkg1[1] <- pkg
          }
        } else { # fail because of various reasons
          pkg1 <- pkg
          val <- character()
        }
      } else {
        if (length(pkg1) > 1) {
          valExtra <- append(list(val), valExtra)
        }
        pkg1 <- pkg1[-1]
        break
      }
    }
    if (length(valExtra)) {
      if (!requireNamespace("tibble")) stop("Please install tibble")
      valExtra <- do.call(rbind, valExtra)
      newDeps <- append(val$deps, valExtra$deps)
      newDeps <- do.call(rbind, newDeps)
      data.table::setDT(newDeps)
      newDeps <- newDeps[newDeps[, .I[max(version) == version], by = "ref"]$V1,]
      whOverride <- which(newDeps$ref %in% valExtra$package)
      newDeps <- newDeps[whOverride, ref := valExtra$ref]
      newDeps <- tibble::as_tibble(newDeps)
      val$deps <- list(newDeps)
    }

    val

  }
  )

  if (useCache) {

    if (!is.null(haveAlready)) {
      newList <- Map(x = packagesOrig, function(x) list())
      newList[names(depsList)] <- depsList
      if (NROW(deps))
        newList[-haveAlready] <- deps
      deps <- newList
    }
    assign("depsList", deps, envir = pakEnv())
  }

  hasDeps <- lengths(deps) > 0
  if (simplify %in% TRUE && any(hasDeps)) {
    deps[hasDeps] <- Map(dep = deps[hasDeps], nam = names(deps)[hasDeps], function(dep, nam) {
      dd <- try(dep$deps)
      if (is(dd, "try-error")) browser()
      dep$deps <- Map(innerDep = dep$deps, outerPkg = dep$package, function(innerDep, outerPkg) {
        if (!nam %in% outerPkg || !any(tolower(unlist(which)) == "suggests")) {
          innerDep[tolower(innerDep$type) %in%
                     setdiff(tolower(unlist(which)), "suggests"),]
        } else {
          innerDep
        }})
      dep
    })
    deps[hasDeps] <- Map(dep = deps[hasDeps], packFullName = packagesOrig[hasDeps], function(dep, packFullName) {
      rr <- rbindlist(dep$deps) # only gets 1st order dependencies; still need self
      rr <- unique(rr)
      # rr <- rr[tolower(type) %in%  setdiff(tolower(unlist(which)), "suggests")]
      rr <- packageFullNameFromPkgVers(rr)
      hasWeirdSource <- grep("^.+::", rr$packageFullName)
      if (any(hasWeirdSource)) {
        rr[hasWeirdSource, packageFullName := trimVersionNumber(packageFullName)]
      }
      # rr[, packageFullName := paste0(ref, ifelse(nzchar(version), paste0(" (", op, " ", version, ")"), ""))]

      if (includeSelf) {
        selfPkgs <- toPkgDTFull(packFullName)
        setnames(selfPkgs, old = c("Package", "versionSpec", "inequality"),
                 new = c("package", "version", "op"))
        selfPkgs[is.na(version), `:=`(version = "", op = "")]
        selfPkgs[, type := "Depends"]
        selfPkgs[, ref := trimVersionNumber(packageFullName)]
        keepCols <- colnames(rr)
        rr <- rbindlist(list(rr, selfPkgs[, ..keepCols]))
        pkg <- extractPkgName(packFullName)
        if (!identical(dep$ref, pkg)) {
          replacement <- if (grepl("^url", dep$ref[dep$package %in% pkg])) {
            dep$ref[dep$package %in% pkg]
          } else {
            packFullName
          }
          rr[package %in% pkg, packageFullName := replacement]
        }

      }

      setorderv(rr, c("package", "op"), order = c(1L, -1L))
      rr <- unique(rr, by = "package")

      lcPackage <- "package"
      ucPackage <- "Package"
      setnames(rr, old = lcPackage, new = ucPackage)
      rr <- rmRifInPackageCol(rr)
      setnames(rr, old = ucPackage, new = lcPackage)
      if (!includeBase) {
        rr <- rr[!package %in% .basePkgs]
      }

      deps <- rr$packageFullName

      if (keepVersionNumber %in% FALSE)
        deps <- trimVersionNumber(deps)
      deps
    })

  }
  deps
}

packageFullNameFromPkgVers <- function(rr) {
  rr[, packageFullName := paste0(ref, ifelse(nzchar(version), paste0(" (", op, " ", version, ")"), ""))]
}


DESCRIPTIONfileFromModule <- function(module, md, deps, hasNamespaceFile, NAMESPACEFile,
                                      filePathImportSpadesCore = file.path(".", fileext = ".R"),
                                      packageFolderName = module, verbose = getOption("Require.verbose")) {
  d <- list()
  # d$Package <- .moduleNameNoUnderscore(module)
  d$Package <- module
  d$Type <- "Package"

  d$Title <- md$name
  d$Description <- md$description
  d$Version <- as.character(eval(md$version[[2]]))
  d$Date <- Sys.Date()
  d$Authors <- md$authors
  d$Authors <- c(paste0("  ", format(d$Authors)[1]), format(d$Authors)[-1])


  # hasSC <- grepl("SpaDES.core", deps)
  # if (all(!hasSC))
  #   deps <- c("SpaDES.core", deps)

  d$Imports <- Require::extractPkgName(deps)
  versionNumb <- Require::extractVersionNumber(deps)
  needRemotes <- which(!is.na(Require::extractPkgGitHub(deps)))
  d$Remotes <- Require::trimVersionNumber(deps[needRemotes])

  hasVersionNumb <- !is.na(versionNumb)
  inequality <- paste0("(", gsub("(.+)\\((.+)\\)", "\\2", deps[hasVersionNumb]), ")")
  missingSpace <- !grepl("[[:space:]]", inequality)
  if (any(missingSpace))
    inequality[missingSpace] <- gsub("([=><]+)", "\\1 ", inequality[missingSpace])

  namespaceImports <- d$Imports
  # Create "import all" for each of the packages, unless it is already in an @importFrom
  if (hasNamespaceFile) {
    nsTxt <- readLines(NAMESPACEFile)
    hasImportFrom <- grepl("importFrom", nsTxt)
    if (any(hasImportFrom)) {
      pkgsNotNeeded <- unique(gsub(".+\\((.+)\\,.+\\)", "\\1", nsTxt[hasImportFrom]))
      namespaceImports <- grep(paste(pkgsNotNeeded, collapse = "|"),
                               namespaceImports, invert = TRUE, value = TRUE)
    }
  }

  # cat(paste0("#' @import ", namespaceImports, "\nNULL\n"), sep = "\n",
  #     file = filePathImportSpadesCore, fill = TRUE)

  d$Imports[hasVersionNumb] <- paste(d$Imports[hasVersionNumb], inequality)

  dFile <- filenameFromFunction(packageFolderName, "DESCRIPTION", fileExt = "")
  if (!dir.exists(packageFolderName))
    dir.create(packageFolderName, recursive = TRUE, showWarnings = FALSE)
  origDESCtxt <- if (file.exists(dFile)) read.dcf(dFile) else character()

  cat(paste("Package:", d$Package), file = dFile, sep = "\n")
  cat(paste("Type:", d$Type), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Title:", d$Title), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Version:", d$Version), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Description:", paste(d$Description, collapse = " ")), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Date:", d$Date), file = dFile, sep = "\n", append = TRUE)
  cat(c("Authors@R:  ", format(d$Authors)), file = dFile, sep = "\n", append = TRUE)

  if (length(d$Imports) || length(origDESCtxt))
    mergeField(origDESCtxt = origDESCtxt, field = d$Imports, fieldName = "Imports", dFile)

  # suggs <- c('knitr', 'rmarkdown', 'testthat', 'withr', 'roxygen2')
  # if (length(suggs) || length(origDESCtxt))
  #   mergeField(origDESCtxt = origDESCtxt, field = suggs, fieldName = "Suggests", dFile)

  if (length(d$Remotes) || length(origDESCtxt))
    mergeField(origDESCtxt = origDESCtxt, field = d$Remotes, fieldName = "Remotes", dFile)

  cat("Encoding: UTF-8", sep = "\n", file = dFile, append = TRUE)
  cat("License: GPL-3", sep = "\n", file = dFile, append = TRUE)
  cat("VignetteBuilder: knitr, rmarkdown", sep = "\n", file = dFile, append = TRUE)
  cat("ByteCompile: yes", sep = "\n", file = dFile, append = TRUE)
  cat("Roxygen: list(markdown = TRUE)", sep = "\n", file = dFile, append = TRUE)
  # cat(paste0("RoxygenNote: ", as.character(packageVersion("roxygen2"))), sep = "\n",
  #     file = dFile, append = TRUE)


  messageVerbose("New/updated DESCRIPTION file is: ", dFile, verbose = verbose)
  return(dFile)
}


filenameFromFunction <- function(packageFolderName, fn = "", subFolder = "", fileExt = ".R") {
  normPath(file.path(packageFolderName, subFolder, paste0(gsub("\\.", "", fn), fileExt)))
}

mergeField <- function(origDESCtxt, field, dFile, fieldName = "Imports") {
  fieldVals <- character()
  if (fieldName %in% colnames(origDESCtxt))
    fieldVals <- strsplit(origDESCtxt[, fieldName], split = ",+\n")[[1]]
  if (length(field)) {
    field <- trimRedundancies(unique(c(field, fieldVals)))
  }
  cat(c(paste0(fieldName, ":"), paste("   ", sort(field$packageFullName), collapse = ",\n")),
      sep = "\n", file = dFile, append = TRUE)
}



equalsToAt <- function(pkgs) {
  gsub(" {0,3}\\(== {0,4}(.+)\\)", "@\\1", pkgs)
}

lessThanToAt <- function(pkgs) {
  hasLT <- grepl("<", pkgs) # only < not <=
  if (any(hasLT %in% TRUE)) {
    #trulyLT <- grepl("<[^=]", pkgs) # only < not <=
    #whTrulyLT <- which(trulyLT)
    #val <- character(length(pkgs))
    # if (any(trulyLT)) {
    pkgDT <- toPkgDTFull(pkgs[hasLT])#[whTrulyLT])
    vers <- Map(pkg = pkgDT$packageFullName, function(pkg) {

      isGH <- isGH(pkg)
      if (any(isGH)) {
        isOK <- pakCheckGHversionOK(pkg)
        notOK <- isOK %in% FALSE
        if (any(notOK)) {
          pkg2 <- pkg[!notOK]
          if (length(pkg2) == 0)
            return(character())
          pkg[!notOK] <- pkg2
        }
      }

      # vers <- Map(pkg = pkgs[whTrulyLT], function(pkg) {
      pkgNoVersion <- trimVersionNumber(pkg)
      his <- try(pak::pkg_history(pkgNoVersion))
      if (is(his, "try-error")) browser()
      whOK <- compareVersion2(his$Version, pkgDT$versionSpec, pkgDT$inequality)
      if (all(whOK %in% FALSE)) {
        warning(msgPleaseChangeRqdVersion(pkgNoVersion, ineq = ">=", newVersion = tail(his$Version, 1)))
      }
      vers <- tail(his$Version[whOK], 1)
    })
    noneAvail <- lengths(vers) == 0
    if (any(noneAvail)) {
      pkgDT <- pkgDT[!noneAvail]
      vers <- vers[!noneAvail]
      hasLT <- hasLT[!noneAvail]
    }
    if (any(noneAvail %in% FALSE)) {
      set(pkgDT, NULL, "Version", vers)
      # set(pkgDT, whTrulyLT, "Version", vers)
      set(pkgDT, NULL, "packageFullName", paste0(pkgDT$Package, "@", pkgDT$Version))
      pkgs[hasLT] <- pkgDT$packageFullName
    } else {
      pkgs <- pkgDT$packageFullName
    }
    # val[trulyLT] <- pkgDT$packageFullName
    # }
    # LTorET <- trulyLT %in% FALSE
    # if (any(LTorET)) {
    #   val[LTorET] <- gsub(" {0,3}\\(<= {0,4}(.+)\\)", "@\\1", pkgs[LTorET])
    # }
  }
  pkgs
}

HEADtoNone <- function(pkgs) {
  gsub(" {0,3}\\(HEAD\\)", "", pkgs)
}

isGT <- function(pkgs) grepl(">", pkgs)

pakGetArchive <- function(pkg2, packages = pkg2, whRm = seq_along(packages)) {
  pkg2Orig <- pkg2
  pkgNoVer <- trimVersionNumber(pkg2)
  hasVer <- pkgNoVer != packages[whRm]

  isCRAN <- unlist(whIsOfficialCRANrepo(getOption("repos"), srcPackageURLOnCRAN))
  his <- try(tail(pak::pkg_history(pkgNoVer), 1), silent = TRUE)
  if (any(pkgNoVer != packages[whRm])) {
    vers <- extractVersionNumber(packages[whRm][hasVer])
    ineq <- "=="
    hasOKVersion <- compareVersion2(his$Version, versionSpec = vers, ineq)
    if (hasOKVersion %in% FALSE) {
      warning(msgPleaseChangeRqdVersion(trimVersionNumber(pkgNoVer), ">=", his$Version))
      packages <- packages[-whRm]
      return(packages)
    }
  }
  if (!is(his, "try-error") || grep(pattern = isCRAN, getOption("repos")) != 1) {
    # opt <- options(repos = isCRAN)
    # on.exit(options(opt))
    if (isWindows() || isMacOSX()) {
      type <- "binary"
    }
    ap <- available.packagesWithCallingHandlers(isCRAN, type = type) |> as.data.table()
    onCurrent <- ap[Package %in% pkg2]
    if (NROW(onCurrent)) {
      fileext <- if (identical(type, "binary")) ".zip" else ".tar.gz"
      pth <- file.path(paste0(onCurrent$Package, "_", onCurrent$Version, fileext))
    } else {
      type <- "source"
      pth <- file.path("Archive", his$Package, paste0(his$Package, "_", his$Version, ".tar.gz"))
    }
    if (isTRUE(!startsWith(isCRAN, "https"))) isCRAN <- paste0("https://", isCRAN)
    pth <- paste0("url::",file.path(contrib.url(isCRAN, type = type), pth))
    packages[whRm] <- pth
  }

  # his <- try(tail(pak::pkg_history(pkgNoVer), 1), silent = TRUE)
  # if (!is(his, "try-error")) {
  #   pth <- file.path("Archive", his$Package, paste0(his$Package, "_", his$Version, ".tar.gz"))
  #   if (isTRUE(!startsWith(isCRAN, "https"))) isCRAN <- paste0("https://", isCRAN)
  #   pth <- paste0("url::",file.path(contrib.url(isCRAN), pth))
  #   packages[whRm] <- pth
  # } else {
  #   messageCantInstallNoVersion(pkg2)
  # }
  packages
}


.txtDummyPackage <- "dummy"


pakCheckGHversionOK <- function(pkg) {
  pkgDT <- toPkgDTFull(pkg)
  dl <- try(pak::pkg_download(trimVersionNumber(pkg), dest_dir = tempdir2()))
  if (is(dl, "try-error")) browser()
  vers <- extractVersionNumber(filenames = basename(dl$fulltarget))
  isOK <- compareVersion2(vers, versionSpec = pkgDT$versionSpec, inequality = pkgDT$inequality)
  isOK
}


pakCacheDeleteTryAgain <- function(pkg2, packages, whRm) {
  prevFail <- get0("failedPkgs", envir = pakEnv())
  pkg3 <- extractPkgName(pkg2)
  if (pkg3 %in% prevFail) {
    nowFails <- setdiff(pkg3, prevFail)
    assign("failedPkgs", nowFails, envir = pakEnv())
    packages <- packages[-whRm]
  } else {
    pak::cache_delete(package = pkg3)
    nowFails <- c(prevFail, pkg3)
    assign("failedPkgs", nowFails, envir = pakEnv())
  }
  packages
}
