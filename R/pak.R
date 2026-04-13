utils::globalVariables(c(
  "..keepCols", "op", "package", "ref"
))

.txtFailedToBuildSrcPkg <- "Failed to build source package"
.txtCantFindPackage <- "Can't find package called "

# Wrap a pak call to honour Require's verbose level.
# pak produces two kinds of output:
#   (1) Progress/spinner — controlled by options(pkg.show_progress).
#       pak's remote() passes pkg.show_progress = is_verbose() to its subprocess,
#       where is_verbose() reads options(pkg.show_progress) (falling back to
#       interactive()). Setting this option before calling pak is sufficient.
#   (2) cli messages forwarded from the subprocess as message() conditions
#       (class "callr_message"). suppressMessages() catches these.
#
# Three levels:
#   verbose >= 1  : full output  — progress bars + messages (pak defaults)
#   verbose == 0  : messages only — no progress spinner, cli messages still shown
#   verbose <= -1 : silent       — no progress, no messages
#
# Two suppression mechanisms are needed for verbose <= -1:
#   (1) options(pkg.show_progress = FALSE) — tells pak's subprocess not to render
#       the animated progress spinner.
#   (2) suppressMessages() — catches cli_message conditions forwarded from the
#       subprocess as message() conditions (e.g. "Installing X packages...").
#   (3) capture.output(type = "output") — catches anything written directly to
#       stdout via cat()/writeLines() by pak's cli_server_default renderer, such
#       as "ℹ No downloads are needed, 1 pkg is cached".
pakCall <- function(expr, verbose = getOption("Require.verbose")) {
  verbose <- verbose %||% 0L
  if (verbose <= -1L) {
    old <- options(pkg.show_progress = FALSE)
    on.exit(options(old), add = TRUE)
    .res <- NULL
    utils::capture.output(.res <- suppressMessages(force(expr)), type = "output")
    .res
  } else if (verbose == 0L) {
    old <- options(pkg.show_progress = FALSE)
    on.exit(options(old), add = TRUE)
    force(expr)
  } else {
    force(expr)
  }
}

pakErrorHandling <- function(err, pkg, packages, verbose = getOption("Require.verbose")) {
  grp <- c(
    .txtCntInstllDep, .txtFailedToBuildSrcPkg, .txtConflictsWith,
    .txtCantFindPackage, .txtMissingValueWhereTFNeeded, .txtCldNotSlvPkgDeps,
    .txtFailedToDLFrom, .txtPakNoPkgCalledPak, .txtUnknownArchiveType
  )
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
        pkgLong <- splitStr[whDeps - 1]
        pkgLong <- strsplit(pkgLong, spl[2])[[1]]
        pkgLong <- basename(pkgLong)
        filename <- pkgLong[nchar(pkgLong) > 2]
        pkg2 <- extractPkgName(filenames = basename(filename))
        whRm <- grep(pkg2, packages)
        packages2 <- pakCacheDeleteTryAgain(pkg2 = pkg2, packages = packages, whRm = whRm)
        if (!identical(length(packages2), length(packages)))
          packages <- packages[-whRm]
      }
      # For "Failed to build source package X" errors, the ANSI-based splitting
      # (spl[2] = ANSI escape pattern) fails when as.character(err) has no ANSI codes
      # (which is the case when try(..., silent=TRUE) captures the plain message).
      # Directly extract the package name from the error text as a reliable fallback.
      if (grp[i] == .txtFailedToBuildSrcPkg && length(pkg2) > 1) {
        directName <- sub(".*Failed to build source package ([^. \\t\\n\\033]+).*",
                          "\\1", paste(splitStr, collapse = " "))
        directName <- gsub("\\033\\[[0-9;]*m", "", directName)  # strip residual ANSI
        directName <- trimws(directName)
        if (nzchar(directName) && directName != paste(splitStr, collapse = " ")) {
          pkg2 <- directName
        }
      }

      if (length(pkg2) > 1) {
        d <- Map(x = b, whDep = whDeps, function(x, whDep) {
          idx <- whDep + 1L
          if (length(idx) == 0L || idx > length(x)) return(character())
          x[[idx]]
        })
        pkg2 <- gsub("@.+$", "", unlist(d))
      }
      pkgNoVersion <- trimVersionNumber(pkg2)

      vers <- tryCatch(Map(x = b, whDep = whDeps, function(x, whDep) {
        idx <- whDep + 3L
        if (length(idx) == 0L || idx > length(x)) return("")
        x[[idx]]
      }), error = function(x) "")
      whRm <- unlist(unname(lapply(
        paste0("^", pkgNoVersion, ".*", vers, "|/", pkgNoVersion, ".*", vers), grep, x = pkg)))

      if (grp[i] == .txtMissingValueWhereTFNeeded) {
        packages <- pakGetArchive(pkgNoVersion, packages = packages, whRm = whRm)
        break
      }
      if (grp[i] == .txtFailedToDLFrom) {
        #
      }

      if (grp[i] == .txtCntInstllDep) {
        whRmAll <- integer()
        for (j in seq_along(pkgNoVersion)) {
          if (isGH(pkgNoVersion[j])) { # "PredictiveEcology/fpCompare (>=2.0.0)"
            if (is.na(pkg[whRm[j]]) || !length(whRm[j])) next
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
        } else if (grp[i] == .txtCantFindPackage) {
          # Transitive dep: pkg2 is not directly in packages (whRm is empty).
          # Append the archive url:: ref so the caller can include it in the retry.
          packages2 <- pakGetArchive(pkg2, packages, whRm = integer(0))
          if (!identical(length(packages2), length(packages))) {
            packages <- packages2
          }
          break
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

pakPkgSetup <- function(pkgs, doDeps, verbose = getOption("Require.verbose")) {

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
        his <- pakCall(pak::pkg_deps(trimVersionNumber(pkg)), verbose)
        his <- his[his$package %in% extractPkgName(pkg), ]
        setnames(his, old = "version", new = "Version")
      } else {
        his <- pakCall(pak::pkg_history(trimVersionNumber(pkg)), verbose)
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
      err <- try(outs <- pakCall(pak::pak(c(
        paste0("deps::", td3),
        pkgsList$direct
      ), lib = libPaths[1], ask = FALSE,

      # already done in pakPkgSetup # doDeps,
      # FALSE doesn't work when `deps::` is used
      dependencies = doDeps,
      upgrade = upgrade), verbose),
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
  pkgDT[, packageFullName := Package]
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
                 ifelse(length(which), NA, FALSE))

    pkgDone <- character()
    supplement <- character(0)  # archive url:: refs for transitive deps discovered during retry
    i <- 0
    while (length(pkg1) > 0) {
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

      val <- try(pakCall(pak::pkg_deps(c(pkg, supplement), dependencies = wh), verbose), silent = TRUE)
      if (is(val, "try-error")) {
        pkgDone <- unique(c(pkg, pkgDone))
        pkgOrig2 <- pkg
        pkg <- pakErrorHandling(val, pkg, pkg, verbose = verbose)
        if (length(pkg)) {
          if (length(pkg) > length(pkgOrig2)) {
            # New archive url:: refs added for transitive deps.
            # Add to supplement so they're passed in the next pak::pkg_deps call,
            # but don't update pkg1 (the main package hasn't changed).
            newRefs <- setdiff(pkg, pkgOrig2)
            supplement <- unique(c(supplement, newRefs))
            pkgDone <- pkgDone[pkgDone != pkgOrig2]  # allow retry of main pkg with wh=NA
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
        # For url:: refs (archived CRAN packages), replace the full URL with the
        # plain package name so that extractPkgName() and allNeeded checks work.
        # The version constraint (op + version) is preserved if present.
        rr[hasWeirdSource, packageFullName := {
          vs <- if (!is.na(version) && nzchar(version)) paste0(" (", op, " ", version, ")") else ""
          paste0(package, vs)
        }]
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
          # Always use the user-supplied packFullName (plain name or GitHub ref).
          # Using dep$ref here would set url:: archive refs as packageFullName,
          # which extractPkgName() cannot parse back to the plain package name.
          rr[package %in% pkg, packageFullName := packFullName]
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
      if (is(his, "try-error")) return(character())
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
  # Strip pak source prefixes (any::, cran::, url::, etc.) to get the bare package name
  pkg2 <- gsub("^[A-Za-z][A-Za-z0-9+.-]*::", "", pkg2)
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
  if (!is(his, "try-error") || length(isCRAN) > 0) {
    # opt <- options(repos = isCRAN)
    # on.exit(options(opt))
    type <- if (isWindows() || isMacOS()) "binary" else "source"
    ap <- available.packagesWithCallingHandlers(isCRAN, type = type) |> as.data.table()
    onCurrent <- ap[Package %in% pkg2]
    if (NROW(onCurrent)) {
      fileext <- if (identical(type, "binary")) ".zip" else ".tar.gz"
      pth <- file.path(paste0(onCurrent$Package, "_", onCurrent$Version, fileext))
    } else {
      if (is(his, "try-error")) {
        # Package not found in archive either — remove it and warn
        packages <- packages[-whRm]
        warning(.txtCouldNotBeInstalled, ": ", pkgNoVer, call. = FALSE)
        return(packages)
      }
      type <- "source"
      pth <- file.path("Archive", his$Package, paste0(his$Package, "_", his$Version, ".tar.gz"))
    }
    if (isTRUE(!startsWith(isCRAN, "https"))) isCRAN <- paste0("https://", isCRAN)
    pth <- paste0("url::",file.path(contrib.url(isCRAN, type = type), pth))
    if (length(whRm) > 0L) {
      packages[whRm] <- pth
    } else {
      # whRm is empty when the archived package is a transitive dep not directly in
      # the packages list (e.g. called from pakPkgDep with the direct package as pkg).
      # Append the archive ref so the retry includes it explicitly.
      packages <- c(packages, pth)
    }
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
  if (is(dl, "try-error")) return(FALSE)
  vers <- extractVersionNumber(filenames = basename(dl$fulltarget))
  isOK <- compareVersion2(vers, versionSpec = pkgDT$versionSpec, inequality = pkgDT$inequality)
  isOK
}

# Extract the most informative line(s) from a pak try-error string.
# Strips ANSI codes, removes generic framing lines, and returns up to two
# lines that explain WHY the build/install failed.
pakBuildFailReason <- function(errStr) {
  lines <- strsplit(as.character(errStr), "\n")[[1]]
  lines <- gsub("\033\\[[0-9;]*m", "", lines)   # strip ANSI escape sequences
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  # Remove generic R/pak framing lines that don't explain the root cause
  lines <- grep("^Error in pak::|pakRetryLoop|^\\s*$|^Error$", lines,
                value = TRUE, invert = TRUE)
  # Prioritise lines that contain diagnostic keywords
  diag <- grep(paste(
    "namespace '[^']+' .+ is being loaded",
    "invalid.*expression", "ERROR:", "permission denied",
    "unable to move", "cannot remove", "compilation failed",
    "lazy loading failed", "Execution halted",
    sep = "|"), lines, value = TRUE, ignore.case = FALSE)
  if (length(diag)) return(paste(head(unique(diag), 2L), collapse = "; "))
  # Fallback: first non-"Error in" line
  fb <- head(lines[!startsWith(lines, "Error in")], 1L)
  if (length(fb) && nzchar(fb)) fb else ""
}

pakCacheDeleteTryAgain <- function(pkg2, packages, whRm) {
  prevFail <- get0("failedPkgs", envir = pakEnv())
  pkg3 <- extractPkgName(pkg2)
  if (any(pkg3 %in% prevFail)) {
    # Already tried clearing cache; give up on this package.
    # Do NOT modify failedPkgs (setdiff would clear it when pkg3 is already present).
    packages <- packages[-whRm]
  } else {
    try(pak::cache_delete(package = pkg3[1]), silent = TRUE)
    nowFails <- c(prevFail, pkg3)
    assign("failedPkgs", nowFails, envir = pakEnv())
  }
  packages
}

# ---------------------------------------------------------------------------
# pakWhoNeeds() — diagnostic: given a pak_result (from pak::pkg_deps()), show
# which packages list `pkg` as a direct dependency (of any type), and flag any
# that list it under a "remotes"-style ref.
#
# Usage:
#   # After any Require call with usePak = TRUE (uses in-memory cache):
#   Require:::pakWhoNeeds("BH")
#
#   # Or supply pak_result directly:
#   pak_result <- pak::pkg_deps(c("SpaDES.core", "data.table"), dependencies = NA)
#   Require:::pakWhoNeeds("BH", pak_result)
# ---------------------------------------------------------------------------
pakWhoNeeds <- function(pkg, pak_result = NULL) {
  if (is.null(pak_result)) {
    # Try to pull the most-recently stored result from the in-memory cache.
    envKeys <- ls(envir = pakEnv(), pattern = "^pakDeps_")
    if (!length(envKeys)) {
      message("No cached pak_result found. Run Require::Install(...) with ",
              "options(Require.usePak = TRUE) first, or supply pak_result directly.")
      return(invisible(NULL))
    }
    # Use the most recently assigned key (last element of ls() is arbitrary, but
    # for a single active session there is usually only one).
    pak_result <- get(envKeys[length(envKeys)], envir = pakEnv(), inherits = FALSE)
  }
  if (is.null(pak_result) || !NROW(pak_result)) {
    message("pak_result is NULL or empty.")
    return(invisible(NULL))
  }
  hits <- lapply(seq_len(NROW(pak_result)), function(i) {
    dep_tbl <- tryCatch(as.data.table(pak_result$deps[[i]]), error = function(e) NULL)
    if (is.null(dep_tbl) || !NROW(dep_tbl)) return(NULL)
    matched <- dep_tbl[package == pkg]
    if (!NROW(matched)) return(NULL)
    cbind(data.table(parent = pak_result$package[i],
                     parent_ref = pak_result$ref[i]),
          matched[, .(dep_type = type, dep_ref = ref, op, version)])
  })
  hits <- rbindlist(Filter(Negate(is.null), hits), fill = TRUE, use.names = TRUE)
  if (!NROW(hits)) {
    message(pkg, " is not listed as a direct dependency of any package in pak_result.")
    return(invisible(hits))
  }
  hits[]
}

# ---------------------------------------------------------------------------
# pakDepsResolve() — cached wrapper around pak::pkg_deps() retry loop
#
# Runs the full retry-and-fallback resolution and caches the resulting
# pak_result data.table in two tiers:
#
#   1. In-memory  : pakEnv() keyed by MD5 hash of inputs.  Free on purge or
#                   when R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE elapses.
#   2. Disk       : cacheDir()/pak/pkg_deps/<hash>.rds — survives R restarts,
#                   giving cross-session speed-up for repeat calls.
#
# TTL defaults to 24 h (longer than the 1-h available.packages TTL because
# the dep tree changes far less often than package availability metadata).
# Override with options(Require.pak.depCacheTTL = <seconds>).
# ---------------------------------------------------------------------------
.pakDepsCacheTTL <- 24 * 3600   # 24 hours default

pakDepsCacheKey <- function(pkgsForPak, wh, repos) {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(list(pkgs  = sort(pkgsForPak),
               wh    = sort(as.character(unlist(wh))),
               repos = sort(repos)),
          tmp, compress = FALSE)
  unname(tools::md5sum(tmp))
}

pakDepsCacheDir <- function() {
  file.path(cacheDir(), "pak", "pkg_deps")
}

pakDepsResolve <- function(pkgsForPak, wh, repos, verbose, purge) {

  # --- 1. Compute cache key ---
  key      <- pakDepsCacheKey(pkgsForPak, wh, repos)
  envKey   <- paste0("pakDeps_", key)
  cacheDir <- pakDepsCacheDir()
  cacheFile <- file.path(cacheDir, paste0(key, ".rds"))
  ttl      <- getOption("Require.pak.depCacheTTL", .pakDepsCacheTTL)
  offline  <- isTRUE(getOption("Require.offlineMode"))

  # --- 2. In-memory cache hit ---
  if (!isTRUE(purge)) {
    cached <- get0(envKey, envir = pakEnv(), inherits = FALSE)
    if (!is.null(cached)) {
      messageVerbose("pakDepsResolve: using in-memory cached dep tree (",
                     length(unique(cached$package)), " packages).",
                     verbose = verbose, verboseLevel = 2)
      return(cached)
    }
  }

  # --- 3. Disk cache hit ---
  if (!isTRUE(purge) && file.exists(cacheFile)) {
    age <- as.numeric(difftime(Sys.time(), file.mtime(cacheFile), units = "secs"))
    if (offline || age < ttl) {
      cached <- tryCatch(readRDS(cacheFile), error = function(e) NULL)
      if (!is.null(cached)) {
        assign(envKey, cached, envir = pakEnv())
        messageVerbose("pakDepsResolve: using disk-cached dep tree (",
                       length(unique(cached$package)), " packages; ",
                       round(age / 3600, 1), "h old).",
                       verbose = verbose, verboseLevel = 2)
        return(cached)
      }
    }
  }

  # --- 4. Cache miss: run the full retry + fallback resolution ---
  pak_result <- NULL

  for (.pakDepsAttempt in 1:5) {
    pak_result_or_err <- tryCatch(
      list(result = pakCall(pak::pkg_deps(pkgsForPak, dependencies = wh), verbose), err = NULL),
      error = function(e) list(result = NULL, err = conditionMessage(e))
    )
    pak_result <- pak_result_or_err$result
    if (!is.null(pak_result)) break

    errMsg <- pak_result_or_err$err

    # pak error messages often contain ANSI escape codes; strip them so that
    # nchar() gives the visible width and extracted refs are clean for matching.
    stripAnsi <- function(x) gsub("\033\\[[0-9;]*m", "", x)
    errLines <- stripAnsi(strsplit(errMsg, "\n")[[1]])
    changed  <- FALSE

    # --- Handle "X: Conflicts with Y" / "X conflicts with Y, to be installed" ---
    # pak reports this (case-insensitive) when two different refs resolve to the same
    # package. Two formats are seen in practice:
    #   "* owner/pkg@branch: Conflicts with pkg"            (format A)
    #   "* owner/pkg@branch: owner/pkg@branch conflicts with pkg, to be installed" (format B)
    # Strategy: keep the GitHub ref and remove the plain CRAN name from pkgsForPak.
    conflictRows    <- list()  # accumulate rows for the summary table
    conflictLines <- grep("(?i)conflicts with", errLines, value = TRUE, perl = TRUE)
    if (length(conflictLines)) {
      for (cl in conflictLines) {
        cl  <- trimws(sub("^\\*\\s*", "", cl))           # strip leading "* "
        lhs <- trimws(sub(":.*",  "", cl))               # before first ":"
        # Extract the RHS (what it conflicts with), case-insensitive, strip trailing noise
        rhs <- trimws(sub("(?i).*conflicts with\\s*", "", cl, perl = TRUE))
        rhs <- trimws(sub(",.*$", "", rhs))              # strip ", to be installed" etc.
        # Remove whichever is a plain CRAN ref (no @branch, no owner/);
        # if both are GitHub, remove the one without a @branch spec
        lhsGH <- isGH(lhs) || grepl("@", lhs)
        rhsGH <- isGH(rhs) || grepl("@", rhs)
        toRm  <- if (!rhsGH) rhs else if (!lhsGH) lhs else rhs
        pkgNmToRm <- extractPkgName(toRm)
        keep <- if (!rhsGH) lhs else rhs
        # Remove every pkgsForPak entry for this package name that is NOT the winner.
        # Only mark changed if something was actually removed — otherwise the same
        # conflict will appear in the next attempt and we'll loop until attempt limit.
        before <- length(pkgsForPak)
        pkgsForPak <- pkgsForPak[
          !(extractPkgName(pkgsForPak) == pkgNmToRm &
              trimVersionNumber(pkgsForPak) != trimVersionNumber(keep))
        ]
        if (length(pkgsForPak) < before) {
          changed <- TRUE
          conflictRows[[length(conflictRows) + 1L]] <-
            list(Package = pkgNmToRm,
                 Conflict   = paste0(toRm, "  vs  ", keep),
                 Resolution = paste0("keep ", keep))
        }
      }
    }

    # --- Handle "Can't find package called X" (archived packages) ---
    cantLines <- grep(.txtCantFindPackage, errLines, value = TRUE)
    cantPkgs  <- trimws(sub(paste0(".*", .txtCantFindPackage), "", cantLines))
    cantPkgs  <- sub("\\.$", "", cantPkgs)
    cantPkgs  <- cantPkgs[nzchar(cantPkgs) & !grepl("::", cantPkgs)]
    if (length(cantPkgs)) {
      newRefs <- character(0)
      for (cp in cantPkgs) {
        urlRef <- tryCatch(
          pakGetArchive(cp, packages = cp, whRm = 1L),
          error = function(e) cp
        )
        urlRef <- grep("^url::", urlRef, value = TRUE)
        if (length(urlRef)) {
          newRefs <- c(newRefs, urlRef[1L])
          conflictRows[[length(conflictRows) + 1L]] <-
            list(Package = cp,
                 Conflict   = paste0(cp, " (not on CRAN)"),
                 Resolution = paste0("use ", urlRef[1L]))
        }
      }
      if (length(newRefs)) {
        pkgsForPak <- pkgsForPak[!extractPkgName(pkgsForPak) %in% cantPkgs]
        pkgsForPak <- c(pkgsForPak, newRefs)
        changed <- TRUE
      }
    }

    # --- Handle "X: dependency conflict" (Remotes-based CRAN/GitHub collision) ---
    # pak reports "X: dependency conflict" when X is listed as a plain CRAN ref in
    # pkgsForPak AND some GitHub package in the dep tree has "Remotes: owner/X" in its
    # DESCRIPTION, causing pak to see two different refs for the same package.
    # Unlike "Conflicts with" (where both refs are explicit), here only the CRAN ref
    # is in pkgsForPak; the GitHub ref was added implicitly via Remotes following.
    # Strategy: remove the plain CRAN ref from pkgsForPak so pak can resolve consistently
    # through the Remotes path. Step 2b normalization then restores CRAN for any package
    # the user originally requested from CRAN.
    # Pattern: "* ggplot2: dependency conflict" — the leading "* " is NOT whitespace,
    # so we must NOT anchor with [[:space:]]* at the start.
    depConflictLines <- grep(":[[:space:]]*dependency conflict$", errLines, value = TRUE)
    if (length(depConflictLines)) {
      depConflictPkgs <- trimws(sub("^[[:space:]]*\\*[[:space:]]*", "", depConflictLines))
      depConflictPkgs <- trimws(sub("[[:space:]]*:[[:space:]]*dependency conflict$", "", depConflictPkgs))
      depConflictPkgs <- depConflictPkgs[nzchar(depConflictPkgs) & !grepl("[/:]", depConflictPkgs)]
      for (dcp in depConflictPkgs) {
        # Only remove plain CRAN-style refs (no /, no @, no ::)
        crankIdx <- which(extractPkgName(pkgsForPak) == dcp &
                          !isGH(pkgsForPak) & !grepl("::", pkgsForPak))
        if (length(crankIdx)) {
          pkgsForPak <- pkgsForPak[-crankIdx]
          changed <- TRUE
          # Try to find the GitHub ref pak saw via Remotes-following (may appear in
          # the error lines as a "conflicts with" entry for the same package).
          ghRef    <- character(0)
          viaRef   <- character(0)  # the other-package whose Remotes caused the clash
          conflictForDcp <- grep(paste0("(?i)", dcp, ".*conflicts with|conflicts with.*", dcp),
                                 errLines, value = TRUE, perl = TRUE)
          if (length(conflictForDcp)) {
            cl2   <- trimws(sub("^\\*\\s*", "", conflictForDcp[1L]))
            lhs2  <- trimws(sub(":.*", "", cl2))
            rhs2  <- trimws(sub("(?i).*conflicts with\\s*", "", cl2, perl = TRUE))
            rhs2  <- trimws(sub(",.*$", "", rhs2))
            cand  <- if (isGH(lhs2) || grepl("@", lhs2)) lhs2 else rhs2
            # Only accept cand as the GitHub ref for dcp when it is actually a ref
            # FOR dcp (e.g. owner/sp@branch).  If cand is a different package
            # (e.g. SpaDES.core "Conflicts with sp"), record it as the via-source
            # instead so the message can say "via SpaDES.core Remotes".
            if (nzchar(cand) && extractPkgName(cand) == dcp) {
              ghRef  <- cand
            } else if (nzchar(cand)) {
              viaRef <- extractPkgName(cand)
            }
          }
          # Build the conflict table row.
          # Case 1: we found a concrete GitHub ref for dcp itself → "dcp vs owner/dcp@branch"
          # Case 2: we only know which other package's Remotes caused it → "dcp (via X Remotes)"
          # Case 3: no context at all → skip row (avoid misleading entries)
          if (length(ghRef) && nzchar(ghRef)) {
            conflictRows[[length(conflictRows) + 1L]] <-
              list(Package    = dcp,
                   Conflict   = paste0(dcp, "  vs  ", ghRef),
                   Resolution = "drop CRAN ref; resolve via GitHub Remotes")
          } else if (length(viaRef) && nzchar(viaRef)) {
            conflictRows[[length(conflictRows) + 1L]] <-
              list(Package    = dcp,
                   Conflict   = paste0(dcp, " (CRAN)  vs  ", dcp, " (via ", viaRef, " Remotes)"),
                   Resolution = "drop CRAN ref; resolve via GitHub Remotes")
          }
        }
      }
    }

    # Print a summary table of what was found and how it will be resolved.
    # Full error detail is available at verboseLevel >= 3 for debugging.
    if (changed && length(conflictRows)) {
      tbl <- rbindlist(conflictRows, fill = TRUE, use.names = TRUE)
      w1 <- max(nchar(c("Package",    tbl$Package)))
      w2 <- max(nchar(c("Conflict",   tbl$Conflict)))
      w3 <- max(nchar(c("Resolution", tbl$Resolution)))
      hdr  <- sprintf("  %-*s  %-*s  %-*s", w1, "Package", w2, "Conflict", w3, "Resolution")
      sep  <- paste0("  ", strrep("-", w1), "  ", strrep("-", w2), "  ", strrep("-", w3))
      rows <- sprintf("  %-*s  %-*s  %-*s",
                      w1, tbl$Package, w2, tbl$Conflict, w3, tbl$Resolution)
      messageVerbose(
        "Note: pak detected conflicts/archived packages (attempt ", .pakDepsAttempt,
        "); adjusting and retrying:\n",
        paste(c(hdr, sep, rows), collapse = "\n"),
        verbose = verbose, verboseLevel = 2)
    }
    messageVerbose("pak::pkg_deps full error (attempt ", .pakDepsAttempt, "):\n", errMsg,
                   verbose = verbose, verboseLevel = 3)

    if (!changed) break  # error is not one we know how to fix; give up
  }

  if (is.null(pak_result)) {
    # Final fallback: resolve each package individually so pak never sees cross-package
    # conflicts. Package A may list "SpaDES.tools" (CRAN) and package B may list
    # "PredictiveEcology/SpaDES.tools@development" — resolving them separately avoids
    # the conflict. We then merge all dep tables and let Require's conflict resolution
    # (confirmEqualsDontViolateInequalitiesThenTrim + trimRedundancies) pick the winner.
    # Also pass any accumulated url:: archive refs to each call, so packages with
    # archived transitive deps (e.g. pryr) can still be resolved.
    messageVerbose("Note: batch dependency resolution found unresolvable conflicts; ",
                   "switching to per-package resolution. ",
                   "This is normal when mixing CRAN and GitHub packages — Require will handle it.",
                   verbose = verbose, verboseLevel = 1)
    archiveRefs <- grep("^url::", pkgsForPak, value = TRUE)
    nonArchivePkgs <- pkgsForPak[!grepl("^url::", pkgsForPak)]
    per_pkg_results <- lapply(nonArchivePkgs, function(pkg) {
      # First try with archive refs (for packages with archived transitive deps).
      # If that fails (e.g., archive refs introduce new CRAN/GitHub conflicts), retry
      # without archive refs — it's better to get a partial dep tree than nothing.
      query <- if (length(archiveRefs)) unique(c(pkg, archiveRefs)) else pkg
      result <- tryCatch(pakCall(pak::pkg_deps(query, dependencies = wh), verbose), error = function(e) NULL)
      if (is.null(result) && length(archiveRefs))
        result <- tryCatch(pakCall(pak::pkg_deps(pkg, dependencies = wh), verbose), error = function(e) NULL)
      result
    })
    per_pkg_results <- per_pkg_results[!sapply(per_pkg_results, is.null)]
    if (length(per_pkg_results)) {
      pak_result <- tryCatch(
        rbindlist(per_pkg_results, fill = TRUE, use.names = TRUE),
        error = function(e) NULL
      )
    }
  }

  # --- 5. Store successful result in both cache tiers ---
  if (!is.null(pak_result)) {
    assign(envKey, pak_result, envir = pakEnv())
    tryCatch({
      dir.create(cacheDir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(pak_result, cacheFile)
    }, error = function(e) NULL)   # non-fatal if disk write fails
  }

  pak_result
}

# ---------------------------------------------------------------------------
# Invalidate the pak dep-tree disk cache for a given set of inputs.
# Called after successful installation so the next call re-resolves freshly
# (installed state changed; cache key stays the same but should be revalidated
# sooner than the normal TTL would allow).
# ---------------------------------------------------------------------------
pakDepsCacheInvalidate <- function(pkgsForPak, wh, repos) {
  key      <- tryCatch(pakDepsCacheKey(pkgsForPak, wh, repos), error = function(e) NULL)
  if (is.null(key)) return(invisible(NULL))
  envKey   <- paste0("pakDeps_", key)
  cacheFile <- file.path(pakDepsCacheDir(), paste0(key, ".rds"))
  rm(list = intersect(envKey, ls(envir = pakEnv())), envir = pakEnv())
  if (file.exists(cacheFile)) unlink(cacheFile)
  invisible(NULL)
}

# Resolve package dependencies using pak, returning a Require-format pkgDT.
# This replaces the pkgDep() + parsePackageFullname() + ... pipeline when usePak = TRUE.
pakDepsToPkgDT <- function(packages, which, libPaths, standAlone, verbose,
                          purge = getOption("Require.purge", FALSE)) {
  if (!requireNamespace("pak", quietly = TRUE)) stop("Please install pak")

  # pak spawns a subprocess that inherits .libPaths(). Set .libPaths() to match
  # Require's standAlone semantics before calling pak, then restore on exit.
  #
  # standAlone = TRUE  → c(libPaths[1], base_pkg_lib)   (isolated project library)
  # standAlone = FALSE → c(libPaths[1], existing .libPaths())  (shared)
  #
  # In both cases, pak's own library must be present so the subprocess can load pak.
  pakLib    <- tryCatch(dirname(find.package("pak")), error = function(e) NULL)
  basePkgLib <- tail(.libPaths(), 1L)   # always the base R packages path
  origPaths  <- .libPaths()
  if (isTRUE(standAlone)) {
    newPaths <- unique(c(libPaths[1L], basePkgLib))
  } else {
    newPaths <- unique(c(libPaths[1L], origPaths))
  }
  if (!is.null(pakLib) && !pakLib %in% newPaths)
    newPaths <- c(newPaths, pakLib)
  .libPaths(newPaths)
  on.exit(.libPaths(origPaths), add = TRUE)

  # pak uses logical: TRUE = include Suggests, NA = standard (Imports/Depends/LinkingTo)
  wh <- if (any(grepl("suggests", tolower(unlist(which))))) TRUE else NA

  # Track which packages the user originally requested as plain CRAN refs (no GitHub, no url::).
  # Used in step 2b to normalize Remotes-based GitHub refs back to plain CRAN names so that
  # pakInstallFiltered installs from CRAN rather than from a fork.
  userCRANpkgs <- extractPkgName(packages[!isGH(packages) & !grepl("::", packages)])

  # Pre-resolve conflicts in the package list using Require's own deduplication logic
  # before handing anything to pak. This handles:
  #   (a) Same package as both CRAN ref and GitHub ref → trimRedundantVersionAndNoVersion
  #       removes the no-version entry, keeping whichever has a version constraint.
  #       If neither has a version spec, the GitHub ref (higher repoLocation priority)
  #       is kept by the subsequent name-based dedup below.
  #   (b) Multiple GitHub branches for same package (e.g. @master vs @development) →
  #       the branch with the highest version constraint wins.
  resolvedPkgs <- tryCatch(
    trimRedundancies(packages[!extractPkgName(packages) %in% .basePkgs])$packageFullName,
    error = function(e) packages
  )

  # Strip version specs and HEAD flags for the pak query; pak resolves from the ref alone
  pkgsForPak <- resolvedPkgs
  pkgsForPak <- HEADtoNone(pkgsForPak)
  pkgsForPak <- trimVersionNumber(pkgsForPak)
  pkgsForPak <- pkgsForPak[!pkgsForPak %in% .basePkgs]
  # For any remaining duplicated package names (both have no version spec), prefer GH ref
  pkgNms <- extractPkgName(pkgsForPak)
  dupNms <- unique(pkgNms[duplicated(pkgNms)])
  if (length(dupNms)) {
    toRemove <- integer(0)
    for (pn in dupNms) {
      idx <- which(pkgNms == pn)
      ghIdx <- idx[isGH(pkgsForPak[idx])]
      if (length(ghIdx) > 0) toRemove <- c(toRemove, setdiff(idx, ghIdx[1L]))
      else                    toRemove <- c(toRemove, idx[-1L])
    }
    if (length(toRemove)) pkgsForPak <- pkgsForPak[-toRemove]
  }
  pkgsForPak <- unique(pkgsForPak)
  # Convert == version specs to pak @version format for the dep query
  pkgsForPak <- equalsToAt(pkgsForPak)

  if (!length(pkgsForPak)) return(toPkgDTFull(character()))

  # 1. Resolve the full dep tree via pak, with two-tier caching (in-memory + disk).
  #    pakDepsResolve() handles the retry loop, conflict resolution, per-package
  #    fallback, and cache read/write. Returns NULL only if all strategies fail.
  pak_result <- pakDepsResolve(pkgsForPak, wh,
                               repos   = getOption("repos"),
                               verbose = verbose,
                               purge   = purge)

  if (is.null(pak_result)) {
    messageVerbose("pak::pkg_deps: all strategies failed; using direct package list only.",
                   verbose = verbose, verboseLevel = 2)
    return(toPkgDTFull(packages))
  }

  # 2. Flatten all deps sub-tables to get the raw version requirements.
  # pak$deps[[i]] has columns: ref, type, package, op, version
  # 'type' is lowercase ("imports", "depends", "linkingto", "suggests")
  # 'op' is ">=" or "" (empty string means no version constraint)
  # 'version' is the minimum required version from the DESCRIPTION file
  validTypes <- tolower(unlist(which))
  all_reqs_list <- lapply(pak_result$deps, function(dep_tbl) {
    if (is.null(dep_tbl) || !NROW(dep_tbl)) return(NULL)
    dep_tbl <- as.data.table(dep_tbl)
    dep_tbl <- dep_tbl[tolower(type) %in% validTypes]
    dep_tbl <- dep_tbl[!package %in% c(.basePkgs, "R")]
    dep_tbl
  })
  all_reqs <- rbindlist(all_reqs_list, fill = TRUE, use.names = TRUE)

  # Filter out "Require" itself from transitive deps: Require is always running
  # (we are inside it), so it is never absent. Including it as a transitive dep
  # causes needToRestartR() to fire NeedRestart=TRUE, which incorrectly marks
  # data.table and sys as "Need to restart R" when an impossible version constraint
  # like data.table (>=100.0) is in the user's package list.
  if (NROW(all_reqs)) {
    all_reqs <- all_reqs[package != "Require"]
  }

  # 2b. Normalize refs in all_reqs to prevent CRAN/GitHub conflicts during install.
  # The dep sub-tables carry the raw dep ref (e.g. "tidyverse/ggplot2" from a Remotes
  # field) which can conflict with plain CRAN entries in pakInstallFiltered.  Normalize:
  # (1) Packages the user originally requested as plain CRAN → always use plain name.
  # (2) Packages pak resolved as type "cran"/"standard" → also use plain name.
  # This ensures pakInstallFiltered passes "any::ggplot2" (not "tidyverse/ggplot2")
  # to pak::pak(), avoiding spurious CRAN/GitHub conflicts during the install step.
  if (NROW(all_reqs)) {
    # User-requested CRAN packages: unconditionally normalize ref to plain name
    all_reqs[package %in% userCRANpkgs, ref := package]
    # pak-resolved CRAN packages: also normalize (covers transitive CRAN deps)
    if (!is.null(pak_result)) {
      pakResDT <- tryCatch(as.data.table(pak_result), error = function(e) NULL)
      if (!is.null(pakResDT) && all(c("package", "type") %in% names(pakResDT))) {
        refNorm <- unique(pakResDT[, .(package, src_type = type)])
        refNorm <- refNorm[order(!(src_type %in% c("cran", "standard")))]
        refNorm <- refNorm[!duplicated(package)]
        cran_pkgs <- refNorm[src_type %in% c("cran", "standard"), package]
        all_reqs[package %in% cran_pkgs, ref := package]
      }
    }
  }

  # 3. Build packageFullName from pak's ref + op + version
  if (NROW(all_reqs)) {
    all_reqs[, packageFullName := paste0(
      ref,
      ifelse(nzchar(op) & nzchar(version),
             paste0(" (", op, " ", version, ")"),
             "")
    )]
  }

  # 3b. Check that pak's resolved versions can actually satisfy any >= / > constraints
  # the user specified. pak silently installs the latest available version even when
  # it doesn't satisfy the constraint (e.g., fpCompare 0.2.4 installed despite >=2.0.0).
  # Catch these now: warn and remove the package so it is never passed to pakInstallFiltered.
  #
  # Only applies to CRAN-like packages. GitHub (owner/repo@branch) and url:: refs are
  # excluded: for GitHub refs pak installs exactly from the specified branch/commit, so
  # if the branch has the required version pak will install it; if not, pak errors during
  # install (not silently installs wrong version). Applying this check to GitHub refs
  # causes false positives when pak resolved an older cached/CRAN version for the same
  # package name while the user's GitHub ref is the one that actually satisfies the constraint.
  if (NROW(pak_result)) {
    pakVerMap <- setNames(pak_result$version, pak_result$package)
    origCheck <- toPkgDTFull(packages[!extractPkgName(packages) %in% .basePkgs])
    # Exclude GitHub and url:: refs from the version check — only check CRAN-like packages.
    isCRANcheck <- !isGH(origCheck$packageFullName) &
                   !startsWith(origCheck$packageFullName, "url::")
    needCheck  <- origCheck[isCRANcheck &
                            !is.na(inequality) & inequality %in% c(">=", ">") &
                            !is.na(versionSpec) & nzchar(versionSpec) &
                            Package %in% names(pakVerMap) &
                            # Skip packages where pak returned NA/empty version (e.g. some GitHub
                            # deps resolved without metadata). compareVersion2("", ...) returns FALSE,
                            # which would incorrectly flag them as unsatisfiable.
                            nzchar(pakVerMap[Package]) & !is.na(pakVerMap[Package])]
    if (NROW(needCheck)) {
      canSatisfy <- compareVersion2(pakVerMap[needCheck$Package],
                                    needCheck$versionSpec, needCheck$inequality)
      badPkgs <- needCheck$Package[canSatisfy %in% FALSE]
      if (length(badPkgs)) {
        badFullNames <- needCheck$packageFullName[canSatisfy %in% FALSE]
        warning(messageCantInstallNoVersion(badFullNames), call. = FALSE)
        packages <- packages[!extractPkgName(packages) %in% badPkgs]
      }
    }
  }

  # 4. Include the user's originally stated packages (with their version specs).
  # These may have stricter requirements than what DESCRIPTION files state.
  user_pkgFN <- packages[!extractPkgName(packages) %in% .basePkgs]

  # 4a. Sync url:: archive refs from pkgsForPak back into user_pkgFN.
  # The retry loop may have replaced plain package names (e.g. "fastdigest") with
  # url:: archive refs (e.g. "url::https://.../fastdigest_0.6-4.tar.gz") in
  # pkgsForPak. Without this sync, user_pkgFN still has the plain name, so
  # pakInstallFiltered would try "any::fastdigest" instead of the url:: ref.
  archiveRefsInPkgsForPak <- grep("^url::", pkgsForPak, value = TRUE)
  if (length(archiveRefsInPkgsForPak)) {
    archivePkgNamesFromPak <- extractPkgName(
      filenames = basename(sub("^url::", "", archiveRefsInPkgsForPak))
    )
    for (.i in seq_along(archivePkgNamesFromPak)) {
      matchIdx <- which(extractPkgName(user_pkgFN) == archivePkgNamesFromPak[.i])
      if (length(matchIdx))
        user_pkgFN[matchIdx] <- archiveRefsInPkgsForPak[.i]
    }
  }

  # 5. Combine all packageFullName strings and parse through Require's existing pipeline
  all_pkgFN <- unique(c(
    user_pkgFN,
    if (NROW(all_reqs)) all_reqs$packageFullName else character()
  ))
  all_pkgFN <- all_pkgFN[nzchar(all_pkgFN)]

  pkgDT <- toPkgDTFull(all_pkgFN)

  # Fix Package column for url:: refs (archived packages).
  # extractPkgName() cannot parse "url::https://...pkg_ver.tar.gz" correctly —
  # it returns the full URL string instead of the package name.  Extract the
  # package name from the filename component of the URL so deduplication and
  # version checking work correctly.
  urlPkgRows <- which(startsWith(pkgDT$Package, "url::"))
  if (length(urlPkgRows)) {
    urlPkgNames <- extractPkgName(
      filenames = basename(sub("^url::", "", pkgDT$Package[urlPkgRows]))
    )
    # Break any SEXP aliasing between Package and packageFullName before any := .
    # toPkgDTFull() calls toDT(Package = extractPkgName(x), packageFullName = x).
    # For url:: refs extractPkgName() returns its input unchanged (same R SEXP),
    # so both columns end up pointing to the SAME character vector.  A := on either
    # column would then silently modify the other column too — sequential := calls
    # would interfere.  Forcing as.character() allocates a new vector, breaking the
    # aliasing so the two columns become fully independent.
    set(pkgDT, NULL, "packageFullName", as.character(pkgDT$packageFullName))
    pkgDT[urlPkgRows, Package := urlPkgNames]
    # packageFullName still holds the original "url::..." strings for those rows.
    # Remove plain-name rows for packages that have a url:: ref — the url:: version
    # carries the correct install path and must be used for the actual installation.
    archivePkgs <- pkgDT[startsWith(packageFullName, "url::")]$Package
    pkgDT <- pkgDT[!(Package %in% archivePkgs & !startsWith(packageFullName, "url::"))]
  }

  pkgDT <- confirmEqualsDontViolateInequalitiesThenTrim(pkgDT)
  pkgDT <- trimRedundancies(pkgDT)

  pkgDT
}

# Install only the packages Require has determined need installing (needInstall == .txtInstall).
# pak is called with exact version pins or any:: to avoid re-resolving deps.
pakInstallFiltered <- function(pkgDT, libPaths, repos, standAlone, verbose) {
  if (!requireNamespace("pak", quietly = TRUE)) stop("Please install pak")

  # Mirror the same .libPaths() logic as pakDepsToPkgDT so the install subprocess
  # sees the same library set that was used for dependency resolution.
  pakLib    <- tryCatch(dirname(find.package("pak")), error = function(e) NULL)
  basePkgLib <- tail(.libPaths(), 1L)
  origPaths  <- .libPaths()
  if (isTRUE(standAlone)) {
    newPaths <- unique(c(libPaths[1L], basePkgLib))
  } else {
    newPaths <- unique(c(libPaths[1L], origPaths))
  }
  if (!is.null(pakLib) && !pakLib %in% newPaths)
    newPaths <- c(newPaths, pakLib)
  .libPaths(newPaths)
  on.exit(.libPaths(origPaths), add = TRUE)

  toInstall <- pkgDT[needInstall == .txtInstall]
  if (!NROW(toInstall)) return(pkgDT)

  # Deduplicate: if the same Package appears as both a CRAN ref and a GitHub/url:: ref,
  # keep only the non-CRAN ref. pak::pak() would reject the list with a "Conflicts with"
  # error if both "any::SpaDES.tools" (CRAN) and "owner/SpaDES.tools@branch" (GitHub)
  # appear together, because dependencies = FALSE still does conflict detection.
  if (anyDuplicated(toInstall$Package)) {
    toInstall[, isNonCRAN := isGH(packageFullName) | startsWith(packageFullName, "url::")]
    toInstall[, hasNonCRAN := any(isNonCRAN), by = Package]
    # Remove plain CRAN rows when a non-CRAN ref exists for the same package
    toInstall <- toInstall[!(hasNonCRAN == TRUE & isNonCRAN == FALSE)]
    # If duplicates still remain (e.g., two GitHub branches), keep first
    toInstall <- unique(toInstall, by = "Package")
    toInstall[, c("isNonCRAN", "hasNonCRAN") := NULL]
  }

  # Convert Require's package specs to pak format
  pkgs <- toInstall$packageFullName

  # Strip HEAD flags (Require already decided to install HEAD packages)
  pkgs <- HEADtoNone(pkgs)

  # == version → @version (exact pin for pak)
  pkgs <- equalsToAt(pkgs)

  # <= version → find highest satisfying version via pak::pkg_history() → @version
  pkgs <- lessThanToAt(pkgs)

  # >= version: strip the constraint. Since Require already checked that the installed
  # version does NOT satisfy >=, installing the latest will always satisfy it.
  pkgs <- gsub("[[:space:]]*\\(>=[[:space:]]*[^)]+\\)", "", pkgs)

  # > version: same logic as >=
  pkgs <- gsub("[[:space:]]*\\(>[[:space:]]*[^)]+\\)", "", pkgs)

  # For plain CRAN packages without any version pin or :: prefix, add "any::" so pak
  # resolves installation order from CRAN metadata. Archived packages not on CRAN will
  # fail with "Can't find package called any::pkg", which pakErrorHandling handles by
  # converting to a url:: archive reference on the next retry.
  # Note: isGH() requires all-alpha owner names; also exclude owner/repo refs with
  # hyphens (e.g. "s-u/fastshp") by checking for "/" directly.
  isCRANlike <- !isGH(pkgs) & !grepl("[@:/]", pkgs) & nzchar(pkgs)
  pkgs[isCRANlike] <- paste0("any::", pkgs[isCRANlike])

  # GitHub packages: strip any remaining version spec (already decided to install)
  whGH <- isGH(pkgs)
  if (any(whGH))
    pkgs[whGH] <- trimVersionNumber(pkgs[whGH])

  # Remove empty strings (e.g., if lessThanToAt() removed a package with no valid version)
  hasRemoved <- !nzchar(pkgs)
  if (any(hasRemoved)) {
    toInstall <- toInstall[!hasRemoved]
    pkgs <- pkgs[!hasRemoved]
    pkgDT[toInstall$Package, needInstall := .txtDontInstall, on = "Package"]
  }

  if (!length(pkgs)) return(pkgDT)

  # Install all packages in one call with dependencies = FALSE.
  #
  # Require's philosophy: only install/update what the version specs require.
  # dependencies = FALSE ensures pak does NOT upgrade already-installed packages
  # beyond what Require determined is necessary (e.g. tibble 3.2.1 → 3.3.1 when
  # no constraint requires it). pakDepsToPkgDT already resolved the complete
  # transitive dep tree via pak::pkg_deps(), so toInstall contains exactly the
  # right set. pak still reads DESCRIPTION files for topological install ordering
  # even with dependencies = FALSE, so LearnBayes-style ordering failures do not
  # occur as long as all required deps are present in toInstall.
  pakRetryLoop <- function(packages, repos, verbose) {
    for (i in seq_len(15)) {
      pkgsIn <- packages
      opts <- options(repos = repos)
      err <- try(
        pakCall(
          pak::pak(packages, lib = libPaths[1], ask = FALSE,
                   dependencies = FALSE, upgrade = FALSE),
          verbose),
        silent = TRUE
      )
      options(opts)
      if (!is(err, "try-error")) break
      alreadyWarned <- FALSE
      packages <- tryCatch(
        pakErrorHandling(as.character(err), pkgsIn, packages, verbose = verbose),
        error = function(e) {
          warning(.txtCouldNotBeInstalled, ": ", conditionMessage(e), call. = FALSE)
          alreadyWarned <<- TRUE
          character(0)
        }
      )
      if (!length(packages)) {
        if (!alreadyWarned) {
          # Include the actual build/install failure reason so the user knows
          # why the package could not be installed (e.g. file locked on Windows,
          # namespace version mismatch, bad regex in source, etc.).
          reason <- pakBuildFailReason(as.character(err))
          if (nzchar(reason)) {
            warning(.txtCouldNotBeInstalled, ": ", reason, call. = FALSE)
          } else {
            warning(.txtCouldNotBeInstalled, call. = FALSE)
          }
        }
        break
      }
    }
    invisible(NULL)
  }

  # Snapshot pre-install versions before pak runs so we can detect build failures:
  # if a package's version is unchanged after the install attempt it means the
  # install failed (build error, cancelled batch, etc.) rather than pak choosing
  # an older version that doesn't satisfy the constraint.  The two cases require
  # different user-facing messages.
  preInstallVers <- setNames(as.character(toInstall$Version), toInstall$Package)

  pakRetryLoop(pkgs, repos, verbose)

  # Update pkgDT with installation results.
  # Use wh[1L] for scalar reads (versionSpec/inequality) but the full wh vector
  # for set() calls so that any duplicate Package rows are all updated consistently.
  nowInstalled <- as.data.table(as.data.frame(installed.packages(lib.loc = libPaths[1]),
                                              stringsAsFactors = FALSE))

  for (pkg in toInstall$Package) {
    wh <- which(pkgDT$Package == pkg)
    if (!length(wh)) next
    nowRow <- nowInstalled[Package == pkg]
    if (NROW(nowRow)) {
      installedVer <- nowRow$Version[1]
      # Check if installed version actually satisfies the original requirement.
      vSpec <- pkgDT$versionSpec[wh[1L]]
      ineq  <- pkgDT$inequality[wh[1L]]
      if (!is.na(vSpec) && nzchar(vSpec) && !is.na(ineq) && nzchar(ineq)) {
        satisfies <- compareVersion2(installedVer, versionSpec = vSpec, inequality = ineq)
        if (!isTRUE(satisfies)) {
          # Only suggest "Please change required version" when pak actually installed a
          # different (but still insufficient) version.  If the version is unchanged the
          # install attempt failed (build error, cancelled batch, etc.) and
          # pakRetryLoop already emitted .txtCouldNotBeInstalled — a second, misleading
          # "Please change required version e.g., pkg (>=<old-ver>)" would tell the user
          # to lower their requirement to the pre-existing version, which is wrong.
          preVer <- preInstallVers[pkg]
          versionChanged <- !isTRUE(!is.na(preVer) && identical(preVer, installedVer))
          if (versionChanged)
            warning(msgPleaseChangeRqdVersion(pkg, ineq = ">=", newVersion = installedVer), call. = FALSE)
          set(pkgDT, wh, "installed",     FALSE)
          set(pkgDT, wh, "Version",       installedVer)
          set(pkgDT, wh, "LibPath",       nowRow$LibPath[1])
          set(pkgDT, wh, "installResult", .txtCouldNotBeInstalled)
          next
        }
      }
      set(pkgDT, wh, "installed",      TRUE)
      set(pkgDT, wh, "Version",        installedVer)
      set(pkgDT, wh, "LibPath",        nowRow$LibPath[1])
      set(pkgDT, wh, "installResult",  "OK")
    } else {
      set(pkgDT, wh, "installed",      FALSE)
      set(pkgDT, wh, "installResult",  .txtCouldNotBeInstalled)
    }
  }

  pkgDT
}
