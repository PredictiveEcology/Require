utils::globalVariables(c(
  "..keepCols"
))


.txtFailedToBuildSrcPkg <- "Failed to build source package"
.txtCantFindPackage <- "Can't find package called "
pakErrorHandling <- function(err, pkg, packages) {
  grp <- c("Can't install", .txtFailedToBuildSrcPkg, "Conflicts with ", .txtCantFindPackage)
  spl <- c(" |\\)", "\033\\[..{0,1}m", " |@", " |\\.")
  pat <- c("dependency", grp[2], "with", "called")
  for (i in seq_along(grp)) {
    a <- grep(grp[i], strsplit(err, split = "\n")[[1]], value = TRUE)
    if (length(a)) {
      b <- strsplit(a, split = spl[i])
      whDeps <- sapply(b, grep, pattern = pat[i])
      d <- Map(x = b, whDep = whDeps, function(x, whDep) x[[whDep + 1]])
      pkg2 <- gsub("@.+$", "", d)
      vers <- tryCatch(Map(x = b, whDep = whDeps, function(x, whDep) x[[whDep + 3]]),
                       error = function(x) "")
      whRm <- unlist(unname(lapply(paste0("^", pkg2, ".*", vers), grep, x = pkg)))
      if (length(whRm) > 0) {
        if (grp[i] == .txtCantFindPackage) {
          # This is the case when a package is archived
          his <- try(tail(pkg_history(pkg2), 1), silent = TRUE)
          if (!is(his, "try-error")) {
            isCRAN <- unlist(whIsOfficialCRANrepo(getOption("repos"), srcPackageURLOnCRAN))
            pth <- file.path("Archive", his$Package, paste0(his$Package, "_", his$Version, ".tar.gz"))
            if (isTRUE(!startsWith(isCRAN, "https"))) isCRAN <- paste0("https://", isCRAN)
            pth <- paste0("url::",file.path(contrib.url(isCRAN), pth))
            packages[whRm] <- pth
          } else {
            messageCantInstallNoVersion(pkg2, verbose = verbose)
          }
        } else {
          if (grp[i] == .txtFailedToBuildSrcPkg) {
            ftbsp <- "FailedToBuildSrcPkg"
            prevFtbsp <- get0(ftbsp, envir = pakEnv())
            if (pkg2 %in% prevFtbsp)
              stop(err)
            # When this error happens, it seems to be because of corrupt local cache
            cache_delete(package = pkg2)
            assign(ftbsp, unique(c(prevFtbsp, pkg2)), envir = pakEnv())
            break
          }

          packages <- packages[-whRm]
          break
        }
      } else {
        stop(err)
      }
    }
  }
  packages
}


pakPkgSetup <- function(pkgs) {

  # rm spaces
  pkgs <- gsub(" {0,3}(\\()(..{0,1}) {0,4}(.+)(\\))", " \\1\\2\\3\\4", pkgs)

  if (TRUE) {
    deps <- list()
    deps <- pkgDep(pkgs)
    # for (pk in pkgs) {
    #   pkLabel <- pk
    #   for (i in 1:2) {
    #     val <- try(pkgDep(pk))
    #     if (is(val, "try-error")) {
    #       pk <- pakErrorHandling(val, pk, pk)
    #     } else {
    #       deps[pkLabel] <- val
    #       break
    #     }
    #   }
    # }

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
        his <- pkg_deps(trimVersionNumber(pkg))
        his <- his[his$package %in% extractPkgName(pkg), ]
        setnames(his, old = "version", new = "Version")
      } else {
        his <- pkg_history(trimVersionNumber(pkg))
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

  whGT <- grep(">", pkgs)

  whHEAD <- grep("\\(HEAD\\)", pkgs)

  whAlreadyColoned <- grep("::", pkgs)

  whNormal <- ind[-sort(c(whEquals, whGT, whLT, whHEAD, whGH, whAlreadyColoned))]

  if (length(whEquals))
    pkgs[whEquals] <- gsub(" {0,3}\\(== {0,4}(.+)\\)", "@\\1", pkgs[whEquals])
  if (length(whLT))
    pkgs[whLT] <- gsub(" {0,3}\\(<= {0,4}(.+)\\)", "@\\1", pkgs[whLT])
  if (length(whHEAD))
    pkgs[whHEAD] <- gsub(" {0,3}\\(HEAD\\)", "", pkgs[whHEAD])

  if (length(whNormal))
    pkgs[whNormal] <- paste0("any::", pkgs[whNormal])

  pkgsForDESCRIPTIONFile <- if (length(whGT)) pkgs[whGT] else character()
  pkgsForDirect <- if (length(whGT)) pkgs[-whGT] else pkgs

  list(DESC = pkgsForDESCRIPTIONFile, direct = pkgsForDirect)
}


RequireForPak <- function(packages, libPaths, doDeps, upgrade, verbose, packagesOrig) {
  requireNamespace("pak")
  envPakCreate() # this will be used to track which packages failed

  packages <- unique(packages)
  packages <- packages[!extractPkgName(packages) %in% .basePkgs]

  pkgs <- list()
  for (i in 1:15) {
  # while(!identical(packages, pkgs) ) {
    pkgs <- packages
    if (length(pkgs)) {
      pkgsList <- pakPkgSetup(pkgs)
      td3 <- tempdir3()
      on.exit({unlink(dirname(td3))}, add = TRUE)
      dfile <- DESCRIPTIONfileFromModule(verbose = -2,
                                         packageFolderName = td3,
                                         "dummy",
                                         md = list(name = "dummy", description = "dummy",
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
      log <- tempfile2(fileext = ".txt")

      withCallingHandlers(
        err <- try(outs <- pak::pak(c(
          paste0("deps::", td3),
          pkgsList$direct
          ), lib = libPaths[1], ask = FALSE,
                                    dependencies = doDeps, # already done in pakPkgSetup # doDeps,
                                    upgrade = upgrade),
                   silent = verbose <= 1)
        , message = function(m) {
          cat(m$message, file = log, append = TRUE)
          if (verbose < 1)
            invokeRestart("muffleMessage")
        }
      )
      if (!is(err, "try-error"))
        break

      # deal with errors
      packages <- pakErrorHandling(err, pkgs, packages)
      if (length(packages) == 0)
        stop(err)

    } else {
      outs <- list()
      break
    }
  }

  pkgDT <- try(as.data.table(outs))
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

  deps <- list()
  # for (pk in pkgs) {
  #   pkLabel <- pk
  #   for (i in 1:2) {
  #     val <- try(pkgDep(pk))
  #     if (is(val, "try-error")) {
  #       pk <- pakErrorHandling(val, pk, pk)
  #     } else {
  #       deps[pkLabel] <- val
  #       break
  #     }
  #   }
  # }

  deps <- Map(pkg = packages, function(pkg) {
    for (i in 1:2) {
      withCallingHandlers({
        val <- try(pak::pkg_deps(trimVersionNumber(pkg), dependencies =
                                   ifelse(any(grepl("suggests", tolower(unlist(which)))), TRUE, NA)))
        if (is(val, "try-error")) {
          pkg <- pakErrorHandling(val, pkg, pkg)
        } else {
          break
        }
      }, message = function(m) {
        if (verbose < 1)
          invokeRestart("muffleMessage")
      }
      )
    }
    val

  }
  )

  if (simplify %in% TRUE) {
    # deps2 <- lapply(deps, function(x) x$ref)

    deps <- Map(dep = deps, nam = names(deps), function(dep, nam) {
      dep$deps <- Map(innerDep = dep$deps, outerPkg = dep$package, function(innerDep, outerPkg) {
        if (!nam %in% outerPkg || !any(tolower(unlist(which)) == "suggests")) {
          innerDep[tolower(innerDep$type) %in%
                     setdiff(tolower(unlist(which)), "suggests"),]
        } else {
          innerDep
        }})
      dep
    })
    deps <- Map(dep = deps, packFullName = packages, function(dep, packFullName) {
      rr <- rbindlist(dep$deps) # only gets 1st order dependencies; still need self
      rr <- unique(rr)
      # rr <- rr[tolower(type) %in%  setdiff(tolower(unlist(which)), "suggests")]
      rr <- packageFullNameFromPkgVers(rr)
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
    field <- Require:::trimRedundancies(unique(c(field, fieldVals)))
  }
  cat(c(paste0(fieldName, ":"), paste("   ", sort(field$packageFullName), collapse = ",\n")),
      sep = "\n", file = dFile, append = TRUE)
}


