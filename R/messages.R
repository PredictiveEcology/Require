msgPackageViolation <- "Package version violation detected; installing this"
.txtMsgIsInUse <- "in use and will not be installed"
.txtCannotOpenFile <- "cannot open file.+DESCRIPTION.+No such file or directory"
comma <- ", "

.txtGetArchiveDetailsInner <- "getArchiveDetailsInner"
.txtGetSHAfromGitHub <- "getSHAfromGitHub"
.txtPkgHasGHP <- "hasGHP"
.txtPkgFailed <- "package failed: "
.txtPleaseRestart <- "Please restart R"
.txtPleaseChangeReqdVers <- "Please change required version"
.txtRetrying <- "; retrying ... "
.txtInternetExistsTime <- "internetExistsTime"
.txtCntInstllDep <- "Can't install dependency"
.txtConflictsWith <- "Conflicts with"

.txtMissingValueWhereTFNeeded <- "missing value where TRUE/FALSE needed"
.txtUnableToAccessIndex <- "unable to access index"
.txtGitHub <- "GitHub"
.txtCouldNotBeInstalled <- "could not be installed"
.txtCldNotSlvPkgDeps <- "Could not solve package dependencies"
.txtFailedToDLFrom <- "Failed to download.+from"
.txtPakCurrentlyPakNoSnapshots <- "Currently, pak is unlikely to work for package snapshots"
.txtPakNoPkgCalledPak <- "there is no package called 'pak'"
.txtUnknownArchiveType <- "unknown archive type"

# "installation of package 'ccissr' had non-zero exit status"
# "installation of 2 packages failed"
.txtInstallationNonZeroExit <- "installation of.+had non-zero exit status"

# "installation of.+failed" # "installation of 2 packages failed:"
.txtInstallationPkgFailed <- "installation of.+failed"

.txtGitHubCols <- list()
.txtGitHubCols$Br <- "Branch"
.txtGitHubCols$Repo <- "Repo"
.txtGitHubHasSubFolder <- "hasSubFolder"
.txtGitHubCols$GSF <- "GitSubFolder"
.txtGitHubCols$Acct <- "Account"

.txtGitHubParsedCols <- unname(unlist(.txtGitHubCols))
# .txtGitHubParsedCols <- c(.txt, "Repo", "Branch", "Account")

.txtDontInstall <- "dontInstall"
.txtInstall <- "install"
.txtInstallingColon <- "Installing:"

.txtLocal <- "Local"
.txtNoLocal <- "noLocal"

.txtShaUnchangedNoInstall <- "SHA unchanged; no install"

.txtNoneAvailable <- "noneAvailable"



messageCantFind <- function(br, acct, repo)
  paste0("Can't find ", paste0(acct, "/", repo, "@", br), "; \n -- does it exist? --")

GitHubMessage <- 0


messageCantInstallNoVersion <- function(packagesFullName) {
  N <- unique(packagesFullName)
  dd <- singularPlural(c("doesn't", "don't"), l = N)
  vv <- singularPlural(c("specification", "specifications"), l = N)
  turquoise(
    paste(unique(packagesFullName), collapse = comma),
    " ", .txtCouldNotBeInstalled, "; package ", dd ,
    " exist or the version ", vv," cannot be met"
  )
}


msgPleaseChangeRqdVersion <- function(Package, ineq, newVersion) {
  paste0(.txtPleaseChangeReqdVers, " e.g., ",
         paste0(Package, " (", ineq, newVersion,")"))
}


msgStdOut <- function(mess, logFile, verbose) {
  # pkg <- extractPkgNameFromWarning(mess)
  # justPackage <- !identical(mess, pkg) && !grepl("\\/|\\\\", pkg)
  cat(blue(mess), file = logFile, append = TRUE, fill = TRUE)

  messOrig <- mess
  appendLF <- endsWith(mess, "\n") %in% FALSE
  if (verbose <= 1) {
    # if (grepl("\\<sf\\>|\\<terra\\>|\\<RcppParallel\\>|\\<units\\>", messOrig)) browser()
    errs <- "Error in dyn.load|Execution halted|Aborted"
    if (!any(grepl(errs, mess))) {

      omitPreSplitMess <- c("configure script", "configure", "config.status",
                            "compilation", "lpcre2",
                            "checking for pkg-config")
      if (grepl(".+installing \\*source\\*\\spackage", mess) %in% FALSE) {
        for (om in omitPreSplitMess) {
          if (grepl(om, mess)) {
            mess <- grep(om, mess, invert = TRUE, value = TRUE)
            # spinner |\-/-
            if (FALSE) {
              pe <- pkgEnv()
              if (is.null(pe[["spinner"]])) assign("spinner", "|", envir = pe)

              spinner <- get("spinner", envir = pe)
              spinner <- ifelse (spinner == "|", "\\",
                                 ifelse(spinner == "\\", "-",
                                        ifelse(spinner == "-", "/",
                                               ifelse(spinner == "/", "|"))))
              assign("spinner", spinner, envir = pe)
              # mess <- paste0("\b\b", spinner)
            }
            if (length(mess) == 0) mess <- ""
            # mess <- ""
          }
        }
      }



      mod <- "^\\r\\n"
      while (grepl(mod, mess)) {
        mess <- gsub(mod, "  ", mess)
      }

      # omit <- "^.+downloaded binary packages.+$"
      # while (grepl(omit, mess)) {
      #   mess <- gsub(omit, "", mess)
      # }

      mess <- unlist(strsplit(mess, "\\r\\n|\\n"))
      mess2 <- unique(mess)
      if (!identical(mess, mess2))
        mess <- paste(mess2, sep = "\\r\\n")

      omits <- c("\\(as 'lib' is unspecified\\)",
                 "(.+)The downloaded source packages.+",
                 "Content",
                 "^.+downloaded binary packages.+$",
                 "^.+\\.dll.+$",
                 "^=+", # we want the ones that say `>=` because they are errors
                 "g\\+\\+",
                 "^$",
                 "^\\*{2,5}",
                 "^.+rtools.+$",
                 # "MD5 sums",
                 "was already a binary package and will not be rebuilt",
                 "creating tarball",
                 "packaged installation of",
                 "using .+ compiler",
                 "gcc",
                 "\\.o")

      for (om in omits) {
        mess <- grep(om, mess, invert = TRUE, value = TRUE)
      }

      messOrig <- mess
      keeps <- c(
        # "begin installing package",
        "\\* installing \\*.+\\* package",
        "\\* DONE \\(")
      keepsInds <- lapply(keeps, function(ke) grep(ke, messOrig))
      mess <- messOrig[sort(unlist(keepsInds))]

      anyErrors <- c("ERROR", "\\<error\\>")
      anyErrs <- lapply(anyErrors, function(ae) grep(ae, messOrig))

      if (length(anyErrs)) {
        verbose <- min(1, verbose)
        mess <- messOrig[sort(c(unlist(keepsInds), unlist(anyErrs)))]
      }

      if (length(mess) > 1)
        mess <- paste(mess, collapse = "\r\n")

      if (length(mess) == 1)
        appendLF <- TRUE

    }
  }

  if (length(mess) || !isWindows()) {
    msgWithLineFeedIterative(blue(mess), name = ".messInstPkgCounter",
                             verbose = installPackageVerbose(verbose), appendLF = appendLF,
                             reset = TRUE)
  } else {
    if (verbose == 1) {
      mess <- paste(paste0(extractPkgNameFromWarning(messOrig), " "), collapse = "")
      msgWithLineFeedIterative(mess, appendLF = FALSE, name = ".messInstPkgCounter", verbose = verbose)
    }
  }

}

msgStdErr <- function(mess, logFile, verbose) {
  messOrig <- mess # used for debugging -- not necessary in production
  cat(greyLight(mess), file = logFile, append = TRUE, fill = TRUE)
  appendLF <- FALSE
  if (verbose <= 1) {
    errs <- "ERROR: lazy loading failed for package"
    if (!any(grepl(errs, mess))) {

      omitPreSplitMess <- c("using C\\+\\+ compiler")
      for (om in omitPreSplitMess) {
        if (grepl(om, mess)) {
          so <- "staged installation"
          mess <- strsplit(mess, split = so)
          newMess <- mess[[1]]
          mess <- if (length(newMess) > 1) paste0(newMess[[1]], so) else newMess
          # mess <- grep(om, mess, invert = TRUE, value = TRUE)

        }
      }

      omitPreSplitMess <- "\\.h:"
      for (om in omitPreSplitMess) {
        if (grepl(om, mess)) {
          mess <- grep(om, mess, invert = TRUE, value = TRUE)
          # spinner |\-/-
          pe <- pkgEnv()
          if (is.null(pe[["spinner"]])) assign("spinner", "|", envir = pe)

          spinner <- get("spinner", envir = pe)
          spinner <- ifelse (spinner == "|", "\\",
                             ifelse(spinner == "\\", "-",
                                    ifelse(spinner == "-", "/",
                                           ifelse(spinner == "/", "|"))))
          assign("spinner", spinner, envir = pe)
          mess <- paste0("\b\b", spinner)
        }
      }


      mess <- unlist(strsplit(mess, "\r*\n"))

      omits <- c("\\(as .lib. is unspecified\\)",
                 "(.+)The downloaded source packages.+",
                 "Content",
                 "=+",
                 "g\\+\\+",
                 "^$",
                 "installing to",
                 "^\\*{2,5}",
                 "MD5 sums",
                 "was already a binary package and will not be rebuilt",
                 "creating tarball",
                 "packaged installation of",
                 "using .+ compiler",
                 "gcc")
      for (om in omits) {
        mess <- grep(om, mess, invert = TRUE, value = TRUE)
      }


      # if (grepl(omit, mess))
      #   mess <- gsub(paste0("\\r\\n", omit), "", mess)

      keeps <- c("(^.+\\*source\\*.+.\\.{3,3}).+",
                 ".+(packaged installation.+)$")
      for (keep in keeps) {
        whKeep <- grepl(keep, mess)
        if (any(whKeep))
          mess <- mess[whKeep]
      }

      rmEOL <- c("downloaded", "DONE \\(") # DONE already has dots
      dots <- c(" ... ", "")

      for (ind in seq(rmEOL)) {
        # first check if it is first mess ... if yes, then \b it
        if (identical(grep(rmEOL[ind], mess), 1L) ) {
          mess[1] <- paste0("\b", dots[ind], mess[1])
        }
        rm <- grep(paste0("^", rmEOL[ind]), mess)
        if (length(rm)) {
          pre <- rm - 1
          mess[pre] <- paste0(mess[pre], "...", mess[rm], sep = "\r\n")
          mess[rm] <- ""
          mess <- grep("^$", mess, invert = TRUE, value = TRUE)
        }
      }

      converts <- c("\\r\\n\\r\\n", "\\* DONE (.+)")
      converts2 <- c("\r\n", " DONE")
      for (i in seq(converts)) {
        while (any(grepl(converts[i], mess))) {
          mess <- gsub(converts[i], converts2[i], mess)
        }
      }

      mess <- paste(mess, collapse = "\r\n")
      appendLF <- endsWith(mess, "\n") %in% FALSE && nchar(mess) != 0
    }
  }

  pe <- pkgEnv()
  if (is.null(pe$.messInstPkgCounter)) pe$.messInstPkgCounter <- 0
  if (length(mess)) {
    messageVerbose(greyLight(mess), verbose = installPackageVerbose(verbose), appendLF = appendLF)
    pe$.messInstPkgCounter <- 0 # reset
  }
}


msgStdOutForBuild <- function(mess, logFile, verbose) {
  pkg <- extractPkgNameFromWarning(mess)
  cat(blue(mess), file = logFile, append = TRUE, fill = TRUE)
  appendLF <- endsWith(mess, "\n") %in% FALSE
  if (grepl("building", mess)) {
    mess <- gsub(".+(building.+)", "\\1", mess)
  }
  if (verbose >= 2) {
    messageVerbose(blue(mess), verbose = installPackageVerbose(verbose), appendLF = appendLF)
  }
}


msgStdErrForBuild <- function(mess, logFile, verbose) {
  cat(greyLight(mess), file = logFile, append = TRUE, fill = TRUE)
  appendLF <- endsWith(mess, "\n") %in% FALSE
  if (verbose <= 1) {
    appendLF <- endsWith(mess, "\n") %in% FALSE
  }
  messageVerbose(greyLight(mess), verbose = installPackageVerbose(verbose), appendLF = appendLF)
}

msgShaNotChanged <- function(Account, Repo, Branch) {
  paste0("Skipping install of ", paste0(Account, "/", Repo, "@", Branch),
         ", the SHA1 has not changed from last install")
}


msgWithLineFeedIterative <- function(mess, appendLF = FALSE, pe = pkgEnv(),
                                     lineWidth = getOptionWidthWithBuffer(),
                                     name = ".messInstPkgCounter", reset = FALSE,
                            verbose) {
  if (is.null(pe[[name]])) pe[[name]] <- 0
  if (pe[[name]] > lineWidth) {
    pe[[name]] <- 0 # reset
    appendLF <- TRUE
  }
  pe[[name]] <- pe[[name]] + nchar(mess) + 1 # The +1 is for the space
  messageVerbose(mess, verbose = verbose, appendLF = appendLF)
  if (isTRUE(reset))
    pe[[name]] <- 0 # reset
  return(invisible())
}


paste0WithLineFeed <- function(mess, lineWidth = getOptionWidthWithBuffer()) {

  lapply(mess, function(m) {
    if (nchar(m) > lineWidth) {
      nch <- lineWidth
      m <- gsub(paste0('(.{1,',nch,'})(\\s|$)'), '\\1\n', m)
    }
    # remove last one
    m <- gsub("\n$", "", m)
    m
  })
  mess
}

getOptionWidthWithBuffer <- function(buff = 10) getOption("width") - 10
