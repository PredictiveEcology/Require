msgPackageViolation <- "Package version violation detected; installing this"
msgIsInUse <- "in use and will not be installed"

GitHubMessage <- 0


messageCantInstallNoVersion <- function(packagesFullName) {
  turquoise(
    paste(unique(packagesFullName), collapse = ", "),
    " could not be installed; package doesn't exist or the version specification cannot be met"
  )
}


msgStdOut <- function(mess, logFile, verbose) {
  pkg <- extractPkgNameFromWarning(mess)
  justPackage <- !identical(mess, pkg)
  cat(blue(mess), file = logFile, append = TRUE)
  appendLF <- endsWith(mess, "\n") %in% FALSE

  mod <- "^\\r\\n"
  while (grepl(mod, mess)) {
    mess <- gsub(mod, "  ", mess)
  }

  omit <- "^.+downloaded binary packages.+$"
  while (grepl(omit, mess)) {
    mess <- gsub(omit, "", mess)
  }

  omit <- "^.+\\.dll.+$"
  if (grepl(omit, mess)) {
    mess <- gsub(omit, "", mess)
  }

  omit <- "^.+rtools.+$"
  if (grepl(omit, mess)) {
    mess <- gsub(omit, "", mess)
  }
  mess1 <- unlist(strsplit(mess, "\\r\\n"))
  mess2 <- unique(mess1)
  if (!identical(mess1, mess2))
    mess <- paste(mess2, sep = "\\r\\n")


  if (nchar(mess)) {
    if (!justPackage || verbose >= 2 || grepl("Warning", mess)) {
      messageVerbose(blue(mess), verbose = verbose, appendLF = appendLF)
    } else {
      messageVerbose(blue("Installed: ", pkg), verbose = verbose, appendLF = TRUE)
    }
  }
}

msgStdErr <- function(mess, logFile, verbose) {
  messOrig <- mess # used for debugging -- not necessary in production
  cat(greyLight(mess), file = logFile, append = TRUE)
  appendLF <- endsWith(mess, "\n") %in% FALSE
  if (verbose <= 1) {

    mess <- unlist(strsplit(mess, "\r*\n"))

    omits <- c("\\(as 'lib' is unspecified\\)",
               "(.+)The downloaded source packages.+",
               "Content",
               "=+",
               "g\\+\\+",
               "^$",
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



    if (FALSE) {
      omit <- "\\(as 'lib' is unspecified\\)"
      if (grepl(omit, mess))
        mess <- gsub(paste0("\\r\\n", omit), "", mess)

      omit <- "(^.+\\*source\\*.+.\\.{3,3}).+"
      if (grepl(omit, mess))
        mess <- gsub(omit, "\\1", mess)

      omit <- "(.+)The downloaded source packages.+"
      if (grepl(omit, mess))
        mess <- gsub(omit, "\\1", mess)
      omit <- ".+(packaged installation.+)$"
      if (grepl(omit, mess))
        mess <- gsub(omit, "\\1", mess)

      omit <- "^(.+)+Content.+=+(\\r\\n)*(.*)$"
      while (grepl(omit, mess)) {
        mess <- gsub(omit, "\\1\\3", mess)
      }

      omit <- "=+(\r\n)*"
      while (grepl(omit, mess)) {
        mess <- gsub(omit, "", mess)
      }

      mod <- "\\r\\n\\r\\n"
      while (grepl(mod, mess)) {
        mess <- gsub(mod, "\r\n", mess)
      }
      # omit <- "^(.+)\\r\\n(downloaded.+)$"
      # while (grepl(omit, mess)) {
      #   mess <- gsub(omit, "\\1 ... \\2", mess)
      # }
    }

    mess <- paste(mess, collapse = "\r\n")
    appendLF <- endsWith(mess, "\n") %in% FALSE && nchar(mess) != 0
    if (length(mess)) {
      # if (grepl("nstalling.+package", mess) | grepl("ERROR|halted|DONE|downloaded|trying", mess)) {
        messageVerbose(greyLight(mess), verbose = verbose, appendLF = appendLF)
      # }
    }
  } else {
    messageVerbose(greyLight(mess), verbose = verbose, appendLF = appendLF)
  }
}


msgStdOutForBuild <- function(mess, logFile, verbose) {
  pkg <- extractPkgNameFromWarning(mess)
  cat(blue(mess), file = logFile, append = TRUE)
  appendLF <- endsWith(mess, "\n") %in% FALSE
  if (verbose >= 2) {
    messageVerbose(blue(mess), verbose = verbose, appendLF = appendLF)
  } else {
    if (grepl("building", mess)) {
      mess <- gsub(".+(building.+)", "\\1", mess)
      messageVerbose(blue(mess), appendLF = appendLF)
    }
  }
}


msgStdErrForBuild <- function(mess, logFile, verbose) {
  cat(greyLight(mess), file = logFile, append = TRUE)
  appendLF <- endsWith(mess, "\n") %in% FALSE
  if (verbose <= 1) {
    appendLF <- endsWith(mess, "\n") %in% FALSE
    messageVerbose(greyLight(mess), verbose = verbose, appendLF = appendLF)
  } else {
    messageVerbose(greyLight(mess), verbose = verbose, appendLF = appendLF)
  }
}

