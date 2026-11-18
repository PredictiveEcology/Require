# Load check for cloned packages (Require.cloneFrom).
#
# Every clone made in a Require()/Install() call is loaded, after the whole
# install, by one fresh R process. The process records each result in a status
# file in the destination library ("._RequireCloneCheck_<stamp>", tab
# separated: Package, Version, From, Mtime, Status, Reason), rewriting it after
# each package. In an interactive session it runs in the background and the
# next Require()/Install() on that library acts on the file; otherwise the call
# waits and acts on it at once. Acting means: delete each clone that failed to
# load (only the project's link; the source copy is untouched) and install it
# normally. A failed package+version is never cloned into that library again.
# A check whose file has not been updated for `cloneCheckStaleSecs` died (for
# example, a crash while loading): its unfinished packages count as failed.

.txtCloneCheckFile <- "._RequireCloneCheck"
cloneCheckStaleSecs <- 300

cloneCheckFiles <- function(lib) {
  dir(lib, pattern = paste0("^", .txtCloneCheckFile, "_[^.]+$"), all.files = TRUE,
      full.names = TRUE)
}

cloneCheckRead <- function(f) {
  st <- tryCatch(utils::read.delim(f, colClasses = "character", quote = "", na.strings = character()),
                 error = function(e) NULL)
  if (is.null(st) || !all(c("Package", "Version", "Mtime", "Status") %in% names(st))) return(NULL)
  st
}

cloneCheckWrite <- function(st, f) {
  tmp <- paste0(f, ".tmp")
  utils::write.table(st, tmp, sep = "\t", quote = FALSE, row.names = FALSE)
  file.rename(tmp, f)
}

# Called by cloneCopies(): remember what was cloned into `toLib` in this call.
cloneCheckRecord <- function(cp, toLib) {
  desc <- file.path(toLib, cp[["Package"]], "DESCRIPTION")
  ok <- file.exists(desc)
  if (!any(ok)) return(invisible())
  rec <- data.frame(Package = cp[["Package"]][ok], Version = cp[["Version"]][ok],
                    From = cp[["LibPath"]][ok], Mtime = format(as.numeric(file.mtime(desc[ok])), digits = 15),
                    Status = "untested", Reason = "", stringsAsFactors = FALSE)
  pending <- get0("cloneCheckPending", envir = pkgEnv(), inherits = FALSE)
  pending[[toLib]] <- rbind(pending[[toLib]], rec)
  assign("cloneCheckPending", pending, envir = pkgEnv())
  invisible()
}

# package + version pairs that failed the check in `lib`: never cloned there again
cloneCheckExcluded <- function(lib) {
  if (is.null(lib)) return(character())
  unlist(lapply(cloneCheckFiles(lib), function(f) {
    st <- cloneCheckRead(f)
    if (is.null(st)) return(NULL)
    st <- st[st[["Status"]] %in% c("fail", "excluded"), , drop = FALSE]
    paste(st[["Package"]], st[["Version"]])
  }))
}

# The script the fresh R process runs: base R only (Require may not be on its
# library paths). Arguments: the status file, then the library paths.
cloneCheckScript <- function() {
  c('args <- commandArgs(TRUE); f <- args[1]; .libPaths(args[-1])',
    'st <- utils::read.delim(f, colClasses = "character", quote = "", na.strings = character())',
    'for (i in which(st$Status == "untested")) {',
    '  res <- tryCatch({ loadNamespace(st$Package[i]); "" }, error = function(e) conditionMessage(e))',
    '  st$Status[i] <- if (identical(res, "")) "ok" else "fail"',
    '  st$Reason[i] <- gsub("[\\t\\r\\n]+", " ", res)',
    '  tmp <- paste0(f, ".tmp")',
    '  utils::write.table(st, tmp, sep = "\\t", quote = FALSE, row.names = FALSE)',
    '  file.rename(tmp, f)',
    '}')
}

# After the install: start the load check on this call's clones. Returns the
# status file, invisibly; with `wait`, after the check has finished.
cloneCheckStart <- function(libPaths, wait = !interactive(), verbose = getOption("Require.verbose")) {
  lib <- libPaths[1]
  pending <- get0("cloneCheckPending", envir = pkgEnv(), inherits = FALSE)
  rec <- pending[[lib]]
  pending[[lib]] <- NULL
  assign("cloneCheckPending", pending, envir = pkgEnv())
  if (!NROW(rec)) return(invisible())
  stamp <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", Sys.getpid(), "_",
                  sample.int(1e6, 1))
  f <- file.path(lib, paste0(.txtCloneCheckFile, "_", stamp))
  cloneCheckWrite(rec, f)
  script <- paste0(f, ".R")
  writeLines(c(cloneCheckScript(), paste0('unlink("', gsub("\\\\", "/", script), '")')), script)
  messageVerbose("  -- Checking that the ", NROW(rec), " cloned package", if (NROW(rec) > 1) "s",
                 " load", if (!wait) " (in the background; any that fail are reinstalled by the next Require()/Install() here)",
                 verbose = verbose)
  system2(file.path(R.home("bin"), "Rscript"),
          c("--vanilla", shQuote(script), shQuote(f), shQuote(libPaths)),
          wait = wait, stdout = FALSE, stderr = FALSE)
  invisible(f)
}

# Act on finished (or dead) checks in `lib`: delete each clone that failed to
# load and is still the copy that was checked. Returns the deleted packages.
cloneCheckRepair <- function(lib, verbose = getOption("Require.verbose")) {
  removed <- character()
  for (f in cloneCheckFiles(lib)) {
    st <- cloneCheckRead(f)
    if (is.null(st)) next
    untested <- st[["Status"]] %in% "untested"
    if (any(untested)) {
      age <- as.numeric(difftime(Sys.time(), file.mtime(f), units = "secs"))
      if (age < cloneCheckStaleSecs) next # still running
      st[["Status"]][untested] <- "fail"
      st[["Reason"]][untested] <- "the load check stopped before reaching this package"
    }
    bad <- st[["Status"]] %in% "fail"
    if (any(bad)) {
      desc <- file.path(lib, st[["Package"]], "DESCRIPTION")
      sameCopy <- bad & file.exists(desc) &
        abs(as.numeric(file.mtime(desc)) - as.numeric(st[["Mtime"]])) < 1
      if (any(sameCopy)) {
        messageVerbose(
          "  -- These cloned packages failed to load and will be installed instead:\n",
          paste0("     ", st[["Package"]][sameCopy], " ", st[["Version"]][sameCopy], ": ",
                 substr(st[["Reason"]][sameCopy], 1, 200), collapse = "\n"),
          verbose = verbose, verboseLevel = 0)
        unlink(file.path(lib, st[["Package"]][sameCopy]), recursive = TRUE)
        removed <- c(removed, st[["Package"]][sameCopy])
      }
      st[["Status"]][bad] <- "excluded"
    }
    st <- st[st[["Status"]] %in% "excluded", , drop = FALSE]
    if (NROW(st)) cloneCheckWrite(st, f) else unlink(f)
  }
  unique(removed)
}

# Compiled copies are cloned only from a library on a local disk: one on a
# network drive may have been built on another machine against other system
# libraries. `df -l <dir>` lists the file system only when it is local; any
# other outcome (an error, no df) counts as not local.
isLocalLib <- function(libs) {
  vapply(libs, function(l) {
    out <- tryCatch(suppressWarnings(system2("df", c("-l", shQuote(l)), stdout = TRUE, stderr = FALSE)),
                    error = function(e) structure(character(), status = 1L))
    is.null(attr(out, "status")) && length(out) >= 2
  }, logical(1), USE.NAMES = FALSE)
}
