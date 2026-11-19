# Run by Require (R/cloneCheck.R) in a fresh R process to check that each
# cloned package loads. Base R only: Require may not be on these library paths.
# Arguments: the status file, then the library paths.
args <- commandArgs(TRUE)
f <- args[1]
.libPaths(args[-1])
st <- utils::read.delim(f, colClasses = "character", quote = "", na.strings = character())
for (i in which(st$Status == "untested")) {
  res <- tryCatch({ loadNamespace(st$Package[i]); "" }, error = function(e) conditionMessage(e))
  st$Status[i] <- if (identical(res, "")) "ok" else "fail"
  st$Reason[i] <- gsub("[\t\r\n]+", " ", res)
  tmp <- paste0(f, ".tmp")
  utils::write.table(st, tmp, sep = "\t", quote = FALSE, row.names = FALSE)
  file.rename(tmp, f)
}
