#library(Require)
a <- pkgDep("Require", recursive = TRUE)
testit::assert(length(a) == 1)
testit::assert(!isTRUE(all.equal(lapply(a, trimVersionNumber), a)))
a1 <- pkgDep("Require", keepVersionNumber = FALSE, recursive = TRUE) # just names
testit::assert(isTRUE(all.equal(lapply(a1, trimVersionNumber), a1)))

pkg <- "PredictiveEcology/reproducible"
a2 <- pkgDep(pkg) # GitHub
testit::assert(length(a2) == 1)
testit::assert(all(names(a2) == pkg))

pkg <- "PredictiveEcology/reproducible"
b <- pkgDep("PredictiveEcology/reproducible", recursive = TRUE) # GitHub
testit::assert(length(b) == 1)
testit::assert(all(names(b) == pkg))
testit::assert(length(b[[1]]) > length(a1[[1]]))

d <- pkgDep(c("PredictiveEcology/reproducible", "Require")) # GitHub package and local packages
testit::assert(length(d) == 2)
testit::assert(isTRUE(all.equal(a$Require, d$Require)))

e <- pkgDep(c("PredictiveEcology/reproducible", "Require", "plyr")) # GitHub, local, and CRAN packages
testit::assert(length(e) == 3)
testit::assert(isTRUE(all.equal(e[[pkg]], d[[pkg]])))
testit::assert(isTRUE(all.equal(d$Require, e$Require)))

mess <- utils::capture.output(type = "message", f <- pkgDep("Require", depends = TRUE))
testit::assert(isTRUE(any(grepl("Please use", mess))))

mess <- utils::capture.output(type = "message", f <- pkgDep("Require", linkingTo = TRUE))
testit::assert(isTRUE(any(grepl("Please use", mess))))
mess <- utils::capture.output(type = "message", f <- pkgDep("Require", imports = TRUE))
testit::assert(isTRUE(any(grepl("Please use", mess))))
mess <- utils::capture.output(type = "message", f <- pkgDep("Require", suggests = TRUE))
testit::assert(isTRUE(any(grepl("Please use", mess))))

a <- pkgDep("Require", which = "all", recursive = FALSE)
b <- pkgDep("Require", which = "most", recursive = FALSE)
d <- pkgDep("Require", which = TRUE, recursive = FALSE)
testit::assert(isTRUE(all.equal(a, b)))
testit::assert(isTRUE(all.equal(a, d)))

### pkgDepTopoSort
out <- pkgDepTopoSort(c("data.table", "Require"), reverse = TRUE, recursive = TRUE)
knownRevDeps <- list(Require = c("reproducible", "SpaDES", "SpaDES.addins", "SpaDES.core",
                                 "SpaDES.experiment", "SpaDES.tools"),
                     data.table = c("quickPlot",
                                    "reproducible", "Require", "SpaDES", "SpaDES.addins", "SpaDES.core",
                                    "SpaDES.experiment", "SpaDES.tools"))
test <- unlist(lapply(names(out), function(p) {
  setdiff(out[[p]], knownRevDeps[[p]])
}))

testit::assert(length(test) == 0)
