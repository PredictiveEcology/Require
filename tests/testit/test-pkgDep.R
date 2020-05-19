library(Require)
a <- pkgDep("Require", recursive = TRUE)
testit::assert(length(a) == 1)
testit::assert(!isTRUE(all.equal(lapply(a, trimVersionNumber), a)))
a <- pkgDep("Require", keepVersionNumber = FALSE, recursive = TRUE) # just names
testit::assert(isTRUE(all.equal(lapply(a, trimVersionNumber), a)))

pkg <- "PredictiveEcology/reproducible"
a <- pkgDep(pkg) # GitHub
testit::assert(length(a) == 1)
testit::assert(all(names(a) == pkg))

pkg <- "PredictiveEcology/reproducible"
b <- pkgDep("PredictiveEcology/reproducible", recursive = TRUE) # GitHub
testit::assert(length(b) == 1)
testit::assert(all(names(b) == pkg))
testit::assert(length(b[[1]]) > length(a)[[1]])


d <- pkgDep(c("PredictiveEcology/reproducible", "Require")) # GitHub package and local packages
testit::assert(length(d) == 2)
testit::assert(isTRUE(all.equal(a[[1]], d[[1]])))

e <- pkgDep(c("PredictiveEcology/reproducible", "Require", "plyr")) # GitHub, local, and CRAN packages
testit::assert(length(e) == 3)
testit::assert(isTRUE(all.equal(a[[pkg]],
                                d[[pkg]])))
testit::assert(isTRUE(all.equal(d$Require, e$Require)))

mess <- capture.output(type = "message", f <- pkgDep("Require", depends = TRUE))
testit::assert(isTRUE(any(grepl("Please use", mess))))


a <- pkgDep("Require", which = "all", recursive = FALSE)
b <- pkgDep("Require", which = "most", recursive = FALSE)
d <- pkgDep("Require", which = TRUE, recursive = FALSE)
testit::assert(isTRUE(all.equal(a, b)))
testit::assert(isTRUE(all.equal(a, d)))
