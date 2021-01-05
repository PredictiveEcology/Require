origLibPathsAllTests <- .libPaths()

Sys.setenv("R_REMOTES_UPGRADE" = "never")
Sys.setenv('CRANCACHE_DISABLE' = TRUE)
outOpts <- options("Require.persistentPkgEnv" = TRUE,
                   "install.packages.check.source" = "never",
                   "install.packages.compile.from.source" = "never",
                   "Require.unloadNamespaces" = TRUE)
if (Sys.info()["user"] == "emcintir") {
  outOpts2 <- options("Require.Home" = "~/GitHub/Require",
                      "Require.RPackageCache" = "~/._RPackageCache/")
} else {
  outOpts2 <- options("Require.Home" = "~/GitHub/PredictiveEcology/Require")
}

#library(Require)
a <- pkgDep("Require", recursive = TRUE)
testit::assert({length(a) == 1})
testit::assert({!isTRUE(all.equal(lapply(a, trimVersionNumber), a))})
a1 <- pkgDep("Require", keepVersionNumber = FALSE, recursive = TRUE) # just names
testit::assert({isTRUE(all.equal(lapply(a1, trimVersionNumber), a1))})

pkg <- "PredictiveEcology/reproducible"
a2 <- pkgDep(pkg) # GitHub
testit::assert({length(a2) == 1})
testit::assert({all(names(a2) == pkg)})

b <- pkgDep(pkg, recursive = TRUE) # GitHub
testit::assert({length(b) == 1})
testit::assert({all(names(b) == pkg)})
testit::assert({length(b[[1]]) > length(a1[[1]])})

bAlt <- pkgDepAlt(pkg, recursive = TRUE, purge = TRUE) # GitHub
testit::assert({length(setdiff(extractPkgName(b[[1]]), extractPkgName(bAlt[[1]]))) == 0})

pkg2 <- c(pkg, "Require")
d <- pkgDep(pkg2) # GitHub package and local packages
testit::assert({length(d) == 2})
testit::assert({isTRUE(all.equal(a$Require, d$Require))})

dAlt <- pkgDepAlt(pkg2, recursive = TRUE) 
testit::assert({length(setdiff(extractPkgName(d[[1]]), extractPkgName(dAlt[[1]]))) == 0})
testit::assert({length(setdiff(extractPkgName(d[[2]]), extractPkgName(dAlt[[2]]))) == 0})
testit::assert({length(d) == length(dAlt)})
testit::assert({names(d) == names(dAlt)})

pkg3 <- c(pkg2, "plyr")
e <- pkgDep(pkg3) # GitHub, local, and CRAN packages
testit::assert({length(e) == 3})
testit::assert({isTRUE(all.equal(e[[pkg]], d[[pkg]]))})
testit::assert({isTRUE(all.equal(d$Require, e$Require))})

eAlt <- pkgDepAlt(pkg3, recursive = TRUE) 
testit::assert({length(setdiff(extractPkgName(e[[1]]), extractPkgName(eAlt[[1]]))) == 0})
testit::assert({length(setdiff(extractPkgName(e[[2]]), extractPkgName(eAlt[[2]]))) == 0})
testit::assert({length(setdiff(extractPkgName(e[[3]]), extractPkgName(eAlt[[3]]))) == 0})
testit::assert({length(e) == length(eAlt)})
testit::assert({names(e) == names(eAlt)})

a <- pkgDep("Require", which = "all", recursive = FALSE)
b <- pkgDep("Require", which = "most", recursive = FALSE)
d <- pkgDep("Require", which = TRUE, recursive = FALSE)
e <- pkgDep("Require", recursive = FALSE)
testit::assert({isTRUE(all.equal(a, b))})
testit::assert({isTRUE(all.equal(a, d))})
testit::assert({!isTRUE(all.equal(a, e))})
aAlt <- pkgDepAlt("Require", which = "all", recursive = FALSE, purge = TRUE)
bAlt <- pkgDepAlt("Require", which = "most", recursive = FALSE)
dAlt <- pkgDepAlt("Require", which = TRUE, recursive = FALSE)
eAlt <- pkgDepAlt("Require", recursive = FALSE)
testit::assert({isTRUE(all.equal(a, aAlt))})
testit::assert({isTRUE(all.equal(b, bAlt))})
testit::assert({isTRUE(all.equal(d, dAlt))})
testit::assert({isTRUE(all.equal(e, eAlt))})

### pkgDepTopoSort
out <- pkgDepTopoSort(c("data.table", "Require"), reverse = TRUE, recursive = TRUE)
knownRevDeps <- list(Require = c("reproducible", "SpaDES", "SpaDES.addins", "SpaDES.core",
                                 "SpaDES.experiment", "SpaDES.tools"),
                     data.table = c("quickPlot",
                                    "reproducible", "Require", "SpaDES", "SpaDES.addins", "SpaDES.core",
                                    "SpaDES.experiment", "SpaDES.tools"))
test <- unlist(lapply(names(out), function(p) {
  setdiff(knownRevDeps[[p]], out[[p]])
}))
if (interactive()) {
  testit::assert({length(test) == 0})
}

pkg <- c("Require (==0.0.6)")
d <- pkgDepAlt(pkg) # GitHub package and local packages
testit::assert({identical(sort(c("data.table (>= 1.10.4)", "remotes")), sort(d[[1]]))})

if (!identical(origLibPathsAllTests, .libPaths()))
  Require::setLibPaths(origLibPathsAllTests, standAlone = TRUE, exact = TRUE)
options(outOpts)
options(outOpts2)
