RequireDeps <- c("data.table", "remotes", "tools", "utils", "versions", "methods",
                 "stats", "grDevices", "graphics")
helpers <- dir(pattern = "helper", full.names = TRUE)
out <- lapply(helpers, source, local = environment())

# testInitOut <- testInit()
tmpdir <- if (Sys.info()["user"] != "emcintir") {
  file.path(tempdir(), Require:::rndstr(1,6))
} else {
  "c:/Eliot/TempLib5"
}

checkPath(tmpdir, create = TRUE)
oldLibPaths <- .libPaths()
on.exit(.libPaths(oldLibPaths))
.libPaths(tmpdir)

Require2(c("bitops (<=1.0-5)", "Holidays (>=0.0.1)", "achubaty/amc@development", "PredictiveEcology/LandR (>=0.0.1)",
           "PredictiveEcology/LandR (>=0.0.2)", "Holidays (>=0.0.2)", "ianmseddy/LandR.CS (<=0.0.1)"))

pkgs <- list(c("SpaDES.core (>=0.9)",
               "PredictiveEcology/map@development (>= 4.0.9)",

               "achubaty/amc@development (>=0.1.5)", "data.table (>=100.0)",
               "digest (>=0.6.23)", "PredictiveEcology/LandR@development (>= 1.0.2)", "versions (>=0.3)",
               "fastdigest (>=0.0.0.9)", "PredictiveEcology/map@development (>= 0.1.0.9)",
               "achubaty/amc@development (>=0.0.0.9)", "data.table (>=0.0.0.9)",
               "PredictiveEcology/LandR@development(>= 0.0.0.9)", "fastdigest (>=1000.0.0.8)",
               "fastdigest", "quickPlot", "testthat",
               "PredictiveEcology/map@development (>= 0.0.0.9)",
               "PredictiveEcology/map@development (>= 0.0.0.10)",
               "PredictiveEcology/map@development (>= 1111.0.9)",
               "PredictiveEcology/map@master (>= 0.0.0.9)",
               "PredictiveEcology/map@master (>= 0.0.0.10)"
),
c("SpaDES.core (>=0.9)",
  "PredictiveEcology/map@development (>= 5.0.0.9)",
  "achubaty/amc@development (>=0.1.5)",
  "data.table (>=100.0)",
  paste0("digest (>=", packageVersion("digest"),")"),
  "PredictiveEcology/LandR@development (>= 1.0.2)"),
c("fastdigest (>=0.0.0.9)",
  "PredictiveEcology/map@development (>= 0.0.0.9)",
  "achubaty/amc@development (>=0.0.0.9)",
  "data.table (>=0.0.0.9)",
  paste0("digest (>=", packageVersion("digest"),")"),
  "PredictiveEcology/LandR@development(>= 0.0.0.9)"),
# Multiple conflicting version numbers, and with NO version number
c("fastdigest (>=0.0.0.8)", "fastdigest (>=0.0.0.9)", "fastdigest"), #"quickPlot", "testthat"),
c("fastdigest (>=1000.0.0.8)", "fastdigest (>=0.0.0.9)", "fastdigest"),
#          "quickPlot", "testthat"),
c("fastdigest (>=0.0.0.9)",
  "PredictiveEcology/map@development (>= 0.0.0.9)",
  "PredictiveEcology/map@development (>= 0.0.0.10)",
  "PredictiveEcology/map@development (>= 110.0.9)",
  "achubaty/amc@development (>=0.0.0.9)",
  "data.table (>=0.0.0.9)",
  paste0("digest (>=", packageVersion("digest"),")"),
  "PredictiveEcology/LandR@development(>= 0.0.0.9)"),
"Holidays (>=1000.3.1)",
c("Holidays (>=1.0.1)", "fpCompare"),
"Holidays (>=1.3.1)"
)
#   options("reproducible.Require.install" = TRUE)
#   Sys.setenv("R_REMOTES_UPGRADE" = "never")
i <- 0

for (pkg in pkgs) {
  out <- unloadNSRecursive()
  i <- i + 1
  print(i)
  outFromRequire <- Require2(pkg, repos = repo, standAlone = FALSE)
  testit::assert(all(outFromRequire))

}
