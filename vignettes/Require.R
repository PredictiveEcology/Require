## ----eval=FALSE---------------------------------------------------------------
# # No version specifications — CRAN version installed, or nothing if already installed
# Require::Install(c("PredictiveEcology/reproducible@development", "reproducible"))
# 
# # `HEAD` after the GitHub ref forces the tip of the development branch
# Require::Install(c("PredictiveEcology/reproducible@development (HEAD)", "reproducible"))
# 
# # Same: `HEAD` after the package name (of either form) forces the tip
# Require::Install(c("PredictiveEcology/reproducible@development", "reproducible (HEAD)"))
# 
# # No conflict: version requirement is satisfiable by the named branch
# Require::Install(c("PredictiveEcology/reproducible@modsForLargeArchives (>= 2.0.10.9010)",
#                    "PredictiveEcology/reproducible (>= 2.0.10)"))
# 
# # Even if a branch doesn't exist, no error if a later requirement names a different branch
# Require::Install(c("PredictiveEcology/reproducible@modsForLargeArchives (>= 2.0.10.9010)",
#                    "PredictiveEcology/reproducible@validityTest (>= 2.0.9)"))


## ----eval=FALSE---------------------------------------------------------------
# Require::Install(
#   c("PredictiveEcology/reproducible@development (HEAD)",
#     "PredictiveEcology/SpaDES.core@development (>=2.0.5.9004)"))


## ----eval=FALSE---------------------------------------------------------------
# Require(c("data.table (>= 1.16)", "lme4", "PredictiveEcology/SpaDES.core@development"))


## ----eval=FALSE---------------------------------------------------------------
# # Won't work — pak does not parse this
# try(pak::pak("data.table (>= 1.8.0)"))
# 
# # What you have to write instead — pick an exact version yourself
# pak::pak("data.table@1.8.0")


## ----eval=FALSE---------------------------------------------------------------
# Require::Install(c("data.table (>= 1.16)",
#                    "stringfish (<= 0.15.8)",
#                    "qs (== 0.27.3)"))


## ----eval=FALSE,message=TRUE--------------------------------------------------
# # pak: errors out — both branches of LandR are requested
# try(pak::pak(c("PredictiveEcology/LandR@development",
#                "PredictiveEcology/LandR@main")))
# 
# # Require: takes them in order — main wins
# Require::Install(c("PredictiveEcology/LandR@main",
#                    "PredictiveEcology/LandR@development"))
# 
# # Require: takes by version requirement — development wins because it satisfies the constraint
# Require::Install(c("PredictiveEcology/LandR@main",
#                    "PredictiveEcology/LandR@development (>= 1.1.5)"))


## ----eval=FALSE,message=FALSE-------------------------------------------------
# # pak: fails — `knn` is archived
# try(pak::pkg_install("knn"))
# 
# # Require: succeeds — fetches the most recent archived copy
# Require::Install("knn")

