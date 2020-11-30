<<<<<<< HEAD
*Wow, no problems at all. :)*
=======
# SpaDES.core

<details>

* Version: 1.0.2
* GitHub: https://github.com/PredictiveEcology/SpaDES.core
* Source code: https://github.com/cran/SpaDES.core
* Date/Publication: 2020-08-28 08:00:02 UTC
* Number of recursive dependencies: 158

Run `revdep_details(, "SpaDES.core")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > # set modulePath
    > setPaths(modulePath = system.file("sampleModules", package = "SpaDES.core"))
    Setting:
      options(
        spades.modulePath = '/home/achubaty/Documents/GitHub/PredictiveEcology/Require/revdep/checks/SpaDES.core/new/SpaDES.core.Rcheck/SpaDES.core/sampleModules'
      )
    Paths set to:
      options(
        rasterTmpDir = '/tmp/Rtmpfz8H41/raster'
        reproducible.cachePath = '/tmp/Rtmpfz8H41/myProject/cache'
        spades.inputPath = '/tmp/Rtmpfz8H41/myProject/inputs'
        spades.outputPath = '/tmp/Rtmpfz8H41/myProject/outputs'
        spades.modulePath = '/home/achubaty/Documents/GitHub/PredictiveEcology/Require/revdep/checks/SpaDES.core/new/SpaDES.core.Rcheck/SpaDES.core/sampleModules'
      )
    > # use Require and reqdPkgs
    > if (!interactive()) chooseCRANmirror(ind = 1) #
    > Require(unlist(reqdPkgs(module = c("caribouMovement", "randomLandscapes", "fireSpread"))))
    Error in `[.data.table`(pkgDT, packageFullName %in% packagesOrig[origPackagesHaveNames],  : 
      Supplied 7 items to be assigned to 8 items of column 'Package'. If you wish to 'recycle' the RHS please use rep() to make this intent clear to readers of your code.
    Calls: Require -> [ -> [.data.table
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      
          citation
      
      [31m──[39m [31m1. Error: /home/achubaty/Documents/GitHub/PredictiveEcology/Require/revdep/ch[39m
      Supplied 7 items to be assigned to 8 items of column 'Package'. If you wish to 'recycle' the RHS please use rep() to make this intent clear to readers of your code.
      [1mBacktrace:[22m
      [90m 1. [39mRequire::Require(...)
      [90m 3. [39mdata.table:::`[.data.table`(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 513 | SKIPPED: 18 | WARNINGS: 2 | FAILED: 1 ]
      1. Error: /home/achubaty/Documents/GitHub/PredictiveEcology/Require/revdep/checks/SpaDES.core/new/SpaDES.core.Rcheck/00_pkg_src/SpaDES.core/man/simList-accessors-metadata.Rd 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RCurl’
      All declared Imports should be used.
    ```

>>>>>>> development
