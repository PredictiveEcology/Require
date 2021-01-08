# SpaDES.core

<details>

* Version: 1.0.5
* GitHub: https://github.com/PredictiveEcology/SpaDES.core
* Source code: https://github.com/cran/SpaDES.core
* Date/Publication: 2021-01-07 20:20:14 UTC
* Number of recursive dependencies: 132

Run `revdep_details(, "SpaDES.core")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in 'SpaDES.core-Ex.R' failed
    The error most likely occurred in:
    
    > ### Name: moduleVersion
    > ### Title: Parse and extract a module's version
    > ### Aliases: moduleVersion moduleVersion,character,character,missing-method
    > ###   moduleVersion,character,missing,missing-method
    > ###   moduleVersion,character,missing,simList-method
    > 
    > ### ** Examples
    ...
    downloaded 1.1 MB
    
    package 'SpaDES.tools' successfully unpacked and MD5 sums checked
    
    The downloaded binary packages are in
    	C:\Users\emcintir.W-VIC-A144916\AppData\Local\Temp\RtmpOGZRKQ\downloaded_packages
    Error in .doLoadActions(where, attach) : 
      error in load action .__A__.1 for package raster: loadModule(module = "spmod", what = TRUE, env = ns, loadNow = TRUE): Unable to load module "spmod": object of type 'closure' is not subsettable
    Calls: simInit ... asNamespace -> loadNamespace -> <Anonymous> -> .doLoadActions
    Execution halted
    ```

