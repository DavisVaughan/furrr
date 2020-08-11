# abjutils

<details>

* Version: 0.2.3
* Source code: https://github.com/cran/abjutils
* URL: https://github.com/abjur/abjutils
* Date/Publication: 2019-02-07 21:43:35 UTC
* Number of recursive dependencies: 96

Run `revdep_details(, "abjutils")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘lifecycle’
    Namespaces in Imports field not imported from:
      ‘httr’ ‘progress’
      All declared Imports should be used.
    Missing or unexported object: ‘devtools::use_package’
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘httr’ ‘progress’
      All declared Imports should be used.
    Missing or unexported object: ‘devtools::use_package’
    ```

# baguette

<details>

* Version: 0.0.1
* Source code: https://github.com/cran/baguette
* URL: https://github.com/tidymodels/baguette
* BugReports: https://github.com/tidymodels/baguette/issues
* Date/Publication: 2020-04-14 14:20:04 UTC
* Number of recursive dependencies: 111

Run `revdep_details(, "baguette")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Problem with `mutate()` input `model`.
    [31m✖[39m missing value where TRUE/FALSE needed
    [34mℹ[39m Input `model` is `iter(...)`.
    Backtrace:
    [90m     [39m█
    [90m  1. [39m├─baguette::bagger(...)
    [90m  2. [39m├─baguette:::bagger.data.frame(...)
    [90m  3. [39m│ └─baguette:::bagger_bridge(...)
    [90m  4. [39m│   └─baguette:::mars_bagger(rs, control, ...)
    [90m  5. [39m│     └─`%>%`(...)
    [90m  6. [39m├─dplyr::mutate(...)
    [90m  7. [39m├─dplyr:::mutate.data.frame(...)
    [90m  8. [39m│ └─dplyr:::mutate_cols(.data, ...)
    [90m  9. [39m│   ├─base::withCallingHandlers(...)
    [90m 10. [39m│   └─mask$eval_all_mutate(dots[[i]])
    [90m 11. [39m├─furrr:::iter(...)
    [90m 12. [39m│ └─furrr:::furrr_map2_template(...)
    [90m 13. [39m│   └─furrr:::furrr_template(...)
    [90m 14. [39m│     └─furrr:::get_globals_and_packages(...)
    [90m 15. [39m│       └─future::getGlobalsAndPackages(dots, envir = env_gl
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 93 | SKIPPED: 0 | WARNINGS: 5 | FAILED: 10 ]
      1.  Error: check C5.0 opt (@test-C5.R#15) 
      2.  Error: check model reduction (@test-C5.R#49) 
      3.  Failure: check C5 parsnip interface (@test-C5.R#81) 
      4.  Error: check C5 parsnip interface (@test-C5.R#88) 
      5.  Error: var_imp (@test-interfaces.R#38) 
      6.  Error: check mars opt (@test-mars.R#17) 
      7.  Error: check model reduction (@test-mars.R#72) 
      8.  Failure: bad inputs (@test-validation.R#128) 
      9.  Failure: bad inputs (@test-validation.R#138) 
      10. Failure: bad inputs (@test-validation.R#138) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking whether package ‘baguette’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘parsnip’ was built under R version 4.0.2
    See ‘/Users/davis/Desktop/r/packages/furrr/revdep/checks.noindex/baguette/new/baguette.Rcheck/00install.out’ for details.
    ```

# disk.frame

<details>

* Version: 0.3.7
* Source code: https://github.com/cran/disk.frame
* URL: https://diskframe.com
* BugReports: https://github.com/xiaodaigh/disk.frame/issues
* Date/Publication: 2020-07-07 13:10:03 UTC
* Number of recursive dependencies: 105

Run `revdep_details(, "disk.frame")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   # only run in interactive()
    +   setup_disk.frame(gui = TRUE)
    + }
    > 
    > # set the number workers to 2
    > setup_disk.frame(2)
    Warning in socketConnection("localhost", port = port, server = TRUE, blocking = TRUE,  :
      port 37400 cannot be opened
    Error in socketConnection("localhost", port = port, server = TRUE, blocking = TRUE,  : 
      Failed to launch and connect to R worker on local machine ‘localhost’ from local machine ‘daviss-mbp-2.lan’.
     * The error produced by socketConnection() was: ‘cannot open the connection’
     * In addition, socketConnection() produced 1 warning(s):
       - Warning #1: ‘port 37400 cannot be opened’ (which suggests that this port is either already occupied by another process or blocked by the firewall on your local machine)
     * The localhost socket connection that failed to connect to the R worker used port 37400 using a communication timeout of 120 seconds and a connection timeout of 120 seconds.
     * Worker launch call: '/Library/Frameworks/R.framework/Resources/bin/Rscript' --default-packages=datasets,utils,grDevices,graphics,stats,methods -e '#label=UNKNOWN:12345:daviss-mbp-2.lan:davis' -e 'try(suppressWarnings(cat(Sys.getpid(),file="/var/folders/41/qx_9ygp112nfysdfgxcssgwc0000gn/T//RtmpVsZsNC/future.parent=12345.30397fb712.pid")), silent = TRUE)' -e 'workRSOCK <- tryCatch(parallel:::.slaveRSOCK, error=function(e) parallel:::.workRSOCK); workRSOCK()' MASTER=localhost PORT=37400 OUT=/dev/null TIMEOUT=120 XDR=TRUE.
     * Worker (PID 12721) was successfully killed: TRUE
     * Troubleshooting suggestions:
       - Suggestion #1: Set 'verbose=TRUE' to see more details.
       - Suggestion #2: Set 'outfile=NULL' to see output from worker.
    Calls: setup_disk.frame ... tryCatchList -> tryCatchOne -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

## In both

*   checking whether package ‘disk.frame’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘dplyr’ was built under R version 4.0.2
    See ‘/Users/davis/Desktop/r/packages/furrr/revdep/checks.noindex/disk.frame/new/disk.frame.Rcheck/00install.out’ for details.
    ```

# photosynthesis

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/photosynthesis
* Date/Publication: 2020-07-01 09:40:02 UTC
* Number of recursive dependencies: 74

Run `revdep_details(, "photosynthesis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘photosynthesis-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: A_supply
    > ### Title: CO2 supply and demand function (mol / m^2 s)
    > ### Aliases: A_supply A_demand
    > 
    > ### ** Examples
    > 
    > bake_par <- make_bakepar()
    Error in which %>% match.arg(c("bake", "constants", "enviro", "leaf")) %>%  : 
      no function to return from, jumping to top level
    Calls: make_bakepar -> %<>% -> <Anonymous> -> parameter_names -> %>%
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 0 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 15 ]
      1. Error: baked parameters do not equal unbaked unless Temp = 25 (@test-bake.R#6) 
      2. Error: constants returns class constants and list (@test-constants.R#5) 
      3. Error: fails when a parameter is left out (@test-constants.R#15) 
      4. Error: removes an improper parameter (@test-constants.R#26) 
      5. Error: nu_constant returns a list of two numbers (@test-constants.R#33) 
      6. Error: sh_constant returns a vector of one unitless number of numeric class (@test-constants.R#77) 
      7. Error: constants returns class enviro_par and list (@test-enviro-par.R#5) 
      8. Error: fails when a parameter is left out (@test-enviro-par.R#11) 
      9. Error: removes an improper parameter (@test-enviro-par.R#17) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking whether package ‘photosynthesis’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘units’ was built under R version 4.0.2
    See ‘/Users/davis/Desktop/r/packages/furrr/revdep/checks.noindex/photosynthesis/new/photosynthesis.Rcheck/00install.out’ for details.
    ```

# simhelpers

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/simhelpers
* URL: https://meghapsimatrix.github.io/simhelpers/index.html
* BugReports: https://github.com/meghapsimatrix/simhelpers/issues
* Date/Publication: 2020-03-31 15:00:06 UTC
* Number of recursive dependencies: 98

Run `revdep_details(, "simhelpers")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > ### ** Examples
    > 
    > df <- data.frame(
    +   n = 3:5,
    +   lambda = seq(8, 16, 4)
    + )
    > 
    > evaluate_by_row(df, rpois)
    Warning: The `.progress` argument of `future_pmap()` is deprecated as of furrr 0.2.0.
    The `.progress` argument has been deprecated in favor of the progressr package. It provides a much more robust solution, and is integrated with future in such a way that it can relay real-time progress updates even on remote connections.
    [90mThis warning is displayed once every 8 hours.[39m
    [90mCall `lifecycle::last_warnings()` to see where this warning was generated.[39m
    Warning: `future_options()` is deprecated as of furrr 0.2.0.
    Please use `furrr_options()` instead.
    [90mThis warning is displayed once every 8 hours.[39m
    [90mCall `lifecycle::last_warnings()` to see where this warning was generated.[39m
    Error in ...furrr_fn(...) : could not find function "...furrr_fn"
    Calls: evaluate_by_row ... resolve.list -> signalConditionsASAP -> signalConditions
    Timing stopped at: 0.097 0.009 0.107
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘future’ ‘magrittr’ ‘purrr’ ‘rlang’ ‘utils’
      All declared Imports should be used.
    ```

