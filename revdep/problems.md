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
    > mars_bag <- bagger(x = biomass_tr[, -6], y = biomass_tr$HHV,
    +                    base_model = "MARS", times = 5, control = ctrl)
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
    [90m  6. [39m│       ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
    [90m  7. [39m│       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
    [90m  8. [39m│         └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
    [90m  9. [39m│           └─baguette:::`_fseq`(`_lhs`)
    [90m 10. [39m│             └─magrittr::freduce(value, `_function_list`)
    [90m 11. [39m│               ├─base::withVisible(function_list[[k]](value))
    [90m 12. [39m│               └─function_list[[k]](value)
    [90m 13. [39m│              
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
    Timing stopped at: 0.114 0.012 0.13
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘future’ ‘magrittr’ ‘purrr’ ‘rlang’ ‘utils’
      All declared Imports should be used.
    ```

