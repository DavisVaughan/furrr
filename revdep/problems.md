# baguette

<details>

* Version: 0.0.1
* Source code: https://github.com/cran/baguette
* URL: https://github.com/tidymodels/baguette
* BugReports: https://github.com/tidymodels/baguette/issues
* Date/Publication: 2020-04-14 14:20:04 UTC
* Number of recursive dependencies: 111

Run `cloud_details(, "baguette")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    ✖ missing value where TRUE/FALSE needed
    ℹ Input `model` is `iter(...)`.
    Backtrace:
         █
      1. ├─baguette::bagger(...)
      2. ├─baguette:::bagger.data.frame(...)
      3. │ └─baguette:::bagger_bridge(...)
      4. │   └─baguette:::mars_bagger(rs, control, ...)
      5. │     └─`%>%`(...)
      6. │       ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      7. │       └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      8. │         └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      9. │           └─baguette:::`_fseq`(`_lhs`)
     10. │             └─magrittr::freduce(value, `_function_list`)
     11. │               ├─base::withVisible(function_list[[k]](value))
     12. │               └─function_list[[k]](value)
     13. │                 ├─dplyr::mutate(...)
     14. │                 └─dplyr:::mutate.data.frame(...)
     15. │                   └─dplyr:::mutate_cols(.data, ...)
     16. 
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 93 | SKIPPED: 0 | WARNINGS: 2 | FAILED: 10 ]
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

