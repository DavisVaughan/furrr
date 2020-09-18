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

# bigdist

<details>

* Version: 0.1.4
* Source code: https://github.com/cran/bigdist
* URL: https://github.com/talegari/bigdist
* BugReports: https://github.com/talegari/bigdist/issues
* Date/Publication: 2019-03-16 14:13:30 UTC
* Number of recursive dependencies: 78

Run `cloud_details(, "bigdist")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bigdist-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: as_bigdist
    > ### Title: Convert to bigdist
    > ### Aliases: as_bigdist
    > 
    > ### ** Examples
    > 
    > temp3 <- as_bigdist(dist(mtcars), file = file.path(tempdir(), "temp_ex4"))
    ----
    Location: /tmp/Rtmp7bZ2sG/temp_ex4_32_double.bk
    Size on disk: 0 GB
    Error in plan() : could not find function "plan"
    Calls: as_bigdist ... furrr_map_template -> reconcile_progress_with_strategy
    Execution halted
    ```

# cort

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/cort
* URL: https://github.com/lrnv/cort
* BugReports: https://github.com/lrnv/cort/issues
* Date/Publication: 2020-05-13 23:40:09 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "cort")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cort-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: CortForest-Class
    > ### Title: Bagged Cort estimates
    > ### Aliases: CortForest-Class CortForest
    > 
    > ### ** Examples
    > 
    > (CortForest(LifeCycleSavings[,1:3],number_max_dim=2,n_trees=2))
    ======================== Computing trees...
    Error in plan() : could not find function "plan"
    Calls: CortForest ... furrr_map_template -> reconcile_progress_with_strategy
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("cort")
      ── 1. Error: (unknown) (@test-CortForest.R#5)  ─────────────────────────────────
      could not find function "plan"
      Backtrace:
       1. cort::CortForest(LifeCycleSavings[, 1:2], verbose_lvl = 0, n_trees = 3)
       2. furrr::future_map(...)
       3. furrr:::furrr_map_template(...)
       4. furrr:::reconcile_progress_with_strategy(progress)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 116 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: (unknown) (@test-CortForest.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        libs   4.4Mb
    ```

# disk.frame

<details>

* Version: 0.3.7
* Source code: https://github.com/cran/disk.frame
* URL: https://diskframe.com
* BugReports: https://github.com/xiaodaigh/disk.frame/issues
* Date/Publication: 2020-07-07 13:10:03 UTC
* Number of recursive dependencies: 105

Run `cloud_details(, "disk.frame")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘disk.frame-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cmap2
    > ### Title: 'cmap2' a function to two disk.frames
    > ### Aliases: cmap2 map2 map_by_chunk_id
    > 
    > ### ** Examples
    > 
    > cars.df = as.disk.frame(cars)
    > 
    > cars2.df = cmap2(cars.df, cars.df, ~data.table::rbindlist(list(.x, .y)))
    Error in plan() : could not find function "plan"
    Calls: cmap2 ... furrr_pmap_template -> reconcile_progress_with_strategy
    Execution halted
    ```

# ezcox

<details>

* Version: 0.7.0
* Source code: https://github.com/cran/ezcox
* URL: https://github.com/ShixiangWang/ezcox
* BugReports: https://github.com/ShixiangWang/ezcox/issues
* Date/Publication: 2020-07-01 14:50:03 UTC
* Number of recursive dependencies: 85

Run `cloud_details(, "ezcox")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ezcox-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ezcox_parallel
    > ### Title: Parallelly Run Cox Analysis in Batch Mode
    > ### Aliases: ezcox_parallel
    > 
    > ### ** Examples
    > 
    > 
    > library(survival)
    > ezcox_parallel(lung, covariates = c("sex", "ph.ecog"), controls = "age")
    Loading required namespace: furrr
    Error in plan() : could not find function "plan"
    Calls: ezcox_parallel ... furrr_map_template -> reconcile_progress_with_strategy
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("ezcox")
      ── 1. Error: (unknown) (@test-test_ezcox.R#30)  ────────────────────────────────
      could not find function "plan"
      Backtrace:
       1. ezcox::ezcox_parallel(...)
       2. furrr::future_map(...)
       3. furrr:::furrr_map_template(...)
       4. furrr:::reconcile_progress_with_strategy(progress)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 2 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: (unknown) (@test-test_ezcox.R#30) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# idmodelr

<details>

* Version: 0.3.2
* Source code: https://github.com/cran/idmodelr
* URL: http://www.samabbott.co.uk/idmodelr, https://github.com/seabbs/idmodelr
* BugReports: https://github.com/seabbs/idmodelr/issues
* Date/Publication: 2020-06-11 14:20:03 UTC
* Number of recursive dependencies: 129

Run `cloud_details(, "idmodelr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Error: Problem with `mutate()` input `simulations`.
    ✖ could not find function "plan"
    ℹ Input `simulations` is `future_map(...)`.
    ℹ The error occurred in group 1: scenario = "test_1", tmp_var_1 = 0.
    Backtrace:
         █
      1. ├─idmodelr::scenario_analysis(...)
      2. │ └─`%>%`(...)
      3. │   ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
      4. │   └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      5. │     └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
      6. │       └─idmodelr:::`_fseq`(`_lhs`)
      7. │         └─magrittr::freduce(value, `_function_list`)
      8. │           ├─base::withVisible(function_list[[k]](value))
      9. │           └─function_list[[k]](value)
     10. │             ├─dplyr::mutate(...)
     11. │             └─dplyr:::mutate.data.frame(...)
     12. │               └─dplyr:::mutate_cols(.data, ...)
     13. │                 ├─base::withCallingHandlers(...)
     14. │                 └─mask$eval_a
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ℹ The error occurred in group 1: scenario = "test_1", tmp_var_1 = 0.
      Backtrace:
        1. idmodelr::scenario_analysis(...)
       18. base::.handleSimpleError(...)
       19. dplyr:::h(simpleError(msg, call))
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 46 | SKIPPED: 41 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: scenaria_analysis works correctly on sample data
                with a dummy model and simulation function (@test-scenario_analysis.R#32) 
      2. Error: scenario_analysis works correctly on sample data
                with a dummy model and simulation function over multiple cores (@test-scenario_analysis.R#45) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# photosynthesis

<details>

* Version: 2.0.0
* Source code: https://github.com/cran/photosynthesis
* Date/Publication: 2020-09-12 05:40:03 UTC
* Number of recursive dependencies: 97

Run `cloud_details(, "photosynthesis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    1   3.743 [Pa] 200 [umol/m^2/s] 27.238 [Pa] 16.582 [kPa] 1 [1] 1 [1] 1 [1]
      leafsize     phi_J          R_d25     T_leaf   theta_J         V_cmax25
    1  0.1 [m] 0.331 [1] 2 [umol/m^2/s] 298.15 [K] 0.825 [1] 150 [umol/m^2/s]
               V_tpu25 g_mc gamma_star J_max    K_C    K_O R_d V_cmax V_tpu   C_air
    1 200 [umol/m^2/s]    4      3.743   200 27.238 16.582   2    150   200 41 [Pa]
                   O              P              PPFD      RH    wind
    1 21.27565 [kPa] 101.3246 [kPa] 1500 [umol/m^2/s] 0.5 [1] 2 [m/s]
    > 
    > # Multiple parameter sets with 'photosynthesis'
    > 
    > leaf_par <- make_leafpar(
    +   replace = list(
    +     T_leaf = set_units(c(293.14, 298.15), "K")
    +   ), use_tealeaves = FALSE
    + )
    > photosynthesis(leaf_par, enviro_par, bake_par, constants,
    +   use_tealeaves = FALSE
    + )
    Solving for photosynthetic rate from 2 parameter sets ...Error in plan() : could not find function "plan"
    Calls: photosynthesis -> find_As
    Execution halted
    ```

# sigminer

<details>

* Version: 1.0.16
* Source code: https://github.com/cran/sigminer
* URL: https://github.com/ShixiangWang/sigminer
* BugReports: https://github.com/ShixiangWang/sigminer/issues
* Date/Publication: 2020-09-12 14:30:02 UTC
* Number of recursive dependencies: 191

Run `cloud_details(, "sigminer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sigminer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_bayesian_result
    > ### Title: Get Specified Bayesian NMF Result from Run
    > ### Aliases: get_bayesian_result
    > 
    > ### ** Examples
    > 
    > load(system.file("extdata", "toy_copynumber_tally_W.RData",
    +   package = "sigminer", mustWork = TRUE
    + ))
    > 
    > res <- sig_auto_extract(cn_tally_W$nmf_matrix, result_prefix = "Test_copynumber", nrun = 1)
    Error in plan() : could not find function "plan"
    Calls: sig_auto_extract ... furrr_map_template -> reconcile_progress_with_strategy
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        extdata   3.5Mb
    ```

# tealeaves

<details>

* Version: 1.0.5
* Source code: https://github.com/cran/tealeaves
* Date/Publication: 2020-06-18 10:30:02 UTC
* Number of recursive dependencies: 80

Run `cloud_details(, "tealeaves")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > tleaf(leaf_par, enviro_par, constants)
    
    Solving for T_leaf ... done
            T_leaf         value convergence            R_abs              S_r
    1 301.4181 [K] -2.122124e-08           0 1363.813 [W/m^2] 907.9499 [W/m^2]
                     H                L                      E          Ar       Gr
    1 107.3552 [W/m^2] 348.5078 [W/m^2] 0.00794791 [mol/m^2/s] 0.004827203 788182.4
            Re       g_bw
    1 12778.08 0.02972824
    > 
    > # tleaves for multiple parameter set:
    > 
    > enviro_par <- make_enviropar(
    +   replace = list(
    +     T_air = set_units(c(293.15, 298.15), K)
    +   )
    + )
    > tleaves(leaf_par, enviro_par, constants)
    Solving for T_leaf from 2 parameter sets...Error in plan() : could not find function "plan"
    Calls: tleaves -> find_tleaves
    Execution halted
    ```

