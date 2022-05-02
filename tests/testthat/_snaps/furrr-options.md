# can avoid handling conditions altogether

    Code
      future_map(1:5, fn, .options = opts)
    Condition
      Warning in `...furrr_fn()`:
      hello
      Warning in `...furrr_fn()`:
      hello
      Warning in `...furrr_fn()`:
      hello
      Warning in `...furrr_fn()`:
      hello
      Warning in `...furrr_fn()`:
      hello
    Output
      [[1]]
      [1] 1
      
      [[2]]
      [1] 2
      
      [[3]]
      [1] 3
      
      [[4]]
      [1] 4
      
      [[5]]
      [1] 5
      

---

    Code
      future_map(1:5, fn, .options = opts)
    Output
      [[1]]
      [1] 1
      
      [[2]]
      [1] 2
      
      [[3]]
      [1] 3
      
      [[4]]
      [1] 4
      
      [[5]]
      [1] 5
      

# validates `conditions`

    Code
      (expect_error(furrr_options(conditions = 1)))
    Output
      <error/rlang_error>
      Error in `validate_conditions()`:
      ! `conditions` must be a character vector.

