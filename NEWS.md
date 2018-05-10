# furrr 0.1.0

Features:

  * `future_pmap_*()` functions have been added to mirror `pmap()`.

  * The `future.*` arguments to each function have been replaced with an 
  overarching `.options` argument. Use `future_options()` to create a set of options
  suitable to be passed to `.options`. This change streamlines the interface 
  greatly, and simplifies documentation (#8, @hadley).
  
  * `future_invoke_map_*()` functions have been added to mirror `invoke_map()`.
  
  * More documentation and examples have been added.
  
  * Added the ability to use a progress bar with `.progress = TRUE` for 
  multicore, multiprocess, and multisession `plan()`s.

Bug Fixes:

  * Fixed a bug with using `~` inside a `mutate()` + `map()` combination.

  * Added a missed `future_imap_int()`.

# furrr 0.0.0

* Original GitHub release of `furrr` on 2018-04-13. 
