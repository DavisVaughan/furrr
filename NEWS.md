# furrr 0.1.0.9002

* rlang >= 0.3.0 is now required to ensure that the rlang `~` is serializable.
  The hacks in furrr that tried to work around this have been removed (#123).

* future >= 1.17.0 is now required to be able to use `future::value()` instead
  of the soon to be deprecated `future::values()` (#108).

* A MIT license is now used.

* Added an advanced furrr vignette detailing how to use furrr with
  remote connections.

* `future_walk()` and friends have been added to mirror `purrr::walk()`.

* `.x` is now searched for globals. Only globals found in the slice of `.x`
  that corresponds to worker X are exported to worker X. This is relevant if
  `.x` is, say, a list of functions where each has their own set of globals
  (#16).

* `globals (>= 0.12.1)` is now required because of substantial new speed boosts
  there related to searching for global variables.

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
