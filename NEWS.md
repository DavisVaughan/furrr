# furrr 0.2.0

## Breaking changes:

* All furrr functions now enforce tidyverse recycling rules (for example, 
  between `.x` and `.y` in `future_map2()`). Previously this was mostly the
  case, except with size zero input. Recycling between input of size 0 and
  input of size >1 no longer recycles to size 0, and is instead an error.
  purrr will begin to do this as well in the next major release (#134).

* `future_options()` has been deprecated in favor of `furrr_options()`.
  Calling `future_options()` will still work, but will trigger a once per
  session warning and will eventually be removed. This change was made to
  free up this function name in case the future package ever wants to use it.
  
* In a future version of furrr, the `.progress` argument will be deprecated
  and removed in favor of the 
  [progressr](https://CRAN.R-project.org/package=progressr)
  package. The progress bar has not yet been removed in furrr 0.2.0, however
  I would encourage you to please start using progressr if possible. It uses
  a much more robust idea, and has been integrated with future
  in such a way that it can relay near real-time progress updates from
  sequential, multisession, and even cluster futures (meaning that remote
  connections can return live updates). Multicore support will come at some
  point as well. That said, be aware that it is a relatively new package
  and the API is still stabilizing. As more people use it, its place in the
  future ecosystem will become clearer, and tighter integration with furrr
  will likely be possible.

## Features / Fixes:

* [New pkgdown article](https://davisvaughan.github.io/furrr/articles/articles/progress.html)
  on using furrr with [progressr](https://CRAN.R-project.org/package=progressr)
  for generating progress updates.

* [New pkgdown article](https://davisvaughan.github.io/furrr/articles/articles/carrier.html)
  discussing an alternative strategy to automatic globals detection using
  the [carrier](https://CRAN.R-project.org/package=carrier) package.

* [New pkgdown article](https://davisvaughan.github.io/furrr/articles/articles/chunking.html)
  discussing how furrr "chunks" input to send if off to workers.

* [New pkgdown article](https://davisvaughan.github.io/furrr/articles/articles/gotchas.html)
  on common gotchas when using furrr.

* [New pkgdown article](https://davisvaughan.github.io/furrr/articles/articles/remote-connections.html)
  detailing how to use furrr with remote AWS EC2 connections.

* `future_walk()` and friends have been added to mirror `purrr::walk()`.

* `furrr_options()` now has a variety of new arguments for fine tuning furrr.
  These are based on advancements made in both future and future.apply. The
  most important is `chunk_size`, which can be used as an alternative
  to `scheduling` to determine how to break up `.x` into chunks to send off
  to the workers. See `?furrr_options` for full details.

* `future_pmap()` and its variants now propagate the names of the first element
  of `.l` onto the output (#116).

* `future_pmap()` and its variants now work with empty `list()` input (#135).

* `future_modify()`, `future_modify_if()` and `future_modify_at()` have been
  brought up to date with the changes in purrr 0.3.0 to their non-parallel
  equivalents. Specifically, they now wrap `[[<-` and return the same type
  as the input when the input is an atomic vector (#119).

* `future_map_if()` and `future_modify_if()` gained the `.else` argument that
  was added to purrr's `map_if()` and `modify_if()` in purrr 0.3.0 (#132).

* All `*_raw()` variants from purrr have been added, such as
  `future_map_raw()` (#122).
  
* All furrr functions gained a new argument, `.env_globals`, which determines
  the environment in which globals for `.x` and `...` are
  looked up. It defaults to the caller environment, which is different than
  what was previously used, but should be more correct in some edge cases.
  Most of the time, you should not have to touch this argument. Additionally,
  globals for `.f` are now looked up in the function environment of `.f`
  (HenrikBengtsson/future.apply#62, #153).
  
* The future specific global option `future.globals.maxSize` now scales with
  the number of elements of `.x` that get exported to each worker. This
  helps prevent some false positives about exporting objects that are too large,
  and is the same approach taken in future.apply (#113).

* `.x` is now searched for globals. Only globals found in the slice of `.x`
  that corresponds to worker X are exported to worker X. This is relevant if
  `.x` is, say, a list of functions where each has their own set of globals
  (#16).

* The progress bar furrr creates now outputs to stderr rather than stdout.

* The progress bar is now only enabled for multisession, multicore, and
  multiprocess strategies. It has never worked for sequential futures or
  cluster futures using remote connections, but `.progress` is now forced
  to false in those cases.

* `future_invoke_map()` and its variants have been marked as retired to match
  `purrr::invoke_map()`.
  
* The internals of furrr have been overhauled to unify the implementations of
  `future_map()`, `future_map2()`, `future_pmap()` and all of their variants.
  This should make furrr much easier to maintain going forward (#44).

* A MIT license is now used.

## Version requirements:

* rlang >= 0.3.0 is now required to ensure that the rlang `~` is serializable.
  The hacks in furrr that tried to work around this have been removed (#123).

* future >= 1.19.1 is now required to be able to use `future::value()` instead
  of the soon to be deprecated `future::values()` and to access a few bug
  fixes (#108).
  
* purrr >= 0.3.0 is now required to gain access to various new features and
  breaking changes. For example, `map_if()` gained an `.else` argument, which
  has been added to `future_map_if()`.

* globals >= 0.13.1 is now required because of substantial new speed boosts
  there related to searching for global variables, and to gain access to a few
  bug fixes.

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
