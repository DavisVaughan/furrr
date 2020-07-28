#' `future` specific options
#'
#' These options are used by `future()` internally to tweak the environment that
#' the expressions are called in. The most important ones are `globals` and
#' `packages` which allow you to be explicit about the variables and packages
#' that are exported to each worker.
#'
#' @param globals A logical, a character vector, or a named list for
#'        controlling how globals are handled. For details, see `Global variables and packages`.
#'
#' @param packages (optional) a character vector specifying packages
#'        to be attached in the R environment evaluating the future.
#'
#' @param seed A logical or an integer (of length one or seven),
#'        or a list of `length(.x)` with pre-generated random seeds.
#'        For details, see below section.
#'
#' @param lazy Specifies whether the futures should be resolved
#'        lazily or eagerly (default).
#'
#' @param scheduling Average number of futures ("chunks") per worker.
#'        If `0.0`, then a single future is used to process all elements
#'        of `.x`.
#'        If `1.0` or `TRUE`, then one future per worker is used.
#'        If `2.0`, then each worker will process two futures
#'        (if there are enough elements in `.x`).
#'        If `Inf` or `FALSE`, then one future per element of
#'        `.x` is used.
#'
#' @section Global variables and packages:
#'
#' By default, the `future` package will perform black magic to look up the
#' global variables and packages that your `furrr` call requires, and it
#' will export these to each worker. However, it is not always perfect, and
#' can be refined with the `globals` and `packages` arguments.
#'
#' `globals` may be used to control how globals
#' should be handled similarly how the `globals` argument is used with
#' `future()`. Since all function calls use the same set of globals, this function can do
#' any gathering of globals upfront (once), which is more efficient than if
#' it would be done for each future independently.
#'
#' * If `TRUE` or `NULL`, then globals are automatically identified and gathered.
#' * If a character vector of names is specified, then those globals are gathered.
#' * If a named list, then those globals are used as is.
#' * In all cases, `.f` and any `...` arguments are automatically
#' passed as globals to each future created as they are always needed.
#'
#' `packages` may be used to control the packages that are exported
#' to each worker.
#'
#' * If a character vector of packages names is specified, those are exported
#' to each worker.
#' * In all cases, `purrr` is exported, as it is always required on each worker.
#'
#' @section Reproducible random number generation (RNG):
#'
#' Unless `seed = FALSE`, this function guarantees to generate
#' the exact same sequence of random numbers _given the same initial
#' seed / RNG state_ - this regardless of type of futures and scheduling
#' ("chunking") strategy.
#'
#' RNG reproducibility is achieved by pregenerating the random seeds for all
#' iterations (over `.x`) by using L'Ecuyer-CMRG RNG streams.  In each
#' iteration, these seeds are set before calling \code{.f(.x[[ii]], ...)}.
#' _Note, for large `length(.x)` this may introduce a large overhead._
#'
#' As input (`seed`), a fixed seed (integer) may be given, either
#' as a full L'Ecuyer-CMRG RNG seed (vector of 1+6 integers) or as a seed
#' generating such a full L'Ecuyer-CMRG seed.
#' If `seed = TRUE`, then \code{\link[base:Random]{.Random.seed}}
#' is returned if it holds a L'Ecuyer-CMRG RNG seed, otherwise one is created
#' randomly.
#' If `seed = NA`, a L'Ecuyer-CMRG RNG seed is randomly created.
#' If none of the function calls \code{.f(.x[[ii]], ...)} uses random number
#' generation, then `seed = FALSE` may be used.
#'
#' In addition to the above, it is possible to specify a pre-generated
#' sequence of RNG seeds as a list such that
#' `length(seed) == length(.x)` and where each element is an
#' integer seed that can be assigned to \code{\link[base:Random]{.Random.seed}}.
#' Use this alternative with caution.
#' **Note that `as.list(seq_along(.x))` is _not_ a valid set of such
#' `.Random.seed` values.**
#'
#' In all cases but `seed = FALSE`, the RNG state of the calling
#' R processes after this function returns is guaranteed to be
#' "forwarded one step" from the RNG state that was before the call and
#' in the same way regardless of `seed`, `scheduling`
#' and future strategy used.  This is done in order to guarantee that an \R
#' script calling `future_map()` multiple times should be numerically
#' reproducible given the same initial seed.
#'
#' @export
future_options <- function(globals = TRUE, packages = NULL, seed = FALSE, lazy = FALSE, scheduling = 1.0) {

  .options <- new_future_options(
    globals    = globals,
    packages   = packages,
    seed       = seed,
    lazy       = lazy,
    scheduling = scheduling
  )

  validate_future_options(.options)
}

new_future_options <- function(globals, packages, seed, lazy, scheduling) {

  stopifnot(inherits(globals,    c("NULL", "logical", "character", "list")))
  stopifnot(inherits(packages,   c("NULL", "character")))
  stopifnot(inherits(seed,       c("logical", "integer", "list")))
  stopifnot(inherits(lazy,       c("logical")))
  stopifnot(inherits(scheduling, c("logical", "numeric")))

  structure(
    list(
      globals    = globals,
      packages   = packages,
      seed       = seed,
      lazy       = lazy,
      scheduling = scheduling
    ),
    class = "future_options"
  )
}

validate_future_options <- function(.options) {

  # Globals checks
  if(is.list(.options$globals)) {
    nms <- names(.options$globals)
    if(any(nms == "") || is.null(nms)) {
      stop("Every element of a globals list must be named.", call. = FALSE)
    }
  }

  # Seed checks
  if(is.integer(.options$seed)) {
    if( !(length(.options$seed) == 1L || length(.options$seed) == 7L) ) {
      stop("Integer seeds must be of length 1 or 7.", call. = FALSE)
    }
  }

  # Scheduling checks
  if(!length(.options$scheduling == 1L)) {
    stop("scheduling must be length 1.", call. = FALSE)
  }

  .options
}

assert_future_options <- function(.options) {
  if(!inherits(.options, "future_options")) {
    stop(".options must be created from future_options().", call. = FALSE)
  }
}
