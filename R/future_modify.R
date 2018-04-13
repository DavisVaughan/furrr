#' Modify elements selectively via futures
#'
#' These functions work exactly the same as [purrr::modify()] functions, but allow
#' you to modify in parallel. There are a number of `future.*` arguments
#' to allow you to fine tune the parallel processing.
#'
#' @inheritParams purrr::modify
#'
#' @param future.globals A logical, a character vector, or a named list for
#'        controlling how globals are handled. For details, see below section.
#'
#' @param future.packages (optional) a character vector specifying packages
#'        to be attached in the R environment evaluating the future.
#'
#' @param future.seed A logical or an integer (of length one or seven),
#'        or a list of `length(.x)` with pre-generated random seeds.
#'        For details, see below section.
#'
#' @param future.lazy Specifies whether the futures should be resolved
#'        lazily or eagerly (default).
#'
#' @param future.scheduling Average number of futures ("chunks") per worker.
#'        If `0.0`, then a single future is used to process all elements
#'        of `.x`.
#'        If `1.0` or `TRUE`, then one future per worker is used.
#'        If `2.0`, then each worker will process two futures
#'        (if there are enough elements in `.x`).
#'        If `Inf` or `FALSE`, then one future per element of
#'        `.x` is used.
#'
#' @details
#'
#' From purrr) Since the transformation can alter the structure of the input;
#' it's your responsibility to ensure that the transformation produces a valid
#' output. For example, if you're modifying a data frame, `.f` must preserve the
#' length of the input.
#'
#' @return
#' An object the same class as .x
#'
#' @section Global variables:
#' Argument `future.globals` may be used to control how globals
#' should be handled similarly how the `globals` argument is used with
#' `future()`.
#' Since all function calls use the same set of globals, this function can do
#' any gathering of globals upfront (once), which is more efficient than if
#' it would be done for each future independently.
#' If `TRUE`, `NULL` or not is specified (default), then globals
#' are automatically identified and gathered.
#' If a character vector of names is specified, then those globals are gathered.
#' If a named list, then those globals are used as is.
#' In all cases, `.f` and any `...` arguments are automatically
#' passed as globals to each future created as they are always needed.
#'
#' @section Reproducible random number generation (RNG):
#' Unless `future.seed = FALSE`, this function guarantees to generate
#' the exact same sequence of random numbers _given the same initial
#' seed / RNG state_ - this regardless of type of futures and scheduling
#' ("chunking") strategy.
#'
#' RNG reproducibility is achieved by pregenerating the random seeds for all
#' iterations (over `.x`) by using L'Ecuyer-CMRG RNG streams.  In each
#' iteration, these seeds are set before calling \code{.f(.x[[ii]], ...)}.
#' _Note, for large `length(.x)` this may introduce a large overhead._
#' As input (`future.seed`), a fixed seed (integer) may be given, either
#' as a full L'Ecuyer-CMRG RNG seed (vector of 1+6 integers) or as a seed
#' generating such a full L'Ecuyer-CMRG seed.
#' If `future.seed = TRUE`, then \code{\link[base:Random]{.Random.seed}}
#' is returned if it holds a L'Ecuyer-CMRG RNG seed, otherwise one is created
#' randomly.
#' If `future.seed = NA`, a L'Ecuyer-CMRG RNG seed is randomly created.
#' If none of the function calls \code{.f(.x[[ii]], ...)} uses random number
#' generation, then `future.seed = FALSE` may be used.
#'
#' In addition to the above, it is possible to specify a pre-generated
#' sequence of RNG seeds as a list such that
#' `length(future.seed) == length(.x)` and where each element is an
#' integer seed that can be assigned to \code{\link[base:Random]{.Random.seed}}.
#' Use this alternative with caution.
#' **Note that `as.list(seq_along(.x))` is _not_ a valid set of such
#' `.Random.seed` values.**
#'
#' In all cases but `future.seed = FALSE`, the RNG state of the calling
#' R processes after this function returns is guaranteed to be
#' "forwarded one step" from the RNG state that was before the call and
#' in the same way regardless of `future.seed`, `future.scheduling`
#' and future strategy used.  This is done in order to guarantee that an \R
#' script calling `future_modify()` multiple times should be numerically
#' reproducible given the same initial seed.
#'
#'
#' @examples
#'
#' library(furrr)
#' library(dplyr) # for the pipe
#'
#' plan(multiprocess)
#'
#' # Convert each col to character, in parallel
#' future_modify(mtcars, as.character)
#'
#' iris %>%
#'  future_modify_if(is.factor, as.character) %>%
#'  str()
#'
#' mtcars %>% future_modify_at(c(1, 4, 5), as.character) %>% str()
#'
#' @export
future_modify <- function(.x, .f, ..., future.globals = TRUE, future.packages = NULL, future.seed = FALSE, future.lazy = FALSE, future.scheduling = 1.0) {
  UseMethod("future_modify")
}

#' @export
future_modify.default <- function(.x, .f, ..., future.globals = TRUE, future.packages = NULL, future.seed = FALSE, future.lazy = FALSE, future.scheduling = 1.0) {
  .x[] <- future_map(.x, .f, ..., future.globals = future.globals, future.packages = future.packages, future.seed = future.seed, future.lazy = future.lazy, future.scheduling = future.scheduling)
  .x
}

#' @rdname future_modify
#' @export
future_modify_at <- function(.x, .at, .f, ..., future.globals = TRUE, future.packages = NULL, future.seed = FALSE, future.lazy = FALSE, future.scheduling = 1.0) {
  UseMethod("future_modify_at")
}

#' @export
future_modify_at.default <- function(.x, .at, .f, ..., future.globals = TRUE, future.packages = NULL, future.seed = FALSE, future.lazy = FALSE, future.scheduling = 1.0) {
  sel <- inv_which(.x, .at)
  .x[sel] <- future_map(.x[sel], .f, ..., future.globals = future.globals, future.packages = future.packages, future.seed = future.seed, future.lazy = future.lazy, future.scheduling = future.scheduling)
  .x
}

#' @rdname future_modify
#' @export
future_modify_if <- function(.x, .p, .f, ..., future.globals = TRUE, future.packages = NULL, future.seed = FALSE, future.lazy = FALSE, future.scheduling = 1.0) {
  UseMethod("future_modify_if")
}

#' @export
future_modify_if.default <- function(.x, .p, .f, ..., future.globals = TRUE, future.packages = NULL, future.seed = FALSE, future.lazy = FALSE, future.scheduling = 1.0) {
  sel <- probe(.x, .p)
  .x[sel] <- future_map(.x[sel], .f, ..., future.globals = future.globals, future.packages = future.packages, future.seed = future.seed, future.lazy = future.lazy, future.scheduling = future.scheduling)
  .x
}
