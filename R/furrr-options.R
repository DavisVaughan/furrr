#' Options to fine tune furrr
#'
#' These options fine tune furrr functions, such as [future_map()]. They
#' are either used by furrr directly, or are passed on to [future::future()].
#'
#' @param ... These dots are reserved for future extensibility and must
#'   be empty.
#'
#' @param stdout A logical.
#'
#'   - If `TRUE`, standard output of the underlying futures is relayed as soon
#'   as possible.
#'
#'   - If `FALSE`, output is silenced by sinking it to the null device.
#'
#' @param conditions A character string of conditions classes to be relayed.
#'   The default is to relay all conditions, including messages and warnings.
#'   Errors are always relayed. To not relay any conditions (besides errors),
#'   use `conditions = character()`. To selectively ignore specific classes,
#'   use `conditions = structure("condition", exclude = "message")`.
#'
#' @param globals A logical, a character vector, a named list, or `NULL` for
#'   controlling how globals are handled. For details, see the
#'   `Global variables` section below.
#'
#' @param packages A character vector, or `NULL`. If supplied, this specifies
#'   packages that are guaranteed to be attached in the R environment where the
#'   future is evaluated.
#'
#' @param seed A logical, an integer of length `1` or `7`, a list of
#'   `length(.x)` with pre-generated random seeds, or `NULL`. For details, see
#'   the `Reproducible random number generation (RNG)` section below.
#'
#' @param scheduling A single integer, logical, or `Inf`. This argument
#'   controls the average number of futures ("chunks") per worker.
#'
#'   - If `0`, then a single future is used to process all elements of `.x`.
#'
#'   - If `1` or `TRUE`, then one future per worker is used.
#'
#'   - If `2`, then each worker will process two futures (provided there
#'     are enough elements in `.x`).
#'
#'   - If `Inf` or `FALSE`, then one future per element of `.x` is used.
#'
#'   This argument is only used if `chunk_size` is `NULL`.
#'
#' @param chunk_size A single integer, `Inf`, or `NULL`. This argument
#'   controls the average number of elements per future (`"chunk"`). If `Inf`,
#'   then all elements are processed in a single future. If `NULL`, then
#'   `scheduling` is used instead to determine how `.x` is chunked.
#'
#' @param prefix A single character string, or `NULL`. If a character string,
#'   then each future is assigned a label as `{prefix}-{chunk-id}`. If `NULL`,
#'   no labels are used.
#'
#'
#' @section Global variables:
#'
#' `globals` controls how globals are identified, similar to the `globals`
#' argument of [future::future()]. Since all function calls use the same set of
#' globals, furrr gathers globals upfront (once), which is more efficient than
#' if it was done for each future independently.
#'
#' * If `TRUE` or `NULL`, then globals are automatically identified and
#'   gathered.
#'
#' * If a character vector of names is specified, then those globals are
#'   gathered.
#'
#' * If a named list, then those globals are used as is.
#'
#' * In all cases, `.f` and any `...` arguments are automatically passed as
#'   globals to each future created, as they are always needed.
#'
#' @section Reproducible random number generation (RNG):
#'
#' Unless `seed = FALSE`, furrr functions are guaranteed to generate
#' the exact same sequence of random numbers _given the same initial
#' seed / RNG state_ regardless of the type of futures and scheduling
#' ("chunking") strategy.
#'
#' Setting `seed = NULL` is equivalent to `seed = FALSE`, except that the
#' `future.rng.onMisuse` option is not consulted to potentially monitor the
#' future for faulty random number usage. See the `seed` argument of
#' [future::future()] for more details.
#'
#' RNG reproducibility is achieved by pre-generating the random seeds for all
#' iterations (over `.x`) by using L'Ecuyer-CMRG RNG streams. In each
#' iteration, these seeds are set before calling `.f(.x[[i]], ...)`.
#' _Note, for large `length(.x)` this may introduce a large overhead._
#'
#' A fixed `seed` may be given as an integer vector, either as a full
#' L'Ecuyer-CMRG RNG seed of length `7`, or as a seed of length `1` that
#' will be used to generate a full L'Ecuyer-CMRG seed.
#'
#' If `seed = TRUE`, then `.Random.seed` is returned if it holds a
#' L'Ecuyer-CMRG RNG seed, otherwise one is created randomly.
#'
#' If `seed = NA`, a L'Ecuyer-CMRG RNG seed is randomly created.
#'
#' If none of the function calls `.f(.x[[i]], ...)` use random number
#' generation, then `seed = FALSE` may be used.
#'
#' In addition to the above, it is possible to specify a pre-generated
#' sequence of RNG seeds as a list such that `length(seed) == length(.x)` and
#' where each element is an integer seed that can be assigned to `.Random.seed`.
#' Use this alternative with caution. _Note that `as.list(seq_along(.x))` is
#' not a valid set of such `.Random.seed` values._
#'
#' In all cases but `seed = FALSE`, after a furrr function returns, the RNG
#' state of the calling R process is guaranteed to be "forwarded one step" from
#' the RNG state before the call. This is true regardless of the future
#' strategy / scheduling used. This is done in order to guarantee that an R
#' script calling `future_map()` multiple times should be numerically
#' reproducible given the same initial seed.
#'
#' @export
#' @examples
#' furrr_options()
furrr_options <- function(...,
                          stdout = TRUE,
                          conditions = "condition",
                          globals = TRUE,
                          packages = NULL,
                          seed = FALSE,
                          scheduling = 1.0,
                          chunk_size = NULL,
                          prefix = NULL) {
  check_dots_empty()

  stdout <- validate_stdout(stdout)
  conditions <- validate_conditions(conditions)
  globals <- validate_globals(globals)
  packages <- validate_packages(packages)
  seed <- validate_seed(seed)
  scheduling <- validate_scheduling(scheduling)
  chunk_size <- validate_chunk_size(chunk_size)
  prefix <- validate_prefix(prefix)

  out <- list(
    stdout = stdout,
    conditions = conditions,
    globals = globals,
    packages = packages,
    seed = seed,
    scheduling = scheduling,
    chunk_size = chunk_size,
    prefix = prefix
  )

  structure(out, class = "furrr_options")
}

#' @export
print.furrr_options <- function(x, ...) {
  cat_line("<furrr_options>")
}

# ------------------------------------------------------------------------------

#' Deprecated furrr options
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' As of furrr 0.3.0, `future_options()` is defunct in favor of
#' [furrr_options()].
#'
#' @inheritParams furrr_options
#'
#' @keywords internal
#' @export
#' @examples
#' try(future_options())
future_options <- function(globals = TRUE,
                           packages = NULL,
                           seed = FALSE,
                           scheduling = 1.0) {
  lifecycle::deprecate_stop("0.3.0", "future_options()", "furrr_options()")

  furrr_options(
    globals = globals,
    packages = packages,
    seed = seed,
    scheduling = scheduling
  )
}

# ------------------------------------------------------------------------------

assert_furrr_options <- function(x) {
  if (!is_furrr_options(x)) {
    abort("`.options` must be created from `furrr_options()`.")
  }

  invisible(x)
}

is_furrr_options <- function(x) {
  inherits(x, "furrr_options")
}

# ------------------------------------------------------------------------------

validate_stdout <- function(x) {
  vctrs::vec_assert(x, size = 1L, arg = "stdout")

  # Allowed to be `NA`, as this means output is not intercepted.
  # We test for this but it is explicitly not recommended by future so we don't
  # document it.
  if (!is.logical(x)) {
    abort("`stdout` must be `TRUE`, `FALSE`, or `NA`.")
  }

  # Always drop stdout from the future objects.
  # Only the values are ever returned, so it is useless and potentially
  # expensive to transfer this back from the workers (#216).
  attr(x, "drop") <- TRUE

  x
}

validate_conditions <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  if (!is.character(x)) {
    abort("`conditions` must be a character vector.")
  }

  # Always drop conditions from the future objects.
  # Only the values are ever returned, so it is useless and potentially
  # expensive to transfer these back from the workers (#216).
  attr(x, "drop") <- TRUE

  x
}

validate_globals <- function(x) {
  if (is.null(x)) {
    return(TRUE)
  }

  if (is.logical(x)) {
    if (!is_bool(x)) {
      abort("A logical `globals` must be length 1 and can't be `NA`.")
    }

    return(x)
  }

  if (is.character(x)) {
    return(x)
  }

  if (is.list(x)) {
    if (!is_named(x)) {
      abort("`globals` cannot be an unnamed or partially named list.")
    }

    return(x)
  }

  abort("`globals` must be `NULL`, a logical, a character vector, or a named list.")
}

validate_packages <- function(x) {
  if (is.null(x)) {
    return(x)
  }

  x <- vctrs::vec_cast(x, character(), x_arg = "packages")

  if (any(is.na(x))) {
    abort("`packages` can't be `NA`.")
  }

  x
}

validate_seed <- function(x) {
  if (is.null(x)) {
    return(x)
  }

  # Size is validated when we have `.x`
  if (is.list(x)) {
    x <- validate_seed_list(x)
    return(x)
  }

  if (is.logical(x)) {
    if (!is_bool(x)) {
      abort("A logical `seed` must be length 1 and must not be `NA`.")
    }

    return(x)
  }

  if (is.double(x)) {
    x <- vctrs::vec_cast(x, integer(), x_arg = "seed")
  }

  if (is.integer(x)) {
    if (length(x) != 1L && length(x) != 7L) {
      abort("An integer `seed` must be length 1 or 7.")
    }

    if (any(is.na(x))) {
      abort("An integer `seed` cannot have `NA` values.")
    }

    return(x)
  }

  abort("`seed` must be a logical, integer, or list.")
}

validate_seed_list <- function(x) {
  seeds_are_integers <- purrr::map_lgl(x, ~typeof(.x) == "integer")
  if (!all(seeds_are_integers)) {
    abort("All elements of a list `seed` must be integers.")
  }

  unique_lengths <- unique(lengths(x))
  if (length(unique_lengths) != 1L) {
    abort("All elements of a list `seed` must have the same length.")
  }

  if (identical(unique_lengths, 1L)) {
    abort(paste0(
      "All pre-generated random seed elements of a list `seed` ",
      "must be valid `.Random.seed` seeds, which means they should be all ",
      "integers and consists of two or more elements, not just one."
    ))
  }

  # For efficiency, just check the first seed for validity
  seed <- x[[1]]
  if (!is_valid_random_seed(seed)) {
    abort(paste0(
      "All pre-generated random seed elements of a list `seed` ",
      "must be valid `.Random.seed` seeds, but the first does not seem to be."
    ))
  }

  x
}

validate_scheduling <- function(x) {
  if (length(x) != 1L) {
    abort("`scheduling` must be length 1.")
  }

  if (identical(x, Inf)) {
    return(x)
  }

  if (is.logical(x)) {
    if (!is_bool(x)) {
      abort("A logical `scheduling` value can't be `NA`.")
    }

    return(x)
  }

  x <- vctrs::vec_cast(x, integer(), x_arg = "scheduling")

  if (x < 0L) {
    abort("`scheduling` must be greater than or equal to zero.")
  }

  x
}

validate_chunk_size <- function(x) {
  if (is.null(x)) {
    return(x)
  }

  if (identical(x, Inf)) {
    return(x)
  }

  vctrs::vec_assert(x, size = 1L, arg = "chunk_size")
  x <- vctrs::vec_cast(x, integer(), x_arg = "chunk_size")

  if (is.na(x)) {
    abort("`chunk_size` can't be `NA`.")
  }

  if (x <= 0L) {
    abort("`chunk_size` must be greater than zero.")
  }

  x
}

validate_prefix <- function(x) {
  if (is.null(x)) {
    return(x)
  }

  if (!is_string(x)) {
    abort("`prefix` must be a character string, or `NULL`.")
  }

  x
}
