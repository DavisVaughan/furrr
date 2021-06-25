#' Invoke functions via futures
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions work exactly the same as [purrr::invoke_map()] functions, but
#' allow you to invoke in parallel.
#'
#' @inheritParams purrr::invoke_map
#' @inheritParams future_map
#'
#' @param .f A list of functions.
#' @param .x A list of argument-lists the same length as `.f` (or length 1). The
#'   default argument, `list(NULL)`, will be recycled to the same length as
#'   `.f`, and will call each function with no arguments (apart from any
#'   supplied in `...`).
#'
#' @export
#' @examples
#' \donttest{plan(multisession, workers = 2)}
#'
#' df <- dplyr::tibble(
#'   f = c("runif", "rpois", "rnorm"),
#'   params = list(
#'     list(n = 10),
#'     list(n = 5, lambda = 10),
#'     list(n = 10, mean = -3, sd = 10)
#'   )
#' )
#'
#' future_invoke_map(df$f, df$params, .options = furrr_options(seed = 123))
#'
#' \dontshow{
#' # Close open connections for R CMD Check
#' if (!inherits(plan(), "sequential")) plan(sequential)
#' }
future_invoke_map <- function(.f,
                              .x = list(NULL),
                              ...,
                              .env = NULL,
                              .options = furrr_options(),
                              .env_globals = parent.frame(),
                              .progress = FALSE) {
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)

  future_map2(
    .x = .f,
    .y = .x,
    .f = purrr::invoke,
    ...,
    .env = .env,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )
}

#' @rdname future_invoke_map
#' @export
future_invoke_map_chr <- function(.f,
                                  .x = list(NULL),
                                  ...,
                                  .env = NULL,
                                  .options = furrr_options(),
                                  .env_globals = parent.frame(),
                                  .progress = FALSE) {
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)

  future_map2_chr(
    .x = .f,
    .y = .x,
    .f = purrr::invoke,
    ...,
    .env = .env,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )
}

#' @rdname future_invoke_map
#' @export
future_invoke_map_dbl <- function(.f,
                                  .x = list(NULL),
                                  ...,
                                  .env = NULL,
                                  .options = furrr_options(),
                                  .env_globals = parent.frame(),
                                  .progress = FALSE) {
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)

  future_map2_dbl(
    .x = .f,
    .y = .x,
    .f = purrr::invoke,
    ...,
    .env = .env,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )
}

#' @rdname future_invoke_map
#' @export
future_invoke_map_int <- function(.f,
                                  .x = list(NULL),
                                  ...,
                                  .env = NULL,
                                  .options = furrr_options(),
                                  .env_globals = parent.frame(),
                                  .progress = FALSE) {
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)

  future_map2_int(
    .x = .f,
    .y = .x,
    .f = purrr::invoke,
    ...,
    .env = .env,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )
}

#' @rdname future_invoke_map
#' @export
future_invoke_map_lgl <- function(.f,
                                  .x = list(NULL),
                                  ...,
                                  .env = NULL,
                                  .options = furrr_options(),
                                  .env_globals = parent.frame(),
                                  .progress = FALSE) {
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)

  future_map2_lgl(
    .x = .f,
    .y = .x,
    .f = purrr::invoke,
    ...,
    .env = .env,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )
}

#' @rdname future_invoke_map
#' @export
future_invoke_map_raw <- function(.f,
                                  .x = list(NULL),
                                  ...,
                                  .env = NULL,
                                  .options = furrr_options(),
                                  .env_globals = parent.frame(),
                                  .progress = FALSE) {
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)

  future_map2_raw(
    .x = .f,
    .y = .x,
    .f = purrr::invoke,
    ...,
    .env = .env,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )
}

#' @rdname future_invoke_map
#' @export
future_invoke_map_dfr <- function(.f,
                                  .x = list(NULL),
                                  ...,
                                  .env = NULL,
                                  .options = furrr_options(),
                                  .env_globals = parent.frame(),
                                  .progress = FALSE) {
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)

  future_map2_dfr(
    .x = .f,
    .y = .x,
    .f = purrr::invoke,
    ...,
    .env = .env,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )
}

#' @rdname future_invoke_map
#' @export
future_invoke_map_dfc <- function(.f,
                                  .x = list(NULL),
                                  ...,
                                  .env = NULL,
                                  .options = furrr_options(),
                                  .env_globals = parent.frame(),
                                  .progress = FALSE) {
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)

  future_map2_dfc(
    .x = .f,
    .y = .x,
    .f = purrr::invoke,
    ...,
    .env = .env,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )
}

# ------------------------------------------------------------------------------

as_invoke_function <- function(f) {
  if (is.function(f)) {
    list(f)
  }
  else {
    f
  }
}
