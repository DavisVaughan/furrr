#' Invoke functions via futures
#'
#' These functions work exactly the same as [purrr::invoke_map()] functions, but allow
#' you to invoke in parallel.
#'
#' @inheritParams purrr::invoke_map
#' @inheritParams future_map
#'
#' @param .f A list of functions.
#' @param .x A list of argument-lists the same length as `.f` (or length 1).
#' The default argument, `list(NULL)`, will be recycled to the same length as
#' `.f`, and will call each function with no arguments
#' (apart from any supplied in `...`.)
#'
#' @examples
#'
#' \donttest{
#' plan(multiprocess)
#' }
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
#' future_invoke_map(df$f, df$params)
#'
#'
#' @export
#'
future_invoke_map <- function(.f, .x = list(NULL), ..., .env = NULL, .progress = FALSE, .options = future_options()) {
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  future_map2(.f, .x, purrr::invoke, ..., .env = .env, .progress = .progress, .options = .options)
}

#' @rdname future_invoke_map
#' @export
future_invoke_map_chr <- function(.f, .x = list(NULL), ..., .env = NULL, .progress = FALSE, .options = future_options()) {
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  future_map2_chr(.f, .x, purrr::invoke, ..., .env = .env, .progress = .progress, .options = .options)
}

#' @rdname future_invoke_map
#' @export
future_invoke_map_dbl <- function(.f, .x = list(NULL), ..., .env = NULL, .progress = FALSE, .options = future_options()) {
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  future_map2_dbl(.f, .x, purrr::invoke, ..., .env = .env, .progress = .progress, .options = .options)
}

#' @rdname future_invoke_map
#' @export
future_invoke_map_int <- function(.f, .x = list(NULL), ..., .env = NULL, .progress = FALSE, .options = future_options()) {
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  future_map2_int(.f, .x, purrr::invoke, ..., .env = .env, .progress = .progress, .options = .options)
}

#' @rdname future_invoke_map
#' @export
future_invoke_map_lgl <- function(.f, .x = list(NULL), ..., .env = NULL, .progress = FALSE, .options = future_options()) {
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  future_map2_lgl(.f, .x, purrr::invoke, ..., .env = .env, .progress = .progress, .options = .options)
}

#' @rdname future_invoke_map
#' @export
future_invoke_map_dfr <- function(.f, .x = list(NULL), ..., .env = NULL, .progress = FALSE, .options = future_options()) {
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  future_map2_dfr(.f, .x, purrr::invoke, ..., .env = .env, .progress = .progress, .options = .options)
}

#' @rdname future_invoke_map
#' @export
future_invoke_map_dfc <- function(.f, .x = list(NULL), ..., .env = NULL, .progress = FALSE, .options = future_options()) {
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  future_map2_dfc(.f, .x, purrr::invoke, ..., .env = .env, .progress = .progress, .options = .options)
}
