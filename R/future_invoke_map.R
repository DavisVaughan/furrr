#' Invoke functions via futures
#'
#' These functions work exactly the same as [purrr::invoke_map()] functions, but allow
#' you to invoke in parallel. There are a number of `future.*` arguments
#' to allow you to fine tune the parallel processing.
#'
#' @inheritParams future_map
#' @inheritParams purrr::invoke_map
#'
#' @export
#'
future_invoke_map <- function(.f, .x = list(NULL), ..., .env = NULL, .progress = FALSE, future.globals = TRUE, future.packages = NULL, future.seed = FALSE, future.lazy = FALSE, future.scheduling = 1.0) {
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  future_map2(.f, .x, purrr::invoke, ..., .env = .env, .progress = .progress, future.globals = future.globals, future.packages = future.packages, future.seed = future.seed, future.lazy = future.lazy, future.scheduling = future.scheduling)
}

#' @rdname future_invoke_map
#' @export
future_invoke_map_dfr <- function(.f, .x = list(NULL), ..., .env = NULL, .progress = FALSE, future.globals = TRUE, future.packages = NULL, future.seed = FALSE, future.lazy = FALSE, future.scheduling = 1.0) {
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  future_map2_dfr(.f, .x, purrr::invoke, ..., .env = .env, .progress = .progress, future.globals = future.globals, future.packages = future.packages, future.seed = future.seed, future.lazy = future.lazy, future.scheduling = future.scheduling)
}
