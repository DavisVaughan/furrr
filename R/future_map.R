#' Apply a function to each element of a vector via futures
#'
#' These functions work exactly the same as [purrr::map()] functions, but allow
#' you to run the map in parallel. The documentation is
#' adapted from both `purrr::map()`, and `future.apply::future_lapply()`,
#' so look there for more details.
#'
#' @inheritParams purrr::map
#'
#' @param .progress A logical, for whether or not to print a progress bar for
#' multiprocess, multisession, and multicore plans.
#'
#' @param .options The `future` specific options to use with the workers. This must
#' be the result from a call to [future_options()].
#'
#' @return
#' All functions return a vector the same length as `.x`.
#'
#' [future_map()] returns a list, [future_map_lgl()] a logical vector,
#' [future_map_int()] an integer vector, [future_map_dbl()] a double vector,
#' and [future_map_chr()] a character vector.
#' The output of `.f` will be automatically typed upwards,
#'  e.g. logical -> integer -> double -> character.
#'
#'
#' @examples
#'
#' library(furrr)
#' library(dplyr) # for the pipe
#'
#' \donttest{
#' plan(multiprocess)
#' }
#'
#' 1:10 %>%
#'   future_map(rnorm, n = 10) %>%
#'   future_map_dbl(mean)
#'
#' # If each element of the output is a data frame, use
#' # future_map_dfr to row-bind them together:
#' mtcars %>%
#'   split(.$cyl) %>%
#'   future_map(~ lm(mpg ~ wt, data = .x)) %>%
#'   future_map_dfr(~ as.data.frame(t(as.matrix(coef(.)))))
#'
#' # You can be explicit about what gets exported to the workers
#'
#' # To see this, use multisession (NOT multicore if on a Mac as the forked workers
#' # still have access to this environment)
#' \donttest{
#' plan(multisession)
#' }
#' x <- 1
#' y <- 2
#'
#' # This will fail, y is not exported (no black magic occurs)
#' # future_map(1, ~y, .options = future_options(globals = "x"))
#'
#' # y is exported
#' future_map(1, ~y, .options = future_options(globals = "y"))
#'
#'
#' @export
future_map <- function(.x, .f, ..., .progress = FALSE, .options = future_options()) {
  future_map_template(purrr::map, "list", .x, .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_map
#' @export
future_map_chr <- function(.x, .f, ..., .progress = FALSE, .options = future_options()) {
  future_map_template(purrr::map_chr, "character", .x, .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_map
#' @export
future_map_dbl <- function(.x, .f, ..., .progress = FALSE, .options = future_options()) {
  future_map_template(purrr::map_dbl, "double", .x, .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_map
#' @export
future_map_int <- function(.x, .f, ..., .progress = FALSE, .options = future_options()) {
  future_map_template(purrr::map_int, "integer", .x, .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_map
#' @export
future_map_lgl <- function(.x, .f, ..., .progress = FALSE, .options = future_options()) {
  future_map_template(purrr::map_lgl, "logical", .x, .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_map
#' @export
future_map_dfr <- function(.x, .f, ..., .id = NULL, .progress = FALSE, .options = future_options()) {
  # Passing through the template doesn't work because of the way fold() works.
  # Could parameterize around fold(res, ___), but this is easier
  if (!rlang::is_installed("dplyr")) {
    rlang::abort("`future_map_dfr()` requires dplyr")
  }

  res <- future_map(.x, .f, ..., .progress = .progress, .options = .options)
  dplyr::bind_rows(res, .id = .id)
}

#' @rdname future_map
#' @export
future_map_dfc <- function(.x, .f, ..., .progress = FALSE, .options = future_options()) {
  # Passing through the template doesn't work because of the way fold() works.
  # Could parameterize around fold(res, ___), but this is easier
  if (!rlang::is_installed("dplyr")) {
    rlang::abort("`future_map_dfc()` requires dplyr")
  }

  res <- future_map(.x, .f, ..., .progress = .progress, .options = .options)
  dplyr::bind_cols(res)
}

#' @rdname future_map
#' @export
#' @importFrom purrr list_along set_names
future_map_if <- function(.x, .p, .f, ..., .progress = FALSE, .options = future_options()) {
  sel <- probe(.x, .p)
  out <- list_along(.x)
  out[sel] <- future_map(.x[sel], .f, ..., .progress = .progress, .options = .options)
  out[!sel] <- .x[!sel]
  set_names(out, names(.x))
}

#' @rdname future_map
#' @export
#' @importFrom purrr list_along set_names
future_map_at <- function(.x, .at, .f, ..., .progress = FALSE, .options = future_options()) {
  sel <- inv_which(.x, .at)
  out <- list_along(.x)
  out[sel] <- future_map(.x[sel], .f, ..., .progress = .progress, .options = .options)
  out[!sel] <- .x[!sel]
  set_names(out, names(.x))
}

