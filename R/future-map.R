#' Apply a function to each element of a vector via futures
#'
#' These functions work exactly the same as [purrr::map()] functions, but allow
#' you to run the map in parallel. The documentation is
#' adapted from both `purrr::map()`, and `future.apply::future_lapply()`,
#' so look there for more details.
#'
#' @inheritParams purrr::map
#'
#' @param .env The environment to look for globals required by `.f`, `...`, and
#'   `.x`.
#'
#' @param .options The `future` specific options to use with the workers. This must
#' be the result from a call to [furrr_options()].
#'
#' @param .progress `r lifecycle::badge("deprecated")`:
#'   No longer has any effect. Use the
#'   [progressr](https://cran.r-project.org/web/packages/progressr/index.html)
#'   package instead.
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
#' # future_map(1, ~y, .options = furrr_options(globals = "x"))
#'
#' # y is exported
#' future_map(1, ~y, .options = furrr_options(globals = "y"))
#'
#' \dontshow{
#' # Close open connections for R CMD Check
#' if (!inherits(plan(), "sequential")) plan(sequential)
#' }
#' @export
future_map <- function(.x,
                       .f,
                       ...,
                       .env = parent.frame(),
                       .options = furrr_options(),
                       .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_map")

  furrr_map_template(
    .x = .x,
    .f = .f,
    .options = .options,
    .type = "list",
    .map_fn = purrr::map,
    .env = .env,
    .env_dots = environment()
  )
}

#' @rdname future_map
#' @export
future_map_chr <- function(.x,
                           .f,
                           ...,
                           .env = parent.frame(),
                           .options = furrr_options(),
                           .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_map_chr")

  furrr_map_template(
    .x = .x,
    .f = .f,
    .options = .options,
    .type = "character",
    .map_fn = purrr::map_chr,
    .env = .env,
    .env_dots = environment()
  )
}

#' @rdname future_map
#' @export
future_map_dbl <- function(.x,
                           .f,
                           ...,
                           .env = parent.frame(),
                           .options = furrr_options(),
                           .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_map_dbl")

  furrr_map_template(
    .x = .x,
    .f = .f,
    .options = .options,
    .type = "double",
    .map_fn = purrr::map_dbl,
    .env = .env,
    .env_dots = environment()
  )
}

#' @rdname future_map
#' @export
future_map_int <- function(.x,
                           .f,
                           ...,
                           .env = parent.frame(),
                           .options = furrr_options(),
                           .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_map_int")

  furrr_map_template(
    .x = .x,
    .f = .f,
    .options = .options,
    .type = "integer",
    .map_fn = purrr::map_int,
    .env = .env,
    .env_dots = environment()
  )
}

#' @rdname future_map
#' @export
future_map_lgl <- function(.x,
                           .f,
                           ...,
                           .env = parent.frame(),
                           .options = furrr_options(),
                           .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_map_lgl")

  furrr_map_template(
    .x = .x,
    .f = .f,
    .options = .options,
    .type = "logical",
    .map_fn = purrr::map_lgl,
    .env = .env,
    .env_dots = environment()
  )
}

#' @rdname future_map
#' @export
future_map_dfr <- function(.x,
                           .f,
                           ...,
                           .id = NULL,
                           .env = parent.frame(),
                           .options = furrr_options(),
                           .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_map_dfr")

  # Passing through the template doesn't work because of the way fold() works.
  # Could parameterize around fold(res, ___), but this is easier
  if (!rlang::is_installed("dplyr")) {
    rlang::abort("`future_map_dfr()` requires dplyr")
  }

  res <- future_map(.x, .f, ..., .env = .env, .options = .options)
  dplyr::bind_rows(res, .id = .id)
}

#' @rdname future_map
#' @export
future_map_dfc <- function(.x,
                           .f,
                           ...,
                           .env = parent.frame(),
                           .options = furrr_options(),
                           .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_map_dfc")

  # Passing through the template doesn't work because of the way fold() works.
  # Could parameterize around fold(res, ___), but this is easier
  if (!rlang::is_installed("dplyr")) {
    rlang::abort("`future_map_dfc()` requires dplyr")
  }

  res <- future_map(.x, .f, ..., .env = .env, .options = .options)
  dplyr::bind_cols(res)
}

# ------------------------------------------------------------------------------

#' Apply a function to each element of a vector conditionally via futures
#'
#' These functions work exactly the same as [purrr::map_if()] and
#' [purrr::map_at()], but allow you to run them in parallel.
#'
#' @inheritParams purrr::map_if
#' @inheritParams future_map
#'
#' @return
#' Both functions return a list the same length as `.x` with the elements
#' conditionally transformed.
#'
#' @examples
#' library(furrr)
#' \donttest{plan(multiprocess)}
#'
#' # Modify the even elements
#' future_map_if(1:5, ~.x %% 2 == 0L, ~ -1)
#'
#' future_map_at(1:5, c(1, 5), ~ -1)
#' \dontshow{
#' # Close open connections for R CMD Check
#' if (!inherits(plan(), "sequential")) plan(sequential)
#' }
#' @export
#' @inheritParams purrr::map_if
future_map_if <- function(.x,
                          .p,
                          .f,
                          ...,
                          .env = parent.frame(),
                          .options = furrr_options(),
                          .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_map_if")

  sel <- probe(.x, .p)
  out <- purrr::list_along(.x)
  out[sel] <- future_map(.x[sel], .f, ..., .env = .env, .options = .options)
  out[!sel] <- .x[!sel]
  set_names(out, names(.x))
}

#' @rdname future_map_if
#' @export
#' @inheritParams purrr::map_at
future_map_at <- function(.x,
                          .at,
                          .f,
                          ...,
                          .env = parent.frame(),
                          .options = furrr_options(),
                          .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_map_at")

  sel <- inv_which(.x, .at)
  out <- purrr::list_along(.x)
  out[sel] <- future_map(.x[sel], .f, ..., .env = .env, .options = .options)
  out[!sel] <- .x[!sel]
  set_names(out, names(.x))
}

