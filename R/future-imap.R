#' Apply a function to each element of a vector, and its index via futures
#'
#' These functions work exactly the same as [purrr::imap()] functions,
#' but allow you to map in parallel.
#'
#' @inheritParams purrr::imap
#' @inheritParams future_map
#'
#' @return
#' A vector the same length as .x.
#'
#' @export
#' @examples
#' \donttest{plan(multiprocess)}
#'
#' future_imap_chr(sample(10), ~ paste0(.y, ": ", .x))
#'
#' \dontshow{
#' # Close open connections for R CMD Check
#' if (!inherits(plan(), "sequential")) plan(sequential)
#' }
future_imap <- function(.x,
                        .f,
                        ...,
                        .options = furrr_options(),
                        .env_globals = parent.frame(),
                        .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_imap")

  future_map2(
    .x = .x,
    .y = vec_index(.x),
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals
  )
}

#' @rdname future_imap
#' @export
future_imap_chr <- function(.x,
                            .f,
                            ...,
                            .options = furrr_options(),
                            .env_globals = parent.frame(),
                            .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_imap_chr")

  future_map2_chr(
    .x = .x,
    .y = vec_index(.x),
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals
  )
}

#' @rdname future_imap
#' @export
future_imap_dbl <- function(.x,
                            .f,
                            ...,
                            .options = furrr_options(),
                            .env_globals = parent.frame(),
                            .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_imap_dbl")

  future_map2_dbl(
    .x = .x,
    .y = vec_index(.x),
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals
  )
}

#' @rdname future_imap
#' @export
future_imap_int <- function(.x,
                            .f,
                            ...,
                            .options = furrr_options(),
                            .env_globals = parent.frame(),
                            .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_imap_int")

  future_map2_int(
    .x = .x,
    .y = vec_index(.x),
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals
  )
}

#' @rdname future_imap
#' @export
future_imap_lgl <- function(.x,
                            .f,
                            ...,
                            .options = furrr_options(),
                            .env_globals = parent.frame(),
                            .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_imap_lgl")

  future_map2_lgl(
    .x = .x,
    .y = vec_index(.x),
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals
  )
}

#' @rdname future_imap
#' @export
future_imap_dfr <- function(.x,
                            .f,
                            ...,
                            .id = NULL,
                            .options = furrr_options(),
                            .env_globals = parent.frame(),
                            .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_imap_dfr")

  future_map2_dfr(
    .x = .x,
    .y = vec_index(.x),
    .f = .f,
    ...,
    .id = .id,
    .options = .options,
    .env_globals = .env_globals
  )
}

#' @rdname future_imap
#' @export
future_imap_dfc <- function(.x,
                            .f,
                            ...,
                            .options = furrr_options(),
                            .env_globals = parent.frame(),
                            .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_imap_dfc")

  future_map2_dfc(
    .x = .x,
    .y = vec_index(.x),
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals
  )
}
