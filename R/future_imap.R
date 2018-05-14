#' Apply a function to each element of a vector, and its index via futures
#'
#' These functions work exactly the same as [purrr::imap()] functions, but allow
#' you to map in parallel.
#'
#' @inheritParams purrr::imap
#' @inheritParams future_map
#'
#' @return
#' A vector the same length as .x.
#'
#' @examples
#'
#' library(furrr)
#' \donttest{
#' plan(multiprocess)
#' }
#'
#' future_imap_chr(sample(10), ~ paste0(.y, ": ", .x))
#'
#' @export
future_imap <- function(.x, .f, ..., .progress = FALSE, .options = future_options()) {
  future_map2(.x, vec_index(.x), .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_imap
#' @export
future_imap_chr <- function(.x, .f, ..., .progress = FALSE, .options = future_options()) {
  future_map2_chr(.x, vec_index(.x), .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_imap
#' @export
future_imap_dbl <- function(.x, .f, ..., .progress = FALSE, .options = future_options()) {
  future_map2_dbl(.x, vec_index(.x), .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_imap
#' @export
future_imap_int <- function(.x, .f, ..., .progress = FALSE, .options = future_options()) {
  future_map2_int(.x, vec_index(.x), .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_imap
#' @export
future_imap_lgl <- function(.x, .f, ..., .progress = FALSE, .options = future_options()) {
  future_map2_lgl(.x, vec_index(.x), .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_imap
#' @export
future_imap_dfr <- function(.x, .f, ..., .id = NULL, .progress = FALSE, .options = future_options()) {
  future_map2_dfr(.x, vec_index(.x), .f, ..., .id = .id, .progress = .progress, .options = .options)
}

#' @rdname future_imap
#' @export
future_imap_dfc <- function(.x, .f, ..., .progress = FALSE, .options = future_options()) {
  future_map2_dfc(.x, vec_index(.x), .f, ..., .progress = .progress, .options = .options)
}
