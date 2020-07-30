#' @rdname future_map
#' @export
future_walk <- function(.x, .f, ..., .options = furrr_options()) {
  # Can't just capture the invisible() output from walk().
  # We iterate over seq_along() in the template so the output would not be .x
  # Instead, just return .x invisibly to mimic behavior
  future_map_template(purrr::walk, "list", .x, .f, ..., .options = .options)
  invisible(.x)
}

#' @rdname future_map2
#' @export
future_walk2 <- function(.x, .y, .f, ..., .options = furrr_options()) {
  future_pwalk(list(.x, .y), .f, ..., .options = .options)
  invisible(.x)
}

#' @rdname future_map2
#' @export
future_pwalk <- function(.l, .f, ..., .options = furrr_options()) {
  future_pmap_template(purrr::pwalk, "list", .l, .f, ..., .options = .options)
  invisible(.l)
}

#' @rdname future_imap
#' @export
future_iwalk <- function(.x, .f, ..., .options = furrr_options()) {
  .f <- purrr::as_mapper(.f, ...)
  future_walk2(.x, vec_index(.x), .f, ..., .options = .options)
}
