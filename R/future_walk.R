#' @rdname future_map
#' @export
future_walk <- function(.x, .f, ..., .progress = FALSE, .options = future_options()) {
  # Can't just capture the invisible() output from walk().
  # We iterate over seq_along() in the template so the output would not be .x
  # Instead, just return .x invisibly to mimic behavior
  future_map_template(purrr::walk, "list", .x, .f, ..., .progress = .progress, .options = .options)
  invisible(.x)
}

#' @rdname future_map2
#' @export
future_walk2 <- function(.x, .y, .f, ..., .progress = FALSE, .options = future_options()) {
  future_pwalk(list(.x, .y), .f, ..., .progress = .progress, .options = .options)
  invisible(.x)
}

#' @rdname future_map2
#' @export
future_pwalk <- function(.l, .f, ..., .progress = FALSE, .options = future_options()) {
  future_pmap_template(purrr::pwalk, "list", .l, .f, ..., .progress = .progress, .options = .options)
  invisible(.l)
}

#' @rdname future_imap
#' @export
future_iwalk <- function(.x, .f, ..., .progress = FALSE, .options = future_options()) {
  .f <- purrr::as_mapper(.f, ...)
  future_walk2(.x, vec_index(.x), .f, ..., .progress = .progress, .options = .options)
}
