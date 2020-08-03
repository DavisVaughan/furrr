#' @rdname future_map
#' @export
future_walk <- function(.x,
                        .f,
                        ...,
                        .env = parent.frame(),
                        .options = furrr_options()) {
  future_map(.x, .f, ..., .env = .env, .options = .options)
  invisible(.x)
}

#' @rdname future_map2
#' @export
future_walk2 <- function(.x,
                         .y,
                         .f,
                         ...,
                         .env = parent.frame(),
                         .options = furrr_options()) {
  future_map2(.x, .y, .f, ..., .env = .env, .options = .options)
  invisible(.x)
}

#' @rdname future_map2
#' @export
future_pwalk <- function(.l,
                         .f,
                         ...,
                         .env = parent.frame(),
                         .options = furrr_options()) {
  future_pmap(.l, .f, ..., .env = .env, .options = .options)
  invisible(.l)
}

#' @rdname future_imap
#' @export
future_iwalk <- function(.x,
                         .f,
                         ...,
                         .env = parent.frame(),
                         .options = furrr_options()) {
  future_walk2(.x, vec_index(.x), .f, ..., .env = .env, .options = .options)
}
