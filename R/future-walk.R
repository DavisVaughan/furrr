#' @rdname future_map
#' @export
future_walk <- function(.x,
                        .f,
                        ...,
                        .options = furrr_options(),
                        .env_globals = parent.frame()) {
  future_map(.x, .f, ..., .options = .options, .env_globals = .env_globals)
  invisible(.x)
}

#' @rdname future_map2
#' @export
future_walk2 <- function(.x,
                         .y,
                         .f,
                         ...,
                         .options = furrr_options(),
                         .env_globals = parent.frame()) {
  future_map2(.x, .y, .f, ..., .options = .options, .env_globals = .env_globals)
  invisible(.x)
}

#' @rdname future_map2
#' @export
future_pwalk <- function(.l,
                         .f,
                         ...,
                         .options = furrr_options(),
                         .env_globals = parent.frame()) {
  future_pmap(.l, .f, ..., .options = .options, .env_globals = .env_globals)
  invisible(.l)
}

#' @rdname future_imap
#' @export
future_iwalk <- function(.x,
                         .f,
                         ...,
                         .options = furrr_options(),
                         .env_globals = parent.frame()) {
  future_walk2(.x, vec_index(.x), .f, ..., .options = .options, .env_globals = .env_globals)
}
