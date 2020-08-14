#' @rdname future_map
#' @export
future_walk <- function(.x,
                        .f,
                        ...,
                        .options = furrr_options(),
                        .env_globals = parent.frame(),
                        .progress = FALSE) {
  future_map(
    .x = .x,
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )

  invisible(.x)
}

#' @rdname future_map2
#' @export
future_walk2 <- function(.x,
                         .y,
                         .f,
                         ...,
                         .options = furrr_options(),
                         .env_globals = parent.frame(),
                         .progress = FALSE) {
  future_map2(
    .x = .x,
    .y = .y,
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )

  invisible(.x)
}

#' @rdname future_map2
#' @export
future_pwalk <- function(.l,
                         .f,
                         ...,
                         .options = furrr_options(),
                         .env_globals = parent.frame(),
                         .progress = FALSE) {
  future_pmap(
    .l = .l,
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )

  invisible(.l)
}

#' @rdname future_imap
#' @export
future_iwalk <- function(.x,
                         .f,
                         ...,
                         .options = furrr_options(),
                         .env_globals = parent.frame(),
                         .progress = FALSE) {
  future_walk2(
    .x = .x,
    .y = vec_index(.x),
    .f = .f, ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )
}
