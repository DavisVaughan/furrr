#' @rdname future_map
#' @export
future_walk <- function(.x,
                        .f,
                        ...,
                        .options = furrr_options(),
                        .env_globals = parent.frame(),
                        .progress = FALSE) {
  furrr_map_template(
    x = .x,
    fn = .f,
    dots = list(...),
    options = .options,
    progress = .progress,
    type = "list",
    map_fn = purrr::walk,
    env_globals = .env_globals
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
  furrr_map2_template(
    x = .x,
    y = .y,
    fn = .f,
    dots = list(...),
    options = .options,
    progress = .progress,
    type = "list",
    map_fn = purrr::walk2,
    env_globals = .env_globals
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
  furrr_pmap_template(
    l = .l,
    fn = .f,
    dots = list(...),
    options = .options,
    progress = .progress,
    type = "list",
    map_fn = purrr::pwalk,
    env_globals = .env_globals
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
