#' @rdname future_map2
#' @export
future_pmap <- function(.l,
                        .f,
                        ...,
                        .options = furrr_options(),
                        .env_globals = parent.frame(),
                        .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_pmap")

  furrr_pmap_template(
    .l = .l,
    .f = .f,
    .options = .options,
    .type = "list",
    .map_fn = purrr::pmap,
    .env_globals = .env_globals,
    .env_dots = environment()
  )
}

#' @rdname future_map2
#' @export
future_pmap_chr <- function(.l,
                            .f,
                            ...,
                            .options = furrr_options(),
                            .env_globals = parent.frame(),
                            .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_pmap_chr")

  furrr_pmap_template(
    .l = .l,
    .f = .f,
    .options = .options,
    .type = "character",
    .map_fn = purrr::pmap_chr,
    .env_globals = .env_globals,
    .env_dots = environment()
  )
}

#' @rdname future_map2
#' @export
future_pmap_dbl <- function(.l,
                            .f,
                            ...,
                            .options = furrr_options(),
                            .env_globals = parent.frame(),
                            .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_pmap_dbl")

  furrr_pmap_template(
    .l = .l,
    .f = .f,
    .options = .options,
    .type = "double",
    .map_fn = purrr::pmap_dbl,
    .env_globals = .env_globals,
    .env_dots = environment()
  )
}

#' @rdname future_map2
#' @export
future_pmap_int <- function(.l,
                            .f,
                            ...,
                            .options = furrr_options(),
                            .env_globals = parent.frame(),
                            .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_pmap_int")

  furrr_pmap_template(
    .l = .l,
    .f = .f,
    .options = .options,
    .type = "integer",
    .map_fn = purrr::pmap_int,
    .env_globals = .env_globals,
    .env_dots = environment()
  )
}

#' @rdname future_map2
#' @export
future_pmap_lgl <- function(.l,
                            .f,
                            ...,
                            .options = furrr_options(),
                            .env_globals = parent.frame(),
                            .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_pmap_lgl")

  furrr_pmap_template(
    .l = .l,
    .f = .f,
    .options = .options,
    .type = "logical",
    .map_fn = purrr::pmap_lgl,
    .env_globals = .env_globals,
    .env_dots = environment()
  )
}

#' @rdname future_map2
#' @export
future_pmap_dfr <- function(.l,
                            .f,
                            ...,
                            .id = NULL,
                            .options = furrr_options(),
                            .env_globals = parent.frame(),
                            .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_pmap_dfr")

  if (!rlang::is_installed("dplyr")) {
    rlang::abort("`future_map_dfr()` requires dplyr")
  }

  res <- future_pmap(.l, .f, ..., .options = .options, .env_globals = .env_globals)
  dplyr::bind_rows(res, .id = .id)
}

#' @rdname future_map2
#' @export
future_pmap_dfc <- function(.l,
                            .f,
                            ...,
                            .options = furrr_options(),
                            .env_globals = parent.frame(),
                            .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_pmap_dfc")

  if (!rlang::is_installed("dplyr")) {
    rlang::abort("`future_map_dfc()` requires dplyr")
  }

  res <- future_pmap(.l, .f, ..., .options = .options, .env_globals = .env_globals)
  dplyr::bind_cols(res)
}
