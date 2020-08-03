#' @rdname future_map2
#' @export
future_pmap <- function(.l,
                        .f,
                        ...,
                        .env = parent.frame(),
                        .options = furrr_options(),
                        .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_pmap")

  furrr_pmap_template(
    .l = .l,
    .f = .f,
    .options = .options,
    .type = "list",
    .map_fn = purrr::pmap,
    .env = .env,
    .env_dots = environment()
  )
}

#' @rdname future_map2
#' @export
future_pmap_chr <- function(.l,
                            .f,
                            ...,
                            .env = parent.frame(),
                            .options = furrr_options(),
                            .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_pmap_chr")

  furrr_pmap_template(
    .l = .l,
    .f = .f,
    .options = .options,
    .type = "character",
    .map_fn = purrr::pmap_chr,
    .env = .env,
    .env_dots = environment()
  )
}

#' @rdname future_map2
#' @export
future_pmap_dbl <- function(.l,
                            .f,
                            ...,
                            .env = parent.frame(),
                            .options = furrr_options(),
                            .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_pmap_dbl")

  furrr_pmap_template(
    .l = .l,
    .f = .f,
    .options = .options,
    .type = "double",
    .map_fn = purrr::pmap_dbl,
    .env = .env,
    .env_dots = environment()
  )
}

#' @rdname future_map2
#' @export
future_pmap_int <- function(.l,
                            .f,
                            ...,
                            .env = parent.frame(),
                            .options = furrr_options(),
                            .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_pmap_int")

  furrr_pmap_template(
    .l = .l,
    .f = .f,
    .options = .options,
    .type = "integer",
    .map_fn = purrr::pmap_int,
    .env = .env,
    .env_dots = environment()
  )
}

#' @rdname future_map2
#' @export
future_pmap_lgl <- function(.l,
                            .f,
                            ...,
                            .env = parent.frame(),
                            .options = furrr_options(),
                            .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_pmap_lgl")

  furrr_pmap_template(
    .l = .l,
    .f = .f,
    .options = .options,
    .type = "logical",
    .map_fn = purrr::pmap_lgl,
    .env = .env,
    .env_dots = environment()
  )
}

#' @rdname future_map2
#' @export
future_pmap_dfr <- function(.l,
                            .f,
                            ...,
                            .id = NULL,
                            .env = parent.frame(),
                            .options = furrr_options(),
                            .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_pmap_dfr")

  # Passing through the template doesn't work because of the way fold() works.
  # Could parameterize around fold(res, ___), but this is easier
  if (!rlang::is_installed("dplyr")) {
    rlang::abort("`future_map_dfr()` requires dplyr")
  }

  res <- future_pmap(.l, .f, ..., .env = .env, .options = .options)
  dplyr::bind_rows(res, .id = .id)
}

#' @rdname future_map2
#' @export
future_pmap_dfc <- function(.l,
                            .f,
                            ...,
                            .env = parent.frame(),
                            .options = furrr_options(),
                            .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_pmap_dfc")

  # Passing through the template doesn't work because of the way fold() works.
  # Could parameterize around fold(res, ___), but this is easier
  if (!rlang::is_installed("dplyr")) {
    rlang::abort("`future_map_dfc()` requires dplyr")
  }

  res <- future_pmap(.l, .f, ..., .env = .env, .options = .options)
  dplyr::bind_cols(res)
}
