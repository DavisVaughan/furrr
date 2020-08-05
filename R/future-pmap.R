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
    l = .l,
    fn = .f,
    dots = list(...),
    options = .options,
    type = "list",
    map_fn = purrr::pmap,
    env_globals = .env_globals
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
    l = .l,
    fn = .f,
    dots = list(...),
    options = .options,
    type = "character",
    map_fn = purrr::pmap_chr,
    env_globals = .env_globals
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
    l = .l,
    fn = .f,
    dots = list(...),
    options = .options,
    type = "double",
    map_fn = purrr::pmap_dbl,
    env_globals = .env_globals
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
    l = .l,
    fn = .f,
    dots = list(...),
    options = .options,
    type = "integer",
    map_fn = purrr::pmap_int,
    env_globals = .env_globals
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
    l = .l,
    fn = .f,
    dots = list(...),
    options = .options,
    type = "logical",
    map_fn = purrr::pmap_lgl,
    env_globals = .env_globals
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

  names_to <- compat_id(.id)

  out <- future_pmap(
    .l = .l,
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals
  )

  vctrs::vec_rbind(!!!out, .names_to = names_to)
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

  out <- future_pmap(
    .l = .l,
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals
  )

  furrr_list_cbind(out)
}
