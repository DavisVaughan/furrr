#' @rdname future_map2
#' @export
future_pmap <- function(.l, .f, ..., .progress = FALSE, .options = future_options()) {
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  future_pmap_template(purrr::pmap, "list", .l, .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_map2
#' @export
future_pmap_chr <- function(.l, .f, ..., .progress = FALSE, .options = future_options()) {
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  future_pmap_template(purrr::pmap_chr, "character", .l, .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_map2
#' @export
future_pmap_dbl <- function(.l, .f, ..., .progress = FALSE, .options = future_options()) {
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  future_pmap_template(purrr::pmap_dbl, "double", .l, .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_map2
#' @export
future_pmap_int <- function(.l, .f, ..., .progress = FALSE, .options = future_options()) {
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  future_pmap_template(purrr::pmap_int, "integer", .l, .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_map2
#' @export
future_pmap_lgl <- function(.l, .f, ..., .progress = FALSE, .options = future_options()) {
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  future_pmap_template(purrr::pmap_lgl, "logical", .l, .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_map2
#' @export
future_pmap_dfr <- function(.l, .f, ..., .id = NULL, .progress = FALSE, .options = future_options()) {
  # Passing through the template doesn't work because of the way fold() works.
  # Could parameterize around fold(res, ___), but this is easier
  if (!rlang::is_installed("dplyr")) {
    rlang::abort("`future_map_dfr()` requires dplyr")
  }

  res <- future_pmap(.l, .f, ..., .progress = .progress, .options = .options)
  dplyr::bind_rows(res, .id = .id)
}

#' @rdname future_map2
#' @export
future_pmap_dfc <- function(.l, .f, ..., .progress = FALSE, .options = future_options()) {
  # Passing through the template doesn't work because of the way fold() works.
  # Could parameterize around fold(res, ___), but this is easier
  if (!rlang::is_installed("dplyr")) {
    rlang::abort("`future_map_dfc()` requires dplyr")
  }

  res <- future_pmap(.l, .f, ..., .progress = .progress, .options = .options)
  dplyr::bind_cols(res)
}
