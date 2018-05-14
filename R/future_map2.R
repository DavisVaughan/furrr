#' Map over multiple inputs simultaneously via futures
#'
#' These functions work exactly the same as [purrr::map2()] functions, but allow
#' you to run the map in parallel. Note that "parallel" as described in `purrr`
#' is just saying that you are working with multiple inputs, and parallel in
#' this case means that you can work on multiple inputs AND process
#' them all in parallel as well.
#'
#' @inheritParams purrr::map2
#' @inheritParams future_map
#'
#' @return
#' An atomic vector, list, or data frame, depending on the suffix.
#' Atomic vectors and lists will be named if `.x` or the first element of `.l` is named.
#'
#' If all input is length 0, the output will be length 0.
#' If any input is length 1, it will be recycled to the length of the longest.
#'
#'
#' @examples
#'
#' library(furrr)
#' \donttest{
#' plan(multiprocess)
#' }
#'
#' x <- list(1, 10, 100)
#' y <- list(1, 2, 3)
#' z <- list(5, 50, 500)
#'
#' future_map2(x, y, ~ .x + .y)
#'
#' # Split into pieces, fit model to each piece, then predict
#' by_cyl <- split(mtcars, mtcars$cyl)
#' mods <- future_map(by_cyl, ~ lm(mpg ~ wt, data = .))
#' future_map2(mods, by_cyl, predict)
#'
#' future_pmap(list(x, y, z), sum)
#'
#' # Matching arguments by position
#' future_pmap(list(x, y, z), function(a, b ,c) a / (b + c))
#'
#' # Vectorizing a function over multiple arguments
#' df <- data.frame(
#'   x = c("apple", "banana", "cherry"),
#'   pattern = c("p", "n", "h"),
#'   replacement = c("x", "f", "q"),
#'   stringsAsFactors = FALSE
#' )
#' future_pmap(df, gsub)
#' future_pmap_chr(df, gsub)
#'
#' @export
future_map2 <- function(.x, .y, .f, ..., .progress = FALSE, .options = future_options()) {
  future_map2_template(purrr::map, "list", .x, .y, .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_map2
#' @export
future_map2_chr <- function(.x, .y, .f, ..., .progress = FALSE, .options = future_options()) {
  future_map2_template(purrr::map_chr, "character", .x, .y, .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_map2
#' @export
future_map2_dbl <- function(.x, .y, .f, ..., .progress = FALSE, .options = future_options()) {
  future_map2_template(purrr::map_dbl, "double", .x, .y, .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_map2
#' @export
future_map2_int <- function(.x, .y, .f, ..., .progress = FALSE, .options = future_options()) {
  future_map2_template(purrr::map_int, "integer", .x, .y, .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_map2
#' @export
future_map2_lgl <- function(.x, .y, .f, ..., .progress = FALSE, .options = future_options()) {
  future_map2_template(purrr::map_lgl, "logical", .x, .y, .f, ..., .progress = .progress, .options = .options)
}

#' @rdname future_map2
#' @export
future_map2_dfr <- function(.x, .y, .f, ..., .id = NULL, .progress = FALSE, .options = future_options()) {
  # Passing through the template doesn't work because of the way fold() works.
  # Could parameterize around fold(res, ___), but this is easier
  if (!rlang::is_installed("dplyr")) {
    rlang::abort("`future_map2_dfr()` requires dplyr")
  }

  res <- future_map2(.x, .y, .f, ..., .progress = .progress, .options = .options)
  dplyr::bind_rows(res, .id = .id)
}

#' @rdname future_map2
#' @export
future_map2_dfc <- function(.x, .y, .f, ..., .progress = FALSE, .options = future_options()) {
  # Passing through the template doesn't work because of the way fold() works.
  # Could parameterize around fold(res, ___), but this is easier
  if (!rlang::is_installed("dplyr")) {
    rlang::abort("`future_map2_dfc()` requires dplyr")
  }

  res <- future_map2(.x, .y, .f, ..., .progress = .progress, .options = .options)
  dplyr::bind_cols(res)
}
