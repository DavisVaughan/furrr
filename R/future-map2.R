#' Map over multiple inputs simultaneously via futures
#'
#' These functions work exactly the same as [purrr::map2()] and its variants,
#' but allow you to map in parallel. Note that "parallel" as described in purrr
#' is just saying that you are working with multiple inputs, and parallel in
#' this case means that you can work on multiple inputs and process them all in
#' parallel as well.
#'
#' @inheritParams purrr::map2
#' @inheritParams future_map
#'
#' @return An atomic vector, list, or data frame, depending on the suffix.
#'   Atomic vectors and lists will be named if `.x` or the first element of `.l`
#'   is named.
#'
#'   If all input is length 0, the output will be length 0. If any input is
#'   length 1, it will be recycled to the length of the longest.
#'
#' @export
#' @examples
#' \donttest{plan(multisession, workers = 2)}
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
#'
#' future_pmap(df, gsub)
#' future_pmap_chr(df, gsub)
#'
#' \dontshow{
#' # Close open connections for R CMD Check
#' if (!inherits(plan(), "sequential")) plan(sequential)
#' }
future_map2 <- function(.x,
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
    map_fn = purrr::map2,
    env_globals = .env_globals
  )
}

#' @rdname future_map2
#' @export
future_map2_chr <- function(.x,
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
    type = "character",
    map_fn = purrr::map2_chr,
    env_globals = .env_globals
  )
}

#' @rdname future_map2
#' @export
future_map2_dbl <- function(.x,
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
    type = "double",
    map_fn = purrr::map2_dbl,
    env_globals = .env_globals
  )
}

#' @rdname future_map2
#' @export
future_map2_int <- function(.x,
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
    type = "integer",
    map_fn = purrr::map2_int,
    env_globals = .env_globals
  )
}

#' @rdname future_map2
#' @export
future_map2_lgl <- function(.x,
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
    type = "logical",
    map_fn = purrr::map2_lgl,
    env_globals = .env_globals
  )
}

#' @rdname future_map2
#' @export
future_map2_raw <- function(.x,
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
    type = "raw",
    map_fn = purrr::map2_raw,
    env_globals = .env_globals
  )
}

#' @rdname future_map2
#' @export
future_map2_dfr <- function(.x,
                            .y,
                            .f,
                            ...,
                            .id = NULL,
                            .options = furrr_options(),
                            .env_globals = parent.frame(),
                            .progress = FALSE) {
  if (!rlang::is_installed("dplyr")) {
    rlang::abort("`future_map2_dfr()` requires dplyr")
  }

  res <- future_map2(
    .x = .x,
    .y = .y,
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )

  dplyr::bind_rows(res, .id = .id)
}

#' @rdname future_map2
#' @export
future_map2_dfc <- function(.x,
                            .y,
                            .f,
                            ...,
                            .options = furrr_options(),
                            .env_globals = parent.frame(),
                            .progress = FALSE) {
  if (!rlang::is_installed("dplyr")) {
    rlang::abort("`future_map2_dfc()` requires dplyr")
  }

  res <- future_map2(
    .x = .x,
    .y = .y,
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )

  dplyr::bind_cols(res)
}
