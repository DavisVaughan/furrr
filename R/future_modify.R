#' Modify elements selectively via futures
#'
#' These functions work exactly the same as [purrr::modify()] functions, but allow
#' you to modify in parallel.
#'
#' @inheritParams purrr::modify
#' @inheritParams future_map
#'
#' @details
#'
#' From `purrr`) Since the transformation can alter the structure of the input;
#' it's your responsibility to ensure that the transformation produces a valid
#' output. For example, if you're modifying a data frame, `.f` must preserve the
#' length of the input.
#'
#' @return
#' An object the same class as .x
#'
#' @examples
#'
#' library(furrr)
#' library(dplyr) # for the pipe
#'
#' \donttest{
#' plan(multiprocess)
#' }
#'
#' # Convert each col to character, in parallel
#' future_modify(mtcars, as.character)
#'
#' iris %>%
#'  future_modify_if(is.factor, as.character) %>%
#'  str()
#'
#' mtcars %>% future_modify_at(c(1, 4, 5), as.character) %>% str()
#'
#' @export
future_modify <- function(.x, .f, ..., .progress = FALSE, .options = future_options()) {
  UseMethod("future_modify")
}

#' @export
future_modify.default <- function(.x, .f, ..., .progress = FALSE, .options = future_options()) {
  .x[] <- future_map(.x, .f, ..., .progress = .progress, .options = .options)
  .x
}

#' @rdname future_modify
#' @export
future_modify_at <- function(.x, .at, .f, ..., .progress = FALSE, .options = future_options()) {
  UseMethod("future_modify_at")
}

#' @export
future_modify_at.default <- function(.x, .at, .f, ..., .progress = FALSE, .options = future_options()) {
  sel <- inv_which(.x, .at)
  .x[sel] <- future_map(.x[sel], .f, ..., .progress = .progress, .options = .options)
  .x
}

#' @rdname future_modify
#' @export
future_modify_if <- function(.x, .p, .f, ..., .progress = FALSE, .options = future_options()) {
  UseMethod("future_modify_if")
}

#' @export
future_modify_if.default <- function(.x, .p, .f, ..., .progress = FALSE, .options = future_options()) {
  sel <- probe(.x, .p)
  .x[sel] <- future_map(.x[sel], .f, ..., .progress = .progress, .options = .options)
  .x
}
