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
#' \dontshow{
#' # Close open connections for R CMD Check
#' if (!inherits(plan(), "sequential")) plan(sequential)
#' }
#' @export
future_modify <- function(.x, .f, ..., .options = furrr_options(), .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_modify")
  UseMethod("future_modify")
}

#' @export
future_modify.default <- function(.x, .f, ..., .options = furrr_options(), .progress = deprecated()) {
  .x[] <- future_map(.x, .f, ..., .options = .options)
  .x
}

#' @rdname future_modify
#' @export
future_modify_at <- function(.x, .at, .f, ..., .options = furrr_options(), .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_modify_at")
  UseMethod("future_modify_at")
}

#' @export
future_modify_at.default <- function(.x, .at, .f, ..., .options = furrr_options(), .progress = deprecated()) {
  sel <- inv_which(.x, .at)
  .x[sel] <- future_map(.x[sel], .f, ..., .options = .options)
  .x
}

#' @rdname future_modify
#' @export
future_modify_if <- function(.x, .p, .f, ..., .options = furrr_options(), .progress = deprecated()) {
  maybe_warn_deprecated_progress(is_present(.progress), what = "future_modify_if")
  UseMethod("future_modify_if")
}

#' @export
future_modify_if.default <- function(.x, .p, .f, ..., .options = furrr_options(), .progress = deprecated()) {
  sel <- probe(.x, .p)
  .x[sel] <- future_map(.x[sel], .f, ..., .options = .options)
  .x
}
