#' Modify elements selectively via futures
#'
#' These functions work exactly the same as [purrr::modify()] functions, but
#' allow you to modify in parallel.
#'
#' @inheritParams purrr::modify
#' @inheritParams future_map
#'
#' @details
#' From purrr:
#'
#' Since the transformation can alter the structure of the input;
#' it's your responsibility to ensure that the transformation produces a valid
#' output. For example, if you're modifying a data frame, `.f` must preserve the
#' length of the input.
#'
#' @return An object the same class as `.x`
#'
#' @export
#' @examples
#' library(magrittr)
#' \donttest{plan(multisession, workers = 2)}
#'
#' # Convert each col to character, in parallel
#' future_modify(mtcars, as.character)
#'
#' iris %>%
#'  future_modify_if(is.factor, as.character) %>%
#'  str()
#'
#' mtcars %>%
#'   future_modify_at(c(1, 4, 5), as.character) %>%
#'   str()
#'
#' \dontshow{
#' # Close open connections for R CMD Check
#' if (!inherits(plan(), "sequential")) plan(sequential)
#' }
future_modify <- function(.x,
                          .f,
                          ...,
                          .options = furrr_options(),
                          .env_globals = parent.frame(),
                          .progress = FALSE) {
  UseMethod("future_modify")
}

#' @export
future_modify.default <- function(.x,
                                  .f,
                                  ...,
                                  .options = furrr_options(),
                                  .env_globals = parent.frame(),
                                  .progress = FALSE) {
  result <- future_map(
    .x = .x,
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )

  for (i in seq_along(.x)) {
    .x[[i]] <- result[[i]]
  }

  .x
}

#' @export
future_modify.character <- function (.x,
                                     .f,
                                     ...,
                                     .options = furrr_options(),
                                     .env_globals = parent.frame(),
                                     .progress = FALSE) {
  result <- future_map_chr(
    .x = .x,
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )

  .x[] <- result

  .x
}

#' @export
future_modify.double <- function (.x,
                                  .f,
                                  ...,
                                  .options = furrr_options(),
                                  .env_globals = parent.frame(),
                                  .progress = FALSE) {
  result <- future_map_dbl(
    .x = .x,
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )

  .x[] <- result

  .x
}

#' @export
future_modify.integer <- function (.x,
                                   .f,
                                   ...,
                                   .options = furrr_options(),
                                   .env_globals = parent.frame(),
                                   .progress = FALSE) {
  result <- future_map_int(
    .x = .x,
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )

  .x[] <- result

  .x
}

#' @export
future_modify.logical <- function (.x,
                                   .f,
                                   ...,
                                   .options = furrr_options(),
                                   .env_globals = parent.frame(),
                                   .progress = FALSE) {
  result <- future_map_lgl(
    .x = .x,
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )

  .x[] <- result

  .x
}

#' @export
future_modify.pairlist <- function (.x,
                                    .f,
                                    ...,
                                    .options = furrr_options(),
                                    .env_globals = parent.frame(),
                                    .progress = FALSE) {
  result <- future_map(
    .x = .x,
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )

  as.pairlist(result)
}

# ------------------------------------------------------------------------------

#' @rdname future_modify
#' @export
future_modify_at <- function(.x,
                             .at,
                             .f,
                             ...,
                             .options = furrr_options(),
                             .env_globals = parent.frame(),
                             .progress = FALSE) {
  UseMethod("future_modify_at")
}

#' @export
future_modify_at.default <- function(.x,
                                     .at,
                                     .f,
                                     ...,
                                     .options = furrr_options(),
                                     .env_globals = parent.frame(),
                                     .progress = FALSE) {
  where <- at_selection(names(.x), .at)
  sel <- inv_which(.x, where)

  future_modify_if(
    .x = .x,
    .p = sel,
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )
}

#' @export
future_modify_at.integer <- function(.x,
                                     .at,
                                     .f,
                                     ...,
                                     .options = furrr_options(),
                                     .env_globals = parent.frame(),
                                     .progress = FALSE) {
  where <- at_selection(names(.x), .at)
  sel <- inv_which(.x, where)

  .x[sel] <- future_map_int(
    .x = .x[sel],
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )

  .x
}

#' @export
future_modify_at.double <- function(.x,
                                    .at,
                                    .f,
                                    ...,
                                    .options = furrr_options(),
                                    .env_globals = parent.frame(),
                                    .progress = FALSE) {
  where <- at_selection(names(.x), .at)
  sel <- inv_which(.x, where)

  .x[sel] <- future_map_dbl(
    .x = .x[sel],
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )

  .x
}

#' @export
future_modify_at.character <- function(.x,
                                       .at,
                                       .f,
                                       ...,
                                       .options = furrr_options(),
                                       .env_globals = parent.frame(),
                                       .progress = FALSE) {
  where <- at_selection(names(.x), .at)
  sel <- inv_which(.x, where)

  .x[sel] <- future_map_chr(
    .x = .x[sel],
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )

  .x
}

#' @export
future_modify_at.logical <- function(.x,
                                     .at,
                                     .f,
                                     ...,
                                     .options = furrr_options(),
                                     .env_globals = parent.frame(),
                                     .progress = FALSE) {
  where <- at_selection(names(.x), .at)
  sel <- inv_which(.x, where)

  .x[sel] <- future_map_lgl(
    .x = .x[sel],
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )

  .x
}

# ------------------------------------------------------------------------------

#' @rdname future_modify
#' @export
future_modify_if <- function(.x,
                             .p,
                             .f,
                             ...,
                             .else = NULL,
                             .options = furrr_options(),
                             .env_globals = parent.frame(),
                             .progress = FALSE) {
  UseMethod("future_modify_if")
}

#' @export
future_modify_if.default <- function(.x,
                                     .p,
                                     .f,
                                     ...,
                                     .else = NULL,
                                     .options = furrr_options(),
                                     .env_globals = parent.frame(),
                                     .progress = FALSE) {
  sel <- probe(.x, .p)
  index <- seq_along(.x)

  result <- future_map(
    .x = .x[sel],
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )

  locs <- index[sel]

  for (i in seq_along(locs)) {
    loc <- locs[[i]]
    .x[[loc]] <- result[[i]]
  }

  if (!is_null(.else)) {
    result_else <- future_map(
      .x = .x[!sel],
      .f = .else,
      ...,
      .options = .options,
      .env_globals = .env_globals,
      .progress = FALSE
    )

    locs_else <- index[!sel]

    for (i in seq_along(locs_else)) {
      loc <- locs_else[[i]]
      .x[[loc]] <- result_else[[i]]
    }
  }

  .x
}

#' @export
future_modify_if.integer <- function(.x,
                                     .p,
                                     .f,
                                     ...,
                                     .else = NULL,
                                     .options = furrr_options(),
                                     .env_globals = parent.frame(),
                                     .progress = FALSE) {
  future_modify_if_variant(
    .x = .x,
    .p = .p,
    .f = .f,
    ...,
    .else = .else,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress,
    .future_map_variant = future_map_int
  )
}

#' @export
future_modify_if.double <- function(.x,
                                    .p,
                                    .f,
                                    ...,
                                    .else = NULL,
                                    .options = furrr_options(),
                                    .env_globals = parent.frame(),
                                    .progress = FALSE) {
  future_modify_if_variant(
    .x = .x,
    .p = .p,
    .f = .f,
    ...,
    .else = .else,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress,
    .future_map_variant = future_map_dbl
  )
}

#' @export
future_modify_if.character <- function(.x,
                                       .p,
                                       .f,
                                       ...,
                                       .else = NULL,
                                       .options = furrr_options(),
                                       .env_globals = parent.frame(),
                                       .progress = FALSE) {
  future_modify_if_variant(
    .x = .x,
    .p = .p,
    .f = .f,
    ...,
    .else = .else,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress,
    .future_map_variant = future_map_chr
  )
}

#' @export
future_modify_if.logical <- function(.x,
                                       .p,
                                       .f,
                                       ...,
                                       .else = NULL,
                                       .options = furrr_options(),
                                       .env_globals = parent.frame(),
                                       .progress = FALSE) {
  future_modify_if_variant(
    .x = .x,
    .p = .p,
    .f = .f,
    ...,
    .else = .else,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress,
    .future_map_variant = future_map_lgl
  )
}

future_modify_if_variant <- function(.x,
                                     .p,
                                     .f,
                                     ...,
                                     .else,
                                     .options,
                                     .env_globals,
                                     .progress,
                                     .future_map_variant) {
  sel <- probe(.x, .p)

  result <- .future_map_variant(
    .x = .x[sel],
    .f = .f,
    ...,
    .options = .options,
    .env_globals = .env_globals,
    .progress = .progress
  )

  .x[sel] <- result

  if (!is_null(.else)) {
    not_sel <- !sel

    result <- .future_map_variant(
      .x = .x[not_sel],
      .f = .else,
      ...,
      .options = .options,
      .env_globals = .env_globals,
      .progress = FALSE
    )

    .x[not_sel] <- result
  }

  .x
}
