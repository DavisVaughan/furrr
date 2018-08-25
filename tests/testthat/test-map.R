context("test-map.R")

# ------------------------------------------------------------------------------
# Setup

.th <- retrieve_test_helpers()
test_msg <- .th$test_msg
test_dat <- .th$test_dat

# ------------------------------------------------------------------------------
# Testing

for(.e in .th$executors) {

  # Don't test multicore on non-Mac
  if(.e == "multicore" && .th$system.os != "Darwin") {
    next
  }

  plan(.e, substitute = FALSE)

  test_that(test_msg(.e, "equivalence with map()"), {
    .f <- class
    .purrr <- purrr::map(test_dat, .f)
    .furrr <- furrr::future_map(test_dat, .f)
    expect_equal(.purrr, .furrr)
  })

  test_that(test_msg(.e, "equivalence with vector map()s"), {
    .f <- class
    .purrr <- purrr::map_chr(test_dat, .f)
    .furrr <- furrr::future_map_chr(test_dat, .f)
    expect_equal(.purrr, .furrr)
  })

  test_that(test_msg(.e, "equivalence with df map()s"), {
    .f <- ~data.frame(x = class(.x))
    .purrr <- purrr::map_dfr(test_dat, .f)
    .furrr <- furrr::future_map_dfr(test_dat, .f)
    expect_equal(.purrr, .furrr)
  })

  test_that(test_msg(.e, "equivalence with map_at()"), {
    .f <- class
    .purrr <- purrr::map_at(test_dat, 1, .f)
    .furrr <- furrr::future_map_at(test_dat, 1, .f)
    expect_equal(.purrr, .furrr)
  })

  test_that(test_msg(.e, "equivalence with map_if()"), {
    .f <- class
    .purrr <- purrr::map_if(test_dat, ~.x == 1, .f)
    .furrr <- furrr::future_map_if(test_dat, ~.x == 1, .f)
    expect_equal(.purrr, .furrr)
  })

  # See issue #7
  test_that(test_msg(.e, "Working mutate+map double nest with ~"), {
    skip_on_cran()
    deep_list <- dplyr::tibble(
      deep_nest = list(
        list(
          list(a = 4),
          list(b = 6)
        )
      )
    )

    res <- dplyr::mutate(
      .data = deep_list,
      mod_nest = future_map(
        .x = deep_nest,
        .f = ~{
          x <- .x
          purrr::map(x, ~.x)
        })
    )

    expect_equal(rlang::squash_dbl(res$mod_nest), c(a = 4, b = 6))
  })

  # See issue #16
  test_that(test_msg(.e, "Globals in .x are found (.x could be a fn)"), {

    my_robust_sum <- function(x) sum(x, na.rm = TRUE) # Can you find me?
    multi_x <- list(c(1, 2, NA), c(2, 3, 4))
    Xs <- purrr::map(multi_x, ~ purrr::partial(my_robust_sum, .x))

    .purrr <- purrr::map(.x = Xs, .f = ~.x())
    .furrr <- future_map(.x = Xs, .f = ~.x())

    expect_equal(.purrr, .furrr)
  })

  test_that(test_msg(.e, "Globals in .x are only exported to workers that use them"), {

    # This test is difficult to check without manually inspecting what gets exported

    my_mean  <- function(x) { mean(x, na.rm = TRUE) } # Exported to worker 1
    my_mean2 <- function(x) { mean(x, na.rm = TRUE) } # Exported to worker 2

    my_wrapper  <- function(x) { my_mean(x)  }
    my_wrapper2 <- function(x) { my_mean2(x) }

    .l <- list(my_wrapper, my_wrapper2)

    .purrr <- purrr::map(.x = .l, .f = ~.x(1))
    .furrr <- future_map(.x = .l, .f = ~.x(1))

    expect_equal(.purrr, .furrr)
  })

}
