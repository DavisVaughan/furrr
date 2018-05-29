context("test-map2.R")

# ------------------------------------------------------------------------------
# Setup

.th <- retrieve_test_helpers()
test_msg <- .th$test_msg
test_dat <- .th$test_dat
test_dat2 <- test_dat

# ------------------------------------------------------------------------------
# Testing

for(.e in .th$executors) {

  # Don't test multicore on non-Mac
  if(.e == "multicore" && .th$system.os != "Darwin") {
    next
  }

  plan(.e, substitute = FALSE)

  test_that(test_msg(.e, "equivalence with map2()"), {
    .f <- identical
    .purrr <- purrr::map2(test_dat, test_dat2, .f)
    .furrr <- furrr::future_map2(test_dat, test_dat2, .f)
    expect_equal(.purrr, .furrr)
  })

  test_that(test_msg(.e, "equivalence with vector map2()s"), {
    .f <- ~as.character(sum(.x, .y))
    .purrr <- purrr::map2_chr(test_dat, test_dat2, .f)
    .furrr <- furrr::future_map2_chr(test_dat, test_dat2, .f)
    expect_equal(.purrr, .furrr)
  })

  test_that(test_msg(.e, "equivalence with df map2()s"), {
    .f <- ~data.frame(x = sum(.x, .y))
    .purrr <- purrr::map2_dfr(test_dat, test_dat2, .f)
    .furrr <- furrr::future_map2_dfr(test_dat, test_dat2, .f)
    expect_equal(.purrr, .furrr)
  })

  # See issue #16
  test_that(test_msg(.e, "Globals in .x and .y are found (.y could be a fn)"), {

    my_robust_sum <- function(x) sum(x, na.rm = TRUE)  # Can you find me?
    my_robust_sum2 <- function(x) sum(x, na.rm = TRUE) # Can you find me?
    multi_x <- list(c(1, 2, NA), c(2, 3, 4))
    Xs <- purrr::map(multi_x, ~ purrr::partial(my_robust_sum, .x))
    Ys <- purrr::map(multi_x, ~ purrr::partial(my_robust_sum2, .x))

    .purrr <- purrr::map2(.x = Xs, .y = Ys, .f = ~c(.x(), .y()))
    .furrr <- future_map2(.x = Xs, .y = Ys, .f = ~c(.x(), .y()))

    expect_equal(.purrr, .furrr)
  })

}
