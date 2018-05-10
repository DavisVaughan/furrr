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

}
