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

}
