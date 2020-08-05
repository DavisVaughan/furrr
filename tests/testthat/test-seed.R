test_that("checks list of seeds for length", {
  old_rngkind <- RNGkind("L'Ecuyer-CMRG")
  on.exit(RNGkind(old_rngkind[[1]]), add = TRUE)

  lecuyer_seed <- get_random_seed()

  x <- list(lecuyer_seed, lecuyer_seed, lecuyer_seed)
  y <- list(lecuyer_seed)

  expect_identical(make_seeds(x, 3), x)
  expect_error(make_seeds(y, 3), "3, not length 1")
})

test_that("can make `seeds` from `TRUE` `seed`", {
  x <- make_seeds(TRUE, 5)
  expect_identical(length(x), 5L)
  expect_true(all(purrr::map_int(x, length) == 7L))
})

test_that("can make reproducible `seeds` from integer `seed`", {
  x <- make_seeds(1L, 5)
  y <- make_seeds(1L, 5)
  expect_identical(length(x), 5L)
  expect_true(all(purrr::map_int(x, length) == 7L))
  expect_identical(x, y)
})
