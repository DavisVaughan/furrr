furrr_test_that("future_modify() default method works", {
  expect_identical(future_modify(list(1, 2), ~3), list(3, 3))
  expect_identical(future_modify(data.frame(x = 1, y = 2), ~3), data.frame(x = 3, y = 3))
})

# TODO: Fix `NULL` behavior to match what is done here
# https://github.com/tidyverse/purrr/pull/754
furrr_test_that("future_modify() is not stable when returning `NULL`", {
  expect_identical(
    future_modify(list(1, 2), ~ if (.x == 1) { NULL } else { .x }),
    list(2, 2)
  )
})

furrr_test_that("future_modify() variants work", {
  expect_identical(future_modify(c(1L, 2L, 3L), ~2L), rep(2L, 3))
  expect_identical(future_modify(c(1, 2, 3), ~2), rep(2, 3))
  expect_identical(future_modify(c("a", "b", "c"), toupper), c("A", "B", "C"))
  expect_identical(future_modify(c(TRUE, FALSE, TRUE), ~TRUE), rep(TRUE, 3))
})

furrr_test_that("future_modify(<pairlist>) works", {
  x <- as.pairlist(list(1, 2))
  expect_identical(class(future_modify(x, ~.x)), "pairlist")
})
