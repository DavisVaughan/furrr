# ------------------------------------------------------------------------------
# future_modify()

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

# ------------------------------------------------------------------------------
# future_modify_at()

furrr_test_that("future_modify_at() default works", {
  expect_identical(future_modify_at(list(1, 2, 3), c(1, 3), ~5), list(5, 2, 5))
  expect_identical(future_modify_at(data.frame(x = 1, y = 2), 2, ~3), data.frame(x = 1, y = 3))
})

furrr_test_that("future_modify_at() variants works", {
  expect_identical(future_modify_at(c(1L, 2L, 3L), c(1, 3), ~5L), c(5L, 2L, 5L))
  expect_identical(future_modify_at(c(1, 2, 3), c(1, 3), ~5), c(5, 2, 5))
  expect_identical(future_modify_at(c("a", "b", "c"), c(1, 3), toupper), c("A", "b", "C"))
  expect_identical(future_modify_at(c(TRUE, FALSE, TRUE), c(1, 3), ~NA), c(NA, FALSE, NA))
})

# ------------------------------------------------------------------------------
# future_modify_if()

furrr_test_that("future_modify_if() default works", {
  expect_identical(future_modify_if(list(1, 2), ~.x == 1, ~3), list(3, 2))
  expect_identical(future_modify_if(data.frame(x = 1, y = 2), ~.x == 1, ~3), data.frame(x = 3, y = 2))
})

furrr_test_that("future_modify_if() `.else` works for default", {
  expect_identical(future_modify_if(list(1, 2, 1, 4), ~.x == 1, ~3, .else = ~4), list(3, 4, 3, 4))
})

furrr_test_that("future_modify_if() variants works", {
  expect_identical(future_modify_if(c(1L, 2L, 3L), ~.x == 1L, ~2L, .else = ~3L), c(2L, 3L, 3L))
  expect_identical(future_modify_if(c(1, 2, 3), ~.x == 1, ~2, .else = ~3), c(2, 3, 3))
  expect_identical(future_modify_if(c("a", "b", "c"), ~.x == "a", toupper, .else = ~"d"), c("A", "d", "d"))
  expect_identical(future_modify_if(c(TRUE, FALSE, TRUE), ~.x == TRUE, ~TRUE, .else = ~NA), c(TRUE, NA, TRUE))
})
