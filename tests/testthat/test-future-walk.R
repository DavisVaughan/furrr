furrr_test_that("walk functions work", {
  x <- 1:5
  out <- future_walk(x, ~"hello")
  expect_identical(out, x)

  y <- 6:10
  out <- future_walk2(x, y, ~"hello")
  expect_identical(out, x)

  l <- list(x, y)
  out <- future_pwalk(list(x, y), ~"hello")
  expect_identical(out, l)

  out <- future_iwalk(x, ~"hello")
  expect_identical(out, x)
})
