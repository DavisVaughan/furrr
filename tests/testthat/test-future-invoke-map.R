# ------------------------------------------------------------------------------
# future_invoke_map()

furrr_test_that("future_invoke_map() matches invoke_map() for simple cases", {
  expect_identical(
    future_invoke_map(list(mean, median), list(list(c(1, 2, 3, 6)))),
    invoke_map(list(mean, median), list(list(c(1, 2, 3, 6))))
  )
})

furrr_test_that("named empty input makes named empty output", {
  x <- set_names(list(), character())
  expect_named(
    future_invoke_map(x, list(list(c(1, 2, 3, 6)))),
    character()
  )
})

# ------------------------------------------------------------------------------
# atomic variants

furrr_test_that("future_invoke_map_dbl() works", {
  x <- list(list(c(1, 2, 3)))

  expect_identical(
    future_invoke_map_dbl(mean, x),
    invoke_map_dbl(mean, x)
  )
})

furrr_test_that("future_invoke_map_int() works", {
  x <- list(list(1L))

  expect_identical(
    future_invoke_map_int(identity, x),
    invoke_map_int(identity, x)
  )
})

furrr_test_that("future_invoke_map_lgl() works", {
  x <- list(list(TRUE))

  expect_identical(
    future_invoke_map_lgl(identity, x),
    invoke_map_lgl(identity, x)
  )
})

furrr_test_that("future_invoke_map_chr() works", {
  x <- list(list("a"))

  expect_identical(
    future_invoke_map_chr(identity, x),
    invoke_map_chr(identity, x)
  )
})

furrr_test_that("future_invoke_map_raw() works", {
  x <- list(list(as.raw(1)))

  expect_identical(
    future_invoke_map_raw(identity, x),
    invoke_map_raw(identity, x)
  )
})

furrr_test_that("names of `.x` are retained", {
  f <- list(x = mean, y = median)
  x <- list(list(1))
  expect_named(future_invoke_map_dbl(f, x), c("x", "y"))
})

# ------------------------------------------------------------------------------
# data frame variants

furrr_test_that("future_invoke_map_dfr() works", {
  x <- list(list("a"))

  f <- function(x) {
    data.frame(x = x)
  }

  expect_identical(
    future_invoke_map_dfr(f, x),
    invoke_map_dfr(f, x)
  )
})

furrr_test_that("invoke_future_map_dfc() works", {
  x <- list(list("a"))

  f <- function(x) {
    as.data.frame(set_names(list(1), x))
  }

  expect_identical(
    future_invoke_map_dfc(f, x),
    invoke_map_dfc(f, x)
  )
})
