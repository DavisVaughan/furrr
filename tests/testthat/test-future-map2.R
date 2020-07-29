# ------------------------------------------------------------------------------
# map2()

furrr_test_that("future_map2() matches map2() for simple cases", {
  expect_identical(
    future_map2(1:3, 4:6, ~.x + .y),
    map2(1:3, 4:6, ~.x + .y)
  )
})

furrr_test_that("names of `.x` are retained", {
  x <- c(a = 1, b = 2)
  y <- c(c = 1, d = 2)
  expect_named(future_map2(x, y, ~1), c("a", "b"))
})

# ------------------------------------------------------------------------------
# atomic variants

furrr_test_that("future_map2_dbl() works", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)

  expect_identical(
    future_map2_dbl(x, y, ~.x + .y),
    map2_dbl(x, y, ~.x + .y)
  )
})

furrr_test_that("future_map2_int() works", {
  x <- c(1L, 2L, 3L)
  y <- c(4L, 5L, 6L)

  expect_identical(
    future_map2_int(x, y, ~.x + .y),
    map2_int(x, y, ~.x + .y)
  )
})

furrr_test_that("future_map2_lgl() works", {
  x <- c(TRUE, FALSE, TRUE)
  y <- c(FALSE, TRUE, TRUE)

  expect_identical(
    future_map2_lgl(x, y, ~.x || .y),
    map2_lgl(x, y, ~.x || .y)
  )
})

furrr_test_that("future_map2_chr() works", {
  x <- c("a", "b", "c")
  y <- c("d", "e", "f")

  expect_identical(
    future_map2_chr(x, y, ~.y),
    map2_chr(x, y, ~.y)
  )
})

furrr_test_that("names of `.x` are retained", {
  x <- c(a = 1, b = 2)
  y <- c(c = 1, d = 2)
  expect_named(future_map2_dbl(x, y, ~1), c("a", "b"))
})

# ------------------------------------------------------------------------------
# data frame variants

furrr_test_that("future_map2_dfr() works", {
  x <- c("a", "b", "c")
  y <- c("d", "e", "f")

  expect_identical(
    future_map2_dfr(x, y, ~data.frame(x = .x, y = .y)),
    map2_dfr(x, y, ~data.frame(x = .x, y = .y))
  )
})

furrr_test_that("future_map2_dfc() works", {
  x <- c("a", "b", "c")
  y <- c("d", "e", "f")

  expect_identical(
    future_map2_dfc(x, y, ~as.data.frame(set_names(list(.x), .y))),
    map2_dfc(x, y, ~as.data.frame(set_names(list(.x), .y)))
  )
})

# ------------------------------------------------------------------------------
# Miscellaneous

furrr_test_that("globals in `.x` and `.y` are found (#16)", {
  fn1 <- function(x) sum(x, na.rm = TRUE)
  fn2 <- function(x) sum(x, na.rm = FALSE)

  x <- list(c(1, 2, NA), c(2, 3, 4))

  fns1 <- map(x, ~ purrr::partial(fn1, x = .x))
  fns2 <- map(x, ~ purrr::partial(fn2, x = .x))

  expect_identical(
    future_map2(fns1, fns2, ~c(.x(), .y())),
    list(c(3, NA), c(9, 9))
  )
})

furrr_test_that("chunk balancing is correct after a recycle (#30)", {
  expect_identical(
    future_map2(1, 1:4, ~c(.x, .y)),
    list(c(1, 1), c(1, 2), c(1, 3), c(1, 4))
  )
})
