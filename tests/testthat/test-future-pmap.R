# ------------------------------------------------------------------------------
# pmap()

furrr_test_that("future_pmap() matches pmap() for simple cases", {
  expect_identical(
    future_pmap(list(1:3, 4:6, 7:9), ~.x + .y + ..3),
    pmap(list(1:3, 4:6, 7:9), ~.x + .y + ..3)
  )
})

furrr_test_that("names of `.x` are retained", {
  x <- c(a = 1, b = 2)
  y <- c(c = 1, d = 2)
  expect_named(future_pmap(list(x, y), ~1), c("a", "b"))
})

furrr_test_that("named empty input makes named empty output", {
  x <- set_names(list(), character())
  expect_named(future_pmap(list(x, x), ~.x), character())
})

# ------------------------------------------------------------------------------
# atomic variants

furrr_test_that("future_pmap_dbl() works", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)

  expect_identical(
    future_pmap_dbl(list(x, y), ~.x + .y),
    pmap_dbl(list(x, y), ~.x + .y)
  )
})

furrr_test_that("future_pmap_int() works", {
  x <- c(1L, 2L, 3L)
  y <- c(4L, 5L, 6L)

  expect_identical(
    future_pmap_int(list(x, y), ~.x + .y),
    pmap_int(list(x, y), ~.x + .y)
  )
})

furrr_test_that("future_pmap_lgl() works", {
  x <- c(TRUE, FALSE, TRUE)
  y <- c(FALSE, TRUE, TRUE)

  expect_identical(
    future_pmap_lgl(list(x, y), ~.x || .y),
    pmap_lgl(list(x, y), ~.x || .y)
  )
})

furrr_test_that("future_pmap_chr() works", {
  x <- c("a", "b", "c")
  y <- c("d", "e", "f")

  expect_identical(
    future_pmap_chr(list(x, y), ~.y),
    pmap_chr(list(x, y), ~.y)
  )
})

furrr_test_that("future_pmap_raw() works", {
  x <- c("a", "b", "c")
  y <- as.raw(1:3)

  expect_identical(
    future_pmap_raw(list(x, y), ~.y),
    pmap_raw(list(x, y), ~.y)
  )
})

furrr_test_that("names of `.x` are retained", {
  x <- c(a = 1, b = 2)
  y <- c(c = 1, d = 2)
  expect_named(future_pmap_dbl(list(x, y), ~1), c("a", "b"))
})

# ------------------------------------------------------------------------------
# data frame variants

furrr_test_that("future_pmap_dfr() works", {
  x <- c("a", "b", "c")
  y <- c("d", "e", "f")

  expect_identical(
    future_pmap_dfr(list(x, y), ~data.frame(x = .x, y = .y)),
    pmap_dfr(list(x, y), ~data.frame(x = .x, y = .y))
  )
})

furrr_test_that("future_pmap_dfc() works", {
  x <- c("a", "b", "c")
  y <- c("d", "e", "f")

  expect_identical(
    future_pmap_dfc(list(x, y), ~as.data.frame(set_names(list(.x), .y))),
    pmap_dfc(list(x, y), ~as.data.frame(set_names(list(.x), .y)))
  )
})

# ------------------------------------------------------------------------------
# size

furrr_test_that("future_pmap() works with completely empty list", {
  expect_identical(future_pmap(list(), identity), list())
  expect_identical(future_pmap_dbl(list(), identity), double())
})

furrr_test_that("future_pmap() works with size zero input", {
  expect_identical(future_pmap(list(list(), list()), identity), list())
})

furrr_test_that("atomic variants work with size zero input", {
  expect_identical(future_pmap_chr(list(list(), list()), identity), character())
  expect_identical(future_pmap_dbl(list(list(), list()), identity), double())
  expect_identical(future_pmap_int(list(list(), list()), identity), integer())
  expect_identical(future_pmap_lgl(list(list(), list()), identity), logical())
  expect_identical(future_pmap_raw(list(list(), list()), identity), raw())
})

furrr_test_that("size one recycling works", {
  expect_identical(
    future_pmap(list(1, 1:2), ~c(.x, .y)),
    list(c(1, 1), c(1, 2))
  )

  expect_identical(
    future_pmap(list(1:2, 1), ~c(.x, .y)),
    list(c(1, 1), c(2, 1))
  )

  expect_identical(
    future_pmap(list(integer(), 1), ~c(.x, .y)),
    list()
  )

  expect_identical(
    future_pmap(list(1, integer()), ~c(.x, .y)),
    list()
  )
})

furrr_test_that("generally can't recycle to size zero", {
  expect_error(
    future_pmap(list(1:2, integer()), ~c(.x, .y)),
    "Can't recycle"
  )

  expect_error(
    future_pmap(list(integer(), 1:2), ~c(.x, .y)),
    "Can't recycle"
  )
})

# ------------------------------------------------------------------------------
# Miscellaneous

furrr_test_that("named arguments can be passed through", {
  vec_mean <- function(.x, .y, na.rm = FALSE) {
    mean(c(.x,.y), na.rm = na.rm)
  }

  x <- list(c(NA, 1), 1:2)

  expect_identical(
    future_pmap(x, vec_mean, na.rm = TRUE),
    list(1, 1.5)
  )
})

furrr_test_that("arguments can be matched by name", {
  x <- list(x = c(1, 2), y = c(3, 5))

  fn <- function(y, x) {y - x}

  expect_identical(future_pmap_dbl(x, fn), c(2, 3))
})

furrr_test_that("unused components can be absorbed", {
  x <- list(c(1, 2), c(3, 5))

  fn1 <- function(x) {x}
  fn2 <- function(x, ...) {x}

  expect_error(future_pmap_dbl(x, fn1))
  expect_identical(future_pmap_dbl(x, fn2), c(1, 2))
})

furrr_test_that("globals in `.x` and `.y` are found (#16)", {
  fn1 <- function(x) sum(x, na.rm = TRUE)
  fn2 <- function(x) sum(x, na.rm = FALSE)

  x <- list(c(1, 2, NA), c(2, 3, 4))

  fns1 <- map(x, ~ purrr::partial(fn1, x = .x))
  fns2 <- map(x, ~ purrr::partial(fn2, x = .x))

  expect_identical(
    future_pmap(list(fns1, fns2), ~c(.x(), .y())),
    list(c(3, NA), c(9, 9))
  )
})

test_that("globals in `.l` are only exported to workers that use them", {
  plan(multisession, workers = 2)
  on.exit(plan(sequential), add = TRUE)

  # Use `local()` to ensure that the wrapper functions and the anonymous
  # functions created with `~` don't pick up extra globals
  my_wrapper1 <- local({
    my_mean1 <- function(x) mean(x, na.rm = TRUE)

    function(x) {
      my_mean1(x)
      exists("my_mean1")
    }
  })

  my_wrapper2 <- local({
    my_mean2 <- function(x) mean(x, na.rm = FALSE)

    function(x) {
      my_mean2(x)
      exists("my_mean1")
    }
  })

  x <- list(my_wrapper1, my_wrapper2)

  expect_identical(
    future_pmap_lgl(list(x), .f = ~.x(c(1, NA))),
    c(TRUE, FALSE)
  )
})
