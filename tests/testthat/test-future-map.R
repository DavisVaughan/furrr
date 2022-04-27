# ------------------------------------------------------------------------------
# map()

furrr_test_that("future_map() matches map() for simple cases", {
  expect_identical(
    future_map(1:3, ~.x),
    map(1:3, ~.x)
  )
})

furrr_test_that("names of `.x` are retained", {
  x <- c(a = 1, b = 2)
  expect_named(future_map(x, ~1), c("a", "b"))
})

furrr_test_that("named empty input makes named empty output", {
  x <- set_names(list(), character())
  expect_named(future_map(x, ~.x), character())
})

# ------------------------------------------------------------------------------
# atomic variants

furrr_test_that("future_map_dbl() works", {
  x <- c(1, 2, 3)

  expect_identical(
    future_map_dbl(x, ~.x),
    map_dbl(x, ~.x)
  )
})

furrr_test_that("future_map_int() works", {
  x <- c(1L, 2L, 3L)

  expect_identical(
    future_map_int(x, ~.x),
    map_int(x, ~.x)
  )
})

furrr_test_that("future_map_lgl() works", {
  x <- c(TRUE, FALSE, TRUE)

  expect_identical(
    future_map_lgl(x, ~.x),
    map_lgl(x, ~.x)
  )
})

furrr_test_that("future_map_chr() works", {
  x <- c("a", "b", "c")

  expect_identical(
    future_map_chr(x, ~.x),
    map_chr(x, ~.x)
  )
})

furrr_test_that("future_map_raw() works", {
  x <- as.raw(1:3)

  expect_identical(
    future_map_raw(x, ~.x),
    map_raw(x, ~.x)
  )
})

furrr_test_that("names of `.x` are retained", {
  x <- c(a = 1, b = 2)
  expect_named(future_map_dbl(x, ~1), c("a", "b"))
})

# ------------------------------------------------------------------------------
# data frame variants

furrr_test_that("future_map_dfr() works", {
  x <- c("a", "b", "c")

  expect_identical(
    future_map_dfr(x, ~data.frame(x = .x)),
    map_dfr(x, ~data.frame(x = .x))
  )
})

furrr_test_that("future_map_dfc() works", {
  x <- c("a", "b", "c")

  expect_identical(
    future_map_dfc(x, ~as.data.frame(set_names(list(1), .x))),
    map_dfc(x, ~as.data.frame(set_names(list(1), .x)))
  )
})

# ------------------------------------------------------------------------------
# size

furrr_test_that("future_map() works with size zero input", {
  expect_identical(future_map(list(), identity), list())
})

furrr_test_that("atomic variants work with size zero input", {
  expect_identical(future_map_chr(list(), identity), character())
  expect_identical(future_map_dbl(list(), identity), double())
  expect_identical(future_map_int(list(), identity), integer())
  expect_identical(future_map_lgl(list(), identity), logical())
  expect_identical(future_map_raw(list(), identity), raw())
})

# ------------------------------------------------------------------------------
# at / if variants

furrr_test_that("future_map_at() works", {
  x <- list("a", "b", "c")

  expect_identical(
    future_map_at(x, 2, ~3),
    map_at(x, 2, ~3)
  )
})

furrr_test_that("names of `.x` are retained", {
  x <- list(a = "a", b = "b", c = "c")
  expect_named(future_map_at(x, 2, ~3), c("a", "b", "c"))
})

furrr_test_that("future_map_if() works", {
  x <- list("a", "b", "c")

  expect_identical(
    future_map_if(x, ~.x %in% c("a", "c"), ~3),
    map_if(x, ~.x %in% c("a", "c"), ~3)
  )
})

furrr_test_that("names of `.x` are retained", {
  x <- list(a = "a", b = "b", c = "c")
  expect_named(future_map_if(x, ~.x %in% c("a", "c"), ~3), c("a", "b", "c"))
})

furrr_test_that("`.else` can be used", {
  x <- list("a", "b", "c")

  expect_identical(
    future_map_if(x, ~.x %in% c("a", "c"), ~ 3, .else = ~ -1),
    map_if(x, ~.x %in% c("a", "c"), ~ 3, .else = ~ -1)
  )
})

# ------------------------------------------------------------------------------
# Miscellaneous

furrr_test_that("Calling `~` from within `.f` works", {
  x <- list(
    list(a = 4, b = 6),
    list(c = 5, d = 7)
  )

  expect_identical(future_map(x, ~map(.x, ~.x)), x)
})

furrr_test_that("Calling `~` from within `.f` inside a `mutate()` works (#7, #123)", {
  x <- list(
    list(a = 4, b = 6),
    list(c = 5, d = 7)
  )

  df <- dplyr::tibble(x = x)

  expect_identical(
    dplyr::mutate(df, x = future_map(x, ~map(.x, ~.x))),
    df
  )
})

furrr_test_that("globals in `.x` are found (#16)", {
  fn <- function(x) sum(x, na.rm = TRUE)

  x <- list(c(1, 2, NA), c(2, 3, 4))

  fns1 <- map(x, ~ purrr::partial(fn, x = .x))
  fns2 <- map(x, ~ function() fn(.x))

  expect_identical(future_map_dbl(fns1, ~.x()), c(3, 9))
  expect_identical(future_map_dbl(fns2, ~.x()), c(3, 9))
})


test_that("globals in `.x` are only exported to workers that use them", {
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
    future_map_lgl(.x = x, .f = ~.x(c(1, NA))),
    c(TRUE, FALSE)
  )
})

test_that("furrr is not loaded on the workers", {
  # future has some workarounds when running covr to ensure
  # that the libpath that covr uses is on the workers. I think
  # that somehow this loads furrr, so we just avoid this test when doing covr.
  # https://github.com/HenrikBengtsson/future/blob/d3a3e55cfdfd1bc4c47df3790923ad15c8c0bed1/R/ClusterFuture-class.R#L138
  skip_if("covr" %in% loadedNamespaces())

  plan(multisession, workers = 2)
  on.exit(plan(sequential), add = TRUE)

  # Evaluate in the global env to avoid furrr being
  # in the parent envs of this fn
  fn <- globally(function(x) {
    isNamespaceLoaded("furrr")
  })

  expect_identical(
    future_map_lgl(1:2, fn),
    c(FALSE, FALSE)
  )
})

furrr_test_that("base package functions can be exported to workers (HenrikBengtsson/future#401)", {
  expect_identical(future_map(1:2, identity), list(1L, 2L))
})

furrr_test_that("`.f` globals are only looked up in the function env of `.f` (#153)", {
  fn <- function(x) {
    y
  }

  fn2 <- local({
    y <- -1

    function(x) {
      y
    }
  })

  wrapper <- function(f) {
    y <- 1
    future_map(1:2, f)
  }

  expect_error(wrapper(fn), "'y' not found")
  expect_identical(wrapper(fn2), list(-1, -1))
})

furrr_test_that("`...` globals/packages are found", {
  fn <- globally({
    function(x, fn_arg) {
      fn_arg()
    }
  })

  # Function is passed through `...`
  # Evaluate in the global env so rlang isn't captured in the function env
  # as a package to load
  fn_arg <- globally({
    # `rlang` needs to be loaded on the worker!
    expr <- rlang::expr

    function() {
      expr(1)
    }
  })

  expect_identical(
    future_map(1:2, fn, fn_arg = fn_arg),
    list(1, 1)
  )
})
