test_that("can selectively export globals on multisession", {
  local_multisession()

  fn <- function(x) {
    exists("y")
  }

  wrapper <- function(options = future_options()) {
    y <- 1

    future_map_lgl(1, fn, .options = options)
  }

  expect_identical(wrapper(), FALSE)

  skip("Until future.apply#62 is resolved")
  options <- future_options(globals = "y")
  expect_identical(wrapper(options), TRUE)
})

test_that("can selectively export packages on multisession", {
  local_multisession()

  opts <- future_options(packages = "dplyr")

  expect_error(
    future_map(1:2, ~tibble(x = .x))
  )

  expect_identical(
    future_map(1:2, ~tibble(x = .x), .options = opts),
    list(dplyr::tibble(x = 1L), dplyr::tibble(x = 2L))
  )
})

furrr_test_that("setting seed keeps reproducible numbers", {
  opts <- future_options(seed = 1L)

  expect_identical(
    future_map(5, runif, .options = opts),
    future_map(5, runif, .options = opts)
  )
})
