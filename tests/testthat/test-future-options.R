# ------------------------------------------------------------------------------
# furrr_options(globals =)

test_that("can detect globals from the caller environment (HenrikBengtsson/future.apply#62)", {
  plan(multisession, workers = 2)
  on.exit(plan(sequential), add = TRUE)

  fn <- function(x) {
    exists("y")
  }

  wrapper <- function(options = furrr_options()) {
    y <- 1
    future_map_lgl(1, fn, .options = options)
  }

  expect_identical(wrapper(), FALSE)

  options <- furrr_options(globals = "y")
  expect_identical(wrapper(options), TRUE)
})

# ------------------------------------------------------------------------------
# furrr_options(packages =)

test_that("can selectively export packages on multisession", {
  plan(multisession, workers = 2)
  on.exit(plan(sequential), add = TRUE)

  opts <- furrr_options(packages = "dplyr")

  expect_error(
    future_map(1:2, ~tibble(x = .x))
  )

  expect_identical(
    future_map(1:2, ~tibble(x = .x), .options = opts),
    list(dplyr::tibble(x = 1L), dplyr::tibble(x = 2L))
  )
})

# ------------------------------------------------------------------------------
# furrr_options(seed =)

furrr_test_that("setting seed keeps reproducible numbers", {
  skip("Until HenrikBengtsson/future#401 is fixed")

  opts <- furrr_options(seed = 1L)

  expect_identical(
    future_map(5, runif, .options = opts),
    future_map(5, runif, .options = opts)
  )
})

# ------------------------------------------------------------------------------
# validation

test_that("validates `stdout`", {
  expect_error(furrr_options(stdout = "x"))
  expect_error(furrr_options(stdout = c(TRUE, TRUE)))
})

test_that("validates `conditions`", {
  expect_error(furrr_options(conditions = 1))
})

test_that("validates `globals`", {
  expect_error(furrr_options(globals = 1))
  expect_error(furrr_options(globals = c(TRUE, TRUE)))
  expect_error(furrr_options(globals = list(a = 1, 2)))
})

test_that("validates `packages`", {
  expect_error(furrr_options(packages = 1))
})

test_that("validates `lazy`", {
  expect_error(furrr_options(lazy = 2))
  expect_error(furrr_options(lazy = NA))
  expect_error(furrr_options(lazy = c(TRUE, FALSE)))
})

test_that("validates `seed`", {
  expect_error(furrr_options(seed = "x"))
  expect_error(furrr_options(seed = c(1, 2)))
  expect_error(furrr_options(seed = c(TRUE, FALSE)))
  expect_error(furrr_options(seed = 1.5))
})

test_that("validates `scheduling`", {
  expect_error(furrr_options(scheduling = c(1, 2)))
  expect_error(furrr_options(scheduling = "x"))
  expect_error(furrr_options(scheduling = 1.5))
  expect_error(furrr_options(scheduling = NA))
})

test_that("validates `chunk_size`", {
  expect_error(furrr_options(chunk_size = c(1, 2)))
  expect_error(furrr_options(chunk_size = "x"))
  expect_error(furrr_options(chunk_size = 1.5))
  expect_error(furrr_options(chunk_size = NA))
})

test_that("validates `prefix`", {
  expect_error(furrr_options(prefix = 1))
  expect_error(furrr_options(prefix = c("x", "y")))
})
