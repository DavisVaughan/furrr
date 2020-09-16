# ------------------------------------------------------------------------------
# furrr_options(globals =)

test_that("can export globals using a named list", {
  plan(multisession, workers = 2)
  on.exit(plan(sequential), add = TRUE)

  opts <- furrr_options(globals = list(y = 1))

  fn <- function(x) {
    exists("y")
  }

  expect_identical(future_map_lgl(1:2, fn, .options = opts), c(TRUE, TRUE))
})

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

test_that("can export globals with sequential futures", {
  skip("Until HenrikBengtsson/future.apply#10 is fixed")

  plan(sequential)

  fn <- function(x) {
    exists("y")
  }

  wrapper <- function(options = furrr_options()) {
    y <- 1
    future_map_lgl(1, fn, .options = options)
  }

  # With named list
  opts <- furrr_options(globals = list(y = 1))
  expect_identical(future_map_lgl(1:2, fn, .options = opts), c(TRUE, TRUE))

  # With character lookup in caller env
  opts <- furrr_options(globals = "y")
  expect_identical(wrapper(opts), TRUE)
})

test_that("validates `globals`", {
  expect_error(furrr_options(globals = 1))
  expect_error(furrr_options(globals = c(TRUE, TRUE)))
  expect_error(furrr_options(globals = list(a = 1, 2)))
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

test_that("validates `packages`", {
  expect_error(furrr_options(packages = 1))
})

# ------------------------------------------------------------------------------
# furrr_options(stdout =)

furrr_test_that("can use `stdout = FALSE`", {
  opts <- furrr_options(stdout = FALSE)

  fn <- function(x) {
    con <- stdout()
    write("hello", con)
    x
  }

  expect_silent(future_map(1:5, fn, .options = opts))
})

test_that("validates `stdout`", {
  expect_error(furrr_options(stdout = "x"))
  expect_error(furrr_options(stdout = c(TRUE, TRUE)))
})

# ------------------------------------------------------------------------------
# furrr_options(conditions =)

test_that("can capture no conditions", {
  # Only works when not doing sequential, see HenrikBengtsson/future#403
  plan(multisession, workers = 2)
  on.exit(plan(sequential), add = TRUE)

  opts <- furrr_options(conditions = character())

  fn <- function(x) {
    warning("hello")
    x
  }

  expect_warning(future_map(1:5, fn, .options = opts), NA)
})

test_that("validates `conditions`", {
  expect_error(furrr_options(conditions = 1))
})

# ------------------------------------------------------------------------------
# furrr_options(lazy =)

furrr_test_that("can use lazy futures", {
  opts <- furrr_options(lazy = TRUE)
  expect_identical(future_map(1:5, ~.x, .options = opts), as.list(1:5))
})

test_that("validates `lazy`", {
  expect_error(furrr_options(lazy = 2))
  expect_error(furrr_options(lazy = NA))
  expect_error(furrr_options(lazy = c(TRUE, FALSE)))
})

# ------------------------------------------------------------------------------
# furrr_options(seed =)

furrr_test_that("can use integer `seed` of size 1", {
  old_seed <- get_random_seed()

  opts <- furrr_options(seed = 123)

  x <- future_map(5, runif, .options = opts)

  expect_false(identical(old_seed, get_random_seed()))

  y <- future_map(5, runif, .options = opts)

  expect_identical(x, y)
})

furrr_test_that("can use integer `seed` of size 7", {
  old_rngkind <- RNGkind("L'Ecuyer-CMRG")
  lecuyer_seed <- get_random_seed()

  # Reset
  RNGkind(old_rngkind[[1]])
  old_seed <- get_random_seed()

  opts <- furrr_options(seed = lecuyer_seed)

  x <- future_map(5, runif, .options = opts)

  expect_false(identical(old_seed, get_random_seed()))

  y <- future_map(5, runif, .options = opts)

  expect_identical(x, y)
})

furrr_test_that("`TRUE` advances `.Random.seed` but `FALSE` doesn't", {
  old_seed <- get_random_seed()

  opts <- furrr_options(seed = FALSE)
  future_map(1:5, ~.x, .options = opts)

  expect_identical(old_seed, get_random_seed())

  opts <- furrr_options(seed = TRUE)
  future_map(1:5, ~.x, .options = opts)

  expect_false(identical(old_seed, get_random_seed()))
})

furrr_test_that("`NULL` seed doesn't advance `.Random.seed`", {
  old_seed <- get_random_seed()

  opts <- furrr_options(seed = NULL)
  future_map(1:5, ~.x, .options = opts)

  expect_identical(old_seed, get_random_seed())
})

test_that("must use valid L'Ecuyer-CMRG seed", {
  opts <- furrr_options(seed = 1:7)
  expect_error(future_map(1:5, ~.x, .options = opts), "must be")
})

test_that("validates `seed`", {
  expect_error(furrr_options(seed = "x"))
  expect_error(furrr_options(seed = c(1, 2)))
  expect_error(furrr_options(seed = c(TRUE, FALSE)))
  expect_error(furrr_options(seed = 1.5))
  expect_error(furrr_options(seed = NA))
})

# ------------------------------------------------------------------------------
# furrr_options(scheduling =)

test_that("validates `scheduling`", {
  expect_error(furrr_options(scheduling = c(1, 2)))
  expect_error(furrr_options(scheduling = "x"))
  expect_error(furrr_options(scheduling = 1.5))
  expect_error(furrr_options(scheduling = NA))
})

# ------------------------------------------------------------------------------
# furrr_options(chunk_size =)

test_that("can specify `chunk_size`", {
  x <- furrr_options(chunk_size = 2)
  expect_identical(x$chunk_size, 2L)
})

test_that("validates `chunk_size`", {
  expect_error(furrr_options(chunk_size = c(1, 2)))
  expect_error(furrr_options(chunk_size = "x"))
  expect_error(furrr_options(chunk_size = 1.5))
  expect_error(furrr_options(chunk_size = NA))
})

# ------------------------------------------------------------------------------
# furrr_options(prefix =)

furrr_test_that("labels can be passed through without erroring", {
  opts <- furrr_options(prefix = "foo")
  expect_error(future_map(1:5, ~.x, .options = opts), NA)
})

test_that("validates `prefix`", {
  expect_error(furrr_options(prefix = 1))
  expect_error(furrr_options(prefix = c("x", "y")))
})

# ------------------------------------------------------------------------------
# validation

test_that("option object is validated", {
  expect_error(future_map(1:5, ~.x, .options = 1))
  expect_error(future_map2(1:5, 1:5, ~.x, .options = 1))
  expect_error(future_pmap(list(1:5), ~.x, .options = 1))
})
