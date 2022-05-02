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

furrr_test_that("`future_walk()` - template returns `NULL`s (#205)", {
  .x <- 1:2
  .f <- identity
  .options <- furrr_options()
  .progress <- FALSE
  .env_globals <- environment()

  # `map()` template returns `.f` output
  map_fn <- purrr::map
  out <- furrr_map_template(
    x = .x,
    fn = .f,
    dots = list(),
    options = .options,
    progress = .progress,
    type = "list",
    map_fn = map_fn,
    env_globals = .env_globals
  )
  expect_identical(out, list(1L, 2L))

  # `walk()` template returns `NULL`s
  map_fn <- purrr::walk
  out <- furrr_map_template(
    x = .x,
    fn = .f,
    dots = list(),
    options = .options,
    progress = .progress,
    type = "list",
    map_fn = map_fn,
    env_globals = .env_globals
  )
  expect_identical(out, list(NULL, NULL))
})

furrr_test_that("`future_walk2()` - template returns `NULL`s (#205)", {
  .x <- 1:2
  .y <- 3:4
  .f <- function(x, y) c(x, y)
  .options <- furrr_options()
  .progress <- FALSE
  .env_globals <- environment()

  # `map2()` template returns `.f` output
  map_fn <- purrr::map2
  out <- furrr_map2_template(
    x = .x,
    y = .y,
    fn = .f,
    dots = list(),
    options = .options,
    progress = .progress,
    type = "list",
    map_fn = map_fn,
    env_globals = .env_globals
  )
  expect_identical(out, list(c(1L, 3L), c(2L, 4L)))

  # `walk2()` template returns `NULL`s
  map_fn <- purrr::walk2
  out <- furrr_map2_template(
    x = .x,
    y = .y,
    fn = .f,
    dots = list(),
    options = .options,
    progress = .progress,
    type = "list",
    map_fn = map_fn,
    env_globals = .env_globals
  )
  expect_identical(out, list(NULL, NULL))
})

furrr_test_that("`future_pwalk()` - template returns `NULL`s (#205)", {
  .l <- list(1:2, 3:4, 5:6)
  .f <- function(x, y, z) c(x, y, z)
  .options <- furrr_options()
  .progress <- FALSE
  .env_globals <- environment()

  # `pmap()` template returns `.f` output
  map_fn <- purrr::pmap
  out <- furrr_pmap_template(
    l = .l,
    fn = .f,
    dots = list(),
    options = .options,
    progress = .progress,
    type = "list",
    map_fn = map_fn,
    env_globals = .env_globals
  )
  expect_identical(out, list(c(1L, 3L, 5L), c(2L, 4L, 6L)))

  # `pwalk()` template returns `NULL`s
  map_fn <- purrr::pwalk
  out <- furrr_pmap_template(
    l = .l,
    fn = .f,
    dots = list(),
    options = .options,
    progress = .progress,
    type = "list",
    map_fn = map_fn,
    env_globals = .env_globals
  )
  expect_identical(out, list(NULL, NULL))
})
