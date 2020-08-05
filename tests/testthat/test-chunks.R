test_that("can make chunks from logical `scheduling`", {
  expect_identical(
    make_chunks(n_x = 6L, n_workers = 2L, scheduling = TRUE, chunk_size = NULL),
    list(1:3, 4:6)
  )

  expect_identical(
    make_chunks(n_x = 6L, n_workers = 2L, scheduling = TRUE, chunk_size = NULL),
    make_chunks(n_x = 6L, n_workers = 2L, scheduling = 1L, chunk_size = NULL)
  )

  expect_identical(
    make_chunks(n_x = 5L, n_workers = 3L, scheduling = TRUE, chunk_size = NULL),
    list(1:2, 3L, 4:5)
  )

  expect_identical(
    make_chunks(n_x = 5L, n_workers = 3L, scheduling = TRUE, chunk_size = NULL),
    make_chunks(n_x = 5L, n_workers = 3L, scheduling = 1L, chunk_size = NULL)
  )

  expect_identical(
    make_chunks(n_x = 6L, n_workers = 2L, scheduling = FALSE, chunk_size = NULL),
    as.list(1:6)
  )

  expect_identical(
    make_chunks(n_x = 6L, n_workers = 2L, scheduling = FALSE, chunk_size = NULL),
    make_chunks(n_x = 6L, n_workers = 2L, scheduling = Inf, chunk_size = NULL)
  )
})

test_that("can make chunks from integer `scheduling`", {
  expect_identical(
    make_chunks(n_x = 6L, n_workers = 2L, scheduling = 0L, chunk_size = NULL),
    list(1:6)
  )

  expect_length(
    make_chunks(n_x = 7L, n_workers = 3L, scheduling = 2L, chunk_size = NULL),
    6L
  )

  expect_identical(
    make_chunks(n_x = 3L, n_workers = 3L, scheduling = 2L, chunk_size = NULL),
    list(1L, 2L, 3L)
  )

  expect_identical(
    make_chunks(n_x = 20L, n_workers = 2L, scheduling = 2L, chunk_size = NULL),
    list(1:5, 6:10, 11:15, 16:20)
  )
})

test_that("can make chunks from Inf `scheduling`", {
  expect_identical(
    make_chunks(n_x = 6L, n_workers = 2L, scheduling = Inf, chunk_size = NULL),
    as.list(1:6)
  )
})

test_that("can make chunks from integer `chunk_size`", {
  expect_identical(
    make_chunks(n_x = 6L, n_workers = 2L, scheduling = 1L, chunk_size = 1L),
    as.list(1:6)
  )

  expect_identical(
    make_chunks(n_x = 5L, n_workers = 2L, scheduling = 1L, chunk_size = 2L),
    list(1:2, 3L, 4:5)
  )
})

test_that("can make chunks from Inf `chunk_size`", {
  expect_identical(
    make_chunks(n_x = 6L, n_workers = 2L, scheduling = 1L, chunk_size = Inf),
    list(1:6)
  )
})

test_that("`chunk_size` takes presedence over `scheduling`", {
  expect_identical(
    make_chunks(n_x = 6L, n_workers = 2L, scheduling = 2L, chunk_size = Inf),
    list(1:6)
  )
})

test_that("can use random `ordering` attribute on `scheduling`", {
  x <- structure(2L, ordering = "random")

  set.seed(123)

  o1 <- make_order(n_x = 6L, scheduling = x, chunk_size = NULL)

  expect_length(o1, 6L)
  expect_true(all(1:6 %in% o1))

  # making the order randomly resets the seed
  o2 <- make_order(n_x = 6L, scheduling = x, chunk_size = NULL)
  expect_identical(o1, o2)
})

test_that("can use integer `ordering` attribute", {
  x <- structure(2L, ordering = c(3L, 2L, 1L))

  o <- make_order(n_x = 3L, scheduling = x, chunk_size = NULL)

  expect_identical(o, attr(x, "ordering", exact = TRUE))
})

test_that("can use function `ordering` attribute", {
  ordering_fn <- function(n) {
    rev(seq_len(n))
  }

  x <- structure(2L, ordering = ordering_fn)

  o <- make_order(n_x = 3L, scheduling = x, chunk_size = NULL)

  expect_identical(o, 3:1)
})

test_that("can use random `ordering` attribute on `chunk_size`", {
  x <- structure(2L, ordering = "random")

  set.seed(123)

  o1 <- make_order(n_x = 6L, scheduling = 1L, chunk_size = x)

  expect_length(o1, 6L)
  expect_true(all(1:6 %in% o1))

  # making the order randomly resets the seed
  o2 <- make_order(n_x = 6L, scheduling = x, chunk_size = NULL)
  expect_identical(o1, o2)
})

furrr_test_that("ordering is undone at the end", {
  ordering_fn <- function(n) {
    rev(seq_len(n))
  }

  x <- structure(2L, ordering = ordering_fn)

  opts <- furrr_options(scheduling = x)

  expect_identical(future_map(1:10, ~.x, .options = opts), as.list(1:10))
})

test_that("validates ordering", {
  x <- structure(2L, ordering = "rand")
  expect_error(make_order(n_x = 6L, scheduling = 1L, chunk_size = x), "Unknown")

  x <- structure(2L, ordering = 1L)
  expect_error(make_order(n_x = 6L, scheduling = 1L, chunk_size = x), "length equal")

  x <- structure(2L, ordering = 1.5)
  expect_error(make_order(n_x = 6L, scheduling = 1L, chunk_size = x))
})
