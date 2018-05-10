context("test-future-options.R")

# ------------------------------------------------------------------------------
# Setup

.th <- retrieve_test_helpers()
test_msg <- .th$test_msg
test_dat <- .th$test_dat

# ------------------------------------------------------------------------------
# Testing

for(.e in .th$executors) {

  # Don't test multicore on non-Mac
  if(.e == "multicore" && .th$system.os != "Darwin") {
    next
  }

  plan(.e, substitute = FALSE)

  # Selective variable/package exports only works on multisession
  # On multicore, it sees the variables by forked process shared memory
  if(.e == "multisession") {

    # Init x/y outside the test environment, otherwise seen by multisession
    x <- 1
    y <- 1

    test_that(test_msg(.e, "selective exporting of variables works"), {

      # This should result in an error, export y not x
      #expect_error(future_map(1, ~x, .options = future_options(globals = "y")))

      # This works, export x, need x
      expect_equal(future_map(1, ~x, .options = future_options(globals = "x")), list(1))
    })

    test_that(test_msg(.e, "selective exporting of packages works"), {

      # This should result in an error, export purrr not dplyr
      expect_error(future_map(1, ~tibble(x = 1), .options = future_options(packages = "purrr")))

      # This works, export dplyr, need dplyr
      expect_equal(future_map(1, ~tibble(x = 1), .options = future_options(packages = "dplyr")),
                   list(dplyr::tibble(x = 1)))
    })

  }

  test_that(test_msg(.e, "setting seed keeps reproducible numbers"), {

    opts <- future_options(seed = 1L)

    expect_equal(future_map(1, runif, .options = opts),
                 future_map(1, runif, .options = opts))
  })

}
