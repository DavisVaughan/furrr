context("test-progress.R")

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

  # No progress on sequential
  if(.e == "sequential") {
    next
  }

  plan(.e, substitute = FALSE)

  test_that(test_msg(.e, "Progress bar is emitted on long running tasks"), {
    output <- capture.output(
      .furrr <- furrr::future_map(test_dat, ~Sys.sleep(runif(1, 2, 3)), .progress = TRUE)
    )
    # Is "Progress" in the output text?
    expect_true(grepl("Progress", output[1]))
  })

  test_that(test_msg(.e, "Progress bar is not emitted on short running tasks"), {
    output <- capture.output(
      .furrr <- furrr::future_map(test_dat, ~.x, .progress = TRUE)
    )
    # Is "Progress" in the output text?
    expect_false(grepl("Progress", output[1]))
  })

}
