test_that("can use deprecated `future_options()`", {
  local_options(lifecycle_verbosity = "warning")

  expect_identical(
    expect_warning(future_options()),
    furrr_options()
  )
  expect_identical(
    expect_warning(
      future_options(globals = "x", packages = "dplyr", seed = 1, lazy = TRUE, scheduling = 2)
    ),
    furrr_options(globals = "x", packages = "dplyr", seed = 1, lazy = TRUE, scheduling = 2)
  )
})

test_that("deprecated `future_options()` warns when used", {
  local_options(lifecycle_verbosity = "warning")

  expect_snapshot(
    future_options()
  )
})
