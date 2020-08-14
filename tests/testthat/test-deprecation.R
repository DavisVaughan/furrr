test_that("can use deprecated `future_options()`", {
  expect_identical(
    expect_deprecated(
      future_options(),
      "`future_options[(][)]` is deprecated as of furrr 0.2.0"
    ),
    furrr_options()
  )

  expect_identical(
    expect_deprecated(
      future_options(globals = "x", packages = "dplyr", seed = 1, lazy = TRUE, scheduling = 2)
    ),
    furrr_options(globals = "x", packages = "dplyr", seed = 1, lazy = TRUE, scheduling = 2)
  )
})
