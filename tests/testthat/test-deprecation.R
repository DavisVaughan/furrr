test_that("`future_options()` is hard deprecated", {
  expect_snapshot(error = TRUE, {
    future_options()
  })
})
