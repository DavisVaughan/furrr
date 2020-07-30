furrr_test_that <- function(desc, code) {
  test_code <- enquo(code)

  # Ensure that we reset to sequential plan even on error to
  # close any straggler processes
  on.exit(plan(sequential), add = TRUE)

  for (strategy in supported_strategies()) {
    for (cores in seq2(1L, supported_max_cores(strategy))) {
      if (identical(strategy, "sequential")) {
        plan(strategy)
      } else {
        plan(strategy, workers = cores)
      }

      test_desc <- paste0(desc, " / strategy - ", strategy, " / cores - ", cores)
      test_expr <- expr(test_that(test_desc, !!test_code))

      eval_tidy(test_expr)
    }
  }
}

supported_strategies <- function() {
  strategies <- c("sequential", "multisession", "multicore")

  supportsMulticore <- import_future("supportsMulticore")

  if (!supportsMulticore()) {
    strategies <- setdiff(strategies, "multicore")
  }

  strategies
}

supported_max_cores <- function(strategy) {
  if (identical(strategy, "sequential")) {
    1L
  } else {
    min(2L, future::availableCores())
  }
}
