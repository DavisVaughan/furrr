retrieve_test_helpers <- function() {
  # Take advantage of scoping
  test_msg <- function(executor, ...) {
    paste(executor, ..., sep = " - ")
  }

  executors <- c("sequential", "multisession", "multicore")
  system.os <- Sys.info()[["sysname"]]

  test_dat <- seq_len(4)

  list(
    test_msg = test_msg,
    executors = executors,
    system.os = system.os,
    test_dat = test_dat
  )
}
