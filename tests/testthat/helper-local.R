local_multisession <- function(workers = supported_max_cores("multisession"),
                               frame = caller_env()) {
  plan(multisession, workers = workers)
  withr::defer(plan(sequential), envir = frame)
}
