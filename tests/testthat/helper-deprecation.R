# Until this can be fixed in lifecycle itself
expect_deprecated <- function(expr, regexp = NULL) {
  local_options(lifecycle_verbosity = "warning")
  info <- capture_deprecated(enquo(expr))

  testthat::expect(is_true(info$thrown), "Deprecation warning was not thrown.")

  if (!is.null(regexp)) {
    found <- grepl(regexp, info$cnd$message)[[1L]]
    testthat::expect(found, "`regexp` was not found in deprecation warning message.")
  }

  info$value
}

capture_deprecated <- function(expr) {
  info <- list(thrown = FALSE)

  handler <- function(cnd) {
    info$thrown <<- TRUE
    info$cnd <<- cnd
    cnd_muffle(cnd)
  }

  info$value <- withCallingHandlers(
    eval_tidy(expr),
    lifecycle_warning_deprecated = handler
  )

  info
}
