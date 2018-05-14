context("test-pmap.R")

# ------------------------------------------------------------------------------
# Setup

.th <- retrieve_test_helpers()
test_msg <- .th$test_msg
test_dat <- .th$test_dat

test_l <- list(test_dat, test_dat)

# ------------------------------------------------------------------------------
# Testing

for(.e in .th$executors) {

  # Don't test multicore on non-Mac
  if(.e == "multicore" && .th$system.os != "Darwin") {
    next
  }

  plan(.e, substitute = FALSE)

  test_that(test_msg(.e, "equivalence with pmap()"), {
    .f <- identical
    .purrr <- purrr::pmap(test_l, sum)
    .furrr <- future_pmap(test_l, sum)
    expect_equal(.purrr, .furrr)
  })

  test_that(test_msg(.e, "equivalence with vector pmap()s"), {
    .f <- ~as.character(sum(.x, .y))
    .purrr <- purrr::pmap_chr(test_l, .f)
    .furrr <- future_pmap_chr(test_l, .f)
    expect_equal(.purrr, .furrr)
  })

  test_that(test_msg(.e, "equivalence with df pmap()s"), {
    .f <- ~data.frame(x = sum(.x, .y))
    .purrr <- purrr::pmap_dfr(test_l, .f)
    .furrr <- future_pmap_dfr(test_l, .f)
    expect_equal(.purrr, .furrr)
  })

  test_that(test_msg(.e, "named arguments can be passed through"), {

    vec_mean <- function(.x, .y, na.rm = FALSE) {
      mean(c(.x,.y), na.rm = na.rm)
    }

    test_l_na <- test_l
    test_l_na[[1]][1] <- NA

    .purrr <- purrr::pmap(test_l_na, vec_mean, na.rm = TRUE)
    .furrr <- future_pmap(test_l_na, vec_mean, na.rm = TRUE)

    expect_equal(.purrr, .furrr)
  })

  test_that(test_msg(.e, "arguments can be matched by name"), {

    test_l_named <- test_l
    names(test_l_named) <- c("x", "y")
    test_l_named[["y"]] <- test_l_named[["y"]] * 2

    .f <- function(y, x) {y - x}

    .purrr <- purrr::pmap(test_l_named, .f)
    .furrr <- future_pmap(test_l_named, .f)

    expect_equal(.purrr, .furrr)
  })

  test_that(test_msg(.e, "unused components can be absorbed"), {

    .f_bad <- function(x)      {x}
    .f     <- function(x, ...) {x}

    .purrr <- purrr::pmap(test_l, .f)
    .furrr <- future_pmap(test_l, .f)

    expect_error(future_pmap(test_l, .f_bad))
    expect_equal(.purrr, .furrr)
  })

}
